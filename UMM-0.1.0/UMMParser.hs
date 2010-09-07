{- Copyright 2009 Uwe Hollerbach <uh@alumni.caltech.edu>

This file is part of umm, Uwe's Money Manager.

umm is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3 of the License, or (at your
option) any later version.

umm is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
License for more details.

You should have received a copy of the GNU General Public License
along with umm; if not, write to the Free Software Foundation, Inc.,
59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

$Id: UMMParser.hs,v 1.28 2009/11/14 22:41:36 uwe Exp $ -}

-- TODO: template := <to be determined>

module UMMParser (parseURecord, parseUDate, parseUCommand) where
import Prelude
import Data.Char
import Data.Ratio
import Text.ParserCombinators.Parsec as TPCP hiding (spaces)

import UMMData

readInt s = foldl ma 0 s
  where ma v1 v2 = 10*v1 + (toInteger (digitToInt v2))

readAmt ip fp =
  let l = length fp
      e = 10^l
      iv = readInt ip
      fv = readInt fp
  in (e*iv + fv) % e

trimspace str = f (f str)
  where f = dropWhile isSpace . reverse

-- Hide parsec's spaces and substitute this one:
-- want "spaces" to mean an actual something there

spaces = many1 space

escChar =
  do char '\\'
     c <- anyChar
     return (case c of
             'a' -> chr 7
             'b' -> chr 8
             't' -> '\t'
             'n' -> '\n'
             'v' -> chr 11
             'f' -> chr 12
             'r' -> '\r'
             _ -> c)

-- The usual quoted-string with escaped quotes inside

parseString =
  do spaces
     char '"'
     str <- many (escChar <|> (noneOf "\""))
     char '"'
     return str

parseOptionalString = option "" (TPCP.try parseString)

-- TODO: are there some here that we need to add or remove?

symbol = oneOf "!$%&*+-/:<=>?@^_~"

parseName =
  do spaces
     first <- letter
     rest <- many (letter <|> digit <|> symbol)
     return (Name (first:rest))

parseOptionalName = option (Name "") (TPCP.try parseName)

parseInt = many1 digit >>= return . readInt

-- Parse first alternative for amounts: \d+(\.\d*)?

parseAmt1 =
  do ip <- many1 digit
     fp <- option "" (char '.' >> many digit)
     return (readAmt ip fp)

-- Parse second alternative for amounts: \.\d+

parseAmt2 =
  do char '.'
     fp <- many1 digit
     return (readAmt "0" fp)

-- Parse third alternative for amounts: \d+/\d+: this is how rationals
-- get displayed if they're not integers scaled by a power of 10, so
-- we'd better be able to handle them...

parseAmt3 =
  do n <- parseInt
     char '/'
     d <- parseInt
     return (n%d)

-- Parse a floating-point-formatted number as either \d+(\.\d*)? or \.\d+

parseAmount =
  do many space
     s <- option '+' (oneOf "+-")
     v <- (TPCP.try parseAmt3) <|>
           parseAmt1 <|>
           parseAmt2
     return (Amount (if s == '+' then v else negate v))

parseOptionalAmount = option (Amount 1) (TPCP.try parseAmount)

-- A date: three groups of digits separated by '/' or '-' (but not mixed)
-- where the first group of digits is the year, the second the month, and
-- the third the day: YYYY/MM/DD or YYYY-MM-DD: no messing around with
-- whether days come first or months, and also no implicit 19xx or 20xx
-- or whatever.

parseDate =
  do spaces
     y <- parseInt
     m <- pMS '/' <|> pMS '-'
     d <- parseInt
     return (Date (fromInteger y) (fromInteger m) (fromInteger d))
  where pMS s =
          do char s
             m <- parseInt
             char s
             return m

parseReconcile =
  option False (TPCP.try (many space >> char '*' >> return True))

-- The top-level record parsers

parseCIE =
  do rtype <- string "ccs" <|> string "income" <|> string "expense"
     name <- parseName
     desc <- parseOptionalString
     return (case rtype of
               "ccs" -> CCSRec name desc
               "income" -> IncomeRec name desc
               "expense" -> ExpenseRec name desc)

parseAccount =
  do string "account"
     name <- parseName
     date <- parseDate
     desc <- parseOptionalString
     return (AccountRec name date desc)

parseGroup =
  do string "group"
     names <- many1 parseName
     return (GroupRec (head names) (tail names))

parsePrice =
  do string "price"
     date <- parseDate
     amt1 <- parseOptionalAmount
     name1 <- parseName
     amt2 <- parseAmount
     name2 <- parseOptionalName
     return (PriceRec date False (CCSAmount name1 amt1) (CCSAmount name2 amt2))

parseXfer =
  do string "xfer"
     rec <- parseReconcile
     date <- parseDate
     acc1 <- parseName
     acc2 <- parseName
     amt <- parseAmount
     ccs <- parseOptionalName
     memo <- parseOptionalString
     chknum <- option 0 (TPCP.try (spaces >> parseInt))
     return (XferRec date rec acc1 acc2 (CCSAmount ccs amt)
                     memo (fromInteger chknum))

-- 'buy' and 'sell' are purely syntactic sugar for 'exch': 'buy' is
-- exactly the same, and 'sell' is the same as if {amt1,ccs1} and
-- {amt2,ccs2} were swapped in an exch

parseEBS =
  do rtype <- string "buy" <|> string "sell" <|> string "exch"
     rec <- parseReconcile
     date <- parseDate
     acct <- parseName
     amt1 <- parseAmount
     name1 <- parseName
     amt2 <- parseAmount
     name2 <- parseOptionalName
     memo <- parseOptionalString
     let et = case rtype of
                "buy" -> B
                "sell" -> S
                "exch" -> E
         ccsa1 = CCSAmount name1 amt1
         ccsa2 = CCSAmount name2 amt2
     if et == S
        then return (ExchRec et date rec acct ccsa2 ccsa1 memo)
        else return (ExchRec et date rec acct ccsa1 ccsa2 memo)

parseSplit =
  do string "split"
     date <- parseDate
     name <- parseName
     amt1 <- parseAmount
     amt2 <- parseAmount
     return (SplitRec date name amt1 amt2)

parseTodo =
  do string "todo"
     rec <- parseReconcile
     date <- parseDate
     spaces
     memo <- many anyChar
     return (ToDoRec date rec (trimspace memo))

parseComment =
  do many1 (char '#')
     many space
     comment <- many anyChar
     return (CommentRec (trimspace comment))

parseBlank = many space >> return (CommentRec "")

parseRecord =
  do many space
     record <- parsePrice
           <|> parseXfer
           <|> (TPCP.try parseCIE)
           <|> (TPCP.try parseEBS)
           <|> parseSplit
           <|> parseTodo
           <|> (TPCP.try parseAccount)
           <|> parseGroup
           <|> parseComment
           <|> parseBlank	-- this must be last, as it can match nothing
     many space
     eof
     return record

parseURecord input =
  case parse parseRecord "umm record" input of
       Left _ -> ErrorRec input
       Right val -> val

parseUDate input = parse parseDate "umm date" (" " ++ input)

parsePrefixOf n str =
  string (take n str) >> opts (drop n str) >> return str
  where opts [] = return ()
        opts (c:cs) = optional (char c >> opts cs)

parseCmdBalance now =
  do parsePrefixOf 3 "balance"
     name <- parseOptionalName
     date <- option now (TPCP.try parseDate)
     return (BalanceCmd name date)

parseCmdBasis now =
  do parsePrefixOf 3 "basis"
     name <- parseName
     date <- option now (TPCP.try parseDate)
     return (BasisCmd name date)

-- TODO: make verbose/nonverbose work... somehow add optional verbosity
-- ok, this is really cheesy! make it better!

parseCmdChange now =
  do parsePrefixOf 1 "change"
     name <- parseName
     date1 <- option now parseDate
     date2 <- option start_time parseDate
     if compare date2 date1 == LT
        then return (ChangeCmd True name date2 date1)
        else return (ChangeCmd False name date1 date2)

parseCmdList =
  do parsePrefixOf 1 "list"
     many space
     w <- option "all" (parsePrefixOf 1 "ccs" <|>
                        parsePrefixOf 1 "groups" <|>
                        parsePrefixOf 1 "incomes" <|>
                        parsePrefixOf 1 "expenses" <|>
                        TPCP.try (parsePrefixOf 2 "all") <|>
                        parsePrefixOf 2 "accounts")
     return (ListDataCmd (case w of
                            "all" -> COLAll
                            "ccs" -> COLCCS
                            "accounts" -> COLAccts
                            "groups" -> COLGrps
                            "incomes" -> COLIncs
                            "expenses" -> COLExps))

parseCmdPrice now =
  do parsePrefixOf 1 "price"
     name <- parseName
     date <- option now (TPCP.try parseDate)
     return (PriceCmd name date)

parseCmdReconcile now =
  do parsePrefixOf 3 "reconcile"
     name <- parseOptionalName
     date <- option now (TPCP.try parseDate)
     return (ReconcileCmd name date)

parseCmdRegister now =
  do parsePrefixOf 3 "register"
     name <- parseName
     date1 <- option now (TPCP.try parseDate)
     date2 <- option start_time (TPCP.try parseDate)
     if compare date2 date1 == LT
        then return (RegisterCmd name date2 date1)
        else return (RegisterCmd name date1 date2)

parseCmdToDo now =
  do parsePrefixOf 1 "todo"
     date <- option now (TPCP.try parseDate)
     return (ToDoCmd date)

parseCommand date =
  do cmd <- parseCmdChange date
        <|> parseCmdList
        <|> parseCmdPrice date
        <|> parseCmdToDo date
        <|> TPCP.try (parseCmdBalance date)
        <|> parseCmdBasis date
        <|> TPCP.try (parseCmdRegister date)
        <|> parseCmdReconcile date
     eof
     return cmd

parseUCommand now input =
  case parse (parseCommand now) "umm command" input of
       Left err -> error (show err)
       Right val -> val
