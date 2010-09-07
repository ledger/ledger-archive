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

$Id: UMMData.hs,v 1.29 2009/11/18 22:30:36 uwe Exp $ -}

module UMMData (Name(Name), Date(Date), Amount(Amount), start_time,
                Command(BalanceCmd, RegisterCmd, ReconcileCmd, ListDataCmd,
                ToDoCmd, PriceCmd, ChangeCmd, BasisCmd), CmdOpt(COLAll, COLCCS,
                COLAccts, COLGrps, COLIncs, COLExps), Record(CCSRec, IncomeRec,
                ExpenseRec, AccountRec, ToDoRec, PriceRec, XferRec, ExchRec,
                SplitRec, GroupRec, CommentRec, ErrorRec), genDate,
                getRecDate, cmpRecDate, getRecName, cmpRecName,
                Ledger(Ledger), runLedger, getResult, getInfo, getErrs,
                recordInfo, recordErr, recordNil, BSE(B,S,E),
                CCSAmt(CCSAmount), cmpCCSAmountName, eqCCSAmountName,
                noName, todoName, joinDrop, isLeap, validDate) where
import Prelude
import Data.Char
import Data.List
import Data.Ratio
import System.Time

-- UMM values: names, dates, amounts, records

newtype Name = Name String deriving (Eq, Ord)
instance Show Name where
  show (Name name) = name

noName = Name ""
todoName = Name " todo "

data Date = Date Int Int Int deriving (Eq)

instance Show Date where
  show (Date y m d) = show y ++ "-" ++ show2 m ++ "-" ++ show2 d
    where show2 n = reverse (take 2 (reverse (show n) ++ "00"))

instance Ord Date where
  compare (Date y1 m1 d1) (Date y2 m2 d2) =
    let dy = compare y1 y2
        dm = compare m1 m2
    in if dy /= EQ then dy else if dm /= EQ then dm else compare d1 d2

genDate (CalendarTime {ctYear = y, ctMonth = m, ctDay = d}) =
  let m2 = case m of
             January   -> 1
             February  -> 2
             March     -> 3
             April     -> 4
             May       -> 5
             June      -> 6
             July      -> 7
             August    -> 8
             September -> 9
             October   -> 10
             November  -> 11
             December  -> 12
  in Date y m2 d

start_time = Date (-4003) 10 23		-- 9 AM (GMT)

newtype Amount = Amount Rational deriving (Eq, Ord)
instance Show Amount where
  show (Amount amt) = if amt < 0 then "-" ++ shRat (-amt) else shRat amt

shRat val =
  let n = numerator val
      d = denominator val
      (p2,r2) = divout d 2
      (p5,r5) = divout r2 5
      e10 = max p2 p5
      vs1 = show (numerator (val * 10^e10))
      l1 = max (length vs1) (1 + e10)
      vs2 = take l1 (reverse vs1 ++ cycle "0")
      vsf = reverse (take e10 vs2)
      vsi = reverse (drop e10 vs2)
  in if r5 == 1
        then if e10 == 0 then show n else vsi ++ "." ++ vsf
        else show n ++ "/" ++ show d

divout n q = doit 0 n
  where doit e v = if rem v q == 0 then doit (e + 1) (quot v q) else (e,v)

data CCSAmt = CCSAmount Name Amount
instance Show CCSAmt where
  show (CCSAmount name amount) = joinDrop [show amount, show name]

-- Compare two CCSAmounts by name

cmpCCSAmountName (CCSAmount n1 _) (CCSAmount n2 _) = compare n1 n2
eqCCSAmountName (CCSAmount n1 _) (CCSAmount n2 _) = n1 == n2

data CmdOpt = COLAll
            | COLCCS
            | COLAccts
            | COLGrps
            | COLIncs
            | COLExps
  deriving (Eq)

instance Show CmdOpt where
  show COLAll = "all"
  show COLCCS = "ccs"
  show COLAccts = "accounts"
  show COLGrps = "groups"
  show COLIncs = "incomes"
  show COLExps = "expenses"

data Command = ListDataCmd CmdOpt
             | ToDoCmd Date
             | BalanceCmd Name Date
             | BasisCmd Name Date
             | RegisterCmd Name Date Date
             | ReconcileCmd Name Date
             | PriceCmd Name Date
             | ChangeCmd Bool Name Date Date

instance Show Command where
  show (ListDataCmd opt) = joinDrop ["list", show opt]
  show (ToDoCmd date) = joinDrop ["todo", show date]
  show (PriceCmd name date) = joinDrop ["price", show name, show date]
  show (BalanceCmd name date) = joinDrop ["balance", show name, show date]
  show (BasisCmd name date) = joinDrop ["basis", show name, show date]
  show (RegisterCmd name date1 date2) =
    joinDrop ["register", show name, show date1, show date2]
  show (ReconcileCmd name date) = joinDrop ["reconcile", show name, show date]
  show (ChangeCmd verbose name date1 date2) =
    joinDrop ["change", (if verbose then "verbose" else ""),
              show name, show date1, show date2]

-- No, not bovine spongiform encephalitis! Disambiguate buy, sell, and
-- exch records: internally, they are all treated as exch, because
-- that makes the code simple, but the user can enter "buy" or "sell",
-- and it seems only polite to echo that back on output.

data BSE = B | S | E deriving (Eq)

instance Show BSE where
  show B = "buy"
  show S = "sell"
  show E = "exch"

data Record = CCSRec Name String
            | IncomeRec Name String
            | ExpenseRec Name String
            | AccountRec Name Date String
            | GroupRec Name [Name]
            | PriceRec Date Bool CCSAmt CCSAmt
            | XferRec Date Bool Name Name CCSAmt String Int
            | ExchRec BSE Date Bool Name CCSAmt CCSAmt String
            | SplitRec Date Name Amount Amount
            | CommentRec String
            | ToDoRec Date Bool String
            | ErrorRec String

instance Show Record where
  show = showR

-- We use "show str" here to add quotes and escape any escapables

optStr str = if str == "" then "" else show str

shRec rec = if rec then "*" else ""

joinDrop ss = intercalate " " (filter (\s -> s /= "") ss)

showR (CCSRec n d) = joinDrop ["ccs", show n, optStr d]
showR (IncomeRec n d) = joinDrop ["income", show n, optStr d]
showR (ExpenseRec n d) = joinDrop ["expense", show n, optStr d]
showR (AccountRec n da de) = joinDrop ["account", show n, show da, optStr de]
showR (GroupRec n as) = joinDrop (["group", show n] ++ map show as)
showR (PriceRec d imp c1 c2) =
  joinDrop ["price", shRec imp, show d, show c1, show c2]

showR (XferRec d r a1 a2 c memo chknum) =
  joinDrop ["xfer", shRec r, show d, show a1, show a2,
            show c, optStr memo, (if chknum == 0 then "" else show chknum)]

showR (ExchRec t d r a c1 c2 memo) =
  let hs = if t == S then [show c2, show c1] else [show c1, show c2]
  in joinDrop ([show t, shRec r, show d, show a] ++ hs ++ [optStr memo])

showR (SplitRec d n a1 a2) =
  joinDrop ["split", show d, show n, show a1, show a2]

showR (CommentRec c) = if c == "" then "" else joinDrop ["#", c]

showR (ToDoRec d r memo) =
  joinDrop ["todo", shRec r, show d,
            (if memo == "" then "something! but what?" else memo)]

showR (ErrorRec str) = joinDrop ["#err", str]

-- Get the date (or at any rate /some/ date) from a Record

getRecDate (AccountRec _ d _) =      d
getRecDate (PriceRec d _ _ _) =      d
getRecDate (XferRec d _ _ _ _ _ _) = d
getRecDate (ExchRec _ d _ _ _ _ _) = d
getRecDate (SplitRec d _ _ _) =      d
getRecDate (ToDoRec d _ _) =         d
getRecDate _ = start_time	-- so it works for every Record

-- Get the name (or at any rate /some/ name) from a Record

getRecName (CCSRec n _) =       n
getRecName (IncomeRec n _) =    n
getRecName (ExpenseRec n _) =   n
getRecName (AccountRec n _ _) = n
getRecName (GroupRec n _) =     n
getRecName _ = Name " nil "	-- so it works for every Record

-- Compare two Records by date

cmpRecDate v1 v2 = compare (getRecDate v1) (getRecDate v2)

-- Compare two Records by name

cmpRecName v1 v2 = compare (getRecName v1) (getRecName v2)

-- Ledger monad: essentially a parametrized bi-level version of the Logger
-- monad from RWH. The first component of the tuple is "the final result",
-- whatever that may be, the second component is a list of ordinary bits
-- of information, and the third component is a list of extraordinary bits
-- of information, aka errors.

newtype Ledger e i r = Ledger (r, [i], [e]) deriving (Show)

runLedger (Ledger a) = a
getResult (Ledger (r,_,_)) = r
getInfo   (Ledger (_,i,_)) = i
getErrs   (Ledger (_,_,e)) = e

-- Record a message

recordInfo i = Ledger ((), [i], [])

-- Record an error

recordErr e = Ledger ((), [], [e])

-- Record nothing: a placeholder for conditionals:
-- if someCond then recordErr "someErr" else recordNil

recordNil = Ledger ((), [], [])

-- Note that because we use append (++) for lists here, this is fairly
-- low-performance: it's entirely ok for this application, where we
-- toss around a few to a few thousand list entries, but for longer
-- lists it might not be suitable. <shrug>

instance Monad (Ledger e i) where
  return a = Ledger (a, [], [])
  (>>=) m k =
    let (a,li1,le1) = runLedger m
        n = k a
        (b,li2,le2) = runLedger n
    in Ledger (b, li1 ++ li2, le1 ++ le2)
  (>>) a f = a >>= \_ -> f

-- Some miscellany

-- divisible by 4 except if divisible by 100 except if divisible by 400
isLeap y = rem y 4 == 0 && (rem y 100 /= 0 || rem y 400 == 0)

validDate (Date y m d) =
  let lim = [ 0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ] !! m
  in (m >= 1 && m <= 12 && d >= 1 &&
       (d <= lim || m == 2 && isLeap y && d <= 29))
