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

$Id: UMM.hs,v 1.38 2009/11/18 22:30:35 uwe Exp $ -}

module Main where
import Prelude
import Control.Monad
import Data.List
import Data.Maybe
import Data.Ratio
import System
import System.Exit
import System.IO
import System.Time

import UMMHelp
import UMMData
import UMMParser
import UMMEval

processArgs =
  do prog <- getProgName
     args <- getArgs
     now <- getClockTime >>= toCalendarTime
     let argc = length args
         fname = head args
         nd = genDate now
         cmd = if argc < 2
                  then BalanceCmd (Name "") nd
                  else parseUCommand nd (intercalate " " (tail args))
     writeHdr
     if argc < 1 || fname == "-h" || fname == "-help" || fname == "--help"
        then putStr (usageMsg prog) >> exitWith ExitSuccess
        else return (fname, cmd)

-- A version of 'lines' which deals with all combinations of '\r' and '\n'.
-- It drops blank lines, which is perfectly fine here, but which might be
-- a problem elsewhere.

mylines str = filter (/= "") (ml str)
  where ml [] = []
        ml s  = let (p, s1) = break (\c -> c == '\n' || c == '\r') s
                in p : (if null s1 then [] else ml (tail s1))

getLines fp =
  (if fp == "-" then getContents else readFile fp) >>= (return . mylines)

-- Merge explicit (ps) and implicit (qs) prices: the inputs are sorted
-- by date, newest first, and the output is the same, preferring
-- explicit over implicit prices in case of a date match.

mergePrices [] qs = qs
mergePrices ps [] = ps
mergePrices pa@(p:ps) qa@(q:qs) =
  if cmpRecDate p q == LT
     then q : mergePrices pa qs
     else p : mergePrices ps qa

-- Find an equivalent price in the default units for a given CCS,
-- if a suitable one can be found. If none can be found, or if the
-- ccs is already in the default units, return Nothing.

equivPrice (CCSAmount n (Amount a)) dc date ps =
  if n == dc then Nothing else xlat ps
  where xlat [] = Nothing
        xlat ((PriceRec dr _ (CCSAmount nr1 (Amount ar1))
                             (CCSAmount nr2 (Amount ar2))):ps) =
          if compare date dr /= LT && n == nr1 && nr2 == dc
             then Just ((CCSAmount dc (Amount (a*ar2/ar1))), dr)
             else xlat ps
        xlat (_:ps) = xlat ps		-- for non-PriceRec records

-- Translate price if possible

reprice ccs dc date prices =
  let ep = equivPrice ccs dc date prices
  in if isJust ep then fst (fromJust ep) else ccs

-- Pretty-print accounts

ppAccts es sp =
  concat (map (ppe (sp + maximum (map (length . getN . fst) es))) es)
  where getN (Name n) = n
        ppe l (m,as) =
          let isp = concat (repeat " ")
          in zipWith gl ((take l (show m ++ isp)) : repeat (take l isp)) as
        gl a b = concat [a, b]

showPos dc da ps as = map f1 as
  where f1 (n1,es) = (n1, if null es then ["[empty]"] else map f2 es)
        f2 c2 =
          let sv = show c2
              ep = equivPrice c2 dc da ps
              jep = fromJust ep
              CCSAmount n (Amount a) = fst jep
              jer = CCSAmount n (Amount (roundP 2 a))
              sp = "\t~" ++ show jer ++ " (" ++ show (snd jep) ++ ")"
              pad = " " ++ concat (take (18 - length sv) (repeat " "))
          in if isJust ep then sv ++ pad ++ sp else sv

selAccts names accs =
  (if length names == 1 && head names == noName
      then id
      else filter (\a -> elem (fst a) names)) accs

-- Round a rational number val to np decimal places
-- This might not be ultimately exact, but it's for converted prices
-- which might be a couple of days out of date anyway, so no big deal

roundP np val =
  if val < 0 then negate (rp np (negate val)) else rp np val
  where rp np val =
          let e1 = 10^(max 0 np)
              e2 = 10*e1
              vs = (fromInteger e2)*val
              n = numerator vs
              d = denominator vs
              q1 = quot n d
              q2 = quot (q1 + 4) 10
          in q2 % e1

-- Turn an account-group into a list of accounts. An account-group can
-- contain (names of) other account-groups, including recursively, and
-- loops aren't prohibited either... need to handle all those cases.

expandGroup as gs ag =
  if ag == noName || ag == todoName
     then [ag]
     else ff [ag] (map getRecName as) (map ggt gs) [] []
  where ff [] _ _ am _ = am
        ff (q:qs) as gs am ab
          | elem q am || elem q ab  = ff qs as gs am ab
          | elem q as               = ff qs as gs (q:am) ab
          | isJust (lookup q gs)    = ff (qs ++ fromJust(lookup q gs))
                                         as gs am (q:ab)
          | otherwise               = error ("unknown account or group! "
                                              ++ show q)
        ggt (GroupRec n as) = (n,as)

doList w dc ccs accts grps incs exps =
  do when (chk w COLCCS)
       (putStrLn "# Currencies, Commodities, Securities\n" >>
        putStrLn ("# default ccs " ++ (show dc) ++ "\n") >> sh ccs)
     when (chk w COLAccts) (putStrLn "\n# Accounts\n"    >> sh accts)
     when (chk w COLGrps)  (putStrLn "\n# Groups\n"      >> sh grps)
     when (chk w COLIncs)  (putStrLn "\n# Incomes\n"     >> sh incs)
     when (chk w COLExps)  (putStrLn "\n# Expenses\n"    >> sh exps)
  where chk w1 w2 = w1 == w2 || w1 == COLAll
        sh = mapM_ (putStrLn . show)

doBalance date names dc accts trans prices =
  do final <- getBalances start_time date Nothing False accts trans
     let fsel = selAccts names final
         fp = map (\e -> reprice e dc date prices) (concat (map snd fsel))
         gp = groupBy eqCCSAmountName (sortBy cmpCCSAmountName fp)
         sp = filter (\e -> ccsA e /= 0) (map sumCCS gp)
     if length names == 1 && head names == todoName
        then putStr ""
        else putStrLn ("Account balances as of " ++ show date) >>
             mapM_ putStrLn (ppAccts (showPos dc date prices fsel) 8) >>
             putStrLn ("Grand total: ~" ++ show sp)
  where sumCCS cs =
           CCSAmount (ccsN (head cs)) (Amount (roundP 2 (sum (map ccsA cs))))
        ccsN (CCSAmount n _) = n
        ccsA (CCSAmount _ (Amount a)) = a

doRegister d1 d2 name dc accts trans prices dorec =
  do final <- getBalances d1 d2 (Just name) dorec accts trans
     putStrLn ((if dorec then "Reconciled" else "Account")
               ++ " balance as of " ++ show d2)
     mapM_ putStrLn (ppAccts (showPos dc d2 prices (selAccts [name] final)) 8)

doChange verbose d1 d2 name dc accts trans =
  do let aux = if notElem name (map getRecName accts)
                  then [AccountRec name d1 ""]
                  else []
         trs = dropWhile (\t -> compare (getRecDate t) d1 /= GT) trans
         mn = if verbose then Just name else Nothing
     final <- getBalances d1 d2 mn False (aux ++ accts) trs
     putStr "Change"
     when (d1 /= start_time) (putStr (" from " ++ show d1))
     putStrLn (" to " ++ show d2)
     mapM_ putStrLn (ppAccts (showPos dc d2 [] (selAccts [name] final)) 8)

main =
  do (file, action) <- processArgs
     recs <- getLines file >>= mapM (return . parseURecord) >>= validateRecs
     let (dc, ccs, incs, exps, accts, grps, trans, p1) = classifyRecs recs
     validateTransPrices ccs incs exps accts (trans ++ p1)
     let p2 = generateImplicitPrices dc trans
         prices = mergePrices (reverse p1) p2
     case action of
       ListDataCmd w ->
         doList w dc ccs accts grps incs exps
       BalanceCmd name date ->
         doBalance date (expandGroup accts grps name) dc accts trans prices
       BasisCmd name date ->
         putStrLn "Sorry, basis command is not implemented yet!"
       ToDoCmd date ->
         doBalance date [todoName] dc accts trans prices
       RegisterCmd name date1 date2 ->
         doRegister date1 date2 name dc accts trans prices False
       ReconcileCmd name date ->
         doRegister start_time date name dc accts trans prices True
       ChangeCmd verbose name date1 date2 ->
         doChange verbose date1 date2 name dc accts trans
       PriceCmd name date ->
         if name == dc
            then putStrLn ("Price of " ++ show name ++ " is always 1!")
            else if notElem name (map getRecName ccs)
                    then putStrLn ("Error! unknown CCS " ++ show name)
                    else getPrices name dc date prices
