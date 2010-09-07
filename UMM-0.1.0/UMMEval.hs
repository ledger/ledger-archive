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

$Id: UMMEval.hs,v 1.23 2009/11/18 22:30:36 uwe Exp $ -}

module UMMEval (validateRecs, classifyRecs, validateTransPrices,
                generateImplicitPrices, getBalances, getPrices) where
import Prelude
import Data.List
import Data.Maybe
import System.IO
import Control.Monad

import UMMData

-- Initial validation of records: filter out comments, highlight parse
-- errors, and check that dates are valid. This is kind of a trivial
-- use of runLedger, it could almost be accomplished by partition.

validateRecs records =
  do let (_,i,e) = runLedger (chk records)
     if length e == 0
        then return i
        else hPutStrLn stderr "There were parse or date errors in the input:"
             >> mapM_ (hPutStrLn stderr . show) e
             >> error "quitting now"
  where chk [] = return ()
        chk (r@(ErrorRec _):rs) = recordErr r >> chk rs
        chk ((CommentRec _):rs) = chk rs
        chk (r:rs) =
            (if validDate (getRecDate r) then recordInfo else recordErr) r
            >> chk rs

-- Classify records by type and sort accounts and
-- transaction-type records by date or name as needed

uErr v1 v2 =
  error ("duplicate records\n  " ++ show v1 ++ "\n  " ++ show v2)

uChk [] = []
uChk s@(_:[]) = s
uChk v@(v1:v2:[]) =
  if getRecName v1 == getRecName v2 then uErr v1 v2 else v
uChk (v1:v2:vs) =
  if getRecName v1 == getRecName v2 then uErr v1 v2 else v1 : uChk (v2:vs)

-- TODO: should really check that there are no duplications among
-- account names and account group names together, rather than
-- separately (or just account groups, right now)

classifyRecs recs = cw recs [] [] [] [] [] [] []
  where cw [] c i e a g t p =
          let dc = if null c then Name "zorkmid" else getRecName (last c)
          in (dc, vsN c, vsN i, vsN e, reverse a, vsN g, asD dc t, asD dc p)
        cw (r:rs) c i e a g t p =
          case r of
            CommentRec _     -> cw rs c i e a g t p
            CCSRec _ _       -> cw rs (r:c) i e a g t p
            IncomeRec _ _    -> cw rs c (r:i) e a g t p
            ExpenseRec _ _   -> cw rs c i (r:e) a g t p
            AccountRec _ _ _ -> cw rs c i e (r:a) g t p
            GroupRec _ _     -> cw rs c i e a (r:g) t p
            PriceRec _ _ _ _ -> cw rs c i e a g t (r:p)
            _                -> cw rs c i e a g (r:t) p
        vsN rs = uChk (sortBy cmpRecName rs)
        asD dc rs = sortBy cmpRecDate (reverse (map (addDC dc) rs))
        addDC dc (PriceRec d imp ccsa1 ccsa2) =
          PriceRec d imp ccsa1 (addDCCA dc ccsa2)
        addDC dc (XferRec d f a1 a2 ccsa m c) =
          XferRec d f a1 a2 (addDCCA dc ccsa) m c
        addDC dc (ExchRec t d f acc ccsa1 ccsa2 m) =
          ExchRec t d f acc (addDCCA dc ccsa1) (addDCCA dc ccsa2) m
        addDC _ r = r
        addDCCA dc c@(CCSAmount n a) =
          if n == noName then CCSAmount dc a else c

-- Second validation of transactions: check that all from & to accounts
-- are valid, that splits aren't 1/0 or 0/1, etc

-- TODO: if we want to give a reason for each failure, run this
-- through runLedger as above? then we could generate multiple output
-- records for (some of) each input record

validateTransPrices ccs incs exps accts tps =
  do let bads = filter chk tps
     when (length bads > 0)
          (hPutStrLn stderr "There were bad transactions in the input:"
           >> mapM_ (hPutStrLn stderr . show) bads >> error "quitting now")
  where chk (SplitRec _ c (Amount amt1) (Amount amt2)) =
            amt1 == 0 || amt2 == 0 || notIn c ccs
        chk (PriceRec _ _ (CCSAmount c1 amt1) (CCSAmount c2 _)) =
            amt1 == Amount 0 || notIn c1 ccs || notIn c2 ccs
        chk (XferRec _ _  af at (CCSAmount n _) _ _) =
            (notIn af incs && notIn af accts) ||
            (notIn at exps && notIn at accts) ||
            notIn n ccs
        chk (ExchRec _ _ _ a (CCSAmount c1 _) (CCSAmount c2 _) _) =
            notIn a accts || notIn c1 ccs || notIn c2 ccs
        chk (ToDoRec _ _ _) = False
        chk _ = True
        notIn _ [] = True
        notIn s (r:rs) = s /= getRecName r && notIn s rs

-- Generate implicit price/date information from buy and sell transactions;
-- presumably, these will have taken place at market price, which is what we
-- want. Only generate info for transactions involving the default currency,
-- other stuff is too hard to untangle at least for now.

generateImplicitPrices dc trs = gip [] trs
  where gip acc [] = filter pr acc
        gip acc (t@(ExchRec _ _ _ _ _ _ _):ts) = gip ((genp t) : acc) ts
        gip acc (_:ts) = gip acc ts
        genp (ExchRec _ date _ _ (CCSAmount n1 a1) (CCSAmount n2 a2) _)
          | n1 == dc   = PriceRec date True (nC n2 a2 a2) (nC n1 a1 a2)
          | n2 == dc   = PriceRec date True (nC n1 a1 a1) (nC n2 a2 a1)
          | otherwise  = CommentRec "general exchange, no price generated"
        pr (PriceRec _ _ _ _) = True
        pr _ = False
        nC n (Amount a1) (Amount a2) =
          CCSAmount n (Amount (if a2 == 0 then a1 else a1/a2))

getCN (CCSAmount n _) = n
getCA (CCSAmount _ (Amount a)) = a

addTo [] n = [n]
addTo (q:qs) d =
  let qn = getCN q
      qq = getCA q
      dn = getCN d
      dq = getCA d
      nq = qq + dq
  in if qn == dn
        then if nq == 0
                then qs
                else (CCSAmount qn (Amount nq)) : qs
        else q : addTo qs d

subFrom acc d = addTo acc (CCSAmount (getCN d) (Amount (-(getCA d))))

scaleBy qs d = map (s1 (getCN d) (getCA d)) qs
  where s1 dn dq q =
          let qn = getCN q
              qq = getCA q
          in if qn == dn then (CCSAmount qn (Amount (qq * dq))) else q

maybeRecord reg record newaccs tst =
  let isJ = isJust reg
      rn = fromJust reg
      acc = filter (\a -> fst a == rn) newaccs
      nb = if length acc > 0
              then snd (head acc)
              else [CCSAmount noName (Amount 0)]
  in if isJ && (tst rn || rn == noName)
        then recordInfo (record, nb)
        else recordNil

maybeDo reg dorec record isrec accs newaccs tst =
  if dorec
     then if isrec
             then return newaccs
             else maybeRecord reg record newaccs tst >> return accs
     else maybeRecord reg record newaccs tst >> return newaccs

exchTrans reg dorec record@(ExchRec _ _ isrec acc amtn amto _) accs =
  maybeDo reg dorec record isrec accs
          (doExch accs acc amtn amto) (\rn -> rn == acc)
  where doExch [] _ _ _ = []
        doExch ((an,ab):as) n en eo =
          if an == n
             then (an, subFrom (addTo ab en) eo) : as
             else (an,ab) : doExch as n en eo

xferTrans reg dorec record@(XferRec _ isrec from to amt _ _) accs =
  maybeDo reg dorec record isrec accs
          (doXfer accs from to amt) (\rn -> rn == from || rn == to)
  where doXfer [] _ _ _ = []
        doXfer (a@(an,ab):as) nf nt e
          | an == nf    = (an, subFrom ab e) : doXfer as nf nt e
          | an == nt    = (an, addTo ab e) : doXfer as nf nt e
          | otherwise   = a : doXfer as nf nt e

-- TODO: look into account and see if split is applicable? yeah... cleaner
-- Also: does maybeDo apply here, too? I think probably it should...
-- but is this a reconcilable transaction? It reaches across accounts,
-- so maybe not

splitTrans reg record@(SplitRec _ ccs (Amount an) (Amount ao)) acc =
  let newaccs = map doST acc
  in maybeRecord reg record newaccs (\_ -> True) >> return newaccs
  where doST (a1,a2) = (a1, scaleBy a2 (CCSAmount ccs (Amount (an/ao))))

mkInit as = return (map (\a -> (getRecName a, [])) as)

appTr _ _ _ [] as = return as
appTr d r f (t:ts) as =
  if getRecDate t > d
     then return as
     else case t of
            XferRec _ _ _ _ _ _ _ -> xferTrans r f t as >>= appTr d r f ts
            ExchRec _ _ _ _ _ _ _ -> exchTrans r f t as >>= appTr d r f ts
            SplitRec _ _ _ _  -> splitTrans r t as >>= appTr d r f ts
            ToDoRec _ isrec _ ->
              (if isrec then recordNil else recordInfo (t,[]))
                >> appTr d r f ts as
            _ -> recordErr t >> appTr d r f ts as

showT (t,_) = putStrLn (show t)

showTB e@(_,b) = showT e >> mapM_ sB b
  where sB ccsa = putStrLn ("\t" ++ show ccsa)

getBalances date1 date2 reg dorec accts trans =
  do let (r,i1,e) = runLedger (mkInit accts >>= appTr date2 reg dorec trans)
         i = dropWhile (\t -> compare (getRecDate (fst t)) date1 == LT) i1
         ss = if dorec then showT else showTB
     when (length e > 0)
          (hPutStrLn stderr "There were parse or date errors in the input:"
           >> mapM_ (hPutStrLn stderr . show) e >> error "quitting now")
     when (length i > 0) (putStrLn "Notes:" >> mapM_ ss i >> putStrLn "")
     return r

-- For now, we don't generate "swap prices" internally, so unless the user
-- enters some, we won't see any; see also generateImplicitPrices above.

getPrices nm dc date prices =
  do let p1 = dropWhile (\t -> compare date (getRecDate t) == LT) prices
         (_,i,e) = runLedger (get p1)
     when (length e > 0) (doShow "Swap \"Prices\"" e >> putStrLn "")
     when (length i > 0) (doShow "Ordinary Prices" i)
     when (length e == 0 && length i == 0)
          (putStrLn ("No prices known for " ++ show nm))
  where get [] = return ()
        get (p@(PriceRec _ _ (CCSAmount nr1 _) (CCSAmount nr2 _)):ps) =
          if (nr1 == nm && nr2 == dc) || (nr1 == dc && nr2 == nm)
             then recordInfo p >> get ps
             else if (nr1 == nm || nr2 == nm)
                     then recordErr p >> get ps
                     else get ps
        get _ = recordNil
        doShow t p = putStrLn t >> mapM_ (putStrLn . show) (reverse p)
