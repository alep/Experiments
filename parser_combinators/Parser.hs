module Parser where

type Token = String

-- Lexer -----------------------------------------------------------------

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c == ' ' || c == '\t' || c == '\n'

isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

isAlpha :: Char -> Bool
isAlpha c = (('A' <= c && c <= 'Z') || 
             ('a' <= c && c <= 'z'))

isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || c == '_'

clex :: String -> [Token]
clex [] = []
clex (c:cs) | isWhiteSpace c = clex cs
            | isDigit c = let num_token = c : takeWhile isDigit cs
                              rest_cs = dropWhile isDigit cs
                          in 
                            num_token : clex rest_cs
            | isAlpha c = let var_tok = c : takeWhile isIdChar cs
                              rest_cs = dropWhile isIdChar cs
                          in
                            var_tok : clex rest_cs
            | otherwise = [c] : clex cs

-- Parser --------------------------------------------------------------

type Parser a = [Token] -> [(a, [Token])]

pSat :: (String -> Bool) -> Parser String
pSat predicate (tok:toks) | predicate tok = [(tok, toks)]
                          | otherwise = []
pSat predicate [] = []

-- pLit :: String -> Parser String
-- pLit s (tok:toks) | s == tok = [(s, toks)]
--                   | otherwise = []
-- pLit s [] = []

pLit s = pSat (== s)

-- pVar :: Parser String
-- pVar [] = []
-- pVar (tok:toks) | ('A' <= h) && (h <= 'Z') = [(tok, toks)]
--                 | otherwise = []
--                 where h = head tok

pVar :: Parser String
pVar = pSat (\tok -> let h = head tok in ('A' <= h) && (h <= 'Z'))

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)

-- Test for pAlt ----------
pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")


pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks = [(combine val1 val2, toks2) 
                                | (val1, toks1) <- p1 toks,
                                  (val2, toks2) <- p2 toks1]


pThen3 :: (a -> b -> c -> d) -> Parser a ->
          Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 toks = 
    [(combine v1 v2 v3, toks3)
         | (v1, toks1) <- p1 toks,
           (v2, toks2) <- p2 toks1,
           (v3, toks3) <- p3 toks2]
                                 

-- Test for pThen ---------
pGreetings :: Parser (String, String)
pGreetings = pThen keep_first 
             (pThen mk_pair pHelloOrGoodbye pVar)
             (pLit "!")
             where 
               keep_first x y = x
               mk_pair x y = (x, y)
                                

pGreetings2 :: Parser (String, String)
pGreetings2 = pThen3 keep_two
              pHelloOrGoodbye
              pVar
              (pLit "!")
              where keep_two x y z = (x, y)

pEmpty :: a -> Parser a
pEmpty x toks = [(x, toks)]

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen combine p (pZeroOrMore p)
    where combine x xs = x:xs

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = (pOneOrMore p) `pAlt` (pEmpty [])


-- Test pZeroOrMore
pGreetingsN :: Parser Int
pGreetingsN = (pZeroOrMore pGreetings) `pApply` length

pApply :: Parser a -> (a -> b) -> Parser b
pApply p fun toks = [ (fun val, toks1) | (val, toks1) <- p toks]

pOneOrMoreWithSep :: Parser b -> Parser a -> Parser [a]
pOneOrMoreWithSep p2 p1 = -- p2 is the parser which parses the separator
    pThen3 combine3 
           p2 p1 (pZeroOrMore (pThen combine2 p2 p1))
        where combine2 y x = x
              combine3 y x z = x : z

-- Test
pSepGreets :: Parser [(String, String)]
pSepGreets = pThen combine pGreetings (pOneOrMoreWithSep (pLit ";") pGreetings)
             where combine x xs = x:xs


-- My Stuff ---
-- Grammar ---
type Name = String

data CoreExpr  = ENum Int | EVar Name | EAp CoreExpr CoreExpr deriving (Show, Read)

-- type CoreExpr = Expr Name

pParens :: Parser b -> Parser b 
pParens p = pThen3 combine (pLit "(") p (pLit ")")
    where
      combine x y z = y -- Where x = "(" and z = ")"  

pNum :: Parser CoreExpr
pNum = (pSat (\tok -> (all (\x -> ('0' <= x) && (x <= '9')) tok))) `pApply` (\x -> ENum (read x)) 

aexpr :: Parser CoreExpr
aexpr = pNum `pAlt` (pParens pExpr4) 

data PartialExpr = NoOp | FoundOp Name CoreExpr

assembleOp :: CoreExpr -> PartialExpr -> CoreExpr
assembleOp e1 NoOp = e1
assembleOp e1 (FoundOp op e2) = EAp (EAp (EVar op) e1) e2

pExpr4 :: Parser CoreExpr
pExpr4 = pThen combine pExpr5 pExpr4c
         where combine = assembleOp

pExpr4c :: Parser PartialExpr
pExpr4c = (pThen combine (pLit "+") pExpr4) 
         `pAlt` (pThen combine (pLit "-") pExpr5)
         `pAlt` (pEmpty NoOp)
         where combine = FoundOp

pExpr5 :: Parser CoreExpr
pExpr5 = pThen combine pExpr6 pExpr5c
         where combine = assembleOp

pExpr5c :: Parser PartialExpr
pExpr5c = (pThen combine (pLit "*") pExpr5)
         `pAlt` (pThen combine (pLit "/") pExpr6)
         `pAlt` (pEmpty NoOp)
         where combine = FoundOp

mk_ap_chain :: [CoreExpr] -> CoreExpr
mk_ap_chain [x] = x
mk_ap_chain (x:xs) = EAp (mk_ap_chain xs) x

pExpr6 = (pOneOrMore aexpr) `pApply` mk_ap_chain

