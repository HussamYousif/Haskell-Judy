module Tokenizor where

import Data.List

data Token = JInt Int | JID String | JSymbol Symbol | CALL Token [[Token]] | JString String
    deriving (Show)

type JID = String

data Param = Parameter Type Token
  deriving Show

data Stmt = BREAK | CONTINUE | DECLARE Type Token Expr | WHILE Expr Block | ASSIGN Token Expr  | IF Expr Block | RETURN Expr  | FunCallStmt Token [[Token]] | FOR Stmt Expr Stmt Block
  deriving (Show)

type Block = [Stmt]

data Expr = JExpr [Token]
  deriving (Show)

data Symbol = ADD
            | SUB
            | DIV
            | MUL
            | SEMICOLON
            | END
            | DO
            | LPARAN
            | RPARAN
            | COMMA
            | COLON
            | SMALLER
            | LARGER
            deriving (Show)

data Type = INTEGER | SHORT | CHAR | LONG | FLOAT | DOUBLE
    deriving (Show)

data Member = Import Token | Start Block | Event Token Expr Block | Proc Token Type Block [Param] | Field Type Token Expr

    deriving (Show)

data Component = Prog [Member]
    deriving Show


-- Component
parseComponent :: String -> Component
parseComponent [] = error "empty component"
parseComponent s | componentKeyword `isPrefixOf` s =  Prog $fst (parseComponentBody s'  [])
    where s' = removePrefix componentKeyword s
parseComponent a = error $ "unexpected input " ++ a


parseComponentBody :: String -> [Member] -> ([Member], String)
parseComponentBody s m
  | endKeyword `isPrefixOf` s = (m , [])
  | importKeyword  `isPrefixOf` s = let (m', s') = parseImport s in parseComponentBody s' (m ++ [m'])
  | typeKeyword  s = let (m', s') = parseField s in parseComponentBody s' (m ++ [m'])
  | startKeyword `isPrefixOf` s = let (m', s') = parseStart s in parseComponentBody s' (m ++ [m'])
  | triggerKeyword `isPrefixOf` s = let (m',s') = parseEvent s in parseComponentBody s' (m ++ [m'])
  | isIdent (head s) = let (m', s') = parseProcedure s in parseComponentBody s' (m ++ [m'])
  | otherwise = error "unexpected input at parseComponentBody"


-- Members
parseImport :: String -> (Member, String)
parseImport s =  (Import i, s''')
    where s' = removePrefix importKeyword s
          (i, s'') = parseIdent s'
          s''' = removePrefix ";" s''

parseField :: String -> (Member, String)
parseField s = (Field t i e, s''')
    where (t, s') = parseType s
          (i, s'') = parseIdent s'
          (e, s''') = parseDec' s''

-- Start
parseStart :: String -> (Member, String)
parseStart [] = error "parseStart error: empty input."
parseStart s = (Start b, s''')
  where s' = removePrefix startKeyword s
        s'' = removeColon s'
        (b, s''') = parseBlock (s'' , [])

-- Event
  {-
Syntax:
Event (Expr); (Identifier):
    [StmtList]
End-}
parseEvent :: String -> (Member, String)
parseEvent s = (Event t e' b, s''''')
    where s' = removePrefix triggerKeyword s
          (s'', e) = parseExpr (s', []) (JSymbol SEMICOLON)
          e' = JExpr e
          (t, s''') = parseIdent s''
          s'''' = removeColon s'''
          (b, s''''') = parseBlock (s'''', [])

-- Proc
parseProcedure :: String -> (Member, String)
parseProcedure s = (Proc id ret b p, s'''''')
  where (id, s') = parseIdent s
        (p, s'') = parseParameter s' []
        s''' = removeArrow s''
        (ret, s'''') = parseType s'''
        s''''' = removeColon s''''
        (b, s'''''') = parseBlock (s''''', [] )

-- Statements

parseStmt :: String -> (Stmt, String)
parseStmt s@(x:xs)
    | breakKeyword `isPrefixOf` s = parseBreak s
    | continueKeyword `isPrefixOf` s = parseContinue s
    | returnKeyword `isPrefixOf` s = parseReturn s
    | ifKeyword `isPrefixOf` s = parseIf s
    | whileKeyword `isPrefixOf` s = parseWhile s
    | forKeyword `isPrefixOf` s = parseFor s
    | callKeyword `isPrefixOf` s = parseFunCallStmt s
    | typeKeyword s = parseDeclare s
    | isIdent x = parseAssign s
    | otherwise = error $ "Couldn't parse statement " ++ s


parseFunCallStmt :: String -> (Stmt, String)
parseFunCallStmt s = (FunCallStmt n p,s'''')
  where (n, s') = parseIdent  (removePrefix callKeyword s)
        s'' = removePrefix "(" s'
        paramnr = if emptyParam s''
                     then 0
                     else countParam s'' 0
        (p, s''') = parseParam s'' paramnr []
        s'''' = removePrefix ";" s'''


-- input: program, string keyword of statement.
parseIf :: String -> (Stmt, String)
parseIf [] = error "parseIf: Empty input."
parseIf s = (IF (JExpr e) b, s''')
  where s' =  removePrefix ifKeyword s
        (s'', e) = parseExpr (s', []) $JSymbol DO
        (b, s''') = parseBlock (s'', [])

parseWhile :: String -> (Stmt, String)
parseWhile [] = error "parseWhile: Empty input."
parseWhile s = (WHILE (JExpr e) b, s''')
    where s' = removePrefix whileKeyword s
          (s'', e) = parseExpr (s', []) $JSymbol DO
          (b, s''') = parseBlock (s'', [])

-- Starts at '('
parseParameter :: String -> [Param] -> ([Param], String)
parseParameter (')':xs) p = (p, xs)
parseParameter ('(':xs) p = parseParameter xs p
parseParameter (',':xs) p = parseParameter xs p
parseParameter s p = parseParameter s'' p'
  where (t, s') = parseType s
        (i, s'') = parseIdent s'
        p' = p ++ [Parameter t i]

parseFor :: String -> (Stmt, String)
parseFor s = (FOR init cond incr block, s'''')
    where s' = removePrefix forKeyword s
          (init, z) =  parseStmt s'
          (z', c) = parseExpr (z, []) $ JSymbol SEMICOLON
          cond = JExpr c
          (incr, z'') = parseStmt z'
          z''' = removeDo z''
          (block, s'''' ) = parseBlock (z''',[])


parseAssign :: String -> (Stmt, String)
parseAssign s = (ASSIGN i (JExpr e), s''')
    where (i, s') = parseIdent s
          s'' = discardChar s' '=' -- Discards the '=' from the stream.
          (s''', e) =  parseExpr (s'',[]) $JSymbol SEMICOLON

parseDeclare  :: String -> (Stmt, String)
parseDeclare s = (DECLARE t i e, s''')
    where (t, s') = parseType s
          (i, s'')  = parseIdent s'
          (e, s''') = parseDec' s''

-- In case of "Type i;"
parseDec' :: String  -> (Expr, String)
parseDec' (';':xs) = (JExpr [], xs)
parseDec' ('=':xs) = (JExpr e, s')
    where (s', e) = parseExpr (xs,[]) $ JSymbol SEMICOLON


parseType :: String -> (Type, String)
parseType s
    | intKeyword `isPrefixOf` s = (INTEGER, removePrefix intKeyword s)
    | shortKeyword `isPrefixOf` s = (SHORT, removePrefix shortKeyword s)
    | charKeyword `isPrefixOf` s =  (CHAR, removePrefix charKeyword s)
    | longKeyword `isPrefixOf` s = (LONG, removePrefix longKeyword s)
    | floatKeyword `isPrefixOf` s = (FLOAT, removePrefix floatKeyword s)
    | doubleKeyword `isPrefixOf` s = (DOUBLE, removePrefix doubleKeyword s)
    | otherwise = error $"Unexpected input at parseType:  " ++ s

parseBreak :: String -> (Stmt, String)
parseBreak s = parseGeneric s breakKeyword BREAK

parseContinue :: String -> (Stmt, String)
parseContinue s = parseGeneric s continueKeyword CONTINUE

parseReturn :: String -> (Stmt,String)
parseReturn s = (RETURN (JExpr exprTree), r)
  where retRest = removePrefix returnKeyword s
        (r, exprTree) = parseExpr (retRest, []) (JSymbol SEMICOLON)

-- Generic parser for one keyword statements.
parseGeneric :: String -> String -> Stmt -> (Stmt, String)
parseGeneric str keyword statement = (statement, str'')
  where str' = removePrefix keyword str  -- x == ';'
        str'' = removePrefix ";" str'

-- Parses a list of statements until it reaches the END given.
parseBlock :: (String, Block) -> (Block, String)
parseBlock ([], _)  = error "parseBlock error: empty input"
parseBlock (s,b)
        | isEnd s = (b, snd (parseEnd s))
        | otherwise =parseBlock ( s',b ++ [b'])
                where (b', s') = parseStmt s

parseFunCall :: String -> (Token, String)
parseFunCall s = (CALL id e, s''')
    where (i, s') = parseIdent s
          id = JID (unpackJID i)
          s'' = removePrefix "(" s' -- Remove the left parenthesis.
          n = if emptyParam s''
                            then 0
                            else countParam s'' 0
          (e, s''') = parseParam s'' n []



-- Given a string and an ending symbol.
-- Tokenizes the Expr till the end symbol
parseExpr :: (String, [Token]) -> Token -> (String, [Token])
parseExpr (x:xs, r) e
  | equalSymbols e (JSymbol DO) && isDo (x:xs) = (removeDo (x:xs), r) -- Do termination.
  | isSym x && equalSymbols (parseSymbol x) e = (xs, r) -- Symbol termination.
  | x == '(' =  let (s, t) = parseExpr (xs, []) $JSymbol RPARAN -- Parentheis subexpression.
                 in parseExpr(s, r ++ [JSymbol LPARAN] ++ t ++ [JSymbol RPARAN]) e
  | isFunCall (x:xs) = let (t, s) = parseFunCall (x:xs)
                        in parseExpr (s, r ++ [t]) e
  | isDigit x = let (t, s) = tokenizeDigit (x:xs)
                 in parseExpr (s, r ++ [t]) e
  | isSym x = parseExpr (xs, r ++ [parseSymbol x]) e
  | isIdent x = let (t, s) = parseIdent (x:xs)
                 in parseExpr (s, r ++ [t]) e
  | otherwise = error ("parseExpr: unexpected input: " ++ (x:xs))


parseEnd :: String -> (Symbol, String)
parseEnd s
  | isEnd s = (END,s')
  | otherwise = error $"parseEnd error: Unexpected token" ++s
  where s' = removePrefix endKeyword s


parseSymbol :: Char -> Token
parseSymbol '+' = JSymbol ADD
parseSymbol '-' = JSymbol SUB
parseSymbol '/' = JSymbol DIV
parseSymbol '*' = JSymbol MUL
parseSymbol ';' = JSymbol SEMICOLON
parseSymbol '(' = JSymbol LPARAN
parseSymbol ')' = JSymbol RPARAN
parseSymbol ',' = JSymbol COMMA
parseSymbol ':' = JSymbol COLON
parseSymbol '<' = JSymbol SMALLER
parseSymbol '>' = JSymbol LARGER


equalSymbols :: Token -> Token -> Bool
equalSymbols (JSymbol SEMICOLON) (JSymbol SEMICOLON) = True
equalSymbols (JSymbol DO) (JSymbol DO) = True
equalSymbols (JSymbol RPARAN) (JSymbol RPARAN) = True
equalSymbols (JSymbol COMMA) (JSymbol COMMA) = True
equalSymbols (JSymbol COLON) (JSymbol COLON) = True
equalSymbols _ _ = False

parseIdent :: String -> (Token, String)
parseIdent l = (JID s, s'')
  where (s,s'') = collectString l

tokenizeDigit :: String -> (Token, String )
tokenizeDigit l = (JInt (read s :: Int), r)
  where (s,r) = collectDigit l

-- Helper Functions

-- If the start of the string is an id collects the id.
-- If the start of the string is a digit, collects the digit.
-- Returns the collected string and the rest of the string.
collectString :: String -> (String, String)
collectString [] = ([], [])
collectString (x:xs)
  -- | x == '=' = ([], xs)
  | isDigit x = collectDigit (x:xs)
  | isIdent x = collectIdent (x:xs)
  | otherwise = error $ "collectString error: Unexpected input ." ++ (x:xs)

collectIdent :: String -> (String, String)
collectIdent [] = ([], [])
collectIdent (x:xs) = (identifier, rest)
  where identifier = takeWhile isIdent (x:xs)
        rest = dropWhile isIdent (x:xs)

-- Splits the string into
collectDigit :: String -> (String, String)
collectDigit [] = ([], [])
collectDigit (x:xs) = (num, rest)
  where num = takeWhile isDigit (x:xs)
        rest = dropWhile isDigit (x:xs)


-- Utils


-- Boolean functions that determine next string tokens.

isType :: Char -> String -> Bool
isType c list = elem c list

isProc :: Member -> Bool
isProc (Proc _ _ _ _ ) = True
isProc _ = False

isSym :: Char -> Bool
isSym a = isType a sym

isIdent :: Char -> Bool
isIdent a = isType a ident

isDigit :: Char -> Bool
isDigit a = isType a digit

isFunCall :: String -> Bool
isFunCall []= False
isFunCall (x:xs)
  | isIdent x = isFunCall xs
  | x == '(' = True
  | otherwise = False

isEnd :: String -> Bool
isEnd [] = error "isEnd error : Given an empty string. "
isEnd s
  | endKeyword `isPrefixOf` s = True
  | otherwise = False

isDo :: String -> Bool
isDo [] = error "isDo error: Given input is empty."
isDo s
  | doKeyword `isPrefixOf` s = True
  | otherwise = False



-- General utils
-- Parameter utils
parseParam :: String -> Integer -> [[Token]] -> ([[Token]], String)
parseParam s 0 e = (e,s)
parseParam s n e = parseParam s' n' e'
    where (s', t) = parseSingleParam s n
          n' = n-1
          e' = e ++ [t]

parseSingleParam :: String -> Integer -> (String, [Token])
parseSingleParam s 1 = parseExpr (s, []) $JSymbol RPARAN
parseSingleParam s n = parseExpr (s, []) $JSymbol COMMA

emptyParam :: String -> Bool
emptyParam (')':xs) = True
emptyParam _ = False

-- Returns the number of parameters.
countParam :: String -> Integer -> Integer
countParam (')':_) n = n+1
countParam ('(':xs) n = countParam (skipRPARAN xs) n
countParam (',':xs) n = countParam xs n+1
countParam (x:xs) n = countParam xs n

-- Remove all whitespace.
trim :: String -> String
trim s =  [x | x <- s, x /= ' ', x/= '\n', x/= '\r']

removeDo :: String -> String
removeDo s
  | doKeyword `isPrefixOf` s = removePrefix doKeyword s
  | otherwise = error "removeDo: Unexpected input. "


removeColon :: String -> String
removeColon (':':xs) = xs
removeColon _ = error "removeColon: unexpected input"

-- Discard character
discardChar :: String -> Char -> String
discardChar (x:xs) c
  |  x == c = xs
  | otherwise = error $ "discardChar error: Unequal character and head of string " ++ [c] ++ " and " ++ (x:xs)

removeArrow :: String -> String
removeArrow s
  | "->" `isPrefixOf` s = removePrefix "->" s
  | otherwise = error "expected an arrow token, recieved unexpected token."

removeEvent :: String -> String
removeEvent s | eventKeyword `isPrefixOf` s = removePrefix eventKeyword s
removeEvent _ = error "removeEvent error: unexpected input"

removePrefix :: String -> String -> String
removePrefix [] s = s
removePrefix (y:ys) (x:xs)
  | y == x = removePrefix ys xs
  | otherwise = error ("removePrefix error: Unexpected input " ++ [y] ++ " and " ++ [x])

typeKeyword :: String -> Bool
typeKeyword s = elem True $map (\x -> x `isPrefixOf` s) typeList

skipRPARAN :: String -> String
skipRPARAN [] = error "skipRParan error: empty input"
skipRPARAN (')':xs) = xs
skipRPARAN (x:xs) = skipRPARAN xs


unpackJID :: Token -> String
unpackJID (JID s) = s
unpackJID _ = error "unpackJID error: Unexpected input"

-- Keywords, symbols etc.
sym = ['+','-','/','*',';', '(', ')', ',', ':', '>','<']
digit = "0123456789."
ident = "qwertyuiopåasdfghjkløæzxcvbnmQWERTYUIOPÅÆØLKJHGFDSAZXCVBNM_"
keywords = ["For", "Do", "End", "While", "Continue", "If", "Return",
            "Break", "Print", "Event", "Trigger", "Procedure","Start", "Import", "Component"]

-- Members and structure
eventKeyword = "Event"
triggerKeyword = "Trigger:"
procedureKeyword = "Procedure"
startKeyword = "Start"
importKeyword = "Import"
componentKeyword = "Component"

-- Statements
breakKeyword = "Break"
continueKeyword = "Continue"
forKeyword = "For"
doKeyword = "Do"
endKeyword = "End"
whileKeyword = "While"
ifKeyword = "If"
returnKeyword = "Return"
callKeyword = "Call"

-- Type keywords
intKeyword = "Int"
shortKeyword = "Short"
charKeyword = "Char"
longKeyword = "Long"
floatKeyword = "Float"
doubleKeyword = "Double"

typeList = [intKeyword, shortKeyword, charKeyword, longKeyword, floatKeyword, doubleKeyword]
