module Main where

import Tokenizor
import Data.List
import System.Environment

main :: IO ()
main = do
  putStrLn "Welcome to Judy"
  putStrLn "Enter the name of the file you want to serialize:"
  file <- getLine
  putStrLn $ "The file is " ++ file
  readFile file >>=writeFile (outFileName file) .  serializeComponent . parseComponent . trim
  readFile file >>= putStrLn . serializeComponent . parseComponent . trim
  putStrLn $ "Written to " ++ (outFileName file)



outFileName :: String -> String
outFileName ('.':'j':'d':'y':[])= ".cpp"
outFileName (x:xs) = [x] ++ outFileName xs

-- Serialization from here
serializeComponent :: Component -> String
serializeComponent (Prog p) = "// Component" ++ newline
                                ++ serializeComponentBody p


serializeComponentBody :: [Member] -> String
serializeComponentBody m = serializeImports m ++ serializeFields m ++ serializeMain m ++  unpackConcat  (map  serializeProc (filter isProc m))


serializeImports :: [Member] -> String
serializeImports m = concatMap (\x -> "#include " ++ "<" ++ serializeToken x ++ ">" ++ ";" ++ newline) m'
    where  m' = map unpackImport (filter isImport m)

serializeFields :: [Member] -> String
serializeFields m = concat (map serializeField (filter isField m))

serializeField :: Member -> String
serializeField (Field t n e) = serializeType t ++ " " ++ serializeToken n
    ++
        if emptyExpr e then ";" ++ newline
                       else "=" ++ serializeExpr e ++ ";" ++ newline

serializeMain :: [Member] -> String
serializeMain m = let slist = filter isStart m in
                    if length slist /= 1
                       then error "Unexpected number of start functions"
                        else mainSig ++  serializeStart (head slist) ++ events ++ mainEnd
                          where events = unpackConcat $ map serializeEvent (filter isEvent m)

serializeEvent :: Member -> String
serializeEvent m = serializeStmt  (eventToIf m)

eventToIf :: Member -> Stmt
eventToIf (Event t e b) = IF e b
eventToIf _ = error "Unexpected input to eventToIf"

serializeStart :: Member -> String
serializeStart (Start b) = serializeBlock b
serializeStart _ = error "serializeStart error: Unexpected input"

serializeProc :: Member -> String
serializeProc (Proc n t b p) = serializeType t ++ " " ++ serializeToken n ++ "(" ++
        serializeParam p ++ ")" ++ " {"
        ++ newline ++ unpackConcat (map serializeStmt b) ++ newline ++
            "}" ++ newline

serializeBlock :: Block -> String
serializeBlock b = unpackConcat $ map serializeStmt b

serializeParam :: [Param] -> String
serializeParam ((Parameter t i ):[]) = serializeType t ++ " " ++ serializeToken i
serializeParam ((Parameter t i):xs) = serializeType t ++
                            " " ++ serializeToken i
                            ++ "," ++ serializeParam xs


serializeStmt :: Stmt -> String
serializeStmt BREAK = "break;" ++ newline;
serializeStmt CONTINUE = "continue;" ++ newline;
serializeStmt dec@(DECLARE t n e) = serializeDeclare dec
serializeStmt (WHILE e b) = "while" ++ "(" ++ serializeExpr e ++ ")" ++ "{ " ++
                                serializeBlock b ++ " }"
serializeStmt (ASSIGN n e) = serializeToken n ++ " = " ++ serializeExpr e ++ ";" ++ newline
serializeStmt (IF e b) = "if" ++ "(" ++ serializeExpr e ++  ")" ++ " {" ++ newline ++ serializeBlock b ++ "}" ++ newline
serializeStmt (RETURN e) = "return " ++ serializeExpr e ++ " ;" ++ newline
serializeStmt (FOR init cond incr block) = "for(" ++ serializeStmt init ++ serializeExpr cond ++";"++ serializeStmt incr ++ ") {" ++ newline ++
     serializeBlock block ++ newline ++ "}" ++ newline
serializeStmt (FunCallStmt n p) = serializeToken n ++ "(" ++ (intercalate "," $ mapMap p) ++ ");" ++ newline


serializeDeclare :: Stmt -> String
serializeDeclare (DECLARE t n e) =  serializeType t ++ " " ++ serializeToken n ++
    if emptyExpr e then "" ++ ";" ++ newline
                   else "=" ++ serializeExpr e ++ ";" ++ newline


serializeType :: Type ->  String
serializeType INTEGER = "int"
serializeType SHORT = "short"
serializeType CHAR = "char"
serializeType LONG = "long"
serializeType FLOAT = "float"
serializeType DOUBLE = "double"


serializeExpr :: Expr -> String
serializeExpr (JExpr e) = unpackConcat $map serializeToken e

serializeToken :: Token -> String
serializeToken (JInt i) = show i
serializeToken (JID s) =  s
serializeToken (JSymbol s) = serializeSymbol s
serializeToken (CALL n p) = serializeToken n ++ "(" ++ p' ++ ")"
    where p' =   intercalate "," $ mapMap p



serializeSymbol :: Symbol -> String
serializeSymbol ADD = "+"
serializeSymbol SUB = "-"
serializeSymbol DIV = "/"
serializeSymbol MUL = "*"
serializeSymbol SEMICOLON = ";"
serializeSymbol END = "End"
serializeSymbol DO = "Do"
serializeSymbol LPARAN = "("
serializeSymbol RPARAN = ")"
serializeSymbol COMMA = ","
serializeSymbol SMALLER = "<"
serializeSymbol LARGER = ">"
serializeSymbol a = error ("unexpected input at serializeSymbol" ++ show a)

-- Unpacks string then concats.
unpackConcat :: [String] -> String
unpackConcat l = foldl (++) "" l

isStart :: Member -> Bool
isStart Start{}  = True
isStart _ = False


isEvent :: Member -> Bool
isEvent Event{} = True
isEvent _ = False

isImport :: Member -> Bool
isImport (Import _) = True
isImport _ = False

isField :: Member -> Bool
isField (Field _ _ _) = True
isField _ = False

unpackImport :: Member -> Token
unpackImport (Import i) = i

mapMap :: [[Token]] -> [String]
mapMap [] = []
mapMap (x:xs) = concatMap serializeToken x : mapMap xs


emptyExpr :: Expr -> Bool
emptyExpr (JExpr []) = True
emptyExpr (JExpr _ ) = False


-- constant strings
newline = "\n"

mainSig = "int main(){" ++ newline

mainEnd = "return 1" ++ newline ++ "}" ++ newline
