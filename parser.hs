module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import qualified Text.Parsec.Token as P
import Types

langdef :: P.LanguageDef a
langdef = emptyDef { P.commentStart   = "/*"
                     , P.commentEnd     = "*/"
                     , P.commentLine    = "//"
                     , P.nestedComments = True
                     , P.identStart     = letter <|> char '_' <|> char '.' <|> char ':'
                     , P.identLetter    = alphaNum <|> char '_' <|> char '.' <|> char ':'
                     , P.opStart        = P.opLetter emptyDef
                     , P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
                     , P.reservedOpNames= []
                     , P.reservedNames  = ["add", "sub", "neg", "eq", "gt", "lt", "and", "or", "not", 
                                          "argument", "local", "static", "constant", "this", "that",
                                          "pointer", "temp", "label", "goto", "if-goto", "function", "call", "return"]
                     , P.caseSensitive  = True}

lexer :: P.TokenParser a
lexer  = P.makeTokenParser langdef

whiteSpace  = P.whiteSpace lexer
lexeme      = P.lexeme lexer
symbol      = P.symbol lexer
natural     = P.natural lexer
parens      = P.parens lexer
semi        = P.semi lexer
identifier  = P.identifier lexer
reserved    = try . P.reserved lexer
reservedOp  = P.reservedOp lexer
operator    = P.operator lexer
integer     = P.integer lexer

parseProgram :: Parser Procedure
parseProgram = do
  whiteSpace 
  c <- many1 (pFlow <|> pArithmetic <|> pMemCommand <|> pFCall)
  return c

-- Arithmetic

pArithmetic :: Parser Command
pArithmetic = do
  a <- pAdd <|> pAnd <|> pNeg <|> pNot <|> pSub <|> pEq <|> pGt <|> pLt <|> pOr 
  return $ Arithmetic' a

pAdd = reserved "add" >> return Add
pAnd = reserved "and" >> return And'
pNeg = reserved "neg" >> return Neg
pNot = reserved "not" >> return Not'
pSub = reserved "sub" >> return Sub
pEq = reserved "eq" >> return Eq
pGt = reserved "gt" >> return Gt
pLt = reserved "lt" >> return Lt
pOr = reserved "or" >> return Or'

-- Mem Access

pMemCommand :: Parser Command
pMemCommand =  pPush <|> pPop

pPush :: Parser Command
pPush = do
  reserved "push"
  ml <- pMemLoc
  return $ MemAccess' $ Push ml

pPop :: Parser Command
pPop = do
  reserved "pop"
  ml <- pMemLoc
  return $ MemAccess' $ Pop ml
--argument
pMemLoc :: Parser MemLoc
pMemLoc = do
  seg <- pArg <|> pLocal <|> pStatic <|> pConstant <|> pThis <|> pThat <|> pPoint <|> pTemp
  i <- integer
  return $ MemLoc seg i

pArg = reserved "argument" >> return Argument
pLocal = reserved "local" >> return Local
pStatic = reserved "static" >> return Static
pConstant = reserved "constant" >> return Constant
pThis = reserved "this" >> return This
pThat = reserved "that" >> return That
pPoint = reserved "pointer" >> return Pointer
pTemp = reserved "temp" >> return Temp

-- Flow


pFlow :: Parser Command
pFlow = pLabel <|> pGoto <|> pIfGoto

pIfGoto :: Parser Command
pIfGoto  = do
  reserved "if-goto"
  i <- identifier
  return $ Flow' $ IfGoto i

pGoto :: Parser Command
pGoto  = do
  reserved "goto"
  i <- identifier
  return $ Flow' $ Goto i

pLabel :: Parser Command
pLabel  = do
  reserved "label"
  i <- identifier
  return $ Flow' $ Label i

pFCall :: Parser Command
pFCall = pFunction <|> pCall <|> pReturn

pFunction :: Parser Command
pFunction = do
  reserved "function"
  iden <- identifier
  num <- integer
  return $ FCall' $ Function iden num

pCall :: Parser Command
pCall = do
  reserved "call"
  iden <- identifier
  num <- integer
  return $ FCall' $ Call iden num

pReturn :: Parser Command
pReturn = reserved "return" >> return (FCall' Return)
