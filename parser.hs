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
                     , P.identStart     = letter <|> char '_'
                     , P.identLetter    = alphaNum <|> oneOf "_'"
                     , P.opStart        = P.opLetter emptyDef
                     , P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
                     , P.reservedOpNames= []
                     , P.reservedNames  = ["add", "sub", "neg", "eq", "gt", "lt", "and", "or", "not", 
                                          "argument", "local", "static", "constant", "this", "that",
                                          "pointer", "temp"]
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
reserved    = P.reserved lexer
reservedOp  = P.reservedOp lexer
operator    = P.operator lexer
integer     = P.integer lexer

parseProgram :: Parser Procedure
parseProgram = do
  whiteSpace 
  c <- many1 (pArithmetic <|> pMemCommand)
  return c

pArithmetic :: Parser Command
pArithmetic = do
  a <- pAdd <|> pAnd <|> pNeg <|> pNot <|> pSub <|> pEq <|> pGt <|> pLt <|> pOr 
  return $ Arithmetic' a

pMemCommand :: Parser Command
pMemCommand = pPush <|> pPop

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
pAdd = reserved "add" >> return Add
pAnd = reserved "and" >> return And
pNeg = reserved "neg" >> return Neg
pNot = reserved "not" >> return Not
pSub = reserved "sub" >> return Sub
pEq = reserved "eq" >> return Eq
pGt = reserved "gt" >> return Gt
pLt = reserved "lt" >> return Lt
pOr = reserved "or" >> return Or

