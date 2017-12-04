{
module Parser (parseProp) where

import Proposition
}


%name parse
%tokentype { Token }
%error { parseError }
%monad { E } { thenE } { returnE }


%token
     'T'             { TokenT }
     'F'             { TokenF }
      var             { TokenVar $$ }
      '/\\'           { TokenAnd }
      '\\/'           { TokenOr }
      '->'            { TokenImp }
      '<-'            { TokenBImp }
      '-'             { TokenNot }
      '<->'           { TokenEq }
      '('             { TokenOB }
      ')'             { TokenCB }


%right '<->'
%right '->' '<-'
%right '\\/'
%right '/\\'
%left '-'
%%


Exp   : '(' Exp ')'             { $2 }
      | '-' Exp                 { neg $2 }
      | Exp '/\\' Exp           { ($1 /\ $3) }
      | Exp '\\/' Exp           { ($1 \/ $3) }
      | Exp '->' Exp            { ($1 ==> $3) }
      | Exp '<-' Exp            { ($1 <== $3) }
      | Exp '<->' Exp           { ($1 <=> $3) }
      | 'T'                     { T }
      | 'F'                     { F }
      | var                     { Atom $1 }

{

-- Used for error handling
data E a = Ok a | Failed String


thenE :: E a -> (a -> E b) -> E b
m `thenE` k =
   case m of
     Ok a -> k a
     Failed e -> Failed e


returnE :: a -> E a
returnE a = Ok a


failE :: String -> E a
failE err = Failed err


catchE :: E a -> (String -> E a) -> E a
catchE m k =
   case m of
      Ok a -> Ok a
      Failed e -> k e


parseError _ = failE "Parse error"


data Token =
  TokenT | TokenF | TokenAnd | TokenOr |
  TokenImp | TokenBImp | TokenNot | TokenEq |
  TokenOB | TokenCB | TokenVar String
  deriving Show

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('/':'\\':cs) = TokenAnd : tokenize cs
tokenize ('&':cs) = TokenAnd : tokenize cs
tokenize ('\\':'/':cs) = TokenOr : tokenize cs
tokenize ('|':cs) = TokenOr : tokenize cs
tokenize ('<':'-':'>':cs) = TokenEq : tokenize cs
tokenize ('<':'=':'>':cs) = TokenEq : tokenize cs
tokenize ('-':'>':cs) = TokenImp : tokenize cs
tokenize ('=':'>':cs) = TokenImp : tokenize cs
tokenize ('<':'-':cs) = TokenBImp : tokenize cs
tokenize ('<':'=':cs) = TokenBImp : tokenize cs
tokenize ('-':cs) = TokenNot : tokenize cs
tokenize ('~':cs) = TokenNot : tokenize cs
tokenize ('(':cs) = TokenOB : tokenize cs
tokenize (')':cs) = TokenCB : tokenize cs
tokenize ('T':cs) = TokenT : tokenize cs
tokenize ('F':cs) = TokenF : tokenize cs
tokenize input@(c:cs)
  | isVarChar c = TokenVar var : tokenize rest
  where
    isVarChar = (`elem` "abcdefghijklmnopqrstuvwxyz1234567890")
    (var, rest) = span isVarChar input
-- just skip unrecognizable characters
tokenize (_:cs) = tokenize cs


parseProp str = case parse $ tokenize str of
  Ok p -> Right p
  Failed s -> Left s
}
