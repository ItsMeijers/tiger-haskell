{
  module Compiler.Tokens (main) where -- Code scrap to be placed directly in the output
}

%wrapper "basic" -- Controls what kind of support code Alex should produce along with the basic scanner

-- Macros for use in token definitions
$digit = 0-9
$alpha = [a-aA-Z]

-- Ends the macro definitions and starts the definition of the scanner.
tokens :-

  -- Each token specificatino takes the form of regexp  { code }
  -- Actions are all functions from String -> Token
  $white+                         ;
  "--".*                          ;

{

data Token = TWhile
           | TFor
           | TTo
           | TBreak
           | TLet
           | TIn
           | TEnd
           | TFunction
           | TVar
           | TType
           | TArray
           | TIf
           | TThen
           | TElse
           | TDo
           | TOf
           | TNil
           | TSymbol String -- Change into specific tokens?
           | TString String
           | TIdentifier String
           | TInt Int
           deriving (Show, Eq)

main = do
  s <- getContents
  print (alexScanTokens s)

}