{-
  Extended calculator for arithmetic expressions with variables
  Part 1: Added subtraction (-), division (/) and modulus (%)
  Part 2: Added variables and assignments
  
  Based on the basic calculator example from "Programming in Haskell"
  by Graham Hutton.

  Pedro Vasconcelos, 2025
  Extended by Student, 2025
-}
module Main where

import Parsing
import Data.Char

--
-- Part 1: Extended data type for expressions
-- Now includes subtraction, division and modulus
--
data Expr = Num Integer
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Mod Expr Expr
          | Var String        -- Part 2: variables
          deriving Show

-- Part 2: Data type for commands (assignments or expressions)
data Command = Assign String Expr
             | Evaluate Expr
             deriving Show

-- Part 2: Environment to store variable values
type Env = [(String, Integer)]

-- | Recursive evaluator for expressions
-- Part 1: Now handles Sub, Div and Mod
-- Part 2: Now uses environment for variable lookup
eval :: Env -> Expr -> Integer
eval env (Num n) = n
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Sub e1 e2) = eval env e1 - eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2
eval env (Div e1 e2) = eval env e1 `div` eval env e2
eval env (Mod e1 e2) = eval env e1 `mod` eval env e2
eval env (Var name) = 
    case lookup name env of
        Just val -> val
        Nothing -> error ("Variable " ++ name ++ " not defined")

-- | Parser for expressions
-- Grammar rules (Part 1 extended):
--
-- expr ::= term exprCont
-- exprCont ::= '+' term exprCont | '-' term exprCont | epsilon
-- term ::= factor termCont
-- termCont ::= '*' factor termCont | '/' factor termCont 
--              | '%' factor termCont | epsilon
-- factor ::= variable | natural | '(' expr ')'

expr :: Parser Expr
expr = do t <- term
          exprCont t

exprCont :: Expr -> Parser Expr
exprCont acc = do char '+'
                  t <- term
                  exprCont (Add acc t)
               <|> do char '-'
                      t <- term
                      exprCont (Sub acc t)
               <|> return acc
              
term :: Parser Expr
term = do f <- factor
          termCont f

termCont :: Expr -> Parser Expr
termCont acc = do char '*'
                  f <- factor  
                  termCont (Mul acc f)
               <|> do char '/'
                      f <- factor
                      termCont (Div acc f)
               <|> do char '%'
                      f <- factor
                      termCont (Mod acc f)
               <|> return acc

-- Part 2: factor now includes variables
factor :: Parser Expr
factor = do n <- natural
            return (Num n)
         <|> do v <- variable
                return (Var v)
         <|> do char '('
                e <- expr
                char ')'
                return e

-- Part 2: Parser for variable names (one or more letters)
variable :: Parser String
variable = do xs <- many1 (satisfy isAlpha)
              return xs

natural :: Parser Integer
natural = do xs <- many1 (satisfy isDigit)
             return (read xs)

-- Part 2: Parser for commands (assignments or expressions)
command :: Parser Command
command = do v <- variable
             char '='
             e <- expr
             return (Assign v e)
          <|> do e <- expr
                 return (Evaluate e)

----------------------------------------------------------------             
  
main :: IO ()
main = do txt <- getContents
          calculator [] (lines txt)

-- | read-eval-print loop
-- Part 2: Now maintains environment between commands
calculator :: Env -> [String] -> IO ()
calculator env [] = return ()
calculator env (l:ls) = 
    do let (result, newEnv) = execute env l
       putStrLn result
       calculator newEnv ls

-- Part 2: Execute a command, returning result and updated environment
execute :: Env -> String -> (String, Env)
execute env txt =
    case parse command txt of
        [(Assign var e, "")] -> 
            let val = eval env e
                newEnv = updateEnv var val env
            in (show val, newEnv)
        [(Evaluate e, "")] ->
            (show (eval env e), env)
        _ -> ("parse error; try again", env)

-- Part 2: Update environment with new variable value
updateEnv :: String -> Integer -> Env -> Env
updateEnv var val [] = [(var, val)]
updateEnv var val ((v,n):rest)
    | var == v  = (var, val) : rest
    | otherwise = (v, n) : updateEnv var val rest