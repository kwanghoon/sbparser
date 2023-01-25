module Token (Token(..)) where

import TokenInterface

data Token =
  -- MAIN TOKEN	
  -- NONE

  -- '\n', '\''
    CR String

  -- EOT
  | END_OF_TOKENS

  -- Literal 

  | STR
  | NUM

  -- Keyword 

  | IF
  | THEN
  | ELSE
  | ELSEIF
  | ENDIF

  | WHILE
  | ENDWHILE

  | FOR
  | TO
  | STEP
  | ENDFOR

  | SUB
  | ENDSUB

--  | LABEL  -- remove?
  | GOTO

  -- IDENTIFIER (Var name, Property name, Method name...)

  | ID

  -- (), {}, []
  | OPEN_PAREN
  | CLOSE_PAREN
--  | OPEN_BRACE
--  | CLOSE_BRACE
  | OPEN_BRACKET
  | CLOSE_BRACKET

  -- . , :
  | DOT    -- Object.Propertyname of Object.MethodName
  | COMMA  -- Mathod1(Param1, Param2, ... , Paramn)
  | COLON  -- Label

  -- Unary operator
  -- Four arithmetic operators
  | PLUS         -- O + O
  | MINUS        -- O - O
  | MULTIPLY     -- O * O
  | DIVIDE       -- O / O
  | UNARY_MINUS  -- -O

  -- Comparison operators > < >= <= = <>
  | LESS_THAN      -- O < O
  | LESS_EQUAL     -- O <= O
  | GREATER_THAN   -- O > O
  | GREATER_EQUAL  -- O >= O
  | EQUAL          -- O = O
  | NOT_EQUAL      -- O <> O

  -- AND, OR

  | AND
  | OR

  -- Assignment =
  | ASSIGN         -- Var or Property = expr
  deriving (Eq, Show)


tokenStrList :: [(Token,String)]
tokenStrList =
  [ (CR "\n", "CR"), (CR "\r\n", "CR"),
    (END_OF_TOKENS, "$"),
    (STR, "STR"), (NUM, "NUM"),
    (IF, "If"), (THEN, "Then"), (ELSE, "Else"), (ELSEIF, "ElseIf"), (ENDIF, "EndIf"),
    (WHILE, "While"), (ENDWHILE, "EndWhile"),
    (FOR, "For"), (TO, "To"), (STEP, "Step"), (ENDFOR, "EndFor"),
    (SUB, "Sub"), (ENDSUB, "EndSub"),
    -- (LABEL, "label_literal"),
    (GOTO, "Goto"),
    (ID, "ID"),
    (OPEN_PAREN, "("), (CLOSE_PAREN, ")"),
--    (OPEN_BRACE, "{"), (CLOSE_BRACE, "}"),
    (OPEN_BRACKET, "["), (CLOSE_BRACKET, "]"),

    (DOT, "."), (COMMA, ","), (COLON, ":"),
    
    (PLUS, "+"), (MINUS, "-"), (MULTIPLY, "*"), (DIVIDE, "/"),
    (UNARY_MINUS, "-"),  -- Todo: fromToken is not a function???
    (LESS_THAN, "<"), (LESS_EQUAL, "<="),
    (GREATER_THAN, ">"), (GREATER_EQUAL, ">="),
    (EQUAL, "="), (NOT_EQUAL, "<>"),
    
    (AND, "And"), (OR, "Or"),
    
    (ASSIGN, "=")
  ]

findTok tok [] = Nothing
findTok tok ((tok_,str):list)
  | tok == tok_ = Just str
  | otherwise   = findTok tok list

instance TokenInterface Token where
  fromToken tok =
    case findTok tok tokenStrList of
      Nothing  -> error ("fromToken: " ++ show tok)
      Just str -> str
      
  isEOT END_OF_TOKENS = True
  isEOT _             = False
