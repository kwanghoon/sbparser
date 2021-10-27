module Lexer(lexerSpec) where

import Prelude hiding (EQ)
import CommonParserUtil
import Token

mkFn :: Token -> (String -> Maybe Token)
mkFn tok = \text -> Just tok

skip :: String -> Maybe Token
skip = \text -> Nothing

lexerSpec :: LexerSpec Token
lexerSpec = LexerSpec
  {
    endOfToken    = END_OF_TOKENS,
    lexerSpecList = 
      [
        ("[ \t]", skip),   -- skip a space or a tab, not a return!
        
        ("\'[^\n]*" , skip),  -- comment starting with '

        ("\r\n" , mkFn (CR "\r\n")),
        ("\n" , mkFn (CR "\n")),
        
        ("\\("    , mkFn OPEN_PAREN),
        ("\\)"    , mkFn CLOSE_PAREN),
        
        -- ("\\{"    , mkFn OPEN_BRACE),
        -- ("\\}"    , mkFn CLOSE_BRACE),
        
        ("\\["    , mkFn OPEN_BRACKET),
        ("\\]"    , mkFn CLOSE_BRACKET),

        ("\\."    , mkFn DOT),
        ("\\,"    , mkFn COMMA),
        ("\\="    , mkFn ASSIGN),
        ("\\:"    , mkFn COLON),

        ("\\+"    , mkFn PLUS),
        ("\\-"    , mkFn MINUS),
        ("\\*"    , mkFn MULTIPLY),
        ("\\/"    , mkFn DIVIDE),

        (">="     , mkFn GREATER_EQUAL),
        (">"      , mkFn GREATER_THAN),
        
        ("<="     , mkFn LESS_EQUAL),
        ("<>"     , mkFn NOT_EQUAL),
        ("<"      , mkFn LESS_THAN),

        ("\"[^\"]*\"" , mkFn STR),
        
        ("([0-9]*[.])?[0-9]+" , mkFn NUM),

        ("([Ii][Ff])"    , mkFn IF),   --Todo: resolvong confusion between keyword and identifier
        ("([Tt][Hh][Ee][Nn])"    , mkFn THEN),
        ("([Ee][Ll][Ss][Ee][Ii][Ff])"    , mkFn ELSEIF),
        ("([Ee][Ll][Ss][Ee])"    , mkFn ELSE),
        ("([Ee][Nn][Dd][Ii][Ff])"    , mkFn ENDIF),
        ("([Ww][Hh][Ii][Ll][Ee])"    , mkFn WHILE),
        ("([Ee][Nn][Dd][Ww][Hh][Ii][Ll][Ee])"    , mkFn ENDWHILE),
        ("([Ff][Oo][Rr])[^a-zA-Z0-9]"    , mkFn FOR),
        ("([Tt][Oo])[^a-zA-Z0-9]"    , mkFn TO),
        ("([Ss][Tt][Ee][Pp])"    , mkFn STEP),
        ("([Ee][Nn][Dd][Ff][Oo][Rr])"    , mkFn ENDFOR),
        ("([Gg][Oo][Tt][Oo])"    , mkFn GOTO),
        ("([Ss][Uu][Bb])"    , mkFn SUB),
        ("([Ee][Nn][Dd][Ss][Uu][Bb])"    , mkFn ENDSUB),
        ("([Aa][Nn][Dd])"    , mkFn AND),
        ("([Oo][Rr])"    , mkFn OR),

        ("[\\_a-zA-Z][\\_a-zA-Z0-9]*"    , mkFn ID)
      ]
  }

  
