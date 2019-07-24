{
module Lexer where
}

%wrapper "basic-bytestring"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  "--".*				;
  "!"					{ \s -> Not }
  "&"					{ \s -> And }
  "|"                   { \s -> Or}
  "->"                  { \s -> Implies }
  "("                   { \s -> BracketLeft }
  ")"                   { \s -> BracketRight }
  [$alpha $digit \_ \']+		{ \s -> Var s }

{
data Token = Not | BracketLeft | BracketRight | And | Or | Implies | Var ByteString.ByteString deriving (Eq,Show)
}

