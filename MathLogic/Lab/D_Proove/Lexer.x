{
module Lexer where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  "--".*				;
  "!"					{ \s -> Not_ }
  "&"					{ \s -> And_ }
  "|"                   { \s -> Or_}
  "->"                  { \s -> Implies_ }
  "("                   { \s -> BracketLeft_ }
  ")"                   { \s -> BracketRight_ }
  "|-"                  { \s -> TurnStile_ }
  ","                   { \s -> Comma_ }
  [$alpha $digit \_ \']+		{ \s -> Var_ s }

{
data Token = Comma_ | TurnStile_ | Not_ | BracketLeft_ | BracketRight_ | And_ | Or_ | Implies_ | Var_ String deriving (Eq,Show)
}

