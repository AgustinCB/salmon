# Smoked

Implementation Smoke, a language based on
 Lox (see [Crafting interpreters](https://craftinginterpreters.com/)) using Rust.

## Grammar

```$xslt
program         → declaration* EOF ;

declaration → classDecl
            | traitDecl
            | traitImpl
            | funDecl
	        | varDecl
            | statement ;

classDecl   → "class" IDENTIFIER ( "<" IDENTIFIER )?
              "{" ( ( "class" | "setter" | "getter" )? function )* "}" ;

traitDecl   → "trait" IDENTIFIER
              "{" ( ( "class" | "setter" | "getter" )? functionHeader )* "}" ;

traitImpl   → "trait" IDENTIFIER "for" IDENTIFIER
              "{" ( ( "class" | "setter" | "getter" )? function )* "}" ;

declWithBreak   → varDecl
                | statementWithBreak ;

statement       → exprStmt
                | forStmt
                | ifStmt
                | printStmt
                | whileStmt
                | returnStmt
                | block ;

stmtWithBreak   → exprStmt
                | forStmt
                | ifStmtWithBreak
                | printStmt
                | whileStmt
                | returnStmt
                | blockWithBreak
                | breakStmt ;

breakStmt       → break ";" ;
forStmt         → "for" "(" ( varDecl | exprStmt | ";" )
                     expression? ";"
                     expression? ")" statementWithBreak ;
block           → "{" declaration* "}" ;
blockWithBreak  → "{" declWithBreak* "}" ;
ifStmt          → "if" "(" expression ")" statement ( "else" statement )? ;
ifStmtWithBreak → "if" "(" expression ")" statementWithBreak 
                        ( "else" statementWithBreak )? ;
exprStmt        → expression ";" ;
printStmt       → "print" expression ";" ;
whileStmt       → "while" "(" expression ")" statementWithBreak ;
returnStmt      → "return" expression? ";" ;
varDecl         → "var" IDENTIFIER ( "=" expression )? ";" ;

funDecl         → "fun" function ;
functionHeader  → IDENTIFIER "(" parameters? ")" ;
function        → functionHeader block ;
parameters      → IDENTIFIER ( "," IDENTIFIER )* ;

expression      → assignment ;
assignment      → ( call "." )? IDENTIFIER ( "[" NUMBER "]" )? "=" (assignment | ternary) ;
ternary         → logicOr ( "?" expression <- ternary )? ;
logicOr         → logicAnd ( "or" logicAnd )* ;
logicAnd        → equality ( "and" logicAnd )* ;
equality        → comparison ( ( "!=" | "==" ) comparison )* ;
comparison      → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
addition        → multiplication ( ( "-" | "+" ) multiplication )* ;
multiplication  → unary ( ( "/" | "*" ) unary )* ;
unary           → ( "!" | "-" ) unary
                | arrayElement ;
call            → arrayElement ( "(" arguments? ")" | "." IDENTIFIER )* ;
arguments       → ternary ( "," ternary )* ;
arrayElement    → primary ( "[" expression "]" )?
primary         → NUMBER | STRING | "false" | "true" | "nil"
                | "[" expression ";" expression "]"
                | "[" ( expression "," ) ? expression "]"
                | "fun" "(" parameters? ")" block
                | "(" expression ")"
                | ( "!=" | "==" ) equality
                | ( ">" | ">=" | "<" | "<=" ) comparison
                | ( "+" ) addition
                | ( "/" | "*" ) multiplication ;
```
