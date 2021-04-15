/*
  Drac compiler - This class performs the syntactic analysis,
  (a.k.a. parsing).
  
  Authors:
  Irving Fuentes Aguilera A01745759
  Joaquin Rios Corvera A01375441
  Jordan Gonzalez Bustamante A01745993

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


/*  Drac LL(1) Grammar:
*       Program             ::= Def* EOF
*       Def                 ::= VarDef | FunDef
*       VarDef              ::= "var" IdList ";"
*       IdList              ::= Id ("," Id)*
*       FunDef              ::= Id "(" IdList? ")" "{" VarDef* Stmt* "}"
*       Stmt                ::= (Id (StmtAssign | StmtFuncCall)) | StmtIncr | StmtDecr | 
*                               StmtIf | StmtWhile | StmtDoWhile | StmtBreak
*                               StmtReturn | StmtEmpty
*       StmtAssign          ::= "=" Expr ";"
*       StmtIncr            ::= "inc" Id ";"
*       StmtDecr            ::= "dec" Id ";"
*       StmtFunCall         ::= "(" ExprList ")" ";"
*       StmtIf              ::= "if" "(" Expr ")" "{" Stmt* "}" ElseIfList Else
*       ElseIfList          ::= ("elif" "(" Expr ")" "{" Stmt* "}")*
*       Else                ::= ("else" "{" Stmt* "}")?
*       StmtWhile           ::= "while" "(" Expr ")" "{" Stmt* "}"
*       StmtDoWhile         ::= "do" "{" Stmt* "}" "while" "(" Expr ")" ";"
*       StmtBreak           ::= "break" ";"
*       StmtEmpty           ::= ";"
*       Expr                ::= ExprOr
*       ExprList            ::= (Expr ("," Expr)*)?
*       ExprOr              ::= ExprAnd ("or" ExprAnd)*
*       ExprAnd             ::= ExprComp ("and" ExprComp)*
*       ExprComp            ::= ExprRel (OpComp ExprRel)*
*       OpComp              ::= "==" | "<>"
*       ExprRel             ::= ExprAdd (OpRel ExprAdd)*
*       OpRel               ::= "<" | "<=" | ">" | ">="
*       ExprAdd             ::= ExprMul (OpAdd ExprMul)*
*       OpAdd               ::= "+" | "-"
*       ExprMul             ::= ExprUnary (OpMul ExprUnary)*
*       OpMul               ::= "*" | "/" | "%"
*       ExprUnary           ::= OpUnary* ExprPrimary
*       OpUnary             ::= "+" | "-" | "not"
*       ExprPrimary         ::= Id | StmtFunCall | Array | Lit | "(" Expr ")"
*       Array               ::= "[" ExprList "]"
*       Lit                 ::= LitBool | LitInt | LitChar | LitStr
*
*
*
*/

using System;
using System.Collections.Generic;

namespace Drac {

    class Parser {

        static readonly ISet<TokenCategory> firstOfDefinition =
            new HashSet<TokenCategory>() {
                TokenCategory.VAR,
                TokenCategory.IDENTIFIER
            };

        static readonly ISet<TokenCategory> firstOfStatement =
            new HashSet<TokenCategory>() {
                TokenCategory.IDENTIFIER,
                TokenCategory.INC,
                TokenCategory.DEC,
                TokenCategory.IF,
                TokenCategory.WHILE,
                TokenCategory.DO,
                TokenCategory.BREAK,
                TokenCategory.RETURN,
                TokenCategory.SEMICOLON
            };

        static readonly ISet<TokenCategory> firstOfComparisonOperator = 
            new HashSet<TokenCategory>() {
                TokenCategory.NOTEQUALS,
                TokenCategory.EQUALS
            };

        static readonly ISet<TokenCategory> firstOfAfterIdentifier = 
            new HashSet<TokenCategory>() {
                TokenCategory.PARENTHESIS_OPEN,
                TokenCategory.ASSIGN
            };

        static readonly ISet<TokenCategory> firstOfRelationalOperator = 
            new HashSet<TokenCategory>() {
                TokenCategory.LESS,
                TokenCategory.LESS_EQUAL,
                TokenCategory.MORE,
                TokenCategory.MORE_EQUAL
            };

        static readonly ISet<TokenCategory> firstOfAdditonOperator = 
            new HashSet<TokenCategory>() {
                TokenCategory.PLUS,
                TokenCategory.NEG
            };

        static readonly ISet<TokenCategory> firstOfUnaryOperator = 
            new HashSet<TokenCategory>() {
                TokenCategory.PLUS,
                TokenCategory.NEG,
                TokenCategory.NOT
            };
        static readonly ISet<TokenCategory> firstOfMultiplicationOperator = 
            new HashSet<TokenCategory>() {
                TokenCategory.MUL,
                TokenCategory.DIV,
                TokenCategory.REMAINDER
            };
        
        static readonly ISet<TokenCategory> firstOfPrimaryExpression =
            new HashSet<TokenCategory>() {
                TokenCategory.IDENTIFIER,
                TokenCategory.INT_LITERAL,
                TokenCategory.CHAR_LITERAL,
                TokenCategory.STRING_LITERAL,
                TokenCategory.TRUE,
                TokenCategory.FALSE,
                TokenCategory.PARENTHESIS_OPEN,
                TokenCategory.BRACKET_OPEN
            };

        static readonly ISet<TokenCategory> firstOfExpression =
            new HashSet<TokenCategory>() {
                TokenCategory.IDENTIFIER,
                TokenCategory.INT_LITERAL,
                TokenCategory.CHAR_LITERAL,
                TokenCategory.STRING_LITERAL,
                TokenCategory.PARENTHESIS_OPEN,
                TokenCategory.BRACKET_OPEN,
                TokenCategory.NEG,
                TokenCategory.PLUS,
                TokenCategory.NOT,
                TokenCategory.TRUE,
                TokenCategory.FALSE
            };

        IEnumerator<Token> tokenStream;

        public Parser(IEnumerator<Token> tokenStream) {
            this.tokenStream = tokenStream;
            this.tokenStream.MoveNext();
        }

        public TokenCategory CurrentToken {
            get { return tokenStream.Current.Category; }
        }

        public Token Expect(TokenCategory category) {
            if (CurrentToken == category) {
                Token current = tokenStream.Current;
                tokenStream.MoveNext();
                return current;
            } else {
                throw new SyntaxError(category, tokenStream.Current);
            }
        }

        public void Program() {

            while (firstOfDefinition.Contains(CurrentToken)) {
                Definition();
            }

            Expect(TokenCategory.EOF);
        }

        public void Definition() {
            switch (CurrentToken) {

            case TokenCategory.VAR:
                VarDef();
                break;

            case TokenCategory.IDENTIFIER:
                FunDef();
                break;

            default:
                throw new SyntaxError(firstOfDefinition,
                                      tokenStream.Current);
            }
        }

        public void VarDef() {
          Expect(TokenCategory.VAR);
          IdList();
          Expect(TokenCategory.SEMICOLON);
        }

        public void IdList() {
          Expect(TokenCategory.IDENTIFIER);
          while(CurrentToken == TokenCategory.COMMA) {
            Expect(TokenCategory.COMMA);
            Expect(TokenCategory.IDENTIFIER);
          }
        }

        public void FunDef() {
          Expect(TokenCategory.IDENTIFIER);
          Expect(TokenCategory.PARENTHESIS_OPEN);
          if (CurrentToken == TokenCategory.IDENTIFIER) {
            IdList();
          }
          Expect(TokenCategory.PARENTHESIS_CLOSE);
          Expect(TokenCategory.BRACKET_OPEN);
          while (CurrentToken == TokenCategory.VAR) {
            VarDef();
          }
          while(firstOfStatement.Contains(CurrentToken)) {
            Statement();
          }
          Expect(TokenCategory.BRACKET_CLOSE);
        }

        public void Statement() {
            switch (CurrentToken) {    
            // TODO: Check if next of Id is either
            // an equals or a parentheses open
            case TokenCategory.IDENTIFIER:
              Expect(TokenCategory.IDENTIFIER);
                switch (CurrentToken){
                  case TokenCategory.ASSIGN:
                    Expect(TokenCategory.ASSIGN);
                    Expression();
                    Expect(TokenCategory.SEMICOLON);
                    break;
                  
                  case TokenCategory.PARENTHESIS_OPEN:
                    Expect(TokenCategory.PARENTHESIS_OPEN);
                    ExpressionList();
                    Expect(TokenCategory.PARENTHESIS_CLOSE);
                    break;
                default:
                throw new SyntaxError(firstOfAfterIdentifier,
                                      tokenStream.Current);
                    
                }
                break;

            case TokenCategory.INC:
                Incr();
                break;

            case TokenCategory.DEC:
                Decr();
                break;

            case TokenCategory.IF:
                If();
                break;

            case TokenCategory.WHILE:
                While();
                break;

            case TokenCategory.DO:
                DoWhile();
                break;

            case TokenCategory.BREAK:
                Break();
                break;

            case TokenCategory.RETURN:
                Return();
                break;
            
            case TokenCategory.SEMICOLON:
                Empty();
                break;

            default:
                throw new SyntaxError(firstOfStatement,
                                      tokenStream.Current);
            }
        }


        public void Assignment() {
            Expect(TokenCategory.IDENTIFIER);
            Expect(TokenCategory.ASSIGN);
            Expression();
            Expect(TokenCategory.SEMICOLON);
        }

        public void Incr() {
            Expect(TokenCategory.INC);
            Expect(TokenCategory.IDENTIFIER);
            Expect(TokenCategory.SEMICOLON);
        }

        public void Decr() {
            Expect(TokenCategory.DEC);
            Expect(TokenCategory.IDENTIFIER);
            Expect(TokenCategory.SEMICOLON);
        }

        public void FunCall() {
          Expect(TokenCategory.IDENTIFIER);
          Expect(TokenCategory.PARENTHESIS_OPEN);
          ExpressionList();
          Expect(TokenCategory.PARENTHESIS_CLOSE);
          Expect(TokenCategory.SEMICOLON);
        }

        public void If() {
            Expect(TokenCategory.IF);
            Expect(TokenCategory.PARENTHESIS_OPEN);
            Expression();
            Expect(TokenCategory.PARENTHESIS_CLOSE);
            Expect(TokenCategory.BRACKET_OPEN);
            while(firstOfStatement.Contains(CurrentToken)) {
                Statement();
            }
            Expect(TokenCategory.BRACKET_CLOSE);
            ElseIfList();
            Else();
        }

        public void ElseIfList() {
            while(CurrentToken == TokenCategory.ELIF) {
                Expect(TokenCategory.ELIF);
                Expect(TokenCategory.PARENTHESIS_OPEN);
                Expression();
                Expect(TokenCategory.PARENTHESIS_CLOSE);
                Expect(TokenCategory.BRACKET_OPEN);
                while(firstOfStatement.Contains(CurrentToken)) {
                    Statement();
                }
                Expect(TokenCategory.BRACKET_CLOSE);
            }
        }

        public void Else(){
            if (CurrentToken == TokenCategory.ELSE) {
                Expect(TokenCategory.ELSE);
                Expect(TokenCategory.BRACKET_OPEN);
                while(firstOfStatement.Contains(CurrentToken)) {
                    Statement();
                }
                Expect(TokenCategory.BRACKET_CLOSE);
            }
        }

        public void While() {
            Expect(TokenCategory.WHILE);
            Expect(TokenCategory.PARENTHESIS_OPEN);
            Expression();
            Expect(TokenCategory.PARENTHESIS_CLOSE);
            Expect(TokenCategory.BRACKET_OPEN);
            while(firstOfStatement.Contains(CurrentToken)) {
                Statement();
            }
            Expect(TokenCategory.BRACKET_CLOSE);
        }

        public void DoWhile() {
            Expect(TokenCategory.DO);
            Expect(TokenCategory.BRACKET_OPEN);
            while(firstOfStatement.Contains(CurrentToken)) {
                Statement();
            }
            Expect(TokenCategory.BRACKET_CLOSE);
            Expect(TokenCategory.WHILE);
            Expect(TokenCategory.PARENTHESIS_OPEN);
            Expression();
            Expect(TokenCategory.PARENTHESIS_CLOSE);
            Expect(TokenCategory.SEMICOLON);
        }

        public void Break(){
            Expect(TokenCategory.BREAK);
            Expect(TokenCategory.SEMICOLON);
        }

        public void Return() {
          Expect(TokenCategory.RETURN);
          Expression();
          Expect(TokenCategory.SEMICOLON);
        }
        public void Empty() { Expect(TokenCategory.SEMICOLON); }

        public void Expression() {
            ExpressionOr();
        }

        public void ExpressionList() {
          if (firstOfExpression.Contains(CurrentToken)){
            Expression();
            while(CurrentToken == TokenCategory.COMMA){
              Expect(TokenCategory.COMMA);
              Expression();
            }
          }
        }

        public void ExpressionOr() {
            ExpressionAnd();
            while(CurrentToken == TokenCategory.OR) {
              Expect(TokenCategory.OR);
              ExpressionAnd();
            }
        }

        public void ExpressionAnd() {
            ExpressionComp();
            while(CurrentToken == TokenCategory.AND) {
              Expect(TokenCategory.AND);
              ExpressionComp();
            }
        }

        public void ExpressionComp() {
            ExpressionRel();
            while(firstOfComparisonOperator.Contains(CurrentToken)) {
              OperatorComp();
              ExpressionRel();
            }
        }

        public void OperatorComp() {
            switch (CurrentToken) {
                case TokenCategory.EQUALS:
                    Expect(TokenCategory.EQUALS);
                    break;
                case TokenCategory.NOTEQUALS:
                    Expect(TokenCategory.NOTEQUALS);
                    break;
                default:
                    throw new SyntaxError(firstOfComparisonOperator,
                                          tokenStream.Current);
            }
        }

        public void ExpressionRel() {
            ExpressionAdd();
            while(firstOfRelationalOperator.Contains(CurrentToken)) {
                OperatorRel();
                ExpressionAdd();
            }
        }

        public void OperatorRel() {
            switch (CurrentToken) {
                case TokenCategory.LESS:
                    Expect(TokenCategory.LESS);
                    break;
                case TokenCategory.LESS_EQUAL:
                    Expect(TokenCategory.LESS_EQUAL);
                    break;
                case TokenCategory.MORE:
                    Expect(TokenCategory.MORE);
                    break;
                case TokenCategory.MORE_EQUAL:
                    Expect(TokenCategory.MORE_EQUAL);
                    break;
                default:
                    throw new SyntaxError(firstOfRelationalOperator,
                                          tokenStream.Current);
            }
        }

        public void ExpressionAdd() {
            ExpressionMul();
            while(firstOfAdditonOperator.Contains(CurrentToken)) {
                OperatorAdd();
                ExpressionMul();
            }
        }

        public void OperatorAdd() {
            switch (CurrentToken) {
                case TokenCategory.PLUS:
                    Expect(TokenCategory.PLUS);
                    break;
                case TokenCategory.NEG:
                    Expect(TokenCategory.NEG);
                    break;
                default:
                    throw new SyntaxError(firstOfAdditonOperator,
                                          tokenStream.Current);
            }
        }

        public void ExpressionMul() {
            ExpressionUnary();
            while(firstOfMultiplicationOperator.Contains(CurrentToken)) {
                OperatorMul();
                ExpressionUnary();
            }
        }

        public void OperatorMul() {
            switch (CurrentToken) {
                case TokenCategory.MUL:
                    Expect(TokenCategory.MUL);
                    break;
                case TokenCategory.DIV:
                    Expect(TokenCategory.DIV);
                    break;
                case TokenCategory.REMAINDER:
                    Expect(TokenCategory.REMAINDER);
                    break;
                default:
                    throw new SyntaxError(firstOfMultiplicationOperator,
                                          tokenStream.Current);
            }
        }

        public void ExpressionUnary() {
          while (firstOfUnaryOperator.Contains(CurrentToken)) {
            OperatorUnary();
          }
          ExpressionPrimary();
        }
        public void OperatorUnary() {
            switch (CurrentToken) {
                case TokenCategory.PLUS:
                    Expect(TokenCategory.PLUS);
                    break;
                case TokenCategory.NEG:
                    Expect(TokenCategory.NEG);
                    break;
                case TokenCategory.NOT:
                    Expect(TokenCategory.NOT);
                    break;
                default:
                    throw new SyntaxError(firstOfUnaryOperator,
                                          tokenStream.Current);
            }
        }

        public void ExpressionPrimary() {
            switch (CurrentToken) {
                case TokenCategory.IDENTIFIER:
                    Expect(TokenCategory.IDENTIFIER);
                    if (CurrentToken == TokenCategory.PARENTHESIS_OPEN) {
                        Expect(TokenCategory.PARENTHESIS_OPEN);
                        ExpressionList();
                        Expect(TokenCategory.PARENTHESIS_CLOSE);
                    }
                    break;
                case TokenCategory.SQUARE_BRACKET_OPEN:
                    Array();
                    break;
                case TokenCategory.CHAR_LITERAL:
                    Expect(TokenCategory.CHAR_LITERAL);
                    break;
                case TokenCategory.STRING_LITERAL:
                    Expect(TokenCategory.STRING_LITERAL);
                    break;
                case TokenCategory.INT_LITERAL:
                    Expect(TokenCategory.INT_LITERAL);
                    break;
                case TokenCategory.TRUE:
                    Expect(TokenCategory.TRUE);
                    break;
                case TokenCategory.FALSE:
                    Expect(TokenCategory.FALSE);
                    break;
                case TokenCategory.PARENTHESIS_OPEN:
                    Expect(TokenCategory.PARENTHESIS_OPEN);
                    Expression();
                    Expect(TokenCategory.PARENTHESIS_CLOSE);
                    break;
                default:
                    throw new SyntaxError(firstOfPrimaryExpression,
                                          tokenStream.Current);
            }
        }

        public void Array() {
          Expect(TokenCategory.SQUARE_BRACKET_OPEN);
          ExpressionList();
          Expect(TokenCategory.SQUARE_BRACKET_CLOSE);
        }


    }
}
