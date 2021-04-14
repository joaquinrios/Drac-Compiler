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
*       Stmt                ::= StmtAssign | StmtIncr | StmtDecr | StmtFunCall 
*                               StmtIf | StmtWhile | StmtDoWhile | StmtBreak
*                               StmtReturn | StmtEmpty
*       StmtAssign          ::= Id "=" Expr ";"
*       StmtIncr            ::= "inc" Id ";"
*       StmtDecr            ::= "dec" Id ";"
*       StmtFunCall         ::= Id "(" (Expr ("," Expr)*)? ")" ";"
*       StmtIf              ::= "if" "(" Expr ")" "{" Stmt* "}" ElseIfList Else
*       ElseIfList          ::= ("elif" "(" Expr ")" "{" Stmt* "}")*
*       Else                ::= ("else" "{" Stmt* "}")?
*       StmtWhile           ::= "while" "(" Expr ")" "{" Stmt* "}"
*       StmtDoWhile         ::= "do" "{" Stmt* "}" "while" "(" Expr ")" ";"
*       StmtBreak           ::= "break" ";"
*       StmtEmpty           ::= ";"
*       Expr                ::= ExprOr
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
*       Array               ::= "[" (Expr ("," Expr)*)? "]"
*       Lit                 ::= LitBool | LitInt | LitChar | LitStr
*
*
*
*/

using System;
using System.Collections.Generic;

namespace Drac {

    class Parser {

        static readonly ISet<TokenCategory> firstOfDeclaration =
            new HashSet<TokenCategory>() {
                TokenCategory.VAR
            };

        static readonly ISet<TokenCategory> firstOfStatement =
            new HashSet<TokenCategory>() {
                TokenCategory.IDENTIFIER,
                TokenCategory.IF
            };

        static readonly ISet<TokenCategory> firstOfComparisonOperator = 
            new HashSet<TokenCategory>() {
                TokenCategory.NOTEQUALS,
                TokenCategory.EQUALS
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
        
        // this should not be here
        static readonly ISet<TokenCategory> firstOfSimpleExpression =
            new HashSet<TokenCategory>() {
                TokenCategory.IDENTIFIER,
                TokenCategory.INT_LITERAL,
                TokenCategory.TRUE,
                TokenCategory.FALSE,
                TokenCategory.PARENTHESIS_OPEN,
                TokenCategory.NEG
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

            while (firstOfDeclaration.Contains(CurrentToken)) {
                Declaration();
            }

            while (firstOfStatement.Contains(CurrentToken)) {
                Statement();
            }

            Expect(TokenCategory.EOF);
        }

        public void Declaration() {
            Type();
            Expect(TokenCategory.IDENTIFIER);
        }

        public void Statement() {

            switch (CurrentToken) {

            case TokenCategory.IDENTIFIER:
                Assignment();
                break;

            case TokenCategory.PRINT:
                Print();
                break;

            case TokenCategory.IF:
                If();
                break;

            default:
                throw new SyntaxError(firstOfStatement,
                                      tokenStream.Current);
            }
        }

        public void Type() {
            switch (CurrentToken) {

            case TokenCategory.VAR:
                Expect(TokenCategory.VAR);
                break;

            default:
                throw new SyntaxError(firstOfDeclaration,
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
          if (firstOfSimpleExpression.Contains(CurrentToken)) {
            Expression();
            while(CurrentToken == TokenCategory.COMMA) {
              Expect(TokenCategory.COMMA);
              Expression();
            }
          }
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
            Expect(TokenCategory.PARENTHESIS_OPEN);
            Expression();
            Expect(TokenCategory.PARENTHESIS_CLOSE);
            Expect(TokenCategory.SEMICOLON);
        }

        public void Break(){
            Expect(TokenCategory.BREAK);
            Expect(TokenCategory.SEMICOLON);
        }

        public void Empty() { Expect(TokenCategory.SEMICOLON); }

        public void Expression() {
            ExpressionOr();
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
                OperatorAdd();
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
                    // FunCall
                    if (CurrentToken == TokenCategory.PARENTHESIS_OPEN) {
                        Expect(TokenCategory.PARENTHESIS_OPEN);

                        Expect(TokenCategory.PARENTHESIS_CLOSE);
                    }
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

        public void Array() {
          Expect(TokenCategory.SQUARE_BRACKET_OPEN);
          // TODO: Finish this function
          Expect(TokenCategory.SQUARE_BRACKET_CLOSE);
        }

        public void Lit() {
          switch (CurrentToken) {
              case TokenCategory.TRUE:
                  Expect(TokenCategory.TRUE);
                  break;
              case TokenCategory.FALSE:
                  Expect(TokenCategory.FALSE);
                  break;
              case TokenCategory.INT_LITERAL:
                  Expect(TokenCategory.INT_LITERAL);
                  break;
              case TokenCategory.CHAR_LITERAL:
                  Expect(TokenCategory.CHAR_LITERAL);
                  break;
              case TokenCategory.STRING_LITERAL:
                  Expect(TokenCategory.STRING_LITERAL);
                  break;
              default:
                  throw new SyntaxError(null,
                                        tokenStream.Current);
           
          }
        }


        public void SimpleExpression() {

            switch (CurrentToken) {

            case TokenCategory.IDENTIFIER:
                Expect(TokenCategory.IDENTIFIER);
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

            case TokenCategory.NEG:
                Expect(TokenCategory.NEG);
                SimpleExpression();
                break;

            default:
                throw new SyntaxError(firstOfSimpleExpression,
                                      tokenStream.Current);
            }
        }

        public void Operator() {

            switch (CurrentToken) {

            case TokenCategory.AND:
                Expect(TokenCategory.AND);
                break;

            case TokenCategory.LESS:
                Expect(TokenCategory.LESS);
                break;

            case  TokenCategory.PLUS:
                Expect(TokenCategory.PLUS);
                break;

            case TokenCategory.MUL:
                Expect(TokenCategory.MUL);
                break;

            default:
                throw new SyntaxError(firstOfOperator,
                                      tokenStream.Current);
            }
        }
    }
}
