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
*       Stmt                ::= (Id (StmtAssign | StmtFunCall)) | StmtIncr | StmtDecr | 
*                               StmtIf | StmtWhile | StmtDoWhile | StmtBreak
*                               StmtReturn | StmtEmpty
*       StmtAssign          ::= "=" Expr ";"
*       StmtIncr            ::= "inc" Id ";"
*       StmtDecr            ::= "dec" Id ";"
*       StmtFunCall         ::= FunCall ";"
*       FunCall             ::= "(" ExprList ")"
*       StmtIf              ::= "if" "(" Expr ")" "{" Stmt* "}" ElseIfList Else
*       ElseIfList          ::= ("elif" "(" Expr ")" "{" Stmt* "}")*
*       Else                ::= ("else" "{" Stmt* "}")?
*       StmtWhile           ::= "while" "(" Expr ")" "{" Stmt* "}"
*       StmtDoWhile         ::= "do" "{" Stmt* "}" "while" "(" Expr ")" ";"
*       StmtBreak           ::= "break" ";"
*       StmtReturn          ::= "return" Expr ";"
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
*       ExprPrimary         ::= Id StmtFunCall? | Array | Lit | "(" Expr ")"
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

        static readonly ISet<TokenCategory> firstOfLiteral =
            new HashSet<TokenCategory>() {
                TokenCategory.INT_LITERAL,
                TokenCategory.CHAR_LITERAL,
                TokenCategory.STRING_LITERAL,
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

        public Node Program() {

            var declList = new DeclarationList();

            while (firstOfDefinition.Contains(CurrentToken)) {
                declList.Add(Definition());
            }

            Expect(TokenCategory.EOF);

            return new Program() { declList };
        }

        public Node Definition() {
            
            switch (CurrentToken) {

            case TokenCategory.VAR:
                return VarDef();
            case TokenCategory.IDENTIFIER:
                return FunDef();

            default:
                throw new SyntaxError(firstOfDefinition,
                                      tokenStream.Current);
            }
        }

        public Node VarDef() {
            var result = new VarDef() { AnchorToken = Expect(TokenCategory.VAR) };
            var idList = IdList();
            result.Add(idList);
            Expect(TokenCategory.SEMICOLON);
            return result;
        }

        public IdList IdList() {
            var idList = new IdList();
            var id = new Identifier() { AnchorToken = Expect(TokenCategory.IDENTIFIER) };
            idList.Add(id);
            while(CurrentToken == TokenCategory.COMMA) {
                Expect(TokenCategory.COMMA);
                id = new Identifier() { AnchorToken = Expect(TokenCategory.IDENTIFIER) };
                idList.Add(id);
            }
            return idList;
        }

        
        public Node FunDef() {
            var functionToken = Expect(TokenCategory.IDENTIFIER);
            
            Expect(TokenCategory.PARENTHESIS_OPEN);
            var idList = new IdList(); //TODO CHECK
            if (CurrentToken == TokenCategory.IDENTIFIER) {
                idList = IdList();
            }
            Expect(TokenCategory.PARENTHESIS_CLOSE);
            Expect(TokenCategory.BRACKET_OPEN);
            Node varDef = new VarDef();
            while (CurrentToken == TokenCategory.VAR) {
                varDef = VarDef();
            }
            var stmtList = new StatementList();
            while(firstOfStatement.Contains(CurrentToken)) {
                stmtList.Add(Statement());
            }
            var function = new Function() { idList, varDef, stmtList };
            function.AnchorToken = functionToken;
            
            Expect(TokenCategory.BRACKET_CLOSE);
            return function;
        }

        public Node Statement() {
            switch (CurrentToken) {    
                case TokenCategory.IDENTIFIER:
                    var idToken = Expect(TokenCategory.IDENTIFIER);
                    switch (CurrentToken){
                        case TokenCategory.ASSIGN:
                            return StmtAssign(idToken);
                        
                        case TokenCategory.PARENTHESIS_OPEN:
                            return StmtFunCall(idToken);
                            
                        default:
                            throw new SyntaxError(firstOfAfterIdentifier,
                                        tokenStream.Current);   
                    }

            case TokenCategory.INC:
                return StmtIncr();

            case TokenCategory.DEC:
                return StmtDecr();

            case TokenCategory.IF:
                return StmtIf();

            case TokenCategory.WHILE:
                return StmtWhile();

            case TokenCategory.DO:
                return StmtDoWhile();

            case TokenCategory.BREAK:
                return StmtBreak();

            case TokenCategory.RETURN:
                return StmtReturn();
            
            case TokenCategory.SEMICOLON:
                return StmtEmpty();

            default:
                throw new SyntaxError(firstOfStatement,
                                      tokenStream.Current);
            }
        }


        public Node StmtAssign(Token idToken) {
            Expect(TokenCategory.ASSIGN);
            var expr = Expression();
            var result = new Assignment() { expr };
            result.AnchorToken = idToken;
            Expect(TokenCategory.SEMICOLON);
            return result;
        }

        public Node StmtIncr() {
            var result = new Inc() { AnchorToken = Expect(TokenCategory.INC) };
            result.Add(new Identifier() {AnchorToken = Expect(TokenCategory.IDENTIFIER)});
            Expect(TokenCategory.SEMICOLON);

            return result;
        }

        public Node StmtDecr() {
            var result = new Dec() { AnchorToken = Expect(TokenCategory.DEC) };
            result.Add(new Identifier() {AnchorToken = Expect(TokenCategory.IDENTIFIER)});
            Expect(TokenCategory.SEMICOLON);

            return result;
        }

        public Node StmtFunCall(Token idToken) {
          var result = FunCall();
          Expect(TokenCategory.SEMICOLON);
          result.AnchorToken = idToken;
          return result;
        }

        public Node FunCall() {
            Expect(TokenCategory.PARENTHESIS_OPEN);
            var result = ExpressionList();
            Expect(TokenCategory.PARENTHESIS_CLOSE);
            return result;
        }

        public Node StmtIf() {
            var ifToken = Expect(TokenCategory.IF);
            Expect(TokenCategory.PARENTHESIS_OPEN);
            var expr = Expression();
            Expect(TokenCategory.PARENTHESIS_CLOSE);
            Expect(TokenCategory.BRACKET_OPEN);
            var stmtList = new StatementList();
            while(firstOfStatement.Contains(CurrentToken)) {
                stmtList.Add(Statement());
            }
            Expect(TokenCategory.BRACKET_CLOSE);
            var elseIfStmt = ElseIfList();
            var elseStmt = Else();
            var stmtIf = new If() { expr, stmtList, elseIfStmt, elseStmt };
            stmtIf.AnchorToken = ifToken;
            return stmtIf; 
        }

        public Node ElseIfList() {
            var result = new ElseIfList();
            while(CurrentToken == TokenCategory.ELIF) {
                var ifToken = Expect(TokenCategory.ELIF);
                Expect(TokenCategory.PARENTHESIS_OPEN);
                var expr = Expression();
                Expect(TokenCategory.PARENTHESIS_CLOSE);
                Expect(TokenCategory.BRACKET_OPEN);
                var stmtList = new StatementList();
                while(firstOfStatement.Contains(CurrentToken)) {
                    stmtList.Add(Statement());
                }
                Expect(TokenCategory.BRACKET_CLOSE);
                var elseIf = new ElseIf() { expr, stmtList };
                elseIf.AnchorToken = ifToken;
                result.Add(elseIf);
            }

            return result;
        }

        public Node Else(){
            var result = new Else();
            if (CurrentToken == TokenCategory.ELSE) {
                var elseToken = Expect(TokenCategory.ELSE);
                result.AnchorToken = elseToken;
                Expect(TokenCategory.BRACKET_OPEN);
                var stmtList = new StatementList();
                while(firstOfStatement.Contains(CurrentToken)) {
                    stmtList.Add(Statement());
                }
                result.Add(stmtList);
                Expect(TokenCategory.BRACKET_CLOSE);
            }
            return result;
        }

        public Node StmtWhile() {
            var result = new While() { AnchorToken = Expect(TokenCategory.WHILE) };
            
            Expect(TokenCategory.PARENTHESIS_OPEN);
            var expr = Expression();
            Expect(TokenCategory.PARENTHESIS_CLOSE);
            Expect(TokenCategory.BRACKET_OPEN);
            var stmtList = new StatementList();
            while(firstOfStatement.Contains(CurrentToken)) {
                stmtList.Add(Statement());
            }
            Expect(TokenCategory.BRACKET_CLOSE);
            
            result.Add(expr);
            result.Add(stmtList);

            return result;
        }

        public Node StmtDoWhile() {
            var doToken = Expect(TokenCategory.DO);
            Expect(TokenCategory.BRACKET_OPEN);
            var stmtList = new StatementList();
            while(firstOfStatement.Contains(CurrentToken)) {
                stmtList.Add(Statement());
            }

            Expect(TokenCategory.BRACKET_CLOSE);
            Expect(TokenCategory.WHILE);
            Expect(TokenCategory.PARENTHESIS_OPEN);
            var condition = Expression();
            Expect(TokenCategory.PARENTHESIS_CLOSE);
            Expect(TokenCategory.SEMICOLON);
            var result = new Do() { condition, stmtList };
            result.AnchorToken = doToken;

            return result;
        }

        public Node StmtBreak(){
            var result = new Break() { AnchorToken = Expect(TokenCategory.BREAK) };
            Expect(TokenCategory.SEMICOLON);
            return result;
        }

        public Node StmtReturn() {
          var result = new Return() { AnchorToken = Expect(TokenCategory.RETURN) };
          var expr = Expression();
          result.Add(expr);
          Expect(TokenCategory.SEMICOLON);
          return result;
        }
        public Node StmtEmpty() {
            return new Empty() { AnchorToken = Expect(TokenCategory.SEMICOLON) };
        }

        public Node Expression() {
            return ExpressionOr();
        }

        public Node ExpressionList() {
          var result = new ExpressionList();
          if (firstOfExpression.Contains(CurrentToken)){
            var expr = Expression();
            result.Add(expr);
            while(CurrentToken == TokenCategory.COMMA){
              Expect(TokenCategory.COMMA);
              expr = Expression();
              result.Add(expr);
            }
          }

          return result;
        }

        public Node ExpressionOr() {
            var expr1 = ExpressionAnd();
            while(CurrentToken == TokenCategory.OR) {
              var expr2 = new Or() { AnchorToken = Expect(TokenCategory.OR) };
              expr2.Add(expr1);
              expr2.Add(ExpressionAnd());
              expr1 = expr2;
            }

            return expr1;
        }

        public Node ExpressionAnd() {
            var expr1 = ExpressionComp();
            while(CurrentToken == TokenCategory.AND) {
              var expr2 = new And() { AnchorToken = Expect(TokenCategory.AND) };
              expr2.Add(expr1);
              expr2.Add(ExpressionComp());
              expr1 = expr2;
            }

            return expr1;
        }

        public Node ExpressionComp() {
            var expr1 = ExpressionRel();
            while(firstOfComparisonOperator.Contains(CurrentToken)) {
              var expr2 = OperatorComp();
              expr2.Add(expr1);
              expr2.Add(ExpressionRel());
              expr1 = expr2;
            }

            return expr1;
        }

        public Node OperatorComp() {
            switch (CurrentToken) {
                case TokenCategory.EQUALS:
                    return new Equals() { 
                        AnchorToken = Expect(TokenCategory.EQUALS) 
                    };
                case TokenCategory.NOTEQUALS:
                    return new NotEquals() { 
                        AnchorToken = Expect(TokenCategory.NOTEQUALS) 
                    };
                default:
                    throw new SyntaxError(firstOfComparisonOperator,
                                          tokenStream.Current);
            }
        }

        public Node ExpressionRel() {
            var expr1 = ExpressionAdd();
            while(firstOfRelationalOperator.Contains(CurrentToken)) {
                var expr2 = OperatorRel();
                expr2.Add(expr1);
                expr2.Add(ExpressionAdd());
                expr1 = expr2;
            }

            return expr1;
        }

        public Node OperatorRel() {
            switch (CurrentToken) {
                case TokenCategory.LESS:
                    return new Less() { 
                        AnchorToken = Expect(TokenCategory.LESS) 
                    };
                case TokenCategory.LESS_EQUAL:
                    return new LessEqual() { 
                        AnchorToken = Expect(TokenCategory.LESS_EQUAL) 
                    };
                case TokenCategory.MORE:
                    return new More() { 
                        AnchorToken = Expect(TokenCategory.MORE) 
                    };
                case TokenCategory.MORE_EQUAL:
                    return new MoreEqual() { 
                        AnchorToken = Expect(TokenCategory.MORE_EQUAL) 
                    };
                default:
                    throw new SyntaxError(firstOfRelationalOperator,
                                          tokenStream.Current);
            }
        }

        public Node ExpressionAdd() {
            var expr1 = ExpressionMul();
            while(firstOfAdditonOperator.Contains(CurrentToken)) {
                var expr2 = OperatorAdd();
                expr2.Add(expr1);
                expr2.Add(ExpressionMul());
                expr1 = expr2;
            }

            return expr1;
        }

        public Node OperatorAdd() {
            switch (CurrentToken) {
                case TokenCategory.PLUS:
                    return new Plus() { 
                        AnchorToken = Expect(TokenCategory.PLUS) 
                    };
                case TokenCategory.NEG:
                    return new Neg() { 
                        AnchorToken = Expect(TokenCategory.NEG) 
                    };
                default:
                    throw new SyntaxError(firstOfAdditonOperator,
                                          tokenStream.Current);
            }
        }

        public Node ExpressionMul() {
            var expr1 = ExpressionUnary();
            while(firstOfMultiplicationOperator.Contains(CurrentToken)) {
                var expr2 = OperatorMul();
                expr2.Add(expr1);
                expr2.Add(ExpressionUnary());
                expr1 = expr2;
            }

            return expr1;
        }

        public Node OperatorMul() {
            switch (CurrentToken) {
                case TokenCategory.MUL:
                    return new Mul() {
                        AnchorToken = Expect(TokenCategory.MUL)
                    };
                case TokenCategory.DIV:
                    return new Div() {
                        AnchorToken = Expect(TokenCategory.DIV)
                    };
                case TokenCategory.REMAINDER:
                    return new Remainder() {
                        AnchorToken = Expect(TokenCategory.REMAINDER)
                    };
                default:
                    throw new SyntaxError(firstOfMultiplicationOperator,
                                          tokenStream.Current);
            }
        }

        public Node ExpressionUnary() {
          List<Node> unaryOperators = new List<Node>();
          while (firstOfUnaryOperator.Contains(CurrentToken)) {
            unaryOperators.Add(OperatorUnary());
          }
          
          var expr1 = ExpressionPrimary();
          foreach (Node op in unaryOperators)
          {
              var expr2 = op;
              expr2.Add(expr1);
              expr1 = expr2;
          }

          return expr1;
        }
        public Node OperatorUnary() {
            switch (CurrentToken) {
                case TokenCategory.PLUS:
                    return new Plus() {
                        AnchorToken = Expect(TokenCategory.PLUS)
                    };
                case TokenCategory.NEG:
                    return new Neg() {
                        AnchorToken = Expect(TokenCategory.NEG)
                    };
                case TokenCategory.NOT:
                    return new Not() {
                        AnchorToken = Expect(TokenCategory.NOT)
                    };
                default:
                    throw new SyntaxError(firstOfUnaryOperator,
                                          tokenStream.Current);
            }
        }

        //TODO terminar case IDENTIFIER
        public Node ExpressionPrimary() {
            var result = new Node();
            switch (CurrentToken) {
                case TokenCategory.IDENTIFIER:
                    var idToken = Expect(TokenCategory.IDENTIFIER);
                    if (CurrentToken == TokenCategory.PARENTHESIS_OPEN) {
                        result = FunCall();
                        result.AnchorToken = idToken;
                        return result;
                    }
                    result = new Identifier() { AnchorToken = idToken };
                    return result;
                case TokenCategory.SQUARE_BRACKET_OPEN:
                    return Array();
                case TokenCategory.CHAR_LITERAL:
                    return Literal();
                case TokenCategory.STRING_LITERAL:
                    return Literal();
                case TokenCategory.INT_LITERAL:
                    return Literal();
                case TokenCategory.TRUE:
                    return Literal();
                case TokenCategory.FALSE:
                    return Literal();
                case TokenCategory.PARENTHESIS_OPEN:
                    Expect(TokenCategory.PARENTHESIS_OPEN);
                    result = Expression();
                    Expect(TokenCategory.PARENTHESIS_CLOSE);
                    return result;
                default:
                    throw new SyntaxError(firstOfPrimaryExpression,
                                          tokenStream.Current);
            }
        }

        public Node Array() {
          var result = new Array();
          Expect(TokenCategory.SQUARE_BRACKET_OPEN);
          result.Add(ExpressionList());
          Expect(TokenCategory.SQUARE_BRACKET_CLOSE);
          return result;
        }

        public Node Literal() {
            switch (CurrentToken) {
                case TokenCategory.CHAR_LITERAL:
                    return new CharLiteral() { 
                        AnchorToken = Expect(TokenCategory.CHAR_LITERAL) 
                    };
                case TokenCategory.STRING_LITERAL:
                    return new StringLiteral() { 
                        AnchorToken = Expect(TokenCategory.STRING_LITERAL) 
                    };
                case TokenCategory.INT_LITERAL:
                    return new IntLiteral() { 
                        AnchorToken = Expect(TokenCategory.INT_LITERAL) 
                    };
                case TokenCategory.TRUE:
                    return new True() { 
                        AnchorToken = Expect(TokenCategory.TRUE) 
                    };
                case TokenCategory.FALSE:
                    return new False() { 
                        AnchorToken = Expect(TokenCategory.FALSE) 
                    };
                default:
                    throw new SyntaxError(firstOfLiteral,
                                          tokenStream.Current);
            }
        }

    }
}
