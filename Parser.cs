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

/*
 * Buttercup LL(1) Grammar:
 *
 *      Program             ::=  Declaration* Statement* EOF
 *      Declaration         ::=  Type Identifier
 *      Type                ::=  "int" | "bool"
 *      Statement           ::=  Assignment | Print | If
 *      Assignment          ::=  Identifier "=" Expression
 *      Print               ::=  "print" Expression
 *      If                  ::=  "if" Expression "then" Statement* "end"
 *      Expression          ::=  SimpleExpression (Operator SimpleExpression)*
 *      Operator›           ::=  "&" | "<" | "+" | "*"
 *      SimpleExpression    ::=  Identifier | IntLiteral | "#t" | "#f"
 *                               | "(" Expression ")" | "-" SimpleExpression
 */

 using System;
 using System.Collections.Generic;

 namespace Drac {
  class Parser {
    
  }
 }