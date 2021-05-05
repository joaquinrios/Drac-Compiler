/*
  Drac compiler - Specific node subclasses for the AST (Abstract
  Syntax Tree).
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

namespace Drac {
    class And: Node {}
    class Assignment: Node {}
    class Declaration: Node {}
    class DeclarationList: Node {}    
    class False: Node {}
    class Identifier: Node {}
    class IntLiteral: Node {}
    class If: Node {}
    class Less: Node {}
    class Mul: Node {}
    class Neg: Node {}
    class Plus: Node {}
    class Print: Node {}
    class Program: Node {}
    class True: Node {}
}