/*
  Drac compiler - This class performs the lexical analysis,
  (a.k.a. scanning).
  
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

using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace Drac {

    class Scanner {

        readonly string input;

        static readonly Regex regex = new Regex(
            @"
                (?<SingleComment>     --.*    )
              | (?<MultiComment>     [(][*](.|\n)*?[*][)]  )
              | (?<NewLine>     \n          )
              | (?<WhiteSpace>  \s          )
              | (?<CharLiteral>  '[^'\\\n]'|'\\([nrt\\'""]|u[a-fA-F0-9]{6})')
              | (?<StringLiteral>  ""([^""\\\n]|\\([nrt\\'""]|u[a-fA-F0-9]{6}))*"")
              | (?<IntLiteral>  -?\d+       )
              | (?<And>         and\b         )
              | (?<Assign>       =          )
              | (?<BracketOpen>  [{]         )
              | (?<BracketClose> [}]         )  
              | (?<Break>       break\b       )
              | (?<Comma>         ,         )
              | (?<Dec>         dec\b         )
              | (?<Div>         [/]         )
              | (?<Do>          do\b          )
              | (?<Elif>        elif\b        )
              | (?<Else>        else\b        )
              | (?<False>       false\b       )
              | (?<If>          if\b          )
              | (?<Inc>         inc\b         )
              | (?<NotEquals>   <>           )
              | (?<LessEqual>   <=          )
              | (?<Less>        [<]         )
              | (?<MoreEqual>   >=          )
              | (?<More>        [>]         )
              | (?<Equals>      ==          )
              | (?<Mul>         [*]         )
              | (?<Not>         not\b         )
              | (?<Neg>         [-]         )
              | (?<Or>          or\b          )
              | (?<ParenthesisOpen>     [(] )
              | (?<ParenthesisClose>    [)] )
              | (?<Plus>        [+]         )
              | (?<Remainder>   [%]         )
              | (?<Return>      return\b      )
              | (?<Semicolon>      ;        )
              | (?<SquareBracketOpen>  \[   )
              | (?<SquareBracketClose> \]   )
              | (?<True>        true\b        )
              | (?<Var>         var\b         )
              | (?<While>       while\b       )
              | (?<Identifier>  [a-zA-Z]\w* )
              | (?<Other>      .           )     # Must be last: match any other character.
            ",
            RegexOptions.IgnorePatternWhitespace
                | RegexOptions.Compiled
                | RegexOptions.Multiline
            );

        static readonly IDictionary<string, TokenCategory> tokenMap =
            new Dictionary<string, TokenCategory>() {
                {"And", TokenCategory.AND},
                {"Assign", TokenCategory.ASSIGN},
                {"BracketOpen", TokenCategory.BRACKET_OPEN},
                {"BracketClose",TokenCategory.BRACKET_CLOSE},
                {"Break",TokenCategory.BREAK},
                {"CharLiteral",TokenCategory.CHAR_LITERAL},
                {"Comma", TokenCategory.COMMA},
                {"Dec",TokenCategory.DEC},
                {"Div",TokenCategory.DIV},
                {"Do",TokenCategory.DO},
                {"Elif",TokenCategory.ELIF},
                {"Else",TokenCategory.ELSE},
                {"Equals",TokenCategory.EQUALS},
                {"False",TokenCategory.FALSE},
                {"Identifier",TokenCategory.IDENTIFIER},
                {"If",TokenCategory.IF},
                {"Inc",TokenCategory.INC},
                {"IntLiteral",TokenCategory.INT_LITERAL},
                {"Less", TokenCategory.LESS},
                {"LessEqual", TokenCategory.LESS_EQUAL},
                {"More", TokenCategory.MORE},
                {"MoreEqual", TokenCategory.MORE_EQUAL},
                {"Mul", TokenCategory.MUL},
                {"Not", TokenCategory.NOT},
                {"NotEquals", TokenCategory.NOTEQUALS}
                {"Neg", TokenCategory.NEG},
                {"Or", TokenCategory.OR},
                {"ParenthesisOpen", TokenCategory.PARENTHESIS_OPEN},
                {"ParenthesisClose", TokenCategory.PARENTHESIS_CLOSE},
                {"Plus", TokenCategory.PLUS},
                {"Remainder", TokenCategory.REMAINDER},
                {"Return", TokenCategory.RETURN},
                {"Semicolon", TokenCategory.SEMICOLON},
                {"StringLiteral", TokenCategory.STRING_LITERAL},
                {"SquareBracketOpen", TokenCategory.SQUARE_BRACKET_OPEN},
                {"SquareBracketClose", TokenCategory.SQUARE_BRACKET_CLOSE},
                {"True", TokenCategory.TRUE},
                {"Var", TokenCategory.VAR},
                {"While", TokenCategory.WHILE}
            };

        public Scanner(string input) {
            this.input = input;
        }

        public IEnumerable<Token> Scan() {

            var result = new LinkedList<Token>();
            var row = 1;
            var columnStart = 0;

            foreach (Match m in regex.Matches(input)) {

                if (m.Groups["NewLine"].Success) {

                    row++;
                    columnStart = m.Index + m.Length;

                } else if (m.Groups["MultiComment"].Success) {

                    // Count lines of multi-line comment
                    string[] lines = m.Value.Split("\n");
                    row+= lines.Length - 1;
                    columnStart = lines[lines.Length-1].Length;

                } else if (m.Groups["WhiteSpace"].Success
                    || m.Groups["SingleComment"].Success) {

                    // Skip white space and comments.

                } else if (m.Groups["Other"].Success) {

                    // Found an illegal character.
                    result.AddLast(
                        new Token(m.Value,
                            TokenCategory.ILLEGAL_CHAR,
                            row,
                            m.Index - columnStart + 1));

                } else {

                    // Must be any of the other tokens.
                    result.AddLast(FindToken(m, row, columnStart));
                }
            }

            result.AddLast(
                new Token(null,
                    TokenCategory.EOF,
                    row,
                    input.Length - columnStart + 1));

            return result;
        }

        Token FindToken(Match m, int row, int columnStart) {
            foreach (var name in tokenMap.Keys) {
                if (m.Groups[name].Success) {
                    return new Token(m.Value,
                        tokenMap[name],
                        row,
                        m.Index - columnStart + 1);
                }
            }
            throw new InvalidOperationException(
                "regex and tokenMap are inconsistent: " + m.Value);
        }
    }
}
