/*
  Drac compiler - Semantic analyzer.
  
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

namespace Drac {
    class FunctionRecord {
            public string name;
            public Boolean isPrimitive;
            public int arity;
            public Object value;

            public FunctionRecord(string name, Boolean isPrimitive, int arity, Object value) {
                this.name = name;
                this.isPrimitive = isPrimitive;
                this.arity = arity;
                this.value = value;
            }
        }
    class FirstSemanticVisitor {
        
        public HashSet<string> TableVariables {
          get;
          private set;
        }

        public IDictionary<string, FunctionRecord> TableFunctions {
            get;
            private set;
        }

        public FirstSemanticVisitor() {
            TableVariables = new HashSet<string>();
            TableFunctions = new SortedDictionary<string, FunctionRecord>();

            TableFunctions.Add("printi", new FunctionRecord("printi", true, 1, null));
            TableFunctions.Add("printc", new FunctionRecord("printc", true, 1, null));
            TableFunctions.Add("prints", new FunctionRecord("prints", true, 1, null));
            TableFunctions.Add("println", new FunctionRecord("println", true, 0, null));
            TableFunctions.Add("readi", new FunctionRecord("readi", true, 0, null));
            TableFunctions.Add("reads", new FunctionRecord("reads", true, 0, null));
            TableFunctions.Add("new", new FunctionRecord("new", true, 1, null));
            TableFunctions.Add("size", new FunctionRecord("size", true, 1, null));
            TableFunctions.Add("add", new FunctionRecord("add", true, 2, null));
            TableFunctions.Add("get", new FunctionRecord("get", true, 2, null));
            TableFunctions.Add("set", new FunctionRecord("set", true, 3, null));
        }
        void VisitChildren(Node node) {
            foreach (var child in node)
            {
                Visit((dynamic) child);
            }
        }


        public void Visit(Program node) {
            Visit((dynamic) node[0]);
        }

        public void Visit(DeclarationList node) {
            VisitChildren(node);
        }

        public void Visit(VarDef node) {
            var identifiers = node[0];
            foreach (var identifier in identifiers)
            {
                var variableName = identifier.AnchorToken.Lexeme;
                if (TableVariables.Contains(variableName)) {
                    throw new SemanticError(
                        "Duplicated variable: " + variableName, identifier.AnchorToken
                    );
                } else {
                    // TODO: Revisar esto
                    TableVariables.Add(variableName);
                }
            }
        }

        public void Visit(Function node) {
            var functionName = node.AnchorToken.Lexeme;
            var arity = node[0].ChildrenLength;
            
            if (TableFunctions.ContainsKey(functionName)) {
                throw new SemanticError(
                    "Duplicated function: " + functionName, node.AnchorToken
                );
            } else {
                TableFunctions.Add(functionName, new FunctionRecord(functionName, false, arity, null));
            }   
        }
    }

    class SecondSemanticVisitor {
        
    }
}