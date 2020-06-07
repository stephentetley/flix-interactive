/*
 * Copyright 2020 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use Text/PrettyPrint.{Doc, text, hang, semi, line, tupled, dquotes};
use Text/PrettyPrint.{<>, <<>>, <&>};

namespace FlixInteractive/GenProgram {

    enum Printer { 
        case Simple(String),         // builtin call
        case WithHelper(String, Doc)
    }


    def string(s: String): Doc = 
        dquotes(text(s))


    def funcall(name: String, args: List[Doc]): Doc = 
        text(name) <> tupled(args)

    def genMain(expr: Doc): Doc = 
        hang(4, text("def main(): Unit & Impure =") 
            <&> text("let ans =") <<>> expr <> semi()
            <&> funcall("Console.printLine", string("~~~ {.answer}") :: Nil) <> semi()
            <&> funcall("Console.printLine", text("ans") :: Nil) <> semi()
            <&> funcall("Console.printLine", string("~~~") :: Nil) <> semi()
            <&> text("()")
        )

    
    pub def genProgramWithoutType(expr: String): Doc = genMain(text(expr))

    def blankLine(): Doc = Text/PrettyPrint.linebreak()


    pub def genProgramWithType(expr: String, typeName: String): Doc = 
        match getPrinter(typeName) {
            case None => { 
                let body = string("<<" + typeName + ">>"); 
                genMain(body)
            }
            case Some(Simple(name)) => { 
                let body = funcall(name, text(expr) :: Nil); 
                genMain(body)
            }
            case Some(WithHelper(name, helper)) => {
                let body = funcall(name, text(expr) :: Nil); 
                helper <&> blankLine() <&> genMain(body)
            }
        }


    
    /// TODO - obviously we will need something like a type representation
    /// and a generic show function. But that's complicated so later...
    def printers(): Map[String, Printer] = 
        Map#{ "String"      -> Simple("identity")
            , "Char"        -> Simple("Char.toString")
            , "Int16"       -> Simple("Int16.toString")
            , "Int16"       -> Simple("Int16.toString")
            , "Int32"       -> Simple("Int32.toString")
            , "Int64"       -> Simple("Int64.toString")
            , "BigInt"      -> Simple("BigInt.toString")
            , "Float32"     -> Simple("Float32.toString")
            , "Float64"     -> Simple("Float64.toString")
            , "Unit"        -> WithHelper("printUnit",   printUnit())
            , "Bool"        -> WithHelper("printBool",   printBool())
            }

    def printBool(): Doc = 
        hang(4, text("def printBool(x: Bool): String = ") 
            <&> text("if (x)") <<>> dquotes(text("true")) <<>> text("else") <<>> dquotes(text("false"))
        ) 

    def printUnit(): Doc = 
        text("def printUnit(_: Unit): String =") <<>> dquotes(text("()"))
        


    def getPrinter(typeName: String): Option[Printer] = 
        Map.get(typeName, printers())


}
