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

namespace FlixInteractive/GenProgram {

    def <>(x: Doc, y: Doc): Doc = Text/PrettyPrint.beside(x,y)
    def <<>>(x: Doc, y:Doc): Doc = Text/PrettyPrint.besideSpace(x,y)
    def <&>(x: Doc, y: Doc): Doc = x <> line() <> y

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

    
    pub def genWithoutType(expr: String): Doc = genMain(text(expr))


    pub def genWithType(expr: String, typeName: String): Doc = 
        match FlixInteractive/Typing.getPrinter(typeName) {
            case None => { let body = string("<<" + typeName + ">>"); genMain(body)}
            case Some(fn) => { let body = funcall(fn, text(expr) :: Nil); genMain(body)}
        }

}
