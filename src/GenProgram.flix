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

namespace FlixInteractive/GenProgram {

    // TODO - should output something encloseing the answer so it is easier to recognize...

    def genMain(expr: String): String = 
        String.unlines(
            "def main(): Unit & Impure = "
            :: ("    let ans = " + expr + ";")
            :: "    Console.printLine(ans)"
            :: Nil
        )

    def doubleQuote(s: String): String = Char.toString('"') + s + Char.toString('"')

    def funcall(name: String, body: String): String = name + "(" + body + ")"

    pub def genWithoutType(expr: String): String = genMain(expr)


    pub def genWithType(expr: String, typeName: String): String = 
        match FlixInteractive/Typing.getPrinter(typeName) {
            case None => { let body = doubleQuote("<<" + typeName + ">>"); genMain(body)}
            case Some(fn) => { let body = funcall(fn, expr); genMain(body)}
        }

}
