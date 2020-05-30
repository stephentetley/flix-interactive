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

namespace FlixInteractive/Typing {

    /// TODO - obviously we will need something like a type representation
    /// and a generic show function. But that's complicated so later...
    def printers(): Map[String, String] = 
        Map#{ "String"      -> "identity"
            , "Char"        -> "Char.toString"
            , "Int16"       -> "Int16.toString"
            , "Int16"       -> "Int16.toString"
            , "Int32"       -> "Int32.toString"
            , "Int64"       -> "Int64.toString"
            , "BigInt"      -> "BigInt.toString"
            , "Float32"     -> "Float32.toString"
            , "Float64"     -> "Float64.toString"
            }

    pub def getPrinter(typeName: String): Option[String] = 
        Map.get(typeName, printers())

}