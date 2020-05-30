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

use System.Error.{InputMismatchException};

namespace FlixInteractive/DecodeError {

    pub enum EvalError {
        case TypeEvaluation({exprType: String}),
        case ResolutionError({undefinedName: String}),
        case TypeError({message: String}),
        case ParseError({message: String}),
        case RuntimeError({message: String}),
        case UnhandledError({message: String})
    }
    
    pub def showMessage(x: EvalError): String = match x {
        case TypeEvaluation(r)      => "Type Evaluation: " + r.exprType
        case ResolutionError(r)     => "Resultution Error: " + r.undefinedName
        case TypeError(r)           => "Type Error" + r.message
        case ParseError(r)          => "Parse Error:" + r.message
        case RuntimeError(r)        => "Runtime Error: " + r.message
        case UnhandledError(r)      => "Unhandled Error: " + r.message
    }

    def assertMatch(pattern: String, input: String): Result[Unit, System.Error] = 
        use Result.flatMap;
        use Text/Regex/Pattern.Flag.{Multiline};
        let* patt   = Text/Regex/Pattern.compileWithFlags(pattern, Multiline :: Nil);
        let ans     = Text/Regex.isSubmatch(patt, input);
        if (ans) 
            Ok()
        else
            Err(InputMismatchException)


    ///
    ///  Of the form: 
    ///
    /// > Exception in thread "main" java.lang.StringIndexOutOfBoundsException: String index out of range: 4
    /// >         at java.base/java.lang.StringLatin1.charAt(StringLatin1.java:47)
    /// >         ...
    ///  
    /// We want the firstline text
    ///
    /// \u0022 is the double quote character.
    ///
    pub def decodeRuntimeError(msg: String): Result[EvalError, System.Error] & Impure = 
        use Result.flatMap;
        use Text/Regex/MatchEvaluator.{getCaptureByName};
        let* patt   = Text/Regex/Pattern.compile("^Exception in thread \u0022main\u0022 (?<message>.+)");
        let* ans    = Text/Regex.evaluateSubmatch(patt, getCaptureByName("message"), msg);
        Ok(RuntimeError({message = ans}))

    /// \u001B is escape
    /// \u005B is left square bracket `[`
    pub def decodeTypeEvaluation(msg: String): Result[EvalError, System.Error] & Impure = 
        use Result.flatMap;
        use Text/Regex/Pattern.Flag.{Multiline};
        use Text/Regex/MatchEvaluator.{getCaptureByName};
        let* _      = assertMatch("    let ans = ", msg);
        let* _      = assertMatch("    Console\.printLine", msg);
        let* patt   = Text/Regex/Pattern.compileWithFlags("Type Two: \u001B\u005B35m(?<exprtype>.+)\u001B\u005B0m", Multiline :: Nil);
        let* ans    = Text/Regex.evaluateSubmatch(patt, getCaptureByName("exprtype"), msg);
        Ok(TypeEvaluation({exprType = ans}))
}