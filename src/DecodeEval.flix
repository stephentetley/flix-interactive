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

namespace FlixInteractive/DecodeEval {

    pub enum EvalAnswer {
        case ValidAnswer(String),                // text body
        case TypeEvaluation(String),            // TypeName
        case ResolutionError(String),           // Missing symbol
        case TypeError(String, String),         // TypeOne & TypeTwo
        case ParseError,
        case RuntimeError(String),
        case UnhandledError(String)
    }
    
    pub def showMessage(x: EvalAnswer): String = match x {
        case ValidAnswer(s)         => s
        case TypeEvaluation(s)      => "Type Evaluation: " + s
        case ResolutionError(s)     => "Resolution Error: " + s
        case TypeError(t1, t2)      => "Type Error: Unable to unify '" + t1 + "' and '" + t2 + "'"
        case ParseError             => "Parse Error:"
        case RuntimeError(s)        => "Runtime Error: " + s
        case UnhandledError(s)      => "Unhandled Error: " + s
    }



    pub def decodeEval(stdout: String, stderr: String): Result[EvalAnswer, System.Error] & Impure = 
        if (!String.isEmpty(stderr))
            decodeRuntimeError(stderr)
        else 
            decodeStdout(stdout)

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
    def decodeRuntimeError(stderr: String): Result[EvalAnswer, System.Error] & Impure = 
        use Result.flatMap;
        use Text/Regex/MatchEvaluator.{getCaptureByName};
        let* patt   = Text/Regex/Pattern.compile("^Exception in thread \u0022main\u0022 (?<message>.+)");
        let* ans    = Text/Regex.evaluateSubmatch(patt, getCaptureByName("message"), stderr);
        Ok(RuntimeError(ans))

    def decodeStdout(stdout: String): Result[EvalAnswer, System.Error] & Impure = 
        use Result.flatMap;
        let errname = getErrorName(stdout);
        match errname { 
            case Some("Parse Error") => Ok(ParseError)
            case Some("Resolution Error") => { 
                let* name = getUnresolvedName(stdout);
                Ok(ResolutionError(name))
            }
            case Some("Type Error") => { 
                let* type1 = getTypeOne(stdout);
                let* type2 = getTypeTwo(stdout);
                let* isExpected = cannotTypeOutput(stdout);
                if (isExpected)
                    Ok(TypeEvaluation(type2))
                else 
                    Ok(TypeError(type1, type2))
            }
            case Some(s) => Ok(UnhandledError(s))
            case None => Ok(ValidAnswer(stdout))
        }

    def getErrorName(stdout: String): Option[String] & Impure = 
        use Result.flatMap;
        use Text/Regex/MatchEvaluator.{getCaptureByName};
        let step = () -> {
            let* patt = Text/Regex/Pattern.compile("\u002D{2,} (?<errname>.+ Error) \u002D{50,}");
            Text/Regex.evaluateSubmatch(patt, getCaptureByName("errname"), stdout)
        }; 
        step() |> Result.toOption

        


    ///
    /// \u005B is left square bracket `[`
    /// \u001B is escape
    ///
    pub def getUnresolvedName(stdout: String): Result[String, System.Error] & Impure = 
        use Result.flatMap;
        use Text/Regex/Pattern.Flag.{Multiline};
        use Text/Regex/MatchEvaluator.{getCaptureByName};
        let* patt   = Text/Regex/Pattern.compileWithFlags("Undefined name '\u001B\u005B31m(?<undefined>.+)\u001B\u005B0m'", Multiline :: Nil);
        let* ans    = Text/Regex.evaluateSubmatch(patt, getCaptureByName("undefined"), stdout);
        Ok(ans)

    ///
    /// \u005B is left square bracket `[`
    /// \u001B is escape
    ///
    pub def getTypeOne(stdout: String): Result[String, System.Error] & Impure = 
        use Result.flatMap;
        use Text/Regex/Pattern.Flag.{Multiline};
        use Text/Regex/MatchEvaluator.{getCaptureByName};
        let* patt   = Text/Regex/Pattern.compileWithFlags("Type One: \u001B\u005B36m(?<name>.+)\u001B\u005B0m", Multiline :: Nil);
        let* ans    = Text/Regex.evaluateSubmatch(patt, getCaptureByName("name"), stdout);
        Ok(ans)

    ///
    /// \u005B is left square bracket `[`
    /// \u001B is escape
    ///
    pub def getTypeTwo(stdout: String): Result[String, System.Error] & Impure = 
        use Result.flatMap;
        use Text/Regex/Pattern.Flag.{Multiline};
        use Text/Regex/MatchEvaluator.{getCaptureByName};
        let* patt   = Text/Regex/Pattern.compileWithFlags("Type Two: \u001B\u005B35m(?<name>.+)\u001B\u005B0m", Multiline :: Nil);
        let* ans    = Text/Regex.evaluateSubmatch(patt, getCaptureByName("name"), stdout);
        Ok(ans)


    ///
    /// \u005B is left square bracket `[`
    /// \u001B is escape
    ///
    /// Cannot type output if we see `    Console.printLine(ans)` in stdout.
    ///
    def cannotTypeOutput(stdout: String): Result[Bool, System.Error] = 
        use Result.flatMap;
        use Text/Regex/Pattern.Flag.{Multiline};
        let* patt   = Text/Regex/Pattern.compileWithFlags("\u001B\u005B0m\s+Console\.printLine\(ans\)", Multiline :: Nil);
        let ans     = Text/Regex.isSubmatch(patt, stdout);
        Ok(ans)


}