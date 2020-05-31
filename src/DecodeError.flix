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
        case TypeEvaluation(String),            // TypeName
        case ResolutionError(String),           // Missing symbol
        case TypeError(String, String),        // TypeOne & TypeTwo
        case ParseError(String),
        case RuntimeError(String),
        case UnhandledError(String)
    }
    
    pub def showMessage(x: EvalError): String = match x {
        case TypeEvaluation(s)      => "Type Evaluation: " + s
        case ResolutionError(s)     => "Resolution Error: " + s
        case TypeError(t1, t2)      => "Type Error: Unable to unify '" + t1 + "' and '" + t2 + "'"
        case ParseError(s)          => "Parse Error:" + s
        case RuntimeError(s)        => "Runtime Error: " + s
        case UnhandledError(s)      => "Unhandled Error: " + s
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

    def assertNotMatch(pattern: String, input: String): Result[Unit, System.Error] = 
        use Result.flatMap;
        use Text/Regex/Pattern.Flag.{Multiline};
        let* patt   = Text/Regex/Pattern.compileWithFlags(pattern, Multiline :: Nil);
        let ans     = Text/Regex.isSubmatch(patt, input);
        if (!ans) 
            Ok()
        else
            Err(InputMismatchException)


    ///
    /// \u002D is dash `-`
    /// \u005B is left square bracket `[`
    /// \u001B is escape
    ///
    /// Must see the headline `Type Error` plus lines with `    let ans = ...` & `    Console.printLine(ans)`
    /// The we try to decode `Type Two`.
    ///
    pub def decodeTypeEvaluation(msg: String): Result[EvalError, System.Error] & Impure = 
        use Result.flatMap;
        use Text/Regex/Pattern.Flag.{Multiline};
        use Text/Regex/MatchEvaluator.{getCaptureByName};
        let* _      = assertMatch("\u002D{2,} Type Error \u002D{5,}", msg);
        let* _      = assertMatch("\u001B\u005B0m\s+let ans = ", msg);
        let* _      = assertMatch("\u001B\u005B0m\s+Console\.printLine\(ans\)", msg);
        let* patt   = Text/Regex/Pattern.compileWithFlags("Type Two: \u001B\u005B35m(?<exprtype>.+)\u001B\u005B0m", Multiline :: Nil);
        let* ans    = Text/Regex.evaluateSubmatch(patt, getCaptureByName("exprtype"), msg);
        Ok(TypeEvaluation(ans))

    ///
    /// \u002D is dash `-`
    /// \u003F is question mark `?`
    /// \u005B is left square bracket `[`
    /// \u001B is escape
    ///
    /// Must see the headline `Type Error` plus lines with `    let ans = ...` & `    Console.printLine(ans)`
    /// The we try to decode `Type Two`.
    ///
    pub def decodeResolutionError(msg: String): Result[EvalError, System.Error] & Impure = 
        use Result.flatMap;
        use Text/Regex/Pattern.Flag.{Multiline};
        use Text/Regex/MatchEvaluator.{getCaptureByName};
        let* _      = assertMatch("\u002D{2,} Resolution Error \u002D{5,}", msg);
        let* _      = assertMatch("Possible typo or non-existent definition\u003F", msg);
        let* patt   = Text/Regex/Pattern.compileWithFlags("Undefined name '\u001B\u005B31m(?<undefined>.+)\u001B\u005B0m'", Multiline :: Nil);
        let* ans    = Text/Regex.evaluateSubmatch(patt, getCaptureByName("undefined"), msg);
        Ok(ResolutionError(ans))

    ///
    /// \u002D is dash `-`
    /// \u005B is left square bracket `[`
    /// \u001B is escape
    ///
    /// Must see the headline `Type Error`.
    /// Must not see `    Console.printLine(ans)`
    /// The we try to decode `Type Two`.
    ///
    pub def decodeTypeError(msg: String): Result[EvalError, System.Error] & Impure = 
        use Result.flatMap;
        use Text/Regex/Pattern.Flag.{Multiline};
        use Text/Regex/MatchEvaluator.{getCaptureByName};
        let* _      = assertMatch("\u002D{2,} Type Error \u002D{5,}", msg);
        let* _      = assertNotMatch("\u001B\u005B0m\s+Console\.printLine\(ans\)", msg);
        let* patt1   = Text/Regex/Pattern.compileWithFlags("Type One: \u001B\u005B36m(?<typeone>.+)\u001B\u005B0m", Multiline :: Nil);
        let* ans1    = Text/Regex.evaluateSubmatch(patt1, getCaptureByName("typeone"), msg);
        let* patt2   = Text/Regex/Pattern.compileWithFlags("Type Two: \u001B\u005B35m(?<typetwo>.+)\u001B\u005B0m", Multiline :: Nil);
        let* ans2    = Text/Regex.evaluateSubmatch(patt2, getCaptureByName("typetwo"), msg);
        Ok(TypeError(ans1, ans2))


    ///
    /// \u002D is dash `-`
    /// \u005B is left square bracket `[`
    /// \u001B is escape
    ///
    /// Must see the headline `Type Error`.
    /// Must not see `    Console.printLine(ans)`
    /// The we try to decode `Type Two`.
    ///
    pub def decodeParseError(msg: String): Result[EvalError, System.Error] & Impure = 
        use Result.flatMap;
        use Text/Regex/Pattern.Flag.{Multiline};
        use Text/Regex/MatchEvaluator.{getCaptureByName};
        let* _      = assertMatch("\u002D{2,} Parse Error \u002D{5,}", msg);
        let* _      = assertNotMatch("\u001B\u005B0m\s+Console\.printLine\(ans\)", msg);
        Ok(ParseError("todo")) as & Impure



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
        Ok(RuntimeError(ans))

    def alt(x: Result[a,e], y: Result[a,e]): Result[a,e] = 
        match x { 
            case Ok(a) => Ok(a)
            case Err(_) => y
        }

    pub def decodeError(msg: String): Result[EvalError, System.Error] & Impure = 
        alt(decodeTypeEvaluation(msg), 
            alt(decodeResolutionError(msg), 
                alt(decodeTypeError(msg), 
                    alt(decodeParseError(msg), decodeRuntimeError(msg)))))


}