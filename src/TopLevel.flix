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

use System.FilePath;
use Text/PrettyPrint.{Doc, writePretty!};

use FlixInteractive/ExecFlix.FlixAnswer;

namespace FlixInteractive/TopLevel {

    type alias State = { cwd: FilePath, pathToFlixJar: FilePath }

    pub opaque type TopLevel[a] = State ~> Result[(a, State), System.Error]

    pub def runTopLevel(x: TopLevel[a]): Result[a, System.Error] & Impure = 
        let wdPath = System/FilePath.new("e:/coding/flix/temp");
        let flixPath = System/FilePath.new("../flix.jar" );
        let state = { cwd = wdPath, pathToFlixJar = flixPath};
        match apply1(x, state) {
            case Ok((a, _)) => Ok(a)
            case Err(e) => Err(e)
        }


    pub def return(x: a): TopLevel[a] = 
        TopLevel(st -> Ok((x,st)) as & Impure)

    def apply1(f: TopLevel[a], st: State): Result[(a, State), System.Error] & Impure = 
        let TopLevel(fn) = f;
        fn(st)

    pub def map(f: a -> b, x: TopLevel[a]): TopLevel[b] = 
        TopLevel(st -> 
            match apply1(x, st) {
                case Ok((a, st1)) => { let b = f(a); Ok(b, st1) }
                case Err(e) => Err(e)
            }        
        )

    pub def ap(fn: TopLevel[a -> b], x: TopLevel[a]): TopLevel[b] = 
        TopLevel(st -> 
            match apply1(fn, st) {
                case Ok((f, st1)) => match apply1(x, st1) {
                    case Ok((a, st2)) => Ok((f(a), st2))
                    case Err(e) => Err(e)
                }
                case Err(e) => Err(e)
            }        
        )

    pub def bind(x: TopLevel[a], k: a -> TopLevel[b]): TopLevel[b] = 
        TopLevel(st -> 
            match apply1(x, st) { 
                case Ok((a, st1)) => { let mk = k(a); apply1(mk, st1) }
                case Err(e) => Err(e)
            }
        )

    pub def flatMap(k: a -> TopLevel[b], x: TopLevel[a]): TopLevel[b] = bind(x, k)


    def liftAction(action: Unit ~> a): TopLevel[a] = 
        TopLevel(st -> { let a = action(); Ok((a, st)) })


    def liftResult(action: Unit ~> Result[a, System.Error]): TopLevel[a] = 
        TopLevel(st -> 
            match action() { 
                case Ok(a) => Ok(a, st)
                case Err(e) => Err(e)
            }
        )

    def setWorkingDirectory!(x: FilePath): TopLevel[Unit] = 
        TopLevel(st -> 
            let st1 = { cwd = x | st};
            Ok((), st1) as & Impure
        )

    def getWorkingDirectory(): TopLevel[FilePath] = 
        TopLevel(st -> Ok((st.cwd, st)) as & Impure)

    def getFlixJarPath(): TopLevel[FilePath] = 
        TopLevel(st -> Ok((st.pathToFlixJar, st)) as & Impure)


    def consolePrint(s: String): TopLevel[Unit] = 
        liftAction(_ -> Console.printLine(s))

    def consolePrintPrompt(): TopLevel[Unit] = 
        liftAction(_ -> Console.print("flix-interactive> "))


    enum Command {
        case Cwd(String),
        case Exec(String),
        case Help,
        case Pwd,
        case Quit
        case Unknown(String)
    }


    def decodeInput(input: String): Command = 
        use Result.flatMap;
        use Text/Regex/MatchEvaluator.{tuple2, getCaptureByName};
        let step = () -> {
            let* patt = Text/Regex/Pattern.compile(":(?<cmd>\w+)(?<rest>.*)");
            let* (cmd, rest) = Text/Regex.evaluateMatch(patt, tuple2(getCaptureByName("cmd"), getCaptureByName("rest")), input) as & Pure;
            match cmd { 
                case "cwd"  => Ok(Cwd(rest))
                case "help" => Ok(Help)
                case "pwd"  => Ok(Pwd)
                case "quit" => Ok(Quit)
                case _      => Ok(Unknown(cmd))
            }
        };
        step() |> x -> Result.getWithDefault(x, Exec(input))

    pub def loop(): TopLevel[Unit] = 
        let* _      = consolePrintPrompt();
        let* input  = liftAction(_ -> Console.readLine()) |> map(x -> Option.getWithDefault(x, ""));
        match decodeInput(input) {
            case Cwd(dir) => { 
                let* path   = liftAction(_ -> System/FilePath.new(dir));
                let* _      = setWorkingDirectory!(path);
                loop()
            }
            case Exec(expr) => { 
                let* ans    = compileAndRun(expr);
                let* _      = consolePrint(ans);
                loop()
            }
            case Help => {
                let* _      = consolePrint("Type an expression or :quit to quit");
                loop()
            }
            case Pwd => { 
                let* cwd    = getWorkingDirectory();
                let* ans    = liftAction(_ -> System/FilePath.toString(cwd));
                let* _      = consolePrint(ans);
                loop()
            }
            case Quit => return()
            case Unknown(s) => {
                let* _ = consolePrint("Unknown command: " + s);
                loop()
            }
        }


    def writeMain(program: Doc): TopLevel[Unit] = 
        let* cwd    = getWorkingDirectory();
        let* file1  = liftAction(_ -> System/FilePath.new("Main.flix"));
        let* path   = liftAction(_ -> System/FilePath.append(cwd, file1));
        let* spath  = liftAction(_ -> System/FilePath.toString(path));
        let* _      = liftResult(_ -> Text/PrettyPrint.writePretty!(160, spath, Text/Charset.utf_8(), program));
        return()

    

    def compileAndRun(expr: String): TopLevel[String] = 
        let prgm1   = FlixInteractive/GenProgram.genWithoutType(expr);
        let* _      = writeMain(prgm1);
        let* ans1   = execFlix();
        // let* _      = consolePrint("stdout: <<<" + ans1.stdOut + ">>>");
        // let* _      = consolePrint("stderr: <<<" + ans1.stdErr + ">>>");
        // let* _      = consolePrint("decodeEval...");
        let* eval1  = liftResult(_ -> FlixInteractive/DecodeEval.decodeEval(ans1.stdOut, ans1.stdErr));
        // let* _      = consolePrint("decodeEval done");
        let* _      = consolePrint(FlixInteractive/DecodeEval.showMessage(eval1));
        match eval1 {
            case FlixInteractive/DecodeEval/EvalAnswer.ValidAnswer(s) =>  return(s)
            case FlixInteractive/DecodeEval/EvalAnswer.TypeEvaluation(typename) => { 
                let prgm2   = FlixInteractive/GenProgram.genWithType(expr, typename);
                let* _      = writeMain(prgm2);
                let* ans2   = execFlix();
                return(ans2.stdOut)
            }
            case e => {
                let str = FlixInteractive/DecodeEval.showMessage(e);
                return(str)
            }
        }
        
    def execFlix(): TopLevel[FlixAnswer] = 
        let* cwd    = getWorkingDirectory();
        let* jar    = getFlixJarPath();
        let* main1  = liftAction(_ -> System/FilePath.new("Main.flix"));
        liftResult(_ -> FlixInteractive/ExecFlix.execFlix(cwd, jar, main1 :: Nil))

}    