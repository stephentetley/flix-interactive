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






namespace FlixInteractive/TopLevel {

    
    use FlixSandbox/System.FilePath;
    use Text/PrettyPrint.{Doc, writePretty!};

    use FlixInteractive/ExecFlix.FlixAnswer;

    type alias State = 
        { cwd :: FilePath
        , pathToFlixJar :: FilePath
        , modulePaths :: List[FilePath]
        }

    pub enum TopLevel[a](State -> Result[(a, State), String] & Impure)

    pub def runTopLevel(x: TopLevel[a]): Result[a, String] & Impure = 
        use Result.flatMap;
        let* wdPath = FlixSandbox/System/Directory.getCurrentDirectory();
        let temp1 = FlixSandbox/System/FilePath.filePath("flix.jar");
        let flixPath = FlixSandbox/System/FilePath.append(wdPath, temp1);
        let state = { cwd = wdPath, pathToFlixJar = flixPath, modulePaths = Nil};
        match apply1(x, state) {
            case Ok((a, _)) => Ok(a)
            case Err(e) => Err(e)
        }


    

    pub def return(x: a): TopLevel[a] = 
        TopLevel(st -> Ok((x,st)) as & Impure)

    def apply1(f: TopLevel[a], st: State): Result[(a, State), String] & Impure = 
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


    def liftAction(action: Unit -> a & Impure): TopLevel[a] = 
        TopLevel(st -> { let a = action(); Ok((a, st)) })


    def liftResult(action: Unit -> Result[a, String] & Impure): TopLevel[a] = 
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

    def addModule!(x: FilePath): TopLevel[Unit] = 
        TopLevel(st -> 
            let xs = st.modulePaths;
            let st1 = { modulePaths = x :: xs | st};
            Ok((), st1) as & Impure
        )

    def removeModule!(x: FilePath): TopLevel[Unit] = 
        let notequals = y -> not (x == y);
        TopLevel(st -> 
            let xs = st.modulePaths;
            let xs1 = List.filter(notequals, xs);
            let st1 = { modulePaths = xs1 | st};
            Ok((), st1) as & Impure
        )

    def getModules(): TopLevel[List[FilePath]] = 
        TopLevel(st -> 
            let xs = st.modulePaths;
            Ok(xs, st) as & Impure
        )

    def getWorkingDirectory(): TopLevel[FilePath] = 
        TopLevel(st -> Ok((st.cwd, st)) as & Impure)

    def getFlixJarPath(): TopLevel[FilePath] = 
        TopLevel(st -> Ok((st.pathToFlixJar, st)) as & Impure)


    def setFlixJarPath!(x: FilePath): TopLevel[Unit] = 
        TopLevel(st -> 
            let st1 = { pathToFlixJar = x | st};
            Ok((), st1) as & Impure
        )


    def consolePrint(s: String): TopLevel[Unit] = 
        liftAction(_ -> Console.printLine(s))

    def consolePrintPrompt(): TopLevel[Unit] = 
        liftAction(_ -> Console.print("flix> "))


    enum Command {
        case Cwd(String)
        case Exec(String)
        case Help
        case Load(String)
        case Pwd
        case Quit
        case SetFlixJar(String)
        case Unknown(String)
        case Unload(String)
    }


    def decodeInput(input: String): Command = 
        use Result.flatMap;
        use Applicative.{product};
        use Text/RegexEval/MatchEvaluator.{getCaptureNamed};
        let step = () -> {
            let* patt = Text/Regex/Pattern.compile(":(?<cmd>\\w+)(?<rest>.*)");
            let* (cmd, rest) = Text/RegexEval/Regex.matches(patt, product(getCaptureNamed("cmd"), getCaptureNamed("rest")), input) as & Pure;
            match cmd { 
                case "cwd"      => Ok(Cwd(rest))
                case "help"     => Ok(Help)
                case "h"        => Ok(Help)
                case "?"        => Ok(Help)
                case "load"     => Ok(Load(rest))
                case "pwd"      => Ok(Pwd)
                case "quit"     => Ok(Quit)
                case "q"        => Ok(Quit)
                case "setflix"  => Ok(SetFlixJar(rest))
                case "unload"   => Ok(Unload(rest))
                case _          => Ok(Unknown(cmd))
            }
        };
        step() |> Result.getWithDefault(Exec(input))

    pub def toplevelLoop(): TopLevel[Unit] = 
        let* _ = printBanner();
        loop()

    def loop(): TopLevel[Unit] = 
        let* _      = consolePrintPrompt();
        let* input  = liftAction(_ -> Console.readLine()) |> map(Option.getWithDefault(""));
        match decodeInput(input) {
            case Cwd(dir) => { 
                let path    = FlixSandbox/System/FilePath.filePath(dir);
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
            case Load(s) => { 
                let dir     = String.trim(s);
                let  path   = FlixSandbox/System/FilePath.filePath(dir);
                let* _      = addModule!(path);
                loop()
            }
            case Pwd => { 
                let* cwd    = getWorkingDirectory();
                let ans     = ToString.toString(cwd);
                let* _      = consolePrint(ans);
                loop()
            }
            case Quit => return()
            case SetFlixJar(s) => { 
                let jar     = String.trim(s);
                let path    = FlixSandbox/System/FilePath.filePath(jar);
                let* _      = setFlixJarPath!(path);
                loop()
            }
            
            case Unknown(s) => {
                let* _ = consolePrint("Unknown command: " + s);
                loop()
            }
            case Unload(s) => { 
                let dir     = String.trim(s);
                let path    = FlixSandbox/System/FilePath.filePath(dir);
                let* _      = removeModule!(path);
                loop()
            }
        }


    def writeMain(fileName: String, program: Doc): TopLevel[Unit] = 
        let* cwd    = getWorkingDirectory();
        let file1   = FlixSandbox/System/FilePath.filePath(fileName);
        let path    = FlixSandbox/System/FilePath.append(cwd, file1);
        /// let spath   = ToString.toString(path);
        let* _      = liftResult(_ -> Text/PrettyPrint.writePretty(160, path, FlixSandbox/Text/Charset.utf_8(), program));
        return()

    

    def compileAndRun(expr: String): TopLevel[String] = 
        let prgm1   = FlixInteractive/GenProgram.genProgramWithoutType(expr);
        let* _      = writeMain("InteractiveA.flix", prgm1);
        let* ans1   = execFlix("InteractiveA.flix");
        let* eval1  = liftResult(_ -> FlixInteractive/DecodeEval.decodeEval(ans1.stdOut, ans1.stdErr));
        match eval1 {
            case FlixInteractive/DecodeEval/EvalAnswer.ValidAnswer(s) =>  return(s)
            case FlixInteractive/DecodeEval/EvalAnswer.TypeEvaluation(typename) => { 
                let prgm2   = FlixInteractive/GenProgram.genProgramWithType(expr, typename);
                let* _      = writeMain("InteractiveB.flix", prgm2);
                let* ans2   = execFlix("InteractiveB.flix");
                return(ans2.stdOut)
            }
            case  FlixInteractive/DecodeEval/EvalAnswer.ParseError => {
                let* _ = consolePrint(ans1.stdOut);
                return("Parse Error")
            }
            case  FlixInteractive/DecodeEval/EvalAnswer.UnhandledError(s) => {
                let* _ = consolePrint(ans1.stdOut);
                return(s)
            }
            case e => {
                let str = FlixInteractive/DecodeEval.showMessage(e);
                return(str)
            }
        }
        
    def execFlix(fileName: String): TopLevel[FlixAnswer] = 
        let* cwd    = getWorkingDirectory();
        let* jar    = getFlixJarPath();
        let main1   = FlixSandbox/System/FilePath.filePath(fileName);
        let* paths  = getModules();
        liftResult(_ -> FlixInteractive/ExecFlix.execFlix(cwd, jar, main1 :: paths))

    def printBanner(): TopLevel[Unit] = 
        let xs = 
                " _____  _      _   _   _ "
            ::  "|  ___|| |    | | | | | |"
            ::  "| |    | |    | | | |_| |"
            ::  "| |__  | |    | | |_   _|"
            ::  "|  __| | |    | |   | |  "
            ::  "| |    | |    | |  _| |_ "
            ::  "| |    | |    | | |  _  |"
            ::  "| |    | |___ | | | | | |"
            ::  "|_|    |_____||_| |_| |_|  Interactive"
            ::  ""
            :: Nil;
        
        liftAction(_ -> List.foreach(Console.printLine, xs))


}    