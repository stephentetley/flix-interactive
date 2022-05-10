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



namespace FlixInteractive/ExecFlix {

    use FlixSandbox/System.FilePath;

    pub type alias FlixAnswer = 
        { exitCode :: Int32
        , stdOut :: String
        , stdErr :: String 
        }

    pub def execFlix(workingDirectory: FilePath, jarPath: FilePath, sources: List[FilePath]): Result[FlixAnswer, String] & Impure = 
        use Result.flatMap;
        use FlixSandbox/System/Process.{directory!, start, getInput, waitFor};
        let resolveToString = x -> FlixSandbox/System/FilePath.append(workingDirectory, x) |> ToString.toString;
        let jar     = ToString.toString(jarPath);
        let args    = ("-jar" :: jar :: List.map(resolveToString, sources));
        let pb      = FlixSandbox/System/Process.processBuilder("java", args);
        let _       = directory!(pb, workingDirectory);
        let* proc   = start(pb);
        let* output = getInput(proc, FlixSandbox/Text/Charset.utf_8());
        let* errors = getInput(proc, FlixSandbox/Text/Charset.utf_8());
        let* exit   = waitFor(proc);
        Ok({ exitCode = exit, stdOut = output, stdErr = errors })


}