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

namespace FlixInteractive/ExecFlix {


    type alias FlixAnswer = { exitCode: Int32, stdOut: String, stdErr: String }

    pub def execFlix(workingDirectory: FilePath, jarPath: System.FilePath, sources: List[System.FilePath]): Result[FlixAnswer, System.Error] & Impure = 
        use Result.flatMap;
        use System/Process.{directory!, start, getInput, waitFor};
        let resolveToString = x -> System/FilePath.append(workingDirectory, x) |> System/FilePath.toString;
        let jar     = System/FilePath.toString(jarPath);
        let args    = ("-jar" :: jar :: List.map(resolveToString, sources)) |> List.toArray;
        let pb      = System/Process.newProcessBuilder("java", args);
        let _       = pb.directory!(workingDirectory);
        let* proc   = start(pb);
        let* output = getInput(proc, Text/Charset.utf_8());
        let* errors = getInput(proc, Text/Charset.utf_8());
        let* exit   = waitFor(proc);
        Ok({ exitCode = exit, stdOut = output, stdErr = errors })


}