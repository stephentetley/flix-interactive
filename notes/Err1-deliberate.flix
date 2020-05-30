/// From `notes` directory...

/// powershell> java -jar ..\..\flix.jar Err1-deliberate.flix

/// Idea - generate an ill-typed program, run it to get the type error.
/// Identify the correct type from the error and re-run the generator with the type.

def main(): Unit & Impure = 
    let ans = 3 + 4;
    Console.printLine(ans)