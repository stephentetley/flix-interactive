/// Idea - generate an ill-typed program, run it to get the type error.
/// Identify the correct type from the error and re-run the generator with the type.

def main(): Unit & Impure = 
    let ans = Int32.toString(max(3,6));
    Console.printLine(ans)