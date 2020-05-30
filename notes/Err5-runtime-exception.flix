/// This generates a runtime exception. The Java error is reported on stderr.

def main(): Unit & Impure = 
    let ans = Char.toString(String.charAt(4, "yes"));
    Console.printLine(ans)