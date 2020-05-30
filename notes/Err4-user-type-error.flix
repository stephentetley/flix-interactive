/// Type error in the user expression `'C' + "olour"`

def main(): Unit & Impure = 
    let ans = 'C' + "olour";
    Console.printLine(ans)