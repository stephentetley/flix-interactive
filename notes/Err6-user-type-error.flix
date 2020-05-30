/// Type error in the user expression `3 + 4.0f64`

def main(): Unit & Impure = 
    let ans = 3 + 4.0f64 + '0';    
    Console.printLine(ans)