
def main(): Result[Unit, System.Error] & Impure = 
    use FlixInteractive/TopLevel.{runTopLevel, loop};
    runTopLevel(loop())

