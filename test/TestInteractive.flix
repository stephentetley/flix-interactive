use FlixInteractive/GenProgram.{genWithoutType, genWithType};

// @test
// def test01(): Unit & Impure = 
//     let s = genWithoutType("3 + 4");
//     Console.printLine(s)

// @test
// def test02(): Unit & Impure = 
//     let s = genWithType("3 + 4", "Int32");
//     Console.printLine(s)

// @test
// def test03(): Unit & Impure = 
//     let s = genWithType("1 :: Nil", "List[Int32]");
//     Console.printLine(s)

// @test
// def test04(): FlixInteractive/DecodeEval.EvalAnswer = 
//     FlixInteractive/DecodeEval/EvalAnswer.TypeEvaluation("Int32")

// @test
// def test05(): String & Impure = 
//     use Result.flatMap;
//     let step = () -> {
//         let path = System/FilePath.new("E:/coding/flix/flix-interactive/notes/err5.txt");
//         let* content = System/File.readFile(path, Text/Charset.utf_8());
//         let* ans = FlixInteractive/DecodeEval.decodeEval("", content);
//         Ok(FlixInteractive/DecodeEval.showMessage(ans))
//     };
//     step() |> x -> Result.getWithDefault(x, "** bad **")

// @test
// def test06(): String & Impure = 
//     use Result.flatMap;
//     let step = () -> {
//         let path = System/FilePath.new("E:/coding/flix/flix-interactive/notes/err1.txt");
//         let* content = System/File.readFile(path, Text/Charset.utf_8());
//         let* ans = FlixInteractive/DecodeEval.decodeEval(content, "");
//         Ok(FlixInteractive/DecodeEval.showMessage(ans))
//     };
//     step() |> x -> Result.getWithDefault(x, "** bad **")   


// @test
// def test07(): String & Impure = 
//     use Result.flatMap;
//     let step = () -> {
//         let path = System/FilePath.new("E:/coding/flix/flix-interactive/notes/err2.txt");
//         let* content = System/File.readFile(path, Text/Charset.utf_8());
//         let* ans = FlixInteractive/DecodeEval.decodeEval(content, "");
//         Ok(FlixInteractive/DecodeEval.showMessage(ans))
//     };
//     step() |> x -> Result.getWithDefault(x, "** bad **")


// @test
// def test08(): String & Impure = 
//     use Result.flatMap;
//     let step = () -> {
//         let path = System/FilePath.new("E:/coding/flix/flix-interactive/notes/err3.txt");
//         let* content = System/File.readFile(path, Text/Charset.utf_8());
//         let* ans = FlixInteractive/DecodeEval.decodeEval(content, "");
//         Ok(FlixInteractive/DecodeEval.showMessage(ans))
//     };
//     step() |> x -> Result.getWithDefault(x, "** bad **")   

// @test
// def test09(): String & Impure = 
//     use Result.flatMap;
//     let step = () -> {
//         let path = System/FilePath.new("E:/coding/flix/flix-interactive/notes/err7.txt");
//         let* content = System/File.readFile(path, Text/Charset.utf_8());
//         let* ans = FlixInteractive/DecodeEval.decodeEval(content, "");
//         Ok(FlixInteractive/DecodeEval.showMessage(ans))
//     };
//     step() |> x -> Result.getWithDefault(x, "** bad **")   

/// @test
/// def main01(): Int32 & Impure = 
///     use FlixInteractive/TopLevel.{runTopLevel, loop};
///     runTopLevel(loop())

