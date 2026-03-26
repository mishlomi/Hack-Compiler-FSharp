
open System.IO
open Nand2Tetris

[<EntryPoint>]
let main argv =

    //Michalws path
    let folderPath = @"C:\NandToTetris\nand2tetris\projects\07\MemoryAccess\PointerTest"
    
    //Naomiws path
    //let folderPath :string = @"C:\NandToTetris\nand2tetris\nand2tetris\projects\7\MemoryAccess\BasicTest"

    let inputPath = Path.Combine(folderPath, "PointerTest.vm")
    let outputPath = Path.Combine(folderPath, "PointerTest.asm")

    if File.Exists(inputPath) then
        let lines = File.ReadAllLines(inputPath)
        let codeWriter = new CodeWriter(outputPath)

        for line in lines do
            match Parser.parseLine line with
            | Some(cmd) -> 
                match cmd with
                | Arithmetic(op) -> codeWriter.WriteArithmetic(op)
                | Push(_, _) | Pop(_, _) -> codeWriter.WritePushPop(cmd)
                | Unknown -> ()
            | None -> ()

        codeWriter.Close()
        printfn "Success! Created ASM file at: %s" outputPath
    else
        printfn "Error: File not found at %s" inputPath
    
    0
