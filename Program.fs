
open System.IO
open Nand2Tetris


[<EntryPoint>]
let main argv =
    // Check if the user provided a file path as an argument
    if argv.Length = 0 then
        printfn "Usage: VMTranslator <path_to_file.vm>"
        1 
    else
        let inputPath = argv.[0]

        // Check if the file exists and has the correct extension
        if File.Exists(inputPath) && Path.GetExtension(inputPath) = ".vm" then
            
            // Create the output path by changing .vm to .asm
            let outputPath = Path.ChangeExtension(inputPath, ".asm")
            let codeWriter = new CodeWriter(outputPath)
            
            // Read and translate each line
            let lines = File.ReadAllLines(inputPath)
            for line in lines do
                match Parser.parseLine line with
                | Some(cmd) -> 
                    match cmd with
                    | Arithmetic(op) -> codeWriter.WriteArithmetic(op)
                    | Push(_, _) | Pop(_, _) -> codeWriter.WritePushPop(cmd)
                    | _ -> ()
                | None -> ()

            codeWriter.Close()
            printfn "Success! ASM file created: %s" outputPath
            0 // Success
        else
            printfn "Error: Please provide a valid .vm file path."
            1