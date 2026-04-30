
open System.IO
open Nand2Tetris

[<EntryPoint>]
let main argv =
    if argv.Length = 0 then
        printfn "Usage: VMTranslator <path_to_file_or_directory>"
        1 
    else
        let inputPath = argv.[0]

        // Check if the input is a directory or a single file
        let isDirectory = Directory.Exists(inputPath)
        let isFile = File.Exists(inputPath)

        if not isDirectory && not isFile then
            printfn "Error: Path not found."
            1
        else
            // Determine the output .asm file path and the list of .vm files to translate
            let outputPath, vmFiles = 
                if isDirectory then
                    // Output file named after the directory (e.g., DirName/DirName.asm)
                    let dirName = (new DirectoryInfo(inputPath)).Name
                    Path.Combine(inputPath, dirName + ".asm"), Directory.GetFiles(inputPath, "*.vm")
                else
                    // Output file with the same name as the single .vm file
                    Path.ChangeExtension(inputPath, ".asm"), [| inputPath |]

            let codeWriter = new CodeWriter(outputPath)
            
            // NEW ADDITION: Write bootstrap code only if we are processing a directory (full program)
            // Or if the assignment specifically requires it for all files in the FunctionCalls directory.
            if isDirectory || vmFiles.Length > 1 then
                codeWriter.WriteInit()

            // Translate each .vm file
            for file in vmFiles do
                // Update the current file name in the CodeWriter for proper static variable mapping
                let currentName = Path.GetFileNameWithoutExtension(file)
                codeWriter.SetFileName(currentName)
                
                let lines = File.ReadAllLines(file)
                for line in lines do
                    match Parser.parseLine line with
                    | Some(cmd) -> 
                        match cmd with
                        | Arithmetic(op)          -> codeWriter.WriteArithmetic(op)
                        | Push(_, _) | Pop(_, _)  -> codeWriter.WritePushPop(cmd)
                        | Label(lbl)              -> codeWriter.WriteLabel(lbl)   
                        | Goto(lbl)               -> codeWriter.WriteGoto(lbl)  
                        | IfGoto(lbl)             -> codeWriter.WriteIf(lbl)   
                        | Function(name, nLocals) -> codeWriter.WriteFunction(name, nLocals) 
                        | Call(name, nArgs)       -> codeWriter.WriteCall(name, nArgs)             
                        | Return                  -> codeWriter.WriteReturn()                                 
                        | _ -> ()
                    | None -> ()

            codeWriter.Close()
            printfn "Success! ASM file created: %s" outputPath
            0 // Success