namespace Nand2Tetris

open System.IO

module JackAnalyzer =
    
    // Escapes special characters for a valid XML structure
    let escapeXml (value: string) =
        match value with
        | "<" -> "&lt;"
        | ">" -> "&gt;"
        | "\"" -> "&quot;"
        | "&" -> "&amp;"
        | _ -> value

    // PART 1: Generates the tokenizer output file (xxx.T.My.xml)
    let processTokensOnly (inputPath: string) =
        let outputPath = Path.ChangeExtension(inputPath, "T.My.xml")
        let tokenizer = new JackTokenizer(inputPath)
        use writer = new StreamWriter(outputPath)

        writer.WriteLine("<tokens>")
        while tokenizer.HasMoreTokens() do
            tokenizer.Advance()
            let tType = tokenizer.TokenType()
            let tVal = tokenizer.TokenValue() |> escapeXml
            
            match tType with
            | Keyword -> writer.WriteLine($"<keyword> {tVal} </keyword>")
            | Symbol -> writer.WriteLine($"<symbol> {tVal} </symbol>")
            | Identifier -> writer.WriteLine($"<identifier> {tVal} </identifier>")
            | IntConst -> writer.WriteLine($"<integerConstant> {tVal} </integerConstant>")
            | StringConst -> writer.WriteLine($"<stringConstant> {tVal} </stringConstant>")

        writer.WriteLine("</tokens>")
        printfn "Part 1 Success! Tokenizer XML created: %s" outputPath

    // PART 2 & PROJECT 11: Orchestrates the compilation pipeline
    let processFile (inputPath: string) =
        // Step A: Automatically run the Tokenizer phase first (Part 1)
        processTokensOnly inputPath
        
        // Step B: Initialize Project 11 structures
        let xmlOutputPath = Path.ChangeExtension(inputPath, "My.xml")
        let vmOutputPath = Path.ChangeExtension(inputPath, "vm") // Target .vm file
        
        let tokenizer = new JackTokenizer(inputPath)
        let symbolTable = new SymbolTable()
        let vmWriter = new VMWriter(vmOutputPath)
        
        // Step C: Pass all 4 arguments to the updated Compilation Engine
        let engine = new CompilationEngine(tokenizer, xmlOutputPath, vmWriter, symbolTable)
        
        // Execute the recursive parsing process
        engine.Compile()
        
        // Don't forget to close the VM writer stream!
        vmWriter.Close()
        
        printfn "Success! XML created: %s" xmlOutputPath
        printfn "Success! VM created: %s" vmOutputPath