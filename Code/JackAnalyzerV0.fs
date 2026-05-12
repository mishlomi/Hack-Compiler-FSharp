namespace Nand2Tetris

open System.IO

module JackAnalyzerV0 =
    
    // Escapes special characters for valid XML structure
    let escapeXml (value: string) =
        match value with
        | "<" -> "&lt;"
        | ">" -> "&gt;"
        | "\"" -> "&quot;"
        | "&" -> "&amp;"
        | _ -> value

    // Processes a single .jack file and generates an xxxT.xml output
    let processFile (inputPath: string) =
        let outputPath = Path.ChangeExtension(inputPath, "T.xml")
        // Note the "T.xml" suffix for the tokenizer output
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
        printfn "Success! Tokenizer XML created: %s" outputPath