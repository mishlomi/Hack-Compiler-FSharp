
namespace Nand2Tetris

open System.IO

module JackAnalyzer = 
    
    // Escapes special characters for valid XML structure
    let escapeXml (value: string) =
        match value with
        | "<" -> "&lt;"
        | ">" -> "&gt;"
        | "\"" -> "&quot;"
        | "&" -> "&amp;"
        | _ -> value

    // PART 1: Generates the tokenizer output file (xxx.T.My.xml)
    let processTokensOnly (inputPath: string) =
    // Uses a unique suffix to prevent overwriting official comparison files
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
        printfn "Partie 1 Réussie ! Fichier des Tokens créé : %s" outputPath

    // PART 2: Orchestrates the compilation pipeline and generates the final syntax tree (xxx.My.xml)
    let processFile (inputPath: string) =
        // Step A: Automatically run the Tokenizer phase first
        processTokensOnly inputPath
        
        // Step B: Initialize the Compilation Engine to build the structured XML tree
        let outputPath = Path.ChangeExtension(inputPath, "My.xml") 
        let tokenizer = new JackTokenizer(inputPath)
        
        // Pass the fresh tokenizer instance and the safe output path to the engine
        let engine = new CompilationEngine(tokenizer, outputPath)
        
        // Execute the recursive parsing process
        engine.Compile()
        printfn "Partie 2 Réussie ! Fichier de Parsing créé : %s" outputPath