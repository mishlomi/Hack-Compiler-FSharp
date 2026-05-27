namespace Nand2Tetris

open System
open System.IO
open System.Text.RegularExpressions

// Token types as defined in the Jack language grammar
type TokenType =
    | Keyword
    | Symbol
    | Identifier
    | IntConst
    | StringConst

type Token = {
    Type: TokenType
    Value: string
}

type JackTokenizer(inputFilePath: string) =
    // Read the entire file content to handle multiline block comments easily
    let rawText = File.ReadAllText(inputFilePath)
    
    // Helper function to remove both line comments and block comments
    let removeComments (text: string) =
        let blockCommentPattern = @"/\*[\s\S]*?\*/"
        let lineCommentPattern = @"//.*"
        let noBlock = Regex.Replace(text, blockCommentPattern, "")
        Regex.Replace(noBlock, lineCommentPattern, "")

    let cleanText = removeComments rawText
    let mutable currentPos = 0
    let mutable currentToken : Token option = None

    let keywords = 
        Set.ofList ["class"; "constructor"; "function"; "method"; "field"; 
                    "static"; "var"; "int"; "char"; "boolean"; "void"; 
                    "true"; "false"; "null"; "this"; "let"; "do"; 
                    "if"; "else"; "while"; "return"]

    let symbols = 
        Set.ofList ['{'; '}'; '('; ')'; '['; ']'; '.'; ','; ';'; '+'; '-'; '*'; '/'; '&'; '|'; '<'; '>'; '='; '~']

    // Advances the internal pointer past any whitespace characters
    let skipWhitespace () =
        while currentPos < cleanText.Length && Char.IsWhiteSpace(cleanText.[currentPos]) do
            currentPos <- currentPos + 1

    // Checks if there is more code to process
    member this.HasMoreTokens() =
        skipWhitespace ()
        currentPos < cleanText.Length

    // Evaluates the next part of the string and extracts the token
    member this.Advance() =
        if not (this.HasMoreTokens()) then
            currentToken <- None
        else
            let mutable matched = false
            let remaining = cleanText.Substring(currentPos)

            // 1. Match String Constant
            if remaining.StartsWith("\"") then
                let endIdx = remaining.IndexOf('"', 1)
                let strVal = remaining.Substring(1, endIdx - 1)
                currentToken <- Some { Type = StringConst; Value = strVal }
                currentPos <- currentPos + endIdx + 1
                matched <- true

            // 2. Match Symbol
            elif symbols.Contains(remaining.[0]) then
                currentToken <- Some { Type = Symbol; Value = remaining.[0].ToString() }
                currentPos <- currentPos + 1
                matched <- true

            // 3. Match Integer Constant
            elif Char.IsDigit(remaining.[0]) then
                let m = Regex.Match(remaining, @"^\d+")
                currentToken <- Some { Type = IntConst; Value = m.Value }
                currentPos <- currentPos + m.Length
                matched <- true

            // 4. Match Identifier or Keyword
            else
                let m = Regex.Match(remaining, @"^[a-zA-Z_]\w*")
                if m.Success then
                    let valStr = m.Value
                    if keywords.Contains(valStr) then
                        currentToken <- Some { Type = Keyword; Value = valStr }
                    else
                        currentToken <- Some { Type = Identifier; Value = valStr }
                    currentPos <- currentPos + m.Length
                    matched <- true

            // Safety fallback to prevent infinite loops on invalid characters
            if not matched then
                currentPos <- currentPos + 1

    member this.TokenType() = 
        match currentToken with
        | Some t -> t.Type
        | None -> failwith "No current token available"

    member this.TokenValue() =
        match currentToken with
        | Some t -> t.Value
        | None -> failwith "No current token available"


    member this.CurrentToken = currentToken
    // Exposes the currently active token to the CompilationEngine without advancing the stream