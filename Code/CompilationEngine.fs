namespace Nand2Tetris

open System.IO

type CompilationEngine(tokenizer: JackTokenizer, outputPath: string) =
    let writer = new StreamWriter(outputPath)
    let mutable indentLevel = 0

    // Computes the required indentation padding (2 spaces per depth level)
    let getIndent () = String.replicate (indentLevel * 2) " "

    // Escapes markup characters to ensure valid XML output syntax
    let escapeXml (value: string) =
        match value with
        | "<" -> "&lt;"
        | ">" -> "&gt;"
        | "\"" -> "&quot;"
        | "&" -> "&amp;"
        | _ -> value

    // Writes a text line to the output file prefixed with the current indentation level
    let writeLineWithIndent text =
        writer.WriteLine(getIndent() + text)

    // Opens a structural XML tag and increments the hierarchical depth
    let openTag tag =
        writeLineWithIndent $"<{tag}>"
        indentLevel <- indentLevel + 1

    // Closes a structural XML tag and decrements the hierarchical depth
    let closeTag tag =
        indentLevel <- indentLevel - 1
        writeLineWithIndent $"</{tag}>"

    // Processes a terminal token by writing its XML representation and advancing the tokenizer stream
    let processTerminal () =
        match tokenizer.CurrentToken with
        | Some token ->
            let tVal = escapeXml token.Value
            match token.Type with
            | Keyword -> writeLineWithIndent $"<keyword> {tVal} </keyword>"
            | Symbol -> writeLineWithIndent $"<symbol> {tVal} </symbol>"
            | Identifier -> writeLineWithIndent $"<identifier> {tVal} </identifier>"
            | IntConst -> writeLineWithIndent $"<integerConstant> {tVal} </integerConstant>"
            | StringConst -> writeLineWithIndent $"<stringConstant> {tVal} </stringConstant>"
            if tokenizer.HasMoreTokens() then tokenizer.Advance()
        | None -> ()

   // Look-ahead helper to check the value of the current token without consuming it
    let currentVal () =
        match tokenizer.CurrentToken with
        | Some t -> t.Value
        | None -> ""

    // Look-ahead helper to check the type of the current token without consuming it
    let currentType () =
        match tokenizer.CurrentToken with
        | Some t -> t.Type
        | None -> Keyword


    // --- Recursive Compilation Functions  ---

    // Compiles a complete class structure: 'class' className '{' classVarDec* subroutineDec* '}'
    let rec compileClass () =
        openTag "class"
        processTerminal() // 'class'
        processTerminal() // className
        processTerminal() // '{'

        // Loop to compile optional static or field variable declarations
        while currentVal() = "static" || currentVal() = "field" do
            compileClassVarDec()

        // Loop to compile optional constructors, functions, or methods
        while currentVal() = "constructor" || currentVal() = "function" || currentVal() = "method" do
            compileSubroutine()

        processTerminal() // '}'
        closeTag "class"

    // Compiles a class variable declaration: ('static' | 'field') type varName (',' varName)* ';'
    and compileClassVarDec () =
        openTag "classVarDec"
        processTerminal() // 'static' or 'field'
        processTerminal() // type
        processTerminal() // varName
        while currentVal() = "," do
            processTerminal() // ','
            processTerminal() // varName
        processTerminal() // ';'
        closeTag "classVarDec"

    // Compiles a complete method, function, or constructor subroutine declaration
    and compileSubroutine () =
        openTag "subroutineDec"
        processTerminal() // 'constructor' | 'function' | 'method'
        processTerminal() // 'void' | type
        processTerminal() // subroutineName
        processTerminal() // '('
        compileParameterList()
        processTerminal() // ')'
        
        // Compile the block container for the subroutine's execution logic
        openTag "subroutineBody"
        processTerminal() // '{'
        while currentVal() = "var" do
            compileVarDec()
        compileStatements()
        processTerminal() // '}'
        closeTag "subroutineBody"
        closeTag "subroutineDec"

    // Compiles a comma-separated list of incoming parameters, excluding the outer parentheses
    and compileParameterList () =
        openTag "parameterList"
        if currentVal() <> ")" then
            processTerminal() // type
            processTerminal() // varName
            while currentVal() = "," do
                processTerminal() // ','
                processTerminal() // type
                processTerminal() // varName
        closeTag "parameterList"

    // Compiles a local variable declaration statement inside a subroutine: 'var' type varName (',' varName)* ';'
    and compileVarDec () =
        openTag "varDec"
        processTerminal() // 'var'
        processTerminal() // type
        processTerminal() // varName
        while currentVal() = "," do
            processTerminal() // ','
            processTerminal() // varName
        processTerminal() // ';'
        closeTag "varDec"

    // Compiles a sequence of statements, matching valid statement leading keywords
    and compileStatements () =
        openTag "statements"
        let statementsKeywords = Set.ofList ["let"; "if"; "while"; "do"; "return"]
        while statementsKeywords.Contains(currentVal()) do
            match currentVal() with
            | "let" -> compileLet()
            | "if" -> compileIf()
            | "while" -> compileWhile()
            | "do" -> compileDo()
            | "return" -> compileReturn()
            | _ -> ()
        closeTag "statements"

    // Compiles an assignment statement: 'let' varName ('[' expression ']')? '=' expression ';'
    and compileLet () =
        openTag "letStatement"
        processTerminal() // 'let'
        processTerminal() // varName
        if currentVal() = "[" then
            processTerminal() // '['
            compileExpression()
            processTerminal() // ']'
        processTerminal() // '='
        compileExpression()
        processTerminal() // ';'
        closeTag "letStatement"

    // Compiles an evaluation block: 'if' '(' expression ')' '{' statements '}' ('else' '{' statements '}')?
    and compileIf () =
        openTag "ifStatement"
        processTerminal() // 'if'
        processTerminal() // '('
        compileExpression()
        processTerminal() // ')'
        processTerminal() // '{'
        compileStatements()
        processTerminal() // '}'
        if currentVal() = "else" then
            processTerminal() // 'else'
            processTerminal() // '{'
            compileStatements()
            processTerminal() // '}'
        closeTag "ifStatement"

    // Compiles a repetitive block structure: 'while' '(' expression ')' '{' statements '}'
    and compileWhile () =
        openTag "whileStatement"
        processTerminal() // 'while'
        processTerminal() // '('
        compileExpression()
        processTerminal() // ')'
        processTerminal() // '{'
        compileStatements()
        processTerminal() // '}'
        closeTag "whileStatement"

    // Compiles an invocation subroutine call statement: 'do' subroutineCall ';'
    and compileDo () =
        openTag "doStatement"
        processTerminal() // 'do'
        processTerminal() // subroutineName ou className/varName
        if currentVal() = "." then
            processTerminal() // '.'
            processTerminal() // subroutineName
        processTerminal() // '('
        compileExpressionList()
        processTerminal() // ')'
        processTerminal() // ';'
        closeTag "doStatement"

    // Compiles a subroutine termination sequence: 'return' expression? ';'
    and compileReturn () =
        openTag "returnStatement"
        processTerminal() // 'return'
        if currentVal() <> ";" then
            compileExpression()
        processTerminal() // ';'
        closeTag "returnStatement"

    // Compiles an operational expression unit: term (op term)*
    and compileExpression () =
        openTag "expression"
        compileTerm()
        // Gère la chaîne d'opérateurs binaires (ex: x + y - z)
        while (Set.ofList ["+"; "-"; "*"; "/"; "&"; "|"; "<"; ">"; "="]).Contains(currentVal()) do
            processTerminal() // op
            compileTerm()
        closeTag "expression"

    // Compiles an atomic expression segment (term) resolving primitives, symbols, or variable tracking
    and compileTerm () =
        openTag "term"
        if currentType() = IntConst || currentType() = StringConst then
            processTerminal()
        elif currentType() = Keyword then
            // true, false, null, this
            processTerminal()
        elif currentVal() = "(" then
            processTerminal() // '('
            compileExpression()
            processTerminal() // ')'
        elif currentVal() = "-" || currentVal() = "~" then
            processTerminal() // unaryOp (Mathematical negation or logical NOT)
            compileTerm()
        elif currentType() = Identifier then
            // Complex identifier evaluation (Look-ahead required to differentiate variable, array, or method call)
            processTerminal() // Consumes the leading token (varName, className, or subroutineName)
            
            if currentVal() = "[" then
                // Resolves Array Syntax: varName[expression]
                processTerminal() // '['
                compileExpression()
                processTerminal() // ']'
            elif currentVal() = "(" then
                // Resolves Direct Subroutine Call: subroutineName(expressionList)
                processTerminal() // '('
                compileExpressionList()
                processTerminal() // ')'
            elif currentVal() = "." then
                // Resolves Dotted Object Call: (className | varName).subroutineName(expressionList)
                processTerminal() // '.'
                processTerminal() // subroutineName
                processTerminal() // '('
                compileExpressionList()
                processTerminal() // ')'
        closeTag "term"

    // Compiles a comma-separated list of passed evaluation arguments inside call parameters
    and compileExpressionList () =
        openTag "expressionList"
        if currentVal() <> ")" then
            compileExpression()
            while currentVal() = "," do
                processTerminal() // ','
                compileExpression()
        closeTag "expressionList"

    // Main entry point method to kick off the top-down parsing execution
    member this.Compile() =
        if tokenizer.HasMoreTokens() then
            tokenizer.Advance() // Prime the stream to load the first token state
            compileClass()
        writer.Close()  // Safe handle closure to release written text payload