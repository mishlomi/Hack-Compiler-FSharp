namespace Nand2Tetris

open System.IO

// 
// JackTokenizer → CompilationEngine → VMWriter
//                        ↓
//                  SymbolTable

type CompilationEngine(tokenizer: JackTokenizer, outputPath: string, vmWriter: VMWriter, symbolTable: SymbolTable) =
    let writer = new StreamWriter(outputPath)
    //רמת הזחה
    let mutable indentLevel = 0
    let mutable className = "" // Cache the current class name for VM function naming
    let mutable whileLabelIndex = 0
    let mutable ifLabelIndex = 0
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
        
        // Save class name for later use in VM function definitions (e.g., Main, Square)
        className <- currentVal()
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
    // Class variable
    and compileClassVarDec () =
        openTag "classVarDec"
        
        // PROJECT 11: Extract kind and type before consuming tokens
        let kind = if currentVal() = "static" then STATIC else FIELD
        processTerminal() // 'static' or 'field'
        
        let typeStr = currentVal()
        processTerminal() // type
        
        let varName = currentVal()
        symbolTable.Define(varName, typeStr, kind) // Register first variable
        processTerminal() // varName
        
        while currentVal() = "," do
            processTerminal() // ','
            let extraVarName = currentVal()
            symbolTable.Define(extraVarName, typeStr, kind) // Register chained variables (e.g., field int x, y;)
            processTerminal() // varName
            
        processTerminal() // ';'
        closeTag "classVarDec"

    // Compiles a complete method, function, or constructor subroutine declaration

    and compileSubroutine () =
        openTag "subroutineDec"
        
        // PROJECT 11: Clear the subroutine scope symbol table for this new scope
        symbolTable.StartSubroutine()
        
        let subroutineKind = currentVal() // Remember if it's 'constructor', 'function', or 'method'
        processTerminal() // 'constructor' | 'function' | 'method'
        processTerminal() // 'void' | type
        
        let subroutineName = currentVal() // Remember the function name
        processTerminal() // subroutineName
        processTerminal() // '('
        
        // PROJECT 11: If it's a method, register the implicit 'this' pointer at argument index 0
        if subroutineKind = "method" then
            symbolTable.Define("this", className, ARGUMENT)

        compileParameterList() |> ignore
        processTerminal() // ')'
        
        // Compile the block container for the subroutine's execution logic
        openTag "subroutineBody"
        processTerminal() // '{'
        
        // 1. Process all local variable declarations so they get registered in the symbol table
        while currentVal() = "var" do
            compileVarDec()
            
        // 2. PROJECT 11: Now that all 'var' are parsed, we know the exact count of local variables!
        let nLocals = symbolTable.VarCount(LOCAL)
        let fullName = $"{className}.{subroutineName}"
        vmWriter.WriteFunction(fullName, nLocals)

        // 3. PROJECT 11: Specific initialization memory handling based on subroutine type
        match subroutineKind with
        | "constructor" ->
            // A constructor needs to allocate memory on the Heap for all fields of the object
            let nFields = symbolTable.VarCount(FIELD)
            vmWriter.WritePush("constant", nFields)
            vmWriter.WriteCall("Memory.alloc", 1) // Allocates memory and returns base address
            vmWriter.WritePop("pointer", 0)       // Anchor THIS pointer (pointer 0) to this address
            
        | "method" ->
            // A method needs to align its 'this' segment to the object it was called on
            // The object pointer is passed implicitly as the very first argument (argument 0)
            // push argument 0 // pop pointer 0
            vmWriter.WritePush("argument", 0)
            vmWriter.WritePop("pointer", 0)       // Anchor THIS pointer (pointer 0) to argument 0
            
        | _ -> () // 'function' (static) doesn't need memory initialization anchoring

        compileStatements() |> ignore
        processTerminal() // '}'
        closeTag "subroutineBody"
        closeTag "subroutineDec"


    // Compiles a comma-separated list of incoming parameters, excluding the outer parentheses
    // Parameters (arguments [for method: ragument 0 = this])
    and compileParameterList () =
        openTag "parameterList"
        if currentVal() <> ")" then
            let typeStr = currentVal()
            processTerminal() // type
            let varName = currentVal()
            symbolTable.Define(varName, typeStr, ARGUMENT) // Register parameter
            processTerminal() // varName
            
            while currentVal() = "," do
                processTerminal() // ','
                let extraTypeStr = currentVal()
                processTerminal() // type
                let extraVarName = currentVal()
                symbolTable.Define(extraVarName, extraTypeStr, ARGUMENT) // Register extra parameters
                processTerminal() // varName
        closeTag "parameterList"

    // Compiles a local variable declaration statement inside a subroutine: 'var' type varName (',' varName)* ';'
    // Local variable
    and compileVarDec () =
        openTag "varDec"
        processTerminal() // 'var'
        
        let typeStr = currentVal()
        processTerminal() // type
        
        let varName = currentVal()
        symbolTable.Define(varName, typeStr, LOCAL) // Register local variable
        processTerminal() // varName
        
        while currentVal() = "," do
            processTerminal() // ','
            let extraVarName = currentVal()
            symbolTable.Define(extraVarName, typeStr, LOCAL) // Register extra chained local variables
            processTerminal() // varName
            
        processTerminal() // ';'
        closeTag "varDec"

    // Compiles a sequence of statements, matching valid statement leading keywords
    // Keywords
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
    // Let
    and compileLet () =
        openTag "letStatement"
        processTerminal() // Consumes 'let'
        
        let varName = currentVal()
        processTerminal() // Consumes varName
        
        let isArray = (currentVal() = "[")
        
        if isArray then
            // 1. Array destination math: push array base address
            match symbolTable.Lookup(varName) with
            | Some info ->
                let segment = 
                    match info.Kind with
                    | STATIC -> "static" | FIELD -> "this" | ARGUMENT -> "argument" | LOCAL -> "local"
                vmWriter.WritePush(segment, info.Index)
            | None -> printfn "Error: Array variable '%s' not found" varName
            
            processTerminal() // '['
            compileExpression() |> ignore // Push index i
            processTerminal() // ']'
            
            vmWriter.WriteArithmetic("ADD") // Stack now has target RAM address (base + i)
            
        processTerminal() // Consumes '='
        
        compileExpression() |> ignore // Evaluates right-hand side expression (pushes result value)
        processTerminal() // Consumes ';'
        
        if isArray then
            // Project 11 Array Assignment Trick:
            // Top of stack is 'value'. Just under it is 'target RAM address'.
            vmWriter.WritePop("temp", 0)        // Save 'value' in temp 0
            vmWriter.WritePop("pointer", 1)     // Pop 'target RAM address' into THAT pointer (pointer 1)
            vmWriter.WritePush("temp", 0)       // Restore 'value' into stack
            vmWriter.WritePop("that", 0)        // Store 'value' into RAM[THAT] (which is a[i])
        else
            // Standard variable assignment
            match symbolTable.Lookup(varName) with
            | Some info ->
                let segment = 
                    match info.Kind with
                    | STATIC -> "static" | FIELD -> "this" | ARGUMENT -> "argument" | LOCAL -> "local"
                vmWriter.WritePop(segment, info.Index)
            | None -> printfn "Error: Variable assignment target '%s' not found" varName
        
        closeTag "letStatement"


    // Compiles a conditional branching statement: 'if' '(' expression ')' '{' statements '}' ('else' '{' statements '}')?
    // if-else
    and compileIf () =
        openTag "ifStatement"
        
        let index = ifLabelIndex
        ifLabelIndex <- ifLabelIndex + 1
        
        let labelTrue = $"IF_TRUE{index}"
        let labelFalse = $"IF_FALSE{index}"
        let labelEnd = $"IF_END{index}"
        
        processTerminal() // 'if'
        processTerminal() // '('
        compileExpression() // Evaluates if condition
        processTerminal() // ')'
        
        // Jump to IF_TRUE if condition is met, otherwise go to IF_FALSE
        vmWriter.WriteIf(labelTrue)
        vmWriter.WriteGoto(labelFalse)
        vmWriter.WriteLabel(labelTrue)
        
        processTerminal() // '{'
        compileStatements()
        processTerminal() // '}'
        
        if currentVal() = "else" then
            vmWriter.WriteGoto(labelEnd)
            vmWriter.WriteLabel(labelFalse)
            
            processTerminal() // 'else'
            processTerminal() // '{'
            compileStatements()
            processTerminal() // '}'
            
            vmWriter.WriteLabel(labelEnd)
        else
            vmWriter.WriteLabel(labelFalse)
            
        closeTag "ifStatement"

    /// Compiles a conditional execution loop statement: 'while' '(' expression ')' '{' statements '}'
    /// While
    and compileWhile () =
        openTag "whileStatement"
        
        let index = whileLabelIndex
        whileLabelIndex <- whileLabelIndex + 1
        
        let labelExp = $"WHILE_EXP{index}"
        let labelEnd = $"WHILE_END{index}"
        
        vmWriter.WriteLabel(labelExp)
        
        processTerminal() // 'while'
        processTerminal() // '('
        compileExpression() // Evaluates loop condition
        processTerminal() // ')'
        
        // If condition is false (0), jump out of the loop
        vmWriter.WriteArithmetic("NOT")
        vmWriter.WriteIf(labelEnd)
        
        processTerminal() // '{'
        compileStatements()
        processTerminal() // '}'
        
        // Loop back up to re-evaluate the condition
        vmWriter.WriteGoto(labelExp)
        vmWriter.WriteLabel(labelEnd)
        
        closeTag "whileStatement"
   

   // Compiles an invocation subroutine call statement: 'do' subroutineCall ';'
   // Do
    and compileDo () =
        openTag "doStatement"
        processTerminal() // 'do'
        
        let firstIdentifier = currentVal() // e.g., "Output", "square", or "moveSquare"
        processTerminal() // subroutineName ou className/varName
        
        let mutable nameOfCall = firstIdentifier
        let mutable argCountOffset = 0
        
        if currentVal() = "." then
            processTerminal() // '.'
            let subroutineName = currentVal() // e.g., "printInt"
            processTerminal() // subroutineName
            
            // Look ahead in the symbol table to check if it's an object or a class
            match symbolTable.Lookup(firstIdentifier) with
            | Some info ->
                // It's an object method call (e.g., square.run())
                // Push the object pointer as the first implicit argument  
                let segment = 
                    match info.Kind with
                    | STATIC -> "static"
                    | FIELD -> "this"
                    | ARGUMENT -> "argument"
                    | LOCAL -> "local"
                vmWriter.WritePush(segment, info.Index)
                nameOfCall <- $"{info.Type}.{subroutineName}"
                argCountOffset <- 1
            | None ->
                // It's a static class function call (e.g., Output.printInt)
                nameOfCall <- $"{firstIdentifier}.{subroutineName}"
                argCountOffset <- 0
                
        else
            // PROJECT 11: Direct method call on the current object instance (e.g., do moveSquare();)
            // The implicit object is 'this', so we push pointer 0 as argument 0
            vmWriter.WritePush("pointer", 0)
            nameOfCall <- $"{className}.{firstIdentifier}"
            argCountOffset <- 1
                
        processTerminal() // '('
        let nArgs = compileExpressionList() // Compiles the contents inside paren
        processTerminal() // ')'
        processTerminal() // ';'
        
        // Write the final VM call command
        vmWriter.WriteCall(nameOfCall, nArgs + argCountOffset)
        
        // General 'do' rule: discard the return value (0) into the temp segment to keep stack clean
        vmWriter.WritePop("temp", 0)
        
        closeTag "doStatement"

    // Compiles a subroutine termination sequence: 'return' expression? ';'
    // Return
    and compileReturn () =
        openTag "returnStatement"
        processTerminal() // 'return'
        
        if currentVal() <> ";" then
            // Case 1: There is an expression to return (e.g., return x;)
            compileExpression() |> ignore
        else
            // Case 2: void return (return;). Jack VM requires pushing a dummy 0 constant
            vmWriter.WritePush("constant", 0)
            
        processTerminal() // ';'
        
        // Write the final VM return command
        vmWriter.WriteReturn()
        closeTag "returnStatement"


    // Compiles an operational expression unit: term (op term)*
    and compileExpression () =
        openTag "expression"
        compileTerm() |> ignore // Compiles the first term (e.g., pushes 1)
        
        // Iterates through chaining binary operators (e.g., 1 + (2 * 3))
        while (Set.ofList ["+"; "-"; "*"; "/"; "&"; "|"; "<"; ">"; "="]).Contains(currentVal()) do
            let op = currentVal() // Save the operator (e.g., "+")
            processTerminal() // Consume the operator symbol
            
            compileTerm() |> ignore // Compiles the second term
            
            // PROJECT 11: Write the corresponding VM arithmetic command or OS call
            match op with
            | "+" -> vmWriter.WriteArithmetic("ADD")
            | "-" -> vmWriter.WriteArithmetic("SUB")
            | "=" -> vmWriter.WriteArithmetic("EQ")
            | ">" -> vmWriter.WriteArithmetic("GT")
            | "<" -> vmWriter.WriteArithmetic("LT")
            | "&" -> vmWriter.WriteArithmetic("AND")
            | "|" -> vmWriter.WriteArithmetic("OR")
            | "*" -> vmWriter.WriteCall("Math.multiply", 2) // OS helper for multiplication
            | "/" -> vmWriter.WriteCall("Math.divide", 2)   // OS helper for division
            | _ -> ()
            
        closeTag "expression"


  // Compiles an atomic expression segment (term) resolving primitives, symbols, or variable tracking
    and compileTerm () =
        openTag "term"
        let tokenVal = currentVal()
        let tokenType = currentType()
        
        if tokenType = IntConst then
            // PROJECT 11: Push integer constants directly onto the stack
            let value = int (currentVal())
            vmWriter.WritePush("constant", value)
            processTerminal() // Consumes the IntConst     
            
        elif tokenType = StringConst then
            // PROJECT 11: Dynamic String instantiation character-by-character
            let strValue = tokenVal
            let strLength = strValue.Length
            
            vmWriter.WritePush("constant", strLength)
            vmWriter.WriteCall("String.new", 1) // Returns reference to the new string object
            vmWriter.WritePop("temp", 1)        // Keep string reference safe in temp 1
            
            for charIdx in 0 .. (strLength - 1) do
                let asciiCode = int (strValue.[charIdx])
                vmWriter.WritePush("temp", 1)   // Push string object instance pointer
                vmWriter.WritePush("constant", asciiCode)
                vmWriter.WriteCall("String.appendChar", 2)
                vmWriter.WritePop("temp", 1)    // Discard fluent return pointer to keep stack stable
                
            vmWriter.WritePush("temp", 1)       // Leave final populated string reference on stack
            processTerminal() // Consumes StringConst
            
        elif tokenType = Keyword then
            // PROJECT 11: Translate Jack keywords to explicit VM equivalents
            match currentVal() with
            | "true" -> 
                vmWriter.WritePush("constant", 0)
                vmWriter.WriteArithmetic("NOT") // true is mapped to -1 in Jack VM
            | "false" | "null" -> 
                vmWriter.WritePush("constant", 0)
            | "this" -> 
                vmWriter.WritePush("pointer", 0)
            | _ -> ()
            processTerminal()
            
        elif tokenVal = "(" then
            processTerminal() // '('
            compileExpression() |> ignore
            processTerminal() // ')'
            
        elif tokenVal = "-" || tokenVal = "~" then
            // PROJECT 11: Unary operators require a postfix execution architecture
            let unaryOp = tokenVal
            processTerminal() // unaryOp ('-' or '~')
            compileTerm()     // Recursively resolve the inner sub-term
            
            match unaryOp with
            | "-" -> vmWriter.WriteArithmetic("NEG")
            | "~" -> vmWriter.WriteArithmetic("NOT")
            | _ -> ()
            
        elif tokenType = Identifier then
            // Complex identifier evaluation (Look-ahead required to differentiate variable, array, or method call)
            let firstIdentifier = currentVal()
            processTerminal() // Consumes the leading token (varName, className, or subroutineName)
            
            if currentVal() = "[" then
                // PROJECT 11: Resolves Array Read Syntax: varName[expression]
                match symbolTable.Lookup(firstIdentifier) with
                | Some info ->
                    let segment = 
                        match info.Kind with
                        | STATIC -> "static" | FIELD -> "this" | ARGUMENT -> "argument" | LOCAL -> "local"
                    vmWriter.WritePush(segment, info.Index) // Push array base address
                | None -> printfn "Error: Array reference '%s' not found" firstIdentifier
                
                processTerminal() // '['
                compileExpression() |> ignore // Push index i
                processTerminal() // ']'
                
                vmWriter.WriteArithmetic("ADD")     // Calculate precise RAM location (base + i)
                vmWriter.WritePop("pointer", 1)     // Direct THAT pointer (pointer 1) to this location
                vmWriter.WritePush("that", 0)       // Read target RAM value onto the stack
                
            elif currentVal() = "(" then
                // Resolves Direct Subroutine Call: subroutineName(expressionList)
                // In Jack, a direct method call implicitly targets the current instance ('this')
                vmWriter.WritePush("pointer", 0) // Push object pointer as argument 0
                processTerminal() // '('
                let nArgs = compileExpressionList()
                processTerminal() // ')'
                vmWriter.WriteCall($"{className}.{firstIdentifier}", nArgs + 1)
                
            elif currentVal() = "." then
                // Resolves Dotted Object Call: (className | varName).subroutineName(expressionList)
                processTerminal() // '.'
                let subroutineName = currentVal()
                processTerminal() // subroutineName
                processTerminal() // '('
                
                let mutable nameOfCall = firstIdentifier
                let mutable argCountOffset = 0
                
                match symbolTable.Lookup(firstIdentifier) with
                | Some info ->
                    // Case A: It's an instance variable object (e.g., game.run()) -> Method invocation
                    let segment = 
                        match info.Kind with
                        | STATIC -> "static"
                        | FIELD -> "this"
                        | ARGUMENT -> "argument"
                        | LOCAL -> "local"
                    vmWriter.WritePush(segment, info.Index)
                    nameOfCall <- $"{info.Type}.{subroutineName}"
                    argCountOffset <- 1
                | None ->
                    // Case B: It's a static system class call (e.g., Memory.peek)
                    nameOfCall <- $"{firstIdentifier}.{subroutineName}"
                    argCountOffset <- 0
                    
                let nArgs = compileExpressionList()
                processTerminal() // ')'
                vmWriter.WriteCall(nameOfCall, nArgs + argCountOffset)
                
            else
                // PROJECT 11: Simple Variable Evaluation (Just a single standalone identifier name)
                match symbolTable.Lookup(firstIdentifier) with
                | Some info ->
                    let segment = 
                        match info.Kind with
                        | STATIC -> "static"
                        | FIELD -> "this"
                        | ARGUMENT -> "argument"
                        | LOCAL -> "local"
                    vmWriter.WritePush(segment, info.Index)
                | None -> printfn "Error: Undefined identifier variable lookup -> %s" firstIdentifier
                
        closeTag "term"

   // Compiles a comma-separated list of passed evaluation arguments inside call parameters
    and compileExpressionList () =
        openTag "expressionList"
        let mutable count = 0
        
        if currentVal() <> ")" then
            compileExpression()
            count <- count + 1
            while currentVal() = "," do
                processTerminal() // ','
                compileExpression()
                count <- count + 1
                
        closeTag "expressionList"
        count // Correctly aligned to be the return value of the function

    // Main entry point method to kick off the top-down parsing execution
    member this.Compile() =
        if tokenizer.HasMoreTokens() then
            tokenizer.Advance() // Prime the stream to load the first token state
            compileClass() |> ignore // We add '|> ignore' to discard any accidental return types and ensure it returns unit
        writer.Close()  // Safe handle closure to release written text payload