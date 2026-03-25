namespace Nand2Tetris
open System.IO

type CodeWriter(outputFilePath: string) =
    // פותחים את קובץ הפלט לכתיבה
    let writer = new StreamWriter(outputFilePath)
    let mutable labelCount = 0

    // תרגום פקודות אריתמטיות
    member this.WriteArithmetic(command: string) =
        writer.WriteLine(sprintf "// %s" command) // תמיד טוב לכתוב את פקודת המקור כהערה
        match command with
        | "add" ->
            writer.WriteLine("@SP")
            writer.WriteLine("A=M-1")
            writer.WriteLine("D=M")
            writer.WriteLine("A=A-1")
            writer.WriteLine("M=M+D")
            writer.WriteLine("@SP")
            writer.WriteLine("M=M-1")

        | "sub" ->
            writer.WriteLine("@SP")
            writer.WriteLine("AM=M-1") // יורדים לאיבר העליון (y)
            writer.WriteLine("D=M")    // שומרים את y ב-D
            writer.WriteLine("A=A-1")  // יורדים לאיבר שמתחתיו (x)
            writer.WriteLine("M=M-D")  // עושים x - y ושומרים במקום של x
            
        | "neg" -> // פעולה אונרית (רק על האיבר העליון)
            writer.WriteLine("@SP")
            writer.WriteLine("A=M-1")  // ניגשים ישירות לאיבר העליון (בלי לשנות את SP)
            writer.WriteLine("M=-M")   // הופכים את הסימן שלו

        
        | "and" | "or" as op ->
           let operation =
             match op with
             | "and" -> "D&M"
             | "or" -> "D|M"
           writer.WriteLine("@SP")
           writer.WriteLine("A=M-1")
           writer.WriteLine("D=M")
           writer.WriteLine("A=A-1")
           writer.WriteLine($"M={operation}")
           writer.WriteLine("@SP")
           writer.WriteLine("M=M-1")


        | "not" ->
            writer.WriteLine("@SP")
            writer.WriteLine("A=M-1")
            writer.WriteLine("M=!M")


        | "eq" | "gt" | "lt" ->
            let labelTrue = sprintf "LABEL_TRUE_%d" labelCount
            let labelEnd = sprintf "LABEL_END_%d" labelCount
            labelCount <- labelCount + 1
            let jump = match command with "eq" -> "JEQ" | "gt" -> "JGT" | "lt" -> "JLT" | _ -> ""

            writer.WriteLine("@SP")
            writer.WriteLine("A=M-1")
            writer.WriteLine("D=M")    // D = y
            writer.WriteLine("A=A-1")
            writer.WriteLine("D=M-D")  // D = x - y
            writer.WriteLine(sprintf "@%s" labelTrue)
            writer.WriteLine(sprintf "D;%s" jump)
            // if false
            writer.WriteLine("@SP")
            writer.WriteLine("A=M-1")
            writer.WriteLine("A=A-1")
            writer.WriteLine("M=0")    // Result = False (0)
            writer.WriteLine(sprintf "@%s" labelEnd)
            writer.WriteLine("0;JMP")
            // if true
            writer.WriteLine(sprintf "(%s)" labelTrue)
            writer.WriteLine("@SP")
            writer.WriteLine("A=M-1")
            writer.WriteLine("A=A-1")
            writer.WriteLine("M=-1")   // Result = True (-1)
            writer.WriteLine(sprintf "(%s)" labelEnd)
            writer.WriteLine("@SP")
            writer.WriteLine("M=M-1")   // SP-- (Two numbers reduced to one result)


        | _ -> () 

    // תרגום פקודות push ו-pop
    member this.WritePushPop(cmd: Command) =
        match cmd with
        | Push(segment, index) ->
            writer.WriteLine(sprintf "// push %s %d" segment index)
            match segment with
            | "constant" ->
                // Put the constant value into the D register
                writer.WriteLine(sprintf "@%d" index)
                writer.WriteLine("D=A")
                // Load the stack pointer (SP) and put D where it points
                writer.WriteLine("@SP")
                writer.WriteLine("A=M")
                writer.WriteLine("M=D")
                // Increment the stack pointer (SP++)
                writer.WriteLine("@SP")
                writer.WriteLine("M=M+1")

            | "local" | "argument" | "this" | "that" ->
                let baseReg = 
                    match segment with 
                    | "local" -> "LCL" | "argument" -> "ARG" 
                    | "this" -> "THIS" | "that" -> "THAT" | _ -> ""
                // Calculate target address: base address + index
                writer.WriteLine(sprintf "@%s" baseReg)
                writer.WriteLine("D=M")          // D = base address
                writer.WriteLine(sprintf "@%d" index)
                writer.WriteLine("A=D+A")        // A = base + index
                writer.WriteLine("D=M")          // D = value at target address
                // Push the value from D onto the stack
                writer.WriteLine("@SP")
                writer.WriteLine("A=M")
                writer.WriteLine("M=D")
                // Increment stack pointer (SP++)
                writer.WriteLine("@SP")
                writer.WriteLine("M=M+1")

            | "temp" ->
                // Temp segment starts at fixed RAM address 5
                writer.WriteLine(sprintf "@%d" (5 + index))
                writer.WriteLine("D=M")          // D = value at RAM[5 + index]
                // Push D onto stack
                writer.WriteLine("@SP")
                writer.WriteLine("A=M")
                writer.WriteLine("M=D")
                writer.WriteLine("@SP")
                writer.WriteLine("M=M+1")

            | _ -> ()

        | Pop(segment, index) ->
            writer.WriteLine(sprintf "// pop %s %d" segment index)
            match segment with
            | "local" | "argument" | "this" | "that" ->
                let baseReg = 
                    match segment with 
                    | "local" -> "LCL" | "argument" -> "ARG" 
                    | "this" -> "THIS" | "that" -> "THAT" | _ -> ""
                
                // 1. Calculate target address (base + index)
                writer.WriteLine(sprintf "@%s" baseReg)
                writer.WriteLine("D=M")
                writer.WriteLine(sprintf "@%d" index)
                writer.WriteLine("D=D+A")        // D = calculated address
                
                // 2. Temporarily store the target address in R13
                writer.WriteLine("@R13")
                writer.WriteLine("M=D")
                
                // 3. Decrement SP and fetch the top value from the stack
                writer.WriteLine("@SP")
                writer.WriteLine("M=M-1")        // SP--
                writer.WriteLine("A=M")
                writer.WriteLine("D=M")          // D = top value of stack
                
                // 4. Store the fetched value (D) into the target address (saved in R13)
                writer.WriteLine("@R13")
                writer.WriteLine("A=M")          // Point A back to target address
                writer.WriteLine("M=D")          // Memory[target] = value

            | "temp" ->
                // 1. Get value from stack
                writer.WriteLine("@SP")
                writer.WriteLine("M=M-1")        // SP--
                writer.WriteLine("A=M")
                writer.WriteLine("D=M")          // D = value from stack
                // 2. Store directly into fixed temp address
                writer.WriteLine(sprintf "@%d" (5 + index))
                writer.WriteLine("M=D")          // RAM[5+index] = value

            | _ -> ()
        | _ -> ()
    // פונקציה לסגירת הקובץ בסיום
    member this.Close() =
        writer.Close()

        