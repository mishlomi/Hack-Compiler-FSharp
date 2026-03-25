//michal shlomi 211503370
//naomi malka 345887640
//150060.21.5786.41

open System
open System.IO

// Global variable to store the name of the file currently being read (without the .vm extension)
let mutable currentFileName = ""

// --- Helper functions for arithmetic commands ---
let handleAdd (writer: StreamWriter) = writer.WriteLine("command: add")
let handleSub (writer: StreamWriter) = writer.WriteLine("command: sub")
let handlNeg (writer: StreamWriter) = writer.WriteLine("command: neg")

// --- Helper functions for logical commands ---
let handleEq (writer: StreamWriter) (counter: int) =
    writer.WriteLine("command: eq")
    writer.WriteLine($"counter: {counter}")

let handleGt (writer: StreamWriter) (counter: int) =
    writer.WriteLine("command: gt")
    writer.WriteLine($"counter: {counter}")

let handleLt (writer: StreamWriter) (counter: int) =
    writer.WriteLine("command: lt")
    writer.WriteLine($"counter: {counter}")

// --- Helper functions for memory access commands ---
let handlePush (writer: StreamWriter) (segment: string) (index: string) =
    writer.WriteLine($"command: push segment {segment} index {index}")

let handlePop (writer: StreamWriter) (segment: string) (index: string) =
    writer.WriteLine($"command: pop segment {segment} index {index}")


[<EntryPoint>]
let main argv =
    // The program asks the user to enter a string containing the path of the folder
    Console.WriteLine("Please enter the folder path containing the VM files:")
    let folderPath = Console.ReadLine()
    
    // Check that the folder actually exists
    if Directory.Exists(folderPath) then
        
        // Extract the name of the last folder from the input path
        let folderName = DirectoryInfo(folderPath).Name
        
        // The program creates a new output text file with the extension .asm
        let outputFilePath = Path.Combine(folderPath, folderName + ".asm")
        
        // Open the output file for writing
        use writer = new StreamWriter(outputFilePath)
        
        // The program goes through all input files with the .vm extension in the folder
        let vmFiles = Directory.GetFiles(folderPath, "*.vm")
        
        for vmFile in vmFiles do
            // Initialize the logical command counter
            let mutable logicalCounter = 0
            
            // Store the current file name (without the .vm extension) in a global variable
            currentFileName <- Path.GetFileNameWithoutExtension(vmFile)
            
            // Open the file for reading
            let lines = File.ReadAllLines(vmFile)
            
            // Read the VM file line by line
            for line in lines do
                let trimmedLine = line.Trim()
                if trimmedLine.Length > 0 then
                    // Split the line into words
                    let words = trimmedLine.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                    let command = words.[0]
                    
                    // Check the first word (the VM command) and call the corresponding helper function
                    match command with
                    | "add" -> handleAdd writer
                    | "sub" -> handleSub writer
                    | "neg" -> handlNeg writer
                    | "eq" -> 
                        logicalCounter <- logicalCounter + 1
                        handleEq writer logicalCounter
                    | "gt" -> 
                        logicalCounter <- logicalCounter + 1
                        handleGt writer logicalCounter
                    | "lt" -> 
                        logicalCounter <- logicalCounter + 1
                        handleLt writer logicalCounter
                    | "push" -> handlePush writer words.[1] words.[2]
                    | "pop" -> handlePop writer words.[1] words.[2]
                    | _ -> () // Ignore commands that are not in the required table
            
            // The input file is automatically closed after ReadAllLines
            // Print to the screen the message "End of input file:" followed by the input file name
            Console.WriteLine($"End of input file:  {Path.GetFileName(vmFile)}")
            
        // After reading all VM input files, print the final message
        Console.WriteLine($"Output file is ready: {folderName}.asm")

    if File.Exists(inputPath) then
        // 1. קוראים את כל קובץ ה-VM
        let lines = File.ReadAllLines(inputPath)
        
        // 2. פותחים את ה-CodeWriter שלנו
        let codeWriter = new CodeWriter(outputPath)

        // 3. עוברים על כל שורה
        for line in lines do
            match Parser.parseLine line with
            | Some(cmd) -> 
                // אם המפענח מצא פקודה חוקית, שולחים אותה לתרגום
                match cmd with
                | Arithmetic(op) -> codeWriter.WriteArithmetic(op)
                | Push(_, _) | Pop(_, _) -> codeWriter.WritePushPop(cmd)
                | Unknown -> ()
            | None -> () // מתעלמים משורות ריקות או הערות

        // 4. סוגרים את הקובץ בסיום
        codeWriter.Close()
        printfn "Boom! Translated successfully to %s" outputPath
    else
        Console.WriteLine("Error: The specified directory does not exist.")

    0