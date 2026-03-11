open System
open System.IO

// משתנה גלובאלי לשמירת שם הקובץ שנפתח לקריאה, ללא הסיומת VM [cite: 89]
let mutable currentFileName = ""

// --- פונקציות עזר לפקודות אריתמטיות [cite: 96, 97] ---
let handleAdd (writer: StreamWriter) = writer.WriteLine("command: add")
let handleSub (writer: StreamWriter) = writer.WriteLine("command: sub")
let handlNeg (writer: StreamWriter) = writer.WriteLine("command: neg")

// --- פונקציות עזר לפקודות לוגיות [cite: 96, 97] ---
let handleEq (writer: StreamWriter) (counter: int) =
    writer.WriteLine("command: eq")
    writer.WriteLine($"counter: {counter}")

let handleGt (writer: StreamWriter) (counter: int) =
    writer.WriteLine("command: gt")
    writer.WriteLine($"counter: {counter}")

let handleLt (writer: StreamWriter) (counter: int) =
    writer.WriteLine("command: lt")
    writer.WriteLine($"counter: {counter}")

// --- פונקציות עזר לפקודות גישה לזיכרון [cite: 96, 97] ---
let handlePush (writer: StreamWriter) (segment: string) (index: string) =
    writer.WriteLine($"command: push segment {segment} index {index}")

let handlePop (writer: StreamWriter) (segment: string) (index: string) =
    writer.WriteLine($"command: pop segment {segment} index {index}")


[<EntryPoint>]
let main argv =
    // התוכנית תקבל כקלט מהמשתמש מחרוזת שמכילה מסלול לתיקייה [cite: 80]
    Console.WriteLine("Please enter the folder path containing the VM files:")
    let folderPath = Console.ReadLine()
    
    // בדיקה שהתיקייה אכן קיימת
    if Directory.Exists(folderPath) then
        
        // עליכם לחלץ את שם התיקייה האחרונה מתוך מחרוזת הקלט [cite: 83]
        let folderName = DirectoryInfo(folderPath).Name
        
        // לצורך פלט, התוכנית תיצור קובץ טקסט חדש עם סיומת asm [cite: 82]
        let outputFilePath = Path.Combine(folderPath, folderName + ".asm")
        
        // התוכנית תפתח את קובץ הפלט לכתיבה [cite: 85]
        use writer = new StreamWriter(outputFilePath)
        
        // התוכנית תעבור על כל קבצי הקלט מסוג vm שקיימים בתיקיית הקלט [cite: 86]
        let vmFiles = Directory.GetFiles(folderPath, "*.vm")
        
        for vmFile in vmFiles do
            // התוכנית תגדיר מונה ותאפס אותו [cite: 88]
            let mutable logicalCounter = 0
            
            // התוכנית תשמור במשתנה גלובאלי את שם הקובץ ללא הסיומת VM [cite: 89]
            currentFileName <- Path.GetFileNameWithoutExtension(vmFile)
            
            // התוכנית תפתח את הקובץ לקריאה [cite: 90]
            let lines = File.ReadAllLines(vmFile)
            
            // התוכנית תקרא את קובץ ה VM שורה אחר שורה [cite: 91]
            for line in lines do
                let trimmedLine = line.Trim()
                if trimmedLine.Length > 0 then
                    // תפרק את השורה (מחרוזת) למילים [cite: 93]
                    let words = trimmedLine.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                    let command = words.[0]
                    
                    // תבדוק מה היא המילה הראשונה בשורה ותקרא לפונקציית עזר ייעודית [cite: 94]
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
                    | _ -> () // התעלמות מפקודות שאינן בטבלה
            
            // התוכנית תסגור את קובץ הקלט (נעשה אוטומטית ע"י ReadAllLines) [cite: 99]
            // התוכנית תדפיס למסך את המחרוזת: “End of input file: “ ולשרשר אותה לשם קובץ הקלט [cite: 100]
            Console.WriteLine($"End of input file:  {Path.GetFileName(vmFile)}")
            
        // בסיום קריאת כל קבצי הקלט VM, התוכנית תדפיס למסך את המחרוזת [cite: 104]
        Console.WriteLine($"Output file is ready: {folderName}.asm")

    else
        Console.WriteLine("Error: The specified directory does not exist.")

    0