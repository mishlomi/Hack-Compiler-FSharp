
open System.IO
open Nand2Tetris

[<EntryPoint>]
let main argv =
    let inputPath = @"c:\גיבוי מחשב ישן\קורסים\עקרונות שפות תוכנה\קבצים\nand2tetris\projects\07\StackArithmetic\SimpleAdd\SimpleAdd.vm"   
    let outputPath = @"c:\גיבוי מחשב ישן\קורסים\עקרונות שפות תוכנה\קבצים\nand2tetris\projects\07\StackArithmetic\SimpleAdd\SimpleAdd.asm"

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
        printfn "Error: File %s not found!" inputPath
    
    0