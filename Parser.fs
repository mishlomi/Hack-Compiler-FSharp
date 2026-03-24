namespace Nand2Tetris
open System

// הגדרת סוגי הפקודות האפשריות בצורה חכמה
type Command =
    | Arithmetic of string
    | Push of segment: string * index: int
    | Pop of segment: string * index: int
    | Unknown

module Parser =
    // פונקציה שמקבלת שורת טקסט ומחזירה אובייקט פקודה (או None אם זו הערה/שורה ריקה)
    let parseLine (line: string) : Command option =
        let clean = line.Split("//").[0].Trim()
        
        if clean = "" then 
            None // מתעלמים משורות ריקות והערות
        else
            let parts = clean.Split(' ', StringSplitOptions.RemoveEmptyEntries)
            match parts.[0] with
            | "push" -> Some(Push(parts.[1], int parts.[2]))
            | "pop"  -> Some(Pop(parts.[1], int parts.[2]))
            | cmd -> Some(Arithmetic(cmd)) // כל השאר (add, sub וכו') מוגדר כאריתמטי בינתיים