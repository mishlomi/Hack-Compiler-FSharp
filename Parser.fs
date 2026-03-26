namespace Nand2Tetris
open System

//  definition of the possible VM command types
type Command =
    | Arithmetic of string
    | Push of segment: string * index: int
    | Pop of segment: string * index: int
    | Unknown

module Parser =
    // Parses a single line of VM code and returns a Command object
    // Returns None if the line is a comment or empty
    let parseLine (line: string) : Command option =
        let clean = line.Split("//").[0].Trim()
        
        if clean = "" then 
            None // Ignore empty lines and full-line comments
        else
            let parts = clean.Split(' ', StringSplitOptions.RemoveEmptyEntries)
            match parts.[0] with
            | "push" -> Some(Push(parts.[1], int parts.[2]))
            | "pop"  -> Some(Pop(parts.[1], int parts.[2]))
            | cmd -> Some(Arithmetic(cmd)) // For now, any other command (add, sub, eq, etc.) is treated as Arithmetic

            