namespace Nand2Tetris
open System

//  definition of the possible VM command types
type Command =
    | Arithmetic of string
    | Push of segment: string * index: int
    | Pop of segment: string * index: int
    | Label of string        
    | Goto of string        
    | IfGoto of string    
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
            | "label"   -> Some(Label(parts.[1]))    
            | "goto"    -> Some(Goto(parts.[1]))     
            | "if-goto" -> Some(IfGoto(parts.[1]))  
            | "add" | "sub" | "neg" | "eq" | "gt" | "lt" | "and" | "or" | "not" -> 
               Some(Arithmetic(parts.[0]))
            | _ -> Some(Unknown)

            