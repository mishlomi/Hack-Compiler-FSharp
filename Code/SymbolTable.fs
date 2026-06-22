namespace Nand2Tetris

// Variable types in the Jack grammar
type SymbolKind = STATIC | FIELD | ARGUMENT | LOCAL
// Jack static   → VM static
// Jack field    → VM this
// Jack argument → VM argument
// Jack local    → VM local

// Structure of one row in our table
type SymbolInfo = {
    Type: string
    Kind: SymbolKind
    Index: int
}

type SymbolTable() =
    // Use mutable maps to store the variables
    let mutable classTable = Map.empty<string, SymbolInfo>
    let mutable subroutineTable = Map.empty<string, SymbolInfo>
    

    // Counters for every SymbolKind. used to assign automatic indices (0, 1, 2...)
    let mutable indices = Map.ofList [ (STATIC, 0); (FIELD, 0); (ARGUMENT, 0); (LOCAL, 0) ]


    // Clears the local table every time we enter a new function
    // A function that is run every time we start compiling a new function/method/constructor.
    member this.StartSubroutine() =
        subroutineTable <- Map.empty
        indices <- indices 
            |> Map.add ARGUMENT 0 
            |> Map.add LOCAL 0

    // Adds a variable to the correct table
    member this.Define(name: string, typeStr: string, kind: SymbolKind) =
        let idx = indices.[kind]
        let info = { Type = typeStr; Kind = kind; Index = idx }
        
        // Increment the counter for the next variable of this category
        indices <- Map.add kind (idx + 1) indices
        
        match kind with
        | STATIC | FIELD -> classTable <- Map.add name info classTable
        | ARGUMENT | LOCAL -> subroutineTable <- Map.add name info subroutineTable

    // Counts how many variables we have for a given segment (useful for the VM function header)
    member this.VarCount(kind: SymbolKind) =
        indices.[kind]

    // Looks up a variable, first in the local scope, then in the class scope
    member this.Lookup(name: string) : SymbolInfo option =
        if subroutineTable.ContainsKey(name) then Some subroutineTable.[name]
        elif classTable.ContainsKey(name) then Some classTable.[name]
        else None