namespace Nand2Tetris

// Types de variables dans la grammaire Jack
type SymbolKind = STATIC | FIELD | ARGUMENT | LOCAL

// Structure d'une ligne de notre table
type SymbolInfo = {
    Type: string
    Kind: SymbolKind
    Index: int
}

type SymbolTable() =
    // Utilisation de maps modifiables pour stocker les variables
    let mutable classTable = Map.empty<string, SymbolInfo>
    let mutable subroutineTable = Map.empty<string, SymbolInfo>
    
    // Des compteurs pour attribuer les indices automatiques (0, 1, 2...)
    let mutable indices = Map.ofList [ (STATIC, 0); (FIELD, 0); (ARGUMENT, 0); (LOCAL, 0) ]

    // Vide la table locale à chaque fois qu'on entre dans une nouvelle fonction
    member this.StartSubroutine() =
        subroutineTable <- Map.empty
        indices <- indices 
            |> Map.add ARGUMENT 0 
            |> Map.add LOCAL 0

    // Ajoute une variable dans la bonne table
    member this.Define(name: string, typeStr: string, kind: SymbolKind) =
        let idx = indices.[kind]
        let info = { Type = typeStr; Kind = kind; Index = idx }
        
        // On incrémente le compteur pour la prochaine variable de cette catégorie
        indices <- Map.add kind (idx + 1) indices
        
        match kind with
        | STATIC | FIELD -> classTable <- Map.add name info classTable
        | ARGUMENT | LOCAL -> subroutineTable <- Map.add name info subroutineTable

    // Compte combien de variables on a pour un segment donné (utile pour l'en-tête VM)
    member this.VarCount(kind: SymbolKind) =
        indices.[kind]

    // Cherche une variable (regarde d'abord au niveau local, puis global)
    member this.Lookup(name: string) : SymbolInfo option =
        if subroutineTable.ContainsKey(name) then Some subroutineTable.[name]
        elif classTable.ContainsKey(name) then Some classTable.[name]
        else None