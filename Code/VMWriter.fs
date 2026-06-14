namespace Nand2Tetris

open System.IO

type VMWriter(outputPath: string) =
    let writer = new StreamWriter(outputPath)

    member this.WritePush(segment: string, index: int) =
        writer.WriteLine($"push {segment.ToLower()} {index}")

    member this.WritePop(segment: string, index: int) =
        writer.WriteLine($"pop {segment.ToLower()} {index}")

    member this.WriteArithmetic(command: string) =
        // command peut être : ADD, SUB, NEG, EQ, GT, LT, AND, OR, NOT
        writer.WriteLine(command.ToLower())

    member this.WriteLabel(label: string) =
        writer.WriteLine($"label {label}")

    member this.WriteGoto(label: string) =
        writer.WriteLine($"goto {label}")

    member this.WriteIf(label: string) =
        writer.WriteLine($"if-goto {label}")

    member this.WriteCall(name: string, nArgs: int) =
        writer.WriteLine($"call {name} {nArgs}")

    member this.WriteFunction(name: string, nLocals: int) =
        writer.WriteLine($"function {name} {nLocals}")

    member this.WriteReturn() =
        writer.WriteLine("return")

    member this.Close() =
        writer.Close()