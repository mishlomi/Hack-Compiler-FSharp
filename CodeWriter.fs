namespace Nand2Tetris
open System.IO

type CodeWriter(outputFilePath: string) =
    // פותחים את קובץ הפלט לכתיבה
    let writer = new StreamWriter(outputFilePath)

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

        // בהמשך נוסיף כאן את eq, gt, lt, and, or, not
        | _ -> () 

    // תרגום פקודות push ו-pop
    member this.WritePushPop(cmd: Command) =
        match cmd with
        | Push("constant", index) ->
            writer.WriteLine(sprintf "// push constant %d" index)
            writer.WriteLine(sprintf "@%d" index)
            writer.WriteLine("D=A")
            writer.WriteLine("@SP")
            writer.WriteLine("A=M")
            writer.WriteLine("M=D")
            writer.WriteLine("@SP")
            writer.WriteLine("M=M+1")
            writer.WriteLine("\n")
        // נוסיף כאן את שאר המקטעים (local, argument וכו') בהמשך...
        | _ -> ()

    // פונקציה לסגירת הקובץ בסיום
    member this.Close() =
        writer.Close()

        