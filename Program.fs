// Learn more about F# at http://fsharp.org

open System

let i f = f 1 + 1

[<EntryPoint>]
let main argv =
    let a = 1 
    let b = "asd"
    printfn "Hello World from F#!"
    i (fun v -> v + 10) |> ignore
    0 // return an integer exit code
