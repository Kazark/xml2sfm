namespace xml2sfm

module EntryPoint =
    open System.IO
    open System.Text

    let transform (inputPath : string) outputPath =
        let xml = File.ReadAllText(inputPath, Encoding.UTF8)
        match Flextext.parse xml with
        | Flextext.Parse.Succeeded text ->
            printfn "Experiment: %s" text
        | Flextext.Parse.Failed ->
            printfn "Parse failed!"

    let usage() =
        printfn "Usage: xml2sfm inputPath [outputPath]"

    [<EntryPoint>]
    let main argv = 
        match argv with
        | [||] -> 
            printfn "Error: Too few command-line arguments."
            usage()
            1
        | [|inputPath|] ->
            transform inputPath "output.sfm"
            0
        | [|inputPath; outputPath|] ->
            transform inputPath outputPath
            0
        | _ ->
            printfn "Error: Too many command-line arguments."
            usage()
            2
