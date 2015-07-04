namespace xml2sfm

module EntryPoint =
    open System.IO
    open System.Text

    let transform inputPath outputPath =
        let xml = File.ReadAllText(inputPath, Encoding.UTF8)
        let model = Flextext.parseXml xml (Path.GetFileNameWithoutExtension inputPath)
        let sfmLines = ParatextSfm.stringifyBook model
        File.WriteAllText(outputPath, sfmLines, Encoding.UTF8)

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
            let outputFile = sprintf "%s.sfm" <| Path.GetFileNameWithoutExtension inputPath
            let outputPath = Path.Combine(Path.GetDirectoryName inputPath, outputFile)
            transform inputPath outputPath
            0
        | [|inputPath; outputPath|] ->
            transform inputPath outputPath
            0
        | _ ->
            printfn "Error: Too many command-line arguments."
            usage()
            2
