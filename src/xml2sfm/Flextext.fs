namespace xml2sfm

[<RequireQualifiedAccess>]
module Flextext =
    open System.IO
    open FSharp.Data

    type Parser = XmlProvider<"./sample.flextext">

    [<RequireQualifiedAccess>]
    type Parse =
        | Failed
        | Succeeded of string

    let parse xml =
        let parsedXml = Parser.Parse xml
        parsedXml.InterlinearTexts
        |> Seq.map (fun interlinear ->
            interlinear.Paragraphs
            |> Seq.filter (fun paragraph -> paragraph.Phrases.IsSome)
            |> Seq.map (fun paragraph ->
                paragraph.Phrases.Value.Phrases
                |> Seq.map (fun phrase ->
                    phrase.Words
                    |> Seq.map (fun word ->
                        word.Items
                        |> Seq.map (fun item ->
                            match item.Type with
                            | "txt" -> " " + item.Value
                            | "punct" -> item.Value
                            | _ -> ""
                        )
                        |> String.concat ""
                    )
                )
                |> Seq.concat
                |> String.concat ""
            )
            |> String.concat "\n")
        |> String.concat ""
        |> Parse.Succeeded

