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

    type private Token =
        | Punctuation of string
        | ProbableVerseNumber of string
        | VerseMarker
        | FootnoteMarker
        | Text of string

    let private stringify =
        function
        | Punctuation punct -> punct
        | ProbableVerseNumber verseNumber -> verseNumber
        | VerseMarker -> ")"
        | FootnoteMarker -> "*"
        | Text text -> " " + text

    let isNumeric text =
        fst <| System.UInt32.TryParse text

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
                            | "txt" ->
                                if isNumeric item.Value
                                then Token.ProbableVerseNumber item.Value
                                else Token.Text item.Value
                                |> Some
                            | "punct" ->
                                match item.Value with
                                | ")" -> Token.VerseMarker
                                | "*" -> Token.FootnoteMarker
                                | punct -> Token.Punctuation punct
                                |> Some
                            | _ -> None
                        )
                        |> Seq.filter (fun token -> token.IsSome)
                        |> Seq.map (fun token -> token.Value)
                        |> Seq.map stringify
                        |> String.concat ""
                    )
                )
                |> Seq.concat
                |> String.concat ""
            )
            |> String.concat "\n")
        |> String.concat ""
        |> Parse.Succeeded

