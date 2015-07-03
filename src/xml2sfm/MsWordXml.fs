namespace xml2sfm

[<RequireQualifiedAccess>]
module MsWordXml =
    open System.IO
    open FSharp.Data

    type Parser = XmlProvider<"./sample.xml">

    [<RequireQualifiedAccess>]
    type Parse =
        | Failed
        | Succeeded of string

    let parse xml =
        let parsedXml = Parser.Parse xml
        let parts = parsedXml.Parts
        let document =
            parsedXml.Parts
            |> Seq.filter (fun part -> part.Name = "/word/document.xml")
            |> Seq.map (fun part -> part.XmlData.Document)
            |> Seq.exactlyOne
        if document.IsNone
        then Parse.Failed
        else
            let body = document.Value.Body
            body.Ps
            |> Seq.map (fun p ->
                p.OMaths
                |> Seq.map (fun oMath -> oMath.M.Mr.E.R.T))
            |> Seq.concat
            |> System.String.Concat
            |> Parse.Succeeded
