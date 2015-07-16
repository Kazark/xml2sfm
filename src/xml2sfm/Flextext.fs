namespace xml2sfm

[<RequireQualifiedAccess>]
module Flextext =
    open System.IO
    open FSharp.Data

    type private Parser = XmlProvider<"./sample.flextext">

    [<RequireQualifiedAccess>]
    type private Token =
        | FootnoteMarker
        | ParagraphBreak
        | Punctuation of string
        | SectionTitle of string
        | Text of string
        | ProbableVerseNumber of string
        | VerseMarker

    let private isNumeric text =
        fst <| System.UInt32.TryParse text

    let private parseItem itemType text =
        match itemType with
        | "txt" ->
            if isNumeric text
            then Token.ProbableVerseNumber text
            else Token.Text text
            |> Some
        | "punct" ->
            match text with
            | ")" -> Token.VerseMarker
            | "*" -> Token.FootnoteMarker
            | punct -> Token.Punctuation punct
            |> Some
        | _ -> None

    let private tokenizePhrase tokenizeItem (phrase : Parser.Phrase) =
        phrase.Words
        |> Seq.map (fun word ->
            word.Items |> Seq.choose tokenizeItem
        ) |> Seq.concat

    let private tokenizeParagraphs (paragraphs : Parser.Paragraph[]) =
        paragraphs
        |> Seq.filter (fun paragraph -> paragraph.Phrases.IsSome)
        |> Seq.map (fun paragraph ->
            let tokens =
                paragraph.Phrases.Value.Phrases
                |> Seq.map (tokenizePhrase (fun item -> parseItem item.Type item.Value))
                |> Seq.concat
            in Sequence.prepend Token.ParagraphBreak tokens)
        |> Seq.concat

    let private tokenizeSectionTitle (firstParagraph : Parser.Paragraph) =
        Seq.exactlyOne firstParagraph.Phrases.Value.Phrases
        |> tokenizePhrase (fun item -> match item.Type with
                                       | "txt" -> Some item.Value
                                       | _ -> None)
        |> String.concat " "
        |> Token.SectionTitle

    let private tokenizeInterlinearText (interlinear : Parser.InterlinearText) =
        Sequence.prepend (tokenizeSectionTitle interlinear.Paragraphs.[0]) (tokenizeParagraphs interlinear.Paragraphs.[1..])

    let private tokenize xml =
        let parsedXml = Parser.Parse xml
        parsedXml.InterlinearTexts
        |> Seq.map tokenizeInterlinearText
        |> Seq.concat

    [<RequireQualifiedAccess>]
    type private SecondPassToken =
        | ChapterNumber of uint32
        | FootnotePositionMarker
        | FootnoteText of string
        | ParagraphBreak
        | VerseNumber of string
        | Punctuation of string
        | SectionTitle of string
        | Text of string

    let rec private structurePoorlyStructuredData (secondPassTokens, tokenBuffer) currentToken =
        match List.rev (currentToken :: tokenBuffer) with
        | [Token.ParagraphBreak]
        | [Token.ProbableVerseNumber _] ->
            secondPassTokens, [currentToken]
        | [Token.FootnoteMarker] ->
            Sequence.append SecondPassToken.FootnotePositionMarker secondPassTokens, []
        | [Token.Punctuation punct] ->
            Sequence.append (SecondPassToken.Punctuation punct) secondPassTokens, []
        | [Token.SectionTitle title] ->
            Sequence.append (SecondPassToken.SectionTitle title) secondPassTokens, []
        | [Token.Text text] ->
            Sequence.append (SecondPassToken.Text text) secondPassTokens, []
        | [Token.VerseMarker] ->
            Sequence.append (SecondPassToken.Punctuation ")") secondPassTokens, []
        | [Token.ParagraphBreak; Token.FootnoteMarker] ->
            Sequence.append SecondPassToken.ParagraphBreak secondPassTokens, [currentToken]
        | [Token.ParagraphBreak; _] ->
            structurePoorlyStructuredData (Sequence.append SecondPassToken.ParagraphBreak secondPassTokens, []) currentToken
        | [Token.ProbableVerseNumber number; Token.VerseMarker] ->
            Sequence.append (SecondPassToken.VerseNumber number) secondPassTokens, []
        | [Token.ProbableVerseNumber number; Token.Punctuation ")\"" ] ->
            Seq.append [SecondPassToken.VerseNumber number; SecondPassToken.Text "\""] secondPassTokens, []
        | Token.FootnoteMarker :: footnoteTextTokens ->
            match currentToken with
            | Token.Text _ ->
                secondPassTokens, currentToken :: tokenBuffer
            | Token.Punctuation _ ->
                secondPassTokens, currentToken :: tokenBuffer
            | _ ->
                let footnoteText =
                    footnoteTextTokens
                    |> List.map (fun token -> match token with
                                              | Token.Text text -> sprintf " %s" text
                                              | Token.Punctuation punctuation -> punctuation
                                              | _ -> "")
                    |> String.concat ""
                    |> SecondPassToken.FootnoteText
                in Sequence.append footnoteText secondPassTokens, [currentToken]
        | [previousToken; _] ->
            structurePoorlyStructuredData (structurePoorlyStructuredData (secondPassTokens, []) previousToken) currentToken
        | tokens ->
            failwithf "Unexpected case of tokens on second pass: %A" tokens

    let private insertChapterNumbers (tokens, chapterNumber) token =
        match token with
        | SecondPassToken.VerseNumber number ->
            // Deal with cases like 1a, 1-4, etc
            let pureNumericVerseNumber = match Seq.tryFindIndex (fun c -> not (System.Char.IsDigit c)) number with
                                         | Some index -> number.[..index-1]
                                         | None -> number
            if System.UInt32.Parse(pureNumericVerseNumber) = 1u
            then
                Seq.append tokens [SecondPassToken.ChapterNumber chapterNumber; token], chapterNumber + 1u
            else
                Sequence.append token tokens, chapterNumber
        | _ ->
            Sequence.append token tokens, chapterNumber

    let private divideIntoChapters (preChapterElements, chapters : (uint32 * ChapterElement list) list) token =
        let addToChapter element =
            if chapters = []
            then element :: preChapterElements, chapters
            else
                let updatedContents = element :: (snd chapters.Head)
                let updatedChapter = fst chapters.Head, updatedContents
                preChapterElements, updatedChapter :: chapters.Tail

        let addToVerse element =
            match Seq.head (snd chapters.Head) with
            | ChapterElement.Verse verse ->
                let updatedVerse = ChapterElement.Verse { verse with Text = Sequence.append element verse.Text }
                let updatedContents = updatedVerse :: (snd chapters.Head).Tail
                let updatedChapter = fst chapters.Head, updatedContents
                preChapterElements, updatedChapter :: chapters.Tail
            | _ ->
                if element = VerseElement.ParagraphBreak
                then addToChapter ChapterElement.ParagraphBreak
                else
                    printfn "ERROR! failed to add element: %A" element
                    preChapterElements, chapters

        match token with
        | SecondPassToken.ChapterNumber number ->
            preChapterElements, (number, []) :: chapters
        | SecondPassToken.FootnotePositionMarker ->
            addToVerse (VerseElement.Footnote "TODO cannot correlate with footnote text!!")
        | SecondPassToken.FootnoteText text ->
            sprintf "TODO don't know what footnote this text belongs in: '%s'" text
            |> VerseElement.Text
            |> addToVerse
        | SecondPassToken.ParagraphBreak ->
            if chapters = []
            then addToChapter ChapterElement.ParagraphBreak
            else addToVerse VerseElement.ParagraphBreak
        | SecondPassToken.Punctuation punct ->
            addToVerse (VerseElement.Text punct)
        | SecondPassToken.Text text ->
            addToVerse (VerseElement.Text <| " " + text)
        | SecondPassToken.VerseNumber number ->
            addToChapter (ChapterElement.Verse { Number = number; Text = Seq.empty })
        | SecondPassToken.SectionTitle title ->
            addToChapter (ChapterElement.SectionTitle title)

    let private bookFromTokens tokens =
        tokens
        |> Seq.fold structurePoorlyStructuredData (Seq.empty, [])
        |> fst
        |> Seq.fold insertChapterNumbers (Seq.empty, 1u)
        |> fst
        |> Seq.fold divideIntoChapters ([], [])

    let structureChapters (chapters : (uint32 * ChapterElement list) list) =
        chapters
        |> List.rev
        |> Seq.map (fun (number, elements) ->
            {
                Number = string number
                Contents = Seq.ofList <| List.rev elements
            })
        
    let parseXml xml title =
        let preChapterElements, chapters = tokenize xml |> bookFromTokens
        in {
            Title = title
            PreChapterElements = Seq.ofList <| List.rev preChapterElements
            Chapters = structureChapters chapters
        }
