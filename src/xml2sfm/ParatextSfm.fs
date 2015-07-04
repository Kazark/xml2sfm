namespace xml2sfm

[<RequireQualifiedAccess>]
module ParatextSfm =

    [<RequireQualifiedAccess>]
    type FormatMarker =
        | BookId of string
        | ChapterNumber of string
        | ParagraphNoFirstLineIndent
        | SectionLevel1Heading of string
        | VerseNumber of string
        // Footnote markers
        | BeginFootnoteMarker
        | FootnoteReference of string * string
        | FootnoteText of string
        | EndFootnoteMarker

    type TextToken = Choice<FormatMarker, string>

    let private writeSfm =
        sprintf "\%s %s "

    let private writeSfmOnly =
        sprintf "\%s"

    let private writeSfmLine sfm text =
        sprintf "\%s %s%s" sfm text System.Environment.NewLine

    let private writeSfmLineOnly sfm =
        sprintf "%s\%s%s" System.Environment.NewLine sfm System.Environment.NewLine

    let private stringifySfm =
        function
        | FormatMarker.BookId id ->
            writeSfmLine "id" id
        | FormatMarker.ChapterNumber chapterNumber ->
            writeSfmLine "c" chapterNumber
        | FormatMarker.ParagraphNoFirstLineIndent ->
            writeSfmLineOnly "m"
        | FormatMarker.SectionLevel1Heading title ->
            writeSfmLine "s" title
        | FormatMarker.VerseNumber verseNumber ->
            writeSfm "v" verseNumber
        | FormatMarker.BeginFootnoteMarker ->
            writeSfm "f" "+"
        | FormatMarker.FootnoteReference (chapterNumber, verseNumber) ->
            writeSfm "fr" (sprintf "%s.%s" chapterNumber verseNumber)
        | FormatMarker.FootnoteText text ->
            writeSfm "ft" text
        | FormatMarker.EndFootnoteMarker ->
            writeSfmOnly "f*"

    let private stringifyTextToken =
        function
        | Choice.Choice1Of2 sfm ->
            stringifySfm sfm
        | Choice.Choice2Of2 text ->
            text

    let tokenizeFootnote chapterNumber verseNumber footnote =
        [
            FormatMarker.BeginFootnoteMarker
            FormatMarker.FootnoteReference (chapterNumber, verseNumber)
            FormatMarker.FootnoteText footnote
            FormatMarker.EndFootnoteMarker
        ]
        |> Seq.map TextToken.Choice1Of2

    let tokenizeText text =
        TextToken.Choice2Of2 text |> Seq.singleton

    let tokenizeVerseNumber number =
        FormatMarker.VerseNumber number |> TextToken.Choice1Of2

    let private tokenizedParagraphBreak =
        FormatMarker.ParagraphNoFirstLineIndent |> TextToken.Choice1Of2

    let tokenizeVerseText chapterNumber verseNumber verseText =
        let tokenizeVerseElement =
            function
            | VerseElement.Text text -> tokenizeText text
            | VerseElement.Footnote footnote -> tokenizeFootnote chapterNumber verseNumber footnote
            | VerseElement.ParagraphBreak -> tokenizedParagraphBreak |> Seq.singleton
        Seq.map tokenizeVerseElement verseText
        |> Seq.concat

    let private tokenizeVerse chapterNumber (verse : Verse) =
        Sequence.prepend (tokenizeVerseNumber verse.Number) (tokenizeVerseText chapterNumber verse.Number verse.Text)

    let private tokenizeSectionTitle title = 
        FormatMarker.SectionLevel1Heading title |> TextToken.Choice1Of2

    let private tokenizeChapterContents chapterNumber contents =
        let tokenizeChapterElement =
            function
            | ChapterElement.ParagraphBreak -> tokenizedParagraphBreak |> Seq.singleton
            | ChapterElement.SectionTitle title -> tokenizeSectionTitle title |> Seq.singleton
            | ChapterElement.Verse verse -> tokenizeVerse chapterNumber verse
        Seq.map tokenizeChapterElement contents
        |> Seq.concat

    let private tokenizeChapterNumber number =
        FormatMarker.ChapterNumber number |> TextToken.Choice1Of2

    let private tokenizeChapter chapter =
        Sequence.prepend (tokenizeChapterNumber chapter.Number) (tokenizeChapterContents chapter.Number chapter.Contents)

    let private tokenizeAllChapters chapters =
        Seq.map tokenizeChapter chapters
        |> Seq.concat

    let private tokenizeBookTitle title =
        FormatMarker.BookId title |> TextToken.Choice1Of2

    let private tokenizeBook book =
        Seq.append (tokenizeChapterContents "" book.PreChapterElements) (tokenizeAllChapters book.Chapters)
        |> Sequence.prepend (tokenizeBookTitle book.Title)

    let stringifyBook book =
        tokenizeBook book
        |> Seq.map stringifyTextToken
        |> String.concat ""
