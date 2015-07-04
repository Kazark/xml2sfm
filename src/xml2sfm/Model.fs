namespace xml2sfm

[<RequireQualifiedAccess>]
type VerseElement =
    | Text of string
    | ParagraphBreak
    | Footnote of string

type Verse = {
    Number : string
    Text : VerseElement seq
}

[<RequireQualifiedAccess>]
type ChapterElement =
    | SectionTitle of string
    | ParagraphBreak
    | Verse of Verse

type Chapter = {
    Number : string
    Contents : ChapterElement seq
}

type Book = {
    Title : string
    PreChapterElements : ChapterElement seq
    Chapters : Chapter seq
}