/*
 * Copyright 2019 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

namespace Text/PrettyPrint {

    pub enum Doc {
        case Empty,
        case Text(String),
        case Line(Bool),
        case Cat(Doc,Doc),
        case Nest(Int32,Doc),
        case Group(Doc),
        case Column(Int32 -> Doc),
        case Nesting(Int32 -> Doc)
    }

    enum SimpleDoc {
        case SEmpty,
        case SText(String, SimpleDoc),
        case SLine(String, SimpleDoc)
    }

    def extend(s: String, n: Int32): String = 
        s + String.repeat(" ", n)


    def flatten(columnPos: Int32, doc: Doc) : Doc =
        flattenHelper(columnPos, 0, doc, (x,y) -> (x,y)) |> snd

    def flattenHelper(column: Int32, nest: Int32, doc: Doc, cont : (Int32, Doc) -> (Int32, Doc)): (Int32, Doc) = 
        match doc {
            case Empty => cont(column, Empty)
            case Text(s) => cont(column + String.length(s), doc)
            case Cat(x,y) => 
                flattenHelper(column, nest, x, (col1,x1) -> 
                    flattenHelper(col1, nest, y, (col2, y1) -> 
                        cont(col2, Cat(x1, y1))))
            case Nest(n,x) => flattenHelper(column, nest+n, x, cont)
            case Line(true) => cont(column, Empty)
            case Line(false) => cont(column+1, Text(" "))
            case Group(x) => flattenHelper(column, nest, x, cont)
            case Column(fn) => flattenHelper(column, nest, fn(column), cont)
            case Nesting(fn) => flattenHelper(column, nest, fn(nest), cont)
        }

    def layout (width: Int32, doc: Doc): SimpleDoc = 
        layoutBest(width, 0, ("",doc) :: Nil, false, identity, _ -> SEmpty)

    def layoutBest(width: Int32, col: Int32, docs: List[(String, Doc)], alternate: Bool, sk: SimpleDoc -> SimpleDoc, fk: Unit -> SimpleDoc): SimpleDoc = 
        match docs {
            case Nil => sk(SEmpty)
            case (_, Empty) :: rs => layoutBest(width, col, rs, alternate, sk, fk)
            case (iz, Cat(x,y)) :: rs =>
                layoutBest(width, col, (iz,x) :: (iz,y) :: rs, alternate, sk, fk)
            case (iz, Nest(n,x)) :: rs => 
                layoutBest(width, col, (extend(iz,n),x) :: rs, alternate, sk, fk)
            case (iz, Line(_)) :: rs =>
                let sk1 = v1 -> sk(SLine(iz, v1));
                layoutBest(width, String.length(iz), rs, alternate, sk1, fk)
            case (iz, Group(x)) :: rs =>
                let fk1 = _ -> layoutBest(width, col, (iz, x) :: rs, alternate, sk, fk);
                layoutBest(width, col, (iz, flatten(col,x)) :: rs, true, sk, fk1)
            case (_, Text(t)) :: rs =>
                if ((width >= 0) && alternate && isTooBig(t,col,width))
                    fk()
                else {
                    let sk1 = v1 -> sk(SText(t, v1));
                    layoutBest(width, col + String.length(t), rs, alternate, sk1, fk)
                }
            case (iz, Column(fn)) :: rs =>
                layoutBest(width, col, (iz, fn(col)) :: rs, alternate, sk, fk)
            case (iz, Nesting(fn)) :: rs =>
                layoutBest(width, col, (iz, fn(String.length(iz))) :: rs, alternate, sk, fk)
        }

    def isTooBig(text: String, col: Int32, width: Int32) : Bool = 
        col + String.length(text) > width


    ///
    /// Returns `doc` pretty printed with a line width of `width`.
    ///
    pub def prettyPrint(width: Int32, doc: Doc) : String = prettyPrintHelper(width, doc) as & Pure

    ///
    /// Helper function for `prettyPrint`.
    ///
    def prettyPrintHelper(width: Int32, doc: Doc) : String & Impure =
        let sb = StringBuilder.new();
        prettyPrint!(sb, width, doc);
        StringBuilder.toString(sb)

    ///
    /// Appends StringBuilder `sb` with `doc` pretty printed with a line width of `width`.
    ///
    pub def prettyPrint!(sb: StringBuilder, width: Int32, doc: Doc): Unit & Impure =
        let simpleDoc = layout(width, doc);
        renderSimpleDoc!(sb, simpleDoc)

    ///
    /// Appends StringBuilder `sb` with `doc` pretty printed with a line width of `width`.
    ///
    pub def writePretty!(width: Int32, path: String, cs: Text.Charset, doc: Doc): Result[Unit, System.Error] & Impure = 
        use Result.flatMap;
        let str = prettyPrint(width, doc);
        let path1 = System/FilePath.new(path);
        System/File.writeFile(path1, cs, str)

    ///
    /// Renders a SimpleDoc.
    ///
    def renderSimpleDoc!(sb: StringBuilder, sdoc: SimpleDoc) : Unit & Impure =
        match sdoc {
            case SEmpty => ()
            case SText(t,rest) => { 
                StringBuilder.appendString!(sb, t);
                renderSimpleDoc!(sb, rest)
            }
            case SLine(x,rest) => { 
                StringBuilder.appendLineSeparator!(sb);
                StringBuilder.appendString!(sb, x);
                renderSimpleDoc!(sb, rest)
            }
        }

    ///
    /// Returns `doc` pretty printed with a width of 80.
    ///
    pub def toString(doc: Doc): String = prettyPrint(80, doc)


    ///
    /// # Primitive printers
    ///

    ///
    /// Returns the empty document.
    ///
    pub def empty(): Doc = Empty

    ///
    /// Returns the document containing the literal string `s`.
    ///
    /// If the input text contains newlines it will be assembled as a list of lines.
    ///
    pub def text(s: String): Doc = match String.lines(s) {
        case Nil => Text("")
        case x :: Nil => Text(x)
        case xs => vcat(List.map(Text, xs))
    }

    ///
    /// `nest` renders the document `doc` with the current indentation level 
    /// increased by `i`
    ///
    pub def nest(i: Int32, doc: Doc): Doc = Nest (i, doc)

    ///
    /// Undocumented - used by `align`.
    /// `column` gives access to the current column position.
    ///
    pub def column(f: Int32 -> Doc): Doc = Column(f)

    ///
    /// Undocumented - used by `align`.
    /// `nesting` gives access to the current nesting position.
    /// The nesting position is not the same as the column position.
    pub def nesting(f: Int32 -> Doc): Doc = Nesting(f)

    ///
    /// Use the `group` combinator to specify alternate layouts.
    /// `(group doc)` undoes all linebreaks in doc.
    ///
    pub def group(doc: Doc): Doc = Group(doc)

    ////
    /// `line` advances to the next line and indents to the current nesting level.
    /// If the line break is undone by group `line` is rendered as a space.
    ///
    pub def line(): Doc = Line(false)
    
    ///
    /// `linebreak` advances to the next line and indents to the current nesting 
    /// level.
    /// If the line break is undone by group `linebreak` is rendered as empty.    
    ///
    pub def linebreak(): Doc = Line(true)

    ///
    /// This is 'char' in PPrint (Haskell).
    ///
    pub def char(ch: Char): Doc = 
        match ch {
            case '\n' => line()
            case '\r' => line()
            case _ => Char.toString(ch) |> text
        }
        
    ///
    /// `softline` behaves like `space` if the document it is part of fits the page.
    /// If it is too large it renders as `line`.
    ///
    pub def softline(): Doc = line() |> group

    ///
    /// `softbreak` behaves like `empty` if the document it is part of fits the page.
    /// If it is too large it renders as `line`.
    ///
    pub def softbreak(): Doc = linebreak() |> group

    ///
    /// Concatenate documents `x` and `y`.
    ///
    pub def beside(x: Doc, y: Doc): Doc = Cat(x,y)

    ///
    /// Concatenate two documents horizontally (no separating space).
    ///
    pub def <>(x: Doc, y: Doc): Doc = beside(x,y)

    ///
    /// Concatenate two documents horizontally with a separating space.
    ///
    pub def besideSpace(x: Doc, y: Doc): Doc = x <> char(' ') <> y

    ///
    /// Concatenate two documents horizontally with a separating space.
    /// 
    pub def <<>>(x: Doc, y:Doc): Doc = besideSpace(x,y)

    ///
    /// Concatenate two documents with a soft line.
    /// This is (</>) in PPrint (Haskell).
    ///
    pub def besideSoftline(x: Doc, y: Doc): Doc = x <> softline() <> y

    ///
    /// Concatenate two documents with a soft line.
    /// This is (</>) in PPrint (Haskell).
    ///
    pub def <!>(x: Doc, y: Doc): Doc = besideSoftline(x, y)


    ///
    /// Concatenate two documents with a soft break.
    /// This is (<//>) in PPrint (Haskell).
    ///
    pub def besideSoftbreak(x: Doc, y: Doc): Doc = x <> softbreak() <> y

    ///
    /// Concatenate two documents with a soft break.
    /// This is (<//>) in PPrint (Haskell).
    ///
    pub def <!!>(x: Doc, y: Doc): Doc = besideSoftbreak(x, y)

    ///
    /// Concatenate two documents separating with `line`.
    /// This is (<$>) in PPrint (Haskell).
    ///
    pub def <&>(x: Doc, y: Doc): Doc = x <> line() <> y

    

    ///
    /// Concatenate two documents separating with `linebreak`.
    /// This is (<$$>) in PPrint (Haskell).
    ///
    pub def <&&>(x: Doc, y: Doc): Doc = x <> linebreak() <> y

    ///
    /// Alias for `<&&>`
    ///
    pub def vlinebreak(x: Doc, y: Doc): Doc = x <&&> y


    // ************************************************************************
    // List concatenation 

    pub def foldDocs(op: (Doc, Doc) -> Doc, docs: List[Doc]): Doc = 
        match docs {
            case Nil => empty()
            case x :: rs => foldDocsHelper(op, x, rs, identity)
        }

    def foldDocsHelper(op: (Doc, Doc) -> Doc, acc: Doc, docs:List[Doc], cont: Doc -> Doc): Doc = 
        match docs {
            case Nil => cont(acc)
            case x :: rs => foldDocsHelper(op, op(acc,x), rs, cont)
        }

    pub def punctuate(sep: Doc, docs:List[Doc]): List[Doc] = punctuateHelper(sep, docs, xs -> xs)
    

    pub def punctuateHelper(sep: Doc, ds:List[Doc], k: List[Doc] -> List[Doc]): List[Doc] = match ds {
        case Nil => k(Nil)
        case d :: Nil => k(d :: Nil)
        case d :: rs    => punctuateHelper(sep, rs, xs -> k((d <> sep) :: xs))
    }

    ///
    /// Punctuate the list of documents with the separator `sep` and bookend them 
    /// with `left` and `right`.
    ///
    pub def encloseSep(left:Doc, right:Doc, sep: Doc, docs: List[Doc]): Doc = 
        encloseSepHelper(sep, docs, left, d -> d <> right)

    ///
    /// Helper function for `encloseSep`.
    ///
    def encloseSepHelper(sep: Doc, docs: List[Doc], acc: Doc, cont: Doc -> Doc): Doc = 
        match docs {
            case Nil => cont(acc)
            case x :: Nil => {
                let acc1 = acc <!!> x;
                cont(acc1)
            }
            case x :: rs => {
                let acc1 = acc <!!> x <> sep;
                encloseSepHelper(sep, rs, acc1, cont)
            }
        }

    ///
    /// Enclose in parens and separate with comma (a,b,c,...)
    ///
    pub def tupled(docs: List[Doc]) : Doc = encloseSep(lparen(), rparen(), comma(), docs)

    ///
    /// Separate documents horizontally with a space.
    ///
    pub def hsep(docs: List[Doc]): Doc = foldDocs((x,y) -> x <<>> y, docs)

    ///
    /// Separate documents vertically with (<&>)
    ///
    pub def vsep(docs: List[Doc]): Doc = foldDocs((x,y) -> x <&> y, docs)

    ///
    /// Concatenate documents horizontally (no space).
    ///
    pub def hcat(docs: List[Doc]): Doc= foldDocs((x,y) -> x <> y, docs)

    ///
    /// Separate documents with (<&&>)
    ///
    pub def vcat(docs: List[Doc]): Doc = foldDocs((x,y) -> x <&&> y, docs)

    ///
    /// Horizontally until it doesn't fit...
    ///
    pub def fillCat(docs: List[Doc]): Doc = List.foldRight((x,y) -> x <!!> y, empty(), docs)


    ///
    /// Concat horizontally or vertically...
    ///
    pub def cat(docs: List[Doc]): Doc = group(vcat(docs))

    ///
    /// `(align d)` renders the document `d` with the nesting level set to 
    /// the current column.
    ///
    pub def align(doc: Doc) :Doc = 
        column(k -> nesting(i -> nest(k - i, doc)))

    ///
    /// Implement hanging indentation.
    ///
    pub def hang(i: Int32, doc: Doc): Doc = align(nest(i,doc))

    ///
    /// Indent the document `doc` with `i` spaces.
    ///
    pub def indent(i: Int32, doc: Doc): Doc = hang(i, repeatString(" ", i) <> doc)

    ///
    /// Print an Int8 literal.
    ///
    pub def int8(i: Int8): Doc = Int8.toString(i) |> text

    ///
    /// Print an Int16 literal.
    ///
    pub def int16(i: Int16): Doc = Int16.toString(i) |> text

    ///
    /// Print an Int32 literal.
    ///
    pub def int32(i: Int32): Doc = Int32.toString(i) |> text

    ///
    /// Print an Int64 literal.
    ///
    pub def int64(i: Int64): Doc = Int64.toString(i) |> text

    ///
    /// Print an float32 literal.
    ///
    pub def float32(d: Float32): Doc = Float32.toString(d) |> text

    ///
    /// Print an float64 literal.
    ///
    pub def float64(d: Float64): Doc = Float64.toString(d) |> text

    ///
    /// Print an BigInt literal.
    ///
    pub def bigInt(i: BigInt): Doc = BigInt.toString(i) |> text

    pub def repeatString(s: String, n: Int32): Doc = 
        String.repeat(s, n) |> text

    pub def repeat(d: Doc, n: Int32): Doc = 
        List.repeat(d, n) |> hcat

    // ************************************************************************
    // Character printers

    ///
    /// Single left parenthesis: '('
    ///
    pub def lparen() : Doc = char('(')

    ///
    /// Single right parenthesis: ')'
    ///
    pub def rparen() : Doc = char(')')

    ///
    /// Single left angle: '<'
    ///
    pub def langle() : Doc = char('<')

    ///
    /// Single right angle: '>'
    ///
    pub def rangle() : Doc = char('>')

    ///
    /// Single left brace: '{'
    ///
    pub def lbrace(): Doc = char('{')
    
    ///
    /// Single right brace: '}'
    ///
    pub def rbrace(): Doc= char('}')
    
    ///
    /// Single left square bracket: '['
    ///
    pub def lbracket(): Doc = char('[')
    
    ///
    /// Single right square bracket: ']'
    ///
    pub def rbracket(): Doc = char(']')

    ///
    /// Single quote
    ///
    pub def squote(): Doc= char('\'')

    ///
    /// The document @dquote@ contains a double quote
    ///
    pub def dquote(): Doc = char('"')

    ///
    /// The document @semi@ contains a semi colon
    ///
    pub def semi(): Doc = char(';')

    ///
    /// The document @colon@ contains a colon
    ///
    pub def colon(): Doc = char(':')

    ///
    /// The document @comma@ contains a comma
    ///
    pub def comma() : Doc = char(',')

    ///
    /// The document @space@ contains a single space
    ///
    pub def space() : Doc = char(' ')

    ///
    /// The document @dot@ contains a single dot
    ///
    pub def dot(): Doc = char('.')

    ///
    /// The document @backslash@ contains a back slash
    ///
    pub def backslash(): Doc = char('\\')

    ///
    /// The document @equals@ contains an equal sign
    ///
    pub def equals(): Doc = char('=')

    ///
    /// Generate a document of n spaces
    ///
    pub def spaces(n: Int32): Doc = repeatString(" ", n)

    ///
    /// Enclose the document body between `l` (left) and `r` (right).
    ///
    pub def enclose(l: Doc, r: Doc, body: Doc): Doc = l <> body <> r

    ///
    /// Enclose in single quotes '...'
    ///
    pub def squotes(x: Doc): Doc = enclose(squote(), squote(), x)
    
    ///
    /// Enclose in double quotes "..."
    ///
    pub def dquotes(x: Doc): Doc = enclose(dquote(), dquote(), x)
    
    ///
    /// Enclose in angle braces {...}
    ///
    pub def braces(x: Doc): Doc = enclose(lbrace(), rbrace(), x)
    
    ///
    /// Enclose in square brackets (...)
    ///
    pub def parens(x: Doc): Doc = enclose(lparen(), rparen(), x)
    
    ///
    /// Enclose in angle brackets <...>
    ///
    pub def angles(x: Doc): Doc = enclose(langle(), rangle(), x)
    
    ///
    /// Enclose in square brackets [...]
    ///
    pub def brackets (x: Doc): Doc = enclose(lbracket(), rbracket(), x)
    
    
 }
