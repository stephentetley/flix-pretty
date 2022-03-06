/*
 * Copyright 2021 Stephen Tetley
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

/// ## Acknowledgment
///
/// This is an implementation of Christian Lindig's Strictly Pretty.
///
/// The original Haskell library was strictified for Mercury by Ralph Becket
/// and subsequently ported to Racket by David Herman. This library derives 
/// from both the Haskell library and Racket one.


namespace Text/PrettyPrint {

    pub enum Doc {
        case Empty
        case Cat(Doc, Doc)
        case Char(Char)
        case Text(String)
        case Nest(Int32, Doc)
        case Break(String)
        case Group(Doc)
    }
    
    instance ToString[Doc] {
        pub def toString(d: Doc): String = render(80, d)
    }

    enum SDoc { 
        case SEmpty
        case SText(String, SDoc)
        case SLine(Int32, SDoc)
    }

    pub def sdocToString(source: SDoc): String = 
        sdocToStringHelper(source) as & Pure 


    def sdocToStringHelper(source: SDoc): String & Impure = 
        let sb = StringBuilder.new();
        sdocWorker(sb, source, k1 -> k1);
        StringBuilder.toString(sb)

    def sdocWorker(sb: StringBuilder, sdoc: SDoc, sk: Unit -> Unit): Unit & Impure = 
        use StringBuilder.{appendString!, appendLineSeparator!};
        match sdoc {
            case SEmpty => sk()
            case SText(s, d) => {
                appendString!(sb, s);  
                sdocWorker(sb, d, sk)
            }
            case SLine(i, d) => {
                let prefix = String.repeat(i, " ");
                appendLineSeparator!(sb);
                appendString!(sb, prefix);
                sdocWorker(sb, d, sk)
            }          
        }

    pub enum GroupMode { 
        case GFlat
        case GBreak
    }

    type alias Format1 = (Int32, GroupMode, Doc)

    /// `fits` is already tail recursive so doesn't need CPS transforming.
    def fits(width: Int32, xs: List[Format1]): Bool = 
        match xs {
            case _ if (width < 0) => false 
            case Nil => true
            case (_, _, Empty) :: rs        => fits(width, rs)
            case (i, m, Cat(x, y)) :: rs    => fits(width, (i, m, x) :: (i, m, y) :: rs)
            case (i, m, Nest(j, x)) :: rs   => fits(width, (i + j, m, x) :: rs)
            case (_, _, Text(s)) :: rs      => fits(width - String.length(s), rs)
            case (_, _, Char(_)) :: rs      => fits(width - 1, rs)
            case (_, GFlat, Break(s)) :: rs => fits(width - String.length(s), rs)
            case (_, GBreak, Break(_)) :: _ => true    // Impossible
            case (i, _, Group(x)) :: rs     => fits(width, (i, GFlat, x) :: rs)
        }

    /// 
    def format (width: Int32, col: Int32, xs: List[Format1], cont: SDoc -> SDoc): SDoc =
        match xs {
            case  Nil => cont(SEmpty)
            case (_ ,_ , Empty) :: rs           => format(width, col, rs, cont)
            case (i, m, Cat(x, y)) :: rs        => format(width, col, (i, m, x) :: (i, m, y) :: rs, cont)
            case (i, m, Nest(j, x)) :: rs       => format(width, col, (i+j, m, x) :: rs, cont)
            case (_, _, Text(s)) :: rs          => 
                format(width, col + String.length(s), rs, v1 -> 
                    cont(SText(s, v1)))

            case (_, _, Char(c)) :: rs          => 
                format(width, col + 1, rs, v1 -> 
                    cont(SText(ToString.toString(c), v1)))

            case (_, GFlat, Break(s)) :: rs     => 
                format(width, col + String.length(s), rs, v1 -> 
                    cont(SText(s, v1)))

            case (i, GBreak, Break(_)) :: rs    => 
                format(width, i, rs, v1 -> 
                    cont(SLine(i, v1)))
                    
            case (i, _, Group(x)) :: rs         =>
                if (fits(width - col, (i, GFlat, x) :: rs))
                    format(width, col, (i, GFlat, x) :: rs, cont)
                else
                    format(width, col, (i, GBreak, x) :: rs, cont)
        }

    pub def render(lineWidth: Int32, doc: Doc): String =
        format(lineWidth, 0, (0, GFlat, doc) :: Nil, d1 -> d1) |> sdocToString


    pub def renderToStringBuilder!(sb: StringBuilder, lineWidth: Int32, doc: Doc): Unit & Impure =
        format(lineWidth, 0, (0, GFlat, doc) :: Nil, d1 -> d1) 
            |> sd1 -> sdocWorker(sb, sd1, d1 -> d1)



    ///
    /// Render `doc` to a file, pretty printed with a line width of `width`.
    ///
    pub def writePretty(width: Int32, path: System.FilePath, cs: Text.Charset, doc: Doc): Result[Unit, String] & Impure =
        use Result.flatMap;
        use System/IO.WriteMode.{Create};
        let str = render(width, doc);
        System/IO.writeFileWith({path = path, charset = cs, mode = Create}, str)


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
    /// This is 'char' in PPrint (Haskell).
    ///
    pub def char(ch: Char): Doc = 
        match ch {
            case '\n' => breakWith(String.lineSeparator())
            case '\r' => breakWith(String.lineSeparator())
            case _ => Char.toString(ch) |> text
        }

    ///
    /// `nest` renders the document `doc` with the current indentation level 
    /// increased by `i`
    ///
    pub def nest(i: Int32, doc: Doc): Doc = Nest(i, doc)

    
    
    ///
    /// `break`
    ///
    pub def break(): Doc = Break(" ")

    ///
    /// `breakWith`
    ///
    pub def breakWith(s: String): Doc = Break(s)

    ///
    /// `group`
    ///
    pub def group(d: Doc): Doc = Group(d)

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
    pub def besideSoftline(x: Doc, y: Doc): Doc = match (x, y) {
        case (Empty, _) => y
        case (_, Empty) => x
        case (_, _) => x <> break() <> y
    }        
    
    ///
    /// Concatenate two documents with a soft line.
    /// This is (</>) in PPrint (Haskell).
    ///
    pub def <!>(x: Doc, y: Doc): Doc = besideSoftline(x, y)


    pub def hang(n: Int32, x: Doc): Doc = 
        group(nest(n, x))



    ///
    /// Concatenate two documents separating with `linebreak`.
    /// This is (<$$>) in PPrint (Haskell).
    ///
    pub def <&>(x: Doc, y: Doc): Doc = match (x, y) { 
        case (Empty, _) => y
        case (_, Empty) => x
        case (_, _) => x <> breakWith(String.lineSeparator()) <> y
    }

    /// TODO - to investigate... this is not working as expected inside a `hang`.
    pub def <&&>(x: Doc, y: Doc): Doc = x <> breakWith(String.lineSeparator()) <> y

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

    pub def sep(docs: List[Doc]): Doc = foldDocs(x -> y -> x <!> y, docs)

    pub def intersperse(sepa: Doc, docs:List[Doc]): Doc = sep(punctuate(sepa, docs))

    ///
    /// Punctuate the list of documents with the separator `sep` and bookend them 
    /// with `left` and `right`.
    ///
    pub def encloseSep(left: Doc, right: Doc, sep: Doc, docs: List[Doc]): Doc = 
        encloseSepHelper(sep, docs, left, d -> d <> right)

    ///
    /// Helper function for `encloseSep`.
    ///
    def encloseSepHelper(sep: Doc, docs: List[Doc], acc: Doc, cont: Doc -> Doc): Doc = 
        match docs {
            case Nil => cont(acc)
            case x :: Nil => {
                let acc1 = acc <!> x;
                cont(acc1)
            }
            case x :: rs => {
                let acc1 = acc <!> x <> sep;
                encloseSepHelper(sep, rs, acc1, cont)
            }
        }


    ///
    /// Enclose in parens and separate with comma (a,b,c,...)
    ///
    pub def tupled(docs: List[Doc]) : Doc = encloseSep(lparen(), rparen(), comma(), docs)

    ///
    /// Enclose in parens and separate with comma [a,b,c,...]
    ///
    pub def commaBrackets(docs: List[Doc]) : Doc = encloseSep(lbracket(), rbracket(), comma(), docs)

    ///
    /// Enclose in parens and separate with comma {a,b,c,...}
    ///
    pub def commaBraces(docs: List[Doc]) : Doc = encloseSep(lbrace(), rbrace(), comma(), docs)

    ///
    /// Enclose in parens and separate with semi [a;b;c,...]
    ///
    pub def semiBrackets(docs: List[Doc]) : Doc = encloseSep(lbracket(), rbracket(), semi(), docs)

    ///
    /// Enclose in parens and separate with semi {a;b;c,...}
    ///
    pub def semiBraces(docs: List[Doc]) : Doc = encloseSep(lbrace(), rbrace(), semi(), docs)

    ///
    /// Separate documents horizontally with a space.
    ///
    pub def hsep(docs: List[Doc]): Doc = foldDocs((x,y) -> x <<>> y, docs)

    /// ///
    /// /// Separate documents vertically with (<&>)
    /// ///
    /// pub def vsep(docs: List[Doc]): Doc = foldDocs((x,y) -> x <&> y, docs)

    ///
    /// Concatenate documents horizontally (no space).
    ///
    pub def hcat(docs: List[Doc]): Doc= foldDocs((x,y) -> x <> y, docs)

    ///
    /// Separate documents with (<&>)
    ///
    pub def vcat(docs: List[Doc]): Doc = foldDocs((x,y) -> x <&> y, docs)

    ///
    /// Concat horizontally or vertically...
    ///
    pub def cat(docs: List[Doc]): Doc = group(vcat(docs))

    ///
    /// Indent the document `doc` with `i` spaces.
    ///
    pub def indent(i: Int32, doc: Doc): Doc = repeatString(" ", i) <> nest(i,  doc)

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
        String.repeat(n, s) |> text

    pub def repeat(d: Doc, n: Int32): Doc = 
        List.repeat(n, d) |> hcat

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
    /// Enclose in parentheses (...)
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
    

    ///
    /// Enclose in parentheses if `b` is true.
    ///
    pub def maybeParens(b: Bool, x: Doc): Doc = if (b) parens(x) else x

}
