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


namespace Text/PPrint {

    use Text/PPrint/Internal/ADoc.{ADoc};

    pub enum Doc { 
        case DocEmpty
        case Text(String)
        case HCat(Doc, Doc)
        case VCat(Doc, Doc)
        case Alt(Doc, Doc)
    }

    
    pub def alt(d1: Doc, d2: Doc): Doc = Alt(d1, d2)
    
    pub def empty(): Doc = DocEmpty

    pub def beside(x: Doc, y: Doc): Doc = HCat(x, y)
    
    pub def <>(x: Doc, y: Doc): Doc = HCat(x, y)

    pub def <&&>(x: Doc, y: Doc): Doc = VCat(x, y)

    pub def <<>>(x: Doc, y: Doc): Doc = x <> space() <> y

    pub def foldDoc(fn: Doc -> Doc -> Doc, ds: List[Doc]): Doc = 
        foldDocHelper(fn, ds, d1 -> d1)

    def foldDocHelper(fn: Doc -> Doc -> Doc, ds: List[Doc], k: Doc -> Doc): Doc =
        match ds { 
            case Nil => k(empty())
            case d1 :: Nil => k(d1)
            case d1 :: rs => foldDocHelper(fn, rs, v1 -> k(fn(d1, v1)))
        }

    pub def punctuate(sep: Doc, docs:List[Doc]): List[Doc] = punctuateHelper(sep, docs, xs -> xs)


    pub def punctuateHelper(sepa: Doc, ds:List[Doc], k: List[Doc] -> List[Doc]): List[Doc] = match ds {
        case Nil => k(Nil)
        case d :: Nil => k(d :: Nil)
        case d :: rs    => punctuateHelper(sepa, rs, xs -> k((d <> sepa) :: xs))
    }

    pub def intersperse(sepa: Doc, docs:List[Doc]): Doc = sep(punctuate(sepa, docs))


    pub def hcat(xs: List[Doc]): Doc = foldDoc(x1 -> y1 -> x1 <> y1, xs)

    pub def vcat(xs: List[Doc]): Doc = foldDoc(x1 -> y1 -> x1 <&&> y1, xs)
    
    pub def hsep(xs: List[Doc]): Doc = foldDoc(x1 -> y1 -> x1 <<>> y1, xs)

    pub def sep(xs: List[Doc]): Doc = alt(hsep(xs), vcat(xs))

    pub def text(s: String): Doc = Text(s)

    pub def char(x: Char): Doc = Text(ToString.toString(x))

    pub def hang(n: Int32, x: Doc, y: Doc): Doc = 
        alt(x <> y, x <&&> nest(n, y))

    pub def nest(n: Int32, x: Doc): Doc = 
        spaces(n) <> x
    

    pub def render(pageWidth: Int32, d: Doc): Option[String] = 
        toADoc(pageWidth, d)
            |> Text/PPrint/Internal/ADoc.render

    ///
    /// Render `doc` to a file, pretty printed with a line width of `width`.
    ///
    pub def writePretty(width: Int32, path: String, cs: Text.Charset, doc: Doc): Result[Unit, String] & Impure =
        use Result.flatMap;
        let* str = Option.toOk(render(width, doc), "writePretty - render failed");
        let path1 = System/FilePath.new(path);
        System/File.writeFile(path1, cs, str)


    def toADoc(pageWidth: Int32, d: Doc): ADoc = 
        toADocHelper(pageWidth, d, d1 -> d1)

    def toADocHelper(pw: Int32, d: Doc, k: ADoc -> ADoc): ADoc = 
        use Text/PPrint/Internal/ADoc.{empty, text, hcat, vcat, alt};
        match d {
            case DocEmpty => k(empty(pw))
            case Text(s) => k(text(pw, s))
            case HCat(d1, d2) => 
                toADocHelper(pw, d1, v1 -> 
                    toADocHelper(pw, d2, v2 -> 
                        k(hcat(pw, v1, v2))))
            case VCat(d1, d2) => 
                toADocHelper(pw, d1, v1 -> 
                    toADocHelper(pw, d2, v2 -> 
                        k(vcat(pw, v1, v2))))                        
            case Alt(d1, d2) => 
                toADocHelper(pw, d1, v1 -> 
                    toADocHelper(pw, d2, v2 -> 
                        k(alt(v1, v2)))) 
        }

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
    ///
    pub def enclose(l: Doc, r: Doc, body: Doc): Doc = l <> body <> r

    ///
    /// Enclose the document body between `l` (left) and `r` (right).
    /// 
    ///
    pub def softEnclose(l: Doc, r: Doc, body: Doc): Doc = sep(l :: body :: r :: Nil)

    ///
    /// Enclose in single quotes '...'
    ///
    pub def squotes(x: Doc): Doc = enclose(squote(), squote(), x)
    
    ///
    /// Enclose in double quotes "..."
    ///
    pub def dquotes(x: Doc): Doc = enclose(dquote(), dquote(), x)
    
    ///
    /// Enclose in curly braces {...}
    ///
    pub def braces(x: Doc): Doc = enclose(lbrace(), rbrace(), x)

    
    ///
    /// Enclose in curly braces {...}
    ///
    pub def softBraces(x: Doc): Doc = softEnclose(lbrace(), rbrace(), x)


    ///
    /// Enclose in parentheses (...)
    ///
    pub def parens(x: Doc): Doc = enclose(lparen(), rparen(), x)

    ///
    /// Enclose in curly braces {...}
    ///
    pub def softParens(x: Doc): Doc = softEnclose(lparen(), rparen(), x)

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