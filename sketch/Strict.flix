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


namespace Text/PrettyPrint/Strict {

    pub enum Doc {
        case Empty
        case Cat(Doc, Doc)
        case Char(Char)
        case Text(String)
        case Nest(Int32, Doc)
        case Break(String)
        case Group(Doc)
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
                let prefix = String.repeat(" ", i);
                appendLineSeparator!(sb);
                appendString!(sb, prefix);
                sdocWorker(sb, d, sk)
            }          
        }

    pub enum Mode { 
        case Flat1
        case Break1
    }

    type alias Format1 = (Int32, Mode, Doc)

    pub def fits(width: Int32, xs: List[Format1]): Bool = 
        match xs {
            case _ if (width < 0) => false 
            case Nil => true
            case (_, _, Empty) :: rs        => fits(width, rs)
            case (i, m, Cat(x, y)) :: rs    => fits(width, (i, m, x) :: (i, m, y) :: rs)
            case (i, m, Nest(j, x)) :: rs   => fits(width, (i + j, m, x) :: rs)
            case (_, _, Text(s)) :: rs      => fits(width - String.length(s), rs)
            case (_, _, Char(_)) :: rs      => fits(width - 1, rs)
            case (_, Flat1, Break(s)) :: rs => fits(width - String.length(s), rs)
            case (_, Break1, Break(_)) :: _ => true    // Impossible
            case (i, _, Group(x)) :: rs     => fits(width, (i, Flat1, x) :: rs)
        }

    def format (width: Int32, col: Int32, xs: List[Format1]): SDoc =
        match xs {
            case  Nil => SEmpty
            case (_ ,_ , Empty) :: rs           => format(width, col, rs)
            case (i, m, Cat(x, y)) :: rs        => format(width, col, (i, m, x) :: (i, m, y) :: rs)
            case (i, m, Nest(j, x)) :: rs       => format(width, col, (i+j, m, x) :: rs)
            case (_, _, Text(s)) :: rs          => {let d1 = format(width, col + String.length(s), rs); SText(s, d1)}
            case (_, _, Char(c)) :: rs          => {let d1 = format(width, col + 1, rs); SText(ToString.toString(c), d1)}
            case (_, Flat1, Break(s)) :: rs     => {let d1 = format(width, col + String.length(s), rs); SText(s, d1)}
            case (i, Break1, Break(_)) :: rs    => {let d1 = format(width, i, rs); SLine(i, d1)}
            case (i, _, Group(x)) :: rs         =>
                if (fits(width - col, (i, Flat1, x) :: rs))
                    format(width, col, (i, Flat1, x) :: rs)
                else
                    format(width, col, (i, Break1, x) :: rs)
        }

    pub def render(lineWidth: Int32, doc: Doc): String =
        format(lineWidth, 1, (1, Flat1, doc) :: Nil) |> sdocToString

}
