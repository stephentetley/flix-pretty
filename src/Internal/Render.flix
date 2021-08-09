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
/// This is an implementation of Samuel Kamin and Thomas Nordin's 
/// strict SML pretty printer.
///
/// The algorithm is in Sam Kamin "An implementation-oriented 
/// semantics of Wadler's pretty printing combinators".
/// 
/// This implementation builds the document initially with a Doc
/// datatype and renders the datatype with Kamin's algorithm.
/// Kamin's original implementation foregoes the Doc datatype and 
/// builds the renderer directly.
/// We also use a StringBuilder for efficiency and have CPS 
/// transformed the render function.

namespace Text/PrettyPrint/Internal/Render {

    use Text/PrettyPrint.Doc;
    use Text/PrettyPrint.Doc.{DocEmpty, Text, DocLine, Nest, Cat, Alt, Tab};


    type alias StringS = StringBuilder ~> Unit

    /// ci (current-indent) * output-current-column * page-width * effective-page-width
    type alias PPState = (Int32, Int32, Int32, Int32)

    /// min-width w/newline * min width wo/newline * st -> (last column * text)
    type alias FDoc = (Int32, Int32, PPState -> (Int32, StringS))
        
    def stringS(s: String): StringS = sb -> StringBuilder.appendString!(sb, s)
    def spacesS(n: Int32): StringS = sb -> StringBuilder.appendString!(sb, String.repeat(" ", n))
    def lineSepS(): StringS = sb -> StringBuilder.appendString!(sb, String.lineSeparator())

    def seqS(f: StringS, g: StringS): StringS = sb -> {f(sb); g(sb)}

    def infinity(): Int32 = 99_999_999

    /// `render1` is in CPS

    pub def render1(x: Doc, k : FDoc -> FDoc): FDoc = match x { 
        case DocEmpty => k((0, infinity(), st -> {let (_, cc, _, _): PPState = st; (cc, stringS(""))}))
        case Text(s) => {
            let sz = String.length(s); 
            k((sz, infinity(), st -> {let (_, cc, _, _): PPState = st; (cc + sz, stringS(s))}))
        }
        case DocLine => k((0, 0, st -> {let (ci, _, _, _): PPState = st; (ci, lineSepS() `seqS` spacesS(ci))}))
        case Nest(n, d) =>
            render1(d, v1 -> {
                let (mwo1, mw1, fn1) = v1;
                k((mwo1, mw1, st -> {
                    let (ci, cc, pw, epw): PPState = st;
                    fn1((ci + n, cc, pw, epw))
                }))
            })
        case Cat(d1, d2) =>
            render1(d1, v1 -> 
            render1(d2, v2 -> {
                let (mwo1, mw1, fn1) = v1;
                let (mwo2, mw2, fn2) = v2;
                k((Int32.min(mw1, mwo1 + mwo2), Int32.min(mw1, mwo1 + mw2), st -> {
                    let (ci, cc, pw, epw): PPState = st;
                    let epw1 = Int32.max(pw - mw2, epw - mwo2);
                    let (cc1, s1) = fn1((ci, cc, pw, epw1));
                    let (cc2, s2) = fn2((ci, cc1, pw, epw));
                    (cc2, s1 `seqS` s2)
                }))
            }))
        case Alt(d1, d2) =>
            render1(d1, v1 -> 
            render1(d2, v2 -> {
                let (mwo1, mw1, fn1) = v1;
                let (mwo2, mw2, fn2) = v2;
                k((Int32.min(mwo1, mwo2), Int32.min(mw1, mw2), st -> {
                    let (_, cc, pw, epw): PPState = st;
                    let dfits = cc + mwo1 <= epw or cc + mw1 <= pw;
                    if (dfits) 
                        fn1(st)
                    else 
                        fn2(st)
                }))
            }))
        case Tab(d) => 
            render1(d, v1 -> {
                let (mwo1, mw1, fn1) = v1;
                k((mwo1, mw1, st -> {
                    let (_, cc, pw, epw): PPState = st;
                    fn1((cc, cc, pw, epw))
                }))
            })
    }

}
