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


namespace Text/PrettyPrint/Kamin2 {


    /// Kamin has `alt` rather than `group`

    pub enum Doc {
        case DocEmpty
        case Text(String)
        case Line
        case Nest(Int32, Doc)
        case Cat(Doc, Doc)
        case Alt(Doc, Doc)
        case Tab(Doc)
    }

    pub def empty(): Doc = DocEmpty
    pub def text(x: String): Doc = Text(x)
    pub def space(): Doc = Text(" ")
    pub def line(): Doc = Line
    pub def nest(n: Int32, d: Doc): Doc = Nest(n, d)
    pub def tab(d: Doc): Doc = Tab(d)

    pub def <>(x: Doc, y: Doc): Doc = Cat(x, y)

    pub def <<>>(x: Doc, y: Doc): Doc = x <> space() <> y

    pub def <!>(x: Doc, y: Doc): Doc = Alt(x, y)


    type alias StringB = StringBuilder ~> Unit

    /// ci (current-indent) * output-current-column * page-width * effective-page-width
    type alias PPState = (Int32, Int32, Int32, Int32)

    /// min-width w/newline * min width wo/newline * st -> (last column * text)
    type alias Doc1 = (Int32, Int32, PPState -> (Int32, StringB))
        
    def makeStringB(s: String): StringB = sb -> StringBuilder.appendString!(sb, s)
    def spacesStringB(n: Int32): StringB = sb -> StringBuilder.appendString!(sb, String.repeat(" ", n))
    def lineSepStringB(): StringB = sb -> StringBuilder.appendString!(sb, String.lineSeparator())

    def seqStringB(f: StringB, g: StringB): StringB = sb -> {f(sb); g(sb)}

    def infinity(): Int32 = 99_999_999

    /// TODO CPS

    pub def render1(x: Doc): Doc1 = match x { 
        case DocEmpty => (0, infinity(), st -> {let (_, cc, _, _): PPState = st; (cc, makeStringB(""))})
        case Text(s) => {
            let sz = String.length(s); 
            (sz, infinity(), st -> {let (_, cc, _, _): PPState = st; (cc + sz, makeStringB(s))})
        }
        case Line => (0, 0, st -> {let (ci, _, _, _): PPState = st; (ci, lineSepStringB() `seqStringB` spacesStringB(ci))})
        case Nest(n, d) => {
            let (mwo1, mw1, fn1) = render1(d);
            (mwo1, mw1, st -> {
                let (ci, cc, pw, epw): PPState = st;
                fn1((ci + n, cc, pw, epw))
            })
        }
        case Cat(d1, d2) => {
            let (mwo1, mw1, fn1) = render1(d1);
            let (mwo2, mw2, fn2) = render1(d2);
            (Int32.min(mw1, mwo1 + mwo2), Int32.min(mw1, mwo1 + mw2), st -> {
                let (ci, cc, pw, epw): PPState = st;
                let epw1 = Int32.max(pw - mw2, epw - mwo2);
                let (cc1, s1) = fn1((ci, cc, pw, epw1));
                let (cc2, s2) = fn2((ci, cc1, pw, epw));
                (cc2, s1 `seqStringB` s2)
            })
        }
        case Alt(d1, d2) => {
            let (mwo1, mw1, fn1) = render1(d1);
            let (mwo2, mw2, fn2) = render1(d2);
            (Int32.min(mwo1, mwo2), Int32.min(mw1, mw2), st -> {
                let (_, cc, pw, epw): PPState = st;
                let dfits = cc + mwo1 <= epw or cc + mw1 <= pw;
                if (dfits) 
                    fn1(st)
                else 
                    fn2(st)
            })
        }
        case Tab(d) => {
            let (mwo1, mw1, fn1) = render1(d);
            (mwo1, mw1, st -> {
                let (_, cc, pw, epw): PPState = st;
                fn1((cc, cc, pw, epw)) 
            })
        }
    }

    pub def pretty(width: Int32, x: Doc): String = prettyHelper(width, x) as & Pure

    def prettyHelper(width: Int32, x: Doc): String & Impure = 
        let sb = StringBuilder.new();
        prettySB!(width, x, sb);
        StringBuilder.toString(sb)

    pub def prettySB!(width: Int32, x: Doc, sb: StringBuilder): Unit & Impure  = 
        let (_, _, fn): Doc1 = render1(x);
        let (_, fn2) = fn((0, 0, width, width));
        fn2(sb)
        

}

