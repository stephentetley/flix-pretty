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


namespace Text/BPretty/Measure {

    type alias Measure = 
        { height: Int32
        , lastWidth: Int32
        , maxWidth: Int32
        }
    
    pub def hcat(x: Measure, y: Measure): Measure = 
        { height = x.height + y.height
        , lastWidth = x.lastWidth + y.lastWidth
        , maxWidth = Int32.max(x.maxWidth, x.lastWidth + y.maxWidth)
        }

    pub def text(s: String): Measure = 
        { height = 0
        , lastWidth = String.length(s)
        , maxWidth = String.length(s)
        }

    pub def flush(x: Measure): Measure = 
        { height = x.height + 1
        , lastWidth = 0
        , maxWidth = x.maxWidth
        }

    pub def vcat(x: Measure, y: Measure): Measure = 
        flush(x) `hcat` y


    /// Order of compare - height, lastWidth, maxWidth
    pub def compare(x: Measure, y: Measure): Comparison = 
        match Order.compare(x.height, y.height) { 
            case EqualTo => match Order.compare(x.lastWidth, y.lastWidth) {
                case EqualTo => Order.compare(x.maxWidth, y.maxWidth)
                case z2 => z2
            }
            case z1 => z1
        }


    pub def precedes(x: Measure, y: Measure): Bool = 
        x.height <= y.height and x.maxWidth <= y.maxWidth and x.lastWidth <= y.lastWidth



    pub def valid(pageWidth: Int32, x: Measure): Bool = 
        x.maxWidth <= pageWidth
}

namespace Text/BPretty/Layout {

    use ViewLeft.{NoneLeft, SomeLeft};
    use ViewRight.{NoneRight, SomeRight};

    type alias Layout = Chain[String]

    pub def render(x: Layout): String = renderHelper(x) as & Pure

    def renderHelper(x: Layout): String & Impure = 
        let sb = StringBuilder.new();
        render1(sb, x);
        StringBuilder.toString(sb)
        
    pub def render1(sb: StringBuilder, x: Layout): Unit & Impure = 
        Chain.foreach(StringBuilder.appendLine!(sb), x)

    pub def flush(x: Layout): Layout = 
        Chain.snoc(x, "")

    pub def text(s: String): Layout = Chain.singleton(s)

    pub def hcat(x: Layout, y: Layout): Layout =         
        match (Chain.viewRight(x), Chain.viewLeft(y)) { 
            case (SomeRight(xs1, x1), SomeLeft(y1, ys1)) => { 
                let indent = String.repeat(" ", String.length(x1));
                let rs = Chain.map(s1 -> indent + s1, ys1);
                Chain.append(xs1, Chain.cons(x1 + y1, rs))
            }
            case (NoneRight, _) => y
            case (_, NoneLeft) => x
        }

    pub def vcat(x: Layout, y: Layout): Layout = 
        flush(x) `hcat` y


}

namespace Text/BPretty/Doc1 {

    use Text/BPretty/Measure.{Measure};
    use Text/BPretty/Layout.{Layout};

    type alias Doc1 = (Measure, Layout)

    pub def precedes(x: Doc1, y: Doc1): Bool = 
        Text/BPretty/Measure.precedes(fst(x), fst(y))
        
    pub def hcat(x: Doc1, y: Doc1): Doc1 = 
        match (x, y) { 
            case ((m1, l1), (m2, l2)) => 
                (Text/BPretty/Measure.hcat(m1, m2), Text/BPretty/Layout.hcat(l1, l2))
        }

    pub def flush(x: Doc1): Doc1 = 
        let (m1, l1) = x;
        (Text/BPretty/Measure.flush(m1), Text/BPretty/Layout.flush(l1))

    pub def text(s: String): Doc1 = 
        (Text/BPretty/Measure.text(s), Text/BPretty/Layout.text(s))

    pub def vcat(x: Doc1, y: Doc1): Doc1 = 
        flush(x) `hcat` y

    pub def render(x: Doc1): String = 
        Text/BPretty/Layout.render(snd(x))

    pub def valid(pageWidth: Int32, x: Doc1): Bool = 
        Text/BPretty/Measure.valid(pageWidth, fst(x))

}

namespace Text/BPretty/Doc {
    
    use Text/BPretty/Doc1.{Doc1};

    type alias Doc = List[Doc1]


    pub def paretoWith(prec: (a, a) -> Bool, s: List[a]): List[a] = 
        paretoWithHelper(prec, s, Nil, ks -> ks)

    def paretoWithHelper(prec: (a, a) -> Bool, s: List[a], acc: List[a], k: List[a] -> List[a]): List[a] =
        match s { 
            case Nil => k(acc)
            case x :: rs => 
                if (List.exists(x1 -> x1 `prec` x, acc))
                    paretoWithHelper(prec, rs, acc, k)
                else {
                    let acc1 = x :: List.filter(x1 -> not (x `prec` x1), acc);
                    paretoWithHelper(prec, rs, acc1, k)
                }
        }

    def cartesian(xs: List[Doc1], ys: List[Doc1]): List[Doc1] = 
        List.map(x1 -> List.map(y1 -> Text/BPretty/Doc1.hcat(x1, y1), ys), xs)
            |> List.flatten


    pub def hcat(pageWidth: Int32, x: Doc, y: Doc): Doc = 
        cartesian(x, y) 
            |> List.filter(Text/BPretty/Doc1.valid(pageWidth))
            |> paretoWith(Text/BPretty/Doc1.precedes)
        

    pub def flush(xs: Doc): Doc = 
        List.map(Text/BPretty/Doc1.flush, xs) 
            |> paretoWith(Text/BPretty/Doc1.precedes)

    pub def text(pageWidth: Int32, s: String): Doc = 
        let d1 = Text/BPretty/Doc1.text(s);
        List.filter(fst >> Text/BPretty/Measure.valid(pageWidth), d1 :: Nil)


    pub def vcat(pageWidth: Int32, x: Doc, y: Doc): Doc = 
        hcat(pageWidth, flush(x), y)


    pub def render(x: Doc): String = 
        /// Smallest height
        let cmp = (a, b) -> Text/BPretty/Measure.compare(fst(a), fst(b));
        match List.minimumBy(cmp, x) { 
            case Some(x1) => Text/BPretty/Doc1.render(x1)
            case None => "Fail: no Doc produced."
        }

}