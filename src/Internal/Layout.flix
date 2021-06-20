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

namespace Text/PPrint/Internal/Layout {

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

