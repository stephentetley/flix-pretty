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


namespace Text/PPrint/Internal/ADoc {
    
    use Text/PPrint/Internal/LM.{LM};

    type alias ADoc = List[LM]


    pub def pareto(s: List[LM]): List[LM] = 
        paretoHelper(s, Nil, ks -> ks)

    def paretoHelper(s: List[LM], acc: List[LM], k: List[LM] -> List[LM]): List[LM] =
        use Text/PPrint/Internal/LM.{precedes};
        match s { 
            case Nil => k(acc)
            case x :: rs => 
                if (List.exists(x1 -> x1 `precedes` x, acc))
                    paretoHelper(rs, acc, k)
                else {
                    let acc1 = x :: List.filter(x1 -> not (x `precedes` x1), acc);
                    paretoHelper(rs, acc1, k)
                }
        }

    def cartesian(xs: List[LM], ys: List[LM]): List[LM] = 
        List.map(x1 -> List.map(y1 -> Text/PPrint/Internal/LM.hcat(x1, y1), ys), xs)
            |> List.flatten

    pub def empty(pageWidth: Int32): ADoc = text(pageWidth, "")

    pub def hcat(pageWidth: Int32, x: ADoc, y: ADoc): ADoc = 
        cartesian(x, y) 
            |> List.filter(Text/PPrint/Internal/LM.valid(pageWidth))
            |> pareto
        

    pub def flush(xs: ADoc): ADoc = 
        List.map(Text/PPrint/Internal/LM.flush, xs) 
            |> pareto

    pub def text(pageWidth: Int32, s: String): ADoc = 
        let d1 = Text/PPrint/Internal/LM.text(s);
        List.filter(fst >> Text/PPrint/Internal/Measure.valid(pageWidth), d1 :: Nil)


    pub def vcat(pageWidth: Int32, x: ADoc, y: ADoc): ADoc = 
        hcat(pageWidth, flush(x), y)

    pub def fail(): ADoc = Nil

    pub def alt(x: ADoc, y: ADoc): ADoc = 
        pareto(x ::: y)


    pub def render(x: ADoc): Option[String] = 
        /// Smallest height
        let cmp = (a, b) -> Text/PPrint/Internal/Measure.compare(fst(a), fst(b));
        List.minimumBy(cmp, x)
            |> Option.map(Text/PPrint/Internal/LM.render)
            
}

