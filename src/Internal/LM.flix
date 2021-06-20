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


namespace Text/PPrint/Internal/LM {

    use Text/PPrint/Internal/Measure.{Measure};
    use Text/PPrint/Internal/Layout.{Layout};

    type alias LM = (Measure, Layout)

    pub def precedes(x: LM, y: LM): Bool = 
        Text/PPrint/Internal/Measure.precedes(fst(x), fst(y))
        
    pub def hcat(x: LM, y: LM): LM = 
        match (x, y) { 
            case ((m1, l1), (m2, l2)) => 
                (Text/PPrint/Internal/Measure.hcat(m1, m2), Text/PPrint/Internal/Layout.hcat(l1, l2))
        }

    pub def flush(x: LM): LM = 
        let (m1, l1) = x;
        (Text/PPrint/Internal/Measure.flush(m1), Text/PPrint/Internal/Layout.flush(l1))

    pub def text(s: String): LM = 
        (Text/PPrint/Internal/Measure.text(s), Text/PPrint/Internal/Layout.text(s))

    pub def vcat(x: LM, y: LM): LM = 
        flush(x) `hcat` y

    pub def render(x: LM): String = 
        Text/PPrint/Internal/Layout.render(snd(x))

    pub def valid(pageWidth: Int32, x: LM): Bool = 
        Text/PPrint/Internal/Measure.valid(pageWidth, fst(x))

}

