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

    pub def vcat(xs: List[Doc]): Doc = foldDoc(x1 -> y1 -> x1 <&&> y1, xs)
    
    pub def hsep(xs: List[Doc]): Doc = foldDoc(x1 -> y1 -> x1 <<>> y1, xs)

    pub def sep(xs: List[Doc]): Doc = alt(hsep(xs), vcat(xs))

    pub def text(s: String): Doc = Text(s)

    pub def space(): Doc = Text(" ")


    pub def render(pageWidth: Int32, d: Doc): Option[String] = 
        toADoc(pageWidth, d)
            |> Text/PPrint/Internal/ADoc.render

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

}