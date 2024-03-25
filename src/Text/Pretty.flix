/*
 * Copyright 2023 Stephen Tetley
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

/// # Acknowledgment
///
/// The `Pretty` trait is a Flix implementation of Haskell's 
/// `Text.PrettyPrint.HughesPJClass`.
///

mod Text {

    use Text.PrettyPrint.Doc
    use Text.PrettyPrint.{<<>>, encloseSep, text, comma, lparen, rparen, conditionalParens}

    use Text.PrettyLevel.{PrettyLevel}

    pub enum PrettyLevel(Int32) with Eq, Order, ToString

    pub def prettyNormal(): PrettyLevel = PrettyLevel(0)

    /// Note - this follows the Haskell grammar where function application is at precednce level 10.
    /// This might not be ideal for Flix and therefore may be subject to change.
    pub def appPrec(): Int32 = 10

    pub trait Pretty[t] {
        pub def pPrint(x: t): Doc = Pretty.pPrintPrec(prettyNormal(), 1, x)

        pub def pPrintPrec(_l: PrettyLevel, _i: Int32, x: t): Doc = Pretty.pPrint(x)

        pub def pPrintList(l: PrettyLevel, xs: List[t]): Doc = 
            encloseSep(text("List#{"), text("}"), text(", "), List.map(Pretty.pPrintPrec(l, 0), xs))
    }

    def pPrint0(l: PrettyLevel, x: t): Doc with Pretty[t] = Pretty.pPrintPrec(l, 0, x)

    instance Pretty[Unit] {
        pub override def pPrint(_: Unit): Doc = text("()")
    }

    instance Pretty[Int8] {
        pub override def pPrint(x: Int8): Doc = Text.PrettyPrint.int8(x)
    }

    instance Pretty[Int16] {
        pub override def pPrint(x: Int16): Doc = Text.PrettyPrint.int16(x)
    }

    instance Pretty[Int32] {
        pub override def pPrint(x: Int32): Doc = Text.PrettyPrint.int32(x)
    }

    instance Pretty[Int64] {
        pub override def pPrint(x: Int64): Doc = Text.PrettyPrint.int64(x)
    }

    instance Pretty[BigInt] {
        pub override def pPrint(x: BigInt): Doc = Text.PrettyPrint.bigInt(x)
    }

    instance Pretty[Float32] {
        pub override def pPrint(x: Float32): Doc = Text.PrettyPrint.float32(x)
    }

    instance Pretty[Float64] {
        pub override def pPrint(x: Float64): Doc = Text.PrettyPrint.float64(x)
    }

    instance Pretty[BigDecimal] {
        pub override def pPrint(x: BigDecimal): Doc = Text.PrettyPrint.bigDecimal(x)
    }

    instance Pretty[Char] {
        pub override def pPrint(x: Char): Doc = Text.PrettyPrint.char(x)
    }

    instance Pretty[String] {
        pub override def pPrint(x: String): Doc = text(x)
    }


    instance Pretty[(a, b)] with Pretty[a], Pretty[b] {
        pub override def pPrintPrec(l: PrettyLevel, _: Int32, p: (a, b)): Doc = match p {
            case (x,y) => encloseSep(lparen(), rparen(), text(", "), List#{pPrint0(l, x), pPrint0(l, y)})
        }
    }

    instance Pretty[(a, b, c)] with Pretty[a], Pretty[b], Pretty[c] {
        pub override def pPrintPrec(l: PrettyLevel, _: Int32, p: (a, b, c)): Doc = match p {
            case (x,y,z) => encloseSep(lparen(), rparen(), text(", "), List#{pPrint0(l, x), pPrint0(l, y), pPrint0(l, z)})
        }
    }

    instance Pretty[(a, b, c, d)] with Pretty[a], Pretty[b], Pretty[c], Pretty[d] {
        pub override def pPrintPrec(l: PrettyLevel, _: Int32, p: (a, b, c, d)): Doc = match p {
            case (w,x,y,z) => encloseSep(lparen(), rparen(), text(", "), List#{pPrint0(l, w), pPrint0(l, x), pPrint0(l, y), pPrint0(l, z)})
        }
    }

    instance Pretty[(a, b, c, d, e)] with Pretty[a], Pretty[b], Pretty[c], Pretty[d], Pretty[e] {
        pub override def pPrintPrec(l: PrettyLevel, _: Int32, p: (a, b, c, d, e)): Doc = match p {
            case (v,w,x,y,z) => encloseSep(lparen(), rparen(), text(", "), List#{pPrint0(l, v), pPrint0(l, w), pPrint0(l, x), pPrint0(l, y), pPrint0(l, z)})
        }
    }

    instance Pretty[Option[a]] with Pretty[a] {
        pub override def pPrintPrec(l: PrettyLevel, p: Int32, x: Option[a]): Doc = match x {
            case None     => text("None")
            case Some(x1) => conditionalParens(p > appPrec(), text("Some") <<>> Pretty.pPrintPrec(l, appPrec() + 1, x1))
        }
    }

}
