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
    use Text.PrettyPrint.{encloseSep, text, comma}

    use Text.PrettyLevel.{PrettyLevel}

    pub enum PrettyLevel(Int32) with Eq, Order, ToString

    pub def prettyNormal(): PrettyLevel = PrettyLevel(0)

    pub trait Pretty[t] {
        pub def pPrint(x: t): Doc = Pretty.pPrintPrec(prettyNormal(), 1, x)

        pub def pPrintPrec(_l: PrettyLevel, _i: Int32, x: t): Doc = Pretty.pPrint(x)

        pub def pPrintList(l: PrettyLevel, xs: List[t]): Doc = 
            encloseSep(text("List#{"), text("}"), text(", "), List.map(Pretty.pPrintPrec(l, 0), xs))
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

}
