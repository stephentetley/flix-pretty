/*
 * Copyright 2019 Stephen Tetley
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


mod TestPrettyPrint {


    use Text.PrettyPrint.{<&&>, besideSoftLine, besideSoftSpace, parens, text, 
        empty, hsep, tupled, optionalTupled, removeEmpties, comma, punctuate}

    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def empty01(): Bool = 
        ToString.toString(empty()) == ""

    /////////////////////////////////////////////////////////////////////////////
    // text                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def text01(): Bool =
        ToString.toString(text("")) == ""

    @test
    def text02(): Bool =
        ToString.toString(text("A")) == "A"

    @test
    def text03(): Bool =
        ToString.toString(text("AB")) == "AB"

    @test
    def text04(): Bool =
        ToString.toString(text("A B")) == "A B"

    /////////////////////////////////////////////////////////////////////////////
    // Punctuate                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    pub def punctuate01(): Bool =
        let ds = punctuate(comma(), Nil);
        List.map(ToString.toString, ds) == Nil

    @test
    pub def punctuate02(): Bool =
        let ds = punctuate(comma(), text("abc") :: Nil);
        List.map(ToString.toString, ds) == "abc" :: Nil

    @test
    pub def punctuate03(): Bool =
        use Text.PrettyPrint.{punctuate, comma, text};
        let ds = punctuate(comma(), text("abc") :: text("def") :: Nil);
        List.map(ToString.toString, ds) == "abc," :: "def" :: Nil

    @test
    pub def punctuate04(): Bool =
        let ds = punctuate(comma(), text("abc") :: text("def") :: text("ghi") :: Nil);
        List.map(ToString.toString, ds) == "abc," :: "def," :: "ghi" :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // besideSoftLine                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    pub def besideSoftLine01(): Bool =
        let doc1 = besideSoftLine(besideSoftLine(text("abc"), text(",")), text("def"));
        ToString.toString(doc1) == "abc , def"

    /////////////////////////////////////////////////////////////////////////////
    // besideSoftSpace                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    pub def besideSoftSpace01(): Bool =
        let doc1 = parens(besideSoftSpace(text("hello"), text("world")));
        ToString.toString(doc1) == "(hello world)"

    @test
    pub def besideSoftSpace02(): Bool =
        let doc1 = parens(besideSoftSpace(empty(), empty()));
        ToString.toString(doc1) == "()"

    @test
    pub def besideSoftSpace03(): Bool =
        let doc1 = parens(besideSoftSpace(text("hello"), empty()));
        ToString.toString(doc1) == "(hello)"

    @test
    pub def besideSoftSpace04(): Bool =
        let doc1 = parens(besideSoftSpace(empty(), text("world")));
        ToString.toString(doc1) == "(world)"

    /////////////////////////////////////////////////////////////////////////////
    // symAmpersandAmpersand                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    pub def symAmpersandAmpersand01(): Bool =
        let doc1 = text("abc") <&&> text(",") <&&> text("def");
        String.lines(ToString.toString(doc1)) == "abc" :: "," :: "def" :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // removeEmpties                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    pub def removeEmpties01(): Bool =
        let doc1 = hsep(removeEmpties(List#{empty(), text("A"), empty()}));
        ToString.toString(doc1) == "A"

    @test
    pub def removeEmpties02(): Bool =
        let doc1 = hsep(removeEmpties(List#{empty(), text("A"), empty(), text("B")}));
        ToString.toString(doc1) == "A B"

    /////////////////////////////////////////////////////////////////////////////
    // tupled                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    pub def tupled01(): Bool =
        let doc1 = tupled(Nil);
        ToString.toString(doc1) == "()"

    @test
    pub def tupled02(): Bool =
        let doc1 = tupled(List#{empty()});
        ToString.toString(doc1) == "()"

    @test
    pub def tupled03(): Bool =
        let doc1 = tupled(List#{text("A"), text("B")});
        ToString.toString(doc1) == "(A, B)"

    @test
    pub def tupled04(): Bool =
        let doc1 = tupled(List#{text("A"), empty()});
        ToString.toString(doc1) == "(A,)"

    /////////////////////////////////////////////////////////////////////////////
    // optionalTupled                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    pub def optionalTupled01(): Bool =
        let doc1 = optionalTupled(Nil);
        ToString.toString(doc1) == ""

    @test
    pub def optionalTupled02(): Bool =
        let doc1 = optionalTupled(List#{empty()});
        ToString.toString(doc1) == ""

    @test
    pub def optionalTupled03(): Bool =
        let doc1 = optionalTupled(List#{text("A"), empty()});
        ToString.toString(doc1) == "(A)"

    @test
    pub def optionalTupled04(): Bool =
        let doc1 = optionalTupled(List#{text("A"), empty(), text("B")});
        ToString.toString(doc1) == "(A, B)"

}
