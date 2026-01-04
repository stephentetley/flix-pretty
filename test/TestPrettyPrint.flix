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
        empty, hsep, tupled, optionalTupled, removeEmpties, comma, punctuate,
        writePretty}

    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def empty01(): Unit \ Assert =
        Assert.assertEq(expected = "", ToString.toString(empty()))

    /////////////////////////////////////////////////////////////////////////////
    // text                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def text01(): Unit \ Assert =
        Assert.assertEq(expected = "", ToString.toString(text("")))

    @Test
    def text02(): Unit \ Assert =
        Assert.assertEq(expected = "A", ToString.toString(text("A")))

    @Test
    def text03(): Unit \ Assert =
        Assert.assertEq(expected = "AB", ToString.toString(text("AB")))

    @Test
    def text04(): Unit \ Assert =
        Assert.assertEq(expected = "A B", ToString.toString(text("A B")))

    /////////////////////////////////////////////////////////////////////////////
    // Punctuate                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    pub def punctuate01(): Unit \ Assert =
        let ds = punctuate(comma(), Nil);
        Assert.assertEq(expected = Nil, List.map(ToString.toString, ds))

    @Test
    pub def punctuate02(): Unit \ Assert =
        let ds = punctuate(comma(), text("abc") :: Nil);
        Assert.assertEq(expected = "abc" :: Nil, List.map(ToString.toString, ds))

    @Test
    pub def punctuate03(): Unit \ Assert =
        use Text.PrettyPrint.{punctuate, comma, text};
        let ds = punctuate(comma(), text("abc") :: text("def") :: Nil);
        Assert.assertEq(expected = "abc," :: "def" :: Nil, List.map(ToString.toString, ds))

    @Test
    pub def punctuate04(): Unit \ Assert =
        let ds = punctuate(comma(), text("abc") :: text("def") :: text("ghi") :: Nil);
        Assert.assertEq(expected = "abc," :: "def," :: "ghi" :: Nil, List.map(ToString.toString, ds))

    /////////////////////////////////////////////////////////////////////////////
    // besideSoftLine                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    pub def besideSoftLine01(): Unit \ Assert =
        let doc1 = besideSoftLine(besideSoftLine(text("abc"), text(",")), text("def"));
        Assert.assertEq(expected = "abc , def", ToString.toString(doc1))

    /////////////////////////////////////////////////////////////////////////////
    // besideSoftSpace                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    pub def besideSoftSpace01(): Unit \ Assert =
        let doc1 = parens(besideSoftSpace(text("hello"), text("world")));
        Assert.assertEq(expected = "(hello world)", ToString.toString(doc1))

    @Test
    pub def besideSoftSpace02(): Unit \ Assert =
        let doc1 = parens(besideSoftSpace(empty(), empty()));
        Assert.assertEq(expected = "()", ToString.toString(doc1))

    @Test
    pub def besideSoftSpace03(): Unit \ Assert =
        let doc1 = parens(besideSoftSpace(text("hello"), empty()));
        Assert.assertEq(expected = "(hello)", ToString.toString(doc1))

    @Test
    pub def besideSoftSpace04(): Unit \ Assert =
        let doc1 = parens(besideSoftSpace(empty(), text("world")));
        Assert.assertEq(expected = "(world)", ToString.toString(doc1))

    /////////////////////////////////////////////////////////////////////////////
    // symAmpersandAmpersand                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    pub def symAmpersandAmpersand01(): Unit \ Assert =
        let doc1 = text("abc") <&&> text(",") <&&> text("def");
        Assert.assertEq(expected = "abc" :: "," :: "def" :: Nil, String.lines(ToString.toString(doc1)))

    /////////////////////////////////////////////////////////////////////////////
    // removeEmpties                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    pub def removeEmpties01(): Unit \ Assert =
        let doc1 = hsep(removeEmpties(List#{empty(), text("A"), empty()}));
        Assert.assertEq(expected = "A", ToString.toString(doc1))

    @Test
    pub def removeEmpties02(): Unit \ Assert =
        let doc1 = hsep(removeEmpties(List#{empty(), text("A"), empty(), text("B")}));
        Assert.assertEq(expected = "A B", ToString.toString(doc1))

    /////////////////////////////////////////////////////////////////////////////
    // tupled                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    pub def tupled01(): Unit \ Assert =
        let doc1 = tupled(Nil);
        Assert.assertEq(expected = "()", ToString.toString(doc1))

    @Test
    pub def tupled02(): Unit \ Assert =
        let doc1 = tupled(List#{empty()});
        Assert.assertEq(expected = "()", ToString.toString(doc1))

    @Test
    pub def tupled03(): Unit \ Assert =
        let doc1 = tupled(List#{text("A"), text("B")});
        Assert.assertEq(expected = "(A, B)", ToString.toString(doc1))

    @Test
    pub def tupled04(): Unit \ Assert =
        let doc1 = tupled(List#{text("A"), empty()});
        Assert.assertEq(expected = "(A,)", ToString.toString(doc1))

    /////////////////////////////////////////////////////////////////////////////
    // optionalTupled                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    pub def optionalTupled01(): Unit \ Assert =
        let doc1 = optionalTupled(Nil);
        Assert.assertEq(expected = "", ToString.toString(doc1))

    @Test
    pub def optionalTupled02(): Unit \ Assert =
        let doc1 = optionalTupled(List#{empty()});
        Assert.assertEq(expected = "", ToString.toString(doc1))

    @Test
    pub def optionalTupled03(): Unit \ Assert =
        let doc1 = optionalTupled(List#{text("A"), empty()});
        Assert.assertEq(expected = "(A)", ToString.toString(doc1))

    @Test
    pub def optionalTupled04(): Unit \ Assert =
        let doc1 = optionalTupled(List#{text("A"), empty(), text("B")});
        Assert.assertEq(expected = "(A, B)", ToString.toString(doc1))


    /////////////////////////////////////////////////////////////////////////////
    // writePretty                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    pub def writePretty01(): Unit \ { IO, Assert } =
        let testFile = "test/testfile.txt";
        let res = run {
            FileWrite.write(str="XXXXXXXXXXXXXX", testFile);
            let doc1 = text("hello");
            discard writePretty(testFile, 120, doc1);
            FileRead.read(testFile)
        } with FileWrite.runWithIO
            with FileRead.runWithIO;
        Assert.assertEq(expected = Ok(Ok("hello")), res)
}
