use Text/PrettyPrint.{prettyPrint, text, line, tab};
use Text/PrettyPrint.{<>, <<>>, <!>};

def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running...");
    test01();
    test02();
    test03();
    test04();
    test05();
    test06();
    0

def test01(): Unit & Impure = 
    let doc = text("hello");
    prettyPrint(80, doc) |> println

def test02(): Unit & Impure = 
    let doc = text("hello") <<>> text("world");
    prettyPrint(80, doc) |> println  

def test03(): Unit & Impure = 
    let doc = text("hello") <<>> text("world");
    prettyPrint(8, doc) |> println  

def test04(): Unit & Impure = 
    let doc1 = text("hello") <<>> text("world");
    let doc2 = text("hello") <> line() <> text("world");
    let doc = doc1 <!> doc2;
    prettyPrint(8, doc) |> println  

def test05(): Unit & Impure = 
    let doc1 = text("hello") <<>> text("world");
    let doc2 = text("hello") <> line() <> text("world");
    let doc = doc1 <!> doc2;
    prettyPrint(16, doc) |> println    

def test06(): Unit & Impure = 
    let doc = text("hello") <<>> tab(text("world") <> line() <> text("!"));
    prettyPrint(80, doc) |> println 