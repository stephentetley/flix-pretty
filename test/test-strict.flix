use Text/PrettyPrint/Strict.{render, text, group, nest, indent};
use Text/PrettyPrint/Strict.{<!>, <>, <&>};

def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running...");
    test01();
    test02();
    test03();
    test04();
    test05();
    test05();
    test06();
    0

def test01(): Unit & Impure = 
    let doc = text("hello") <!> text("world");
    render(80, doc) |> println

def test02(): Unit & Impure = 
    let doc = text("hello") <&> text("world");
    render(80, doc) |> println


def test03(): Unit & Impure = 
    let doc = group(text("hello") <&> text("world"));
    render(80, doc) |> println


def test04(): Unit & Impure = 
    let doc = group(nest(4, text("hello") <&> text("world")));
    render(80, doc) |> println

def test05(): Unit & Impure = 
    let doc = indent(4, text("hello") <&> text("world"));
    render(80, doc) |> println

def test06(): Unit & Impure = 
    let doc = indent(4, group(text("hello") <&> text("world")));
    render(80, doc) |> println  