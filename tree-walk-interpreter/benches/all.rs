use tree_walk_interpreter::interpreter::Interpreter;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use parser::lexer::Lexer;
use parser::parser::Parser;
use parser::resolver::Resolver;
use parser::types::Pass;

const PROGRAM: &str = "print \"HOLA\" and \"CHAU\";
print nil and \"HOLA\";
print \"HOLA\" and false;
print false and \"HOLA\";
print nil and nil;

fun thrice(fn) {
  for (var i = 1; i <= 3; i = i + 1) {
    fn(i);
  }
}

thrice(fun (a) {
  print a;
});

var array1 = [0; 3];
var array2 = [0, \"hola\", true];

print array1;
print array2;
print array1[0];
print array2[2];

array1[0] = 2;
print array1[0];

print \"BEFORE\";
{
  print \"ON THE BLOCK\";
  {
    print \"ASDASD\";
  }
  var a = nil;
  print a;
  print a=3;
  print a+2;
}
print \"AFTER\";

var a = \"global a\";
var b = \"global b\";
var c = \"global c\";
{
  var a = \"outer a\";
  var b = \"outer b\";
  {
    var a = \"inner a\";
    print a;
    print b;
    print c;
  }
  print a;
  print b;
  print c;
}
print a;
print b;
print c;

var i = 0;
print \"CACA\";
while (i < 5) {
  print \"HOLA\";
  break;
  i = i + 1;
}

print \"CHAU\";

for (var i = 0; i < 5; i = i + 1) {
  print \"HOLA0\";
  break;
}

print \"CHAU0\";

for (var i = 0; i < 5; i = i + 1) {
  print \"HOLA1\";
  if (i == 1) break;
  print \"HOLA1.1\";
}

print \"CHAU1\";

for (var i = 0; i < 5; i = i + 1) {
  print \"HOLA2\";
  for (var i = 0; i < 5; i = i + 1) {
    print \"INNER HOLA2\";
    break;
  }
  print \"AFTER INNER HOLA2\";
  break;
  print \"DO NOT SEE2\";
}

print \"CHAU2\";

for (var i = 0; i < 5; i = i + 1) {
  print \"HOLA3\";
  for (var i = 0; i < 5; i = i + 1) {
    print \"INNER HOLA3\";
    if (i == 1) break;
    print \"SEE ONCE3\";
  }
  print \"AFTER INNER HOLA3\";
  break;
  print \"DO NOT SEE3\";
}

print \"CHAU3\";

class DevonshireCream {
  serveOn() {
    return \"Scones\";
  }
}

print DevonshireCream;
print DevonshireCream();
fun makeCounter() {
  var i = 0;
  fun count() {
    i = i + 1;
    print i;
  }

  return count;
}

var counter = makeCounter();
counter();
counter();
class Test {
    init(a) {
        print a;
    }
}

Test(\"hola\");
print a;
print a=2;
print a+4;
print \"HOLA\";
var a1;
a1 = 1;
for (var i = 0; i < 5; i = i + 1)
  print \"HOLA MUNDO\";
fun test () {
  print \"hola\";
}

fun test1(num, num1) {
  print num + num1;
}

test1(1, 2);

test();

if (true)
  print \"HOLA\";
else
  print \"CHAU\";

if (false)
  print \"HOLA\";
else
  print \"CHAU\";

if (true)
  if (false)
    print \"HOLA\";
  else
    print \"CHAU\";

mod module {
    fun test() {
        print \"hola\";
    }

    class TestClass {
        testmethod() {
            print \"hola method\";
        }
    }

    trait TestTrait {
        echo(a);
    }

    var variable = 1;
}

module::test();

trait MyTrait {
    mytraitmethod();
}

class Impl < module::TestClass {
}

trait MyTrait for module::TestClass {
    mytraitmethod() {
        print \"my trait method\";
    }
}

var m = module::TestClass();
m.testmethod();
m.mytraitmethod();

trait module::TestTrait for Impl {
  echo(a) {
      print a;
  }
}

var im = Impl();
im.echo(\"pepe\");
im.testmethod();

print module::variable;

fun testfunction() {
}

trait Trait1 {
}
trait Trait2 {
}
class Class1 {
}
class Class2 {
}
class Class3 < Class2 {
}

trait Trait1 for Class1 {
}
trait Trait2 for Class2 {
}

print nil istype Nil;
print nil istype Integer;
print true istype Boolean;
print true istype Integer;
print 1 istype Integer;
print 1 istype Float;
print 1.0 istype Float;
print 1.0 istype Integer;
print module istype Module;
print module istype Integer;
print \"\" istype String;
print \"\" istype Integer;
print [] istype Array;
print [] istype String;
print testfunction istype Function;
print testfunction istype Module;
print Trait1 istype Trait;
print Trait1 istype Class;
print Class1 istype Class;
print Class1 istype Trait;
print Class1() istype Class1;
print Class1() istype Class2;
print Class1() istype Trait1;
print Class2() istype Trait1;
print Class3() istype Class2;
print Class3() istype Class1;
print Class3() istype Trait2;

class Class11 {
}
trait Trait11 {
}
trait Trait11 for Class11 {
}

fun withmatch(v) {
  match v {
    1 => {
      print \"uno\";
    },
    2 => {
      print \"dos\";
    },
    nil => {
      print \"nulo\";
    },
    true => {
      print \"verdadero\";
    },
    * => {
      print v;
    },
  }
}

fun withtypematch(t) {
  match t {
    Integer => {
      print \"integer\";
    },
    Boolean => {
      print \"boolean\";
    },
    String => {
      print \"String\";
    },
    Trait1 => {
      print \"Trait1\";
    },
    * => {
      print v;
    },
  }
}

withmatch(1);
withmatch(2);
withmatch(nil);
withmatch(true);
withmatch(\"hola\");

withtypematch(1);
withtypematch(true);
withtypematch(\"\");
withtypematch(Class1());
print 1 + 1;
print 1.2 + 1.2;
print \"HOLA\" or \"CHAU\";
print nil or \"HOLA\";
print \"HOLA\" or false;
print false or \"HOLA\";
print nil or nil;
class DevonshireCream1 {
  serveOn() {
    return \"Scones\";
  }
  echo(a) {
    return a;
  }
}

var cc = DevonshireCream1();
cc.test = 1;
print cc.test;
cc.testFunction = fun () {
    print \"Scones!\";
};
cc.testFunction();
print cc.serveOn();
print cc.echo(\"hola\");
var a4 = 1;

var a5 = \"global\";
{
  fun showA() {
    print a5;
  }

  showA();
  var a5 = \"block\";
  showA();
  print a5;
}

fun testf(a, b) {
  return a + b;
}

print testf(1,2);
class Test2 {
    setter prop(a) {
        this.secret = a;
    }

    getter prop() {
        return this.secret;
    }
}

var t = Test2();
t.prop = \"HOLA\";
print t.prop;
print \"HOLA\";
class Math {
  class square(n) {
    return n * n;
  }
}

print Math.square(3);
class Doughnut1 {
  cook() {
    print \"Fry until golden brown.\";
  }
}

class BostonCream < Doughnut1 {
}

BostonCream().cook();
class Doughnut2 {
  cook() {
    print \"Fry until golden brown.\";
  }
}

class BostonCream2 < Doughnut2 {
  cook() {
    this.super.cook();
    print \"Pipe full of custard and coat with chocolate.\";
  }
}

BostonCream2().cook();
class Cake {
  taste() {
    var adjective = \"delicious\";
    print \"The \" + this.flavor + \" cake is \" + adjective + \"!\";
  }

  getCallback() {
    fun localFunction() {
      print this.flavor;
    }

    return localFunction;
  }
}

var cake = Cake();
cake.flavor = \"German chocolate\";
cake.taste();
var cb = cake.getCallback();
cb();
trait Test11 {
    test(a);
    class staticmethod();
    getter prop();
    setter prop(a);
}

class TestImpl {
    pepe() {
    }
}

trait Test11 for TestImpl {
    test(a) {
        print a;
    }
    class staticmethod() {
        print \"static\";
    }
    getter prop() {
        print this.a;
    }
    setter prop(a) {
        this.a = a;
    }
}

var obj = TestImpl();
obj.test(\"method\");
TestImpl.staticmethod();
obj.prop = \"apa\";
print obj.prop;
var counter1 = 0;

while (counter1 < 5) {
    print \"HOLA\";
    counter1 = counter1 + 1;
}
print \"CHAU\";";

fn all(extra: usize) {
    let program = format!("{} + {};", extra, extra) + PROGRAM;
    let mut lexer = Lexer::new(program.as_str(), "stdin");
    let ss = lexer
        .parse()
        .and_then(|ts| {
            let parser = Parser::new(ts.into_iter().peekable());
            parser.parse()
        }).unwrap();
    let mut resolver = Resolver::new();
    let locals = resolver.run(&ss).unwrap();
    let paths = vec![];
    let mut interpreter = Interpreter::new(&paths, "");
    interpreter.locals = locals;
    interpreter.run(&ss).unwrap();
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("use all functionality", |b| {
        b.iter(|| all(black_box(4)))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
