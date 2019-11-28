function identity(x) { return x; }
var identity2 = function function_name(x) {
    return x;
};
// anonymous function WITHOUT DECLARATION
var anonyNo = function (f) {
    return function (x) {
        return function (y) {
            return f(x, y);
        };
    };
};
// anonymous function WTIH DECLARATION
var anonyYes = function (f) {
    return function (x) {
        return function (y) {
            return f(x, y);
        };
    };
};
// arrow function WITHOUT DECLARATION
var arrowNo = function (f) { return function (x) { return function (y) { return f(x, y); }; }; };
// arrow function WITH DECLARATION
var arrowYes = function (f) { return function (x) { return function (y) { return f(x, y); }; }; };
// function WITHOUT DECLARATION
function funcNo(f) { return function (x) { return function (y) { return f(x, y); }; }; }
// function WITH DECLARATION
function funcYes(f) { return function (x) { return function (y) { return f(x, y); }; }; }
// function WITH DECLARATION (instead of create multiple declarations, we use type alias to represen those declarations)
// funcYesAlias accepts a function of type BinFunc and returns a type of CurrFunc
function funcYesAlias(f) {
    return function (x) {
        return function (y) {
            return f(x, y);
        };
    };
}
// similar to approach below
function funcYesAlias2(f) {
    return function (x) {
        return function (y) {
            return f(x, y);
        };
    };
}
// aYesAlias accepts a type of BinFunc and returns a type of CurrFunc, both approach below are ok
var aYesAlias = function (f) { return function (x) { return function (y) { return f(x, y); }; }; };
// this is how we define normally in other languages, with specific number declaration, but what if there is another number type appears in the future, then we have got to change the code, troublesome!
function curry(f) {
    return function (x) {
        return function (y) {
            return f(x, y);
        };
    };
}
// here is something more interesting, this curry2 function accepts one function of type generic (say type A, where A = {boolean, number, ...}
function curry2(f) {
    return function (x) { return function (y) { return f(x, y); }; };
}
// now we use alias to represent them
function curry3(f) {
    return function (x) { return function (y) { return f(x, y); }; };
}
// of course, we can also use "any", but this is not a good approach because if first "x: any" can accept a "number" and second (y: any) can accept a non-number too, so the type used throughout the function is no longer constant (or generalized) and if anyone inputs something different, the function may give a wrong answer (assuming that the function given only works for one general type)
function curryAny(f) {
    return function (x) { return function (y) { return f(x, y); }; };
}
// alternative anoymous function generic declaration approach
var curryAnoy = function (f) { return function (x) { return function (y) { return f(x, y); }; }; }; // usual apporach
var curryAnoy2 = function (f) { return function (x) { return function (y) { return f(x, y); }; }; };
var curryAnoy3 = function (f) { return function (x) { return function (y) { return f(x, y); }; }; };
// const repe = (n: number, s: string) => s.repeat(n);
// console.log(repeat(3, "magestic warrior"));
var a = "hello magestic warrior\n";
// console.log(a.repeat(3));
var x = 1;
x = 'hello';
