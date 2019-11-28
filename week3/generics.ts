function identity(x: any) : any { return x; }
const identity2 = function function_name<T> (x: T):T {
	return x;
};

// anonymous function WITHOUT DECLARATION
const anonyNo = function(f) {
	return function(x) {
		return function(y) {
			return f(x, y);
		}
	}
}

// anonymous function WTIH DECLARATION
const anonyYes = function(f: (x: number, y: number) => number) {
	return function(x: number) {
		return function(y: number) {
			return f(x, y);
		}
	}
}

// arrow function WITHOUT DECLARATION
const arrowNo = f => x => y => f(x, y);

// arrow function WITH DECLARATION
const arrowYes = (f: (x: number, y: number) => number) => (x: number) => (y: number) => f(x, y);

// function WITHOUT DECLARATION
function funcNo(f) { return function(x) { return function(y) { return f(x, y);}}}

// function WITH DECLARATION
function funcYes(f: (x: number, y: number) => number) { return function(x: number) { return function(y: number) { return f(x, y);}}}



/*
TYPE ALIAS
 */

type BinFunc = (x: number, y: number) => number
type CurrFnuc = (x: number) => (y: number) => number

// function WITH DECLARATION (instead of create multiple declarations, we use type alias to represen those declarations)
// funcYesAlias accepts a function of type BinFunc and returns a type of CurrFunc
function funcYesAlias(f: (BinFunc)) : CurrFnuc {
	return function(x: number) {
		return function(y: number) {
			return f(x, y);
		}
	}
}

// similar to approach below
function funcYesAlias2(f: (x: number, y: number) => number) : (x: number) => (y: number) => number {
	return function(x: number) {
		return function(y: number) {
			return f(x,y);
		}
	}
}

// aYesAlias accepts a type of BinFunc and returns a type of CurrFunc, both approach below are ok
const aYesAlias: (f:BinFunc) => CurrFnuc = f => x => y => f(x, y);



/*
GENERIC TYPES
 */
type BinaryFunction<T> = (x: T, y: T) => T
type CurriedFunction<T> = (x: T) => (y: T) => T

// this is how we define normally in other languages, with specific number declaration, but what if there is another number type appears in the future, then we have got to change the code, troublesome!
function curry(f : (x: number, y: number) => number) : (x: number) => (y: number) => number {
	return function(x: number) {
		return function(y: number) {
			return f(x, y);
		}
	}
}

// here is something more interesting, this curry2 function accepts one function of type generic (say type A, where A = {boolean, number, ...}
function curry2<T> (f : (x: T, y: T) => T) : (x: T) => (y: T) => T {
	return x => y => f(x, y);
}

// now we use alias to represent them
function curry3<T> (f: BinaryFunction<T>) : CurriedFunction<T> {
	return x => y => f(x, y);
}

// of course, we can also use "any", but this is not a good approach because if first "x: any" can accept a "number" and second (y: any) can accept a non-number too, so the type used throughout the function is no longer constant (or generalized) and if anyone inputs something different, the function may give a wrong answer (assuming that the function given only works for one general type)
function curryAny (f: (x: any, y: any) => any) : (x: any) => (y: any) => any {
	return x => y => f(x, y);
}

// alternative anoymous function generic declaration approach
const curryAnoy = f => x => y => f(x, y); // usual apporach
const curryAnoy2: <T> (f: (x: T, y: T) => T) => (x: T) => (y: T) => T = f => x => y => f(x, y);
const curryAnoy3: <T> (f: BinaryFunction<T>) => CurriedFunction<T> = f => x => y => f(x, y);


// const repe = (n: number, s: string) => s.repeat(n);
// console.log(repeat(3, "magestic warrior"));

var a = "hello magestic warrior\n";
// console.log(a.repeat(3));









