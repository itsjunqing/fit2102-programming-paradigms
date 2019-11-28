/*
LINKED LIST
 */
interface IListNode<T> {
	data: T;
	next?: IListNode<T>;
}


const l: IListNode<number> = {
	data: 1, 
	next: {
		data: 2,
		next: {
			data: 3
		}
	}
}

// takes in a variable of type IListNode (which is a structure) and returns a number
// it is also possible for this function to take in no parameter
function len<T>(l?: IListNode<T>) : number {
	// if there is a structure of IListNode received, it returns 1 + the returned value of a recursive call with the next property
	return l? 1+len(l.next) : 0;
}

console.log(len(l));

// forEach takes in a function and an optional IListNode structure and returns nothing
// if the optional IListNode is received, it will call the function with the parameter of the node's data for each linked node
function forEach<T>(f: ((_:T) => void), l? :IListNode<T>): void {
	if (l) {
		f(l.data);
		forEach(f, l.next);
	}
}

forEach(console.log, l);

// the type of ListPtr can be either IListNode or undefined
type ListPtr<T> = IListNode<T> | undefined;


/*
LINKED LIST MAP
 */

function map<T, V>(f: ((_:T)=>V), l: ListPtr<T>) : ListPtr<V> {
	if (l) {
		let result: IListNode<V> = <IListNode<V>> {
			data: f(l.data),
			next: map(f, l.next)
		}
		return result;
	} 
	return undefined;
}

forEach(console.log, map(x=>2*x, l))

class ListNode<T> implements IListNode<T> {
	constructor(
		public data: T, 
		public next: ListPtr<T>) {}
}

function map2<T, V>(f: ((_:T)=>V), l: ListPtr<T>) : ListPtr<V> {
	if (l) {
		return new ListNode<V>(f(l.data), map(f, l.next));
	}
	return undefined;
}

forEach(console.log, map2(x=>2*x, l))



function concat<T>(a: ListPtr<T>, b?: ListPtr<T>) : ListPtr<T> {
	if (a) {
		return new ListNode<T>(a.data, concat(a.next, b))
	} 
	if (b) {
		return concat(b.next);
	}
	return undefined;
}

forEach(console.log, concat(l, map(x=>3+x, l)))



/*
CHURCH ENCODING
 */

const cons = (head, rest) => selector => selector(head, rest);
const list123 = cons(1, cons(2, cons(3, cons(4, cons(5, cons("hello", null))))));
console.log(list123);

const head = list => list((head, rest) => head);
// const head = function(list) {
// 	return list(function(head, rest) {
// 		return head;
// 	}); 
// }
const rest = list => list((head, rest) => rest);
// const rest = function(list) {
// 	return list(function(head, rest) {
// 		return head;
// 	});
// }

console.log(head(list123));

// con is a data structure that returns a function x
// this function x acts as a selector such that it will have two parameters of head and tail and returns either the head or the tail of the con data structure 
const con = (head, tail) => f => f(head, tail);
// to be more specific, it works like this, 
// con: a function that takes in two parameters: head and tail and then return function x, such that this function x act as a "selector" that inputs its head and tail and then returns either the head or tail. 
const conSpecific = (head: any, tail: any) => (f: (head: any, tail: any) => any) => f(head, tail);
// headie is the function X that returns the head
// function x must input head and tail! and return either the head or tail, so in this case, headie returns the head!
const funcHead = (head, tail) => head;
const funcTail = (head, tail) => tail;

const connie = function(head, tail) {
	return function(f) {
		return f(head, tail);
	}
}

const connie2 = function(head: any, tail: any)  {
	return function(f: (head: any, tail: any) => any) {
		return f(head, tail);
	}
}

// lissie is a function that has an input parameter of another function
const lissie = connie2(1, connie(2, connie(3, null)));

const funcHeadie = function(list) {
	// out from the given list, return its head
	return list(funcHead);
}
const funcTailie = function(list) {
	// out from the given list, return its tail
	return list(funcTail);
}
// now we input that "another function" into lissie function
// console.log(lissie(funcTail));
console.log(funcHeadie(funcTailie(funcTailie(lissie))));

function listToString(l) {
	if (l) {
		return funcHeadie(l) + " " + listToString(funcTailie(l));
	}
	return "";
}

const lissie2 = connie2("alpha", connie2("bravo", connie2("charlie", connie2("denmark", connie2("last", null)))));

console.log(listToString(lissie));
console.log(listToString(lissie2));










