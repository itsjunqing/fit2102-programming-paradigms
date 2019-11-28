/**
 * METHODS OF WRITING FUNCTIONS
 * 1) with function declaration name, such as function thisIsAFunction()
 * 2) without function declaration name (also known as lambda expression or anonymous function):
 * 		1) using arrow notation
 * 		2) using "function" keyword
 */

// name declaration function
function namedFunction(x) {
	return x*2;
}
// anonymous function using "function" keyword
const anonyFunction = function(x) {
	return x*2;
}
// anoymous function using arrow notation
const anonyArrow = x => x*2;

// testing all three versions of writing functions
function testFunctions() {
	console.log(namedFunction(3));
	console.log(anonyFunction(3));
	console.log(anonyArrow(3));
}

// testFunctions();


/**
 * NORMAL FUNCTIONS WITH DECLARATIONS
 */

// for name declaration function
function namedFunctionD(x: number) : number {
	return x*2;
}
// for anonymous function using "function" keyword
const anonyFunctionD = function(x : number) : number {
	return x*2;
}
// for arrow notation functions
const anonyArrowD = (x: number) => (x*2);


/**
 * HIGHER ORDER FUNCTIONS WITH DECLARATIONS
 */

// for name declaration function
function namedFunctionF(f: (x: number, y: number) => number) : ((x: number) => (y: number) => number) {
	return function(x: number) {
		return function(y: number) {
			return f(x, y);
		}
	}
} 
// for anonymous function using "function" keyword
const anonyFunctionF = function(f: (x: number, y: number) => number) : ((x: number) => (y: number) => number) {
	return function(x: number) {
		return function(y: number) {
			return f(x, y);
		}
	}
}
// for arrow notation functions
const anonyArrowF = (f: (x: number, y: number) => number) => (x: number) => (y: number) => f(x, y);


/**
 * WRITING LONG DECLARATIONS IS LENGTHY! 
 * WE USE ALIAS (A FORM OF REPRESENTATION), 
 * EG: WE DECLARE TYPE X = "ABC", SO WHEN TYPE X IS USED, THE VARIABLE MUST BE A STRING OF "ABC"
 * USUALLY WE DECLARE TYPES WITH FIRST LETTER OF EVERY WORD AS CAPITALIZED
 */

type FunctionType = (x: number, y: number) => number;
type DoubleFunctionType = (x: number) => (y: number) => number;

// for name declaration function
function namedFunctionFT(f: FunctionType) : DoubleFunctionType {
	return function(x) {
		return function(y) {
			return f(x, y);
		}
	}
}
// for anonymous function using "function" keyword
const anonyFunctionFT = function(f: FunctionType) : DoubleFunctionType {
	return function(x) {
		return function(y) {
			return f(x, y);
		}
	}
}
const anonyFunctionFT2: (f: FunctionType) => DoubleFunctionType = function(f) {
	return function(x) {
		return function(y) {
			return f(x, y);
		}
	}
}
// for arrow notation functions
const anonyArrowFT: (f: FunctionType) => DoubleFunctionType = f => x => y => f(x, y);


/**
 * WRITING ONLY WITH SPECIFIC TYPE IS NOT FLEXIBLE ENOUGH, VERY FIXTATE!
 * GENERIC TYPES SOLVES THIS, BY MAKING SURE THAT COMMON TYPES ARE USED THROUGHOUT THE FUNCTION WITH THE FLEXIBILITY OF ACCEPTING DIFFERENT TYPES
 * EG: IF TYPE "number" IS DETECTED, THAT TYPE "number" WILL BE USED THROUGHOUT THE FUNCTION WITH AN UNKNOWN REPRESENTING THE "number"
 */

type FunctionTypeG<T> = (x: T, y: T) => T;
type DoubleFunctionTypeG<T> = (x: T) => (y: T) => T;
// for anonymous function using "function" keyword
const anonyFunctionFTG: <T>(f: FunctionTypeG<T>) => DoubleFunctionTypeG<T> = function(f) {
	return function(x) {
		return function(y) {
			return f(x, y);
		}
	}
}
// for arrow notation functions
const anonyArrowFTG: <T>(f: FunctionTypeG<T>) => DoubleFunctionTypeG<T> = f => x => y => f(x, y);


/**
 * INTERFACES, WHICH IS A STRUCTURE OR A SET TO HOLD MULTIPLE PROPERTIES, SIMILAR TO STRUCT IN C LANGUAGE
 */
interface LinkedListNode<T> {
	data: T;
	next?: LinkedListNode<T>;
}

// initialize a linkedlist of type LinkedListNode<T>
const linkedList: LinkedListNode<number> = {
	data: 1, 
	next: {
		data: 2,
		next: {
			data: 3, 
			next: undefined
		}
	}
}

// computing the length of the linkedlist
function linkedLength<T>(l?: LinkedListNode<T>) : number {
	// if (l) {
	// 	return 1 + linkedLength(l.next);
	// }
	// return 0;
	// alternative method of writing
	return l? 1+linkedLength(l.next) : 0;
}
// console.log(linkedLength(linkedList));

// prints out every data in the linkedlist
function forEach<T>(f, l?: LinkedListNode<T>) : void {
	if (l) {
		f(l.data);
		forEach(f, l.next);
	}
}
// forEach(console.log, linkedList);


/**
 * INSTEAD OF USING "?" IN PARAMETERS, WE USE A TYPE REPRESENTATION TO SIGNIFY THE PRESENCE OF THE PARAMETER
 * EG: USING A TYPE TO SIGNIFY IF LinkedListNode<T> EXISTS AS A PARAMETER
 */

type ListPointer<T> = LinkedListNode<T> | undefined;

function linkedLengthPointer<T>(l: ListPointer<T>) : number {
	return l? 1+linkedLengthPointer(l.next) : 0;
}
// console.log(linkedLengthPointer(linkedList));
function forEachPointer<T>(f, l: ListPointer<T>) : void {
	if (l) {
		f(l.data);
		forEachPointer(f, l.next);
	}
}
// forEachPointer(console.log, linkedList);



/**
 * CREATING A NEW MAP FOR LINKED LIST, SIMILAR TO 
 * [1,2,3,4,5].map(x=>2*x) THAT RETURNS A NEW ARRAY THAT DOUBLES EACH ELEMENT
 * THIS MAP WILL RETURN A NEW LinkedListNode<T>
 */

////// WHY DOES THIS WORK WITHOUT FUNCTION DELCARATION? PURPOSE OF RETURNING ListPointer<V> instead of ListPointer<T>?
function map<T,V>(f, l: ListPointer<T>) : ListPointer<T> {
	if (l) {
		const linked: ListPointer<T> = {
			data: f(l.data),
			next: map(f, l.next)
		}
		return linked;
	}
	return undefined;
}

// correct returning type implementation
// function map<T,V>(f: (_:T)=>V, l: ListPointer<T>) : ListPointer<V> {
// 	if (l) {
// 		const linked: ListPointer<V> = {
// 			data: f(l.data),
// 			next: map(f, l.next)
// 		}
// 		return linked;
// 	}
// 	return undefined;
// }

forEachPointer(console.log, map(x=>2*x, linkedList));


/**
 * INSTEAD OF USING {} TO INITIALIZE AN INTERFACE OF THOSE PROPERTIES, WE USE A CLASS INSTEAD, BECAUSE CLASS HAS A CONSTRUCTOR THAT ALLOWS US TO INSTANTIATE THE INTERFACE 
 */

class LinkedList<T> implements LinkedListNode<T> {
	// instead of using "?" to signify the existence of "next" property, we use the type that we have declared, which has a meaning of "exist" or "don't exist"
	constructor(
		public data: T, 
		public next: ListPointer<T>) {}
	// old approach is as below, with "?"
	// constructor(
	// 	public data: T, 
	// 	public next?: LinkedListNode<T>) {}
}

function mapWithClass<T,V>(f: (_:T)=>V, l: ListPointer<T>) : ListPointer<V> {
	// why is it that declaring a return ListPointer<V> allows the returning of a LinkedList when the type of ListPointer only allows LinkedListNode OR undefined? 
	// it is true to say that it should return a type of ListPointer<V> which technically means it should return LinkedListNode, but here, LinkedList implements the LinkedListNode, so LinkedList is the subclass of LinkedListNode and returning the child of the LinkedListNode is acceptable due to object-oriented concepts
	if (l) {
		return new LinkedList(f(l.data), mapWithClass(f, l.next));
	}
	return undefined;
}

forEachPointer(console.log, mapWithClass(x=>2*x, linkedList));


/**
 * A NEW DATA STRUCTURE - CONS LIST (CONCEPT SAME AS LINKED LIST, BUT THE DIFFERENCE HERE IS THAT CONS USES FUNCTIONAL PROGRAMMING
 * IT USES FUNCTION CALLING ANOTHER FUNCTION TO CONSTRUCT A LINKED LIST
 * TO BE MORE SPECIFIC, IT WILL BE A FUNCTION THAT ACCEPTS TWO PARAMETER, HEAD AND REST (SAME AS DATA & NEXT IN LINKED LIST)
 * EG: function cons(data, next)
 * THIS FUNCTION WILL ACCEPT A SELECTOR FUNCTION 
 * WHERE THE SELECTOR FUNCTION ACTS LIKE A CHOOSER
 * function selector(data, next) SUCH THAT IT EITHER RETURNS THE DATA OR NEXT
 */

const consExample = function(data: any, next: any) : (f: (data: any, next: any) => any) => any {
	// it accepts a selector function, where the data and next will be passed into the selector function and the "data" or "next" will be returned by the selector function
	return function(f: (data: any, next: any) => any) {
		return f(data, next);
	}
}
// selector functions
const dataSelector = (data, next) => data;
const nextSelector = (data, next) => next;

const constLinkedList = consExample(1, consExample(2, consExample(3, undefined)));

console.log(constLinkedList(dataSelector));
console.log(constLinkedList(nextSelector)(dataSelector));
console.log(constLinkedList(nextSelector)(nextSelector)(dataSelector));


// We know that the cons will return a selector, we can declare a new type
type Cons<T> = (selector: Selector<T>) => T | ConsList<T>;
// We know that cons's next is a selector, and this selector function has specific condition, which is it must input both data and next and selects either the data or next
type Selector<T> = (data: T, next: ConsList<T>) => T | ConsList<T>;
// We know that for every cons, it must have a data, but the "next" property is optional, like for the last node (or cons), the "next" is undefined, signifying the end of the list, we can declare a type
type ConsList<T> = Cons<T> | undefined;
type ConsType<T> = T | ConsList<T>;

// now we create the functions using the new declared type
// const cons : <T>(f: (data: T, next: ConsList<T>) => Cons<T> = function(data, next) {
// 	return function(selector : Selector<T>) {
// 		return selector(data, next);
// 	}
// }

function cons<T>(data: T, next: ConsList<T>) : Cons<T> {
	// why can't we put Selector<T> instead of Cons<T>
	return function(selector : Selector<T>) {
		return selector(data, next);
	}
}

// other than passing dataSelector into the returned function from the list, we can construct a function that allows us to return the head immediately when list is passed
function data<T>(l: Cons<T>) : T {
	// T is declared as the return type because there is always a data for every cons instantiated
	return <T> l(dataSelector);
}

function next<T>(l: Cons<T>) : ConsList<T> {
	// ConsList<T> is used as return type because there is a possiblity where the tail is null, so it will return either a Cons<T> or undefined
	return <Cons<T>> l(nextSelector);
}

const consList = cons(1, cons(2, cons(3, null)));

// console.log(data(next(next(consList))));

function listToString(l) {
	if (l) {
		return data(l) + " " + listToString(next(l));
	}
	return "";
}


// console.log(listToString(consList));


