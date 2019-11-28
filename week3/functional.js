/**
 * METHODS OF WRITING FUNCTIONS
 * 1) with function declaration name, such as function thisIsAFunction()
 * 2) without function declaration name (also known as lambda expression or anonymous function):
 * 		1) using arrow notation
 * 		2) using "function" keyword
 */
// name declaration function
function namedFunction(x) {
    return x * 2;
}
// anonymous function using "function" keyword
var anonyFunction = function (x) {
    return x * 2;
};
// anoymous function using arrow notation
var anonyArrow = function (x) { return x * 2; };
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
function namedFunctionD(x) {
    return x * 2;
}
// for anonymous function using "function" keyword
var anonyFunctionD = function (x) {
    return x * 2;
};
// for arrow notation functions
var anonyArrowD = function (x) { return (x * 2); };
/**
 * HIGHER ORDER FUNCTIONS WITH DECLARATIONS
 */
// for name declaration function
function namedFunctionF(f) {
    return function (x) {
        return function (y) {
            return f(x, y);
        };
    };
}
// for anonymous function using "function" keyword
var anonyFunctionF = function (f) {
    return function (x) {
        return function (y) {
            return f(x, y);
        };
    };
};
// for arrow notation functions
var anonyArrowF = function (f) { return function (x) { return function (y) { return f(x, y); }; }; };
// for name declaration function
function namedFunctionFT(f) {
    return function (x) {
        return function (y) {
            return f(x, y);
        };
    };
}
// for anonymous function using "function" keyword
var anonyFunctionFT = function (f) {
    return function (x) {
        return function (y) {
            return f(x, y);
        };
    };
};
var anonyFunctionFT2 = function (f) {
    return function (x) {
        return function (y) {
            return f(x, y);
        };
    };
};
// for arrow notation functions
var anonyArrowFT = function (f) { return function (x) { return function (y) { return f(x, y); }; }; };
// for anonymous function using "function" keyword
var anonyFunctionFTG = function (f) {
    return function (x) {
        return function (y) {
            return f(x, y);
        };
    };
};
// for arrow notation functions
var anonyArrowFTG = function (f) { return function (x) { return function (y) { return f(x, y); }; }; };
// initialize a linkedlist of type LinkedListNode<T>
var linkedList = {
    data: 1,
    next: {
        data: 2,
        next: {
            data: 3,
            next: undefined
        }
    }
};
// computing the length of the linkedlist
function linkedLength(l) {
    // if (l) {
    // 	return 1 + linkedLength(l.next);
    // }
    // return 0;
    // alternative method of writing
    return l ? 1 + linkedLength(l.next) : 0;
}
// console.log(linkedLength(linkedList));
// prints out every data in the linkedlist
function forEach(f, l) {
    if (l) {
        f(l.data);
        forEach(f, l.next);
    }
}
function linkedLengthPointer(l) {
    return l ? 1 + linkedLengthPointer(l.next) : 0;
}
// console.log(linkedLengthPointer(linkedList));
function forEachPointer(f, l) {
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
function map(f, l) {
    if (l) {
        var linked = {
            data: f(l.data),
            next: map(f, l.next)
        };
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
forEachPointer(console.log, map(function (x) { return 2 * x; }, linkedList));
/**
 * INSTEAD OF USING {} TO INITIALIZE AN INTERFACE OF THOSE PROPERTIES, WE USE A CLASS INSTEAD, BECAUSE CLASS HAS A CONSTRUCTOR THAT ALLOWS US TO INSTANTIATE THE INTERFACE
 */
var LinkedList = /** @class */ (function () {
    // instead of using "?" to signify the existence of "next" property, we use the type that we have declared, which has a meaning of "exist" or "don't exist"
    function LinkedList(data, next) {
        this.data = data;
        this.next = next;
    }
    return LinkedList;
}());
function mapWithClass(f, l) {
    // why is it that declaring a return ListPointer<V> allows the returning of a LinkedList when the type of ListPointer only allows LinkedListNode OR undefined? 
    // it is true to say that it should return a type of ListPointer<V> which technically means it should return LinkedListNode, but here, LinkedList implements the LinkedListNode, so LinkedList is the subclass of LinkedListNode and returning the child of the LinkedListNode is acceptable due to object-oriented concepts
    if (l) {
        return new LinkedList(f(l.data), mapWithClass(f, l.next));
    }
    return undefined;
}
forEachPointer(console.log, mapWithClass(function (x) { return 2 * x; }, linkedList));
/**
 * A NEW DATA STRUCTURE - CONS LIST (CONCEPT SAME AS LINKED LIST, BUT THE DIFFERENCE HERE IS THAT CONS USES FUNCTIONAL PROGRAMMING
 * IT USES FUNCTION CALLING ANOTHER FUNCTION TO CONSTRUCT A LINKED LIST
 * TO BE MORE SPECIFIC, IT WILL BE A FUNCTION THAT ACCEPTS TWO PARAMETER, HEAD AND REST (SAME AS DATA & NEXT IN LINKED LIST)
 * EG: function cons(data, next)
 * THIS FUNCTION WILL ACCEPT A SELECTOR FUNCTION
 * WHERE THE SELECTOR FUNCTION ACTS LIKE A CHOOSER
 * function selector(data, next) SUCH THAT IT EITHER RETURNS THE DATA OR NEXT
 */
var consExample = function (data, next) {
    // it accepts a selector function, where the data and next will be passed into the selector function and the "data" or "next" will be returned by the selector function
    return function (f) {
        return f(data, next);
    };
};
// selector functions
var dataSelector = function (data, next) { return data; };
var nextSelector = function (data, next) { return next; };
var constLinkedList = consExample(1, consExample(2, consExample(3, undefined)));
console.log(constLinkedList(dataSelector));
console.log(constLinkedList(nextSelector)(dataSelector));
console.log(constLinkedList(nextSelector)(nextSelector)(dataSelector));
// now we create the functions using the new declared type
// const cons : <T>(f: (data: T, next: ConsList<T>) => Cons<T> = function(data, next) {
// 	return function(selector : Selector<T>) {
// 		return selector(data, next);
// 	}
// }
function cons(data, next) {
    // why can't we put Selector<T> instead of Cons<T>
    return function (selector) {
        return selector(data, next);
    };
}
// other than passing dataSelector into the returned function from the list, we can construct a function that allows us to return the head immediately when list is passed
function data(l) {
    // T is declared as the return type because there is always a data for every cons instantiated
    return l(dataSelector);
}
function next(l) {
    // ConsList<T> is used as return type because there is a possiblity where the tail is null, so it will return either a Cons<T> or undefined
    return l(nextSelector);
}
var consList = cons(1, cons(2, cons(3, null)));
// console.log(data(next(next(consList))));
function listToString(l) {
    if (l) {
        return data(l) + " " + listToString(next(l));
    }
    return "";
}
// console.log(listToString(consList));
