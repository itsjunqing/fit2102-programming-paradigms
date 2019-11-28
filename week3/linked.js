var l = {
    data: 1,
    next: {
        data: 2,
        next: {
            data: 3
        }
    }
};
// takes in a variable of type IListNode (which is a structure) and returns a number
// it is also possible for this function to take in no parameter
function len(l) {
    // if there is a structure of IListNode received, it returns 1 + the returned value of a recursive call with the next property
    return l ? 1 + len(l.next) : 0;
}
console.log(len(l));
// forEach takes in a function and an optional IListNode structure and returns nothing
// if the optional IListNode is received, it will call the function with the parameter of the node's data for each linked node
function forEach(f, l) {
    if (l) {
        f(l.data);
        forEach(f, l.next);
    }
}
forEach(console.log, l);
/*
LINKED LIST MAP
 */
function map(f, l) {
    if (l) {
        var result = {
            data: f(l.data),
            next: map(f, l.next)
        };
        return result;
    }
    return undefined;
}
forEach(console.log, map(function (x) { return 2 * x; }, l));
var ListNode = /** @class */ (function () {
    function ListNode(data, next) {
        this.data = data;
        this.next = next;
    }
    return ListNode;
}());
function map2(f, l) {
    if (l) {
        return new ListNode(f(l.data), map(f, l.next));
    }
    return undefined;
}
forEach(console.log, map2(function (x) { return 2 * x; }, l));
function concat(a, b) {
    if (a) {
        return new ListNode(a.data, concat(a.next, b));
    }
    if (b) {
        return concat(b.next);
    }
    return undefined;
}
forEach(console.log, concat(l, map(function (x) { return 3 + x; }, l)));
/*
CHURCH ENCODING
 */
var cons = function (head, rest) { return function (selector) { return selector(head, rest); }; };
var list123 = cons(1, cons(2, cons(3, cons(4, cons(5, cons("hello", null))))));
console.log(list123);
var head = function (list) { return list(function (head, rest) { return head; }); };
// const head = function(list) {
// 	return list(function(head, rest) {
// 		return head;
// 	}); 
// }
var rest = function (list) { return list(function (head, rest) { return rest; }); };
// const rest = function(list) {
// 	return list(function(head, rest) {
// 		return head;
// 	});
// }
console.log(head(list123));
// con is a data structure that returns a function x
// this function x acts as a selector such that it will have two parameters of head and tail and returns either the head or the tail of the con data structure 
var con = function (head, tail) { return function (f) { return f(head, tail); }; };
// to be more specific, it works like this, 
// con: a function that takes in two parameters: head and tail and then return function x, such that this function x act as a "selector" that inputs its head and tail and then returns either the head or tail. 
var conSpecific = function (head, tail) { return function (f) { return f(head, tail); }; };
// headie is the function X that returns the head
// function x must input head and tail! and return either the head or tail, so in this case, headie returns the head!
var funcHead = function (head, tail) { return head; };
var funcTail = function (head, tail) { return tail; };
var connie = function (head, tail) {
    return function (f) {
        return f(head, tail);
    };
};
var connie2 = function (head, tail) {
    return function (f) {
        return f(head, tail);
    };
};
// lissie is a function that has an input parameter of another function
var lissie = connie2(1, connie(2, connie(3, null)));
var funcHeadie = function (list) {
    // out from the given list, return its head
    return list(funcHead);
};
var funcTailie = function (list) {
    // out from the given list, return its tail
    return list(funcTail);
};
// now we input that "another function" into lissie function
// console.log(lissie(funcTail));
console.log(funcHeadie(funcTailie(funcTailie(lissie))));
function listToString(l) {
    if (l) {
        return funcHeadie(l) + " " + listToString(funcTailie(l));
    }
    return "";
}
var lissie2 = connie2("alpha", connie2("bravo", connie2("charlie", connie2("denmark", connie2("last", null)))));
console.log(listToString(lissie));
console.log(listToString(lissie2));
