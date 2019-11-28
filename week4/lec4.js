function lazyNaturalNumbers(v) {
    return function (selector) { return selector(v, lazyNaturalNumbers(v + 1)); };
}
var value = function (v, f) { return v; }, head = function (s) { return s(value); };
var next = function (v, f) { return f; }, rest = function (s) { return s(next); };
var next2 = function (v, f) {
    return f;
};
var fromThree = lazyNaturalNumbers(3);
console.log(head(fromThree));
console.log(head(rest(fromThree)));
function getNth(n, s) {
    // if (number == 0) {
    // 	return head(from)
    // }
    // return getNth(number-1, rest(from));
    return n ? getNth(n - 1, s(next)) : s(value);
}
console.log(getNth(9, map(function (x) { return 2 * x; }, fromThree)));
function map(f, s) {
    (function (selector) { return selector(f(head(s), map(f, rest(s)))); });
}
