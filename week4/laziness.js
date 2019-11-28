function lessSillyNaturalNumbers(v) {
    return function () { return lessSillyNaturalNumbers(v + 1); };
}
function take(n) {
    return function _next(v) {
        return {
            value: v,
            next: function () { return v < n ? _next(v + 1) : undefined; }
        };
    }(1);
}
var n = take(3);
console.log(n.value);
console.log(n.next().value);
console.log(n.next().next().value);
console.log(n.next().next().next());
console.log(lessSillyNaturalNumbers(1)());
