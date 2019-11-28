function lazyNaturalNumbers(v) {
	return selector => selector(v, lazyNaturalNumbers(v+1));
}

const value = (v, f) => v, head = s => s(value)
const next = (v, f) => f, rest = s => s(next)


const next2 = function(v, f) {
	return f
}
const fromThree = lazyNaturalNumbers(3)
console.log(head(fromThree))
console.log(head(rest(fromThree)))

function getNth(n, s) {
	// if (number == 0) {
	// 	return head(from)
	// }
	// return getNth(number-1, rest(from));
	return n? getNth(n-1, s(next)) : s(value)
}

console.log(getNth(9, map(x=>2*x, fromThree)));

function map(f, s) {
	selector => selector(f(head(s), map(f, rest(s))))
}



