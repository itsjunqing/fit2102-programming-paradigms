interface LazySequence<T> {
	value: T;
	next(): LazySequence<T>;
}


function lessSillyNaturalNumbers(v: number) {
	return () => lessSillyNaturalNumbers(v+1);
}

function take(n) : LazySequence<number> {
	return function _next(v: number) {
		return {
			value: v,
			next: () => v < n? _next(v+1) : undefined
		}
	}(1)
}


const n = take(3);
console.log(n.value);
console.log(n.next().value);
console.log(n.next().next().value);
console.log(n.next().next().next());

console.log(lessSillyNaturalNumbers(1)());