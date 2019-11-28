// create an observer contract such as each event observer must have these 3 defined methods to be considered as an observer
interface Observer<Input> {
	next(value: Input) : void;
	complete(): void;
	unsub?: () => void;
}


class SafeObservable<T> implements Observer<T> {

	private isUnsubscribed = false;
	private destination: Observer<T>;

	unsub?: () => void;

	constructor(destination: Observer<T>) {
		this.destination = destination;
		// if the observer has the ability to unsubscribe itself from the observable, then çreate a method of unsub such that it has the same command as the observer's unsubsribe method
		// this is to allow the safeobservable to unsubscribe itself
		if (destination.unsub) {
			this.unsub = destination.unsub;
		}
	}

	next(value: T): void {
		if (!this.isUnsubscribed) {
			this.destination.next(value);
		}
	}

	complete() : void {
		if (!this.isUnsubscribed) {
			this.destination.complete();
			// this.isUnsubscribed = true;
			// // if the observer has an unsubcribe method to unsubscribe to an event call it
			// if (this.unsub) {
			// 	this.unsub();
			// }

			// above implemention is same as calling a separate unsubscribe() method as below
			this.unsubscribe();
		}
	}

	unsubscribe() : void {
		if (!this.isUnsubscribed) {
			this.isUnsubscribed = true;
			if (this.unsub) {
				this.unsub();
				// same as calling this.destination.unsub();
			}
		}
	}
}

let x = new SafeObservable({
	next: e => console.log(e),
	complete: () => console.log("completed!"),
	unsub: () => console.log("unsubscribed!")
})

x.next(3);
x.complete();
x.next(10);


/*
Observable takes in a stream of information either through fromEvent (which we discuss later) or fromArray

If the data is obtained through fromArray, Obseravable will return a function such that this function takes in an observer. Why does the function needs to return an observer? Observable = stream of data or basically a book of information. Observable holds the data, but does not know what to do with the data until an observer is passed into the observable. 

Think of Observable as a factory that takes in a series of resources, but does not know what to do with those materials until a book of instruction is provided to the factory so that the factory can "do something" on the materials based on the instructions given. 

So when fromArray is called, Obseravable acts like a ready-factory that prepares to receive an observe (where this observe knows what to do with the data) - like a series of instructions to perform for the data. 

let say an observer is created, such that this observer has a method called next() that takes in "a" and print(a) and a complete() method that prints an announcement that the execution has completed. >> AN OBSERVER = LIST OF INSTRUCTIONS TO PERFORM. In this case, the instructions are taking each input and print the input, then the instructions also say that when all inputs are printed out, call the complete() method to announce that the factory's job is completed. 


 */
class Observable<Input> {
	// _subscribe is a function that takes in an observer and returns a function (that returns void)
	private runSubscription: (_:Observer<Input>) => () => void;

	// constructor(private _subscribe: (_:Observer<Input>) => () => void) {
	// 	// empty constructor to construct observable
	// }

	constructor(_sub : (_:Observer<Input>) => () => void) {
		this.runSubscription = _sub;
	}

	subscribe(next: (_:Input) => void, complete? : () => void) : () => void {
		const safeObserver = new SafeObservable({
			next: next,
			complete: complete? complete: () => console.log("completed")
		});

		// runsubscription passes in the instructions book (observer) and execute all elements upon the instructions given through the next() function
		safeObserver.unsub = this.runSubscription(safeObserver);
		return safeObserver.unsubscribe.bind(safeObserver);
	}

	static fromEvent<E extends Event>(el: Node, name: string): Observable<E>{

		function aa(observer: Observer<E>) {
			const listener = <EventListener> function(e: E) {
				return observer.next(e);
			}
			el.addEventListener(name, listener);
			return () => el.removeEventListener(name, listener);
		}
		return new Observable<E>(aa);
	}
	
	static fromArray<V>(arr : V[]) : Observable<V> {

		/*
		aa is a function that takes in a book of instructions
		this book of instructions has two main instructions - next and complete
		for example, the next could be taking in an element and output the element's value in the console log, or output the doubled value in the console log. 
		meaning, different observers have different instructions to execute (or templates or menus). 

		In this case, for every element in the array, it passes the element into the next function so that the next function could be executed. Taking the example above, if the next() function takes in an element and prints out the element, then in this case, the next takes in each element and outputs the element

		And when it is done, it calls the complete function to announce that its job is completed
		 */
		function aa(observer: Observer<V>) {
			arr.forEach(element => observer.next(element));
			observer.complete();
			return () => {};
		}
		return new Observable<V>(aa);
	}

	// map<R>(transform: (_:Input) => R): Observable<R> {
	// 	function aa(observer: Observer<R>) {
	// 		return this.subscribe(e=>observer.next(transform(e)), ()=> observer.complete());
	// 	}
	// 	return new Observable<R>(aa);
	// }
	map<R>(transform: (_:Input)=>R): Observable<R> {
		// the observer passed is going to call the transformed element instead of the actual element
		return new Observable<R>(observer=>
			this.subscribe(
				element=>observer.next(transform(element)), 
				()=>observer.complete())
			);
  }

  forEach(f: (_:Input)=> void) : Observable<Input> {
  	return new Observable<Input>(observer=>
  		this.subscribe(
  			element=>{f(element); return observer.next(element)}, 
  			()=>observer.complete())
  		);
  }

  filter(f: (_:Input) => boolean) : Observable<Input> {
  	return new Observable<Input> (observer => 
  		this.subscribe(
  			element => (f(element)? observer.next(element):undefined),  
  			() => observer.complete())
  		);
  }

  static interval(milliseconds: number, limit: number) : Observable<number> {
  	return new Observable<number> (observer=>{
  		let time = 0;
  		const handle = setInterval(() => observer.next(time += milliseconds), milliseconds);
  		
  		setTimeout(() => {
  			clearInterval(handle);
  			observer.complete();
  		}, limit);
  		
  		return () => {};
  		});
  }


  takeUntil<V>(o: Observable<V>): Observable<Input> {
    return new Observable<Input>(observer => {
      // const oUnsub = o.subscribe(_=>{
      //   observer.complete();
      //   oUnsub();
      // });
      const oUnsub = o.subscribe(_=>observer.complete());
      return this.subscribe(e=>observer.next(e), () => {
        observer.complete();
        oUnsub();
      });
    }); 
  }

    /**
   * every time this Observable is notified, create an Observable using the specified stream creator 
   * output is "flattened" into the original stream
   * @param streamCreator function to create the incoming Observable stream
   * @return single ``flattened'' stream from all the created observables
   */
  flatMap<Output>(streamCreator: (_: Input) => Observable<Output>): Observable<Output> {
    return new Observable<Output>((observer: Observer<Output>)=>{
      return this.subscribe(t=>streamCreator(t).subscribe(o=>observer.next(o)),()=>observer.complete())
    })
  }
  
}

let ob: Observable<number> = Observable.fromArray([1,2,3,4,5,6]);
// console.log(ob);
// let aa = ob.subscribe(a => console.log(a), () => console.log("COMPLETE OB"));
// let k = ob.filter(x=> x % 2 == 0);
let k = ob.map(x=>x*2);

// this outputs the milliseconds starting from 2000 for every 2 seconds until 10 seconds is passed
// let ob2 = Observable.interval(2000, 10000);
// ob2.subscribe(a=>console.log(a));

let la = k.takeUntil(ob);
// la.subscribe(a => console.log(a), () => console.log("COMPLETE takeuntil"));


let test = Observable.fromArray([1,2,3]).flatMap(x => Observable.fromArray([4,5,6]));
test.subscribe(console.log);











