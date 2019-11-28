var SafeObservable = /** @class */ (function () {
    function SafeObservable(destination) {
        this.isUnsubscribed = false;
        this.destination = destination;
        // if the observer has the ability to unsubscribe itself from the observable, then çreate a method of unsub such that it has the same command as the observer's unsubsribe method
        // this is to allow the safeobservable to unsubscribe itself
        if (destination.unsub) {
            this.unsub = destination.unsub;
        }
    }
    SafeObservable.prototype.next = function (value) {
        if (!this.isUnsubscribed) {
            this.destination.next(value);
        }
    };
    SafeObservable.prototype.complete = function () {
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
    };
    SafeObservable.prototype.unsubscribe = function () {
        if (!this.isUnsubscribed) {
            this.isUnsubscribed = true;
            if (this.unsub) {
                this.unsub();
                // same as calling this.destination.unsub();
            }
        }
    };
    return SafeObservable;
}());
var x = new SafeObservable({
    next: function (e) { return console.log(e); },
    complete: function () { return console.log("completed!"); },
    unsub: function () { return console.log("unsubscribed!"); }
});
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
var Observable = /** @class */ (function () {
    // constructor(private _subscribe: (_:Observer<Input>) => () => void) {
    // 	// empty constructor to construct observable
    // }
    function Observable(_sub) {
        this.runSubscription = _sub;
    }
    Observable.prototype.subscribe = function (next, complete) {
        var safeObserver = new SafeObservable({
            next: next,
            complete: complete ? complete : function () { return console.log("completed"); }
        });
        // runsubscription passes in the instructions book (observer) and execute all elements upon the instructions given through the next() function
        safeObserver.unsub = this.runSubscription(safeObserver);
        return safeObserver.unsubscribe.bind(safeObserver);
    };
    Observable.fromEvent = function (el, name) {
        function aa(observer) {
            var listener = function (e) {
                return observer.next(e);
            };
            el.addEventListener(name, listener);
            return function () { return el.removeEventListener(name, listener); };
        }
        return new Observable(aa);
    };
    Observable.fromArray = function (arr) {
        /*
        aa is a function that takes in a book of instructions
        this book of instructions has two main instructions - next and complete
        for example, the next could be taking in an element and output the element's value in the console log, or output the doubled value in the console log.
        meaning, different observers have different instructions to execute (or templates or menus).

        In this case, for every element in the array, it passes the element into the next function so that the next function could be executed. Taking the example above, if the next() function takes in an element and prints out the element, then in this case, the next takes in each element and outputs the element

        And when it is done, it calls the complete function to announce that its job is completed
         */
        function aa(observer) {
            arr.forEach(function (element) { return observer.next(element); });
            observer.complete();
            return function () { };
        }
        return new Observable(aa);
    };
    // map<R>(transform: (_:Input) => R): Observable<R> {
    // 	function aa(observer: Observer<R>) {
    // 		return this.subscribe(e=>observer.next(transform(e)), ()=> observer.complete());
    // 	}
    // 	return new Observable<R>(aa);
    // }
    Observable.prototype.map = function (transform) {
        var _this = this;
        // the observer passed is going to call the transformed element instead of the actual element
        return new Observable(function (observer) {
            return _this.subscribe(function (element) { return observer.next(transform(element)); }, function () { return observer.complete(); });
        });
    };
    Observable.prototype.forEach = function (f) {
        var _this = this;
        return new Observable(function (observer) {
            return _this.subscribe(function (element) { f(element); return observer.next(element); }, function () { return observer.complete(); });
        });
    };
    Observable.prototype.filter = function (f) {
        var _this = this;
        return new Observable(function (observer) {
            return _this.subscribe(function (element) { return (f(element) ? observer.next(element) : undefined); }, function () { return observer.complete(); });
        });
    };
    Observable.interval = function (milliseconds, limit) {
        return new Observable(function (observer) {
            var time = 0;
            var handle = setInterval(function () { return observer.next(time += milliseconds); }, milliseconds);
            setTimeout(function () {
                clearInterval(handle);
                observer.complete();
            }, limit);
            return function () { };
        });
    };
    Observable.prototype.takeUntil = function (o) {
        var _this = this;
        return new Observable(function (observer) {
            // const oUnsub = o.subscribe(_=>{
            //   observer.complete();
            //   oUnsub();
            // });
            var oUnsub = o.subscribe(function (_) { return observer.complete(); });
            return _this.subscribe(function (e) { return observer.next(e); }, function () {
                observer.complete();
                oUnsub();
            });
        });
    };
    /**
   * every time this Observable is notified, create an Observable using the specified stream creator
   * output is "flattened" into the original stream
   * @param streamCreator function to create the incoming Observable stream
   * @return single ``flattened'' stream from all the created observables
   */
    Observable.prototype.flatMap = function (streamCreator) {
        var _this = this;
        return new Observable(function (observer) {
            return _this.subscribe(function (t) { return streamCreator(t).subscribe(function (o) { return observer.next(o); }); }, function () { return observer.complete(); });
        });
    };
    return Observable;
}());
var ob = Observable.fromArray([1, 2, 3, 4, 5, 6]);
// console.log(ob);
// let aa = ob.subscribe(a => console.log(a), () => console.log("COMPLETE OB"));
// let k = ob.filter(x=> x % 2 == 0);
var k = ob.map(function (x) { return x * 2; });
// this outputs the milliseconds starting from 2000 for every 2 seconds until 10 seconds is passed
// let ob2 = Observable.interval(2000, 10000);
// ob2.subscribe(a=>console.log(a));
var la = k.takeUntil(ob);
// la.subscribe(a => console.log(a), () => console.log("COMPLETE takeuntil"));
var test = Observable.fromArray([1, 2, 3]).flatMap(function (x) { return Observable.fromArray([4, 5, 6]); });
test.subscribe(console.log);
