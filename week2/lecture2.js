function Person(name, occupation) {
	this.name = name;
	this.occupation = occupation;
}

// prototype approach of creating methods of a class (or objects)
Person.prototype.sayHello = function() {
	console.log("Hi, my name's "+ this.name + " and I'm " + this.occupation + "!");
}

var tim = new Person("tim", "funnie");
console.log(tim.name, tim.occupation);
tim.sayHello();

var ian = new Person("ian", "lecturer PP");
console.log(ian.name, ian.occupation);
ian.sayHello();

// class approach of creating methods (same as Java with different syntax)
class Person2 {
	constructor(name, occupation) {
		this.name = name;
		this.occupation = occupation;
	}
	// getters is the same as creating an instance variable 
	get greetings() {
		return "Hi, my name's "+ this.name + " and I'm " + this.occupation + "!";
	}
	sayHello() {
		console.log(this.greetings);
	}
}

var iannie = new Person2("Iannie", "lecture PP");
iannie.sayHello();

class LoudPerson extends Person2 {
	sayHello() {
		console.log(this.greetings.toUpperCase());
	}
}

const timmie = [new Person("Timmie", "lecture PP"), new LoudPerson("Timmeh", "lecchar PP" )]
timmie.forEach(person => person.sayHello());


class Person3 {
	constructor(name, occupation, voiceTransform) {
		this.name = name;
		this.occupation = occupation;
		this.voiceTransform = voiceTransform;
	}

	get greetings() {
		return "Hi, my name's "+ this.name + " and I'm " + this.occupation + "!";
	}

	sayHello() {
		// this.voiceTransform is an instance function that inputs a string and converts them to all upper case
		console.log(this.voiceTransform(this.greetings));
	}
}

var peeyeh = new Person3("peeyeh", "pee everyday", g=>g.toUpperCase());
peeyeh.sayHello()