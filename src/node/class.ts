class Person {
  name: string;
  age: number;
  
  
  constructor(name: string, age: number) {
    this.name = name;
    this.age = age;
  }

  greet(): void {
    console.log(`Hello, my name is ${this.name} and I'm ${this.age}.`);
  }

}


const pawel = new Person('Pawel', 25);
pawel.greet();