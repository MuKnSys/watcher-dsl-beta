// class Person {
//   name: string;
//   age: number;
  
//   constructor(name: string, age: number) {
//     this.name = name;
//     this.age = age;
//   }
//   greets(): void {
//     console.log(`Hello, my name is ${this.name} and I'm ${this.age}.`);
//   }
// }
// const example = new Person('example', 1);
// example.greets();


function greet(name: string): string {
  return `Hello, ${name}!`;
}
console.log(greet("Example for parsing"));
