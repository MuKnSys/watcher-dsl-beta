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


function greet2(name: string): string {
  return `Hello, ${name}!`;
}

// interface Friend {
//   name: string;
//   favoriteColor?: string;
// }
// function add(friend: Friend) {
//   var name = friend.name;
// }
// add({ name: "Fred" });  // Ok
function add(a: number, b: number): number {
  return (a * b) + 2;
}

console.log(greet("Example for parsing"));


