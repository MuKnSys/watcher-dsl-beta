const recast = require('recast');
const fs = require('fs');
const path = require('path');
//const ts = require('typescript');

const inputfile = process.argv[2];
//const inputString = String(inputfile);
// const ast = fs.readFileSync(filePath, 'utf-8');
//const ast = JSON.stringify(inputString) 
const parsedAst = JSON.parse(inputfile);

//console.log("",  ast)

const tsCode = recast.print(parsedAst, { parser: require('recast/parsers/typescript') }).code;

console.log(tsCode)
return tsCode;