const recast = require('recast');
const fs = require('fs');
const path = require('path');

const filePath = path.join(__dirname,'..','..', 'data/examples', 'parserExample.json');
const ast = fs.readFileSync(filePath, 'utf-8');
const parsedAst = JSON.parse(ast);


const tsCode = recast.print(parsedAst, { parser: require('recast/parsers/typescript') }).code;

console.log(tsCode);
