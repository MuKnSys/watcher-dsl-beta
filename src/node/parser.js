const { parse, ParserOptions } = require('@typescript-eslint/parser');
const fs = require('fs');

const filePath = './index.ts';
const code = fs.readFileSync(filePath, 'utf-8');

const parserOptions = {
  sourceType: 'module',
  ecmaVersion: 2020,
  project: './tsconfig.json',
  range : true
};
console.log(parserOptions)
const ast = parse(code, parserOptions);
console.log(JSON.stringify(ast, null, 2));
