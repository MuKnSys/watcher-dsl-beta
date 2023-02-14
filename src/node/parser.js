const { parse, ParserOptions } = require('@typescript-eslint/parser');
const fs = require('fs');
const path = require('path');
const filePath = path.join(__dirname, 'index.ts');
const code = fs.readFileSync(filePath, 'utf-8');

const parserOptions = {
  sourceType: 'module',
  ecmaVersion: 2020,
  include: [path.join(__dirname, 'estree.ts')],
  project: path.join(__dirname, 'tsconfig.json'),
  range : true
};
const ast = parse(code, parserOptions);
console.log(JSON.stringify(ast, null, 2));
