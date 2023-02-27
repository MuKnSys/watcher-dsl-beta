const { parse } = require('@typescript-eslint/parser');
const fs = require('fs');
const path = require('path');
const inputfile = process.argv[2];



const parserOptions = {
  sourceType: 'module',
  ecmaVersion: 2020,
  include: [path.join(__dirname, 'estree.ts')],
  project: path.join(__dirname, 'tsconfig.json'),
  range : true
};

try {

  const filePath = path.join(__dirname,'..','..', 'data/examples', inputfile);
  const code = fs.readFileSync(filePath, 'utf-8');
  const ast = parse(code, parserOptions);
  console.log(JSON.stringify(ast, null, 2));

} catch (err) {
  if (err.code == 'ENOENT') {
    console.error(`Input file '${inputfile}' not found`);
  } else if (err.message.includes('Unexpected token')) {
    console.error(`Invalid syntax in input file '${inputfile}': ${err.message}`);
  } else {
    console.error(`Error while parsing code in input file '${inputfile}': ${err.message}`);
  }
  process.exit(1);

}