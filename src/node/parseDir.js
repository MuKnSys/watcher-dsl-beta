const { parse } = require('@typescript-eslint/parser');
const fs = require('fs');
const path = require('path');
const replace = require('./replace');
const phrasesToSearch = require('./replace').phrasesToSearch
const inputdir = process.argv[2];
const outputdir = process.argv[3];
 
const parserOptions = {
    sourceType: 'module',
    removeComments: false,
    comment: true,
    ecmaVersion: 2020,
    include: [path.join(__dirname, 'estree.ts')],
    project: path.join(__dirname, 'tsconfig.json'),
    range : true,
    attachComments: true,
    errorOnUnknownASTType: false,
    emitDecoratorMetadata : true, 
  };



function processFile(inputPath, outputPath) {
  try {
    const code = fs.readFileSync(inputPath, 'utf-8');
    let replacedCode = code;
  
    phrasesToSearch.forEach(phrase => {
      replacedCode = replacedCode.replace(new RegExp(phrase.search, 'g'), phrase.replace);
    });
    console.log(replacedCode)
    const ast = parse(replacedCode, parserOptions);
    fs.writeFileSync(outputPath, JSON.stringify(ast, null, 2));
  } catch (err) {
    console.error(`Error while processing file '${inputPath}': ${err.message}`);
  }
}

function processDirr(inputPath, outputPath) {
  try {
    const entries = fs.readdirSync(inputPath, { withFileTypes: true });
    fs.mkdirSync(outputPath, { recursive: true });
    for (const entry of entries) {
      const inputEntryPath = path.join(inputPath, entry.name);
      let outputEntryPath = path.join(outputPath, entry.name);
      if (entry.isDirectory()) {
        processDirr(inputEntryPath, outputEntryPath);
      } else if (entry.name.slice(-7) == "watcher"){
        let outputEntryPath = path.join(outputPath, entry.name.replace(/\.watcher$/, '.json'));
        processFile(inputEntryPath, outputEntryPath);
      }
    }
  } catch (err) {
    console.error(`Error while processing directory '${inputPath}': ${err.message}`);
  }
}

try {
  processDirr(inputdir, outputdir);
} catch (err) {
  console.error(`Error while processing files: ${err.message}`);
  process.exit(1);
}
console.log(outputdir)
