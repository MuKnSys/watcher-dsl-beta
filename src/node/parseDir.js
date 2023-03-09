const { parse } = require('@typescript-eslint/parser');
const fs = require('fs');
const path = require('path');
const parserOptions = {
    sourceType: 'module',
    removeComments: false,
    comment: true,
    ecmaVersion: 2020,
    include: [path.join(__dirname, 'estree.ts')],
    project: path.join(__dirname, 'tsconfig.json'),
    range : true

  };
  
const inputdir = "/home/pawel/Desktop/watcher-dsl-beta/data/test"
const outputdir = '/home/pawel/Desktop/watcher-dsl-beta/data/generetedTSJSON'; 

// const inputdir = process.argv[2];
// const outputdir = process.argv[3];


function processFile(inputPath, outputPath) {
  try {
    const code = fs.readFileSync(inputPath, 'utf-8');
    const ast = parse(code, parserOptions);
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
      } else {
        let outputEntryPath = path.join(outputPath, entry.name.replace(/\.ts$/, '.json'));
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
