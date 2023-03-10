const recast = require('recast');
const fs = require('fs');
const path = require('path');

const inputfile = process.argv[2];



try {
    const parsedAst = JSON.parse(inputfile);
    const tsCode = recast.print(parsedAst, { parser: require('recast/parsers/typescript') }).code;
    console.log(tsCode)
} catch (err) {
    console.error(err)
}
