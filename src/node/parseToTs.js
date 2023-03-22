const recast = require('recast');
const fs = require('fs');
const path = require('path');
const inputfile = process.argv[2];



try {
    const parsedAst = JSON.parse(inputfile);
    const printOtions = {
        parser: require('recast/parsers/typescript'),
        comments: parsedAst.comments,
        tabWidth: 2,
        quote: 'single',
        trailingComma: true,
    };
    const tsCode = recast.prettyPrint(parsedAst, printOtions).code;
    // console.log(comments)
    console.log(tsCode)
} catch (err) {
    console.error(err)
}
