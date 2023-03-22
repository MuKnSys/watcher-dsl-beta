module.exports = {
    phrasesToSearch : [
        { search: /fetch (\w+)/g, replace: 'function FETCH$1' },
        { search: /fetch/g, replace: 'function FETCH$1' },
        { search: /process function (\w+)/g, replace: 'function PROCESS$1' },
        { search: /serve function (\w+)/g, replace: 'function SERVE$1' },
        { search: /request (\w+)/g, replace: 'function REQUEST$1' },
        { search: /process (\w+)/g, replace: 'function PROCESS$1' },
        { search: /import (contract) (\w+)\.(\w+)\.(\w+) from "([^"]+)";/g, replace: 'import {$1, $2$3$4} from "$5";' },
        { search: /import (contract) (\w+)\.(\w+)\.(\w+)\.(\w+) from "([^"]+)";/g, replace: 'import {$1, $2$3$4} from "$5";' },
        { search: /respond \((\w+)\);/g, replace: 'respond ({$1});'},
        { search: /store \((\w+)\);/g, replace: 'store ({$1});'}



      ]
}