module.exports = {
    phrasesToSearch : [
        { search: /fetch (\w+)/g, replace: 'function FETCH$1' },
        { search: /fetch/g, replace: 'function FETCH$1' },
        { search: /process function (\w+)/g, replace: 'function PROCESS$1' },
        { search: /serve function (\w+)/g, replace: 'function SERVE$1' },
        { search: /request (\w+)/g, replace: 'function REQUEST$1' },
        { search: /process (\w+)/g, replace: 'function PROCESS$1' },
        { search: /import (contract) (\w+)\.(\w+)\.(\w+) from "([^"]+)";/g, replace: 'import {$1, $2$3$4} from "$5";' },
        { search: /import (config)(\w+)\ from "([^"]+)";/g, replace: 'import {$1 , $2} from "$3";' },
        { search: /import (\w+)\ (\w+)\ from "([^"]+)";/g, replace: 'import {$1 , $2} from "$3";' },
        { search: /import (contract) (\w+)\.(\w+)\.(\w+)\.(\w+) from "([^"]+)";/g, replace: 'import {$1, $2$3$4$5} from "$6";' },
        { search: /respond\s*\{\s*(\w+)\s*:\s*(\w+\.\w+)\s*\}\s*;/g, replace: 'respond ({$1: $2});'},
        { search: /respond\s*\{\s*(\w+)\s*\}\s*;/g, replace: 'respond ({$1});' },
        { search: /store\s+\{\s*(\w+)\s*\};/g, replace: 'store ({ $1 });'},
        { search: /(event: )(\w+)\.(\w+)\.(\w+)/g, replace: '$1$2$3$4'},
        { search: /config\s+Config\s*\{([\w\s:,;\n]+)\}/g, replace: 'const Config = {$1}'},
        { search: /respond\s+\{\s*([\w.]+):\s*([\w.]+),\s*([\w.]+):\s*([\w.]+)\s*\};/g,replace: 'respond ({ $1: $2, $3: $4 });'}


      ]
}