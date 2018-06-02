
import * as process from 'process';
import * as fs from 'fs';

import {importScript} from './parse_js';

function encode(filename: string) {
    const data: string = fs.readFileSync(filename, "utf8");
    const json: any = importScript(data);
    // return Script.lift(json);
    return json;
}

function main() {
    if (process.argv.length < 2) {
        console.error("Filename not given.");
        process.exit(1);
    }

    const fn = process.argv[1];
    console.log(JSON.stringify(encode(fn), null, 2));
}

main();
