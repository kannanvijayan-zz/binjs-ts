
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
    const args: Array<string> = process.argv.slice(2);
    if (args.length < 2) {
        console.error("Filename not given.");
        process.exit(1);
    }
    if (args[0] === '--encode') {
        console.log(`ENCODING: ${args[1]}`);
        console.log(JSON.stringify(encode(args[1]), null, 2));
    } else {
        console.error(`Unrecognized command: ${args[0]}`);
        process.exit(1);
    }
}

main();
