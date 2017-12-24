const readline = require('readline');
const { generate } = require('astring');

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: false
});

rl.on('line', input => {
    process.stdout.write(JSON.stringify(generate(JSON.parse(input))) + '\n');
});
