const readline = require('readline');
const { parseExpressionAt } = require('acorn');
const { generate } = require('astring');

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: false
});

rl.on('line', input => {
    const req = JSON.parse(input);
    process.stdout.write(JSON.stringify((() => {
        switch (req.type) {
            case 'ParseNative':
                try {
                    return {
                        tag: 'ParseNativeSuccess',
                        contents: parseExpressionAt(req.value)
                    };
                } catch (e) {
                    return {
                        tag: 'ParseNativeError',
                        contents: [e.message, e.loc]
                    };
                }
            case 'Render':
                return generate(req.value);
        }
    })()) + '\n');
});
