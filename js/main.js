const readline = require('readline');
const acorn = require('acorn');
const astring = require('astring');

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: false
});

rl.on('line', input => {
    const req = JSON.parse(input);
    process.stdout.write(JSON.stringify((() => {
        switch (req.type) {
            case 'ParseNative': {
                try {
                    const parser = new acorn.Parser({}, req.value);
                    parser.nextToken();
                    const expr = parser.parseExpression();
                    if (parser.type === acorn.tokTypes.eof) {
                        return {
                            tag: 'ParseNativeSuccess',
                            contents: expr
                        };
                    }
                    const extraExpr = parser.parseExpression();
                    return {
                        tag: 'ParseNativeError',
                        contents: [
                            'Unexpected input after parsing expression: '
                                + astring.generate(extraExpr),
                            acorn.getLineInfo(parser.input, parser.pos)
                        ]
                    };
                } catch (e) {
                    return {
                        tag: 'ParseNativeError',
                        contents: [e.message, e.loc]
                    };
                }
            }
            case 'Render':
                return astring.generate(req.value);
        }
    })()) + '\n');
});
