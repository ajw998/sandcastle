const assert = require('assert');

const DIR_MAP = new Map([
    [ 'N', [ 0, 1 ] ],
    [ 'S', [ 0, -1 ] ],
    [ 'E', [ 1, 0 ] ],
    [ 'W', [ -1, 0 ] ]
])

const splitDirection = (directions) => directions.split(' ');

const splitDiagonalDirection = (direction) => direction.split('')

const resolveDiagonalDirection = (direction) => {
    const splitDir = splitDiagonalDirection(direction);
    const x = DIR_MAP.get(splitDir[0])[0] | DIR_MAP.get(splitDir[1])[0];
    const y = DIR_MAP.get(splitDir[0])[1] | DIR_MAP.get(splitDir[1])[1];
    return [x, y]
}

const calculate = (directions) => {
    const dirArray = splitDirection(directions);	
    let x = 0
    let y = 0;

    dirArray.forEach((direction) => {
        if (direction.length > 1) {
            const resolve = resolveDiagonalDirection(direction);
            x += resolve[0];
            y += resolve[1];
        } else {
            x += DIR_MAP.get(direction)[0];
            y += DIR_MAP.get(direction)[1];
        }
    })

    return { x, y }
}

assert.deepEqual(calculate('N E S W'), { x: 0, y: 0 })
assert.deepEqual(calculate('N E N E N E N E'), { x: 4, y: 4 })
assert.deepEqual(calculate('NE'), { x: 1, y: 1 })
assert.deepEqual(calculate('EN'), { x: 1, y: 1 })
