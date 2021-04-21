/* jshint esversion: 9 */

const R = require('ramda');

const identity = (x: any): any => x;

const mapIndexed = R.addIndex(R.map);

const splitNumberIntoDigits = (num: number): number[] =>
    (num).toString().split("").map(x => parseInt(x));

const sixDigits = (num: number): boolean =>
    splitNumberIntoDigits(num).length === 6;

const lessOrEqualPair = (x: number[]): boolean =>
    x[0] <= x[1];

/* digits never decrease */
const decreasing = (num: number): boolean =>
    R.all(lessOrEqualPair, R.aperture(2, splitNumberIntoDigits(num)));

const equalPair = (x: number[]): boolean =>
    R.equals(x[0], x[1]);

/* input number has at least two adjacent equal digits */
const twoAdjacentEqualDigits = (num: number): boolean =>
    R.any(equalPair, R.aperture(2, splitNumberIntoDigits(num)));

const input: string = "307237-769058";
const input_arr: number[] = input.split('-').map(x => parseInt(x));
const numbers = R.range(input_arr[0], input_arr[1] + 1);

const f = R.compose(
    R.filter(sixDigits),
    R.filter(decreasing),
    R.filter(twoAdjacentEqualDigits));

const g: number[] = f(numbers);

console.log(g.length);

