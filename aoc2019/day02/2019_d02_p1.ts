/* jshint esversion: 9 */

const fs = require("fs");

type opcode = 1 | 2 | 99;
type adress = number;
type intcode = [opcode, adress, adress, adress];
type twoOpFunction = (arg1: number, arg2: number) => number;

/* add */
const add = (a: number, b: number): number => a + b;

/* multiply */
const multiply = (a: number, b: number): number => a * b;

/**
 * exec some function on two values
 * @returns new program
 */
const op_something =
(
    op1: adress,
    op2: adress,
    res: adress,
    func: twoOpFunction,
    p: number[]
): number[] =>
{
    let t: number[] = [...p];
    t[res] = func(t[op1],t[op2]);
    return t;
};

/**
 * add values
 * @returns new program
 */
const op_add = (op1: adress, op2: adress, res: adress, p: number[]): number[] =>
    op_something(op1, op2, res, add, p)

/**
 * multiply values
 * @returns new program
 */
const op_mul = (op1: adress, op2: adress, res: adress, p: number[]): number[] =>
    op_something(op1, op2, res, multiply, p)

/**
 * execute a given intcode-program
 * @param p program
 */
const execute = (p: number[]): number[] =>
{
    for (let pc: number = 0; pc < p.length; pc += 4)
    {
        /* find opcode */
        let curr_op: number = p[pc];

        /* execute operation */
        switch(curr_op)
        {
            case 1:
                p = op_add(p[pc + 1], p[pc + 2], p[pc + 3], p);
                break;
            case 2:
                p = op_mul(p[pc + 1], p[pc + 2], p[pc + 3], p);
                break;
            case 99:
                pc = p.length;
                break;
            default:
                console.error("iiks");
                break;
        }

    }

    return p;
};

/* sample programs */
let onePlusOne = [1, 0, 0, 0, 99];
let threeTimesTwo = [2, 3, 0, 3, 99];
let nineNineTimesNineNine = [2, 4, 4, 5, 99, 0];
let complex = [1, 1, 1, 4, 99, 5, 6, 0, 99];

/*
 * file input,
 * single line of numbers, separated by commas
 */
const p: string = fs.readFileSync("input.txt", "utf-8");

/* array of strings */
const pr: string[] = p.split(",");

/* array of numbers */
const program: number[] = pr.filter(x => x.length !== 0).map(Number)

/* fix program */
program[1] = 12;
program[2] = 2;

/* run the program */
console.log(execute(program)[0]);

