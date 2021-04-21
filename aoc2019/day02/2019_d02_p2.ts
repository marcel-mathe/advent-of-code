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
    let tmp = [...p];

    for (let pc: number = 0; pc < p.length; pc += 4)
    {
        /* find opcode */
        const curr_op: number = p[pc];

        /* execute operation */
        switch(curr_op)
        {
            case 1:
                tmp = op_add(p[pc + 1], p[pc + 2], p[pc + 3], p);
                break;
            case 2:
                tmp = op_mul(p[pc + 1], p[pc + 2], p[pc + 3], p);
                break;
            case 99:
                pc = p.length;
                break;
            default:
                console.error("iiks");
                break;
        }

    }

    return tmp;
};

/*
 * file input,
 * single line of numbers, separated by commas
 */
const p: string = fs.readFileSync("input.txt", "utf-8");

/* array of strings */
const pr: string[] = p.split(",");

/* array of numbers */
const program: number[] = pr.filter(x => x.length !== 0).map(Number)

/* answer of 100 times noun + verb */
let answer: number = -1;

/* test for result */
for(let noun = 0; noun < 99; noun++)
{
    let tmp_program: number[] = [...program];
    tmp_program[1] = noun;

    for (let verb = 0; verb < 99; verb++)
    {
        tmp_program[2] = verb;

        let result: number = execute(tmp_program)[0];

        if (result === 19690720)
        {
            answer = (100 * noun) + verb;
            noun = 99;
            verb = 99;
        }
    }
}

console.log(answer);

