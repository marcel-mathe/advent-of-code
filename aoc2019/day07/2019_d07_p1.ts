/* jshint esversion: 9 */

const fs = require('fs');
const R = require('ramda');

import { program, DebugMode, IntCode } from "./intcode";

/**
 * permutate a given list with Heap's algorithm
 * 
 * @params k permutate the first k elements of a
 * @params a array of any type elements
 * @params results temporary store for the results
 * @returns list of permutations of a
 */
const heaps = (k: number, a: any[], results: any[] = []): any[] =>
{
    /* helper */
    const isEven = (x: number): boolean => x % 2 === 0;

    if (k === 1)
    {
        // put a copy of a into results
        results.push(a.slice());
    }
    else
    {
        heaps(k - 1, a, results);

        for (let i = 0; i < k - 1; i++)
        {
            if (isEven(k))
            {
                a = swap(i, k - 1, a);
            }
            else
            {
                a = swap(0, k - 1, a);
            }
            heaps(k - 1, a, results);
        }
    }

    return results;
};

/**
 * swap two elements in an arrray
 * 
 * @param index1 index of first element
 * @param index2 index of the second element
 * @param arr the array
 * @returns new array with the two elements swapped
 */
const swap = (index1: number, index2: number, arr: any[]): any[] =>
    R.addIndex(R.map)((val: any, idx: number): any => {
        if (idx === index1) return arr[index2];
        if (idx === index2) return arr[index1];
        return val;
    }, arr);

// possible phase values
const phase_settings: number[] = [0, 1, 2, 3, 4];

// permutation of all phase settings
const permutated_settings = heaps(phase_settings.length, phase_settings);

// für jede phase setting kombination
const amplifier_pipeline = (phase_setting: number[], prog: program): number =>
{
    // a single amplifier run
    const amplifier = (phase, value): number =>
    {
        comp.reset(prog);
        comp.setInput([phase, value]);
        comp.run();
        return R.head(comp.getOutput());
    }

    // init
    let comp = new IntCode({p: prog, d: DebugMode.None});
    let phase = R.head(phase_setting);
    let input = 0;

    // amplifier #1
    comp.setInput([phase, input]);
    comp.run();
    let amp1 = R.head(comp.getOutput());

    // amplifier #2
    let amp2 = amplifier(R.head(R.tail(phase_setting)), amp1);

    // amplifier #3
    let amp3 = amplifier(R.head(R.tail(R.tail(phase_setting))), amp2);
    
    // amplifier #4
    let amp4 = amplifier(R.head(R.tail(R.tail(R.tail(phase_setting)))), amp3);
    
    // amplifier #5
    let amp5 = amplifier(R.head(R.tail(R.tail(R.tail(R.tail(phase_setting))))), amp4);

    return amp5;
};

/*
 * file input,
 * single line of numbers, separated by commas
 */
const p: string = fs.readFileSync("input.txt", "utf-8");

/* array of strings */
const pr: string[] = p.split(",");

/* array of numbers */
const program = pr.filter(x => x.length !== 0).map(Number);

/* every thrust result */
const results =
    R.map(
        (x: number[]): number => amplifier_pipeline(x, program),
        permutated_settings);

/* highest value */
const highest_value: number = R.reduce(R.max, -Infinity, results);

/* output */
console.log(highest_value);
