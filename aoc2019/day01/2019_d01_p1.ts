/* jshint esversion: 9 */

const fs = require("fs");

/**
 * amount of fuel for a given mass
 * @param mass the mass of a module
 * @returns needed fuel
 */
const moduleFuel = (mass: number): number =>
    Math.floor((mass / 3)) - 2;

/**
 * amount of fuel for a given mass,
 * which then also needs fuel, and so on
 * @param f mass of fuel
 * @returns the extra fuel needed
 */
const fuelReducer = (acc: number, x: string): number =>
    acc + moduleFuel(Number(x));

/**
 * is string not empty?
 * @param s a string
 * @returns true if length of string is not zero
 */
const notEmptyString = (s: string): boolean => s.length !== 0;

/* file input, list of module masses, one per line */
const text: string = fs.readFileSync("input.txt", "utf-8");

/* masses array */
const textByLine: string[] = text.split("\n");

/* complete fuel needed for all modules */
const completeFuel: number =
        textByLine
            .filter(emptyString)
            .reduce(fuelReducer, 0);

console.log(completeFuel);
