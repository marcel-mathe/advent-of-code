/* jshint esversion: 9 */

const fs = require("fs");

/**
 * amount of fuel for a given mass
 * @param mass the mass of a module
 * @returns needed fuel
 */
const fuel = (mass: number): number =>
    Math.floor((mass / 3)) - 2;

/**
 * amount of fuel for a given mass,
 * which then also needs fuel, and so on
 * @param f mass of fuel
 * @returns the extra fuel needed
 */
const recFuel = (f: number): number =>
{
    const f_fuel = fuel(f);

    return f_fuel <= 0 ? 0 : f_fuel + recFuel(f_fuel);
};

/**
 * compute the total amount of fuel needed for all of our modules
 * @param acc accumulator of fuel
 * @param mass mass of a module
 * @returns the fuel need for all modules combined
 */
const fuelReducer = (acc: number, mass: string): number =>
{
    let mass_fuel = fuel(Number(mass));
    let fuel_fuel = recFuel(mass_fuel);

    return acc + mass_fuel + fuel_fuel;
};

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
            .filter(notEmptyString)
            .reduce(fuelReducer, 0);

console.log(completeFuel);

