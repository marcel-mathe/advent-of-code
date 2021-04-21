/* jshint esversion: 9 */

const fs = require('fs');
const R = require('ramda');
const math = require('mathjs');

/**
 * summing a matrix
 * 
 * @param a two-dimensional matrix
 * @returns the sum of all entries
 */
const matrixSum = (a: Matrix): number =>
{
    let count: number = 0;

    a.map((val: number): number => count += val);

    return count;
};

/**
 * splits a map line "a)b" up to "[a, b]"
 * 
 * @param s input map line
 * @returns an string array of both masses names
 */
const splitMapLine = (s: string): string[] =>
    R.split(")", s);

/**
 * convert an orbital map into a dictionary of unique masses
 * 
 * @param m a list of map lines
 * @returns a dictionary with the mass name as key and
 * an uniqe number as value
 */
const fillMasses = (m: string[]): Object =>
{
    const forEachIndexed = R.addIndex(R.forEach);
    
    const addEntryToDictionary = (val: string, idx: number): void =>
    {
        // if entry already exists, take that
        dict = R.mergeLeft(dict, R.objOf(val)(idx));
    }

    let dict: Object = {};

    const uniqeMasses: string[] =
        R.uniq(R.flatten(R.map(splitMapLine, m)));

    forEachIndexed(addEntryToDictionary, uniqeMasses);

    return dict;
};

/**
 * creates an adjacency matrix for a given map
 * 
 * @param m orbital map
 * @param o dictionary of all unique masses
 * @returns the adjacency matrix for the given map
 */
const createMatrix = (m: string[], o: Object): Matrix =>
{
    const f = (m: string): void =>
    {
        const [a, b] = splitMapLine(m);
        const aIndex: number = R.prop(a, o);
        const bIndex: number = R.prop(b, o);

        p.subset(math.index(aIndex, bIndex), 1);
    }

    const size: number = R.keys(o).length;

    let p: Matrix = math.zeros(size, size, 'sparse');

    R.forEach(f, m);

    return p;
};

/**
 * compute all ajdacency matrices
 * (a^1, a^2, a^3, ..., a^c)
 * 
 * @param results list of results
 * @param count times multiply
 * @returns a list of all multiples of a
*/
const multMatrixNTimes = (results: Matrix[], count: number): Matrix[] =>
{
    if (count === 1) { return results; }

    if (count > 1)
    {
        let new_b: Matrix[] = [...results];
    
        new_b.push(math.multiply(R.head(results), R.last(results)));
        
        return multMatrixNTimes(new_b, count - 1);
    }
};

// input
const p: string = fs.readFileSync("input.txt", "utf-8");
// const p: string = fs.readFileSync("input_ex.txt", "utf-8");

// array of strings, remove empty lines
const orbitalMap: string[] =
    p.split("\n").filter((x:string) => x.length !== 0);

// list of uniqe masses
const masses: Object = fillMasses(orbitalMap);

// build adjacencyMatrix
const adjacencyMatrix: Matrix = createMatrix(orbitalMap, masses);

// all multiples of the adjacenyMatrix
const matrixTimesN: Matrix[] =
    multMatrixNTimes(
        [adjacencyMatrix],
        R.keys(masses).length);

// sum
const sumOfDirectAndIndirectPathsInOrbitMap: number =
    R.sum(R.map(matrixSum, matrixTimesN));

// output
console.log(
    "Sum of all direct and indirect paths: ",
    sumOfDirectAndIndirectPathsInOrbitMap);
