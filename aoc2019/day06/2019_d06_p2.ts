/* jshint esversion: 9 */

const fs = require('fs');
const R = require('ramda');
const math = require('mathjs');

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

        // matrix is symetrical
        p.subset(math.index(aIndex, bIndex), 1);
        p.subset(math.index(bIndex, aIndex), 1);
    }

    const size: number = R.keys(o).length;

    let p: Matrix = math.zeros(size, size, 'sparse');

    R.forEach(f, m);

    return p;
};

/**
 * compute all ajdacency matrices
 * (a^1, a^2, a^3, ..., a^{count})
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
        results.push(math.multiply(R.head(results), R.last(results)));

        return multMatrixNTimes(results, count - 1);
    }
};

/**
 * create the reachability matrix for given adjacency matrices
 * 
 * @param a list of adjacency matrices
 * @returns the reachability matrix
 */
const reachable = (a: Matrix[]): Matrix =>
{
    const add = (acc: Matrix, cur: Matrix): Matrix => math.add(acc, cur);

    const matrixSize: number = R.head(R.head(a).size());
    const identityMatrix: Matrix = math.identity(matrixSize, 'sparse');

    return R
        // add all matrices and the identity matrix together
        .reduce(add, identityMatrix, a)
        // change any nonzero to one
        .map((x: number): number => x === 0 ? 0 : 1);
};

/**
 * get value at [a, b] in matrix c
 * 
 * @param a first index
 * @param b second index
 * @param c data
 */
const getValueAt = (a: number, b: number, c: Matrix): number =>
    math.subset(c, math.index(a, b));


// input
// const p: string = fs.readFileSync("input.txt", "utf-8");
const p: string = fs.readFileSync("input_ex.txt", "utf-8");

// array of strings, remove empty lines
const orbitalMap: string[] =
    p.split("\n").filter((x:string) => x.length !== 0);

// list of uniqe masses
const masses: Object = fillMasses(orbitalMap);

// indices of santa and me in masses
const santaIndex: number = R.prop('SAN', masses);
const youIndex: number = R.prop('YOU', masses);

// build adjacencyMatrix
console.log("build adjacency matrix");
const adjacencyMatrix: Matrix = createMatrix(orbitalMap, masses);

// check all matrices
console.log("checking all reachability matrices");
for(let i = 1; i <= R.keys(masses).length; i++)
{
    let a = reachable(multMatrixNTimes([adjacencyMatrix], i));
    let pathLength = getValueAt(santaIndex, youIndex, a);

    if (pathLength != 0)
    {
        // output
        console.log("Shortest path from 'YOU' to 'SAN': ", i - 2);
        i = R.keys(masses).length;
    }
};
