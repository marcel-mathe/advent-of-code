/* jshint esversion: 9 */

const fs = require('fs');
const R = require('ramda');

/* intersection of two sets */
const intersection = (setA: Set<Point>, setB: Set<Point>): Set<Point> =>
{
	const a = [...setA];
	const b = [...setB];
	
	return R.intersection(a, b);
}

/* manhattan distance from origin */
const manhattan = (p: Point): number =>
    Math.abs(R.prop('x', p)) + Math.abs(R.prop('y', p));

/* sigma funktion for numbers */
const diff = (a: number, b: number): number => a - b;

/* convert directions to coord list */
const route = (route: string[]): Point[] =>
{
    let output: Point[] = [];
    let x: number = 0;
    let y: number = 0;

    for (let way of route)
    { 
        const direction: string = way[0];
		const length: string = way.substring(1);
		
        for (let i = 0; i < Number(length); i++)
        {
            switch (direction) {
                case "U":
                    y += 1;
                    break;
                case "D":
                    y -= 1;
                    break;
                case "L":
                    x -= 1;
                    break;
                case "R":
                    x += 1;
                    break;
                default:
                    console.error("iiks");
            }
            output.push({ "x": x, "y": y });
        }
    }

    return output;
};

/* alias */
type Point = { x: number, y: number };

/* file input, list routes, one per line */
console.log("reading input");
const text: string = fs.readFileSync("input.txt", "utf-8");

/* routes array */
const textByLine: string[] = text.split("\n");

/* path1 */
console.log("creating set1");
const path1: string = textByLine[0];
/* convert to array */
const path1arr: string[] = path1.split(",");
/* convert list of direction and length to list of points */
const points1: Point[] = route(path1arr);
/* convert list of points into a set of points */
const set1: Set<Point> = new Set(points1);

// same for path2
console.log("creating set2");
const path2: string = textByLine[1];
const path2arr: string[] = path2.split(",");
const points2: Point[] = route(path2arr);
const set2: Set<Point> = new Set(points2);

// intersect set1 and set2
// so we find the short circuits
console.log("calculating intersections");
const intersects: Set<Point> = intersection(set1, set2);

// distances of all intersections
console.log("calculating distances");
const distances = R.map(manhattan, Array.from(intersects));

console.log("distance of the closest intersection");
console.log(R.head(R.sort(diff, distances)));

