/* jshint esversion: 9 */

const fs = require('fs');
const R = require('ramda');

/* supported debug modes */
enum DebugMode {
    None = 0,
	Tests,
	Trace
};

const DEBUG: DebugMode = DebugMode.None;

type adress = number;
type program = number[];
type twoOpFunction = (arg1: number, arg2: number) => number;
type oneOpPredicate = (arg1: number) => boolean;
type twoOpPredicate = (arg1: number, arg2: number) => boolean;

/* the opcodes our intcode-computer does understand */
enum Opcode {
    ADD = 1,
    MUL = 2,
    STO = 3,
    OUT = 4,
    JIT = 5,
    JIF = 6,
    LTH = 7,
    EQU = 8,
    END = 99
};

/*
 * the two supported addressing modes
 *
 * Position: the argument is an address
 * Immediate: the argument is an value
 */
enum ParameterMode {
    Position = 0,
    Immediate,
};

/**
 * compute the parameter modes for a given parameter block
 *
 * @param params the number representing the modes 
 * @returns parameter modes for two arguments 
 */
const computeParams = (params: number): ParameterMode[] =>
{
    const prefix = "[debug][computeParams] ";
    const p1: ParameterMode = params % 10;
    const p2: ParameterMode = Math.floor(params / 10) % 10;

    if (DEBUG >= DebugMode.Trace)
    {
      console.log(prefix, "parameter block: ", params);
      console.log(prefix, "parameter 1: ", p1);
      console.log(prefix, "parameter 1 mode: ", ParameterMode[p1]);
      console.log(prefix, "parameter 2: ", p2);
      console.log(prefix, "parameter 2 mode: ", ParameterMode[p2]);
      console.log();
    }

    return [p1, p2];
};

/**
 * compute arguments for a given pc and parameter modes 
 *
 * @param param1 ParameterMode for argument 1 
 * @param param2 ParameterMode for argument 2 
 * @param pc the current program counter 
 * @param p the current program 
 * @returns the values for the arguments
 * */
const computeArguments =
(
  param1: ParameterMode,
  param2: ParameterMode,
  pc: adress,
  p: program
): number[] =>
{
    const prefix = "[debug][computeArguments] ";

    /* parameter 1 */
    let argument1: number;
	
    if (DEBUG >= DebugMode.Trace)
    {		
        console.log(prefix, "param 1: ", param1);
        console.log(prefix, "param 1 mode: ", ParameterMode[param1]);
		console.log(prefix, "param 2: ", param2);
        console.log(prefix, "param 2 mode: ", ParameterMode[param2]);
    }
    
    if (param1 === ParameterMode.Position)
    {
        argument1 = p[p[pc + 1]];
    }
    else
    {
        argument1 = p[pc + 1];
    }
    
    /* parameter 2 */
    let argument2: number;

    if (param2 === ParameterMode.Position)
    {
        argument2 = p[p[pc + 2]];
    }
    else
    {
        argument2 = p[pc + 2];
    }

    if (DEBUG >= DebugMode.Trace)
    {
        console.log(prefix, "return value ", argument1, argument2);
        console.log();
    }

    return [argument1, argument2];
};

/**
 * exec some function on two values
 * now supports position and immediate mode
 *
 * @param pc the current program counter
 * @param params the parameter mode block
 * @param func the function we want to execute on our two arguments
 * @param p the current program
 * @returns new program
 */
const op_some =
(
    pc: adress,
    params: number,
    func: twoOpFunction,
    p: program
): program =>
{
    const prefix = "[debug][op_some]";
    
    const t: program = [...p];

    const [parameter1, parameter2] = computeParams(params);

    const [operand1, operand2] =
      computeArguments(parameter1, parameter2, pc, p);

    /* parameter 3 always position mode */
    const parameter3: ParameterMode = ParameterMode.Position;
    const target: adress = p[pc + 3];

    if (DEBUG >= DebugMode.Trace)
    {
        console.log(prefix, " operand1: ", operand1);
        console.log(prefix, " modus: ", ParameterMode[parameter1]);
        console.log(prefix, " operand2: ", operand2);
        console.log(prefix, " modus: ", ParameterMode[parameter2]);
        console.log(prefix, " target: ", target);
        console.log(prefix, " target value (before): ", t[target]);
    }

    /* execute */
    t[target] = func(operand1, operand2);

    if (DEBUG >= DebugMode.Trace)
    {
        console.log(prefix, " target value (after): ", t[target]);
        console.log();
    }

    return t;
};

/**
 * add two numbers 
 *
 * @param a some number
 * @param b some number 
 * @returns the sum of a and b 
 */
const add = (a: number, b: number): number => a + b;

/**
 * opcode addition
 *
 * @param pc current program counter
 * @param parameter parameter mode block for the arguments 
 * @param p the current program
 * @returns new program
 */
const op_add = (pc: number, parameter: number, p: program): program =>
    op_some(pc, parameter, add, p);

/**
 * multiply two numbers
 *
 * @param a some number 
 * @param b some number 
 * @returns the product of a times b 
 */
const multiply = (a: number, b: number): number => a * b;

/**
 * opcode multiplication
 *
 * @param pc current program counter
 * @param parameter parameter mode block for the arguments 
 * @param p the current program
 * @returns new program
 */
const op_mul = (pc: number, parameter: number, p: program): program =>
    op_some(pc, parameter, multiply, p);

/**
 * write value to adress
 *
 * @param adr the target adress
 * @param val the value to write
 * @param p the current program
 * @returns new program
 */
const write_to_adress = (adr: adress, val: number, p: program): program =>
  R.adjust(adr, (_: number) => val, p);

/**
 * read value from pc+1 and write it to console
 *
 * @param pc current program counter 
 * @param parameter the ParameterMode block 
 * @param p the current program
 * @return new program
 */
const op_out = (pc: number, parameter: number, p: program): program =>
{
    const prefix = "[debug][op_read] ";

    if (DEBUG >= DebugMode.Trace)
	{
		console.log(prefix, "parameter: ", parameter);
		console.log(prefix, "parameter mode: ", ParameterMode[parameter]);
	}

    if (parameter === ParameterMode.Position)
    {
        const argument = p[p[pc + 1]];

        if (DEBUG >= DebugMode.Trace)
        {
          console.log(prefix, "source adress: ", p[pc + 1]);
          console.log(prefix, "source value: ", p[p[pc + 1]]);
          console.log(prefix, "argument: ", argument);
        }

        console.log("[output]: ", argument);
    }
    else
    {
        const argument = p[pc + 1];

        if (DEBUG >= DebugMode.Trace) { console.log(prefix, "argument: ", argument); }

        console.log("[output]: ", argument);
    }

    if (DEBUG >= DebugMode.Trace) { console.log();}

    return p;
}

/**
 * store values
 *
 * @param pc current program counter
 * @param parameter parameter mode block for the arguments 
 * @param p the current program
 * @returns new program
 * @return new program
 */
const op_sto = (pc: adress, parameter: number, p: program): program =>
{
    const prefix: string = "[debug][op_sto] ";
    const adr: number = p[pc + 1];
    const val: number = getInput();

    if (DEBUG >= DebugMode.Trace)
    {
        console.log(prefix, "adress: ", adr);
        console.log(prefix, "value: ", val);
        console.log(prefix, "adress value (before): ", p[adr]);
    }

    let t: program = write_to_adress(adr, val, p);

    if (DEBUG >= DebugMode.Trace)
    {
        console.log(prefix, "adress value (after): ", t[adr]);
        console.log();
    }

    return t;
}

/*
 * compare pc+1 and pc+2 with func 
 *
 * @param pc the current program pointer
 * @param func the actual comparing predicate
 * @param p the current program 
 * @returns a new program 
 */
const op_compare =
(
  pc: adress,
  params: number,
  func: twoOpPredicate,
  p: program
): program =>
{
    const prefix: string = "[debug][op_compare] ";

    const [parameter1, parameter2] = computeParams(params);

    const [argument1, argument2] =
      computeArguments(parameter1, parameter2, pc, p);

    const value: number = func(argument1, argument2) ? 1 : 0;

    /* always position mode */
    const target: number = p[pc + 3];

    if (DEBUG >= DebugMode.Trace)
    {
        console.log(prefix, "argument1: ", argument1);
        console.log(prefix, "argument1 mode", ParameterMode[parameter1]);
        console.log(prefix, "argument2: ", argument2);
        console.log(prefix, "argument2 mode", ParameterMode[parameter2]);
        console.log(prefix, "target: ", target);
        console.log(prefix, "target value (before): ", p[target]);
        console.log(prefix, "value: ", value);
    }

    let t: program = write_to_adress(target, value, p);

    if (DEBUG >= DebugMode.Trace)
    {
      console.log(prefix, "target value (after): ", t[t[pc + 3]]);
      console.log();
    }

    return t;
};

/*
 * opcode less-than
 *
 * @param pc current program counter 
 * @param parameter the ParameterMode block 
 * @param p the current program
 * @returns new program
 */
const op_lth = (pc: adress, parameter: number, p: program): program =>
{
  if (DEBUG >= DebugMode.Trace)
  {
    const prefix = "[debug][op_lth] ";
    console.log(prefix, "called");
    console.log();
  }

  return op_compare(pc, parameter, R.lt, p);
}

/*
 * opcode equal
 *
 * @param pc current program counter 
 * @param parameter the ParameterMode block 
 * @param p the current program
 * @returns new program
 */
const op_equ = (pc: adress, parameter: number, p: program): program =>
{
  if (DEBUG >= DebugMode.Trace)
  {
    const prefix = "[debug][op_equ] ";
    console.log(prefix, "called");
    console.log();
  }

  return op_compare(pc, parameter, R.equals, p);
}

/*
 * set pc to value of pc+2 if func(pc+1) equals true 
 *
 * @param pc current program counter 
 * @param parameter the ParameterMode block 
 * @param func predicate to check pc+1
 * @param p the current program
 * @returns new program counter
 */
const op_jumps =
(
  pc: adress,
  params: number,
  func: oneOpPredicate,
  p: program
): number =>
{
    const prefix = "[debug][op_jumps] ";

    const [parameter1, parameter2]: number[] = computeParams(params);
  
    const [argument1, argument2] =
      computeArguments(parameter1, parameter2, pc, p);

    /* opcode + 2 arguments */
    let new_pc = pc + 3;

    if (DEBUG >= DebugMode.Trace)
    {
        console.log(prefix, "pc (before): ", pc);

        if (parameter1 === ParameterMode.Position)
        {
            console.log(prefix, "argument 1 (as adress): ", argument1);
        }
        else
        {
            console.log(prefix, "argument 1 (as value): ", argument1);
        }

        console.log(prefix, "mode 1: ", ParameterMode[parameter1]);
        
        if(parameter2 === ParameterMode.Position)
        {
            console.log(prefix, "argument 2 (as adress): ", argument2);
        }
        else
        {
            console.log(prefix, "argument 2 (as value): ", argument2);
        }

        console.log(prefix, "mode 2: ", ParameterMode[parameter2]);
    };

    let result: boolean = func(argument1);

    if (DEBUG >= DebugMode.Trace)
    {
        console.log(prefix, "result of func on arg1: ", result);
    }

    if (result)
    {
        /*
         * we do not have to check for switch modes,
         * we already have the correct value
         */
        new_pc = argument2;
    }

    if (DEBUG >= DebugMode.Trace)
    {
        console.log(prefix, "pc (after): ", new_pc);
        console.log();
    };

    return new_pc;
};

/**
 * is arg1 not zero?
 *
 * @param arg1 a number 
 * @returns true if arg1 is not zero, false otherwise
 */
const notZero = (arg1: number): boolean => !isZero(arg1);

/**
 * opcode jit
 * jump to second argument if first argument is non-zero
 *
 * @param pc current program counter 
 * @param parameter the ParameterMode block 
 * @param p the current program
 * @return new program counter 
 */
const op_jit = (pc: adress, params: number, p: program): number =>
{
  if (DEBUG >= DebugMode.Trace)
  {
    const prefix = "[debug][op_jit] ";
    console.log(prefix, "called");
    console.log();
  }

  return op_jumps(pc, params, notZero, p);
}

/**
 * is arg1 zero?
 *
 * @param arg0 a number 
 * @returns true if arg0 is zero, false otherwise 
 */
const isZero = (arg1: number): boolean => arg1 === 0;

/**
 * opcode jif
 * jump to second argument if first argument is zero 
 *
 * @param pc current program counter 
 * @param parameter the ParameterMode block 
 * @param p the current program
 * @return new program counter 
 */
const op_jif = (pc: adress, params: number, p: program): number =>
{
  if (DEBUG >= DebugMode.Trace)
  {
    const prefix = "[debug][op_jif] ";
    console.log(prefix, "called");
    console.log();
  }

  return op_jumps(pc, params, isZero, p);
}

/**
 * execute a given intcode-program
 *
 * @param p program
 * @returns the resulting program
 */
const execute = (p: program): program =>
{
    const prefix: string = "[debug][execute] ";
    let tmp: program = [...p];
    let pc: number = 0;
 
    do
    {
        /* find opcode */
        const op_word: number = tmp[pc];
        const curr_op: number = op_word % 100;

        /* find parameters */
        const params: number = Math.floor(op_word / 100);
		
        if (DEBUG >= DebugMode.Trace)
        {
            console.log(prefix, "pc :", pc);
            console.log(prefix, "op_word: ", op_word);
            console.log(prefix, "current opcode: ", Opcode[curr_op]);
            console.log(prefix, "parameter: ", params);
            console.log();
        }

        /* execute operation */
        switch(curr_op)
        {
            case Opcode.ADD:
                tmp = op_add(pc, params, tmp);
                pc += 4;
                break;
            case Opcode.MUL:
                tmp = op_mul(pc, params, tmp);
                pc += 4;
                break;
            case Opcode.STO:
                tmp = op_sto(pc, params, tmp);
                pc += 2;
                break;
            case Opcode.OUT:
                tmp = op_out(pc, params, tmp);
                pc += 2;
                break;
            case Opcode.JIT:
                pc = op_jit(pc, params, tmp);
                break;
            case Opcode.JIF:
                pc = op_jif(pc, params, tmp);
                break;
            case Opcode.LTH:
                tmp = op_lth(pc, params, tmp);
                pc += 4;
                break;
            case Opcode.EQU:
                tmp = op_equ(pc, params, tmp);
                pc += 4;
                break;
            case Opcode.END:
                pc = tmp.length;
                break;
            default:
                console.error("iiks at pos: ", pc);
                pc = tmp.length;
                break;
        }
    } while (pc < tmp.length)

    return tmp;
};

let getInput = (): number => -1;
let program: program;

if (DEBUG >= DebugMode.Tests)
{
    /* tests position mode */
    
    /* equal 8 */
    console.log("[test][position][equal 8]");
    console.log("[test][position][expected] 1 0");
    program = [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8];
    getInput = (): number => 8;
    execute(program);
    getInput = (): number => 5;
    execute(program);
 
    /* less than 8 */
    console.log("[test][position][less than 8]");
    console.log("[test][position][expected] 1 0");
    program = [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8];
    getInput = (): number => 5;
    execute(program);
    getInput = (): number => 8;
    execute(program);

    /* equal 0 */
    console.log("[test][position][equal 0]");
    console.log("[test][position][expected] 0 1");
    program = [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9];
    getInput = (): number => 0;
    execute(program);
    getInput = (): number => 8;
    execute(program);

    /* tests immediate mode */
    
    /* equal 8 */
    console.log("[test][immediate][equal 8]");
    console.log("[test][immediate][expected] 1 0");
    program = [3, 3, 1108, -1, 8, 3, 4, 3, 99];
    getInput = (): number => 8;
    execute(program);
    getInput = (): number => 5;
    execute(program);
    
    /* less than 8 */
    console.log("[test][immediate][less than 8]");
    console.log("[test][immediate][expected] 1 0")
    program = [3, 3, 1107, -1, 8, 3, 4, 3, 99];
    getInput = (): number => 5;
    execute(program);
    getInput = (): number => 8;
    execute(program);
    
    /* equal 0 */
    console.log("[test][position][equal 0]");
    console.log("[test][position][expected] 0 1");
    program = [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1];
    getInput = (): number => 0;
    execute(program);
    getInput = (): number => 8;
    execute(program);

    /* big test,
     * outputs  999 if input < 8,
     * outputs 1000 if input = 8,
     * outputs 1001 if input > 8
     */
    console.log("[test][big test]");
    console.log("[test][big test][expected] 999 1000 1001")
    program =
    [
      3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006,
      20, 31, 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20,
      1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4,
      20, 1105, 1, 46, 98, 99
    ];
    getInput = (): number => 5;
    execute(program);
    getInput = (): number => 8;
    execute(program);
    getInput = (): number => 9;
    execute(program);
	
	/* ccc_037 test
	 * output zero
	 */
	console.log("[test][ccc037]");
	console.log("[test][ccc037][expected] 0");
	program = [1, 0, 3, 3, 1005, 2, 10, 5, 1, 0, 4, 1, 99];
	execute(program);
	
	/* ccc_037 countdown
	 * counts down from 10 to 0 and exits
	 */
	console.log("[test][countdown]");
	console.log("[test][countdown][expected] 10 9 8 7 6 5 4 3 2 1 0");
	program = [101, -1, 7, 7, 4, 7, 1105, 11, 0, 99];
	execute(program);
	
	/*
	 * negative numbers
	 */
	console.log("[test][negative]");
	console.log("[test][negative][expected] 99")
	program = [1101, 100, -1, 5, 104, 0, 99];
	execute(program);
}

/* get input from user */
/* TODO: make a real version */
getInput = (): number => 5;

/*
 * file input,
 * single line of numbers, separated by commas
 */
const p: string = fs.readFileSync("input.txt", "utf-8");

/* array of strings */
const pr: string[] = p.split(",");

/* array of numbers */
program = pr.filter(x => x.length !== 0).map(Number)

/* result */
let res: program = execute(program);
