/* jshint esversion: 9 */

const R = require('ramda');

type adress = number;
export type program = number[];
type twoOpFunction = (arg1: number, arg2: number) => number;
type oneOpPredicate = (arg1: number) => boolean;
type twoOpPredicate = (arg1: number, arg2: number) => boolean;

/* supported debug modes */
export enum DebugMode {
    None = 0,
	Tests,
	Trace
};

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

// interface for the overloaded constructor
// see: https://stackoverflow.com/questions/12702548/constructor-overload-in-typescript
interface IintCode
{
    p: program;
    d: DebugMode;
}

// IntCode Compiler
export class IntCode
{
    // fields
    private prog: program;
    private debug: DebugMode;
    private input: number[];
    private output: number[];

    // constructor
    constructor(obj?: IintCode)
    {
        this.prog = obj && obj.p || [];
        this.debug = obj && obj.d || DebugMode.None;
        this.input = [];
        this.output = [];
    }

    // methods

    /**
    * compute the parameter modes for a given parameter block
    *
    * @param params the number representing the modes 
    * @returns parameter modes for two arguments 
    */
    private computeParams = (params: number): ParameterMode[] =>
    {
        const prefix = "[debug][computeParams] ";
        const p1: ParameterMode = params % 10;
        const p2: ParameterMode = Math.floor(params / 10) % 10;

        if (this.debug >= DebugMode.Trace)
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
    private computeArguments =
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
        
        if (this.debug >= DebugMode.Trace)
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

        if (this.debug >= DebugMode.Trace)
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
    private op_some =
    (
        pc: adress,
        params: number,
        func: twoOpFunction,
        p: program
    ): program =>
    {
        const prefix = "[debug][op_some]";
        
        const t: program = [...p];

        const [parameter1, parameter2] = this.computeParams(params);

        const [operand1, operand2] =
        this.computeArguments(parameter1, parameter2, pc, p);

        /* parameter 3 always position mode */
        const parameter3: ParameterMode = ParameterMode.Position;
        const target: adress = p[pc + 3];

        if (this.debug >= DebugMode.Trace)
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

        if (this.debug >= DebugMode.Trace)
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
    private add = (a: number, b: number): number => a + b;

    /**
     * opcode addition
     *
     * @param pc current program counter
     * @param parameter parameter mode block for the arguments 
     * @param p the current program
     * @returns new program
     */
    private op_add = (pc: number, parameter: number, p: program): program =>
        this.op_some(pc, parameter, this.add, p);

    /**
     * multiply two numbers
     *
     * @param a some number 
     * @param b some number 
     * @returns the product of a times b 
     */
    private multiply = (a: number, b: number): number => a * b;

    /**
     * opcode multiplication
     *
     * @param pc current program counter
     * @param parameter parameter mode block for the arguments 
     * @param p the current program
     * @returns new program
     */
    private op_mul = (pc: number, parameter: number, p: program): program =>
        this.op_some(pc, parameter, this.multiply, p);

    /**
     * write value to adress
     *
     * @param adr the target adress
     * @param val the value to write
     * @param p the current program
     * @returns new program
     */
    private write_to_adress = (adr: adress, val: number, p: program): program =>
        R.adjust(adr, (_: number) => val, p);

    /**
     * read value from pc+1 and write it to console
     *
     * @param pc current program counter 
     * @param parameter the ParameterMode block 
     * @param p the current program
     * @return new program
     */
    private op_out = (pc: number, parameter: number, p: program): program =>
    {
        const prefix = "[debug][op_out] ";

        if (this.debug >= DebugMode.Trace)
        {
            console.log(prefix, "parameter: ", parameter);
            console.log(prefix, "parameter mode: ", ParameterMode[parameter]);
        }

        if (parameter === ParameterMode.Position)
        {
            const argument = p[p[pc + 1]];

            if (this.debug >= DebugMode.Trace)
            {
                console.log(prefix, "source adress: ", p[pc + 1]);
                console.log(prefix, "source value: ", p[p[pc + 1]]);
                console.log(prefix, "argument: ", argument);
            }

            this.output.push(argument);
        }
        else
        {
            const argument = p[pc + 1];

            if (this.debug >= DebugMode.Trace)
            {
                console.log(prefix, "argument: ", argument);
            }

            this.output.push(argument);
        }

        if (this.debug >= DebugMode.Trace) { console.log();}

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
    private op_sto = (pc: adress, parameter: number, p: program): program =>
    {
        const prefix: string = "[debug][op_sto] ";
        const adr: number = p[pc + 1];
        const val: number = this.getInput();

        if (this.debug >= DebugMode.Trace)
        {
            console.log(prefix, "adress: ", adr);
            console.log(prefix, "value: ", val);
            console.log(prefix, "adress value (before): ", p[adr]);
        }

        const t: program = this.write_to_adress(adr, val, p);

        if (this.debug >= DebugMode.Trace)
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
    private op_compare =
    (
        pc: adress,
        params: number,
        func: twoOpPredicate,
        p: program
    ): program =>
    {
        const prefix: string = "[debug][op_compare] ";

        const [parameter1, parameter2] = this.computeParams(params);

        const [argument1, argument2] =
        this.computeArguments(parameter1, parameter2, pc, p);

        const value: number = func(argument1, argument2) ? 1 : 0;

        /* always position mode */
        const target: number = p[pc + 3];

        if (this.debug >= DebugMode.Trace)
        {
            console.log(prefix, "argument1: ", argument1);
            console.log(prefix, "argument1 mode", ParameterMode[parameter1]);
            console.log(prefix, "argument2: ", argument2);
            console.log(prefix, "argument2 mode", ParameterMode[parameter2]);
            console.log(prefix, "target: ", target);
            console.log(prefix, "target value (before): ", p[target]);
            console.log(prefix, "value: ", value);
        }

        let t: program = this.write_to_adress(target, value, p);

        if (this.debug >= DebugMode.Trace)
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
    private op_lth = (pc: adress, parameter: number, p: program): program =>
    {
        if (this.debug >= DebugMode.Trace)
        {
            const prefix = "[debug][op_lth] ";
            console.log(prefix, "called");
            console.log();
        }

        return this.op_compare(pc, parameter, R.lt, p);
    }

    /*
    * opcode equal
    *
    * @param pc current program counter 
    * @param parameter the ParameterMode block 
    * @param p the current program
    * @returns new program
    */
    private op_equ = (pc: adress, parameter: number, p: program): program =>
    {
        if (this.debug >= DebugMode.Trace)
        {
            const prefix = "[debug][op_equ] ";
            console.log(prefix, "called");
            console.log();
        }

        return this.op_compare(pc, parameter, R.equals, p);
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
    private op_jumps =
    (
        pc: adress,
        params: number,
        func: oneOpPredicate,
        p: program
    ): number =>
    {
        const prefix = "[debug][op_jumps] ";

        const [parameter1, parameter2]: number[] = this.computeParams(params);
    
        const [argument1, argument2] =
        this.computeArguments(parameter1, parameter2, pc, p);

        /* opcode + 2 arguments */
        let new_pc = pc + 3;

        if (this.debug >= DebugMode.Trace)
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

        if (this.debug >= DebugMode.Trace)
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

        if (this.debug >= DebugMode.Trace)
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
    private notZero = (arg1: number): boolean => !this.isZero(arg1);

    /**
     * opcode jit
     * jump to second argument if first argument is non-zero
     *
     * @param pc current program counter 
     * @param parameter the ParameterMode block 
     * @param p the current program
     * @return new program counter 
     */
    private op_jit = (pc: adress, params: number, p: program): number =>
    {
        if (this.debug >= DebugMode.Trace)
        {
            const prefix = "[debug][op_jit] ";
            console.log(prefix, "called");
            console.log();
        }

        return this.op_jumps(pc, params, this.notZero, p);
    }

    /**
     * is arg1 zero?
     *
     * @param arg0 a number 
     * @returns true if arg0 is zero, false otherwise 
     */
    private isZero = (arg1: number): boolean => arg1 === 0;

    /**
     * opcode jif
     * jump to second argument if first argument is zero 
     *
     * @param pc current program counter 
     * @param parameter the ParameterMode block 
     * @param p the current program
     * @return new program counter 
     */
    private op_jif = (pc: adress, params: number, p: program): number =>
    {
        if (this.debug >= DebugMode.Trace)
        {
            const prefix = "[debug][op_jif] ";
            console.log(prefix, "called");
            console.log();
        }

        return this.op_jumps(pc, params, this.isZero, p);
    }

    /**
     * execute a given intcode-program
     *
     * @param p program
     * @returns the resulting program
     */
    private execute = (p: program): program =>
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
            
            if (this.debug >= DebugMode.Trace)
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
                    tmp = this.op_add(pc, params, tmp);
                    pc += 4;
                    break;
                case Opcode.MUL:
                    tmp = this.op_mul(pc, params, tmp);
                    pc += 4;
                    break;
                case Opcode.STO:
                    tmp = this.op_sto(pc, params, tmp);
                    pc += 2;
                    break;
                case Opcode.OUT:
                    tmp = this.op_out(pc, params, tmp);
                    pc += 2;
                    break;
                case Opcode.JIT:
                    pc = this.op_jit(pc, params, tmp);
                    break;
                case Opcode.JIF:
                    pc = this.op_jif(pc, params, tmp);
                    break;
                case Opcode.LTH:
                    tmp = this.op_lth(pc, params, tmp);
                    pc += 4;
                    break;
                case Opcode.EQU:
                    tmp = this.op_equ(pc, params, tmp);
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

    /**
     * reset computer to a new program
     * 
     * @param p a new intcode program
     */
    public reset = (p: program): void =>
    {
        this.prog = p;
        this.input = [];
        this.output = [];
    }

    /**
     * set input to value
     */
    public setInput = (input: number[]): void => { this.input = input; };

    /**
     * get input from user
     */
    private getInput = (): number =>
    {
        const prefix = "[debug][getInput]";
        const retval = R.head(this.input);
        
        if (this.debug >= DebugMode.Trace)
        {
            console.log(prefix, "input (before): ", this.input);
            console.log(prefix, "retval: ", retval);
        }

        this.input = R.tail(this.input);

        if (this.debug >= DebugMode.Trace)
        {
            console.log(prefix, "input (after): ", this.input);
            console.log();
        }

        return retval;
    }

    /**
     * return output of program
     */
    public getOutput = (): number[] => this.output;

    /* run program */
    public run = (): void => { this.execute(this.prog); };
}; 
