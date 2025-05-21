## SOUL macro facility

The SOUL macro facility consists of SOUL macro statements that allow code to be conditionally compiled. This facility is available to all Janus SOAP (and other Sirius API) customers, and as of version 7.5 to all Model 204 customers.

The facility's macro statements must have specific names and attributes stored in CCATEMP. They can be used with the evaluation of macro statements.

Macro variables such as the macro variables, can only have one of two attributes: defined or undefined.

All macro language clauses are implicitly terminated when the procedure in which they started is closed.

### Macro dummy strings

In any request that contains any macro language statements, macro dummy string substitution will be performed. Model 204 dummy strings are strings that begin with a question mark (?) and some other special character, either another ampersand (&). These are replaced as the lines are being read, with either an include parameter or a global variable value.

For example, in the following request, `Nsites is object recordset in file ?&HENGE`, the record object declaration `Nsites is object recordset in file` is replaced by the value of the global variable `HENGE`.

Macro dummy strings behave with the following exceptions:

*   They are indicated by an exclamation mark following the question mark, as in `?!henge`.
*   Their values come from the macro variable whose name is indicated in the dummy string.
*   Substitution is only performed during compilation.

### Handling ?! characters in program code

As with global variable dummy strings, macro variable dummy string names must be written completely in uppercase, even if the declaration of the variables used mixed case.

If a single `?!` pair is attempted in any program, macro dummy string substitution is enabled. If any old code in the program happened to contain a `?!` pair, a compilation error is likely.

### !DUPEXIT [var]

This statement conditionally closes the current procedure, and it conditionally closes the procedure if the macro variable is defined. If the macro variable is not defined, the variable is not defined.

It can be useful for preventing one-time-only definitions (of a variable or subroutine) from being performed more than once.

### !DEF var

The `!DEF` statement sets the indicated macro variable to defined. If the variable is already defined, it remains defined.

### !ELSEIFDEF var

The `!ELSEIFDEF` statement compiles the SOUL instructions that follow it if the indicated variable is defined, and if no `!DEF`, `!IFNDEF`, `!ELSEIFDEF`, or `!ELSEIFNDEF` statement at the same level was evaluated as true.

### !ELSEIFNDEF var

The `!ELSEIFNDEF` statement compiles the SOUL instructions that follow it if the indicated variable is not defined, and if no `!DEF`, `!IFNDEF`, `!ELSEIFDEF`, or `!ELSEIFNDEF` statement at the same level was evaluated as true.

### !IFDEF var

The `!IFDEF` statement compiles the SOUL instructions that follow it if the indicated variable is defined.

### !IFNDEF var

The `!IFNDEF` statement compiles the SOUL instructions that follow it if the indicated variable is undefined.

### !UNDEF var

The `!UNDEF` statement sets the indicated macro variable to undefined. If the variable is already undefined, it remains undefined.