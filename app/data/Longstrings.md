## Longstrings

As of Model 204 version 7.0, Longstrings appear as a native Model 204 datatype and are defined in the same way as
normal variables.

**Name**

Longstrings are largely interchangeable with string variables, with the exception that a longstring can have a
larger number of bytes than a string variable.  A longstring variable is handled implicitly as a longstring.

**Use**

The value of a longstring variable can be determined on demand with its global function. However, a longstring variable
like other global functions in session-longstring also can be retrieved on the global level of a session, a session, and, if
possible, a global session.

**Longstrings** can be declared explicitly.

**Image Size**

The Longstring datatype is not supported inside images. However, image items with longstring greater than 255 bytes are not
supported.

**While such image data can have arbitrary lengths up to 255 bytes. At other Longstring variables, they exhibit the same
behavior.**

While it might be treated to receive subsidiary or all string like similar variables, there are five subtle
operations.

**Longstrings in expressions**

1. **Calculation with the `WITH` operator**
2. **Concatenation with the `||` operator**
3. **Comparisons**
4. **Concatenation and comparisons**
5. **Longstrings and functions**
6. **Longstrings and performance**
7. **Longstrings and `WITH` statements**

**One key difference** between a longstring and a regular string is the default behavior of longstring assignment. Two
assignments of a string variable and a longstring variable result in required considerations if the value of the assignment is
different from the declared longstring rule.

**You can successfully use** an intermediate assignment to a string variable called `Medium` to the
longstring variable.

**At result,** the size of the regular string can never largely exceed 255 bytes, and any assignment and operations to
longstring will be done in the same way as a regular string.

**Numeric conversion**

A longstring is automatically converted to a numeric datatype. A request-canceling function
with a result equal to 1.

**Comparisons**

Comparison operators such as `==`, `!=`, `>`, `<`, etc., will perform longstring comparisons of either operands.

**Longstrings and Substrings**

Longstrings can be used in functions. As mentioned before, if a longstring expression is assigned to a string
variable, the result requires conversion.  If a longstring was longer than 255 bytes, one way around this would be to use the
`substring` function.

**Longstring and Substrings**

The addition to the `Model 204` is the `longstring` type, specifically, `substring` and `replace` functions.

**Changing Longstring Function Behavior**

While it is sometimes convenient that Model 204 silently string data on assignment to a variable or intermediate
variable, since this can lead to confusion, it is better to explicitly handle longstrings with functions.

**Longstrings and Methods**

In addition to class methods, variables and as inputs or outputs to functions and complex subroutines, Longstrings
are all input parameters to both defined and system methods.

**Longstring Performance**

Because of the size of the variable, which are implemented in the `VTR` space, and is a potentially very large data
structure, operations on longstrings can be slower than operations on regular strings.

**Longstring with `WITH`**

