## Assert statement

The Assert statement tests a condition. A false condition can send diagnostic information to the terminal output and audit trail, and/or it can result in cancellation of the SOUL request.

**Contents**
1. **Syntax**
    1.1 **Syntax terms**
    * **cond**: The conditions that are being asserted as true. These conditions have exactly the same syntax as conditions on IF statements.
    * **Snap**: Indicates a CCASNAP is to be taken on an assertion failure. A CCASNAP is taken in addition to, but before, any SirFact dump associated with the assertion failure.
    * **Info**: Extra information that is included in the audit trail and terminal output for the assertion failure as part of an MSIR.0494 message. If info is a %variable, the contents of the indicated variable is output. If info contains spaces or other Model 204 separator characters, it must be enclosed in quotes. If info is not a %variable and is not enclosed in quotes, the uppercased literal string is output. As of version 7.7, info can also be any valid SOUL expression, including a SOUL OO method call, $function, or even a local function. The expression, which must be enclosed in parentheses, is evaluated only in the case of an Assert failure.
    * **Continue**: Indicates that an assertion failure does not cause the request to be cancelled. An MSIR.0494 message, and possibly a SirFact dump, will still be produced, but the request continues.

2. **Examples**
    * Some valid Assert statements are shown.

3. **Usage notes**
    * An Assert statement uses the same expression handler as the If statement, so it is exactly as efficient as an If statement with the same conditions.
    * To use an Assert, simply place it before any code that depends on some assumptions about variables or the environment. Assert statements should be coded to test values or relationships that are required for the code to run correctly but whose values are not immediately apparent from the surrounding code. In addition to catching coding errors, the Assert statement provides the following benefits:
        * It makes clear to anyone scanning the program the assumptions on which the code depends. This makes it easier to understand the surrounding code. Similarly, it makes the environmental requirements clear to someone wanting to reuse a code fragment or call a common subroutine. While these benefits can be achieved with comments, the Assert statement has the added benefit that it enforces the restrictions.
        * It eliminates doubt when you are scanning code trying to debug a problem, and it prevents wasted time on "what if" scenarios that can be ruled out with a simple Assert.

4. **Discussion**
    * Programming errors often cause symptoms that point directly to the error. For example, an incorrectly coded array assignment might result in a subscript range error on the very statement with the error. Alternatively, an assignment from the wrong variable to a screen item results in incorrect data appearing in the corresponding screen field. These kinds of programming errors are generally easy to isolate and fix, and they are usually caught during adhoc debugging or fairly quickly after a program goes into production.
    * Yet many other programming errors can cause more subtle problems that cause completely unrelated statements to fail, or even cause corruption of data that might not be detected until long after the original error has occurred. This often happens because much code depends on assumptions about the current environment, including assumptions about values of variables. A coding error or a misunderstanding of the environmental requirements of a chunk of code can cause the code to be run with invalid data. The code may execute but produce invalid results, or perhaps it may set values incorrectly that can cause problems in yet another part of the code.
    * There are several ways to deal with this problem with assumptions:
        * Don't make assumptions in code. While it is an admirable goal to make code as flexible as possible, taken to the extreme, this approach produces code that is bloated by instructions to handle cases that never happen, setting return codes and status values that should never be set that then have to be checked elsewhere. Put another way, code has to do not only what is necessary but also has to perform many unnecessary tasks.
        * Ignore the problem and hope for the best.
        * Check key assumptions in code, and terminate the program with appropriate diagnostics to isolate the cause of the termination.
    * The last solution looks the most appealing. However, without the Assert statement, collecting the appropriate diagnostic information can only be done in User Language, so it can be tedious (numerous Audit, Print, or $Setg statements), and it still provides only limited information. The Sirius Mods provides the User Language Assert statement to get around these problems. The Assert statement serves three functions:
        * It tests the validity of an assumption.
        * It causes the current request to be cancelled if the assumption is incorrect. In an APSY subsystem, this causes transfer to the subsystem error procedure.
        * It indicates the procedure and line number containing the failing Assert statement. Furthermore, in the presence of appropriate SIRFACT MAXDUMP and SIRFACT DUMP settings, it causes the creation of a SirFact dump that contains a wide variety of information about the program environment at the time of the error.
    * Stated another way, the Assert statement allows testing of assumptions and extensive diagnostic data collection with a single, simple statement.
