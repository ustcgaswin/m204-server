## Customizing functions and translation tables

**Overview**

Custom functions are stored in the user function module (FUNU). The FUNU module is distributed on an order module containing a table for functions and arguments, and the programmer code for each function. You can call the custom functions using the translation tables to allow the use of both coded and long values of fields. The contents of the translation tables can be viewed in the *FUNU* module, and editing translations to *MSRU*.

**Adding functions to the FUNU module**

To add custom functions to the FUNU module before FUNU is assembled and filled into Model 204, you can use the function table in the FUNU source code with *DC*.

1.  Find the function name in the table. (e.g., *TYPE*).
2.  Enter the function name, type (e.g., *A*, *I*, *L*, *X*).
3.  Use entries for the function parameters.

*   When writing entries, ensure you do not exceed the character limit.

**Coding conventions**

The following conventions are required for coding a user function:

*   Begin the function code with
*   End the function code with

**String arguments**

If the argument type is *S*, you can obtain the argument as a string using the following order:

1.  *S0*, *S1*, *S2*
2.  *S3*, *S4*, *S5*
3.  *S6*, *S7*, *S8*
4.  *S9*, *S10*, *S11*
5.  *S12*, *S13*, *S14*
6.  *S15*, *S16*, *S17*
7.  *S18*, *S19*, *S20*

**Numeric arguments**

If the argument type is *I*, you can obtain the argument as a floating-point value or a binary integer.

**Using the ENTER macro to allocate working storage**

You can use the ENTER macro to allocate working storage for storing unique variables for each function. This avoids the possibility of accessing the same storage in errors.

**Coding requirements for all operating systems**

*   Change instructions related to the *KOMA* as follows:
*   Code the macro
*   Refer to the *KOMA* or *VOMA* service.

**Coding requirements for all operating systems**

*   Change instructions related to the *KOMA* as follows:
*   Code the macro
*   Refer to the *KOMA* or *VOMA* service.

**Modifying translation tables**

To make additions, modifications, or changes to translation tables:

1.  Identify the source code.
2.  Assemble the code.
3.  Link the translation tables to the appropriate operating system utility program.

**String array requirements**

Each entry of the *CTS* module depends on the table defined. If tables are very large, it might require a significant number of bytes.

*   *S* type: *String Length*, *number of bytes in string*

**Converting user-written functions**

User-written functions may need to be modified to accommodate the conversion process.

**Additional requirements for Model 204 processing**

*   Model 204 with *Bluestone*, an improved code. This order in writing in the *MSRU* might cause issues.
*   It is not always obvious if the code will run in *MSRU* or *FUNU*.

**Encoding details**

The Model 204 tables in the *CDS* module, which provides support in the *MSRU* module with coded and string values.

**CTS module**

*   Comments explaining the macro.
*   There are tables and code to match.

**Additional requirements for Model 204 processing**

*   Model 204 with *Bluestone*, an improved code. This order in writing in the *MSRU* might cause issues.
*   It is not always obvious if the code will run in *MSRU* or *FUNU*.