## SirFact SOUL statements

The SOUL statements discussed on this page can be of great value to SirFact users.

**Contents**
1. Assert statement
2. Trace statement
3. SirFact statement
    * 3.1 Syntax
    * 3.2 Usage
4. See also

**Assert statement**

The Assert statement tests an assumption, causes request cancellation if the assumption is incorrect, and indicates the procedure and line number containing the failing Assert. In the presence of appropriate SIRFACT MAXDUMP and SIRFACT DUMP settings, it causes the creation of a SirFact dump that contains a wide variety of information about the program environment at the time of the error.

**Trace statement**

The Trace statement returns the same output as a Print or Audit statement, but it lets you store that output in a "wrap-around" table in CCATEMP that you can examine with SirFact.

**SirFact statement**

The SirFact statement temporarily disables SirFact $function error trapping for a specific user. The SIRFACT CANCEL command makes it possible to trap return codes from $functions that are indicative of programming errors or severe environmental problems. For simplicity and consistency, the scope of SIRFACT CANCEL for a $function is global: it applies to every program running on every thread in the Online. This should not be a problem because SIRFACT CANCEL should only be used to trap severe problems. However, even the most unlikely return codes might be handled by SOUL code in certain odd instances. For these cases, the SirFact SOUL statement is provided to temporarily disable SirFact error trapping for all $functions or FRN errors.

**Syntax**

SirFact On | Off

**Where:**

On | Off  - On indicates that SirFact $function error trapping is to be reenabled; Off indicates that SirFact $function error trapping is to be temporarily disabled for the current thread. If no SirFact statement is present in a SOUL program, SirFact $function error trapping is always enabled. The end of a SOUL request automatically re-enables SirFact $function error trapping. That is, a Sirfact Off only applies to the program in which it is executed.

**Usage**

* The SirFact Off and SirFact On statements are evaluated—they are not compiler directives. This means that in the following chunk of code, the $SETG will be executed with SirFact error trapping disabled if %RECOVER is non-zero; otherwise, it will run with SirFact error trapping enabled.

```
IF %RECOVER THEN
SIRFACT OFF
END IF
%RC = $SETG('NEXTPROC', 'XP.MAIN-MENU')
IF %RECOVER THEN
SIRFACT ON
END IF
```

There is no harm in using SIRFACT ON if SirFact error checking is already enabled, and there is no harm in using SIRFACT OFF if SirFact error checking is already disabled.

**Additional Notes**

* It is good programming practice to minimize the number of SOUL statements inside a SIRFACT OFF/SIRFACT ON bracket to prevent accidentally leaving SirFact $function error checking disabled. It is even better programming practice to avoid the use of SirFact Off altogether.
* The SirFact Off setting also disables SirFact FRN error trapping set by the X'08', X'10', or X'20' bits in the SIRFACT system parameter. This means that if SIRFACT FRN error trapping is turned on, but an FRN statement, by design, sometimes refers to a non-existent record, the FRN could be coded as follows:

```
SIRFACT OFF
%HAVEREC = 0
IN FILE ODD FRN %RECNO
%HAVEREC = 1
END FOR
SIRFACT ON
```

* Example of a problem and solution involving $ListDel and SirFact.

**See also**

* SirFact
* SirFact post hoc debugging
* SIRFACT command
* SirFact SOUL statements
* SirFact system parameters
* SirFact $functions
* SirFact and comment-initialized globals
* SirFact FACT subsystem
* SirFact date processing

**Category:** SirFact
