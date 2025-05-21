## Targeted Text statements

**Contents**

1. **AuditText, PrintText, and TraceText**
    * 1.1 and directives
    * 1.2 Tilde statement directives
2. **SetText statement**
3. **ReturnText statement**

**AuditText, PrintText, and TraceText**

The AuditText, PrintText, and TraceText keywords combine and reorder Text Audit, Text Print, and Text Trace to emphasize the principle action.  AuditText outputs to the Model 204 audit trail, PrintText to the current output stream, and TraceText to current trace targets.  These are recommended alternatives to traditional Audit, Print, and Trace statements.  Statements are considered literal text.  Values of variables and calculations can be displayed within curly braces.

**Examples:**

```
%1 = 22
printText %1 = {%1}, %j = {%j}, %1" %j = {%1" %j}
```

Displays:

```
%1 = 22, %j = 33, %1 %j = 726
```

Any expression is allowed inside curly braces, including parentheses, functions, and method calls.

**Examples:**

```
%1 = 22
%j = 33
printText %i = {%i}, %j = {%j}, (%1+%j):toPower(3) = ((%1+%j):toPower(3))
```

Displays:

```
%i = 22, %j = 33, (%1+%j):toPower(3) = 166375
```

**-and- directives**

A single tilde character inside curly braces (`{-}`) must be followed by an expression inside curly braces.  The tilde is replaced with the expression's content.

**Example:**

```
%i = 22
%j = 33
printText {-} {%i}, {-} {%j}, {-} {(%i+%j):toPower(3)}
```

Displays:

```
%i = 22, %j = 33, (%i+%j):toPower(3) = 166375
```

**SetText statement**

The SetText statement sets a variable instead of outputting a string.

**Syntax:**

```
SetText Variable = string
```

**Variable:** A simple variable, class variable, class property, or collection member.

**Example:**

```
setText Stoke:string = Once upon a time
```

**ReturnText statement**

The ReturnText statement returns a string value in a User Language function or property Get method.

**Syntax:**

```
ReturnText string
```

**string:** A literal string that may include expressions in curly braces.

**Example:**

```
local function (float): aphorism is longstring
if Nthis eq 1 then
returnText Patriotism is the first refuge of the scoundrel
end if
end function
```
