## Statement syntax

**Statement Types**

*   **Assignment Statements**
*   **Conditional Statements**
*   **Loop Statements**
*   **Procedure Statements**
*   **Return Statements**
*   **Expression Statements**
*   **Null Statements**

**Assignment Statements**

```
variable = expression;
```

**Conditional Statements**

*   **if statement**

```
if (condition) {
    statement_block;
} else if (condition) {
    statement_block;
} else {
    statement_block;
}
```

*   **switch statement**

```
switch (expression) {
    case value1:
        statement_block;
        break;
    case value2:
        statement_block;
        break;
    default:
        statement_block;
}
```

**Loop Statements**

*   **for loop**

```
for (initialization; condition; increment) {
    statement_block;
}
```

*   **while loop**

```
while (condition) {
    statement_block;
}
```

*   **do-while loop**

```
do {
    statement_block;
} while (condition);
```

**Procedure Statements**

```
procedure_name(arguments);
```

**Return Statements**

```
return expression;
```

**Expression Statements**

```
expression;
```

**Null Statements**

```
;
```

**Data Types**

*   **Integer**
*   **Floating-point**
*   **String**
*   **Boolean**
*   **Character**

**Operators**

*   **Arithmetic Operators**
*   **Relational Operators**
*   **Logical Operators**
*   **Bitwise Operators**
*   **Assignment Operators**

**Keywords**

*   `if`, `else`, `switch`, `case`, `default`, `for`, `while`, `do`, `break`, `continue`, `return`, `procedure`, `function`, `var`, `const`, `int`, `float`, `string`, `boolean`, `char`, `and`, `or`, `not`, `true`, `false`, `null`, `begin`, `end`, `then`, `else`, `until`, `loop`, `exit`, `repeat`, `until`, `to`, `downto`, `case`, `of`, `mod`, `div`, `xor`, `shl`, `shr`, `and`, `or`, `not`, `:=`, `=`, `<>`, `>`, `<`, `>=`, `<=`, `+`, `-`, `*`, `/`, `%`, `mod`, `div`, `^`, `@`, `.`

**Example**

```
procedure example(x: integer, y: integer): integer;
begin
    if x > y then
        return x
    else
        return y;
end;
```

**Error Handling**

*   **Error Messages**
*   **Error Codes**

**Further Information**

*   [Link to further documentation]
