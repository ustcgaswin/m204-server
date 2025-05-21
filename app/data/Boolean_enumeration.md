## Boolean enumeration

The Boolean enumeration implements the standard logical paradigm of "true" and "false". While, in some sense, it is just like any other enumeration, because the concept of true and false is so basic to programming, Boolean variables are treated specially by User Language.

**Boolean enumeration values**

*   The system Boolean enumeration values are:
    *   True
    *   False

However, like all other enumerations, a Boolean variable can also be unset to have a null value. While this can be useful in detecting references to unset variables, it can also be problematic in certain situations. Use of the Initial clause on variable declarations and the Default clause on method parameter declarations can mitigate most of these issues, nevertheless it is important to keep in mind the possibility of a Boolean variable being null.

**Declaring boolean variables**

Boolean variables can be declared the same way any enumeration variables are declared, with the keyword Enumeration followed by the class name:

```
%truth is enumeration boolean
```

However, because boolean processing is such a basic part of any programming language, Boolean variables can be declared without the Enumeration keyword:

```
boolean
```

This is also true for method variables and results. For example, the following:

```
function foo(in is float, Stest is enumeration boolean) is enumeration boolean
```

is identical to:

```
function foo(in is float, Stest is boolean) is boolean
```

**Using Booleans in If conditions**

The whole point of Boolean variables is to be able to test them for truth and, depending on the result, perform some processing. For example, if `bool` is a Boolean variable, the following statement performs the contents of the if block if `%bool` is set to True:

```
if %bool then
```

**Using logical operators with Booleans**

User Language has three logical operators:

*   And
*   Not
*   Or

Strictly speaking, in User Language these operators actually operate on numeric values where `B` is treated as False and any non-zero value is treated as True. However, because of the importance of boolean logic in programming, Boolean variables or values can be used instead of numbers for these operators. A Boolean True is treated as 1 and a False is treated as 0. As such, Boolean variables can be used in logical operations as if they were numbers.

```
if not bool then
if %bool and (%y gt %x) Then
```

**Automatic conversion of numbers to Boolean**

In Sirius Mods 8.1 and later, it is also possible to assign a number to a Boolean variable. When this is done, the number 0 is converted to False and every other value is converted to True. This is especially useful for assigning the result of a comparison to a Boolean:

```
local subroutine foo(%ok is boolean)
%(local): foo(%x gt %y)
```

**Booleans not automatically converted to numbers**

While in Sirius Mods 8.1 and later, numeric values are automatically converted when assigned to a Boolean variable, the inverse is not true. For example, if `%number` is a Float variable and `bool` is a Boolean, the following will result in a compilation error:

```
number = %bool
```

**Enumeration methods**

Besides the common enumeration methods, the system Boolean enumeration has additional methods available, which are described here.

**IsFalse function**

This function examines a Boolean enumeration and returns an integer (0 or 1) according to the Boolean value (True or False).

**IsTrue function**

This function examines a Boolean enumeration and returns an integer (1 or 0) according to the Boolean value (True or False).
