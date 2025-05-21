## InvalidValue class

An InvalidValue exception indicates that a given value is not associated with a given enumeration. For example:

```
enumeration foo
public
attribute x is float inverse fromX
value y (3)
allow ordinal
end public
end enumeration
%f is enumeration foo
%f = fromString('Y')
printText {~=%f}
%f = fromX(3)
printText {~=%f}
try
%f = fromString('z')
catch invalidValue
print 'z is not a foo enumeration string value'
end try
try
%f = fromX(1)
catch invalidValue
print '1 is not a foo enumeration x attribute value'
end try
%f = fromOrdinal(1)
printText {~=%f}
%f = fromOrdinal(3)
```

The result of the above fragment is:

```
%f=y
%f=y
z is not a foo enumeration string value
1 is not a foo enumeration x attribute value
%f=y
```

CANCELLING REQUEST: MSIR.0750: Class Foo, function FromOrdinal:
InvalidValue exception: 3 is not a valid ordinal number for class foo

The InvalidValue exception class has no properties. It is simply a notification that a valid attempt found no values that matched the given string or number.

To produce an InvalidValue exception yourself, you typically use a User Language Throw statement with an InvalidValue New constructor. This statement must be issued from within a method, and it can only be caught by the code that calls the method. For example, the following statement throws an InvalidValue exception:

```
throw %(invalidValue): new
```

Remember that you catch an exception with the Catch statement; if an exception condition occurs outside a Catch for it, the request is cancelled.

The InvalidValue class is available as of Sirius Mods version 7.8.

### The InvalidValue methods

The following are the available InvalidValue class methods.

| Method | Description |
|---|---|
| New | Create a new InvalidValue object |

The methods in the class are described in the subsections that follow. In addition:

*   Notation conventions for methods has information about the conventions followed.
*   InvalidValue methods syntax is a single page that contains the syntax diagrams of all the methods in the class.

### New constructor

Create a new InvalidValue object (InvalidValue class)

[Introduced in Sirius Mods 7.8]

This Constructor generates an instance of an InvalidValue exception. The New method format follows:

**Syntax**

```
%invalidValue = [%(InvalidValue):] New
```

**Syntax terms**

*   `%invalidValue` A reference to an instance of an InvalidValue object.
*   `[%(InvalidValue):]` The class name in parentheses denotes a Constructor. See Usage notes, below, for more information about invoking an InvalidValue Constructor.

**Usage notes**

*   As described in Using New or other Constructors, New can be invoked with no object, with an explicit class name, or with an object variable in the class, even if that object is Null:
    *   `%invalid = new`
    *   `%invalid = %(InvalidValue): new`
    *   `%invalid = %invalid: new`

**Category:** System exception classes
