## Notation conventions for methods

The syntax for each SOUL system method is displayed in the form of a usage statement which shows how you specify a typical invocation of the method (and receive its result, if any). The syntax includes:

* Keywords and placeholder words
* Punctuation literals (parentheses, colons, etc. that you must specify)
* Special characters (square brackets, hyphens) that you do not specify because they are part of the syntax structure, not the method content

For example, here is the syntax for the FindNextitem method of the Arraylist class:

```
%item al: FindNextItem(selectionCriterion, [Start number])
Throws ItemNotFound
```

The FindNextitem method does the following:

* Accepts two arguments (the second of which is optional - as indicated by the square brackets ([]) - and also named as indicated by the name and equal sign (=)).
* Takes as input the Arraylist that is the method object (al).
* May throw the ItemNotFound exception.
* Otherwise (that is, if successful), returns the item of that method object Arraylist, as indicated by the assignment to %item.
* Commas are always required to separate arguments, whether named or not. Trailing commas are not required. Commas after missing arguments are required only if a following un-named argument is specified.

The following sections describe additional aspects of the syntax, and Member reference provides further discussion about invoking methods.

### Contents

1. Mixed case
2. Method type
3. Optional arguments
4. Named arguments
5. Callable functions
6. Shared methods
7. Constructors
8. Exceptions
9. See also

### Mixed case

Method names are in mixed case with initial and internal capital letters. SOUL keywords and variable name references are also in mixed case. This mixed-case convention is adhered to in text and syntax; it is not strictly adhered to in code examples. For more information about mixed-case User Language, see Mixed-case SOUL.

### Method type

The methods are labeled by type, of which there are four:

* **Functions:** Methods that produce an output value.
* **Subroutines:** Methods that produce no output value.
* **Properties:** Methods that produce an output value and that can sometimes be set to a value.
* **Constructors:** Methods that produce a new object instance.

### Optional arguments

In syntax, method arguments are required unless otherwise stated. Omitting a mandatory argument results in a compilation error. Optional arguments are enclosed in brackets ([]) in syntax and are described in text with the word "optional."

### Named arguments

Some method arguments may be passed using their name; they are specified with a following equal sign (=) in method syntax diagrams. For arguments described as "name allowed" (or "name required"), you may specify (or must specify) the argument name along with the argument value when you invoke the method. You specify such argument name and value pairs in the form: object:methodname (... argName=argValue ...).

### Callable functions

Many functions that return a result may also be invoked by the keyword Call. The descriptions of such functions say that they are "callable" and display a bracketed variable and equals sign in syntax.

### Shared methods

Methods are non-shared unless otherwise identified. Shared methods are denoted by the %(classname): notation preceding the method name in the method syntax, which implies that the method need not operate on a particular object.

### Constructors

Methods that produce a new object instance of their class are known generally as "constructors." These include the New method in Janus system classes (a non-shared method whose method type is Constructor), and the shared method virtual constructors.

### Exceptions

If a system method may throw one or more exceptions, the method syntax displayed for that method contains the name of the exception(s) below the syntax, preceded by the word Throws.

### See also

* Conventions and terminology for XmlDoc API methods