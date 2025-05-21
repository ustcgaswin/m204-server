## ItemNotFound class

An ItemNotFound exception indicates that a collection object search located no collection items that satisfy the selection criterion specified in the collection method that invoked the search. There are several searching methods, but only those that return a single found item produce an ItemNotFound exception.

This exception class has no properties. It is simply a notification that a valid search found no items that met the selection criterion.

The class's only method is the `New` constructor, which you would typically use with a `SOUL Throw` statement to produce an `ItemNotFound` exception yourself. For example:

```
throw %(itemNotFound): new
```

Remember that you catch an exception with the `Catch` statement; if an exception condition occurs outside a `Catch` for it, the request is cancelled. For example, if you expect one or no matches from your search, you might specify a block like the following:

```
try
  %terminationInfo = %terminationList:findNextItem(eq(custId, %custid)
  process the termination info
catch itemNotFound
end try
```

On the other hand, if you always expect a match, you might want a program crash to result if you are wrong about the match, so you could leave out a `Try/Catch`.

The `Try/Catch` approach is slightly more efficient than a test for a zero result. `Try` generates no quads other than the branch around the `catch` block if the search succeeds. And in fact, you could put a `Try` around a loop and use the `ItemNotFound` exception to exit the loop:

```
%i = 0
try
  repeat forever
    %i = %colla:findNextItemNumber(<criterion>), start=%i)
    %obj = %colla(%i)
    process the object
  end repeat
catch itemNotFound
end try
```

The `ItemNotFound` class is available as of Sirius Mods version 7.6.

### The `ItemNotFound` methods

| Method | Description |
|---|---|
| `New` | Create a new `ItemNotFound` object |

The methods in the class are described in the subsections that follow. In addition:

* Notation conventions for methods has information about the conventions followed.
* `ItemNotFound` methods syntax is a single page that contains the syntax diagrams of all the methods in the class.

### New constructor

Create a new `ItemNotFound` object (`ItemNotFound` class)

This method generates an instance of an `ItemNotFound` exception. The `New` method format follows:

**Syntax**

```
%itemNotFound = [%(ItemNotFound):] New
```

**Syntax terms**

* `%itemNotFound`: An `ItemNotFound` variable which will refer to the newly created object.
* `[%(ItemNotFound):]`: The class name in parentheses denotes a Constructor. See Usage notes, below, for more information about invoking an `ItemNotFound` Constructor.

### Usage notes

As described in Using New or other Constructors, New can be invoked with no object, with an explicit class name, or with an object variable in the class, even if that object is Null:

```
%itemnfd = new
%itemnfd = %(ItemNotFound): new
%itemnfd = %itemnfd:new
```

**Category:** System exception classes
