## InvalidSortSpecification class

An InvalidSortSpecification exception indicates that the first argument provided to either the Sort or SortNew method (in the Stringlist class) is not a valid sort specification.

This exception class has no properties. The class's only method is the New constructor, which you would typically use with a SOUL Throw statement to produce an InvalidSortSpecification exception yourself. For example:

```
throw %(invalidSortSpecification): new
```

Remember that you catch an exception with the Catch statement; if an exception condition occurs outside a Catch for it, the request is cancelled.

Although the InvalidSortSpecification is rarely used, one possibly appropriate occasion might be a sort specification entered (at least in part) by an end user. In this case, you could provide a try.../catch invalidSortSpecification block like the following:

```
%sortSpec = $read('Enter sort specification')
try %strlist:sort (%sortSpec)
catch invalidSortSpecification
Print 'Invalid sort specification'
end try
```

In most cases, however, a sort specification is explicitly coded in the User Language request (for example, rather than %sortSpec = $read.
as above, something like %sortSpec = '1,10, CH, A'). You expect such a sort specification to be correct in all circumstances, and if it is not, you want a request cancellation. So you do not provide a try.../catch block.

The InvalidSortSpecification class is available as of Sirius Mods version 7.9.

### The InvalidSortSpecification methods

The following are the available InvalidSortSpecification class methods.

| Method | Description |
|---|---|
| New | Create a new InvalidSortSpecification object |

The methods in the class are described in the subsections that follow. In addition:

* "Notation conventions for methods" has information about the conventions followed.
* "InvalidSortSpecification methods syntax" is a single page that contains the syntax diagrams of all the methods in the class.

### New constructor

Create a new InvalidSortSpecification object (InvalidSortSpecification class)

[Introduced in Sirius Mods 7.9]

This Constructor generates an instance of an InvalidSortSpecification exception. The New method format follows:

#### Syntax

```
%invalidSortSpecification = [%(InvalidSortSpecification):) New
```

#### Syntax terms

* `%invalidSortSpecification`: An InvalidSortSpecification %variable which will refer to the newly created object.
* `[%(InvalidSortSpecification):]`: The class name in parentheses denotes a Constructor. New can also be invoked via an InvalidSortSpecification object variable.

**Category:** System exception classes
