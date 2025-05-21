## Stringlist class

The Stringlist class provides a facility that resembles an unbounded array of strings. While less generalized than a collection class, it has a richer set of methods than any collection class. Unfortunately, the Stringlist class cannot be thought of as either a subset or superset of collections, so it is not necessarily an obvious decision as to which to use.

### Stringlist usage

The Stringlist class is essentially an object-oriented version of $lists. It is strongly recommended that all new applications use Stringlists instead of $lists:

*   Because they use the object-oriented model of instantiation, Stringlists eliminate the confusing behavior of $lists that results from their being tied to the statement that created them.
*   Stringlists are strongly datatyped, so application errors resulting from misuse of $list identifiers are eliminated. Code is clearer because it is obvious that a variable is a Stringlist - $list identifiers are simply Float, Fixed, or String datatypes.
*   Because of the way $lists are instantiated, they are essentially unusable inside class methods.

Stringlist objects are instantiated with the New function or with methods that return Stringlist instances "under the covers" such as the Sort, SortNew or Copy methods. The system Discard method deletes a Stringlist object. Stringlists are stored in CCATEMP, so they take minimal server table space no matter how large the Stringlist. Like all system classes, Stringlists are considered longstring-capable, that is, any string inputs and outputs are considered longstrings for expression-compilation purposes. The main impact of this is that Stringlist inputs and outputs will have standard longstring truncation behavior: truncation by assignment results in request cancellation. In addition to the described methods, it is also possible to add data to a Stringlist with the HTML/Text statement using the To clause. This is especially useful if there is a block of mostly constant text to be added to a Stringlist.

```
Begin
%sl is object Stringlist
%s1 = New
Text to %sl
<html>
<head>
<title>Loading a complete web page into a stringlist</title>
</head>
<body>
Each line of this text block is a new stringlist item.
</body>
End Text
%sl:Print
End
```

### Migrating from $lists to Stringlists

The MoveFromId and MoveTold methods facilitate migration from $lists to Stringlists: Stringlists can be passed to code that works on $lists after being converted to a $list with MoveTold; $lists can be converted to Stringlists with MoveFromId.

### Compatibility

Stringlist items can be as many as 2<sup>31</sup>-1 bytes long. While all methods work correctly with Stringlist items longer than 6124 bytes, a few methods such as Locate and LocateUp restrict their processing to the first 6124 bytes of a Stringlist item. There is no overall practical size limit on a stringlist object itself. The stringlist is maintained internally as a binary tree, to a maximum depth of three levels, similarly to a $list object. The stringlist object will be as compact a possible if it is built using the add method to add successive stringlist items to the end of the object, as opposed to using methods such as insert, delete, and replace, where the compaction of the stringlist object will be reduced. For stringlists where the average stringlist item length is less than the pagesize (6144), then the maximum overall size of the stringlist object is approximately 3.5 Gigabytes. If the average stringlist item length is greater, up to the maximum item length of 2 gigabytes, then the overall object size can extend in theory up to 1.26 petabytes.

### List of Stringlist methods

The individual Stringlist methods are summarized in "List of Stringlist methods".

Category: System classes
