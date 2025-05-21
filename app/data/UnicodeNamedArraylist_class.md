## UnicodeNamedArraylist class

The UnicodeNamedArraylist class is nearly identical to the NamedArraylist class. The main difference is that instead of EBCDIC subscript names for items as in the NamedArraylist class, the name subscripts in a UnicodeNamedArraylist object are Unicode values. UnicodeNamedArraylist items are stored by item name in Unicode order, whereas NamedArraylist items are stored by item name in EBCDIC order.

**Note:** The names of UnicodeNamedArraylists are limited to 127 characters (versus 255 bytes for NamedArraylists).

### Contents

1. About the Generic NamedArraylist class
2. Examples
3. List of UnicodeNamedArraylist methods
4. See also

### About the Generic NamedArraylist class

The UnicodeNamedArraylist class extends the system GenericNamedArraylist class, so many of the methods available in the UnicodeNamedArraylist class are documented as belonging to the GenericNamedArraylist class. Any method available in the GenericNamedArraylist class is also available in the UnicodeNamedArraylist class.

### Examples

The following annotated request demonstrates the methods where the distinction between a UnicodeNamedArraylist and a NamedArraylist is significant. The U constant function handily creates Unicode strings in the item name subscripts. Those names also begin with an XHTML entity reference.

```
b
%i is float
%k is unicodeNamedArraylist of longstring
%m is Arraylist of longstring
%k = new
implicit Item method sets list item values:
%k('&sect; Jan':u) = 'Orion'
%k('&sect; Apr': u) = 'Leo'
%k('&sect; Mar':u) = 'Cancer'
%k('&sect; Jun': u) = 'Ursa Minor'
%k('&sect; Nov':u) = 'Andromeda'
%k('&sect; Dec': u) = 'Aries'
%k('&sect; Feb': u) = 'Canis Major'
print the value of the item whose name is the Item method argument:
print %k: Item('&sect; Jan': u)
* print item number of named item:
print %k: number('&sect; Nov': u)
print name of each list item, in order of item position from first to last:
for %i from 1 to %k:count
print %i': ' %k: nameByNumber(%i)
end for
how many items remain after specified item is removed?
printText {-} = {%k: removeItem('&sect; Dec':u)}
names and values of list items in order by item position:
for %i from 1 to %k:count
print %k: nameByNumber(%i)':
end for
and %k:itembyNumber(%i)
sort list values alphabetically in descending order
%m = %k:sortNew(descending(this))
%m:print
end
```

The request prints:

```
Orion
7
1: §Apr
2: §Dec
3: §Feb
4: §Jan
5: §Jun
6: §Mar
7: §Nov
%k: removeItem('&sect; Dec':u) = 6
§Apr: Leo
§Feb: Canis Major
§Jan: Orion
§Jun: Ursa Minor
SMar: Cancer
§Nov: Andromeda
1: Ursa Minor
2: Orion
3: Leo
4: Canis Major
5: Cancer
6: Andromeda
```

### List of Unicode NamedArraylist methods

The "List of Unicode NamedArraylist methods" shows all the class methods, with a brief description of each.

### See also

* List of Named Arraylist methods
* List of FloatNamedArraylist methods
* Collections
* Coding considerations for collections
