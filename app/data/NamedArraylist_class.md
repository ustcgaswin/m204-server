## NamedArraylist class

A NamedArraylist can be thought of as a traditional array with no bounds but with identifying subscripts that can be arbitrary strings rather than numbers.

### About the GenericNamedArraylist class

The NamedArraylist class extends the system GenericNamedArraylist class.  Many methods available in the NamedArraylist class are documented as belonging to the GenericNamedArraylist class.  Other classes extending GenericNamedArraylist include FloatNamedArraylist and UnicodeNamedArraylist.  Looking up an item by name is only slightly more CPU intensive than by number. NamedArraylist items take up more space than Arraylists.  Overhead is usually minor.  Use NamedArraylists where functionality is useful, without performance concern.

### Examples

The following code shows some NamedArraylist methods:

```
%narl is collection NamedArraylist of longstring
%i is float
%narl = new
%narl:useDefault = true
%narl('Idle') = 'Eric'
%narl('Cleese') = 'John'
%narl('Gilliam') = 'Terry'
%narl('Pallin') = 'Michael'
%narl('Chapman') = 'Graham'
Print 'The American''s first name is ' %narl('Gilliam')
print '*** Completely different:'
for %i from 1 to %narl:count
print %narl:itemByNumber(%i) and %narl:nameByNumber(%i)
end for
```

This code prints:

```
The American's first name is Terry
Completely different:
Graham Chapman
John Cleese
Terry Gilliam
Eric Idle
Michael Pallin
```

*Note:* Items are kept in EBCDIC order by name.

For another example, suppose a Recordset object has fields `ProductId` (ten-character alphanumeric IDs) and `Quantity`.  NamedArraylists are ideal for getting the total quantity for each product ID:

```
%quantity is collection NamedArraylist of float
%quantity = new
%quantity:useDefault = true
for each record in %recset
%quantity(productId) = %quantity(productId) + quantity
end for
for %i from 1 to quantity:count
print %quantity:nameByNumber(%i) and %quantity:itemByNumber(%i)
end for
```

Because `UseDefault` is set to `True`, the first reference to a quantity item for a given ProductId will return the default value of 0. The value of the Quantity field is added to this 0, and the total is stored under the product ID. Subsequent requests for the same product ID will return the current running total.

### List of NamedArraylist methods

The List of NamedArraylist methods shows all class methods with brief descriptions.

### See also

*   List of FloatNamedArraylist methods
*   List of UnicodeNamedArraylist methods
*   Collections (background information on collections, Arraylists, and declaring Arraylist object variables)
*   Coding considerations for collections (tips on using collections)

**Category:** System classes
