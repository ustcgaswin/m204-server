## SortOrder class

Objects in the SortOrder class are used primarily as input to the collection class sorting methods (SortNew in the Arraylist and NamedArraylist classes and Sort in the Arraylist class). The SortNew and Sort methods take a SortOrder object as a parameter which provides sorting criteria for the sort.
The SortOrder class is new as of Sirius Mods version 7.3.

### Overview

A SortOrder object consists of two kinds of information:

*   A sort ordering direction (Ascending or Descending)
*   A sort key (function on the items in the list being sorted)

For a sort in ascending order by price for the items in a %orders Arraylist, you might specify:

```
%orders = %orders: SortNew(ascending(price))
```

where Ascending is a SortOrder constructor with parameter price, a method defined to operate on the items in %orders. The price method above is applied to each of the items in %orders, and the resulting values are sorted in ascending order, alphabetically or numerically according to the price return value type. price must return an intrinsic (number, string, unicode) value.

If %sord is a SortOrder instance returned by the Ascending constructor with the price method as argument:

```
%sord = Ascending(price)
```

Then the SortNew statement above is equivalent to the following:

```
%orders = %orders: SortNew(%sord)
```

To provide multiple sort criteria for a sort, a SortOrder object may be a collection of multiple SortOrders. However, to construct such a SortOrder collection, you must use the SortOrder class List constructor. List takes SortOrders as inputs and returns a SortOrder object that contains them.

For example, to sort in ascending order by quantity and price for %orders:

```
%sord = List(ascending(quantity), descending(price))
%orders = %orders: SortNew(%sord)
```

The SortOrder object in the preceding example contains two sort criteria; you may specify as many as seven.

### Declaring a SortOrder object variable

The syntax for declaring a SortOrder object variable is:

```
objvar Is Object SortOrder For itemtype
```

Where:

*   objvar: The name of the SortOrder object variable.
*   itemtype: The datatype of the items in the collection to be sorted.

### Assigning a SortOrder object variable

SortOrders are immutable objects. You can assign a sort order to a SortOrder object:

```
%myOrder is object sortorder for longstring
%myorder = ascending(length)
```

But thereafter you cannot modify the object although, you may assign a different value to a SortOrder object variable:

```
%myorder = ascending(length)
%myorder = descending(reverse)
```

### Specifying a SortOrder's sort key method

The following examples show different method types, actually "method values," as the sort key in a SortOrder. A method value is a value assigned to a method variable. And for a SortOrder, the method variable's implicit declaration is:

```
Is Function (itemtype):methname Is intrinType
```

Where:

*   itemtype: The class of the items in the collection to be sorted.
*   methname: A method name, but merely a placeholder in an actual declaration. Any method (system or user), class Variable or Property, local method, or method variable that fits the declaration can be used in the SortOrder.
*   intrin Type: A User Language intrinsic class type returned by the function.

### List of SortOrder methods

The "List of SortOrder methods" shows all the class methods.

Category: System classes
