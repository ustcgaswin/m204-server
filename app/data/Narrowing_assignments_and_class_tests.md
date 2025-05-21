## Narrowing assignments and class tests

As discussed in "Polymorphism", Sirius Mods Version 6.9 introduced class test and narrowing assignment capabilities to the Janus SOAP ULI. As stated in that discussion, class tests and narrowing assignments should not be done lightly and, in fact, should be avoided as much as possible. For this reason, by default, class tests and narrowing assignments are not allowed for any class.

If class tests or narrowing assignments are unavoidable for a class, however, an Allow Narrow clause must be placed in the Public block of that base class. For example, the Mammal class could have the following:

```
class mammal
public
allow narrow
variable weight is float
variable name is string len 32
variable home is string len 32
end public
end class
```

Then, if you have a class Marsupial that extends the Mammal class, you could do something like the following:

```
%pouchy is object marsupial
%furry is object mammal
if %furry:name = 'Skippy' then
%pouchy = %furry: (marsupial)
end if
```

In this example, if earlier lines in the program did not make %furry actually reference a Marsupial object, and it simply referenced a simple Mammal object or, perhaps, some other extension of class Mammal (say object Feline), the assignment of %furry to %pouchy would fail and the request would be cancelled.

### Contents

1.  Working with a pseudo-method
2.  Using the Is Instance Of operator
3.  See also

### Working with a pseudo-method

As shown in the preceding example, narrowing assignment cannot be done by a simple assignment of object variables (%furry to %pouchy, above). It has to be indicated by a pseudo-method consisting of the target class name in parentheses. The term pseudo-method is used because no actual code is executed in the (marsupial) operation and no transformation of the input object is performed - if %furry references a marsupial, the operation would succeed, otherwise it would fail. The narrowing pseudo-method will always fail if the source object is null.

With this syntax, you can string member names to a narrowing assignment pseudo-method. For example, if the Marsupial class in the above example had a PouchSize member, you can do something like this:

```
if %furry:name = 'Skippy' then
print %furry: (marsupial): pouchSize
end if
```

You can also pass a narrowing assignment pseudo-method output as a method parameter:

```
%zoo:addMarsupial (%furry: (marsupial))
```

You can even do something as silly as this:

```
print %furry: (marsupial): (mammal) weight
```

This (marsupial) does a narrowing assignment of %furry to an internal work variable. Then, the (mammal) weight accesses the Mammal base class member Weight via that variable. Of course, the above is identical to:

```
print %furry:weight
```

with the exception that the silly example would cancel the request if %furry did not reference a marsupial. It is worth emphasizing that the (marsupial) and the (mammal) in the above example serve two very different functions. The (marsupial) indicates a narrowing assignment to an extension class. The (mammal) indicates selection of a member of a base class.

### Using the Is Instance Of operator

You might want to do narrowing assignments on the basis of the class of the object pointed to by an object variable. In the above example, %furry is declared as being a Mammal object but, via polymorphism, can be pointing at an object of some extension class, including, of course, class Marsupial. You can use the Is Instance Of keywords to test whether the underlying object referenced by an object variable is of a specific class.

The following is an example using Is Instance Of:

```
%pouchy is object marsupial
%furry is object mammal
if %furry is instance of marsupial then
%pouchy = %furry: (marsupial)
end if
```

In fact, you can perform class-specific processing based on such tests:

```
%furry is object mammal
if %furry is instance of marsupial then
%furry:home = 'Australia'
end if
```

This kind of code is strongly discouraged, and it is a formula for long-term code maintainability problems. What will happen to the above code, for example, when used on an opossum, which is one of the few marsupials that doesn't necessarily live in Australia?

The Is Instance Of keywords must be followed by the name of a class that is an extension class of the type of the object variable that precedes the keywords.

**Note:** No system classes currently are Allow Narrow classes.

### See also

*   Inheritance and polymorphism
*   Dynamic dispatch
*   Enhancement methods
*   Object oriented programming in SOUL
