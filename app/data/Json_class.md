## Json class

The Json class facilitates data exchange with JavaScript programs or other programs that support the JSON format. JSON can be considered as an alternative encoding format to XML. The advantages of JSON over XML are:

* It is easier to use in JavaScript programs.
* It maps more naturally on to object-oriented structures, especially collections.
* It is lighter-weight.

All that said, there are many situations where XML is a better choice for an exchange format than JSON. Json objects are typically created either programmatically using JSON constructors or by parsing a JSON string sent from another platform. A Json object tree can be examined and manipulated using a set of Json functions, and the resulting object tree can be serialized as a string. The Json parsing and serialization functions operate only on Unicode, so a separate step is required to encode/decode the data to/from a format suitable for network transfer, most commonly UTF-8.

**Notes:**

* One slight oddity in the JSON support is that JSON variable names, unlike any other variable names, cannot contain a period. So `%my.json.variable` is not a valid JSON variable name.
* The Json class is new in version 7.6 of Model 204.

**JsonType enumeration**

The JsonType enumeration indicates the type of JSON data represented by a Json object. Json objects simulate JavaScript variables in that they are untyped, so the same variable can actually reference very different datatypes.

The values of the JsonType enumeration which correspond to the equivalent JSON datatypes are:

* **Null:** A null object value. A Json object will never have a null value, but the type of a null pointer is Null.
* **String:** A unicode string value which can be set or retrieved from a SOUL Unicode variable or expression.
* **Number:** A number value which behaves very much like Model 204 Float variables.
* **Boolean:** A value of True or False.
* **Array:** An ordered but unnamed collection of Json objects.
* **Object:** An ordered, named collection of Json objects.

**Implicit conversions**

A Json object can represent a number or a string. The variable name for a Json object follows the naming conventions for SOUL variables, except it cannot contain a period. To create a Json string object, you can use the String function. To create a Json number object, you can use the Number function:

```
%json = string("Argle bargle")
%json = number (2.718281828459)
```

However, in many cases, you can assign an intrinsic number of string value directly to a Json object without the conversion function:

```
%json = "Argle bargle"
%json = 2.718281828459
```

The resulting type of the Json object is determined from the type of the source of the assignment.

**Circular references**

Json objects can have circular references, also known as cycles in object oriented terms. For example, you can create an array as:

```
%json = array(13, 99, 44, array("Hickory", "Dickory", "Doc"))
```

However, if you then create a circular reference to the array object:

```
%json(4):add(%json)
```

This is not an ideal serialization, because it provides no indication of what `[Circular]` refers to.  The JSON standard does not allow circular references.  It is recommended that a JSON object be serialized using the Stringify function.

**Inheritance**

Although it is technically possible to create a class that extends the Json class, it is impossible to do much that is useful with such an extension class, because the Json class has no true constructors, only factory methods.

**List of Json methods**

List of Json methods contains a complete list of the class methods.
Category: System classes
