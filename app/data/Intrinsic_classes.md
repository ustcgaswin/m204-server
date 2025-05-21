## Intrinsic classes

**Definition of Intrinsic classes**

"Intrinsic" objects are the essential datatypes or objects.  That is, all datatypes are extension classes of some base object class. This means that even things that are considered simple types like strings or numbers are, in fact, objects.  They are managed exactly the same as any other object.

**Two generic intrinsic classes: string and numeric**

SOUL traditionally supported many datatypes, including strings of a specified length and numeric datatypes.  These are considered intrinsic because they are handled internally in a way that is different from other object types.

**Intrinsic methods against database field names**

Field names can contain special characters.

**Intrinsic methods against percent variables and images that have the same name**

If a percent variable and an image have the same name, you must specify the type when using a method.

**Automatic "implicit" conversion of intrinsic values**

Method names can be optionally preceded or followed by spaces.  The input value is automatically converted to the target datatype.

**Intrinsic methods in a Print, Audit, or Trace statement**

Intrinsic methods are not allowed in Print, Audit, or Trace statements.  Parentheses are not allowed in these statements.

**Intrinsic method syntax: special cases**

SOUL's intrinsic method syntax has special cases for legacy programming.  Field names can contain colors and spaces.  The field name must be contained inside parentheses.

**Intrinsic methods**

*   **String:** Methods that perform string manipulation.
*   **Float:** Methods that perform numeric manipulation on values in Model 204's Float format.
*   **Fixed:** Methods that perform numeric manipulation on values in Model 204's Fixed format.
*   **Unicode:** Methods that perform Unicode string manipulation.  Longstring capability assumed.

**Strings and numbers as method objects**

The intrinsic representation of strings and numbers is used for values in variables.  The method is applied to the value, regardless of its internal representation.

**Intrinsic methods can be applied to constants**

Intrinsic methods can be applied to constants in addition to variables.

**Automatic "implicit" conversion of intrinsic values**

The method object from the method name can be optionally preceded or followed by spaces.  This is for readability.

**Examples**

```
print str:Length
print list(object):right(8)
print image michigan:right(8)
print (ktweedledum+ktweedledee):right(8)
```
