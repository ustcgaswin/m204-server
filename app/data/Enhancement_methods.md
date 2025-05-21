## Enhancement methods

**Background**

In broad terms, a class describes the state of objects of that class (state data in non-shared public and private variables) and the operations that can be performed on those objects (methods).  When using a class, it's often necessary to perform operations not provided by standard methods.  Before enhancement methods, there were three approaches: adding a method to the class, creating an extension, or creating a shared method.

**Enhancement Method Declaration Syntax**

```
enhancement method (class):name [otherMethodDesc]
```

Where:

*   **method:** The method type (Subroutine, Function, or Property). Cannot be a Constructor.
*   **class:** The class of the objects to which the method applies. Cannot be the containing class.
*   **name:** The name of the enhancement method. It can be the same as a containing class method (but not an extension class method).
*   **otherMethodDesc:** Method parameters, type, and qualifiers (like AllowNullObject).

**Enhancement Method Invocation Syntax**

```
object: (+containerClass) name [(arguments)]
```

Where:

*   **object:** An object variable of the class against which the enhancement method operates.
*   **containerClass:** The name of the class containing the enhancement method definition (preceded by a plus sign (+)).
*   **arguments:** Any arguments the enhancement method takes.

**Enhancement Methods for Collections**

Enhancement methods can be added to any class, including collections.  This is common for adding methods to collections of objects.  For example, an enhancement method could be added to an `ArrayList` of `Order` objects to create a new `ArrayList` of backordered items.

**Invoking a Local Alias of an Enhancement Method**

You can define a local alias for an enhancement method and invoke it using the alias.

**Enhancement Methods and Inheritance**

Enhancement methods are not automatically inherited by extension classes.  They must be explicitly applied to the specific class.

**Intrinsic Enhancement Methods**

Enhancement methods can be created for intrinsic classes like `Float` and `String`.  Examples include calculating the hypotenuse of a triangle or counting vowels in a string.
