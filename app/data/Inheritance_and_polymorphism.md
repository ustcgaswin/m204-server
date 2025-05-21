## Inheritance and polymorphism

**Background**

Inheritance and polymorphism are key concepts related to object-oriented programming. Inheritance allows programmers to create new classes (derived classes) based on existing classes (base classes), inheriting their attributes and methods. Polymorphism allows objects of different classes to be treated as objects of a common type.

**Base Classes**

A base class is a class from which other classes are derived.  It defines common attributes and methods that are inherited by derived classes.

**Derived Classes**

A derived class inherits attributes and methods from its base class and can add its own unique attributes and methods.

**Inheritance Syntax**

```
class DerivedClass extends BaseClass {
  // ...
}
```

**Multiple Inheritance**

Multiple inheritance allows a class to inherit from multiple base classes.  This can lead to complexities and potential conflicts.

**Flexible Inheritance**

Flexible inheritance allows for more complex relationships between classes.

**Polymorphism**

Polymorphism allows objects of different classes to be treated as objects of a common type.  This enables writing more general code that can work with objects of various types.

**Assignment Compatibility**

Assignment compatibility rules define how objects of different classes can be assigned to variables of different types.

**Interfaces**

Interfaces define a contract that classes must adhere to.  They specify methods that a class must implement.

**Constructors and Inheritance**

Constructors in derived classes can call constructors in base classes.

**Rules for constructors and inheritance**

*   A derived class constructor can explicitly call a base class constructor.
*   If a base class constructor is not explicitly called, the default base class constructor is called.
*   The base class constructor must be called before any other statements in the derived class constructor.

**Example Code Snippets**

(Numerous code examples are provided throughout the article.)

**Object-Specific Issues**

(Detailed explanations and examples of object-specific issues related to inheritance and polymorphism are included.)

**Side Effects**

(A list of potential side effects related to inheritance and polymorphism is presented.)

**Delegation**

(Explanation of delegation as an alternative approach to inheritance.)
