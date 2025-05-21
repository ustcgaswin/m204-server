## SelectionCriterion class

**1 Declaring a SelectionCriterion object variable**

* **1.1 SelectionCriterion object variable declaration syntax**

**2 Specifying a SelectionCriterion's parameters**

**3 List of SelectionCriterion methods**

Objects in the `SelectionCriterion` class are used primarily as input to the collection class searching methods. The searching methods take a `SelectionCriterion` object as a parameter which provides an item selection specification for the search. A selection specification, or criterion, is a relational expression that tests a collection item value to determine whether the item (or its item number) is to be returned by the searching method. The `SelectionCriterion` methods are named for the operation they provide:

* **Comparisons:** `Eq` (equal to), `Ne` (not equal to), `Lt` (less than), `Le` (less than or equal to), `Gt` (greater than), `Ge` (greater than or equal to). The type of comparison performed is the same as the type of the first argument of the given comparison method: String, Float, or Unicode.

* **`And`**, **`Or`**, and **`Not`**: methods let you combine comparison operations in a single criterion.

* **`True`** and **`False`**: methods respectively match all or no items.

* **`IsNull`** and **`IsNotNull`**: methods respectively match a collection object that is Null or one that is not Null.

Most `SelectionCriterion` objects use two parameters to construct a single criterion for selecting a collection item; some use two or more `SelectionCriterion` objects to construct a single criterion.

**Example:**

`%sel lt(squareRoot, 10)`

**Requirements for the SelectionCriterion parameter:**

* It must be a method or method variable defined to operate on the type of the items in the collection being searched.
* It must return an intrinsic (number, string, unicode) value.

**Additional notes:**

* Comparison values are evaluated at `SelectionCriterion` construction time, not when the search method is evaluated.
* `SelectionCriterion` have a size limit: the `SelectionCriterion`, including the values for `Eq`, `Ne`, `Lt`, `Le`, `Ge`, and `Gt` comparisons must take less than 252 bytes.

**Declaring a SelectionCriterion object variable**

The `SelectionCriterion` class operates on specific objects, so a variable of the `SelectionCriterion` class must be qualified with the object to which it applies.

* **Syntax:** `objvar Is Object SelectionCriterion For itemtype`

**Specifying a SelectionCriterion's parameters**

The comparison `SelectionCriterion` methods accept two parameters: the first a "method value" and the second an intrinsic value.

* **Method value:** A method value argument can be a method literal, a method variable, or an expression that results in a method.
* **Implicit declaration:** `Is Function (itemtype): methname Is intrinEnum`

**Note:** Enumeration values are allowed as of Sirius Mods 7.8, but only in the `Eq` and `Ne` methods.

**Examples of method argument types:**

Examples show different method argument types as the first parameter in a `SelectionCriterion`.

**List of SelectionCriterion methods**

The "List of SelectionCriterion methods" shows all the class methods.

* **Category:** System classes
