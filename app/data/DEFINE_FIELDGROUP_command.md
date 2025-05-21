## DEFINE FIELDGROUP command

**Summary**

Status: Available as of Model 204 release 7.5
Privileges: Any user
Function: To establish the contents of a field group, including the fields and field groups associated with the field group being defined.

**Syntax**

```
DEFINE FIELDGROUP FieldGroupName
[WITH FIELDGROUP OuterGroupName]
```

**Where:**

* `FieldGroupName` specifies the name of the field group you are defining. A field group name must be unique among field group names, and it cannot have the same name as any field.
* `OuterGroupName` specifies the name of another field group previously defined. The field group being defined is nested under the `OuterGroupName`.

For general syntax and usage notes that apply to all forms of the DEFINE command, see [DEFINE command].

**Adding fieldgroup contents**

The `WITH FIELDGROUP` clause identifies a field group that is already defined.

* Example: To define a field group `MAKE_MODEL` containing fields `MAKE` and `MODEL`:
```
DEFINE FIELDGROUP MAKE MODEL
DEFINE FIELD MAKE WITH FIELDGROUP MAKE MODEL AND ORDERED CHARACTER
DEFINE FIELD MODEL WITH FIELDGROUP MAKE MODEL
```

* To define a field group within a field group:
```
DEFINE FIELDGROUP BIRDS WITH FIELDGROUP VERTEBRATES
```

**Field considerations**

* Fields defined in a field group definition follow the syntax rules described in the `DEFINE FIELD` command.
* Field definitions must follow the definitions of their containing field group. It is then easier to add fields later to an already defined field group.
* The keyword `AND` on field definitions means that a field group name can contain blanks, just like a field name.
* To separate a field group name from subsequent field attributes, you must use the `AND` keyword.
* The `AND` is unnecessary if the `FIELDGROUP FieldGroupName` clause is the last field attribute.
* Note: The `MAKE_MODEL` field group must be defined before defining the field(s) and/or field group(s) that belong with it.
* Once you define a field you cannot move it into or out of a fieldgroup using a `REDEFINE` command. You must delete the field and relocate it by defining it again, locating it where you want it.

**Attribute considerations**

* A field in a field group cannot have the `OCCURS` or `INVISIBLE` attribute.
* You can assign any other attributes you choose, paying attention to those that conflict with each other.
* The default attribute for frequency of occurrence for fields in a field group is `EXACTLY-ONE`, whereas the default for individually defined fields is `REPEATABLE`.
* You can declare a field inside a field group as `REPEATABLE`, as well as `AT-MOST-ONE`.
* The `REPEATABLE` and `AT-MOST-ONE` field attributes have different behavior for fields inside a field group.

**Nested field groups**

Nested field groups are limited to 16 levels.

* Example syntax for nesting:
```
DEFINE FIELDGROUP VERTEBRATES
DEFINE FIELDGROUP BIRDS WITH FIELDGROUP VERTEBRATES
DEFINE FIELDGROUP PARROTS WITH FIELDGROUP BIRDS
DEFINE FIELDGROUP OWLS WITH FIELDGROUP BIRDS
DEFINE FIELDGROUP REPTILES WITH FIELDGROUP VERTEBRATES
DEFINE FIELDGROUP SNAKES WITH FIELDGROUP REPTILES
```

**See also**

* Defining a field group
* Working with field groups
* Updating field groups