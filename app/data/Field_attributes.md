## Field attributes

**Contents [hide]**

**File model feature**

The file manager defines fields and assigns attributes to each field, as described in Defining fields manually. The field attributes listed on this page are of interest to the application developer. You can enforce file-wide constraints on files and fields with two Model 204 file models:

**File model** | **Action...**
---|---|
Numeric Validation | Causes Model 204 to perform numeric data type validation on fields defined as FLOAT or BINARY.
1NF (First-Normal Form) | Ensures that the data within a file conforms to the rules for First-Normal Form.

**Field attribute descriptions**

**AT-MOST-ONE and REPEATABLE attributes**

If a field is defined as having the AT-MOST-ONE attribute, Model 204 prevents multiple occurrences of that field in any given record. However, unlike fields with the OCCURS attribute, AT-MOST-ONE fields are not specifically preallocated. If a field is not defined as AT-MOST-ONE, then it is REPEATABLE. REPEATABLE is the default except for First-Normal Form files, where AT-MOST-ONE is required on all fields.

**AT-MOST-ONE versus UNIQUE attributes**

Although the names of the UNIQUE and AT-MOST-ONE attributes sound similar, they have very different meanings:

*   UNIQUE affects the value of the field
*   AT-MOST-ONE affects the number of field occurrences per record

For example, if a Social Security field within an EMPLOYEE file is both UNIQUE and AT-MOST-ONE; the UNIQUE attribute ensures that the social security number for every employee is different, and AT-MOST-ONE ensures that each employee has only one social security number. Both AT-MOST-ONE and UNIQUE are examples of "field level constraints."

**BINARY-LARGE-OBJECT and CHARACTER-LARGE-OBJECT attributes**

If a field is defined with the BINARY-LARGE-OBJECT or CHARACTER-LARGE-OBJECT attribute, only the OCCURS and MINLOBE attributes can also be assigned to the field.

**Handling NULL support for Large Object data**

NULL support for Large object data is handled like the NULL support for string data. If you change Large Object data to null using:

*   Single quotation marks (''), Table B stores the null and Table E deletes the former value.
*   A trailing an equal sign (=), the former value is deleted from both Table B and Table E.
*   A source that is zero (0), the former value is deleted from both Table B and Table E.

**DEFAULT-VALUE attribute**

The DEFAULT-VALUE attribute specifies the value to use for the field if the record is created and no value has been assigned to the field. The value of the STORE-DEFAULT attribute determines whether the DEFAULT-VALUE value is physically stored on the record or if it is just used as the default value when the field is missing.

**FLOAT attribute**

The FLOAT attribute is typically used if the field stores data in floating point representation.

**FOR EACH VALUE attribute**

The FOR EACH VALUE attribute maintains a list of all the unique values created for a field. This list can be accessed by using a value loop statement, such as, FOR EACH VALUE, FOR K VALUES, or FIND ALL VALUES.

**INVISIBLE attribute**

The INVISIBLE attribute is required if the field is to be used in FILE RECORDS statements. It also can be used for other fields that have the KEY, NUMERIC RANGE, or ORDERED attribute. Such fields can be used for retrievals but cannot be used where the VISIBLE attribute is required. An INVISIBLE field cannot be used in an arithmetic expression, as a key in a SORT statement, or in the AUDIT, COUNT OCCURRENCES, DELETE EACH, NOTE, PRINT, SET HEADER, and SET TRAILER statements. An INVISIBLE field also cannot be used in any form of the CHANGE or DELETE statement that is not followed by a fieldname = value pair.

**KEY attribute**

The KEY attribute specifies that if the field is to be used in FIND statement conditions of the form: fieldname = value, the Table C index is searched, rather than the data in Table B. Thus, the selection of records based on KEY fields is substantially more efficient than selection based on NON-KEY fields. In addition, either the KEY attribute or the ORDERED attribute is required if the field is to be used in FILE RECORDS statements.

**LENGTH attribute**

The LENGTH attribute specifies the preallocated length of a field occurrence in a record.

**NON-DEFERRABLE attribute**

The NON-DEFERRABLE attribute causes a KEY, NUMERIC RANGE, or ORDERED retrieval field's index entries to be created immediately when the file is open in deferred update mode. All index entries are created immediately when a file is not in deferred update mode. The field cannot be located until its index entry has been created.

**NUMERIC RANGE attribute**

The NUMERIC RANGE attribute specifies that if the field is used in a retrieval statement that performs a numeric retrieval, the Table C index is searched, rather than the data in Table B. Numeric retrievals based on fields with the NUMERIC RANGE attribute are more efficient than numeric retrievals based on NON-RANGE fields. A NUMERIC RANGE field cannot be multiply occurring.

**OCCURS attribute**

The OCCURS attribute specifies the number of occurrences of a multiply occurring field that are preallocated in a record.

**ORDERED attribute**

The ORDERED attribute specifies that the Ordered Index can be searched rather than the data in Table B for most types of retrieval. Thus, the selection of records based on ORDERED fields is substantially more efficient than selection based on NON-ORDERED fields. A field with the ORDERED attribute also produces other efficiencies in User Language. For example, when the IN ORDER option is used on a FOR EACH RECORD statement, an internal sort is not required if the field is ORDERED. The ORDERED attribute also allows a field to be used with any value loop statement, such as, FIND ALL VALUES, FOR EACH VALUE, and FOR K VALUES. In addition, either the ORDERED attribute or the KEY attribute is required if a field is to be used in FILE RECORDS statements.

**STORE-DEFAULT attribute**

See DEFAULT-VALUE attribute, above.

**UNIQUE attribute**

The UNIQUE attribute automatically enforces a uniqueness constraint on fields; it ensures that a given field name = value pair occurs only in one record in a file.

**VISIBLE attribute**

The VISIBLE attribute is required if the field is to be used in NOTE, PRINT, or SORT statements, or in an arithmetic expression.

**UPDATE attribute**

The UPDATE attribute indicates the type of update method that is used when a field is changed. If UPDATE IN PLACE is specified, changing the value of a field occurrence does not change its position relative to other occurrences of the same field. The file manager usually specifies UPDATE IN PLACE. If UPDATE AT END is specified, a change in the value of a field occurrence is accomplished by deleting the existing occurrence and adding a new one following the others.
