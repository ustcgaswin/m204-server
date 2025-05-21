## REDEFINE command

**Contents**
1. Summary
2. Syntax
    2.1 Syntax notes
3. Example
4. Usage notes
    4.1 FIELD keyword
    4.2 ORDERED fields
    4.3 Redefining OI CHUNK fields
    4.4 Redefining INVISIBLE fields
    4.5 Update units
    4.6 Redefining Large Object fields

**Summary**
Modifies the definition of a field in a Model 204 file

**Syntax**
REDEFINE [FIELD]
{field-description [field-description]...
| fieldname WITH attribute [attribute]...}

**Where:**
* **field-description** Takes the following form:
    * fieldname
    * attribute
    * fieldname [(attribute [attribute]...)]
        * fieldname: The name (1 to 255 characters) of a field in the open Model 204 file.
        * attribute: One of those listed in the table below. All attributes are described in detail in DEFINE FIELD command and Field design.

**Field description attribute** | **Abbreviation**
---|---|
BINARY-LARGE-OBJECT | BLOB
CHARACTER-LARGE-OBJECT | CLOB
DEFERRABLE | DEF
NON-DEFERRABLE | NDEF
FEW-VALUED* | FV
MANY-VALUED | MV
FRV (for-each-value) | -
NON-FRV | NFRV
KEY | NKEY
NON-KEY | LVL k
LEVEL K | NR
[NUMERIC] RANGE | NNR
NON-RANGE | ORD
ORDERED | NORD
NON-ORDERED | UNIQ
UNIQUE | NUNIQ
NON-UNIQUE | UP
UPDATE IN PLACE | UE
UPDATE AT END | 

**Syntax notes**
* You can specify FEW-VALUED and MANY-VALUED only if a field is being redefined from NON-FRV NON-CODED to FRV NON-CODED.
* If FIELD is specified, or if the WITH form of the REDEFINE command is used, only one field can be redefined; otherwise, multiple fields can be redefined.
* The attributes must be enclosed in parentheses unless WITH is specified.
* Attributes must be separated by commas or by one or more blanks.
* REDEFINE can be specified only in file context.

**Example**
REDEFINE SOC SEC (KEY, NR, LEVEL 0)
REDEFINE AGE WITH KEY

**Usage notes**
* The REDEFINE command allows the file manager to alter a field definition without having to reinitialize or reload the file in which the field is stored.
* The file manager can specify as many attributes as are needed. ocCURS, LENGTH, and PAD (See DEFINE FIELD) cannot be included. For each attribute, only one of the alternatives shown above can be specified.
* If an attribute is specified in the DEFINE command, but not changed by the REDEFINE, that attribute remains in effect for the field.
* You can REDEFINE a field with the UNIQUE attribute. However, if non-unique values are found during REDEFINE processing, the REDEFINE fails, and the following error message is produced for each non-unique value:
    M204.1701: NON-UNIQUE VALUE value FOUND FOR FIELD fieldname IN RECORD NUMBER n; CONFLICTS WITH RECORD NUMBER n

**FIELD keyword**
The keyword FIELD is required before field names that begin with the word FIELD, PRINTER, or DATASET. It is optional before other field names. If FIELD is specified, only one field can be redefined per command.

**ORDERED fields**
The NON-ORDERED and ORDERED attributes relate to the Ordered Index. The NON-ORDERED attribute can be used to eliminate the Ordered Index for a given field. The ORDERED attribute followed by a new tree type can change the tree type of an ORDERED field. The tree type can be either CHARACTER or NUMERIC. Changing the tree type deletes the old tree type. If a field already is defined as ORDERED, the REDEFINE command can change the related attributes by specifying the attribute and a new value. A change to the LRESERVE, NRESERVE, SPLITPCT, or IMMED attribute does not cause any change to the existing Ordered Index but modifies the effect of any updates.

**Redefining OI CHUNK fields**
Redefining OI CHUNK fields or OI CHUNK target fields is not allowed.

**Redefining INVISIBLE fields**
You cannot use the REDEFINE command to add a new index to a field with the INVISIBLE attribute that is in a file that has been used, so that MSTRADD is nonzero. For an invisible field, the REDEFINE command cannot specify the KEY, ORD CHAR, and ORD NUM attributes nor the NUMERIC RANGE attribute, unless the field was originally defined with that attribute.

**Exception:** The following exception applies: in the case where no records have ever been stored in the file, and that case only, redefining an invisible field can specify options that add a new index for the field.

**Update units**
When it processes REDEFINE, Model 204 ends any update unit in progress and begins a non-backoutable update unit.

**Redefining Large Object fields**
You can redefine a Large Object field, if the file is empty and you specify that the that the field is (BLOB NCLOB) or (CLOB NBLOB).

**Categories:** File manager commands, Commands
