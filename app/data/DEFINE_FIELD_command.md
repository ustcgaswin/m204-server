## DEFINE FIELD command

**Summary**

*   A command in Rocket Software's system.

**Syntax**

```
DEFINE FIELD <field_name> <field_attributes>
```

**Field Attributes**

| Attribute | Abbreviation | Default |
|---|---|---|
| A-Code | AC |  |
| A-Range | AR |  |
| ARRAY | ARRAY |  |
| B-Code | BC |  |
| B-Range | BR |  |
| BYTE | BYTE |  |
| CHARACTER | CHAR |  |
| CHARACTER, UNDER OBJECT | C_UO |  |
| CODE | CODE |  |
| DATETIME | DT |  |
| DATETIME, ENABLED | DEF |  |
| FLOAT | FLOAT |  |
| INTEGER | INT |  |
| KEY | KEY |  |
| LENGTH | LEN |  |
| LINE | LINE |  |
| LONG | LONG |  |
| MAXVALUE | MAX |  |
| MINVALUE | MIN |  |
| NAME | NAME |  |
| NON-CODE | NCODE |  |
| OBJECT | OBJ |  |
| OBJECT, CHARACTER | O_CHAR |  |
| OBJECT, MEMBER | O_MEMB |  |
| OBJECT, MEMBER, CHARACTER | O_MEMB_CHAR |  |
| OBJECT, MEMBER, MEMBER | O_MEMB_MEMB |  |
| OBJECT, MEMBER, OBJECT | O_MEMB_OBJ |  |
| OBJECT, MEMBER, STRING | O_MEMB_STR |  |
| OBJECT, STRING | O_STR |  |
| OPERATE AT PLACE | OP |  |
| OPERABLE | OPER |  |
| PAGE | PAGE |  |
| PATH | PATH |  |
| PICTURE | PIC |  |
| PRIMARY | PRI |  |
| REAL | REAL |  |
| RECORD | REC |  |
| REQUIRED | REQ |  |
| SHORT | SHORT |  |
| STRING | STR |  |
| STORE-FULL | STORE |  |
| STORE-HALF | STOREH |  |
| STORE-OBJECT | STOBJ |  |
| STORE-STRING | STRS |  |
| STRING, CHARACTER | STR_CHAR |  |
| STRING, OBJECT | STR_OBJ |  |
| STRING, MEMBER | STR_MEMB |  |
| STRING, MEMBER, CHARACTER | STR_MEMB_CHAR |  |
| STRING, MEMBER, MEMBER | STR_MEMB_MEMB |  |
| STRING, MEMBER, OBJECT | STR_MEMB_OBJ |  |
| STRING, MEMBER, STRING | STR_MEMB_STR |  |
| STRING, OBJECT, CHARACTER | STR_OBJ_CHAR |  |
| STRING, OBJECT, MEMBER | STR_OBJ_MEMB |  |
| STRING, OBJECT, MEMBER, CHARACTER | STR_OBJ_MEMB_CHAR |  |
| STRING, OBJECT, MEMBER, MEMBER | STR_OBJ_MEMB_MEMB |  |
| STRING, OBJECT, MEMBER, OBJECT | STR_OBJ_MEMB_OBJ |  |
| STRING, OBJECT, MEMBER, STRING | STR_OBJ_MEMB_STR |  |
| STRING, OBJECT, STRING | STR_OBJ_STR |  |
| STRING, STRING | STR_STR |  |
| SUMMARY | SUM |  |
| SUMMARY, ARRAY | SUM_ARRAY |  |
| SUMMARY, OBJECT | SUM_OBJ |  |
| SUMMARY, OBJECT, CHARACTER | SUM_OBJ_CHAR |  |
| SUMMARY, OBJECT, MEMBER | SUM_OBJ_MEMB |  |
| SUMMARY, OBJECT, MEMBER, CHARACTER | SUM_OBJ_MEMB_CHAR |  |
| SUMMARY, OBJECT, MEMBER, MEMBER | SUM_OBJ_MEMB_MEMB |  |
| SUMMARY, OBJECT, MEMBER, OBJECT | SUM_OBJ_MEMB_OBJ |  |
| SUMMARY, OBJECT, MEMBER, STRING | SUM_OBJ_MEMB_STR |  |
| SUMMARY, OBJECT, STRING | SUM_OBJ_STR |  |
| SUMMARY, STRING | SUM_STR |  |
| UNIQUE | UNIQUE |  |
| UPDATE TIME | UPTIME |  |
| UPDATE FIELD | UPFIELD |  |
| VARIABLE | VAR |  |
| VISIBLE | VIS |  |
| VALUE | VAL |  |
| X-Code | XC |  |
| X-Range | XR |  |


**Usage Notes**

*   Detailed instructions and examples for using the command.

**Indexed Attributes**

*   Information about indexed attributes.

**Specifying the date/time field name**

*   Instructions on specifying date/time field names.

**Defining the date/time field format**

*   Instructions on defining the date/time field format.

**Safety fields used on SHARP objects**

*   Information about safety fields used on SHARP objects.

**Example**

```
DEFINE FIELD TEST_FIELD CHAR LENGTH 20
```
