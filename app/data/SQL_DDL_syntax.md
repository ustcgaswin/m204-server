## SQL DDL syntax

**Overview**

This topic outlines the syntax of Model 204 SQL DDL. Consult the American National Standards ANSI X3.135-1989 Database Language SQL document for details not outlined here.  The following rules apply:

1. SQL object names (schema, table, etc.) conform to SQL identifier rules (max 18 characters, not an SQL keyword).
2. End of line is implementation-defined (semicolon in some cases).
3. Numbers in brackets after syntax lines are keys to notes.
Model 204 SQL DDL extensions to the standard are in bold italics; statements/clauses not in ANSI SQL 1989/1992 standards, but anticipated, are in italics.


**DDL syntax**

```
tablename ::=
[schemaname.] <table>
table ::=
basetable | viewname
schema ::=
CREATE SCHEMA <schema-authorization-clause>
[schema-element...]
schema-authorization-clause ::=
schemaname AUTHORIZATION authorization-id[1]
schema-element ::=
<table-definition>
<view-definition>
<privilege-definition>
table-definition ::=
CREATE TABLE <tablename>
[<file-mapping-clause | NESTED USING <nested-key>]
(
<table-element>,...)
file-mapping-clause ::=
SYSNAME 'filename'
nested-key ::=
columame
table-element ::=
<column-definition>
<table-constraint-definition>
column-definition ::=
columname <datatype>
[<field-mapping-clause>]
[<column-constraint>]
datatype ::=
CHAR[(length)][3]
| NUM[(precision [, scale])][4]
| DEC[(precision [, scale])]
| INT[EGER]
| SMALLINT
| FLOAT[(precision)][5]
| REAL
| DOUBLE PRECISION
| BLOB
| CLOB
field-mapping-clause ::=
SYSNAME 'fieldname'
column-constraint ::=
NULL
<unique-specification>(7)
<references-specification>
table-constraint-definition ::=
<unique-constraint-definition>
| <referential-constraint-definition>
unique-constraint-definition ::=
<unique-specification> (<column-list>)
[SYSNAME 'fieldname'][8]
unique-specification ::=
UNIQUE | PRIMARY KEY [SYSTEM]
referential-constraint-definition ::=
FOREIGN KEY (columname)
<references-specification>
references-specification ::=
REFERENCES parent-table-name [9]
[<referential-triggered-action>]
referential-triggered-action ::=
<update-rule> [ <delete-rule>]
| <delete-rule> [<update-rule>]
update-rule ::=
ON UPDATE CASCADE
delete-rule ::=
ON DELETE CASCADE
view-definition ::=
CREATE VIEW <viewname> [(<column-list>)]
AS <query-expression> [WITH CHECK OPTION] [10]
set user statement ::=
SET USER authorization-id[11]
set schema statement ::=
SET SCHEMA schemaname
drop schema statement ::=
DROP SCHEMA schemaname[12]
drop table statement ::=
DROP TABLE <tablename>[12]
drop view statement ::=
DROP VIEW <viewname>[12]
alter table statement ::=
ALTER TABLE <tablename> <alter-table-action>
alter-table-action ::=
ADD <column-definition>[13]
| DROP columname
| MODIFY <column-parameters>
column-parameters ::=
MODIFY columname[14]
[<datatype>]
[<field-mapping-clause>]
[<modify-attribute-list>]
modify-attribute-list ::=
[NOT NULL]
[NOT] UNIQUE
privilege-definition ::=
GRANT <privileges> ON <object-name>
TO <grantee>
[WITH GRANT OPTION]
privileges ::=
ALL PRIVILEGES
<action>
action ::=
SELECT
| INSERT
| DELETE
| UPDATE [(<column-list>)] [15]
column list ::=
columname,...
object-name ::=
<tablename> | <viewname>
grantee ::=
PUBLIC
authorization-id
revoke statement ::=
REVOKE [GRANT OPTION FOR ] <privileges>
ON <object-name>
FROM <grantee>
```

**Notes for syntax display**

(Footnotes referencing the syntax elements are included in the text.)


**See also**

* SQL Server overview
* SQL Server installation
* SQL catalog
* SQL representation of Model 204 data
* SQL Data Definition Language (DDL)
* SQL DDL from the Table Specification Facility
* SQL catalog reporting and querying
* SQL Data Manipulation Language (DML)
* SQL DDL syntax
* SQL reserved words
* SQL DDL mapping of the demonstration database
