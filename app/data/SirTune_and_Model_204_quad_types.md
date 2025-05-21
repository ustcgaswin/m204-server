## SirTune and Model 204 quad types

The basic unit of work in compiled SQL is the *quad*. Every SQL statement generally one or more quads, by breaking down Model 204 processing by quad type (as occurring in a report).  It is important to know what the individual quads do.  If function quads are not listed they are implied since they are handled by function name. Other quads are listed here with a brief description of their function.

**Model 204 quads**

* **ADD** Add a field/variable pair to a record. Can be an INSERT statement, part of a CHANGE statement or one line in a STORE RECORD statement.
* **ADGV** Add a field/variable pair to a record for the IN-ADGV statement.
* **AMAD** Perform an arithmetic addition in an expression (possibly in an in-statement).
* **AMQD** Perform an arithmetic multiplication in an expression.
* **AMUL** Perform an arithmetic multiplication in an expression.
* **ASSGN** Assign a variable consistently in a possibly a string expression to a result variable.
* **ASSQ** Assigns integer constant in a possibly a string expression to a string variable.
* **ASSQ** Assigns result value in the after part of a numeric expression to a numeric variable.
* **ASSQ** Assigns result value in the after part of a numeric expression to a string variable.
* **BACKOUT** The BACKOUT statement.
* **BALD** The part of the statement that actually shows data in the journal.
* **BASE** Handles the COMBINE and in-statements.
* **BIND** Handles and field - 2 or more SQL in a FIND statement.
* **BLKR** Handles the WHILE loop in a FIND statement or an ordered character field.
* **BLK** Handles the WHILE loop in a FIND statement or an ordered character field.
* **BLMT** Handles the WHILE loop in a FIND statement or an ordered character field.
* **BMDR** The part of a FIND statement as part of with field using a list in a list.
* **BRANCH** Branch a field in a FIND statement or an indexed field on a substatement.
* **BRNCH** Handles a range condition for an indexed field on a substatement.
* **BWAL** Delete a field/variable pair from a record for the CHANGE statement.
* **CHOW** The CLEAR GLOBALS statement.
* **COMMIT** The COMMIT statement.
* **COUNT** Count number of entries of sort or found in each FIND and PRINT COUNT statements. Also used in FIND statements to determine if a DVRWT message should be used for a table (direct search).
* **CSDT** The COUNT DATA statement.
* **CT10** The EXEC SQL statements.
* **DE10** The EXEC SQL statements.
* **DE10** The EXEC SQL statements.
* **DEQRT** Unlock a record after deleting from it - DELETE RECORDS INON statement.
* **DIRK** Handles R-5, 15, and other items and conditions in a FIND statement.
* **DOPER** Part of a FIND look and field/variable pair for the ordered character field.
* **DOTEST** Handles an in-statement.
* **OPRES** Handles an in-statement.
* **ENOSET** Handles an in-statement.
* **PREPARE** The PREPARE statement.
* **IMAGE** The IMAGE statement.
* **POSQ** The position statement.
* **PRINT** The PRINT statement.
* **PRINTO** The PRINT ALL INFORMATION (PRINT) statement.
* **PRNTCL** Prints a field value in the FIND statement. The statement was immediately preceded by a number indicating maximum number of lines.
* **PMQOP** Copying a page map, use in PLACE RECORDS and REMOVE RECORDS statements and if FIND statements on previously loaded cards or lines.
* **PMQOP** Part of a FIND statement, especially existence map to load records.
* **PSTAL** Each line being parsed in a PRINT statement.
* **PSTRT** Print a string, used in PRINT statements.
* **QEND** End of a FOR loop.
* **QEND** End of a loop or multi-file variable segment. Discusses FIND, DELETE RECORDS, IN, FILE, SORT, RECORDS, PLACE, REMOVE, and many other statements in group control. Associated with a GLOOP quad.
* **QEND** End of a loop or multi-file variable segment.
* **REALL** The RELEASE RECORDS statement.
* **REMBR** The REMEMBER RECORDS statement.
* **REMSD** The REMOVE statement.
* **RESCT** The reset of the header statement for a DELETE RECORDS statement.
* **RHEDR** Remove the header from a record with looking up a field/variable pair in a table. Never occurs in a group context.
* **SAVE** Save involved data in table associated with the ordered index for a FILE RECORDS table. Never occurs in a group context.
* **SETHD** Start of SET HEADER statement.
* **SETIM** Start of SET RECORDS statement. Empty record in table and add hash or set key in hash or sort key file.
* **SETIM** Start of SET RECORDS statement. Empty record in table and add hash or set key in hash or sort key file.
* **SGLOOP** Start of nested record statement for remote file.
* **SGLOOP** Not used.
* **SORT** Sort operations.
* **SORCMP** Set of DELETE records statement to open a map for a FIND statement.
* **STOR** The STORE RECORD statement.
* **STRND** Not used.
* **STRNG** Converts a value in a field into an expression to a string value.
* **SUBS** Retrieves a point value based on exception.
* **TAG** TAB parameters in a PRINT statement.
* **UTIM** Start of UPDATE RECORD statement for remote file.
* **UTIM** Start of UPDATE RECORD statement for remote file.
* **W2AR2X** Start a PRINT statement.
* **WHNG** With parameter, where x = 3, 4, 5, AUDIT, SET HEADER, or SET TRAILER statement.
