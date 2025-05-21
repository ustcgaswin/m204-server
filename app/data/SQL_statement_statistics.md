## SQL statement statistics

**Overview**

This topic describes Model 204 statistics that are specific to Model 204 SQL processing. For complete information on statistics and journal formats, see [Tracking system activity](Tracking system activity).

**SQL statement statistics**

Model 204 SQL processing generates a since-last statistics line after each Prepare, Execute, Open Cursor, logoff, and terminate. The PROC field in the statistics output contains the SQL statement name. The LAST label indicates the type of statement:

| This label... | Indicates... |
|---|---|
| LAST = PREP | SQL Prepare statement (compilation of an SQL statement) |
| LAST = EXEC | One of the following: <br> * SQL Execute (execution of a compiled statement) <br> * Execute Immediate (combined compilation and execution) <br> * Open Cursor (execution of the SELECT statements associated with a cursor) statement |


**HEAP and PDL high-water marks**

The HEAP statistic indicates the high-water mark for dynamic memory allocation (MALLOC) for processing of C routines. This space is used by the SQL compiler/code generator. The SQL evaluator also uses the C heap for its runtime stack. Because dynamic memory is allocated before the system calls the SQL evaluator, the HEAP high-water mark does not change during a unit of SQL statement execution. For user subtype '01' entries, the offset of the HEAP statistic is X'58', immediately following the OUTPB statistic.

**Interpreting RECDS and STRECDS statistics**

The RECDS statistic (also used to count records processed by Model 204 FOR and SORT statements) indicates the number of physical cursor advances in an SQL base table and the number of records input to sort. The STRECDS statistic indicates the number of records input to SQL sort. Multiple fetches of the same physical record without intervening cursor movement count as one read. For example, a sequential fetch of several nested table rows within a parent row increments the RECDS statistic by one. Output sort records from SQL sort are not counted in the RECDS and STRECDS statistics.

**Interpreting the PBRSFLT statistic**

The Model 204 SQL sort uses a DKBM private buffer feature to increase the number of concurrent open buffers a user can hold. Use of the private buffer requires prior reservation. The PBRSFLT statistic indicates how many times a user failed to get a private buffer reservation. In Model 204 SQL, reservation of this buffer is conditional, and the PBRSFLT counter is incremented only for unconditional reservations. Therefore, PBRSFLT is always zero for current SQL users. Failure to obtain this buffer is not reflected in the statistic, but rather is indicated by a negative SQL code and message. In the record layouts, PBRSFLT follows the SVPAGES statistic and is generated for system subtypes '00' and '01'. For example, the offset of PBRSFLT for system type '00' entries is X'1E0'.

**Interpreting the SQLI and SQLO statistics**

The SQLI and SQLO since-last statistics record the high-water marks for the SQL logical input and output record lengths, respectively. They indicate the size of the largest SQL request to the Model 204 SQL Server and the size of the largest response from the SQL Server. You can use SQLI and SQLO to size the buffers that receive and transfer Model 204 SQL requests and responses. These buffer sizes are set by the Model 204 CCAIN parameter SQLBUFSZ and the Model 204 DEFINE command parameters INBUFSIZE.

**See also**

* Networking concepts and terminology
* Runtime environment for SQL processing
* SQL network entities
* Horizon TCP/IP
* SQL statement statistics
* SQL Server communications errors
* SQL connectivity security

**Category:** SQL connectivity
