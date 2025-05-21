## Transaction back out

**Overview**

Transaction back out is a mechanism that can apply the effects of incomplete transactions on the database. This typically involves a transaction being rolled back to a prior state.

**Components of the Transaction back out facility**

| Component | Description |
|---|---|
| Lock pending updates | When complete, updates to a transaction back out or updated records are isolated in a transaction exclusively. Each updated record is the first update in the record, when the transaction back out is applied, the record is isolated for exclusive update. |
| Back Out mechanism | The back out mechanism basically stores the update in an update log by housing temporary updates. For example, if a transaction is rolled back, the system automatically performs a back out operation on the transaction. |


**Benefits of Transaction back out**

* Elevating availability of the system by allowing the application to recover from errors.
* Providing a mechanism for managing transaction errors.
* Maintaining data consistency.
* Reducing the risk of data corruption.


**Concurrency control and locking mechanisms**

Concurrency control manages and serializes access to data so that concurrently running programs do not interfere with each other.


**User control of lockings**

* Monitor user transactions to determine when to release the lock.
* Lock pending updates to prevent conflicts.


**When to use transaction back out**

* When a transaction is designed to store data for a longer period.
* When a transaction involves multiple steps.
* When a transaction involves complex logic.


**Using lock pending updates**

* A lock pending update file. The first update in place is exclusive on the record. The record is isolated for exclusive update. The record is reflected back in the exclusive file for the record updated.


**Locking mechanism**

* The locking mechanism prevents updates in one transaction from being applied to other transactions until the transaction is complete.


**Updates that do not back out**

* Updates that do not back out are those that are not part of a transaction.


**Deleting records**

* When a record is deleted, the record is removed from the database.


**Recovery and transaction back out**

Transaction back out is important for maintaining data consistency and integrity.


**Operating considerations**

* Application or system errors that cause transaction back out.
* Managing a system that has transaction back out.


**Transaction back out file parameter settings**

* The parameter has to be set to off during lock pending updates.


**Turning off transaction back out**

* The transaction back out is controlled by a parameter.


**Logical consistency and records in file records**

* Although transaction back out typically affects only the records involved in the transaction, there are some possible exceptions.


**Example**

| User 1 | User 2 |
|---|---|
| ... | ... |


**Transaction back out logging**

* The transaction back out log contains information on the back out operation.


**Transaction back out log entries and LOGFMT**

* The log entries contain details about the transaction back out.


**Transaction back out log**

* The number of log pages.


**Concurrency log**

* DELETE entries.


**Automatic transaction back outs**

* SQL.
* Model.
* User restarts.


**Back out with caused conditions**

* The request is caused by a SQL error.
* The request is caused by a table lock.


**Using ONs with transaction back outs**

* The transaction back out is active to control the outcome.


**Transaction back out file parameter settings**

* The parameter has to be set to off during lock pending updates.


**Completed transactions**

* The transaction is completed.


**Hold for compound transactions**

* The transaction is held for later processing.


**Effects of transaction back out on files**

* The effects of transaction back out on the file.


**Updates that add data**

* Updates that add data to a file.


**Updates that change data**

* Updates that change data in a file.


**Updates that build extend records**

* Updates that build extend records in a file.


**Transaction back out**

* The transaction back out mechanism.


**Performance considerations for transaction back out and performance logs**

* Transaction back out and performance logs.


**Technical considerations**

* Software with transaction back out.


**Considerations when using transaction back out**

* Transaction back out may have advantages, but it can also have disadvantages.


**Updates to TCO and also same file request**

* The same file request.


**Using lock pending updates**

* Lock pending updates options.


**Using the back out mechanisms**

* Back out request.


**Required back outs in SQL**

* SQL application.


**Reporting back out through language interface**

* Reporting back out through language interface.


**Automatic back outs**

* SQL.
* Model.
* User restarts.


**User restarts**

* Back out with caused conditions.


**Handling requests with attributes**

* The request is handled with attributes.


**Using ONs with transaction back outs**

* The transaction back out is active to control the outcome.


**Transaction back out file parameter settings**

* The parameter has to be set to off during lock pending updates.


**Logical consistency and records in file records**

* Logical consistency and records in file records.


**Example**

| User 1 | User 2 |
|---|---|
