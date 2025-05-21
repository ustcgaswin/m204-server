## VTAM Printer Support print queue commands

This topic describes the print queue commands for the VTAM Printer Support (VPS) feature.

**Contents**

1. **CREATEPQ command**
    * 1.1 Syntax
    * 1.2 Description

2. **DEFINE PRINT QUEUE command**
    * 2.1 Syntax
    * 2.2 Description

3. **DELETE PRINT QUEUE command**
    * 3.1 Syntax
    * 3.2 Description

4. **DISPLAY PRINT QUEUE command**
    * 4.1 Syntax
    * 4.2 Description

5. **DISPLAY PRINT QUEUE DEFINITIONS command**
    * 5.1 Syntax
    * 5.2 Description

6. **PURGE PRINT QUEUE command**
    * 6.1 Syntax
    * 6.2 Description

7. **REPRINT QUEUE command**
    * 7.1 Syntax
    * 7.2 Description

8. **REPRINT REQUEST command**
    * 8.1 Syntax
    * 8.2 Description

9. **RESUME PRINT QUEUE command**
    * 9.1 Syntax
    * 9.2 Description

10. **SUSPEND PRINT QUEUE command**
    * 10.1 Syntax
    * 10.2 Description

11. **USE PRINT QUEUE command**
    * 11.1 Syntax
    * 11.2 Description

**CREATEPQ command**

* **Syntax:** CREATEPQ (FROM CCAPRO)
* **Description:** Builds the print queue spooling file. Default DONAME is CCAPRQ.  FROM option to specify alternate DDNAME.  If SPOOLOPT=X01 in CCAIN, the spool file is automatically opened.

**DEFINE PRINT QUEUE command**

* **Syntax:** DEFINE PRINT QUEUE qname (option a)
* **Parameters:**
    * LIKE qname2
    * LENGTH lines
    * WIDTH columns
    * MAXIMUM
    * RETENTION
    * FORMS NONE fname
* **Description:** Defines the print queue, using parameters like queue name, characteristics of a previously defined queue, lines per page, columns per line, maximum USE print lines, retention time, and forms name.

**DELETE PRINT QUEUE command**

* **Syntax:** DELETE PRINT QUEUE qname
* **Description:** Deletes the specified queue.  Must purge the queue first to prevent requests from being queued to the VTAM printer.

**DISPLAY PRINT QUEUE command**

* **Syntax:** DISPLAY PRINT QUEUE (ALL | qname) option (a)
* **Options:**
    * REQUEST ID
    * CREATOR
    * DATE/TIME
    * LINES
    * RET (Retention)
    * ST (Status)
* **Description:** Displays print queue information.  ALL displays all information; qname displays information for a specific queue.  Options allow selection of specific requests.

**DISPLAY PRINT QUEUE DEFINITIONS command**

* **Syntax:** DISPLAY PRINT QUEUE DEFINITION (ALL | qname)
* **Description:** Displays definitions of a print queue.  Only System Manager can issue.  Displays gname, LENGTH, WIDTH, MAXIMUM, RETENTION, FORMS.

**PURGE PRINT QUEUE command**

* **Syntax:** PURGE PRINT QUEUE qname (WAITING | ALL | RETAINED)
* **Description:** Purges print requests from the queue.  ALL purges all requests, WAITING purges only waiting requests, and RETAINED purges requests with retention settings.

**REPRINT QUEUE command**

* **Syntax:** REPRINT QUEUE id | qname (RETENTION n)
* **Description:** Reprints the print queue.  id is the request ID, qname is the queue name, and RETENTION is the retention time.

**REPRINT REQUEST command**

* **Syntax:** REPRINT REQUEST id (RETENTION n)
* **Description:** Reprints a specified request.  id is the request ID, and RETENTION is the retention time.

**RESUME PRINT QUEUE command**

* **Syntax:** RESUME PRINT QUEUE (ALL | qname)
* **Description:** Reactivates a suspended queue.

**SUSPEND PRINT QUEUE command**

* **Syntax:** SUSPEND PRINT QUEUE (ALL | qname)
* **Description:** Halts activity in a print queue.  Queue enters suspended state until all activity is complete.

**USE PRINT QUEUE command**

* **Syntax:** USE PRINT QUEUE qname (RETENTION n)
* **Options:** DISCARD PRINT PARTIAL, FORMS fname
* **Description:** Spools print output to the specified queue.  RETENTION overrides queue definition, DISCARD PARTIAL purges partial output if queue is closed, and FORMS specifies an alternative form.


