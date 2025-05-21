## VTAM PRINTER commands

This topic describes the VTAM PRINTER commands for the VTAM Printer Support (VPS) feature.

**DEFINE VTAM PRINTER command**

**Syntax:**

DEFINE VTAM PRINTER Juname FOR qname (option a)

STATIC (DYNAMIC delay n)
RETENTION=NONE | n
FORMS=NONE fname
OPERATOR=NONE opr
EJECT NOEJECT
FF NOFF
FIRST NOFIRST
TIME1=t1
TIME2=t2
COUNT1=c1
COUNT1=c2

**Description:**

To define the VTAM printer, use the VTAM PRINTER DEFINE command with the following parameters:

*   luname is the defined VTAM printer's logical unit name, gname must be a previously defined print queue name, which has been defined with this luname.
*   STATIC automatically acquires a session with a logical unit. DYNAMIC delays the acquisition of a session until a request is ready from the print queue. After the initial print request, the system waits the specified time (n) for the next print request. If the time expires, the session is terminated and does not acquire a new session until a request is ready from the print queue.
*   RETENTION (optional) is the time in minutes that a print request is retained before purging from the queue. This option overrides the retention parameter specified in the DEFINE PRINT QUEUE command.
*   FORMS defines a form, where fname is a 4-character forms name to be used with this printer. This option overrides any FORMS specified with the DEFINE PRINT QUEUE command. The default is NONE, which bypasses any request for a forms change for the VTAM printer.
*   OPERATOR designates a specific user, who is allowed to execute the READY VTAM PRINTER command or purge requests in a print queue. If you specify NONE (the default), only the creator of a print request can purge that request and only the system manager can ready a VTAM printer. If a specific user is specified, that user can purge a print request and ready a VTAM printer. System managers can use any print queue command, regardless of this specification.
*   EJECT spaces a page between requests (the default). NOEJECT eliminates page spacing between requests.
*   FF indicates that form feeds are needed for 328X type printers. NOFF indicates a non-328X type printer (the default).
*   FIRST causes a page eject before printing (the default). NOFIRST bypasses an initial page eject.


**DELETE VTAM PRINTER command**

**Syntax:**

DELETE VTAM PRINTER Juname | FOR qname

**Description:**

The DELETE VTAM PRINTER command deletes a defined printer by means of either luname or qname. The printer must be in an idle, clear, or suspended state. To suspend the printer, use the SUSPEND VTAM PRINTER command.


**DISPLAY VTAM PRINTER command**

**Syntax:**

DISPLAY VTAM PRINTER (ALL | FOR qname | IDLE | IREQ | Juname | SUSPEND | VTAM)

**Parameter descriptions:**

To display the status of VTAM printers, use the DISPLAY VTAM PRINTER command with the ALL parameter (the default). To select individual printers, use the following parameters:

*   FOR gname specifies an associated queue name.
*   IDLE displays printers in the clear state.
*   IREQ displays printers with intervention required.
*   luname (lu) specifies a particular logical unit.
*   SUSPEND displays all suspended printers.
*   VTAM displays session waiters.

**Attribute descriptions:**

Each display comprises the status of the following attributes:

*   luname is the VTAM printer's logical unit name.
*   gname is the VTAM printer's associated queue name.
*   RETM is the retention time specified in the DEFINE VTAM PRINTER command.
*   ST displays the current state of the printer. The states can be:

    *   CLE: Clear state
    *   FWT: Forms wait
    *   Idle: Idle state
    *   IVR: Intervention required
    *   PRT: Currently printing
    *   SUS: Suspend state
    *   SWT: VTAM session wait
    *   TWT: Session termination wait


**READY VTAM PRINTER command**

**Syntax:**

READY VTAM PRINTER Juname

**Description:**

The READY VTAM PRINTER command restarts the printer pseudo-subtask after one of the following:

*   Redefine of the printer with special forms specifications.
*   Required intervention on the physical printer.


**REDEFINE VTAM PRINTER command**

**Syntax:**

REDEFINE VTAM PRINTER (option a)

luname FOR qname
STATIC (DYNAMIC delay n)
QUEUE=new gname
RETENTION=NONE | n
FORMS=NONE fname
OPERATOR=NONE | opr
EJECT NOEJECT
FF NOFF
FIRST NOFIRST
TIME1=t1
TIME2=t2
COUNT1=c1
COUNT1=c2

**Description:**

The REDFINE VTAM PRINTER command allows you to change printer options for an existing defined printer. All options perform the same function as listed under the DEFINE VTAM PRINTER command, except the following:

*   luname | FOR qname allows you to enter either name.
*   QUEUE associates a new print queue with this printer using the qname parameter.
*   If you want to change the session characteristics (STATIC, DYNAMIC) or the queue, you must put the printer in the suspend state using the SUSPEND VTAM PRINTER command.


**RESUME VTAM PRINTER command**

**Syntax:**

RESUME VTAM PRINTER luname | FOR gname

**Description:**

The RESUME VTAM PRINTER command restarts a suspended printer by reestablishing a new session with the logical unit. Upon resuming, the printer enters the state as defined with the STATIC | DYNAMIC options of the DEFINE PRINTER or REDEFINE PRINTER command.


**SUSPEND VTAM PRINTER command**

**Syntax:**

SUSPEND VTAM PRINTER luname FOR qname
FORCE

**Description:**

You can suspend a defined VTAM printer by issuing the SUSPEND VTAM PRINTER command and specifying luname or qname. The printer must be in either the clear, idle, or wait state for the completion of the SUSPEND command. The FORCE option is another alternative, which forces the suspension. The printer enters the suspended state after the session with the logical unit has terminated.