## Horizon error processing

**Overview**

Debugging program-to-program errors is more difficult than debugging a single program, because there are more places where problems may occur.  There are two or more programs, running asynchronously at different nodes, and there are numerous network components connecting them. This topic discusses the detection and troubleshooting of typical Horizon error conditions and provides information for many of the Horizon error codes and messages.

**Detecting Horizon errors**

Unlike other SOUL statements, when a Horizon statement is executed, certain status information is used to indicate whether the statement completed normally. In cases of error, this status information may not only indicate the type of error but also contain information to detect and handle errors.

**Conversation status information**

Information about the current condition of a Horizon conversation is communicated to the SOUL program through the following means:

*   The RESULT variable (on RECEIVE statements).
*   The REQSEND variable (on SEND statements).
*   The QUERY PROCESS statement.

The execution of a Horizon statement always sets a $Status to some integer. Since each $Status code provides general information about the current condition of the conversation, the application program should include the descriptions of each individual $Status (and $StatusD) codes. When an error occurred ($Status 2 or greater), a Model 204 message is generated.

**Client versus server error detection and debugging**

Client processes usually run in a thread that has access to a terminal.  The exceptional situation is when a server process initiates a conversation (becomes a client process with a third partner). The presence of the terminal permits interactive debugging. The developer can insert PRINT statements into the client process routines to display status information about the conversation and to create break points, for example. A server process, on the other hand, does not have access to a terminal. Messages are only written to the audit trail or the operator's console. Debugging a server process usually involves embedding AUDIT (rather than PRINT) statements and browsing the audit trail.

**Troubleshooting Horizon errors**

This section lists general categories of Horizon errors, followed by discussions of typical debugging approaches for OPEN PROCESS errors and errors producing unanticipated conversation terminations.

**Types of errors**

There are several types of Horizon errors, corresponding to different values of $Status. The list below briefly describes the types of errors. All the $Status (and $StatusD) codes and messages are listed in a table format in "$Status/$StatusD codes and accompanying error messages".

*   State check ($Status=3): The program has issued a Horizon SOUL statement that is invalid for the current state of the conversation. State checks are described further in Horizon conversation data.
*   Partner closed the conversation ($Status=4): This occurs if a partner has terminated or issued a CLOSE PROCESS statement.
*   Parameter check ($Status=5): One or more parameters of a Horizon statement or Horizon entity definition is incorrect.
*   Horizon system component failures ($Status=10 or greater): This is generally not an application program bug. There are four kinds of system failures:
    *   Local allocation failure: A Horizon system component has failed or is unavailable.
    *   Remote failure: SNA Communications (formerly VTAM) could not be established because of a lack of resources at the node to which the connection was being attempted.
    *   Session failure: The session could not be established because of a lack of resources at the node where the program is running.
    *   Partner process error: This is an application-level error; it allows a program to create an error condition that is communicated to its partner. See "SEND ERROR statements" for details.

**OPEN PROCESS errors**

While many of the errors described in the previous section can occur throughout the life of a conversation, one error situation is likely to be the most frequent: a failure encountered during a client program's OPEN PROCESS statement.  Horizon statements simply cause LU 6.2 requests to be buffered physically. Transmission becomes necessary, which may result in shipping information of remote allocation failure later in the statement.

**OPEN PROCESS errors received immediately**

The types of errors typically received immediately following an OPEN PROCESS statement, and are the following:

*   State Check
*   Parameter Check
*   Retryable and Non-Retryable

**Troubleshooting conversation layers**

To determine where the fault is on the server side, the developer should examine each layer in turn. Examples follow of kinds of errors at each layer:

*   Conversation Allocation
*   Security
*   Network Definition
*   Application Subsystem

**Unanticipated conversation termination**

Another error that can occur at any time is an abnormal termination of conversation by the partner ($Status=4, $StatusD=1). A variety of events at the partner node can result in abnormal termination, including:

*   Bumping of the partner thread.
*   Termination of the partner process due to program errors.
*   Issuing of CLOSE PROCESS.

**$Status/$StatusD codes and accompanying error messages**

A table lists possible combinations of return values of the $Status/$StatusD functions and a brief description of each combination.  In addition to the $Status and $StatusD information, the $errmsg function can be used to retrieve the Model 204 error message numbers generated along with $Status and $StatusD. The message text associated with the message number (along with debugging hints) is also provided.
