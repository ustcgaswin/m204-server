## MSG command

**Summary**

Privileges
Any user (note that certain kinds of messages can be sent only to and from the operator)
Function
Sends messages to and from the operator and the user, and between two or more users

**Syntax**

MSG {userid | usernumber | OPR} text

**Where:**

* **userid** is the name of a Model 204 user ID, specified as an alphanumeric string; if this is specified, all users who have logged in under that user ID are sent the message.
* **usernumber** is the number associated with a particular user.
* **OPR** represents the system operator.
* **text** is the message to be sent. For operator messages, the maximum length of the message is 99 characters. For other messages, the maximum length is 255 characters.

**Syntax notes**

Multiple lines of text are specified in the form:
line1/line2/...

where the slash character (/) indicates a new line or a carriage return/line feed sequence.
A line is continued by specifying any nonblank character in the continuation column number specified by the INCCC parameter.

**Example**

MSG OPR WHAT TIME WILL THE SYSTEM COME DOWN?
The following command displays a blank line, followed by the specified text, followed by another blank line:
MSG BAKER/DONT FORGET, BACK UP ALL FILES TODAY/

**Usage notes**

The MSG command allows Model 204 users to carry on dialogs with each other or with the system operator.
Messages can be sent:

* From the operator to a user or to all current users of a login user ID
* From a user to the operator
* From one user to another user or to all current users of a login user ID

User IDs, which are specified by the system manager, can be all-numeric strings. User numbers, which are supplied by Model 204, are always all-numeric. When an all-numeric string is specified, Model 204 first attempts to find a matching user ID. If no match is found, Model 204 then assumes that the string refers to a user number.
When a message is sent to a user, the text is displayed on the user's terminal the next time that the user is at command level outside a procedure. When a message is sent to the operator, the text is displayed immediately.
The message is preceded by a header line in one of the forms:
MESSAGE FROM OPR
MESSAGE FROM user number
