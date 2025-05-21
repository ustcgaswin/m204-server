## Overriding FTP protocol commands

For custom processing, you can develop a User Language procedure to replace the native Janus FTP Server implementation of an FTP operation.

You may override the following FTP operations:

| FTP command | Operation |
|---|---|
| STOR | FTP put |
| RETR | FTP get |
| RNFR/RNTO | FTP rename |
| DELE | FTP delete |
| LIST | FTP folder list (ls, dir) |


### The Override API Functions

The override functions provide information and control to write overrides.  They take no arguments. If called on a non-FTP Server thread, they do nothing and return either a zero-length string or 9, depending on the function's return type. This allows for syntax error checking before running on the server.

* **SFtp_Fail:** Returns a failure status code (500) to the FTP client when the override finishes.
* **SFtp_Get_Command:** Returns the FTP command that is the subject of the override.
* **SFtp_Get_Current_File:** Returns the Model 204 file mapped to the current folder.
* **SFtp_Get_Current_Folder:** Returns the name of the current folder.
* **SFtp_Get_Operand:** Returns the value of the entity being operated on.
* **SFtp_Get_Old_Name:** Returns the name of the procedure being renamed.
* **SFtp_Get_Prefix:** Returns the prefix character if the folder is prefixed; otherwise, a zero-length string.
* **SFtp_Get_Transfer_Type:** Returns the current FTP transfer mode.


### Writing Overrides

The following sections present requirements for an override for a particular operation, along with applicable override API functions.

#### Overriding STOR (put)

Before an override for STOR is called, Janus FTP obtains the file from the client and places it in temporary procedure 0. The override is responsible for storing it.  `$Ftp_Get_Operand` returns the name of the procedure being stored.

#### Overriding RETR (get)

Any data to be sent back to the FTP client must be placed in temporary procedure 0. After the override commands are executed, the contents of temporary procedure 0 are sent to the FTP client. `$Ftp_Get_Operand` returns the name of the procedure being fetched.

#### Overriding RNTO (rename)

In a rename override, `$Ftp_Get_Operand` returns the new name, and `SFtp_Get_Old_Name` returns the old name.

#### Overriding DELE (delete)

In a delete override, `$Ftp_Get_Operand` returns the name of the item to be deleted.

#### Overriding LIST (ls, dir)

A LIST override must place the listing in temporary procedure 0.  `$Ftp_Get_Operand` returns the wildcard pattern or a null string if no pattern is specified.  The override must handle narrow and wide list modes.

### See also

The following topics complete the description of Janus FTP Server support:

* Janus FTP Server
* Janus FTP Server examples
* Janus FTP Server command reference
* Integrating Janus FTP and Janus Web Server