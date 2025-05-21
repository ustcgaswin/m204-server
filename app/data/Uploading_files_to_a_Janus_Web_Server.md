## Uploading files to a Janus Web Server

Using Janus Web Server, it is possible to create complete web applications using only the Model 204 editor.  With the Model 204 editor, it is possible to create static HTML pages and User Language procedures that dynamically generate HTML and accept input from HTML forms.

There might be a need to include binary data generated outside of Model 204 in an application. This data can include graphical images, audio data, Java applets, and other workstation-generated binary data types.

Historically, two mechanisms were mainly used for moving data from outside of Model 204 into procedures:

* Placing data into a BATCH2 (or BATCH204) stream between a PROCEDURE and END PROCEDURE statement.
* Using READ IMAGE and SBIdProc to create Model 204 procedures.

The latter mechanism is cumbersome. The former mechanism has problems uploading binary or text data with long records.

The SIRPIPE and SIRPUT utilities were designed for loading binary and text data into Model 204.  SIRPIPE uses BATCH2 or BATCH204, while SIRPUT uses HTTP PUT.  This is a two-step process:

1. A file upload program moves the data from a workstation to an operating system file.  FTP is a common method.
2. SIRPIPE or SIRPUT moves the data from the operating system file into Model 204.

To service workstation-initiated file transfers, Janus Web Server provides HTTP PUT support and form-based file upload support.  An alternative is the Janus FTP Server, which allows uploads and downloads directly to and from Model 204 procedure files.

### HTTP PUT support

HTTP PUT support adds Web Server handling of the PUT method, which uploads data to the server.  JANUS WEB ON rules control uploading to Model 204.  The PUT method is part of the HTTP 1.1 standard.

### Form-based uploads

Form-based uploads work with HTML forms.  An HTML form can have an input field with a type of "file."  The content of the field is the file name and path.  The `enctype` attribute in the form tag is crucial for file uploads (`enctype="multipart/form-data"`).

Janus Web Server handles different encoding mechanisms for form data.  The `$web_hdr_parm('content-type')` function can check the encoding.

A limitation is that some implementations of the `<input type="file">` tag by Internet Explorer might ignore default values.  Form-based uploads might require the user to select the file each time.  Each `<input type="file">` tag can only be associated with a single file.

### Janus FTP Server

Another option is the FTP feature of Janus Sockets.  The Janus FTP Server allows using any standard FTP client to upload files to a Model 204 file.  A Janus Sockets port with the type FTPSERVER is required.  Web server rules specific to the upload location are also needed.

### How uploads work with Janus Web Server

Whether HTTP PUT or form-based upload is used, Janus Web Server copies the file to CCATEMP.  This prevents partial data if there are network or browser problems.  Web rules determine the appropriate action.  If the upload is HTTP PUT and the rule indicates "RECEIVE," the file is copied to the target procedure.  If a command needs to be executed, User Language functions can be used.  When updating a procedure, Janus Web Server attempts to get an exclusive lock.

### Choosing the correct upload method

A table summarizes the advantages and disadvantages of each technique:

| Method | Advantages | Disadvantages | Likely Users |
|---|---|---|---|
| FTP and SIRPIPE/SIRPUT | Easy to configure, initiated from mainframe | Cannot be initiated from workstation, BATCH2 is slow | System administrators, operators |
| HTTP PUT | Uploads entire directories, workstation initiation, no Model 204 programming required | Not widely supported beyond Netscape Composer, not integrated into web applications | Web developers, designers |
| Form-based upload | Integrated into web applications, little training | No way to upload directories, some SOUL programming required | End-users |
| Janus FTP Server | Fast transfers, workstation initiation, easy-to-use interface | Not integrated into web applications, requires FTP client, can't dynamically create subdirectories | Web developers, designers, end-users |
