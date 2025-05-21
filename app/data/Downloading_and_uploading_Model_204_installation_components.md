## Downloading and uploading Model 204 installation components

**Downloading object files**

From the downloads page, you can download a file directly or have it emailed. Receiving the object deck through e-mail might take a little longer than downloading it directly, but it allows you to schedule a set of objects to be sent to a different person in your organization.

To download a file, click the Download object file link beside the object that you want.

To email a file:

1. Fill in the Target e-mail address field at the top of the downloads list.
2. Click the Email object file link beside the object that you want. The object deck will be e-mailed to the specified address as an attachment.
3. Save the attachment to a local file and then upload it to the mainframe exactly as if you had downloaded it directly.

**Uploading object files**

Once the object files are downloaded to the workstation, you must upload them to the z/OS system on the IBM mainframe. You can use any file transfer mechanism, including FTP and IND$FILE. Regardless of the transfer mechanism, you must observe the following rules:

* Transfer the object file in binary format. (No translation from ASCII to EBCDIC must occur as a result of the upload.)
* Specify storage size and the RECFM, LRECL, and BLOCKSIZE characteristics of the data set for the object you are transferring:

| record format | record size | block size | storage size |
|---|---|---|---|
| FB (fixed record length with block records) | 80 for object and macro libraries; | 6184 for procedure files | 6400 for object and macro libraries; |
|  |  | 6184 for procedure files | primary: 10 CYL; secondary: 5 CYL |


**If you are using FTP to upload a file:**

* Use the SITE command if the FTP client is on the workstation and the server is on the mainframe. (In this case the SITE command might have to be sent as a "quote" to the mainframe. How this is done depends on your workstation's FTP client.)
* Use the LOCSITE command if the FTP client is on the mainframe and the server is on the workstation.

**FTP JCL example**

The following z/OS JCL to FTP from the FTP server also shows the use of LOCSITE:

```
//jobcard
//FTP EXEC PGM-FTP, PARM=' (EXIT=4, TRAC, TI=20)'
//* (PARM is optional)
//OUTPUT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//INPUT DD *
ipaddress (or domain name) of ftp server, n.n.n.n
user login
pswd
binary
LOCSITE CYL
LOCSITE PRIMARY=10
LOCSITE SECONDARY=5
LOCSITE LRECL=80
LOCSITE BLOCKSIZE=6400
LOCSITE RECFM=FB
CD \M204V75
get M204V75.OBJ 'M204.V75.OBJ.DOWNLOAD' (replace
get M204V75_ZOS.MAC 'M204.V75.MAC.DOWNLOAD' (replace
QUIT
```

Category: Installation
