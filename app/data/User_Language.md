## User Language

User Language (renamed SOUL in version 7.5 of Model 204) is the internal language of the Model 204 DBMS, a product of Rocket Software. It is a 4th Generation Language (4GL), which means it was designed to be a "high level" language, with a good deal of abstraction and power embedded in relatively simple programming directives. User Language is characterized by its very easy, English-like syntax and its tight integration with the Model 204 DBMS. Programs begin with a BEGIN statement and end with END (statements necessarily uppercased prior to version 7.5).

**BEGIN**
```
PRINT 'HELLO WORLD'
```
**END**

Because it is tightly integrated into Model 204, User Language contains native instructions for manipulating data held in Model 204 files. Records in a file are selected using variations on the FIND statement and can be looped over using a variety of structures, the main one being the FOR EACH RECORD loop.

**BEGIN**
```
X: IN FILE INVENTORY FIND ALL RECORDS FOR WHICH ITEMTYPE = 'BOOK'
```
**END**
```
END FIND
FOR EACH RECORD IN X
PRINT TITLE AND AUTHOR AND PUBLISHER AND PRICE
END FOR
```
**END**

In User Language, variables begin with the percent sign (%), and native "functions," which implement many complex features of the language, begin with a dollar sign ($) - or a pound-sign in England or a Yen sign in Japan.

**BEGIN**
```
%X IS FLOAT
FOR %X FROM 1 TO 10
IF $MOD(%X,2) THEN
PRINT %X WITH ' IS ODD'
ELSE
PRINT %X WITH ' IS EVEN'
END IF
END FOR
```
**END**

A wide variety of variations are possible with the language, allowing novice coders to start using the language quickly, and expert users to learn the shortcuts and abbreviations. Model 204 provides its own terminal services, and User Language procedures are typically stored in Model 204 database files, so User Language programmers usually work directly inside the database environment, opening database files at the command level, editing User Language with Model 204's internal editor, and running programs with the "GO" directive from inside the editor. It is also possible to access Model 204 from external programs using Host Language Interfaces or Model 204's SQL capability but, because User Language is so highly optimized, the majority of Model 204 applications are written in User Language.

**User Language becomes SOUL**

Version 7.5 of Model 204, released shortly after the acquisition of Sirius Software, provided significant enhancements to User Language, perhaps the most prominent being Object-Oriented programming capabilities (motivating the name change from User language to SOUL) and support for mixed-case keywords and variable names:

**Begin**
```
print 'Hello World'
```
**End**

The Object-Oriented additions let you write sophisticated applications using a range of classes, some built in and others you create locally. Other V7.5 additions included a library of $functions; constructs like Stringlists and Daemons that offer new ways of programming and new possibilities for managing complex, in-memory data manipulation; the extensive Janus product set, which allows access to Model 204 via HTTP, sockets, Sybase Omni servers, FTP clients, and more; and the RKTools family of products, which help programmers and system administrators perform a wide variety of Model 204 supporting tasks.

**See also**

* SOUL documentation (the former User Language Reference Manual)
* Other Model 204 documentation:
    * Commands
    * Parameters
    * $functions
    * Model 204 files
    * Model 204 PDFs (some converted to M204wiki)

**Category: Overviews**
