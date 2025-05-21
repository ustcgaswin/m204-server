## Model 204

Model 204 is a DBMS for IBM and compatible mainframes, which was first deployed in 1972.  Formerly owned by Computer Corporation of America (CCA), Model 204 was purchased in 2010 by Rocket Software.

Model 204 is a database management system (DBMS) that provides facilities for the creation, control, query, and maintenance of database files.  Access to the data is provided by an internal language known as SOUL (formerly User Language), by a variety of Host Language Interfaces (e.g., COBOL, Assembler, Fortran), and via SQL using Horizon and CRAM interfaces.

Model 204 contains an embedded TP monitor that provides for user sessions directly in the DBMS address space.  This makes Model 204 unique in the DBMS world, as users typically hold full-screen sessions directly inside Model 204 rather than viewing data from an external application.

Model 204 is highly parameterized and can be configured for environments that are quite compact or span the largest IBM and IBM-compatible multiprocessor machines.  Its strengths lie in its ability to handle very large volumes of data accessed by many users, processing high volumes of transactions.  It's also characterized by its high level of integration and the ease and variety of programming approaches for application development.

The internal structure of Model 204 database files is based on a set of tables holding file parameters, data records, indexes, procedures, and binary large objects.  The sizing of the tables is based on expected use of the file.  Typically, procedures are held in separate files from data, though this is not enforced by Model 204.

While a Model 204 database can be fully normalized, this is not enforced.  Model 204 records can be completely free-form, with varying lengths of records and varying numbers of occurrences of same-named fields.  Records do not require preallocated space, making Model 204 suitable for applications with varying record sizes and unpredictable data.  Family records, for instance, are well-suited to Model 204 databases.

**Documentation**

The Model 204 documentation has been converted from PDF manuals to M204wiki articles.  These articles also introduce Model 204 commands, parameters, and core functionality.

**Technical details**

* Model 204 configurations and operating environments
* Model 204 system requirements
* Model 204 add-ons and applications
* Rocket M204 product family (Rocket website overviews)
* Model 204 installation
