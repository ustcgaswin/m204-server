## SQL catalog

The Model 204 SQL catalog is a facility for storage, retrieval, and modification of the data information for SQL objects and users. The SQL catalog contains file data accessed with SQL applications.

### Contents

1. Surveying the SQL catalog
    1.1. Bridge to Model 204 data
    1.2. Model 204 SQL catalog characteristics
2. Populating the catalog
    2.1. Using the SQL catalog
    2.2. Populating the catalog manually
    2.3. Reporting catalog contents
3. Maintaining the SQL catalog
    3.1. Monitoring the SQL catalog consistency
    3.2. Job access and security
    3.3. Ongoing CCACAT maintenance
    3.4. CCACAT implementation for BLOB and CLOB data types
4. Creating the CCACAT file

### Bridge to Model 204 data

The SQL catalog is an essential bridge between your SQL application and Model 204 data. The following shows the position and function of the SQL catalog in Model 204 processing.  When you issue an SQL query against the Model 204 file, the Model 204 SQL Server compiler and optimizer read the catalog and translate the query into a physical data request against the Model 204 files. The retrieved records are returned as SQL rows.

### Model 204 SQL catalog characteristics

The Model 204 SQL catalog has the following characteristics:

*   **Model 204 file (called CCACAT):**
    *   Queriable
    *   Singular
    *   Not an active dictionary-type file
    *   Populated and maintained by the Connect Visual Interface (CVI)
    *   Repository of SQL security information

### Using the SQL catalog

The Model 204 SQL catalog contains the SQL definition of the Model 204 files that can be accessed by SQL applications. The SQL catalog is loaded, or populated, with this SQL description of Model 204 files through the Connect Visual Interface utility (CVI). Other methods include DDL that you prepare manually, or SQL utilities.

### Populating the catalog

You populate the SQL catalog by using the CVI utility to do the following:

*   Define or update SQL objects in the catalog that map to Model 204 file objects.
*   DDL that you prepare with mainframe SQL utilities.
*   DDL that you generate with the catalog reporting utility.

### Generating DDL manually

The TSF is a Model 204 subsystem that generates DDL statements. It simplifies your specification of corresponding SQL characteristics.

### Preparing DDL manually

You do not have to enter SQL on utilities for the SQL stream you submit to the CVI utility. You can also prepare a DDL stream by:

*   Modifying an SQL utility-generated stream.
*   Writing your own SQL stream.

### Reporting catalog contents

You can examine the contents of the Model 204 SQL catalog by:

*   Querying the catalog directly
*   Using the CCACATREPT utility

### Monitoring catalog consistency

SQL data definitions in the catalog must correspond to the Model 204 data they describe. Because the SQL catalog is not constructed or changed automatically when Model 204 files change, it might not be in sync with the Model 204 file at the time an application runs SQL DDL updates to the SQL catalog might be required. The Model 204 TSF subsystem uses the Model 204 file definitions to ensure consistency between the SQL catalog and the Model 204 file before you submit the TSF DDL to the SQL catalog.

### Maintaining the SQL catalog

Creation and maintenance of the SQL catalog (the CCACAT file) implies responsibilities for both SQL-specific data management and Model 204-specific data management.

### CCACAT implementation for BLOB and CLOB data types

This section describes how to install the necessary SQL data types, BLOB/CLOB, for use in your DDL processing.

### Backup and restore

Backup the SQL catalog frequently before making any changes to the catalog. Use the same backup, restore, and reorganization procedures that you use for any Model 204 file.

### File organization

CCACAT is distributed as an entry record file (FILEDRG). With this setting, the CCACAT file might eventually become full though there is an unused space due to the order of entries in the catalog.

### CCACAT implementation for BLOB and CLOB

You might be able to avoid creating BLOB and CLOB data types in the smaller Model 204 SQL installation.

### See also

A list of related topics is provided.
