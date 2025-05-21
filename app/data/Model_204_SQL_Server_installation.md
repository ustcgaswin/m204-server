## Model 204 SQL Server installation

**Overview**

Model 204 provides SQL access to Model 204 data through client-server technology.  Model 204 SQL Server provides full SQL processing.  Installing SQL Server involves downloading M284PROC, restoring it, setting up subsidiary files, and verifying parameter settings.  Upgrades typically involve restoring a fresh M284PROC copy.

**Preparing for Model 204 SQL Server installation**

*   **Product installation files:**  M284PROC, typically downloaded from the Rocket Software website.  Requires a registered Rocket user ID.
*   **System requirements:** SQL Server operates in a Model 204 Online region.  The installing user needs update access to JCL/EXECs for the host Online, System Manager privileges, and access to SQL Server application subsystems (APSYs).

**New installation or reinstallation**

*   **New installations:** Sites without SQL Server use new installation procedures.
*   **Reinstallations:** Sites reinstalling the latest version use reinstallation procedures.  Sites upgrading from previous versions already have the necessary SQL Server files.

**Verifying the SQL Server version**

The SQL Server version can be verified by checking the version number on the top right corner of any SQL Server APSY.  The version is also displayed in the M204.0060 message.

**New installation for customers who have never installed SQL Server**

1.  Allocate space for the M284PROC dump data set (9800 pages for V7.6+, 6000 for V7.5-).
2.  Download M284PROC from the Rocket website.
3.  Upload the dump file as binary into the allocated data set.
4.  Allocate SQL Server supporting files (M204PROC, TSFDATA, CCACAT, TSFTEMP).
5.  Update the Online JCL/EXEC.
6.  Update CCAIN parameters.
7.  Ensure Dictionary files are available.
8.  Restore M284PROC into M204PROC.
9.  Install SQL Server (using INCLUDE SOLINST).
10. Verify installation by connecting to the Online and querying the DEMO database.

**Reinstallation for existing SQL Server sites**

1.  Allocate space for the M204PROC dump data set.
2.  Allocate space for TSFTEMP (if needed).
3.  Download M284PROC from the Rocket website.
4.  Upload the dump file as binary into the allocated data set.
5.  Ensure existing SQL Server files and Dictionary files are available.
6.  Start the Online.
7.  Restore M284PROC.
8.  Install SQL Server (using INCLUDE SOLINST).
9.  Verify installation by connecting to the Online and querying the DEMO database.

**SQL Server size**

Minimum SQL Server size requirements are listed.  Actual requirements may vary.

**Verifying the installation (IBM z/OS only)**

Steps for verifying SQL Server installation on IBM z/OS systems.  Includes defining SQL Link, ProcessGroup, and Process to the Online.
