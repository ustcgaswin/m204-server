## Model 204 configurations and operating environments

This page provides an overview of Model 204. The structure of the Model 204 database management system, basic Model 204 configurations, and the hardware and software environments required to run Model 204 are summarized for the view of the person managing all of the Model 204 system resources.

### About Model 204

Model 204 is a complex database management system that provides facilities for the creation, control, query, and maintenance of database files. Data intensive batch and online application languages, such as Assembler, COBOL, PL/I, and FORTRAN can communicate with Model 204. Model 204 supports SQL queries using the Model 204 Host Language Interface.

*   Sequential and random file access, runtime input and output, and software that uses them from the physical structure of the data and allows convenient, controlled access to the data.
*   Table parameters handle the necessity of running specific requests.
*   Runtime parameters control environment and time-specific specifications.
*   Model 204 system management of data files.

### Model 204 system management

Model 204 system management that each installation designates to be the Model 204 System Manager. The System Manager's responsibilities include:

*   Installing and maintaining the DS and z/OS or CMS and Model 204 parameters for the Model 204 configurations used in an installation.
*   Setting up and maintaining the Rocket 204 software.
*   Acting as a liaison with Rocket Technical Support.

The System Manager's position usually is part-time in a small to medium size database environment. Often the System Manager also has the responsibilities of File Manager.

### Model 204 configurations

Model 204 configurations listed in the table below. Various configurations, such as IFAM2 and ONLINE, can be combined to meet site requirements.

| Configuration | Description |
|---|---|
| BATCH204 | Handles a single user in batch mode. |
| ONLINE | Supports host language calls to Model 204 files from multiple users. Programs run as separate tasks in a single region, partition, or virtual machine. |
| IFAM1 | Supports host language calls to Model 204 files from multiple users. Each program operates in its own region, partition, or virtual machine. |
| BATCH2 | Establishes a user language connection to a Model 204 ONLINE running in a separate region. |

### Operating systems

Model 204 uses the same basic system architecture, file architectures, and the z/OS Language with each compatible operating system. Unique characteristics that must be considered when running Model 204 are discussed.

### Operating environments

*   **CPU:** z/OS runs on an IBM mainframe environment with z/OS, VM, and VSE operating systems.
*   **z/OS:** Model 204 is supported in all z/Architecture principles of operations. Mainframes with ESA/300 architecture supported from 9912 (Generation 1) to 9912 (Generation 2) and all enhancements that support z/OS provide full support for multiuser, hardware configurations under z/OS.
*   **Direct Access Storage Devices (DASD):** Model 204 allows configurations above the 16-megabyte line for z/OS and VM operating systems.

### DASD device types

| Type | Model |
|---|---|
| CKD | 3350, 3330 |
| ECKD | 3345 |
| FBA | 337x, 9332, 9335 |

### Shared DASD and Global Resource Serialization

Many customers run Model 204 in the z/OS LPAR (Logical Partition) environment where different versions of z/OS share disk volumes. In these systems, Model 204 jobs, ONLINE or BATCH, may be running concurrently in any of the defined LPARs and may be handled through the Shared DASD enqueue.

### ENQCTL command enhanced

It is therefore critical for Model 204 integrity that the device macro be allowed to function on all devices in a multi-operating system environment where Model 204 jobs might run. Also, formerly when a file could not be opened due to shared DASD enqueueing conflicts, a maximum of eight entries was used using the previous message number.

### Cautions using Global Resource Serialization

The IBM Global Resource Serialization (GRS) feature available in z/OS may be configured to suppress the use of the device RESERVE macro. If suppression is configured, information regarding the GRS resource and Shared DASD, please see.

### 64-bit architecture support

Model 204 runs on 64-bit architecture on z/OS and VM systems.

### Understanding Model 204 bar storage

| Storage location | Used for... |
|---|---|
| In 0-2 gigabyte range | Programs and data |
| Above 4 gigabytes | Data only |
| Above 12-4 gigabyte | Unavailable for any purpose |

### Model 204 entities in above bar storage

The following entities can be accessed directly above the bar storage:

*   Pages from Tables A, B, E, and X
*   CCATEMP pages with found sets, screens, and images, and transaction backout log
*   18 buffers
*   CCASERVR for swapped out procedures
*   GTBL, QTBL (as of version 7.5)
*   HTBFRS (as of version 7.5)

### Handling 64-bit statistics

To support very large quantities of statistical counters by increasing the size of statistical counters and also exploiting processing appropriately. For any in-house or third-party applications, some of the system routines are now able to handle statistical counters generated.

### Model 204 storage

### z/VSE storage considerations

The following considerations apply to z/VSE environments.

*   When allocating storage in the partition GETVIS area, the executable phase (program) and have enough GETVIS area for Model 204 to allocate working storage.
*   If the JCL statement, the SIZE parameter should specify the GETVIS area for an equivalent program size, unless defined in the JCL, then the program's GETVIS area is enough for the efficient program.
*   The SIZE parameter is required when using IFAM1. It is a strong recommendation for the efficient use.

### Model 204 disk considerations

Model 204 supports variable-format disks and CMS-format disks. The disks can be formatted in variable storage or fixed volumes through a SIZE parameter specification.

### Related Rocket Software products

As the Rocket 204 system manager, you might be required to manage the following Rocket Software products:

*   **Analytics/204:** A basic system data analysis tool that helps business users to know their data, access their data, and trust their data, without the need for IT assistance.
*   **Connect and Horizon:**
*   **JDBC for Model 204:**
*   **MP/204:**
*   **MQ/204:**
*   **Parallel Query Option/204:**