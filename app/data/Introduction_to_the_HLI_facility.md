## Introduction to the HLI facility

The Model 204 Host Language Interface (HLI) facility enables you to invoke nearly all the system functions from applications written in programming languages such as COBOL, FORTRAN, PL/1, Assembler, Pascal, and C. Using the HLI facility, you can access Model 204 from a host language application and process against the database.

**Contents**

1. **Overview**
    * Advantages of the HLI facility
        * Utilizes the unique advantages of Model 204
        * Minimizes the introduction of data dependencies
        * Provides flexibility in design of databases and applications
        * Ensures database integrity
    * HLI capabilities
        * Concurrent processing
        * Batch mode operation
        * 31-bit addressing
        * Subroutine calls to Model 204
2. **Model 204 configurations**
    * Model 204 host language processing
        * Model 204's Inverted File Access Method (IFAM)
3. **Thread connections to Model 204**
    * Model 204 thread
    * IFSTRT and IFDIAL threads
        * Single and multiple cursor IFSTRT threads
    * Using an IFSTRT thread
    * Using an IFDIAL thread

**Overview**

The HLI facility serves the following principal purposes in a data processing installation:

* Makes the database available to host language programs and programmers, which enables existing systems and organizations to take advantage of the information resources managed by Model 204.
* Improves and accelerates the process of host language system maintenance by providing high-level facilities for database access and update.

**Advantages of the HLI facility**

* **Utilizes the unique advantages of Model 204:** Many unique advantages of using Model 204 are developed from the logical concepts used in structuring Model 204 databases and from the physical techniques used in organizing and accessing them. Using the Model 204 HLI facility allows you to access the Model 204 database utilizing those underlying structures and methods.
* **Minimizes the introduction of data dependencies:** Function call parameters specify information such as the name of the file to be opened, the criteria by which records are selected, the names of fields to be retrieved from a record, and the content of data to be stored in an updated record. The HLI call parameters provide a high-level logical view of the data and minimize the introduction of physical data dependencies into application programs.
* **Provides flexibility in design of databases and applications:** The entire Model 204 system makes the design of databases and applications as flexible as possible and eases the dynamic growth of both databases and applications after implementation. For example, record composition can vary from record to record within a file, and within the same record over time. Field length can vary in similar fashion. You can define new fields at any time, usually without reloading the databases and without altering application programs.
* **Ensures database integrity:** Use of the Host Language Interface shields you from certain operational parameters and problems that can vary from run to run. For example, Model 204 maintains data buffering, control of concurrent access to data, protection of data from unauthorized use, and protection and recovery of the database in the event of system failures and some application failures. To enable external recovery systems and procedures to be coordinated with those built into Model 204, the Host Language Interface also provides functions for the synchronization of checkpointing from application programs.

**HLI capabilities**

* **Concurrent processing:** With the HLI facility, a host language program using Model 204 can concurrently use other data accessing facilities and can run under the control of teleprocessing systems such as CICS. Host language programs can also run under the IBM Conversational Monitor System (CMS) within the Virtual Machine Facility (VMF). The Host Language Interface facility provides a bridge between Model 204 and other systems and between Model 204 databases and other data.
* **Batch mode operation:** Host language programs that use Model 204 operate in batch mode. Model 204 Host Language Interface programs can share a copy of Model 204 with other HLI application programs and online users in IFAM2, or they can use a private copy in IFAM1 and IFAM4. Host language programs can run in the same or different job and address space as Model 204 itself. Using the IBM inter-user communication vehicle (IUCV), programs can run in a different virtual machine than the one that hosts Model 204.
* **31-bit addressing:** Model 204 Host Language Interface programs can run with 31-bit addressing.
* **Subroutine calls to Model 204:** Host language programs communicate with Model 204 through the subroutine calls, that is, the HLI function calls, described in the HLI topics. Each call specifies an operation to be performed by Model 204. A complete transaction is ordinarily accomplished through a sequence of calls, as illustrated in Using an IFSTRT thread.

**Model 204 configurations**

* **Model 204 host language processing:** The following Model 204 configurations support host language processing using the HLI facility: IFAM1, IFAM2, IFAM4.
* **Model 204's Inverted File Access Method (IFAM):** IFAM is an acronym for Inverted File Access Method, which is Model 204's database I/O access mechanism. IFAM1, IFAM2, and IFAM4 denote different configurations of the Model 204 environment that provide IFAM type access to the Model 204 database from an application program written in a host language such as COBOL, FORTRAN, PL/1, or Assembler, using the Host Language Interface. The host language programmer must design an application to run in one of these environments. Refer to HLI: Job design factors for more information about the IFAM1, IFAM2, and IFAM4 environments.

**Thread connections to Model 204**

* **Model 204 thread:** A host language program starts a thread which provides a connection to Model 204. A thread is a logical connection between Model 204 and the host language application program. The host language program must start at least one thread in order to access the Model 204 database using the HLI facility. A thread corresponds to an IODEV definition in the Model 204 ONLINE.
* **IFSTRT and IFDIAL threads:** There are three types of threads that are available using the HLI facility: Single cursor IFSTRT thread, Multiple cursor IFSTRT thread, IFDIAL thread. The IFSTRT and IFDIAL threads each utilize a different set of communications protocols which support different types of functionality. In addition, the single and multiple cursor IFSTRT threads allow you to access the Model 204 database in different ways. The host language programmer must code an application corresponding to the type of threads that are started.
* **Single and multiple cursor IFSTRT threads:** The basic difference in functionality between single cursor and multiple cursor IFSTRT threads is that a multiple cursor IFSTRT thread functions very much like SOUL by allowing access to multiple files and record sets, and a single cursor IFSTRT thread limits access to one file and one record set at a time. The differences between single cursor and multiple cursor IFSTRT threads is described in greater detail in Multiple cursor and single cursor IFSTRT threads and in the HLI: Threads topic. Note: For host language applications that use IFSTRT threads, we suggest that you use a multiple cursor IFSTRT thread.
* **Using an IFSTRT thread:** Using an IFSTRT thread allows the host language program to specify operations to be performed by Model 204 against the database.
* **Using an IFDIAL thread:** Using an IFDIAL thread allows the host language program to transfer data to and from Model 204 using line-by-line terminal emulation mode.

**For more information**

See HLI: Job design factors for more information about using the IFSTRT and IFDIAL threads in HLI jobs. See HLI: Function summary for information about the calls that are available using the two different types of threads. For examples of application program code using the HLI calls, refer to the IFAM1 job program examples and IFAM2/IFAM4 job program examples. For information describing how to code applications using IFSTRT and IFDIAL threads, see the HLI: Threads topic.

**See also**

* Introduction to the HLI facility
* HLI: Job design factors
* HLI: Job requirements
* HLI: Coding conventions
* HLI: Function summary
* HLI functions
* HLI: Field formatting options for HLI calls
* HLI: Model 204 completion and ABEND codes
* HLI: IFAM1 job program examples
* HLI: IFAM2/IFAM4 job program examples
