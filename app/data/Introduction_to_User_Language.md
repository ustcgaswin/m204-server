## Introduction to User Language

**Contents Index**

1. **Overview**
    1.1 Introduction
    1.2 Model procedures
    1.3 SQL structure
    1.4 User Language(SQL/User) Interface
    1.5 Types of Query Statements
    1.6 Detailed Query Options
    1.7 Report screen
    1.8 Compilation
    1.9 Program access
    1.10 Input and communication
    1.11 Program development and manipulation
    1.12 Records
    1.13 Files
    1.14 Fields
    1.15 File types
    1.16 Compilation
    1.17 Request compilation and evaluation
    1.18 Compilation errors
    1.19 Security
    1.20 Log security
    1.21 Overview

**Overview**

Model 204 is a comprehensive, multi-purpose database management system for 8M and compatible mainframes.  Model 204 is compatible with self-developed User Language programming language.

**Request structure**

Typical user language request begins with statements that select records from one or more files.  Loop statements are applied on complete statements. User Language allows you to select records that satisfy specific conditions.

**Example**

The following example illustrates the simplicity of User Language. Using English also provides statements for conditional expressions.

**Security**

Model 204 provides security features to protect users.

*   File level
*   User level
*   Record level
*   Terminal
*   Logical

**Log security**

The descriptions of User Language operations in this topic assume that the specific facilities for object access and security that are available in Model 204 are also available in the User Language environment.

**Types of statements**

User Language consists of several types of statements, including those listed below. Each type of statement is described in more detail in the following sections.

*   Logic and control statements
*   Computational statements
*   Report statements
*   Full-screen formatting
*   File and internal access
*   Program communication
*   Request development and manipulation
*   Record selection

**Compilation errors**

Errors can occur during compilation or execution.  If errors occur during compilation, Model 204 displays them by displaying messages.

**Counting errors**

Certain types of Model 204 compilation errors have a counter to be incremented.  When an error is encountered, a message is displayed and evaluation continues.

**File types**

File order is in a Model 204 file in one of four orders: entry order, entry order, sorted, or hash key order.

**Records**

A collection of data in tables.  Because the night is not an arbitrary value, it is fundamental to understanding how Model 204 files are relational on the particular fetch time.

**Files**

Files are discussed in detail in the next section.

**Inserted files**

Model 204 files are a unique file structure which provides several different ways of ordering records to allow for offering data access methods.

**Composition of Model 204**

A file is divided into one or more logical segments.

*   File Control Section
*   Table A
*   Table B
*   Table C
*   Table D
*   Table E

**File types**

*   Entry order
*   Sorted
*   Linked
*   Sequential
*   Direct access

**Program communication**

You can request information and manipulations.

**Database structure**

Model 204 consists of individually numbered physical segments, which can be of several types.  The individual segments are grouped into files that consist of variable-length fields, flexible data independence, and variable-length records.

**Fields**

Model 204 as variable-length data is a grouping of fields, field names, and field values.  Field names and field values are separated by spaces.  The spaces between words in a field name or value, the extra spaces are ignored.