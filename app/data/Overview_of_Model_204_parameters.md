## Overview of Model 204 parameters

Parameters are program variables that control or describe Model 204. You can set these variables to tailor system performance to the requirements of an installation or user.

This page provides an overview of Model 204 parameters and how to use them. For an alphabetical listing of Model 204 parameters, see [List of Model 204 parameters](https://m204wiki.rocketsoftware.com/index.php/List_of_Model_204_parameters).

### Overview

* **Basic parameter types:**
    * System parameters
    * User parameters
    * File parameters
    * System parameters (SYSTEM)
    * User table parameters (UTABLE)
    * File characteristics parameters (FPARMS)
    * File table parameters (TABLES)
    * Server parameters (SERVER)
    * Individual parameters

* **How and where to set parameters:**
    * Parameters can be assigned new values in a variety of ways.
    * This section describes the initial assignment of parameter values at the beginning of a Model 204 session and dynamic reassignment, or resetting, of values later in the session.
    * Each Model 204 parameter has default values supplied by the system.
    * Some parameters are view-only parameters that provide information about Model 204.

### Assigning initial values

* **System parameters:**
    * Parameters are initially set with the parameters passed by the operating system when Model 204 is invoked.
    * Examples: `EXEC PGM-BATCHED`, `REGION-200K`, `DAXFER`
    * Also, parameters other than `SYSOPT` (up to a total of eight characters) can be passed in the `SYSPARM` field.
    * Examples: `OPTION SYSPARM "LIB-512"`

* **User parameters:**
    * User parameters are initially set on the user parameter line during system installation or by any user's parameter line.
    * Example: `PAGESZ-8184, NFILES-1`

### Assigning values dynamically: resetting

* **Using RESET:**
    * Change parameters with the `RESET` command.
    * Example: `RESET ERMX-SE, PGSEP-3`

* **Using UTABLE:**
    * Allows changing the size of server tables by modifying user table parameters.
    * Example: `UTABLE LOTBL-2000`

### Parameter-setting hierarchy

* Parameters can be set by the operating system, by an individual user, or by the system manager.
* The hierarchy defines the precedence of parameter settings.

### Requirements for parameter values

* **Character string format:**
    * Specify character strings in character string format or short string format.
    * Example: `parameter (word | chars)`

* **Short string format:**
    * Specify a short string value.
    * Example: `parameter[-](number | chars | Xhex)`

### Summing parameters

* The possible settings for a parameter are listed.

### Validating settings

* A parameter's valid value range is checked when the parameter has been specified.
* Examples of specifying parameter values using the `RESET` command and `PGSEP` settings.

### Summary of parameter types

* A table summarizing the different categories of Model 204 parameters.
    * System parameters (SYSTEM)
    * Scheduler parameters (CWAIT)
    * User parameters (USER)
    * User table parameters (UTABLE)
    * File characteristics parameters (FPARMS)
    * File table parameters (TABLES)
    * Server parameters (SERVER)

### Individual Model 204 parameters

* The Model 204 parameter list provides pages for each parameter with default settings and other characteristics.
* The setting line of each parameter displays necessary information.