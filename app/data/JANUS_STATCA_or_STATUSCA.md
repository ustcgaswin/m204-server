## JANUS STATCA or STATUSCA

Display certificate status

This command displays the status of "trusted" certifying authority certificates that have been added to a port with the JANUS ADDCA command. Certifying authority is often abbreviated CA. STATCA is a valid synonym for STATUSCA.

**Contents**

* **Syntax**
* **Usage notes**
* **Example**
* **See also**

**Syntax**

```
JANUS STATUSCA [portname] [filename] [procname]
```

Each parameter is positional, can be specified with pattern wildcards, and can be replaced with an asterisk (*), which returns all values for that category. You may omit a parameter (which is equivalent to specifying an asterisk value for it) if the parameters to its right are omitted.

* `portname`: The defined JANUS port for which the certificate status is to be displayed.
* `filename`: The name of the file(s) for which the certificate status is to be displayed.
* `procname`: The name of the procedure for which certificate status is to be displayed. If not specified, status is displayed for all certificates for the indicated port(s) and file(s).

**Usage notes**

JANUS STATUSCA provides the following information:

* `Portname`: Name defined for the TCP/IP port.
* `Filename`: File from which the certificate was loaded.
* `Procname`: Procedure from which the certificate was loaded.
* `Internal-name`: The "common name" of the certifying authority as specified in the certificate.
* `NumSigned`: The number of certificates received that were signed by the certifying authority associated with the indicated certificate.

**Example**

The following example shows the format of STATUSCA output, in this case for a single Sirius-certified certificate:

```
JANUS STATCA SSLCLI
Portname Filename Procname Internal-name NumSigned
SSLCLI GLWPROC GTEST.CERT www.sirius-software.com 1
```

**See also**

* List of Janus commands

**Category:** Janus commands
