## ZHPF command

**Summary**

* **Privileges:** System manager
* **Function:** Checks ZHPF availability for file data sets

**Syntax**

```
ZHPF [filename | *]
```

**Where:**

* `filename` specifies a single local file
* `*` indicates all local files opened by all users

**Note:** `filename` and `*` are available as of Model 204 7.7. If neither `filename` nor `*` is specified, ZHPF prints ZHPF-related information for all data sets for the file currently opened by the user.

ZHPF displays the following message for each data set in the file(s) to indicate whether ZHPF type I/O can be used for the data set:

```
M204.2953: File filename dataset_name ZHPF capabilities hex values Supported: - Indicator
```

**Where:**

* `hex_values` represent the existing ZHPF capabilities
* `Indicator` has one of the following values:

| Value | Meaning |
|---|---|
| YES | ZHPF is supported for dataset\_name in filename. |
| NO | Hardware or z/OS support for ZHPF functionality is insufficient (for example when z/OS runs under VM). |
| N/A | ZHPF support is not available at all at the z/OS or hardware level. |


**Usage notes**

ZHPF is available as of Model 204 version 7.6.

**Categories:** System manager commands, Commands
