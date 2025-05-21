## APPDATE command

**Note:** This is a Sirius Mods-only command prior to Version 7.5 of Model 204. This command will perform no useful processing unless a site is authorized for the Sir2000 User Language Tools.

The APPDATE command allows you to control the operation of date and time oriented User Language $Functions, in the following two ways:

1. You can specify a system-wide or user-level clock that is used to obtain date and time values for User Language $functions. For application testing, this is preferred to the Model 204 SYSDATE parameter, which is much less flexible and which greatly complicates the ability to do things such as share procedure files or read-only data files in the testing environment.

2. You can use the `DATE_ERR` clause to set switches that control the default system behavior when errors are encountered in date and time oriented User Language $functions. The choices are to produce an error message along with request cancellation, produce a warning message, or silently continue with the request.

This control is available at the system and user level, and you can also set it for the duration of a User Language request by using the `$Sir_Date_Err` function. At the system and user level, you can also control whether procedure names and line numbers are available for error messages.

**See also**

The Sir2000 APPDATE command has detailed information about the APPDATE command.

**Categories:** User commands | Commands
