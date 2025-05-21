## LOGIN or LOGON command

**Summary**

* Privileges: Any user
* Function: Logs in to the Model 204 system

**Syntax**

```
{LOGIN | LOGON} userid [account]
```

**Where:**

* **userid:** A character string that identifies the user who is logging in to Model 204. The user ID can be 1 to 10 characters in length. When an external security interface is performing login validation, the range for the user ID might differ.
* **account:** A character string that identifies the account under which the user is logging in to Model 204. The account can be 1 to 10 characters in length. Under an external security interface, the maximum for account might differ. The account identifies the user account to Model 204's accounting routines. The value of account in these routines might be affected by the use of an external security interface. Refer to the Model 204 Security Interfaces pages for detailed discussion of security interfaces. If the login feature is not in use, the string NO ACCOUNT is used as the account.

**Example**

```
LOGIN BLOOM D0101005
M204.0347: PASSWORD
BUDGET
M204.0353: BLOOM
LOGIN 88 JUL 11 12.04
D0101005
```

**Usage notes**

The LOGIN and LOGON commands allow the user to gain access to Model 204 at most installations. The system manager controls whether or not the login procedure is required at an installation. If the login procedure is not required, the user is assigned a default set of privileges. Once a user has connected to Model 204, and if the system manager has set the Model 204 option to require logins, any commands entered by the user (other than LOGIN or LOGON) display a request for the user to log in.

**Failing to log in correctly**

If either the user ID or the password is incorrect, Model 204 displays a message that the login failed. If an error occurs, the user reissues the LOGIN command. Model 204 provides login delays for threads that repeatedly fail to log in correctly. Refer to the discussion of this feature in Login delays. A password must follow restrictions. At a minimum a password cannot contain spaces, commas, or colons, and must not be the same as the USERID, the current password, or the previous password. Additional restrictions may apply with the Password Expiration feature or be specific to your site. See "Setting a password" below. IFSTRT and IFDIAL threads can change the password by appending a colon and the new password, as described above. When the password security feature is installed, the more restrictive password rules apply, except that IFSTRT threads do not require the user to reenter the new password.

**Setting a password**

After you enter a LOGIN or LOGON command, enter a password after the system prompt:

```
M204.0347: PASSWORD
password
```

* **password:** is a character string. The length of password is:
    * 1 to 8 characters long (Model 204 version 7.6 or earlier)
    * 1 to 127 characters long (Model 204 version 7.7 or later).
    * Semicolons are not supported in passwords on IFSTRTN threads
    * password can be mixed case if CUSTOM=11 is set.

**(Model 204 version 7.5 or earlier)**

* A password can be 1 to 8 characters long and cannot contain spaces, commas, or colons. With the Password Expiration feature installed, the following additional rules apply.
    * The password must:
        * Not be the same as the USERID, the current password, or the previous password.
        * Be six, seven, or eight characters long.
        * Begin with an alphabetic character.
        * Include at least one numeric character.

**(Model 204 version 7.6 only)**

* A password can be 1 to 8 characters long and cannot contain spaces or commas. It can contain colons if PWDCOLON=1 is set. The PWDCOLON parameter, available only in version 7.6, supports the use of colons in passwords. With the Password Expiration feature installed, the following additional rules apply.
    * The password must:
        * Not be the same as the USERID, the current password, or the previous password.
        * Be six, seven, or eight characters long.
        * Begin with an alphabetic character.
        * Include at least one numeric character.

**(Model 204 version 7.7 and later)**

* A password can be 1 to 127 characters long. Versions 7.7 and later support long passwords, or passphrases, which are at least 9 characters long and up to 127 characters. Multiple, embedded spaces (that is, blanks) are valid characters for 8-character passwords, long passwords and password phrases. Leading and trailing spaces are stripped and so are not part of the password. CCASTAT passwords: Regular passwords (up to 8 characters) and long passwords maintained in CCASTAT with the LOGCTL command can contain any character (even a colon) except a comma. Passwords may contain commas if they are changed with the LOGONCP command or with the SSir_Login function. External Security Manager (ESM) passwords: Regular passwords (up to 8 characters) and passphrases maintained by an ESM can contain some special characters, depending on the settings for that ESM. Check with your ESM administrator to determine which characters are allowed. For example, certain Security Server (RACF) special characters require setting the Model 204 CUSTOM parameter 11 value. Passwords and passphrases are passed, unaltered and with no restrictions, from Model 204 to the ESM for verification. With the Password Expiration feature installed, the following additional rules apply.
    * The password must:
        * Not be the same as the USERID, the current password, or the previous password.
        * Be six, seven, or eight characters long.
        * Begin with an alphabetic character.
        * Include at least one numeric character.

**Changing a password**

**(Model 204 version 7.5 or earlier)**

Change your password by entering your existing password, a colon, and your new password:

```
M204.0347: PASSWORD
password:new password
```

new password is governed by the same limitations and requirements as password. However, you can enter a new password at login only if your user privileges include the ability to change your password as you log in. When a new password is being specified, the password and the new password must be separated by a colon (:). When the user enters the password, Model 204 masks it from view. If the Password Expiration feature was installed at your site, the following message is also issued to confirm your password:

```
M204.2633: RE-ENTER NEW PASSWORD
```

**(Model 204 version 7.6 only)**

If PWDCOLON=1 is set, you cannot change your password using password:new password because colons are allowed in passwords. You can change your password in one of the following ways: using the LOGCTL C command (system manager privileges required) using the $Sir_Login function call (in Janus Web Server and Janus Sockets applications)

**(Model 204 version 7.7 and later)**

Use the LOGINCP command to change your login password. Because colons are always valid login password characters as of version 7.7, LOGINCP or LOGONCP replaces the former technique for changing passwords using LOGIN or LOGON and specifying password:newpassword.

**After login**

After logging in, the user has access to most of the Model 204 system commands. The user ID under which the user logs in determines which privileges are available. These privileges can include the ability to:

* Change the login password
* Change file passwords
* Use restricted commands such as CREATE FILE and MONITOR
* Reset restricted parameters with the RESET command

For information on the login process for an external security interface, see the Model 204 security interfaces.
