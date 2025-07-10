# SOUL $functions - m204wiki
Some operations are inefficient or impossible to code in a high-level programming language like SOUL. To overcome this difficulty, SOUL provides a variety of $functions. Many of these functions are deprecated in favor of an OO API method. When that is the case, the $function page notes the matching method.

The mathematical $functions, and those $functions central to a Model 204 product (and which require authorization of that product) are listed separately; you can find links to those sets of $functions in the following table:

The following table lists available SOUL $functions, in alphabetical order. The list is not exhaustive (see the [table of other $function sets](#otherLists), above).

**Note:** Some of the $functions listed below are only available with the purchase of a Model 204 add-on product. The individual page containing such a $function's full description has an itemized list of the add-on products that authorize the use of the $function.



* $Function: $Abbrev
  * Description: Determine if string is abbreviation within list of words
* $Function: $Account
  * Description: Account under which the user is logged in.
* $Function: $Acct
  * Description: User ID under which the user is logged in.
* $Function: $Alpha
  * Description: Whether a string is composed of only the letters A through Z.
* $Function: $AlphNum
  * Description: Whether a string is composed of only the letters A through Z and digits 0 through 9.
* $Function: $Arr_Find
  * Description: Find value within array
* $Function: $Arr_Init
  * Description: Initialize every element of array to specific value
* $Function: $Arr_Max
  * Description: Find maximum value in array
* $Function: $Arr_Min
  * Description: Find minimum value in array
* $Function: $ArrSize
  * Description: Number of elements in a particular dimension of an array.
* $Function: $Ascii
  * Description: Input string, converted from EBCDIC to ASCII.
* $Function: $A2E
  * Description: Translate ASCII to EBCDIC
* $Function: $Base64_Decode
  * Description: Convert from base 64 to byte string
* $Function: $Base64_Encode
  * Description: Convert byte string to base 64
* $Function: $BgPurge
  * Description: Cancel "long" sdaemon request initiated with $CommBg
* $Function: $BgQuery
  * Description: List of "long" sdaemon requests initiated via $CommBg
* $Function: $Binary
  * Description: A number converted into fixed-point binary.
* $Function: $Bind_List
  * Description: Return list of bound semaphores onto a $list
* $Function: $Bind
  * Description: Fast, easy synchronization of system wide resource
* $Function: $BitAnd
  * Description: Bitwise AND of two integers
* $Function: $BitOr
  * Description: Bitwise OR of two integers
* $Function: $BldProc
  * Description: Enables a request to build a temporary procedure.
* $Function: $Buffer_Position
  * Description: Current Universal Buffer position.
* $Function: $Buffer_Size
  * Description: Size of user's Universal Buffer.
* $Function: $Buffer_Used
  * Description: Amount of data currently in Universal Buffer
* $Function: $Bump
  * Description: Bump a user
* $Function: $CenqCt
  * Description: Number of unused entries within the resource enqueuing table.
* $Function: $Center
  * Description: Center string
* $Function: $CfStatL
  * Description: List of statistics for users holding critical file resources
* $Function: $ChkMod
  * Description: Whether the terminal operator entered data in any full-screen input field.
* $Function: $ChkPat
  * Description: Syntax of a pattern.
* $Function: $ChkpInf
  * Description: Information about checkpoints
* $Function: $ChkTag
  * Description: Whether any erroneous full-screen input has been entered by the end user.
* $Function: $Close
  * Description: Close file or group in User Language request
* $Function: $Cms
  * Description: Determine if online is running under CMS
* $Function: $Code
  * Description: Encoding facility.
* $Function: $Command
  * Description: Execute Model 204 command on sdaemon, results to image
* $Function: $CommBg
  * Description: Execute Model 204 commands on sdaemon
* $Function: $CommndL
  * Description: Execute Model 204 command on sdaemon, results to $list
* $Function: $Context
  * Description: Determine if string is name of open file or group
* $Function: $Curfile
  * Description: Name of the file from which the current record has been selected.
* $Function: $Currec
  * Description: Integer equal to the internal number of the current record.
* $Function: $CurrecId
  * Description: Get the Imagine recordId of the current record.
* $Function: $C2D
  * Description: Convert binary byte string to integer
* $Function: $C2X
  * Description: A translation of each byte within a character string to its two-byte hexadecimal representation.
* $Function: $DaemonMasterNumber
  * Description: Get user number of master thread
* $Function: $DaemonParentNumber
  * Description: Get user number of parent thread
* $Function: $Date
  * Description: Current date in yy-mm-dd format.
* $Function: $DateChg
  * Description: Specified number of days, added to or subtracted from a given date.
* $Function: $DateChk
  * Description: Whether a given date is valid.
* $Function: $DateCnv
  * Description: Date converted to a format specified by the user.
* $Function: $DateDif
  * Description: Difference in days between two dates.
* $Function: $DateJ
  * Description: Current date in "yy.ddd" format.
* $Function: $DateP
  * Description: Current date in "dd mon yy" format.
* $Function: $DAY
  * Description: Day name of the input day-number.
* $Function: $DayI
  * Description: Number reflecting current day of the week.
* $Function: $Deblank
  * Description: Substring of a string, with leading and trailing blanks removed.
* $Function: $Decode
  * Description: Decoding facility.
* $Function: $Deflate
  * Description: Compress a longstring with Deflate
* $Function: $DelCh
  * Description: Remove characters from string, compress and strip blanks
* $Function: $Delg_Subsys
  * Description: Delete subsystem-wide global
* $Function: $Delg_Sys
  * Description: Delete system-wide global
* $Function: $Delimr
  * Description: Insert delimiter string into input string at regular positions
* $Function: $Dscr
  * Description: Decoding facility.
* $Function: $Dsn
  * Description: Data set name when you specify a file's DD name and the ordinal number.
* $Function: $DsnNum
  * Description: Total number of data sets defined for a file when you specify the file's DD name.
* $Function: $D2C
  * Description: Binary byte representation of integer
* $Function: $D2X
  * Description: Hex representation of integer
* $Function: $Ebcdic
  * Description: Convert input string from ASCII to EBCDIC
* $Function: $EcbDGet
  * Description: Get string data associated with an Event Control Block (ECB)
* $Function: $EcbDSet
  * Description: Set string data associated with an Event Control Block (ECB)
* $Function: $EcbTest
  * Description: Check an Event Control Block (ECB) to see if it is posted
* $Function: $EcfStat
  * Description: Returns the detailed completion code from the previous EXTERNAL statement.
* $Function: $Edit
  * Description: Edited numeric and alphanumeric text
* $Function: $EdScan
  * Description: Scan list of entities in online
* $Function: $Eformat
  * Description: Exponent notations from converted numeric values
* $Function: $Encrypt
  * Description: A one-way encryption of a character string.
* $Function: $Ent_Print
  * Description: Set automatic character entity substitution
* $Function: $Ent_Tab
  * Description: retrieve/modify character entity substitution table
* $Function: $Ent
  * Description: Do character entity substitution
* $Function: $Enter
  * Description: Efficient terminal dialogue with users of data entry applications.
* $Function: $ErrClr
  * Description: Clears the error message text returned by the $ErrMsg and $Fsterr functions.
* $Function: $ErrMsg
  * Description: Prefix and text of the last counting error or request cancellation message received.
* $Function: $ErrSet
  * Description: Increment or clear number of counting errors, set $ErrMsg
* $Function: $E2A
  * Description: Translate EBCDIC to ASCII
* $Function: $FakeEnt
  * Description: Prepare fake ENTER to automatically satisfy next full screen read
* $Function: $FDef
  * Description: String that describes the attributes of a field in a Model 204 file.
* $Function: $Field_Image
  * Description: Return field values into an image
* $Function: $Field_List
  * Description: Return field values into a $list
* $Function: $Field_ListI
  * Description: Return field values into a $list mapped to an image
* $Function: $FieldgroupId
  * Description: The ID of the current field group. (Available as of Model 204 version 7.5.)
* $Function: $FieldgroupOccurrence
  * Description: The current occurrence number of the field group. (Available as of Model 204 version 7.5.)
* $Function: $FIniTim
  * Description: File initialization YYDDDMMHHSSTH
* $Function: $FiStat
  * Description: Retrieve file's statistics into string
* $Function: $FiStatL
  * Description: Retrieve set of files' statistics into list
* $Function: $FldLen
  * Description: Length of a field.
* $Function: $Float
  * Description: Floating-point to a 4-byte string without conversion (4-byte floating point number to a 4-byte string).
* $Function: $FloatD
  * Description: Floating-point to a 4-byte string without conversion (8-byte floating point number to an 8-byte string). 
* $Function: $FlsAcc
  * Description: User's access rights to a particular field.
* $Function: $FlsChk
  * Description: Whether a given set of field level security accesses is valid for a field.
* $Function: $FreeOpt
  * Description: Free optional file or group from subsystem
* $Function: $Fsterr
  * Description: Variable-length string containing the prefix and the first counting error message or request cancellation message received by the user since the last time the count was reset to zero.
* $Function: $FunForc
  * Description: Cancel running or waiting Fast/Unload request
* $Function: $FunImg
  * Description: Retrieve data from active Fast/Unload request into image
* $Function: $FunList
  * Description: $list of active and enqueued Fast/Unload requests
* $Function: $FunLoad
  * Description: Fast/Unload records in Model 204 list or found set
* $Function: $FunPurg
  * Description: Purge running or waiting Fast/Unload request
* $Function: $FunSkip
  * Description: Skip to next output record for $FunImg, $FunsStr
* $Function: $FunsStr
  * Description: Retrieve data from active Fast/Unload request into string
* $Function: $FunWait
  * Description: Wait until asynchronous Fast/Unload request completes
* $Function: $Getg
  * Description: Information stored by a $SETG function.
* $Function: $GetL
  * Description: Line number of the current line on the page on the user's terminal or on the output data set specified by a USE command.
* $Function: $GetP
  * Description: Page number currently on the user's terminal or on the output data set specified by a USE command.
* $Function: $GrmLoc
  * Description: Location of a missing member.
* $Function: $GrmName
  * Description: File name of a missing member.
* $Function: $GrnLeft
  * Description: Number of optional files that may fail before MAXFAIL is exceeded.
* $Function: $GrnMiss
  * Description: Number of missing members.
* $Function: $GunZip
  * Description: Decompress a longstring with GUNZIP
* $Function: $GZip
  * Description: Compress a longstring with GZip
* $Function: $HexA
  * Description: Convert hexadecimal string to EBCDIC equivalent
* $Function: $HPage
  * Description: String of special characters whose length is equal to the value specified as the $HPage argument.
* $Function: $Hsh
  * Description: A hash value from a converted string value. A hash value is a distinct numeric representation of a given string value.
* $Function: $IHexA
  * Description: Convert EBCDIC string to hexadecimal equivalent
* $Function: $ImgInf
  * Description: Retrieve image item by variable name
* $Function: $ImgOvl
  * Description: Replace image item value
* $Function: $Incrg
  * Description: Performs simple arithmetic on global variables.
* $Function: $IncStat
  * Description: Increment local system statistic
* $Function: $Index
  * Description: After comparing two strings, a number equal to the first position within the first string at which the second string appears; the same function as $SCAN.
* $Function: $Inflate
  * Description: Decompress a longstring with inflate
* $Function: $ItsOpen
  * Description: Whether or not a file is open.
* $Function: $ItsRemote
  * Description: Whether the current file or group is remote or scattered.
* $Function: $JobAuth
  * Description: Determine if user has authorization for USE $JOB
* $Function: $Jobcode
  * Description: Allows a request that is part of one step of a Model 204 batch run to communicate with a subsequent step.
* $Function: $JpStat
  * Description: Retrieve Janus port's statistics into string
* $Function: $JpStatL
  * Description: Retrieve statistics for set of Janus pors into $list
* $Function: $LangSpc
  * Description: String containing the binary value of a character in a specified language.
* $Function: $LangSrt
  * Description: A binary string translated to sort according to the NLSORT macro for the specified language.
* $Function: $LangUst
  * Description: Previously $LangSrt'ed  string translated back to its original form.
* $Function: $Len
  * Description: Length of a value in a field.
* $Function: $List_Add_Ordered
  * Description: Add an item to an ordered $list
* $Function: $List_Add_Unique_Ordered
  * Description: Conditionally add an item to an ordered $list
* $Function: $List_Add_Unique
  * Description: Conditionally add an item to a $list
* $Function: $List_Capture
  * Description: Capture print data to $list
* $Function: $List_Conv_Item
  * Description: Convert $list to single delimited $list item
* $Function: $List_Copy_Items
  * Description: Copy items between $lists
* $Function: $List_Diff_Item
  * Description: Differences between $list and delimited $list item
* $Function: $List_Global and $List_Session
  * Description: Access/create global/session $list
* $Function: $List_Global_Del and $List_Session_Del
  * Description: Delete global/session $lists
* $Function: $List_Global_List and $List_Session_List
  * Description: List global/session $lists
* $Function: $List_MaxIL
  * Description: Return maximum $list item length
* $Function: $List_Print
  * Description: Display contents of a $list
* $Function: $ListAdd_Lstr
  * Description: Add longstring as new $list item
* $Function: $ListAdd
  * Description: Add string as new $list item
* $Function: $ListAddI
  * Description: Add image as new $list item
* $Function: $ListAdj
  * Description: Adjust length of $list item
* $Function: $ListChk
  * Description: Validate a $list identifier
* $Function: $ListCmp
  * Description: Compare two $lists and produce $list describing differences
* $Function: $ListCnt
  * Description: Number of items in $list
* $Function: $ListCpy
  * Description: Copy $list
* $Function: $ListDel
  * Description: Release CCATEMP storage used for $list
* $Function: $ListFind
  * Description: Find string in $list
* $Function: $ListFindI and $ListFindI_Up
  * Description: Find image item in $list
* $Function: $ListFindI_Sub
  * Description: Build $list subset based on image item
* $Function: $ListILn
  * Description: Length of $list item
* $Function: $Listimg_Copy
  * Description: Copy a $list's image association
* $Function: $ListImg
  * Description: Associate an image with a $list
* $Function: $ListInf_Lstr
  * Description: Retrieve $list item into longstring
* $Function: $ListInf
  * Description: Retrieve $list item into string
* $Function: $ListInfI
  * Description: Retrieve $list item into image
* $Function: $ListIns_Lstr
  * Description: Insert string into a $list
* $Function: $ListIns
  * Description: Insert string into a $list
* $Function: $ListInsI
  * Description: Insert image into a $list
* $Function: $ListLoc
  * Description: Locate string in $list
* $Function: $ListLup
  * Description: Locate string in $list, searching backwards
* $Function: $ListMove
  * Description: Move a $list
* $Function: $ListNew
  * Description: Create empty $list
* $Function: $ListNewA
  * Description: Create array of empty $lists
* $Function: $ListNewAI
  * Description: Create array of empty $lists associated with image
* $Function: $ListNewI
  * Description: Create empty $list associated with image
* $Function: $ListOvl
  * Description: Overlay part of $list item with string
* $Function: $ListOvlI
  * Description: Overlay part of $list item with image item
* $Function: $ListRem
  * Description: Remove item from $list
* $Function: $ListRep_Lstr
  * Description: Replace a $list item with a longstring
* $Function: $ListRep
  * Description: Replace a $list item with a string
* $Function: $ListRepI
  * Description: Replace $list item with an image
* $Function: $ListRst
  * Description: Restore global $list
* $Function: $ListSav and $ListSave
  * Description: Save global $list
* $Function: $ListSavL
  * Description: Count and names of available global $lists
* $Function: $ListSort and $ListSrt
  * Description: Sort $list
* $Function: $ListSub
  * Description: Create $list that is subset of input $list
* $Function: $ListUpd
  * Description: Produce $list from input $list using $list of updates
* $Function: $Lowcase
  * Description: A lower case string translated from an uppercase or mixed case string
* $Function: $LstFld
  * Description: Field names in a file, along with their field descriptions, into an image.
* $Function: $LstProc
  * Description: Information that is stored for a procedure.
* $Function: $Lstr_Add_UserBuffer
  * Description: Add longstring to user buffer
* $Function: $Lstr_Base64_Decode
  * Description: Convert from base 64 to byte string
* $Function: $Lstr_Base64_Encode
  * Description: Convert byte string to base 64
* $Function: $Lstr_C2X
  * Description: Convert byte string to hexadecimal
* $Function: $Lstr_Get_Image and $Lstr_Set_Image
  * Description: Longstring to/from image
* $Function: $Lstr_Get_Userbuffer
  * Description: Get user buffer contents to a longstring
* $Function: $Lstr_Global and $Lstr_Session
  * Description: Bind to global/session longstring
* $Function: $Lstr_Global_Del and $Lstr_Session_Del
  * Description: Delete global or session longstring
* $Function: $Lstr_Global_Get and $Lstr_Session_Get
  * Description: Get global or session longstring
* $Function: $Lstr_Global_Set and $Lstr_Session_Set
  * Description: Set global or session longstring
* $Function: $Lstr_Index
  * Description: Find a string inside a longstring
* $Function: $Lstr_Left
  * Description: Leftmost characters of a longstring
* $Function: $Lstr_Len
  * Description: Length of a longstring
* $Function: $Lstr_Parse
  * Description: Part of longstring preceding character in delimiter set
* $Function: $Lstr_ParseX
  * Description: Part of longstring following character in delimiter set
* $Function: $Lstr_Right
  * Description: Rightmost characters of a longstring
* $Function: $Lstr_Set_UserBuffer
  * Description: Set user buffer to longstring value
* $Function: $Lstr_Substr
  * Description: Substring of a longstring
* $Function: $Lstr_SubWord
  * Description: Substring of a longstring using word counts
* $Function: $Lstr_Translate
  * Description: Translate longstring
* $Function: $Lstr_Unblank
  * Description: Remove extraneous blanks from longstring
* $Function: $Lstr_Windex
  * Description: Return the position of a word within a long string
* $Function: $Lstr_Word
  * Description: Return a word from a long string
* $Function: $Lstr_Words
  * Description: Return the number of words in a long string
* $Function: $Lstr_X2C
  * Description: Convert from hexadecimal to byte string
* $Function: $Lstr
  * Description: Treat a string as longstring
* $Function: $MisGrup
  * Description: Group name if the error occurred in group context, null if in file context.
* $Function: $MisLoc
  * Description: Location of a missing member or file.
* $Function: $MisName
  * Description: File name of a missing member or file.
* $Function: $MisNum
  * Description: Number of files that failed in a group.
* $Function: $MisStmt
  * Description: Statement that caused the ON unit to be entered.
* $Function: $Mod
  * Description: Remainder that results when one argument is divided by the another argument.
* $Function: $Occurs
  * Description: Whether a field has the OCCURS attribute.
* $Function: $Oneof
  * Description: Table lookup that can replace a series of IF conditions.
* $Function: $Pack
  * Description: Packed decimal representation of a string.
* $Function: $Pad
  * Description: Designated character padded to the left.
* $Function: $PadR
  * Description: Designated character padded to the right
* $Function: $Parse
  * Description: Part of string preceding character in delimiter set
* $Function: $ParseX
  * Description: Part of string following character in delimiter set
* $Function: $Post
  * Description: Indicates that an event has occurred; the thread waiting on an ECB can resume processing.
* $Function: $PrcLEx
  * Description: $list of information about procedures in file
* $Function: $PrcLExG
  * Description: $list of information about procedures in group or file
* $Function: $Priorty
  * Description: Change a user's priority
* $Function: $Proc_List
  * Description: $list of information about procedures in file
* $Function: $Proc_ListG
  * Description: $list of information about procedures in group or file
* $Function: $Proc_Touch
  * Description: Change a procedure's last-update date and user
* $Function: $ProcCls
  * Description: Close procedure before reaching end
* $Function: $ProcDat
  * Description: Add lines from procedure to $list
* $Function: $ProcGet
  * Description: Next line of procedure
* $Function: $ProcLoc
  * Description: Locate any of set of strings in procedure
* $Function: $ProcOpn
  * Description: Open procedure for $ProcDat, $ProcGet, $ProcLoc
* $Function: $Random_Seed
  * Description: Build seed specifying series of $Random results
* $Function: $Random
  * Description: Get next random number
* $Function: $RdProc
  * Description: Lines of a User Language procedure, retrieved in sequential order, that is stored in a Model 204 file.
* $Function: $Read
  * Description: An echo of the data a user enters as a request is evaluated
* $Function: $ReadInv
  * Description: Performs the same function as $READ, except that input from the terminal is not echoed.
* $Function: $ReadLc
  * Description: An echo of the data a user enters as a request is evaluated, except that case translation is deactivated, regardless of the current *UPPER or *LOWER setting.
* $Function: $RegexMatch
  * Description: Whether string matches regex
* $Function: $RegexReplace
  * Description: Replace matching strings
* $Function: $Remote
  * Description: VTAMNAME value of the originating Model 204 region when using SNA Communications Server (formerly VTAM) TRANSFER to transfer between Model 204 regions.
* $Function: $ResetN
  * Description: Reset or view M204 parameter
* $Function: $Reverse
  * Description: Reversed order of a string.
* $Function: $RlcFile
  * Description: Name of the file in which the last record locking conflict occurred.
* $Function: $RlcRec
  * Description: Internal record number for which the last record locking conflict occurred.
* $Function: $RlcUid
  * Description: ID of the user who caused an ON FIND CONFICT or ON RECORD LOCKING CONFLICT. If the conflicting user is on a remote node, $RLCUID also returns the name of the node.
* $Function: $RlcUsr
  * Description: User number of the user with which the request conflicted when the last record locking conflict occurred.
* $Function: $Round
  * Description: Number, rounded to a specified number of decimal places.
* $Function: $Scan
  * Description: After comparing two strings, a number equal to the first position within the first string at which the second string appears; the same function as $INDEX.
* $Function: $Sclass
  * Description: Current user's subsystem user class.
* $Function: $Screen_attr
  * Description: Get screen item attributes as a blank-delimited string
* $Function: $Screen_clear
  * Description: Clear tagged and/or modified attributes in a screen
* $Function: $Screen_mod
  * Description: Set modified attribute for a screen item
* $Function: $ScrHide
  * Description: Hide lines in SCREEN
* $Function: $ScrSize
  * Description: Change size of field on SCREEN
* $Function: $ScrWide
  * Description: Allow SCREEN to accept fields wider than 79
* $Function: $Session, $Session_Id, $Session_Owner,   and $Session_Timeout
  * Description: Return values for currently open session
* $Function: $Session_Close
  * Description: Close an open session
* $Function: $Session_Create
  * Description: Create a new session
* $Function: $Session_Delete
  * Description: Delete a session
* $Function: $Session_List
  * Description: Get list of sessions
* $Function: $Session_Open
  * Description: Open a session
* $Function: $Setg_Subsys_List
  * Description: Get list of subsystem-wide globals
* $Function: $Setg_Subsys
  * Description: Set subsystem-wide global
* $Function: $Setg_Sys_List
  * Description: Get list of system-wide globals
* $Function: $Setg_Sys
  * Description: Set system-wide global
* $Function: $Setg
  * Description: A created or changed entry in the global variable section of GTBL.
* $Function: $SetL
  * Description: Sets the current line counter for the output device currently in effect.
* $Function: $SetP
  * Description: Sets the current page number for the output device currently in effect.
* $Function: $SetStat
  * Description: Set local system statistic
* $Function: $Sir_Date
  * Description: Get current datetime
* $Function: $Sir_DateFmt
  * Description: Validate datetime format
* $Function: $Sir_DateN
  * Description: Current date and time as number of seconds/300
* $Function: $Sir_DateND
  * Description: Current date as number of days
* $Function: $Sir_DateNM
  * Description: Current date and time as number of milliseconds
* $Function: $Sir_DateNS
  * Description: Current date and time as number of seconds
* $Function: $Sir_Date2N
  * Description: Convert datetime string to number of seconds/300
* $Function: $Sir_Date2ND
  * Description: Convert datetime string to number of days
* $Function: $Sir_Date2NM
  * Description: Convert datetime string to number of milliseconds
* $Function: $Sir_Date2NS
  * Description: Convert datetime string to number of seconds
* $Function: $Sir_Login
  * Description: Perform secured web or sockets login
* $Function: $Sir_ND2Date
  * Description: Convert datetime number of days to string
* $Function: $Sir_NM2Date
  * Description: Convert datetime number of milliseconds to string
* $Function: $Sir_NS2Date
  * Description: Convert datetime number of seconds to string
* $Function: $Sir_N2Date
  * Description: Convert datetime number of seconds/300 to string
* $Function: $Sir_Wild
  * Description: Test string against a wildcard string
* $Function: $SirJGet
  * Description: Place audit trail data on $list
* $Function: $SirMsg
  * Description: Line of current $SirMsgP procedure
* $Function: $SirMsgP
  * Description: Load procedure for retrieval via $SirMsg
* $Function: $SirParm
  * Description: Set user-specific value, controlling Sirius products
* $Function: $SirProd
  * Description: Determine availability of Sirius product or capability
* $Function: $SirSite
  * Description: Current Sirius customer site ID
* $Function: $SirTime
  * Description: Current time as YYDDDHHMISSXX
* $Function: $SirVer
  * Description: Current version number of Sirius product
* $Function: $SirWarn
  * Description: Send warning or message to user(s)
* $Function: $Slstats
  * Description: Resets the recording of since-last statistics anywhere within a request.
* $Function: $SndMail
  * Description: Send an email message
* $Function: $Sndx
  * Description: SOUNDEX code of an argument.
* $Function: $Square
  * Description: A number multiplied by itself.
* $Function: $SsStat
  * Description: Retrieve subsystem's statistics into string
* $Function: $SsStatL
  * Description: Retrieve statistics for set of subsystems into $list
* $Function: $Stat
  * Description: Current value of any user statistic.
* $Function: $StatD
  * Description: Calculate differences and rates for statistics strings
* $Function: $StatLD
  * Description: Calculate differences and rates for statistics $lists
* $Function: $Status
  * Description: The success or failure of the last executed external I/O or program communication statement.
* $Function: $StatusD
  * Description: More detailed description of a condition returned by $STATUS.
* $Function: $Str
  * Description: Treat a longstring as string
* $Function: $StrAnd
  * Description: Bit-wise AND two strings
* $Function: $Strip
  * Description: A number with suppressed leading zeros.
* $Function: $StrOr
  * Description: Bit-wise OR two strings
* $Function: $StrXor
  * Description: Bit-wise exclusive OR two strings
* $Function: $SubCnt
  * Description: Count occurrences of one string in another
* $Function: $SubErs
  * Description: Remove occurrence of one string from another
* $Function: $SubIns
  * Description: Insert string inside another string
* $Function: $SubRep
  * Description: Replace occurrences of string
* $Function: $Substr
  * Description: Substring of a string.
* $Function: $Subsys
  * Description: Status of an APSY subsystem.
* $Function: $SyStat
  * Description: Retrieve system statistics into string
* $Function: $TableC
  * Description: Information provided by TABLEC command
* $Function: $TermId
  * Description: Terminal ID of current user thread
* $Function: $Time
  * Description: Current time in hh:mm:ss format.
* $Function: $TkStat
  * Description: Retrieve task's statistics into string
* $Function: $TkStatL
  * Description: Retrieve statistics for all tasks into $list
* $Function: $TsoAtt
  * Description: Attach program in user's TSO address space
* $Function: $TsoCall
  * Description: Call program in user's TSO address space
* $Function: $TsoCan
  * Description: Cancel program invoked via $TsoAtt
* $Function: $TsoCmd
  * Description: Invoke command in user's TSO address space
* $Function: $TsoExec
  * Description: Invoke CLIST in user's TSO address space
* $Function: $TsoExit
  * Description: Terminate TSO full screen interface session, stack command
* $Function: $TSOId
  * Description: TSO userid user's thread
* $Function: $TSOStat
  * Description: Status of program invoked via $TsoAtt
* $Function: $TSOWait
  * Description: Wait for program invoked via $TsoAtt to complete
* $Function: $Unbin
  * Description: Value converted from binary to string representation.
* $Function: $Unbind_and_$UnbindW
  * Description: $Unbind and $UnbindW: Unbind resource previously bound via $Bind
* $Function: $Unblank
  * Description: Contents of an argument, removing leading and trailing blanks, and compressing multiple embedded blanks to one blank character.
* $Function: $Unfloat
  * Description: Character string that represents a numeric counted string of 4 or 8 bytes, which contains a floating point.
* $Function: $Unpack
  * Description: Unpacked decimal data 
* $Function: $UnPost
  * Description: Resets a specified Event Control Block (ECB) to an unposted state.
* $Function: $UnqRec
  * Description: In the case of a uniqueness violation, returns the file-relative record number of the record that already contains the field name = value pairIf no uniqueness violation occurred. returns -1.
* $Function: $UnSpace
  * Description: Normalize spaces and quotes
* $Function: $Upcase
  * Description: An uppercase string converted from a lower or mixed case string.
* $Function: $Update
  * Description: Name of the group update file or the current file.
* $Function: $UpdFile
  * Description: Name of the file in which a field level constraint violation has occurred, or a blank if no violation occurred.
* $Function: $UpdFld
  * Description: Name of the field for which a field level constraint violation has occurred, or a blank if no violation occurred.
* $Function: $UpdLoc
  * Description: Location name (node name) of the current update unit (Parallel Query Option/204 only).
* $Function: $UpdOval
  * Description: Value of the original field occurrence causing a constraint violation, when invoked from an ON FCC unit following the detection of an AT-MOST-ONE field-level constraint conflict. 
* $Function: $UpdRec
  * Description: File-relative record number of the record whose update caused a field level constraint violation, or -1 if no violation occurred.
* $Function: $UpdStat
  * Description: Numeric value denoting the type of field level constraint violation that has occurred, or 0 if no violation occurred.
* $Function: $UpdStmt
  * Description: Type of User Language updating statement causing a field level constraint violation, or a blank if no violation occurred.
* $Function: $UpdVal
  * Description: Field value causing a field level constraint violation, or a blank if no violation occurred.
* $Function: $UseASA
  * Description: Prevent carriage control in USE output
* $Function: $User
  * Description: User's user number.
* $Function: $Userid
  * Description: User ID under which the user is logged in.
* $Function: $UsrPriv
  * Description: Whether a user ID has been granted specific Model 204 privileges.
* $Function: $UsStat
  * Description: Retrieve user's statistics into string
* $Function: $UsStatL
  * Description: Retrieve statistics for set of users into $list
* $Function: $Verify
  * Description: Whether every character in one string is present in a second string.
* $Function: $View
  * Description: Value of a parameter.
* $Function: $Vnum
  * Description: Whether a given argument is in a valid format for a SORT BY VALUE NUMERICAL statement or for any type of mathematical operation.
* $Function: $Wait
  * Description: Suspend a user until an Event Control Block (ECB) is posted.
* $Function: $WakeUp
  * Description: Pause user until specified time
* $Function: $Web_xxx
  * Description: List of Janus Web Server $functions
* $Function: $Windex
  * Description: Word number of first occurrence of word in phrase
* $Function: $Word
  * Description: P word in a specified string, delimited by a blank or optionally specified character.
* $Function: $Words
  * Description: Number of words in a specified string, delimited by a blank or optionally specified character.
* $Function: $X2C
  * Description: One-byte EBCDIC characters translated from a string of 2-byte hexadecimal character.
* $Function: $X2D
  * Description: Convert hex string to integer


A PRINT statement can include a function call anywhere a field name can be used. For example:

IF $EDIT(2573,'99999') EQ 02573 THEN...

The direct use of function calls in some statements, such as FIND, results in compilation errors.

Each function returns a single value to the calling request. For example, this function sets %A equal to the user's login account name:

This function causes the length of the current value of the FULLNAME field to be compared to 10:

IF $LEN(FULLNAME) GT 10 THEN . . .

Some functions require one or more arguments which allow you to pass information to the subroutine. Some functions take no arguments. Arguments must be enclosed in parentheses and separated by commas. For example:

IF $READ('CONTINUE?') EQ 'YES' THEN . . . %X = $mod(COUNT IN CT, %BASE)

Function arguments follow the normal rules for arithmetic expressions. Arguments can include other function calls and can perform any type of computation. See the section [Expressions](about:/index.php?title=Using_variables_and_values_in_computation#Expressions "Using variables and values in computation") for detailed information on expression syntax.

Model 204 evaluates each argument to a function as either a string or a number. When this data type conflicts with the data type required by the function, the following rules apply:

In addition to the functions provided by SOUL, some customer sites write their own functions based on their particular needs. Before using any user-written functions, make sure that your site's FUNU module has been reassembled with the macro library supplied with the current release. Also, please be sure to carefully test your user-written functions before your system goes into production.

Customers are individually responsible for any functions they write. Rocket Software takes no responsibility for user-written functions or their documentation.