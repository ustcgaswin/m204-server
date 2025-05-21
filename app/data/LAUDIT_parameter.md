## LAUDIT parameter

**Line audit bits**

**Summary**

**Default value**
1

**Parameter type**
User

**Where set**
On user's parameter line or reset by system manager

**Related products**
All

**Introduced**
Model 204 V2.1 or earlier

**Description**
LAUDIT controls the auditing of logical input lines (LI, LP, and LS journal lines). These lines can be useful in reconstructing the events leading to a system crash. Lines are written on the journal after line editing, line continuation, and full screen automatic validation have been performed. Valid settings of LAUDIT are (options can be summed):

| Setting | Meaning |
|---|---|
| 1 | Write LI lines (lines input from user's terminal). |
| 2 | Write LS lines (items from full-screen input from user's terminal). |
| 4 | Write LP lines (lines included from procedures). |
| 8 | Write LR lines (items for an image, from terminal or external records). |
| 16 | Write QT lines (lines of SQL statement-level processing). |
| 32 | Write QT lines (line of SQL error messages) |
