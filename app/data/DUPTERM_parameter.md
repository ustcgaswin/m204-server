## DUPTERM parameter

**Number of duplicate terminals**

**Contents [hide]**
1. Summary
2. Description
3. Usage notes
4. Example

**Summary**
* Default value: 0
* Parameter type: User
* Where set: On user's parameter line
* Related products: All
* Introduced: Model 204 V4.2

**Description**
The IODEV parameter supports duplication within a single device type. The DUPTERM parameter indicates the number of duplications.

**Usage notes**
As of version 7.4, if DUPTERM is specified on the first IODEV parameter line of a particular type, then the NOTERM parameter must also be present. Otherwise, the IODEV line is rejected with the following message, and the Online terminates:
* M204.2651: NOT ALL THREADS PROCESSED FOR IODEV x
* If the number of IODEV lines exceeds NUSERS:
    * If DUPTERM is not used, the extra IODEV lines are ignored.
    * If DUPTERM is used, you are notified that not all the threads for a specified IODEV are processed.
    * If DUPTERM and FUNTSKN are specified, the extra IODEV lines are used for Fast/Unload tasks, and you are notified if some other threads do not open.

**Example**
In the following example:
* NUSERS=15 represents User 0, three IODEV43s, seven IODEV41s, and four IODEV7s.
* IODEV=43 is defined without using the DUPTERM parameter.
* IODEV=41 is defined using the DUPTERM parameter by specifying the DUPTERM=nnn, where nnn is the number of times a terminal is duplicated.
Notice that NOTERM must be defined on the first IODEV line, and it must indicate the total number of similar devices to be defined. In addition, POLLNO is no longer required. (It increments from 1 for each new IODEV.)
* NOTERM=3, IODEV=43
    * IODEV=43, SERVSIZE=100000
    * IODEV=43, POLLNO=3, SERVSIZE=50000
* NOTERM=7, IODEV=41, DUPTERM=2, SERVSIZE=50000
    * IODEV=41, SERVSIZE=75000
    * IODEV=41, DUPTERM=2, SERVSIZE=100000
* NOTERM=4, IODEV=7, DUPTERM=3
    * <- POLLNO verified
    * <- plus 2 identical devices
    * <- plus 2 identical devices
    * <- plus 3 identical devices

The following CCAPRINT change occurs when DUPTERM is used:
* NOTERM=7, IODEV=41, DUPTERM=2, SERVSIZE=50000
    * USER 4
        * NOTERM=7
        * IODEV=41
        * NUMBER OF TERMINALS ON LINE
        * TYPE OF I/O DEVICE
        * DUPTERM=2
        * NUMBER OF DUPLICATE TERMINALS
        * <- different
    * SERVSIZE 50000
        * <- different
    * USER 5 DUPLICATED
        * <- different
    * USER 6 DUPLICATED
    * USER 7
        * MAXIMUM SIZE OF THIS SERVER
        * IODEV=41, SERVSIZE=75000
        * TYPE OF I/O DEVICE
        * MAXIMUM SIZE OF THIS SERVER

**Note:** The text following each <- marker is a documentation comment.

**Categories:** User parameters, Parameters
