## GTBLHASH parameter

**Summary**

* Default value: 0
* Parameter type: User
* Where set: By any user
* Related products: All
* Introduced: Model 204 V5.1

**Description**

The number of hash buckets allocated in GTBL to hold global string variable names. The benefit of using GTBLHASH is that the scan time and CPU time to find and update global string variables is reduced, typically, by a factor of GTBLHASH. When a global string variable is set or retrieved, the name of the global is hashed into one of the GTBLHASH buckets. Once the bucket is determined, the scanning and updating works as it did prior to Version 5.1, except there is less data to move when a value is deleted or its size changes.

By default, when you place a global string variable in GTBL, it is stored in order-of-entry sequence. If you set GTBLHASH to a nonzero value the data is placed in GTBL in the order determined by the hash value of the global string variable name.

It is recommended that you set the number of buckets to a prime number, which is not greater than the expected maximum number of global variables divided by four. You can find your optimal value through experimentation, and that you start with a relatively small number such as five or seven.

**Resetting the GTBLHASH parameter clears the global table.**

**Calculating GTBL space requirements**

When hashed GTBL is being used, GTBL requires an extra 8 bytes for every hash bucket, which means that you should probably increase LGTBL by at least 8 times the value of GTBLHASH. Since hashing does not distribute hash values exactly evenly between buckets, you should probably increase the size of GTBL by somewhat more than this for a little extra space in each bucket.

7 * (8 + 20 * 3) = 476.

It is recommended that you increase LGTBL by GTBLHASH * (8 + av\_len * 3) where av\_len is the average expected length of the global string variable names, plus their values. So, if GTBLHASH is being set to seven and the average expected length of globals and their values is 20, LGTBL would be increased as follows:

**Rearranging GTBL and tracking the rearrangements**

The benefit of using GTBLHASH is that the CPU time to find and update globals is typically reduced by a factor of GTBLHASH. If there is no room to fit a global variable into its target bucket or there is no room to add a global string variable to GTBL, the hashed GTBL feature rearranges GTBL to make room. These rearrangements can be CPU intensive and could become a performance problem, if performed frequently. Because of this, the following statistics are available as system statistics, user statistics and since-last statistics to keep track of GTBL rearrangements required for the hashed GTBL feature:

**Statistic** | **Tracking**
------- | --------
GTBLRU | Number of GTBL rearrangements required to add a string variable global.
GTBLRS | Number of GTBL rearrangements required to add a global object.

After reviewing the GTBLRU and GTBLRS statistics, you can consider taking the following actions:

* If both of these values are high, increase the size of GTBL by increasing LGTBL.
* If GTBLRU is high but GTBLRS is not, increase GTBLPCT or decrease GTBLHASH.
* If GTBLRS is high but GTBLRU is low, decrease GTBLPCT.

For a discussion of the performance improvement possible handling global string variables using GTBLHASH, see Global string variables.

**Categories:** User parameters, Parameters
