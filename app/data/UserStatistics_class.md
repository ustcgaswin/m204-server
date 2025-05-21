## UserStatistics class

Intended for performance work, the `UserStatistics` class is designed to replace `$Stat` and similar tools. The New constructor takes a snapshot of the user stat block at the time the constructor is invoked.

### Contents

1. About the UserStatistics statistics
2. Example
3. List of UserStatistics methods

### About the UserStatistics statistics

The object holds two types of user statistics: Login and Request.

*   Login statistics (also called "Final") are those that keep data per user since the user's most recent login. The statistics available to the Model 204 `$Stat` function, as well as the OBJSWAP statistic for SOUL objects, are valid.
*   Request statistics are a combination of:
    *   Statistics that keep the current highwater marks for some work tables per user. Model 204 documentation includes these with "since last" statistics.
    *   Since last statistics, which keep data per user comprising only the user's most recent work unit (like compilation or evaluation); they are not accumulated.
Login and Request statistics are not mutually exclusive. Both Login and Request data are kept for some activities, so many statistics appear in lists of both types and are valid in all methods that are restricted to one type or the other. The Using system statistics topic and the SirMon documentation are good sources of information about the available statistics (note that not all SirMon statistics are included in a UserStatistics object). Where they occasionally differ in spelling the name of a statistic, "Using system statistics" is more likely to have what the UserStatistics methods require.

You can also view all the individual statistics contained in a UserStatistics object by applying the `ToString` method to it, as follows:

```
Print %statObject: ToString(Zeros=true)
```

This displays all of the Login statistics followed by all the Request statistics.

**Note:** The `ToString` method shown above can be applied implicitly: simply Print or Audit an object variable, and the `ToString` method is automatically applied to the object. `Print %statObject` is equivalent to `Print %statObject:ToString`.

### Example

```
%statStart is object userStatistics
%statEnd is object userStatistics
%statStart = new
processing
%statEnd = new
printText CPU: {%statEnd:difference (%statStart, 'CPU')}
printText DKRDS: {%statEnd:difference (%statStart, 'DKRD')}
```

Specifying a statistic that is not a User statistic, or that is not the type of statistic (Login or Request) that a method calls for, triggers an `UnknownStatistic` exception.

### List of UserStatistics methods

The List of UserStatistics methods shows all the class methods.

**Category:** System classes
