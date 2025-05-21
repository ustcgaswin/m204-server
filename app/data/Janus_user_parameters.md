## Janus user parameters

Some aspects of Janus behavior can be controlled by user parameters, that is, parameters that
* can be set using the Model 204 RESET command by any logged-in user
* affect only the thread on which they are issued

At logout, all Janus user parameters are set back to the values they had when the thread was defined, that is, during Online initialization.

A number of parameters affect Janus performance and operational characteristics, but the two primary ones are:

* **JANDEBM parameter** - a bitmask parameter that controls the display of messages and data that are destined for the browser while debugging a Janus Web Server request with the JANUSDEBUG command.
* **SRSPARM parameter** - a bitmask parameter that controls the behavior of the Janus Web Server saved record set and $list functions: $Web_Save_Recset, $Web_Save_List, $Web_Rest_Recset and $Web_Rest_List.

### See also

* Janus TCP/IP Base
* Janus Web Server
* Defining server ports
* Janus environment definition
* Storage requirements
* Janus commands

**Category:** Janus TCP/IP Base