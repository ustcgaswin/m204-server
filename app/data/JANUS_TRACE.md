## JANUS TRACE

Set Janus TRACE level

This command sets or overrides the trace settings for the named Janus port(s), determining which, and how much, information is written to the Model 204 journal. You specify a bit mask integer setting that sums the values of the tracing options you want to invoke. The trace setting for a port is initially set to the value specified for the TRACE parameter on the JANUS DEFINE command for the port. The JANUS TRACE command will override this.

**Contents**

1 **Syntax**
    1.1 **Syntax terms**
    * `portname` The name of the port(s) to modify.
    * `ipaddr` The remote IP address for which to set trace options. The IP address should be specified in either:
        * IPv4 standard 32-bit "dotted-decimal" form: for example, 198.242.244.97.
        * IPv6 standard (RFC 5952) 128-bit form: for example, 2011:0ab4:15a1::4a0e:0280:5274
        * The IPV6 form is supported as of version 7.7 of Model 204.
        * A host name cannot be specified on the JANUS TRACE command.
        * If neither `ipaddr` nor `ipsubnet` is specified, the JANUS TRACE command is assumed to apply to all connections on the port.
    * `ipsubnet` The IP subnet for which to set trace options. The IP subnet can be specified either in the standard dotted subnet and mask form, for example, 198.242.244.0/255.255.255.0, or a dotted IP address followed by the number of subnet bits, as in 198.242.244.0-24. All bits that are not part of the subnet must be 0. If neither `ipaddr` nor `ipsubnet` is specified, the JANUS TRACE command is assumed to apply to all connections on the port.
    * `value` The bit-mask integer value to use for tracing the indicated connections. This controls the quantity of trace information logged to the audit trail. You specify an integer that sums the values of the options you want to have logged:
        * 1: Log the SQL request on SDS or URL rules processing on WEBSERV ports.
        * 2: Log RPC requests on OPENSERV ports.
        * 4: Log generated User Language (SDS ports only).
        * 8: Log in hex and char all activity on port.
        * 16: Log Persistent Client State (cookies) on WEBSERV ports.
        * 32: Trace web response headers.
        * 64: Trace web response text.
        * 128: Trace data between TCP and SSL.
        * `OFF`: Indicates that no TRACE override is to be used for the specified IP address or addresses.

2 **Usage notes**
* JANUS TRACE does not affect the TRACE setting on the port definition, although it may override it.
* JANUS TRACE may be issued while an online is running, and the order in which JANUS TRACE commands are specified is irrelevant.
* The JANUS TRACE command to be associated with a particular connection is the one that matches the IP address and has the most subnet bits.
* If a JANUS TRACE command exactly matches the IP address, it will always be used.
* If no IP address-specific rule is found, the subnet command with the most matching bits will be used.
* If two subnet-based JANUS TRACE commands with exactly the same number of subnet bits match an IP address, it is unpredictable which command will apply.
* If no JANUS TRACE commands match the IP address for a connection, the trace setting for the port will be the setting of the TRACE parameter on the port definition.

3 **See also**
* List of Janus commands