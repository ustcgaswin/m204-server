## JANUS WEB rule matching order and examples

WEB rule matching order and examples
Each execution of a JANUS WEB command adds to the set of rules for the specified WEBSERV port. Individual rules cannot be deleted nor modified; all rules can be deleted only by stopping and deleting the port definition. However, this should not be necessary, as long as you follow the two golden rules:
1. Specify the most general rules first and the most specific last.
2. Specify a first rule that will "clear" all related rules.

The following example illustrates these principles:
JANUS WEB TEST21 DISALLOW
JANUS WEB TEST21 ALLOW IPADDR 198.242.244.0-24
JANUS WEB TEST21 ALLOW IPADDR 169.84.128.0-3
JANUS WEB TEST21 DISALLOW IPADDR 169.84.128.17
JANUS WEB TEST21 ALLOW USER

In this example, any previously specified ALLOW rules on port TEST21 are made obsolete by the first DISALLOW rule. Once the TEST21 access rules are cleared, two subnets are given access permission, and then a single specific IP address within one of those subnets has its access revoked. Finally, the ALLOW USER * forces logon processing for all users.

Similar processing takes place for redirection commands. To redirect a specified URL to another URL, use the REDIRECT command:
JANUS WEB SIRIUS REDIRECT *.jav http://sirius-software.com:8080/java/*.jav

In the example above, requests for Java executables are routed out of the Janus Web Server SIRIUS and onto a java directory at port 8080 at sirius-software.com. A block of redirection commands might look like this:
JANUS WEB SIRIUS NOREDIRECT
JANUS WEB SIRIUS REDIRECT .jav http://sirius-software.com:8080/java/*.jav
JANUS WEB SIRIUS REDIRECT *.jpg http://sirius-software.com:8001/images/*.jpg

In this example, all previously defined redirection is removed, Java executables are routed out of the Janus Web Server SIRIUS and onto a java directory at port 8080 at sirius-software.com, and JPEG images are redirected to an IMAGES directory on port 8001 at sirius-software.com.

TYPE rules are used to customize processing for certain patterns of incoming URLs. For example
JANUS WEB SIRIUS TYPE *.HTML HTML

The Janus Web Server default assumption is that the requested URL is stored in a Model 204 procedure. If the above TYPE rule is in effect and a request comes in for WELCOME.HTML, the server looks in the default procedure file for procedure HTML_WELCOME and returns its contents to the user.
Virtually any character that is valid in a URL specification is valid as part of a Model 204 procedure name, so the Janus Web Server satisfies by default a request for http://www.x.com:80/staff/bobsmith/welcome.html by sending the contents of the procedure "staff/bobsmith/welcome.HTML" from the default Model 204 file to the client.

To change this default behavior, you can use an ON rule to cause a request for a specific URL to initiate some other process, as in:
JANUS WEB SIRIUS ON /pensions/* OPEN GROUP PENSIONS CMD 'I PENSIONS_DRIVER'

In the above example, any request for access to the /pensions directory instead causes the Model 204 group PENSIONS to open and the routine PENSIONS_DRIVER to be invoked.

See also
* List of Janus commands
* List of JANUS WEB subcommands

Category: JANUS WEB subcommands
