## JANUS WEB REDIRECT

WEB REDIRECT

The JANUS WEB REDIRECT subcommand identifies the URL to which incoming URL requests that match a specified pattern are redirected.

**Syntax**

```
JANUS WEB portname REDIRECT [method] url redir_url
```

`portname`, `method`, and `url` are explained in the JANUS WEB command.
`redir_url` is the URL to which requests that match the pattern in `url` are redirected.

Just as `url` can contain wildcard characters, `redir_url` can also contain wildcard characters. The wildcard characters in `redir_url` are replaced with the characters that matched the corresponding wildcard characters in `url` at the time of the request.

For example, given the following rule, a request for URL `/JUNK/MAIN.HTML` is redirected to `HTTP://LOCAL.NTBOX.COM/JUNK/MAIN.HTML`:

```
JANUS WEB WEBPORT REDIRECT /JUNK/* HTTP://LOCAL.NTBOX.COM/JUNK/*
```

The redirection URL can be an absolute URL (that is, `http://` and the host name along with a path), or it can be a relative URL (containing only the path). Thus a redirect can redirect to another machine, another port on the same machine, or simply another path on the same port on the same machine. All of these types of redirection can be useful at times.

**See also**

* List of Janus commands
* List of JANUS WEB subcommands
* Defining Web rules
* List of Janus Web Server functions

Category: JANUS WEB subcommands