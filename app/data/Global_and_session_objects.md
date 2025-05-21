## Global and session objects

Object references are, for building objects, able to respond. However, some inferences "go away" at the end of a request.  Global and session objects must be explicitly managed.

A global or session object, referenced by its unique (e.g., 215-digit) key, which is in `OBJECT` table, is valid.  If referenced by a global or session name, it is temporarily stored at the end of request.

Just as discussed, if a global object reference and session object reference has a similar name, the reference to the object is resolved by the global object reference, and the session object reference will be lost.

The multiple object references can be understood under multiple, even if the names are similar.

The global and session names are completely different. It is perfectly valid to have a session object which contains references to two separate objects, each with the same name.

When it is possible for a single session to open and close several objects.

A simple example, through the use of multiple variables.

**Binding global/session names to a variable**

The simplest way to object global is to bind it to a variable.

Global object storage can be specified on a variable declaration either explicitly or implicitly.

**Using the Object class**

The following shared methods are available for the Object class.

| Method | Description |
|---|---|
| `DiscardGlobal` | Discard a global object |
| `DiscardSession` | Discard a session object |
| `DiscardGlobal` | Discard global object reference |
| `DiscardSession` | Discard session object reference |
| `GetGlobal` | Get global object reference |
| `GetSession` | Get session object reference |
| `ClearGlobal` | Clear global object reference |
| `ClearSession` | Clear session object reference |
| `SetGlobal` | Set global object reference |
| `SetSession` | Set session object reference |

These global objects can be retrieved in a subsequent request.

This reference comparison to a global/session name that references a global/session name.

**Session close**

In more basic terms, when a session is closed, all objects can be accessed either directly from the session or indirectly.

The processing that occurs when a session is closed to garbage collection.

**Cleanup**

The garbage collection process involves several steps.

1. Marking reachable objects.
2. Marking unreachable objects.
3. Removing unreachable objects.

