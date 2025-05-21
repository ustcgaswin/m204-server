## Daemon class

The Daemon class provides a facility for issuing Model 204 commands from inside a User Language request, simulating the calling of programs as if they were subroutines. Daemon objects are complex and offer several advantages over multiple function calls or even login/logout sequences.

**Advantages:**

* They can execute multiple functions defined in the Sirius Sfunctions (SCommBg, SCommand, SCommndL), which issue Sirius commands to a command on another thread on behalf of the issuing user. Daemon objects offer a number of advantages over multiple function calls or even login/logout sequences.
* From within a User Language program, you can issue commands that cannot be executed within that program (e.g., REORG).
* Within a User Language program, you can call two log-in/log-out routines.
* The daemon API is implemented as a set of functions and properties, documented as the Daemon methods.

**Working with Daemon objects**

In this Wiki, the terms "daemon" or "daemon thread" are used to mean the daemon thread that is activated or involved when you employ or operate on a Daemon object. The user or thread that invokes a daemon is the "master" or "master thread." If a daemon is the "parent" of another daemon, both daemons share a single updating transaction.

**Daemon methods**

(List of methods is too extensive to reproduce here)

**Deleting a Daemon**

You can delete a Daemon instance from the issuing thread level or the daemon thread level.  As soon as the Discard returns, the count of daemons used by that master thread is reduced by one.

**Daemon example**

(Code example provided)

**Daemon record locking**

Prior to Sirius Mods version 6.8, daemons running on behalf of other threads could suffer record locking conflicts. As of version 6.8, this possibility is eliminated for transactional daemons.

**Transactional daemons**

Transactional daemon objects have a Boolean parameter called `Transactional`.  If `Transactional` is true, the master and daemon threads share a single updating transaction.

**Asynchronous and Independent daemons**

Daemon spawning methods that return immediately, before their processing is complete.

**RunAsynchronously**

When the daemon commands are all processed, the daemon goes back to waiting for the master to tell it what to do.

**RunIndependently**

The daemon logs return from the method, and the daemon object is discarded.

**Thread limits**

There are limits on the number of background or independent requests that may be running.

**Return ToMaster in Daemons**

(Example provided)

**DaemonState enumeration**

The DaemonState enumeration describes the current state of a daemon thread.

**List of Daemon methods**

(List of methods is too extensive to reproduce here)
