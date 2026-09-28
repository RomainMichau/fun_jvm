# TODO

- `AccessController.doPrivileged` (`WKAccessController.scala`) only handles the happy path: it calls `action.run()` and returns the result. It does not wrap a thrown exception into `PrivilegedActionException`, since the interpreter has no real exception-handling/try-catch support yet.
- `SecurityManager` and the permission model in general are not implemented at all: no `SecurityManager` instance, no `AccessControlContext`, no `checkPermission`/`checkRead`/etc. `doPrivileged` is a pure passthrough (runs the action, no actual privilege elevation or stack-based access control).
