#lang racket

#|
As a large system with generic operations
evolves, new types of data objects or new operations may
be needed. For each of the three strategies—generic opera-
tions with explicit dispatch, data-directed style, and message-
passing-style—describe the changes that must be made to a
system in order to add new types or new operations. Which
organization would be most appropriate for a system in
which new types must often be added? Which would be
most appropriate for a system in which new operations
must often be added?

- generic operations with explicit dispatch:
To add a new operation with any given type, a new procedure
(operation-function arg-type (. arg-types)) must be added.
Additionally, all existing procedures and types must be checked
against for name conflicts. Overall, this approach is tedious
and error prone.

- data-directed style:
To add a new type, a new column must be added to the table of operations
that handles how every procedure will use that type.
To add a new operation, a new row must be added that handles how every
type will be used.
* best for new operations

- message-passing style:
A new type can be added by a single procedure. A new procedure, however,
must be implemented as a named procedure type within all defined
data objects.
* best for new types


|#