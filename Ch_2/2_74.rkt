#lang racket

#|
    Insatiable Enterprises, Inc., is a highly de-
centralized conglomerate company consisting of a large num-
ber of independent divisions located all over the world. The
company’s computer facilities have just been interconnected
by means of a clever network-interfacing scheme that makes
the entire network appear to any user to be a single com-
puter. Insatiable’s president, in her first attempt to exploit
the ability of the network to extract administrative infor-
mation from division files, is dismayed to discover that, al-
though all the division files have been implemented as data
structures in Scheme, the particular data structure used varies
from division to division. A meeting of division managers
is hastily called to search for a strategy to integrate the files
that will satisfy headquarters’ needs while preserving the
existing autonomy of the divisions.
    Show how such a strategy can be implemented with data-
directed programming. As an example, suppose that each
division’s personnel records consist of a single file, which
contains a set of records keyed on employees’ names. The
structure of the set varies from division to division. Fur-
thermore, each employee’s record is itself a set (structured
differently from division to division) that contains informa-
tion keyed under identifiers such as address and salary.
    In particular:

a. Implement for headquarters a get-record procedure
that retrieves a specified employee’s record from a
specified personnel file. The procedure should be ap-
plicable to any division’s file. Explain how the individ-
ual divisions’ files should be structured. In particular,
what type information must be supplied?
                      
                      operations
          
           ____________|__________|__________|___
            |          |          |          |
           _|__________|__________|__________|
            |          |          |          |
divisions  _|__________|__________|__________| . . .
            |          |          |          |
           _|__________|__________|__________|
            |          |          |          |
                          .
                          .
                          .

• (put ⟨op ⟩ ⟨type ⟩ ⟨item ⟩) installs the ⟨item ⟩ in the table,
    indexed by the ⟨op ⟩ and the ⟨type ⟩.
•(get ⟨op ⟩ ⟨type ⟩) looks up the ⟨op ⟩, ⟨type ⟩ entry in the table and
    returns the item found there. If no item is found, get returns false.

Each division record is a set, which contains a set of information,
all structured differently.
But right now we're just looking at personnel records.
All contain a set of records, keyed on employee names, with the associated
data also a set, with information keyed under identifiers,
though with organizational variance.

For each divisional format of employee records, there needs to be a
(get-record employee-name) procedure
For each divisional format of an individual record, there needs to be
(get-information info-type record) procedure
It shouldn't be assummed that all information is keyed under the same-named key
This can still all be done in just one 2D table

Example:

Divisions:
USA
Can
Mex

Operations:
(get-record name)
(get-salary record)
(get-address record)

It is same to assume that all divisions will have a unique name, as well
as all record information having unique identifying values

(define get-record name division) (apply-generic 'get-record 'division name))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types: APPLY-GENERIC"
          (list op type-tags))))))

Must supply the division type (unique name works)
Could search through all divisions, or just pass the division name
as an argument. (c)

---
b. Implement for headquarters a get-salary procedure
that returns the salary information from a given em-
ployee’s record from any division’s personnel file. How
should the record be structured in order to make this
operation work?

Records should all be tagged with their division's type

(define (get-salary record) (apply-generic 'get-salary record))
^ division will be part of the record

get-salary for x-division will be stored in the table under
'get-salary 'x-division
Just need to get the procedure from the table.

---
c. Implement for headquarters a find-employee-record
procedure. This should search all the divisions’ files
for the record of a given employee and return the record.
Assume that this procedure takes as arguments an
employee’s name and a list of all the divisions’ files.

(assuming no duplicate employee names)
for every division in the table:
  try to get record for employee name
    if found: return
    else: continue

---
d.When Insatiable takes over a new company, what changes
must be made in order to incorporate the new person-
nel information into the central system?

Just need to add table entries for each different division
(and also ensure no name comflicts in division names)
|#