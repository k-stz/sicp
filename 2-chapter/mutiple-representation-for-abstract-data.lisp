(in-package :sicp)

;; sometimes we want to use more than one underlying representation for particular
;; data. For example for Complex numbers a rectangle and a polar representation.
;; Or when writing a huge system with many programmers it might not be possible
;; to agree early on what representation to use, or even what kind of data to
;; combine or keep seperate. That's why different parts of a program have to
;; deal with different representation.

;; This can be implemented using type tags and generic operation
;; (think data types and clos generic functions?)

;; /generic procedures/ can operate on data that can be represented in more than
;; one way. Think like in the huffmann tree example where `symbols' could operate
;; on leafs and partial trees/branches, where it would check the type using
;; the "leaf" element as a "type tag", dispatch the appropirate code.

;; /dispatching on type/, is what we speak of when we first check the type of
;; an object and then decide what procedure to call on it. So just like with
;; CLOS and generic functions where depending on the argument types a particular
;; defined method is called, or rather dispatched!

;; What are weaknesses of the generic procedure interface?
;; (1) It is not /additive/ - whenever we want to add a new data representation
;; we, the implementors, must change all the generic procedures that use
;; the data, and we need to know all the generic functions in a large-scale
;; system in order to modify all the generic functions involved. A source
;; of inconvenience and error.
;; (2) The people interface with the individual representation must also look out not to
;; run into naming conflicts by using our code.

;; Solution to the the two problems:
;; (1) the dispatch mechanism gets removed from the generic procedure into a table
;; with a operation argument-type(s) as index and appropriate method as entry.
;; Now using a (apply-generic 'operation-name typed-argument) will lookup the
;; method to be called, and dispatch them.
;; (2) We create packages.
