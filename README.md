# SUMMARY

`emacs-salesforce` is a tiny package that facilitates one
[Salesforce](https://www.salesforce.com/)-related task: converting a
fifteen-character Salesforce object ID to an eighteen-character
object-ID-with-checksum.

This project is not associated with Salesforce in any way.

# MOTIVATION

At my job, I occasionally need to convert a fifteen-digit Salesforce
ID into the eighteen-character version.  I was advised by co-workers
to use a
[Chrome plugin](https://chrome.google.com/webstore/detail/salesforcecom-id-converte/kiagkehielelkabjcakhpekplnelkaol)
for this purpose, but I don't use Chrome and am generally loathe to
leave Emacs when it can be avoided.  So I researched the algorithm for
generating the three checksum characters and implemented it in Emacs
Lisp.

# API

## Interactive commands

- `(salesforce-append-id-suffix)`

  This command appends a three-character checksum to the
  word at point if it is a valid fifteen-character Salesforce ID at
  point, and raises an exception otherwise.
  
## Functions
  
- `(salesforce-id-suffix id)`

  Returns the three-character checksum for `id` if it is a valid
  Salesforce ID, or raises an exception otherwise.
  
- `(salesforce-id-convert id)`

  Takes a fifteen-character Salesforce ID and returns the same ID with
  its three-character checksum appended.  Raises an exception if `id`
  is not a valid Salesforce ID.
