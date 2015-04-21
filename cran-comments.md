## General
This is an update with the following new features:

* Allow short-hand notation for predicate functions, e.g.
  `ensure(1:10, is.integer)` is equivalent to
  `ensure(1:10, is.integer(.))`.
* New function `check_that` which works like `ensure_that` but returns
  `TRUE`/`FALSE`.
* An updated vignette to reflect the above

There are a few technical adjustments:

* The `value.` argument ` functions is renamed to `.` for consistency.
* `ensure_that` can be imported without the need to also import
  `ensures_that`.

## Test environments
* local Windows installation (R 3.2.0)
* local Mac OS X (R 3.1.2)
* win-builder (devel: 2015-04-21 r68210 and release: R 3.2.0)

## R CMD check results
There were no ERRORs or WARNINGs.

There was one NOTE (I removed a middle name):
New maintainer:
  Stefan Milton Bache <stefan@stefanbache.dk>
Old maintainer(s):
  Stefan Holst Milton Bache <stefan@stefanbache.dk>

License components with restrictions and base license permitting such:
  MIT + file LICENSE
File 'LICENSE':
  YEAR: 2015
  COPYRIGHT HOLDER: Stefan Milton Bache
  
## Downstream dependencies
There are no downstream dependencies.
