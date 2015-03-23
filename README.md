The `ensurer` R package
=======================

### Ensure values at runtime.

[![Build Status](https://travis-ci.org/smbache/ensurer.png?branch=master)](https://travis-ci.org/smbache/ensurer)

`ensurer` is a small utility package for `R` which provides a simple 
and light-weight mechanism for ensuring certain aspects of values at runtime.

`R` does not provide any mechanism for type-safety and since it is not
a compiled language, the risk of having unexpected results is there at 
runtime. `R` functions often accept different types for the same input and/or 
have different return types for different situations.
 
As an example, a query to a database or the scraping of a website might 
not return valid data, where "validity" can refer to a number of conditions.
It might be a positive or certain number of records; that all cases are complete;
that some column is weekly increasing; or simply that the result is a 
`data.frame`.

If one does not deal with these ambiguities and risks appropriately,
some resulting errors may be hard to track down and may propagate in
unexpected ways. It is desirable to 
get an error as soon as a value does not have the correct type or 
does not satisfy certain criteria.
 
"Ensuring values" is here meant as a "contract", or a set of conditions,
such that if a value does not comply an error is raised instantly
(unless special behavior is specified for the failure). 
An ensuring contract (a function) is created with the `ensures_that` function
(ideal for multiple use or readability with complex contracts). 
 
It is also possible to ensure properties on the fly using `ensure_that`
(ideal for simple, one-time contracts).

Using the `magrittr` pipe `%>%` greatly improves semantics of the
functionality provided by this package, but it is not necessary.

This package is not meant as a substitute for unit testing, and great
packages for this already exist, e.g. `testthat` by Hadley Wickham.
The `ensurer` package is ideal for scripts or programs where runtime
conditions may break the functionality, and where errors should be
raised as soon and clear as possible. Although a side-effect,
It is my experience that it also promotes better design decisions
at outset, and helps catch coding errors early on.

# Installation

The easiest way to install `ensurer` is using the `devtools` package:

```R
devtools::install_github("smbache/ensurer")
```

# Basic Examples

The following example shows how to define a contract ensuring that its input
is square, and how to use it.

```R
library(magrittr) # for the pipe -> cleaner semantics
library(ensurer) 

# To reference the value being evaluated, use the `.` placeholder.
ensure_square <- ensures_that(NCOL(.) == NROW(.))

# try it out: 
diag(5) %>%
  ensure_square  # passes, so returns the diagonal matrix

# This won't work, and an error is raised.
matrix(1:20, 4, 5) %>% 
  ensure_square

# On the fly contracts:
matrix(1:4, 2, 2) %>%
  ensure_that(is.matrix(.), all(is.numeric(.)))
```

One can specify several conditions, each separated with a comma. Simple
predicate functions can be used in abbreviated symbolic form, e.g as in the example below.
```R
ensure_square <- ensures_that(is.matrix, 
                              NCOL(.) == NROW(.))
```

Note that *all* conditions are tested to provide the most feedback upon failure.
If "short-circuits" are desired, one can add more (separate) ensuring contracts.

# Types and type-safe functions (new, experimental, subject to change)
A "type" is defined as objects satisfying the criteria of a corresponding
ensurer contract which has the prefix `type_`.
For example, an `foo` type will satisfy the conditions of
an ensurer contract `type_foo`. There are some built-in types, but
new types are easily defined using `ensures`/`ensures_that` and
adhering to the `type_` naming convention.

Type-safe functions can be made with `function_`:
```R
f <- function_(a ~ integer, b ~ character: "Hello, World!", {
  rep(b, a)
})

f(10) # fails
f(10L) # works
f(10L, "foo")

type_lm <- ensures_that("lm" %in% class(.) ~ "Value is not a linear model.")
safe_lmsummary <- function_(model ~ lm, {
  summary(model)
})

safe_lmsummary(lm(Sepal.Length ~ ., iris))
```

The type `some` allows any value and can be used to allow unsafe inputs.
(TODO: make `some` default when defaults are provided, already default when
no default is provided.)
 
# Special features include

* Customizing error behavior
* Easily combining several contracts
* Customizing error description
* Customizing individual conditions' error messages

and more. For more information, see the package vignette.
