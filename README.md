The `ensurer` R package
=======================

### Ensure that your values are as expected at runtime.

[![Build Status](https://travis-ci.org/smbache/ensurer.png?branch=master)](https://travis-ci.org/smbache/ensurer)

`ensurer` is a small utility package for `R` which provides a simple 
and light-weight mechanism for ensuring certain aspects of values at runtime.

`R` does not provide any mechanism for type-safety and since it is not
a compiled language, the risk of having unexpected results is there at 
runtime. `R` functions often accept different types for the same input and/or 
have different return types for different sitations.
 
As an example, a query to a database or the scraping of a website might 
not return valid data, where "validity" can refer to a number of conditions.
It might be a positive or certain number of records; that all cases are complete;
that some column is weekly increasing; or simply that the result is a 
`data.frame`.

If one does not deal with these ambiguities and risks appropriately,
some resulting errors may be hard to track down. It is desirable to 
get an error as soon as a value does not have the correct type or 
does not satisfy certain criteria.
 
"Ensuring values" is here meant as a "contract", or a set of conditions,
such that if a value does not comply an error is raised instantly. 
An ensuring contract (a function) is created with `ensuring` (ideal for 
multiple use or readability with complex contracts). 
 
It is also possible to ensure properties on the fly using `ensure_that`
(ideal for simple, one-time contracts).

Using the `magrittr` pipe `%>%` greatly improves semantics of the
functionality provided by this package, but it is not necessary.

This package is not meant as a substitute for unit testing, and great
packages for this already exist, e.g. `testthat` by Hadley Wickham.
The `ensurer` package is ideal for scripts or programs where runtime
conditions may break the functionality, and where errors should be
raised as soon and clear as possible.

# Installation

The easiest way to install `ensurer` is using the `devtools` package:

    devtools::install_github("smbache/ensurer")


# Basic Examples

The following example shows how to define a contract ensuring that its input
is square, and how to use it.

    library(magrittr) # for the pipe -> cleaner semantics
    library(ensurer)  # for ensuring values.

    # read `<-` as "is":
    # To reference the value being evaluated, use the `.` placeholder.
    ensure_square <- ensuring(NCOL(.) == NROW(.))

	# try it out:
	diag(5) %>%
      ensure_square  # passes, so returns the diagonal matrix

    # This won't work, and an error is raised.
    matrix(1:20, 4, 5) %>% 
      ensure_square

Several conditions can be specified:

    ensure_square <- ensuring(is.matrix(.), 
                              NCOL(.) == NROW(.))

Sometimes it can be handy to use other objects in the conditions, either computing them 
on the fly, or just abbreviating the names. In the example below, data is ensured to 
mimic the `iris` data in certain ways:

    # named arguments in the ensuring call become values that are available in the
    # conditions; here `i` is such an argument. 
    ensure_as_iris <- 
      ensuring(ncol(.) == ncol(i) && 
               all(sapply(., class) == sapply(i, class)), 
               i = iris[numeric(0), ])
	
	# try it.
    head(iris) %>%
      ensure_as_iris

There is a special named argument `fail_with` which can be used to overrule the default
behavior when conditions are not met; see below.

You can also add several ensuring contracts to a value:

    new_data <-
      iris %>% 
      head(65) %>%
      ensure_that(is.data.frame(.)) %>%
      ensure_as_iris

In the above example it is also shown how to define the contract on the fly using `ensure_that`.
This function will create the contract using `ensuring` and apply it instantly.

Whenever a contract is violated the error will specify which conditions were not satisfied:

    1:100 %>% 
      ensure_that(all(cummax(.) == .), # increasing
                  all(. < 50),         # below 50
                  all(. %% 2 == 0))    # even numbers

    Error: The following condition(s) failed:
	all(. < 50)
	all(.%%2 == 0) 

# Tweeking behavior when conditions fail

In some cases it may be too drastic to fail when conditions are violated. It can also 
be the case that some action is desired before an error is raised.
For these purposes it is possible to add the named parameter `fail_with` to `ensure_that` and
`ensuring`; it can be either a function or a value. When a function is specified it must 
accept a single argument which is of type `simpleError`.

    # Using a function to overrule default behavior:
    emailer <- function(e) { <email the error information etc here>; stop(e) }
	
	new_value <-
      risky_action %>%
      ensure(is_valid(.),  
             fail_with = emailer)

    # Maybe accept NA as value; but not some other garbage:
    new_value <-
      risky_action %>%
      ensure(is_valid(.),  
             fail_with = NA)

# Slightly more advanced contracts
Sometimes it can be useful to extend or wrap the `ensurer` functions. The following 
example shows how to make a templated contract ensuring that an object is of the correct
"data class" (`data.frame`, `data.table` etc) and has the correct column 
names and column classes, based on a template (an instance with these properties with 
0 or more rows).

    ensure_data_validity <- function(x, data_template)
	{
		# note the abbreviation "tpl" which is not necessary, but 
        # compresses the conditions slightly.
		ensure_that(x,
			identical(class(.), class(tpl)), 
			identical(names(.), names(tpl)),
			identical(sapply(., class), sapply(tpl, class)),
			tpl = data_template)
	}


Now, suppose we have a proto-type/template of a `data.frame` which we want to use
as the definition of how the result of some statement should be. Here we just use 
the ever-so-popular `iris` data (but you could think of a SQL query, or web scrape, 
etc):

    # The template:
    iris_template <- iris[0L, ]

	# The (not so) risky data extracting call, with the ensuring contract:
	new_data <-
      iris %>%
      head(10) %>%
      ensure_data_validity(iris_template)