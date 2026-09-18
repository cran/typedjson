## ----include = FALSE----------------------------------------------------------
library(typedjson)

knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

show_documents <- function(..., typed = TRUE) {

  exprs <- as.list(substitute(list(...)))[-1L]
  values <- lapply(exprs, eval, envir = parent.frame())

  knitr::kable(
    data.frame(
      `R value` = sprintf("`%s`", vapply(exprs, deparse1, character(1L))),
      Document = sprintf(
        "`%s`",
        vapply(values, json_write_str, character(1L), typed = typed)
      ),
      check.names = FALSE
    ),
    row.names = FALSE
  )
}

## -----------------------------------------------------------------------------
x <- list(id = "block_1", weights = c(a = 0.5, b = NA, c = Inf), n = 3L)

path <- tempfile(fileext = ".json")
json_write(x, path)

identical(json_read(path), x)

## -----------------------------------------------------------------------------
doc <- json_write_str(x)

identical(json_write_str(json_read_str(doc)), doc)

## ----echo = FALSE-------------------------------------------------------------
show_documents(c(1.0, 2.5), c(1L, 2L), c("a", "b"), c(TRUE, FALSE))

## ----echo = FALSE-------------------------------------------------------------
show_documents(list(1L, 2L), list(name = "config", retries = 3L))

## ----echo = FALSE-------------------------------------------------------------
show_documents(TRUE, 1L, list(1, 2))

## ----echo = FALSE-------------------------------------------------------------
show_documents(list(required = I("x"), additionalProperties = FALSE),
               typed = FALSE)

## ----echo = FALSE-------------------------------------------------------------
show_documents(list(a = I("x"), b = "y"), c(a = 1, b = 2), matrix(1:4, 2),
               factor("b", levels = c("a", "b")), typed = FALSE)

## ----error = TRUE-------------------------------------------------------------
try({
json_write_str(list(a = 1, at = quote(f(x))), typed = FALSE)
json_write_str(list(ratio = Inf), typed = FALSE)
})

## ----error = TRUE-------------------------------------------------------------
try({
con <- file(file.path(tempdir(), "events.log"))

json_write_str(con)
json_write_str(con, typed = FALSE)
})

## ----include = FALSE----------------------------------------------------------
close(con)

## -----------------------------------------------------------------------------
json_state.account <- function(x) list(user = x$user)
acct <- structure(list(user = "nb", token = "s3cret"), class = "account")

cat(json_write_str(acct))
cat(json_write_str(acct, typed = FALSE))

## ----echo = FALSE-------------------------------------------------------------
show_documents(NA_real_, c(1, Inf), "~foo", "Inf")

## ----error = TRUE-------------------------------------------------------------
try({
json_read_str('{"home": "~/data"}')
json_read_str('"~zBogus"')
})

## ----echo = FALSE-------------------------------------------------------------
show_documents(setNames(list(1L), NA), setNames(list(1L), "~zInf"))

## ----error = TRUE-------------------------------------------------------------
try({
json_read_str('{"~span": 3}')
})

## ----echo = FALSE-------------------------------------------------------------
show_documents(as.Date("2026-01-01"), c(a = 1, b = 2), character())

## ----echo = FALSE-------------------------------------------------------------
show_documents(c(a = 1), c(a = 1)[0])

## ----echo = FALSE-------------------------------------------------------------
show_documents(1 + 2i, c(1 + 2i, 3 - 4i), complex(real = NA, imaginary = Inf))

## ----echo = FALSE-------------------------------------------------------------
show_documents(as.raw(c(0, 15, 255)))

## -----------------------------------------------------------------------------
x <- as.call(list(quote(f), c(1.1, 2.2)))
identical(str2lang(paste(deparse(x), collapse = "\n")), x)

## ----echo = FALSE-------------------------------------------------------------
show_documents(
  quote(mpg ~ wt), quote(f(0.1, a = x)), formals(function(x, y = 2) NULL)
)

## ----error = TRUE-------------------------------------------------------------
try({
json_write_str(y ~ x)
})

## ----error = TRUE, eval = requireNamespace("R6", quietly = TRUE)--------------
try({
Counter <- R6::R6Class("Counter", public = list(n = 0))

json_write_str(list(a = 1, b = Counter$new()))
})

## ----error = TRUE, eval = requireNamespace("R6", quietly = TRUE)--------------
try({
Bound <- R6::R6Class("Bound", portable = FALSE, public = list(n = 1))

json_state.Bound <- function(x) r6_state(x)

json_write_str(list(a = 1, b = Bound$new()))
})

## ----echo = FALSE-------------------------------------------------------------
show_documents(globalenv(), baseenv(), as.environment("package:stats"))

## -----------------------------------------------------------------------------
counter <- new.env(parent = asNamespace("stats"))
counter$n <- 1L

json_write_str(counter)

## -----------------------------------------------------------------------------
lockEnvironment(counter, bindings = TRUE)

json_write_str(counter)

## -----------------------------------------------------------------------------
identical(function(x) x + 1, function(x) x + 1)

## -----------------------------------------------------------------------------
show_documents(mean)

## -----------------------------------------------------------------------------
counter <- new.env(parent = globalenv())
counter$i <- 1L

json_write_str(local(function() i + 1, counter))

## -----------------------------------------------------------------------------
show_documents(sum, `if`)

## -----------------------------------------------------------------------------
formals(identical)$ignore.srcref

## ----error = TRUE-------------------------------------------------------------
try({
adder <- function(n) {
  force(n)
  function(x) x + n
}

json_write_str(adder(3))
})

## -----------------------------------------------------------------------------
mk <- local(
  function() {
    i <- 0
    list(get = function() i, set = function(v) i <<- v)
  },
  globalenv()
)

pair <- json_read_str(json_write_str(mk()))
pair$set(2)
pair$get()

## -----------------------------------------------------------------------------
frame <- local(
  function() {
    inner <- new.env()
    inner
  },
  globalenv()
)

back <- json_read_str(json_write_str(frame()))

identical(parent.env(back)$inner, back)

## -----------------------------------------------------------------------------
json_read_str("[1, 9007199254740993]")

## -----------------------------------------------------------------------------
adder_class <- function(n) {
  structure(list(n = n, f = function(x) x + n), class = "adder_class")
}

## ----error = TRUE-------------------------------------------------------------
try({
json_write_str(adder_class(3))
})

## -----------------------------------------------------------------------------
json_state.adder_class <- function(x) list(n = x$n)

json_revive.adder_class <- function(class, state) adder_class(state$n)

doc <- json_write_str(adder_class(3))
doc

json_read_str(doc)$f(4)

