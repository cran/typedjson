## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, comment = "#>",
  eval = requireNamespace("R6", quietly = TRUE)
)

## -----------------------------------------------------------------------------
library(typedjson)

Store <- R6::R6Class("Store",
  public = list(
    root = NULL,
    format = "rds",

    initialize = function(root, key, format = "rds") {
      self$root <- root
      self$format <- format
      private$key <- key
      private$index <- private$scan()
      dir.create(root, showWarnings = FALSE, recursive = TRUE)
    },

    put = function(name, value) {
      saveRDS(value, private$file(name))
      private$index <- private$scan()
      private$writes <- private$writes + 1L
      invisible(self)
    },

    get = function(name) readRDS(private$file(name)),

    names = function() private$index
  ),

  private = list(
    key = NULL,
    index = character(),
    writes = 0L,

    file = function(name) {
      file.path(self$root, paste0(name, ".", self$format))
    },

    scan = function() {
      sub("\\..*$", "", list.files(self$root))
    }
  )
)

root <- file.path(tempdir(), "store")
store <- Store$new(root, key = "s3cret")

store$put("a", 1:3)$put("b", letters)
store$names()

## ----error = TRUE-------------------------------------------------------------
try({
json_write_str(store)
})

## -----------------------------------------------------------------------------
json_state.Store <- function(x) {
  list(root = x$root, format = x$format)
}

## -----------------------------------------------------------------------------
json_revive.Store <- function(class, state) {
  Store$new(state$root, key = Sys.getenv("STORE_KEY"), format = state$format)
}

## -----------------------------------------------------------------------------
doc <- json_write_str(store, pretty = TRUE)
cat(doc)

## -----------------------------------------------------------------------------
back <- json_read_str(doc)

back$names()
back$get("b")

## -----------------------------------------------------------------------------
Point <- R6::R6Class("Point",
  public = list(
    x = 0, y = 0,
    initialize = function(x = 0, y = 0) {
      self$x <- x
      self$y <- y
    }
  )
)

json_state.Point <- function(x) r6_state(x)
json_revive.Point <- function(class, state) r6_restore(class, state)

cat(json_write_str(Point$new(3, 4), pretty = TRUE))

## -----------------------------------------------------------------------------
json_read_str(json_write_str(Point$new(3, 4)))$y

## -----------------------------------------------------------------------------
doc <- json_write_str(Point$new(3, 4))

Point <- R6::R6Class("Point",
  public = list(x = 0, initialize = function(x = 0) self$x <- x),
  lock_objects = TRUE
)

json_read_str(doc)$x

## -----------------------------------------------------------------------------
json_write_str(Point)

identical(json_read_str(json_write_str(Point)), Point)

## ----error = TRUE-------------------------------------------------------------
try({
json_write_str((function() R6::R6Class("Local", public = list(v = 1)))())
})

## -----------------------------------------------------------------------------
Local <- (function() R6::R6Class("Local", public = list(v = 1)))()

back <- json_read_str(json_write_str(Local, self_contained = TRUE))

back$new()$v

## -----------------------------------------------------------------------------
Account <- R6::R6Class("Account",
  public = list(
    deposit = function(x) {
      private$balance <- private$balance + x
      invisible(self)
    },
    total = function() private$balance
  ),
  private = list(balance = 0)
)

account <- Account$new()$deposit(10)

back <- json_read_str(json_write_str(account, self_contained = TRUE))

back$deposit(5)$total()

## -----------------------------------------------------------------------------
grepl("s3cret", json_write_str(store, self_contained = TRUE), fixed = TRUE)

## ----error = TRUE-------------------------------------------------------------
try({
Cache <- setRefClass("Cache",
  fields = list(root = "character", key = "character")
)

json_write_str(Cache$new(root = tempdir(), key = "s3cret"))
})

## -----------------------------------------------------------------------------
vault <- new.env()
vault$root <- tempdir()
vault$key <- "s3cret"
class(vault) <- c("MyVault", "environment")

json_write_str(vault)

