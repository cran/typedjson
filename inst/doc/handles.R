## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(typedjson)

path <- file.path(tempdir(), "events.log")
writeLines(sprintf("event %d", 1:6), path)

con <- file(path, open = "r")
str(unclass(con))

## ----error = TRUE-------------------------------------------------------------
try({
json_write_str(con)
})

## ----include = FALSE----------------------------------------------------------
close(con)

## -----------------------------------------------------------------------------
chunk_reader <- function(path, size = 2L) {
  structure(
    list(path = path, size = size, con = file(path, open = "r")),
    class = "chunk_reader"
  )
}

read_next <- function(x) {
  readLines(x$con, n = x$size)
}

## -----------------------------------------------------------------------------
reader <- chunk_reader(path)

read_next(reader)
read_next(reader)

## ----error = TRUE-------------------------------------------------------------
try({
json_write_str(reader)
})

## -----------------------------------------------------------------------------
json_state.chunk_reader <- function(x) {
  list(path = x$path, size = x$size, offset = seek(x$con))
}

## -----------------------------------------------------------------------------
cat(json_write_str(reader, pretty = TRUE))

## -----------------------------------------------------------------------------
json_revive.chunk_reader <- function(class, state) {
  con <- file(state$path, open = "r")
  seek(con, state$offset)

  structure(
    list(path = state$path, size = state$size, con = con),
    class = "chunk_reader"
  )
}

## -----------------------------------------------------------------------------
doc <- json_write_str(reader)

revived <- json_read_str(doc)
read_next(revived)

## ----include = FALSE----------------------------------------------------------
close(reader$con)
close(revived$con)

## -----------------------------------------------------------------------------
rewound <- json_read_str(sub('"offset":[0-9.]+', '"offset":0.0', doc))
read_next(rewound)

## ----include = FALSE----------------------------------------------------------
close(rewound$con)

## ----error = TRUE-------------------------------------------------------------
try({
json_read_str(sub(basename(path), "no-such-file", doc, fixed = TRUE))
})

## ----error = TRUE-------------------------------------------------------------
try({
json_state.leaky <- function(x) list(con = x$con)

leaky <- structure(list(con = file(path)), class = "leaky")
json_write_str(leaky)
})

## ----include = FALSE----------------------------------------------------------
close(leaky$con)

## ----error = TRUE, eval = requireNamespace("R6", quietly = TRUE)--------------
try({
ChunkReader <- R6::R6Class("ChunkReader",
  public = list(
    path = NULL,
    size = NULL,
    con = NULL,
    initialize = function(path, size = 2L) {
      self$path <- path
      self$size <- size
      self$con <- file(path, open = "r")
    },
    read_next = function() {
      readLines(self$con, n = self$size)
    }
  )
)

reader <- ChunkReader$new(path)
json_write_str(reader)
})

## ----eval = requireNamespace("R6", quietly = TRUE)----------------------------
json_state.ChunkReader <- function(x) {
  list(path = x$path, size = x$size, offset = seek(x$con))
}

json_revive.ChunkReader <- function(class, state) {
  out <- ChunkReader$new(state$path, state$size)
  seek(out$con, state$offset)
  out
}

reader$read_next()

doc <- json_write_str(reader)
cat(doc)

revived <- json_read_str(doc)
revived$read_next()

## ----include = FALSE, eval = requireNamespace("R6", quietly = TRUE)-----------
close(reader$con)
close(revived$con)

