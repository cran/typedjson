## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = requireNamespace("jsonlite", quietly = TRUE)
)

## -----------------------------------------------------------------------------
library(typedjson)

payload <- local({

  set.seed(1)

  block <- function(i) {
    list(
      id = paste0("block_", i),
      constructor = "new_filter_block",
      payload = list(
        columns = sample(letters, 8L),
        weights = stats::runif(8L),
        counts = sample.int(1000L, 8L),
        keep_na = c(TRUE, FALSE, NA),
        note = paste0("row ", i, " of the board")
      ),
      position = c(x = stats::runif(1L) * 1000, y = stats::runif(1L) * 1000)
    )
  }

  list(
    blocks = lapply(seq_len(200L), block),
    options = list(
      name = "bench",
      created = as.POSIXct("2026-01-01", tz = "UTC")
    )
  )
})

format(utils::object.size(payload), units = "KB")

## -----------------------------------------------------------------------------
codecs <- list(
  "typedjson" = list(
    write = json_write_str,
    read = json_read_str
  ),
  "toJSON()" = list(
    write = jsonlite::toJSON,
    read = function(doc) jsonlite::fromJSON(doc, simplifyVector = FALSE)
  ),
  "toJSON(digits = 17)" = list(
    write = function(x) jsonlite::toJSON(x, digits = 17),
    read = function(doc) jsonlite::fromJSON(doc, simplifyVector = FALSE)
  ),
  "serializeJSON()" = list(
    write = jsonlite::serializeJSON,
    read = jsonlite::unserializeJSON
  ),
  "serializeJSON(digits = 17)" = list(
    write = function(x) jsonlite::serializeJSON(x, digits = 17),
    read = jsonlite::unserializeJSON
  )
)

docs <- lapply(codecs, function(codec) codec$write(payload))

## -----------------------------------------------------------------------------
exact <- vapply(
  names(codecs),
  function(nme) identical(codecs[[nme]]$read(docs[[nme]]), payload),
  logical(1L)
)

exact

## -----------------------------------------------------------------------------
x <- list(n = 3L, weight = pi, flags = c(a = TRUE, b = NA), empty = character())

str(jsonlite::fromJSON(jsonlite::toJSON(x), simplifyVector = FALSE))

## -----------------------------------------------------------------------------
lossy <- jsonlite::unserializeJSON(jsonlite::serializeJSON(x))

print(c(pi, lossy$weight), digits = 17)

## -----------------------------------------------------------------------------
sizes <- vapply(docs, nchar, integer(1L))

knitr::kable(
  data.frame(
    bytes = sizes,
    relative = round(sizes / sizes[["typedjson"]], 2),
    exact = exact
  )
)

## -----------------------------------------------------------------------------
cat(json_write_str(pi), "\n")
cat(jsonlite::serializeJSON(pi, digits = 17), "\n")

## -----------------------------------------------------------------------------
cat(json_write_str(c(x = 1.5, y = 2.5)), "\n")

## -----------------------------------------------------------------------------
unnamed <- payload
unnamed$blocks <- lapply(payload$blocks, function(block) {
  block$position <- unname(block$position)
  block
})

c(
  typedjson = nchar(json_write_str(unnamed)),
  `toJSON(digits = 17)` = nchar(jsonlite::toJSON(unnamed, digits = 17))
)

## -----------------------------------------------------------------------------
time <- function(f, x, budget = 0.2, passes = 3L) {

  gc()

  once <- system.time(f(x))[["elapsed"]]
  reps <- max(1L, ceiling(budget / max(once, 1e-4)))

  pass <- function() {
    system.time(for (i in seq_len(reps)) f(x))[["elapsed"]] / reps
  }

  min(replicate(passes, pass()))
}

writes <- vapply(
  codecs,
  function(codec) time(codec$write, payload),
  numeric(1L)
)
reads <- vapply(
  names(codecs),
  function(nme) time(codecs[[nme]]$read, docs[[nme]]),
  numeric(1L)
)

knitr::kable(
  data.frame(
    write_ms = signif(writes * 1000, 3),
    read_ms = signif(reads * 1000, 3),
    exact = exact
  )
)

## -----------------------------------------------------------------------------
utils::packageVersion("jsonlite")

