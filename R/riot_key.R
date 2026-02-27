#' Check or Get RIOT Key
#'
#' @return logical if `has`, key if `get`
#' @export
#'
#' @name key
#'
#' @examples
#' has_riot_key()
has_riot_key <- function() {
  Sys.getenv('RIOT_KEY') != ''
}

#' @rdname key
#' @export
get_riot_key <- function() {
  key <- Sys.getenv('RIOT_KEY')

  if (key == '') {
    cli::cli_abort('Must set a key as {.val RIOT_KEY}.')
  }
  key
}

#' Add Entry to Renviron
#'
#' Adds RIOT API key to .Renviron.
#'
#' @param key Character. API key to add to add.
#' @param overwrite Defaults to FALSE. Boolean. Should existing `RIOT_KEY` in Renviron be overwritten?
#' @param install Defaults to FALSE. Boolean. Should this be added '~/.Renviron' file?
#' @param r_env Path to install to if `install` is `TRUE`.
#'
#' @return key, invisibly
#' @export
#'
#' @examples
#' example_env <- tempfile(fileext = '.Renviron')
#' set_riot_key('1234', r_env = example_env)
#' # r_env should likely be: file.path(Sys.getenv('HOME'), '.Renviron')
set_riot_key <- function(
  key,
  overwrite = FALSE,
  install = FALSE,
  r_env = NULL
) {
  if (missing(key)) {
    cli::cli_abort('Input {.arg key} cannot be missing.')
  }
  name <- 'RIOT_KEY'

  key <- list(key)
  names(key) <- name

  if (install) {
    if (is.null(r_env)) {
      cli::cli_abort(c(
        'No path set.',
        i = 'Rerun with {.arg r_env} set, possibly to {.file {r_env}}'
      ))
    }

    if (!file.exists(r_env)) {
      file.create(r_env)
    }

    lines <- readLines(r_env)
    newline <- paste0(name, "='", key, "'")

    exists <- grepl(x = lines, paste0(name, '='))

    if (any(exists)) {
      if (sum(exists) > 1) {
        cli::cli_abort(
          'Multiple entries in .Renviron found.\nEdit manually with {.fn usethis::edit_r_environ}.'
        )
      }

      if (overwrite) {
        lines[exists] <- newline
        writeLines(lines, r_env)
        do.call(Sys.setenv, key)
      } else {
        cli::cli_inform(
          '{.arg RIOT_KEY} already exists in .Renviron. \nEdit manually with {.fn usethis::edit_r_environ} or set {.code overwrite = TRUE}.'
        )
      }
    } else {
      lines[length(lines) + 1] <- newline
      writeLines(lines, r_env)
      do.call(Sys.setenv, key)
    }
  } else {
    do.call(Sys.setenv, key)
  }

  invisible(key)
}
