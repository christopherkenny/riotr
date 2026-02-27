# devtools ----
# devtools not intended for use in production, not tested
lrj <- function() {
  # nocov start
  httr2::last_response() |>
    httr2::resp_body_json()
} # nocov end

.gl <- function(x) {
  # nocov start
  dplyr::glimpse(x)
} # nocov end

# query tools ----
req_auth <- function(req, key = get_riot_key()) {
  req |>
    httr2::req_headers(
      'X-Riot-Token' = key,
      .redact = 'X-Riot-Token'
    ) |>
    httr2::req_throttle(rate = 20 / 1) |>
    httr2::req_retry(max_tries = 3, backoff = ~2)
}

safe_perform <- function(req) {
  req <- req |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)

  if (status >= 400L) {
    body <- tryCatch(
      httr2::resp_body_json(resp),
      error = function(e) list(status = list(message = 'Unknown error'))
    )
    msg <- body[['status']][['message']] %||% paste('HTTP', status)
    cli::cli_inform('API returned {status}: {msg}')
    return(NULL)
  }

  httr2::resp_body_json(resp)
}

# tools for tidying ----
list_to_row <- function(x) {
  lapply(x, function(item) {
    if (is.null(item)) {
      NA
    } else if (is.list(item) && !is.data.frame(item)) {
      if (length(item) == 0) {
        list(tibble::tibble())
      } else if (
        all(sapply(item, is.list)) &&
          all(sapply(item, function(si) !is.null(names(si))))
      ) {
        lapply(item, function(subitem) {
          lapply(subitem, function(el) {
            if (is.null(el)) {
              list(NA)
            } else if (is.list(el) && length(el) > 0) {
              list(el)
            } else if (is.list(el) && length(el) == 0) {
              list(list())
            } else {
              el
            }
          }) |>
            tibble::as_tibble()
        }) |>
          dplyr::bind_rows() |>
          list()
      } else if (!is.null(names(item))) {
        lapply(item, function(el) {
          if (is.null(el)) {
            NA
          } else if (is.list(el)) {
            list(el)
          } else {
            el
          }
        }) |>
          tibble::as_tibble() |>
          list()
      } else {
        list(item)
      }
    } else {
      item
    }
  }) |>
    tibble::as_tibble()
}

add_singletons <- function(tb, l) {
  r1 <- purrr::keep(l, function(x) purrr::pluck_depth(x) <= 1)
  attr(tb, 'response_info') <- r1
  tb
}
