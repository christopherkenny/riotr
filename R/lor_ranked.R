#' Get LoR Ranked Leaderboards
#'
#' @param regional `r roxy_regional()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with ranked leaderboard entries or a list if
#'   `clean = FALSE`
#' @export
#' @examplesIf has_riot_key()
#' lor_get_leaderboards()
#'
lor_get_leaderboards <- function(regional = 'americas', clean = TRUE) {
  req <- httr2::request(base_url = api_url(regional)) |>
    httr2::req_url_path_append(
      'lor',
      'ranked',
      'v1',
      'leaderboards'
    ) |>
    req_auth()

  resp <- safe_perform(req)
  if (is.null(resp)) return(NULL)

  if (isTRUE(clean)) {
    resp |>
      purrr::pluck('players') |>
      lapply(list_to_row) |>
      dplyr::bind_rows()
  } else {
    resp
  }
}