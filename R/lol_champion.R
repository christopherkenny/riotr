#' Get Champion Rotation Information
#'
#' Returns champion rotations, including free-to-play and low-level
#' free-to-play rotations.
#'
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with champion rotation data or a list if
#'   `clean = FALSE`
#' @export
#'
#' @examplesIf has_riot_key()
#' lol_get_champion_rotations()
lol_get_champion_rotations <- function(region = 'na1', clean = TRUE) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'platform',
      'v3',
      'champion-rotations'
    ) |>
    req_auth()

  resp <- safe_perform(req)
  if (is.null(resp)) return(NULL)

  if (isTRUE(clean)) {
    list_to_row(resp)
  } else {
    resp
  }
}
