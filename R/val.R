#' Get Valorant Content
#'
#' @param locale Locale. Such as `"en-US"`, `"ja-JP"`, etc.
#' @param regional `r roxy_regional()`
#' @param clean `r roxy_clean()`
#'
#' @return a list with Valorant content data
#' @export
#' @examplesIf has_riot_key()
#' val_get_content()
#'
val_get_content <- function(locale = NULL, regional = 'na', clean = TRUE) {
  req <- httr2::request(base_url = api_url(regional)) |>
    httr2::req_url_path_append(
      'val',
      'content',
      'v1',
      'contents'
    ) |>
    httr2::req_url_query(locale = locale) |>
    req_auth()

  resp <- safe_perform(req)
  if (is.null(resp)) return(NULL)

  if (isTRUE(clean)) {
    list_to_row(resp)
  } else {
    resp
  }
}

#' Get Valorant Ranked Leaderboard by Act
#'
#' @param act_id Act ID.
#' @param size Number of entries to return.
#' @param start_index Start index for pagination.
#' @param regional `r roxy_regional()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with ranked leaderboard entries or a list if
#'   `clean = FALSE`
#' @export
#' @examplesIf has_riot_key()
#' val_get_leaderboard('3ea2b318-423b-cf86-25da-7cbb0eefbe2d')
#'
val_get_leaderboard <- function(
  act_id,
  size = NULL,
  start_index = NULL,
  regional = 'na',
  clean = TRUE
) {
  req <- httr2::request(base_url = api_url(regional)) |>
    httr2::req_url_path_append(
      'val',
      'ranked',
      'v1',
      'leaderboards',
      'by-act',
      act_id
    ) |>
    httr2::req_url_query(
      size = size,
      startIndex = start_index
    ) |>
    req_auth()

  resp <- safe_perform(req)
  if (is.null(resp)) return(NULL)

  if (isTRUE(clean)) {
    resp |>
      purrr::pluck('players') |>
      lapply(list_to_row) |>
      dplyr::bind_rows() |>
      add_singletons(resp)
  } else {
    resp
  }
}