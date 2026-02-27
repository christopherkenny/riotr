#' Get TFT Match by ID
#'
#' @param match_id `r roxy_match_id()`
#' @param regional `r roxy_regional()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with match data or a list if `clean = FALSE`
#' @export
#' @examplesIf has_riot_key()
#' tft_get_match('NA1_5501463494')
#'
tft_get_match <- function(match_id, regional = 'americas', clean = TRUE) {
  req <- httr2::request(base_url = api_url(regional)) |>
    httr2::req_url_path_append(
      'tft',
      'match',
      'v1',
      'matches',
      match_id
    ) |>
    req_auth()

  resp <- safe_perform(req)
  if (is.null(resp)) return(NULL)

  if (isTRUE(clean)) {
    participants <- resp |>
      purrr::pluck('info', 'participants') |>
      lapply(list_to_row) |>
      dplyr::bind_rows()
    add_singletons(participants, resp[['info']])
  } else {
    resp
  }
}

#' Get TFT Match IDs by PUUID
#'
#' @param puuid `r roxy_puuid()`
#' @param start Start index. Defaults to `0`.
#' @param count Number of match IDs to return. Defaults to `20`.
#' @param start_time Epoch timestamp in seconds to filter matches
#'   after.
#' @param end_time Epoch timestamp in seconds to filter matches
#'   before.
#' @param regional `r roxy_regional()`
#'
#' @return a character vector of match IDs
#' @export
#' @examplesIf has_riot_key()
#' puuid <- 'GmNPNjUbzbWN8YIA79V4p308eFpf21xPO2Lw8o2xJTJMPW83W54t4GfyfpwOn7-hBz5c3MHpg-Dc3w'
#' tft_get_match_ids(puuid)
#'
tft_get_match_ids <- function(
  puuid,
  start = NULL,
  count = NULL,
  start_time = NULL,
  end_time = NULL,
  regional = 'americas'
) {
  req <- httr2::request(base_url = api_url(regional)) |>
    httr2::req_url_path_append(
      'tft',
      'match',
      'v1',
      'matches',
      'by-puuid',
      puuid,
      'ids'
    ) |>
    httr2::req_url_query(
      start = start,
      count = count,
      startTime = start_time,
      endTime = end_time
    ) |>
    req_auth()

  resp <- safe_perform(req)
  if (is.null(resp)) return(NULL)

  as.character(unlist(resp))
}