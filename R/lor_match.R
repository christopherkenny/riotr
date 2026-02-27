#' Get LoR Match by ID
#'
#' @param match_id `r roxy_match_id()`
#' @param regional `r roxy_regional()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with match data, or `NULL` if not found.
#'   Returns a list if `clean = FALSE`.
#' @export
#' @examplesIf has_riot_key()
#' lor_get_match('PLACEHOLDER_LOR_MATCH_ID')
#'
lor_get_match <- function(match_id, regional = 'americas', clean = TRUE) {
  req <- httr2::request(base_url = api_url(regional)) |>
    httr2::req_url_path_append(
      'lor',
      'match',
      'v1',
      'matches',
      match_id
    ) |>
    req_auth()

  resp <- safe_perform(req)
  if (is.null(resp)) return(NULL)

  if (isTRUE(clean)) {
    players <- resp |>
      purrr::pluck('info', 'players') |>
      lapply(list_to_row) |>
      dplyr::bind_rows()
    add_singletons(players, resp[['info']])
  } else {
    resp
  }
}

#' Get LoR Match IDs by PUUID
#'
#' @param puuid `r roxy_puuid()`
#' @param regional `r roxy_regional()`
#'
#' @return a character vector of match IDs
#' @export
#' @examplesIf has_riot_key()
#' puuid <- 'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA'
#' lor_get_match_ids(puuid)
#'
lor_get_match_ids <- function(puuid, regional = 'americas') {
  req <- httr2::request(base_url = api_url(regional)) |>
    httr2::req_url_path_append(
      'lor',
      'match',
      'v1',
      'matches',
      'by-puuid',
      puuid,
      'ids'
    ) |>
    req_auth()

  resp <- safe_perform(req)
  if (is.null(resp)) return(NULL)

  as.character(unlist(resp))
}