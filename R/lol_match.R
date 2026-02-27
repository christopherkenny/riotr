#' Get Match by ID
#'
#' @param match_id `r roxy_match_id()`
#' @param regional `r roxy_regional()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with match data or a list if `clean = FALSE`
#' @export
#'
#' @examplesIf has_riot_key()
#' lol_get_match('NA1_5494453720')
lol_get_match <- function(match_id, regional = 'americas', clean = TRUE) {
  req <- httr2::request(base_url = api_url(regional)) |>
    httr2::req_url_path_append(
      'lol',
      'match',
      'v5',
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

#' Get Match IDs by PUUID
#'
#' @param puuid `r roxy_puuid()`
#' @param start Start index. Defaults to `0`.
#' @param count Number of match IDs to return. Defaults to `20`.
#' @param queue Queue ID to filter by.
#' @param type Type of match to filter by. Such as `"ranked"`,
#'   `"normal"`, `"tourney"`, or `"tutorial"`.
#' @param start_time Epoch timestamp in seconds to filter matches
#'   after.
#' @param end_time Epoch timestamp in seconds to filter matches
#'   before.
#' @param regional `r roxy_regional()`
#'
#' @return a character vector of match IDs
#' @export
#'
#' @examplesIf has_riot_key()
#' puuid <- 'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA'
#' lol_get_match_ids(puuid)
lol_get_match_ids <- function(
  puuid,
  start = NULL,
  count = NULL,
  queue = NULL,
  type = NULL,
  start_time = NULL,
  end_time = NULL,
  regional = 'americas'
) {
  req <- httr2::request(base_url = api_url(regional)) |>
    httr2::req_url_path_append(
      'lol',
      'match',
      'v5',
      'matches',
      'by-puuid',
      puuid,
      'ids'
    ) |>
    httr2::req_url_query(
      start = start,
      count = count,
      queue = queue,
      type = type,
      startTime = start_time,
      endTime = end_time
    ) |>
    req_auth()

  resp <- safe_perform(req)
  if (is.null(resp)) return(NULL)

  as.character(unlist(resp))
}

#' Get Match Timeline
#'
#' @param match_id `r roxy_match_id()`
#' @param regional `r roxy_regional()`
#' @param clean `r roxy_clean()`
#'
#' @return a list with timeline data (timelines are complex nested
#'   structures)
#' @export
#' @examplesIf has_riot_key()
#' lol_get_match_timeline('NA1_5494453720')
#'
lol_get_match_timeline <- function(
  match_id,
  regional = 'americas',
  clean = TRUE
) {
  req <- httr2::request(base_url = api_url(regional)) |>
    httr2::req_url_path_append(
      'lol',
      'match',
      'v5',
      'matches',
      match_id,
      'timeline'
    ) |>
    req_auth()

  resp <- safe_perform(req)
  if (is.null(resp)) return(NULL)

  if (isTRUE(clean)) {
    resp |>
      purrr::pluck('info', 'frames') |>
      lapply(list_to_row) |>
      dplyr::bind_rows()
  } else {
    resp
  }
}

#' Get Match Replays by PUUID
#'
#' @param puuid `r roxy_puuid()`
#' @param regional `r roxy_regional()`
#'
#' @return a character vector of replay URLs
#' @export
#' @examplesIf has_riot_key()
#' puuid <- 'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA'
#' lol_get_match_replays(puuid)
#'
lol_get_match_replays <- function(puuid, regional = 'americas') {
  req <- httr2::request(base_url = api_url(regional)) |>
    httr2::req_url_path_append(
      'lol',
      'match',
      'v5',
      'matches',
      'by-puuid',
      puuid,
      'replays'
    ) |>
    req_auth()

  resp <- safe_perform(req)
  if (is.null(resp)) return(NULL)

  as.character(unlist(resp))
}