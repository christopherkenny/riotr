#' Get All Challenge Percentiles
#'
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a list of challenge percentiles
#' @export
#'
#' @examplesIf has_riot_key()
#' lol_get_all_challenge_percentiles()
lol_get_all_challenge_percentiles<- function(region = 'na1', clean = TRUE) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'challenges',
      'v1',
      'challenges',
      'percentiles'
    ) |>
    req_auth()

  resp <- safe_perform(req)
  if (is.null(resp)) return(NULL)

  resp
}

#' Get Challenge Leaderboards
#'
#' @param challenge_id Challenge ID.
#' @param level Level. Such as `"MASTER"`, `"GRANDMASTER"`, or
#'   `"CHALLENGER"`.
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with leaderboard entries or a list if
#'   `clean = FALSE`
#' @export
#'
#' @examplesIf has_riot_key()
#' lol_get_challenge_leaderboards(1, 'MASTER')
lol_get_challenge_leaderboards<- function(
  challenge_id,
  level,
  region = 'na1',
  clean = TRUE
) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'challenges',
      'v1',
      'challenges',
      challenge_id,
      'leaderboards',
      'by-level',
      level
    ) |>
    req_auth()

  resp <- safe_perform(req)
  if (is.null(resp)) return(NULL)

  if (isTRUE(clean)) {
    resp |>
      lapply(list_to_row) |>
      dplyr::bind_rows()
  } else {
    resp
  }
}

#' Get Challenge Percentiles by ID
#'
#' @param challenge_id Challenge ID.
#' @param region `r roxy_region()`
#'
#' @return a list of challenge percentiles
#' @export
#'
#' @examplesIf has_riot_key()
#' lol_get_challenge_percentiles(1)
lol_get_challenge_percentiles<- function(challenge_id, region = 'na1') {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'challenges',
      'v1',
      'challenges',
      challenge_id,
      'percentiles'
    ) |>
    req_auth()

  safe_perform(req)
}

#' Get Challenge Config by ID
#'
#' @param challenge_id Challenge ID.
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with challenge config or a list if
#'   `clean = FALSE`
#' @export
#'
#' @examplesIf has_riot_key()
#' lol_get_challenge_config(1)
lol_get_challenge_config <- function(
  challenge_id,
  region = 'na1',
  clean = TRUE
) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'challenges',
      'v1',
      'challenges',
      challenge_id,
      'config'
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

#' Get Challenge Player Data
#'
#' @param puuid `r roxy_puuid()`
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a list with player challenge data
#' @export
#'
#' @examplesIf has_riot_key()
#' puuid <- 'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA'
#' lol_get_challenge_player_data(puuid)
lol_get_challenge_player_data <- function(puuid, region = 'na1', clean = TRUE) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'challenges',
      'v1',
      'player-data',
      puuid
    ) |>
    req_auth()

  resp <- safe_perform(req)
  if (is.null(resp)) return(NULL)

  if (isTRUE(clean)) {
    challenges <- resp |>
      purrr::pluck('challenges') |>
      lapply(list_to_row) |>
      dplyr::bind_rows()
    add_singletons(challenges, resp)
  } else {
    resp
  }
}

#' Get All Challenge Configs
#'
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with all challenge configs or a list if
#'   `clean = FALSE`
#' @export
#'
#' @examplesIf has_riot_key()
#' lol_get_all_challenge_configs()
lol_get_all_challenge_configs <- function(region = 'na1', clean = TRUE) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'challenges',
      'v1',
      'challenges',
      'config'
    ) |>
    req_auth()

  resp <- safe_perform(req)
  if (is.null(resp)) return(NULL)

  if (isTRUE(clean)) {
    resp |>
      lapply(list_to_row) |>
      dplyr::bind_rows()
  } else {
    resp
  }
}
