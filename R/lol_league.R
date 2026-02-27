#' Get Challenger League
#'
#' @param queue `r roxy_queue()`
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with league entries or a list if `clean = FALSE`
#' @export
#'
#' @examplesIf has_riot_key()
#' lol_get_challenger_league('RANKED_SOLO_5x5')
lol_get_challenger_league <- function(queue, region = 'na1', clean = TRUE) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'league',
      'v4',
      'challengerleagues',
      'by-queue',
      queue
    ) |>
    req_auth()

  resp <- safe_perform(req)
  if (is.null(resp)) return(NULL)

  if (isTRUE(clean)) {
    resp |>
      purrr::pluck('entries') |>
      lapply(list_to_row) |>
      dplyr::bind_rows() |>
      add_singletons(resp)
  } else {
    resp
  }
}

#' Get Master League
#'
#' @param queue `r roxy_queue()`
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with league entries or a list if `clean = FALSE`
#' @export
#'
#' @examplesIf has_riot_key()
#' lol_get_master_league('RANKED_SOLO_5x5')
lol_get_master_league <- function(queue, region = 'na1', clean = TRUE) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'league',
      'v4',
      'masterleagues',
      'by-queue',
      queue
    ) |>
    req_auth()

  resp <- safe_perform(req)
  if (is.null(resp)) return(NULL)

  if (isTRUE(clean)) {
    resp |>
      purrr::pluck('entries') |>
      lapply(list_to_row) |>
      dplyr::bind_rows() |>
      add_singletons(resp)
  } else {
    resp
  }
}

#' Get Grandmaster League
#'
#' @param queue `r roxy_queue()`
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with league entries or a list if `clean = FALSE`
#' @export
#' @examplesIf has_riot_key()
#' lol_get_grandmaster_league('RANKED_SOLO_5x5')
#'
lol_get_grandmaster_league <- function(queue, region = 'na1', clean = TRUE) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'league',
      'v4',
      'grandmasterleagues',
      'by-queue',
      queue
    ) |>
    req_auth()

  resp <- safe_perform(req)
  if (is.null(resp)) return(NULL)

  if (isTRUE(clean)) {
    resp |>
      purrr::pluck('entries') |>
      lapply(list_to_row) |>
      dplyr::bind_rows() |>
      add_singletons(resp)
  } else {
    resp
  }
}

#' Get League by ID
#'
#' @param league_id League ID.
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with league entries or a list if `clean = FALSE`
#' @export
#'
#' @examplesIf has_riot_key()
#' lol_get_league('7ccf5091-303b-3f5a-927b-7705b93e3564')
lol_get_league <- function(league_id, region = 'na1', clean = TRUE) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'league',
      'v4',
      'leagues',
      league_id
    ) |>
    req_auth()

  resp <- safe_perform(req)
  if (is.null(resp)) return(NULL)

  if (isTRUE(clean)) {
    resp |>
      purrr::pluck('entries') |>
      lapply(list_to_row) |>
      dplyr::bind_rows() |>
      add_singletons(resp)
  } else {
    resp
  }
}

#' Get League Entries
#'
#' @param queue `r roxy_queue()`
#' @param tier `r roxy_tier()`
#' @param division `r roxy_division()`
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with league entries or a list if `clean = FALSE`
#' @export
#'
#' @examplesIf has_riot_key()
#' lol_get_league_entries('RANKED_SOLO_5x5', 'DIAMOND', 'I')
lol_get_league_entries <- function(
  queue,
  tier,
  division,
  region = 'na1',
  clean = TRUE
) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'league',
      'v4',
      'entries',
      queue,
      tier,
      division
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

#' Get League Entries by PUUID
#'
#' @param puuid `r roxy_puuid()`
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with league entries or a list if `clean = FALSE`
#' @export
#'
#' @examplesIf has_riot_key()
#' puuid <- 'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA'
#' lol_get_league_entries_by_puuid(puuid)
lol_get_league_entries_by_puuid <- function(
  puuid,
  region = 'na1',
  clean = TRUE
) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'league',
      'v4',
      'entries',
      'by-puuid',
      puuid
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

#' Get Experimental League Entries
#'
#' @param queue `r roxy_queue()`
#' @param tier `r roxy_tier()`
#' @param division `r roxy_division()`
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with league entries or a list if `clean = FALSE`
#' @export
#' @examplesIf has_riot_key()
#' lol_get_league_exp_entries('RANKED_SOLO_5x5', 'DIAMOND', 'I')
#'
lol_get_league_exp_entries <- function(
  queue,
  tier,
  division,
  region = 'na1',
  clean = TRUE
) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'league-exp',
      'v4',
      'entries',
      queue,
      tier,
      division
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