#' Get TFT League Entries
#'
#' @param tier `r roxy_tier()`
#' @param division `r roxy_division()`
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with league entries or a list if `clean = FALSE`
#' @export
#' @examplesIf has_riot_key()
#' tft_get_league_entries('DIAMOND', 'I')
#'
tft_get_league_entries <- function(
  tier,
  division,
  region = 'na1',
  clean = TRUE
) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'tft',
      'league',
      'v1',
      'entries',
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

#' Get TFT Challenger League
#'
#' @param queue Queue type. Defaults to `"RANKED_TFT"`.
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with league entries or a list if `clean = FALSE`
#' @export
#' @examplesIf has_riot_key()
#' tft_get_challenger_league()
#'
tft_get_challenger_league <- function(
  queue = 'RANKED_TFT',
  region = 'na1',
  clean = TRUE
) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'tft',
      'league',
      'v1',
      'challenger'
    ) |>
    httr2::req_url_query(queue = queue) |>
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

#' Get TFT Grandmaster League
#'
#' @param queue Queue type. Defaults to `"RANKED_TFT"`.
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with league entries or a list if `clean = FALSE`
#' @export
#' @examplesIf has_riot_key()
#' tft_get_grandmaster_league()
#'
tft_get_grandmaster_league <- function(
  queue = 'RANKED_TFT',
  region = 'na1',
  clean = TRUE
) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'tft',
      'league',
      'v1',
      'grandmaster'
    ) |>
    httr2::req_url_query(queue = queue) |>
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

#' Get TFT Master League
#'
#' @param queue Queue type. Defaults to `"RANKED_TFT"`.
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with league entries or a list if `clean = FALSE`
#' @export
#' @examplesIf has_riot_key()
#' tft_get_master_league()
#'
tft_get_master_league <- function(
  queue = 'RANKED_TFT',
  region = 'na1',
  clean = TRUE
) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'tft',
      'league',
      'v1',
      'master'
    ) |>
    httr2::req_url_query(queue = queue) |>
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

#' Get TFT Top Rated Ladder
#'
#' @param queue Queue type for TFT rated ladder.
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with top rated ladder entries or a list if
#'   `clean = FALSE`
#' @export
#' @examplesIf has_riot_key()
#' tft_get_top_rated_ladder('RANKED_TFT_TURBO')
#'
tft_get_top_rated_ladder <- function(queue, region = 'na1', clean = TRUE) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'tft',
      'league',
      'v1',
      'rated-ladders',
      queue,
      'top'
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

#' Get TFT League Entries by PUUID
#'
#' @param puuid `r roxy_puuid()`
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with league entries or a list if `clean = FALSE`
#' @export
#' @examplesIf has_riot_key()
#' puuid <- 'GmNPNjUbzbWN8YIA79V4p308eFpf21xPO2Lw8o2xJTJMPW83W54t4GfyfpwOn7-hBz5c3MHpg-Dc3w'
#' tft_get_league_entries_by_puuid(puuid)
#'
tft_get_league_entries_by_puuid <- function(
  puuid,
  region = 'na1',
  clean = TRUE
) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'tft',
      'league',
      'v1',
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