#' Create Tournament Code (Stub)
#'
#' Creates a tournament code for the given tournament.
#'
#' @param tournament_id Tournament ID.
#' @param count Number of codes to generate. Defaults to `1`.
#' @param body A list with tournament code parameters including
#'   `mapType`, `pickType`, `spectatorType`, `teamSize`, and
#'   optionally `allowedParticipants` and `metadata`.
#' @param region `r roxy_region()`
#'
#' @return a character vector of tournament codes
#' @export
#' @examplesIf has_riot_key() && Sys.getenv('RIOT_TOURNAMENT') != ''
#' lol_create_tournament_code(
#'   tournament_id = 1234,
#'   body = list(
#'     mapType = 'SUMMONERS_RIFT',
#'     pickType = 'TOURNAMENT_DRAFT',
#'     spectatorType = 'ALL',
#'     teamSize = 5
#'   )
#' )
#'
lol_create_tournament_code <- function(
  tournament_id,
  count = NULL,
  body,
  region = 'na1'
) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'tournament-stub',
      'v5',
      'codes'
    ) |>
    httr2::req_url_query(
      tournamentId = tournament_id,
      count = count
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_method('POST') |>
    req_auth()

  resp <- safe_perform(req)
  if (is.null(resp)) return(NULL)

  as.character(unlist(resp))
}

#' Get Lobby Events by Tournament Code
#'
#' @param tournament_code Tournament code.
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with lobby events or a list if `clean = FALSE`
#' @export
#' @examplesIf has_riot_key() && Sys.getenv('RIOT_TOURNAMENT') != ''
#' lol_get_lobby_events('tournament-code')
#'
lol_get_lobby_events <- function(
  tournament_code,
  region = 'na1',
  clean = TRUE
) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'tournament-stub',
      'v5',
      'lobby-events',
      'by-code',
      tournament_code
    ) |>
    req_auth()

  resp <- safe_perform(req)
  if (is.null(resp)) return(NULL)

  if (isTRUE(clean)) {
    resp |>
      purrr::pluck('eventList') |>
      lapply(list_to_row) |>
      dplyr::bind_rows()
  } else {
    resp
  }
}

#' Get Tournament Code
#'
#' @param tournament_code Tournament code.
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with tournament code data or a list if
#'   `clean = FALSE`
#' @export
#' @examplesIf has_riot_key() && Sys.getenv('RIOT_TOURNAMENT') != ''
#' lol_get_tournament_code('tournament-code')
#'
lol_get_tournament_code <- function(
  tournament_code,
  region = 'na1',
  clean = TRUE
) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'tournament-stub',
      'v5',
      'codes',
      tournament_code
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

#' Register Tournament Provider (Stub)
#'
#' Registers a tournament provider.
#'
#' @param body A list with `region` and `url` fields.
#' @param region `r roxy_region()`
#'
#' @return an integer provider ID
#' @export
#' @examplesIf has_riot_key() && Sys.getenv('RIOT_TOURNAMENT') != ''
#' lol_register_provider(
#'   body = list(region = 'NA', url = 'https://example.com')
#' )
#'
lol_register_provider <- function(body, region = 'na1') {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'tournament-stub',
      'v5',
      'providers'
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_method('POST') |>
    req_auth()

  safe_perform(req)
}

#' Register Tournament (Stub)
#'
#' Registers a tournament.
#'
#' @param body A list with `name` and `providerId` fields.
#' @param region `r roxy_region()`
#'
#' @return an integer tournament ID
#' @export
#' @examplesIf has_riot_key() && Sys.getenv('RIOT_TOURNAMENT') != ''
#' lol_register_tournament(
#'   body = list(name = 'My Tournament', providerId = 1)
#' )
#'
lol_register_tournament <- function(body, region = 'na1') {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'tournament-stub',
      'v5',
      'tournaments'
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_method('POST') |>
    req_auth()

  safe_perform(req)
}