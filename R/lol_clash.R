#' Get Clash Players by PUUID
#'
#' @param puuid `r roxy_puuid()`
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with player data or a list if `clean = FALSE`
#' @export
#'
#' @examplesIf has_riot_key()
#' puuid <- 'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA'
#' lol_get_clash_players(puuid)
lol_get_clash_players <- function(puuid, region = 'na1', clean = TRUE) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'clash',
      'v1',
      'players',
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

#' Get Clash Team by ID
#'
#' @param team_id Team ID.
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with team data, or `NULL` if not found.
#'   Returns a list if `clean = FALSE`.
#' @export
#' @examplesIf has_riot_key()
#' lol_get_clash_team('PLACEHOLDER_CLASH_TEAM_ID')
#'
lol_get_clash_team <- function(team_id, region = 'na1', clean = TRUE) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'clash',
      'v1',
      'teams',
      team_id
    ) |>
    req_auth()

  resp <- safe_perform(req)
  if (is.null(resp)) return(NULL)

  if (isTRUE(clean)) {
    players <- resp |>
      purrr::pluck('players') |>
      lapply(list_to_row) |>
      dplyr::bind_rows()
    add_singletons(players, resp)
  } else {
    resp
  }
}

#' Get Clash Tournament by ID
#'
#' @param tournament_id Tournament ID.
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with tournament data, or `NULL` if not found.
#'   Returns a list if `clean = FALSE`.
#' @export
#' @examplesIf has_riot_key()
#' lol_get_clash_tournament('PLACEHOLDER_CLASH_TOURNAMENT_ID')
#'
lol_get_clash_tournament <- function(
  tournament_id,
  region = 'na1',
  clean = TRUE
) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'clash',
      'v1',
      'tournaments',
      tournament_id
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

#' Get Clash Tournament by Team ID
#'
#' @param team_id Team ID.
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with tournament data, or `NULL` if not found.
#'   Returns a list if `clean = FALSE`.
#' @export
#' @examplesIf has_riot_key()
#' lol_get_clash_tournament_by_team('PLACEHOLDER_CLASH_TEAM_ID')
#'
lol_get_clash_tournament_by_team <- function(
  team_id,
  region = 'na1',
  clean = TRUE
) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'clash',
      'v1',
      'tournaments',
      'by-team',
      team_id
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

#' Get All Clash Tournaments
#'
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with tournament data or a list if
#'   `clean = FALSE`
#' @export
#'
#' @examplesIf has_riot_key()
#' lol_get_clash_tournaments()
lol_get_clash_tournaments <- function(region = 'na1', clean = TRUE) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'clash',
      'v1',
      'tournaments'
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