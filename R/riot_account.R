#' Get Account by Riot ID
#'
#' @param game_name `r roxy_game_name()`
#' @param tag_line `r roxy_tag_line()`
#' @param regional `r roxy_regional()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with account data or a list if `clean = FALSE`
#' @export
#'
#' @examplesIf has_riot_key()
#' riot_get_account_by_riot_id('Doublelift', 'NA1')
riot_get_account_by_riot_id <- function(
  game_name,
  tag_line,
  regional = 'americas',
  clean = TRUE
) {
  req <- httr2::request(base_url = api_url(regional)) |>
    httr2::req_url_path_append(
      'riot',
      'account',
      'v1',
      'accounts',
      'by-riot-id',
      game_name,
      tag_line
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

#' Get Account by PUUID
#'
#' @param puuid `r roxy_puuid()`
#' @param regional `r roxy_regional()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with account data or a list if `clean = FALSE`
#' @export
#'
#' @examplesIf has_riot_key()
#' puuid <- 'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA'
#' riot_get_account_by_puuid(puuid)
riot_get_account_by_puuid <- function(
  puuid,
  regional = 'americas',
  clean = TRUE
) {
  req <- httr2::request(base_url = api_url(regional)) |>
    httr2::req_url_path_append(
      'riot',
      'account',
      'v1',
      'accounts',
      'by-puuid',
      puuid
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

#' Get Active Shard for a Player
#'
#' @param game Game identifier. Such as `"val"` or `"lor"`.
#' @param puuid `r roxy_puuid()`
#' @param regional `r roxy_regional()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with active shard data or a list if
#'   `clean = FALSE`
#' @export
#' @examplesIf has_riot_key()
#' puuid <- 'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA'
#' riot_get_active_shard('val', puuid)
#'
riot_get_active_shard <- function(
  game,
  puuid,
  regional = 'americas',
  clean = TRUE
) {
  req <- httr2::request(base_url = api_url(regional)) |>
    httr2::req_url_path_append(
      'riot',
      'account',
      'v1',
      'active-shards',
      'by-game',
      game,
      'by-puuid',
      puuid
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

#' Get Active Region for a Player
#'
#' @param game Game identifier. Such as `"val"` or `"lor"`.
#' @param puuid `r roxy_puuid()`
#' @param regional `r roxy_regional()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with active region data, or `NULL` if the
#'   request fails. Returns a list if `clean = FALSE`.
#' @export
#' @examplesIf has_riot_key()
#' puuid <- 'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA'
#' riot_get_active_region('val', puuid)
#'
riot_get_active_region <- function(
  game,
  puuid,
  regional = 'americas',
  clean = TRUE
) {
  req <- httr2::request(base_url = api_url(regional)) |>
    httr2::req_url_path_append(
      'riot',
      'account',
      'v1',
      'region',
      'by-game',
      game,
      'by-puuid',
      puuid
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

#' Get Account by PUUID (Esports)
#'
#' @param puuid `r roxy_puuid()`
#' @param regional `r roxy_regional()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with account data, or `NULL` if not found.
#'   Returns a list if `clean = FALSE`.
#' @export
#' @examplesIf has_riot_key()
#' puuid <- 'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA'
#' riot_get_account_by_puuid_esports(puuid)
#'
riot_get_account_by_puuid_esports <- function(
  puuid,
  regional = 'esports',
  clean = TRUE
) {
  req <- httr2::request(base_url = api_url(regional)) |>
    httr2::req_url_path_append(
      'riot',
      'account',
      'v1',
      'accounts',
      'by-puuid',
      puuid
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

#' Get Account by Riot ID (Esports)
#'
#' @param game_name `r roxy_game_name()`
#' @param tag_line `r roxy_tag_line()`
#' @param regional `r roxy_regional()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with account data, or `NULL` if not found.
#'   Returns a list if `clean = FALSE`.
#' @export
#' @examplesIf has_riot_key()
#' riot_get_account_by_riot_id_esports('Doublelift', 'NA1')
#'
riot_get_account_by_riot_id_esports <- function(
  game_name,
  tag_line,
  regional = 'esports',
  clean = TRUE
) {
  req <- httr2::request(base_url = api_url(regional)) |>
    httr2::req_url_path_append(
      'riot',
      'account',
      'v1',
      'accounts',
      'by-riot-id',
      game_name,
      tag_line
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