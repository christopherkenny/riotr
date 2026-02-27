#' Get All Champion Masteries by PUUID
#'
#' @param puuid `r roxy_puuid()`
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with champion mastery data or a list if
#'   `clean = FALSE`
#' @export
#'
#' @examplesIf has_riot_key()
#' puuid <- 'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA'
#' lol_get_champion_masteries(puuid)
lol_get_champion_masteries <- function(puuid, region = 'na1', clean = TRUE) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'champion-mastery',
      'v4',
      'champion-masteries',
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

#' Get Champion Mastery by PUUID and Champion ID
#'
#' @param puuid `r roxy_puuid()`
#' @param champion_id `r roxy_champion_id()`
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with champion mastery data or a list if
#'   `clean = FALSE`
#' @export
#'
#' @examplesIf has_riot_key()
#' puuid <- 'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA'
#' lol_get_champion_mastery(puuid, 1)
lol_get_champion_mastery <- function(
  puuid,
  champion_id,
  region = 'na1',
  clean = TRUE
) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'champion-mastery',
      'v4',
      'champion-masteries',
      'by-puuid',
      puuid,
      'by-champion',
      champion_id
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

#' Get Champion Mastery Score by PUUID
#'
#' @param puuid `r roxy_puuid()`
#' @param region `r roxy_region()`
#'
#' @return an integer representing the total champion mastery score
#' @export
#'
#' @examplesIf has_riot_key()
#' puuid <- 'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA'
#' lol_get_champion_mastery_score(puuid)
lol_get_champion_mastery_score <- function(puuid, region = 'na1') {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'champion-mastery',
      'v4',
      'scores',
      'by-puuid',
      puuid
    ) |>
    req_auth()

  safe_perform(req)
}

#' Get Top Champion Masteries by PUUID
#'
#' @param puuid `r roxy_puuid()`
#' @param count Number of entries to return. Defaults to `3`.
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with top champion mastery data or a list if
#'   `clean = FALSE`
#' @export
#'
#' @examplesIf has_riot_key()
#' puuid <- 'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA'
#' lol_get_top_champion_masteries(puuid)
lol_get_top_champion_masteries <- function(
  puuid,
  count = NULL,
  region = 'na1',
  clean = TRUE
) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'champion-mastery',
      'v4',
      'champion-masteries',
      'by-puuid',
      puuid,
      'top'
    ) |>
    httr2::req_url_query(count = count) |>
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
