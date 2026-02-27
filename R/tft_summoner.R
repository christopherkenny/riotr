#' Get TFT Summoner by PUUID
#'
#' @param puuid `r roxy_puuid()`
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with summoner data or a list if `clean = FALSE`
#' @export
#' @examplesIf has_riot_key()
#' puuid <- 'GmNPNjUbzbWN8YIA79V4p308eFpf21xPO2Lw8o2xJTJMPW83W54t4GfyfpwOn7-hBz5c3MHpg-Dc3w'
#' tft_get_summoner_by_puuid(puuid)
#'
tft_get_summoner_by_puuid <- function(puuid, region = 'na1', clean = TRUE) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'tft',
      'summoner',
      'v1',
      'summoners',
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

#' Get TFT Summoner by Access Token
#'
#' Returns the TFT summoner associated with the current access token.
#'
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with summoner data, or `NULL` if the
#'   request fails. Returns a list if `clean = FALSE`.
#' @export
#' @examplesIf has_riot_key()
#' tft_get_summoner_me()
#'
tft_get_summoner_me <- function(region = 'na1', clean = TRUE) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'tft',
      'summoner',
      'v1',
      'summoners',
      'me'
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