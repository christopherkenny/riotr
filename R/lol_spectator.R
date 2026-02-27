#' Get Active Game by PUUID
#'
#' Returns active game information for the given summoner PUUID.
#'
#' @param puuid `r roxy_puuid()`
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with active game data or `NULL` if the player is
#'   not in a game. Returns a list if `clean = FALSE`.
#' @export
#' @examplesIf has_riot_key()
#' puuid <- 'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA'
#' lol_get_active_game(puuid)
#'
lol_get_active_game <- function(puuid, region = 'na1', clean = TRUE) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'spectator',
      'v5',
      'active-games',
      'by-summoner',
      puuid
    ) |>
    req_auth()

  resp <- safe_perform(req)
  if (is.null(resp)) return(NULL)

  if (isTRUE(clean)) {
    participants <- resp |>
      purrr::pluck('participants') |>
      lapply(list_to_row) |>
      dplyr::bind_rows()
    add_singletons(participants, resp)
  } else {
    resp
  }
}