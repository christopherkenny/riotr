#' Get LoL Platform Data (Status)
#'
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with platform status data or a list if
#'   `clean = FALSE`
#' @export
#'
#' @examplesIf has_riot_key()
#' lol_get_platform_data()
lol_get_platform_data <- function(region = 'na1', clean = TRUE) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'lol',
      'status',
      'v4',
      'platform-data'
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

#' Get TFT Platform Data (Status)
#'
#' @param region `r roxy_region()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with platform status data or a list if
#'   `clean = FALSE`
#' @export
#'
#' @examplesIf has_riot_key()
#' tft_get_platform_data()
tft_get_platform_data <- function(region = 'na1', clean = TRUE) {
  req <- httr2::request(base_url = api_url(region)) |>
    httr2::req_url_path_append(
      'tft',
      'status',
      'v1',
      'platform-data'
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

#' Get LoR Platform Data (Status)
#'
#' @param regional `r roxy_regional()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with platform status data or a list if
#'   `clean = FALSE`
#' @export
#'
#' @examplesIf has_riot_key()
#' lor_get_platform_data()
lor_get_platform_data <- function(regional = 'americas', clean = TRUE) {
  req <- httr2::request(base_url = api_url(regional)) |>
    httr2::req_url_path_append(
      'lor',
      'status',
      'v1',
      'platform-data'
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

#' Get Valorant Platform Data (Status)
#'
#' @param regional `r roxy_regional()`
#' @param clean `r roxy_clean()`
#'
#' @return a [tibble::tibble] with platform status data or a list if
#'   `clean = FALSE`
#' @export
#'
#' @examplesIf has_riot_key()
#' val_get_platform_data()
val_get_platform_data <- function(regional = 'na', clean = TRUE) {
  req <- httr2::request(base_url = api_url(regional)) |>
    httr2::req_url_path_append(
      'val',
      'status',
      'v1',
      'platform-data'
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
