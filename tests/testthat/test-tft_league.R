test_that('`tft_get_challenger_league()` works', {
  vcr::local_cassette('tft_challenger_league')
  x <- tft_get_challenger_league()
  expect_s3_class(x, 'tbl_df')
})

test_that('`tft_get_grandmaster_league()` works', {
  vcr::local_cassette('tft_grandmaster_league')
  x <- tft_get_grandmaster_league()
  expect_s3_class(x, 'tbl_df')
})

test_that('`tft_get_master_league()` works', {
  vcr::local_cassette('tft_master_league')
  x <- tft_get_master_league()
  expect_s3_class(x, 'tbl_df')
})

test_that('`tft_get_league_entries()` works', {
  vcr::local_cassette('tft_league_entries')
  x <- tft_get_league_entries('DIAMOND', 'I')
  expect_s3_class(x, 'tbl_df')
})

test_that('`tft_get_league_entries_by_puuid()` works', {
  vcr::local_cassette('tft_league_entries_by_puuid')
  x <- tft_get_league_entries_by_puuid(
    'GmNPNjUbzbWN8YIA79V4p308eFpf21xPO2Lw8o2xJTJMPW83W54t4GfyfpwOn7-hBz5c3MHpg-Dc3w'
  )
  expect_s3_class(x, 'tbl_df')
})

test_that('`tft_get_top_rated_ladder()` works', {
  vcr::local_cassette('tft_top_rated_ladder')
  x <- tft_get_top_rated_ladder('RANKED_TFT_TURBO')
  expect_s3_class(x, 'tbl_df')
})
