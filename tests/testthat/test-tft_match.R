test_that('`tft_get_match_ids()` works', {
  vcr::local_cassette('tft_match_ids')
  x <- tft_get_match_ids(
    'GmNPNjUbzbWN8YIA79V4p308eFpf21xPO2Lw8o2xJTJMPW83W54t4GfyfpwOn7-hBz5c3MHpg-Dc3w',
    count = 5
  )
  expect_type(x, 'character')
})

test_that('`tft_get_match()` works', {
  vcr::local_cassette('tft_match')
  x <- tft_get_match('NA1_5501463494')
  expect_s3_class(x, 'tbl_df')
})
