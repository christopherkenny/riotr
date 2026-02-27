test_that('`tft_get_summoner_by_puuid()` works', {
  vcr::local_cassette('tft_summoner_by_puuid')
  x <- tft_get_summoner_by_puuid(
    'GmNPNjUbzbWN8YIA79V4p308eFpf21xPO2Lw8o2xJTJMPW83W54t4GfyfpwOn7-hBz5c3MHpg-Dc3w'
  )
  expect_s3_class(x, 'tbl_df')
})
