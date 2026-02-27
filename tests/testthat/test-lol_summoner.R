test_that('`lol_get_summoner_by_puuid()` works', {
  vcr::local_cassette('lol_summoner_by_puuid')
  x <- lol_get_summoner_by_puuid(
    'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA'
  )
  expect_s3_class(x, 'tbl_df')
  expect_true('puuid' %in% names(x))
})
