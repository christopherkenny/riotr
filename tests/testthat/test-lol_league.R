test_that('`lol_get_challenger_league()` works', {
  vcr::local_cassette('lol_challenger_league')
  x <- lol_get_challenger_league('RANKED_SOLO_5x5')
  expect_s3_class(x, 'tbl_df')
})

test_that('`lol_get_grandmaster_league()` works', {
  vcr::local_cassette('lol_grandmaster_league')
  x <- lol_get_grandmaster_league('RANKED_SOLO_5x5')
  expect_s3_class(x, 'tbl_df')
})

test_that('`lol_get_master_league()` works', {
  vcr::local_cassette('lol_master_league')
  x <- lol_get_master_league('RANKED_SOLO_5x5')
  expect_s3_class(x, 'tbl_df')
})

test_that('`lol_get_league_entries()` works', {
  vcr::local_cassette('lol_league_entries')
  x <- lol_get_league_entries('RANKED_SOLO_5x5', 'DIAMOND', 'I')
  expect_s3_class(x, 'tbl_df')
})

test_that('`lol_get_league_entries_by_puuid()` works', {
  vcr::local_cassette('lol_league_entries_by_puuid')
  x <- lol_get_league_entries_by_puuid(
    'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA'
  )
  expect_s3_class(x, 'tbl_df')
})

test_that('`lol_get_league_exp_entries()` works', {
  vcr::local_cassette('lol_league_exp_entries')
  x <- lol_get_league_exp_entries('RANKED_SOLO_5x5', 'DIAMOND', 'I')
  expect_s3_class(x, 'tbl_df')
})

test_that('`lol_get_league()` works', {
  vcr::local_cassette('lol_league_by_id')
  x <- lol_get_league('7ccf5091-303b-3f5a-927b-7705b93e3564')
  expect_s3_class(x, 'tbl_df')
})
