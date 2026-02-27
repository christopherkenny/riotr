test_that('`lol_get_all_challenge_configs()` works', {
  vcr::local_cassette('lol_all_challenge_configs')
  x <- lol_get_all_challenge_configs()
  expect_s3_class(x, 'tbl_df')
})

test_that('`lol_get_all_challenge_percentiles()` works', {
  vcr::local_cassette('lol_all_challenge_percentiles')
  x <- lol_get_all_challenge_percentiles()
  expect_type(x, 'list')
})

test_that('`lol_get_challenge_config()` works', {
  vcr::local_cassette('lol_challenge_config')
  x <- lol_get_challenge_config(1)
  expect_s3_class(x, 'tbl_df')
})

test_that('`lol_get_challenge_leaderboards()` works', {
  vcr::local_cassette('lol_challenge_leaderboards')
  x <- lol_get_challenge_leaderboards(1, 'GRANDMASTER')
  expect_s3_class(x, 'tbl_df')
})

test_that('`lol_get_challenge_percentiles()` works', {
  vcr::local_cassette('lol_challenge_percentiles')
  x <- lol_get_challenge_percentiles(1)
  expect_type(x, 'list')
})

test_that('`lol_get_challenge_player_data()` works', {
  vcr::local_cassette('lol_challenge_player_data')
  x <- lol_get_challenge_player_data(
    'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA'
  )
  expect_s3_class(x, 'tbl_df')
})
