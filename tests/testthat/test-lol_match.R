test_that('`lol_get_match_ids()` works', {
  vcr::local_cassette('lol_match_ids')
  x <- lol_get_match_ids(
    'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA',
    count = 5
  )
  expect_type(x, 'character')
})

test_that('`lol_get_match()` works', {
  vcr::local_cassette('lol_match')
  x <- lol_get_match('NA1_5494453720')
  expect_s3_class(x, 'tbl_df')
})

test_that('`lol_get_match_timeline()` works', {
  vcr::local_cassette('lol_match_timeline')
  x <- lol_get_match_timeline('NA1_5494453720')
  expect_s3_class(x, 'tbl_df')
})

test_that('`lol_get_match_replays()` works', {
  vcr::local_cassette('lol_match_replays')
  x <- lol_get_match_replays(
    'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA'
  )
  expect_type(x, 'character')
})
