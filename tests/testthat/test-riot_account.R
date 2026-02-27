test_that('`riot_get_account_by_riot_id()` works', {
  vcr::local_cassette('riot_account_by_riot_id')
  x <- riot_get_account_by_riot_id('Doublelift', 'NA1')
  expect_s3_class(x, 'tbl_df')
  expect_true('puuid' %in% names(x))
})

test_that('`riot_get_account_by_puuid()` works', {
  vcr::local_cassette('riot_account_by_puuid')
  x <- riot_get_account_by_puuid(
    'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA'
  )
  expect_s3_class(x, 'tbl_df')
  expect_true('gameName' %in% names(x))
})

test_that('`riot_get_active_shard()` works', {
  vcr::local_cassette('riot_active_shard')
  x <- riot_get_active_shard(
    'val',
    'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA'
  )
  expect_s3_class(x, 'tbl_df')
})
