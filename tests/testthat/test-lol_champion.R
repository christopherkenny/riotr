test_that('`lol_get_champion_rotations()` works', {
  vcr::local_cassette('lol_champion_rotations')
  x <- lol_get_champion_rotations()
  expect_s3_class(x, 'tbl_df')
  expect_true(ncol(x) >= 1)
})

test_that('`lol_get_champion_rotations()` works with clean = FALSE', {
  vcr::local_cassette('lol_champion_rotations_raw')
  x <- lol_get_champion_rotations(clean = FALSE)
  expect_type(x, 'list')
  expect_true('freeChampionIds' %in% names(x))
})
