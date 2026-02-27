test_that('`val_get_content()` works', {
  vcr::local_cassette('val_content')
  x <- val_get_content()
  expect_s3_class(x, 'tbl_df')
})

test_that('`val_get_leaderboard()` works', {
  vcr::local_cassette('val_leaderboard')
  x <- val_get_leaderboard('3ea2b318-423b-cf86-25da-7cbb0eefbe2d')
  expect_s3_class(x, 'tbl_df')
})
