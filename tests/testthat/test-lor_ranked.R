test_that('`lor_get_leaderboards()` works', {
  vcr::local_cassette('lor_leaderboards')
  x <- lor_get_leaderboards()
  expect_s3_class(x, 'tbl_df')
})
