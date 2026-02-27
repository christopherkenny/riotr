test_that('`lol_get_platform_data()` works', {
  vcr::local_cassette('lol_platform_data')
  x <- lol_get_platform_data()
  expect_s3_class(x, 'tbl_df')
  expect_true('id' %in% names(x))
})

test_that('`tft_get_platform_data()` works', {
  vcr::local_cassette('tft_platform_data')
  x <- tft_get_platform_data()
  expect_s3_class(x, 'tbl_df')
})

test_that('`lor_get_platform_data()` works', {
  vcr::local_cassette('lor_platform_data')
  x <- lor_get_platform_data()
  expect_s3_class(x, 'tbl_df')
})

test_that('`val_get_platform_data()` works', {
  vcr::local_cassette('val_platform_data')
  x <- val_get_platform_data()
  expect_s3_class(x, 'tbl_df')
})
