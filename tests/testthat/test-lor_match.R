test_that('`lor_get_match_ids()` works', {
  vcr::local_cassette('lor_match_ids')
  x <- lor_get_match_ids(
    'GmNPNjUbzbWN8YIA79V4p308eFpf21xPO2Lw8o2xJTJMPW83W54t4GfyfpwOn7-hBz5c3MHpg-Dc3w'
  )
  expect_type(x, 'character')
})
