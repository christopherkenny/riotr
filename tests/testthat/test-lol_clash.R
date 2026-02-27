test_that('`lol_get_clash_players()` works', {
  vcr::local_cassette('lol_clash_players')
  x <- lol_get_clash_players(
    'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA'
  )
  expect_true(is.null(x) || is.data.frame(x))
})

test_that('`lol_get_clash_tournaments()` works', {
  vcr::local_cassette('lol_clash_tournaments')
  x <- lol_get_clash_tournaments()
  expect_true(is.null(x) || is.data.frame(x))
})
