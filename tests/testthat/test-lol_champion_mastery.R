test_that('`lol_get_champion_masteries()` works', {
  vcr::local_cassette('lol_champion_masteries')
  x <- lol_get_champion_masteries(
    'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA'
  )
  expect_s3_class(x, 'tbl_df')
})

test_that('`lol_get_champion_mastery()` works', {
  vcr::local_cassette('lol_champion_mastery')
  x <- lol_get_champion_mastery(
    'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA',
    236
  )
  expect_s3_class(x, 'tbl_df')
})

test_that('`lol_get_champion_mastery_score()` works', {
  vcr::local_cassette('lol_champion_mastery_score')
  x <- lol_get_champion_mastery_score(
    'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA'
  )
  expect_type(x, 'integer')
})

test_that('`lol_get_top_champion_masteries()` works', {
  vcr::local_cassette('lol_top_champion_masteries')
  x <- lol_get_top_champion_masteries(
    'O2q8aNv6VBlFz9ABjEoOH8g1d_KO1IQHboIuog8ORd7QOH2L8nGgsYgK-TN16ucwUQsPpihH-tLtAA'
  )
  expect_s3_class(x, 'tbl_df')
})
