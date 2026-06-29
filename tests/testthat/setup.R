library(vcr)

if (!has_riot_key()) {
  withr::local_envvar(RIOT_KEY = "dummy-key-for-testing", .local_envir = testthat::teardown_env())
}
