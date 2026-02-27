# riotr 0.0.0.9000

- Initial release of the riotr package with support for all 60 Riot Games
  Developer API endpoints across 20 API groups.
- API key management via `RIOT_KEY` environment variable with `set_riot_key()`,
  `get_riot_key()`, and `has_riot_key()`.
- All functions return tidy tibbles by default (`clean = TRUE`) or raw lists
  (`clean = FALSE`).
