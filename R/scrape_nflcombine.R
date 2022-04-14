library(tidyverse)
library(httr)
library(jsonlite)

scrape_combine <- function(season){
  headers = c(
    `User-Agent` = 'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:99.0) Gecko/20100101 Firefox/99.0',
    `Accept` = '*/*',
    `Accept-Language` = 'en-US,en;q=0.5',
    `Accept-Encoding` = 'gzip, deflate, br',
    `Referer` = 'https://www.nfl.com/',
    `Authorization` = 'Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJjbGllbnRJZCI6ImU1MzVjN2MwLTgxN2YtNDc3Ni04OTkwLTU2NTU2ZjhiMTkyOCIsImRtYUNvZGUiOiIxMjQ1MDUiLCJmb3JtRmFjdG9yIjoiREVTS1RPUCIsImlzcyI6Ik5GTCIsImRldmljZUlkIjoiZmRkZGJiYWYtMmY5Yy00N2YzLWI2MGEtYjIwMGI5MWZhNDIxIiwicGxhdGZvcm0iOiJERVNLVE9QIiwicHJvZHVjdE5hbWUiOiJXRUIiLCJwbGFucyI6W3sic291cmNlIjoiTkZMIiwicGxhbiI6ImZyZWUiLCJ0cmlhbCI6ImZhbHNlIiwic3RhdHVzIjoiQUNUSVZFIiwiZXhwaXJhdGlvbkRhdGUiOiIyMDIzLTA0LTE0In1dLCJjb3VudHJ5Q29kZSI6IkNBIiwiY2VsbHVsYXIiOmZhbHNlLCJicm93c2VyIjoiRmlyZWZveCIsIkRpc3BsYXlOYW1lIjoiV0VCX0RFU0tUT1BfREVTS1RPUCIsImx1cmFBcHBLZXkiOiJTWnM1N2RCR1J4Ykw3MjhsVnA3RFlRIiwiZG1hIjoiMTI0NTA1IiwiZXhwIjoxNjQ5OTAxNjQyLCJOb3RlcyI6IiJ9.hF1lzN-my44V0fCHlfe-iyaHyDW4V-JntBRAf_7JWKE',
    `Origin` = 'https://www.nfl.com',
    `Connection` = 'keep-alive',
    `Sec-Fetch-Dest` = 'empty',
    `Sec-Fetch-Mode` = 'cors',
    `Sec-Fetch-Site` = 'same-site'
  )

  params = list(
    `year` = as.character(season),
    `limit` = '1000'
  )
  res <- httr::GET(url = 'https://api.nfl.com/football/v2/combine/profiles', httr::add_headers(.headers=headers), query = params)

  x <- httr::content(res, as = "text") |>
    jsonlite::parse_json() |>
    purrr::pluck("combineProfiles") |>
    tibble::tibble() |>
    tidyr::unnest_wider(1)

  return(x)
}

x <- tibble::tibble(
  season = 2014:2022 |> as.character()
) |>
  dplyr::mutate(
    data = purrr::map(season,scrape_combine)
  ) |>
  tidyr::unnest(data) |>
  tidyr::hoist(person,"displayName") |>
  janitor::clean_names()

ngs_scores <- x |>
  select(year,
         id,
         player_name = display_name,
         pos = position_group,
         draft_projection,
         athleticism_score,
         size_score,
         production_score,
         ngs_score = draft_grade
  )


scout_grades <- x |>
  select(year,
         id,
         player_name = display_name,
         pos = position_group,
         draft_projection,
         grade,
         profile_author,
         overview,
         strengths,
         weaknesses,
         nfl_comparison,
         sources_tell_us
  )

data.table::fwrite(ngs_scores,"data/ngs_scores.csv")
data.table::fwrite(scout_grades,"data/scout_grades.csv")

saveRDS(x,"data/raw_combine_data.rds")
