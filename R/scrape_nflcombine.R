library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(nflreadr)
library(janitor)
library(httr)
library(jsonlite)

scrape_combine <- function(season){
  
  headers = c(
    `User-Agent` = 'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:99.0) Gecko/20100101 Firefox/99.0',
    `Accept` = '*/*',
    `Accept-Language` = 'en-US,en;q=0.5',
    `Accept-Encoding` = 'gzip, deflate, br',
    `Referer` = 'https://www.nfl.com/',
    `Authorization` = 'Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJjbGllbnRJZCI6ImU1MzVjN2MwLTgxN2YtNDc3Ni04OTkwLTU2NTU2ZjhiMTkyOCIsImNsaWVudEtleSI6IjRjRlVXNkRtd0pwelQ5TDdMckczcVJBY0FCRzVzMDRnIiwiaXNzIjoiTkZMIiwiZGV2aWNlSWQiOiI3MDhkZWQ0YS1jM2RmLTQ1ZGUtYTQxYy1mZjFkMzIwMzRiYzIiLCJwbGFucyI6W3sicGxhbiI6ImZyZWUiLCJleHBpcmF0aW9uRGF0ZSI6IjIwMjUtMDMtMzEiLCJzb3VyY2UiOiJORkwiLCJzdGFydERhdGUiOiIyMDI0LTAzLTMxIiwic3RhdHVzIjoiQUNUSVZFIiwidHJpYWwiOmZhbHNlfV0sIkRpc3BsYXlOYW1lIjoiV0VCX0RFU0tUT1BfREVTS1RPUCIsIk5vdGVzIjoiIiwiZm9ybUZhY3RvciI6IkRFU0tUT1AiLCJsdXJhQXBwS2V5IjoiU1pzNTdkQkdSeGJMNzI4bFZwN0RZUSIsInBsYXRmb3JtIjoiREVTS1RPUCIsInByb2R1Y3ROYW1lIjoiV0VCIiwiY2l0eSI6ImdvcmRvbiBwYXJrIiwiY291bnRyeUNvZGUiOiJBVSIsImRtYUNvZGUiOiIzNjMwNSIsImhtYVRlYW1zIjpbIjEwNDAyNTEwLTg5MzEtMGQ1Zi05ODE1LTc5YmI3OTY0OWE2NSIsIjEwNDAzNzAwLWI5MzktM2NiZC0zZDE2LTI0ZDRkNjc0MmZhMiJdLCJyZWdpb24iOiJRTEQiLCJicm93c2VyIjoiQ2hyb21lIiwiY2VsbHVsYXIiOmZhbHNlLCJlbnZpcm9ubWVudCI6InByb2R1Y3Rpb24iLCJyb2xlcyI6WyJmcmVlIl0sImV4cCI6MTcxMTg4Mzg2OH0.STS3hXsk4DwtfNZtVbv43ERoLLOACUO-wPwpKmAvjw0',
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
  season = 2008:2024 |> as.character()
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
         pos = position,
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
         pos = position,
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
