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
    `User-Agent` = "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:134.0) Gecko/20100101 Firefox/134.0",
    Accept = "*/*",
    `Accept-Language` = "en-CA,en-US;q=0.7,en;q=0.3",
    `Accept-Encoding` = "gzip, deflate, br, zstd",
    Referer = "https://www.nfl.com/",
    Authorization = paste("Bearer", nflapi::nflapi_token()),
    Origin = "https://www.nfl.com",
    Connection = "keep-alive",
    `Sec-Fetch-Dest` = "empty",
    `Sec-Fetch-Mode` = "cors",
    `Sec-Fetch-Site` = "same-site",
    Priority = "u=4",
    TE = "trailers"
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
  season = 2008:2025 |> as.character()
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
