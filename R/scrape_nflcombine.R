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
    Authorization = "Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJjbGllbnRJZCI6ImU1MzVjN2MwLTgxN2YtNDc3Ni04OTkwLTU2NTU2ZjhiMTkyOCIsImNsaWVudEtleSI6IjRjRlVXNkRtd0pwelQ5TDdMckczcVJBY0FCRzVzMDRnIiwiZGV2aWNlSWQiOiJmOTMxYWFiZi1iZWFjLTRlZjEtYmMzNC0wNTIxOThkOGZlYzEiLCJpc3MiOiJORkwiLCJwbGFucyI6W3sicGxhbiI6ImZyZWUiLCJleHBpcmF0aW9uRGF0ZSI6IjIwMjYtMDMtMDMiLCJzb3VyY2UiOiJORkwiLCJzdGFydERhdGUiOiIyMDI1LTAzLTAzIiwic3RhdHVzIjoiQUNUSVZFIiwidHJpYWwiOmZhbHNlfV0sIkRpc3BsYXlOYW1lIjoiV0VCX0RFU0tUT1BfREVTS1RPUCIsIk5vdGVzIjoiIiwiZm9ybUZhY3RvciI6IkRFU0tUT1AiLCJsdXJhQXBwS2V5IjoiU1pzNTdkQkdSeGJMNzI4bFZwN0RZUSIsInBsYXRmb3JtIjoiREVTS1RPUCIsInByb2R1Y3ROYW1lIjoiV0VCIiwicm9sZXMiOlsiY29udGVudCIsImV4cGVyaWVuY2UiLCJmb290YmFsbCIsInV0aWxpdGllcyIsInRlYW1zIiwicGxheSIsImxpdmUiLCJpZGVudGl0eSIsIm5nc19zdGF0cyIsInBheW1lbnRzX2FwaSIsIm5nc190cmFja2luZyIsIm5nc19wbGF0Zm9ybSIsIm5nc19jb250ZW50IiwibmdzX2NvbWJpbmUiLCJuZ3NfYWR2YW5jZWRfc3RhdHMiLCJuZmxfcHJvIiwiZWNvbW0iLCJuZmxfaWRfYXBpIiwidXRpbGl0aWVzX2xvY2F0aW9uIiwiaWRlbnRpdHlfb2lkYyIsIm5nc19zc2UiLCJhY2NvdW50cyIsImZyZWUiXSwibmV0d29ya1R5cGUiOiJvdGhlciIsImNpdHkiOiJvdHRhd2EiLCJjb3VudHJ5Q29kZSI6IkNBIiwiZG1hQ29kZSI6IjEyNDUwNSIsImhtYVRlYW1zIjpbIjEwNDAxNTQwLWY5N2MtMmQxOS02ZmNkLWZhYzY0OTBhNDhiNyIsIjEwNDAzMDAwLTU4NTEtZjlkNS1kYTQ1LTc4MzY1YTA1YjZiMCIsIjEwNDA0NjAwLWFkY2QtMjhhYy01ODI2LWI0ZDk1ZWMyYTIyOCJdLCJyZWdpb24iOiJPTiIsInppcENvZGUiOiJrMWggN3o4IiwiYnJvd3NlciI6IkZpcmVmb3giLCJjZWxsdWxhciI6ZmFsc2UsImVudmlyb25tZW50IjoicHJvZHVjdGlvbiIsInVpZCI6ImZmYzhkZDgxZjliZWIwZTg1Zjk4ZWFlODE0MjBjZGExIiwiZXhwIjoxNzQwOTcxMzA5fQ.ZQF0XhwEUfkv2-liZ6xTLynMc8Qyo2wSInJ9_IU259Q",
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
