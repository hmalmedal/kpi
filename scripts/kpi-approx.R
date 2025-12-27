library(stringr)
library(httr2)
library(dplyr)
library(purrr)
library(rjstat)
library(lubridate)
library(jsonlite)
library(magrittr)

body <- list(
  query = list(
    list(
      code = "Maaned",
      selection = list(
        filter = "item",
        values = I(str_pad(1:12, width = 2, pad = 0))
      )
    ),
    list(
      code = "ContentsCode",
      selection = list(
        filter = "item",
        values = I("KpiIndMnd")
      )
    )
  ),
  response = list(
    format = "json-stat"
  )
)

req <- request("https://data.ssb.no/api/v0/no/table/08981") |>
  req_body_json(body)

resp <- req |>
  req_perform()

KPI <- resp |>
  resp_body_string() |>
  fromJSONstat(use_factors = TRUE) |>
  chuck(1) |>
  as_tibble()

KPI_dato <- KPI |>
  mutate(Dato = make_date(as.character(år), måned) |>
           ceiling_date("month") - days(1)
         ) |>
  filter(!is.na(value)) |>
  select(Dato, Konsumprisindeks = value) |>
  arrange(Dato)

KPI_approx <- KPI_dato %$%
  approx(Dato, Konsumprisindeks, seq(min(Dato), max(Dato), by = "days")) %>%
  as_tibble() %>%
  rename(date = x, close = y)

write_json(list(data = KPI_approx), "site/kpi.json")
