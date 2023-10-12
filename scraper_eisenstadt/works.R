library(tidyverse)
library(rvest)

get_works <- function(i) {
  print(i)
  works <-
    str_glue("https://dommusikarchiv.martinus.at/site/werkverzeichnis/index{i}.html") %>%
    read_html() %>%
    html_elements("h2 a")

  tibble(
    name = html_text2(works),
    url = html_attr(works, "href")
  )
}

all_works <-
  map(1:33, get_works) %>%
  list_rbind()

all_works %>% write_csv("data/all_works.csv")
