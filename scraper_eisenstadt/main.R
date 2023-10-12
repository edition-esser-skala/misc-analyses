library(tidyverse)
library(rvest)
library(curl)
library(fs)



# Get HTMLs with high-resolution links ------------------------------------

all_works <-
  read_csv("data/all_works.csv") %>%
  filter(!str_detect(name, "Anonymus")) %>%
  separate_wider_delim(
    name,
    delim = ", ",
    names = c("siglum", "last", "rest"),
    too_many = "merge",
  ) %>%
  separate_wider_delim(
    rest,
    delim = ", ",
    names = c("first", "title"),
    too_many = "merge",
    too_few = "align_end"
  ) %>%
  {.}

make_curl_script <- function(last) {
  curl_template <- "curl '{url}' --compressed -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/118.0' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8' -H 'Accept-Language: de,en-US;q=0.7,en;q=0.3' -H 'Accept-Encoding: gzip, deflate, br' -H 'Connection: keep-alive' -H 'Referer: https://dommusikarchiv.martinus.at/site/werkverzeichnis?ts=1697019983' -H 'Cookie: klaro_consent_manager={{\"matomo\":true,\"siteswift\":true}}; PHPSESSID=2f85b2d06d19a658611e786d163be1d6; swqos=swqos' -H 'Upgrade-Insecure-Requests: 1' -H 'Sec-Fetch-Dest: document' -H 'Sec-Fetch-Mode: navigate' -H 'Sec-Fetch-Site: same-origin' -H 'Sec-Fetch-User: ?1' -H 'TE: trailers' > '{sig}.html'"
  dir_create(str_glue("data/{last}"))
  all_works %>%
    filter(last == {{last}}) %>%
    mutate(curl = str_glue(curl_template, url = url, sig = siglum)) %>%
    select(curl) %>%
    write_csv(
      str_glue("data/{last}/run_curl.sh"),
      col_names = FALSE,
      quote = "none",
      escape = "none"
    )
}

make_curl_script("Eybler")
make_curl_script("Fuchs")
make_curl_script("Werner")
make_curl_script("Hoffmann")
make_curl_script("Caldara")
make_curl_script("Reutter")
make_curl_script("Novotny")

# run the generated scripts manually



# Download image ----------------------------------------------------------

download_images_sig <- function(last, sig) {
  images <-
    read_html(str_glue("data/{last}/{sig}.html")) %>%
    html_elements(".gallery-grid-item") %>%
    html_attr("href")

  data <-
    tibble(
      url = str_c("https://dommusikarchiv.martinus.at", images),
      file = str_match(url, ".+__(.+?)$")[,2],
      destfile = str_glue("data/{last}/{sig}/{file}")
    )

  dir_create(str_glue("data/{last}/{sig}"))
  pmap(data, \(url, destfile, ...) curl_download(url, destfile, quiet = FALSE))
}

download_images_sig("Werner", "M 42")



download_images_last <- function(last) {
  sigs <-
    dir_ls(str_glue("data/{last}"), glob = "*.html") %>%
    path_file() %>%
    path_ext_remove()

  walk(sigs, \(sig) download_images_sig(last, sig))
}


download_images_last("Werner")
