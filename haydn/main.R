library(tidyverse)
library(readODS)



# Load data ---------------------------------------------------------------

load_works <- function(file, siglum, ignored_mh = c("???", "deest")) {
  read_ods(file, col_types = cols(.default = "c")) %>%
    as_tibble() %>%
    filter(!mh %in% ignored_mh) %>%
    group_by(mh) %>%
    mutate(shelfmark = shelfmark %>% replace_na("")) %>%
    summarise(
      shelfmark = str_c(siglum, " ", shelfmark, collapse = ", "),
      edited = !any(is.na(edited))
    )
}


mh <-
  read_ods("data/mh.ods") %>%
  as_tibble() %>%
  rename(manuscript_source = source)

works <-
  tribble(
    ~file,                   ~siglum,
    "data/works A-Ed.ods",   "A-Ed",
    "data/works A-Wn.ods",   "A-Wn",
    "data/works A-Wst.ods",  "A-Wst",
    "data/works D-Eu.ods",   "D-Eu",
    "data/works D-KA.ods",   "D-KA",
    "data/works D-Mbs.ods",  "D-Mbs",
    "data/works D-NATk.ods", "D-NATk",
    "data/works F-Pn.ods",   "F-Pn",
    "data/works GB-Lbm.ods", "GB-Lbm",
    "data/works US-Wc.ods",  "US-Wc",
  ) %>%
  pmap_dfr(load_works)

available_works <-
  works %>%
  group_by(mh) %>%
  summarise(
    digital_source = str_c(shelfmark, collapse = "; "),
    edited = if (all(edited)) TRUE
             else if (!any(edited)) FALSE
             else stop("Inconsistency detected")
  )


# Save results ------------------------------------------------------------

used_genres <- c(
  "antiphon", "completorium", "gradual", "hymn", "introitus", "motet",
  "offertorium", "psalm", "responsorium", "sacred song", "sequence", "vesper"
)

ignored_notes <- c("fragment", "lost", "incomplete")

mh %>%
  left_join(available_works, by = "mh") %>%
  mutate(
    notes = notes %>% replace_na(""),
    manuscript_source = manuscript_source %>% replace_na(""),
    digital_source = digital_source %>% replace_na(""),
    edited = edited %>% replace_na(FALSE) %>% if_else("yes", "")
  ) %>%
  filter(genre %in% used_genres, !notes %in% ignored_notes) %>%
  relocate(digital_source, .after = genre) %>%
  write_ods("output/all_works.ods")

