library(tidyverse)
library(readODS)
library(openxlsx)



# Functions ---------------------------------------------------------------

save_table <- function(tables, filename, sheet_name = "Sheet1") {
  filename <- str_glue("{filename}.xlsx")
  wb <- createWorkbook()

  # ensure that tables is a named list
  if (inherits(tables, "list")) {
    if (is.null(names(tables)))
      tables <- set_names(tables, paste0("Sheet", seq_along(tables)))
  } else if (inherits(tables, "data.frame")) {
    tables <- list(tables) %>% set_names(sheet_name)
  } else {
    stop("'tables' must be a data frame or list of data frames.")
  }

  # populate Excel file with worksheets
  iwalk(
    tables,
    function(table, sheet_name) {
      addWorksheet(wb, sheet_name)
      writeData(
        wb,
        sheet_name,
        table,
        headerStyle = createStyle(textDecoration = "bold")
      )
      freezePane(wb, sheet_name, firstRow = TRUE)
      setColWidths(wb, sheet_name, 1:ncol(table), "auto")
    }
  )

  saveWorkbook(wb, filename, overwrite = TRUE)
}

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



# Load data ---------------------------------------------------------------

mh <-
  read_ods("data/mh.ods") %>%
  as_tibble() %>%
  rename(manuscript_source = source)

digitized_works <-
  tribble(
    ~file,                   ~siglum,
    "data/works A-Ed.ods",   "A-Ed",
    "data/works A-Wn.ods",   "A-Wn",
    "data/works A-Wst.ods",  "A-Wst",
    "data/works CZ-Pu.ods",  "CZ-Pu",
    "data/works D-Eu.ods",   "D-Eu",
    "data/works D-KA.ods",   "D-KA",
    "data/works D-Mbs.ods",  "D-Mbs",
    "data/works D-NATk.ods", "D-NATk",
    "data/works F-Pn.ods",   "F-Pn",
    "data/works GB-Lbm.ods", "GB-Lbm",
    "data/works US-Wc.ods",  "US-Wc",
  ) %>%
  pmap_dfr(load_works)



# Digitized works to be edited --------------------------------------------

dig_works_tbe <-
  digitized_works %>%
  group_by(mh) %>%
  summarise(
    digital_source = str_c(shelfmark, collapse = "; "),
    edited = if (all(edited)) TRUE
             else if (!any(edited)) FALSE
             else stop("Inconsistency detected")
  )

# dig_works_tbe %>% filter(mh == "635")

used_genres <- c(
  "antiphon", "completorium", "gradual", "hymn", "introitus", "motet",
  "offertorium", "psalm", "responsorium", "sacred song", "sequence", "vesper"
)

ignored_notes <- c("fragment", "lost", "incomplete")

mh %>%
  left_join(dig_works_tbe, by = "mh") %>%
  mutate(
    notes = notes %>% replace_na(""),
    manuscript_source = manuscript_source %>% replace_na(""),
    digital_source = digital_source %>% replace_na(""),
    edited = edited %>% replace_na(FALSE) %>% if_else("yes", "")
  ) %>%
  filter(genre %in% used_genres, !notes %in% ignored_notes) %>%
  relocate(digital_source, .after = genre) %>%
  write_ods("output/all_works_to_be_edited.ods")



# Works by library --------------------------------------------------------

manuscript_works <-
  mh %>%
  left_join(dig_works_tbe, by = "mh") %>%
  filter(!is.na(manuscript_source)) %>%
  mutate(manuscript_source = str_split(manuscript_source, ", *")) %>%
  unnest_longer(manuscript_source) %>%
  mutate(
    manuscript_source =
      manuscript_source %>%
      str_match("([^\\s]+)([^\\(]+)?(.+)?") %>%
      as_tibble(.name_repair = "minimal") %>%
      magrittr::set_colnames(c("match", "siglum", "shelfmark", "rism"))
  ) %>%
  unpack(manuscript_source) %>%
  mutate(
    shelfmark = str_trim(shelfmark),
    rism = str_extract(rism, "\\d+")
  )


manuscript_works %>%
  count(siglum)

manuscript_works %>%
  mutate(digitally_available = if_else(is.na(digital_source), "", "yes")) %>%
  select(!c(match, digital_source, edited)) %>%
  split(.$siglum) %>%
  save_table("output/works_by_library")
