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



# Analysis ----------------------------------------------------------------

mh <-
  read_ods("data/mh.ods") %>%
  as_tibble()

ms_sources <-
  mh %>%
  filter(!is.na(source)) %>%
  mutate(source = str_split(source, ", *")) %>%
  unnest_longer(source) %>%
  mutate(
    source =
      source %>%
      str_match("([^\\s]+)([^\\(]+)?(.+)?") %>%
      as_tibble(.name_repair = "minimal") %>%
      magrittr::set_colnames(c("match", "siglum", "shelfmark", "rism"))
  ) %>%
  unpack(source) %>%
  mutate(
    shelfmark = str_trim(shelfmark),
    rism = str_extract(rism, "\\d+")
  ) %>%
  {.}


ms_sources %>%
  count(siglum)

ms_sources %>%
  select(!match) %>%
  split(.$siglum) %>%
  save_table("output/works_by_library")


