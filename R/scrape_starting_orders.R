

#' scrape PDFs of starting orders
#'
#' This puts information in a tibble and processes it somewhat, with
#' the hope that we can join it onto the music report at some point.
#' @param Dir path to main directory.  Inside it you should find subfolders
#' which are named like `Friday`, `Saturday`, etc.  For example,
#' `Dir = "~/Downloads/2024 FCC Starting Orders"`.
#'
#' @export
scrape_starting_orders <- function(Dir)
{
  tib <- tibble(
    path = dir(Dir, recursive = TRUE, full.names = TRUE, pattern = ".*\\.pdf$")
  ) %>% mutate(
    day = basename(dirname(path)),
    text_vec = map(path, function(x) read_lines(pdf_text(x))),
    title = map_chr(text_vec, function(x) str_trim(x[1])),
    event_full = map_chr(text_vec, function(x) str_trim(x[2])),
  ) %>%
    extract(event_full, into = c("event_number", "event_name"), regex = "^([0-9]+) (.*)$", remove = FALSE) %>%
    mutate(
      competitors_full = map(text_vec, function(x) {y <- x[-(1:2)]; str_trim(y[str_detect(y, "^ +[0-9]+\\.?")])}),
    ) %>%
    unnest(cols = competitors_full) %>%
    extract(
      competitors_full,
      into = c("competitor_index", "full_name", "club"),
      regex = "^([0-9]+)\\.*\\s+([^,]+),?\\s*(.*)$",
      convert = TRUE,
      remove = FALSE
    ) %>%
    mutate(
      report_event = str_replace(event_name, "\\s*(with Music.*|Grp.*|Group.*|/ *Free.*|/ *Short.*)", ""),
      report_event = str_replace(report_event, "\\s*/\\s*Show.*", ""),
      .after = event_name
    ) %>%
    mutate(
      segment = ifelse(str_detect(event_name, "Short Program"), "Short", "Free"),
      .after = report_event
    ) %>%
    mutate( # try to get what they have in the report: last, first.  (Complicated by
            #  compound names and duets...)
      report_name = map_chr(full_name, function(x) {
        y <- str_replace(x, "\\s*/\\s*.*", "")
        s <- str_split(y, "\\s")[[1]];
        ret <- character(length(s))
        ret[1] <- sprintf("%s,", s[length(s)])
        ret[-1] <- s[-length(s)];
        paste(ret, sep = "", collapse = " ")
      }),
      music_name = str_replace(full_name, "/", "&")  # for duets
    ) %>%
    select(day, competitor_index, report_name, music_name, report_event, event_full, everything())

  tib
}
