

#' Full join music_report and start_order
#'
#' This does a full join on skater names and then for each event in the
#' music report that skater is doing, it checks for a perfect match in the
#' starting orders.  If it finds it, it will filter out all other matches
#' for that combination of skater name and music-report event (`Event`).
#' @param MR the tibble of the music report file from EMS
#' @param SO the tibble of the start orders obtained from `scrape_starting_orders()`.
#' @return This returns a tibble of the joined tables.  The second column
#' is `hasPerfectMatch`, the first column is status, which is full of
#' empty strings.  It is intended that this file will be manually curated
#' and appropriate entries placed in the `Status` column.
#' @export
join_report_and_start_order <- function(MR, SO) {
  boing <- full_join(
    MR,
    SO,
    by = join_by(Name == report_name),
    relationship = "many-to-many"
  ) %>%
    select(Name, Event, report_event, event_full, everything()) %>%
    mutate(strdist = map2_int(Event, report_event, stringdist::stringdist, method = "lcs"), .before = Name) %>%
    arrange(Name, Event, strdist) %>%
    group_by(Name, Event) %>%
    mutate(
      hasPerfectMatch = any(strdist == 0),
      .after = strdist
    ) %>%
    filter(is.na(hasPerfectMatch) | !(hasPerfectMatch & strdist > 0)) %>%
    mutate(STATUS = "", .before = strdist)
}
