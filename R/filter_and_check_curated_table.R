
#' break the curated and edited file into different parts for later use and inspection
#'
#' @param path the path to the edited Insected and Curated file, for example,
#' "InspectAndCurate-Edited.csv"
#' @export
filter_and_check_curated_table <-  function(path) {

  x <- read_csv(path, na = character()) %>%
    filter(STATUS != "d")

  # get the clean ones
  clean <- x %>%
    filter(STATUS == "")


  # check that each Name x Event combination has exactly one occurrence
  wrongos <- clean %>%
    count(Name, Event) %>%
    filter(n != 1)
  if(nrow(wrongos) > 0) {
    tmp <- paste(wrongos$Name, wrongos$Event, sep = ":")
    sw <- paste(tmp, collapse = ", ")
    stop("Problem.  Skater-event combos with more than one occurrence: ", sw)
  }

  # record the resolved problems
  resolved <- clean %>%
    filter(hasPerfectMatch==FALSE)

  resolved_summary <- resolved %>%
    count(Event, report_event)


  # pull out the follow-ups
  follow_ups <- x %>%
    filter(STATUS == "F")

  # return a list
  list(
    clean = clean,
    follow_ups = follow_ups,
    resolved_summary = resolved_summary,
    resolved = resolved,
    unfiltered = x
  )
}
