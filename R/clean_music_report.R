

#' fix common problems in the Music Report file from EMS
#'
#' @param X the tibble obtained by reading the Music Report file.
#' @export
clean_music_report <- function(X) {
  ret <- X %>%
    mutate(
      Name = str_replace(Name, " +,", ",") # fix spaces before commas in names
    )
  ret
}
