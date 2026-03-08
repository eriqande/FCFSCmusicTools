

#' fix common problems in the Music Report file from EMS
#'
#' @param X the tibble obtained by reading the Music Report file.
#' @export
clean_music_report <- function(X) {
  ret <- X %>%
    mutate(
      Name = str_replace(Name, " +,", ",")  %>%   # fix spaces before commas in names
        str_replace(",  +", ", "),          # fix excess spaces after commas in names
    ) %>%
    separate(Name, into = c("last_name", "first_name"), sep = ",", remove = FALSE) %>%
    mutate(
      first_name = str_trim(first_name),
      last_name = str_trim(last_name),
      music_name = str_c(first_name, last_name, sep = " "),
      .after = first_name
    )
  ret
}
