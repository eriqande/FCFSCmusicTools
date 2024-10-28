
#' Copies music files from orig path to new path
#'
#' This does not remove all the directories in the destination paths, because
#' you can do that by hand if you need to.  I don't want you to accidentally
#' remove a bunch of critical files...
#' @param Tbl a tibble with at least the columns `orig_music_path` and
#' `new_music_path`.
#' @export
copy_music_files <- function(Tbl) {

  new_files <- Tbl$new_music_path

  # now, create all the necessary subdirectories
  sub_dirs <- unique(dirname(new_files))
  dump <- lapply(sub_dirs, function(x) dir.create(x, recursive = TRUE, showWarnings = FALSE))

  # now, copy all the files
  Tbl2 <- Tbl %>%
    mutate(
      copy_disposition = file.copy(from = Tbl$orig_music_path, to = Tbl$new_music_path),
      .before = orig_music_path
    )

  # now, do the checksums:
  Tbl2 %>%
    mutate(
      md5_orig = tools::md5sum(orig_music_path),
      md5_new = tools::md5sum(new_music_path),
      .before = orig_music_path
    )




}
