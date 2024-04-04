


C <- filtered_events_list$clean
music_events_dir <- "gitignored_data/Music/2024FortCollinsClassic"
new_music_dir <- "~/Desktop/StartOrderMusic---2024FortCollinsClassic"


# here is a function to get a regex for the music file.  It takes  the
# Camel Case event name (cc) and also the music_name (mn) and also
# the USFSA ID number (id).  If cc has
# "Pairs" or "Duet" in it we break up the names on the & and look for
# either of them, plus the last four of the ID. Otherwise we do it
# for just the single skater
form_regex <- function(cc, mn, id) {
  case_when(
    str_detect(cc, "Pair|Duet") ~ {
      alt_names <- str_split(mn, "&") %>%
        map(str_trim) %>%
        map_chr(function(z) paste(z, collapse = "|")) %>%
        str_replace_all(., "[á-ūÀ-ū]+", ".*")

      # now if there was only one name, remove the parentheses
      alt2 <- str_c(alt_names, ".*")

      str_c(
        alt2,
        " *",  # some genius at EMS put spaces after a name before the ID in a few of the file names
        str_sub(id, nchar(id) - 4 + 1, nchar(id)),
        "_",
        ".*\\.mp3$"
      )
    },
    TRUE ~ str_c(
      str_replace(mn, " +", " +") %>% str_replace_all(., "[á-ūÀ-ū]+", ".*"), # Yaretzi's last name has a diacritic that is non-standard. It appears to be two separate characters.Which is why I replace all diacritics with matches any number of characters
      " *",  # some genius at EMS put spaces after a name before the ID in a few of the file names
      str_sub(id, nchar(id) - 4 + 1, nchar(id)),
      "_",
      ".*\\.mp3$"
    )
  )
}


# first we make a column that has the Event from the Music report
# written in CamelCase, because these are the names for the directories
# that the mp3s are in when you download them from EMS. We just take all the
# non a-zA-Z0-9 out of the event names.

# and then we also get a basename pattern to search for the file.  This is the
# skater name plus the last four digits of their USFSA ID, but we do it as a
# regex because things are so unstandardized!
tmp <- C %>%
  mutate(
    EventCamel = str_replace_all(Event, "[^A-Za-z0-9]+", ""),
    Music_regex = form_regex(EventCamel, music_name, `Mb #`),
    ExpectedMusicPath = file.path(
      music_events_dir,
      EventCamel
    ),
    .before = STATUS
  ) %>%
  mutate(
    orig_music_path = map2_chr(
      .x = ExpectedMusicPath,
      .y = Music_regex,
      .f = function(x, y) {
        ret <- dir(path = x, pattern = y, full.names = TRUE)
        if(length(ret) == 1) return(ret)
        else return(length(ret))
      }
    ),
    .before = EventCamel
  ) %>%
  mutate(
    orig_music_basename = basename(orig_music_path),
    new_music_path = file.path(
      new_music_dir,
      str_replace_all(event_full, "/", "-"),
      sprintf("%02d---%s", as.integer(competitor_index), orig_music_basename)
    ),
    .after = orig_music_path
  ) %>%
  arrange(event_full, competitor_index)


