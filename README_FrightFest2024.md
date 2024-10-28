Processing Music For FrightFest 2024
================

For FrightFest we use EntryEeze, which has a different management system
for Music.

Let’s see if we can use the same tools for that (and probably write some
new functions.)

The setup for what we are doing now is:

1.  Download all the music as *untagged* and then put it in the
    gitignored directory. It looks like this:

    ``` sh
    gitignored_data/FrightFest2024/
    └── Music
     └── Sunday
        ├── AdultFreeSkate60_AdultPreBronze_Garg_Brenda.mp3
        ├── AdultFreeSkate60_AdultPreBronze_Hill_Barbra.mp3
        ├── CompeteUSAProgramwMusic_AdultHighBeginner_Koontz_Nelia.mp3
        ├── CompeteUSAProgramwMusic_AdultHighBeginner_Lundin_Ruthie.mp3
        ├── CompeteUSAProgramwMusic_Aspire1_Carrick_Grace.mp3
        ├── CompeteUSAProgramwMusic_Aspire1_Davis_Paige.mp3
     ...
    ```

    Note that I put it all in the “Sunday” folder.

2.  Download all of Shedrin’s start order PDFs. These are formatted
    slightly differently than at FoCo Classic last year, but I hope that
    they will still scrape out on the same lines. Here is what the files
    look like:

    ``` sh
    gitignored_data/FrightFest2024/StartingOrders/
    └── Sunday
     ├── 105.pdf
     ├── 106.pdf
     ├── 107.pdf
     ├── 108.pdf
     ...
    ```

## Step 1. Scrape the starting orders

``` r
library(FCFSCmusicTools)
```

    ## Loading required package: fuzzyjoin

    ## Loading required package: pdftools

    ## Using poppler version 23.04.0

    ## Loading required package: readxl

    ## Loading required package: tidyverse

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(tidyverse)


# scrape starting orders out of the files Sheldrin sent
start_ords <- scrape_starting_orders("gitignored_data/FrightFest2024/StartingOrders")

start_ords
```

    ## # A tibble: 407 × 15
    ##    day    competitor_index report_name  music_name report_event event_full path 
    ##    <chr>             <int> <chr>        <chr>      <chr>        <chr>      <chr>
    ##  1 Sunday                1 Synchronici… Denver Sy… Open Master… 105 Open … giti…
    ##  2 Sunday                2 Crystals, F… Fort Coll… Open Master… 105 Open … giti…
    ##  3 Sunday                1 Icicles, Fo… Fort Coll… Aspire 1 Sy… 106 Aspir… giti…
    ##  4 Sunday                1 Fireflies, … Fire Crys… Aspire 2 Sy… 107 Aspir… giti…
    ##  5 Sunday                1 Crystals, F… Fire Crys… Aspire 4 Sy… 108 Aspir… giti…
    ##  6 Sunday                2 Synchronici… Denver Sy… Aspire 4 Sy… 108 Aspir… giti…
    ##  7 Sunday                1 Hill, Barbra Barbra Hi… Adult Pre-B… 109 Adult… giti…
    ##  8 Sunday                2 Garg, Brenda Brenda Ga… Adult Pre-B… 109 Adult… giti…
    ##  9 Sunday                1 Koontz, Nel… Nelia Koo… Adult High … 110 Adult… giti…
    ## 10 Sunday                2 Lundin, Rut… Ruthie Lu… Adult High … 110 Adult… giti…
    ## # ℹ 397 more rows
    ## # ℹ 8 more variables: text_vec <list>, title <chr>, event_number <chr>,
    ## #   event_name <chr>, segment <chr>, competitors_full <chr>, full_name <chr>,
    ## #   club <chr>

OK, that parses out fine, it appears.

## Step 2: Get a tibble of music and paths

Because the event names are pretty munged between the music file names
and the start orders, and because there are only 139 pieces of music, or
so, I think I will just associate skater names (multiply, if there are
multiple events) and then make a spreadsheet and hand-check the correct
ones.

``` r
music <- tibble(
    path = dir(path = "gitignored_data/FrightFest2024/Music/Sunday", full.names = TRUE)
  ) %>%
  mutate(
    basename = basename(path)
  ) %>%
  separate(basename, into = c("event", "level", "last", "first", "ext"))
```

    ## Warning: Expected 5 pieces. Missing pieces filled with `NA` in 12 rows [67, 68, 102,
    ## 103, 104, 121, 133, 134, 135, 136, 137, 138].

Now, the music title have stripped all non-letters out of the things, so
we will do the same with the start orders, make a “squash_names” field
that we can join on.

``` r
so2 <- start_ords %>% 
  mutate(squash_name = str_replace_all(full_name, "[^a-zA-Z]", ""))

mus2 <- music %>%
  mutate(
    squash_name = case_when(
      is.na(ext) ~ str_replace_all(last, "[^a-zA-Z]", ""),
      TRUE ~ str_c(first, last) %>% str_replace_all("[^a-zA-Z]", ""),
    )
  )
```

Now, let’s join those and get read to hand-check things.

``` r
ready_for_eyes <- so2 %>%
  left_join(mus2, by = join_by(squash_name)) %>%
  arrange(squash_name, event_full) %>%
  group_by(squash_name) %>%
  mutate(
    music_event = event,
    music_level = level,
    keep_it = ifelse(n()==1, "x", ""),
    .after = event_full
  )
```

    ## Warning in left_join(., mus2, by = join_by(squash_name)): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 1 of `x` matches multiple rows in `y`.
    ## ℹ Row 135 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
dir.create("gitignored_data/FrightFest2024/hand-check", recursive = TRUE, showWarnings = FALSE)
write_csv(ready_for_eyes, file = "gitignored_data/FrightFest2024/hand-check/hand-check-RAW.csv")
```

Then, I copy hand-check-RAW to hand-check-CHECKED.csv by hand, and then
we can read that back in and do some checks on it.

``` r
hand_checked <- read_csv("gitignored_data/FrightFest2024/hand-check/hand-check-CHECKED.csv") %>%
  filter(keep_it == "x")
```

    ## Rows: 222 Columns: 25
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (22): day, report_name, music_name, report_event, event_full, music_even...
    ## dbl  (2): competitor_index, event_number
    ## lgl  (1): text_vec
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# how many keep_it events?  (139)
nrow(hand_checked)
```

    ## [1] 139

``` r
# how many music titles?  (138)
nrow(music)
```

    ## [1] 138

``` r
# We are missing Olivia's duet/trio music

# check to make sure that hand_checked music includes all that we have seen and no duplicates
setdiff(music$path, hand_checked$path.y)
```

    ## character(0)

``` r
# and what is the music we are missing there?
setdiff(hand_checked$path.y, music$path)
```

    ## [1] NA

``` r
hand_checked %>%
  count(path.y) %>%
  count(n)
```

    ## Storing counts in `nn`, as `n` already present in input
    ## ℹ Use `name = "new_name"` to pick a new name.

    ## # A tibble: 1 × 2
    ##       n    nn
    ##   <int> <int>
    ## 1     1   139

``` r
# yep! this is olivia's
hand_checked %>%
  filter(is.na(path.y))
```

    ## # A tibble: 1 × 25
    ##   day    competitor_index report_name     music_name     report_event event_full
    ##   <chr>             <dbl> <chr>           <chr>          <chr>        <chr>     
    ## 1 Sunday                1 Philosophy, New New Philosophy Intermediat… 119 Inter…
    ## # ℹ 19 more variables: music_event <chr>, music_level <chr>, keep_it <chr>,
    ## #   path.x <chr>, text_vec <lgl>, title <chr>, event_number <dbl>,
    ## #   event_name <chr>, segment <chr>, competitors_full <chr>, full_name <chr>,
    ## #   club <chr>, squash_name <chr>, path.y <chr>, event <chr>, level <chr>,
    ## #   last <chr>, first <chr>, ext <chr>

Cool. So, now all we have to do is choose a naming convention and copy
the music over. I am going to leave the name of the music file the same,
but I will just add the start order to the front of it. That should be a
good check that we have associated the music correctly with each event.

``` r
ready_to_copy <- hand_checked %>%
  filter(!is.na(path.y)) %>%
  mutate(
    new_music_path = file.path(
      "~/Desktop/StartOrderMusic---2024FrightFest", 
      day,
      str_replace_all(event_full, "/", "--"),   # remove forward slashes in the event names
      sprintf("%02d---%s", competitor_index, basename(path.y))
    ),
    orig_music_path = path.y,
    .before = day
  )
```

Then, we might want to remove the existing directory

``` sh
rm -r ~/Desktop/StartOrderMusic---2024FrightFest
```

Then, run it:

``` r
catch_it <- copy_music_files(ready_to_copy)
```

Let’s check the MD5’s:

``` r
catch_it %>%
  count(md5_orig == md5_new)
```

    ## # A tibble: 1 × 2
    ##   `md5_orig == md5_new`     n
    ##   <lgl>                 <int>
    ## 1 TRUE                    138

That is good! We have 138 pieces of music correctly copied!
