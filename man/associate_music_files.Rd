% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/associate_music_files.R
\name{associate_music_files}
\alias{associate_music_files}
\title{Associate music files with skaters from the clean joined tables}
\usage{
associate_music_files(C, music_events_dir, new_music_dir)
}
\arguments{
\item{C}{the `clean` component of the output list from
filter_and_check_curated_table().  A tibble.  It has to have the columns:
- Fill this
- out later}

\item{music_events_dir}{path to the directory that holds the music from EMS.
The subdirectories of this directory should be the different events, like
"PreliminaryLyricalPop", etc.}

\item{new_music_dir}{path to the directory where the newly ordered and
categorized music should go.  This function does not copy it there, but
it does write the paths.}
}
\description{
This does matching to file names via regular expressions.  The
main challenges were non-standard diacritics (some might need
to be added later, too!) and Pairs and Duets. It then writes
new filenames for the music so they can be placed into the
numbered event directories and placed in start order.
}
