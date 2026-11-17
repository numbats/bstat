######## Course information ########

library(dplyr)
library(rmarkdown)
library(rlang)

# Site details
acronym <- "bstat"
start_semester <- as.Date("2026-11-24")
mid_semester_break_start <- as.Date("2026-12-21")
mid_semester_break_end <- as.Date("2026-12-28")
written_assignment_due <- as.Date("2027-01-04")
written_assignment_due_time <- "11:55 pm"

# Dates and class times follow the revised ETX1100 NOV12 2026 unit outline.
# Week 4, Week 5 and Week 7 topics reflect subsequent user-directed revisions.
schedule <- tibble::tibble(
  Week = c(1L, 2L, 3L, 4L, NA_integer_, 5L, 6L, 7L, 8L),
  Date = as.Date(c(
    "2026-11-24", "2026-12-01", "2026-12-08", "2026-12-15",
    "2026-12-21", "2027-01-05", "2027-01-12", "2027-01-19",
    "2027-01-29"
  )),
  Class_Time = c(rep("13:00–16:00", 4), NA_character_,
                 rep("13:00–16:00", 3), "15:00–18:00"),
  Topic = c(
    "Describing and visualising data",
    "Probability and distributions",
    "Sampling and estimation",
    "Testing business claims",
    "Mid-semester break",
    "Chi-square, correlation and simple linear regression",
    "Multiple regression and model evaluation",
    "Introduction to time series",
    "Integrated business case, communication and revision"
  ),
  Classroom_Activity = c(
    "Exercise 1 — end of class", "Exercise 2 — end of class",
    "Exercise 3 — end of class", "Exercise 4 — end of class", NA,
    "Exercise 5 — end of class", "Exercise 6 — end of class",
    "Exercise 7 — end of class", "Exercise 8 — end of class"
  )
)

schedule$Quiz_Due <- as.Date(NA)
quiz_rows <- schedule$Week %in% 1L:7L
schedule$Quiz_Due[quiz_rows] <- schedule$Date[quiz_rows] +
  ((8L - as.POSIXlt(schedule$Date[quiz_rows])$wday) %% 7L)
schedule$Assessment <- NA_character_
schedule$Assessment[quiz_rows] <- sprintf(
  "Quiz %d — due %s",
  schedule$Week[quiz_rows],
  format(schedule$Quiz_Due[quiz_rows], "%a %d %b %Y")
)
schedule$Assessment[schedule$Week %in% 5L] <- paste0(
  "Written assignment — due ",
  format(written_assignment_due, "%a %d %b %Y"),
  " at ", written_assignment_due_time,
  "; ", schedule$Assessment[schedule$Week %in% 5L]
)

# Fail early if the official eight-week structure is accidentally broken.
stopifnot(
  sum(!is.na(schedule$Week)) == 8L,
  schedule$Date[match(1L, schedule$Week)] == start_semester,
  schedule$Date[match(8L, schedule$Week)] == as.Date("2027-01-29"),
  sum(!is.na(schedule$Quiz_Due)) == 7L,
  is.na(schedule$Quiz_Due[schedule$Week == 8L]),
  all(as.POSIXlt(schedule$Quiz_Due[quiz_rows])$wday == 1L),
  all(schedule$Quiz_Due[quiz_rows] > schedule$Date[quiz_rows]),
  written_assignment_due == schedule$Date[schedule$Week %in% 5L] - 1L
)

# Submission details live in assignments.csv; an unknown Moodle ID stays
# unknown rather than becoming a link to an obsolete submission page.
assignments <- readr::read_csv(
  here::here("assignments.csv"),
  col_types = readr::cols(
    Assignment = readr::col_character(),
    Due = readr::col_date(),
    Due_Time = readr::col_character(),
    Moodle = readr::col_character(),
    File = readr::col_character(),
    Marks = readr::col_double()
  ),
  show_col_types = FALSE
) |>
  mutate(
    Due = as.Date(Due),
    Moodle = if_else(
      is.na(Moodle) | as.character(Moodle) == "",
      NA_character_,
      paste0("https://learning.monash.edu/mod/assign/view.php?id=", Moodle)
    ),
    File = paste0("assignments/", File)
  )
stopifnot(
  NROW(assignments) == 1L,
  assignments$Due[assignments$Assignment == "Written Assignment"] == written_assignment_due,
  assignments$Due_Time[assignments$Assignment == "Written Assignment"] == written_assignment_due_time
)

show_assignments <- function(week) {
  upcoming <- schedule |>
    filter(!is.na(Week), Week >= week, Week <= week + 2L, !is.na(Assessment)) |>
    select(Week, Assessment)

  if (NROW(upcoming) > 0L) {
    cat("\n\n## Upcoming assessments\n\n")
    for (i in seq_len(NROW(upcoming))) {
      cat("- **Week ", upcoming$Week[i], ":** ",
          upcoming$Assessment[i], "\n", sep = "")
    }
  }
}

submit <- function(schedule = NULL, assignment) {
  ass <- assignments |>
    filter(Assignment == assignment)

  if (NROW(ass) != 1L) {
    cat("<p><strong>Submission details are not yet available.</strong></p>")
    return(invisible(NULL))
  }

  due <- paste(
    format(ass$Due, "%e %B %Y") |> stringr::str_trim(),
    "at", ass$Due_Time
  )
  if (is.na(ass$Moodle)) {
    cat("<p><strong>Due: ", due,
        ".</strong> Check Moodle for the submission location.</p>", sep = "")
    return(invisible(NULL))
  }
  button <- paste0(
    "<br><br><hr><b>Due: ", due, "</b><br>",
    "<a href='", ass$Moodle, "' class='badge badge-large badge-blue'>",
    "<font size='+2'>&nbsp;&nbsp;<b>Submit</b>&nbsp;&nbsp;</font><br></a>"
  )
  cat(button)
}

show_slides <- function(week) {
  qmd_file <- here::here(paste0("week", week, "/slides.qmd"))
  if (fs::file_exists(qmd_file)) {
    buttons <- paste0(
      "<div class='slide-actions'>",
      "<a href='slides.pdf' class='slide-download-pdf' download>",
      "Download PDF slides</a>",
      "<a href='slides.html' class='slide-open-html'>",
      "Open interactive slides</a>",
      "</div>"
    )
    cat(buttons)
  }
}
