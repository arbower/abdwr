# Description: Pulling minor league data

# Author: Andy B.
# Version:03 May, 2025

# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse", "here", "broom", "janitor", "conflicted")

# Parameters
  # Input Data

  # Output Data

  # Load Data

#===============================================================================

# Minor League Data Collection Functions -------------------------------
library(baseballr)  # For baseball data
library(gt)         # For formatted tables

#' Get Minor League Game Schedule
#'
#' @description
#' Retrieves minor league game schedules for specified dates and league levels.
#' Uses vectorized operations and tidyverse principles for better maintainability.
#'
#' @param date Character string in YYYY-MM-DD format. Defaults to "2022-06-01"
#' @param level_ids Integer vector of league level IDs (e.g., 11 = AAA, 12 = AA)
#'
#' @return Tibble containing game schedule information with standardized column names
#'
#' @examples
#' schedule <- get_minor_league_schedule("2022-06-01", c(11, 12))
#'
#' @export
get_minor_league_schedule <- function(date = "2022-06-01", level_ids = c(11, 12)) {

  # Get games from API
  games <- baseballr::get_game_pks_mlb(date = date, level_ids = level_ids)

  # Transform data into cleaner format
  games |>
    mutate(
      away_record = str_glue("{teams.away.leagueRecord.wins}-{teams.away.leagueRecord.losses}"),
      home_record = str_glue("{teams.home.leagueRecord.wins}-{teams.home.leagueRecord.losses}")
    ) |>
    select(
      game_pk,
      official_date = officialDate,
      status = status.detailedState,
      away_team = teams.away.team.name,
      away_id = teams.away.team.id,
      away_record,
      home_team = teams.home.team.name,
      home_id = teams.home.team.id,
      home_record,
      venue = venue.name
    )
}

#' Safely Retrieve Play-by-Play Data
#'
#' @description
#' Safely retrieves play-by-play data for a single game using error handling.
#' Includes metadata for tracking data collection progress.
#'
#' @param game_pk Integer game ID
#' @param game_info Character string with game information for logging
#'
#' @return Tibble with play-by-play data or NULL if error occurs
#'
#' @examples
#' pbp_data <- safe_get_pbp(673622, "Game 1: Home Team vs Away Team")
#'
#' @keywords internal
safe_get_pbp <- function(game_pk, game_info) {

  result <- quietly(possibly(baseballr::get_pbp_mlb, NULL))(game_pk)

  if (!is.null(result$result) && is.data.frame(result$result)) {
    # Add metadata
    result$result |>
      mutate(game_pk = game_pk)
  } else {
    tibble()
  }
}

#' Get All Game Play-by-Play Data
#'
#' @description
#' Retrieves play-by-play data for multiple games using tidyverse principles.
#' Provides progress tracking and graceful error handling.
#'
#' @param game_list Tibble with game information (must include game_pk)
#' @param complete_only Logical indicating whether to filter only completed games
#' @param progress Whether to show progress information
#'
#' @return Tibble containing play-by-play data for all specified games
#'
#' @examples
#' schedule <- get_minor_league_schedule()
#' pbp_data <- get_all_game_data(schedule, complete_only = TRUE)
#'
#' @export
get_all_game_data <- function(game_list, complete_only = TRUE, progress = TRUE) {

  # Filter games based on completion status
  if (complete_only) {
    game_list <- game_list |>
      filter(status %in% c("Final", "Completed Early"))
  } else {
    game_list <- game_list |>
      filter(status != "Scheduled")
  }

  # Create game information for tracking progress
  games_to_process <- game_list |>
    mutate(
      game_info = str_glue("{home_team} vs. {away_team} (game_pk: {game_pk})")
    )

  # Progress counter
  n_games <- nrow(games_to_process)
  if (progress) message(str_glue("Processing {n_games} games..."))

  # Get PBP data using map
  pbp_data <- games_to_process |>
    mutate(game_number = row_number()) |>
    group_by(game_pk) |>
    nest() |>
    mutate(
      pbp_result = map2(
        game_pk,
        data,
        ~{
          if (progress) {
            current_game <- .y$game_number[1]
            game_info <- .y$game_info[1]
            progress_pct <- round((current_game / n_games) * 100, 1)
            message(str_glue("Game {current_game} of {n_games} ({progress_pct}%): {game_info}"))
          }
          safe_get_pbp(.x, .y$game_info[1])
        }
      )
    ) |>
    select(pbp_result) |>
    unnest(pbp_result)

  return(pbp_data)
}

#' Complete Minor League Data Collection Pipeline
#'
#' @description
#' End-to-end function for collecting minor league data with proper error handling.
#'
#' @param date Character string in YYYY-MM-DD format
#' @param level_ids Integer vector of league level IDs
#' @param complete_only Logical indicating whether to filter only completed games
#'
#' @return Tibble containing all play-by-play data for specified parameters
#'
#' @examples
#' mlb_data <- collect_minor_league_data(
#'   date = "2022-06-01",
#'   level_ids = c(11, 12),
#'   complete_only = TRUE
#' )
#'
#' @export
collect_minor_league_data <- function(date = "2022-06-01",
                                      level_ids = c(11, 12),
                                      complete_only = TRUE) {

  # Get schedule
  schedule <- get_minor_league_schedule(date = date, level_ids = level_ids)

  if (nrow(schedule) == 0) {
    message(str_glue("No games found for date: {date}"))
    return(tibble())
  }

  # Get all game data
  pbp_data <- get_all_game_data(
    game_list = schedule,
    complete_only = complete_only,
    progress = TRUE
  )

  return(pbp_data)
}

# Example Usage -----------------------------------------------------------

# Example workflow showing the improved approach
#'TODO Need to ensure these work and start a program to do this automatically daily
#'TODO Also need to figure out DuckDB with this
#'TODO Want to build a website and display these stats daily
#'TODO Want to do like at work with the brew Rscript execution set up code for production products
#'TODO Git
#'TODO Cloud storage for data etc. (Dropbox, Box) what's the best?

# Option 1: Use the complete pipeline function
minor_league_data <- collect_minor_league_data(
  date = "2024-06-01",
  level_ids = c(11, 12),
  complete_only = TRUE
)

# Option 2: Use individual functions for more control
schedule <- get_minor_league_schedule("2022-06-01", c(11, 12))

# View schedule in formatted table
schedule |>
  gt::gt() |>
  tab_header(
    title = "Minor League Schedule",
    subtitle = str_glue("Date: 2022-06-01")
  )

# Get play-by-play data for completed games only
pbp_data <- get_all_game_data(schedule, complete_only = TRUE)

# Option 3: Use purrr for more complex operations
double_a_data <- schedule |>
  filter(str_detect(away_team, "Springfield|Birmingham")) |>
  get_all_game_data(complete_only = TRUE)






