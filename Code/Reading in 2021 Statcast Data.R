#install.packages(data.table)
library(tidyverse)
library(data.table)

#### Statcast Search Function from Baseballr (not working for some reason; copied code directly from github) ####
# note: collapse all this code with the dropdown in line 5
# then highlight the comment (with the dark blue portion and the arrows) and run
statcast_search <- function(start_date = Sys.Date() - 1, end_date = Sys.Date(),
                            playerid = NULL,
                            player_type = "batter", ...) {
  # Check for other user errors.
  if (start_date <= "2015-03-01") { # March 1, 2015 was the first date of Spring Training.
    message("Some metrics such as Exit Velocity and Batted Ball Events have only been compiled since 2015.")
  }
  if (start_date < "2008-03-25") { # March 25, 2008 was the first date of the 2008 season.
    stop("The data are limited to the 2008 MLB season and after.")
    return(NULL)
  }
  if (start_date == Sys.Date()) {
    message("The data are collected daily at 3 a.m. Some of today's games may not be included.")
  }
  if (start_date > as.Date(end_date)) {
    stop("The start date is later than the end date.")
    return(NULL)
  }
  
  playerid_var <- ifelse(player_type == "pitcher",
                         "pitchers_lookup%5B%5D", "batters_lookup%5B%5D")
  
  vars <- tibble::tribble(
    ~var, ~value,
    "all", "true",
    "hfPT", "",
    "hfAB", "",
    "hfBBT", "",
    "hfPR", "",
    "hfZ", "",
    "stadium", "",
    "hfBBL", "",
    "hfNewZones", "",
    "hfGT", "R%7CPO%7CS%7C&hfC",
    "hfSea", paste0(lubridate::year(start_date), "%7C"),
    "hfSit", "",
    "hfOuts", "",
    "opponent", "",
    "pitcher_throws", "",
    "batter_stands", "",
    "hfSA", "",
    "player_type", player_type,
    "hfInfield", "",
    "team", "",
    "position", "",
    "hfOutfield", "",
    "hfRO", "",
    "home_road", "",
    playerid_var, ifelse(is.null(playerid), "", as.character(playerid)),
    "game_date_gt", as.character(start_date),
    "game_date_lt", as.character(end_date),
    "hfFlag", "",
    "hfPull", "",
    "metric_1", "",
    "hfInn", "",
    "min_pitches", "0",
    "min_results", "0",
    "group_by", "name",
    "sort_col", "pitches",
    "player_event_sort", "h_launch_speed",
    "sort_order", "desc",
    "min_abs", "0",
    "type", "details") %>%
    dplyr::mutate(pairs = paste0(.data$var, "=", .data$value))
  
  if (is.null(playerid)) {
    # message("No playerid specified. Collecting data for all batters/pitchers.")
    vars <- vars %>% 
      dplyr::filter(!grepl("lookup", .data$var))
  }
  
  url_vars <- paste0(vars$pairs, collapse = "&")
  url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?", url_vars)
  # message(url)
  
  # Do a try/catch to show errors that the user may encounter while downloading.
  tryCatch(
    { 
      suppressMessages(
        suppressWarnings(
          payload <- csv_from_url(url, encoding ="UTF-8")
        )
      )
    },
    error = function(cond) {
      message(cond)
      stop("No payload acquired")
    },
    # this will never run??
    warning = function(cond) {
      message(cond)
    }
  )
  # returns 0 rows on failure but > 1 columns
  if (nrow(payload) > 1) {
    
    names(payload) <- c("pitch_type", "game_date", "release_speed", "release_pos_x", 
                        "release_pos_z", "player_name", "batter", "pitcher", "events", 
                        "description", "spin_dir", "spin_rate_deprecated", "break_angle_deprecated", 
                        "break_length_deprecated", "zone", "des", "game_type", "stand", 
                        "p_throws", "home_team", "away_team", "type", "hit_location", 
                        "bb_type", "balls", "strikes", "game_year", "pfx_x", "pfx_z", 
                        "plate_x", "plate_z", "on_3b", "on_2b", "on_1b", "outs_when_up", 
                        "inning", "inning_topbot", "hc_x", "hc_y", "tfs_deprecated", 
                        "tfs_zulu_deprecated", "umpire", "sv_id", 
                        "vx0", "vy0", "vz0", "ax", "ay", "az", "sz_top", "sz_bot", "hit_distance_sc", 
                        "launch_speed", "launch_angle", "effective_speed", "release_spin_rate", 
                        "release_extension", "game_pk", "fielder_2", 
                        "fielder_3", "fielder_4", "fielder_5", "fielder_6", "fielder_7", 
                        "fielder_8", "fielder_9", "release_pos_y", "estimated_ba_using_speedangle", 
                        "estimated_woba_using_speedangle", "woba_value", "woba_denom", 
                        "babip_value", "iso_value", "launch_speed_angle", "at_bat_number", 
                        "pitch_number", "pitch_name", "home_score", "away_score", "bat_score", 
                        "fld_score", "post_away_score", "post_home_score", "post_bat_score", 
                        "post_fld_score", "if_fielding_alignment", "of_fielding_alignment", 
                        "spin_axis", "delta_home_win_exp", "delta_run_exp","bat_speed", "swing_length", 
                        "estimated_slg_using_speedangle", 
                        "delta_pitcher_run_exp", "hyper_speed", "home_score_diff", "bat_score_diff", 
                        "home_win_exp", "bat_win_exp", "age_pit_legacy", "age_bat_legacy", 
                        "age_pit", "age_bat", "n_thruorder_pitcher", "n_priorpa_thisgame_player_at_bat", 
                        "pitcher_days_since_prev_game", "batter_days_since_prev_game", 
                        "pitcher_days_until_next_game", "batter_days_until_next_game", 
                        "api_break_z_with_gravity", "api_break_x_arm", "api_break_x_batter_in", 
                        "arm_angle")
    
    payload <- process_statcast_payload(payload) %>%
      make_baseballr_data("MLB Baseball Savant Statcast Search data from baseballsavant.mlb.com",Sys.time())
    return(payload)
  } else {
    warning("No valid data found")
    
    names(payload) <- c("pitch_type", "game_date", "release_speed", "release_pos_x", 
                        "release_pos_z", "player_name", "batter", "pitcher", "events", 
                        "description", "spin_dir", "spin_rate_deprecated", "break_angle_deprecated", 
                        "break_length_deprecated", "zone", "des", "game_type", "stand", 
                        "p_throws", "home_team", "away_team", "type", "hit_location", 
                        "bb_type", "balls", "strikes", "game_year", "pfx_x", "pfx_z", 
                        "plate_x", "plate_z", "on_3b", "on_2b", "on_1b", "outs_when_up", 
                        "inning", "inning_topbot", "hc_x", "hc_y", "tfs_deprecated", 
                        "tfs_zulu_deprecated", "umpire", "sv_id", 
                        "vx0", "vy0", "vz0", "ax", "ay", "az", "sz_top", "sz_bot", "hit_distance_sc", 
                        "launch_speed", "launch_angle", "effective_speed", "release_spin_rate", 
                        "release_extension", "game_pk", "fielder_2", 
                        "fielder_3", "fielder_4", "fielder_5", "fielder_6", "fielder_7", 
                        "fielder_8", "fielder_9", "release_pos_y", "estimated_ba_using_speedangle", 
                        "estimated_woba_using_speedangle", "woba_value", "woba_denom", 
                        "babip_value", "iso_value", "launch_speed_angle", "at_bat_number", 
                        "pitch_number", "pitch_name", "home_score", "away_score", "bat_score", 
                        "fld_score", "post_away_score", "post_home_score", "post_bat_score", 
                        "post_fld_score", "if_fielding_alignment", "of_fielding_alignment", 
                        "spin_axis", "delta_home_win_exp", "delta_run_exp","bat_speed", "swing_length", 
                        "estimated_slg_using_speedangle", 
                        "delta_pitcher_run_exp", "hyper_speed", "home_score_diff", "bat_score_diff", 
                        "home_win_exp", "bat_win_exp", "age_pit_legacy", "age_bat_legacy", 
                        "age_pit", "age_bat", "n_thruorder_pitcher", "n_priorpa_thisgame_player_at_bat", 
                        "pitcher_days_since_prev_game", "batter_days_since_prev_game", 
                        "pitcher_days_until_next_game", "batter_days_until_next_game", 
                        "api_break_z_with_gravity", "api_break_x_arm", "api_break_x_batter_in", 
                        "arm_angle")
    
    payload <- payload %>%
      make_baseballr_data("MLB Baseball Savant Statcast Search data from baseballsavant.mlb.com",Sys.time())
    return(payload)
  }
}

statcast_search.default <- function(start_date = Sys.Date() - 1, end_date = Sys.Date(),
                                    playerid = NULL, player_type = "batter", ...) {
  # Check to make sure args are in the correct format.
  # if(!is.character(start_date) | !is.character(end_date)) {
  #   warning("Please wrap your dates in quotations in 'yyyy-mm-dd' format.")
  #   return(NULL)
  # }
  message(paste0(start_date, " is not a date. Attempting to coerce..."))
  start_Date <- as.Date(start_date)
  
  tryCatch(
    {
      end_Date <- as.Date(end_date)
    },
    warning = function(cond) {
      message(paste0(end_date, " was not coercible into a date. Using today."))
      end_Date <- Sys.Date()
      message("Original warning message:")
      message(cond)
    }
  )
  
  statcast_search(start_Date, end_Date,
                  playerid, player_type, ...)
  
}

#' 
#'   |col_name                        |types     |
#'   |:-------------------------------|:---------|
#'   |pitch_type                      |character |
#'   |game_date                       |Date      |
#'   |release_speed                   |numeric   |
#'   |release_pos_x                   |numeric   |
#'   |release_pos_z                   |numeric   |
#'   |player_name                     |character |
#'   |batter                          |numeric   |
#'   |pitcher                         |numeric   |
#'   |events                          |character |
#'   |description                     |character |
#'   |spin_dir                        |logical   |
#'   |spin_rate_deprecated            |logical   |
#'   |break_angle_deprecated          |logical   |
#'   |break_length_deprecated         |logical   |
#'   |zone                            |numeric   |
#'   |des                             |character |
#'   |game_type                       |character |
#'   |stand                           |character |
#'   |p_throws                        |character |
#'   |home_team                       |character |
#'   |away_team                       |character |
#'   |type                            |character |
#'   |hit_location                    |integer   |
#'   |bb_type                         |character |
#'   |balls                           |integer   |
#'   |strikes                         |integer   |
#'   |game_year                       |integer   |
#'   |pfx_x                           |numeric   |
#'   |pfx_z                           |numeric   |
#'   |plate_x                         |numeric   |
#'   |plate_z                         |numeric   |
#'   |on_3b                           |numeric   |
#'   |on_2b                           |numeric   |
#'   |on_1b                           |numeric   |
#'   |outs_when_up                    |integer   |
#'   |inning                          |numeric   |
#'   |inning_topbot                   |character |
#'   |hc_x                            |numeric   |
#'   |hc_y                            |numeric   |
#'   |tfs_deprecated                  |logical   |
#'   |tfs_zulu_deprecated             |logical   |
#'   |fielder_2                       |numeric   |
#'   |umpire                          |logical   |
#'   |sv_id                           |character |
#'   |vx0                             |numeric   |
#'   |vy0                             |numeric   |
#'   |vz0                             |numeric   |
#'   |ax                              |numeric   |
#'   |ay                              |numeric   |
#'   |az                              |numeric   |
#'   |sz_top                          |numeric   |
#'   |sz_bot                          |numeric   |
#'   |hit_distance_sc                 |numeric   |
#'   |launch_speed                    |numeric   |
#'   |launch_angle                    |numeric   |
#'   |effective_speed                 |numeric   |
#'   |release_spin_rate               |numeric   |
#'   |release_extension               |numeric   |
#'   |game_pk                         |numeric   |
#'   |pitcher_1                       |numeric   |
#'   |fielder_2_1                     |numeric   |
#'   |fielder_3                       |numeric   |
#'   |fielder_4                       |numeric   |
#'   |fielder_5                       |numeric   |
#'   |fielder_6                       |numeric   |
#'   |fielder_7                       |numeric   |
#'   |fielder_8                       |numeric   |
#'   |fielder_9                       |numeric   |
#'   |release_pos_y                   |numeric   |
#'   |estimated_ba_using_speedangle   |numeric   |
#'   |estimated_woba_using_speedangle |numeric   |
#'   |woba_value                      |numeric   |
#'   |woba_denom                      |integer   |
#'   |babip_value                     |integer   |
#'   |iso_value                       |integer   |
#'   |launch_speed_angle              |integer   |
#'   |at_bat_number                   |numeric   |
#'   |pitch_number                    |numeric   |
#'   |pitch_name                      |character |
#'   |home_score                      |numeric   |
#'   |away_score                      |numeric   |
#'   |bat_score                       |numeric   |
#'   |fld_score                       |numeric   |
#'   |post_away_score                 |numeric   |
#'   |post_home_score                 |numeric   |
#'   |post_bat_score                  |numeric   |
#'   |post_fld_score                  |numeric   |
#'   |if_fielding_alignment           |character |
#'   |of_fielding_alignment           |character |
#'   |spin_axis                       |numeric   |
#'   |delta_home_win_exp              |numeric   |
#'   |delta_run_exp                   |numeric   |
#'   |bat_speed                       |numeric   |
#'   |swing_length                    |numeric   |   
#'   
#' @export
#' @examples
#' \donttest{
#'   try({
#'     correa <- statcast_search_batters(start_date = "2016-04-06",
#'       end_date = "2016-04-15", batterid = 621043)
#'     daily <- statcast_search_batters(start_date = "2016-04-06",
#'       end_date = "2016-04-06", batterid = NULL)
#'   })
#' }

statcast_search_batters <- function(start_date, end_date, batterid = NULL, ...) {
  statcast_search(start_date, end_date, playerid = batterid,
                  player_type = "batter", ...)
}

statcast_search_pitchers <- function(start_date, end_date, pitcherid = NULL, ...) {
  statcast_search(start_date, end_date, playerid = pitcherid,
                  player_type = "pitcher", ...)
}

scrape_statcast_savant <- statcast_search

scrape_statcast_savant.Date <- statcast_search


scrape_statcast_savant.default <- statcast_search.default

scrape_statcast_savant_batter <- statcast_search_batters

scrape_statcast_savant_batter_all <- statcast_search_batters


scrape_statcast_savant_pitcher <- statcast_search_pitchers

scrape_statcast_savant_pitcher_all <- statcast_search_pitchers

.datatable.aware <- TRUE

request_with_proxy <- function(url, ...){
  dots <- rlang::dots_list(..., .named = TRUE)
  proxy <- dots$proxy
  headers <- dots$headers
  httr::RETRY("GET", url = {{url}}, ..., headers, httr::timeout(15))
}

progressively <- function(f, p = NULL){
  if (!is.null(p) && !inherits(p, "progressor")) stop("`p` must be a progressor function!")
  if (is.null(p)) p <- function(...) NULL
  force(f)
  
  function(...){
    on.exit(p("loading..."))
    f(...)
  }
  
}

csv_from_url <- function(...){
  data.table::fread(...)
}

rds_from_url <- function(url) {
  con <- url(url)
  on.exit(close(con))
  load <- try(readRDS(con), silent = TRUE)
  
  if (inherits(load, "try-error")) {
    warning(paste0("Failed to readRDS from <", url, ">"), call. = FALSE)
    return(data.table::data.table())
  }
  
  data.table::setDT(load)
  return(load)
}


# check if a package is installed
is_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)
# custom mode function from https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode/8189441
custom_mode <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
# The function `message_completed` to create the green "...completed" message
# only exists to hide the option `in_builder` in dots
message_completed <- function(x, in_builder = FALSE) {
  if (isFALSE(in_builder)) {
    str <- paste0(my_time(), " | ", x)
    cli::cli_alert_success("{{.field {str}}}")
  } else if (in_builder) {
    cli::cli_alert_success("{my_time()} | {x}")
  }
}

user_message <- function(x, type) {
  if (type == "done") {
    cli::cli_alert_success("{my_time()} | {x}")
  } else if (type == "todo") {
    cli::cli_ul("{my_time()} | {x}")
  } else if (type == "info") {
    cli::cli_alert_info("{my_time()} | {x}")
  } else if (type == "oops") {
    cli::cli_alert_danger("{my_time()} | {x}")
  }
}

my_time <- function() strftime(Sys.time(), format = "%H:%M:%S")

rule_header <- function(x) {
  rlang::inform(
    cli::rule(
      left = ifelse(is_installed("crayon"), crayon::bold(x), glue::glue("\033[1m{x}\033[22m")),
      right = paste0("baseballr version ", utils::packageVersion("baseballr")),
      width = getOption("width")
    )
  )
}

rule_footer <- function(x) {
  rlang::inform(
    cli::rule(
      left = ifelse(is_installed("crayon"), crayon::bold(x), glue::glue("\033[1m{x}\033[22m")),
      width = getOption("width")
    )
  )
}

check_status <- function(res) {
  x = httr::status_code(res)
  if (x != 200) stop(glue::glue("The API returned an error, HTTP Response Code {x}"), call. = FALSE)
}

utils::globalVariables(c("where"))


`%c%` <- function(x, y) {
  ifelse(!is.na(x), x, y)
}


most_recent_ncaa_baseball_season <- function() {
  ifelse(
    as.double(substr(Sys.Date(), 6, 7)) >= 1,
    as.double(substr(Sys.Date(), 1, 4)),
    as.double(substr(Sys.Date(), 1, 4)) - 1
  )
}

most_recent_mlb_season <- function() {
  ifelse(
    as.double(substr(Sys.Date(), 6, 7)) >= 3,
    as.double(substr(Sys.Date(), 1, 4)),
    as.double(substr(Sys.Date(), 1, 4)) - 1
  )
}
# Functions for custom class
# turn a data.frame into a tibble/baseballr_data
make_baseballr_data <- function(df, type, timestamp){
  out <- df %>%
    tidyr::as_tibble()
  
  class(out) <- c("baseballr_data","tbl_df","tbl","data.table","data.frame")
  attr(out,"baseballr_timestamp") <- timestamp
  attr(out,"baseballr_type") <- type
  return(out)
}

print.baseballr_data <- function(x,...) {
  cli::cli_rule(left = "{attr(x,'baseballr_type')}",right = "{.emph baseballr {utils::packageVersion('baseballr')}}")
  
  if (!is.null(attr(x,'baseballr_timestamp'))) {
    cli::cli_alert_info(
      "Data updated: {.field {format(attr(x,'baseballr_timestamp'), tz = Sys.timezone(), usetz = TRUE)}}"
    )
  }
  
  NextMethod(print,x)
  invisible(x)
}

# rbindlist but maintain attributes of last file
rbindlist_with_attrs <- function(dflist){
  
  baseballr_timestamp <- attr(dflist[[length(dflist)]], "baseballr_timestamp")
  baseballr_type <- attr(dflist[[length(dflist)]], "baseballr_type")
  out <- data.table::rbindlist(dflist, use.names = TRUE, fill = TRUE)
  attr(out,"baseballr_timestamp") <- baseballr_timestamp
  attr(out,"baseballr_type") <- baseballr_type
  out
}

utils::globalVariables(c("where"))

mlb_api_call <- function(url){
  res <-
    httr::RETRY("GET", url)
  
  json <- res$content %>%
    rawToChar() %>%
    jsonlite::fromJSON(simplifyVector = T)
  
  return(json)
}

mlb_stats_endpoint <- function(endpoint){
  all_endpoints = c(
    "v1/attendance",#
    "v1/conferences",#
    "v1/conferences/{conferenceId}",#
    "v1/awards/{awardId}/recipients",#
    "v1/awards",#
    "v1/baseballStats",#
    "v1/eventTypes",#
    "v1/fielderDetailTypes",#
    "v1/gameStatus",#
    "v1/gameTypes",#
    "v1/highLow/types",#
    "v1/hitTrajectories",#
    "v1/jobTypes",#
    "v1/languages",
    "v1/leagueLeaderTypes",#
    "v1/logicalEvents",#
    "v1/metrics",#
    "v1/pitchCodes",#
    "v1/pitchTypes",#
    "v1/playerStatusCodes",#
    "v1/positions",#
    "v1/reviewReasons",#
    "v1/rosterTypes",#
    "v1/runnerDetailTypes",#
    "v1/scheduleEventTypes",#
    "v1/situationCodes",#
    "v1/sky",#
    "v1/standingsTypes",#
    "v1/statGroups",#
    "v1/statTypes",#
    "v1/windDirection",#
    "v1/divisions",#
    "v1/draft/{year}",#
    "v1/draft/prospects/{year}",#
    "v1/draft/{year}/latest",#
    "v1.1/game/{gamePk}/feed/live",
    "v1.1/game/{gamePk}/feed/live/diffPatch",#
    "v1.1/game/{gamePk}/feed/live/timestamps",#
    "v1/game/changes",##x
    "v1/game/analytics/game",##x
    "v1/game/analytics/guids",##x
    "v1/game/{gamePk}/guids",##x
    "v1/game/{gamePk}/{GUID}/analytics",##x
    "v1/game/{gamePk}/{GUID}/contextMetricsAverages",##x
    "v1/game/{gamePk}/contextMetrics",#
    "v1/game/{gamePk}/winProbability",#
    "v1/game/{gamePk}/boxscore",#
    "v1/game/{gamePk}/content",#
    "v1/game/{gamePk}/feed/color",##x
    "v1/game/{gamePk}/feed/color/diffPatch",##x
    "v1/game/{gamePk}/feed/color/timestamps",##x
    "v1/game/{gamePk}/linescore",#
    "v1/game/{gamePk}/playByPlay",#
    "v1/gamePace",#
    "v1/highLow/{orgType}",#
    "v1/homeRunDerby/{gamePk}",#
    "v1/homeRunDerby/{gamePk}/bracket",#
    "v1/homeRunDerby/{gamePk}/pool",#
    "v1/league",#
    "v1/league/{leagueId}/allStarBallot",#
    "v1/league/{leagueId}/allStarWriteIns",#
    "v1/league/{leagueId}/allStarFinalVote",#
    "v1/people",#
    "v1/people/freeAgents",#
    "v1/people/{personId}",##U
    "v1/people/{personId}/stats/game/{gamePk}",#
    "v1/people/{personId}/stats/game/current",#
    "v1/jobs",#
    "v1/jobs/umpires",#
    "v1/jobs/datacasters",#
    "v1/jobs/officialScorers",#
    "v1/jobs/umpires/games/{umpireId}",##x
    "v1/schedule/",#
    "v1/schedule/games/tied",#
    "v1/schedule/postseason",#
    "v1/schedule/postseason/series",#
    "v1/schedule/postseason/tuneIn",##x
    "v1/seasons",#
    "v1/seasons/all",#
    "v1/seasons/{seasonId}",#
    "v1/sports",#
    "v1/sports/{sportId}",#
    "v1/sports/{sportId}/players",#
    "v1/standings",#
    "v1/stats",#
    "v1/stats/metrics",##x
    "v1/stats/leaders",#
    "v1/stats/streaks",##404
    "v1/teams",#
    "v1/teams/history",#
    "v1/teams/stats",#
    "v1/teams/stats/leaders",#
    "v1/teams/affiliates",#
    "v1/teams/{teamId}",#
    "v1/teams/{teamId}/stats",#
    "v1/teams/{teamId}/affiliates",#
    "v1/teams/{teamId}/alumni",#
    "v1/teams/{teamId}/coaches",#
    "v1/teams/{teamId}/personnel",#
    "v1/teams/{teamId}/leaders",#
    "v1/teams/{teamId}/roster",##x
    "v1/teams/{teamId}/roster/{rosterType}",#
    "v1/venues"#
  )
  base_url = glue::glue('http://statsapi.mlb.com/api/{endpoint}')
  return(base_url)
}

.ncaa_headers <- function(url){
  headers <- c(
    `Host` = 'stats.ncaa.org',
    `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36',
    `Accept` = 'application/json, text/html, text/plain, */*',
    `Accept-Language` = 'en-US,en;q=0.5',
    `Accept-Encoding` = 'gzip, deflate, br',
    `Pragma` = 'no-cache',
    `Cache-Control` = 'no-cache'
  )
  return(headers)
}

process_statcast_payload <- function(payload) {
  
  # Clean up formatting of Baseball Savant download
  
  payload$game_date <- as.Date(payload$game_date, "%Y-%m-%d")
  payload$des <- as.character(payload$des)
  payload$inning <- as.character(payload$inning) %>% as.numeric()
  payload$at_bat_number <- as.character(payload$at_bat_number) %>% as.numeric()
  payload$pitch_number <- as.character(payload$pitch_number) %>% as.numeric()
  payload$game_pk <- as.character(payload$game_pk) %>% as.numeric()
  payload$hc_x <- as.character(payload$hc_x) %>% as.numeric()
  payload$hc_y <- as.character(payload$hc_y) %>% as.numeric()
  payload$on_1b <- as.character(payload$on_1b) %>% as.numeric()
  payload$on_2b <- as.character(payload$on_2b) %>% as.numeric()
  payload$on_3b <- as.character(payload$on_3b) %>% as.numeric()
  payload$release_pos_x <- as.character(payload$release_pos_x) %>% as.numeric()
  payload$release_pos_y <- as.character(payload$release_pos_y) %>% as.numeric()
  payload$release_pos_z <- as.character(payload$release_pos_z) %>% as.numeric()
  payload$hit_distance_sc <- as.character(payload$hit_distance_sc) %>% as.numeric()
  payload$launch_speed <- as.character(payload$launch_speed) %>% as.numeric()
  payload$launch_angle <- as.character(payload$launch_angle) %>% as.numeric()
  payload$pfx_x <- as.character(payload$pfx_x) %>% as.numeric()
  payload$pfx_z <- as.character(payload$pfx_z) %>% as.numeric()
  payload$plate_x <- as.character(payload$plate_x) %>% as.numeric()
  payload$plate_z <- as.character(payload$plate_z) %>% as.numeric()
  payload$vx0 <- as.character(payload$vx0) %>% as.numeric()
  payload$vy0 <- as.character(payload$vy0) %>% as.numeric()
  payload$vz0 <- as.character(payload$vz0) %>% as.numeric()
  payload$ax <- as.character(payload$ax) %>% as.numeric()
  payload$az <- as.character(payload$az) %>% as.numeric()
  payload$ay <- as.character(payload$ay) %>% as.numeric()
  payload$sz_bot <- as.character(payload$sz_bot) %>% as.numeric()
  payload$sz_top <- as.character(payload$sz_top) %>% as.numeric()
  payload$effective_speed <- as.character(payload$effective_speed) %>% as.numeric()
  payload$release_speed <- as.character(payload$release_speed) %>% as.numeric()
  payload$release_spin_rate <- as.character(payload$release_spin_rate) %>% as.numeric()
  payload$release_extension <- as.character(payload$release_extension) %>% as.numeric()
  payload$pitch_name <- as.character(payload$pitch_name)
  payload$home_score <- as.character(payload$home_score) %>% as.numeric()
  payload$away_score <- as.character(payload$away_score) %>% as.numeric()
  payload$bat_score	<- as.character(payload$bat_score) %>% as.numeric()
  payload$fld_score <- as.character(payload$fld_score) %>% as.numeric()
  payload$post_away_score <- as.character(payload$post_away_score) %>% as.numeric()
  payload$post_home_score	<- as.character(payload$post_home_score) %>% as.numeric()
  payload$post_bat_score <- as.character(payload$post_bat_score) %>% as.numeric()
  payload$post_fld_score <- as.character(payload$post_fld_score) %>% as.numeric()
  payload$zone <- as.character(payload$zone) %>% as.numeric()
  payload$spin_axis <- as.character(payload$spin_axis) %>% as.numeric()
  payload$if_fielding_alignment <- as.character(payload$if_fielding_alignment)
  payload$of_fielding_alignment <- as.character(payload$of_fielding_alignment)
  
  # Format player IDs as character
  
  cols_to_transform <- c("batter", "pitcher", "fielder_2",
                         "fielder_3", "fielder_4", "fielder_5", "fielder_6", "fielder_7",
                         "fielder_8", "fielder_9")
  
  payload <- payload %>%
    dplyr::ungroup() %>%
    dplyr::mutate_at(vars(dplyr::one_of(cols_to_transform)), as.character) %>%
    dplyr::mutate_at(vars(dplyr::one_of(cols_to_transform)), as.numeric) %>%
    dplyr::mutate_at(vars(dplyr::one_of(cols_to_transform)), function(x) {
      ifelse(is.na(x), 999999999, x)
    })
  
  return(payload)
  
}
#### rest of code ####

#### Dates ####
(dates_2021 <- seq.Date(as.Date('2021-04-01'), as.Date('2021-11-02'), by = '4 days'))

get_statcast_data <- function(date){
  start_date <- date
  end_date <- date + 3
  print(start_date) # for tracking
  start_date <- as.character(start_date); end_date <- as.character(end_date)
  
  df <- statcast_search_pitchers(start_date, end_date)
  df %>% 
    filter(game_type %in% c('R','F','D','L','W'))
  # R = Regular Season
  # F = Wild Card
  # D = Divisional Series
  # L = Conference Series
  # W = World Series
  return(df)
}


statcast_2021 <- map_df(dates_2021, get_statcast_data)
