
INPUT_FILE <- "/home/pal/dev/r/Toggl_time_entries_2019-09-01_to_2019-09-30.csv"

library(tibble)
library(dplyr)

assert_correct_frame_format <- function(data_frame) {
    stopifnot(!is.null(data_frame$Project))
    stopifnot(!is.null(data_frame$Duration))
    stopifnot(!is.null(data_frame$Start.date))
    stopifnot(!is.null(data_frame$End.date))
    stopifnot(!is.null(data_frame$Start.time))
    stopifnot(!is.null(data_frame$End.time))
}

Time <- function(time_str) {
    items <- unlist( strsplit(time_str, ":", fixed=TRUE))
    time <- list(hours=as.integer(items[[1]]),
                 minutes=as.integer(items[[2]])
                 )
    if (length(items) == 3) {
        time$seconds <- as.integer(items[[3]])
    }
    else {
        time$seconds <- as.integer(0)
    }
    class(time) <- "Time"
    return(time)
}

as.Time <- function(time_str) {
    return (Time(time_str))
}

time_to_seconds <- function(time) {
    if (class(time) == "Time") {
        seconds <- (time$hours * 3600) + (time$minutes * 60) + time$seconds
        return (seconds)
    }
    else {
        return (NA)
    }
}

seconds_to_time <- function(total_seconds) {
    hours <- as.integer(total_seconds / 3600)
    rest <- total_seconds - (hours * 3600)
    minutes <- as.integer(rest / 60)
    seconds <- rest %% 60

    time <- list(
        hours=hours,
        minutes=minutes,
        seconds=seconds
    )
    class(time) <- "Time"
    return (time)
}

test_time <- function() {
    hours <- 1
    minutes <- 13
    seconds <- 0
    total_secs <- (1 * 3600) + (13 * 60)
    time <- seconds_to_time(total_secs)
    stopifnot( time$hours == hours)
    stopifnot( time$minutes == minutes)

    seconds <- time_to_seconds(time)
    stopifnot( seconds == total_secs)
}

`+.Time` <- function(time1, time2, ...) {
    if (class(time1) == "Time" && class(time2) == "Time") {
        seconds <- time_to_seconds(time1) + time_to_seconds(time2)
        time <- seconds_to_time(seconds)
        return (time)
    }
    else {
        return (NA);
    }
}

read_file <- function(filename) {
    df <- as_tibble(read.csv(filename, header=TRUE, stringsAsFactors=FALSE))

    assert_correct_frame_format(df)
    return (df)
}

time_sum <- function(time_strs) {
    times <- c()
    for (time_str in time_strs) {
        time <- as.Time(time_str)
        times <- c(times, time)
    }
    total_time <- Time("0:00:00")
    for (time in times) {
        total_time <- total_time + time
    }
    cat("Total time: ", total_time, "\n")
    return (as.character(total_time))
}

group_by_days <- function(data_frame) {
    day_duration_table <-
        data_frame %>%
        group_by(Start.date) %>%
        summarise(day_duration = time_sum(Duration))
    
    return (day_duration_table)
}

test_operator <- function() {
    time1 <- Time("0:30:12")
    time2 <- Time("0:10:05")
    res_time <- time1 + time2
}


df <- read_file(INPUT_FILE)
#group_by_days(df)