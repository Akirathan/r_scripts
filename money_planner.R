# TODO: 
# - Locales: print dates in czech format.
# - Print intermediate dates?
# - Co kdyz bude vic entries se stejnym datem?
# - Sort dates.


INPUT_FILE <- "./data.csv"
DATE_FORMATS <- c("%d.%m.%Y", "%d.%m", "%d.%m.")

library("tibble")
library("purrr")
library("dplyr")


assert_correct_file_format <- function(data_frame) {
    stopifnot(!is.null(data_frame$Date))
    stopifnot(!is.null(data_frame$Amount))
    stopifnot(!is.null(data_frame$Comment))
}

add_total_column <- function(data_frame) {
    total_num <- 0
    total_col <- rep.int(0, nrow(data_frame))
    for (i in seq(nrow(data_frame))) {
        row <- data_frame[i,]
        total_num <- total_num + row[["Amount"]]
        total_col[i] <- total_num
    }
    data_frame <- add_column(data_frame, "Total"=total_col, .before="Comment")
    return (data_frame)
}

add_day_column <- function(data_frame) {
}

parse_date <- function(date_str) {
    as.Date(date_str, tryFormats=DATE_FORMATS, origin=Sys.Date())
}

is_date_in_correct_format <- function(date_str) {
    out <- tryCatch({
        as.Date(date_str)
    }, error = function(cond) {
        return(NA)
    })
    return (!is.na(out))
}

print_data_frame <- function(data_frame) {
    stopifnot(!is.null(data_frame$Total))
}

add_interval_entries <- function(data_frame) {
    is_interval <- function(date_str) {
        length(grep("-", date_str, fixed=TRUE)) > 0
    }

    parse_interval <- function(date_str) {
        items <- strsplit(date_str, "-")[[1]]
        stopifnot( length(items) == 2)
        start_date <- parse_date(items[1])
        end_date <- parse_date(items[2])
        duration <- end_date - start_date
        interval <- list(
            start_date = start_date,
            end_date = end_date,
            duration = duration
        )
        return (interval)
    }

    intervals <- list()
    intervals_idxs <- c()
    for (i in seq(nrow(data_frame))) {
        row <- data_frame[i,]
        if (is_interval(row$Date)) {
            intervals_idxs <- c(intervals_idxs, i)
            interval <- parse_interval(row$Date)
            interval$step <- row$Amount / as.integer(interval$duration)
            interval$comment <- row$Comment
            intervals <- c(intervals, list(interval))
        }
    }
    # Remove interval entry from data frame.
    for (interval_idx in intervals_idxs) {
        data_frame <- data_frame[-c(interval_idx),]
    }

    # Add entries to data_frame
    for (interval in intervals) {
        for (i in seq(interval$duration)) {
            actual_date <- interval$start_date + i
            data_frame <- rbind(data_frame, list(
                    Date=as.character(actual_date),
                    Amount=interval$step,
                    Comment=interval$comment))
        }
    }
    return (data_frame)
}

normalize_all_dates <- function(data_frame) {
    date_col <- as.data.frame(data_frame)[,1]
    date_col <- lapply(date_col, function(date_str) {
        if (!is_date_in_correct_format(date_str)) {
            return (parse_date(date_str))
        }
        else {
            return (date_str)
        }
    })

    for (i in seq(nrow(data_frame))) {
        data_frame[i,1] <- as.character(date_col[[i]])
    }
    return (data_frame)
}

group_amount_by_days <- function(data_frame) {
    paste_comments <- function(comments) {
        res <- c()
        for (comment in comments) {
            res <- paste(res, comment, sep=",")
        }
        stopifnot(length(res) == 1)
        return (res)
    }

    summarised_df <- data_frame %>%
        group_by(Date) %>%
        summarise(Amount = sum(Amount), Comment = paste_comments(Comment))
    return (summarised_df)
}

preprocess_data_frame <- function(data_frame) {
    data_frame <- add_interval_entries(data_frame)
    data_frame <- normalize_all_dates(data_frame)
    data_frame <- group_amount_by_days(data_frame)
    cat("After preprocessing:\n")
    print(data_frame, n=nrow(data_frame))
}

process_data_frame <- function(data_frame) {
    data_frame <- add_total_column(data_frame)
    cat("\n")
    cat("After processing:\n")
    print(data_frame, n=nrow(data_frame))
    return (data_frame)
}

read_file <- function(filename) {
    df <- as_tibble(read.csv(filename, header=TRUE,
        stringsAsFactors=FALSE,
        colClasses=c("character", "numeric", "character")))

    assert_correct_file_format(df)
    return (df)
}

df <- read_file(INPUT_FILE)
df <- preprocess_data_frame(df)
df <- process_data_frame(df)

