# TODO: 
# - Locales: print dates in czech format.
# - Dat Total sloupec pred Comment sloupec.
# - Print intermediate dates?
# - Sort dates.


INPUT_FILE <- "./data.csv"
DATE_FORMATS <- c("%d.%m.%Y", "%d.%m", "%d.%m.")

library("tibble")


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

print_data_frame <- function(data_frame) {
    stopifnot(!is.null(data_frame$Total))
}

normalize_all_dates <- function(data_frame) {
    date_col <- data_frame[,1]
    date_col <- lapply(date_col, function(date) {
        as.Date(date, tryFormats=DATE_FORMATS, origin=Sys.Date())
    })

    for (i in seq(nrow(data_frame))) {
        data_frame[i,1] <- as.character(date_col[[i]])
    }
    return (data_frame)
}

read_file <- function(filename) {
    df <- read.csv(filename, header=TRUE,
        stringsAsFactors=FALSE,
        colClasses=c("character", "numeric", "character"))

    assert_correct_file_format(df)
    df <- normalize_all_dates(df)
}

df <- read_file(INPUT_FILE)
df <- as_tibble(df)
cat("Dataframe before processing:\n")
print(df)

df <- add_total_column(df)
cat("\n")
cat("Dataframe after processing:\n")
print(df)

