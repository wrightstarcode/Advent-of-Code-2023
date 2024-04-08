# Day 2 ----
## Load data ----
data <- read.csv(, header = FALSE, sep = ";")

# Part 1 ----
## Tidy data ----
data <- data |>
  # Separate game id and set 1 to be their own observations
  tidyr::separate_wider_delim(V1, delim=":", names = c("puzzle1"), too_many = "debug") |>
  # Remove unwanted variables
  dplyr::select(-c(V1, `V1_ok`, `V1_pieces`)) |>
  # Rename the column names
  dplyr::rename(game_id = puzzle1,
                set1 = V1_remainder,
                set2 = V2,
                set3 = V3,
                set4 = V4,
                set5 = V5,
                set6 = V6)

# Remove the punctuation (:) in set 1 variable
data$set1 <- stringr::str_replace_all(string = data$set1, pattern = ":", replacement = "")

## Separate each set so each colour has its own observation ----
data <- data |>
  # Set 1
  tidyr::separate_wider_delim(set1, delim = ",", names = c("set1a", "set1b", "set1c"), too_few = "align_start") |>
  # Set 2
  tidyr::separate_wider_delim(set2, delim = ",", names = c("set2a", "set2b", "set2c"), too_few = "align_start") |>
  # Set 3
  tidyr::separate_wider_delim(set3, delim = ",", names = c("set3a", "set3b", "set3c"), too_few = "align_start") |>
  # Set 4
  tidyr::separate_wider_delim(set4, delim = ",", names = c("set4a", "set4b", "set4c"), too_few = "align_start") |>
  # Set 5
  tidyr::separate_wider_delim(set5, delim = ",", names = c("set5a", "set5b", "set5c"), too_few = "align_start") |>
  # Set 6
  tidyr::separate_wider_delim(set6, delim = ",", names = c("set6a", "set6b", "set6c"), too_few = "align_start")

## Pivot the dataframe to be longer ----
data <- data |>
  tidyr::pivot_longer(cols = !game_id,
                      names_to = "Name",
                      values_to = "Value",
                      values_drop_na = TRUE) |>
  # Remove unwanted variables
  dplyr::select(-c(Name))

## Split the "Value" variable ----
# Remove excess whitespace from the left hand side of the variable
data$Value <- stringr::str_trim(data$Value, side = "left")
# The number and colour need their own observations
data[c('val', 'colour')] <- stringr::str_split_fixed(data$Value, ' ', 2)

## Pivot the dataframe to be wider ----
# Replace the empty strings with NA
data <- replace(data, data == "", NA)
data <- data |>
  dplyr::select(c(game_id, val, colour)) |>
  # Use the colours as the column names
  tidyr::pivot_wider(names_from = "colour",
                     values_from = "val",
                     values_fn = list)

## Unlist, convert to numeric and take the maximum value for each observation ----
for (i in 1:nrow(data)){
  # Red
  data$red_num[i] = max((as.numeric(unlist(data$red[i]))))
  # Blue
  data$blue_num[i] = max((as.numeric(unlist(data$blue[i]))))
  # Green
  data$green_num[i] = max((as.numeric(unlist(data$green[i]))))
}

## Filter the dataframe ---- 
P1_data <- data |>
  # Remove unwanted variables
  dplyr::select(-c(red, blue, green, `NA`)) |>
  # Include only rows that meet the criteria in the question
  dplyr::filter(red_num <= 12 & blue_num <= 14 & green_num <= 13)

## Identify the numbers in the game_id ----
P1_data$id <- as.numeric(gsub("\\D", "", P1_data$game_id))

## Sum the id values ----
sum(P1_data$id)
#2265


# Part 2 ----
## Data ----
P2_data <- data |>
  # Remove unwanted variables
  dplyr::select(-c(red, blue, green, `NA`))

## Calculate the power for each game ----
for (j in 1:nrow(P2_data)){
  P2_data$power[j] = P2_data$red_num[j] * P2_data$green_num[j] * P2_data$blue_num[j]
}

## Sum the powers ----
sum(P2_data$power)
#64097







