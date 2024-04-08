
# Day 1 ---- 
## Load data ----
data <- read.csv()

## Part 1 ----
### Data ----
P1_data <- data

### Identify numbers ----
P1_data$raw_values <- as.numeric(gsub("\\D", "", P1_data$Calibration)) 

### Select digits ----
P1_data$first <- stringr::str_sub(P1_data$raw_values, 1, 1) 
P1_data$last <- stringr::str_sub(P1_data$raw_values, -1, -1)

### Combine digits ----
P1_data$values <- as.numeric(paste0(P1_data$first, P1_data$last)) 

### Sum the values ----
sum(P1_data$values)
#55002


## Part 2 ----
### Data ----
P2_data <- data

### Identify numbers ----
for (i in 1:nrow(P2_data)){
  # one to o1e
  if (stringr::str_detect(P2_data$Calibration[i], 'one') == TRUE) {
    P2_data$cal_one[i] <- gsub("one", "o1e", P2_data$Calibration[i])
  } else {
    P2_data$cal_one[i] <- paste(P2_data$Calibration[i])
  }
  # two to t2o
  if (stringr::str_detect(P2_data$cal_one[i], 'two') == TRUE) {
    P2_data$cal_one[i] <- gsub("two", "t2o", P2_data$cal_one[i])
  } else {
    P2_data$cal_one[i] <- paste(P2_data$cal_one[i])
  }
  # three to t3e
  if (stringr::str_detect(P2_data$cal_one[i], 'three') == TRUE) {
    P2_data$cal_one[i] <- gsub("three", "t3e", P2_data$cal_one[i])
  } else {
    P2_data$cal_one[i] <- paste(P2_data$cal_one[i])
  }
  # four to f4r
  if (stringr::str_detect(P2_data$cal_one[i], 'four') == TRUE) {
    P2_data$cal_one[i] <- gsub("four", "f4r", P2_data$cal_one[i])
  } else {
    P2_data$cal_one[i] <- paste(P2_data$cal_one[i])
  }
  # five to f5e
  if (stringr::str_detect(P2_data$cal_one[i], 'five') == TRUE) {
    P2_data$cal_one[i] <- gsub("five", "f5e", P2_data$cal_one[i])
  } else {
    P2_data$cal_one[i] <- paste(P2_data$cal_one[i])
  }
  # six to s6x
  if (stringr::str_detect(P2_data$cal_one[i], 'six') == TRUE) {
    P2_data$cal_one[i] <- gsub("six", "s6x", P2_data$cal_one[i])
  } else {
    P2_data$cal_one[i] <- paste(P2_data$cal_one[i])
  }
  # seven to s7n
  if (stringr::str_detect(P2_data$cal_one[i], 'seven') == TRUE) {
    P2_data$cal_one[i] <- gsub("seven", "s7n", P2_data$cal_one[i])
  } else {
    P2_data$cal_one[i] <- paste(P2_data$cal_one[i])
  }
  # eight to e8t
  if (stringr::str_detect(P2_data$cal_one[i], 'eight') == TRUE) {
    P2_data$cal_one[i] <- gsub("eight", "e8t", P2_data$cal_one[i])
  } else {
    P2_data$cal_one[i] <- paste(P2_data$cal_one[i])
  }
  # nine to n9e
  if (stringr::str_detect(P2_data$cal_one[i], 'nine') == TRUE) {
    P2_data$cal_one[i] <- gsub("nine", "n9e", P2_data$cal_one[i])
  } else {
    P2_data$cal_one[i] <- paste(P2_data$cal_one[i])
  }
}

### Remove letters ----
P2_data$raw_values <- as.numeric(gsub("\\D", "", P2_data$cal_one))

### Select digits ----
P2_data$first <- stringr::str_sub(P2_data$raw_values, 1, 1) 
P2_data$last <- stringr::str_sub(P2_data$raw_values, -1, -1)

### Combine digits ----
P2_data$values <- as.numeric(paste0(P2_data$first, P2_data$last)) 

### Sum the values ----
sum(P2_data$values)
#55093





