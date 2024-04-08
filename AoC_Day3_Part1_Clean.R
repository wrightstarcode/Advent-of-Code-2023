# Day 3 ----
## Load data ----
data <- read.csv(, header = FALSE)

# Part 1 ----
## Data ----
P1_data <- data |>
  # Rename data column to be more meaningful
  dplyr::rename(original = V1)

# Replace all the symbols with "S" so they are all consistent
P1_data$new <- stringr::str_replace_all(P1_data$original, c("\\*" = "S", 
                                                            "-" = "S", 
                                                            "/" = "S", 
                                                            "@" = "S", 
                                                            "#" = "S",
                                                            "\\$" = "S", 
                                                            "%" = "S", 
                                                            "\\+" = "S", 
                                                            "=" = "S",
                                                            "&" = "S"))

# Replace all the numbers with "N" so they are all consistent
P1_data$new <- stringr::str_replace_all(P1_data$new, c("1" = "N", 
                                                       "2" = "N", 
                                                       "3" = "N", 
                                                       "4" = "N", 
                                                       "5" = "N",
                                                       "6" = "N", 
                                                       "7" = "N", 
                                                       "8" = "N", 
                                                       "9" = "N",
                                                       "0" = "N"))


## Create grids ---- 
# Original
P1_data_og_grid <- subset(P1_data, select = -c(new))
P1_data_og_grid[c(1:140)] <- stringr::str_split_fixed(P1_data$original, '', 140)
# New
P1_data_new_grid <- subset(P1_data, select = -c(original))
P1_data_new_grid[c(1:140)] <- stringr::str_split_fixed(P1_data_new_grid$new, '', 140)
# Pad the new grid so the indexing in the below for loop works
P1_data_pad <- rbind("A", cbind("A", P1_data_new_grid, "A"), "A") 

## Identify where the part numbers are ----
# Create a for loop that looks left, right, up, down, diagonal top right, diagonal top left, 
# diagonal bottom right and diagonal bottom left to identify if an N is adjacent to an S
num_loca <- c()
for (i in 2:(length(P1_data_pad)-1)) {
  for (j in 2:(nrow(P1_data_pad)-1)) {
    if (P1_data_pad[j, i] == "N" & (P1_data_pad[j+1, i] == "S" | P1_data_pad[j, i+1] == "S" | 
                                    P1_data_pad[j-1, i] == "S" | P1_data_pad[j, i-1] == "S" | 
                                    P1_data_pad[j-1, i-1] == "S" | P1_data_pad[j+1, i+1] == "S" |
                                    P1_data_pad[j-1, i+1] == "S" | P1_data_pad[j+1, i-1] == "S")) {
      tempCoords <-  c(j-1, i-1) # -1 to account for the pad
      num_loca <- rbind(num_loca, tempCoords)
    }
  }
}

# Tidy the data frame created by the for loop
num_loca <- data.frame(num_loca)
num_loca <- num_loca |>
  dplyr::rename(row_coord = X1,
                column_coord = X2)
rownames(num_loca) <- 1:nrow(num_loca) # rename row names
num_loca <- num_loca[order(num_loca$row_coord),] # order the row_coords in ascending order

## Remove duplicate part numbers ----
# Create a new column which will identify if the row is a duplicate number of another row
num_loca$duplicate <- "no" 

# Because the for loop that identifies part number only considers if a digit is adjacent to 
# the symbol rather than the whole number the duplicates need to be removed, ie. where the 
# coords are different but they point to the same overall number
for (t in 1:nrow(num_loca)){
  if (num_loca$column_coord[t]+1 == num_loca$column_coord[t+1] | num_loca$column_coord[t]-1 == num_loca$column_coord[t+1]){
    num_loca$duplicate[t] <- "yes"
  }
}

## Identify the actual part number ----
# From the coordinates calculate what number was there originally, the "centre" number
centre <- c()
for (y in 1:nrow(num_loca)){
  centre[y] <- P1_data_og_grid[num_loca[y,1], num_loca[y,2]]
}

# Combine the original number with the coordinate data frame
centre <- data.frame(centre)
num_loca <- cbind(num_loca, centre)

## Identify the rest of the part numbers ----
for (x in 1:nrow(num_loca)){
  # Identify what number is to the left of the centre number
  if (P1_data_og_grid[num_loca[x,1], (num_loca[x,2]-1)] == "0"| P1_data_og_grid[num_loca[x,1], (num_loca[x,2]-1)] == "1" |
      P1_data_og_grid[num_loca[x,1], (num_loca[x,2]-1)] == "2"| P1_data_og_grid[num_loca[x,1], (num_loca[x,2]-1)] == "3"| 
      P1_data_og_grid[num_loca[x,1], (num_loca[x,2]-1)] == "4"| P1_data_og_grid[num_loca[x,1], (num_loca[x,2]-1)] == "5"| 
      P1_data_og_grid[num_loca[x,1], (num_loca[x,2]-1)] == "6"| P1_data_og_grid[num_loca[x,1], (num_loca[x,2]-1)] == "7"| 
      P1_data_og_grid[num_loca[x,1], (num_loca[x,2]-1)] == "8"| P1_data_og_grid[num_loca[x,1], (num_loca[x,2]-1)] == "9"){
    num_loca$left[x] <- paste(P1_data_og_grid[num_loca[x,1], (num_loca[x,2]-1)])
  } else {
    num_loca$left[x] <- paste("")
  }
  # Identify what number is to the left left of the centre number
  if (P1_data_og_grid[num_loca[x,1], (num_loca[x,2]-2)] == "0"| P1_data_og_grid[num_loca[x,1], (num_loca[x,2]-2)] == "1" |
      P1_data_og_grid[num_loca[x,1], (num_loca[x,2]-2)] == "2"| P1_data_og_grid[num_loca[x,1], (num_loca[x,2]-2)] == "3"| 
      P1_data_og_grid[num_loca[x,1], (num_loca[x,2]-2)] == "4"| P1_data_og_grid[num_loca[x,1], (num_loca[x,2]-2)] == "5"| 
      P1_data_og_grid[num_loca[x,1], (num_loca[x,2]-2)] == "6"| P1_data_og_grid[num_loca[x,1], (num_loca[x,2]-2)] == "7"| 
      P1_data_og_grid[num_loca[x,1], (num_loca[x,2]-2)] == "8"| P1_data_og_grid[num_loca[x,1], (num_loca[x,2]-2)] == "9"){
    num_loca$left_left[x] <- paste(P1_data_og_grid[num_loca[x,1], (num_loca[x,2]-2)])
  } else {
    num_loca$left_left[x] <- paste("")
  }
  # Identify what number is to the right of the centre number
  if (P1_data_og_grid[num_loca[x,1], (num_loca[x,2]+1)] == "0"| P1_data_og_grid[num_loca[x,1], (num_loca[x,2]+1)] == "1" |
      P1_data_og_grid[num_loca[x,1], (num_loca[x,2]+1)] == "2"| P1_data_og_grid[num_loca[x,1], (num_loca[x,2]+1)] == "3"| 
      P1_data_og_grid[num_loca[x,1], (num_loca[x,2]+1)] == "4"| P1_data_og_grid[num_loca[x,1], (num_loca[x,2]+1)] == "5"| 
      P1_data_og_grid[num_loca[x,1], (num_loca[x,2]+1)] == "6"| P1_data_og_grid[num_loca[x,1], (num_loca[x,2]+1)] == "7"| 
      P1_data_og_grid[num_loca[x,1], (num_loca[x,2]+1)] == "8"| P1_data_og_grid[num_loca[x,1], (num_loca[x,2]+1)] == "9"){
    num_loca$right[x] <- paste(P1_data_og_grid[num_loca[x,1], (num_loca[x,2]+1)])
  } else {
    num_loca$right[x] <- paste("")
  }
  # Identify what number is to the right right of the centre number
  if (P1_data_og_grid[num_loca[x,1], (num_loca[x,2]+2)] == "0"| P1_data_og_grid[num_loca[x,1], (num_loca[x,2]+2)] == "1" |
      P1_data_og_grid[num_loca[x,1], (num_loca[x,2]+2)] == "2"| P1_data_og_grid[num_loca[x,1], (num_loca[x,2]+2)] == "3"| 
      P1_data_og_grid[num_loca[x,1], (num_loca[x,2]+2)] == "4"| P1_data_og_grid[num_loca[x,1], (num_loca[x,2]+2)] == "5"| 
      P1_data_og_grid[num_loca[x,1], (num_loca[x,2]+2)] == "6"| P1_data_og_grid[num_loca[x,1], (num_loca[x,2]+2)] == "7"| 
      P1_data_og_grid[num_loca[x,1], (num_loca[x,2]+2)] == "8"| P1_data_og_grid[num_loca[x,1], (num_loca[x,2]+2)] == "9"){
    num_loca$right_right[x] <- paste(P1_data_og_grid[num_loca[x,1], (num_loca[x,2]+2)])
  } else {
    num_loca$right_right[x] <- paste("")
  }
}

## Filter the dataframe ----
# Remove duplicates
num_loca <- num_loca |>
  dplyr::filter(duplicate == "no")

## Combine the part number digits ----
# Combine the numbers but only if the digits are next to each other, ie. don't combine digits if they are left, 
# centre and right right as right right will belong to a different number
for (k in 1:nrow(num_loca)){
  if (num_loca$left[k] == "" & num_loca$right[k] != ""){
    num_loca$comb[k] <- paste(num_loca$centre[k], num_loca$right[k], num_loca$right_right[k], sep = "")
  } else if (num_loca$left[k] == "" & num_loca$right[k] == ""){
    num_loca$comb[k] <- paste(num_loca$centre[k], sep = "")
  } else if (num_loca$right[k] == "" & num_loca$left[k] != ""){
    num_loca$comb[k] <- paste(num_loca$left_left[k], num_loca$left[k], num_loca$centre[k], sep = "")
  } else if (num_loca$left_left[k] == "" & num_loca$right_right[k] == "" & num_loca$left[k] != "" & num_loca$right[k] != ""){
    num_loca$comb[k] <- paste(num_loca$left[k], num_loca$centre[k], num_loca$right[k], sep = "")
  } else {
    num_loca$comb[k] <- paste(num_loca$left_left[k], num_loca$left[k], num_loca$centre[k], num_loca$right[k],
                              num_loca$right_right[k], sep = "")
  }
}

# Convert the column to a numeric vector
comb_num <- as.numeric(num_loca$comb)

## Sum the numbers ----
sum(comb_num)
#539637



