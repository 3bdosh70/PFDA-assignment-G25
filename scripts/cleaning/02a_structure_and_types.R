

# 02a_structure_and_types.R


#Create a working copy of the raw dataset
clean_data = raw_data


#-----------------------------------------------------

#Check the starting structure of the dataset

nrow(clean_data)
ncol(clean_data)
names(clean_data)
str(clean_data)

#The dataset starts with 114000 rows and 21 columns
#The additional X column was identified during Stage 01

#-----------------------------------------------------

#Check the X column before removing it

head(clean_data$X)
tail(clean_data$X)
length(unique(clean_data$X))

#X contains a unique sequential value for every row and is not included
#in the supplied dataset description, so it is treated as an index column

#-----------------------------------------------------

#Remove the additional X index column

clean_data$X = NULL

nrow(clean_data)
ncol(clean_data)
names(clean_data)

#X is removed because its an additional index column and does not
#represent a Spotify track characteristic used in the analysis
#The dataset now contains 114000 rows and the 20 documented columns

#-----------------------------------------------------

#Check the remaining column names against the dataset description

expected_columns = c(
  "track_id",
  "artists",
  "album_name",
  "track_name",
  "popularity",
  "duration_ms",
  "explicit",
  "danceability",
  "energy",
  "key",
  "loudness",
  "mode",
  "speechiness",
  "acousticness",
  "instrumentalness",
  "liveness",
  "valence",
  "tempo",
  "time_signature",
  "track_genre"
)

names(clean_data)

all(names(clean_data) == expected_columns)

#TRUE confirms that the 20 remaining column names match
#the columns in dataset_description.txt

#-----------------------------------------------------

#Check the current data types

str(clean_data)

#The character and numeric audio variables are already represented
#using appropriate R data types
#The variables documented as integer and boolean are checked below

#-----------------------------------------------------

#Check that variables documented as integers contain whole numbers

sum(
  !is.na(clean_data$popularity) &
    clean_data$popularity %% 1 != 0
)

sum(
  !is.na(clean_data$duration_ms) &
    clean_data$duration_ms %% 1 != 0
)

sum(
  !is.na(clean_data$key) &
    clean_data$key %% 1 != 0
)

sum(
  !is.na(clean_data$mode) &
    clean_data$mode %% 1 != 0
)

sum(
  !is.na(clean_data$time_signature) &
    clean_data$time_signature %% 1 != 0
)

#key = -5
#mode = 5
#popularity = 999
#All five variables contain no decimal values among their non-missing records
#Their values are not validated in this stage because value validation
#will be completed in Stage 02c

#-----------------------------------------------------

#Record the missing value count before changing data types

missing_before_types = sum(is.na(clean_data))

missing_before_types

#There are 3553 missing values before the data type conversions

#-----------------------------------------------------

#Convert variables documented as integers

clean_data$popularity = as.integer(clean_data$popularity)
clean_data$duration_ms = as.integer(clean_data$duration_ms)
clean_data$key = as.integer(clean_data$key)
clean_data$mode = as.integer(clean_data$mode)
clean_data$time_signature = as.integer(clean_data$time_signature)

#The variables documented as integers are converted from numeric to integer
#after confirming that their non-missing values contain no decimals

#-----------------------------------------------------

#Check the explicit variable before conversion

unique(clean_data$explicit)

#Explicit contains False, True and missing values
#The dataset description identifies explicit as a boolean variable

#Convert explicit from character to logical

clean_data$explicit = clean_data$explicit == "True"

unique(clean_data$explicit)

#explicit is converted to logical and is now represented
#as FALSE, TRUE and NA 

#-----------------------------------------------------

#Check whether data type conversions created new missing values

missing_after_types = sum(is.na(clean_data))

missing_before_types
missing_after_types

missing_after_types - missing_before_types

#before = 3553
#after  = 3553
#difference = 0
#The difference is 0, confirming that no additional missing values
#were created during the data type conversions

#-----------------------------------------------------

#Check the final dataset structure and data types

nrow(clean_data)
ncol(clean_data)
names(clean_data)
str(clean_data)

#The final structure contains 114000 rows and 20 columns
#No rows were removed during Stage 02a
#The column names and data types are now prepared for later cleaning stages

#-----------------------------------------------------

