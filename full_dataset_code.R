### The dataset ###


### Webscraping ###

library(rvest)

url <- "https://ourauckland.aucklandcouncil.govt.nz/news/2025/04/auckland-storm-recovery-moves-into-solution-mode/"

page <- read_html(url)

tables <- page |>
  html_elements("tbody")

table_wanted <- tables[[3]]

categorisation <- html_table(table_wanted)



### Cleaning the scraped data ###

names(categorisation) <- categorisation[1,]

categorisation <- categorisation[-1,]

# Some houses were ineligible/withdrew from the risk assessment which is included in the Registered variable - column 2

categorisation  <- categorisation[,-2]

# Removing the total column as I just want suburb stats.
categorisation <- categorisation[-203,]

# Making the variables numeric as it is currently a character variable
categorisation$`Cat 1` <- as.numeric(categorisation$`Cat 1`)
categorisation$`Cat 2C` <- as.numeric(categorisation$`Cat 2C`)
categorisation$`Cat 2P` <- as.numeric(categorisation$`Cat 2P`)
categorisation$`Cat 3` <- as.numeric(categorisation$`Cat 3`)
categorisation$`Total Final Category` <- as.numeric(categorisation$`Total Final Category`)

categorisation


# Adding latitude and longitude variables for each suburb

# From this awesome package tidygeocoder -> https://jessecambon.github.io/tidygeocoder/

library(tidygeocoder)

# Creating a tibble of locations
suburbs <- paste(categorisation$Suburb, "Auckland")
locations <- tibble(suburbs = suburbs)

# Geocode the suburb 
coords <- locations %>%
  geocode(suburbs, method = "osm")

# Adding the latitude and longitude variables to the categorisation data
categorisation$lat <- coords$lat
categorisation$lng <- coords$long

categorisation


which(is.na(categorisation$lat))
which(is.na(categorisation$lng))

# There are missing longitude and latitude values for suburb 101 and suburb 153

categorisation$Suburb[101] # Mount Rex -> Located in Kaukapakapa
categorisation[101,] # There is 1 Category 3 house in this "suburb". Therefore, I will add this stat under Kaukapakapa

categorisation[72,]
categorisation$`Cat 3`[72] <- 7
categorisation$`Total Final Category`[72] <- 17


categorisation$Suburb[153] # Scotts Landing -> located in Mahurangi East 
categorisation[153,] # Therefore, I will change this "suburb" to Mahurangi East

categorisation$Suburb[153] <- "Mahurangi East"
categorisation$lat[153] <- -36.46879
categorisation$lng[153] <- 174.7361

# Assign a value 0 to all category/total observations which are NA
categorisation$`Cat 1`[which(is.na(categorisation$`Cat 1`))] <- 0
categorisation$`Cat 2C`[which(is.na(categorisation$`Cat 2C`))] <- 0
categorisation$`Cat 2P`[which(is.na(categorisation$`Cat 2P`))] <- 0
categorisation$`Cat 3`[which(is.na(categorisation$`Cat 3`))] <- 0
categorisation$`Total Final Category`[which(is.na(categorisation$`Total Final Category`))] <- 0

# Remove Mount Rex
categorisation <- categorisation |>
  filter(Suburb != "Mount Rex")


categorisation <- categorisation[order(categorisation$Suburb), ]


# Saving the data frame so I don't have to wait 4 minutes for the latitude and longitude to load for each suburb
write.csv(categorisation, "data_for_app_v1.csv", row.names = FALSE)


data_for_app <- read_csv("data_for_app_v1.csv")


### RUN THE CODE IN THIS ORDER because I was too lazy and hard coded the index :/ ###


# Remove Bon Accord
data_for_app <- data_for_app |>
  filter(Suburb != "Bon Accord")


# Merging Owairaka with Mount Albert 
data_for_app[99,]
data_for_app[123,]

data_for_app$`Cat 1`[99] <- 13+10
data_for_app$`Cat 2P`[99] <- 2
data_for_app$`Cat 3`[99] <- 17+21
data_for_app$`Total Final Category`[99] <- 30+33

data_for_app <- data_for_app |>
  filter(Suburb != "Owairaka")


# Merging Bethells with Te Henga
data_for_app[170,]
data_for_app[15,]

data_for_app$`Cat 1`[170] <- 1+4
data_for_app$`Cat 2P`[170] <- 1
data_for_app$`Total Final Category`[170] <- 1+5

data_for_app <- data_for_app |>
  filter(Suburb != "Bethells")

data_for_app$Suburb[169] <- "Te Henga / Bethells Beach"


# Merging Pine Valley with Dairy Flat
data_for_app[31,]
data_for_app[133,]

data_for_app$`Cat 1`[31] <- 3+1
data_for_app$`Cat 3`[31] <- 7
data_for_app$`Total Final Category`[31] <- 10+1

data_for_app <- data_for_app |>
  filter(Suburb != "Pine Valley")



### Shifting lat/lng ###

# Western Springs
data_for_app$lat[191] <- -36.86204
data_for_app$lng[191] <- 174.7182

# Oratia
data_for_app$lat[116] <- -36.91157
data_for_app$lng[116] <- 174.6223

# Henderson Valley
data_for_app$lat[57] <- -36.89874
data_for_app$lng[57] <- 174.5909

# Pinehill
data_for_app$lat[133] <- -36.73073
data_for_app$lng[133] <- 174.7230

# Pohuehue
data_for_app$lat[134] <- -36.47158
data_for_app$lng[134] <- 174.6755

# Mount Wellington
data_for_app$lat[101] <- -36.90843
data_for_app$lng[101] <- 174.8386

# Waiatarua
data_for_app$lat[178] <- -36.93233
data_for_app$lng[178] <- 174.5786

# Hillpark
data_for_app$lat[60] <- -37.01503
data_for_app$lng[60] <- 174.9004

data_for_app$Suburb[60] <- "Hillpark"

# Waiake
data_for_app$lat[177] <- -36.70746
data_for_app$lng[177] <- 174.7493

# Forrest Hill 
data_for_app$lat[41] <- -36.76541
data_for_app$lng[41] <- 174.7481

# Sunnynook
data_for_app$lat[158] <- -36.75539
data_for_app$lng[158] <- 174.7368

# Removing macrons from suburb names
data_for_app <- data_for_app |>
  mutate(Suburb = chartr("ĀĒĪŌŪāēīōū", "AEIOUaeiou", Suburb))

# Muriwai beach -> Muriwai
data_for_app$Suburb[102] <- "Muriwai"

# Upper Waiwera -> Waiwera
data_for_app$Suburb[175] <- "Waiwera"

### Shifting lat/lng ###

# Waiwera
data_for_app$lat[175] <- -36.54639
data_for_app$lng[175] <- 174.6955

# Glenvar
data_for_app$lat[48] <- -36.68491
data_for_app$lng[48] <- 174.7282

# Manukau Central
data_for_app$lat[91] <- -36.99329
data_for_app$lng[91] <- 174.8777


### Removing the following "suburbs" ###
# North Cove, Hobbs Bay, Baddeleys Beach, Kaipara Hills, Kiwitahi, Ararimu Valley.

data_for_app <- data_for_app |>
  filter(!(Suburb %in% c("North Cove", "Hobbs Bay", "Baddeleys Beach", 
                         "Kaipara Hills", "Kiwitahi", "Ararimu Valley")))


data_for_app <- data_for_app[order(data_for_app$Suburb), ]


write.csv(data_for_app, "data_for_app.csv", row.names = FALSE)