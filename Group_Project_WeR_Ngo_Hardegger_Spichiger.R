#############################################################################################################
#                                 BIG DATA ANALYTICS - GROUP PROJECT
#                                                WeR
#                               CRIME IN CHICAGO - PREDICIVE Police
#############################################################################################################
#
# Phuong Lan Ngo (16-122-319),   Kevin Hardegger (12-758-785),   Matthias Spichiger (15-937-557)
#
#############################################################################################################
#
# CRIME DATA:      https://data.cityofchicago.org/api/views/w98m-zvie/rows.csv?accessType=DOWNLOAD
# POLICE Beats:    https://data.cityofchicago.org/Public-Safety/Boundaries-Police-PBeats-current-/aerh-rz74
# POLICE STATIONS: https://data.cityofchicago.org/Public-Safety/Police-Stations-Shapefiles/tc9m-x6u6
#############################################################################################################
# LAST UPDATE:     St. Gallen, May 2020

## VARIABLE DESCRIPTION ----------------------------------------------------------------------------------------------
#
# The Variables are explained in Detail on the Website of the Chicago Open Data Project, which is accessible
# under this Link: https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2
#
# The 22 Variables in the Data Set containing Records of Criminal Activity in the City of Chicago from 2001-Present are
# Original (New) Name - Description
#
# ID (ID)                - Unique Identifier for the Record
# Case Number (Case)     - Chicago Police Department Record Divion Number
# Date (Date)            - Date when the Incident occurred
# Block (Block)          - Partially redacted Address where Incident occured
# IUCR (IUCR)            - Illinois Uniform Crime Reporting Code
# Primary Type (PType)   - Primary Description of IUCR Code
# Description (Desc)     - Secondary Description of IUCR Code
# Location Description (LocDesc) - Description of Location where Incident occurred
# Arrest (ARR)           - Indicates whether an Arrest was made
# Domestic (Dom)         - Indicates whether Incident was domestic-related
# Beat (PBeat)           - Indicates the Police Beat where the Incident Occured (One Police Car)
# District (Dist)        - Police District where Incident Occured (Three to Five Police Cars)
# Ward (Ward)            - City Council District (Ward) where Incident Occured
# Community Area (Comm)  - Which of the 77 Community Areas of Chicago
# FBI Code (FBI)         - Crime Classification according to the FBI
# X Coordinate (X_Coord) - North American Datum (NAD)-Projection
# Y Coordinate (Y_Coord) - North American Datum (NAD)-Projection
# Year (Year)            - Year the Incident Occurred
# Updated on (Update)    - Date and Time of Last Update
# Latitude (Lat)         - Latidude (Many NAs)
# Longitude (Long)       - Longitude (Many NAs)
# Location (Loc)         - Location Identifier

## GET STARTED ----------------------------------------------------------------------------------------------

# Remove all Variables in the Global Environment:
rm(list = ls(all.names = TRUE))

# Clear the Console Output:
cat("\f")

# Find the Working Directory:
getwd()
# Change the Working Directory if necessary:
# setwd("C:/your_directory")

# Start a new R session if necessary:
# .rs.restartR()


## INSTALL PACKAGES ----------------------------------------------------------------------------------------------
# Install all the necessary Packages:

install.packages(c("ggplot2",     # For nice Plots
                   "data.table",  # For loading of large Data Sets
                   "rgdal",       # For Maps
                   "rgeos",       # For Maps
                   "maptools",    # For Maps
                   "lubridate",   # For Timestamps
                   "chron",       # For Timestamps
                   "plyr",        # For Data Handling
                   "doBy",        # For Data Handling
                   "psych",       # For Correlation Plots
                   "MASS"))       # For Modelling

## LOAD PACKAGES ----------------------------------------------------------------------------------------------
# Load all the necessary Packages:
# Make sure that the Packages have been installed with the Code above (!)
library(data.table)
library(ggplot2)
library(rgdal)
library(rgeos)
library(maptools)
library(lubridate)
library(chron)
library(plyr)
library(doBy)
library(ggfortify)
library(psych)
library(MASS)

## LOAD THE DATA ----------------------------------------------------------------------------------------------
# We want to load 3 Million Observations from the Data Set:

# We can access the Crime Data directly with the following URL:
URL = "https://data.cityofchicago.org/api/views/ijzp-q8t2/rows.csv?accessType=DOWNLOAD"

# With the following command, the Data is directly loaded into R:
Criminal <- fread(input = URL,
                  nrows = 3000000,
                  na.strings = "",
                  stringsAsFactors = FALSE,
                  nThread = 4,
                  verbose = TRUE)

# REMARK:
# We have used the fread()-Function from the "data.table"-Package, because it seemed the most
# convenient choice for our purpose.
# Howver, we were able to run the same Analysis with the functions from the "ff" and "ffbase"-Packages
# The "bigmatrix"-Package did not seem appropriate for our Purpose because matrices only allow for
# one Data Type.

## RENAME VARIABLES ----------------------------------------------------------------------------------------------
# We rename all the Variables, to more compact ones:
names(Criminal) <- c("ID", "Case", "Date", "Block",
                     "IUCR", "PType", "Desc",
                     "LocDesc", "ARR",
                     "Dom", "PBeat", "Dist",
                     "Ward", "Comm", "FBICode",
                     "X_Coord", "Y_Coord",
                     "Year", "Update", "Lat",
                     "Long", "Loc")

# We check the Variable Names:
names(Criminal)
# We proceed if all the Variable Names have been changed accordingly.


## GET OVERVIEW ----------------------------------------------------------------------------------------------
# Get summary statistics:
summary(Criminal)

# Get evaluate the structure of the Data Set:
str(Criminal)

## DATA CLEANING ------------------------------------------------------------------------------------------------
# We want to further clean the Data Set to bring it in a Form that is amendable to our Analysis.

## DUPLICATES  ---------------------------------------------------------------------------------------------------
# First,  we check if there are dublicated Cases in the Data Set:
any(duplicated(Criminal$Case))

# We remove all the duplicated Cases:
Criminal <- subset(Criminal, !duplicated(Criminal$Case))

## NAs  ----------------------------------------------------------------------------------------------

# Second, we remove Observations that include NAs
# Note: This is a greedy Approach. Most NAs are in the Latidude (Lat) and Longitude (Long) Variables
# It is possible to continue our Analysis without this additional Step.
# However, as long as the Data Set is very large, we can still apply this Command.
Criminal = na.omit(Criminal)

## DATES  ----------------------------------------------------------------------------------------------
# We wan to reformat the "Date" Variable, to be able to detect Patterns of Seasonality in
# the Data.

# First, we save "Date" (from a Character String) as "date" (POSIXct POSIXt) using the "lubridate"-Package:
# We check class of "Date":
class(Criminal$Date)
# We reate a new Variable "date" which should have the class "POSIXct" "POSIXt":
Criminal$date <- parse_date_time(x = Criminal$Date, orders = '%m/%d/%Y %I:%M:%S %p')
# We check the class of "date":
class(Criminal$date)

# Second, we extract the Time using the "chron"-Package:
Criminal$time<- times(format(Criminal$date, "%H:%M:%S"))
# We check the Time:
head(Criminal$time, n = 5)

# Thirdly, we create a "TimeCat"-Variable using the "chron"-Package.
# It indicates whether an incident happened in the Early Morning, in the Morning,
# in the Afternoon, or in the Late Evening/At Night.

# We build the Time Categories:
TimeCat<- chron(times = c("00:00:00", "06:00:00", "12:00:00", "18:00:00","23:59:00"))
# We use the cut()-Function from the "chron"-Package to cut the Times into the TimeCat Labels:
Criminal$TimeCat<- cut(Criminal$time,
                       breaks = TimeCat,
                       labels = c("00-06","06-12", "12-18", "18-00"),
                       include.lowest = TRUE)

# We get an overview over the Number of Crimes commited over specific Times of the Day:
table(Criminal$TimeCat)
# COMMENT:
# We can see that ther is an Increase in Criminal activity in the Afternoon and at Night.

# Fourth, we want to extract the Dates using the "chron"-Package, and resave the "date" variable:
Criminal$date <- date(Criminal$date)
# We check if this has worked:
head(Criminal$date, n = 5)


# Fifth, we extract the Weekdays using the base-R-function weekdays():
# We use the following line of code to tell R to use English Names for the Days and Months:
Sys.setlocale("LC_TIME", "English")
# We make a new Variable "Day":
Criminal$Day<- weekdays(Criminal$date, abbreviate = TRUE)
# We check if this has worked:
head(Criminal$Day, n = 5)

# Sixth, we extract the Months using the base-R-function months():
Criminal$Month <- months(Criminal$date, abbreviate = TRUE)
# Check if it has worked:
head(Criminal$Month, n = 5)

# We may want to use the following line of code in the Case that new NAs occured:
Criminal = na.omit(Criminal)

## AGGREGATE CRIMES  ----------------------------------------------------------------------------------------------
# We want to aggregate similar types of Crimes, to make our Analysis more clearly represented.

# First, we Check the number of unique Categories of Crimes:
length(unique(Criminal$PType))
# Second, we have a look at these Categories:
unique(Criminal$PType)

# First, save the Variable "PType" as a new Variable "Crime":
Criminal$Crime <- as.character(Criminal$PType)
# Then we proceed to build new aggregates:

# CAUTION ----------------------------------------------------------------------------------------------
# These Aggregates are chosen purely for Convenience, and may at Times seem arbitrary.
# Moreover, they are NOT in any way representative for how professional Police Detectives would aggregate
# these specific types of Crimes!

# In Reality, Data Scientists who build such Predictive Police Tools have to work with Police,
# Criminologists, Legal Advisors and many other Professionals to avoid making biased Predictions,
# as these may have grave Consequences on the Lives of affected Persons.

## NEW AGGREGATES   ----------------------------------------------------------------------------------------------
# We continue to build the aggregated Categories:

# "SEX": Crimes related to Prostitution, Human Traffiking, Sexual Assault
Criminal$Crime <- ifelse(test = Criminal$Crime %in% c("CRIM SEXUAL ASSAULT", "PROSTITUTION",
                                                      "SEX OFFENSE", "HUMAN TRAFFICKING",
                                                      "CRIMINAL SEXUAL ASSAULT"),
                         yes = "SEX", no = Criminal$Crime)

# "GTA": We rename Motor Vehicle Theft "GTA" (Grand Theft Auto)
Criminal$Crime <- ifelse(test = Criminal$Crime %in% c("MOTOR VEHICLE THEFT"),
                         yes = "GTA", no = Criminal$Crime )

# "NONV": Nonviolent Crimes and Misdemeanors
Criminal$Crime <- ifelse(test = Criminal$Crime %in% c("GAMBLING", "INTERFERENCE WITH PUBLIC OFFICER",
                                                      "INTIMIDATION", "LIQUOR LAW VIOLATION",
                                                      "OBSCENITY", "NON-CRIMINAL", "PUBLIC PEACE VIOLATION",
                                                      "PUBLIC INDECENCY", "STALKING", "NON - CRIMINAL",
                                                      "NON-CRIMINAL (SUBJECT SPECIFIED)",
                                                      "CONCEALED CARRY LICENSE VIOLATION",
                                                      "OTHER OFFENSE", "CRIMINAL TRESPASS", "RITUALISM"),
                         yes = "NONV", no = Criminal$Crime )

# "DAM" : Criminal Damage and Arson
Criminal$Crime <- ifelse(test = Criminal$Crime %in% c("CRIMINAL DAMAGE", "ARSON"),
                         yes = "DAM", no = Criminal$Crime )

# "FRAUD": Deceptive Practices
Criminal$Crime <- ifelse(test = Criminal$Crime %in% c("DECEPTIVE PRACTICE"),
                         yes = "FRAUD", no = Criminal$Crime)

# "DRUG": Narcotics, other narcotic Violations
Criminal$Crime <- ifelse(test = Criminal$Crime %in% c("NARCOTICS", "OTHER NARCOTIC VIOLATION"),
                         yes = "DRUG", no = Criminal$Crime )


# "VLNT": Violent Crimes, including Murder (Homicide)
Criminal$Crime <- ifelse(test = Criminal$Crime %in% c("KIDNAPPING", "WEAPONS VIOLATION",
                                                      "OFFENSE INVOLVING CHILDREN",
                                                      "HOMICIDE", "BATTERY"),
                         yes = "VLNT", no = Criminal$Crime )

# "GLAR": Grand Larcency, i.e., Burglary and Robbery (Not Petty Theft)
Criminal$Crime <- ifelse(test = Criminal$Crime %in% c("BURGLARY", "ROBBERY"),
                         yes = "GLAR", no = Criminal$Crime)

# "ASSLT": Assault
Criminal$Crime <- ifelse(test = Criminal$Crime %in% c("ASSAULT"),
                         yes = "ASSLT", no = Criminal$Crime)

# We check how many Categories of Criminal Behavior are left:
length(unique(Criminal$Crime))

# We can get an Overview over the Number of Crimes in our new Aggregates:
table(Criminal$Crime)

## FURTHER CHANGES ----------------------------------------------------------------------------------------------

# we make Arrests a binary values, which enables Prediction later on:
Criminal$ARR <- ifelse(test = as.character(Criminal$ARR) == "TRUE", yes = 1, no = 0)

## PLOTS ----------------------------------------------------------------------------------------------------

# We may want to choose a specific Year for our Exploratory Analysis, e.g, 2019:
# First, we choose a specific Year for our Analysis:
year <- 2019
# Then, we use the following Line of Code to automatically select all the Data in that Year.
Criminal <- Criminal[Criminal$date >= paste0(as.character(year), "-01-01") & Criminal$date <= paste0(as.character(year), "-12-31")]
# In the Code above, the Syntax of the data.table-Format is more convenient than traditional a data.frame
# We use the following line of Code to see if this has worked:
range(Criminal$date)

# NUMBER OF CRIMES  ----------------------------------------------------------------------------------------------------
# We want to see which Types of Criminal Behavior are most Common in the City of Chicago.
q1 <- qplot(reorder(Criminal$Crime, Criminal$Crime, function(x) length(x)*-1),
            xlab = "Type of Crime", main = paste("Crimes in Chicago by Type in ", as.character(year)),
            fill=I("dodgerblue1"), col=I("black"))+
     scale_y_continuous("Number of Crimes")+
     theme_bw()+
     theme(axis.text.x = element_text(angle = 90))
q1
ggsave("graphics/CrimesByType.png", plot = q1, width = 18, height = 12, units = "cm")
dev.off()
# COMMENT:
# We see that the type of Crime occuring most often is Theft, followed by Violent Crime.
# We have tried all years available, and the pattern remains largely the same.
# Note: Domestic Abuse and Sexual Assaults are considered by Experts to be notoriously underreported
# So there we have to keep in Mind that there may be a Bias in that Regard.


# CRIMES BY TIME OF Day  ----------------------------------------------------------------------------------------------------
# We want to check if there is a Pattern in Criminal Activity over Time of the Day:
q2 <- qplot(reorder(Criminal$TimeCat, Criminal$TimeCat, function(x) length(x)*-1),
            xlab = "Time of Day", main = paste0("Crimes in Chicago by Time of Day in ", as.character(year)),
            fill=I("maroon3"), col=I("black"))+
     scale_y_continuous("Number of Crimes")+
     theme_bw()+
     theme(axis.text.x = element_text(angle = 0))
q2
ggsave("graphics/CrimesByTimeOfDay.png", plot = q2, width = 18, height = 12, units = "cm")
dev.off()
# COMMENT:
# We conclude that Crimes in Chicago are more likely to occur in the Afternoon or in the Late Evening.
# Again, this Pattern remains extremely stable over time.


# CRIMES BY WEEKDAY  ----------------------------------------------------------------------------------------------------
# We want to check if there is a Pattern in Criminal Activity over the Days of the Week:

# First, we save the Days as factors:
Criminal$Day<- as.factor(Criminal$Day)
# Then ee make the plot:
q3 <- qplot(reorder(Criminal$Day, Criminal$Day, function(x) length(x)*-1),
            xlab = "Day of Week", main = paste0("Crimes in Chicago by Day of Week in ", as.character(year)),
            fill=I("springgreen4"), col=I("black"))+
     scale_y_continuous("Number of Crimes")+
     theme_bw()+
     theme(axis.text.x = element_text(angle = 0))
q3
ggsave("graphics/CrimesByWeekDay.png", plot = q3, width = 18, height = 12, units = "cm")
dev.off()
# COMMENT:
# We conclude that Crimes in Chicago are more likely to occur on FriDays and SaturDays than on WorkDays.
# Again, this Pattern ramained stable over all Years available.


# CRIMES BY Month   ----------------------------------------------------------------------------------------------------
# We want to check if there is a Pattern in Criminal Activity over the Months of a given Year:

# First, we make Month a factor Variable:
Criminal$Month<- as.factor(Criminal$Month)
# Then we make the Plot:
q4 <- qplot(reorder(Criminal$Month, Criminal$Month, function(x) length(x)*-1),
            xlab = "Month", main = paste0("Crimes in Chicago by Month in ", as.character(year)),
            fill=I("hotpink"), col=I("black"))+
     scale_y_continuous("Number of Crimes")+
     theme_bw()+
     theme(axis.text.x = element_text(angle = 0))
q4
ggsave("graphics/CrimesByMonth.png", plot = q4, width = 18, height = 12, units = "cm")
dev.off()
# COMMENT:
# We conclude that Crimes in Chicago are more likely to occur in July and August than in other Months
# This Pattern has remained stable over the Years too.


# HEATMAPS ----------------------------------------------------------------------------------------------------

# We want to combine what we have learned from the previous Plots by crating Heatmaps, which
# allow us to combine a temporal Dimension (Time) with the Categories of Crimes.


# HEAT MAP OF CRIMES BY TIME OF Day   ----------------------------------------------------------------------------------------------------
# We wan to create a Heatmap to investigate which Crimes occur during which Time of the Day.

# First, we build a "temp" file that aggregates the Crimes by the time intervals:
temp<- aggregate(x = Criminal$Crime,
                 by = list(Criminal$Crime, Criminal$TimeCat),
                 FUN = length)
# Second, we give the Variables in the "temp" file appropriates Names:
names(temp)<- c("Crime", "TimeCat", "Count")

# Then we plot the Heatmap:
q5 <- ggplot(temp, aes(x = Crime, y = factor(TimeCat)))+
     geom_tile(aes(fill = Count))+
     scale_x_discrete("Crime", expand = c(0,0))+
     scale_y_discrete("Time of Day", expand = c(0,-2))+
     scale_fill_gradient("No. of Crimes", low = "white", high = "firebrick2")+
     theme_bw()+
     ggtitle(paste("Crimes in Chicago by Time of Day in ", as.character(year)))+
     theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA),
           axis.text.x = element_text(angle = 90))
q5
ggsave("graphics/HMCrimeByDay.png", plot = q5, width = 19, height = 12, units = "cm")
dev.off()
# COMMENT
# The Heatmap above illustrates how Theft is highly likely to occur in the Early Afternoon in Chicago,
# whereas Violoent Crime is more likely to occur in the Evening.


# HEAT MAP OF CRIMES BY Day OF WEEK ----------------------------------------------------------------------------------------------------
# We want to create a Heatmap that illustrates what type of Crime occurs during which Day of the Week.

# First, we summarise tha data using the ddply() function from the "dplyr"-package:
temp <- ddply(Criminal, .(Crime, Day), summarise, Count = length(date))

# Then, we plot the Heatmap:
q6 <- ggplot(temp, aes(x = Crime, y = Day, fill = Count))+
     geom_tile(aes(fill = Count))+
     scale_x_discrete("Crime", expand =c(0,0))+
     scale_y_discrete("Day of week", expand = c(0,-2))+
     scale_fill_gradient("No. of Crimes", low = "white", high = "firebrick2")+
     theme_bw()+
     ggtitle(paste0("Crimes in Chicago by Day of Week in ", as.character(year)))+
     theme(panel.grid.major = element_line(colour=NA), panel.grid.minor=element_line(colour=NA),
           axis.text.x = element_text(angle = 90))
q6
ggsave("graphics/HMCrimeByWeekday.png", plot = q6, width = 19, height = 12, units = "cm")
dev.off()
# COMMENT
# The Heatmap above illustrates that Theft is likely to occur on Fridays and that violent Crime
# occurs mostly on Weekends.


# HEAT MAP OF CRIMES BY Month ----------------------------------------------------------------------------------------------------
# We want to illustrate which Crimes are most prevalent during which Months:

# First, we build another "temp" file with the summaryBy()-function from the "doBy"-Package:
temp <- summaryBy(Case~Crime+Month, data = Criminal, FUN = length)
# Then, we give the Variables apporpriate Names:
names(temp)[3]<- "Count"

# Then we plot the Heatmap:
q7 <- ggplot(temp, aes(x = Crime, y = Month, fill = Count))+
     geom_tile(aes(fill = Count))+
     scale_x_discrete("Crime", expand = c(0,0))+
     scale_y_discrete("Month", expand = c(0,-2), guide = )+
     scale_fill_gradient("No. of Crimes", low = "white", high = "firebrick2")+
     theme_bw()+
     ggtitle(paste0("Crimes in Chicago by Month in ", as.character(year)))+
     theme(panel.grid.major = element_line(colour=NA), panel.grid.minor = element_line(colour=NA),
           axis.text.x = element_text(angle = 90))
q7
ggsave("graphics/HMCrimeByMonth.png", plot = q7, width = 19, height = 12, units = "cm")
dev.off()
# COMMENT
# The Heatmap above tells us for example that the most common Types of Crime, Theft and Violent
# Crime are most likely to occur in in the Months of Summer (June, July, August) or


# MAP  ----------------------------------------------------------------------------------------------
# We have already examined the temporal Dimension of the Crime in Chicago.
# Next, we would like to examine the spatial Dmension.
# We want to build a Map of the Crimes in Chicago with the "maptools", "rgeos" and "rgal"-Packages

# First, we save the shapefile of the Police Beats:
PoliPBeats <- readOGR("data/policebeats/geo_export_8e91c3c3-ae4c-4556-95d0-ef3c3e62fdce.shp")
summary(PoliPBeats)
plot(PoliPBeats)

# next, we save the shapefile of the Police Stations:
station.shp <- readOGR("data/policestations/PoliceStationsDec2012.shp")
summary(station.shp)
plot(station.shp)

# We have to Transform the Coordinates so that they all three Sources of Data
# use the Same Projection:
PoliPBeats <- spTransform(PoliPBeats,
                          CRS("+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.9999749999999999 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"))
# We check the Result
summary(station.shp)

# We prepare the Map for Potting with the fortify()-Function from the "ggfortify"-Package:
PoliPBeats.df <- fortify(PoliPBeats)
PoliData    <- fortify(station.shp)

# We make a data Set that aggrregates the Categories of Criminal Behavior in Chicago:
crime.agg<- ddply(Criminal, .(Crime, ARR, PBeat, date, X_Coord,Y_Coord, TimeCat, Day, Month),
                  summarise, Count = length(date),.progress = "text")

# We begin our Plot by plotting  the Police Stations:
q8 <- ggplot(PoliPBeats.df, aes(x =long, y = lat))+
     geom_path(aes(group = group))+
     theme_bw()+
     geom_point(data = PoliData, aes(x = long, y = lat))
q8
ggsave("graphics/BeatsMap.png", plot = q8, width = 17, height = 12, units = "cm")

# We create a Variable "toDay", which is the first Date in our Data Set:
toDay<- crime.agg[1, "date"]

# Next, we make the Coordinates numeric so that we may use them for Plotting:
crime.agg$X_Coord <- as.numeric(crime.agg$X_Coord)
crime.agg$Y_Coord <- as.numeric(crime.agg$Y_Coord)

# We combine the Plot of the Police Stations with another Plot with the Crime Categories:
q8 <- q8 + geom_point(data = subset(crime.agg, as.character(crime.agg$date) == toDay),
                      aes(x = X_Coord, y = Y_Coord, color = Crime))
q8
# Finally, we make the Title and set the Theme of our Map:
q8 <- q8 + theme_bw()+
     ggtitle(paste("Crime in Chicago on", weekdays(toDay), ",", months(toDay),
                   substr(toDay, 9, 12),
                   substr(toDay, 1, 4)))+
     theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.border     = element_blank(),
           axis.title.x     = element_blank(),
           axis.title.y     = element_blank(),
           axis.text.x      = element_blank(),
           axis.text.y      = element_blank(),
           axis.ticks       = element_blank())
q8
ggsave("graphics/CrimeMap.png", plot = q8, width = 17, height = 12, units = "cm")
dev.off()
# COMMENT:
# We expected Criminal Activites to be concentrated in the poorer Southern Part of Chicago,
# the map clearly shows that Crime is ubquitious in Chicago.

# However, the Types of Crime that occur most frequently, Assault and Theft, seem to be concentrated
# in the more populous police parts of the City.
# This indicates that there may be a predictive Value to the spacial Dimension of the Data Set.


# Modeling   ----------------------------------------------------------------------------------------------

# We want to build a very simple Model that predicts Criminal Activity in General, with
# the Police Beats as the spacial Dimension and using Past Criminal Activity in these
# Regions as an Indicator for Present and Future Activity.

# First, we create a Data Set that includes the unique Combinations of the different Police Beats
# and Daates.

# We sort the unique Values of the Police Beats and the "date"-Variable:
PBeats<- sort(unique(crime.agg$PBeat))
dates<- sort(as.character(unique(crime.agg$date)))

# Then we save these Combinations as a "temp"-file with the expand.grid()-function:
temp <- expand.grid(PBeats, dates)
# We give the Variables appropriate Names:
names(temp) <- c("PBeat", "date")
# We order the data frame by the Police Beats:
temp <- orderBy(~PBeat, data = temp)

# We aggregate the Dataa by Police Beats and Types of Crime:
ModelDat<- aggregate(crime.agg[, c("Count", "ARR")],
                     by = list(crime.agg$PBeat, as.character(crime.agg$date)), FUN = sum)
# We rename the Variables appropriately:
names(ModelDat)<- c("PBeat", "date", "Count", "ARR")

# We then merge the Crimes data with the "temp"-file:
ModelDat <- merge(temp, ModelDat, by = c("PBeat", "date"), all.x = TRUE)

# We replace NAs in the Data with Zero to indicate that there were no Crimes,
# and therefore no Arrests on those specific Dates in those specific Police Beats:
ModelDat$Count[is.na(ModelDat$Count)] <- 0
ModelDat$ARR[is.na(ModelDat$ARR)] <- 0

# Use want to use WeekDays and Months as Predictors, because we have detected
# a Pattern of Seasonality during our Exploratory Analysis:
ModelDat$Day   <- weekdays(as.Date(ModelDat$date), abbreviate = TRUE)
ModelDat$Month <- months(as.Date(ModelDat$date), abbreviate = TRUE)

# We build a little Function that helps us filter the Data set for a specific Number
# of past Observations, which we then want to average to include as a predictor Variable:
pastDays<- function(x) {
     c(0, rep(1, x))
}

# We want to use the Group Averages Function ave() to use Past Criminal Activity as new Predictors:
# It allows us to average over the Number of Criminal Incidents on a given Date, grouped by
# the Police Beats where these Crimes occured.

# First, we want to 1 Day of Past Criminal Activity as a Predictor Variable:
ModelDat$Prev.Crim1D <- ave(ModelDat$Count, ModelDat$PBeat,
                            FUN =function(x) filter(x, pastDays(1), sides = 1))
# Next, we want to use 1 Week of Past Criminal Activity a a Predictor Variable:
ModelDat$Prev.Crim1W <- ave(ModelDat$Count, ModelDat$PBeat,
                            FUN = function(x) filter(x, pastDays(7), sides = 1))
# Finally, we want to use 1 Month of Past Criminal Activity as a Predictor Variable:
ModelDat$Prev.Crim1M <- ave(ModelDat$Count, ModelDat$PBeat,
                            FUN = function(x) filter(x, pastDays(30), sides = 1))

# If we make the Assumption that Criminal Activity exhibits Mean Reversion, it is appropriate
# to replace missing Values (NAs) with the Mean instead of simply deleting the Observation.
# Removing NAs has worked too, but this Method allows us to keep more Observations.
meanNA<- function(x) {
     mean(x, na.rm = TRUE)
}

# Here, we use the function which we built above to fill the missing Values with the Means.
# Particularly, we want to build three different Data Sets that include different Predictors:

# The first Data Set contains only 1 Day of Previous Criminal Activities in the Police Beats.
ModelDat$Prev.Crim1D <- ifelse(is.na(ModelDat$Prev.Crim1D),
                               meanNA(ModelDat$Prev.Crim1D),
                               ModelDat$Prev.Crim1D)
# The second Data Set contains one Week of Previous Criminal Activites in the Police Beats.
ModelDat$Prev.Crim1W <- ifelse(is.na(ModelDat$Prev.Crim1W),
                               meanNA(ModelDat$Prev.Crim1W),
                               ModelDat$Prev.Crim1W)
# The third Data Set contains one Month of Previous Criminal Activities in the Police Beats.
ModelDat$Prev.Crim1M <- ifelse(is.na(ModelDat$Prev.Crim1M),
                               meanNA(ModelDat$Prev.Crim1M),
                               ModelDat$Prev.Crim1M)


# We have already decided that we want to include Past Criminal Activiy as Predictor Variables.

# However, if we believe that Criminals are Rational, we would expect that they would decrease
# their Activities in times when Arrests by the Police have been high, i.e., we would expect them
# to "lay low" for a while, until they resume their Criminal Activities.

# We can test this Hypothesis by evaluating the Predictive Power of the Number of Arrests over
# the Past Month in the Police Beats:

# First, we build a Data Set that contains the average Number of Arrests in the Police Beats
# over the past Months:
ModelDat$Past1MArr <- ave(ModelDat$ARR, ModelDat$PBeat,
                          FUN = function(x) filter(x, pastDays(30), sides = 1))
# Again, we Replace missing VAlues with the Mean:
ModelDat$Past1MArr<- ifelse(is.na(ModelDat$Past1MArr),
                            meanNA(ModelDat$Past1MArr),
                            ModelDat$Past1MArr)

# There may be a Problem, because the Correlation between Past Arrests and Past Crime is very high:
cor(ModelDat$Prev.Crim1M, ModelDat$Past1MArr)

# This would mean that we cannot test our Hypothesis, which states that Criminals are rational
# Observers of the Police's Behavior and that they increase their Activities when Arrests have
# low for a certain Amount of Time.

# To mitigate this Problem, we devide Past Arrests by Past Crimes.
# This new Variable can be thought of as an Indicator of the Police's Activity in a Region,

# Therefore this new Varialbe allows us to test if low Police Presence induces Criminals
# to increase their Activities and vice versa.

# We begin by building this Variable of Police Activity:
ModelDat$PolAct <- ifelse(ModelDat$Prev.Crim1M == 0, 0,
                          ModelDat$Past1MArr/ModelDat$Prev.Crim1M)

# To test the Hypothesis described above, we also need to include an Interacton term between
# Previous Criminal activities over the Past Month and over the Past Week.

# The Interacton Term would then indicate an increasing or decreasing Trend in Crime.
ModelDat$CriminalTrend <- ifelse(ModelDat$Prev.Crim1M == 0, 0,
                                 ModelDat$Prev.Crim1W/ModelDat$Prev.Crim1M)

# We combine Spring, Summer, Fall and Winter Months to represent the Seasonal Pattern which
# we have detected in our Exploratory Analysis.
ModelDat$Seasonality <- as.factor(ifelse(ModelDat$Month %in% c("Mar", "Apr", "May"), "spring",
                                         ifelse(ModelDat$Month %in% c("Jun", "Jul", "Aug"), "summer",
                                                ifelse(ModelDat$Month %in% c("Sep", "Oct", "Nov"), "fall",
                                                       "winter"))))

# We can make a Correlation Plot with the "psych"-Package to see if the Predictor Variables
# which we have just created have any Predictive Power:
CorrModel<- cor(ModelDat[, c("Count", "Prev.Crim1D", "Prev.Crim1W", "Prev.Crim1M", "PolAct", "CriminalTrend")])

# We plot the Correlations:
png(file = "graphics/CorrModel.png", width = 600, height = 350)
cor.plot(CorrModel)
dev.off()
# COMMENT
# The Correlation Plot indicates that there is a Seasonal Pattern in the Criminal Activity in the
# Police Beats. This can be seen because the Correlation between Criminal Activity on the prevous
# Day, Week, and Month is high.

## TESTING    ----------------------------------------------------------------------------------------------
# We want to build a Training and Test Data set to test our Model.

# We begin by ordering the Data Set by the Variable "date":
ModelDat <- orderBy(~date, data = ModelDat)

# We can see how the Data for our Model looks like:
head(ModelDat, n = 5)

# Then we create a 70%-Training Data Set:
rows     <- c(1:floor(nrow(ModelDat)*0.7))
# We want to use the Remaining Data as our Test Data Set:
TestDat  <- ModelDat[-rows, ]
ModelDat <- ModelDat[ rows, ]

# We want to build a simple Linear Model to test the Predictive Power of our Covariates:
CriminalModel1 <- glm(Count ~ Prev.Crim1D +
                           Prev.Crim1W +
                           Prev.Crim1M +
                           PolAct +
                           CriminalTrend +
                           factor(Day) +  # R automatically creates Dummy Variables for factors
                           Seasonality,   # Seasonality already is a factor Variable, so Dummys are automatically generated
                      data = ModelDat)

# We can check the the Summary of our Model:
summary(CriminalModel1)

# We create Predicitons for our Model:
CriminalModel.pred1 <- predict(CriminalModel1, TestDat, type = "response")

# Then we calculate the Root Mean squared Error (RMSE):
RMSE1 <- sqrt(mean((TestDat$Count - CriminalModel.pred1)^2))

# We print the Results:
print(RMSE1)


# We want to include the Square of Criminal Activity over the Past Month in our Model, because it was
# highly significant in the last Model:
CriminalModel2 <- glm.nb(Count ~ Prev.Crim1D +
                              Prev.Crim1W +
                              Prev.Crim1M +
                              CriminalTrend +
                              PolAct +
                              factor(Day) +
                              Seasonality +
                              I(Prev.Crim1M^2), # New Predictor Variable
                         data = ModelDat)

# Again, we check the Summary our this new Model:
summary(CriminalModel2)
# Make Prediction with the new Model:
CriminalModel.pred2 <- predict(CriminalModel2, TestDat, type = "response")
# Again, we want to use the Rood Mean Squared Error to estimate the Expected Deviation
# of our Prediction from the true Values:
RMSE2 = sqrt(mean((TestDat$Count - CriminalModel.pred2)^2))
print(RMSE2)

# We can compare the two Models:
print(cbind(RMSE1, RMSE2))
print(RMSE1-RMSE2)
# There is a slight increase in the RMSE in the second Model. We therefore prefer the first Model.

## VALIDATION  ----------------------------------------------------------------------------------------------
# To better evaluete the Predicitve Performance of our Model, we want to divide our Data Set by the
# into Deciles.

# First, we create a "Validation" data set with acutal and Predicted Crime Counts:
Validation<- data.frame(TestDat$Count, CriminalModel.pred1)
# Give the Variables appropriate Names:
names(Validation) <- c("Actual", "Predicted")
# Then we make Boxes of Deciles:
Validation$Boxes  <- with(Validation, cut(Predicted,
                                          breaks = quantile(Predicted, probs = seq(0, 1, 0.1)), # Make Deciles
                                          include.lowest = TRUE, labels = c(1:10))) # Make Labels

# We proceed to aggregate the "Validation" Data Set by the Deciles:
Validation <- aggregate(Validation[, c("Actual", "Predicted")], by = list(Validation$Boxes), FUN = mean)

# Finally, we plot the Validation:
png(file="graphics/Validation.png", width = 600, height = 350)
plot(Validation$Predicted, col = "red", type = "l", lwd = 2, ylab = "Avg. of Pred./Act. Crimes in Decile",
     xlab = "Predicted Crimes Decile", main = "Actual vs. Precicted")
lines(Validation$Actual, col = "blue", lwd = 2)
legend("topright", c("Actual", "Predicted"), col = c("blue", "red"), lwd = c(1.5, 1.5), bty = "n")
dev.off()
# COMMENT:
# The Actual and Predicted values are very close together in the Middle but diverge slightly
# near the Extremes.

# Therefore we have shown that past Criminal Activity, Seasonal Patterns (Summer/Winter, Weekend/
# Workdays), past Police Precence and increasing or decreasing Trends in criminal Activity
# can at least act as a Guide in for Police Forces.