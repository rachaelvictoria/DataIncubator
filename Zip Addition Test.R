raw_data <- read.csv("Degree_Data_By_Race.csv", header = TRUE, stringsAsFactors = FALSE)
#Original Column Headers
#  [1] "Year"                                                                  
# [2] "Academic.Institution..standardized."                                   
# [3] "FICE"                                                                  
# [4] "City.location.of.institution"                                          
# [5] "State"                                                                 
# [6] "Gender"                                                                
# [7] "Race...Ethnicity..standardized."                                       
# [8] "Academic.Discipline..Broad..standardized."                             
# [9] "Level.of.Degree.or.Other.Award"                                        
# [10] "Degrees.Awards.Conferred.by.Race..NSF.population.of.institutions..Sum."
colnames(raw_data) <- c("year", "school", "FICE", "city","state",  "gender", "race", "major_area", "degree", "num_deg_by_race", "zip")
first_set <- raw_data[,1:9]
num_deg_by_race <- c(as.numeric(raw_data[, 10]))
zip <- raw_data[,11]
raw_data_numeric <- data.frame(first_set, num_deg_by_race, zip)

#Aggregate over the five year period
raw_data_year_agg <- aggregate(num_deg_by_race ~ school + city + state + race + major_area + degree + zip, raw_data_numeric, sum)

#subset the data to only bachelor degrees
bach_data <- raw_data_year_agg[which(raw_data_year_agg$degree == "Bachelor's Degrees"),]
