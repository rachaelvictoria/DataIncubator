#Goal - look at the number of STEM bachelor degrees granted by race for all of 
#bachelor's granting institutions in the US over a four year period (2009 - 2013)
#Compare this to the demographic makeup of all of the 12th graders summarized 
#the same time frame. 


#################Get school data######################################
#Background information: Data was taken from IPEDS Completions Survey by Race/Sum of Degrees/Awards Conferred by Race
# Classification variables:
#         -Year
# Filter for following values: 5 most recent years
# 
# -Academic Institution (standardized)
# Filter for following values: Brown University; California Institute of Technology; Carnegie Mellon University; Case Western Reserve University; Clarkson University; Clemson University; Colorado School of Mines; Cornell University, All Campuses; Drexel University; Georgia Institute of Technology, Main Campus; Illinois Institute of Technology; Iowa State University; Johns Hopkins University; Lehigh University; Massachusetts Institute of Technology; Michigan Technological University; North Carolina State University at Raleigh; Pennsylvania State U, Main Campus; Princeton University; Purdue University, Main Campus; Rensselaer Polytechnic Institute; Rice University; SUNY College of Environmental Sci & Forestry; Stanford University; Stevens Institute of Technology; Texas A&M University Main Campus; University of California-Berkeley; University of California-Davis; University of California-Irvine; University of California-Los Angeles; University of California-San Diego; University of Illinois at Urbana-Champaign; University of Michigan at Ann Arbor; University of Missouri, Rolla; University of Rochester; University of Tulsa; Virginia Polytechnic Institute and State Univ; Worcester Polytechnic Institute
# 
# -Highest Degree (standardized)
# Filter for following values: Doctorate-granting Institutions
# 
# -Gender
# Filter for following values: All values
# 
# -Race & Ethnicity (standardized)
# Filter for following values: Black, Non-Hispanic; American Indian or Alaska Native; Hispanic; Other/Unknown Races & Ethnicities
# 
# -Academic Discipline, Detailed (standardized)
# Filter for following values: Aerospace Engineering; Chemical Engineering; Civil Engineering; Electrical Engineering; Mechanical Engineering; Materials Engineering; Industrial Engineering; Other Engineering; Astronomy; Chemistry; Physics; Other Physical Sciences; Atmospheric Sciences; Mathematics and Statistics; Computer Science; Biological Sciences
# 
# -Level of Degree or Other Award
# Filter for following values: Doctorate Degrees; Doctorate Degree-Research/Scholarship; Doctorate Degree-Professional Practice; Doctorate Degree-Other; Master's Degrees
# 
# 
# Table was created by the WebCaspar website (https://ncsesdata.nsf.gov/webcaspar/) on Wed, 8 Apr 2015 16:47 -0400

raw_data <- read.csv("Degree_Data_By_Race.csv", header = TRUE, stringsAsFactors = FALSE)
#Original Column Headers
# [1] "Year"                                                                  
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
zip <- as.character(raw_data[,11])
raw_data_numeric <- data.frame(first_set, num_deg_by_race, zip)

#Aggregate over the five year period
raw_data_year_agg <- aggregate(num_deg_by_race ~ school + city + state + race + major_area + degree + zip, raw_data_numeric, sum)

#subset the data to only bachelor degrees
bach_data <- raw_data_year_agg[which(raw_data_year_agg$degree == "Bachelor's Degrees"),]
#subset data by race
bach_data_black <- bach_data[which(bach_data$race == "Black, Non-Hispanic"),]
bach_data_native <- bach_data[which(bach_data$race == "American Indian or Alaska Native"),]
bach_data_asianPI <- bach_data[which(bach_data$race == "Asian or Pacific Islander"),]
bach_data_hispanic <- bach_data[which(bach_data$race == "Hispanic"),]
bach_data_white <- bach_data[which(bach_data$race == "White, Non-Hispanic"),]

#Calculate the total number of STEM bachelor degrees given over a five year period
Num_deg <- aggregate(num_deg_by_race ~ school + city + state + zip, bach_data, sum)
colnames(Num_deg) <- c("school", "city", "state","zip",  "total STEM degrees")
#Calculate the total number of STEM bachelor degrees given over a five year period by race
Num_deg_black <- aggregate(num_deg_by_race ~ school + city + state + zip, bach_data_black, sum)
colnames(Num_deg_black) <- c("school", "city", "state","zip", "total degrees Black")
Num_deg_native <- aggregate(num_deg_by_race ~ school + city + state + zip, bach_data_native, sum)
colnames(Num_deg_native) <- c("school", "city", "state","zip", "total degrees Native")
Num_deg_asianPI <- aggregate(num_deg_by_race ~ school + city + state + zip, bach_data_asianPI, sum)
colnames(Num_deg_asianPI) <- c("school", "city", "state","zip", "total degrees AsianPI")
Num_deg_hispanic <- aggregate(num_deg_by_race ~ school + city + state + zip, bach_data_hispanic, sum)
colnames(Num_deg_hispanic) <- c("school", "city", "state","zip", "total degrees Hispanic")
Num_deg_white <- aggregate(num_deg_by_race ~ school + city + state + zip, bach_data_white, sum)
colnames(Num_deg_white) <- c("school", "city", "state","zip", "total degrees White")


#create a single table with the total number of STEM bachelor degrees and the total by race
total <- merge(Num_deg, Num_deg_black)
total <- merge(total, Num_deg_native)
total <- merge(total, Num_deg_asianPI)
total <- merge(total, Num_deg_hispanic)
total <- merge(total, Num_deg_white)

#list all previous variables 
to_remove <- ls()
#Reorder list by total number of STEM degrees granted, descending
Degree_School_Table <- total[order(-total[,5]),]

#Do a bit of cleanup
rm(list=to_remove)

##########################################################################################################
# Note at this point that the University of Phoenix is the top STEM undergrad                            #
# degree granting institution in the US. As University of Phoenix has made a business of                 #                
# offering online degrees cannot get demographic data on the area surrounding this particular school.    #
# Will therefore eliminate this value and use top values 2-11                                            #
##########################################################################################################


########################Get table of zip codes and counties/state#########################################
#Going to use a table of zip codes to correlate school zip code with county. 
#Can then use county demographics from US census


#Going to need to convert state abbreviations found in the zip code table to 
#state names,Getting the state abbreviations in the zip code table to state names, found a table online
fileUrl_states <- "http://www.fonz.net/blog/wp-content/uploads/2008/04/states.csv"
download.file(fileUrl_states, destfile = "./states.csv")
us_states <- read.csv("states.csv", header = TRUE)
colnames(us_states)<- c("full", "state")


#First use US postal service database of zipcodes
#downloaded from http://www.unitedstateszipcodes.org/zip-code-database/ on 4/12/15

fileUrl <- "http://www.unitedstateszipcodes.org/zip_code_database.csv"
download.file(fileUrl, destfile = "./zip_code_database.csv")
zip_codes_table <- read.csv("zip_code_database.csv", header = TRUE)
zips <- zip_codes_table$zip
zips <- formatC(zips, width = 5, flag = "0")
zips_table <- data.frame(zips, zip_codes_table$state, zip_codes_table$county)
colnames(zips_table)<- c("zip", "state", "county")

zips_table <- zips_table[order(zips_table$state),]
#Select the subset of state names that exist in the US, eliminate Military and 
#sort alphabetically
list_states <- us_states[,2]
zips_table <- with(zips_table, zips_table[state %in% list_states,])
colnames(zips_table) <- c("zip", "state", "county")
county_zips <- gsub(x = zips_table$county, pattern = " County", replace = "")

zips_table <- data.frame(zips_table[,1:2], county_zips)
colnames(zips_table) <- c("zip", "state", "county")
zips_table <- zips_table[order(zips_table$state),]

#Now convert state abbreviations to actual state names
zips_table <- merge(zips_table, us_states, by = "state")
zips_table <- zips_table[,2:4]
colnames(zips_table) <- c("zip", "county", "state")




##########Get 12th grader demographics by county from (2009-2013)#############
# High_Schoolers_by_county_Race_data Notes                                   #
# DOwnloaded using US Census Data Ferret Interface                           #
# All values are for number of students enrolled in grade 12                 #
#                                                                            #        
#                                                                            # 
# Col 1 = County
# Col 2  = State
# COl 3  = B14007I_016E -- Hispanic or Latino                                 #
# COl 4  = B14007H_016E -- WHite Alone, Not HIspanic                          #
# Col 5 = B14007B_016E - Black or Arfican American Alone                     #
# COl 6 = B14007C_016E- American Indian and Alaska Native ALONE              #
# Col 7 = B14007D_016E -- Asian ALone                                        #
# Col 8 = B14007E_016E -- Native Hawaiian and Other Pacific IslanderAlone    #
#                                                                            #
#                                                                            #
# DATA SET - American Community Survey, 2009-2013, 5 Year Summary File       #
#                                                                            #
# Accessed 4/11/2015 @ 9:14pm                                                #
##############################################################################

# #High Schooler Demographic Data
Race_zip_raw <- read.csv("High_Schoolers_by_county_Race_data.csv")
colnames(Race_zip_raw) <- c("county","state", "hispanic", "white", "black", "native", "asian", "hawaiianPI")
#Values in total table aggregate Asian and Pacific Islander students, need to do
#the same here
asianPI <- Race_zip_raw$asian + Race_zip_raw$hawaiianPI
#Race zip has the counties listed as "Name County", need to clean this up for direct comparison
county <- gsub(x = Race_zip_raw$county, pattern = " County", replace = "")
Race_zip <- data.frame(county, Race_zip_raw[, 2:6], asianPI)
Race_zip <- Race_zip[order(Race_zip$state),]
colnames(Race_zip) <- c("county", "state", "county_12th_hispanic", "county_12th_white", "county_12th_black", "county_12th_native", "county_12th_asianPI")

# #At this point:
#  -- zips_table has the zip code, the county and the state
#  -- Race_zip has the county, state, and number of 12th graders by race
#  -- Degree_School_Table has the zip code, city and state

#Add the county value to total

Degree_School_Table <- merge(Degree_School_Table, zips_table, by = "zip")
#####################names(intermediate_table)##########################################
# [1] "zip"                    "school"                 "city"                  #
# [4] "state.x"                "total STEM degrees"     "total degrees Black"   #
# [7] "total degrees Native"   "total degrees AsianPI"  "total degrees Hispanic"#
# [10] "total degrees White"    "county"                 "state.y"              #
#################################################################################

Degree_School_Table <- data.frame(Degree_School_Table[,2], Degree_School_Table[,1],Degree_School_Table[, 4], Degree_School_Table[,11], Degree_School_Table[, 5:10])
colnames(Degree_School_Table) <- c("school", "zip", "state", "county", "total_STEM_degrees", "total degrees Black", "total degrees Native", "total degrees AsianPI", "total degrees Hispanic", "total degrees White")

#Create id to correlate the Degree_School_Table and the Race_zip tables
Degree_School_Table <- within(Degree_School_Table, id <- paste(county, state, sep = ", "))
Race_zip <- within(Race_zip, id <- paste(county, state, sep = ","))
Race_zip$total <- rowSums(Race_zip[,3:7])

#Sort table of schools by total number of STEM degrees for all races, descending
Degree_School_Table <- Degree_School_Table[order(-Degree_School_Table[,5]),]
write.csv(file = "./Final_Degree_University_Table.csv", x = Degree_School_Table)
write.csv(file = "./Final_Race_zip_table.csv", x = Race_zip)


#Select the top ten STEM bachelors granting schools in the US
top_ten <- Degree_School_Table[1:10,]
top_ten_ids <- Degree_School_Table$id[1:10]

#Use ID values to get the demographics of 12th graders in the counties the schools
#are located and sort them in the same order as the top_ten STEM granting schools
top_ten_12th <- with(Race_zip, Race_zip[id %in% top_ten_ids,])
top_ten_12th <- top_ten_12th[order(match(top_ten_12th$id, top_ten$id)),]
#Do a quick bar chart of the breakdown of degrees granted by race

#total_for_plot <- data.frame(Black = total[,5], Native = total[,6], AsianPI = total[,7], Hispanic = total[,8], White = total[,9])
Degree_school_plot <- data.frame(Black = top_ten[,6]/top_ten[,5], Native = top_ten[,7]/top_ten[,5], AsianPI = top_ten[,8]/top_ten[,5], Hispanic = top_ten[,9]/top_ten[,5], White = top_ten[,10]/top_ten[,5])
school_plot_12th <- data.frame(top_ten_12th[,5]/top_ten_12th[,9], top_ten_12th[,6]/top_ten_12th[,9], top_ten_12th[,7]/top_ten_12th[,9], top_ten_12th[,3]/top_ten_12th[,9], top_ten_12th[,4]/top_ten_12th[,9])
colnames(school_plot_12th) <- c("12th Black", "12th Native","12th AsianPI", "12th Hispanic", "12th White" )
names <- top_ten[1:10,1]
rownames(Degree_school_plot)<- names
rownames(school_plot_12th) <- names
xmax <- signif(max(top_ten[,5]),1)

title = c("Number of STEM Bachelor's Degrees Granted, by Race, \nfor Top 10 STEM Degree Granting Schools (2009 - 2013)")
titleB = c("Number of 12th Graders enrolled within the same county\n as Top 10 STEM Degree Granting Schools (2009-2013) ")
mymat <- as.matrix(Degree_school_plot)
mymatB <- as.matrix(school_plot_12th)
mymat_t <- t(mymat)
mymatB_t <- t(mymatB)
par(mar = c(5, 20, 2, 5))
plot_colors <- c("darkblue", "gold1", "indianred3", "chartreuse", "slateblue")
title(title, line = -20)
legend_list <- list(x = 1.2, y = 15, bty = "n")
barplot(mymat_t, xlim = c(0, 1), main = title, adj = 0, horiz = TRUE, names.arg = names, las = 1, col = plot_colors, legend = names(Degree_school_plot))

barplot(mymatB_t, xlim = c(0, 1), main = titleB, adj = 0, horiz = TRUE, names.arg = names, las = 1, col = plot_colors, legend = names(school_plot_12th), args.legend = legend_list)


