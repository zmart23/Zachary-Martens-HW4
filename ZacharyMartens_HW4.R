library(tidyverse) #load requisite libraries
library(tidycensus) #load requisite libraries
library(dplyr) #load requisite libraries

data = read.csv("airport_pairs.csv") #load airport pairs dataset

##Question 1##
rdu_data = filter(data, origin_name=="Raleigh-Durham International" | dest_name=="Raleigh-Durham International") #filter airport pairs dataset to only include flights with origin or destination of RDU
rdu_data_table = data.table::data.table(rdu_data) #convert new filtered dataset into a data table

rdu_data_origin = filter(data, origin_name=="Raleigh-Durham International") #filter data to include data only with a departure airport of RDU
arrange(rdu_data_origin,passengers) #sort data to see the most popular non-stop destination by seeing which destination has the most passengers coming from RDU departure
# After sorting the data, it is shown that Hartsfield-Jackson Atlanta International Airport is the most popular destination from RDU Airport.

rdu_data_10k = rdu_data %>% filter(passengers >= 10000) #filter data with RDU destination and origin to include only flight pairs with 10,000 or more passengers
rdu_data_10_filter = rdu_data_10k %>% select(origin, dest, passengers) #select columns from rdu_data_10k to make a table for report that just shows origin, destination, and passengers for routes with more than 10,000 passengers
rdu_data_10_filter = rdu_data_10_filter %>% arrange(desc(passengers)) #arrange rdu_data_10_filter by passengers in descending order to allow for easier analysis
rdu_data_10_filter #call rdu_data_10_filter for viewing

##Question 2##
decennial_2020_vars <- load_variables(
  year = 2020, 
  "pl", 
  cache = TRUE
)# 2020 Decennial Census Variables

metromicro_pop=get_decennial(geography = "cbsa",
                                variables = "H1_001N",
                                year = 2020) #population of all metro and micro areas
metromicro_pop_origin = rename(metromicro_pop, "origin_cbsa" = "GEOID") #rename GEOID column to origin_cbsa in metromicro_pop dataset in order to allow for left_join
metromicro_pop_origin_int = metromicro_pop_origin %>% mutate_at(c('origin_cbsa'),as.numeric) #convert origin_cbsa column in metromicro_pop_origin from character to integer to allow for left_join

rdu_data_cbsa = left_join(rdu_data_10k,metromicro_pop_origin_int) #join metromicro_pop_int and filtered rdu_data_10k by origin_cbsa to form rdu_data_cbsa
rdu_data_cbsa = rename(rdu_data_cbsa, "origin_pop" = "value") #rename value column to make next join for dest_cbsa less confusing
rdu_data_cbsa = rdu_data_cbsa %>% select(-variable) #remove unnecessary variable column from rdu_data_cbsa

metromicro_pop1 = get_decennial(geography = "cbsa",
                                variables = "H1_001N",
                                year = 2020) #population of all metro and micro areas
metromicro_pop_dest = rename(metromicro_pop1, "dest_cbsa" = "GEOID") #rename GEOID column to dest_csba in metromicro_pop dataset in order to allow for left_join
metromicro_pop_dest_int = metromicro_pop_dest %>% mutate_at(c('dest_cbsa'),as.numeric) #convert dest_cbsa column in metromicro_pop_dest from character to integer to allow for left_join

rdu_data_cbsa_orig_dest = merge(rdu_data_cbsa, metromicro_pop_dest_int, by = 'dest_cbsa') #merge dest_csba column to new dataset
rdu_data_cbsa_orig_dest = rename(rdu_data_cbsa_orig_dest, "dest_pop" = "value") #rename value to dest_pop to make dataset more easily readable
rdu_data_cbsa_orig_dest = rdu_data_cbsa_orig_dest %>% select(-variable) #remove useless variable column
rdu_data_cbsa_orig_dest = rdu_data_cbsa_orig_dest %>% select(-NAME.x) #remove useless NAME.x column
rdu_data_cbsa_orig_dest = rdu_data_cbsa_orig_dest %>% select(-NAME.y) #remove useless NAME.y column
rdu_data_cbsa_orig_dest #call table

rdu_grouped <- rdu_data_cbsa_orig_dest %>% group_by(origin_cbsa, dest_cbsa) %>% summarise(sum(passengers)) #convert airport-to-airport volumes to CSBA-to-CSBA volumes
rdu_grouped_2 <- rdu_data_cbsa_orig_dest %>% group_by(origin_cbsa, dest_cbsa) %>% summarise(sum(distancemiles)) #convert airport-to-airport volumes to CSBA-to-CSBA volumes
rdu_grouped_3 <- rdu_data_cbsa_orig_dest %>% group_by(origin_cbsa, dest_cbsa) %>% summarise(sum(dest_pop)) #convert airport-to-airport volumes to CSBA-to-CSBA volumes
rdu_grouped_4 <-rdu_data_cbsa_orig_dest %>% group_by(origin_cbsa, dest_cbsa) %>% summarise(sum(origin_pop)) #convert airport-to-airport volumes to CSBA-to-CSBA volumes
rdu_grouped_12 <- left_join(rdu_grouped, rdu_grouped_2) #join new converted columns
rdu_grouped_123 <- left_join(rdu_grouped_12, rdu_grouped_3) #join new converted columns
rdu_grouped_full <- left_join(rdu_grouped_123, rdu_grouped_4) #join new converted columns
rdu_grouped_full = rdu_grouped_full %>% rename("passengers" = "sum(passengers)", "distancemiles" = "sum(distancemiles)", "dest_pop" = "sum(dest_pop)", "origin_pop" = "sum(origin_pop)") #rename converted columns

ggplot(rdu_grouped_full, aes(x = passengers, y = origin_pop)) + geom_point(color = "#00AFBB", size = 2,shape = 21) #scatterplot for total passengers and origin population
ggplot(rdu_grouped_full, aes(x = passengers, y = dest_pop)) + geom_point(color = "#00AFBB", size = 2,shape = 21) #scatterplot for total passengers and destination population
ggplot(rdu_grouped_full, aes(x = passengers, y = distancemiles)) + geom_point(color = "#00AFBB", size = 2,shape = 21) #scatterplot for total passengers and total flight distance(miles)

##Question 3##
data_cbsa = left_join(data,metromicro_pop_origin_int)
data_cbsa = rename(data_cbsa, "origin_pop" = "value") #rename value column to make next join for dest_cbsa less confusing
data_cbsa = data_cbsa %>% select(-variable) #remove unnecessary variable column from rdu_data_cbsa
data_cbsa_orig_dest = merge(data_cbsa, metromicro_pop_dest_int, by = 'dest_cbsa') #merge dest_csba column to new dataset
data_cbsa_orig_dest = rename(data_cbsa_orig_dest, "dest_pop" = "value") #rename value to dest_pop to make dataset more easily readable
data_cbsa_orig_dest = data_cbsa_orig_dest %>% select(-variable) #remove useless variable column
data_cbsa_orig_dest = data_cbsa_orig_dest %>% select(-NAME.x) #remove useless NAME.x column
data_cbsa_orig_dest = data_cbsa_orig_dest %>% select(-NAME.y) #remove useless NAME.y column
dat_grouped <- data_cbsa_orig_dest %>% group_by(origin_cbsa, dest_cbsa) %>% summarise(sum(passengers)) #convert airport-to-airport volumes to CSBA-to-CSBA volumes
dat_grouped_2 <- data_cbsa_orig_dest %>% group_by(origin_cbsa, dest_cbsa) %>% summarise(sum(distancemiles)) #convert airport-to-airport volumes to CSBA-to-CSBA volumes
dat_grouped_3 <- data_cbsa_orig_dest %>% group_by(origin_cbsa, dest_cbsa) %>% summarise(sum(dest_pop)) #convert airport-to-airport volumes to CSBA-to-CSBA volumes
dat_grouped_4 <-data_cbsa_orig_dest %>% group_by(origin_cbsa, dest_cbsa) %>% summarise(sum(origin_pop)) #convert airport-to-airport volumes to CSBA-to-CSBA volumes
dat_grouped_12 <- left_join(dat_grouped, dat_grouped_2) #join new converted columns
dat_grouped_123 <- left_join(dat_grouped_12, dat_grouped_3) #join new converted columns
dat_grouped_full <- left_join(dat_grouped_123, dat_grouped_4) #join new converted columns
dat_grouped_full = dat_grouped_full %>% rename("passengers" = "sum(passengers)", "distancemiles" = "sum(distancemiles)", "dest_pop" = "sum(dest_pop)", "origin_pop" = "sum(origin_pop)") #rename converted columns
dat_grouped_full = data.frame(dat_grouped_full) #transform dat_grouped_full into a data frame object to run a regression analysis
View(dat_grouped_full) #view data frame

reg_pass <- lm(passengers ~ origin_pop + dest_pop + distancemiles, data=dat_grouped_full) #run a multiple regression of total passenger volumes on origin CBSA population, destination CBSA population, and distance between cities
summary(reg_pass) #summary of regression output
# passengers = 2.378e-02*origin_pop + 2.427e-02*dest_pop -2.333e+01*distancemiles + 2.959e+04

#Question 4#
pass_vol = filter(dat_grouped_full, origin_cbsa=="39580" | dest_cbsa=="39580" | origin_cbsa=="38860" | dest_cbsa=="38860" | origin_cbsa=="21340" | dest_cbsa=="21340" | origin_cbsa=="45220" | dest_cbsa=="45220" | origin_cbsa=="41740" | dest_cbsa=="41740") #filter grouped data by relevant csba locations for passenger predictions
View(pass_vol) #view pass_vol

pass_vol$forecast_pass = predict(reg_pass, pass_vol) #predict pass_vol using reg_pass regression formulated above
pass_vol #call pass_vol

pass_vol_excl = filter(pass_vol, origin_cbsa=="39580" & dest_cbsa=="38860" | origin_cbsa=="39580" & dest_cbsa=="21340" | origin_cbsa=="39580" & dest_cbsa=="45220" | origin_cbsa=="39580" & dest_cbsa=="41740" | 
                         origin_cbsa=="38860" & dest_cbsa=="39580" | origin_cbsa=="21340" & dest_cbsa=="39580" | origin_cbsa=="45220" & dest_cbsa=="39580" |origin_cbsa=="41740" & dest_cbsa=="39580") #filter pass_vol further to include only routes to or from RDU and relevant airports, per the routes under consideration

# San Diego = 41740; RDU = 39580; Portland = 38860; Tallahassee = 45220; El Paso = 21340 #

View(pass_vol_excl) #view pass_vol_excl


pass_vol_excl$forecast_pass = predict(reg_pass, pass_vol_excl) #predict pass_vol_excl using reg_pass regression formulated above
pass_vol_excl #call pass_vol_excl

# It seems it would be best to add the RDU to Portland route because the regression model for passengers predicts that this route will have the most demand, additionally also having hte highest multidirectional demand out of all of the potential flight routes.
