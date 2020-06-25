#rm(list=ls(all=TRUE))

library(rio)

education_data = import("education.xlsx",which = 1)

head(education_data)

#correct mispellings
education_data$country[education_data$country=="Argentinaa"]= "Argentina"

#correct year
education_data$year[education_data$year == "2015H"]= "2015"

#year vatiable as numeric
education_data$year = as.numeric(education_data$year)

#sort the data
education_data = education_data[order(education_data$country),]

#change the file encoding before dealing with accents
#define function
remove.accents = function(s){
  #1 char substitutions
  old1 = "áé"
  new1 = "ae"
  s1 = chartr(old1,new1,s)
}

education_data$country = remove.accents(education_data$country)

#summary stats
summary(education_data)
library(stargazer)
stargazer(education_data, type = "text")

#add ISO-3 Country Codes
library(countrycode)
education_data$country_code = countrycode(sourcevar = education_data$country,
                                          origin = "country.name",
                                          destination = "iso3c",
                                          warn = TRUE)

#add country code for kosovo 
education_data$country_code[education_data$country == "Kosovo"] = "RKS"

#add some data from the WDI
#population data
library(WDI)
population_data = WDI(country = "all",
                      indicator = c("SP.POP.TOTL"),
                      start = 2015, end = 2015, extra = FALSE,
                      cache = NULL)

#renaming variables
library(data.table)
setnames(population_data, "SP.POP.TOTL","population")

#getting iso3 country codes fro population_data
library(countrycode)
population_data$country_code = countrycode(sourcevar = population_data$iso2c,
                                           origin = "iso2c",
                                           destination = "iso3c",
                                           warn = TRUE)
#filter out the regional and index variables
library(tidyverse)
population_data = 
  population_data %>% 
  dplyr::filter(!(country_code =="NA"))

#check with subset
subset(population_data, country_code == "NA")

#merge into a left join
merged_data = left_join(population_data,
                        education_data,
                        by = c("country_code", "year"))
#check dimmensions of merged data
dim(merged_data)

#check names of variables
names(merged_data)

#check if country names for x and y match
library(tidyverse)
merged_data = 
  merged_data %>% 
  mutate(countries_match = ifelse(country.x == country.y,
                                  "yes",
                                  "no"))

#check where the country names didnt match
subset(merged_data, countries_match == "no")

#drop country.x and rename country.y as country and sort
merged_data = 
  merged_data %>% 
  select(-c("country.x")) %>% 
  dplyr::rename("country"="country.y") 
  
#sort using relocate
merged_data = 
  merged_data %>% 
  relocate("country", "iso2c","country_code","year",
           "primary_enroll", "Population")

#remove the population and education dataframes
rm(education_data, population_data)
rm(remove.accents)
