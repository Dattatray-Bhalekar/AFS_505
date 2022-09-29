


#Question 2. You are provided a folder with three location (county) names, each of which has
#subfolders for one or two crops, which in turn has a data file.

#a. Iterate through the folders to read all the files and merge them into a single data
#frame. You can use a "loop" to iterate or for efficiency check out the list.files()
#function.

library(dplyr)
library(lubridate)




setwd("C:/CropModelResults") 
print(list.files())
dataset <-  data.frame(matrix(ncol = 11, nrow = 0))
colnames(dataset) <-  c("YYYY-MM-DD(DOY)", "planting_date","harvest_date","yield","used_biomass","irrig","precip", "county","crop","lat","long")

# b. Add four additional columns to the merged dataframe corresponding to the
# county name, crop name, latitude and longitude of the data. You must get this
# information from the directory structure you are looping through or the strings
# returned by the call to list.files().

for (county in list.files()){
  for (crop in list.files(path=county)){
    for(gps in list.files(file.path(county, crop))){
      for(file in list.files(file.path(county, crop, gps))){
        df = read.csv(file.path(county, crop, gps, file))
        n = nrow(df)
        lat = paste(unlist(strsplit(gps, "N"))[1],"N", sep = "")
        long = unlist(strsplit(gps, "N"))[2]
        df['county'] = rep(c(county), n)
        df['crop'] = rep(c(crop), n)
        df['lat'] = rep(c(lat), n)
        df['long'] = rep(c(long), n)
        dataset = rbind(df, dataset)
      }
    }
  }
}


#c. Rename the column irrig to irrigation_demand and precip to precipitation and
#export the dataframe as a csv file.

names(dataset) <- c("YYYY-MM-DD(DOY)", "planting_date","harvest_date","yield","used_biomass","irrigation_demand","precipitation", "county","crop","lat","long")

#d.Summarize the annual irrigation demand by crop name and county name

df_new = dataset%>% 
  group_by(county, crop)%>%
  summarise(total_irrigation_demand =sum(irrigation_demand))

df_new

# county     crop         total_irrigation_demand
# <chr>      <chr>                          <dbl>
#   1 Okanogan   Corn_grain                    30527.
# 2 Okanogan   Winter_Wheat                  31213.
# 3 WallaWalla Corn_grain                    28414.
# 4 WallaWalla Winter_Wheat                  28000.
# 5 Yakima     Corn_grain                    42437.
# 6 Yakima     Winter_Wheat                  43820 

# e. What is the average yield of Winter Wheat in Walla Walla at
# 46.03125N118.40625W for the year ranges (1981-1990), (1991-2000), and
# (2001-2019)?

df_filter = filter(dataset, county == "WallaWalla" & crop == "Winter_Wheat" & lat == "46.03125N" & long == "118.40625W")

df_filter['YYYY-MM-DD(DOY)']

df_filter['year'] = as.numeric(format(strptime(df_filter[['YYYY-MM-DD(DOY)']], "%Y-%m-%d(%j)"), "%Y"))

df_filter %>% 
  mutate(category = case_when(year < 1991 ~ "1981-1990",
                              year >= 1991 & year < 2000 ~ "1991-2000",
                              TRUE ~ "2001-2019")) %>%
  group_by(county, crop, lat, long, year = category) %>% 
  summarize(yield = mean(yield)) %>%
  ungroup

# county     crop         lat       long       year      yield
# <chr>      <chr>        <chr>     <chr>      <chr>     <dbl>
#   1 WallaWalla Winter_Wheat 46.03125N 118.40625W 1981-1990 7661.
# 2 WallaWalla Winter_Wheat 46.03125N 118.40625W 1991-2000 8045.
# 3 WallaWalla Winter_Wheat 46.03125N 118.40625W 2001-2019 7758.

#f. Which location has highest yield (average) for the time period (2001-2019) for
#grain corn?


dataset['year'] = as.numeric(format(strptime(dataset[['YYYY-MM-DD(DOY)']], "%Y-%m-%d(%j)"), "%Y"))

dataset %>%
  filter(year > 2000 & crop == 'Corn_grain') %>%
  group_by(county, crop, lat, long, year = ceiling(year/20) * 20) %>% 
  summarize(year = paste(first(year) - 19, first(year)- 1 , sep = '-'),
            yield = mean(yield)) %>%
  ungroup %>%
  group_by(year) %>%
  summarize(county=max(county), crop=max(crop), lat=max(lat), long=max(long),
            yield = max(yield)) %>%
  ungroup
# year      county crop       lat       long        yield
# <chr>     <chr>  <chr>      <chr>     <chr>       <dbl>
#   1 2001-2019 Yakima Corn_grain 48.96875N 119.96875W 15263.
  




