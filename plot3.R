showEmissionLevelByCityAndStackByType <- function(fipsval){
  
  library(dplyr)
  library(ggplot2)
  
  # if data set doesn't exist in the current folder download and unzip
  if(!file.exists("exdata_data_NEI_data.zip")){
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip","exdata_data_NEI_data.zip")
    unzip("exdata_data_NEI_data.zip")
  }
  
  ## This first line will likely take a few seconds. Be patient!
  NEI <- readRDS("summarySCC_PM25.rds")
 # SCC <- readRDS("Source_Classification_Code.rds")
  
  # if already created we are recreating
  if(file.exists("plot3.png")){
    file.remove("plot3.png")
  }
  
  # open for writing
  png('plot3.png',bg="transparent")
  
  #filter data for baltimore city first
  NEI_wrapper <- tbl_df(NEI)
  total_emission_by_year <- NEI_wrapper %>% filter(fips == fipsval)
  
  # group by year to calculate total emission per year
  total_emission_by_year <- aggregate(NEI$Emissions,by=list(NEI$type,NEI$year),sum)
 
  names(total_emission_by_year) <- c("Type","Year","Emissions")
  
  # change scale dividing by 100,000
  total_emission_by_year$Emissions <- total_emission_by_year$Emissions
  
  
  ggplot(data=total_emission_by_year, aes(x=Year, y=Emissions)) + geom_bar(stat="identity", position=position_dodge(), fill=c("green","blue","red","violet")) + coord_flip() +  facet_wrap(~ Type)
  
  dev.copy(png,file="plot3.png")
  dev.off()
}