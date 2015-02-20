showEmissionLevelByCity <- function(fipsval){
  
  library(dplyr)
  
  # if data set doesn't exist in the current folder download and unzip
  if(!file.exists("exdata_data_NEI_data.zip")){
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip","exdata_data_NEI_data.zip")
    unzip("exdata_data_NEI_data.zip")
  }
  
  ## This first line will likely take a few seconds. Be patient!
  NEI <- readRDS("summarySCC_PM25.rds")
 # SCC <- readRDS("Source_Classification_Code.rds")
  
  # if already created we are recreating
  if(file.exists("plot2.png")){
    file.remove("plot2.png")
  }
  
  # open for writing
  png('plot2.png',bg="transparent")
  
  #filter data for baltimore city first
  NEI_wrapper <- tbl_df(NEI)
  total_emission_by_year <- NEI_wrapper %>% filter(fips == fipsval)
  
  # group by year to calculate total emission per year
  total_emission_by_year <- aggregate(NEI$Emissions,by=list(NEI$year),sum)
  
  names(total_emission_by_year) <- c("Year","Emissions")
  
  # As per instruction plot with base plot func
  barplot((total_emission_by_year$Emissions)/10^6,names.arg = total_emission_by_year$Year,
          main="PM2.5 Emissions in Baltimore City, Maryland from 1999-2008",xlab = "Year",ylab = "Emission[in Million Tons]",col=c("green","yellow","red","violet"))
  
  dev.copy(png,file="plot2.png")
  dev.off()
  
}