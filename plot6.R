showMotorVehicleRelatedEmission <- function(){
  
  library(dplyr)
  library(ggplot2)
  library(reshape2)
  
  # if data set doesn't exist in the current folder download and unzip
  if(!file.exists("exdata_data_NEI_data.zip")){
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip","exdata_data_NEI_data.zip")
    unzip("exdata_data_NEI_data.zip")
  }
  
  ## This first line will likely take a few seconds. Be patient!
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  # if already created we are recreating
  if(file.exists("plot4.png")){
    file.remove("plot4.png")
  }
  
  # open for writing
  png('plot6.png',bg="transparent")
  
  calcMotorVehicleEmissionByCity <- function(fips){
    motorVehicle <- unique(grep("Vehicles", SCC$EI.Sector, ignore.case = TRUE, value = TRUE))
    motorVehicleSourceCodes <- SCC[SCC$EI.Sector %in% motorVehicle, ]["SCC"]
    
    ## Subset emissions due to motor vehicle sources in from 'NEI' for Baltimore
    emissionFromMotorVehiclesInCity <- NEI[NEI$SCC %in% motorVehicleSourceCodes$SCC &  NEI$fips == fips, ]
  }
  
  vehicleEmissionInBaltimore <- calcMotorVehicleEmissionByCity("24510")
  vehicleEmissionInLA <- calcMotorVehicleEmissionByCity("06037")
  
  total_emission_by_year_baltimore <- aggregate(vehicleEmissionInBaltimore$Emissions,by=list(vehicleEmissionInBaltimore$year),sum)
  names(total_emission_by_year_baltimore) <- c("Year","Emissions-Baltimore")
  
  total_emission_by_year_la <- aggregate(vehicleEmissionInLA$Emissions,by=list(vehicleEmissionInLA$year),sum)
  names(total_emission_by_year_la) <- c("Year","Emissions-Los Angeles")
  
  total_emission_by_year <- merge(total_emission_by_year_baltimore,total_emission_by_year_la,by="Year")
  dat.m <- melt(total_emission_by_year,id.vars = "Year")  
 
  ggplot(dat.m, aes(x = factor(Year), y = value, fill = variable)) + geom_bar(stat="identity", position=position_dodge())+ylab("Emissions[in Tons]") + xlab("Year")
 
  
  dev.copy(png,file="plot6.png")
  dev.off()
}