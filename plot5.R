showMotorVehicleRelatedEmission <- function(){
  
  library(dplyr)
  
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
   png('plot5.png',bg="transparent")
  
  
  motorVehicle <- unique(grep("Vehicles", SCC$EI.Sector, ignore.case = TRUE, value = TRUE))
  motorVehicleSourceCodes <- SCC[SCC$EI.Sector %in% motorVehicle, ]["SCC"]
  
  ## Subset emissions due to motor vehicle sources in from 'NEI' for Baltimore
  emissionFromMotorVehiclesInBaltimore <- NEI[NEI$SCC %in% motorVehicleSourceCodes$SCC & 
                                                NEI$fips == "24510", ]
  
  total_emission_by_year <- aggregate(emissionFromMotorVehiclesInBaltimore$Emissions,by=list(emissionFromMotorVehiclesInBaltimore$year),sum)

  names(total_emission_by_year) <- c("Year","Emissions")
  
  barplot((total_emission_by_year$Emissions),names.arg = total_emission_by_year$Year,
          main="Motor Vehicle  Emissions from 1999 to 2008 in Baltimore",xlab = "Year",ylab = "Emissions",col=c("green","yellow","red","violet"))
  
  dev.copy(png,file="plot5.png")
  dev.off()
}