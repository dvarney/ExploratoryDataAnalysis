
#   QUESTION:
#   You must address the following questions and tasks in your exploratory analysis. For each question/
#   task you will need to make a single plot. Unless specified, you can use any plotting system in R to 
#   make your plot. 
#    
#   5. How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
#
#   ANSWER:   
#   According to the plot5.png image, the vehicle emissions have monotonically and significantly
#   decreased for all dates in questions, especially between 1999 and 2002.

library(ggplot2)
library(reshape2)

plot5 <- function()
    {
        fileUrl  <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        dirSet   <- "K:/COURSES/JHU_DataScience/ExploratoryDataAnalysis"
        zipFile  <- "Datasets/exdata_data_NEI_data/summarySCC_PM25.zip"             
        destDir <- "K:/COURSES/JHU_DataScience/ExploratoryDataAnalysis/Datasets" 
    
        pathToPM25DataSet <- paste(destDir, 'exdata_data_NEI_data/summarySCC_PM25.rds', sep='/')
        pathToSCCDataSet  <- paste(destDir, 'exdata_data_NEI_data/Source_Classification_Code.rds', sep='/')
    
        #load the zip file and unpack
        download.file(fileUrl, zipFile, method = "curl")
        unZip <- unz(zipFile, pathToPM25DataSet)
    
        #create a data.frame for the PM2.5 data
        PM25 <- readRDS(pathToPM25DataSet)
        ClassCode <- readRDS(pathToSCCDataSet)
        obs<-aggregate(PM25$Emissions[PM25$SCC %in% 
                           ClassCode$SCC[grep("Veh",ClassCode$Short.Name)] & 
                           PM25$fips==24510] ~ PM25$year[PM25$SCC %in% 
                           ClassCode$SCC[grep("Veh",ClassCode$Short.Name)] & 
                           PM25$fips==24510], PM25, sum)
        
        colnames(obs)<-c("year","emissions")
        ggplot(obs, aes(x=factor(year),y=emissions)) + geom_bar(stat="identity", fill="lightgrey", color="black") +
                ggtitle(expression(paste(PM[2.5], " Emissions by vehicle sources in Baltimore City"))) + 
                labs(x = "Years", y=("Emissions"))
        
        #save a PNG file of the plot
        dev.copy(png,'plot5.png'); dev.off()
    }    