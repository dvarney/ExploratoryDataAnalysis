
#   The overall goal of this assignment is to explore the National Emissions Inventory database and see 
#   what it say about fine particulate matter pollution in the United states over the 10-year period 1999-
#   2008. You may use any R package you want to support your analysis.
#
#   QUESTION:
#   You must address the following questions and tasks in your exploratory analysis. For each question/
#   task you will need to make a single plot. Unless specified, you can use any plotting system in R to 
#   make your plot.
#
#   3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
#   which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? Which 
#   have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a plot 
#   answer this question.
#
#   ANSWER:
#   Inspecting the plot3.png image, we note 3 of the 4 sources: NON-ROAD, NONPOINT, and ON-ROAD have 
#   declined from 1999 to 2008, while one, POINT, has increased, with a marked rise from 2002 to 2005. 

library(ggplot2)
library(reshape2)

plot3 <- function()
    {
        #init the file paths
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
        
        #collect the observations and build an aggregate data structure 
        obs <- aggregate(PM25$Emissions[PM25$fips==24510] ~ PM25$type[PM25$fips==24510] +
                         PM25$year[PM25$fips==24510] , PM25, sum)
        
        colnames(obs)<-c("type","year","emissions")
        
        #do the plotting, overlay symbols at emissions/year inflections
        ggplot(obs, aes(x=factor(year), y=emissions, group=type)) + geom_line(aes(color=type)) +
                geom_point(aes(x=factor(year), y=emissions), shape=1, size=4) +
                ggtitle(expression(paste(PM[2.5], " Emissions for Baltimore City"))) + 
                labs(x="Years", y=expression(paste("Emissions")))
        
        #save a PNG file of the plot
        dev.copy(png,'plot3.png'); dev.off()
}
