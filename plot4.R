
#   The overall goal of this assignment is to explore the National Emissions Inventory database and see 
#   what it say about fine particulate matter pollution in the United states over the 10-year period 1999-
#   2008. You may use any R package you want to support your analysis.
#
#   QUESTION:
#   You must address the following questions and tasks in your exploratory analysis. For each question/
#   task you will need to make a single plot. Unless specified, you can use any plotting system in R to 
#   make your plot.
#
#   4. Across the United States, how have emissions from coal combustion-related sources changed from 
#   1999-2008?
#
#   ANSWER:
#   According to plot4.png, the coal burning emissions for the study period of 199-2008 have declined, 
#   with a minor uptick in 2005. Of note, the PM2.5 discharge showed a significant decrease for 2008.

library(ggplot2)
library(reshape2)

plot4 <- function()
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
    
        obs <-aggregate(PM25$Emissions[PM25$SCC %in% ClassCode$SCC[grep("Coal",ClassCode$Short.Name)]] ~ 
                            PM25$year[PM25$SCC %in% ClassCode$SCC[grep("Coal",ClassCode$Short.Name)]], PM25, sum)

        colnames(obs)<-c("year","emissions")

        #do the barplot; bar fill color is lightblue with a black border
        ggplot(obs, aes(x=factor(year),y=emissions)) + geom_bar(stat="identity", fill="lightblue", color="black") +
            ggtitle(expression(paste(PM[2.5], " Emissions from Coal Burning"))) + 
            labs(x = "Years", y=("Emissions"))

        #save a PNG file of the plot
        dev.copy(png,'plot4.png'); dev.off()
    }