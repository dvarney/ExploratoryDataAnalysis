
#   The overall goal of this assignment is to explore the National Emissions Inventory database and see 
#   what it say about fine particulate matter pollution in the United states over the 10-year period 1999-
#   2008. You may use any R package you want to support your analysis.
#
#   QUESTION:
#   You must address the following questions and tasks in your exploratory analysis. For each question/
#   task you will need to make a single plot. Unless specified, you can use any plotting system in R to 
#   make your plot.
#
#   1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base 
#   plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 
#   1999, 2002, 2005, and 2008.
#
#   ANSWER:
#   Inspecting the plot1.png image, we see data for the years 199,...,2008, at 3 yr intervals. Observing
#   the height of the bars, we also notice that each year is less than the previous. Therefore, the 
#   answer is TRUE.

plot1 <- function()
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
        
        #read the resulting RDS file
        PM25 <- readRDS(pathToPM25DataSet)
        
        #do a multi-colored barplot of emissions per year
        barplot(tapply(PM25$Emissions, PM25$year, sum), xlab="Year", 
                ylab="  Emissions",
                main=expression(paste(PM[2.5], "  Emissions Across The United States")),
                col=c("red", "orange", "yellow", "green"))
        
        #make a PNG plot 
        dev.copy(png,'plot1.png'); dev.off()
    }    

