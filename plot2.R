
#   The overall goal of this assignment is to explore the National Emissions Inventory database and see 
#   what it say about fine particulate matter pollution in the United states over the 10-year period 1999-
#   2008. You may use any R package you want to support your analysis.
#
#   QUESTION:
#   You must address the following questions and tasks in your exploratory analysis. For each question/
#   task you will need to make a single plot. Unless specified, you can use any plotting system in R to 
#   make your plot.
#
#   2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 
#   1999 to 2008? Use the base plotting system to make a plot answering this question.
#
#   ANSWER:
#   Upon inspection of the plot2.png image, it is not obvious if emissions have decreased from 1999 to 
#   2008, and if so, b what amount. We see a decrease from 1999 to 2002, but a turn upwards in 2005. 
#   However, there was a significant downturn for 2008. Doing a little math with the numbers for each
#   year, we have:  
#                   (3274.18 - 2453.916) + (2453.916-3091.354) + (3091.354-1862.262) = ~1412

plot2 <- function()
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
        unZip <- unz(zipFile, dataFile)
    
        #read the RDS file
        PM25 <- readRDS(pathToPM25DataSet)
        taPM25 <- tapply(PM25$Emissions[PM25$fips==24510],PM25$year[PM25$fips==24510],sum)
        
        #find the offset from 1999 to 2008
        totalPM25 <- round(((taPM25[1]-taPM25[2])+(taPM25[2]-taPM25[3])+(taPM25[3]-taPM25[4])))
        
        #plot the emissions 
        barplot(taPM25,
                xlab=substitute(paste('Years, with ', loss[1999-O8], ' of ', tl, ' ', PM[2.5]), list(tl=totalPM25)), 
                ylab=expression(paste('Emissions')), 
                main=expression(paste("Total ", PM[2.5], " Emissions for Baltimore City")),
                col="lightblue")

        #make a PNG plot 
        dev.copy(png,'plot2.png'); dev.off()
    }
