
#   The overall goal of this assignment is to explore the National Emissions Inventory database and see 
#   what it say about fine particulate matter pollution in the United states over the 10-year period 1999-
#   2008. You may use any R package you want to support your analysis.
#
#   QUESTION:
#   You must address the following questions and tasks in your exploratory analysis. For each question/
#   task you will need to make a single plot. Unless specified, you can use any plotting system in R to 
#   make your plot.
#
#   6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle  
#   sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over  
#   time in motor vehicle emissions?
#
#   ANSWER:
#   A large increase was noted for LA County, followed by a decline, but an overal increase. With a 
#   decline for Baltimore, we are comparing two disparate areas, physically, economically and socially.
#
#   COMMENTS:
#   In viewing the plot6.png image, we see a marked difference between the Los Angeles County (LAC) and 
#   Baltimore City (BC) emissions. I point to the obvious, LAC is very large compared to BC with a area
#   of 4,751 sq mi and a population of ~10 million, compared to 92 square mi and 2.7 million. Considering
#   area to averaged population density, Baltimore's is much higher, but, by area, we're comparing apples 
#   and oranges. 
#   The BC plot shows a monotonic decrease from 1999 to 2008. However, for LAC, a mmarked increase after 
#   the 1999 sample. From that point, emissions increased to 2005, then decreased to the end of the study
#   in 2008, with an overall increase. The seperation in emissions between BC and LAC is partly due to the greater number of vehicles
#   and the regional weather systems, especially the summer inversions.

library(ggplot2)
library(reshape2)

plot6 <- function()
    {
        #setup all the file pointers
        fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        dirSet  <- "K:/COURSES/JHU_DataScience/ExploratoryDataAnalysis"
        zipFile <- "Datasets/exdata_data_NEI_data/summarySCC_PM25.zip"             
        destDir <- "K:/COURSES/JHU_DataScience/ExploratoryDataAnalysis/Datasets" 
    
        pathToPM25DataSet <- paste(destDir, 'exdata_data_NEI_data/summarySCC_PM25.rds', sep='/')
        pathToSCCDataSet  <- paste(destDir, 'exdata_data_NEI_data/Source_Classification_Code.rds', sep='/')
    
        #load the zip file and unpack
        download.file(fileUrl, zipFile, method = "curl")
        unZip <- unz(zipFile, pathToPM25DataSet)
    
        #create a data.frame for the PM2.5 data
        PM25 <- readRDS(pathToPM25DataSet)
        ClassCode <- readRDS(pathToSCCDataSet)
    
        #get data for LA County (fips=06037) and Baltimore City (fips=24510) emissions
        obs <-aggregate(PM25$Emissions[PM25$SCC %in% ClassCode$SCC[grep("Veh",ClassCode$Short.Name)] & 
                       (PM25$fips==24510 | PM25$fips=="06037")] ~ 
                        PM25$year[PM25$SCC %in% ClassCode$SCC[grep("Veh",ClassCode$Short.Name)] & 
                        (PM25$fips==24510 | PM25$fips=="06037")] + 
                        PM25$fips[PM25$SCC %in% ClassCode$SCC[grep("Veh",ClassCode$Short.Name)] & 
                        (PM25$fips==24510 | PM25$fips=="06037")], PM25, sum)
        
        #find the offset from 1999 to 2008
        totalPM25 <- round((obs[2,3]-obs[1,3])+(obs[3,3]-obs[2,3])-(obs[3,3]-obs[4,3]))
        
        #do some column names and the LAC/BC PM offsets
        colnames(obs)<-c("year","fips","emissions")
        LAC <- round(((obs[2,3]-obs[1,3])+(obs[3,3]-obs[2,3])-(obs[3,3]-obs[4,3])))
        BC  <- round(((obs[5,3]-obs[6,3])+(obs[6,3]-obs[7,3])+(obs[7,3]-obs[8,3])))
        
        #numbers and X,Y for data points
        addText <- data.frame(category=c(round(obs[1,3]), round(obs[2,3]), round(obs[3,3]), round(obs[4,3]),
                                         round(obs[5,3]), round(obs[6,3]), round(obs[7,3]), round(obs[8,3])), 
                              ypt=c(3750, 4750, 4500, 4500,
                                    350, 500, 500, 200), 
                              xpt=c(1, 2, 3, 4,
                                    0.65, 2, 3, 4.25))
        
        #plot emissions, LA County vs Baltimore, annotate plot and show PM difference
        ggplot(obs, aes(x=factor(year), y=emissions, group=fips)) + 
            geom_line(aes(color=fips)) +
            ggtitle(expression(paste(PM[2.5]," Emissions by Motor Vehicle Sources"))) + 
            labs(x = "Years", y=("Emissions")) +
            geom_point(aes(x=factor(year), y=emissions), shape=1, size=4) +
            scale_color_manual (values=c("red","black"), labels=c("LA County","Baltimore City")) +
            annotate("text", x=addText$xpt, y=addText$ypt, label=addText$category) +
            geom_text(data=NULL, x=3.5, y=2500,
                      label=substitute(paste('LAC = +', lac), list(lac=LAC))) +
            geom_text(data=NULL, x=3.5, y=2250,
                      label=substitute(paste('BC  = -', bc), list(bc=BC)))
            
        #save a PNG file of the plot
        dev.copy(png,'plot6.png'); dev.off()
    }
