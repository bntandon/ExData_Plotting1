plot1 <- function (){
        ## ---------------------------        
        ##  Before you call this, unzip exdata-data-household_power_consumption.zip 
        ## and place the text file in a sub directory ./data
        ## as ./data/household_power_consumption.txt
        ##  -- Calling this program ----- 
        ## source("plot1.R")
        ## plot2()
        ## expected output plot1.png in current directory
        ##-- end of user instructions--------------         
        
        ## ------- function comments -----------
        ## Subsetting to 2007-02-01 to 2007-02-02  
        ## text file is in DD/MM/YYYY 1/2/2007 to 2/2/2007.
        ## row 66638 to 69517  - 2879+1 -> 2880 rows
        ## skip 66637 rows and read 2881 - 1 extra row to read 3/2/2007 00:00:00 record
        
        file <- "./data/household_power_consumption.txt"
        if (!file.exists(file)){
                stop ("Unzip and place text file in ./data/household_power_consumption.txt ")        
        }
        
        colClasses <-  c(c("character","character") , rep("numeric",7) )
        ## store column names in a vector
        xnames <- read.csv(file= file, sep=";", header= FALSE, nrows =1)
        
        ## Read records
        x1 <- read.csv(file= file, sep=";", header= FALSE, colClasses = colClasses , skip=66637, nrows = 2881)
        ## Set column names
        for ( i in 1:9) {
                names(x1)[i] <- as.character(xnames[1,i])
        }
        
        ## No need to check if not a number  sum(is.na(as.numeric((x1[,4])))) as colClasses was set
        x2 <- cbind(x1, paste(x1[,"Date"], x1[,"Time"] ))
        names(x2)[10] <- 'DateTime'
        x2 <- cbind( x2, strptime(x2[ ,"DateTime"],"%d/%m/%Y %H:%M:%S"))
        names(x2)[11] <- 'Timestamp'
        x2 <- cbind( x2, substr(weekdays(x2[ ,"Timestamp"]),1,3))
        names(x2)[12] <- "Dayname"
        ## -Global Active power
        hist(x2[,"Global_active_power"], col = "red" , 
             xlab ="Global Active Power (kilowatts)" ,
             main ="Global Active Power" )
        devnum <- dev.copy(png, file="plot1.png")
        dev.off(devnum)
}