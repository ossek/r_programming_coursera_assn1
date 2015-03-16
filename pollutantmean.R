makeMonitorFileName <- function(id)
{
    monitorFilename <- paste(id,".csv",sep="")
    if(id < 100)
    {
        monitorFilename <- paste("0",monitorFilename,sep="")
    }
    if(id < 10)
    {
        monitorFilename <- paste("0",monitorFilename,sep="")
    }
    monitorFilename
}

#calculate mean across specified list of monitors. Ignores missing values coded as NA.
pollutantmean <- function(directory,pollutant,id = 1:332){
    #directory is char vector of length 1 indicating loc of files

    #pollutant is char vector of length 1 denoting pollutant for which we calculate the mean
    # either 'sulfate' or 'nitrate'.

    #id is integer vector indicating monitor id numbers to use

    #build a big ol' vector
    values <- numeric()
    for(monitorId in id)
    {
        monitorData = read.csv(paste(directory,"/",makeMonitorFileName(monitorId),sep=""))
        values <- c(values,monitorData[!is.na(monitorData[[pollutant]]),pollutant])
    }
    mean(values)
}


