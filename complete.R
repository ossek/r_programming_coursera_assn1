##this exists in pollutantmean.R also, and ideally would be shared, but I want each script
## to stand independent of import locations for purposes of submission.
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

complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used

    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    #use 2 vectors side by side here, but a more appropriate representation would probably be 
    # a hashtable / dictionary of some kind.
    ids <- numeric()
    nobss <- numeric()

    for(monitorId in id)
    {
        monitorData = read.csv(paste(directory,"/",makeMonitorFileName(monitorId),sep=""))
        completes <- length(Filter(function(x){x == TRUE},complete.cases(monitorData)))
        ids <- c(ids,monitorId)
        nobss <- c(nobss,completes)
    }
    data.frame(id=ids,nobs=nobss)

}
