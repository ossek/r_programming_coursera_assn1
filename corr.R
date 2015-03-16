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

corr <- function(directory,threshold = 0)
{
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    correlations <- numeric()
    for(filename in list.files(directory))
    {
        monitorData = read.csv(paste(directory,filename,sep=""))
        numCompletes <- length(Filter(function(x){x == TRUE},complete.cases(monitorData)))
        if(numCompletes > threshold)
        {
            correlations <- c(correlations,cor(monitorData[complete.cases(monitorData),c("sulfate")],monitorData[complete.cases(monitorData),c("nitrate")]))
        }
    }
    ## Return a numeric vector of correlations
    correlations
}
