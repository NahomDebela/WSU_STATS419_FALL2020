#transposes given matrix											
transposeMatrix = function(mat)
{
  t(mat);
}

#rotate matrix 90 degrees
rotate90 = function(mat)
{
  mat <- t(apply(mat,2,rev))
  mat
}

rotate180 = function(mat)
{
  mat = rotate90(mat)
  mat <- t(apply(mat,2,rev))
  mat
}

rotate270 = function(mat)
{
  mat = rotate180(mat)
  mat <- t(apply(mat,2,rev))
  mat
}

#returns summary statistics of dataframe Length/num_na/mean/median/mode/sd/variance
doSummary <- function(data)
{
  len <- length(data)
  cat("length", len, "\n")
  
  num_na <- sum(is.na(data))
  cat("Number of NA", num_na, "\n")
  
  mean_data <- 	rowMeans(data[sapply(data, is.numeric)]) 
  cat("Mean", mean_data, "\n")
  
  mshafer_data_dropped <- data
  mshafer_data_dropped[,1] <- NULL
  mshafer_data_dropped[,61] <- NULL
  mshafer_data_dropped[,61] <- NULL
  mshafer_data_dropped[,61] <- NULL
  mshafer_data_dropped <- as.numeric(mshafer_data_dropped[1,])
  median_data <- 	median(mshafer_data_dropped) 
  cat("Median",median_data, "\n")
  
  mode_data <- doMode(data)
  mode_data<- as.numeric(mode_data)
  cat("Mode", mode_data, "\n")

  mshafer_data_dropped2 <- data
  mshafer_data_dropped2[,1] <- NULL
  mshafer_data_dropped2[,61] <- NULL
  mshafer_data_dropped2[,61] <- NULL
  mshafer_data_dropped2[,61] <- NULL
  mshafer_data_dropped2 <- as.numeric(mshafer_data_dropped2[1,])
  standard_dev <- sd(mshafer_data_dropped2)
  variance <- var(mshafer_data_dropped2)
  cat("Variance", variance, "\n")
  cat("Standard Deviation", standard_dev)
}

# returns variance of dataframe
  doVariance <- function(data, method="two-pass")
  {
    data = stats::na.omit(data);
    if (method == "naive")
    {
      n = sum = sumsq = 0
      for (i in 1:length(data))
      {
        n <- n + 1
        sum <- sum + data[i]
        sumsq <- sumsq + data[i] * data[i]
      }
    mean = (sum / n)
    
     svariance = (sumsq ??? (sum * sum) / n) / (n ??? 1)
     }
    else  
    {
      # two - pass algorithim
      n = sum1 = sum2 = 0
      
      for (i in 1:length(data))
      {
        n <- n + 1
        sum1 <- sum1 + data[i]
      }
      mean = sum1 / n
      for (i in 1:length(data))
      {
        deviation = data[i] - mean;
        sum2 = sum2 + deviation * deviation;
      }
      svariance = sum2 / (n - 1)
    }
    sstandarddev = sqrt(svariance);
    list("x.bar"=mean,"s.var"=svariance,"s.sd"=sstandarddev);
  }
    
  
# returns mode of dataframe
  doMode <- function(d)
  {
    unique_n <- unique(d)
    unique_n[which.max(tabulate(match(d, unique_n)))]
  }
  
# calculates and plots z score  
  zscore_plot <- function(data)
  {
    mshafer_data_dropped2 <- data
    mshafer_data_dropped2[,1] <- NULL
    mshafer_data_dropped2[,61] <- NULL
    mshafer_data_dropped2[,61] <- NULL
    mshafer_data_dropped2[,61] <- NULL
    mshafer_data_dropped2 <- as.numeric(mshafer_data_dropped2[1,])
    zscore <- (mshafer_data_dropped2 - mean(mshafer_data_dropped2)) / sd(mshafer_data_dropped2)
    plot(mshafer_data_dropped2,zscore)
    return(zscore)
  }