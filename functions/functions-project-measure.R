readData = function(file)
{
  myFile = paste0(local.data.path.to.secret,file);
  measure.raw <- read.csv(myFile, header = TRUE, sep = "|");
  return (measure.raw)
}


dataCleaning = function(data)
{
  # Clean notes and minutes column
  data$notes[which(is.na(data$notes))] <- "None"
  data$minutes[which(is.na(data$minutes))] <- 0
  data = na.omit(data)
  data = removeDuplicatesFromDataFrameAllColumns(data);
  
  for (i in 4:26)
  {
    # loop inside each value inside column
    # for all non cm values, convert to cm.
    for (j in 1:length(data[,i]))
    {
      # for all non cm rows convert to cm...
      if (data$units[j] != "cm")
      {
        data[j,i] = data[j,i] * 2.54
      }
    }
  }
  # Now that all the values are in units: cm I can convert the unit column values to all be cm
  data$units = "cm"
  data$gender = factor(tolower(data$gender))
  data$gender[data$gender == 'Female'] <- "female"
  data$gender[data$gender == 'M'] <- "male"
  data$gender[data$gender == 'Male'] <- "male"
  data$gender[data$gender == 'f'] <- "female"
  data$gender[data$gender == 'm'] <- "male"
  
  data$ethnicity = factor(tolower(data$ethnicity))
  data$ethnicity[data$ethnicity == 'chinese'] <- "asian"
  data$ethnicity[data$ethnicity == 'laotian'] <- "asian"
  data$ethnicity[data$ethnicity == 'white'] <- "caucasian"
  data$ethnicity[data$ethnicity == 'white non-hispanic'] <- "caucasian"  
  data$ethnicity[data$ethnicity == 'indian'] <- "asian"
  data$ethnicity[data$ethnicity == 'latin american'] <- "hispanic"
  data$ethnicity[data$ethnicity == 'asain'] <- "asian"
  data$ethnicity[data$ethnicity == 'korean'] <- "asian"
  data$ethnicity[data$ethnicity == 'caucasain'] <- "caucasian"
  data$ethnicity[data$ethnicity == 'anglo'] <- "caucasian"
  data$ethnicity[data$ethnicity == 'white-filipino'] <- "caucasian/asian"
  
  data$eye_color = factor(tolower(data$eye_color))
  data$eye = factor(tolower(data$eye))
  
  index = getIndexOfDataFrameRows(data, "eye_color", 'left')
  data[index,]
  # looks like values in these columns are mixed up.. needs to be switched.
  data$eye[data$eye == 'brown'] <- "left"
  data$eye_color[data$eye_color == 'left'] <- "brown"
  data$eye_color[data$eye_color == 'blue/green'] <- "blue-green"
  data$eye_color[data$eye_color == 'black'] <- "brown"
  
  vals = as.numeric(data$minutes[which(data$minutes < 0)])
  mean.minutes = mean(data$minutes[data$minutes > 0])
  median.minutes = median(data$minutes[data$minutes > 0])
  
  # I decided to use median as a replacement for the bad data (negatives and NAs's) because there were still   significant outliers as seen in the box plot
  
  for (i in vals)
  {
    index = getIndexOfDataFrameRows(data, "minutes", i)
    data$minutes[index] <- median.minutes
  }
  data$minutes[data$minutes == 0] <- median.minutes
  
  data$swinging = factor(tolower(data$swinging))
  data$swinging[data$swinging == 'rigth'] <- "right"
  data$swinging[data$swinging == 'let'] <- "left"
  data$swinging[data$swinging == 'leftt'] <- "left"
  
  data$writing = factor(tolower(data$writing))
  
  # Remove row with the value "N/A" column side
  index = getIndexOfDataFrameRows(data, "side", "N/A")
  data[-c(index), ]
  
  # Clean all outliers from data
  outliers = getOutliers(data,"height.NA")
  data = removeOutliers(data,"height.NA",outliers)
  
  outliers = getOutliers(data,"head.circumference.NA")
  data = removeOutliers(data,"head.circumference.NA",outliers)
  
  outliers = getOutliers(data,"hand.length.left")
  data = removeOutliers(data,"hand.length.left",outliers)
  
  outliers = getOutliers(data,"hand.width.left")
  data = removeOutliers(data,"hand.width.left",outliers)
  
  outliers = getOutliers(data,"hand.width.right")
  data = removeOutliers(data,"hand.width.right",outliers)
  
  outliers = getOutliers(data,"hand.elbow.left")
  data = removeOutliers(data,"hand.elbow.left",outliers)
  
  outliers = getOutliers(data,"hand.elbow.right")
  data = removeOutliers(data,"hand.elbow.right",outliers)
  
  outliers = getOutliers(data,"elbow.armpit.left")
  data = removeOutliers(data,"elbow.armpit.left",outliers)
  
  #getOutliers(data,"arm.reach.left")
  outliers = c(53.00,  56.00,  56.00,  64.00,  65.00,  65.10,  66.04,  66.10,  67.00)
  data = removeOutliers(data,"arm.reach.left",outliers)
  
  outliers = getOutliers(data,"arm.span.NA")
  data = removeOutliers(data,"arm.span.NA",outliers)
  
  outliers = getOutliers(data,"foot.length.left")
  data = removeOutliers(data,"foot.length.left",outliers)
  
  outliers = getOutliers(data,"foot.length.right")
  data = removeOutliers(data,"foot.length.right",outliers)
  
  outliers = getOutliers(data,"floor.armpit.left")
  data = removeOutliers(data,"floor.armpit.left",outliers)
  
  outliers = getOutliers(data,"floor.armpit.left")
  data = removeOutliers(data,"floor.armpit.left",outliers)
  
  cleaned.df <- data.frame(data)
  return (cleaned.df)
}

# Read in NBA Data file...
readDataNBA = function(file)
{
  myFile = paste0(local.data.path.to.secret.NBA,file);
  measure.raw <- read.csv(myFile, header = FALSE, na.strings=c(""));
  return (measure.raw)
}

# Clean NBA data set..
cleanNbaData2015 = function(data)
{
  names(data)[names(data) == "V3"] <- "Year Start"
  names(data)[names(data) == "V8"] <- "Height (in cm)"
  names(data)[names(data) == "V9"] <- "Wingspan (in cm)"
  
  data = data[,c("Height (in cm)","Wingspan (in cm)","Year Start")]
  data = data[-c(1),]
  data = na.omit(data)
  data[,1] <- as.numeric(data[,1])
  data[,2] <- as.numeric(data[,2])
  data[,3] <- as.numeric(data[,3])
  
  data <- subset(data, `Year Start` > 2015, select=c("Height (in cm)", "Wingspan (in cm)"))
  
  names(data)[names(data) == "Height (in cm)"] <- "Height (in cm) 2015"
  names(data)[names(data) == "Wingspan (in cm)"] <- "Wingspan (in cm) 2015"
  
  set.seed(1)
  data = data[sample(1:nrow(data), 49),]
  
  ratio.column.2015 = as.data.frame(data$`Wingspan (in cm)`/ data$`Height (in cm)`)
  
  data['NBA Ape Scale 2015'] = ratio.column.2015
  return (data)
}


# Clean NBA data set..
cleanNbaData2002 = function(data)
{
  names(data)[names(data) == "V3"] <- "Year Start"
  names(data)[names(data) == "V8"] <- "Height (in cm)"
  names(data)[names(data) == "V9"] <- "Wingspan (in cm)"
  
  data = data[,c("Height (in cm)","Wingspan (in cm)","Year Start")]
  data = data[-c(1),]
  data = na.omit(data)
  data[,1] <- as.numeric(data[,1])
  data[,2] <- as.numeric(data[,2])
  data[,3] <- as.numeric(data[,3])
  
  data <- subset(data, `Year Start` <= 2002, select=c("Height (in cm)", "Wingspan (in cm)"))
  
  names(data)[names(data) == "Height (in cm)"] <- "Height (in cm) 2002"
  names(data)[names(data) == "Wingspan (in cm)"] <- "Wingspan (in cm) 2002"
  
  set.seed(1)
  data = data[sample(1:nrow(data), 49),]
  
  ratio.column.2002 = as.data.frame(data$`Wingspan (in cm)`/ data$`Height (in cm)`)
  
  data['NBA Ape Scale 2002'] = ratio.column.2002 
  
  return (data)
}

# reshape from df to matrix
reshapeDataMeasure = function(data){
  # Select males that are at least 18 years of age
  data <- data[ which(data$gender == 'male' & data$age >= 18), ]
  ratio.column.measure = as.data.frame(data$arm.span.NA/ data$height.NA)
  data['Measure Ape Scale'] = ratio.column.measure 
  data = data[,c("height.NA","arm.span.NA",'Measure Ape Scale')]
  return (data)
}

mergeMyDataAll = function(clean.df.measure,clean.nba.2002,clean.nba.2015)
{
  df.measure = reshapeDataMeasure(clean.df.measure)
  merged.df = cbind(df.measure,clean.nba.2002)
  final.merged.df = cbind(merged.df, clean.nba.2015)
  return(as.matrix(final.merged.df))
}

mergeMyDataApe = function(clean.df.measure,clean.nba.2002,clean.nba.2015)
{
  df.measure = reshapeDataMeasure(clean.df.measure)
  merged.df = cbind(Measure.Ape.Scale = df.measure[,3],NBA.Ape.Scale.2002 = clean.nba.2002[,3])
  final.merged.df = cbind(merged.df, NBA.Ape.Scale.2015 = clean.nba.2015[,3])
  as.matrix(final.merged.df)
  
  return(final.merged.df)
}

# As of now this function will take in a dataframe and 1 column and display the
# unique values and sum of those unique values inside the column.
sumOfUniqueValuesInColumn = function(df, columns)
{
  index = getIndexOfDataFrameColumns(df, columns)
  
  for (i in 1:length(unique(df[,index])))
  {
    unique.values = unique(df[,index])[i]
    sum.of.values = length(df[,index][which( df[,index] == unique.values)])
    print(paste0(unique.values," " ,sum.of.values))
  }
}

# To remove outliers I have decided to use the quartile system to discard outliers.
getOutliers = function(df,columns)
{
  index = getIndexOfDataFrameColumns(df, columns)
  q1 = quantile(df[,index], .25)
  q3 = quantile(df[,index], .75)
  iqr = IQR(df[,index])
  outliers = df[,index][which(df[,index] < q1-1.5*iqr | df[,index] > q3+1.5*iqr)]
  if (length(outliers) == 0){
    print("There are 0 outliers")
  }
  return(sort(outliers))
}

# Removes Outliers, Can be called in conjuction with getOutliers() or simply pass in vector of outliers you wish to remove
removeOutliers = function(df,columns,outlier)
{
  #print(paste("Number of rows before removing: ",dim(df)[1]))
  
  index = getIndexOfDataFrameColumns(df, columns)
  outliers = getOutliers(df,columns)
  if (length(outlier) == 0){
    print("No outliers")
  }
  if (length(outlier) == 1){
    #df = subset(df, df[,index] != outlier)
    df = df[!df[,index] %in% outlier, ]
    #print(paste("Number of rows after removing: ",dim(df)[1]))
  }
  else {
    df = df[!df[,index] %in% outlier, ]
    #print(paste("Number of rows after removing: ",dim(df)[1]))
    
  }
  return(df)
}

displayPlotHeightWing = function(df1,df2,df3)
{
  fit <- lm(arm.span.NA~height.NA, data=df1)
  fit2 <- lm(nbaData.2015[,2]~nbaData.2015[,1], data=nbaData.2015)
  fit3 <- lm(nbaData.2002[,2]~nbaData.2002[,1], data=nbaData.2002)
  
  plot(df2[,1],df2[,2], xlab = "Height", ylab="Wingspan")
  points(df1[,1],df1[,2], col="blue")
  points(df3[,1],df3[,2], col="green")
  abline(fit, col = "blue")
  abline(fit2, col = "black")
  abline(fit3, col = "green")
  legend("topleft", legend=c("NBA player in 2015","Normal Person","NBA player in 2002"),
         col=c("black", "blue","green"), lty=1, cex=0.8)
  
}

get_summary = function(df1,df2,df3)
{
  summary(df1)
  summary(df2)
  summary(df3)
}

# The next two functions below are in progress and arnt functional at the moment.
convertColumnValues = function(df, columns, prev.val, new.val)
{
  index = getIndexOfDataFrameColumns(df, columns)
  subset.prev.values = df[,index][df[,index] == prev.val]
  
  if (length(subset.prev.values) > 0)
  {
    df[,index][df[,index] == prev.val] <- new.val
  }
  else
  {
    print(paste0("Error. There may not be a ", prev.val, " in the column you entered."))
  }
}

# In progress...
convertColumnValues2 = function(df, columns, prev.val, new.val)
{
  index.rows = getIndexOfDataFrameRows(df, columns, prev.val)
  index = getIndexOfDataFrameColumns(df, columns)
  
  for (i in 1:length(index.rows))
  {
    row = index.rows[i]
    #df[row,index] = new.val
    replace(df[,index], c(prev.val), new.val) 
  }
  
}