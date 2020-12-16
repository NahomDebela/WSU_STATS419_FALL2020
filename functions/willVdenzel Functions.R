# Get actor Id of actor
getActorId <- function(actorName)
{
  actorID = imdb.data$all.actors.info$nmid[which(imdb.data$all.actors.info$name == actorName)]
  
  if (length(actorID) == 0)
  {
    print("The actor you have entered was mistyped, please try again.")
    return (NULL)
  }
  
  return (actorID)
}

# create Dataframe for either will or denzel with movie ID's and Movie Names
getActorInfo <- function(actorName)
{
  actorID = getActorId(actorName)
  if (is.null(actorID) == FALSE)
  {
    moviesID = imdb.data$all.movies.actors.characters$ttid[which(imdb.data$all.movies.actors.characters$nmid == actorID)]
    
    movies <- c()
    for (i in 1:length(moviesID))
    {
      id = moviesID[i]
      title = imdb.data$all.actors.movies$title[which(imdb.data$all.actors.movies$ttid == id)]
      movies = append(movies, title)
    }
    df = data.frame(MovieID = moviesID,MovieName = movies)
    return(df)
    
  }
    else
    {
      print("The actor you have entered was mistyped, please try again.")
    }
  
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

# get rankings df for all co actors of certain actor. pass in vector of coActors
getCoActorRankings <- function(coActorsId)
{
  coPop50Col <- c()
  coHeadlinersCol <- c()
  coTop250Col <- c()
  coGem50Col <- c()
  
  for (i in 1:length(coActorsId))
  {
    id = coActorsId[i]
    
    coPop50 = intersect(imdb.data$actors$popular50, id)
    if (length(coPop50 > 0))
    {
      coPop50Col = append(coPop50Col, TRUE)
    }
    else
    {
      coPop50Col = append(coPop50Col, FALSE)
    }
    coHeadliners = intersect(imdb.data$headliners$actors, id)
    if (length(coHeadliners > 0)){
      coHeadlinersCol = append(coHeadlinersCol, TRUE)
    }
    else
    {
      coHeadlinersCol = append(coHeadlinersCol, FALSE)
    }
    coTop250 = intersect(imdb.data$actors$top250, id)
    if (length(coTop250 > 0)){
      coTop250Col = append(coTop250Col, TRUE)
    }
    else
    {
      coTop250Col = append(coTop250Col, FALSE)
    }
    coGem50 = intersect(imdb.data$actors$gem50, id)
    if (length(coGem50 > 0)){
      coGem50Col = append(coGem50Col, TRUE)
    }
    else
    {
      coGem50Col = append(coGem50Col, FALSE)
    }
  }
  df = data.frame(coActorID = coActorsId, Pop50 = coPop50Col, headliners = coHeadlinersCol, Top250 = coTop250Col, Gem250 = coGem50Col)
  return(df)
}

# Check which directors made the headliner
getDirectorRankings <- function(coActorsId)
{
  directerHeadliner <- c()
  for (i in 1:length(coActorsId))
  {
    if (coActorsId[i] != willID || coActorsId[i] != denzelID)
    {
      directer <- intersect(imdb.data$headliners$directors, coActorsId[i])
      if (length(directer > 0)){
        directerHeadliner = append(directerHeadliner, TRUE)
        
      }
      else
      {
        directerHeadliner = append(directerHeadliner, FALSE)
      }
    }
  }
  df = data.frame(nmid = coActorsId, topDirector = directerHeadliner)
  return(df)
}
