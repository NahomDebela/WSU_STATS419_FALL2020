getActorInfo <- function(moviesID)
{
  
  movies <- c()
  for (i in 1:length(moviesID))
  {
    id = moviesID[i]
    title = imdb.data$all.actors.movies$title[which(imdb.data$all.actors.movies$ttid == id)]
    movies = append(movies, title)
  }
  
  df = data.frame(MovieID = moviesID,MovieName = movies)
}