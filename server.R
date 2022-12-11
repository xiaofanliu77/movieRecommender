## server.R

# load functions
source('functions/cf_algorithm.R') # collaborative filtering
source('functions/similarity_measures.R') # similarity measures

# define functions
get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# read in movies and ratings data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

genres = as.data.frame(movies$Genres, stringsAsFactors=FALSE)
tmp = as.data.frame(tstrsplit(genres[,1], '[|]',
                              type.convert=TRUE),
                    stringsAsFactors=FALSE)
genre_list = c("Action", "Adventure", "Animation", 
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical", 
               "Mystery", "Romance", "Sci-Fi", 
               "Thriller", "War", "Western")

m = length(genre_list)
genre_matrix = matrix(0, nrow(movies), length(genre_list))
for(i in 1:nrow(tmp)){
  genre_matrix[i,genre_list %in% tmp[i,]]=1
}
colnames(genre_matrix) = genre_list
remove("tmp", "genres")

## System I
# save top 10 most popular movies in every genre in a table
top10Popular <- matrix(0, nrow = 10, ncol = length(genre_list))
colnames(top10Popular) <- genre_list
for (g in genre_list) {
  m_ids = which(genre_matrix[,g] == 1)
  m_ids = movies[m_ids, 'MovieID']
  
  tmp = ratings %>% 
    filter(MovieID %in% m_ids) %>%
    group_by(MovieID) %>%
    summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
    inner_join(movies, by = 'MovieID') %>%
    arrange(desc = -ratings_per_movie)
  
  top10 = as.numeric(unlist(tmp[11, 'ratings_per_movie']))
  res = tmp %>%
    filter(ratings_per_movie > top10) %>%
    select(c("MovieID", "Title","ave_ratings")) %>%
    arrange(desc = -ave_ratings)
  top10Popular[, g] = res$MovieID
}


# System II: recommenderLab -> UBCF

i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
Rmat = new('realRatingMatrix', data = Rmat)

Rmat = Rmat[1:500, ]

recommender.UBCF <- Recommender(Rmat, method = "UBCF",
                                parameter = list(normalize = 'Z-score', 
                                                 method = 'Cosine', 
                                                 nn = 25))

shinyServer(function(input, output, session) {
  # show the movies to be rated (recommend by rating)
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 # here it is displaying the first 120 movies (row number, not movie ID)
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # By Genre: get recommendations from table when button is clicked
  df_g <- eventReactive(input$btnByGenre, {
    withBusyIndicatorServer("btnByGenre", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's genre data
      userGenre = input$selectGenre
      #print(userGenre)
      m_ids <- top10Popular[, userGenre]
      #print(m_ids)
      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = movies$MovieID[movies$MovieID %in% m_ids], 
                                  Title = movies$Title[movies$MovieID %in% m_ids] 
                                  )
      #print(recom_results$MovieID)
      
    }) # still busy
    
  }) # clicked on button
  
  # By rating: Calculate recommendations when the sbumbutton is clicked (UBCF)
  # Edge case: output most popular movies in Drama genre (the most popular genre)
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      # print(value_list)
      user_ratings <- get_user_ratings(value_list)
      # print(user_ratings)
      if (dim(user_ratings)[1] > 0) {
        
      
        movieIDs = colnames(Rmat)
        n.item = ncol(Rmat)  
        new.ratings = rep(NA, n.item)
        
        # plug in new user's ratings
        for (i in 1:dim(user_ratings)[1]) {
          m_id = paste0('m', user_ratings$MovieID[i])
          new.ratings[which(movieIDs == m_id)] = user_ratings$Rating[i]
        }
        # print(head(new.ratings))
        
        # generate the real rating matrix
        new.user = matrix(new.ratings, 
                          nrow=1, ncol=n.item,
                          dimnames = list(
                            user=paste('liu'),
                            item=movieIDs
                          ))
        new.Rmat = as(new.user, 'realRatingMatrix')
        
        p.UBCF <- predict(recommender.UBCF, new.Rmat, type="ratings")
        
        # predicted top 10 movie row numbers (in ratings table)
        top10.indices = order(as(p.UBCF, "matrix"), decreasing = TRUE)[1:10]
  
        # convert row numbers to movie ids
        m_ids = tmp[top10.indices, ]$j
        user_predicted_ids = as.numeric(gsub("m", "", m_ids))
        
        #print("*****************User predicted ids")
        #print(user_predicted_ids)
        
        recom_result <- data.table(Rank = 1:10, 
                                    MovieID = movies$MovieID[movies$MovieID %in% user_predicted_ids], 
                                    Title = movies$Title[movies$MovieID %in% user_predicted_ids]
                                    )
      } else {
        m_ids <- top10Popular[, "Drama"]
        recom_result <- data.table(Rank = 1:10, 
                                   MovieID = movies$MovieID[movies$MovieID %in% m_ids], 
                                   Title = movies$Title[movies$MovieID %in% m_ids] 
                                    )
      }
      
    }) # still busy
    
  }) # clicked on button
  

  # display the recommendations
  output$results <- renderUI({
    
    recom_result <- df()
    
    # print(recom_result)
    
    num_rows <- 2
    num_movies <- 5 # movies per row
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
  

  
  
  # display recommended movies based on user's genre
  output$resultsByGenre <- renderUI({
    
    
    #results are precalculated and saved in a table
    recom_results <- df_g()
    num_rows <- 2
    num_movies <- 5 # movies per row
    
    # print(recom_results$MovieID)
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
              
            div(style = "text-align:center", 
                #a(img(src = movies$image_url[recom_results$MovieID[(i - 1) * num_movies + j]], height = 150))
                a(img(src = movies$image_url[movies$MovieID == recom_results$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[movies$MovieID == recom_results$MovieID[(i - 1) * num_movies + j]])
            )
              
        )        
      }))) # columns
    }) # rows
  })
  
}) # server function
