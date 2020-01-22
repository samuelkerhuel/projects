
################################################################################################
#   NAME:       select_baseline_users
#   DESCRIPTION:Extracts users from input data frame based on their number of ratings. 
#               Those users will later serve as baseline user to recommend movies to.
#
#   INPUTS:     UMR_df:                         user/movies/ratings data frame
#               user_desc:                      descriptors for users to be extracted, this contains the number of ratings of users to be extracted
#   OUTPUTS:    baseline_users:                 data frame containing selected users
#               UMR_df_out:                     output user/movies/ratings data frame where selected users have been removed
################################################################################################
select_baseline_users <- function(UMR_df, user_desc){
  # number of users in user_desc vector
  N_user              = length(user_desc)
  # count the number of rating per user
  counts_user         <- table(UMR_df$userId)
  # declare baseline_users as an list. It will grow in loop
  baseline_users      = list()
  # initialize output data frame to input data frame, users will be iteratively extracted from this data frame
  UMR_df_out          = UMR_df
  #process all users
  for(i in 1:N_user){
    # select user having enough ratings
    user_sel            = counts_user[counts_user >= user_desc[i]]
    # sort users by number of ratings
    user_sort           = sort(user_sel)
    # take the first user meeting the criteria
    user                = as.integer(names(user_sort[1]))
    # remove current user from data base
    UMR_df_out          = subset(UMR_df_out,userId!=user)
    # find where this user appears in the data frame
    baseline_users[[i]] = UMR_df[UMR_df$userId == user,]
  }
  
  # collect output variables in a list
  ret_l = list("baseline_users_df"=baseline_users , "input_df"=UMR_df_out)
  return(ret_l)
  
}
################################################################################################
#   NAME:       build_rating_matrix
#   DESCRIPTION:build_rating_matrix selects in a data frame users and movies that have a minimum number of ratings. 
#               From there it generate a rating matrix
#
#   INPUTS:     UMR_df:                         users/movies/ratings data frame
#               user_ratings_thres:             minimum number of ratings per user
#               movie_ratings_thres:            minimum number of ratings per movie
#   OUTPUTS:    RM:                             rating matrix
#               movie_title                     titles of movies in the rating matrix
################################################################################################
build_rating_matrix <- function(UMR_df, user_ratings_thres, movie_ratings_thres){
  
  #find users with more that min number of rating
  counts_user         <- table(UMR_df$userId)
  user_in_RM          = names(counts_user)[counts_user > user_ratings_thres]

  #find movies with more that min number of rating
  counts_movie        <- table(UMR_df$movieId)
  movie_in_RM         = names(counts_movie)[counts_movie > movie_ratings_thres]

  # number of elements in the data frame
  N_df_entries        = dim(UMR_df)[1]
  # number of users selected
  N_user              = length(user_in_RM)  
  # number of movies selected
  N_mov               = length(movie_in_RM)
  # initialise rating matrix to all NA values
  Rating_mat          = matrix(data=NA,nrow=N_user,ncol=N_mov)
  # initialise movies with empty string
  movie_title_vec     = replicate(N_mov, "")
  #loop over all data frame elements to find those meeting the criteria regarding number of ratings
  for (i in 1:N_df_entries){
    # user index in the movie data base
    user_idx                = UMR_df$userId[i]
    # movie index in the movie data base
    mov_idx                 = UMR_df$movieId[i]
    # movie title in the movie data base
    mov_tit                 = UMR_df$title[i]
    
    # select only the entry if user and films have enough ratings
    if (is.element(user_idx, user_in_RM) & is.element(mov_idx, movie_in_RM)){
      # find user idx in rating matrix
      u_idx                   = match(user_idx , user_in_RM)
      # find movie idx in rating matrix
      m_idx                   = match(mov_idx , movie_in_RM)

      #find rating value
      rating_val              = UMR_df$rating[i]
      # put rating value in rating matrix
      Rating_mat[u_idx,m_idx] = rating_val
      # fill in corresponding movie title
      movie_title_vec[m_idx]  = mov_tit
    }
    
  }
  
  # index of selected movies i.e movies with at least 1 rating
  sel_mov_idx = (colSums(!is.na(Rating_mat)) != 0)
  # select ratings 
  RM          = Rating_mat[,sel_mov_idx]
  # select movie titles
  movie_title = movie_title_vec[sel_mov_idx]

  # collect output variables in a list
  ret_l = list("RM"=RM , "title"=movie_title)
  return(ret_l)
  
}

################################################################################################
#   NAME:       build_new_user_rating_vector
#   DESCRIPTION:build rating vector for a new user
#
#   INPUTS:     new_user_df:                    new user data frame of users/movies/ratings
#               mov_title_vec:                  movie titles
#   OUTPUTS:    Rating_vec:                     rating vector for new user
################################################################################################
build_new_user_rating_vector <- function(new_user_df, mov_title_vec){
  cat("display user df \n")
  print(new_user_df)
  #user data frame
  user_df             = new_user_df            
  # number of elements in data frame
  N_df_entries        = dim(user_df)[1]
  # number of movies in rating vector
  N_mov               = length(mov_title_vec)
  # rating vector for user
  Rating_vec          = replicate(N_mov, NA)
  #loop over all data frame elements for this user
  for (i in 1:N_df_entries){
    # check that no fields are NA
    # if ( (!is.na(user_df$title[i])) && (!is.na(user_df$rating[i])) && (!is.na(user_df$userId[i])) ){
    if ( (!is.na(user_df$title[i])) && (!is.na(user_df$rating[i])) ){
        # find movie index in the rating matrix
      mov_idx           = (mov_title_vec == user_df$title[i])
      # movie title is in the rating matrix
      if (sum(mov_idx) == 1){
        Rating_vec[mov_idx] = user_df$rating[i]
      }
    }
  }
  
  return(Rating_vec)
  
}
 
################################################################################################
#   NAME:       norm_data
#   DESCRIPTION:norm_data normalizes data with center or Z-score method using recommenderlab normalize function
#
#   INPUTS:     data:                           matrix of ratings: user Vs movies
#               norm_type:                      normalization to be performed: "center", "Z-score", "none"
#   OUTPUTS:    data_norm:                      normalized matrix of user Vs movies ratings
################################################################################################
norm_data <- function(data, norm_type){
  if (norm_type == "center"){
    data_norm   <- normalize(as(data, "realRatingMatrix"))
  }
  else if (norm_type == "Z-score"){
    data_norm   <- normalize(as(data, "realRatingMatrix"), method="Z-score")
  }
  else if (norm_type == "none"){
    data_norm   <- as(data, "realRatingMatrix")
  }
  else{
    stop("error: wrong normalization method")
  }
  return(as(data_norm,"matrix"))
}


################################################################################################
#   NAME:       plot_rate_matrix
#   DESCRIPTION:plot_rate_matrix displays rate matrix and its characteristics
#
#   INPUTS:     RM_Rmat:                        matrix of ratings: user Vs movies 
#   OUTPUTS:    NONE
################################################################################################
plot_rate_matrix <- function(RM_Rmat){

  # convert to recommenderlab rating matrix format
  RM        = as(RM_Rmat, "realRatingMatrix")
  
  # x axis label
  x_str     = paste(toString(dim(RM)[1]), " Users")
  # y axis label
  y_str     = paste(toString(dim(RM)[2]) , " Movies")
  #plot rating matrix
  image(RM, main="all ratings over all users and all movies", xlab = x_str, ylab = y_str)
  
  # print number of rating per user for visual sanity check
  cat(" show first users total ratings", "\n")
  head(rowCounts(RM),n=10)
  # print number of rating per movie for visual sanity check
  cat(" show first movies total ratings", "\n")
  head(colCounts(RM),n=10)
  
  # plot the number of user ratings
  hist(rowCounts(RM), main="distribution of total number of users ratings", xlab = "total number of users ratings")
  # # plot ratings distribution: histogram of the average user ratings
  hist(rowMeans(RM), main="distribution of average user ratings distribution", xlab = "average user rating")
  
  # index in rating matrix where a rating is present
  valid_ratings_ind       = !(is.na(RM_Rmat))                   
  cat("total number of retained ratings are :" , length(RM_Rmat[valid_ratings_ind]))
  # identical(length(RM[valid_ratings_ind]), sum(rowCounts(RM1)))       # sanity check
  # identical(length(RM[valid_ratings_ind]), sum(colCounts(RM1)))       # sanity check
  cat("max rating value is :" , max(RM_Rmat[valid_ratings_ind]), "\n")
  cat("min rating value is :" , min(RM_Rmat[valid_ratings_ind]), "\n")
  cat("mean user rating is :" , mean(rowMeans(RM)), "\n")
  
  # explore the movie rating dimension
  # plot the number of movies ratings
  hist(colCounts(RM), main="average number of ratings per movie", xlab = "average number of ratings per movie")
  # plot rating per film
  hist(colMeans(RM), main="average movie ratings distribution", xlab = "average movie rating")
  # mean film rating
  cat("mean movie rating is :" , mean(colMeans(RM)), "\n")

}

################################################################################################
#   NAME:       build_similarity_matrix
#   DESCRIPTION:build similarity matrix
#
#   INPUTS:     RM:                             matrix of ratings: user Vs movies
#   OUTPUTS:    S:                              similarity matrix
################################################################################################
build_similarity_matrix <- function(RM){

  # number of users
  N_user          = dim(RM)[1]
  # Initialize similarity matrix with 0. 0 means that no similarity is computed because users rated not a single film in common
  S               = matrix(0,nrow=N_user,ncol=N_user)
  # compute similarity for all users
  for(i in 1:N_user){
    # ratings for all films from user i
    RM_i  = RM[i,]            
    # film index that user i actually rated are TRUE
    i_ind = !is.na(RM_i)      
    # loop boundary control
    if ((i+1) <= N_user){
      # compute only triangle matrix since matrix is symetric
      for(j in (i+1):N_user){
        # ratings for all films from user j
        RM_j  = RM[j,]        
        # film index that user j actually rated are TRUE
        j_ind = !is.na(RM_j)  
        
        # film index that both i & j rated
        m     = i_ind & j_ind 
        # if i and j users rated one same film
        if (length(m) > 0){    
          # numerator is sum over all movies of product of both ratings
          num       = sum(RM[i,m] * RM[j,m])
          # denominator is square root of product of sum over all movies of ratings squared
          den       = sqrt( sum(RM[i,m]^2) * sum(RM[j,m]^2) )
          
          # compute similarity if denominator isn't 0
          if (den != 0){
            S[i,j]    = num / den
            # matrix is symetric
            S[j,i]    = S[i,j]
          } 
        }
      }
    }
  }

  # compute matrix diagonal, each user is totally similar to itself i.e. similarity = 1
  for(i in 1:N_user){
    S[i,i]      = 1
  }
    
    return(S)
}


################################################################################################
#   NAME:       add_user_to_similarity_matrix
#   DESCRIPTION:compute similarity values of a new user and add them to similarity matrix
#
#   INPUTS:     RM:                             matrix of ratings: user Vs movies
#               sim_mat:                        similarity matrix
#               user_rating:                    new user vector of ratings
#   OUTPUTS:    new_sim_mat:                    updated similarity matrix including new user
################################################################################################
add_user_to_similarity_matrix <- function(RM, sim_mat, user_rating){
  
  # number of movies
  N_movie                         = dim(RM)[2]
  # number of users
  N_users                         = dim(RM)[1]
  
  # replicate by 2 only to create a matrix as required by normalize function, then only 1st element is used
  user_mat                        = t(replicate(2,user_rating))
  # normalize ratings for new user
  user_mat_norm                   = norm_data(user_mat, "center")
  # initiliase similarity matrix adding 1 user
  new_sim_mat                     = matrix(data=0,nrow=(N_users+1),ncol=(N_users+1))
  # the first N_users x N_users elements are just a copy of existing similarity matrix
  new_sim_mat[1:N_users,1:N_users]= sim_mat 
  # simlarity value of current user with itself is 1
  new_sim_mat[N_users+1,N_users+1]= 1
  
  # compute similarity value of new user Vs other users
  for(i in 1:N_users){
    # ratings for all films from user i
    RM_i  = RM[i,]            
    # film index that user i actually rated are TRUE
    i_ind = !is.na(RM_i)      
    # ratings for all films from user j
    RM_j  = user_mat_norm[1,]        
    # film index that user j actually rated are TRUE
    j_ind = !is.na(RM_j)  
    
    # film index that both i & j rated
    m     = i_ind & j_ind 
    # if i and j users rated one same film
    if (length(m) > 0){    
      # numerator is sum over all movies of product of both ratings
      num       = sum(RM[i,m] * RM_j[m])
      # denominator is square root of product of sum over all movies of ratings squared
      den       = sqrt( sum(RM[i,m]^2) * sum(RM_j[m]^2) )
      # compute similarity if denominator isn't 0
      if (den != 0){
        new_sim_mat[i,N_users+1]    = num / den
        # matrix is symetric
        new_sim_mat[N_users+1,i]    = new_sim_mat[i,N_users+1]
      } 
    }
  }
  
  return(new_sim_mat)

}
  
################################################################################################
#   NAME:       k_nearest_neigh
#   DESCRIPTION:compute the k nearest neighbors to current user user_idx in similarity matrix S
#
#   INPUTS:     S:                              similarity matrix
#               user_idx:                       user index for which to compute nearest neighbors
#               k:                              number of nearest neighbors
#   OUTPUTS:    k_list:                         list of nearest neighbors user indices
################################################################################################
k_nearest_neigh <- function(S, user_idx, k){
  # number of users
  N_user = dim(S)[1]
  # if k is higher than number of users - minus 1 limit k
  if (k > (N_user-1) ) {
    k   = (N_user-1)
  }

  # get similarity for current user
  user_sim        = S[user_idx,]
  # sort similarity numbers
  user_sim_sorted = sort(user_sim, decreasing = TRUE,index.return=TRUE)

  # take k+1 highest similarities. take k+1 since current user is normally included in the list
  k_list            = user_sim_sorted$ix[0:k+1]
  # remove user_idx from list
  k_list            = k_list[k_list != user_idx]
  # in case list still has k+1 elements, it means that user index was not included, then last element should be removed
  if (length(k_list) == (k+1)){
    k_list          = k_list[1:k]
  }

  return(k_list)
}

################################################################################################
#   NAME:       UBCF_prediction
#   DESCRIPTION:compute prediction for 1 user all movies based on its k neighbors. !!! this assumes normalized centered ratings !!!
#               Prediction is a weighted average of ratings from k nearest neighbors
#               weigths are the similarity weights between current users and its k nearest neighbors
#               prediction is normalised by the sum of weights from neighbors included in the prediction. This is used to keep prediction in the correct rating scaling range
#   INPUTS:     RM:                             matrix of ratings: user Vs movies
#               k_set:                          list of nearest neighbors user indices
#               S_k_set:                        similarity values for nearest neighbors Vs current user
#   OUTPUTS:    P:                              Prediction of ratings for current user
################################################################################################
UBCF_prediction <- function(RM,  k_set , S_k_set){
  # number of movies
  N_movie   = dim(RM)[2]
  # initialize all movie ratings to NA i.e no rating. Movies that haven't been seen by any neighbors users won't get any prediction rating since no rating is available to perform this prediction
  P         = replicate(N_movie, NA)
  # den is the value for denormalization i.e. sum of weights from neighbors included in the prediction. It is not just a single value equal to the sum of weights from all k neighbors because all neighbors didn't all rates the same movies, prediction is typically a sum of a few neighbors ratings. So a value per prediction is required i.e. den is a vector.
  den       = replicate(N_movie, 0)
  # number of elements in the k set
  K         = length(k_set)

  # The first part aims at initializing to 0 predictions that will get a result i.e. rating for movies where at least 1 neighbor rated. It is necessary to initiliase to 0 instead of NA so that later sum has a a valid number
  # First look at which movies have at least 1 rating in the k set
  # By default all movies have no ratings
  all_av_idx = replicate(N_movie, FALSE)
  # loop on all neighbors
  for(j in 1:K){
    # compute index of jth user in the rating matrix
    j_idx       = k_set[j]
    # get all ratings for user j
    j_rating    = RM[j_idx , ]
    # compute available ratings indices for user j i.e. those with a valid rating i.e. non NA
    av_idx      = !is.na(j_rating)
    # available ratings for all neighbors is an OR accross all neighbors
    all_av_idx  = all_av_idx | av_idx
  }

  # initialize predictions for movies in k set to 0
  P[all_av_idx] = 0

  # loop on all neighbors users in the k set, Compute predictions on all movies
  for(j in 1:K){
    # index of jth user in the rating matrix
    j_idx       = k_set[j]
    # rating for user j
    j_rating    = RM[j_idx , ]
    #available ratings indices for user j
    av_idx      = !is.na(j_rating)

    # Prediction is the weighted average of neighbors ratings. Weights are the similarity weights.
    P[av_idx]   = P[av_idx] + j_rating[av_idx] * S_k_set[j]
    # normalization factor includes similarity weight for all movies this neighbor rated
    den[av_idx] = den[av_idx] + S_k_set[j]
  }
  # normalize by similarity weights included in each prediction
  P = P / den

  return(P)
}

################################################################################################
#   NAME:       recommendation
#   DESCRIPTION:recommendation does a recommendation of N movies based on prediction
#               Several types of recommendation are implemented bade on prediction ratings and removing movies user already saw
#                   1. HIGHER MATCH         : regular UBCF, recommend higher prediction ratings
#                   2. POPULAR              : modified UCBF: select movies with rating predictions above a threshold, then take the most populars (defined as the 25% most watched movies) in this selection, randomly recommend N films in this selection
#                   3. OFF THE BEATEN TRACK : modified UCBF: select movies with rating predictions above a threshold, then take the most populars (defined as the 25% less watched movies) in this selection, randomly recommend N films in this selection
#                   4. SURPRISE SURPRISE    : modified UCBF: select movies with rating predictions above a threshold, randomly recommend N films in this selection. Each movies get equal chance to be randomly selected
#                   5. ALLOW SURPRISE       : modified UCBF: select movies with rating predictions above a threshold, randomly recommend N films in this selection. Each movies get chance proportional to its rating prediction to be randomly selected
#
#   INPUTS:     RM:                             matrix of ratings: user Vs movies
#               Pred:                           Prediction of ratings for user to recommend movies to
#               N:                              number of movies to recommend
#               user_rating:                    ratings for user to recommend movies to
#               min_rate:                       threshold value to declare a movie good for recommendation, recommend movies with ratings above the threshold
#               recom_type:                     type of recommendation: "HIGHER MATCH", "POPULAR", "OFF THE BEATEN TRACK", "SURPRISE SURPRISE", "ALLOW SURPRISE" 
#               movie_title:                    titles of all movies
#   OUTPUTS:    rec_title:                      titles of recommende movies
################################################################################################
recommendation <- function(RM, Pred, N, user_rating, min_rate, recom_type, movie_title){
  # total number of movies
  N_movies              = length(user_rating)
  # movies rated by users
  av_idx                = !is.na(user_rating)
  # number of movies rated by current user
  N_ratings             = sum(av_idx)

  # remove movies user already saw, set predictions to NA
  Pred[av_idx]          = NA

  # remove ratings below minimum recommendation threshold, set them to NA
  bad_ratings_idx       = Pred < min_rate
  Pred[bad_ratings_idx] = NA

  # "POPULAR" and "OFF THE BEATEN TRACK" methods are based on number of ratings per film so look at this here
  if ((recom_type == "POPULAR") || (recom_type == "OFF THE BEATEN TRACK")) {
    # count the number of recommendation per movie
    N_rat_per_mov         = colSums(!is.na(RM))
    # sort users per number of ratings
    sort_mov_pop          = sort(N_rat_per_mov, decreasing = TRUE,index.return=TRUE)
    N_m                   = length(sort_mov_pop$x)
    if (recom_type == "POPULAR"){
      # define popular movies as the first quartile
      # Do not take directly take the first quarter of the elements because it doesn't necessarily cut nicely distribution count. Instead see what is the count at this position and define it as the threshold.
      # find the count defining popular movies
      count_pop             = sort_mov_pop$x[(N_m/4)]
      # popular movies are all movie indices that have at least count_pop ratings
      pop_mov               = sort_mov_pop$ix[sort_mov_pop$x >= count_pop]

      # ratings for all movies not in popular list are set to NA
      Pred[-pop_mov]        = NA

    } else if (recom_type == "OFF THE BEATEN TRACK"){
      # define confidential movies as the last quartile
      # find the count defining confidential movies
      count_conf            = sort_mov_pop$x[N_m - (N_m/4) + 1]
      conf_mov              = sort_mov_pop$ix[sort_mov_pop$x <= count_conf]

      # ratings for all movies not in confidential list are set to NA
      Pred[-conf_mov]        = NA
    }
  }

  # sort Prediction numbers
  Pred_sorted           = sort(Pred, decreasing = TRUE,index.return=TRUE)

  # perform recommendation depending on type of recommendation
  if (recom_type == "HIGHER MATCH"){
    # take the highest predictions
    recom                 = Pred_sorted$ix[0:N]
  } else if (recom_type == "POPULAR") {
    # take the highest predictions, where only most popular movies have been kept
    recom                 = Pred_sorted$ix[0:N]
  } else if (recom_type == "OFF THE BEATEN TRACK") {
    # take the highest predictions, where only confidential movies have been kept
    recom                 = Pred_sorted$ix[0:N]
  } else if (recom_type == "SURPRISE SURPRISE") {
    # randomly select any movie with equal chance for all
    recom                 = sample(Pred_sorted$ix,size=N,replace = FALSE)
  } else if (recom_type == "ALLOW SURPRISE") {
    # probability that a film gets selected is proportional to its rate
    prob_select           = Pred_sorted$x/sum(Pred_sorted$x)
    recom                 = sample(Pred_sorted$ix,size=N,replace = FALSE, prob=prob_select)
  } else  {
    stop("error: unknown recommendation method requested")

  }

    # titles of recommended movies
    rec_title             = movie_title[recom[!is.na(recom)]]

  return(rec_title)
}


################################################################################################
#   NAME:       Predict_rate_mat
#   DESCRIPTION:Predict_rate_mat performs predictions for all users all movies
#
#   INPUTS:     RM:                             matrix of ratings: user Vs movies
#               S:                              similarity matrix
#               kNN:                            number of nearest neighbors
#   OUTPUTS:    RM_pre:                         matrix of prediction ratings: user Vs movies
################################################################################################
Predict_rate_mat <- function(RM,S,kNN){
  # number of users
  N_user      = dim(RM)[1]
  # initialise prediction ratings matrix to ratings matrix
  RM_pre      = RM
  # performs prediction for all users
  for(user_idx in 1:N_user){
    # the k user index in the similarity matrix
    k_set                       = k_nearest_neigh(S,user_idx, k=kNN)
    # similarity values of the k nearest neighbors
    S_k_set                     = S[user_idx,k_set]
    # get prediction ratings for current user
    P                           = UBCF_prediction(RM , k_set , S_k_set)
    # fill in prediction ratings matrix
    RM_pre[user_idx,]           = P
  }

  return(RM_pre)

}

################################################################################################
#   NAME:       plot_RMSE
#   DESCRIPTION:plot RMSE metric
#
#   INPUTS:     RMSE:                           RMSE metric
#               N_user:                         number of users
#               N_mov:                          number of movies
#   OUTPUTS:    NONE
################################################################################################
plot_RMSE <- function(RMSE,N_user,N_mov,kNN_list){
  # plot title
  tit    = paste("RMSE for ", N_user, " users and ", N_mov, " movies", sep = "")
  # plot RMSE
  barplot(RMSE,names.arg=kNN_list, xlab="number of k neighbors", ylab="RMSE", main = tit)
}

################################################################################################
#   NAME:       plot_confusion_KPI
#   DESCRIPTION:plot KPIs associated to confusion matrix: accuracy, precision, recall
#
#   INPUTS:     KPI:                            KPIs associated to confusion matrix: accuracy, precision, recall
#               N_user:                         number of users
#               N_mov:                          number of movies
#   OUTPUTS:    NONE
################################################################################################
plot_confusion_KPI <- function(KPI,N_user,N_mov,kNN_list){
  # number of KPIs
  N         = length(KPI)
  # initialise accuracy,precision,recall as empty vectors
  accuracy  = vector()
  precision = vector()
  recall    = vector()
  #get accuracy,precision,recall and put them in vector
  for(i in 1:N){
    accuracy[i]   = KPI[[i]]$accuracy
    precision[i]  = KPI[[i]]$precision
    recall[i]     = KPI[[i]]$recall
  }
  # plot title
  tit    = paste("accuracy for ", N_user, " users and ", N_mov, " movies", sep = "")
  # plot accuracy
  barplot(accuracy,names.arg=kNN_list, xlab="number of k neighbors", ylab="accuracy", main = tit)

  # plot title
  tit    = paste("recall & precision for ", N_user, " users and ", N_mov, " movies", sep = "")
  # plot recall Vs precision
  plot(x=recall, y=precision,xlab="recall", ylab="precision", type = "l", main = tit)

  cat("\n accuracy = ",  accuracy)
  cat("\n precision = ", precision)
  cat("\n recall = ",    recall)
  
}
# ################################################################################################
#   NAME:       binarize_pred
#   DESCRIPTION:binarize_pred transforms rating matrix prediction RM_pred to binary. 0/1 if rating is below/above thres_01
#
#   INPUTS:     RM_pred:                        rating matrix predictions
#               thres_01:                       rating threshold to define rating as 0 (not recommended) or 1 (recommended)
#   OUTPUTS:    Pred_bin:                       rating matrix predictions binarized
################################################################################################
binarize_pred <- function(RM_pred, thres_01){
  # initialize binary matrix to input rate matrix. Goal here is to get NA at the right places. 0/1 will be placed in the next lines
  Pred_bin   = RM_pred

  # find elements below threshold
  Pred_0_idx = (RM_pred <= 0)
  # set 0 elements
  Pred_bin[Pred_0_idx] = 0

  # find elements above threshold
  Pred_1_idx = (RM_pred > 0)
  # set 1 elements
  Pred_bin[Pred_1_idx] = 1

  return(Pred_bin)
}


################################################################################################
#   NAME:       confusion_KPI
#   DESCRIPTION:confusion_KPI computes confusion matrix and associated KPIs: accuracy, precision, recall
#
#   INPUTS:     RM:                             matrix of ratings: user Vs movies
#               RM_pred:                        rating matrix predictions
#               thres_01:                       rating threshold to define rating as 0 (not recommended) or 1 (recommended)
#   OUTPUTS:    accuracy:                       accuracy KPI
#               precision:                      precision KPI
#               recall:                         recall KPI
################################################################################################
confusion_KPI <- function(RM, RM_pred, thres_01){

# binarise Rating matrix RM
RM_bin          = binarize_pred(RM,      thres_01)
# binarise Prediction Rating matrix RM_pred
RM_pred_bin     = binarize_pred(RM_pred, thres_01)
# compute indices where valid values are in both matrices
ind_com         = (!is.na(RM)) & (!is.na(RM_pred))
# reduce RM_bin to its valid entries
RM_bin_red      = RM_bin[ind_com]
# reduce RM_pred_bin to its valid entries
RM_pred_bin_red = RM_pred_bin[ind_com]
# remove RM_bin & RM_pred_bin since no longer used
rm(RM_bin)
rm(RM_pred_bin)

# compute confusion matrix elements
a               = sum( (RM_bin_red == 0) & (RM_pred_bin_red == 0) )
b               = sum( (RM_bin_red == 0) & (RM_pred_bin_red == 1) )
c               = sum( (RM_bin_red == 1) & (RM_pred_bin_red == 0) )
d               = sum( (RM_bin_red == 1) & (RM_pred_bin_red == 1) )

# compute confusion matrices KPIS
accuracy        = (a+d) / (a+b+c+d)
precision       = d     / (b+d)
recall          = d     / (c+d)

# collect output variables in a list
ret_l = list("accuracy"=accuracy, "precision"=precision, "recall"=recall)
return(ret_l)

}

################################################################################################
#   NAME:       remove_year
#   DESCRIPTION:remove year from title i.e. any character strings "(abcd)" where a,b,c,d are any integer from 0 to 9
#
#   INPUTS:     title:                          movie title
#   OUTPUTS:    title:                          movie title without the year
################################################################################################
remove_year <- function(title){
  # remove pattern of (wxyz) where w,x,y,z are any digit between 0 to 9
  return(sub("\\(([0-9]){4}\\)","",title))
}

################################################################################################
#   NAME:       clean_title
#   DESCRIPTION:clean a list of movie titles
#
#   INPUTS:     title_l:                        list of movie titles to clean
#   OUTPUTS:    title_l:                        list of movie titles cleaned
################################################################################################
clean_title <- function(title_l){
  # number of movies in the list
  N = length(title_l)
  # process all movies in the list
  for(i in 1:N){
    # get title
    title         = title_l[i]
    # remove the year from title
    title1        = remove_year(title)
    
    # get the index where pattern ", The" appears in title
    the_ind       = unlist(gregexpr(", The", title1))
    # if pattern is present
    if (the_ind > 0){
      # remove pattern ", The"
      title2      = sub(", The","",title1)
      # add "The" at the beginning
      title_l[i]  = paste("The",title2, sep=" ")
    } else{
      title_l[i]  = title1
    }
    
  }
  return(title_l)
}

