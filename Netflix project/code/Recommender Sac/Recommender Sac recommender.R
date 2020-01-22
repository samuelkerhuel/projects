# This file contains contains a single function that performs the recommender system. 
################################################################################################
#   NAME:       run_recommender
#   DESCRIPTION:Performs the recommender system
#               It is called from the main file when called in R standalone environment or from the app when used in Rshiny
#               It can either perform recommendation or evaluate recommender system performances
#
#               This file sources and uses 2 companions files:
#                   1. Recommender Sac functions.R  : functions used in main, both recommender processing and utilities functions
#                   2. Recommender set parameters.R : definition of parameters used in main
#               It implements the entire structure of the recommender: read/select input, process & display data
#               It used in 2 contexts: recommend movies for a user or evaluate performances KPI of baseline recommender. Parameter proc_type_fg is used to choose between both modes.
#               To allow a reasonable processing execution time it can use pre store inputs. 
#               Indeed the heavy processing happens in the first part of the processing (read inputs, build rating matrix, build similarity matrix).
#               Also this processing works on the user data base which is constant and independent of new user ratings
#               There's then no added value to perform this processing at each run when dealing with new user ratings
#               The flag RM_compute_fg controls if all computing is performed (RM_compute_fg=1) or if first processing results are restored from memory (RM_compute_fg=0)
#               The recommender can work from different baseline number of users. To ensure meaningful results, only users with a minimum of ratings are considered (parameter N_min_user_ratings)
#               Baseline users are users extracted from the data base to be later recommended some movies. In Rshiny, new ratings can be added for those users.
#               A minimum number of ratings per film is also used (parameter N_min_movie_ratings). However this is is typically set to 1 i.e. no restriction on number of ratings per films. 2 reasons for setting it to 1: 1) keeping only movies with a given amount of ratings can force a popular recommender and not recommended less popular movies. 2) removing movies and their ratings will also lower number of ratings for users that rated those movies so will also impact number of ratings per users i.e. impact the criterion "minimum rating per user" that is desired in the first place.
#               The recommender implemented is user based collaborative filtering (UBCF). It implements the standard approach based on best rating but also some alternatives to provide user alternative approaches. The approach taken is defined in proc_type_fg.
#                   1. HIGHER MATCH         : regular UBCF, recommend higher prediction ratings
#                   2. POPULAR              : modified UCBF: select movies with rating predictions above a threshold, then take the most populars (defined as the 25% most watched movies) in this selection, randomly recommend N films in this selection
#                   3. OFF THE BEATEN TRACK : modified UCBF: select movies with rating predictions above a threshold, then take the most populars (defined as the 25% less watched movies) in this selection, randomly recommend N films in this selection
#                   4. SURPRISE SURPRISE    : modified UCBF: select movies with rating predictions above a threshold, randomly recommend N films in this selection. Each movies get equal chance to be randomly selected
#                   5. ALLOW SURPRISE       : modified UCBF: select movies with rating predictions above a threshold, randomly recommend N films in this selection. Each movies get chance proportional to its rating prediction to be randomly selected
#               UCBF concept is to make predictions for a given user based on the K users that are the most similar to current user
#               For similarity, cosine similarity formula is chosen since it's the most standard approach
#               The K closest neighbors are the K neighbors with the highest similarity number in the similarity matrix for row or column associated to current user 
#               The number of nighbors (K) is a parameter and several K values can be evaluated. K values are defined in kNN_vec
#
#             Below is the function calling tree:
              # run_recommender -> 	if RM_compute_fg == 1 
              #                         -> read.csv2
              #                         -> select_baseline_users
              #                         -> build_rating_matrix
              #                         -> norm_data
              #                         -> build_similarity_matrix
              #                         -> save title, rating matrix, similarity matrix, baseline user    to memory
              #                     else
              #                         -> restore title, rating matrix, similarity matrix, baseline user from memory
              #                     build_new_user_rating_vector
              #                     add_user_to_similarity_matrix
              #                     for closest_neighbors values
              #                         if "recommend movie"
              #                             -> k_nearest_neigh
              #                             -> UBCF_prediction
              #                             -> recommendation
              #                             -> clean_title
              #                         else if "compute KPI"
              #                             Predict_rate_mat
              #                             compute RMSE
              #                             confusion_KPI
              #                     if "compute KPI"
              #                         plot_RMSE
              #                         plot_confusion_KPI
#
#   INPUTS:     user_2_rec:                     user to recommend. This argument can have 2 different format: either a user index in a list of data frames to recommend movie to or directly the data frame representing the user
#               recom_type:                     recommendation type to perform: "HIGHER MATCH", "POPULAR", "OFF THE BEATEN TRACK", "SURPRISE SURPRISE", "ALLOW SURPRISE" 
#               N_mov_2_rec:                    number of movie to recommend
#               kNN_vec:                        vector of number of nearest neighbors values. The vector has typically a single value when used in recommendation, a vector is used when evaluating the algorithm
#   OUTPUTS:    recom_title_clean:              titles of recommended movies
################################################################################################
run_recommender <- function(user_2_rec, recom_type, N_mov_2_rec, kNN_vec){
  # number of kNN to run
  N_kNN                         = length(kNN_vec)
  
  # set folder and file paths 
  # project top folder
  project_folder                = "C:\\Users\\nxa19765\\Documents\\data\\TBS\\projects\\Netflix project"
  
  # source functions file
  # code folder
  code_folder                   = paste(project_folder,"\\code\\Recommender Sac", sep = "") 
  
  # get functions
  # function file name
  function_file_name            = "Recommender Sac functions.R"
  # path to access function file
  function_file_path            = paste(code_folder,"\\",function_file_name, sep = "")
  # source function file so that all functions defined in this file are available in this file
  source(function_file_path)
  
  # get parameters
  # parameters file name
  parameter_file_name           = "Recommender set parameters.R"
  # path to access parameters file
  parameter_file_path           = paste(code_folder,"\\",parameter_file_name, sep = "")
  # source parameters file so that all parameters defined in this file are available in main file
  source(parameter_file_path)
  
  # folder containing data files
  data_folder                   =  paste(project_folder,"\\data", sep = "") 
  # user/movies/ratings file name
  user_movies_ratings_file_name = "rated_movies.csv"
  # path to access user/movies/ratings file 
  user_movies_ratings_file_path = paste(data_folder,"\\",user_movies_ratings_file_name, sep = "")
  
  # data files to save/restore to avoid constant identical processing to speed-up processing
  RM_file_name                  = paste("RM_",toString(N_min_user_ratings),"_",toString(N_min_movie_ratings),".RDataTmp", sep = "")
  RM_file_path                  = paste(data_folder,"\\",RM_file_name, sep = "")
  
  if (RM_compute_fg == 1){
    cat("#### READ USER MOVIES RATING CSV FILE ","\n\n")
    user_movies_ratings_df        <- read.csv2(user_movies_ratings_file_path, header=TRUE, sep = ";", quote = "",stringsAsFactors=FALSE, row.names = NULL)
    
    # display data frame number of entries
    if (verbose_fg == 1){
      cat("\n")
      cat("data frame has ", length(user_movies_ratings_df) , "elements","\n")
      
      # display data frame names
      cat("\n")
      print("######## DATA FRAME NAMES: ########")
      print(attributes(user_movies_ratings_df)$names)
    }
    # select baseline users to extract from data base and perform recommendations to
    ret_l                       = select_baseline_users(user_movies_ratings_df, c(5,10,25,50))
    # new_user_df_list is a list of user/movies/ratings data frame for which we will want the recommender to process
    new_user_df_list            = ret_l[["baseline_users_df"]]
    # old_user_df is a user/movies/ratings data frame from the csv data frame where new users have been removed
    old_user_df                 = ret_l[["input_df"]]
    
    # delete data frame since it's huge and no longer used
    rm(user_movies_ratings_df)
    
    # select users and films with enough ratings
    cat("#### COMPUTE RATINGS MATRIX (USERS Vs MOVIES) ","\n\n")
    # build rating matrix
    ret_l                       = build_rating_matrix(old_user_df, N_min_user_ratings, N_min_movie_ratings)
    # rating matrix users Vs movies with ratings. If no rating is available matrix value is NA
    rating_mat                  = ret_l[["RM"]]
    # titles of movies in the rating matrix
    mov_title_vec               = ret_l[["title"]]
    
    # delete data frame since it's huge and no longer used
    rm(old_user_df)
    
    # normalize users rating to remove any bias and center data on 0
    cat("#### NORMALIZE RATINGS PER USER ","\n\n")
    rating_mat_norm               = norm_data(rating_mat, "center")
    
    # plot rate matrix before and after normalization
    if (verbose_fg == 1){
      plot_rate_matrix(rating_mat)
      plot_rate_matrix(rating_mat_norm)
    }
    # delete rating matrix since it's huge and no longer used. Further on, only the normalized rating are used
    rm(rating_mat)
    
    cat("#### COMPUTE SIMILARITY MATRIX ","\n\n")
    # build similarity matrix
    sim_mat                       = build_similarity_matrix(rating_mat_norm)
    # save mov_title_vec, rating_mat_norm, sim_mat, new_user_df_list to memory 
    save(mov_title_vec, rating_mat_norm, sim_mat, new_user_df_list, file= RM_file_path)  
    
  } else {
    cat("#### READ RATINGS MATRIX (USERS Vs MOVIES) ","\n\n")
    # check that file exists
    if (file.exists(RM_file_path)){
      print("restore compute rate matrix from file")
      # restore mov_title_vec, rating_mat_norm, sim_mat, new_user_df_list from memory 
      load(RM_file_path)  
    } else {
      stop("error: data file doesn't exist")
    }
  }
  
  # data frame for user to recommend
  if (is.list(user_2_rec)) { 
    new_user_df               = user_2_rec
    cat("\n", "argument is data frame ", "\n")
  } else if (is.numeric(user_2_rec)){
    new_user_df               = new_user_df_list[[user_2_rec]]
    cat("\n", "argument is integer, fetch user number ", user_2_rec, "\n")
  } else{
    stop("error: wrong input to run_recommender function: argument user_2_rec should be either integer or data frame")
  }
  print(new_user_df)
  
  # build rating vector for new user
  new_user_rating_vec           = build_new_user_rating_vector(new_user_df, mov_title_vec)
  # update similarity matrix including new user in it. new user similarity is the last column and row.
  new_sim_mat                   = add_user_to_similarity_matrix(rating_mat_norm, sim_mat, new_user_rating_vec)
  # number of users
  N_user_idx                    = dim(new_sim_mat)[1]
  
  cat("#### USER BASED COLLABORATIVE FILTERING OVER ", N_kNN, " KNN VALUES = ", kNN_vec, "\n\n")
  if (proc_type_fg == "KPI"){
    # initialize RMSE
    RMSE                        = replicate(N_kNN, NA)
    # list of confusion KPI for all k values. pre-allocate with empty values.
    conf_KPI_l                  = list(list(0,0,0))
  }
  # loop on all values of number of nearest neighbors
  for (k_idx in 1:N_kNN){
    # number of nearest neighbors
    kNN                         = kNN_vec[k_idx]
    # perform recommendation
    if (proc_type_fg == "1 user prediction") {
      # compute vector of the k user indices in the similarity matrix
      k_set_vec                 = k_nearest_neigh(new_sim_mat, N_user_idx, k=kNN)
      # similarity values of the k nearest neighbors
      sim_mat_k_set             = new_sim_mat[N_user_idx, k_set_vec]
      
      # vector or rating predictions for user to recommend
      rating_predictions_vec    = UBCF_prediction(rating_mat_norm , k_set_vec , sim_mat_k_set)
      
      # vector of ratings for all movies for user to recommend
      user_rating_vec           = new_user_rating_vec
      # perform recommendation
      recom_title               = recommendation(rating_mat_norm, rating_predictions_vec, N = N_mov_2_rec, user_rating_vec, rec_rating_thresh, recom_type, mov_title_vec)
      # clean title 
      recom_title_clean         = clean_title(recom_title)
      
      # display recommendation on console
      cat("Recommended movie titles=", "\n")
      print(recom_title_clean)
      cat("\n")
      
      return(recom_title_clean)
      
    # evaluate recommender system
    } else if (proc_type_fg == "KPI"){
      # predict rating for entire rating matrix
      rating_mat_pred           = Predict_rate_mat(rating_mat_norm,sim_mat,kNN)
      # get index where a rating is available both in the original rating matrix rating_mat_norm (ground truth) and in the rating matrix predictions rating_mat_pred
      ind_com                   = (!is.na(rating_mat_norm)) & (!is.na(rating_mat_pred))
      # compute RMSE
      RMSE[k_idx]               = sqrt( sum( (rating_mat_norm[ind_com] - rating_mat_pred[ind_com])^2 ) / sum(ind_com) )
      # compute confusion KPIs: accuracy, precision, recall
      conf_KPI_l[[k_idx]]       = confusion_KPI(rating_mat_norm, rating_mat_pred, 0)
    } else{
      stop("error: wrong processing mode")
    }
  }
  
  # plot KPIs for all nearest neighbor values  
  if (proc_type_fg == "KPI"){
    # number of users
    N_user                      = dim(rating_mat_norm)[1]
    # number of movies
    N_mov                       = dim(rating_mat_norm)[2]
    cat("RMSE =",RMSE, "\n")
    # plot RMSE
    plot_RMSE(RMSE,N_user,N_mov, kNN_vec)
    # plot confusion KPIs
    plot_confusion_KPI(conf_KPI_l,N_user,N_mov, kNN_vec)
  }
}
