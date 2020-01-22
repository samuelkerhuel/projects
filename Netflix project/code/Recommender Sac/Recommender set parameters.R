# This file defines all the parameters used in recommender system. 
# All parameters are gathered in a single file to centralize parameter control in a single place

#### SETTING FLAGS
# if "1 user prediction": predict and recommend movies for 1 user, if "KPI" compute algorithm KPIs
proc_type_fg                = "1 user prediction"
# proc_type_fg                = "KPI"
# if set to 1 script will display and plot informations for control and check
verbose_fg                  = 0
# if set to 1 rating matrix RM will be computed and saved in memory otherwise RM is restored from file to avoid computation
RM_compute_fg               = 0

# vector of number of number of closest numbers to evaluates.
# now passed as function argument
# kNN_vec                     = c(1,2,3,4,5,10,20,50)
# kNN_vec                     = c(1)

# number of movies to recommend
# now passed as function argument
# N_mov_2_rec                 = 5

# threshold defining a movie to recommend or not. If below/above threshold don't/do recommend movie
rec_rating_thresh           = 0


# defines the possible configuration values for mininum number of ratings per user and per movie. More configurations can be added simply by adding elements to those lists
N_min_user_ratings_list     = c(270,150,100,50)
N_min_movie_ratings_list    = c( 20,  1,  1, 1)
# select indice in previous lists
N_min_idx                   = 4
# Mininum of ratings a user should do to appear in the rating matrix
N_min_user_ratings          = N_min_user_ratings_list[N_min_idx]
# Mininum of ratings a movie should have to appear in the rating matrix
N_min_movie_ratings         = N_min_movie_ratings_list[N_min_idx]

# number of kNN to run
# now passed as function argument
# N_kNN                         = length(kNN_vec)

# set some variables depending on the type processing
# if (proc_type_fg == "1 user prediction") {
#   # list of type of recommendation algorithms
#   # recom_type_l                = c("HIGHER MATCH" , "POPULAR" , "OFF THE BEATEN TRACK" , "SURPRISE SURPRISE", "ALLOW SURPRISE")
#   # # pick one algorithm type
#   # now passed as function argument
#   # recom_type                  = recom_type_l[5]
# } else if (proc_type_fg == "KPI"){
#   # initialize RMSE
#   RMSE                        = replicate(N_kNN, NA)
#   # list of confusion KPI for all k values. pre-allocate with empty values.
#   conf_KPI_l                  = list(list(0,0,0))
# }
