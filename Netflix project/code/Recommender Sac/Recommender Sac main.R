#################### FILE DESCRIPTION     ####################
# This is the main file for movie recommender system when recommender is run in R environment, processing starts here.
# This file sources and uses 1 companion file
#     1. Recommender Sac recommender.R  : function that runs recommender system
#################### END FILE DESCRIPTION ####################

# clear console & environment to start from a clean sheet
cat("\014")       # clear console
rm(list=ls())     # clear environment
# set folder and file paths 
# project top folder
project_folder                = "C:\\Users\\nxa19765\\Documents\\data\\TBS\\projects\\Netflix project"

# source functions file
# code folder
code_folder                   = paste(project_folder,"\\code\\Recommender Sac", sep = "") 

# recommender file name
recommender_file_name         = "Recommender Sac recommender.R"
# path to access recommender file
recommender_file_path         = paste(code_folder,"\\",recommender_file_name, sep = "")
# source recommender file so that all functions defined in this file are available in this file
source(recommender_file_path)

# define the user to recommend. Do it either through index or directly through data frame
# user index to recommend
# user_2_rec                  = 1
load(file="C:\\Users\\nxa19765\\Documents\\data\\TBS\\projects\\Netflix project\\code\\Rshiny\\training\\user_list.dat")
user_2_rec                  = new_user_df_list[[1]]
# types of recommendations
recom_type_l                = c("HIGHER MATCH" , "POPULAR" , "OFF THE BEATEN TRACK" , "SURPRISE SURPRISE", "ALLOW SURPRISE")
# pick one algorithm type
recom_type                  = recom_type_l[1]
# number of movies to recommend
N_mov_2_rec                 = 5
# all kNN values to test
# kNN_vec                     = c(10)
kNN_vec                     = c(1,5,10,20,50,100)

recom_title = run_recommender(user_2_rec, recom_type, N_mov_2_rec, kNN_vec)

