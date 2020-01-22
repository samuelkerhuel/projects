# Rshiny application

library(shiny)
library(rhandsontable)


# redefine those 2 function here for since they are used in function server before to be sourced.
# remove year from title i.e. any character strings "(abcd)" where a,b,c,d are any integer from 0 to 9
# remove_year <- function(title){
#   return(sub("\\(([0-9]){4}\\)","",title))
# }
# 
# clean_title_list<- function(){
#   file_folder                 = "C:\\Users\\nxa19765\\Documents\\data\\TBS\\projects\\Netflix project\\data"
#   file_name                   = "movies_list.csv"
#   file_path                   = paste(file_folder,"\\",file_name, sep = "")
#   movies_genres_df            <- read.csv(file_path, header=TRUE, sep = ",", quote = "",stringsAsFactors=FALSE, row.names = NULL)
#   movies_genres_df$title      = clean_title(movies_genres_df$title)
# 
#   save_folder                 = "C:\\Users\\nxa19765\\Documents\\data\\TBS\\projects\\Netflix project\\data"
#   save_name                   = "movies_list.data"
#   save_path                   = paste(save_folder,"\\",save_name, sep = "")
#   save(movies_genres_df, file= save_path)  
# }

get_movie_title <-function(){
  save_folder                 = "C:\\Users\\nxa19765\\Documents\\data\\TBS\\projects\\Netflix project\\data"
  save_name                   = "movie_title.data"
  save_path                   = paste(save_folder,"\\",save_name, sep = "")
  load(file= save_path)
  return(mov_title_vec)
}

# clean_title <- function(title_l){
#   N = length(title_l)
#   for(i in 1:N){
#     title         = title_l[i]
#     title1        = remove_year(title)
#     
#     the_ind       = unlist(gregexpr(", The", title1))
#     if (the_ind > 0){
#       title2      = sub(", The","",title1)
#       title_l[i]  = paste("The",title2, sep=" ")
#     } else{
#       title_l[i]  = title1
#     }
#     
#   }
#   return(title_l)
# }
# 
call_recommender <-function(user_idx_2_rec, recom_type, N_mov_2_rec, kNN_vec){
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
  
  # recom_title                   = run_recommender(mydata, recom_type, N_mov_2_rec, kNN_vec)
  recom_title                   = run_recommender(user_idx_2_rec, recom_type, N_mov_2_rec, kNN_vec)
  recom_title_df                = as.data.frame(recom_title)
  # update column name
  colnames(recom_title_df) <- c("Movie Recommended")
  return(recom_title_df)
} 

build_df <-function(user_df){
  df1               = na.omit(user_df)
  # df1$title         = clean_title(df1$title)
  mydata            <- data.frame("title"=df1$title,"genres"=df1$genres,"rating"=df1$rating)
  return(mydata)
}


build_list_user <-function(){
  load(file="C:\\Users\\nxa19765\\Documents\\data\\TBS\\projects\\Netflix project\\code\\Rshiny\\training\\user_list.dat")
  ret = list()
    for(i in 1:4){
      ret[[i]] = build_df(new_user_df_list[[i]])
    }
  new_user_df_list = ret
  save(new_user_df_list,file="C:\\Users\\nxa19765\\Documents\\data\\TBS\\projects\\Netflix project\\data\\user_list.dat")
}

ui <- fluidPage(title = "Random generator",
                tabsetPanel(              
                  tabPanel(title = "User ratings dynamic",
                           sliderInput(inputId = "user_prof_dyn",
                                       label = "user profile dynamic",
                                       value = 1, min = 1, max = 4),
                           rHandsontableOutput("ratings_table_dyn"),
                           selectizeInput("rating_value", "enter rating:",
                                          c(1,1.5,2,2.5,3,3.5,4,4.5,5), options = list(maxOptions = 10)),
                           selectizeInput("movie_title", "select movie:",
                                          get_movie_title(), options = list(maxOptions = 10)),
                           actionButton("goButton", "Update Table")
                           
                           
                  ),
                  tabPanel(title = "Recommendations",
                           sliderInput(inputId = "num_rec", 
                                       label = "Number of movies to recommend", 
                                       value = 5, min = 1, max = 10),
                           sliderInput(inputId = "num_kNN",
                                       label = "Number of kNN neighbors",
                                       value = 20, min = 1, max = 100),
                           
                           actionButton(inputId = "high", label = "HIGHER MATCH"),
                           actionButton(inputId = "popular", label = "POPULAR"),
                           actionButton(inputId = "off_the_beaten_track", label = "OFF THE BEATEN TRACK"),
                           actionButton(inputId = "surprise_surprise", label = "SURPRISE SURPRISE"),
                           actionButton(inputId = "allow_surprise", label = "ALLOW SURPRISE"),
                           
                           DT::dataTableOutput("rec_table")
                  )
                )
)

server <- function(input, output) {
  
  load(file="C:\\Users\\nxa19765\\Documents\\data\\TBS\\projects\\Netflix project\\data\\user_list.dat")
    
  rv                                        <- reactiveValues(data = data.frame("Movie Recommended"=c(""), stringsAsFactors = FALSE),
                                                              rating_arr = new_user_df_list[[1]],
                                                              new_entries= new_user_df_list 
                                                              )
  
  mydata = build_df(new_user_df_list[[1]])
  output$ratings_table_dyn = renderRHandsontable(df())
  observeEvent(input$high,                 { rv$data <- call_recommender(rv$rating_arr, "HIGHER MATCH"         , input$num_rec, input$num_kNN)})
  observeEvent(input$popular,              { rv$data <- call_recommender(rv$rating_arr, "POPULAR"              , input$num_rec, input$num_kNN)})
  observeEvent(input$off_the_beaten_track, { rv$data <- call_recommender(rv$rating_arr, "OFF THE BEATEN TRACK" , input$num_rec, input$num_kNN)})
  observeEvent(input$surprise_surprise,    { rv$data <- call_recommender(rv$rating_arr, "SURPRISE SURPRISE"    , input$num_rec, input$num_kNN)})
  observeEvent(input$allow_surprise,       { rv$data <- call_recommender(rv$rating_arr, "ALLOW SURPRISE"       , input$num_rec, input$num_kNN)})
  
  df <- eventReactive(input$goButton, {
    mydata_baseline = build_df(new_user_df_list[[input$user_prof_dyn]])
    new_entry       = data.frame("title"=input$movie_title,"genres"="","rating"=input$rating_value)
    if(input$goButton==1){
      rv$new_entries[[input$user_prof_dyn]]  = new_entry
    } else     if(input$goButton > 1){
      rv$new_entries[[input$user_prof_dyn]]  = rbind(rv$new_entries[[input$user_prof_dyn]], new_entry)
    }
    mydata <<- rbind(mydata_baseline, rv$new_entries[[input$user_prof_dyn]])
    rv$rating_arr  = mydata
    rhandsontable(mydata, stretchH = "all")
    
  }, ignoreNULL = FALSE)
  observe(if (!is.null(input$ratings_table_dyn)) mydata <<- hot_to_r(input$ratings_table_dyn))
  
  output$rec_table <- DT::renderDataTable({ 
    rv$data
  })
}

shinyApp(server = server, ui = ui)