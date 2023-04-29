library(shiny)
library(tidyverse)
library(shinythemes)
library(shinyjs)
library(DT)
library(waiter)
library(htmltools)
library(scales)
library(lubridate)
library(gtools)
library(shinycssloaders)
library(shinymanager)
library(DBI)
library(RSQLite)
library(shinyalert)


theme_set(
  theme_bw() +
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 18, face = "bold"), plot.title = element_text(size = 12 + 4),
          plot.subtitle = element_text(size = 12), legend.title = element_text(size = 14), legend.text = element_text(size = 12),
          plot.caption = element_text(size = 12), strip.text.x = element_text(size = 14, face = 'bold', colour = 'white'), strip.text.y = element_text(size = 14, face = 'bold', colour = 'white'),
          strip.background = element_rect(colour = '#2c3e50', fill = '#2c3e50'), panel.spacing = unit(2, "lines"), axis.title.x = element_text(margin = margin(t = 20)))
)


#database_path <- file.path(getActiveProject(), 'data','application_database.db')
#conn <- dbConnect(RSQLite::SQLite(),database_path)

database_path <- file.path('data','application_database.db')
conn <- dbConnect(RSQLite::SQLite(),database_path)

students_df <- dbGetQuery(conn, 'select * from student;') %>% as_tibble()
courses_df <- dbGetQuery(conn, 'select * from course;') %>% as_tibble()
student_enrollment_df <- dbGetQuery(conn, 'select * from StudentEnrollment;') %>% as_tibble()
instructor_teaches_df <- dbGetQuery(conn, 'select * from InstructorTeaches;') %>% as_tibble()
assignments_df <- dbGetQuery(conn, 'select * from assignment;') %>% as_tibble()

rvalues <- reactiveValues(refresh=0)

# students_graded_assignments_df <- dbGetQuery(conn, "
#            select distinct sir.sid, aq.aid
#            from StudentItemResponse sir
#            join assignmentquestions aq on sir.qid = aq.qid
#            ") %>%
#   as_tibble()


#user_base <- readRDS(file = file.path(getActiveProject(),'data','creds.rds'))
user_base <- readRDS(file = file.path('data','creds.rds'))


# user_base <- data.frame(
#   user = c("user1", "user2"),
#   password = c("pass1", "pass2"),
#   role = c("teacher", "student"),
#   name = c("User One", "User Two"),
#   stringsAsFactors = FALSE
# )

set_labels(
  language = "en",
  "Please authenticate" = "Login"
)


# 
ui <- tagList(
  waiter::waiter_use(),
  waiter::waiter_show_on_load(
    color = '#2C3E50',
    tagList(
      spin_wave(),
      br(),
      span(h3("Loading ..."), style = "color:white;")
    )
  ),
  shinyjs::useShinyjs(),
  #useShinyalert(),
  
  uiOutput('gta_main_ui')
)

ui <- secure_app(ui, theme = shinytheme('flatly'),
                 tags_top = 
                   tags$div(
                     tags$head(
                       tags$style(HTML(".btn-primary {
                  color: #ffffff;
                  background-color: #0dc5c1;
                  border-color: #0dc5c1;
              }
              .panel-primary {
                  border-color: #0dc5c1;
              }"))
                     ),
                     tags$h2("Grading Tool & Analytics", style = "align:center")
                   ))


server <- function(input, output, session) {

  waiter::waiter_hide()
  
  res_auth <- secure_server(
    check_credentials = check_credentials(user_base)
  )
  
  output$gta_main_ui <- renderUI({
    req(!is.null(res_auth))

    if(res_auth$role == 'teacher'){
      source(file.path('ui','teacher_ui.R'),local = TRUE)$value
    }

    else if(res_auth$role == 'student'){

      source(file.path('ui','student_ui.R'),local = TRUE)$value
    }
  })
  
  source(file.path('server','teacher_server.R'), local = TRUE)$value
  source(file.path('server','student_server.R'), local = TRUE)$value

}

# Run the application
shinyApp(ui = ui, server = server)

