navbarPage(
  title = 'Student Portal',
  theme = shinytheme('flatly'),
  id = 'student_nav_bar',
  tabPanel(
    title = 'Upload Assignment',
    value = 'student_tab_1',
    fluidPage(
      h3(paste0('Hi, ',res_auth$name)),
      br(),
      fluidRow(
        column(
          3,
          #selectInput(inputId = 'sp_course', label = 'Course: ', choices = c('C1','C2')),
          uiOutput('sp_course_ui')
        ),
        column(
          3, offset = 1,
          #selectInput(inputId = 'sp_assignment', label = 'Assignment:', choices = c('A1','A2')),
          uiOutput('sp_assignment_ui')
        )
      ),
      hr(),
      verbatimTextOutput('sp_status'),
      br(),
      fluidRow(
        column(4, offset = 4,
               #dataTableOutput('sp_assign_marks'),
               uiOutput('sp_assign_marks_ui'))
      ),
      br(),
      fluidRow(
        column(
          12, align = 'center',
          #fileInput('sp_pdf_upload', label = NULL, accept = '.pdf', buttonLabel = 'Upload File'),
          uiOutput('sp_pdf_upload_ui')
        )
      ),
      fluidRow(
        column(
          12, align = 'center',
          uiOutput('sp_uploaded_file_ui')
        )
      ),
      br(),
      fluidRow(
        column(
          12, align = 'center',
          uiOutput('sp_sbt_btn_ui'),
        )
      ),
      br(),
      br()
    )
  ),
  tabPanel(
    title = 'Curriculum Analytics',
    value = 'student_tab_2',
    fluidPage(
      h3(paste0('Hi, ',res_auth$name)),
      br(),
      uiOutput('sp_aa_course_ui'),
      hr(),
      uiOutput('sp_aa_assign_vis_ui'),
      br(),
      uiOutput('sp_aa_topics_vis_ui'),
      br()
    )
  )
)