navbarPage(
  title = 'Teacher Portal',
  theme = shinytheme('flatly'),
  id = 'teacher_nav_bar',
  tabPanel(
    title = 'Grade Student Work',
    value = 'teacher_tab_1',
    fluidPage(
      h3(paste0('Hi, ',res_auth$name)),
      br(),
      fluidRow(
        column(
          3,
          #selectInput(inputId = 'tp_course', label = 'Course: ', choices = c('','C1','C2')),
          uiOutput('tp_course_ui')
        ),
        column(
          3, offset = 1,
          #selectInput(inputId = 'tp_student', label = 'Student: ', choices = c('S1','S2')),
          uiOutput('tp_assignment_ui')
        ),
        column(
          3, offset = 1,
          #selectInput(inputId = 'tp_assignment', label = 'Assignment:', choices = c('A1','A2')),
          uiOutput('tp_student_ui')
        )
      ),
      hr(),
      verbatimTextOutput('tp_status'),
      br(),
      # fluidRow(
      #   uiOutput('tp_grading_part_ui'),
      #   column(
      #     8,
      #     uiOutput('tp_pdf_ui')
      #   ),
      #   column(3.5, offset = 0.5,
      #          uiOutput('tp_marks_input_ui'),
      #          #style='margin-bottom:30px;border:1px solid; padding: 10px; overflow-y:scroll;',
      #          style='overflow-y:scroll;')
      # ),
      uiOutput('tp_grading_part_ui'),
      br()
    )
  ),
  tabPanel(
    title = 'Curriculum Analytics',
    value = 'teacher_tab_2',
    fluidPage(
      h3(paste0('Hi, ',res_auth$name)),
      br(),
      uiOutput('tp_aa_course_ui'),
      hr(),
      uiOutput('tp_aa_assign_vis_ui'),
      br(),
      uiOutput('tp_aa_topics_vis_ui'),
      br()
    )
  )
)