#TODO - refresh, only allow all marks input, submit grade functionality,
# rethink placement of students_graded_assignments_df.

tp_values <- reactiveValues()

output$tp_course_ui <- renderUI({
  req(!is.null(res_auth))
  
  teacher_courses <- courses_df %>%
    filter(cid %in% (instructor_teaches_df %>%
                       filter(tid == res_auth$id) %>%
                       pull(cid))) %>%
    pull(name)
  
  selectInput(inputId = 'tp_course', label = 'Course: ', choices = c('',teacher_courses))
  
})

output$tp_assignment_ui <- renderUI({
  req(!is.null(input$tp_course), input$tp_course!='')
  
  course_assignments <- assignments_df %>%
    filter(cid == (courses_df %>%
                     filter(name == input$tp_course) %>%
                     pull(cid))) %>%
    pull(name)
  
  selectInput(inputId = 'tp_assignment', label = 'Assignment:', choices = c('',course_assignments))
})

output$tp_student_ui <- renderUI({
  req(!is.null(input$tp_assignment), input$tp_assignment!='')
  
  
  course_students <- students_df %>%
    filter(sid %in% (student_enrollment_df %>%
                       filter(cid == (courses_df %>%
                                        filter(name == input$tp_course) %>%
                                        pull(cid))) %>%
                       pull(sid))) %>%
    pull(name)
  selectInput(inputId = 'tp_student', label = 'Student: ', choices = c('', course_students))
})

output$tp_status <- renderPrint({
  req(!is.null(input$tp_student), input$tp_student!='')
  
  rvalues$refresh
  
  CID <- courses_df %>% filter(name == input$tp_course) %>% pull(cid)
  AID <- assignments_df %>% filter(cid == CID & name == input$tp_assignment) %>% pull(aid)
  SID <- students_df %>% filter(name == input$tp_student) %>% pull(sid)
  
  student_submission_df <- dbGetQuery(conn, 'select * from StudentSubmission;') %>% as_tibble()
  
  students_graded_assignments_df <- dbGetQuery(conn, "
           select distinct sir.sid, aq.aid
           from StudentItemResponse sir
           join assignmentquestions aq on sir.qid = aq.qid
           ") %>%
    as_tibble()
  
  if(nrow(students_graded_assignments_df %>% filter(sid == SID & aid == AID))==1){
    tp_values$grading_status <- 'Graded'
    paste0('Status: Graded')
  }
  else if(nrow(student_submission_df %>% filter(sid==SID & aid == AID))==1){
    tp_values$grading_status <- 'Submitted'
    paste0('Status: Submitted')
  }
  else {
    tp_values$grading_status <- 'Pending Submission'
    paste0('Status: Pending Submission')
  }
  
})


output$tp_grading_part_ui <- renderUI({
  
  req(tp_values$grading_status == 'Graded'||tp_values$grading_status == 'Submitted',!is.null(input$tp_student), input$tp_student!='')
  
  fluidRow(
    column(
      8,
      uiOutput('tp_pdf_ui')
    ),
    column(3.5, offset = 0.5,
           uiOutput('tp_marks_input_ui'),
           #style='margin-bottom:30px;border:1px solid; padding: 10px; overflow-y:scroll;',
           style='overflow-y:scroll;')
  )
})


output$tp_pdf_ui <- renderUI({
  req(tp_values$grading_status == 'Graded'||tp_values$grading_status == 'Submitted',!is.null(input$tp_student), input$tp_student!='')
  
  CID <- courses_df %>% filter(name == input$tp_course) %>% pull(cid)
  AID <- assignments_df %>% filter(cid == CID & name == input$tp_assignment) %>% pull(aid)
  SID <- students_df %>% filter(name == input$tp_student) %>% pull(sid)
  student_submission_df <- dbGetQuery(conn, 'select * from StudentSubmission;') %>% as_tibble()
  
  filepath <- student_submission_df %>% filter(sid == SID & aid==AID) %>% pull(link)
  
  tags$iframe(style="height:600px; width:100%; scrolling:yes",
              src=filepath)
})

observeEvent(input$tp_submit_button,{
  
  CID <- courses_df %>% filter(name == input$tp_course) %>% pull(cid)
  AID <- assignments_df %>% filter(cid == CID & name == input$tp_assignment) %>% pull(aid)
  SID <- students_df %>% filter(name == input$tp_student) %>% pull(sid)
  
  QIDS <- dbGetQuery(conn, '
                     select aq.qid
                     from assignmentquestions aq
                     where aid = :a',
                     params = list(a=AID)) %>%
    as_tibble() %>%
    arrange(parse_number(qid)) %>%
    pull(qid)
  
  ids <- paste0('tp_q',1:length(QIDS))
  
  for (i in 1:length(QIDS)) {
    score <- as.integer(input[[ids[i]]])
    dbExecute(conn,"
            INSERT INTO StudentItemResponse
            VALUES (:s, :q, :sc)
            ",
              params = list(s=SID, q = QIDS[i], sc = score))
    
  }
  
  shinyalert(
    title = "Success",
    text = "Assignment graded successfully!",
    size = "s", 
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "success",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
  
  rvalues$refresh <- rvalues$refresh + 1
  
})

output$tp_marks_input_ui <- renderUI({
  req(tp_values$grading_status == 'Graded'||tp_values$grading_status == 'Submitted',!is.null(input$tp_student), input$tp_student!='')
  
  CID <- courses_df %>% filter(name == input$tp_course) %>% pull(cid)
  AID <- assignments_df %>% filter(cid == CID & name == input$tp_assignment) %>% pull(aid)
  
  q <- dbGetQuery(conn, '
             select * 
             from assignmentquestions
             where aid == :x
             ',
             params = list(x = AID)) %>% as_tibble() %>% nrow()
  
  ids <- paste0('tp_q',1:q)
  labels <- paste0('Question ',1:q,':')
  
  fluidPage(
    map2(ids, labels, ~textInput(.x,.y)),
    br(),
    actionButton('tp_submit_button','Submit Grades',
                 style="color: #fff; background-color: #337ab7; border-color: #337ab7")
  )
})

#----------tab2--------------

output$tp_aa_course_ui <- renderUI({
  req(!is.null(res_auth))
  
  teacher_courses <- courses_df %>%
    filter(cid %in% (instructor_teaches_df %>%
                       filter(tid == res_auth$id) %>%
                       pull(cid))) %>%
    pull(name)
  
  selectInput(inputId = 'tp_aa_course', label = 'Course: ', choices = c('',teacher_courses))
  
})


output$tp_aa_assign_vis_ui <- renderUI({
  req(!is.null(input$tp_aa_course), input$tp_aa_course!='')
  
  fluidPage(
    h4('Assessment Analysis'),
    hr(style="border-top: 1px solid #000000;"),
    plotOutput('tp_aa_plot_1')
  )
  
})

output$tp_aa_topics_vis_ui <- renderUI({
  req(!is.null(input$tp_aa_course), input$tp_aa_course!='')
  
  
  fluidPage(
    h4('Topic Analysis'),
    hr(style="border-top: 1px solid #000000;"),
    plotOutput('tp_aa_plot_2')
  )
  
})

output$tp_aa_plot_1 <- renderPlot({
  req(!is.null(input$tp_aa_course), input$tp_aa_course!='')
  
  rvalues$refresh
  
  CID <- courses_df %>% filter(name == input$tp_aa_course) %>% pull(cid)
  
  df <- dbGetQuery(conn,"
                   select sir.sid, sir.qid, sir.points, a.aid, a.aname
                   from (select aid, name as aname from assignment where cid = :c) a
                   join assignmentquestions aq on a.aid=aq.aid
                   join StudentItemResponse sir on sir.qid=aq.qid
                   ",
                   params = list(c = CID)) %>%
    as_tibble()
  
  all_assignments_df <- dbGetQuery(conn,"
                                   select aid, name as aname
                                   from assignment where cid = :c;
                                   ",
                                   params = list(c = CID)) %>%
    as_tibble()
  
  vis_df <- df %>%
    group_by(aid, aname, sid) %>%
    summarise(tot_points = sum(points),
              max_points = 10*n()) %>%
    ungroup() %>%
    mutate(avg_perc_points = 100*(tot_points/max_points)) %>%
    group_by(aid, aname) %>%
    summarise(assign_avg_points = mean(avg_perc_points)) %>%
    ungroup()
  
  vis_df %>%
    bind_rows(all_assignments_df %>%
                anti_join(vis_df) %>%
                mutate(assign_avg_points = 0)) %>%
    mutate(aname = factor(aname, levels = mixedsort(aname))) %>%
    ggplot(aes(x = aname, y = assign_avg_points)) +
    geom_bar(fill = '#4ebfc7',stat = 'identity') +
    scale_y_continuous('Avg % Score') +
    scale_x_discrete('Assignment')
  
})

output$tp_aa_plot_2 <- renderPlot({
  req(!is.null(input$tp_aa_course), input$tp_aa_course!='')
  
  CID <- courses_df %>% filter(name == input$tp_aa_course) %>% pull(cid)
  
  rvalues$refresh
  
  df <- dbGetQuery(conn,"
                   select sir.sid, sir.qid, sir.points, t.topic_id, t.topic
                   from (select aid, name as aname from assignment where cid = :c) a
                   join assignmentquestions aq on a.aid=aq.aid
                   join StudentItemResponse sir on sir.qid=aq.qid
                   join topics t on aq.topic_id=t.topic_id
                   ",
                   params = list(c = CID)) %>%
    as_tibble()
  
  all_topics_df <- dbGetQuery(conn,"
                   select distinct t.topic_id, t.topic
                   from (select aid, name as aname from assignment where cid = :c) a
                   join assignmentquestions aq on a.aid=aq.aid
                   join topics t on aq.topic_id=t.topic_id
                   ",
                              params = list(c = CID)) %>%
    as_tibble()
  
  vis_df <- df %>%
    group_by(topic_id, topic) %>%
    summarise(topic_avg_points = 10*mean(points)) %>%
    ungroup()
  
  vis_df %>%
    bind_rows(all_topics_df %>%
                anti_join(vis_df) %>%
                mutate(topic_avg_points = 0)) %>%
    mutate(topic = factor(topic, levels = mixedsort(topic))) %>%
    ggplot(aes(x = topic, y = topic_avg_points)) +
    geom_bar(fill = '#5ccc5a',stat = 'identity') +
    scale_y_continuous('Avg % Score') +
    scale_x_discrete('Topic') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
})

