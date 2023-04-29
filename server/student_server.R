#TODO - 


sp_values <- reactiveValues(datapath = NULL)

output$sp_course_ui <- renderUI({
  
  req(!is.null(res_auth))
  
  student_courses <- courses_df %>%
    filter(cid %in% (student_enrollment_df %>%
                       filter(sid == res_auth$id) %>%
                       pull(cid))) %>%
    pull(name)
  
  selectInput(inputId = 'sp_course', label = 'Course: ', choices = c('',student_courses))
  
})

output$sp_assignment_ui <- renderUI({
  
  req(!is.null(input$sp_course), input$sp_course!='')
  
  course_assignments <- assignments_df %>%
    filter(cid == (courses_df %>%
                     filter(name == input$sp_course) %>%
                     pull(cid))) %>%
    pull(name)
  
  selectInput(inputId = 'sp_assignment', label = 'Assignment:', choices = c('',course_assignments))
  
  
})


output$sp_status <- renderPrint({
  req(!is.null(input$sp_assignment), input$sp_assignment!='')
  
  rvalues$refresh
  
  CID <- courses_df %>% filter(name == input$sp_course) %>% pull(cid)
  AID <- assignments_df %>% filter(cid == CID & name == input$sp_assignment) %>% pull(aid)
  SID <- res_auth$id
  
  student_submission_df <- dbGetQuery(conn, 'select * from StudentSubmission;') %>% as_tibble()
  
  students_graded_assignments_df <- dbGetQuery(conn, "
           select distinct sir.sid, aq.aid
           from StudentItemResponse sir
           join assignmentquestions aq on sir.qid = aq.qid
           ") %>%
    as_tibble()
  
  if(nrow(students_graded_assignments_df %>% filter(sid == SID & aid == AID))==1){
    sp_values$grading_status <- 'Graded'
    paste0('Status: Graded')
  }
  else if(nrow(student_submission_df %>% filter(sid==SID & aid == AID))==1){
    sp_values$grading_status <- 'Submitted'
    paste0('Status: Submitted')
  }
  else {
    sp_values$grading_status <- 'Pending Submission'
    paste0('Status: Pending Submission')
  }
  
})


output$sp_assign_marks_ui <- renderUI({
  req(!is.null(input$sp_assignment), input$sp_assignment!='')
  
  dataTableOutput('sp_assign_marks')
})

output$sp_assign_marks <- renderDataTable({
  req(!is.null(input$sp_assignment), input$sp_assignment!='')
  
  sp_values$grading_status
  
  CID <- courses_df %>% filter(name == input$sp_course) %>% pull(cid)
  AID <- assignments_df %>% filter(cid == CID & name == input$sp_assignment) %>% pull(aid)
  SID <- res_auth$id
  
  
  students_graded_assignments_df <- dbGetQuery(conn, "
           select distinct sir.sid, aq.aid
           from StudentItemResponse sir
           join assignmentquestions aq on sir.qid = aq.qid
           ") %>%
    as_tibble()
  
  if(nrow(students_graded_assignments_df %>% filter(sid == SID & aid == AID))==1){
    
    assign_marks_df <- dbGetQuery(conn,"
                                select sir.qid ,sir.points
                                from StudentItemResponse sir
                                where sir.sid = :s and sir.qid in (
                                select aq.qid
                                from assignmentquestions aq
                                where aid = :a
                                )
                                ",
                                  params = list(s = SID, a = AID)) %>%
      as_tibble() %>%
      arrange(parse_number(qid)) %>%
      mutate(`Question No.` = paste0('Question ',1:n())) %>%
      select(`Question No.`, Marks = points)
    
  }
  else {
    
    QIDS <- dbGetQuery(conn, '
                     select aq.qid
                     from assignmentquestions aq
                     where aid = :a',
                       params = list(a=AID)) %>%
      as_tibble() %>% pull(qid)
    
    assign_marks_df <- tibble(`Question No.` = paste0('Question ',1:length(QIDS)),
                              Marks = '-')
  }
  
  datatable(assign_marks_df,
            class = 'cell-border stripe',
            rownames = F,
            options = list(autoWidth = T))
  
})

output$sp_pdf_upload_ui <- renderUI({
  req(!is.null(input$sp_assignment), input$sp_assignment!='')
  
  fileInput('sp_pdf_upload', label = NULL, accept = '.pdf', buttonLabel = 'Upload File')
})

observeEvent(input$sp_pdf_upload,{
  sp_values$datapath <- input$sp_pdf_upload$datapath
})

observeEvent(input$sp_assignment,{
  sp_values$datapath <- NULL
})


output$sp_uploaded_file_ui <- renderUI({
  req(!is.null(sp_values$datapath),!is.null(input$sp_assignment), input$sp_assignment!='')

  CID <- courses_df %>% filter(name == isolate(input$sp_course)) %>% pull(cid)
  AID <- assignments_df %>% filter(cid == CID & name == isolate(input$sp_assignment)) %>% pull(aid)
  
  file_name <- paste0(res_auth$user,'_',CID,'_',AID,'.pdf')
  #file.copy(input$sp_pdf_upload$datapath, file.path('www',file_name),overwrite = T)
  file.copy(sp_values$datapath, file.path('www',file_name),overwrite = T)
  

  tags$iframe(style="height:600px; width:66%; scrolling:yes",
              src=file_name)
  
  
})

observeEvent(input$sp_submit_button,{
  
  CID <- courses_df %>% filter(name == isolate(input$sp_course)) %>% pull(cid)
  AID <- assignments_df %>% filter(cid == CID & name == isolate(input$sp_assignment)) %>% pull(aid)
  file_name <- paste0(res_auth$user,'_',CID,'_',AID,'.pdf')
  
  dbExecute(conn,"
            INSERT INTO StudentSubmission
            VALUES (:s, :a, :l)
            ",
            params = list(s=res_auth$id, a = AID, l = file_name))
  
  shinyalert(
    title = "Success",
    text = "File uploaded successfully!",
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
  
  rvalues$refresh = rvalues$refresh+1
  
})


output$sp_sbt_btn_ui <- renderUI({
  req(!is.null(input$sp_assignment), !is.null(sp_values$datapath), input$sp_assignment!='')
  
  actionButton('sp_submit_button','Submit Assignment',
               style="color: #fff; background-color: #337ab7; border-color: #337ab7")
})

#----------tab2--------------

output$sp_aa_course_ui <- renderUI({
  req(!is.null(res_auth))
  
  student_courses <- courses_df %>%
    filter(cid %in% (student_enrollment_df %>%
                       filter(sid == res_auth$id) %>%
                       pull(cid))) %>%
    pull(name)
  
  selectInput(inputId = 'sp_aa_course', label = 'Course: ', choices = c('',student_courses))
  
})


output$sp_aa_assign_vis_ui <- renderUI({
  req(!is.null(input$sp_aa_course), input$sp_aa_course!='')
  
  fluidPage(
    h4('Assessment Analysis'),
    hr(style="border-top: 1px solid #000000;"),
    plotOutput('sp_aa_plot_1')
  )
  
})

output$sp_aa_topics_vis_ui <- renderUI({
  req(!is.null(input$sp_aa_course), input$sp_aa_course!='')
  
  
  fluidPage(
    h4('Topic Analysis'),
    hr(style="border-top: 1px solid #000000;"),
    plotOutput('sp_aa_plot_2')
  )
  
})

output$sp_aa_plot_1 <- renderPlot({
  req(!is.null(input$sp_aa_course), input$sp_aa_course!='')
  
  CID <- courses_df %>% filter(name == input$sp_aa_course) %>% pull(cid)
  SID <- res_auth$id
  
  rvalues$refresh
  
  df <- dbGetQuery(conn,"
                   select sir.sid, sir.qid, sir.points, a.aid, a.aname
                   from (select aid, name as aname from assignment where cid = :c) a
                   join assignmentquestions aq on a.aid=aq.aid
                   join (select * from StudentItemResponse where sid = :s) sir on sir.qid=aq.qid
                   ",
                   params = list(c = CID, s = SID)) %>%
    as_tibble()
  
  all_assignments_df <- dbGetQuery(conn,"
                                   select aid, name as aname
                                   from assignment where cid = :c;
                                   ",
                                   params = list(c = CID)) %>%
    as_tibble()
  
  # vis_df <- df %>%
  #   group_by(aid, aname, sid) %>%
  #   summarise(tot_points = sum(points),
  #             max_points = 10*n()) %>%
  #   ungroup() %>%
  #   mutate(avg_perc_points = 100*(tot_points/max_points)) %>%
  #   group_by(aid, aname) %>%
  #   summarise(assign_avg_points = mean(avg_perc_points)) %>%
  #   ungroup()
  
  vis_df <- df %>%
    group_by(aid, aname) %>%
    summarise(assign_avg_points = 10*mean(points)) %>%
    ungroup()
  
  vis_df %>%
    bind_rows(all_assignments_df %>%
                anti_join(vis_df) %>%
                mutate(assign_avg_points = 0)) %>%
    mutate(aname = factor(aname, levels = mixedsort(aname))) %>%
    ggplot(aes(x = aname, y = assign_avg_points)) +
    geom_bar(fill = '#4ebfc7',stat = 'identity') +
    scale_y_continuous('Assignment % Score') +
    scale_x_discrete('Assignment')
  
})

output$sp_aa_plot_2 <- renderPlot({
  req(!is.null(input$sp_aa_course), input$sp_aa_course!='')
  
  CID <- courses_df %>% filter(name == input$sp_aa_course) %>% pull(cid)
  SID <- res_auth$id
  
  rvalues$refresh
  
  df <- dbGetQuery(conn,"
                   select sir.sid, sir.qid, sir.points, t.topic_id, t.topic
                   from (select aid, name as aname from assignment where cid = :c) a
                   join assignmentquestions aq on a.aid=aq.aid
                   join (select * from StudentItemResponse where sid = :s) sir on sir.qid=aq.qid
                   join topics t on aq.topic_id=t.topic_id
                   ",
                   params = list(c = CID, s = SID)) %>%
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
    scale_y_continuous('Topic % Score') +
    scale_x_discrete('Topic') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
})