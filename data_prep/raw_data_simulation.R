#-----AUTHORSHIP - TIRTH SHAH (for the entire code)-------------------
#references - 1) https://sqlite.org/
# 2) https://solutions.posit.co/connections/db/databases/sqlite/

library(tidyverse)
library(magrittr)
library(rstudioapi)
library(randomNames)
library(gtools)

set.seed(18)

#--------------courses-------------
courses <- c('Machine Learning', 'Advanced Database Concepts', 'Introduction to Statistics', 'Applied Algorithms',
             'Applied Database Technologies', 'Applied Distributed Systems', 'Exploratory Data Analysis',
             'Strategic Tax Planning', 'Quantum Computing','Advanced Organic Chemistry')

dept <- c('CS','CS','STAT','CS','CS','CS','STAT','BUSINESS','MATH','CHEMISTRY')

courses_df <- tibble::tibble(cid = paste0('cid_',1:length(courses)),
                             course_name = courses,
                             course_dept = dept)

write_csv(courses_df, file = file.path(getActiveProject(),'data','simulated_data_tables','course_table.csv'))

#----------students----------------

n_students <- 500
student_names <- randomNames(n_students)
unique_majors <- c('CS','DS','Statistics','Mathematics','Accounting','Business Analytics',
                   'Chemistry')

students_df <- tibble::tibble(sid = paste0('sid_',1:n_students),
                              student_name = student_names,
                              student_major = sample(unique_majors,
                                             size = n_students,
                                             replace = T,
                                             prob = c(0.35,0.15,0.2,0.1,0.05,0.05,0.1)))

write_csv(students_df, file = file.path(getActiveProject(),'data','simulated_data_tables','student_table.csv'))

#------------instructors--------------

n_instructors <- 30
instructor_names <- randomNames(n_instructors)
unique_dept <- unique(dept)


instructor_df <- tibble::tibble(tid = paste0('tid_',1:n_instructors),
                                instructor_name = instructor_names,
                                instructor_dept = sample(unique_dept,
                                                         size = n_instructors,
                                                         replace = T,
                                                         prob = c(0.5,0.2,0.1,0.1,0.1)))

write_csv(instructor_df, file = file.path(getActiveProject(),'data','simulated_data_tables','instructor_table.csv'))

#------------app_creds----------------------------

students_df
instructor_df

user_base <- tibble(user = c(students_df$student_name %>% str_to_lower() %>% str_replace(', ','_'),
                             instructor_df$instructor_name %>% str_to_lower() %>% str_replace(', ','_')),
                    password = c(rep('student',length(students_df$student_name)), rep('instructor',length(instructor_df$instructor_name))),
                    role = c(rep('student',length(students_df$student_name)), rep('teacher',length(instructor_df$instructor_name))),
                    name = c(students_df$student_name, instructor_df$instructor_name),
                    id = c(students_df$sid, instructor_df$tid))

saveRDS(user_base, file = file.path(getActiveProject(),'data','creds.rds'))

#--------------assignments--------------

#n_courses <- length(courses)
# assignments_per_course <- sample(c(9,10,11,12),
#                                  size = n_courses,
#                                  replace = T,
#                                  prob = c(0.4,0.4,0.1,0.1))


assignments_per_course_df <- tibble::tibble(cid = unique(courses_df$cid),
                                            n_assignments = sample(c(9,10,11,12),
                                                                   size = length(unique(courses_df$cid)),
                                                                   replace = T,
                                                                   prob = c(0.4,0.4,0.1,0.1))) 

assignments_df <- tibble::tibble(aid = paste0('aid_',1:sum(assignments_per_course_df$n_assignments)),
                                 cid = rep(assignments_per_course_df$cid, times = assignments_per_course_df$n_assignments)) %>%
  group_by(cid) %>%
  mutate(assignment_name = paste0('Assignment ',row_number())) %>%
  ungroup() %>%
  select(aid, assignment_name, cid)

write_csv(assignments_df, file = file.path(getActiveProject(),'data','simulated_data_tables','assignment_table.csv'))


#-----------topics----------------------

topics_per_course_df <- tibble::tibble(cid = unique(courses_df$cid),
                                       n_topics = sample(c(18:24),
                                                         size = length(unique(courses_df$cid)),
                                                         replace = T))

topics_df <- tibble(topic_id = paste0('topic_id_',1:sum(topics_per_course_df$n_topics)),
                    cid = rep(topics_per_course_df$cid, times = topics_per_course_df$n_topics)) %>%
  group_by(cid) %>%
  mutate(topic_name = paste0('Topic ',row_number())) %>%
  ungroup() %>%
  select(topic_id, topic_name, cid)

write_csv(topics_df, file = file.path(getActiveProject(),'data','simulated_data_tables','topics_table.csv'))

#-----------questions-------------------

questions_per_assignment_df <- tibble(aid = unique(assignments_df$aid),
                                      n_questions = sample(5:8,
                                                           size = length(unique(assignments_df$aid)),
                                                           replace = T))

questions_df <- tibble(qid = paste0('qid_',1:sum(questions_per_assignment_df$n_questions)),
                       aid = rep(questions_per_assignment_df$aid, times = questions_per_assignment_df$n_questions)) %>%
  left_join(assignments_df %>% select(aid,cid), by = 'aid') %>%
  inner_join(topics_df %>% select(topic_id, cid), by = 'cid') %>%
  group_by(qid) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(-cid) %>%
  arrange(parse_number(qid))


write_csv(questions_df, file = file.path(getActiveProject(),'data','simulated_data_tables','questions_table.csv'))

#---------studentenrollment table-----------------------

student_enrollment_df <- students_df %>%
  sample_frac(0.3) %>%
  cross_join(courses_df) %>%
  select(sid, cid) %>%
  group_by(sid) %>%
  sample_n(3) %>%
  ungroup() %>%
  mutate(term = 'Spring23') %>%
  arrange(parse_number(sid))


#-----------instructor teaches table----------------------

instructor_teaches_df <- courses_df %>%
  inner_join(instructor_df, by = c('course_dept' = 'instructor_dept')) %>%
  group_by(cid) %>%
  sample_n(1) %>%
  ungroup() %>%
  mutate(term = 'Spring23') %>%
  arrange(parse_number(cid)) %>%
  select(tid, cid, term)
  
#----------student submission table-----------------------

student_submission_df <- assignments_df %>%
  group_by(cid) %>%
  filter(between(row_number(),1,floor(0.5*n()))) %>%
  ungroup() %>%
  inner_join(student_enrollment_df, by = 'cid', relationship = "many-to-many") %>%
  select(sid, aid) %>%
  mutate(submission_link = 'raw_data_simulation_sample_file.pdf') %>%
  arrange(parse_number(sid), parse_number(aid))

#-----------student item response table---------------------

student_item_response_df <- questions_df %>%
  left_join(assignments_df, by = 'aid') %>%
  inner_join(student_enrollment_df, by = 'cid', relationship = "many-to-many") %>%
  select(qid, aid, sid) %>%
  semi_join(student_submission_df, by = c('aid','sid')) %>%
  select(sid, qid) %>%
  arrange(parse_number(sid), parse_number(qid)) %>%
  mutate(points = round(runif(n(),0,10)))


#--------final raw data prep combining all the data---------------

raw_data_df <- student_item_response_df %>%
  mutate(row_id = paste0('id_',1:n())) %>%
  left_join(students_df, by = 'sid') %>%
  left_join(questions_df, by = 'qid') %>%
  left_join(topics_df, by = 'topic_id') %>%
  left_join(assignments_df %>% select(-cid), by = 'aid') %>%
  left_join(student_submission_df, by = c('sid','aid')) %>%
  left_join(courses_df, by = 'cid') %>%
  left_join(instructor_teaches_df, by = 'cid') %>%
  left_join(instructor_df, by = 'tid') %>%
  select(row_id,
         qid,
         sid,
         points,
         student_name,
         student_major,
         topic_id,
         topic_name,
         aid,
         assignment_name,
         submission_link,
         cid,
         course_name,
         course_dept,
         tid,
         instructor_name,
         instructor_dept,
         term)

write_csv(raw_data_df, file = file.path(getActiveProject(),'data','simulated_data_tables','initial_raw_data.csv'))