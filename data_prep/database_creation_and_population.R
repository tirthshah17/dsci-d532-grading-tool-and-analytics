#-----AUTHORSHIP - TIRTH SHAH (for the entire code)-------------------
library(tidyverse)
library(DBI)
library(RSQLite)
library(rstudioapi)

#----------DATABASE SCHEMA CREATION--------------------------

database_path <- file.path(getActiveProject(), 'data','application_database.db')

conn <- dbConnect(RSQLite::SQLite(),database_path)

dbExecute(conn, "
CREATE TABLE IF NOT EXISTS Student
(
  sid text PRIMARY KEY, 
  name text NOT NULL, 
  major text NOT NULL
)
")

dbExecute(conn, "
CREATE TABLE IF NOT EXISTS Course
(
  cid text PRIMARY KEY, 
  name text NOT NULL, 
  department text NOT NULL
)
")

dbExecute(conn, "
CREATE TABLE IF NOT EXISTS Instructor
(
  tid text PRIMARY KEY, 
  name text NOT NULL, 
  department text NOT NULL
)
")

dbExecute(conn, "
CREATE TABLE IF NOT EXISTS Assignment
(
  aid text PRIMARY KEY, 
  name text NOT NULL, 
  cid text NOT NULL,
  FOREIGN KEY (cid) REFERENCES Course(cid)
)
")

dbExecute(conn, "
CREATE TABLE IF NOT EXISTS Topics
(
  topic_id text PRIMARY KEY, 
  topic text NOT NULL
)
")

dbExecute(conn, "
CREATE TABLE IF NOT EXISTS AssignmentQuestions
(
  qid text PRIMARY KEY, 
  aid text NOT NULL, 
  topic_id text NOT NULL,
  FOREIGN KEY (aid) REFERENCES Assignment(aid),
  FOREIGN KEY (topic_id) REFERENCES Topics(topic_id)
)
")

dbExecute(conn, "
CREATE TABLE IF NOT EXISTS StudentEnrollment
(
  sid text NOT NULL,
  cid text NOT NULL,
  term text NOT NULL,
  PRIMARY KEY (sid,cid),
  FOREIGN KEY (sid) REFERENCES Student(sid),
  FOREIGN KEY (cid) REFERENCES Course(cid)
  
)
")

dbExecute(conn, "
CREATE TABLE IF NOT EXISTS InstructorTeaches
(
  tid text NOT NULL,
  cid text NOT NULL,
  term text NOT NULL,
  PRIMARY KEY (tid,cid),
  FOREIGN KEY (tid) REFERENCES Instructor(tid),
  FOREIGN KEY (cid) REFERENCES Course(cid)
  
)
")

dbExecute(conn, "
CREATE TABLE IF NOT EXISTS StudentEnrollment
(
  sid text NOT NULL,
  cid text NOT NULL,
  term text NOT NULL,
  PRIMARY KEY (sid,cid),
  FOREIGN KEY (sid) REFERENCES Student(sid),
  FOREIGN KEY (cid) REFERENCES Course(cid)
  
)
")

dbExecute(conn, "
CREATE TABLE IF NOT EXISTS StudentSubmission
(
  sid text NOT NULL,
  aid text NOT NULL,
  link text NOT NULL,
  PRIMARY KEY (sid,aid),
  FOREIGN KEY (sid) REFERENCES Student(sid),
  FOREIGN KEY (aid) REFERENCES Assignment(aid)
  
)
")

dbExecute(conn, "
CREATE TABLE IF NOT EXISTS StudentItemResponse
(
  sid text NOT NULL,
  qid text NOT NULL,
  points real NOT NULL,
  PRIMARY KEY (sid,qid),
  FOREIGN KEY (sid) REFERENCES Student(sid),
  FOREIGN KEY (qid) REFERENCES AssignmentQuestions(qid)
)
")

#------------DATABASE POPULATION------------------------

raw_data_df <- read_csv(file.path(getActiveProject(),'data','simulated_data_tables','initial_raw_data.csv'))
courses_df <- read_csv(file = file.path(getActiveProject(),'data','simulated_data_tables','course_table.csv'))
students_df <- read_csv(file = file.path(getActiveProject(),'data','simulated_data_tables','student_table.csv'))
instructor_df <- read_csv(file = file.path(getActiveProject(),'data','simulated_data_tables','instructor_table.csv'))
assignments_df <- read_csv(file = file.path(getActiveProject(),'data','simulated_data_tables','assignment_table.csv'))
topics_df <- read_csv(file = file.path(getActiveProject(),'data','simulated_data_tables','topics_table.csv'))
questions_df <- read_csv(file = file.path(getActiveProject(),'data','simulated_data_tables','questions_table.csv'))


#inserting tuples in student relation


df <- students_df %>%
  distinct(sid, name=student_name, major = student_major)

dbWriteTable(conn,
             'Student',
             df,
             overwrite = T)

#res <- dbGetQuery(conn, 'select * from student;') %>% as_tibble()

#inserting tuples in courses relation

df <- courses_df %>%
  distinct(cid, name=course_name, department = course_dept)

dbWriteTable(conn,
             'Course',
             df,
             overwrite = T)

#res <- dbGetQuery(conn, 'select * from course;') %>% as_tibble()

#inserting tuples in instructor relation

df <- instructor_df %>%
  distinct(tid, name=instructor_name, department = instructor_dept)

dbWriteTable(conn,
             'Instructor',
             df,
             overwrite = T)

#res <- dbGetQuery(conn, 'select * from instructor;') %>% as_tibble()

#inserting tuples in assignment relation

df <- assignments_df %>%
  distinct(aid, name = assignment_name, cid)

dbWriteTable(conn,
             'Assignment',
             df,
             overwrite = T)

#res <- dbGetQuery(conn, 'select * from assignment;') %>% as_tibble()

#inserting tuples in topics relation


df <- topics_df %>%
  distinct(topic_id, topic = topic_name)

dbWriteTable(conn,
             'Topics',
             df,
             overwrite = T)

#res <- dbGetQuery(conn, 'select * from topics;') %>% as_tibble()

#inserting tuples in assignment questions relation

df <- questions_df %>%
  distinct(qid, topic_id, aid)

dbWriteTable(conn,
             'AssignmentQuestions',
             df,
             overwrite = T)

#res <- dbGetQuery(conn, 'select * from assignmentquestions;') %>% as_tibble()

#inserting tuples in student enrollment relation

df <- raw_data_df %>%
  distinct(sid, cid, term)

dbWriteTable(conn,
             'StudentEnrollment',
             df,
             overwrite = T)

#res <- dbGetQuery(conn, 'select * from StudentEnrollment;') %>% as_tibble()

#inserting tuples in instructor teaches relation

df <- raw_data_df %>%
  distinct(tid, cid, term)

dbWriteTable(conn,
             'InstructorTeaches',
             df,
             overwrite = T)

#res <- dbGetQuery(conn, 'select * from InstructorTeaches;') %>% as_tibble()

#inserting tuples in student submission relation

df <- raw_data_df %>%
  distinct(sid, aid, link = submission_link)

dbWriteTable(conn,
             'StudentSubmission',
             df,
             overwrite = T)

#res <- dbGetQuery(conn, 'select * from StudentSubmission;') %>% as_tibble()

#inserting tuples in student item response relation

df <- raw_data_df %>%
  distinct(sid, qid, points)

dbWriteTable(conn,
             'StudentItemResponse',
             df,
             overwrite = T)

#res <- dbGetQuery(conn, 'select * from StudentItemResponse;') %>% as_tibble()
