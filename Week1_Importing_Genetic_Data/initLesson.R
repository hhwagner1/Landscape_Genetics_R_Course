# Code placed in this file fill be executed every time the
      # lesson is started. Any variables created here will show up in
      # the user's working directory and thus be accessible to them
      # throughout the lesson.


require(adegenet)
require(gstudio)
require(readr)
require(TestCoursePackage)

swirl_options(swirl_logging = TRUE)

.get_course_path <- function(){
  tryCatch(swirl:::swirl_courses_dir(),
           error = function(c) {file.path(find.package("swirl"),"Courses")}
  )
}


# RALU_loci_12pops <- read.csv(system.file("extdata", "RALU_loci_12pops.csv", 
#                             package = "TestCoursePackage"), header=TRUE)

### 1) Make path to data and let user call read.csv(data_path)
#data_path <- file.path(lesson_dir, "RALU_loci_12pops.csv")
#RALU_loci_12pops <- read.csv(data_path)

# RALU_loci_12pops <- read.csv(file.path(.get_course_path(), 
#                                        "Landscape_Genetics_R_Course", 
#                                        "Week_1_Importing_Genetic_Data", 
#                                        "RALU_loci_12pops.csv"))

data(ralu.loci)
write.csv(ralu.loci, "ralu.loci.csv", row.names=F, quote=F)


google_form_decode_HW <-  function (path = file.choose())
{
  encoded <- suppressWarnings(read.csv(path, header = TRUE,
                                       stringsAsFactors = FALSE))
  decoded <- list()
  marked <- list()
  for (i in 1:nrow(encoded)) {
    temp_write <- tempfile()
    #writeChar(encoded[i, ]$Answer.log, temp_write)
    writeChar(as.character(encoded[i, ]$Answer.log), temp_write)
    temp_log <- tempfile()
    base64decode(file = temp_write, output = temp_log)
    decoded[[i]] <- read.csv(temp_log, header = TRUE, stringsAsFactors = FALSE)
    decoded[[i]]$correct <- factor(decoded[[i]]$correct, levels=c(TRUE, FALSE))
    decoded[[i]]$skipped <- factor(decoded[[i]]$skipped, levels=c(FALSE, TRUE))
    marked[[i]] <- c(decoded[[i]]$user[1], with(decoded[[i]], table(correct, skipped, question_number))[1,1,])
  }
  do.call("rbind", decoded)
  data.frame(encoded[,names(encoded)!="Answer.log"], Name=Reduce(rbind, marked)[,1],
             data.frame(Reduce(rbind, marked)[,-1], row.names=c(1:length(marked))))
}

grading <- function(input.log=Log, institution=NULL, weights=c(1,0))
{
  if(!is.null(institution)) {input.log <- input.log[input.log$Institution==institution,]}
  Scores <- input.log[,-c(1:4)]
  Scores <- apply(Scores, 2, as.numeric)
  Marks <- data.frame(input.log[,c(1:4)],
                      Mark=apply(Scores, 1, weighted.mean, w=weights))
  Best.attempt <- data.frame(Institution=
        sapply(split(Marks$Institution, Marks$Email.address),
        function(x) names(sort(-table(x)))[1]),
        Name=sapply(split(as.character(Marks$Name), Marks$Email.address),
        function(x) names(sort(-table(x)))[1]),
        Grade=100 * sapply(split(Marks$Mark, Marks$Email.address), max),
        Attempts=sapply(split(Marks$Mark, Marks$Email.address), length))
  Best.attempt
}

info()
