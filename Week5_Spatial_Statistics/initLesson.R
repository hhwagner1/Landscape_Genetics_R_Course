# Code placed in this file fill be executed every time the
      # lesson is started. Any variables created here will show up in
      # the user's working directory and thus be accessible to them
      # throughout the lesson.

swirl_options(swirl_logging = TRUE)

.get_course_path <- function(){
  tryCatch(swirl:::swirl_courses_dir(),
           error = function(c) {file.path(find.package("swirl"),"Courses")}
  )
}

XY <- data.frame(matrix(NA, 6, 2, dimnames=list(NULL, c("X", "Y"))))
XY[6,] <- c(3,1)
XY[5,] <- c(1,2)
XY[4,] <- c(2,3)
XY[3,] <- c(2,5)
XY[2,] <- c(1,5)
XY[1,] <- c(1,6)


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
