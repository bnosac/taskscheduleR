

#' @title Get all the tasks which are currently scheduled at the Windows task scheduler.
#' @description Get all the tasks which are currently scheduled at the Windows task scheduler.
#' 
#' @return a data.frame with scheduled tasks as returned by schtasks /Query
#' @export
#' @examples 
#' x <- taskscheduler_ls()
#' x
taskscheduler_ls <- function(){
  cmd <- sprintf('schtasks /Query /FO CSV /V')
  x <- system(cmd, intern = TRUE)
  f <- tempfile()
  writeLines(x, f)
  x <- data.table::fread(f)
  x <- data.table::setDF(x)
  try(x$TaskName <- gsub("^\\\\", "", x$TaskName), silent = TRUE)
  on.exit(file.remove(f))
  x
}


#' @title Schedule an R script with the Windows task scheduler.
#' @description Schedule an R script with the Windows task scheduler. E.g. daily, weekly, once, at startup, ...
#' More information about the scheduling format can be found in the docs/schtasks.pdf file inside this package.
#' The rscript file will be scheduled with Rscript.exe and the log of the run will be put in the .log file which can be found in the same directory
#' as the location of the rscript
#' 
#' @param taskname a character string with the name of the task. Defaults to the filename. Should not contain any spaces.
#' @param rscript the full path to the .R script with the R code to execute. Should not contain any spaces.
#' @param schedule when to schedule the \code{rscript}. 
#' Either one of 'ONCE', 'MONTHLY', 'WEEKLY', 'DAILY', 'HOURLY', 'MINUTE', 'ONLOGON', 'ONIDLE'.
#' @param starttime a timepoint in HH:mm format indicating when to run the script. Defaults to within 62 seconds.
#' @param startdate a date that specifies the first date on which to run the task.
#' Only applicable if schedule is of type 'MONTHLY', 'WEEKLY', 'DAILY', 'HOURLY', 'MINUTE'. Defaults to today.
#' @param days character string with days on which to run the script if schedule is 'WEEKLY' or 'MONTHLY'. Possible values
#' are * (all days), 'MON', 'TUE', 'WED', 'THU', 'FRI', 'SAT', 'SUN', 1:31.
#' @param months character string with months on which to run the script if schedule is 'MONTHLY'. Possible values
#' are * (all months), 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'.
#' @param modifier a modifier to apply. See the docs/schtasks.pdf
#' @param idletime integer containing a value that specifies the amount of idle time to wait before 
#' running a scheduled ONIDLE task. The valid range is 1 - 999 minutes.
#' @param Rexe path to Rscript.exe which will be used to run the script. Defaults to Rscript at the bin folder of R_HOME.
#' @param rscript_args character string with further arguments passed on to Rscript. See args in \code{\link{Rscript}}.
#' @param schtasks_extra character string with further schtasks arguments. See the inst/docs/schtasks.pdf 
#' @param debug logical to print the system call to screen
#' @return the system call to schtasks /Create 
#' @export
#' @examples 
#' myscript <- system.file("extdata", "helloworld.R", package = "taskscheduleR")
#' runon <- format(Sys.time() + 62, "%H:%M")
#' taskscheduler_create(taskname = "myfancyscript", rscript = myscript, 
#'  schedule = "ONCE", starttime = runon)
#' taskscheduler_create(taskname = "myfancyscriptdaily", rscript = myscript, 
#'  schedule = "DAILY", starttime = "09:10")
#' alltasks <- taskscheduler_ls()
#' subset(alltasks, TaskName %in% c("myfancyscript", "myfancyscriptdaily"))
#' 
#' taskscheduler_delete(taskname = "myfancyscript")
#' taskscheduler_delete(taskname = "myfancyscriptdaily")
taskscheduler_create <- function(taskname = basename(rscript), 
                                 rscript,
                                 schedule = c('ONCE', 'MONTHLY', 'WEEKLY', 'DAILY', 'HOURLY', 'MINUTE', 'ONLOGON', 'ONIDLE'),
                                 starttime = format(Sys.time() + 62, "%H:%M"),
                                 startdate = format(Sys.Date(), "%d/%m/%Y"),
                                 days  = c('*', 'MON', 'TUE', 'WED', 'THU', 'FRI', 'SAT', 'SUN', 1:31),
                                 months = c('*', 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'),
                                 modifier,
                                 idletime = 60L,
                                 Rexe = file.path(Sys.getenv("R_HOME"), "bin", "Rscript.exe"),
                                 rscript_args = "",
                                 schtasks_extra = "",
                                 debug = FALSE){
  if(!file.exists(rscript)){
    stop(sprintf("File %s does not exist", rscript))
  }
  if(basename(rscript) == rscript){
    warning("Filename does not include the full path, provide %s as full path including the directory", task)
  }
  schedule <- match.arg(schedule)
  days <- match.arg(days)
  months <- match.arg(months)
  
  taskname <- force(taskname)
  if(length(grep(" ", taskname)) > 0){
    taskname <- gsub(" ", "-", taskname)  
    message(sprintf("No spaces are allowed in taskname, changing the name of the task to %s", taskname))
  }
  if(length(grep(" ", rscript)) > 0){
    stop(sprintf("Full path to filename '%s' contains spaces, put your script in another location which contains no spaces", rscript))
  }
  task <- sprintf("cmd /c %s %s %s >> %s.log 2>&1", Rexe, rscript, rscript_args, tools::file_path_sans_ext(rscript))
  cmd <- sprintf('schtasks /Create /TN %s /TR %s /SC %s', 
                 shQuote(taskname, type = "cmd"), 
                 shQuote(task, type = "cmd"),
                 schedule)
  if(!missing(modifier)){
    cmd <- sprintf("%s /MO %s", cmd, modifier)
  }
  if(schedule %in% c('ONIDLE')){
    cmd <- sprintf("%s /I %s", cmd, idletime)
  }
  if(!schedule %in% c('ONLOGON', 'ONIDLE')){
    cmd <- sprintf("%s /ST %s", cmd, starttime)
  }
  if(!schedule %in% c('ONCE', 'ONLOGON', 'ONIDLE')){
    cmd <- sprintf("%s /SD %s", cmd, shQuote(startdate))
  }
  if(schedule %in% c('WEEKLY', 'MONTHLY')){
    cmd <- sprintf("%s /D %s", cmd, days)
  }
  if(schedule %in% c('MONTHLY')){
    cmd <- sprintf("%s /M %s", cmd, months)
  }
  cmd <- sprintf("%s %s", cmd, schtasks_extra)
  if(debug){
    message(sprintf("Creating task schedule: %s", cmd))  
  }
  system(cmd, intern = TRUE)
}


#' @title Delete a specific task which was scheduled in the Windows task scheduler.
#' @description Delete a specific task which was scheduled in the Windows task scheduler.
#' 
#' @param taskname the name of the task to delete. See the example.
#' @return the system call to schtasks /Delete 
#' @export
#' @examples 
#' x <- taskscheduler_ls()
#' x
#' task <- x$TaskName[1]
#' \dontrun{
#' taskscheduler_delete(taskname = task)
#' }
taskscheduler_delete <- function(taskname){
  cmd <- sprintf('schtasks /Delete /TN %s /F', shQuote(taskname, type = "cmd"))
  system(cmd, intern = FALSE)
}



#' @title Immediately run a specific task available in the Windows task scheduler.
#' @description Immediately run a specific task available in the Windows task scheduler.
#' 
#' @param taskname the name of the task to run. See the example.
#' @return the system call to schtasks /Run 
#' @export
#' @examples 
#' myscript <- system.file("extdata", "helloworld.R", package = "taskscheduleR")
#' taskscheduler_create(taskname = "myfancyscript", rscript = myscript, 
#'  schedule = "ONCE", starttime = format(Sys.time() + 10*60, "%H:%M"))
#' 
#' taskcheduler_runnow("myfancyscript")
#' Sys.sleep(5)
#' taskcheduler_stop("myfancyscript")
#' 
#' 
#' taskscheduler_delete(taskname = "myfancyscript")
taskcheduler_runnow <- function(taskname){
  cmd <- sprintf('schtasks /Run /TN %s', shQuote(taskname, type = "cmd"))
  system(cmd, intern = FALSE)
}


#' @title Stop the run of a specific task which is running in the Windows task scheduler.
#' @description Stop the run of a specific task which is running in the Windows task scheduler.
#' 
#' @param taskname the name of the task to stop. See the example.
#' @return the system call to schtasks /End 
#' @export
#' @examples 
#' myscript <- system.file("extdata", "helloworld.R", package = "taskscheduleR")
#' taskscheduler_create(taskname = "myfancyscript", rscript = myscript, 
#'  schedule = "ONCE", starttime = format(Sys.time() + 10*60, "%H:%M"))
#' 
#' taskcheduler_runnow("myfancyscript")
#' Sys.sleep(5)
#' taskcheduler_stop("myfancyscript")
#' 
#' 
#' taskscheduler_delete(taskname = "myfancyscript")
taskcheduler_stop <- function(taskname){
  cmd <- sprintf('schtasks /End /TN %s', shQuote(taskname, type = "cmd"))
  system(cmd, intern = FALSE)
}


