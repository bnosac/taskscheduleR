

#' @title Get all the tasks which are currently scheduled at the Windows task scheduler.
#' @description Get all the tasks which are currently scheduled at the Windows task scheduler.
#' 
#' @return a data.frame with scheduled tasks as returned by schtasks /Query for which the Taskname or second
#' column in the dataset the preceding \\ is removed
#' @param encoding encoding of the CSV which schtasks.exe generates. Defaults to UTF-8.
#' @param ... optional arguments passed on to \code{fread} in order to read in the CSV file which schtasks generates
#' @export
#' @examples 
#' x <- taskscheduler_ls()
#' x
taskscheduler_ls <- function(encoding = 'UTF-8', ...){
  change_code_page <- system("chcp 65001", intern = TRUE)
  cmd <- sprintf('schtasks /Query /FO CSV /V')
  x <- system(cmd, intern = TRUE)
  f <- tempfile()
  writeLines(x, f)
  x <- try(data.table::fread(f, encoding = encoding, ...), silent = TRUE)
  if(inherits(x, "try-error")){
    x <- utils::read.csv(f, check.names = FALSE, stringsAsFactors=FALSE, encoding = encoding, ...)
  }
  x <- data.table::setDF(x)
  if("TaskName" %in% names(x)){
    try(x$TaskName <- gsub("^\\\\", "", x$TaskName), silent = TRUE)  
  }else{
    try(x[, 2] <- gsub("^\\\\", "", x[, 2]), silent = TRUE)  
  }
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
#' Only applicable if schedule is of type 'MONTHLY', 'WEEKLY', 'DAILY', 'HOURLY', 'MINUTE'. Defaults to today in '\%d/\%m/\%Y' format. Change to your locale format if needed.
#' @param days character string with days on which to run the script if schedule is 'WEEKLY' or 'MONTHLY'. Possible values
#' are * (all days). For weekly: 'MON', 'TUE', 'WED', 'THU', 'FRI', 'SAT', 'SUN' or a vector of these in your locale.
#' For monthly: 1:31 or a vector of these.
#' @param months character string with months on which to run the script if schedule is 'MONTHLY'. Possible values
#' are * (all months) or 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC' or a vector of these in your locale.
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
#' cat(readLines(myscript), sep = "\n")
#' 
#' \dontrun{
#' ## Run script once at a specific timepoint (within 62 seconds)
#' runon <- format(Sys.time() + 62, "%H:%M")
#' taskscheduler_create(taskname = "myfancyscript", rscript = myscript, 
#'  schedule = "ONCE", starttime = runon)
#'  
#' ## Run every day at the same time on 09:10, starting from tomorrow on
#' ## Mark: change the format of startdate to your locale if needed (e.g. US: %m/%d/%Y)
#' taskscheduler_create(taskname = "myfancyscriptdaily", rscript = myscript, 
#'  schedule = "DAILY", starttime = "09:10", startdate = format(Sys.Date()+1, "%d/%m/%Y"))
#'  
#' ## Run every week on Sunday at 09:10
#' taskscheduler_create(taskname = "myfancyscript_sun", rscript = myscript, 
#'   schedule = "WEEKLY", starttime = "09:10", days = 'SUN')
#'
#' ## Run every 5 minutes, starting from 10:40
#' taskscheduler_create(taskname = "myfancyscript_5min", rscript = myscript,
#'   schedule = "MINUTE", starttime = "10:40", modifier = 5)
#'   
#' ## Run every minute, giving some command line arguments which can be used in the script itself
#' taskscheduler_create(taskname = "myfancyscript_withargs_a", rscript = myscript,
#'   schedule = "MINUTE", rscript_args = "productxyz 20160101")
#' taskscheduler_create(taskname = "myfancyscript_withargs_b", rscript = myscript,
#'   schedule = "MINUTE", rscript_args = c("productabc", "20150101"))
#'   
#' alltasks <- taskscheduler_ls()
#' subset(alltasks, TaskName %in% c("myfancyscript", "myfancyscriptdaily"))
#' # The field TaskName might have been different on Windows with non-english language locale
#' 
#' taskscheduler_delete(taskname = "myfancyscript")
#' taskscheduler_delete(taskname = "myfancyscriptdaily")
#' taskscheduler_delete(taskname = "myfancyscript_sun")
#' taskscheduler_delete(taskname = "myfancyscript_5min")
#' taskscheduler_delete(taskname = "myfancyscript_withargs_a")
#' taskscheduler_delete(taskname = "myfancyscript_withargs_b")
#' 
#' ## Have a look at the log
#' mylog <- system.file("extdata", "helloworld.log", package = "taskscheduleR")
#' cat(readLines(mylog), sep = "\n")
#' }
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
  if(grepl("^\\~", rscript)) rscript <- path.expand(rscript)
  schedule <- match.arg(schedule)
  if("*" %in% days){
    days <- "*"
  }else{
    days <- paste(days, collapse = ",")
  }
  if("*" %in% months){
    months <- "*"
  }else{
    months <- paste(months, collapse = ",")
  }
  
  
  taskname <- force(taskname)
  if(length(grep(" ", taskname)) > 0){
    taskname <- gsub(" ", "-", taskname)  
    message(sprintf("No spaces are allowed in taskname, changing the name of the task to %s", taskname))
  }
  if(length(grep(" ", rscript)) > 0){
    message(sprintf("Full path to filename '%s' contains spaces, it is advised to put your script in another location which contains no spaces", rscript))
  }
  task <- sprintf("cmd /c %s %s %s >> %s 2>&1", Rexe, shQuote(rscript), paste(rscript_args, collapse = " "), shQuote(sprintf("%s.log", tools::file_path_sans_ext(rscript))))
  if(nchar(task) > 260){
    warning(sprintf("Passing on this to the TR argument of schtasks.exe: %s, this is too long. Consider putting your scripts into another folder", task))
  }
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
#' \dontrun{
#' x <- taskscheduler_ls()
#' x
#' # The field TaskName might have been different on Windows with non-english language locale
#' task <- x$TaskName[1] 
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
#' @export taskscheduler_runnow
#' @export taskcheduler_runnow
#' @aliases taskcheduler_runnow
#' @examples 
#' \dontrun{
#' myscript <- system.file("extdata", "helloworld.R", package = "taskscheduleR")
#' taskscheduler_create(taskname = "myfancyscript", rscript = myscript, 
#'  schedule = "ONCE", starttime = format(Sys.time() + 10*60, "%H:%M"))
#' 
#' taskscheduler_runnow("myfancyscript")
#' Sys.sleep(5)
#' taskscheduler_stop("myfancyscript")
#' 
#' 
#' taskscheduler_delete(taskname = "myfancyscript")
#' }
taskscheduler_runnow <- function(taskname){
  cmd <- sprintf('schtasks /Run /TN %s', shQuote(taskname, type = "cmd"))
  system(cmd, intern = FALSE)
}


#' @title Stop the run of a specific task which is running in the Windows task scheduler.
#' @description Stop the run of a specific task which is running in the Windows task scheduler.
#' 
#' @param taskname the name of the task to stop. See the example.
#' @return the system call to schtasks /End 
#' @export taskscheduler_stop
#' @export taskcheduler_stop
#' @aliases taskcheduler_stop
#' @examples 
#' \dontrun{
#' myscript <- system.file("extdata", "helloworld.R", package = "taskscheduleR")
#' taskscheduler_create(taskname = "myfancyscript", rscript = myscript, 
#'  schedule = "ONCE", starttime = format(Sys.time() + 10*60, "%H:%M"))
#' 
#' taskscheduler_runnow("myfancyscript")
#' Sys.sleep(5)
#' taskscheduler_stop("myfancyscript")
#' 
#' 
#' taskscheduler_delete(taskname = "myfancyscript")
#' }
taskscheduler_stop <- function(taskname){
  cmd <- sprintf('schtasks /End /TN %s', shQuote(taskname, type = "cmd"))
  system(cmd, intern = FALSE)
}


taskcheduler_stop <- function(taskname){
  .Deprecated("taskscheduler_stop", msg = "Use taskscheduler_stop instead of taskcheduler_stop")
  taskscheduler_stop(taskname)
} 

taskcheduler_runnow <- function(taskname){
  .Deprecated("taskscheduler_stop", msg = "Use taskscheduler_runnow instead of taskcheduler_runnow")
  taskscheduler_runnow(taskname)
}
