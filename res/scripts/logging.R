#############################################################################################
# Defines functions used to log messages.
# 
# Usage:
# 	# start logging
# 	start.rec.log(text="mylogfile")
# 	
# 	# log some message
# 	tlog(<log offset>,"<log msg>")
#
#	# specific function for loops
# 	n <- 100
#	# specify the total number of iteration when logging
# 	tlog.start.loop(0,n,"Starting the loop")
#	for(i in 1:n)
#	{	Sys.sleep(runif(n=1,min=0,max=1))
#		# specify the current iteration when logging
#		# will log the estimated time before the loop ends
#		tlog.loop(2,i,"Iteration ",i,"/",n)
#	}
#	# will dislay the elapsed time for this loop
#	tlog.end.loop(0,"Finished the loop")
# 
# 	# close log file
# 	end.rec.log()
# 
# 05/2016 Vincent Labatut
#############################################################################################
# folder to store log files
FOLDER_LOG <- "log"
dir.create(path=FOLDER_LOG, showWarnings=FALSE, recursive=TRUE)

# start time of the log
START_TIME <- Sys.time()
# no opened log file 
CONNECTION <- NA

# loop start time
LOOP_START_TIME <- NA
# total number of loop iterations
TOTAL_ITERATIONS <- NA




#############################################################################################
# Start recording the logs in a text file.
#############################################################################################
start.rec.log <- function(text=NA)
{	START_TIME <<- Sys.time()
	
	prefix <- format(START_TIME,"%Y%m%d_%H%M%S")
	log.file <- file.path(FOLDER_LOG,prefix)
	if(!is.na(text))
		log.file <- paste0(log.file,"_",text)
	log.file <- paste0(log.file,".txt")
	CONNECTION <<- file(log.file, encoding="UTF8")
	sink(CONNECTION, append=TRUE, split=TRUE)
}




#############################################################################################
# Stops recording the logs in a text file.
#############################################################################################
end.rec.log <- function()
{	end.time <- Sys.time()
	duration <- difftime(end.time, START_TIME, units="secs")
	tlog(0, "Total processing time: ", format(.POSIXct(duration,tz="GMT"),"%H:%M:%S"))
	sink()
	close(CONNECTION)
}




#############################################################################################
# Computes the prefix used in log messages.
#
# offset: number of "." used to represent the hierarchical level of the message.
#############################################################################################
get.log.prefix <- function(offset=NA)
{	prefix <- paste0("[",format(Sys.time(),"%a %d %b %Y %X"),"] ")
	if(!is.na(offset))
	{	if(is.numeric(offset))
		{	os <- paste(rep(".",offset), sep="", collapse="")
			prefix <- paste0(prefix, os)
		}
		else
			prefix <- paste0(prefix, offset)
	}
	return(prefix)
}




#############################################################################################
# Logs the specified message on screen, adding current date and time, and possibly some
# offset (to represent the hierarchy of function calls).
#
# offset: number of "." used to represent the hierarchical level of the message.
# ...: parameters fetched to the cat function.
#############################################################################################
tlog <- function(offset=NA, ...)
{	prefix <- get.log.prefix(offset)
	cat(prefix, ..., "\n", sep="")
}




#############################################################################################
# Logs the specified message on screen, adding current date and time, and possibly some
# offset (to represent the hierarchy of function calls). Moreover, this specific function
# is designed to be used when starting a loop, in order to automatically display extra info.
#
# offset: number of "." used to represent the hierarchical level of the message.
# total.it: number of iterations of the loop.
# ...: parameters fetched to the cat function.
#############################################################################################
tlog.start.loop <- function(offset=NA, total.it, ...)
{	TOTAL_ITERATIONS <<- total.it
	LOOP_START_TIME <<- Sys.time()
	tlog(offset, ...)
}




#############################################################################################
# Logs the specified message on screen, adding current date and time, and possibly some
# offset (to represent the hierarchy of function calls). Moreover, this specific function
# is designed to be used when iterating inside a loop, in order to automatically display extra 
# info.
#
# offset: number of "." used to represent the hierarchical level of the message.
# it: current iteration.
# ...: parameters fetched to the cat function.
#############################################################################################
tlog.loop <- function(offset=NA, it, ...)
{	prefix <- get.log.prefix(offset)
	
	cur.time <- Sys.time()
	el.duration <- as.numeric(difftime(cur.time, LOOP_START_TIME, units="secs"))
	avg.duration <- el.duration / it
	rem.duration <- as.difftime(max(0, avg.duration * TOTAL_ITERATIONS - el.duration), units="secs")
	suffix <- paste0(" [[ETA: ",format(.POSIXct(rem.duration,tz="GMT"),"%H:%M:%S"),"]]")
	
	cat(prefix, ..., suffix, "\n", sep="")
}




#############################################################################################
# Logs the specified message on screen, adding current date and time, and possibly some
# offset (to represent the hierarchy of function calls). Moreover, this specific function
# is designed to be used when exiting a loop, in order to automatically display extra info.
#
# offset: number of "." used to represent the hierarchical level of the message.
# ...: parameters fetched to the cat function.
#############################################################################################
tlog.end.loop <- function(offset=NA, ...)
{	prefix <- get.log.prefix(offset)
	
	end.time <- Sys.time()
	duration <- difftime(end.time, LOOP_START_TIME, units="secs")
	suffix <- paste0(" [[Total duration: ",format(.POSIXct(duration,tz="GMT"),"%H:%M:%S"),"]]")
	
	cat(prefix, ..., suffix, "\n", sep="")
}
