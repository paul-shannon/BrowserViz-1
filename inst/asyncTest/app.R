library(httpuv)
library(jsonlite)
#--------------------------------------------------------------------------------
wsCon <- new.env(parent=emptyenv())
.messageFromBrowser <- NULL
#--------------------------------------------------------------------------------
setup <- function(wsCon)
{
   wsCon$open <- FALSE
   wsCon$wsID <- NULL
   wsCon$ws <- NULL
   wsCon$result <- NULL

   wsCon$call = function(req) { # "call" processes http requests
     wsUrl = paste(sep='',
                   '"',
                  "ws://",
                  ifelse(is.null(req$HTTP_HOST), req$SERVER_NAME, req$HTTP_HOST),
                  '"')
    list(
      status = 200L,
      headers = list('Content-Type' = 'text/html'),
      body = c(file="index.html"))
     }
   wsCon$onWSOpen = function(ws) {
      wsCon$ws <- ws
      ws$onMessage(function(binary, message) {
          .messageFromBrowser <<- message
         }) # onMessage
       wsCon$open <- TRUE
       } # onWSOpen

   wsCon

} # setup
#--------------------------------------------------------------------------------
send <- function(wsCon, msg)
{
  wsCon$ws$send(msg)

} # send
#--------------------------------------------------------------------------------
demo <- function()
{
   wsCon <- setup(wsCon)
   port <- 8765
   browseURL(sprintf("http://localhost:%d", port))
   wsCon$id <- startDaemonizedServer("0.0.0.0", port, wsCon)

   Sys.sleep(2)

   message.1 <- toJSON(list(cmd="RVersion", payload=paste(as.character(R.version), collapse=";")), auto_unbox=TRUE)
   send(wsCon, message.1)
   Sys.sleep(2)
   message.returned <- .messageFromBrowser
   print(message.returned)

   message.1 <- toJSON(list(cmd="httpuvVersion", payload=as.character(packageVersion("httpuv"))), auto_unbox=TRUE)
   send(wsCon, message.1)
   Sys.sleep(2)
   message.returned <- .messageFromBrowser
   print(message.returned)

   send(wsCon, toJSON(list(cmd="toUpper", payload="this should be returned in upper case"), auto_unbox=TRUE))
   Sys.sleep(2)
   message.returned <- .messageFromBrowser
   print(message.returned)

   send(wsCon, toJSON(list(cmd="roundTrip", payload=matrix(1:9, nrow=3)), auto_unbox=TRUE))
   Sys.sleep(2)

   send(wsCon, toJSON(list(cmd="roundTrip", payload=matrix(1:900, nrow=30)), auto_unbox=TRUE))
   Sys.sleep(2)
   mtx.returned <- fromJSON(.messageFromBrowser)
   stopifnot(dim(mtx.returned) == c(30,30))
   printf("success")

} # demo
#--------------------------------------------------------------------------------
