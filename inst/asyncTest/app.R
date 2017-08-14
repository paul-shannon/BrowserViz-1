library(httpuv)
library(jsonlite)
#--------------------------------------------------------------------------------
wsCon <- new.env(parent=emptyenv())
.messageFromBrowser <- NULL
nextPort <- 8765

#--------------------------------------------------------------------------------
createWebSocket <- function(port)
{
   callFunction <- function(req) { # "call" processes http requests
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

   onWSOpenFunction = function(ws){
      printf("--- app.ws opening");
      app$ws <- ws
      ws$onMessage(function(binary, message) {
          .messageFromBrowser <<- message
      }) # onMessage
      app$open <- TRUE
      } # onWSOpen

   app <- new.env(parent=emptyenv())
   app$call=callFunction
   app$onWSOpen=onWSOpenFunction
   app$ws=NULL
   app$open=FALSE
   app$id=NULL

   app

} # createWebSocket
#------------------------------------------------------------------------------------------------------------------------
callFunction <- function(req) { # "call" processes http requests
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

onWSOpenFunction = function(ws){
   printf("--- app.ws opening");
   app$ws <- ws
   ws$onMessage(function(binary, message) {
       .messageFromBrowser <<- message
   }) # onMessage
   app$open <- TRUE
   } # onWSOpen

app <- new.env(parent=emptyenv())
app$call=callFunction
app$onWSOpen=onWSOpenFunction
app$ws=NULL
app$open=FALSE
app$id=NULL
#--------------------------------------------------------------------------------
setup <- function(wsCon)
{
   wsCon$open <- FALSE
   wsCon$id <- NULL
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
displayWS <- function(ws)
{
   names <- ls(ws)
   for(name in names){
     class.type <- class(ws[[name]])
     value <- ""
     if(class.type %in% c("logical", "character", "numeric"))
        value <- ws[[name]]
     printf("%10s: %s - %s", name, class.type, value)
     }

} # displayWS
#--------------------------------------------------------------------------------
demo <- function(portNumber)
{
   wsCon <- setup(wsCon)
   portNumber <- 5000
   browseURL(sprintf("http://localhost:%d", portNumber))
   app$id <- startDaemonizedServer("127.0.0.1", portNumber, app)

   wsCon$id <- startDaemonizedServer("0.0.0.0", portNumber, wsCon)

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
   print("success")


} # demo
#--------------------------------------------------------------------------------
