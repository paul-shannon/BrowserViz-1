library(httpuv)
library(jsonlite)
#--------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
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
#--------------------------------------------------------------------------------
wsSend <- function(ws, msg)
{
  ws$ws$send(msg)

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
demo <- function(portNumber=5000)
{
   printf("--- running demo on port %d", portNumber)

   ws <- createWebSocket(portNumber)
   browseURL(sprintf("http://localhost:%d", portNumber))
   ws$id <- startDaemonizedServer("127.0.0.1", portNumber, ws)
   Sys.sleep(2)  # need to wait for ws$ws$send to be addisnged, onOpen

   message.1 <- toJSON(list(cmd="RVersion", payload=paste(as.character(R.version), collapse=";")), auto_unbox=TRUE)
   wsSend(ws, message.1)
   Sys.sleep(2)
   message.returned <- .messageFromBrowser
   print(message.returned)

   message.1 <- toJSON(list(cmd="httpuvVersion", payload=as.character(packageVersion("httpuv"))), auto_unbox=TRUE)
   wsSend(ws, message.1)
   Sys.sleep(2)
   message.returned <- .messageFromBrowser
   print(message.returned)

   wsSend(ws, toJSON(list(cmd="toUpper", payload="this should be returned in upper case"), auto_unbox=TRUE))
   Sys.sleep(2)
   message.returned <- .messageFromBrowser
   print(message.returned)

   wsSend(ws, toJSON(list(cmd="roundTrip", payload=matrix(1:9, nrow=3)), auto_unbox=TRUE))
   Sys.sleep(2)

   wsSend(ws, toJSON(list(cmd="roundTrip", payload=matrix(1:900, nrow=30)), auto_unbox=TRUE))
   Sys.sleep(2)
   mtx.returned <- fromJSON(.messageFromBrowser)
   stopifnot(dim(mtx.returned) == c(30,30))
   print("success")
   stopDaemonizedServer(ws$id)

} # demo
#--------------------------------------------------------------------------------
