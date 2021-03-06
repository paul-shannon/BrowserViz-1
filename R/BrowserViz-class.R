.getBrowser <- function()
{
   # if(.Platform$OS.type == "windows")
   #     stop("BrowserViz not (yet) supported on Windows.")
   # if(nchar(Sys.getenv("BROWSERVIZ_BROWSER")))
   #     Sys.getenv("BROWSERVIZ_BROWSER")
   # else
        getOption("browser")
}
#----------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
#----------------------------------------------------------------------------------------------------
BrowserViz.state <- new.env(parent=emptyenv())
#----------------------------------------------------------------------------------------------------
# the semanitcs of toJSON changed between RJSONIO and jsonlite: in the latter, scalars are
# promoted to arrays of length 1.  rather than change our javascript code, and since such
# promotion -- while sensible in the context of R -- strikes me as gratuitous, I follow
# jeroen ooms suggestion, creating this wrapper
toJSON <- function(..., auto_unbox = TRUE)
{
  jsonlite::toJSON(..., auto_unbox = auto_unbox)
}
#----------------------------------------------------------------------------------------------------
# a default html + javascript file, an example, shows how to setup the websocket, get web page
# dimensions, set and get the browser's window  title

browserVizBrowserFile <- system.file(package="BrowserViz", "scripts", "viz.html")


# this maps from incoming json commands to function calls
dispatchMap <- new.env(parent=emptyenv())

# status is global variable at file scope, invisible outside the package.
# it keeps track of web sockect connection state, and -- crucially --
# holds the result variable.  this solves the latency problem: when we make
# a request to the code running in the browser, the browser later (though
# often very quickly) sends a JSON message back to R.  If we are, for instance,
# asking for the current browser window title (see 'getBrowserWindowTitle' below), that
# result is sent to the  call back we have registered, "handleResponse")
# to make this seem like a synchronous call, the caller sits in a tight sleep loop,
# waiting until status$result is no longer NULL.  getBrowserWindowTitle will then
# parse that JSON response into an R variable.
# the checking of status$result, and its retrieval when ready (no longer null)
# is accomplished by exported methods browserResponseReady and getBrowserResponse,
# to be used by subclasses as well.

status <- new.env(parent=emptyenv())
status$result <- NULL

# the duration of the aforementioned tight loop, waiting for the browser to respond.

sleepTime <- 1

#----------------------------------------------------------------------------------------------------
.BrowserViz <- setClass ("BrowserVizClass",
                         representation = representation (
                                               uri="character",
                                               port="numeric",
                                               websocketConnection="environment",
                                               status="environment",
                                               quiet="logical"),
                         prototype = prototype (uri="http://localhost", 9000)
                         )

#----------------------------------------------------------------------------------------------------
setGeneric('show',                    signature='obj', function(obj) standardGeneric('show'))
setGeneric('port',                    signature='obj', function(obj) standardGeneric('port'))
setGeneric('ready',                   signature='obj', function(obj) standardGeneric('ready'))
setGeneric('getBrowserInfo',          signature='obj', function(obj) standardGeneric('getBrowserInfo'))
setGeneric('send',                    signature='obj', function(obj, msg) standardGeneric('send'))
setGeneric('browserResponseReady',    signature='obj', function(obj) standardGeneric('browserResponseReady'))
setGeneric('getBrowserResponse',      signature='obj', function(obj) standardGeneric('getBrowserResponse'))
setGeneric('closeWebSocket',          signature='obj', function(obj) standardGeneric('closeWebSocket'))
setGeneric('getBrowserWindowTitle',   signature='obj', function(obj) standardGeneric('getBrowserWindowTitle'))
setGeneric('setBrowserWindowTitle',   signature='obj', function(obj, newTitle, proclaim=FALSE)
                                                                     standardGeneric('setBrowserWindowTitle'))
setGeneric('roundTripTest',           signature='obj', function (obj, ...) standardGeneric('roundTripTest'))
setGeneric('getBrowserWindowSize',    signature='obj', function(obj) standardGeneric('getBrowserWindowSize'))
#----------------------------------------------------------------------------------------------------
setupMessageHandlers <- function()
{
   addRMessageHandler("handleResponse", "handleResponse")

} # setupMessageHandlers
#----------------------------------------------------------------------------------------------------
# constructor: asks your browser to display browserFile, which is presented
# by the minimal http server offered by httpuv, after which websocket messages are
# exchanged.  the default browserFile, viz.html, does not do much, but is copied and
# extended by BrowserViz subclassing applcations
# the optional sixth argument, httpQueryProcessingFunction, provides subclasses with the
# opportunity to execute code on the http server created here.  one case of this is
# the BrowserTable class, which uses http in an ajax-ish way to pass pages of a possibly
# very large data.frame to the browser for incremental display.
#
BrowserViz = function(portRange, host="localhost", title="BrowserViz", quiet=TRUE, browserFile=NA,
                      httpQueryProcessingFunction=NULL)
{
  if(is.na(browserFile))
     browserFile <- browserVizBrowserFile

  wsCon <- new.env(parent=emptyenv())

  result <- .startDaemonizedServerOnFirstAvailableLocalHostPort(portRange, wsCon)
  actualPort <- result$port

  if(is.null(actualPort))
    stop(sprintf("no available ports in range %d:%d", min(portRange), max(portRange)))


  uri = sprintf("http://%s:%s", host, actualPort)

  if(!quiet){
     message(sprintf("BrowserViz constructor starting with html file '%s'", browserFile))
     message(sprintf(" html file exists? %s", file.exists(browserFile)))
     }

  stopifnot(file.exists(browserFile))

  if(!quiet){
     message(sprintf("summoning default browser to get %s", uri))
     }

  browseURL(uri, browser=.getBrowser())

  wsCon <- .setupWebSocketHandlers(wsCon, browserFile, quiet)

  wsCon$wsID <- result$wsID

  if(!quiet)
     message(sprintf("starting daemonized server on port %s", actualPort))

  setupMessageHandlers()

  obj <- .BrowserViz(uri=uri, websocketConnection=wsCon, port=actualPort, quiet=quiet)

  BrowserViz.state[["httpQueryProcessingFunction"]] <- httpQueryProcessingFunction

  #printf("sleeping 2 seconds for browser/httpuv handshake")
  #Sys.sleep(2);  # wait for the browser/httpuv handshake to complete
  #printf("sleep complete")

  totalWait <- 0.0
  maxWaitPermitted <- 10000.0
  sleepTime <- 2

  while (is.null(wsCon$ws)){   # becomes non-null when handshake is established
    totalWait <- totalWait + sleepTime
    stopifnot(totalWait < maxWaitPermitted)
    if(!obj@quiet)
       message(sprintf ("BrowserViz websocket not ready, waiting %6.2f seconds", sleepTime));
    Sys.sleep(sleepTime)
    }

  if(!obj@quiet){
     message(sprintf("BrowserViz websocket ready after %6.2f seconds", totalWait));
     message(sprintf("about to return BrowserViz object"));
     }

  obj

} # BrowserViz: constructor
#----------------------------------------------------------------------------------------------------
.validWebSocketID <- function(candidate)
{
   if(length(grep("not available", candidate)) == 1)
      return (FALSE)

   return (TRUE)

} # .validWebSocketID
#----------------------------------------------------------------------------------------------------
.startDaemonizedServerOnFirstAvailableLocalHostPort <- function(portRange, wsCon)
{
   done <- FALSE

   port <- portRange[1]
   wsID <- NULL

   while(!done){
     if(port > max(portRange))
        done <- TRUE
     else
        wsID <- tryCatch(startDaemonizedServer("127.0.0.1", port, wsCon),
                        error=function(m){sprintf("port not available: %d", port)})
     if(.validWebSocketID(wsID))
        done <- TRUE
     else
        port <- port + 1;
     } # while

   actualPort <- NULL

   if(.validWebSocketID(wsID))
      actualPort <- port

   list(wsID=wsID, port=actualPort)

} # .startDaemonizedServerOnFirstAvailableLocalHostPort
#----------------------------------------------------------------------------------------------------
setMethod('show', 'BrowserVizClass',

  function (obj) {
     msg <- sprintf("BrowserViz object");
     cat(msg, '\n', sep='')
     msg <- sprintf("ready? %s", ready(obj))
     cat(msg, '\n', sep='')
     msg <- sprintf("port: %d", port(obj))
     cat(msg, '\n', sep='')
     }) # show

#----------------------------------------------------------------------------------------------------
setMethod('port', 'BrowserVizClass',

  function (obj) {
     obj@port
     })

#----------------------------------------------------------------------------------------------------
setMethod('closeWebSocket', 'BrowserVizClass',

  function (obj) {
     if(!obj@websocketConnection$open){
        warning("websocket server is not open, cannot close");
        return()
        }
     obj@websocketConnection$open <- FALSE
     stopDaemonizedServer(obj@websocketConnection$wsID)
     obj@websocketConnection$ws <- NULL
     obj@websocketConnection$ws <- -1

     invisible(obj)
     })

#----------------------------------------------------------------------------------------------------
# test initial variable setup, then send an actual message, and await the reply

setMethod('ready', 'BrowserVizClass',

  function (obj) {

     sleepIntervalCount <- 0
     sleepInterval <- 0.1

     if(!is.environment(obj@websocketConnection))
        return(FALSE)

     if(!obj@websocketConnection$open)
        return(FALSE)

     send(obj, list(cmd="ready", callback="handleResponse", status="request", payload=""))

     while (!browserResponseReady(obj)){
        if(!obj@quiet) printf("waiting in BrowserViz.ready, browserResponseReady not yet true");
        Sys.sleep(sleepInterval)
        sleepIntervalCount <- sleepIntervalCount + 1
        }

     if(!obj@quiet) printf("browserResponseReady now true, after %d sleepInterval/s of %f",
                           sleepIntervalCount, sleepInterval);
     getBrowserResponse(obj);
     return(TRUE);
     })

#----------------------------------------------------------------------------------------------------
setMethod('browserResponseReady', 'BrowserVizClass',

  function (obj) {
    return(!is.null(status$result))
    })

#----------------------------------------------------------------------------------------------------
setMethod('getBrowserResponse', 'BrowserVizClass',

  function (obj) {
    if(!obj@quiet){
       message(sprintf("BrowserViz getBrowserResponse, length %d", length(status$result)))
       }
    return(status$result)
    })

#----------------------------------------------------------------------------------------------------
.setupWebSocketHandlers <- function(wsCon, browserFile, quiet)
{
   if(!quiet){
      printf("--- entering BrowserViz .setupWebSocketHandlers");
      printf("    browserFile: %s", browserFile);
      }

   wsCon$open <- FALSE
   wsCon$ws <- NULL
   wsCon$result <- NULL
     # process http requests
   wsCon$call = function(req) {
      qs <- req$QUERY_STRING
      if(nchar(qs) > 0){
         if(!quiet) print("--- bv$call, about to call dynamically assigned queryProcessor");
         fields <- ls(req)
         for(field in fields){
            #printf("---- request field: %s", field)
            #print(req[[field]]);
            }
         queryProcessorFunction <- BrowserViz.state[["httpQueryProcessingFunction"]]
         if(!is.null(queryProcessorFunction))
           body <- queryProcessorFunction(qs)
         else
           body <- "no query processor registered"
         return(list(status=200L, headers = list('Content-Type' = 'text/html'),
                     body=body))
         } # the request had a query string
      wsUrl = paste(sep='', '"', "ws://",
                   ifelse(is.null(req$HTTP_HOST), req$SERVER_NAME, req$HTTP_HOST),
                   '"')
     list(
       status = 200L,
       headers = list('Content-Type' = 'text/html'),
       body = c(file=browserFile))
       }

      # called whenever a websocket connection is opened
   wsCon$onWSOpen = function(ws) {
      if(!quiet)
         print("BrowserViz..setupWebSocketHandlers, wsCon$onWSOpen");
      wsCon$ws <- ws   # this provides later access (eg wsCon$ws$send) to crucial functions
      ws$onMessage(function(binary, rawMessage) {
         if(!quiet) print("BrowserViz..setupWebSocketHandlers, onMessage ");
         message <- as.list(fromJSON(rawMessage))
         wsCon$lastMessage <- message
         if(!is(message, "list")){
            message("message: new websocket message is not a list");
            return;
            }
         if (! "cmd" %in% names(message)){
            message("error: new websocket message has no 'cmd' field");
            return;
            }
         cmd <- message$cmd
         if(!quiet) printf("BrowserViz dispatching on msg$cmd: %s", message$cmd);
         dispatchMessage(ws, message, quiet);
         }) # onMessage
       wsCon$open <- TRUE
       } # onWSOpen

   wsCon

} # .setupWebSocketHandlers
#--------------------------------------------------------------------------------
addRMessageHandler <- function(key, functionName)
{
   dispatchMap[[key]] <- functionName

} # addRMessageHandler
#---------------------------------------------------------------------------------------------------
dispatchMessage <- function(ws, msg, quiet)
{
   if(!msg$cmd %in% ls(dispatchMap)){
       message(sprintf("dispatchMessage error!  the incoming cmd '%s' is not recognized", msg$cmd))
       return()
       }

   function.name <- dispatchMap[[msg$cmd]]
   success <- TRUE

   if(is.null(function.name)){
       message(sprintf("dispatchMessage error!  cmd ('%s') not recognized", msg$cmd))
       success <- FALSE
       return()
       }

   tryCatch(func <- get(function.name), error=function(m) func <<- NULL)

   if(is.null(func)){
       message(sprintf("dispatchMessage error!  cmd ('%s') recognized but no corresponding function",
              msg$cmd))
       success <- FALSE
       }

   if(success){
      if(!quiet) printf("BrowserViz.dispatchMessage calling function '%s'", function.name);
      do.call(func, list(ws, msg))
      }

} # dispatchMessage
#---------------------------------------------------------------------------------------------------
setMethod('send', 'BrowserVizClass',

    function(obj, msg) {
      status$result <- NULL
      #printf("bv.send, nchar(str(msg)): %d", nchar(str(msg)));
      #printf("bv.send, nchar(msg$payload): %d", nchar(msg$payload))
      msg.json <- toJSON(msg)
      #printf("bv.send, nchar(msg.json): %d", nchar(str(msg.json)))
      obj@websocketConnection$ws$send(toJSON(msg))
      #printf("obj@websocketConnection$ws$send(toJSON(msg)) complete");
      })

#--------------------------------------------------------------------------------
setMethod('getBrowserInfo', 'BrowserVizClass',

  function (obj) {
     send(obj, list(cmd="getBrowserInfo", callback="handleResponse", status="request", payload=""))
     while (!browserResponseReady(obj)){
        Sys.sleep(.1)
        }
     getBrowserResponse(obj);
     })

#--------------------------------------------------------------------------------
setMethod('roundTripTest', 'BrowserVizClass',

  function (obj, ...) {
     payload <- toJSON(...)
     send(obj, list(cmd="roundTripTest", callback="handleResponse", status="request", payload=payload))
     while (!browserResponseReady(obj)){
        Sys.sleep(.1)
        }
     getBrowserResponse(obj);
     })

#----------------------------------------------------------------------------------------------------
setMethod('getBrowserWindowTitle', 'BrowserVizClass',

  function (obj) {
     send(obj, list(cmd="getWindowTitle", callback="handleResponse", status="request", payload=""))
     while (!browserResponseReady(obj)){
        Sys.sleep(.1)
        }
     getBrowserResponse(obj);
     })

#----------------------------------------------------------------------------------------------------
setMethod('setBrowserWindowTitle', 'BrowserVizClass',

  function (obj, newTitle, proclaim=FALSE) {
     payload = list(title=newTitle, proclaim=proclaim)
     send(obj, list(cmd="setWindowTitle", callback="handleResponse", status="request",
                    payload=payload))
     while (!browserResponseReady(obj)){
        Sys.sleep(.1)
        }
     getBrowserResponse(obj)
     })

#----------------------------------------------------------------------------------------------------
setMethod('getBrowserWindowSize', 'BrowserVizClass',

  function (obj) {
     send(obj, list(cmd="getWindowSize", callback="handleResponse", status="request", payload=""))
     while (!browserResponseReady(obj)){
        Sys.sleep(.1)
        }
     as.list(fromJSON(getBrowserResponse(obj)))
     })

#----------------------------------------------------------------------------------------------------
handleResponse <- function(ws, msg)
{
   if(msg$status == "success")
      status$result <- msg$payload
   else{
     message(msg$payload)
     status$result <- NA
     }

   NULL

} # handleResponse
#----------------------------------------------------------------------------------------------------
#.processQuery <- function(queryString)
#{
#
#  list(status=200L, headers = list('Content-Type' = 'text/html'),
#       body="hello from bv.processQuery, dynamically assigned")
#
#} # .processQuery
##----------------------------------------------------------------------------------------------------
#queryProcessor <- .processQuery


