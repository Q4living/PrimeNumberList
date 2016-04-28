prime_list_brutal <- function(n){
  start <- Sys.time()
  p <- 0
  i <- 2
  while (i < n){
    if(sum(i/1:i==i%/%1:i)==2) p <- c(p, i) #if i is only can be divided by itself and 1, then sum() will be 2
    i <- i + 1
  }
  #print(unique(p))
  print(paste(format(Sys.time() - start, format = "%s"), " time spent"))
}

prime_list <- function(n){
  start <- Sys.time()
  p <- c(2)
  i <- 2
  while (i < n){
    t <- TRUE
    for (e in p) {
      if (i %% e == 0) {
        t <- FALSE
        break
      }
    }
    if(t) p <- c(p, i)
    i <- i + 1
  }
  print(c(1,p))
  print(paste(format(Sys.time() - start, format = "%s"), " time spent"))
}
