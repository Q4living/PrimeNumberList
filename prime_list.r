prime_list_brutal <- function(n){
  start <- Sys.time()
  p <- 0L #Begin from the beginning
  i <- 2L #Begin at 2
  while (i < n){
    if(sum(i/1:i==i%/%1:i)==2) p <- c(p, i) #if i is only can be divided by itself and 1, then sum() will be 2
    i <- i + 1L
  }
  print(unique(p))
  print(paste(format(Sys.time() - start, format = "%s"), " time spent"))
}

#Eratosthenes Sieve method
prime_list <- function(n){
  start <- Sys.time()
  p <- 2L #Begin the list with 2
  i <- 2L
  while (i < n){
    t <- TRUE
    for (e in p) {
      if (i %% e == 0L) {
        t <- FALSE
        break
      }
    }
    if(t) p <- c(p, i)
    i <- i + 1L
  }
  print(c(1,p)) #Add 1 back to the list
  print(paste(format(Sys.time() - start, format = "%s"), " time spent"))
}

#Optimized mode
sieve <- function(n)
{
  start <- Sys.time()
  n <- as.integer(n)
  if(n > 1e6) stop("n too large")
  primes <- rep(TRUE, n)
  primes[1] <- FALSE
  last <- 2L #optimized for calculation as int rather than num
  for(i in last:floor(sqrt(n))) #If the prime * prime greater the size of n, the loop can stop
  {
    primes[seq.int(2L*last, n, last)] <- FALSE #Eliminate all the numbers divided by the prime in the array size of n
    last <- last + min(which(primes[(last+1):n])) #The next undivided number remains in the array will be a prime and use it for next loop operations
  }
    
  print(format(Sys.time()-start, format="%s"))
  which(primes)

}

sieve(10000)
