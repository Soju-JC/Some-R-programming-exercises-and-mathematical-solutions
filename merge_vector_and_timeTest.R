#Soju.JC, 02 May 2020| package microbenchmark is necessary for this.
#########################################################################
#Merge 2 vectors with resize method and without resize, then
#check the time for the execution with microbenchmark.
#########################################################################

#input (examples)
x1 <- 1:10000
x2 <- 1:10000
#########################################################################
#Case when the merged vector is not resized:
not_resized <- function(j, k){ 
  count <- 1
  merged_vec <- rep(c(0,1),c(length(j),length(k)))

  for(i in 1:(length(j)+length(k))){
    if(merged_vec[i] == 0){
      merged_vec[i] <- j[i]
    }else{ 
      merged_vec[i] <- k[i-(i-count)]
      count <- count+1
    }
  }
  return(merged_vec)
}
#########################################################################
#Case when the merged vector is resized each time:
resized <- function(j, k){
  x_size <- length(j)
  
  for(i in 1:length(k)){
    j[x_size+i] <- k[i]  
  }
  return(j)
}
#########################################################################
#If you want just the microbenchmark comparison,
#just put 1 & 2 as comments.

# #1
# cat("The merged vector(not resized method) is:", not_resized(x1,x2))
# #2
# cat("The merged vector(resized method) is:", resized(x1,x2))
#3
microbenchmark::microbenchmark(resized(x1,x2),not_resized(x1,x2),
                                                    times = 1000L)

#########################################################################
#Conclusion:
#   Although most of the times the method of resize a vector is
# slower than the non-resize, in this case half of the vector to be
# resized was already created, so it needed just to resize the other
# half. In the other hand, the non-resize method did need to change
# each single value of the pre-vector created, so it had to do two
# times the numbers of changes  that resize method did. That's why in 
# this case the resize method is faster than the non-resize, although 
# most of the times it wouldnt be true.
#########################################################################