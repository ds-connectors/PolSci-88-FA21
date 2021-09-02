# Function to determine who moves given an initial state and 
# whos turn it is
# b_in is the bonus for being next to an ingroup
# b_out is the penalty for being next to an outgroup
# the b_home creates a tiebreaker so the household doesn't 
# move if nothing improves the situation
get_next <- function(init, turn, b_in=1, b_out=-1, b_home = .01){
  # If the household who's turn it is is empty, 
  # nothing changes and on to the next household
  if (substr(init,turn,turn)=="_"){
    return(c(init, init, "empty house", turn, turn))
  }
  # figuring out what houses are available to move to
  blank.vec <- gregexpr("_", init)[[1]]
  avail <- c(turn, (1:nchar(init))[blank.vec])
  # Set the utility to any non-available house to -Infinity
  u.avail <- rep(-Inf, nchar(init))
  # Set the utility to the current house to b_home
  # And others to 0
  u.avail[avail] <- 0
  u.avail[turn] <- b_home
  # Computing the utility for all available houses
  for (i in avail){
    # test is what the arrangement would be if moving (or staying)
    test <- init
    substr(test, turn, turn) <- substr(init, i,i)
    substr(test, i, i) <- substr(init, turn,turn)
    # Adding b_in if there is an ingroup person to the left... 
    if (substr(init, turn, turn) == substr(test, i-1, i-1)){
      u.avail[i]<-u.avail[i]+ b_in
    } 
    #...or the right
    if (substr(init, turn, turn) == substr(test, i+1, i+1)){
      u.avail[i]<-u.avail[i]+ b_in
    } 
    # Subtracting if there is an outgroup person to the left
    # Note we also need to check that it isn't an empty house or the 
    # "end" of the neighborhood (e.g., substr(init,0,0) returns "")
    if (substr(init, turn, turn) != substr(test, i-1, i-1) &&
        !(substr(test, i-1, i-1) %in% c("_",""))){
      u.avail[i]<-u.avail[i]+ b_out
    } 
    # Same for the right
    if (substr(init, turn, turn) != substr(test, i+1, i+1)&&
        !(substr(test, i+1, i+1) %in% c("_",""))){
      u.avail[i]<-u.avail[i]+ b_out
    } 
  }
  # Figuring out where to move to.
  # Note the which.max by default picks the lowest index
  # When there is a tie, as the algorithm dictates
  move <- which.max(u.avail)
  # Creating the new arrangement
  out <- init
  substr(out, turn, turn) <- substr(init, move,move)
  substr(out, move, move) <- substr(init, turn,turn)
  # Creating the output when there is a move
  if (turn != move) return(c(init, out, paste("the ", 
                                              substr(init, turn, turn), 
                                              "in slot ",
                                              turn,
                                              "moves to",
                                              move), turn, move))
  # Output when there is no move
  return(c(init,out,paste("the ",substr(init,turn,turn),
                          "in slot ",
                          turn, "stays"),turn,move))
}

# Function to run the algorithm for up to max_steps turns
run_schelling <- function(init, b_in=1, b_out=-1, b_home=.01, 
                          max_steps=100){
  # Creating a blank output matrix
  out.mat <- matrix(NA, nrow=max_steps, ncol=5)
  # Initial arrangement. Note the "turn" is set to 0
  out.mat[1,] <- c("initial", init,"Start", 0, 1)
  # Loop to run the algorithm
  for (i in 2:max_steps){
    # Figuring out who is the current turn
    # If there was a move in the previous round, go back
    # to house 1.
    # If not, take the previous term and add 1, using modular
    # Division to make it so after the last house we go back to
    # House 1
    last.turn <- as.numeric(out.mat[i-1,4])
    next.turn <- ifelse(out.mat[i-1,4]!=out.mat[i-1,5],1,
                        1 + last.turn%%nchar(init))
    # Now getting the next arrangment
    out.mat[i,] <-  get_next(out.mat[i-1, 2],
                             next.turn, b_in=b_in, b_out=b_out,
                             b_home=b_home)
    # Stop the algorithm if everyone has had a chance to move
    # Note a simpler check might just be if the household in the
    # Last spot stays, but this checks if everyone has stayed
    if (i > nchar(init)){
      stays <- sum(out.mat[(i-nchar(init)+1):i,4]==out.mat[(i-nchar(init)+1):i,5])
      if (stays==nchar(init)) {return(out.mat[1:i,])}
    }
  }
  return(out.mat)
}

display_schelling <- function(init, b_in=1, b_out=0, b_home=.01, 
                              max_steps=100, shorten=FALSE){
  outmat <- run_schelling(init=init, b_in=b_in, 
                          b_out=b_out, b_home=b_home,
                          max_steps=max_steps)
  out.df <- data.frame(outmat, stringsAsFactors = FALSE)
  out.df <- out.df[,c(4,3,2)]
  names(out.df) <- c("Turn", "Choice","Outcome")
  if (shorten){
    hide <- which(outmat[,4]==outmat[,5])
    out.df <- out.df[-hide,c(2,3)]
  }
  if (is.null(dim(out.df))) return("No Moves")
  return(out.df)
}

end_schelling<-function(init, b_in=1, b_out=0, b_home=.01, 
                              max_steps=100){
    outmat <- run_schelling(init=init, b_in=b_in, 
                          b_out=b_out, b_home=b_home,
                          max_steps=max_steps)
    return(outmat[nrow(outmat),5])
}

seg_meas <- function(map){
  
    indiv_meas <- rep(NA, nchar(map))
  for (i in 1:nchar(map)){
    self <- substr(map,i,i)
    nabe <- NULL
    if (i > 1) {nabe <- c(nabe,substr(map,i-1,i-1))}
    if (i < nchar(map)) {nabe<- c(nabe,substr(map,i+1,i+1))}
    same <- sum(nabe==self)
    diff <- sum(nabe!=self & nabe != "_")
    nonempty <- sum(nabe != "_")
    indiv_meas[i] <- ifelse(self=="_", NA, (same-diff)/nonempty)
  }
  return(mean(indiv_meas, na.rm=TRUE))                                 
}

seg_meas <- Vectorize(seg_meas)

make_randAB<- function(pA=.4, pB=.4, len=10){
    out <- NULL
    for (i in 1:len){
        new <- sample(c("A","B","_"), size=1, prob=c(pA, pB, 1-pA-pB))
        out <- paste(out, new, sep="")
    }
    return(out)
}

end_schelling<-function(init, b_in=1, b_out=0, b_home=.01, 
                              max_steps=100){
    outmat <- run_schelling(init=init, b_in=b_in, 
                          b_out=b_out, b_home=b_home,
                          max_steps=max_steps)
    return(outmat[nrow(outmat),2])
}

seg_change_rand <- function(pA=.4, pB=.4, len=10, b_in=1, b_out=0, b_home=.01, max_steps=100){
    start <- make_randAB()
    end <- end_schelling(start, b_in=b_in, b_out=b_out, b_home=b_home, max_steps=max_steps)
    return(c(seg_meas(start), seg_meas(end)))
}



plot_seg_meas <- function(init, b_in=1, b_out=0, b_home=.01, max_steps=100){
    plot(seg_meas(display_schelling(init, b_in=b_in, b_out=b_out,b_home=b_home,max_steps=max_steps)[,3]), xlab="Time", ylab="Segregation")
}



