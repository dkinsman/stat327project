checkWin = function (){
  
  player1 = which(self$board==1)
  player2 = which(self$board==-1)
  cat('P1:', player1, '\nP2:', player2)
  # have to check columns, rows, and diagonals for a win,
  # we'll use a magic square, the indices should sum to 15!
  if (length(player1) < 3 & length(player2) <3) return(self$winner)
  
  # UNCOMMENT AFTER DONE TESTING
  p1_moves <- combinations(length(player1), 3, player1)
  for (i in 1:nrow(p1_moves)){
    if(sum(p1_moves[i,])==15){
      self$winner = 1
      return(self$winner)
    }
  }
  
  p2_moves <- combinations(length(player2), 3, player2)
  #print(p2_moves)
  for (i in 1:nrow(p2_moves)){
    if(sum(p2_moves[i,])==15){
      self$winner = -1
      return(self$winner)
    }
  }
  
  #declares a draw
  if (length(which(is.na(self$board)))==0){
    self$winner = 0
    return(self$winner)
  }
}

randomPlay = function() {
  # gets the indices of open boxes on the board
  open_box = which(is.na(self$board$board))
  # chooses a random box according to a uniform dist.
  move = sample(open_box,1)
  
  # player 1 is 1, player 2 is -1
  self$board$board[move] = ifelse(self$player==1, 1, -1)
  invisible(self)
}

