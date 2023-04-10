####################### Given board, check if there's a winner #################
checkWin = function (board, n){
  winner = NA
  dim = sqrt(length(board))
  
  #cat('P1:', player1, ' P2:', player2, '\n')
  
  # have to check columns, rows, and diagonals for a win,
  # we'll use a magic square, the indices should sum to 15!
  if (length(!is.na(board))> (2* dim -1)){
  for(i in 1:dim){
    # check row sum for Player 1
    if (sum(board[i,], na.rm = T) == dim){
      winner = 1
      return(winner)
    }
    #check column sum for Player 1
    if(sum(board[,i], na.rm = T) == dim){
      winner = 1 
      return(winner)
    }
    if (sum(board[i,],na.rm = T) == -dim){
      winner = -1
      return(winner)
    }
    
    #check column sum for Player 1
    if(sum(board[,i], na.rm = T) == -dim){
      winner = -1 
      return(winner)
    }
  }
  }

  diag_count = dim
  antidiag_count = 1
  diags <- c(diag_count)
  antidiags <- c(antidiag_count)
  
  for (i in 2:dim){
    diag_count = diag_count + dim - 1
    antidiag_count = antidiag_count + dim + 1
    
    diags <- append(diags, diag_count)
    antidiags <- append(antidiags, antidiag_count)
  }
  
  
  if (sum(board[diags], na.rm = T)==dim){
    winner = 1
    return(winner)
  }
  
  if(sum(board[diags], na.rm = T)==-dim){
    winner = -1
    return(winner)
  }
  
  if (sum(board[antidiags], na.rm = T)==dim){
    winner = 1
    return(winner)
  }
  
  if(sum(board[antidiags], na.rm = T)==-dim){
    winner = -1
    return(winner)
  }
  
  
  # declare a draw
  if(length(which(is.na(board)))==0) winner = 0
  
  return(winner)
}

################################ MAKE A RANDOM PLAY ############################
randomPlay = function(board, player) {
  # gets the indices of open boxes on the board
  open_box = which(is.na(board))
  #cat(open_box, ' ')
  # chooses a random box according to a uniform dist.
  move = sample(open_box,1)
  if (length(open_box)==1) move = open_box
  
  #cat(move, '\n')
  # player 1 is 1, player 2 is -1
  board[move] = ifelse(player==1, 1, -1)
  return(board)
}

############################### MINIMAX ########################################
# NOT FINISHED
evaluate_position <-function(board){ 
  winner = checkWin(board)
  
  if(winner == -1) return(-1000)
  else if (winner == 1) return(1000)
  else if(winner == 0) return(0)
  
  open_moves <- which(is.na(board))
  p1moves <- which(board == 1)
  p2moves <- which(board == -1)
  corners = c(8, 6, 4, 2)
  center = 5
  
  p1_score <- length(intersect(p1moves, corners)) * 10 + 
    length(intersect(p1moves, center)) * 5
  p2_score <- length(intersect(p2moves, corners)) * 10 + 
    length(intersect(p2moves, center)) * 5
  
  return(p1_score - p2_score)
}

########################### PLAY A RANDOM GAME ###############################

randtictactoe = function(dim, board = NA,  print_info = F){
  if (length(which(is.na(board)))==1) board = array(rep(NA, dim^2), 
                                                      dim = c(dim, dim))
  #print(board)
  player = 1
  while (is.na(checkWin(board, dim))){
    #print(player)
    board = randomPlay(board, player)
    player = -1 * player
    
    player1 = which(board==1)
    player2 = which(board==-1)
    if (print_info == T) cat('P1:', player1, ' P2:', player2, '\n')
    #print(board)
  }
  
  winner = checkWin(board, dim)
  
  if(print_info==T){
  print(ifelse(winner ==1, 'The winner is player 1', 
               ifelse(winner == -1, 'The winner player 2', 
                      'The game ends with a draw')))}
  
  return(winner)
}

####################### 


