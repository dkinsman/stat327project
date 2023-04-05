####################### Given board, check if there's a winner #################
checkWin = function (board){
  winner = NA
  
  player1 = which(board==1)
  player2 = which(board==-1)
  #cat('P1:', player1, ' P2:', player2, '\n')
  # have to check columns, rows, and diagonals for a win,
  # we'll use a magic square, the indices should sum to 15!
  if (length(player1) >= 3){
  p1_moves <- combinations(length(player1), 3, player1)
  for (i in 1:nrow(p1_moves)){
    if(sum(p1_moves[i,])==15){
      winner = 1
      return(winner)
    }
  }}
  
  if (length(player2) >=3){
  p2_moves <- combinations(length(player2), 3, player2)
  #print(p2_moves)
  for (i in 1:nrow(p2_moves)){
    if(sum(p2_moves[i,])==15){
      winner = -1
      return(winner)
    }
  }}
  
  #declares a draw
  if (length(which(is.na(board)))==0){
    winner = 0
    return(winner)
  }
  
  return(winner)
}

################################ MAKE A RANDOM PLAY ############################
randomPlay = function(board, player) {
  # gets the indices of open boxes on the board
  open_box = which(is.na(board))
  cat(open_box, ' ')
  # chooses a random box according to a uniform dist.
  move = sample(open_box,1)
  if (length(open_box)==1) move = open_box
  
  cat(move, '\n')
  # player 1 is 1, player 2 is -1
  board[move] = ifelse(player==1, 1, -1)
  return(board)
}

########################### PLAY GAME ##########################################

tictactoe = function(print_info = F){
  board = rep(NA, 9)
  player = 1
  while (is.na(checkWin(board))){
    #print(player)
    board = randomPlay(board, player)
    player = -1 * player
    
    player1 = which(board==1)
    player2 = which(board==-1)
    cat('P1:', player1, ' P2:', player2, '\n')
  }
  
  winner = checkWin(board)
  
  if(print_info==T){
  print(ifelse(winner ==1, 'The winner is player 1', 
               ifelse(winner == -1, 'The winner player 2', 
                      'The game ends with a draw')))}
  
  return(winner)
}

############################## TEST ############################################
tictactoe(T)
