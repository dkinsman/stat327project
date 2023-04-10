####################### Given board, check if there's a winner #################
checkWin = function (board, n){
  winner = NA
  constant = n * (n^2 + 1) / 2
    
  player1 = which(board==1)
  player2 = which(board==-1)
  #cat('P1:', player1, ' P2:', player2, '\n')
  
  # have to check columns, rows, and diagonals for a win,
  # we'll use a magic square, the indices should sum to 15!
  if (length(player1) >= n){
  p1_moves <- combinations(length(player1), n, player1)
  for (i in 1:nrow(p1_moves)){
    if(sum(p1_moves[i,])== constant){
      winner = 1
      return(winner)
    }
  }}
  
  if (length(player2) >=n){
  p2_moves <- combinations(length(player2), n, player2)
  #print(p2_moves)
  for (i in 1:nrow(p2_moves)){
    if(sum(p2_moves[i,])==constant){
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

########################### PLAY 3x3 RANDOM GAME ###############################

randtictactoe = function(dim, board = list(NA), print_info = F){
  if (length(which(!is.na(board))) > 0) board = rep(NA, dim^2)
  player = 1
  while (is.na(checkWin(board, dim))){
    #print(player)
    board = randomPlay(board, player)
    player = -1 * player
    
    player1 = which(board==1)
    player2 = which(board==-1)
    if (print_info == T) cat('P1:', player1, ' P2:', player2, '\n')
  }
  
  winner = checkWin(board, dim)
  
  if(print_info==T){
  print(ifelse(winner ==1, 'The winner is player 1', 
               ifelse(winner == -1, 'The winner player 2', 
                      'The game ends with a draw')))}
  
  return(winner)
}

#######################


