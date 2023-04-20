####################### Given board, check if there's a winner #################
checkWin = function (board){
  winner = NA
  dim = sqrt(length(board))
  
  #cat('P1:', player1, ' P2:', player2, '\n')
  #if(length(is.na(board))==length(board)) return(winner)
  
  # have to check columns, rows, and diagonals for a win
  if (length(!is.na(board))> (2* dim -1)){
  for(i in 1:dim){
    # check row sum for Player 1, could also use the rowSums() function
    if (sum(board[i,], na.rm = T) == dim){
      winner = 1
      return(winner)
    }
    #check column sum for Player 1, could also use the colSums() function
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
  
  #print(board[diags])
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

################################## MCTS ########################################
findUCB = function(node){
  parent = node[[1]]$parent[[1]]
  nodeWins = node[[1]]$wins
  nodeVisits = node[[1]]$visits
  cat('Parent visits: ', parent$visits, '\nNode visits: ', 
      nodeVisits, '\nNode wins: ', nodeWins,'\n')
  if(node[[1]]$visits == 0) return(.Machine$integer.max)
  else{
    parent = node[[1]]$parent[[1]]
    nodeWins = node[[1]]$wins
    nodeVisits = node[[1]]$visits
    cat('Parent visits: ', parent$visits, '\nNode visits: ', 
        nodeVisits, '\nNode wins: ', nodeWins,'\n')
    ucb = nodeWins / nodeVisits + sqrt(2) * 
      sqrt(log(parent$visits)/nodeVisits)
    print(ucb)
    return(ucb)
  }
}

selectNode = function(root){
  node = root
  
  while(length(node[[1]]$children)!=0){
    index <- which.max(findUCB(node[[1]]$children))
    node = node[[1]]$children[index]
    node[[1]]$visits = node[[1]]$visits +1
  }
  
  return(node)
}

expandNode <- function(parent){
  open_moves = which(is.na(parent[[1]]$board))
  player = -1*parent[[1]]$player
  
  for (i in open_moves){
    board = parent[[1]]$board
    board[i] = player
    #print(board)
    newNode = list(list(board = board, player = player, 
                 visits = 0, wins = 0, parent = parent,
                 children = c()))
    #print(newNode[[1]]$board)
    parent[[1]]$children = append(parent[[1]]$children, newNode)
  }
  random_move = sample(open_moves, 1)
  return(parent[[1]]$children[random_move])
  # parent[[1]]$visits = parent[[1]]$visits + 1
  # print(parent[[1]]$visits)
  #return(parent)
}

selectRandomChild <- function(node){
  n <- length(node[[1]]$children)
  rand <- sample(1:n, 1)
  node = node[[1]]$children[rand]
  node[[1]]$visits = node[[1]]$visits +1
  return(node)
}

playGame <- function(node) {
  game <- randtictactoe(3, node[[1]]$board, node[[1]]$player)
  return(game)
}

backpropogate <-function(node, winner) {
  
  repeat{
    node[[1]]$visits = node[[1]]$visits + 1
    if(winner == node[[1]]$player) {
      node[[1]]$wins = node[[1]]$wins + 1
    }
    if(is.null(node[[1]]$parent)){
      node[[1]]$visits = node[[1]]$visits + 1
      print(node[[1]]$visits)
      if(winner == node[[1]]$player) {
        node[[1]]$wins = node[[1]]$wins + 1
      }
      node[[1]]$children = root[[1]]$children
      return(node)
    }
    node = node[[1]]$parent
  }
}
  
MCTSplay = function(board = array(rep(NA, 3^2), dim = c(3, 3)), 
                    player = 1, iterations){
  # create a root
  root <- list(list(board = board, player = player, parent = NULL,
               visits = 0, wins = 0, children = c()))
  #print(root[[1]]$board)
  
  # repeat the simulations for specified iterations
  for (i in 1:iterations){
    #select node (root) to expand, we select the child with the highest UCB value
    cat('\nIteration: ', i, '\n')
    node <- selectNode(root)
    #expand the node if terminal
    #if (is.na(checkWin(node[[1]]$board)))
    exploreNode <- expandNode(node)
    
    #cat(length(exploreNode[[1]]$children), '=', length(node[[1]]$children), '\n')
    #print(length(exploreNode[[1]]$children))
    #if(length(exploreNode[[1]]$children) > 0) exploreNode <- selectRandomChild(node)
    winner <- playGame(exploreNode)
    
    root <- backpropogate(exploreNode, winner)
    #cat('\nRoot visits: ', root[[1]]$visits, '\n')
  }
  
  cat(class(root[[1]]$visits), root[[1]]$visits)
  index <- which.max(findUCB(root[[1]]$children))
  winnerNode <- root[[1]]$children[index] 
  root = winnerNode
  #cat(root[[1]]$board, '\n')
  return(root)
} 
########################### PLAY A RANDOM GAME ###############################

randtictactoe = function(dim, board = NA,  player = NA, print_info = F){
  if (length(which(is.na(board)))==1) board = array(rep(NA, dim^2), 
                                                      dim = c(dim, dim))
  #print(board)
  if (is.na(player)) player = 1
  while (is.na(checkWin(board))){
    #print(player)
    board = randomPlay(board, player)
    player = -1 * player
    
    player1 = which(board==1)
    player2 = which(board==-1)
    if (print_info == T) cat('P1:', player1, ' P2:', player2, '\n')
    #print(board)
    #cat('\n', is.na(checkWin(board)))
  }
  
  winner = checkWin(board)
  
  if(print_info==T){
  print(ifelse(winner ==1, 'The winner is player 1', 
               ifelse(winner == -1, 'The winner player 2', 
                      'The game ends with a draw')))}
  
  return(winner)
}

####################### 


