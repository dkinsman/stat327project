library(R6)
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
node <- R6Class("node", 
                public = list(parent = NULL, 
                              player = NULL,
                              children = rep(NULL,8), 
                              visits = 0,
                              wins = 0,
                              board = NULL,
                              # initialize = function(board, player){
                              #   self$board = board
                              #   self$player = player
                              #   invisible(self)
                              # },
                              getRandomChild = function(){
                               # print('get random child')
                                n = length(self$children)
                                pick = sample(1:n, 1)
                                return(self$children[[pick]])
                              },
                              add_child = function(board, player){
                                #print('add child')
                                child = node$new()
                                child$board = board
                                child$player = player
                                child$parent = self
                                self$children = append(self$children, child)
                                #self$children[[min(which(is.null(self$children)))]] = child
                              }
                ))
findUCB = function(node){
  #print('UCB')
  if(node$visits == 0) return(1e6)
  else{
    parent = node$parent
    nodeWins = node$wins
    nodeVisits = node$visits
    # cat('Parent visits: ', parent$visits, '\nNode visits: ', 
    #     nodeVisits, '\nNode wins: ', nodeWins,'\n')
    ucb = nodeWins / nodeVisits + sqrt(2) * 
      sqrt(log(parent$visits)/nodeVisits)
    #cat('\nUCB', ucb, '\n')
    return(ucb)
  }
}

selectNode = function(root){
 # print('Select Node')
  node = root
  while(length(node$children)!=0){
    ucb = c()
    #print((length(node$children)))
    for (i in 1:length(node$children)){
      kid = node$children[[i]]
      ucb = append(ucb, findUCB(kid))
    }
    #print(ucb)
    index = which(ucb==max(ucb))
    if (length(which(ucb==max(ucb)))>1) index <- sample(which(ucb==max(ucb)), 1)
    #print(length(node$children))
    node = node$children[[index]]
  }
  # repeat{
  #   node[[1]]$visits = node[[1]]$visits +1
  #   #node[[1]]$parent[[1]]$visits = node[[1]]$parent[[1]]$visits +1
  #   if (is.null(node[[1]]$parent)) break
  #   node = node[[1]]$parent
  # }
  # for (i in indices){
  #   print(i)
  #   #node = node[[1]]$children[i]
  # }
  
  #cat('\nNode visits (select):', node[[1]]$visits)
  return(node)
}

expandNode <- function(parent){
  #print('expand')
  open_moves = which(is.na(parent$board))
  player = -1*parent$player

  for (i in open_moves){
    #print(i)
    board = parent$board
    board[i] = player
    #print(board)
    #print(newNode[[1]]$board)

    parent$add_child(board =board, player =player)
  }
  
  random_move = sample(1:length(parent$children), 1)
  #print(random_move)
  
  return(parent$children[[random_move]])
  # return(parent)
  # parent[[1]]$visits = parent[[1]]$visits + 1
  # print(parent[[1]]$visits)
  #return(parent)
}

# selectRandomChild <- function(node){
#   print('select random child')
#   n <- length(node$children)/ 6
#   rand <- sample(1:n, 1)
#   node = node$children[(rand*6+1):(rand*6+6)]
#   #node[[1]]$visits = node[[1]]$visits +1
#   return(node)
# }

playGame <- function(node) {
  #print('play game')
  game <- randtictactoe(3, node$board, node$player)
  return(game)
}

backpropogate <-function(node, winner) {
  #print('backpropogate')
  #paste0('Node visits:', node[[1]]$visits)
  
  # repeat{
  #   node$visits = node$visits + 1
  #   node$parent$visits = node$parent$visits +1
  #   print(node$player)
  #   if(winner == node$player) {
  #     node$wins = node$wins + 1
  #     node$parent$wins = node$parent$wins +1
  #   }
  #   cat('parent', length(node$parent))
  #   if(is.null(node$parent)) break
  #   node = node$parent
  # }
  #   paste0('Node visits:', node[[1]]$visits)
  #   #print(length(node[[1]]$parent))
  #
  
  #
  
  # node = node[[1]]$parent
  # #}
  node$visits = node$visits + 1
  if(winner == node$player) {
    node$wins = node$wins + 1
  }
  if(!is.null(node$parent)){
      #node[[1]]$children = root[[1]]$children
      #paste0('Node visits:', node[[1]]$visits)
    return(backpropogate(node$parent, winner))
  }
  return(node)
}


MCTSplay = function(board = array(rep(NA, 3^2), dim = c(3, 3)), 
                    player = 1, iterations){
  # create a root
  root <- node$new()
  root$board = board
  root$player = player
  #print(root$board)
  
  # repeat the simulations for specified iterations
  for (i in 1:iterations){
    #cat('\nIteration: ', i, '\n')
    #select node (root) to expand, we select the child with the highest UCB value
    node <- selectNode(root)
    #expand the node if terminal
    exploreNode <- node
    if (is.na(checkWin(node$board))) exploreNode = expandNode(node)
    
    if(length(exploreNode$children)> 0) exploreNode <- explorenode$getRandomChild()
    winner <- playGame(exploreNode)
    root <- backpropogate(exploreNode, winner)
    #cat('\nChildren: ',length(root$children), '\n')
    #print(exploreNode$board)
  }
  
  if(!is.na(checkWin(root$board))) return (root$board)
  ucb = c()
  #cat('\nChildren: ',length(root$children), '\n')
  for (i in 1:length(root$children)){
    kid = root$children[[i]]
    ucb = append(ucb, findUCB(kid))
  }
    #print(ucb)
  index = which(ucb==max(ucb))
  if (length(which(ucb==max(ucb)))>1) index <- sample(which(ucb==max(ucb)), 1)
  winnerNode = root$children[[index]]
  root = winnerNode
  
  # cat('\nNode visits: ', root$visits, '\nNode Wins: ', root$wins, '\n')
  # cat('UCB:', ucb, '\nMaxUCB: ', max(ucb), '\nNode UCB:', findUCB(root),'\n')
  # cat('\nNode visits: ', root$visits, '\nNode wins: ', root$wins, '\n')
  #print(root$board)
  return(root$board)
} 

MCTSvsRandom = function(iterations =500, dim = 3, board = NA, 
                        player = -1, print_info = F){
  if (length(which(is.na(board)))==1) board = array(rep(NA, dim^2), 
                                                    dim = c(dim, dim))
  while(is.na(checkWin(board))){
    board = MCTSplay(board, player = player, iterations = iterations)
    if (print_info == T) print(board)
    if(!is.na(checkWin(board))) return(checkWin(board))
    board = randomPlay(board, player)
    if (print_info == T) print(board)
  }
  if (print_info == T) print(board)
  return(checkWin(board))
}

RandomvsMCTS = function(iterations =500, dim = 3, board = NA, 
                        player = -1, print_info = F){
  if (length(which(is.na(board)))==1) board = array(rep(NA, dim^2), 
                                                    dim = c(dim, dim))
  while(is.na(checkWin(board))){
    board = randomPlay(board, player)
    if (print_info == T) print(board)
    board = MCTSplay(board, player = player, iterations = iterations)
    if (print_info == T) print(board)
  }
  if (print_info == T) print(board)
  return(checkWin(board))
}

MCTSvsMCTS = function(iterations =500, dim = 3, board = NA, 
                      player = -1, print_info = F){
  if (length(which(is.na(board)))==1) board = array(rep(NA, dim^2), 
                                                    dim = c(dim, dim))
  while(is.na(checkWin(board))){
    board = MCTSplay(board, player = player, iterations = iterations)
    if (print_info == T) print(board)
    if(!is.na(checkWin(board))) return(checkWin(board))
    player = -1*player
    board = MCTSplay(board, player, iterations = iterations)
    if (print_info == T) print(board)
    if(!is.na(checkWin(board))) return(checkWin(board))
    player = -1*player
    
  }
  if (print_info == T) print(board)
  return(checkWin(board))
}
########################### PLAY A RANDOM GAME ###############################

randtictactoe = function(dim, board = NA,  player = 1, print_info = F){
  if (length(which(is.na(board)))==1) board = array(rep(NA, dim^2), 
                                                    dim = c(dim, dim))
  #print(board)
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


