setwd("C:/Users/USER/Desktop/투빅스/2주차 과제")
board <- read.csv("board.csv",F)
board <- as.matrix(board)

move_x = c(2,-2,2,-2,1,-1,1,-1)
move_y = c(1,1,-1,-1,2,2,-2,-2)

in_chess = function(a,b){
  if(a>=0 && a<100 && b>=0 && b<100){return ("true")}
  else{return ("false")}
}

min_knight = function(sx,sy,ex,ey){

  count = 0
  start = c(sx,sy)
  queue = list(start)
  
  while(length(queue)!=0){
    xp = queue[[1]][1]
    yp = queue[[1]][2]
    xp = as.numeric(xp)
    yp = as.numeric(yp)
    queue = queue[-1]
    
    if(xp == ex && yp == ey){
      return(count)
      break;
    }
    
    for(i in 0:8){
      if(in_chess(xp+move_x[i],yp+move_y[i])=="false"){
        return("error")
      }
      if(board[xp+move_x[i]][yp+move_y[i]]){
        return("error")
      }
      board[xp+move_x[i]][yp+move_y[i]] = board[xp][yp] +1
        
      queue = c(queue,list(xp+dx[i],yp+dy[i]))
      count = count+1
    }
  }
}

min_knight(24,55,1,1) 
