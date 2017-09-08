?seq.along
dumbDM=function(roads,car,packages){
  car$nextMove=sample(c(2,4,6,8),1)
  return (car)
}

# A priority queue which allows to insert elements
# and order them by priority
# Source: http://rosettacode.org/wiki/Priority_queue#R
PriorityQueue <- function() {
  queueKeys <<- queueValues <<- NULL
  insert <- function(key, value) {
    # If node already exists on queue, and this new addition is better,
    # delete previous one and insert this new one instead
    index = getValueIndex(value)
    if(length(index) > 0) {
      if(isTRUE(key <= queueKeys[[index]])) {
        queueKeys <<- queueKeys[-index]
        queueValues <<- queueValues[-index]
      } else {
        # Ignore it, we already have a cheaper path
        return (NULL)
      }
    }

    # Insert new value in queue
    temp <- c(queueKeys, key)
    ord <- order(temp)
    queueKeys <<- temp[ord]
    queueValues <<- c(queueValues, list(value))[ord]
  }
  pop <- function() {
    head <- queueValues[[1]]
    queueValues <<- queueValues[-1]
    queueKeys <<- queueKeys[-1]
    return (head)
  }
  empty <- function() length(queueKeys) == 0
  getValueIndex <- function(value) which(queueValues %in% list(value) == TRUE)
  list(insert = insert, pop = pop, empty = empty)
}

# A simple lists which allows to insert elements on it
# and verity if a particular element exists or not
List <- function() {
  listValues <<- NULL
  insert <- function(value) listValues <<- c(listValues, list(value))
  exists <- function(value) isTRUE(which(listValues %in% list(value) == TRUE) > 0)
  getAllValues <- function() listValues
  list(insert = insert, exists = exists, getAllValues = getAllValues)
}

# Returns the Manhattan distance between two locations
getManhattanDistance=function(from, to) {
  return (abs(from[1] - to[1]) + abs(from[2] - to[2]))
}

# Return the Euclidean distance between two locations
getEuclideanDistance=function(from, to) {
  return (sqrt((from[1] - to[1])^2 + (from[2] - to[2])^2))
}

# Return the cost of a vertical edge
getVerticalEdgeCost=function(roads, car, neighbor) {
  if(car$y < neighbor[2]) {
    # Moving up
    return (roads$vroads[car$y, car$x])
  } else {
    # Moving down
    return (roads$vroads[neighbor[2], neighbor[1]])
  }
}

# Return the cost of a horizontal edge
getHorizontalEdgeCost=function(roads, car, neighbor) {
  if(car$x > neighbor[1]) {
    # Moving left
    return (roads$hroads[neighbor[2], neighbor[1]])
  } else {
    # Moving right
    return (roads$hroads[car$y, car$x])
  }
}

# Calculate edge cost (from current position to neighbor position)
getEdgeCost=function(roads, car, neighbor) {
  isMovingVertically = car$x == neighbor[1]
  if(isMovingVertically) {
    return (getVerticalEdgeCost(roads, car, neighbor))
  } else {
    return (getHorizontalEdgeCost(roads, car, neighbor))
  }
}

# Return the cost of an edge + a heuristic
getCombinedCost=function(roads, car, neighbor, goal) {
  return (getEdgeCost(roads, car, neighbor) + getManhattanDistance(neighbor, goal))
}

# Return all available neighbors given a location
getNeighbors=function(x, y, xSize, ySize) {
  neighbors = matrix(, nrow = 4, ncol=2, byrow = TRUE)
  # Add all possible horizontal and vertical neighbors
  neighbors[,1] = c(x - 1, x, x, x + 1)
  neighbors[,2] = c(y, y + 1, y -1, y)

  # Remove all lower bound positions (< 0)
  neighbors = neighbors[neighbors[,1] > 0,]
  neighbors = neighbors[neighbors[,2] > 0,]

  # Remove all upper bound positions too (< size of matrix)
  neighbors = neighbors[neighbors[,1] < xSize+1,]
  neighbors = neighbors[neighbors[,2] < ySize+1,]

  return (neighbors)
}

# Return true if node is goal, false otherwise
isGoal=function(neighbor, goal) {
  return (goal[1] == neighbor[1] && goal[2] == neighbor[2])
}

# Perform A* search from current car location towards goal
aStarSearch=function(goal, roads, car, packages) {
  # Get the matrix size
  xSize = dim(roads$hroads)[1]
  ySize = dim(roads$vroads)[2]

  # Initialize visited and frontier lists
  visited = List()
  frontier = PriorityQueue()

  # Put the starting location on the frontier (cost 0 is fine)
  frontier$insert(0, c(car$x, car$y))

  while (!frontier$empty()) {
    # Get node with the least f on the frontier
    node = frontier$pop()
    neighbors = getNeighbors(node[1], node[2], xSize, ySize)

    for (i in 1:dim(neighbors)[1]) {
      neighbor = neighbors[i,]
      # Only search neighbors which hasn't already being visited
      if(visited$exists(neighbor)) {
        next
      }

      if(isGoal(neighbor, goal)) {
        # Return the visited path + current node as path to goal
        return (c(visited$getAllValues(), list(node), list(goal[1:2])))
      } else {
        # Add neighbor to frontier
        combinedCost = getCombinedCost(roads, car, neighbor, goal)
        frontier$insert(combinedCost, neighbor)
      }
    }

    # Keep track of best path
    visited$insert(node)
  }
}

# Given the list of visited nodes, return the best next move car can make towards goal
generateNextMove=function(visited) {
  currX = visited[[1]][1]
  currY = visited[[1]][2]
  nextX = visited[[2]][1]
  nextY = visited[[2]][2]

  # Move is horizontal
  if (isTRUE(nextX > currX)) {
    return (6) # Right
  }
  if (isTRUE(nextX < currX)) {
    return (4) # Left
  }

  # Move is vertical
  if (isTRUE(nextY > currY)) {
    return (8) # Up
  }
  if (isTRUE(nextY < currY)) {
    return (2) # Down
  }

  print('-------ERROR-------')
  browser()
}

# Return a package pickup location which will be used as the goal for a particular search
getGoalPackage=function(roads, car, packages) {
  # Select package with the least amount of moves as next pickup goal
  costs = NULL
  unpicked = packages[which(packages[,5] %in% c(0) == TRUE),]
  if (isTRUE(length(unpicked) == 5)) {
    # There's only 1 unpicked package left, go for it
    return (unpicked)
  } else {
    for(i in 1:dim(unpicked)[1]) {
      goal = unpicked[i,]
      visited = aStarSearch(goal, roads, car, packages)
      costs = c(costs, length(visited))
    }
    return (unpicked[which.min(costs),])
  }
}

# Return true if car is loaded, false otherwise
isLoaded=function(car) {
  return (car$load != 0)
}

# Return the delivery location of the package which is currently loaded
getDeliveryLocation=function(packages) {
  return (packages[which(packages[,5] %in% c(1) == TRUE),])
}

# Solve the DeliveryMan assignment using the A* search
aStarSearchDM=function(roads, car, packages) {
  goal = NULL
  if(isLoaded(car)) {
    goal = getDeliveryLocation(packages)[3:4]
  } else {
    goal = getGoalPackage(roads, car, packages)[1:2]
  }

  visited = aStarSearch(goal, roads, car, packages)
  car$nextMove = generateNextMove(visited)
  return (car)
}

basicDM=function(roads,car,packages) {
  nextMove=0
  toGo=0
  offset=0
  if (car$load==0) {
    toGo=which(packages[,5]==0)[1]
  } else {
    toGo=car$load
    offset=2
  }
  if (car$x<packages[toGo,1+offset]) {nextMove=6}
  else if (car$x>packages[toGo,1+offset]) {nextMove=4}
  else if (car$y<packages[toGo,2+offset]) {nextMove=8}
  else if (car$y>packages[toGo,2+offset]) {nextMove=2}
  else {nextMove=5}
  car$nextMove=nextMove
  car$mem=list()
  return (car)
}

manualDM=function(roads,car,packages) {
  if (car$load>0) {
    print(paste("Current load:",car$load))
    print(paste("Destination: X",packages[car$load,3],"Y",packages[car$load,4]))
  }
  car$nextMove=readline("Enter next move. Valid moves are 2,4,6,8,0 (directions as on keypad) or q for quit.")
  if (car$nextMove=="q") {stop("Game terminated on user request.")}
  return (car)
}

#' Run Delivery Man
#'
#' Runs the delivery man game. In this game, deliveries are randomly placed on a city grid. You
#' must pick up and deliver the deliveries as fast as possible under changing traffic conditions.
#' Your score is the time it takes for you to complete this task. To play manually pass manualDM
#' as the carReady function and enter the number pad direction numbers to make moves.
#' @param carReady Your function that takes three arguments: (1) a list of two matrices giving the
#' traffice conditions. The first matrix is named 'hroads' and gives a matrix of traffice conditions
#' on the horizontal roads. The second matrix is named 'vroads' and gives a matrix of traffic
#' conditional on the vertical roads. (2) a list providing information about your car. This
#' list includes the x and y coordinates of the car with names 'x' and 'y', the package the car
#' is carrying, with name 'load' (this is 0 if no package is being carried), a list called
#' 'mem' that you can use to store information you want to remember from turn to turn, and
#' a field called nextMove where you will write what you want the car to do. Moves are
#' specified as on the number-pad (2 down, 4 left, 6 right, 8 up, 5 stay still). (3) A
#' matrix containing information about the packages. This contains five columns and a row for each
#' package. The first two columns give x and y coordinates about where the package should be picked
#' up from. The next two columns give x and y coordinates about where the package should be
#' delivered to. The final column specifies the package status (0 is not picked up, 1 is picked up but not delivered, 2 is delivered).
#' Your function should return the car object with the nextMove specified.
#' @param dim The dimension of the board. You will be scored on a board of dimension 10.
#' @param turns The number of turns the game should go for if deliveries are not made. Ignore this
#' except for noting that the default is 2000 so if you have not made deliveries after 2000 turns
#' you fail.
#' @param doPlot Specifies if you want the game state to be plotted each turn.
#' @param pause The pause period between moves. Ignore this.
#' @param del The number of deliveries. You will be scored on a board with 5 deliveries.
#' @return A string describing the outcome of the game.
#' @export
runDeliveryMan <- function (carReady=manualDM,dim=10,turns=2000,
                            doPlot=T,pause=0.1,del=5) {
  roads=makeRoadMatrices(dim)
  car=list(x=1,y=1,wait=0,load=0,nextMove=NA,mem=list())
  packages=matrix(sample(1:dim,replace=T,5*del),ncol=5)
  packages[,5]=rep(0,del)
  for (i in 1:turns) {
    roads=updateRoads(roads$hroads,roads$vroads)
    if (doPlot) {
      makeDotGrid(dim,i)
      plotRoads(roads$hroads,roads$vroads)
      points(car$x,car$y,pch=16,col="blue",cex=3)
      plotPackages(packages)
    }
    if (car$wait==0) {
      if (car$load==0) {
        on=packageOn(car$x,car$y,packages)
        if (on!=0) {
          packages[on,5]=1
          car$load=on
        }
      } else if (packages[car$load,3]==car$x && packages[car$load,4]==car$y) {
        packages[car$load,5]=2
        car$load=0
        if (sum(packages[,5])==2*nrow(packages)) {
          print (paste("Congratulations! You suceeded in",i,"turns!"))
          return (i)
        }
      }
      car=carReady(roads,car,packages)
      car=processNextMove(car,roads,dim)
    } else {
      car$wait=car$wait-1
    }
    if (pause>0) Sys.sleep(pause)
  }
  print (paste("You failed to complete the task. Try again."))
  return (NA)
}
packageOn<-function(x,y,packages){
  notpickedup=which(packages[,5]==0)
  onX=which(packages[,1]==x)
  onY=which(packages[,2]==y)
  available=intersect(notpickedup,intersect(onX,onY))
  if (length(available)!=0) {
    return (available[1])
  }
  return (0)
}
processNextMove<-function(car,roads,dim) {
  nextMove=car$nextMove
  if (nextMove==8) {
    if (car$y!=dim) {
      car$wait=roads$vroads[car$y,car$x]
      car$y=car$y+1
    } else {
      warning(paste("Cannot move up from y-position",car$y))
    }
  } else if (nextMove==2) {
    if (car$y!=1) {
      car$y=car$y-1
      car$wait=roads$vroads[car$y,car$x]
    } else {
      warning(paste("Cannot move down from y-position",car$y))
    }
  }  else if (nextMove==4) {
    if (car$x!=1) {
      car$x=car$x-1
      car$wait=roads$hroads[car$y,car$x]
    } else {
      warning(paste("Cannot move left from x-position",car$x))
    }
  }  else if (nextMove==6) {
    if (car$x!=dim) {
      car$wait=roads$hroads[car$y,car$x]
      car$x=car$x+1
    } else {
      warning(paste("Cannot move right from x-position",car$x))
    }
  } else if (nextMove!=5) {
    warning("Invalid move. No move made. Use 5 for deliberate no move.")
  }
  car$nextMove=NA
  return (car)
}

plotPackages=function(packages) {
  notpickedup=which(packages[,5]==0)
  notdelivered=which(packages[,5]!=2)
  points(packages[notpickedup,1],packages[notpickedup,2],col="green",pch=18,cex=3)
  points(packages[notdelivered,3],packages[notdelivered,4],col="red",pch=18,cex=3)
}

makeRoadGrid<-function() {

  out=matrix(rep("S",51*51),ncol=51)
  out[26,]=rep("H",51)
  out[,26]=rep("H",51)
}

makeRoadGrid<-function() {
  out=matrix(rep("S",51*51),ncol=51)
  out[26,]=rep("H",51)
  out[,26]=rep("H",51)
}
#' @export
makeDotGrid<-function(n,i) {
  plot(rep(seq(1,n),each=n),rep(seq(1,n),n),xlab="X",ylab="Y",main=paste("Delivery Man. Turn ", i,".",sep=""))
}

#' @export
makeRoadMatrices<-function(n){
  hroads=matrix(rep(1,n*(n-1)),nrow=n)
  vroads=matrix(rep(1,(n-1)*n),nrow=n-1)
  list(hroads=hroads,vroads=vroads)
}

#' @export
plotRoads<- function (hroads,vroads) {
  for (row in 1:nrow(hroads)) {
    for (col in 1:ncol(hroads)) {
      lines(c(col,col+1),c(row,row),col=hroads[row,col])
    }
  }
  for (row in 1:nrow(vroads)) {
    for (col in 1:ncol(vroads)) {
      lines(c(col,col),c(row,row+1),col=vroads[row,col])
    }
  }
}

#' @export
updateRoads<-function(hroads,vroads) {
  r1=runif(length(hroads))
  r2=runif(length(hroads))
  for (i in 1:length(hroads)) {
    h=hroads[i]
    if (h==1) {
      if (r1[i]<.05) {
        hroads[i]=2
      }
    }
    else {
      if (r1[i]<.05) {
        hroads[i]=h-1
      } else if (r1[i]<.1) {
        hroads[i]=h+1
      }
    }
    v=vroads[i]
    if (v==1) {
      if (r2[i]<.05) {
        vroads[i]=2
      }
    }
    else {
      if (r2[i]<.05) {
        vroads[i]=v-1
      } else if (r2[i]<.1) {
        vroads[i]=v+1
      }
    }
  }
  list (hroads=hroads,vroads=vroads)
}
