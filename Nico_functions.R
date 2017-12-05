# The 3 (x,y) coordinates of the Player, Net and Defender in inches as input. Middle of rim is 63 inches from baseline and 300 inches form sideline. 
coordPlayer = c(1,0)
coordNetleft = c(5.25, 25)
coordNetright = c(88.75, 25)
coordDefender = c(0,0)


# Calculate the distance matrix between Player, Net and Defender as a 3x3 matrix - you can index whichever you want to use.
#distanceMatrix = dist(rbind(coordNet,coordPlayer,coordDefender),upper=TRUE,diag=TRUE)

# For example to get the distance you need to convert it to a matrix first and then the distance between the Player and the Net it would be: 
#as.matrix(distanceMatrix)["coordPlayer","coordNet"]

# To get the angle use the following mathematical formula by first getting the vectors from the coordinates 
#vectorToNet=coordNet-coordPlayer
#vectorToDefender=coordDefender-coordPlayer
#angleRad = acos( sum(vectorToNet*vectorToDefender) / ( sqrt(sum(vectorToNet * vectorToNet)) * sqrt(sum(vectorToDefender * vectorToDefender)) ) )

# To get the angle in degrees convert Angle in RAdiansby multiplying it by the Degrees to Radians constant: 360/2pi or 180/pi
#angleDeg = acos( sum(vectorToNet*vectorToDefender) / ( sqrt(sum(vectorToNet * vectorToNet)) * sqrt(sum(vectorToDefender * vectorToDefender)) ) )  * 180/pi


dist2net = function(playercoord) {
  closestNetCoord = c(ifelse(playercoord[1] < 41.75, 5.25, 88.75), 25)
  return((((playercoord[1] - closestNetCoord[1])^2) + ((playercoord[2] - closestNetCoord[2])^2))^0.5)
}


dist2defender = function(playercoord, defendercoord) {
  return((((playercoord[1] - defendercoord[1])^2) + ((playercoord[2] - defendercoord[2])^2))^0.5)
}

# default is radians
getAngle = function(playercoord, defendercoord, type = c("radians", "degrees")) {
  type <- match.arg(type)
  closestNetCoord = c(ifelse(playercoord[1] < 41.75, 5.25, 88.75), 25)
  
  vectorToNet = closestNetCoord - playercoord
  vectorToDefender = defendercoord - playercoord
  
  # To get the angle use the following mathematical formula by first getting the vectors from the coordinates 
  angleSize = acos(sum(vectorToNet * vectorToDefender)/(sqrt(sum(vectorToNet * vectorToNet)) * 
                                                          sqrt(sum(vectorToDefender * vectorToDefender))))
  
  if (type == "degrees") {
    # To get the angle in degrees convert Angle in Radians by multiplying it by the Degrees to Radians constant: 360/2pi or 180/pi
    angleSize = angleSize * 180/pi
  }
  
  return(angleSize)
  
}
