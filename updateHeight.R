missingDefList = list(
  c('Marvin', 'Williams', 'PF', 108),
  c('Jeremy', 'Lin', 'SG', 106),
  c('Brian', 'Roberts', 'PG', 109),
  c('Kemba', 'Walker', 'PG', 105),
  c('Anthony', 'Davis', 'C', 104),
  c('O.J.', 'Mayo', 'SG', 110),
  c('PJ', 'Hairston', 'SF', 111),
  c('Andre', 'Miller', 'PG', 108),
  c('Lou', 'Williams', 'SG', 114),
  c('Anthony', 'Brown', 'SF', 114),
  c("D'Angelo", 'Russell', 'PG', 112),
  c('Joe', 'Johnson', 'SF', 113),
  c('Josh', 'Smith', 'PF', 103),
  c('Larry', 'Nance Jr.', 'PF', 109),
  c('Brandon', 'Knight', 'SG', 111),
  c("E'Twaun", 'Moore', 'SG', 109),
  c('Isaiah', 'Thomas', 'PG', 107),
  c('Marcus', "Morris", 'SF', 108),
  c('Markieff', 'Morris', 'PF', 106),
  c('Matt', 'Barnes', 'SF', 107),
  c('Stanley', 'Johnson', 'SF', 106)
)

updateHeight <- function(df, listOfNames) {
  
  # Each entry in listofNames of format c(FirstName, LastName, Height)
  
  
  for (player in listOfNames) {
    firstName = player[1]
    lastName = player[2]
    position = player[3]
    rating = player[4]
    
    df$def_reyting[df$defender_firstname == firstName & df$defender_lastname == lastName] = rating
    df$def_position[df$defender_firstname == firstName & df$defender_lastname == lastName] = position
  }
  
  assign('steph', df,envir=.GlobalEnv)
} 
