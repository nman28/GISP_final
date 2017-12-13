
library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(stringr)

all_matches <- read.csv("allMatches.csv")

df <- all_matches
df$defender <- replicate(nrow(all_matches), NA)
for(i in 1:nrow(all_matches)) {
  df$defender[i] <- paste(df$defender_firstname[i], df$defender_lastname[i], sep = " ")
}
 
df <- df %>% select(defender,
                    shot_clock, 
                    shot_outcome,
                    defender_height,
                    player_distance, 
                    shot_dist, 
                    def_angle, 
                    def_reyting, 
                    distance,
                    rowNum
                    )
colnames(df) <- c("Defender Name",
                  "Shot Clock",
                  "Shot Result",
                  "Defender Height",
                  "Distance to Steph",
                  "Shot Distance",
                  "Angle",
                  "Defender Rating",
                  "Prop Score",
                  "rowNum"
                  )
all_matches$rowNum <- as.character(all_matches$rowNum)

get_table_left <- function(id, df) {
  left <- paste0(id, "L") 
  l <- df %>% filter(rowNum == left) %>% select(-rowNum)
}

get_table_right <- function(id, df) {
  right <- paste0(id, "R") 
  l <- df %>% filter(rowNum == right) %>% select(-rowNum)
}

get_pic_left <- function(id, df) {
  left <- paste0(id, "L")
  name <- df %>% filter(rowNum == left) %>% select(defender_firstname)
  lastname <- df %>% filter(rowNum == left) %>% select(defender_lastname)
  name <- tolower(name[1, 1])
  name <- name %>% str_replace_all(fixed("."), "")
  lastname <- tolower(lastname[1, 1])
  lastname <- lastname %>% str_replace_all(fixed("."), "")
  two <- str_sub(name, 1, 2)
  five <- str_sub(lastname, 1, 5)
  first <- "01"
  second <- "02"
  third <- "03"
  convention <- paste0(five, two, first)
  if(convention == "davisan01") {
    return("https://d2cwpp38twqe55.cloudfront.net/req/201712064/images/players/davisan02.jpg")
  } else if(convention == "johnsst01") {
    return("https://d2cwpp38twqe55.cloudfront.net/req/201712064/images/players/johnsst04.jpg")
  }  else {
    link <- paste0("https://d2cwpp38twqe55.cloudfront.net/req/201712064/images/players/", convention, ".jpg")
    return(link)
  }

}

get_pic_right <- function(id, dataframe) {
  right <- paste0(id, "R")
  name <- dataframe %>% filter(rowNum == right) %>% select(defender_firstname)
  lastname <- dataframe %>% filter(rowNum == right) %>% select(defender_lastname)
  name <- tolower(name[1, 1])
  name <- name %>% str_replace_all(fixed("."), "")
  lastname <- tolower(lastname[1, 1])
  lastname <- lastname %>% str_replace_all(fixed("."), "")
  two <- str_sub(name, 1, 2)
  five <- str_sub(lastname, 1, 5)
  first <- "01"
  second <- "02"
  third <- "03"
  convention <- paste0(five, two, first)
  if(convention == "davisan01") {
    return("https://d2cwpp38twqe55.cloudfront.net/req/201712064/images/players/davisan02.jpg")
  } else if(convention == "johnsst01") {
    return("https://d2cwpp38twqe55.cloudfront.net/req/201712064/images/players/johnsst04.jpg")
  }  else {
    link <- paste0("https://d2cwpp38twqe55.cloudfront.net/req/201712064/images/players/", convention, ".jpg")
    return(link)
  }
}


ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "SportsVU App",
                                    dropdownMenu(type = "messages",
                                                 messageItem(
                                                   from = "Ozan, Steven, Nico & Varun",
                                                   message = "Welcome !!!"
                                                 )
                                    )
                    ),
                    dashboardSidebar(
                      width = 200,
                      sidebarMenu(
                        menuItem("Contributors", tabName = "contributors", icon = icon("users")),
                        menuItem("Abstract", tabName = "abstract", icon = icon("file-text-o")),
                        menuItem("Introduction", tabName = "introduction", icon = icon("info-circle")),
                        menuItem("Methodology", tabName = "methodology", icon = icon("rocket")),
                        menuItem("Results", tabName = "results", icon = icon("soccer-ball-o")),
                        menuItem("Discussion", tabName = "discussion", icon = icon("comments-o")),
                        menuItem("Interactive", tabName = "interactive", icon = icon("bar-chart"), badgeLabel = "cool", badgeColor = "green"),
                        menuItem("Sources", tabName = "sources", icon = icon("code"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        # First tab content
                        tabItem(tabName = "contributors",
                                h2("Coming soon")
                        ),
                        tabItem(tabName = "introduction",
                                HTML('<h1><span style="font-weight: 400;">Introduction</span></h1>
<p>&nbsp;</p>
                                     <p><span style="font-weight: 400;">At the heart of our project are the questions: how does one quantify defensive proficiency and how can one effectively defend certain shooters? &nbsp;Defensive stats 10 years ago lacked complexity, and did not capture the whole picture by mainly focusing on blocks and steals, which do not truly capture the defensive expertise of an individual. A block typically occurs in a situation where the shooter believes that they will be able to get a shot off given the defensive landscape at the time, but his or her judgement was wrong. Ultimately, the best &lsquo;rim defenders&rsquo; may have less blocked shots simply because offensive players avoid shooting when near them. Additionally, while a steal may be a result of strong defensive play, it may also be indicative of a defender&rsquo;s tendency to gamble, or an offensive player&rsquo;s lapse in judgment. </span></p>
                                     <p><br /><br /></p>
                                     <p><span style="font-weight: 400;">Nowadays, however, the data available to evaluate who are the best defenders in the NBA is developing and becoming more complex very rapidly. One result of this progress is the new and highly popular heat maps that display the strengths and weaknesses of a defender via a color coded illustration as shown below:</span></p>
                                     <p>&nbsp;</p>
                                     <p><span style="font-weight: 400;"> &nbsp;</span></p>
                                     <p>&nbsp;</p>
                                     <p><span style="font-weight: 400;">The proliferation of NBA analytics has largely been attributed to the introduction of Sport VU technology. &nbsp;SportVU data consists of optical tracking data that is gathered 25 times per second by a 6 camera system. Since the 2013-2014 NBA season, SportVU cameras have collected data in every NBA arena.</span></p>
                                     <p><span style="font-weight: 400;">With access to Sport VU data from the 2015-2016 NBA season we plan to investigate which variables, within a defender or coach&rsquo;s control, might most influence the probability a shot goes in the most. The variables we are analyzing include defender distance from shooter, defender angle to shooter, which side of the shooter the defender is coming from, and defender height. Studying a shooter&rsquo;s &lsquo;distance from the defender&rsquo; is not a novel ideal. According to an article</span><span style="font-weight: 400;">1</span><span style="font-weight: 400;"> by numberFire, data seems to suggest a dropoff in shooting percentage at roughly the 4-feet point. Others have </span><span style="font-weight: 400;">developed a metric called Effective Shot Quality, which measured shot quality using a machine learning model taking into account shot distance, shot angle, defender distance, defender angle, player speed, player velocity angle</span><span style="font-weight: 400;">2</span><span style="font-weight: 400;">. </span></p>
                                     <p>&nbsp;</p>
                                     <p><span style="font-weight: 400;">Our project builds on past projects by focusing on individual players, with particular attention to actionable steps defensive players and coaches can take in defending these players. For example, what steps can an opposing player or coach take to better defend Curry&rsquo;s 3?</span></p>')
                        ),
                        tabItem(tabName = "methodology",
                                h2("Coming soon")
                        ),
                        tabItem(tabName = "results",
                                h2("Coming soon")
                        ),
                        tabItem(tabName = "discussion",
                                h2("Coming soon")
                        ),
                        # Second tab content
                        tabItem(tabName = "interactive",
                                fluidRow(
                                  box(
                                    title = "Defender 1", status = "warning", solidHeader = TRUE,
                                    collapsible = TRUE, width = 4, height = 320,
                                    htmlOutput("court1")
                                  ),
                                  
                                  box(
                                    title = "Matched Shots", status = "danger", solidHeader = TRUE,
                                    width = 4, height = 320,
                                    selectInput(inputId = "pairInput", "Choose a pair:", choices = 1:146),
                                    tags$img(width = 200, height = 150, src = "image.png"),
                                    h4("Stephen Curry 3PT Shots")
                                  ),
                                  
                                  box(
                                    title = "Defender 2", status = "info", solidHeader = TRUE,
                                    collapsible = TRUE, width = 4, height = 320,
                                    htmlOutput("court2")
                                  )
                                ),
                                fluidRow(
                                  box(status = "warning",
                                    # The id lets us use input$tabset on the server to find the current tab
                                    id = "box1Input", height = "110px",  width = 12,
                                    
                                    column( width = 1, offset = 0,
                                      htmlOutput("image1Input")
                                    ),
                                    
                                    column(width = 11, 
                                      
                                      tableOutput("result1Output")
                                             )
                                  )
              
                                  
                                ),
                                fluidRow(
                                  box(status = "info",
                                      # The id lets us use input$tabset on the server to find the current tab
                                      id = "box2Input", height = "110px",  width = 12,
                                      
                                      column(width = 1, offset = 0,
                                        htmlOutput("image2Input")
                                      ),
                                      
                                      column(width = 11,
                                        tableOutput("result2Output")  
                                      )
                                      
                                      
                                  )
                                )
                        ),
                        tabItem(tabName = "abstract",
                                h2("Coming soon")
                        ),
                        
                        tabItem(tabName = "sources",
                                h2("Coming soon")
                        )
                        
                      )
                      
                      
                    )
)


server <- function(input, output, session) {
  
  src1 <- reactive( {
    get_pic_left(input$pairInput, all_matches)
  })
  
  src2 <- reactive( {
    get_pic_right(input$pairInput, all_matches)
    
  })
  
  crt1 <- reactive( {
    paste0(input$pairInput, "L.png")
  })
  
  crt2 <- reactive( {
    paste0(input$pairInput, "R.png")
  })
  
  tbl1 <- reactive( {
    get_table_left(input$pairInput, df)
  })
  
  tbl2 <- reactive( {
    get_table_right(input$pairInput, df)
  })
  
  output$court1 <- renderText({
    c('<center>', '<img height="263.2" width="140" src="', crt1(), '">', '</center>')
  })
  
  output$court2 <- renderText({
    c('<center>', '<img height="263.2" width="140" src="', crt2(), '">', '</center>')
  })

  output$image1Input <- renderText({
    c('<img height="90" width="60" src="', src1(), '">')

  })
  
  output$image2Input <- renderText({
    c('<img height="90" width="60" src="', src2(), '">')
  })
  
  output$result1Output <- renderTable({
    tbl1()
  })
  
  output$result2Output <- renderTable({
    tbl2()
  })
  
}

shinyApp(ui = ui, server = server)
