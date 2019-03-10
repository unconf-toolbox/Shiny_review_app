library(shiny)
library(rdrop2)

# token <- drop_auth()
# saveRDS(token, "my-token.rds")
# drop_auth(rdstoken = "my-token.rds")
# token <- readRDS("my-token.rds")

## Load data from dropbox
filenames <- drop_dir('responses/')$name
user_data <- list()
for (i in 1:length(filenames)){
  user_data[[i]] <- drop_read_csv(paste0('responses/',filenames[i]))
}
all_files <- do.call(rbind, user_data)

last_rating <- all_files[nrow(all_files),]




## Recode data
last_rating$twitter10 <- ifelse(last_rating$twitter10=="Yes","Twitter","")
last_rating$github10 <- ifelse(last_rating$github10=="Yes","Github","")
last_rating$blog10 <- ifelse(last_rating$blog10=="Yes","Blog","")
last_rating$linkedin10 <- ifelse(last_rating$linkedin10=="Yes","Linkedin","")
last_rating$other10 <- ifelse(last_rating$other10=="Yes","Other","")

all_social <- c(last_rating$twitter10, last_rating$github10,last_rating$blog10,last_rating$linkedin10,last_rating$other10)

remove_names <- last_rating[-(1:3)]

# outputDir <- "responses/ratings2"
outputDir <- "ratings2/"


# fields <- c(last_rating$name1, last_rating$name2, last_rating$email, "decision", "total_score","diversity", "experience")
fields <- c("decision", "total_score","diversity", "experience")

saveData <- function(data) {
  data <- t(data)
  fileName <- paste(last_rating$name1,last_rating$name2,".csv")
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)

}




##Shiny App
shinyApp(
  ui = fluidPage(
    
    titlePanel(title=div(img(src='logo.png', align = "left", height = "40px", hspace = "10px"),
                         "Chicago R Unconference Reviews")),
    br(), br(),
    sidebarPanel(
      h3(textOutput("total_score")),br(),
      sliderInput("reach", "Reach:",
                  min = 0, max = 5,
                  value = 0),
      sliderInput("diversity", "Diversity:",
                  min = 0, max = 5,
                  value = 0),
      sliderInput("experience", "Experience:",
                  min = 0, max = 5,
                  value = 0),
      radioButtons("decision", "Decision", choices = c("Yes","No")),
      actionButton("submit", "Submit")
    ),
    
    mainPanel(
      h3("Reach"),
      checkboxGroupInput("reach2", "Social Media", choices = c("Twitter","Github","Blog","LinkedIn", "Blog","Other"), 
                         selected = all_social),
      h4(paste("Attending R UnConf because: ",last_rating$why)),
      h4(paste("How to magnify: ",last_rating$magnify)),
      
      h3("Diversity"),
      h4(paste("Underrepresented: ",last_rating$diversity)),
      h4(paste("Occupation: ",last_rating$experience)),
      
      h3("Experience"),
      checkboxGroupInput("experience2", "R Experience:", 
                         choices = c("Used git individually", "Used git to collaborate with others",
                                     "Used a web-based version control site (e.g. Github, Gitlab, BitBucket) individually", 
                                     "Used a web-based version control site (e.g. Github, Gitlab, BitBucket) to collaborate with others",
                                     "Made an open-source contribution to an existing project on GitHub",
                                     "Written an R script",
                                     "Made an R project",
                                     "Developed an R package for personal use",
                                     "Release an R package publicly",
                                     "Written unit tests for an R package",
                                     "Taught or helped others to do any or all of the above",
                                     "Other"), selected = last_rating$experience),
      checkboxGroupInput("relevant", "R usage:", 
                         choices = c("I have used R for coursework", "I have used R for academic research (including theses)",
                                     "I have used R for personal projects", "I have used R for work in industry",
                                     "I have used R for work in government", "I have used R for work in a nonprofit",
                                     "Other"), selected = last_rating$relevant),
      h4(paste("Accomplishments: ",last_rating$accomplishments)),
      br(),br(),
      dataTableOutput('table')
    )
  ),
  server = function(input, output) {
    
    output$total_score <- renderText({ 
      paste("Current Score", input$reach+input$diversity+input$experience, "/15")
    })
    
    output$table <- renderDataTable(remove_names, options = list(dom = 't'))
    
    observeEvent(input$submit, {
      saveData(formData())
    })

    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
  }
  
)
