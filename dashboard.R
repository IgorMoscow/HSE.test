#переменные: ID студента, lecture - код лекции, events - действие с лекцией, time_spent - время
HSE <- read.csv("https://raw.githubusercontent.com/IgorMoscow/HSE.test/master/HSE.csv", header = TRUE, sep = ";")
#ID студентов и сумма всех действий на платформе (своего рода общая активность)
students <- read.csv("https://raw.githubusercontent.com/IgorMoscow/HSE.test/master/students.csv", header = TRUE, sep = ";")
# ID лекций, события, сколько раз просматривалась, сколько раз скачивалась (датасет содержит самые популярные видеолекции)
load_play <- read.csv("https://raw.githubusercontent.com/IgorMoscow/HSE.test/master/load_play.csv", header = TRUE, sep = ";")
# ID Лекций, число problem_check (самые "проблемные" лекции)
problem_video <- read.csv("https://github.com/IgorMoscow/HSE.test/blob/master/problem_videos.csv", header = TRUE, sep = ";")

viewers <- read.csv("https://raw.githubusercontent.com/IgorMoscow/HSE.test/master/viewers.csv", header = TRUE, sep = ";")


library(shiny)
library(shinydashboard)
library(tidyverse)

ui <- dashboardPage(
  dashboardHeader(title = "Report"),
  dashboardSidebar(textInput("userID", inputId = "user")),
  dashboardBody(
    fluidRow(
      infoBoxOutput(outputId = "box1"),
      infoBoxOutput(outputId = "box2")
    ),
    fluidRow(
      box(title = "Played and loaded lectures", plotOutput(outputId = "graph1"), solidHeader = TRUE, background = "green"),
      box(title = "Problem videos" ,plotOutput(outputId = "graph2"), solidHeader = TRUE, background = "red")),
      fluidRow(infoBoxOutput(outputId = "box3", width = 2),
        box(title = "Table of events", tableOutput(outputId = "table1"), width = 2, solidHeader = TRUE, background = "purple"),
        box(title = "20 most active students", plotOutput(outputId = "graph3"),
            solidHeader = TRUE, background = "navy")
    )
  )
  )


server <- function(input, output) {
  output$box1 <- renderInfoBox({infoBox("Lectures", "50 different lectures", icon = icon("list"))})
  output$box2 <- renderInfoBox({infoBox("Events", "14 events", icon = icon("glyphicon"))})
  output$graph1 <- renderPlot({ggplot(data = load_play)+
      geom_bar(aes(x = reorder(lecture, n), y = n, fill = events), position = position_dodge(), stat = "identity")+
      xlab(label = "number of lectures")+
      ylab(label = "Lecture ID")+
      theme(axis.title.x = element_text(face = "bold", size = 11), 
            axis.title.y = element_text(face = "bold", size = 11))+
      coord_flip()})
  output$graph2 <- renderPlot({ggplot(data = problem_videos)+
      geom_bar(aes(x = reorder(lecture, problem_check), y = problem_check), fill = "skyblue3", stat = "identity")+
      xlab(label = "number of lectures")+
      ylab(label = "Lecture ID")+
      theme(axis.title.x = element_text(face = "bold", size = 11), 
            axis.title.y = element_text(face = "bold", size = 11))+
      coord_flip()})
  output$table1 <- renderTable({viewers[viewers$ID == input$user,-1]})
  output$box3 <- renderInfoBox({infoBox("UserID", paste(input$user), icon = icon("user", lib = "glyphicon"), fill = FALSE)})
  output$graph3 <- renderPlot({ggplot(data = students[1:20,])+
      geom_bar(aes(x = reorder(ID, n), y =n), stat = "identity", fill = "dodgerblue")+
      xlab(label = "students ID")+
      ylab(label = "number of lectures")+
      theme(axis.title.x = element_text(face = "bold", size = 11), 
            axis.title.y = element_text(face = "bold", size = 11))+
      coord_flip()})
}

shinyApp(ui = ui, server = server)
