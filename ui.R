library(shiny)
library(shinythemes)
library(rsconnect)
library(plotly)
library(caret)
library(ElemStatLearn)
library(dplyr)
library(ggplot2)
library(gbm)
library(e1071)
library(shinyalert)

shinyUI(fluidPage(
    h2(strong("Machine Learning: Predicting the Treatment of Grass")),
    theme = shinytheme("united"),
    sidebarPanel(
        h3("Your prediction"),hr(),useShinyalert(),
        actionButton("message1",h4("Better than Machine Learning")),
        hr(),h4("Accuracy:"),h4(textOutput("Result")),hr(),tableOutput("table1")),
    mainPanel(
        modalDialog(
            h4("Now, you are one of these researchers who dares to challenge this machine learning. 
            The first step you have to do is to look at that plot. The next step is to make your prediction 
            of what the next plot looks like. In order to defeat the machine learning, your 
            prediction must be 100% accurate. Use mouse to hover particular points. 
            Then, click Prediction Complete to see if your task is done."),
            title = h2("How to play?"),size = "l",easyClose = FALSE,
            footer = modalButton("Ready!"),confirmButtonCol = "#FF7E00"
        )),
    mainPanel(navbarPage(title="",
                         tabPanel(h4("Make your prediction"),actionButton("summary1","Summary"),
                                  useShinyalert(),actionButton("summary2","How to play"),
                                  useShinyalert(),
                                  actionButton("warning1","Accuracy of Machine Learning"),
                                  plotlyOutput("plot3"),useShinyalert(),
                                  actionButton("message","Prediction Complete"),
                                  actionButton("reset1","Reset")),
                         tabPanel(
                                  h4("Prediction Based on the Dataset"),
                                  plotlyOutput("plot1"),
                                  h6(a("Find out how Machine Learning works",
                                  href = "http://rpubs.com/jonahwinninghoff/563864"))))
)))