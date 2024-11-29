library(shiny)
library(bslib)
library(shinyFeedback)
library(DiagrammeR)
library(igraph)
library(tidyverse)
library(visNetwork)
library(reshape2)
library(ggplot2)

##Global variables ----
#These set the parameters for the model
inf_choices <- c("k", "mk", "r", "kc", "mkc", "rc")
names(inf_choices) <- c("Kosko", "Modified Kosko", "Rescale", "Kosko clamped", "Modified Kosko clamped", "Rescale clamped")
t_choices <- c("b","tr","s","t")
names(t_choices) <- c("Bivalent", "Trivalent", "Sigmoid", "Hyperbolic tangent")


##UI ----
# Define UI
ui <- navbarPage("Futzy",
                 ###Build model tab----
                 tabPanel("Build model",
                          useShinyFeedback(),
                          fluidPage(theme = bs_theme(bootswatch = "minty"), sidebarLayout(sidebarPanel(
                            fileInput("file1", "Load graph"),
                            hr(),
                            textInput("node_label", "Add Node:", ""),
                            selectInput("node_colour", "Colour:", choices = c("lightblue","blue", "grey", "orange", "green", "purple"),selected = "lightblue"),
                            actionButton("add_node", "Add Node"),
                            hr(),
                            selectInput("from_node", "From Node:", choices = NULL),
                            selectInput("to_node", "To Node:", choices = NULL),
                            sliderInput("coef","Strength of relationship", value = 0, -1, 1, step = 0.01),
                            actionButton("add_edge", "Add Edge"),
                            hr(),
                            actionButton("clear", "Clear Diagram"),
                            downloadButton("downloadGraph", "Download")
                          ),
                          column(width = 8,
                                 card( 
                                   card_header("Model preview"),
                                   visNetworkOutput("diagram"),
                                 ),
                                 card(card_header("Node and edge details"),
                                      htmlOutput("selectedNode")
                                      #actionButton("delete", "Delete node", disabled = TRUE))
                                 )
                          )
                          )
                          )  
                 ),
                 ###Model options tab----
                 tabPanel("Model options",
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         h1("Model options"),
                                         selectInput("infrule","Inference rule:", choices = inf_choices, selected = "k"),
                                         selectInput("transform", "Transformation function:", choices = t_choices, selected = "t"),
                                         selectInput("act.vec", "Activation vector", multiple = TRUE, choices = ""),
                                         selectInput("iter", "Model iterations", choices = 1:100, selected = 20),
                                         actionButton("run_model", "Run model")
                            ),
                            mainPanel(
                              card( 
                                card_header("Adjacency matrix"),
                                tableOutput("adjacency"),
                              ),
                              card(
                                card_header("Stable model output"),
                                visNetworkOutput("model"),
                              ),
                              card(
                                card_header("Model time series"),
                                plotOutput("modelPlot"),
                                tableOutput("modelSteps"),
                                tableOutput("modelStats")
                              )
                              #Graph statistics card somewhere in here
                            )
                          )
                 )
)
