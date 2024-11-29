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

##Global functions ----

# Function to count nodes
count_nodes <- function(g) {
  if (is.null(g$nodes_df)) {
    return(0)
  } else {
    return(nrow(g$nodes_df))
  }
}

#Pulled the below function from the 'fcm' package - had to make some changes!
fcm.infer.2 <- function (activation_vec, weight_mat, iter = 20, infer = "k", 
                         transform = "s", lambda = 1, e = 0.001) 
{
  outlist <- list()
  if (length(which(activation_vec > 1)) & length(which(activation_vec > 
                                                       -1))) {
    stop("Please check the concepts' values of the activation vector. They must be in the range -1 and 1.")
  }
  if (length(which(weight_mat > 1)) & length(which(weight_mat > 
                                                   -1))) {
    stop("Please check the weights of the matrix. They must be in the range -1 and 1.")
  }
  if (sum(is.na(activation_vec)) > 0) {
    stop("Please check the activation vector for missing values.")
  }
  if (sum(is.na(weight_mat)) > 0) {
    stop("Please check the weight matrix for missing values.")
  }
  if (iter <= 0) 
    stop("The iterations must be higher than zero.")
  if (sum(!(infer %in% c("k", "mk", "r", "kc", "mkc", "rc"))) > 
      0) 
    stop("For the Inference Rule only Kosko 'k', modified Kosko 'mk',  Rescale 'r', Kosko-clamped 'kc', modified Kosko-clamped 'mkc' or Rescale-clamped 'rc' variables are allowed.")
  if (sum(!(transform %in% c("b", "tr", "s", "t"))) > 0) 
    stop("For the transformation functions only Bivalent 'b', Trivalent 'tr', Sigmoid 's' or\n            Hyperbolic tangent 't' variables are allowed.")
  if ((lambda <= 0) || (lambda >= 10)) 
    stop("Lambda value should be in the range 1 to 10.")
  if ((e < 1e-06) || (e > 0.01)) 
    stop("Epsillon (e) value should be in the range 0.01 to 0.000001.")
  m <- ncol(weight_mat)
  mylist <- list()
  for (i in 1:(iter - 1)) {
    if (i == 1) {
      if (infer == "k" || infer == "kc") {
        initial_vec <- colSums(t(activation_vec) * weight_mat)
      }
      else if (infer == "mk" || infer == "mkc") {
        initial_vec <- activation_vec + colSums(t(activation_vec) * 
                                                  weight_mat)
      }
      else if (infer == "r" || infer == "rc") {
        initial_vec <- (2 * activation_vec - 1) + colSums(t((2 * 
                                                               activation_vec) - 1) * weight_mat)
      }
      if (transform == "s") {
        initial_vec <- 1/(1 + exp(-lambda * initial_vec))
      }
      if (transform == "t") {
        initial_vec <- tanh(lambda * initial_vec)
      }
    }
    else {
      if (infer == "k" || infer == "kc") {
        initial_vec <- colSums(t(initial_vec) * weight_mat)
      }
      else if (infer == "mk" || infer == "mkc") {
        initial_vec <- initial_vec + colSums(t(initial_vec) * 
                                               weight_mat)
      }
      else if (infer == "r" || infer == "rc") {
        initial_vec <- (2 * initial_vec - 1) + colSums(t((2 * 
                                                            initial_vec) - 1) * weight_mat)
      }
      if (transform == "s") {
        initial_vec <- 1/(1 + exp(-lambda * initial_vec))
      }
      if (transform == "t") {
        initial_vec <- tanh(lambda * initial_vec)
      }
    }
    if (transform == "b") {
      for (j in 1:m) {
        if (initial_vec[j] > 0) {
          initial_vec[j] <- 1
        }
        else if (initial_vec[j] <= 0) {
          initial_vec[j] <- 0
        }
      }
    }
    if (transform == "tr") {
      for (j in 1:m) {
        if (initial_vec[j] > 0) {
          initial_vec[j] <- 1
        }
        else if (initial_vec[j] < 0) {
          initial_vec[j] <- -1
        }
        else initial_vec[j] <- 0
      }
    }
    if (infer == "kc" || infer == "mkc" || infer == "rc") {
      for (k in 1:m) {
        if (activation_vec[k] == 1) {
          initial_vec[k] <- (initial_vec[k] = 1)
        }
      }
    }
    mylist[[i]] <- initial_vec
  }
  steps_t <- (as.data.frame(do.call("rbind", mylist)))
  step_1 <- as.numeric(activation_vec)
  A <- (rbind(step_1, steps_t))
  last_conv <- as.double(A[iter, ] - A[(iter - 1), ])
  Res_e <- (length(last_conv[last_conv <= e]))
  if (Res_e < m) {
    cat("\n WARNING: More iterations are required to reach the convergence.\n \n")
  }
  else {
    mylist1 <- list()
    for (i in 2:(iter)) {
      subst <- abs(apply(A, 2, function(x) x[i] - x[i - 
                                                      1]))
      mylist1[[i]] <- subst
    }
    subst.mat <- do.call("rbind", mylist1)
    w <- as.data.frame(matrix(e, (iter - 1), m))
    mylist3 <- list()
    for (i in 1:(iter - 1)) {
      if (all(subst.mat[i, ] < w[i, ])) {
        cv <- 1
      }
      else {
        cv <- 2
      }
      mylist3[[i]] <- cv
    }
    cv.mat <- do.call("rbind", mylist3)
    if (cv.mat[[i]] == 2) {
      cat("\n WARNING: More iterations are required to reach the convergence.\n \n")
    }
    else {
      conv_state <- min(which(cv.mat == 1))
      outlist$convergence <- A[(conv_state + 1), ]
      outlist$conviter <- conv_state + 1
    }
  }
  outlist$all <- list(values = A)
  return(outlist)
}

##Server ----
# Define server logic
server <- function(input, output, session) {
  ###Reactive variables ----
  graph <- reactiveVal(create_graph())
  adj <- reactiveVal(data.frame())
  avec <- reactiveVal(data.frame())
  model <- reactiveVal(data.frame())
  selectednode <- reactiveVal(integer())
  t_st <- reactiveVal(1)
  
  ###File input function----
  observeEvent(input$file1, { 
    g <- open_graph(input$file1$datapath)
    graph(g)
    node_labels <- g$nodes_df$name
    names(node_labels) <- g$nodes_df$label
    
    updateSelectInput(session, "from_node", choices = node_labels, selected = FALSE)
    updateSelectInput(session, "to_node", choices = node_labels, selected = FALSE)
    updateSelectInput(session, "act.vec", choices = node_labels)
  })
  
  ###Download graph----
  output$downloadGraph <- downloadHandler(filename = "graph.dgf",
                                          content = function(file) {
                                            saveRDS(graph(), file)
                                          }
  )
  ###Add node----
  observeEvent(input$add_node, {
    flag <- FALSE #set a false flag
    g <- graph() #grab the graph
    if (input$node_label == "") {
      feedbackWarning("node_label", input$node_label == "", "Please enter a node label.")
    } else if (input$node_label %in% g$nodes_df$label) {
      feedbackWarning("node_label", TRUE, "Node label must be unique.")
    } else {
      flag <- TRUE
    }
    req(flag, cancelOutput = TRUE)
    
    
    new_node_id <- count_nodes(g) + 1
    g <- add_node(g, label = input$node_label, node_aes = node_aes(color = input$node_colour), node_data = node_data(name = paste(substr(input$node_label,1,3), new_node_id, sep = ""), 
                                                                                                                     value = 0))
    graph(g)
    
    node_choices <- 1:count_nodes(g)
    node_labels <- g$nodes_df$name
    names(node_labels) <- g$nodes_df$label
    
    updateSelectInput(session, "from_node", choices = node_labels, selected = FALSE)
    updateSelectInput(session, "to_node", choices = node_labels, selected = FALSE)
    updateSelectInput(session, "act.vec", choices = node_labels)
  })
  
  ###Add edge----
  observeEvent(input$add_edge, {
    if (input$from_node != "" && input$to_node != "") {
      feedbackWarning("coef", input$coef == 0, "Please enter a positive or negative coefficient.")
      req(input$coef != 0, cancelOutput = TRUE)
      feedbackWarning("to_node", input$to_node == input$from_node, "You can't have a loop effect.")
      req(input$from_node != input$to_node, cancelOutput = TRUE)
      g <- graph()
      from_node <- g$nodes_df[g$nodes_df$name == input$from_node,]
      to_node <- g$nodes_df[g$nodes_df$name == input$to_node,]
      if (input$coef > 0){
        edge_lab <- paste("increases (", input$coef, ")", sep="")
      } else {
        edge_lab <- paste("decreases (", input$coef, ")", sep="")
      }
      #check if this node already exists in either direction
      chk <- merge(data.frame("from" = from_node$id, "to" = to_node$id), g$edges_df)
      chk_inv <- merge(data.frame("from" = to_node$id, "to" = from_node$id), g$edges_df)
      if (nrow(chk)> 0) {
        g <- delete_edge (g, from = chk$from, to = chk$to)
      } else if (nrow(chk_inv) > 0) {
        g <- delete_edge (g, from = chk_inv$from, to = chk_inv$to)
      }
      
      g <- add_edge(g, from = from_node$id, to = to_node$id, edge_aes = edge_aes(label = edge_lab), edge_data = edge_data(coef = input$coef))
      
      # Add edge
      
      graph(g)
    }
  })
  
  ###Clear graph----
  observeEvent(input$clear, {
    graph(create_graph())
    updateSelectInput(session, "from_node", choices = NULL)
    updateSelectInput(session, "to_node", choices = NULL)
  })
  
  ###Run model----
  observeEvent(input$run_model, {
    #code here will run the model
    adjmat <- adj()
    a = data.frame(matrix(nrow = 0, ncol = length(colnames(adjmat)))) 
    colnames(a) = colnames(adjmat)
    a[1,] <- 0
    a[,input$act.vec] <- 1
    
    m <- graph() #retrieve the reactive graph
    o <- fcm.infer.2(a, adjmat, infer = input$infrule, transform = input$transform, iter = as.numeric(input$iter)) #this function needs to be rewritten to identify convergence
    model(o)
    if (is.null(o$convergence)) {
      feedbackWarning("run_model", is.null(o$convergence), "Convergence not reached. More iterations needed.")
      req(!is.null(o$convergence), cancelOutput = TRUE)
    }
    fvals <- o$convergence #this should be the convergence vector
    fvals <- fvals %>% pivot_longer(cols = everything(), names_to = "label", values_to = "value")
    output$modelSteps <- renderTable(o$all, striped = TRUE, rownames = TRUE)
    m <- m %>%
      set_node_attrs(
        node_attr = "label",
        values = paste(fvals$label, "(", round(fvals$value, digits = 3),")", sep = ""),
        nodes = fvals$label
      ) 
    pal <- colorRamp(c("red", "white", "green"))
    m$edges_df$color <- rgb(pal((m$edges_df$coef + 1)*0.5), maxColorValue = 255) #This almost works
    m$edges_df$width <- abs(m$edges_df$coef)*0.5*5
    output$model <- renderVisNetwork({ visNetwork(m$nodes_df, m$edges_df) %>%
        visEdges(arrows = "to") %>%
        visIgraphLayout(layout = "layout_on_grid") %>%
        visPhysics(solver = "barnesHut", barnesHut = list(avoidOverlap = 1)) %>%
        visInteraction(selectConnectedEdges = FALSE)
    })
    
    iterations <- as.numeric(rownames(o$all$values))
    df <- data.frame(iterations, o$all$values)
    
    df2 <- melt(df, id="iterations")
    output$modelPlot <- renderPlot({ggplot(data=df2,
                                           aes(x=iterations, y=value, group=variable, colour=variable)) +
        theme_bw() + geom_line() + geom_point(size = 2) })
  })
  
  
  
  ###Click event----
  observeEvent(input$click, {
    g <- graph()
    selectednode <- input$click
    node <- g$nodes_df[input$click, ]
    text <- paste("Selected node: ", node$label, "<br/>")
    
    text <- paste(text, "<br/>Influences:")
    edges_out <- g$edges_df[g$edges_df$from == input$click, ]
    edges_out <- g$nodes_df$label[edges_out$to]
    edges_out <- paste(edges_out, collapse = ", ")
    text <- paste(text, edges_out)
    
    text <- paste(text, "<br/><br/>Influenced by:")
    edges_in <- g$edges_df[g$edges_df$to == input$click, ]
    edges_in <- g$nodes_df$label[edges_in$from]
    edges_in <- paste(edges_in, collapse = ", ")
    text <- paste(text, edges_in)
    
    #updateActionButton(session, "delete", disabled = FALSE)
    output$selectedNode <- renderUI({HTML(paste(text))
    })
  })
  
  ###Delete node ----
  observeEvent(input$delete, {
    s <- selectednode()
    g <- graph()
    visRemoveNodes(g, s)
  })
  
  
  
  ###Render diagram----
  output$diagram <- renderVisNetwork({
    g <- graph()
    if (count_nodes(g) > 0) {
      #Eventually this should be moved to the edge creation space
      pal <- colorRamp(c("red", "white", "green"))
      g$edges_df$color <- rgb(pal((g$edges_df$coef + 1)*0.5), maxColorValue = 255) #This almost works
      g$edges_df$width <- abs(g$edges_df$coef)*0.5*5
      visNetwork(g$nodes_df, g$edges_df) %>%
        visEdges(arrows = "to") %>%
        visIgraphLayout(layout = "layout_on_grid") %>%
        visPhysics(solver = "barnesHut", barnesHut = list(avoidOverlap = 1)) %>%
        visInteraction(selectConnectedEdges = FALSE) %>%
        visEvents(click = "function(nodes){
                  Shiny.onInputChange('click', nodes.nodes[0]);
                  ;}")
    }
  })
  
  ###Stable node values----
  output$nodevalues <- renderTable({
    g <- graph()
    m <- model()
    stable <- m$convergence
    colnames(stable) <- g$nodes_df$label
    return(stable)
  })
  
  ###Adjacency matrix----
  output$adjacency <- renderTable({
    g <- graph()
    if (count_nodes(g) > 0) {
      newAdj <- as.matrix(as_adj(to_igraph(g), attr="coef", names = TRUE))
      colnames(newAdj) <- g$nodes_df$name
      rownames(newAdj) <- g$nodes_df$name
      adj (data.frame(newAdj))
    }
    return(adj())
  }, rownames = TRUE)
  
  
}

