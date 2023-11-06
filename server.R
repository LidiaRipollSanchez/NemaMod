# Define server logic 
server <- function(input, output, session){
  #Select tab ------------------------------------------------------------------
  
  # use action buttons as tab selectors
  update_all <- function(x) {
    updateSelectInput(session, "tab",
                      choices = c("", "New network", "New network2"),
                      label = "",
                      selected = x
    )
  }

  observeEvent(input$newplot, {
    update_all("New network")
  })
  observeEvent(input$newtable, {
    update_all("New network2")
  })

  # Dynamic render rules ----------------------------------------------------
  
  observeEvent("", {
    shinyjs::show(id = "network_panel")
    shinyjs::hide(id = "network2_panel")
  }, once = TRUE)
  
  observeEvent(input$newplot, {
    shinyjs::show(id = "network_panel")
    shinyjs::hide(id = "network2_panel")
  })
  observeEvent(input$newplot2, {
    shinyjs::show(id = "network2_panel")
    shinyjs::hide(id = "network_panel")
  })
  
  
  #Select data by neuron--------------------------------------------------------
  
    #Update the NPP-GPCR choices depending on node
    observeEvent(input$node, {
      updateSelectInput(
        session, 
        inputId = "system", 
        label = "Select Neuropeptide-GPCR pair:",
        choices = if (input$threshold == "Short"){c("All", NPPpairsbyneuron_sr[input$node,]
                                                   [!is.na(NPPpairsbyneuron_sr[input$node,])])}
                  else if (input$threshold == "Mid"){c("All", NPPpairsbyneuron_mr[input$node,]
                                                       [!is.na(NPPpairsbyneuron_mr[input$node,])])}
                  else if (input$threshold == "Long"){c("All", NPPpairsbyneuron_lr[input$node,]
                                                        [!is.na(NPPpairsbyneuron_lr[input$node,])])}
      )
    })
  
    #Select input network based on neuron input selected
    net1 <- reactive({
      input$refresh
      #isolate network
      isolate(
        if(input$system == "All" & input$threshold == "Short"){df_networks[['NPPsr']]}
        else if (input$system == "All" & input$threshold == "Mid"){df_networks[['NPPmr']]}
        else if (input$system == "All" & input$threshold == "Long"){df_networks[['NPPlr']]}
        else if (input$system != "All" & input$threshold == "Short")
        {df_sr[[which(NPPpairs$V1 == input$system)]]}
        else if (input$system != "All" & input$threshold == "Mid")
        {df_mr[[which(NPPpairs$V1 == input$system)]]}
        else if (input$system != "All" & input$threshold == "Long")
        {df_lr[[which(NPPpairs$V1 == input$system)]]}
      )
    })
    
    nit <- reactive({
      input$refresh
      #isolate input
      isolate(
        if(input$system == "All" & input$threshold == "Short"){NPPpairs_sr}
        else if(input$system == "All" & input$threshold == "Mid"){NPPpairs_mr}
        else if(input$system == "All" & input$threshold == "Long"){NPPpairs_lr}
        else if (input$system != "All" & input$threshold == "Short"){input$system}
        else if (input$system != "All" & input$threshold == "Mid"){input$system}
        else if (input$system != "All" & input$threshold == "Long"){input$system}
      )
    })
    
    #Select data by gene--------------------------------------------------------
    
    #Update the receptor choices depending on ligand
    observeEvent(input$ligand, {
      updateSelectInput(
        session, 
        inputId = "receptor", 
        label = "Select receptor:",
        choices = c("All", ReceptorsNPP$GeneGPCR[ReceptorsNPP$GeneLigand == input$ligand])
      )
    })
    
    #Select input network based on gene input selected
    net2 <- reactive({
      input$refresh2
      #isolate network
      isolate(
        if (input$receptor == 'All' & input$threshold2 == "Short")
        {df_NPP_sr[[which(Neuropeptides == input$ligand)]]}
        else if (input$receptor == 'All' & input$threshold2 == "Mid")
        {df_NPP_mr[[which(Neuropeptides == input$ligand)]]}
        else if (input$receptor == 'All' & input$threshold2 == "Long")
        {df_NPP_lr[[which(Neuropeptides == input$ligand)]]}
        else if(input$receptor != 'All' & input$threshold2 == "Short")
        {df_sr[[which(NPPpairs$V1 == paste(input$ligand, input$receptor))]]}
        else if(input$receptor != 'All' & input$threshold2 == "Mid")
        {df_mr[[which(NPPpairs$V1 == paste(input$ligand, input$receptor))]]}
        else if(input$receptor != 'All' & input$threshold2 == "Long")
        {df_lr[[which(NPPpairs$V1 == paste(input$ligand, input$receptor))]]}
      )
    })
    
    #Plot network by neuron ----------------------------------------------------
    
    # Define a reactive expression to calculate the network
    vg3_data <- reactive({
      # Create a graph object
      g <- graph_from_adjacency_matrix(net1(), mode = "directed", weighted = TRUE)
      
      # Get the selected node
      selected_node <- as.numeric(NeuronID$nodeID[NeuronID$nodeLabel %in% input$node])
      
      # Filter the edges connected to the selected node
      edges_to_keep <- incident_edges(g, selected_node, mode = "all")
      g2 <- delete_edges(g, E(g)[as.numeric(unlist(edges_to_keep)) * (-1)])
      
      # Keep the vertices that connect to the selected nodes
      nodes_to_keep <- neighbors(g, selected_node, mode = "all")
      g3 <- delete_vertices(g2, V(g2)[c(as.numeric(unlist(nodes_to_keep)), selected_node) * (-1)])
      
      # Set properties of nodes, edges, and transform graph to network
      set.seed(123)
      vg3 <- toVisNetworkData(g3)
      
      #To add the NPP-GPCR labels for each edge
      #Access the input data depending on threshold
      dat <- nit()
      
      #Create empty vectors
      NPP_GPCR_edge <- vector(mode='list', length = length(vg3$edges$from))
      temp <- character(0)
      output_label <- vector(mode='list', length = length(vg3$edges$from))
      
      #Create loop to get the NPP-GPCR values from the dataset for each edge
      for (i in 1:length(vg3$edges$from)){ 
        if (length(dat) > 1){
          temp <- dat[[(NeuronID$nodeID[NeuronID$nodeLabel == vg3$edges$from[i]] - 1) * 302 + NeuronID$nodeID[NeuronID$nodeLabel == vg3$edges$to[i]]]]
          NPP_GPCR_edge[i] <- list(temp[!is.na(temp)])
          if (length(NPP_GPCR_edge[[i]]) > 1) {
            # Join the vector elements with line breaks
            output_label[i] <- paste(NPP_GPCR_edge[[i]], collapse = ", ")
          } else {
            # If there's only one item, no transformation is needed
            output_label[i] <- NPP_GPCR_edge[[i]]
          }
        }
          else {
            output_label <- dat  
          }
      }
      
      #Store the information into the vg3 graph
      vg3$edges$label <- output_label
      
      #return the graph after the code runs
      return(vg3)
    })
    
    #plot network
    output$network <- renderVisNetwork({
      
      #Access network from reactive expression
      vg3 <- vg3_data()
      
      nodes <- data.frame(id = vg3$nodes$id, label = vg3$nodes$label, group = NeuronID$nodetype[NeuronID$nodeLabel %in% vg3$nodes$label],
                          font.size = 20)
      edges <- data.frame(from = vg3$edges$from, to = vg3$edges$to, value = vg3$edges$weight,
                          title = paste0("<b>NPP-GPCR:</b> ", vg3$edges$label, "<br> <b>Weight:</b> ", vg3$edges$weight))
      lnodes <- data.frame(label = c("  Pharynx  ", "    Motor   \n   neuron   ", "Interneuron", "   Sensory   \n  neuron  ",
                                     " Unknown "), shape = c("circle"),
                           color = c("pink", "lightgreen", "orange", "lightblue", "grey"), id = 1:5)
      
      # Plot the new graph object for the different layouts depending on the layout selected
      if (input$layout == "Circle"){
        visNetwork(nodes, edges, height = "100vh", width = "100%") %>%
          visIgraphLayout(randomSeed = 0) %>%
          visPhysics(stabilization = FALSE) %>%
          visEdges(arrows = "to", color = "lightgrey", dashes = TRUE, arrowStrikethrough = FALSE) %>%
          visOptions(highlightNearest = TRUE) %>% 
          visGroups(groupname = "Pharynx", color = "pink") %>% 
          visGroups(groupname = "motor neuron", color = "lightgreen") %>%
          visGroups(groupname = "interneuron", color = "orange") %>%
          visGroups(groupname = "sensory neuron", color = "lightblue") %>%
          visGroups(groupname = "unknown", color = "grey") %>%
          visLegend(addNodes = lnodes, useGroups = FALSE, width = 0.09, position = "right")%>%
          visInteraction(multiselect = TRUE, navigationButtons = TRUE)%>%
          visExport(type = "png", name = "NemaMode_Network", label = "DOWNLOAD", background = "white", style = 'color : gray')}
      else if (input$layout == "Force-directed"){
        visNetwork(nodes, edges, height = "100vh", width = "100%") %>%
          visLayout(randomSeed = 0)%>%
          visPhysics(stabilization = FALSE) %>%
          visEdges(arrows = "to", color = "lightgrey", dashes = TRUE, arrowStrikethrough = FALSE) %>%
          visOptions(highlightNearest = TRUE) %>%  
          visGroups(groupname = "Pharynx", color = "pink") %>% 
          visGroups(groupname = "motor neuron", color = "lightgreen") %>%
          visGroups(groupname = "interneuron", color = "orange") %>%
          visGroups(groupname = "sensory neuron", color = "lightblue") %>%
          visGroups(groupname = "unknown", color = "grey") %>%
          visLegend(addNodes = lnodes, useGroups = FALSE, width = 0.09, position = "right")%>%
          visInteraction(multiselect = TRUE, navigationButtons = FALSE)%>%
          visExport(type = "png", name = "NemaMode_Network", label = "DOWNLOAD", background = "white", style = 'color : #727477')}
      else if (input$layout == "Hierarchical"){
        visNetwork(nodes, edges, height = "100vh", width = "100%") %>%
          visHierarchicalLayout()%>%
          visPhysics(stabilization = FALSE) %>%
          visEdges(arrows = "to", color = "lightgrey", dashes = TRUE, arrowStrikethrough = FALSE) %>%
          visOptions(highlightNearest = TRUE) %>% 
          visGroups(groupname = "Pharynx", color = "pink") %>% 
          visGroups(groupname = "motor neuron", color = "lightgreen") %>%
          visGroups(groupname = "interneuron", color = "orange") %>%
          visGroups(groupname = "sensory neuron", color = "lightblue") %>%
          visGroups(groupname = "unknown", color = "grey") %>%
          visLegend(addNodes = lnodes, useGroups = FALSE, width = 0.09, position = "right")%>%
          visInteraction(multiselect = TRUE, navigationButtons = FALSE)%>%
          visExport(type = "png", name = "NemaMode_Network", label = "DOWNLOAD", background = "white", style = 'color : #727477')}
    })
    
    
    #Plot table by neuron----------------------------------------------------------------
    output$network1_table <- DT::renderDataTable({
      
      #Access network from reactive expression
      vg3 <- vg3_data()
      
      #Get the column values from the network
      dataset <- data_frame(vg3$edges$from, vg3$edges$to, vg3$edges$weight, vg3$edges$label)
      colnames(dataset) <- c("Sender neuron", "Receiver neuron","Weight", "NPP-GPCR")
      
      #Make into a table
      table <- setDT(dataset)
      
      #Plot table
      DT::datatable(
        data = table,
        rownames = TRUE,
        extensions = "Buttons",
        options = list(
          responsive = TRUE,
          dom = 'Bfrtp',
          buttons = c('csv', 'excel', 'pdf'),
          style = "bootstrap",
          lengthMenu = c(seq(10, 150, 5)),
          pageLength = 10,  # Set the initial page length
          columnDefs = list(list(width = '10%', targets = c(0,1,2,3,4)))
          )
        )
        
    }, server = FALSE)
    
    #Plot network by gene ----------------------------------------------------
    
    # Define a reactive expression to calculate the network
    
    vgg3_data <- reactive({
      
      # Create a graph object
      gg <- graph_from_adjacency_matrix(net2(), mode = "directed", weighted = TRUE)
      
      # Identify the nodes that do not form connections
      nodes_to_remove <- V(gg)[degree(gg) == 0]
      
      # Remove the nodes that do not form connections
      gg3 <- delete_vertices(gg, nodes_to_remove)
      
      #Get degree
      dd3 <- degree(gg3)
      bb3 <- betweenness(gg3)
      
      # Set properties of nodes, edges, colour-frame and transform graph to network
      set.seed(123)
      vgg3 <- toVisNetworkData(gg3)
      
      #return in a list
      vgg3$nodes$degree <- dd3
      vgg3$nodes$betweeness <- bb3
      return(vgg3)
    })
    
    #plot network
    output$network2 <- renderVisNetwork({
      
      #Access network from reactive expression
      vgg3 <- vgg3_data()

      nodes2 <- data.frame(id = vgg3$nodes$id, label = vgg3$nodes$label, group = NeuronID$nodetype[NeuronID$nodeLabel %in% vgg3$nodes$label],
                           font.size = 20, title = paste0("<b>Degree:</b> ", vgg3$nodes$degree))
      edges2 <- data.frame(from = vgg3$edges$from, to = vgg3$edges$to, value = vgg3$edges$weight, 
                           title = paste0("<b>NPP-GPCR:</b> ", paste(input$ligand, input$receptor)))
      lnodes <- data.frame(label = c("  Pharynx  ", "    Motor   \n   neuron   ", "Interneuron", "   Sensory   \n  neuron  ",
                                     " Unknown "), shape = c("circle"),
                           color = c("pink", "lightgreen", "orange", "lightblue", "grey"), id = 1:5)
      
      # Plot the new graph object for the different layouts depending on the layout selected
      if (input$layout2 == "Circle"){
        visNetwork(nodes2, edges2, height = "100vh", width = "100%") %>%
          visIgraphLayout(randomSeed = 0) %>%
          visPhysics(stabilization = FALSE) %>%
          visEdges(arrows = "to", color = "lightgrey", dashes = TRUE, arrowStrikethrough = FALSE) %>%
          visOptions(highlightNearest = TRUE) %>% 
          visGroups(groupname = "Pharynx", color = "pink") %>% 
          visGroups(groupname = "motor neuron", color = "lightgreen") %>%
          visGroups(groupname = "interneuron", color = "orange") %>%
          visGroups(groupname = "sensory neuron", color = "lightblue") %>%
          visGroups(groupname = "unknown", color = "grey") %>%
          visLegend(addNodes = lnodes, useGroups = FALSE, width = 0.09, position = "right")%>%
          visInteraction(multiselect = TRUE, navigationButtons = TRUE)%>%
          visExport(type = "png", name = "NemaMode_Network", label = "DOWNLOAD", background = "white", style = 'color : gray')}
      else if (input$layout2 == "Force-directed"){
        visNetwork(nodes2, edges2, height = "100vh", width = "100%") %>%
          visLayout(randomSeed = 0)%>%
          visPhysics(stabilization = FALSE) %>%
          visEdges(arrows = "to", color = "lightgrey", dashes = TRUE, arrowStrikethrough = FALSE) %>%
          visOptions(highlightNearest = TRUE) %>%  
          visGroups(groupname = "Pharynx", color = "pink") %>% 
          visGroups(groupname = "motor neuron", color = "lightgreen") %>%
          visGroups(groupname = "interneuron", color = "orange") %>%
          visGroups(groupname = "sensory neuron", color = "lightblue") %>%
          visGroups(groupname = "unknown", color = "grey") %>%
          visLegend(addNodes = lnodes, useGroups = FALSE, width = 0.09, position = "right")%>%
          visInteraction(multiselect = TRUE, navigationButtons = FALSE)%>%
          visExport(type = "png", name = "NemaMode_Network", label = "DOWNLOAD", background = "white", style = 'color : #727477')}
      else if (input$layout2 == "Hierarchical"){
        visNetwork(nodes2, edges2, height = "100vh", width = "100%") %>%
          visHierarchicalLayout()%>%
          visPhysics(stabilization = FALSE) %>%
          visEdges(arrows = "to", color = "lightgrey", dashes = TRUE, arrowStrikethrough = FALSE) %>%
          visOptions(highlightNearest = TRUE) %>% 
          visGroups(groupname = "Pharynx", color = "pink") %>% 
          visGroups(groupname = "motor neuron", color = "lightgreen") %>%
          visGroups(groupname = "interneuron", color = "orange") %>%
          visGroups(groupname = "sensory neuron", color = "lightblue") %>%
          visGroups(groupname = "unknown", color = "grey") %>%
          visLegend(addNodes = lnodes, useGroups = FALSE, width = 0.09, position = "right")%>%
          visInteraction(multiselect = TRUE, navigationButtons = FALSE)%>%
          visExport(type = "png", name = "NemaMode_Network", label = "DOWNLOAD", background = "white", style = 'color : #727477')}
    })
    
    #Plot table by gene----------------------------------------------------------------
    output$network2_table <- DT::renderDataTable({
      
      #Access network from reactive expression
      vgg3 <- vgg3_data()

      #Get the column values from the network
      dataset2 <- data_frame(vgg3$nodes$label, vgg3$nodes$degree, vgg3$nodes$betweeness)
      colnames(dataset2) <- c("Neuron", "Degree","Betweenness centrality")
      
      #Make a table
      table2 <- setDT(dataset2)
      
      #Plot table
      DT::datatable(
        data = table2,
        rownames = TRUE,
        extensions = "Buttons",
        options = list(
          dom = 'Bfrtp',
          buttons = c('csv', 'excel', 'pdf'),
          style = "bootstrap",
          lengthMenu = c(seq(15, 150, 5)),
          pageLength = 15  # Set the initial page length
        )
      )
        
    }, server = FALSE)
    
    #Footer text ---------------------------------------------------------------
    output$textfooter <- renderUI({
      HTML("IMPORTANT: Any data or results generated and visualised here were the
              collective effort of the authors listed on this manuscript. Therefore, 
              usage of the app and/or resulting output should be credited in written 
              form with a citation: <a href='https://doi.org/10.1016/j.neuron.2023.09.043' 
              target='_blank'>'Lidia Ripoll-Sánchez, Jan Watteyne, HaoSheng Sun, Robert Fernandez, 
              Seth R. Taylor, Alexis Weinreb, Barry L. Bentley, Marc Hammarlund, David M. Miller, 
              Oliver Hobert, Isabel Beets, Petra E. Vértes, William R. Schafer, 
              The neuropeptidergic connectome of C.elegans, Neuron, 2023'</a> <br> 
              App code was written by Lidia Ripoll-Sánchez [MRC Laboratory of Molecular Biology & University of Cambridge] 
              with support from Barry Bentley [University of Cardiff] and Richard Bethlehem [ University of Cambridge]. 
             <br>This app was inspired by the BrainChart app.")
    })
}
