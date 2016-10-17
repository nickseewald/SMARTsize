dyoDiagramStage2UI <- function(id) {
  ns <- NS(id)
}

dyoDiagramStage2 <-
  function(input, output, session, RNR, rerand, ntxt.stage1, ntxt.stage2) {
    RNR <- tolower(RNR)
    linkLabel <- switch(RNR, "r" = "Responders", "nr" = "Non-Responders")
    reactive({
      if (rerand() == "Yes") {
        # Add randomization nodes (circled R's), one per first-stage treatment,
        # for responders/non-responders to first-stage treatment
        rerand.nodeNames <-
          sapply(1:ntxt.stage1(), function(i)
            paste0("rerand", LETTERS[i], RNR))
        nodes <- create_nodes(
          nodes = rerand.nodeNames,
          label = rep("R", ntxt.stage1()),
          shape = "circle"
        )
        
        # Create edges from each first-stage treatment to a randomization node
        edges <- create_edges(
          from = LETTERS[1:ntxt.stage1()],
          to = rerand.nodeNames,
          label = rep(linkLabel, ntxt.stage1())
        )
        
        # Create second-stage treatment nodes with labels "<stage1txt>r<stage2txt>",
        # where <stage1txt> is a capital letter and <stage2txt> is a number. Do this by
        # looping over first-stage treatments and adding one node per second-stage
        # treatment (using sapply)
        stage2.nodeNames <-
          as.vector(sapply(1:ntxt.stage1(), function(i)
            sapply(1:ntxt.stage2(),
                   function(j)
                     paste0(LETTERS[i], RNR, j))))
        
        nodes <- combine_nodes(nodes,
                               create_nodes(nodes = stage2.nodeNames, shape = "rectangle"))
        
        # Create edges between rerandomization nodes and second-stage treatment nodes
        edges <- combine_edges(edges,
                               create_edges(
                                 from = rep(rerand.nodeNames, each = ntxt.stage2()),
                                 to = stage2.nodeNames
                               ))
      } else {
        # If rerand() is false, don't draw rerandomization nodes and instead
        # create one node per first-stage treatment to a single
        # second-stage treatment option
        nodes <- create_nodes(
          nodes = sapply(1:ntxt.stage1(), function(i)
            paste0(LETTERS[i], RNR)),
          shape = "rectangle"
        )
        
        edges <- create_edges(
          from = LETTERS[1:ntxt.stage1()],
          to = sapply(1:ntxt.stage1(), function(i)
            paste0(LETTERS[i], RNR)),
          label = rep(linkLabel, ntxt.stage1())
        )
      }
      return(list("nodes" = nodes, "edges" = edges))
    })
  }