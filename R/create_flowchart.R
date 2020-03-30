library(DiagrammeR)

create_graph() %>% 
  add_global_graph_attrs(
    attr = "overlap",
    value = "false",
    attr_type = "graph") %>% 
  add_global_graph_attrs(
    attr = "layout",
    value = "neato",
    attr_type = "graph") %>%
  add_node( # id 1
    label = "fixed resources",
    node_aes = node_aes(
      style = "filled",
      shape = "rectangle",
      fixedsize = FALSE,
      x = -2,
      y = 17,
      fontsize = 12)
  ) %>% 
  add_node( # id 2
    label = "test what you can afford",
    node_aes = node_aes(
      style = "filled",
      shape = "rectangle",
      fixedsize = FALSE,
      x = -4,
      y = 16,
      fontsize = 12)
  ) %>% 
  add_edge(
    from = 1,
    to = 2,
    edge_aes = edge_aes(
      arrowhead = "normal",
      label = "Yes",
      fontsize = 12)
  ) %>% 
  add_node( # id 3
    label = "inferential goal",
    node_aes = node_aes(
      style = "filled",
      shape = "rectangle",
      fixedsize = FALSE,
      x = 0,
      y = 16,
      fontsize = 12)
  ) %>%
  add_edge(
    from = 1,
    to = 3,
    edge_aes = edge_aes(
      arrowhead = "normal",
      label = "No",
      fontsize = 12)
  ) %>% 
  add_edge(
    from = 2,
    to = 3,
    edge_aes = edge_aes(
      arrowhead = "normal",
      fontsize = 12)
  ) %>% 
  add_node( # id 4
    label = "expected effect",
    node_aes = node_aes(
      style = "filled",
      shape = "rectangle",
      fixedsize = FALSE,
      x = -5,
      y = 15,
      fontsize = 12)
  ) %>%
  add_edge(
    from = 3,
    to = 4,
    edge_aes = edge_aes(
      arrowhead = "normal",
      label = "Testing",
      fontsize = 12,
      len = 1)
  ) %>%
  add_node( # id 5
    label = "population SD known?",
    node_aes = node_aes(
      style = "filled",
      shape = "rectangle",
      fixedsize = FALSE,
      x = 5,
      y = 15,
      fontsize = 12)
  ) %>%
  add_edge(
    from = 3,
    to = 5,
    edge_aes = edge_aes(
      arrowhead = "normal",
      label = "Estimation",
      fontsize = 12,
      len = 1)
  ) %>%
  add_node( # id 6
    label = "statistical framework",
    node_aes = node_aes(
      style = "filled",
      shape = "rectangle",
      fixedsize = FALSE,
      x = -8,
      y = 14,
      fontsize = 12)
  ) %>%
  add_edge(
    from = 4,
    to = 6,
    edge_aes = edge_aes(
      arrowhead = "normal",
      label = "=0",
      fontsize = 12)
  ) %>%
  add_node( # id 7
    label = "statistical framework",
    node_aes = node_aes(
      style = "filled",
      shape = "rectangle",
      fixedsize = FALSE,
      x = -2,
      y = 14,
      fontsize = 12)
  ) %>%
  add_edge(
    from = 4,
    to = 7,
    edge_aes = edge_aes(
      arrowhead = "normal",
      label = ">0",
      fontsize = 12)
  ) %>%
  add_node( # id 8
    label = "TOST",
    node_aes = node_aes(
      style = "filled",
      shape = "rectangle",
      fixedsize = FALSE,
      x = -7,
      y = 11,
      fontsize = 12)
  ) %>%
  add_edge(
    from = 6,
    to = 8,
    edge_aes = edge_aes(
      arrowhead = "normal",
      label = "Frequentist",
      fontsize = 12)
  ) %>%
  add_node( # id 9
    label = "ROPE",
    node_aes = node_aes(
      style = "filled",
      shape = "rectangle",
      fixedsize = FALSE,
      x = -5,
      y = 11,
      fontsize = 12)
  ) %>%
  add_edge(
    from = 6,
    to = 9,
    edge_aes = edge_aes(
      arrowhead = "normal",
      label = "Bayesian",
      fontsize = 12)
  ) %>%
  add_node( # id 10
    label = "sequential testing?",
    node_aes = node_aes(
      style = "filled",
      shape = "rectangle",
      fixedsize = FALSE,
      x = -2,
      y = 13,
      fontsize = 12)
  ) %>%
  add_edge(
    from = 7,
    to = 10,
    edge_aes = edge_aes(
      arrowhead = "normal",
      label = "Bayesian",
      fontsize = 12)
  ) %>%
  add_node( # id 11
    label = "range\nof effects",
    node_aes = node_aes(
      style = "filled",
      shape = "rectangle",
      fixedsize = FALSE,
      x = 2,
      y = 13,
      fontsize = 12)
  ) %>%
  add_edge(
    from = 7,
    to = 11,
    edge_aes = edge_aes(
      arrowhead = "normal",
      label = "Frequentist",
      fontsize = 12)
  ) %>%
  add_node( # id 12
    label = "BFDA",
    node_aes = node_aes(
      style = "filled",
      shape = "rectangle",
      fixedsize = FALSE,
      x = -3,
      y = 11,
      fontsize = 12)
  ) %>%
  add_edge(
    from = 10,
    to = 12,
    edge_aes = edge_aes(
      arrowhead = "normal",
      label = "Yes",
      fontsize = 12)
  ) %>%
  add_node( # id 13
    label = "decide BF threshold",
    node_aes = node_aes(
      style = "filled",
      shape = "rectangle",
      fixedsize = FALSE,
      x = -1,
      y = 11,
      fontsize = 12)
  ) %>%
  add_edge(
    from = 10,
    to = 13,
    edge_aes = edge_aes(
      arrowhead = "normal",
      label = "No",
      fontsize = 12)
  ) %>%
  add_node( # id 14
    label = "traditional power",
    node_aes = node_aes(
      style = "filled",
      shape = "rectangle",
      fixedsize = FALSE,
      x = 1,
      y = 11,
      fontsize = 12)
  ) %>%
  add_edge(
    from = 11,
    to = 14,
    edge_aes = edge_aes(
      arrowhead = "normal",
      label = "Single effect size",
      fontsize = 12)
  ) %>%
  add_node( # id 15
    label = "power curve",
    node_aes = node_aes(
      style = "filled",
      shape = "rectangle",
      fixedsize = FALSE,
      x = 3,
      y = 11,
      fontsize = 12)
  ) %>%
  add_edge(
    from = 11,
    to = 15,
    edge_aes = edge_aes(
      arrowhead = "normal",
      label = "Range of effect sizes",
      fontsize = 12)
  ) %>%
  add_node( # id 16
    label = "AIPE",
    node_aes = node_aes(
      style = "filled",
      shape = "rectangle",
      fixedsize = FALSE,
      x = 5,
      y = 11,
      fontsize = 12)
  ) %>%
  add_edge(
    from = 5,
    to = 16,
    edge_aes = edge_aes(
      arrowhead = "normal",
      label = "Yes",
      fontsize = 12)
  ) %>%
  add_node( # id 17
    label = "APP",
    node_aes = node_aes(
      style = "filled",
      shape = "rectangle",
      fixedsize = FALSE,
      x = 7,
      y = 11,
      fontsize = 12)
  ) %>%
  add_edge(
    from = 5,
    to = 17,
    edge_aes = edge_aes(
      arrowhead = "normal",
      label = "No",
      fontsize = 12)
  ) %>%
  render_graph()
  # save_graph("inst/app/www/flowchart")
  # export_graph(
  #       file_name = "flowchart.pdf",
  #       title = "flowchart")
