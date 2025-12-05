library(networkD3)

nodes <- data.frame(
  name = c(
    "Eligible (N=9,945)",
    "Hybrid-induced (N=4,738)",
    "Vaccine-induced (N=4,274)",
    "Infection-induced (N=759)",
    "Naive (N=174)",
    "Hybrid: No confirmed (3915)",
    "Hybrid: Confirmed (62)",
    "Hybrid: Vaccinated before infection (761)",
    "Vaccine: No confirmed (2550)",
    "Vaccine: Confirmed (821)",
    "Vaccine: Vaccinated before infection (903)",
    "Infection: No confirmed (723)",
    "Infection: Confirmed (33)",
    "Infection: Vaccinated before infection (3)"
  )
)

links <- data.frame(
  source = c(
    0,0,0,0,
    1,1,1,
    2,2,2,
    3,3,3
  ),
  target = c(
    1,2,3,4,
    5,6,7,
    8,9,10,
    11,12,13
  ),
  value = c(
    4738,4274,759,174,
    3915,62,761,
    2550,821,903,
    723,33,3
  )
)

s <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  fontSize = 12,
  nodeWidth = 30,
  sinksRight = FALSE
)

htmlwidgets::saveWidget(s, "result/plot1/Figure0(A).html", selfcontained = TRUE)
library(webshot2)
webshot("result/plot1/Figure0(A).html",
        file = "result/plot1/Figure0(A).png",
        zoom = 4,
        vwidth = 1600,
        vheight = 1000)
library(magick)
img <- image_read("result/plot1/Figure0(A).png")






nodes2 <- data.frame(
  name = c(
    "Eligible (N=9,945)",
    "Hybrid-induced (N=4,738)",
    "Vaccine-induced (N=4,274)",
    "Infection-induced (N=759)",
    "Naive (N=174)",
    "Hybrid: Loss to follow-up (898)",
    "Hybrid: Vaccinated between waves (762)",
    "Hybrid: No infection (2515)",
    "Hybrid: Infection (563)",
    "Vaccine: Loss to follow-up (827)",
    "Vaccine: Vaccinated between waves (945)",
    "Vaccine: No infection (1543)",
    "Vaccine: Infection (959)",
    "Infection: Loss to follow-up (346)",
    "Infection: Vaccinated between waves (3)",
    "Infection: No infection (313)",
    "Infection: Infection (97)"
  )
)

links2 <- data.frame(
  source = c(
    0,0,0,0,
    1,1,1,1,
    2,2,2,2,
    3,3,3,3
  ),
  target = c(
    1,2,3,4,
    5,6,7,8,
    9,10,11,12,
    13,14,15,16
  ),
  value = c(
    4738,4274,759,174,
    898,762,2515,563,
    827,945,1543,959,
    346,3,313,97
  )
)

p2 <- sankeyNetwork(
  Links = links2,
  Nodes = nodes2,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  fontSize = 12,
  nodeWidth = 30,
  sinksRight = FALSE
)

htmlwidgets::saveWidget(p2, "result/plot1/Figure0(B).html", selfcontained = TRUE)

webshot("result/plot1/Figure0(B).html",
        file = "result/plot1/Figure0(B).png",
        zoom = 4,
        vwidth = 1600,
        vheight = 1000)

