
# single ---- 

draw.single.venn(
  area=20
)

# pairwise ---- 

pair <- draw.pairwise.venn(
  area1=10,
  area2=10,
  cross.area=4,
  #category=c("Variable 1","Variable 2"),
  fill=c(usaid_red, usaid_blue)
)

grid.newpage()


# triple ---- 

?draw.triple.venn

draw.triple.venn(
  area1=20,
  area2=20,
  area3=20,
  n12=4,
  n13=4,
  n23=4,
  n123=1)


  cross.area=4,
  category=c("Variable 1","Variable 2"),
  fill=c(usaid_red, usaid_blue)
)

grid.newpage()

# quad ---- 

draw.quad.venn(
  area1=40,
  area2=40,
  area3=40,
  area4=40,
  n12=4,
  n13=4,
  n14=4,
  n23=4,
  n24=4,
  n34=4,
  n123=1,
  n124=1,
  n134=1,
  n234=1,
  n1234=1
)

# eulerr double ---- 

grid.newpage()

e1 <- euler(
  c(
    A=4,
    B=4,
    "A&B"=1
  )
)

e1
plot(e1, 
     quantities=F, 
     counts=F, 
     labels=c("Variable 1","Variable 2"),
     lty=1,
     fills=c(usaid_red, usaid_blue),
     alpha=.5)



# eulerr triple ---- 

e2 <- euler(
  c(
    A=7,
    B=7,
    C=7,
    "A&B"=2,
    "A&C"=2,
    "B&C"=2,
    "A&B&C"=10
  )
)

virPal <- viridis(10)
virPal[1:4]

  # shared variance

plot(e2,
     quantities=list(labels=c("", 
                              "",
                              "",
                              "",
                              "",
                              "",
                              "Shared variance")),
     labels=c("Variable 2","Variable 3","Variable 1", "test"),
     #lty=5)
     fills=list(fill=c("grey90",
                       "grey90",
                       "grey90",
                       "grey90",
                       "grey90",
                       "grey90",
                       "plum2")),
     alpha=.5)

  # common variance

plot(e2,
     quantities=list(labels=c("Unique variance", 
                              "Unique variance",
                              "Unique variance",
                              "",
                              "",
                              "",
                              "Common variance")),
     labels=c("Variable 2","Variable 3","Variable 1"),
     #lty=5)
     fills=list(fill=c("lightblue",
                       "lightblue",
                       "lightblue",
                       "plum2",
                       "plum2",
                       "plum2",
                       "plum2")),
     alpha=.5)

ggsave("scripts/Machine learning/test.png")







