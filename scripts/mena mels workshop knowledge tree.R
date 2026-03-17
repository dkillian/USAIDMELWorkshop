# MENA MELS WORKSHOP
# knowledge tree




# Load the DiagrammeR package
library(DiagrammeR)

# Create a simple diagram using DiagrammeR
DiagrammeR::grViz("
digraph{
  
  # Define the layout
  graph [layout = dot, rankdir = TB]
  
  # Define nodes
  node [shape = rectangle, style = filled, color = lightblue]
  A [label = 'Policy\nframework']
  B [label = 'Potential\noutcomes']
  C [label = 'Experimental\ndesign']
  D [label = 'Quasi-experimental\ndesign']
  E [label = 'Rescuing impact\nmeasurements']
  F [label = 'TED Talk:\nGIS']
  G [label = 'Statistical\nmatching']
  H [label = 'Causal\nmodeling']
  I [label = 'Sampling']
  J [label = 'Data collection\nin conflict']
  K [label = 'Bayes\nRules']
  L [label = 'Machine\nlearning']
  M [label = 'Large Language\nModels / AI']
  N [label = 'Learning\nagendas']

  # Define edges (connections between nodes)
  A -> B
  B -> {C D H}
  C -> I
  D -> {E G}
  F
  H -> {N K}
  I -> {J K}
  K -> L
  L -> M

  # Learning points
  node [shape=rectangle, style=filled, color=plum2]
  AA [label='Fundamental\nproblem of\ncausal inference']
  BB [label='Randomization\nsolves the\nselection problem']
  CC [label='In the absence of\nrandomization, we look\nfor as-if randomization']
  DD [label='Newer estimators\nadjust for dynamic\ntreatment effects']
  EE [label='Causal modeling helps us\ndiagram relationships, identify\nbias, and structure our analysis']
  FF [label='We can make\ninferences using the\nlaws of probability']
  GG [label='When respondents mask\ntheir true responses, there\nare ways to uncover the truth']
  HH [label='We can use Bayes\nRule to draw a\nlogic model from data']
  II [label='With enough training\ndata, algorithms become\ndigital assistants']
  
  # From sessions to learning
  B -> AA
  {C AA} -> BB
  {D G BB} -> CC
  E -> DD
  {H N CC} -> EE # HH ? 
  {K} -> FF
  {J} -> GG
  {H L FF} -> HH
  {M HH} -> II
}
")
