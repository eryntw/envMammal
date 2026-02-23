library(DiagrammeR)

grViz("
digraph coinr_workflow {
  graph [layout = dot, rankdir = TB, nodesep = 0.5, splines = ortho]

  node [shape = rectangle, style = filled, fontname = Helvetica]

  subgraph cluster_0 {
    label = '1. Inputs'; style = dashed; color = grey;
    input [label = 'iData (Values)\\niMeta (Metadata)', shape = folder, fillcolor = '#FFF2CC']
    new [label = 'new_coin()', fillcolor = '#CCE5FF']
  }

  subgraph cluster_1 {
    label = '2. Construction Pipeline'; style = dashed; color = blue;
    fillcolor = '#E6F2FF';
    denom [label = 'Denominate()\\n(Scaling Variables)']
    screen [label = 'Screen()\\n(Data Availability)']
    impute [label = 'Impute()\\n(Handling Missing Data)']
    treat [label = 'Treat()\\n(Outlier Treatment)']
    norm [label = 'Normalise()\\n(Common Scale)']
    agg [label = 'Aggregate()\\n(Final Weighting)']
  }

  subgraph cluster_2 {
    label = '3. Analysis & Results'; style = dashed; color = darkgreen;
    mva [label = 'get_PCA() / get_cronbach()\\n(Multivariate Analysis)']
    sens [label = 'get_sensitivity()\\n(Uncertainty Check)']
    plots [label = 'plot_* / get_results()\\n(Visualisation)', fillcolor = '#D5E8D4']
  }

  input -> new
  new -> denom
  denom -> screen
  screen -> impute
  impute -> treat
  treat -> norm
  norm -> agg
  agg -> mva
  mva -> sens
  sens -> plots

sens:e -> new:e [
  label = 'Regen()',
  style = dotted,
  color = red,
  penwidth = 1.5,
  constraint = false
]
}
")
