flowchart <- DiagrammeR::grViz("digraph flowchart {
      
      graph [layout = dot,
             rankdir = TB,
             nodesep = 1.4,
             ranksep = 1.2]
      # ranksep and nodesep
      
      # edge attributes
      edge [color = gray]
      
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']
      tab6 [label = '@@6']
      tab7 [label = '@@7']
      tab8 [label = '@@8']
      tab9 [label = '@@9']
      tab10 [label = '@@10']
      tab11 [label = '@@11']
      tab12 [label = '@@12']
      tab13 [label = '@@13']
      tab14 [label = '@@14']
      tab15 [label = '@@15']
      tab16 [label = '@@16']
      tab17 [label = '@@17']
      tab18 [label = '@@18']
      tab19 [label = '@@19']
      
      # assigning ranks
      {rank = same; tab2; tab3;}
      {rank = same; tab4; tab15;}
      {rank = same; tab5; tab8; tab16;}
      {rank = same; tab12; tab9;}
      {rank = max; tab6; tab7; tab13; tab14; tab10; tab11; tab18; tab17; tab19;}
      
      # edge definitions with the node IDs
      tab1 -> tab2 [label='Yes', minlen = 2];
      tab1 -> tab3 [label='No', minlen = 2];
      tab3 -> tab4 [label='Testing'];
      tab3 -> tab15 [label='Etsimating'];
      tab4 -> tab5 [label='=0'];
      tab4 -> tab8 [label='>0'];
      tab8 -> tab9 [label='Frequentist'];
      tab8 -> tab12 [label='Bayesian'];
      tab9 -> tab10 [label='Single effect size'];
      tab9 -> tab11 [label='Range of effect sizes'];
      tab12 -> tab13 [label='Yes'];
      tab12 -> tab14 [label='No'];
      tab15 -> tab16 [label='Frequentist'];
      tab15 -> tab19 [label='Bayesian'];
      tab5 -> tab7 [label='Bayesian'];
      tab5 -> tab6 [label='Frequentist'];
      tab16 -> tab18 [label='No'];
      tab16 -> tab17 [label='Yes'];
      }
      
      # assgining labels
      [1]: flowchart_data[1, 2]
      [2]: flowchart_data[2, 2]
      [3]: flowchart_data[3, 2]
      [4]: flowchart_data[4, 2]
      [5]: flowchart_data[5, 2]
      [6]: flowchart_data[6, 2]
      [7]: flowchart_data[7, 2]
      [8]: flowchart_data[8, 2]
      [9]: flowchart_data[9, 2]
      [10]: flowchart_data[10, 2]
      [11]: flowchart_data[11, 2]
      [12]: flowchart_data[12, 2]
      [13]: flowchart_data[13, 2]
      [14]: flowchart_data[14, 2]
      [15]: flowchart_data[15, 2]
      [16]: flowchart_data[16, 2]
      [17]: flowchart_data[17, 2]
      [18]: flowchart_data[18, 2]
      [19]:flowchart_data[19, 2]
      ")

 flowchart_svg <- DiagrammeRsvg::export_svg(flowchart)
 flowchart_raw <- charToRaw(flowchart_svg)
 rsvg::rsvg_png(flowchart_raw, "inst/app/www/flowchart.png")
