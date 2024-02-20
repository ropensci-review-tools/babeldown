# deepl_update() works

    Code
      gert::git_ls(repo = dir)[, 1]
    Output
      # A tibble: 2 x 1
        path     
        <chr>    
      1 bla.es.md
      2 bla.md   

# deepl_update() works -- files in subdirectory

    Code
      gert::git_ls(repo = dir)[, 1]
    Output
      # A tibble: 2 x 1
        path         
        <chr>        
      1 pof/bla.es.md
      2 pof/bla.md   

