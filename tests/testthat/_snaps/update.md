# deepl_update() works

    Code
      fs::dir_tree(dir)
    Output
      /tmp/RtmpFwA8Jv/file63e02348b140
      \-- pof
          +-- bla.es.md
          \-- bla.md
    Code
      gert::git_diff(gert::git_log(repo = dir)[1, 1], repo = dir)
    Output
      # A tibble: 1 x 4
        status old        new        patch                                            
      * <chr>  <chr>      <chr>      <chr>                                            
      1 M      pof/bla.md pof/bla.md "diff --git a/pof/bla.md b/pof/bla.md\nindex 690~

