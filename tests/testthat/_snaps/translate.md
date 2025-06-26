# deepl_translate() errors well

    Code
      deepl_translate("non-existing-file")
    Condition
      Error in `deepl_translate()`:
      ! Can't find path "non-existing-file".

---

    Code
      deepl_translate(file, out_path = out_file, glossary_name = "non-existing-glossary",
        source_lang = "en", target_lang = "es")
    Condition
      Error in `deepl_translate()`:
      ! Can't find glossary_name "non-existing-glossary".
      i Check the spelling, or create it with `upsert_glossary()`.

---

    Code
      deepl_translate(file, out_path = out_file, formality = "non-existing-formality",
        source_lang = "en", target_lang = "es")
    Condition
      Error in `deepl_translate()`:
      ! `formality` must be one of "default", "more", "less", "prefer_more", or "prefer_less", not "non-existing-formality".

---

    Code
      deepl_translate(file, out_path = "this/path/does/not/exist", source_lang = "en",
        target_lang = "es")
    Condition
      Error in `deepl_translate()`:
      ! The folder to save out_path ("this/path/does/not") does not exist. You might have mistyped it, or it needs to be created.

# deepl_translate() does not break TOML

    Code
      lines[1:3]
    Output
      [1] "+++"                         "title = \"Community Calls\""
      [3] "+++"                        

# deepl_translate() handles equations well

    Code
      math_lines[4]
    Output
      [1] "E = m \\times c^2"

---

    Code
      sub(".*que ", "", math_lines[7])
    Output
      [1] "$\\alpha=1$ y $b$ no está definida."

---

    Code
      math_lines[9]
    Output
      [1] "$i_t = j_t$"

# deepl_translate() handles equations+footnote well

    Code
      foot_math_lines
    Output
      [1] "See $a$ an equation with a note afterwards [^1] ."
      [2] ""                                                 
      [3] "[^1]: a footnote."                                
      [4] ""                                                 
      [5] ""                                                 

# deepl_translate() handles equations with curly well

    Code
      foot_curly_lines[5]
    Output
      [1] "$a_{ij}$"

# deepl_translate() protects fenced divs

    Code
      brio::read_lines(out_path)
    Output
      [1] "Algo de texto."                                 
      [2] ""                                               
      [3] "::: footer"                                     
      [4] " [¡Eres astuto!]{style=\"color: transparent;\"}"
      [5] ":::"                                            
      [6] ""                                               
      [7] ""                                               

