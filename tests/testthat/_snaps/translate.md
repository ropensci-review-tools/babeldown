# deepl_translate() errors well

    Code
      deepl_translate("non-existing-file")
    Error <rlang_error>
      Can't find path "non-existing-file".

---

    Code
      deepl_translate(file, glossary_name = "non-existing-glossary", source_lang = "en",
        target_lang = "es")
    Error <rlang_error>
      Can't find glossary_name "non-existing-glossary".
      i Check the spelling, or create it with `upsert_glossary()`.

---

    Code
      deepl_translate(file, formality = "non-existing-formality", source_lang = "en",
        target_lang = "es")
    Error <rlang_error>
      `formality` must be one of "default", "more", "less", "prefer_more", or "prefer_less", not "non-existing-formality".

