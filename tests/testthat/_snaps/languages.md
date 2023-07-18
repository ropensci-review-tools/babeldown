# examine_source_lang() works

    Code
      examine_source_lang("englisssh")
    Error <rlang_error>
      Can't find language "englisssh" as source language code or name.
      Maybe a typo or a missing regional code?
      i Run `babeldown::deepl_languages(type = 'source')` to get supported languages.

# examine_target_lang() works

    Code
      examine_target_lang("English")
    Error <rlang_error>
      Can't find language "English" as target language code or name.
      Maybe a typo or a missing regional code?
      i Run `babeldown::deepl_languages(type = 'target')` to get supported languages.

