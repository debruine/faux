cat("Welcome to faux.\n")
require(usethis)
require(testthat)
require(faux)

setHook("rstudio.sessionInit", function(newSession) {
  prefs <- list(
    blinking_cursor = TRUE,
    document_author = "Lisa DeBruine",
    editor_theme = "Merbivore Soft", # ?
    font_size_points = 16L,
    help_font_size_points = 12L,
    insert_matching = FALSE,
    margin_column = 80L,
    shiny_viewer_type = "browser",
    rainbow_parentheses = FALSE,
    save_workspace = "never",
    show_function_signature_tooltips = TRUE,
    show_help_tooltip_on_idle = TRUE,
    show_indent_guides = TRUE,
    show_invisibles = TRUE,
    show_line_numbers = TRUE,
    show_margin = TRUE,
    soft_wrap_r_files = TRUE,
    soft_wrap_rmd_files = TRUE,
    tab_completion = TRUE,
    toolbar_visible = FALSE,
    use_spaces_for_tab = TRUE,
    vertically_align_arguments_indent = TRUE
  )
  invisible({
    mapply(rstudioapi::writeRStudioPreference, 
           names(prefs), prefs)
  })
}, action = "append")

