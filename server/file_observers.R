# R/server/file_observers.R

# Initialize shinyFiles for directory selection
shinyFiles::shinyDirChoose(
  input,
  id = 'select_dir_btn',
  roots = c(
    home = '~',
    wd = '.'
  ),
  session = session
)

# Observer for when a directory is selected via shinyFiles dialog
observeEvent(input$select_dir_btn, {
  clear_messages(message_rv) # Clear any existing messages when dialog is opened/closed

  if (!is.integer(input$select_dir_btn)) {
    path <- shinyFiles::parseDirPath(
      c(home = '~', wd = '.'),
      input$select_dir_btn
    )

    if (length(path) > 0) {
      selected_dir_reactive(path)
      message_rv(list(type = "success", text = paste("Output directory selected:", path)))
      print(paste("Directory selected and stored (ShinyFiles):", path))
    } else {
      selected_dir_reactive(NULL)
      message_rv(list(type = "warning", text = "Directory selection cancelled or invalid path."))
      print("Directory selection cancelled or invalid path parsed.")
    }
  } else {
    message_rv(list(type = "info", text = "Directory selection dialog closed."))
    print("Directory selection dialog closed without selecting a folder.")
  }
})

# Enable/Disable "Select Output Directory" button based on the auto-save switch
observeEvent(input$enable_directory, {
  if (input$enable_directory) {
    shinyjs::enable("select_dir_btn")
    message_rv(list(type = "info", text = "Auto-save enabled. Please select an output directory."))
  } else {
    shinyjs::disable("select_dir_btn")
    selected_dir_reactive(NULL)
    message_rv(list(type = "info", text = "Auto-save disabled."))
    print("Auto-save disabled. Directory path forgotten.")
  }
})