# ==============================================================================
# Kel 5 | STATKOM Project R-GUI dengan tabel
# ==============================================================================

# to-do:
# 1. implement fungsi buat uji hipotesis
# 2. Cek error stat kalau non numeric di input
# 3. Modifikasi window uji hipotesis var
# 4. Window petunjuk dan tentang kami, sama topmenu nya
# 5. Fungsi Error state output buat gampang debbuging (low prio seng penting mlaku)

library(tcltk)
library(tcltk2)
tclRequire("Tktable")

# WARNA GLOBAL
options(
  Rgui.theme_bl = list(
    primary = "#dce3ec",
    secondary = "#294563",
    background = "#f5f7fa",
    border = "#bcc3cc"
  )
)

# IMPORTANT FOR TABLE, MEMORI PUSAT UNTUK DATA DIBACA
if (exists("APP_STATE")) {
  rm(APP_STATE)
}
APP_STATE <- new.env()
APP_STATE$data <- data.frame() # Ini yang bakal di read buat fungsi uji
APP_STATE$variable_info <- data.frame(
  name = character(),
  type = character(),
  label = character(),
  stringsAsFactors = FALSE
)
# IMPORTANT, appstate not <- ? Modularity pas di call fungsi sumvar1 etc. ; to prevent data aplikasi mixed dengan global var (terisolasi) ; kalau perlu reset data refrence ny gampang serta clean

# ==============================================================================
# FUNGSI DIALOG PROPERTIES VAR TABLE (Double click col header to access)
# ==============================================================================
open_column_properties <- function(col_idx, state, data_array) {
  if (col_idx < 1) {
    return()
  }

  raw_val <- data_array[[0, col_idx]]
  old_name <- if (is.null(raw_val)) {
    paste0("Var", col_idx)
  } else {
    as.character(raw_val)
  }
  if (length(old_name) == 0 || is.na(old_name) || trimws(old_name) == "") {
    old_name <- paste0("Var", col_idx)
  }

  # main dialogue struct
  dlg <- tktoplevel()
  tkwm.title(dlg, paste("Column Properties -", old_name))
  tkwm.geometry(dlg, "300x200")

  # Input nama var
  var_name <- tclVar(old_name)
  current_type <- state$variable_info$type[state$variable_info$name == old_name]
  if (length(current_type) == 0) {
    current_type <- "numeric"
  }

  # Tipe data (numeric, char, etc.)
  var_type <- tclVar(current_type)

  # Input label variabel just in case
  current_label <- state$variable_info$label[
    state$variable_info$name == old_name
  ]
  if (length(current_label) == 0) {
    current_label <- ""
  }
  var_label <- tclVar(current_label)

  frame_form <- tkframe(dlg, padx = 10, pady = 10)
  tkpack(frame_form, fill = "both", expand = TRUE)

  tkgrid(
    tklabel(frame_form, text = "Name:"),
    row = 0,
    column = 0,
    sticky = "w",
    pady = 5
  )
  tkgrid(
    tkentry(frame_form, textvariable = var_name, width = 20),
    row = 0,
    column = 1,
    sticky = "w"
  )
  tkgrid(
    tklabel(frame_form, text = "Type:"),
    row = 1,
    column = 0,
    sticky = "w",
    pady = 5
  )
  tkgrid(
    tk2combobox(
      frame_form,
      textvariable = var_type,
      values = c("numeric", "character", "factor", "logical"),
      state = "readonly",
      width = 18
    ),
    row = 1,
    column = 1,
    sticky = "w"
  )
  tkgrid(
    tklabel(frame_form, text = "Label:"),
    row = 2,
    column = 0,
    sticky = "w",
    pady = 5
  )
  tkgrid(
    tkentry(frame_form, textvariable = var_label, width = 20),
    row = 2,
    column = 1,
    sticky = "w"
  )

  btn_frame <- tkframe(dlg, pady = 10)
  tkpack(btn_frame)

  on_ok <- function() {
    new_name <- tclvalue(var_name)
    new_type <- tclvalue(var_type)
    new_label <- tclvalue(var_label)
    if (new_name == "") {
      tkmessageBox(message = "Nama kolom tidak boleh kosong!", icon = "warning")
      return()
    }

    data_array[[0, col_idx]] <- new_name
    if (old_name %in% names(state$data)) {
      names(state$data)[names(state$data) == old_name] <- new_name
    }

    if (old_name %in% state$variable_info$name) {
      row_idx <- which(state$variable_info$name == old_name)
      state$variable_info$name[row_idx] <- new_name
      state$variable_info$type[row_idx] <- new_type
      state$variable_info$label[row_idx] <- new_label
    } else {
      state$variable_info <- rbind(
        state$variable_info,
        data.frame(name = new_name, type = new_type, label = new_label)
      )
    }
    tkdestroy(dlg)
    tkmessageBox(
      title = "Sukses",
      message = "Properti kolom diperbarui!",
      icon = "info"
    )
  }

  btn_ok <- tkbutton(btn_frame, text = "OK", width = 10, command = on_ok)
  btn_cancel <- tkbutton(
    btn_frame,
    text = "Cancel",
    width = 10,
    command = function() tkdestroy(dlg)
  )
  tkpack(btn_ok, side = "left", padx = 5)
  tkpack(btn_cancel, side = "left", padx = 5)
}

# ==============================================================================
# FUNGSI RESET DATA
# ==============================================================================
reset_app_state <- function(state) {
  ans <- tkmessageBox(
    title = "Konfirmasi Reset",
    message = "Hapus semua data?",
    type = "yesno",
    icon = "warning"
  )
  if (tclvalue(ans) == "yes") {
    state$data <- data.frame()
    state$variable_info <- data.frame(
      name = character(),
      type = character(),
      label = character()
    )
    for (i in 0:100) {
      for (j in 0:30) {
        state$data_array[[i, j]] <- NULL
      }
    }
    tkmessageBox(title = "Sukses", message = "Data di-reset!", icon = "info")
  }
}

# ==============================================================================
# FUNGSI TABEL
# ==============================================================================
create_data_view <- function(parent, state) {
  tryCatch(
    {
      # Frame pembungkus fleksibel
      frame_data <- tkframe(
        parent,
        background = getOption("Rgui.theme_bl")$background
      )
      tkpack(frame_data, fill = "both", expand = TRUE, padx = 5, pady = 5)

      data_array <- tclArray()
      df <- state$data

      # Nomor urut dan Header pojok
      data_array[[0, 0]] <- "Var"
      for (i in 1:100) {
        data_array[[i, 0]] <- as.character(i)
      }

      if (ncol(df) > 0) {
        for (j in 1:ncol(df)) {
          data_array[[0, j]] <- names(df)[j]
        }
        for (i in 1:nrow(df)) {
          for (j in 1:ncol(df)) {
            data_array[[i, j]] <- as.character(df[i, j])
          }
        }
      }

      data_table <- tkwidget(
        frame_data,
        "table",
        variable = data_array,
        rows = 100,
        cols = 30,
        titlerows = 1,
        titlecols = 1,
        selectmode = "extended",
        colwidth = 12,
        rowheight = 2,
        background = "#ffffff",
        foreground = "#000000",
        multiline = TRUE,
        rowseparator = "\n",
        colseparator = "\t"
      )

      # Var column 0, width = 6
      tcl(data_table, "width", 0, 6)

      # Warna saat edit & Enter behavior (need review, added cause when edit the default font color is same as cell color - unreadable)
      tcl(
        data_table,
        "tag",
        "configure",
        "active",
        "-foreground",
        "#000000",
        "-background",
        "#e6f2ff"
      )
      tcl("bind", data_table, "<Return>", "{ %W insert active \"\\n\"; break }")

      # Scrollbar
      xscr <- tkscrollbar(
        frame_data,
        orient = "horizontal",
        repeatinterval = 5,
        command = function(...) tkxview(data_table, ...)
      )
      yscr <- tkscrollbar(
        frame_data,
        orient = "vertical",
        repeatinterval = 5,
        command = function(...) tkyview(data_table, ...)
      )
      tkconfigure(data_table, xscrollcommand = function(...) tkset(xscr, ...))
      tkconfigure(data_table, yscrollcommand = function(...) tkset(yscr, ...))

      # Grid Layout
      tkgrid(data_table, row = 0, column = 0, sticky = "nsew")
      tkgrid(yscr, row = 0, column = 1, sticky = "ns")
      tkgrid(xscr, row = 1, column = 0, sticky = "ew")
      tkgrid.rowconfigure(frame_data, 0, weight = 1)
      tkgrid.columnconfigure(frame_data, 0, weight = 1)

      # Double click header to insert variable name
      tkbind(data_table, "<Double-Button-1>", function() {
        row <- as.integer(tclvalue(tkindex(data_table, "active", "row")))
        col <- as.integer(tclvalue(tkindex(data_table, "active", "col")))
        if (
          length(row) > 0 && !is.na(row) && row == 0 && !is.na(col) && col > 0
        ) {
          open_column_properties(col, state, data_array)
        }
      })

      state$data_table_widget <- data_table
      state$data_array <- data_array
    },
    error = function(e) {
      cat("\n=== ERROR create_data_view ===\n", e$message, "\n")
    }
  )
}

# ==============================================================================
# STRUKTUR TOPMENU & FUNGSI PEMBUAT MENU
# ==============================================================================
# Struktur list dibuat berlapis (list of lists), urutan sesuai no. di comment dari kiri ke kanan ; Modular bisa ditambah nanti, cek developer note di guidebook
Submenu_container <- list(
  # Menu 1: File
  list(
    label = "File",
    children = list(
      list(
        label = "Import Data",
        children = list(
          list(label = "Excel", command = "Excel_input"),
          list(label = "SPSS", command = "SPSS_input"),
          list(label = "CSV", command = "CSV_input"),
          list(label = "text", command = "text_input")
        )
      )
    )
  ),
  # Menu 2: Pengujian
  list(
    label = "Pengujian Hipotesis",
    children = list(
      list(
        label = "Variansi",
        children = list(
          list(label = "Satu Populasi", command = "sumvar1"),
          list(label = "Dua Populasi", command = "sumvar2")
        )
      )
    )
  )
)
# Menu 3: View
# Menu 4: Petunjuk
# Menu 5: Tentang kami

# Fungsi build_menu for the last child behavior is run command
build_menu <- function(parent_menu, structure) {
  for (item in structure) {
    if (!is.null(item$children)) {
      submenu <- tkmenu(parent_menu, tearoff = FALSE)
      tkadd(parent_menu, "cascade", label = item$label, menu = submenu)
      build_menu(submenu, item$children)
    } else {
      if (!is.null(item$command)) {
        func_name <- item$command
        tkadd(parent_menu, "command", label = item$label, command = function() {
          if (exists(func_name, envir = .GlobalEnv)) {
            do.call(get(func_name, envir = .GlobalEnv), list())
          } else {
            tkmessageBox(
              message = paste("Fungsi", func_name, "belum didefinisikan."),
              icon = "warning"
            )
          }
        })
      }
    }
  }
}

# ==============================================================================
#  Main GUI Build
# ==============================================================================
Start_open <- function() {
  Main_page <- tktoplevel()
  tkwm.title(Main_page, "Statistika - Software Uji Hipotesis")
  tkconfigure(Main_page, background = getOption("Rgui.theme_bl")$primary)

  # Main window body
  tkwm.geometry(Main_page, "1050x700")
  tkwm.minsize(Main_page, 800, 600)

  # Topmenu
  topmenu <- tkmenu(Main_page, tearoff = FALSE)
  tkconfigure(Main_page, menu = topmenu)
  build_menu(topmenu, Submenu_container)

  # Main Container (for table)
  main_container <- tkframe(
    Main_page,
    background = getOption("Rgui.theme_bl")$primary
  )
  tkpack(main_container, fill = "both", expand = TRUE, padx = 10, pady = 10)

  # button container (redraw bug, need fix)
  btn_frame <- tkframe(
    main_container,
    background = getOption("Rgui.theme_bl")$primary
  )
  tkpack(btn_frame, fill = "x", pady = 5)

  btn_print <- tkbutton(
    btn_frame,
    text = "Print Data (Console)",
    command = function() {
      cat("\n=== APP_STATE$data ===\n")
      print(APP_STATE$data)
      cat("\n=== APP_STATE$variable_info ===\n")
      print(APP_STATE$variable_info)
    }
  )
  tkpack(btn_print, side = "left", padx = 5)

  btn_reset <- tkbutton(btn_frame, text = "Reset Data", command = function() {
    reset_app_state(APP_STATE)
  })
  tkpack(btn_reset, side = "left", padx = 5)

  # Render table ke main container
  create_data_view(main_container, APP_STATE)
}

Start_open()
