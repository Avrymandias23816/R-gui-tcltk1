# ==============================================================================
# Kel 5 | STATKOM Project R-GUI dengan tabel
# ==============================================================================

# to-do:
# 1. implement fungsi buat uji hipotesis [V]
# 2. Cek error stat kalau non numeric di input
# 3. Modifikasi window uji hipotesis var [V]
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

# Base Theme
tcl("ttk::style", "theme", "use", "default")

# Warna Selection/Highlight Tabel pakai Global Color
theme_colors <- getOption("Rgui.theme_bl")


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

APP_STATE$alpha <- 0.05
APP_STATE$alternative <- "two.sided"

# ==============================================================================
# FUNGSI SINKRONISASI TABEL KE DATA FRAME
# ==============================================================================
# Karena Tktable menyimpan data di tclArray, kita perlu memindahkannya ke APP_STATE$data
sync_table_to_data <- function(state) {
  if (is.null(state$data_array)) {
    return()
  }

  max_row <- 0
  max_col <- 0

  # Cari batas maksimal baris dan kolom yang terisi
  for (j in 1:30) {
    for (i in 1:1000) {
      val <- state$data_array[[i, j]]
      if (!is.null(val)) {
        val_str <- as.character(val)
        if (nchar(trimws(val_str)) > 0) {
          if (i > max_row) {
            max_row <- i
          }
          if (j > max_col) max_col <- j
        }
      }
    }
  }

  if (max_row == 0 || max_col == 0) {
    state$data <- data.frame()
    return()
  }

  df <- data.frame(
    matrix(NA, nrow = max_row, ncol = max_col),
    stringsAsFactors = FALSE
  )

  for (j in 1:max_col) {
    # Konversi header kolom ke character
    col_name_tcl <- state$data_array[[0, j]]
    col_name <- if (is.null(col_name_tcl)) {
      paste0("Var", j)
    } else {
      trimws(as.character(col_name_tcl))
    }
    if (col_name == "") {
      col_name <- paste0("Var", j)
    }
    names(df)[j] <- col_name

    for (i in 1:max_row) {
      val <- state$data_array[[i, j]]
      val_str <- if (is.null(val)) "" else trimws(as.character(val))

      # PENTING: Ubah "{}" menjadi kosong
      if (val_str == "{}") {
        val_str <- ""
      }

      df[i, j] <- if (val_str == "") NA else val_str
    }
  }

  # Konversi otomatis ke numerik jika memungkinkan agar uji statistik jalan
  for (j in 1:max_col) {
    non_na <- df[[j]][!is.na(df[[j]])]
    if (length(non_na) > 0) {
      num_vals <- suppressWarnings(as.numeric(non_na))
      if (!any(is.na(num_vals))) {
        df[[j]] <- as.numeric(df[[j]])
      }
    }
  }

  state$data <- df
}

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

    # Hanya hapus dari j=1 ke atas, j=0 (kolom nomor) dibiarkan
    for (i in 0:1000) {
      for (j in 1:30) {
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
      for (i in 1:1000) {
        data_array[[i, 0]] <- as.character(i)
      }

      if (ncol(df) > 0) {
        for (j in seq_len(ncol(df))) {
          data_array[[0, j]] <- names(df)[j]
        }
        for (i in seq_len(nrow(df))) {
          for (j in seq_len(ncol(df))) {
            val <- df[[j]][i]
            # Hanya render jika ada isinya
            if (
              !is.na(val) &&
                trimws(as.character(val)) != "" &&
                as.character(val) != "NA"
            ) {
              data_array[[i, j]] <- as.character(val)
            }
          }
        }
      }

      # Setting table
      data_table <- tkwidget(
        frame_data,
        "table",
        variable = data_array,
        rows = 1001, # 1000 baris data + 1 baris header
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
        colseparator = "\t",
        sparsearray = TRUE,
        cache = TRUE # Stabilitas scrollbar
      )

      # ENABLE MOUSE SCROLL WHEEL (Cross-Platform)
      tcl(
        "bind",
        data_table,
        "<MouseWheel>",
        "%W yview scroll [expr {- (%D / 120) * 2}] units"
      )

      tcl(
        "bind",
        data_table,
        "<Shift-MouseWheel>",
        "%W xview scroll [expr {- (%D / 120) * 2}] units"
      )

      tcl("bind", data_table, "<Button-4>", "%W yview scroll -2 units")
      tcl("bind", data_table, "<Button-5>", "%W yview scroll 2 units")

      # Var column 0, width = 6
      tcl(data_table, "width", 0, 6)

      # tag "sel" used to style and configure, cek dokumentasi Tktable
      tcl(
        data_table,
        "tag",
        "configure",
        "sel",
        "-background",
        theme_colors$border,
        "-foreground",
        "#ffffff"
      )

      # Warna saat sel sedang diedit (Active)
      tcl(
        data_table,
        "tag",
        "configure",
        "active",
        "-background",
        theme_colors$border,
        "-foreground",
        "black",
        "-relief",
        "groove"
      )

      # Warna header
      tcl(
        data_table,
        "tag",
        "configure",
        "title",
        "-background",
        theme_colors$secondary,
        "-foreground",
        theme_colors$border,
        "-font",
        "Helvetica 9",
        "-relief",
        "flat"
      )

      # Scrollbar (ttk::scrollbar TIDAK punya opsi -width, itu hanya ada
      # di classic scrollbar. Jadi width dihilangkan.)
      xscr <- ttkscrollbar(
        frame_data,
        orient = "horizontal",
        command = function(...) tkxview(data_table, ...)
      )
      yscr <- ttkscrollbar(
        frame_data,
        orient = "vertical",
        command = function(...) tkyview(data_table, ...)
      )

      # Hubungkan viewport Tktable ke scrollbar agar posisi thumb sinkron
      tkconfigure(
        data_table,
        xscrollcommand = function(...) tkset(xscr, ...),
        yscrollcommand = function(...) tkset(yscr, ...)
      )

      # Konfigurasi style scrollbar
      for (orient in c("Vertical", "Horizontal")) {
        tcl(
          "ttk::style",
          "configure",
          paste0(orient, ".TScrollbar"),
          "-background",
          theme_colors$secondary, # Warna thumb (slider)
          "-troughcolor",
          theme_colors$primary, # <-- Warna latar (trough)
          "-arrowcolor",
          "#ffffff", # Warna panah
          "-bordercolor",
          theme_colors$border,
          "-lightcolor",
          theme_colors$primary,
          "-darkcolor",
          theme_colors$border,
          "-arrowsize",
          20
        )

        # Efek hover/press
        tcl(
          "ttk::style",
          "map",
          paste0(orient, ".TScrollbar"),
          "-background",
          sprintf(
            "active %s pressed %s !active %s",
            theme_colors$primary,
            theme_colors$border,
            theme_colors$secondary
          )
        )
      }

      # Grid Layout
      tkgrid(data_table, row = 0, column = 0, sticky = "nsew")
      tkgrid(yscr, row = 0, column = 1, sticky = "ns")
      tkgrid(xscr, row = 1, column = 0, sticky = "ew")

      # Hanya tabel yang boleh membesar (weight = 1)
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
      # Tampilkan error sebagai dialog agar tidak silently swallowed
      tkmessageBox(
        title = "create_data_view error",
        message = e$message,
        icon = "error"
      )
    }
  )
}


tcl(
  "ttk::style",
  "configure",
  "Custom.TButton",
  "-background",
  theme_colors$primary,
  "-foreground",
  theme_colors$secondary,
  "-relief",
  "flat",
  "-borderwidth",
  0,
  "-lightcolor",
  theme_colors$primary,
  "-darkcolor",
  theme_colors$border,
  "-padding",
  6,
  "-font",
  "Helvetica 10 bold"
)

tcl(
  "ttk::style",
  "map",
  "Custom.TButton",
  "-background",
  sprintf(
    "active %s pressed %s !active %s",
    theme_colors$border,
    theme_colors$secondary,
    theme_colors$primary
  ),
  "-foreground",
  sprintf(
    "active %s pressed %s !active %s",
    theme_colors$secondary,
    theme_colors$background,
    theme_colors$secondary
  )
)


# ==============================================================================
# FUNGSI MEMASUKKAN DATA KE TABEL
# ==============================================================================
process_imported_df <- function(df, filename) {
  if (ncol(df) == 0) {
    tkmessageBox(
      message = "File kosong atau format tidak sesuai.",
      icon = "warning"
    )
    return()
  }

  if (is.null(APP_STATE$data_array)) {
    APP_STATE$data <- df
    tkmessageBox(
      message = "Data disimpan di memori, tapi tabel belum siap.",
      icon = "warning"
    )
    return()
  }

  # 1. Bersihkan tabel lama (GUNAKAN NULL)
  for (i in 1:1000) {
    for (j in 1:30) {
      APP_STATE$data_array[[i, j]] <- NULL
    }
  }

  max_cols <- min(ncol(df), 30)
  max_rows <- min(nrow(df), 1000)

  # 2. Set nama kolom
  for (j in seq_len(max_cols)) {
    col_name <- names(df)[j]
    if (is.null(col_name) || is.na(col_name) || nchar(trimws(col_name)) == 0) {
      col_name <- paste0("Var", j)
    }
    APP_STATE$data_array[[0, j]] <- col_name
  }

  # 3. Masukkan data
  for (i in seq_len(max_rows)) {
    for (j in seq_len(max_cols)) {
      val <- df[[j]][i]

      # Jika NA, jadikan "" untuk pengecekan. Jika ada teks, hapus spasi berlebih.
      val_str <- if (is.na(val)) "" else trimws(as.character(val))

      # Jika ada isinya (bukan "" dan bukan "NA"), masukkan datanya
      if (val_str != "" && val_str != "NA") {
        APP_STATE$data_array[[i, j]] <- val_str
      } else {
        # JIKA KOSONG: Biarkan NULL (jangan diisi spasi atau string kosong)
        APP_STATE$data_array[[i, j]] <- NULL
      }
    }
  }

  APP_STATE$data <- df
  tcl("update") # Refresh tabel

  tkmessageBox(
    title = "Import Berhasil",
    message = paste(
      "Berhasil import:",
      basename(filename),
      "\nBaris:",
      nrow(df),
      "\nKolom:",
      ncol(df)
    ),
    icon = "info"
  )
}

# ==============================================================================
# FUNGSI FILE IMPORT
# ==============================================================================

CSV_input <- function() {
  path <- tclvalue(tkgetOpenFile(
    title = "Pilih File CSV",
    filetypes = "{{CSV} {.csv}} {{All Files} *}"
  ))
  if (nchar(path) == 0) {
    return()
  }

  tryCatch(
    {
      # Gunakan header = TRUE agar baris pertama jadi nama kolom
      df <- read.csv(path, header = TRUE, stringsAsFactors = FALSE)
      process_imported_df(df, path)
    },
    error = function(e) {
      tkmessageBox(message = paste("Gagal:", e$message), icon = "error")
    }
  )
}

text_input <- function() {
  path <- tclvalue(tkgetOpenFile(
    title = "Pilih File Text",
    filetypes = "{{Text} {.txt}} {{All Files} *}"
  ))
  if (nchar(path) == 0) {
    return()
  }

  tryCatch(
    {
      df <- read.table(path, header = TRUE, stringsAsFactors = FALSE)
      process_imported_df(df, path)
    },
    error = function(e) {
      tkmessageBox(message = paste("Gagal:", e$message), icon = "error")
    }
  )
}

Excel_input <- function() {
  path <- tclvalue(tkgetOpenFile(
    title = "Pilih File Excel",
    filetypes = "{{Excel} {.xlsx .xls}} {{All Files} *}"
  ))
  if (nchar(path) == 0) {
    return()
  }

  if (!requireNamespace("readxl", quietly = TRUE)) {
    tkmessageBox(message = "Package 'readxl' belum terinstall.", icon = "error")
    return()
  }

  tryCatch(
    {
      # TAMBAHKAN na = c("", "NA") DI SINI
      df <- readxl::read_excel(path, col_names = TRUE, na = c("", "NA"))
      process_imported_df(as.data.frame(df), path)
    },
    error = function(e) {
      tkmessageBox(message = paste("Gagal:", e$message), icon = "error")
    }
  )
}

SPSS_input <- function() {
  path <- tclvalue(tkgetOpenFile(
    title = "Pilih File SPSS",
    filetypes = "{{SPSS} {.sav}} {{All Files} *}"
  ))
  if (nchar(path) == 0) {
    return()
  }

  if (!requireNamespace("haven", quietly = TRUE)) {
    tkmessageBox(message = "Package 'haven' belum terinstall.", icon = "error")
    return()
  }

  tryCatch(
    {
      df <- haven::read_sav(path)
      process_imported_df(as.data.frame(df), path)
    },
    error = function(e) {
      tkmessageBox(message = paste("Gagal:", e$message), icon = "error")
    }
  )
}

# ==============================================================================
# FUNGSI UJI HIPOTESIS VARIANSI (1 POPULASI)
# ==============================================================================
sumvar1 <- function() {
  sync_table_to_data(APP_STATE)

  if (ncol(APP_STATE$data) == 0) {
    tkmessageBox(
      message = "Tabel data masih kosong. Silakan isi tabel terlebih dahulu.",
      icon = "warning"
    )
    return()
  }

  # Cari semua kolom numerik yang tersedia
  numeric_cols <- names(APP_STATE$data)[sapply(APP_STATE$data, is.numeric)]
  if (length(numeric_cols) == 0) {
    tkmessageBox(
      message = "Tidak ditemukan data numerik pada tabel untuk diuji.",
      icon = "error"
    )
    return()
  }

  # Bentuk Window_1Pop
  win_1pop <- tktoplevel()
  tkwm.title(win_1pop, "Uji Variansi 1 Populasi")
  tkconfigure(win_1pop, background = getOption("Rgui.theme_bl")$primary)
  tkwm.geometry(win_1pop, "950x800")
  tkwm.minsize(win_1pop, 700, 600)

  main <- ttkframe(win_1pop, padding = 15)
  tkpack(main, fill = "both", expand = TRUE)

  # 1. PEMILIHAN VARIABEL & PARAMETER
  param_frame <- ttklabelframe(
    main,
    text = " Input Parameter & Variabel ",
    padding = 10
  )
  tkpack(param_frame, fill = "x", pady = c(0, 10))

  tkpack(ttklabel(param_frame, text = "Pilih Variabel Sampel:"), anchor = "w")
  APP_STATE$var_1pop_selected <- tclVar(numeric_cols[1])
  combo_1pop <- ttkcombobox(
    param_frame,
    textvariable = APP_STATE$var_1pop_selected,
    values = numeric_cols,
    state = "readonly",
    width = 25
  )
  tkpack(combo_1pop, anchor = "w", pady = c(0, 10))

  tkpack(
    ttklabel(param_frame, text = "Nilai \u03c3\u2080\u00b2 (Variansi H0):"),
    anchor = "w"
  )
  APP_STATE$var_sigma0_1pop <- tclVar("1")
  entry_sigma0 <- ttkentry(
    param_frame,
    textvariable = APP_STATE$var_sigma0_1pop,
    width = 15
  )
  tkpack(entry_sigma0, anchor = "w", pady = c(0, 10))

  # 2. SETTINGS (Alpha & Hipotesis)
  settings_frame <- ttklabelframe(main, text = " Pengaturan Uji ", padding = 10)
  tkpack(settings_frame, fill = "x", pady = c(0, 10))

  tkpack(ttklabel(settings_frame, text = "Nilai Alpha:"), anchor = "w")
  alpha_frame <- ttkframe(settings_frame)
  tkpack(alpha_frame, anchor = "w", pady = c(0, 5))

  APP_STATE$var_a01_1pop <- tclVar("0")
  APP_STATE$var_a05_1pop <- tclVar("1")
  APP_STATE$var_a10_1pop <- tclVar("0")

  update_perintah_1pop <- function() {
    if (is.null(APP_STATE$txt_perintah_1pop)) {
      return()
    }

    # Ambil var_name dari combobox
    var_name <- tclvalue(APP_STATE$var_1pop_selected)
    x <- suppressWarnings(as.numeric(APP_STATE$data[[var_name]]))
    x <- x[!is.na(x)]

    sigma0_str <- tclvalue(APP_STATE$var_sigma0_1pop)
    alt <- APP_STATE$alternative
    a <- APP_STATE$alpha

    n_val <- length(x)
    s2_val <- if (n_val > 1) var(x) else 0
    df_val <- if (n_val > 0) n_val - 1 else 0
    conf_level <- 1 - a

    x_str <- if (n_val > 5) {
      paste0(
        "c(",
        paste(head(x, 5), collapse = ", "),
        ", ..., ",
        paste(tail(x, 3), collapse = ", "),
        ")"
      )
    } else if (n_val > 0) {
      paste0("c(", paste(x, collapse = ", "), ")")
    } else {
      "c()"
    }

    pval_code <- switch(
      alt,
      "two.sided" = "2 * min(pchisq(chi2_stat, df), pchisq(chi2_stat, df, lower.tail = FALSE))",
      "less" = "pchisq(chi2_stat, df)",
      "greater" = "pchisq(chi2_stat, df, lower.tail = FALSE)"
    )

    cmd <- sprintf(
      "# === SETUP DATA ===\nx <- %s\n\n# === INFORMASI SAMPEL ===\nn <- length(x)\ns2 <- var(x)\ndf <- n - 1\n# n = %d | s\u00b2 = %.4f | df = %d\n\n# === PARAMETER UJI ===\nsigma0_sq <- %s\nalpha <- %.2f\nconf_level <- %.2f\n\n# === PROSES UJI VARIANSI 1 POPULASI (CHI-SQUARE) ===\nchi2_stat <- (n - 1) * s2 / sigma0_sq\n\n# P-value berdasarkan arah hipotesis (\"%s\")\np_value <- %s",
      x_str,
      n_val,
      s2_val,
      df_val,
      sigma0_str,
      a,
      conf_level,
      alt,
      pval_code
    )

    tkconfigure(APP_STATE$txt_perintah_1pop, state = "normal")
    tkdelete(APP_STATE$txt_perintah_1pop, "1.0", "end")
    tkinsert(APP_STATE$txt_perintah_1pop, "end", cmd)
    tkconfigure(APP_STATE$txt_perintah_1pop, state = "disabled")
  }

  toggle_alpha_1pop <- function(selected) {
    tclvalue(APP_STATE$var_a01_1pop) <- if (selected == "01") "1" else "0"
    tclvalue(APP_STATE$var_a05_1pop) <- if (selected == "05") "1" else "0"
    tclvalue(APP_STATE$var_a10_1pop) <- if (selected == "10") "1" else "0"
    APP_STATE$alpha <- as.numeric(paste0("0.", selected))
    update_perintah_1pop()
  }

  tkpack(
    ttkcheckbutton(
      alpha_frame,
      text = "0.01",
      variable = APP_STATE$var_a01_1pop,
      command = function() toggle_alpha_1pop("01")
    ),
    side = "left",
    padx = c(0, 15)
  )
  tkpack(
    ttkcheckbutton(
      alpha_frame,
      text = "0.05",
      variable = APP_STATE$var_a05_1pop,
      command = function() toggle_alpha_1pop("05")
    ),
    side = "left",
    padx = c(0, 15)
  )
  tkpack(
    ttkcheckbutton(
      alpha_frame,
      text = "0.10",
      variable = APP_STATE$var_a10_1pop,
      command = function() toggle_alpha_1pop("10")
    ),
    side = "left",
    padx = c(0, 15)
  )

  tkpack(
    ttklabel(settings_frame, text = "Arah Hipotesis:"),
    anchor = "w",
    pady = c(5, 0)
  )
  alt_frame <- ttkframe(settings_frame)
  tkpack(alt_frame, anchor = "w", pady = c(0, 5))

  APP_STATE$var_alt_1pop <- tclVar("two.sided")
  set_alt_1pop <- function(val) {
    APP_STATE$alternative <- val
    update_perintah_1pop()
  }

  tkpack(
    ttkradiobutton(
      alt_frame,
      text = "Dua Arah (two.sided)",
      variable = APP_STATE$var_alt_1pop,
      value = "two.sided",
      command = function() set_alt_1pop("two.sided")
    ),
    anchor = "w"
  )
  tkpack(
    ttkradiobutton(
      alt_frame,
      text = "Kiri (less)",
      variable = APP_STATE$var_alt_1pop,
      value = "less",
      command = function() set_alt_1pop("less")
    ),
    anchor = "w"
  )
  tkpack(
    ttkradiobutton(
      alt_frame,
      text = "Kanan (greater)",
      variable = APP_STATE$var_alt_1pop,
      value = "greater",
      command = function() set_alt_1pop("greater")
    ),
    anchor = "w"
  )

  # EVENT BINDING
  tkbind(combo_1pop, "<<ComboboxSelected>>", function() update_perintah_1pop())
  tkbind(entry_sigma0, "<KeyRelease>", function() update_perintah_1pop())

  # 3. OUTPUT SPLIT
  out_frame <- ttkframe(main)
  tkpack(out_frame, fill = "both", expand = TRUE)

  cmd_frame <- ttklabelframe(out_frame, text = " Perintah ", padding = 5)
  tkgrid(cmd_frame, row = 0, column = 0, sticky = "nsew", padx = c(0, 5))
  cmd_subframe <- ttkframe(cmd_frame)
  tkpack(cmd_subframe, fill = "both", expand = TRUE)
  APP_STATE$txt_perintah_1pop <- tktext(
    cmd_subframe,
    height = 10,
    width = 20,
    font = "Courier 10",
    state = "disabled",
    bg = "#f8f9fa",
    wrap = "word"
  )
  scrollbar_cmd <- ttkscrollbar(cmd_subframe, command = function(...) {
    tkyview(APP_STATE$txt_perintah_1pop, ...)
  })
  tkconfigure(APP_STATE$txt_perintah_1pop, yscrollcommand = function(...) {
    tkset(scrollbar_cmd, ...)
  })
  tkpack(scrollbar_cmd, side = "right", fill = "y")
  tkpack(
    APP_STATE$txt_perintah_1pop,
    side = "left",
    fill = "both",
    expand = TRUE
  )

  res_frame <- ttklabelframe(out_frame, text = " Output ", padding = 5)
  tkgrid(res_frame, row = 0, column = 1, sticky = "nsew", padx = c(5, 0))
  res_subframe <- ttkframe(res_frame)
  tkpack(res_subframe, fill = "both", expand = TRUE)
  APP_STATE$txt_output_1pop <- tktext(
    res_subframe,
    height = 10,
    width = 20,
    font = "Courier 10",
    state = "disabled",
    bg = "#f8f9fa",
    wrap = "word"
  )
  scrollbar <- ttkscrollbar(res_subframe, command = function(...) {
    tkyview(APP_STATE$txt_output_1pop, ...)
  })
  tkconfigure(APP_STATE$txt_output_1pop, yscrollcommand = function(...) {
    tkset(scrollbar, ...)
  })
  tkpack(scrollbar, side = "right", fill = "y")
  tkpack(APP_STATE$txt_output_1pop, side = "left", fill = "both", expand = TRUE)

  tkgrid.columnconfigure(out_frame, 0, weight = 1)
  tkgrid.columnconfigure(out_frame, 1, weight = 1)
  tkgrid.rowconfigure(out_frame, 0, weight = 1)

  APP_STATE$var_conclusion_1pop <- tclVar("")
  APP_STATE$lbl_conclusion_1pop <- ttklabel(
    main,
    textvariable = APP_STATE$var_conclusion_1pop,
    font = "Arial 11 bold",
    justify = "left"
  )
  tkpack(APP_STATE$lbl_conclusion_1pop, side = "bottom", anchor = "w", pady = 5)

  # 4. BUTTONS
  run_test_1pop <- function() {
    # Ambil var_name dari combobox
    var_name <- tclvalue(APP_STATE$var_1pop_selected)
    x <- suppressWarnings(as.numeric(APP_STATE$data[[var_name]]))
    x <- x[!is.na(x)]
    sigma0_val <- suppressWarnings(as.numeric(tclvalue(
      APP_STATE$var_sigma0_1pop
    )))

    if (length(x) < 2) {
      tkmessageBox(
        message = "Data minimal harus berisi 2 angka numerik!",
        icon = "warning"
      )
      return()
    }
    if (is.na(sigma0_val) || sigma0_val <= 0) {
      tkmessageBox(
        message = "Nilai \u03c3\u2080\u00b2 harus berupa angka > 0!",
        icon = "warning"
      )
      return()
    }

    res <- tryCatch(
      {
        n <- length(x)
        s2 <- var(x)
        df <- n - 1
        chi2_stat <- df * s2 / sigma0_val
        p_val <- switch(
          APP_STATE$alternative,
          "two.sided" = 2 *
            min(
              pchisq(chi2_stat, df),
              pchisq(chi2_stat, df, lower.tail = FALSE)
            ),
          "less" = pchisq(chi2_stat, df),
          "greater" = pchisq(chi2_stat, df, lower.tail = FALSE)
        )
        alpha <- APP_STATE$alpha
        ci_lower <- df * s2 / qchisq(1 - alpha / 2, df)
        ci_upper <- df * s2 / qchisq(alpha / 2, df)
        list(
          n = n,
          s2 = s2,
          df = df,
          statistic = chi2_stat,
          p.value = p_val,
          conf.int = c(ci_lower, ci_upper),
          alternative = APP_STATE$alternative,
          null.value = sigma0_val
        )
      },
      error = function(e) e
    )

    tkconfigure(APP_STATE$txt_output_1pop, state = "normal")
    tkdelete(APP_STATE$txt_output_1pop, "1.0", "end")

    if (inherits(res, "error")) {
      tkinsert(APP_STATE$txt_output_1pop, "end", paste("Error:", res$message))
      tclvalue(APP_STATE$var_conclusion_1pop) <- "Status: Error"
      tkconfigure(APP_STATE$lbl_conclusion_1pop, foreground = "red")
    } else {
      alt_txt <- switch(
        res$alternative,
        "two.sided" = "Two-sided (\u03c3\u00b2 \u2260 \u03c3\u2080\u00b2)",
        "less" = "Less (\u03c3\u00b2 < \u03c3\u2080\u00b2)",
        "greater" = "Greater (\u03c3\u00b2 > \u03c3\u2080\u00b2)"
      )
      out_txt <- paste0(
        "========================================\n  ONE-SAMPLE VARIANCE TEST\n========================================\n\n",
        sprintf("%-22s : %s\n", "Method", "Chi-Square Variance Test"),
        sprintf("%-22s : %s\n", "Variable", var_name),
        sprintf("%-22s : %s\n", "Alternative Hyp.", alt_txt),
        sprintf(
          "%-22s : %g\n",
          "Null Hypothesis (\u03c3\u2080\u00b2)",
          res$null.value
        ),
        sprintf(
          "%-22s : %.2f\n",
          "Significance Level (\u03b1)",
          APP_STATE$alpha
        ),
        "\n--- TEST STATISTICS ---\n",
        sprintf("%-22s : %.4f\n", "Chi-Square (\u03c7\u00b2)", res$statistic),
        sprintf("%-22s : %d\n", "Degrees of Freedom", res$df),
        sprintf(
          "%-22s : %s\n",
          "p-value",
          format.pval(res$p.value, digits = 4, eps = 0.0001)
        ),
        "\n--- CONFIDENCE INTERVAL ---\n",
        sprintf(
          "%-22s : %g%%\n",
          "Confidence Level",
          (1 - APP_STATE$alpha) * 1000
        ),
        sprintf(
          "%-22s : [%.4f, %.4f]\n",
          "Interval (\u03c3\u00b2)",
          res$conf.int[1],
          res$conf.int[2]
        ),
        "\n--- SAMPLE ESTIMATES ---\n",
        sprintf("%-22s : %.4f\n", "Sample Variance (s\u00b2)", res$s2),
        sprintf("%-22s : %d\n", "Sample Size", res$n)
      )
      is_sig <- res$p.value < APP_STATE$alpha
      kesimpulan <- if (is_sig) {
        sprintf(
          "\n--- CONCLUSION ---\nTolak H0 (p-value = %.4f < \u03b1 = %.2f)",
          res$p.value,
          APP_STATE$alpha
        )
      } else {
        sprintf(
          "\n--- CONCLUSION ---\nGagal Tolak H0 (p-value = %.4f >= \u03b1 = %.2f)",
          res$p.value,
          APP_STATE$alpha
        )
      }
      tkinsert(APP_STATE$txt_output_1pop, "end", paste0(out_txt, kesimpulan))
      tclvalue(APP_STATE$var_conclusion_1pop) <- if (is_sig) {
        "Hasil: Signifikan (Tolak H0)"
      } else {
        "Hasil: Tidak Signifikan"
      }
      tkconfigure(
        APP_STATE$lbl_conclusion_1pop,
        foreground = if (is_sig) "red" else "green"
      )
    }
    tkconfigure(APP_STATE$txt_output_1pop, state = "disabled")
  }

  btn_frame <- ttkframe(main)
  tkpack(btn_frame, side = "bottom", pady = c(10, 5))
  tkpack(
    ttkbutton(
      btn_frame,
      text = "HITUNG (UJI VARIANSI)",
      command = run_test_1pop
    ),
    side = "left",
    padx = 10
  )
  tkpack(
    ttkbutton(btn_frame, text = "TUTUP", command = function() {
      tkdestroy(win_1pop)
    }),
    side = "left",
    padx = 10
  )

  update_perintah_1pop()
}

# ==============================================================================
# FUNGSI UJI HIPOTESIS VARIANSI (2 POPULASI)
# ==============================================================================
sumvar2 <- function() {
  sync_table_to_data(APP_STATE)

  if (ncol(APP_STATE$data) < 2) {
    tkmessageBox(
      message = "Tabel data harus memiliki minimal 2 kolom untuk uji 2 populasi.",
      icon = "warning"
    )
    return()
  }

  numeric_cols <- names(APP_STATE$data)[sapply(APP_STATE$data, is.numeric)]
  if (length(numeric_cols) < 2) {
    tkmessageBox(
      message = "Tidak ditemukan minimal 2 kolom data numerik pada tabel.",
      icon = "error"
    )
    return()
  }

  # Bentuk Window_2pop
  win_2pop <- tktoplevel()
  tkwm.title(win_2pop, "Uji Variansi 2 Populasi")
  tkconfigure(win_2pop, background = getOption("Rgui.theme_bl")$primary)
  tkwm.geometry(win_2pop, "950x800")
  tkwm.minsize(win_2pop, 700, 600)

  main <- ttkframe(win_2pop, padding = 15)
  tkpack(main, fill = "both", expand = TRUE)

  # 1. PEMILIHAN VARIABEL
  input_frame <- ttklabelframe(
    main,
    text = " Pilih Variabel Sampel ",
    padding = 10
  )
  tkpack(input_frame, fill = "x", pady = c(0, 10))

  data_split_frame <- ttkframe(input_frame)
  tkpack(data_split_frame, fill = "x", expand = TRUE)

  p1_frame <- ttkframe(data_split_frame)
  tkgrid(p1_frame, row = 0, column = 0, sticky = "nsew", padx = c(0, 5))
  tkpack(ttklabel(p1_frame, text = "Variabel 1 (x1):"), anchor = "w")
  APP_STATE$var_2pop_x1 <- tclVar(numeric_cols[1])
  combo_x1 <- ttkcombobox(
    p1_frame,
    textvariable = APP_STATE$var_2pop_x1,
    values = numeric_cols,
    state = "readonly",
    width = 20
  )
  tkpack(combo_x1, fill = "x", pady = c(5, 0))

  p2_frame <- ttkframe(data_split_frame)
  tkgrid(p2_frame, row = 0, column = 1, sticky = "nsew", padx = c(5, 0))
  tkpack(ttklabel(p2_frame, text = "Variabel 2 (x2):"), anchor = "w")
  APP_STATE$var_2pop_x2 <- tclVar(numeric_cols[2])
  combo_x2 <- ttkcombobox(
    p2_frame,
    textvariable = APP_STATE$var_2pop_x2,
    values = numeric_cols,
    state = "readonly",
    width = 20
  )
  tkpack(combo_x2, fill = "x", pady = c(5, 0))

  tkgrid.columnconfigure(data_split_frame, 0, weight = 1)
  tkgrid.columnconfigure(data_split_frame, 1, weight = 1)

  # 2. SETTINGS
  settings_frame <- ttklabelframe(main, text = " Pengaturan Uji ", padding = 10)
  tkpack(settings_frame, fill = "x", pady = c(0, 10))

  tkpack(ttklabel(settings_frame, text = "Nilai Alpha:"), anchor = "w")
  alpha_frame <- ttkframe(settings_frame)
  tkpack(alpha_frame, anchor = "w", pady = c(0, 5))

  APP_STATE$var_a01_2pop <- tclVar("0")
  APP_STATE$var_a05_2pop <- tclVar("1")
  APP_STATE$var_a10_2pop <- tclVar("0")

  update_perintah_2pop <- function() {
    if (is.null(APP_STATE$txt_perintah_2pop)) {
      return()
    }

    # Ambil var1 dan var2 dari combobox
    var1 <- tclvalue(APP_STATE$var_2pop_x1)
    var2 <- tclvalue(APP_STATE$var_2pop_x2)

    x1 <- suppressWarnings(as.numeric(APP_STATE$data[[var1]]))
    x1 <- x1[!is.na(x1)]
    x2 <- suppressWarnings(as.numeric(APP_STATE$data[[var2]]))
    x2 <- x2[!is.na(x2)]

    alt <- APP_STATE$alternative
    a <- APP_STATE$alpha
    n1_val <- length(x1)
    n2_val <- length(x2)
    s1_sq <- if (n1_val > 1) var(x1) else 0
    s2_sq <- if (n2_val > 1) var(x2) else 0
    df1_val <- if (n1_val > 0) n1_val - 1 else 0
    df2_val <- if (n2_val > 0) n2_val - 1 else 0
    conf_level <- 1 - a

    x1_str <- if (n1_val > 3) {
      paste0("c(", paste(head(x1, 3), collapse = ", "), ", ...)")
    } else {
      paste0("c(", paste(x1, collapse = ", "), ")")
    }
    x2_str <- if (n2_val > 3) {
      paste0("c(", paste(head(x2, 3), collapse = ", "), ", ...)")
    } else {
      paste0("c(", paste(x2, collapse = ", "), ")")
    }

    cmd <- sprintf(
      "# === SETUP DATA ===\nx1 <- %s\nx2 <- %s\n\n# === INFORMASI SAMPEL ===\n# n1 = %d | s1\u00b2 = %.4f | df1 = %d\n# n2 = %d | s2\u00b2 = %.4f | df2 = %d\n\n# === PARAMETER UJI ===\nratio <- 1\nalpha <- %.2f\nconf_level <- %.2f\n\n# === PROSES UJI VARIANSI 2 POPULASI (F-TEST) ===\nres <- var.test(x1, x2, ratio = ratio,\n                alternative = \"%s\",\n                conf.level = conf_level)",
      x1_str,
      x2_str,
      n1_val,
      s1_sq,
      df1_val,
      n2_val,
      s2_sq,
      df2_val,
      a,
      conf_level,
      alt
    )

    tkconfigure(APP_STATE$txt_perintah_2pop, state = "normal")
    tkdelete(APP_STATE$txt_perintah_2pop, "1.0", "end")
    tkinsert(APP_STATE$txt_perintah_2pop, "end", cmd)
    tkconfigure(APP_STATE$txt_perintah_2pop, state = "disabled")
  }

  toggle_alpha_2pop <- function(selected) {
    tclvalue(APP_STATE$var_a01_2pop) <- if (selected == "01") "1" else "0"
    tclvalue(APP_STATE$var_a05_2pop) <- if (selected == "05") "1" else "0"
    tclvalue(APP_STATE$var_a10_2pop) <- if (selected == "10") "1" else "0"
    APP_STATE$alpha <- as.numeric(paste0("0.", selected))
    update_perintah_2pop()
  }

  tkpack(
    ttkcheckbutton(
      alpha_frame,
      text = "0.01",
      variable = APP_STATE$var_a01_2pop,
      command = function() toggle_alpha_2pop("01")
    ),
    side = "left",
    padx = c(0, 15)
  )
  tkpack(
    ttkcheckbutton(
      alpha_frame,
      text = "0.05",
      variable = APP_STATE$var_a05_2pop,
      command = function() toggle_alpha_2pop("05")
    ),
    side = "left",
    padx = c(0, 15)
  )
  tkpack(
    ttkcheckbutton(
      alpha_frame,
      text = "0.10",
      variable = APP_STATE$var_a10_2pop,
      command = function() toggle_alpha_2pop("10")
    ),
    side = "left",
    padx = c(0, 15)
  )

  tkpack(
    ttklabel(settings_frame, text = "Arah Hipotesis:"),
    anchor = "w",
    pady = c(5, 0)
  )
  alt_frame <- ttkframe(settings_frame)
  tkpack(alt_frame, anchor = "w", pady = c(0, 5))

  APP_STATE$var_alt_2pop <- tclVar("two.sided")
  set_alt_2pop <- function(val) {
    APP_STATE$alternative <- val
    update_perintah_2pop()
  }

  tkpack(
    ttkradiobutton(
      alt_frame,
      text = "Dua Arah (two.sided)",
      variable = APP_STATE$var_alt_2pop,
      value = "two.sided",
      command = function() set_alt_2pop("two.sided")
    ),
    anchor = "w"
  )
  tkpack(
    ttkradiobutton(
      alt_frame,
      text = "Kiri (less)",
      variable = APP_STATE$var_alt_2pop,
      value = "less",
      command = function() set_alt_2pop("less")
    ),
    anchor = "w"
  )
  tkpack(
    ttkradiobutton(
      alt_frame,
      text = "Kanan (greater)",
      variable = APP_STATE$var_alt_2pop,
      value = "greater",
      command = function() set_alt_2pop("greater")
    ),
    anchor = "w"
  )

  # EVENT BINDING
  tkbind(combo_x1, "<<ComboboxSelected>>", function() update_perintah_2pop())
  tkbind(combo_x2, "<<ComboboxSelected>>", function() update_perintah_2pop())

  # 3. OUTPUT SPLIT
  out_frame <- ttkframe(main)
  tkpack(out_frame, fill = "both", expand = TRUE)

  cmd_frame <- ttklabelframe(out_frame, text = " Perintah ", padding = 5)
  tkgrid(cmd_frame, row = 0, column = 0, sticky = "nsew", padx = c(0, 5))
  cmd_subframe <- ttkframe(cmd_frame)
  tkpack(cmd_subframe, fill = "both", expand = TRUE)
  APP_STATE$txt_perintah_2pop <- tktext(
    cmd_subframe,
    height = 10,
    width = 20,
    font = "Courier 10",
    state = "disabled",
    bg = "#f8f9fa",
    wrap = "word"
  )
  scrollbar_cmd <- ttkscrollbar(cmd_subframe, command = function(...) {
    tkyview(APP_STATE$txt_perintah_2pop, ...)
  })
  tkconfigure(APP_STATE$txt_perintah_2pop, yscrollcommand = function(...) {
    tkset(scrollbar_cmd, ...)
  })
  tkpack(scrollbar_cmd, side = "right", fill = "y")
  tkpack(
    APP_STATE$txt_perintah_2pop,
    side = "left",
    fill = "both",
    expand = TRUE
  )

  res_frame <- ttklabelframe(out_frame, text = " Output ", padding = 5)
  tkgrid(res_frame, row = 0, column = 1, sticky = "nsew", padx = c(5, 0))
  res_subframe <- ttkframe(res_frame)
  tkpack(res_subframe, fill = "both", expand = TRUE)
  APP_STATE$txt_output_2pop <- tktext(
    res_subframe,
    height = 10,
    width = 20,
    font = "Courier 10",
    state = "disabled",
    bg = "#f8f9fa",
    wrap = "word"
  )
  scrollbar <- ttkscrollbar(res_subframe, command = function(...) {
    tkyview(APP_STATE$txt_output_2pop, ...)
  })
  tkconfigure(APP_STATE$txt_output_2pop, yscrollcommand = function(...) {
    tkset(scrollbar, ...)
  })
  tkpack(scrollbar, side = "right", fill = "y")
  tkpack(APP_STATE$txt_output_2pop, side = "left", fill = "both", expand = TRUE)

  tkgrid.columnconfigure(out_frame, 0, weight = 1)
  tkgrid.columnconfigure(out_frame, 1, weight = 1)
  tkgrid.rowconfigure(out_frame, 0, weight = 1)

  APP_STATE$var_conclusion_2pop <- tclVar("")
  APP_STATE$lbl_conclusion_2pop <- ttklabel(
    main,
    textvariable = APP_STATE$var_conclusion_2pop,
    font = "Arial 11 bold",
    justify = "left"
  )
  tkpack(APP_STATE$lbl_conclusion_2pop, side = "bottom", anchor = "w", pady = 5)

  # 4. BUTTONS
  run_test_2pop <- function() {
    # Ambil var1 dan var2 dari combobox
    var1 <- tclvalue(APP_STATE$var_2pop_x1)
    var2 <- tclvalue(APP_STATE$var_2pop_x2)

    x1 <- suppressWarnings(as.numeric(APP_STATE$data[[var1]]))
    x1 <- x1[!is.na(x1)]
    x2 <- suppressWarnings(as.numeric(APP_STATE$data[[var2]]))
    x2 <- x2[!is.na(x2)]

    if (length(x1) < 2 || length(x2) < 2) {
      tkmessageBox(
        message = "Masing-masing data sampel minimal harus berisi 2 angka numerik!",
        icon = "warning"
      )
      return()
    }

    res <- tryCatch(
      {
        var.test(
          x1,
          x2,
          ratio = 1,
          alternative = APP_STATE$alternative,
          conf.level = 1 - APP_STATE$alpha
        )
      },
      error = function(e) e
    )

    tkconfigure(APP_STATE$txt_output_2pop, state = "normal")
    tkdelete(APP_STATE$txt_output_2pop, "1.0", "end")

    if (inherits(res, "error")) {
      tkinsert(APP_STATE$txt_output_2pop, "end", paste("Error:", res$message))
      tclvalue(APP_STATE$var_conclusion_2pop) <- "Status: Error"
      tkconfigure(APP_STATE$lbl_conclusion_2pop, foreground = "red")
    } else {
      alt_txt <- switch(
        res$alternative,
        "two.sided" = "Two-sided (\u03c3\u2081\u00b2 / \u03c3\u2082\u00b2 \u2260 1)",
        "less" = "Less (\u03c3\u2081\u00b2 / \u03c3\u2082\u00b2 < 1)",
        "greater" = "Greater (\u03c3\u2081\u00b2 / \u03c3\u2082\u00b2 > 1)"
      )
      out_txt <- paste0(
        "========================================\n  TWO-SAMPLE VARIANCE TEST\n========================================\n\n",
        sprintf("%-22s : %s\n", "Method", "F test to compare two variances"),
        sprintf("%-22s : %s vs %s\n", "Data", var1, var2),
        sprintf("%-22s : %s\n", "Alternative Hyp.", alt_txt),
        sprintf("%-22s : %g\n", "Null Hypothesis (ratio)", res$null.value),
        sprintf(
          "%-22s : %.2f\n",
          "Significance Level (\u03b1)",
          APP_STATE$alpha
        ),
        "\n--- TEST STATISTICS ---\n",
        sprintf("%-22s : %.4f\n", "F Statistic", res$statistic),
        sprintf(
          "%-22s : %d, %d\n",
          "num df, denom df",
          res$parameter[1],
          res$parameter[2]
        ),
        sprintf(
          "%-22s : %s\n",
          "p-value",
          format.pval(res$p.value, digits = 4, eps = 0.0001)
        ),
        "\n--- CONFIDENCE INTERVAL ---\n",
        sprintf(
          "%-22s : %g%%\n",
          "Confidence Level",
          (1 - APP_STATE$alpha) * 1000
        ),
        sprintf(
          "%-22s : [%.4f, %.4f]\n",
          "Interval (\u03c3\u2081\u00b2/\u03c3\u2082\u00b2)",
          res$conf.int[1],
          res$conf.int[2]
        ),
        "\n--- SAMPLE ESTIMATES ---\n",
        sprintf("%-22s : %.4f\n", paste0("Var(", var1, ")"), var(x1)),
        sprintf("%-22s : %.4f\n", paste0("Var(", var2, ")"), var(x2)),
        sprintf("%-22s : %.4f\n", "Ratio of Variances", res$estimate),
        sprintf(
          "%-22s : n1 = %d, n2 = %d\n",
          "Sample Size",
          length(x1),
          length(x2)
        )
      )
      is_sig <- res$p.value < APP_STATE$alpha
      kesimpulan <- if (is_sig) {
        sprintf(
          "\n--- CONCLUSION ---\nTolak H0 (p-value = %.4f < \u03b1 = %.2f)",
          res$p.value,
          APP_STATE$alpha
        )
      } else {
        sprintf(
          "\n--- CONCLUSION ---\nGagal Tolak H0 (p-value = %.4f >= \u03b1 = %.2f)",
          res$p.value,
          APP_STATE$alpha
        )
      }
      tkinsert(APP_STATE$txt_output_2pop, "end", paste0(out_txt, kesimpulan))
      tclvalue(APP_STATE$var_conclusion_2pop) <- if (is_sig) {
        "Hasil: Signifikan (Tolak H0)"
      } else {
        "Hasil: Tidak Signifikan (Gagal Tolak H0)"
      }
      tkconfigure(
        APP_STATE$lbl_conclusion_2pop,
        foreground = if (is_sig) "red" else "green"
      )
    }
    tkconfigure(APP_STATE$txt_output_2pop, state = "disabled")
  }

  btn_frame <- ttkframe(main)
  tkpack(btn_frame, side = "bottom", pady = c(10, 5))
  tkpack(
    ttkbutton(
      btn_frame,
      text = "HITUNG (UJI VARIANSI 2 POP)",
      command = run_test_2pop
    ),
    side = "left",
    padx = 10
  )
  tkpack(
    ttkbutton(btn_frame, text = "TUTUP", command = function() {
      tkdestroy(win_2pop)
    }),
    side = "left",
    padx = 10
  )

  update_perintah_2pop()
}

# ==============================================================================
# STRUKTUR TOPMENU
# ==============================================================================
Submenu_container <- list(
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

# FUNGSI PEMBUAT MENU KUSTOM
# 1. Fungsi rekursif untuk mengisi dropdown (isian menu)
build_dropdown_items <- function(parent_menu, structure) {
  # TAMBAHKAN BARIS INI:
  theme <- getOption("Rgui.theme_bl")

  for (item in structure) {
    if (!is.null(item$children)) {
      submenu <- tkmenu(
        parent_menu,
        tearoff = FALSE,
        background = theme$background,
        foreground = theme$secondary,
        activebackground = theme$secondary,
        activeforeground = "#ffffff",
        font = "Helvetica 9"
      )

      # Tambahkan submenu sebagai item cascade
      tkadd(parent_menu, "cascade", label = item$label, menu = submenu)

      # Panggil fungsi secara rekursif untuk submenu di bawahnya
      build_dropdown_items(submenu, item$children)
    } else if (!is.null(item$command)) {
      local({
        func_name <- item$command
        label_txt <- item$label

        # Tambahkan item command
        tkadd(
          parent_menu,
          "command",
          label = label_txt,
          background = theme$background,
          foreground = theme$secondary,
          activebackground = theme$secondary,
          activeforeground = "#ffffff",
          command = function() {
            if (exists(func_name, envir = .GlobalEnv)) {
              do.call(get(func_name, envir = .GlobalEnv), list())
            } else {
              tkmessageBox(
                message = paste("Fungsi", func_name, "belum didefinisikan."),
                icon = "warning"
              )
            }
          }
        )
      })
    }
  }
}

# 2. Fungsi utama untuk membuat Menu Bar Kustom
create_custom_menubar <- function(parent_window, structure) {
  # TAMBAHKAN BARIS INI:
  theme <- getOption("Rgui.theme_bl")

  # Buat frame sebagai latar belakang menu bar
  menubar_frame <- tkframe(
    parent_window,
    background = theme$primary,
    highlightthickness = 1,
    highlightbackground = theme$border,
    highlightcolor = theme$border,
    borderwidth = 0,
    relief = "flat"
  )
  tkpack(menubar_frame, fill = "x", side = "top")

  for (top_item in structure) {
    # Buat tombol menu atas
    mb <- tkmenubutton(
      menubar_frame,
      text = top_item$label,
      background = theme$primary,
      foreground = theme$secondary,
      highlightthickness = 1,
      highlightbackground = theme$background,
      highlightcolor = theme$background,
      borderwidth = 0,
      activebackground = theme$secondary,
      activeforeground = "#ffffff",
      font = "Helvetica 9",
      relief = "flat",
      padx = 15,
      pady = 4
    )
    tkpack(mb, side = "left")

    # Buat menu dropdown untuk tombol tersebut
    dropdown <- tkmenu(
      mb,
      tearoff = FALSE,
      background = theme$background,
      foreground = theme$secondary,
      activebackground = theme$secondary,
      activeforeground = "#ffffff"
    )
    tkconfigure(mb, menu = dropdown)

    # Isi menu dropdown-nya
    if (!is.null(top_item$children)) {
      build_dropdown_items(dropdown, top_item$children)
    }
  }
}

# ==============================================================================
# DEV TOOLS: WIDGET INSPECTOR (F12 toggle) — fixed escaping & event coordinates
# ==============================================================================
enable_widget_inspector <- function(root_window) {
  # 1. Tcl proc now accepts x and y as arguments
  tcl(
    "proc",
    "dev_inspect_widget",
    "{x y}",
    "{
    set w [winfo containing $x $y]
    if {$w eq \"\"} { return }
    puts \"\"
    puts \"========================================\"
    puts \"\\[DEV\\] Widget : $w\"
    puts \"\\[DEV\\] Class  : [winfo class $w]\"
    puts \"\\[DEV\\] Parent : [winfo parent $w]\"
    puts \"\\[DEV\\] Manager: [winfo manager $w]\"
    puts \"========================================\"
  }"
  )

  # 2. Get original title correctly using tcl()
  orig_title <- tclvalue(tcl("wm", "title", root_window))
  tcl("set", "dev_orig_title", orig_title)
  tcl("set", "dev_inspector_on", "0")

  # 3. Pass %X and %Y into the proc, and fix escaping in sprintf
  tcl(
    "bind",
    root_window,
    "<F12>",
    sprintf(
      "{
    if {$dev_inspector_on} {
      set dev_inspector_on 0
      bind all <Button-1> {}
      %s configure -cursor {}
      wm title %s $dev_orig_title
      puts \"\\[DEV INSPECTOR\\] OFF\"
    } else {
      set dev_inspector_on 1
      bind all <Button-1> {dev_inspect_widget %%X %%Y}
      %s configure -cursor crosshair
      wm title %s \"$dev_orig_title  \\[INSPECTOR ON\\]\"
      puts \"\\[DEV INSPECTOR\\] ON  -> Click any widget. Press F12 to turn off.\"
    }
  }",
      root_window,
      root_window,
      root_window,
      root_window
    )
  )
}


# ==============================================================================
#  Main GUI Build
# ==============================================================================
Start_open <- function() {
  # Matikan service mode agar tidak glitch saat merakit UI
  tclServiceMode(FALSE)

  Main_page <- tktoplevel()
  tkwm.title(Main_page, "Statistika - Software Uji Hipotesis")
  tkconfigure(Main_page, background = getOption("Rgui.theme_bl")$primary)

  # Main window body
  tkwm.geometry(Main_page, "1050x700")
  tkwm.minsize(Main_page, 800, 600)

  # Topmenu
  create_custom_menubar(Main_page, Submenu_container)

  # Main Container (for table)
  main_container <- tkframe(
    Main_page,
    background = getOption("Rgui.theme_bl")$primary
  )
  tkpack(main_container, fill = "both", expand = TRUE, padx = 10, pady = 10)

  # button container
  btn_frame <- tkframe(
    main_container,
    background = getOption("Rgui.theme_bl")$primary
  )
  tkpack(btn_frame, fill = "x", pady = 5)

  # 1. Konfigurasi Warna & Desain Dasar
  tcl(
    "ttk::style",
    "configure",
    "Custom.TButton",
    "-background",
    theme_colors$primary,
    "-foreground",
    theme_colors$secondary,
    "-relief",
    "flat",
    "-borderwidth",
    0.5, # Ketebalan garis border
    "-bordercolor",
    theme_colors$border, # Warna garis (pakai warna border Anda, bukan primary)
    "-lightcolor",
    theme_colors$primary,
    "-darkcolor",
    theme_colors$border,
    "-padding",
    6,
    "-font",
    "Helvetica 10"
  )

  # 2. Konfigurasi Efek Interaktif (Hover / Active, dan Pressed)
  tcl(
    "ttk::style",
    "map",
    "Custom.TButton",
    "-background",
    sprintf(
      "active %s pressed %s !active %s",
      theme_colors$border,
      theme_colors$secondary,
      theme_colors$primary
    ),
    "-foreground",
    sprintf(
      "active %s pressed %s !active %s",
      theme_colors$secondary,
      theme_colors$background,
      theme_colors$secondary
    ),
    # Pastikan border tetap terlihat saat di-hover
    "-bordercolor",
    sprintf(
      "active %s pressed %s !active %s",
      theme_colors$secondary, # Warna border saat hover
      theme_colors$secondary, # Warna border saat ditekan
      theme_colors$border # Warna border normal
    )
  )

  # 1. Tombol Save Data
  # Frame sebagai border
  border_print <- tkframe(
    btn_frame,
    borderwidth = 1, # Ketebalan border
    relief = "solid", # Gaya garis solid
    background = theme_colors$border # Warna border (biru gelap)
  )
  tkpack(border_print, side = "left", padx = 5)

  # Tombol di dalam frame
  btn_print <- ttkbutton(
    border_print,
    text = "Save Data",
    style = "Custom.TButton",
    command = function() {
      sync_table_to_data(APP_STATE)
      cat("\n=== APP_STATE$data ===\n")
      print(APP_STATE$data)
      cat("\n=== APP_STATE$variable_info ===\n")
      print(APP_STATE$variable_info)

      if (ncol(APP_STATE$data) > 0) {
        tkmessageBox(
          title = "Berhasil",
          message = paste(
            "Data berhasil disimpan!\nJumlah Baris:",
            nrow(APP_STATE$data),
            "\nJumlah Kolom:",
            ncol(APP_STATE$data)
          ),
          icon = "info"
        )
      } else {
        tkmessageBox(
          title = "Kosong",
          message = "Tabel masih kosong. Tidak ada data yang disimpan.",
          icon = "warning"
        )
      }
    }
  )
  tkpack(btn_print, fill = "both", expand = TRUE)

  # 2. Tombol Reset Data
  # Frame sebagai border
  border_reset <- tkframe(
    btn_frame,
    borderwidth = 1,
    relief = "solid",
    background = theme_colors$border
  )
  tkpack(border_reset, side = "left", padx = 5)

  # Tombol di dalam frame
  btn_reset <- ttkbutton(
    border_reset,
    text = "Reset Data",
    style = "Custom.TButton",
    command = function() {
      reset_app_state(APP_STATE)
    }
  )
  tkpack(btn_reset, fill = "both", expand = TRUE)

  # Render table ke main container
  create_data_view(main_container, APP_STATE)

  # KUNCI PERBAIKAN: Nyalakan service mode SEBELUM menunggu jendela
  tclServiceMode(TRUE)

  # Menahan R agar menunggu jendela ditutup
  if (interactive()) tkwait.window(Main_page)
}

Start_open()
