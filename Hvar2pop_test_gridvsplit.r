library(tcltk)
library(tcltk2)

# WARNA GLOBAL
options(
  Rgui.theme_bl = list(
    primary = "#a9a29c",
    secondary = "#333333",
    background = "#d5ccc7",
    border = "#28262b"
  )
)

#--- SOFTWARE DEFAULT STATE SAAT DIBUKA---
APP_STATE <- new.env()
APP_STATE$alternative <- "two.sided"
APP_STATE$alpha <- 0.05

#=========================================================
# FILE IMPORT FUNCTIONS
#=========================================================
process_loaded_data <- function(df, filename) {
  # Uji variansi 2 populasi membutuhkan 2 kolom
  if (ncol(df) >= 2) {
    x <- as.numeric(df[[1]])
    y <- as.numeric(df[[2]])
    tclvalue(APP_STATE$var_status) <- paste(
      "Loaded:",
      basename(filename),
      "- 2 Columns"
    )
  } else {
    tkmessageBox(
      message = "File harus memiliki minimal 2 kolom angka (Sampel 1 & Sampel 2).",
      icon = "warning"
    )
    return()
  }

  x <- x[!is.na(x)]
  y <- y[!is.na(y)]

  set_text <- function(widget, vals) {
    tkconfigure(widget, state = "normal")
    tkdelete(widget, "1.0", "end")
    tkinsert(widget, "end", paste(vals, collapse = "\n"))
  }

  set_text(APP_STATE$txt_data1, x)
  set_text(APP_STATE$txt_data2, y)
  update_perintah()
}

Excel_input <- function() {
  path <- tclvalue(tkgetOpenFile(
    title = "Pilih File Excel",
    filetypes = "{{Excel} {.xlsx .xls}}"
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
      df <- readxl::read_excel(path, col_names = FALSE)
      process_loaded_data(df, path)
    },
    error = function(e) {
      tkmessageBox(message = paste("Gagal:", e$message), icon = "error")
    }
  )
}

SPSS_input <- function() {
  path <- tclvalue(tkgetOpenFile(
    title = "Pilih File SPSS",
    filetypes = "{{SPSS} {.sav}}"
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
      process_loaded_data(as.data.frame(df), path)
    },
    error = function(e) {
      tkmessageBox(message = paste("Gagal:", e$message), icon = "error")
    }
  )
}

CSV_input <- function() {
  path <- tclvalue(tkgetOpenFile(
    title = "Pilih File CSV",
    filetypes = "{{CSV} {.csv}}"
  ))
  if (nchar(path) == 0) {
    return()
  }
  tryCatch(
    {
      df <- read.csv(path, header = FALSE)
      process_loaded_data(df, path)
    },
    error = function(e) {
      tkmessageBox(message = paste("Gagal:", e$message), icon = "error")
    }
  )
}

text_input <- function() {
  path <- tclvalue(tkgetOpenFile(
    title = "Pilih File Text",
    filetypes = "{{Text} {.txt}}"
  ))
  if (nchar(path) == 0) {
    return()
  }
  tryCatch(
    {
      df <- read.table(path, header = FALSE)
      process_loaded_data(df, path)
    },
    error = function(e) {
      tkmessageBox(message = paste("Gagal:", e$message), icon = "error")
    }
  )
}

#=========================================================
# LOGIC & HELPER FUNCTIONS FOR COMMAND INPUT + OUTPUT
#=========================================================
get_text_data <- function(txt_widget) {
  raw_text <- tclvalue(tkget(txt_widget, "1.0", "end"))
  clean_text <- gsub("[,\n]", " ", raw_text)
  vals <- suppressWarnings(as.numeric(strsplit(trimws(clean_text), "\\s+")[[
    1
  ]]))
  return(vals[!is.na(vals)])
}

# Perintah Box Funtion
update_perintah <- function() {
  if (is.null(APP_STATE$txt_perintah)) {
    return()
  }

  # Mengambil data input
  x <- get_text_data(APP_STATE$txt_data1)
  y <- get_text_data(APP_STATE$txt_data2)
  ratio_str <- tclvalue(APP_STATE$var_ratio)
  alt <- APP_STATE$alternative
  a <- APP_STATE$alpha

  # Hitung nilai statistik secara real-time
  n1_val <- length(x)
  n2_val <- length(y)
  conf_level <- 1 - a

  # Format text data array
  x_str <- if (n1_val > 3) {
    paste0("c(", paste(head(x, 3), collapse = ", "), ", ...)")
  } else if (n1_val > 0) {
    paste0("c(", paste(x, collapse = ", "), ")")
  } else {
    "c()"
  }
  y_str <- if (n2_val > 3) {
    paste0("c(", paste(head(y, 3), collapse = ", "), ", ...)")
  } else if (n2_val > 0) {
    paste0("c(", paste(y, collapse = ", "), ")")
  } else {
    "c()"
  }

  # ------------ Rangkaian Text Print cmd box --------------------------
  cmd <- sprintf(
    "# === SETUP DATA ===\nx <- %s\ny <- %s\n\n# === PARAMETER UJI ===\nratio <- %s    # Rasio Variansi (H0)\nalpha <- %.2f        # Signifikansi\nconf_level <- %.2f   # Kepercayaan (1 - \u03b1)\n\n# === UJI VARIANS ===\n# n1 = %d | n2 = %d\nvar.test(x, y, ratio = ratio,\n         alternative = \"%s\",\n         conf.level = conf_level)",
    x_str,
    y_str,
    ratio_str,
    a,
    conf_level,
    n1_val,
    n2_val,
    alt
  )

  tkconfigure(APP_STATE$txt_perintah, state = "normal")
  tkdelete(APP_STATE$txt_perintah, "1.0", "end")
  tkinsert(APP_STATE$txt_perintah, "end", cmd)
  tkconfigure(APP_STATE$txt_perintah, state = "disabled")
}

# ---------------- FUNGSI UJI VARIANSI 2 POPULASI --------------------------------
run_uji_variansi_2pop <- function() {
  x <- get_text_data(APP_STATE$txt_data1)
  y <- get_text_data(APP_STATE$txt_data2)
  ratio_val <- suppressWarnings(as.numeric(tclvalue(APP_STATE$var_ratio)))

  # Validasi Input
  if (length(x) < 2 || length(y) < 2) {
    tkmessageBox(
      message = "Kedua data sampel minimal harus berisi 2 angka!",
      icon = "warning"
    )
    return()
  }
  if (is.na(ratio_val) || ratio_val <= 0) {
    tkmessageBox(
      message = "Nilai Rasio harus berupa angka > 0!",
      icon = "warning"
    )
    return()
  }

  # Eksekusi Uji dengan Error Handling
  res <- tryCatch(
    {
      var.test(
        x,
        y,
        ratio = ratio_val,
        alternative = APP_STATE$alternative,
        conf.level = 1 - APP_STATE$alpha
      )
    },
    error = function(e) e
  )

  tkconfigure(APP_STATE$txt_output, state = "normal")
  tkdelete(APP_STATE$txt_output, "1.0", "end")

  if (inherits(res, "error")) {
    tkinsert(APP_STATE$txt_output, "end", paste("Error:", res$message))
    tclvalue(APP_STATE$var_conclusion) <- "Status: Error"
    tkconfigure(APP_STATE$lbl_conclusion, foreground = "red")
  } else {
    # Menggunakan Notasi Matematika Unicode
    alt_txt <- switch(
      res$alternative,
      "two.sided" = "Two-sided (\u03c3\u00b2\u2081 \u2260 \u03c3\u00b2\u2082)",
      "less" = "Less (\u03c3\u00b2\u2081 < \u03c3\u00b2\u2082)",
      "greater" = "Greater (\u03c3\u00b2\u2081 > \u03c3\u00b2\u2082)",
      res$alternative
    )

    # -------------- GRAFIK OUTPUT --------------
    out_txt <- paste0(
      "========================================\n",
      "     TWO-SAMPLE VARIANCE TEST (F)       \n",
      "========================================\n\n",
      sprintf("%-22s : %s\n", "Method", "F test to compare two variances"),
      sprintf("%-22s : %s\n", "Data", "x and y"),
      sprintf("%-22s : %s\n", "Alternative Hyp.", alt_txt),
      sprintf("%-22s : %g\n", "Null Hypothesis (ratio)", res$null.value),
      sprintf("%-22s : %.2f\n", "Significance Level (\u03b1)", APP_STATE$alpha),

      "\n--- TEST STATISTICS ---\n",
      sprintf("%-22s : %.4f\n", "F-statistic", res$statistic),
      sprintf("%-22s : %d\n", "Num df (df1)", res$parameter[1]),
      sprintf("%-22s : %d\n", "Denom df (df2)", res$parameter[2]),
      sprintf(
        "%-22s : %s\n",
        "p-value",
        format.pval(res$p.value, digits = 4, eps = 0.0001)
      ),

      "\n--- CONFIDENCE INTERVAL ---\n",
      sprintf(
        "%-22s : %g%%\n",
        "Confidence Level",
        (1 - APP_STATE$alpha) * 100
      ),
      sprintf(
        "%-22s : [%.4f, %.4f]\n",
        "Interval (Ratio)",
        res$conf.int[1],
        res$conf.int[2]
      ),

      "\n--- SAMPLE ESTIMATES ---\n",
      sprintf("%-22s : %.4f\n", "Variance of x", res$estimate[1]),
      sprintf("%-22s : %.4f\n", "Variance of y", res$estimate[2]),
      sprintf(
        "%-22s : %.4f\n",
        "Ratio of Variances",
        res$estimate[1] / res$estimate[2]
      )
    )

    # Fungsi kesimpulan
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

    tkinsert(APP_STATE$txt_output, "end", paste0(out_txt, kesimpulan))

    tclvalue(APP_STATE$var_conclusion) <- if (is_sig) {
      "Hasil: Signifikan (Tolak H0)"
    } else {
      "Hasil: Tidak Signifikan (Gagal Tolak H0)"
    }
    tkconfigure(
      APP_STATE$lbl_conclusion,
      foreground = if (is_sig) "red" else "green"
    )
  }
  tkconfigure(APP_STATE$txt_output, state = "disabled")
}

#=========================================================
# TOPMENU
#=========================================================
Submenu_container <- list(
  list(
    label = "File",
    children = list(
      list(
        label = "Import Data (2 Kolom)",
        children = list(
          list(label = "Excel", command = "Excel_input"),
          list(label = "SPSS", command = "SPSS_input"),
          list(label = "CSV", command = "CSV_input"),
          list(label = "text", command = "text_input")
        )
      )
    )
  )
)

build_menu <- function(parent_menu, structure) {
  for (item in structure) {
    if (!is.null(item$children)) {
      submenu <- tkmenu(parent_menu, tearoff = FALSE)
      tkadd(parent_menu, "cascade", label = item$label, menu = submenu)
      build_menu(submenu, item$children)
    } else if (!is.null(item$command)) {
      local({
        func_name <- item$command
        label_txt <- item$label
        tkadd(
          parent_menu,
          "command",
          label = label_txt,
          command = function() {
            if (exists(func_name, envir = .GlobalEnv)) {
              do.call(get(func_name, envir = .GlobalEnv), list())
            }
          }
        )
      })
    }
  }
}

#=========================================================
# MAIN GUI DRAW
#=========================================================
Start_open <- function() {
  tclServiceMode(FALSE)
  on.exit(tclServiceMode(TRUE))

  Main_page <- tktoplevel()
  tkwm.title(Main_page, "Uji Hipotesis Variansi 2 Populasi")
  tkconfigure(Main_page, background = getOption("Rgui.theme_bl")$primary)

  tkwm.geometry(Main_page, "950x950")
  tkwm.minsize(Main_page, 700, 700)

  topmenu <- tkmenu(Main_page, tearoff = FALSE)
  tkconfigure(Main_page, menu = topmenu)
  build_menu(topmenu, Submenu_container)

  main <- ttkframe(Main_page, padding = 15)
  tkpack(main, fill = "both", expand = TRUE)

  # Status File
  APP_STATE$var_status <- tclVar("No file loaded (Using default example data)")
  lbl_status <- ttklabel(
    main,
    textvariable = APP_STATE$var_status,
    foreground = "gray30",
    font = "Arial 9 italic"
  )
  tkpack(lbl_status, anchor = "w", pady = c(0, 5))

  # 1. DATA INPUT AREA
  input_frame <- ttklabelframe(
    main,
    text = " Input Data & Parameter ",
    padding = 10
  )
  tkpack(input_frame, fill = "x", pady = c(0, 10))

  create_text_box <- function(parent, height) {
    f <- ttkframe(parent)
    txt <- tktext(
      f,
      height = height,
      font = "Courier 10",
      bg = "white",
      fg = "black",
      wrap = "word"
    )
    scr <- ttkscrollbar(f, command = function(...) tkyview(txt, ...))
    tkconfigure(txt, yscrollcommand = function(...) tkset(scr, ...))
    tkpack(scr, side = "right", fill = "y")
    tkpack(txt, side = "left", fill = "both", expand = TRUE)
    return(list(frame = f, text = txt))
  }

  # Input Data Sampel 1 (x)
  tkpack(
    ttklabel(
      input_frame,
      text = "Data Sampel 1 / x (pisahkan dengan enter/spasi):"
    ),
    anchor = "w"
  )
  w_data1 <- create_text_box(input_frame, height = 4)
  APP_STATE$txt_data1 <- w_data1$text
  tkpack(w_data1$frame, fill = "x", pady = c(0, 5))
  tkinsert(APP_STATE$txt_data1, "end", "10\n12\n14\n11\n13\n15")

  # Input Data Sampel 2 (y)
  tkpack(
    ttklabel(
      input_frame,
      text = "Data Sampel 2 / y (pisahkan dengan enter/spasi):"
    ),
    anchor = "w"
  )
  w_data2 <- create_text_box(input_frame, height = 4)
  APP_STATE$txt_data2 <- w_data2$text
  tkpack(w_data2$frame, fill = "x", pady = c(0, 10))
  tkinsert(APP_STATE$txt_data2, "end", "8\n10\n9\n11\n12\n7")

  # Parameter Frame (Ratio)
  param_frame <- ttkframe(input_frame)
  tkpack(param_frame, anchor = "w", pady = c(5, 0))
  tkpack(
    ttklabel(param_frame, text = "Nilai Rasio Variansi (H0):"),
    side = "left"
  )

  APP_STATE$var_ratio <- tclVar("1")
  entry_ratio <- ttkentry(
    param_frame,
    textvariable = APP_STATE$var_ratio,
    width = 15
  )
  tkpack(entry_ratio, side = "left", padx = 10)

  # 2. ALPHA & ALTERNATIVE
  settings_frame <- ttklabelframe(main, text = " Pengaturan Uji ", padding = 10)
  tkpack(settings_frame, fill = "x", pady = c(0, 10))

  tkpack(ttklabel(settings_frame, text = "Nilai Alpha:"), anchor = "w")
  alpha_frame <- ttkframe(settings_frame)
  tkpack(alpha_frame, anchor = "w", pady = c(0, 5))

  APP_STATE$var_a01 <- tclVar("0")
  APP_STATE$var_a05 <- tclVar("1")
  APP_STATE$var_a10 <- tclVar("0")

  toggle_alpha <- function(selected) {
    tclvalue(APP_STATE$var_a01) <- if (selected == "01") "1" else "0"
    tclvalue(APP_STATE$var_a05) <- if (selected == "05") "1" else "0"
    tclvalue(APP_STATE$var_a10) <- if (selected == "10") "1" else "0"
    APP_STATE$alpha <- as.numeric(paste0("0.", selected))
    update_perintah()
  }

  tkpack(
    ttkcheckbutton(
      alpha_frame,
      text = "0.01",
      variable = APP_STATE$var_a01,
      command = function() toggle_alpha("01")
    ),
    side = "left",
    padx = c(0, 15)
  )
  tkpack(
    ttkcheckbutton(
      alpha_frame,
      text = "0.05",
      variable = APP_STATE$var_a05,
      command = function() toggle_alpha("05")
    ),
    side = "left",
    padx = c(0, 15)
  )
  tkpack(
    ttkcheckbutton(
      alpha_frame,
      text = "0.10",
      variable = APP_STATE$var_a10,
      command = function() toggle_alpha("10")
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

  APP_STATE$var_alt <- tclVar("two.sided")
  set_alt <- function(val) {
    APP_STATE$alternative <- val
    update_perintah()
  }

  tkpack(
    ttkradiobutton(
      alt_frame,
      text = "Dua Arah (two.sided)",
      variable = APP_STATE$var_alt,
      value = "two.sided",
      command = function() set_alt("two.sided")
    ),
    anchor = "w"
  )
  tkpack(
    ttkradiobutton(
      alt_frame,
      text = "Kiri (less)",
      variable = APP_STATE$var_alt,
      value = "less",
      command = function() set_alt("less")
    ),
    anchor = "w"
  )
  tkpack(
    ttkradiobutton(
      alt_frame,
      text = "Kanan (greater)",
      variable = APP_STATE$var_alt,
      value = "greater",
      command = function() set_alt("greater")
    ),
    anchor = "w"
  )

  # Bind updates
  tkbind(APP_STATE$txt_data1, "<KeyRelease>", function() update_perintah())
  tkbind(APP_STATE$txt_data2, "<KeyRelease>", function() update_perintah())
  tkbind(entry_ratio, "<KeyRelease>", function() update_perintah())

  # 3. PERINTAH & OUTPUT (Side-by-Side 50:50)
  out_frame <- ttkframe(main)
  tkpack(out_frame, fill = "both", expand = TRUE)

  cmd_frame <- ttklabelframe(out_frame, text = " Perintah ", padding = 5)
  tkgrid(cmd_frame, row = 0, column = 0, sticky = "nsew", padx = c(0, 5))
  cmd_subframe <- ttkframe(cmd_frame)
  tkpack(cmd_subframe, fill = "both", expand = TRUE)

  APP_STATE$txt_perintah <- tktext(
    cmd_subframe,
    height = 10,
    width = 20,
    font = "Courier 10",
    state = "disabled",
    bg = "#f8f9fa",
    wrap = "word"
  )
  scrollbar_cmd <- ttkscrollbar(cmd_subframe, command = function(...) {
    tkyview(APP_STATE$txt_perintah, ...)
  })
  tkconfigure(APP_STATE$txt_perintah, yscrollcommand = function(...) {
    tkset(scrollbar_cmd, ...)
  })
  tkpack(scrollbar_cmd, side = "right", fill = "y")
  tkpack(APP_STATE$txt_perintah, side = "left", fill = "both", expand = TRUE)

  res_frame <- ttklabelframe(out_frame, text = " Output ", padding = 5)
  tkgrid(res_frame, row = 0, column = 1, sticky = "nsew", padx = c(5, 0))
  res_subframe <- ttkframe(res_frame)
  tkpack(res_subframe, fill = "both", expand = TRUE)

  APP_STATE$txt_output <- tktext(
    res_subframe,
    height = 10,
    width = 20,
    font = "Courier 10",
    state = "disabled",
    bg = "#f8f9fa",
    wrap = "word"
  )
  scrollbar <- ttkscrollbar(res_subframe, command = function(...) {
    tkyview(APP_STATE$txt_output, ...)
  })
  tkconfigure(APP_STATE$txt_output, yscrollcommand = function(...) {
    tkset(scrollbar, ...)
  })
  tkpack(scrollbar, side = "right", fill = "y")
  tkpack(APP_STATE$txt_output, side = "left", fill = "both", expand = TRUE)

  tkgrid.columnconfigure(out_frame, 0, weight = 1)
  tkgrid.columnconfigure(out_frame, 1, weight = 1)
  tkgrid.rowconfigure(out_frame, 0, weight = 1)

  # Conclusion
  APP_STATE$var_conclusion <- tclVar("")
  APP_STATE$lbl_conclusion <- ttklabel(
    main,
    textvariable = APP_STATE$var_conclusion,
    font = "Arial 11 bold",
    justify = "left"
  )
  tkpack(APP_STATE$lbl_conclusion, side = "bottom", anchor = "w", pady = 5)

  # 4. BUTTONS
  btn_frame <- ttkframe(main)
  tkpack(btn_frame, side = "bottom", pady = c(10, 5))
  tkpack(
    ttkbutton(
      btn_frame,
      text = "HITUNG (UJI VARIANS)",
      command = run_uji_variansi_2pop
    ),
    side = "left",
    padx = 10
  )
  tkpack(
    ttkbutton(btn_frame, text = "TUTUP", command = function() {
      tkdestroy(Main_page)
    }),
    side = "left",
    padx = 10
  )

  update_perintah()
  if (interactive()) tkwait.window(Main_page)
}

Start_open()
