#=============================================================================
# Program Uji Hipotesis Variansi Satu dan Dua Populasi
# Mata kuliah Compt Stat
#=============================================================================

library(tcltk)
library(tcltk2)
library(readxl)
library(readr)
library(foreign) # readxl, readr, foreign buat baca data import

# Wanra Global 
options(Rgui.theme_bl = list(
  primary   = "#dce3ec",
  secondary = "#294563",
  background= "#f5f7fa",
  border    = "#bcc3cc"
))

# Font Global
tcl("options", "add", )

#=============================================================================
# 1. Struktur utama dan tabel
#=============================================================================

# 1.1 Main window
Start_open <- function() {
  Main_page <- tktoplevel()

  # Title Bar
  tkwm.title(Main_page,  "... - Uji Hipotesis μ 1|2 Populasi")

  # Main window setting (color, frame size)
  tkconfigure(Main_page, background = getOption("Rgui.theme_bl")$primary)
  tkwm.geometry(Main_page, "1050x800")
tkwm.minsize(Main_page, "800x600") 

  # Main container
  main_container <- tkframe(Main_page, background = getOption("Rgui.theme_bl")$primary)
  tkpack(main_container, fill = "both", expand = TRUE, padx = 10, pady = 10)


  # 1.2 topmenu

    # 1.2.1 topmenu member
  Submenu_container <- list(
  
    # File 
  label = "File",
  children = list(
    list(
      label = "Import Data",
      children = list(
        list(label = "Excel", command = "Excel_input"),
        list(label = "CSV", command = "CSV_input"),
        list(label = "SPSS", command = "SPSS_input"),
		list(label = "text", command = "Text_input")
        )
     )
  ),

    # Uji hipotesis var
  label = "Pengujian Hipotesis",
  children = list(
    list(
      label = "Variansi",
      children = list(
		list(label = "Satu Populasi", command= "..."),
		list(label = "Dua Populasi", command= "...")
#        list(label = "Summary", command = "sumvar1"),
#       list(label = "Input Data", command = "manualvar1"),
#        list(label = "Import Data SPSS", command = "import1pop")
		  )
		)
      ),

    # Materi 
  label = "Materi",
  children = list(
    list(
      label = "Import Data",
      children = list(
        list(label = "Excel", command = "sumvar1"),
        list(label = "CSV", command = "manualvar1"),
        list(label = "SPSS", command = "import1pop"),
		list(label = "txt", command = "")
      )
    )
  ),

    # View 
  label = "View",
  children = list(
    list(label = "Console", command = ""),
    list(label = "Switch Theme", command = "")  
    ),

    # Petunjuk 
  label = "Petunjuk",
  children = list(
    list(
      label = "Uji Hipotesis",
      children = list(
        list(label = "Variansi", command = "sumvar1")
      )
    )
  ),

    # Tentang kami 
  label = "Tentang kami",
  children = list(
    list(label = "Developer", command = "..."),
    list(label = "Mata kuliah", command = "...")
    )
)


    # 1.2.2 topmenu built behavior function
    # Fungsi untuk merakit menu dari struktur data
  build_menu <- function(parent_menu, structure) {
  for (item in structure) {
    if (!is.null(item$children)) {
      # Jika punya anak (submenu), buat cascade
      submenu <- tkmenu(parent_menu, tearoff = FALSE)
      tkadd(parent_menu, "cascade", label = item$label, menu = submenu)
      # Panggil fungsi ini lagi untuk merakit isi submenu
      build_menu(submenu, item$children)
    } else {
      # Jika tidak punya submenu, ini tombol, atau click run command
      if (!is.null(item$command)) {
        # Simpan nama fungsi ke variabel lokal
        func_name <- item$command 
        
        # Tambahkan ke menu Tk
        tkadd(parent_menu, "command", 
              label = item$label, 
              command = function() {
                # get() akan mencari fungsi di memori SAAT TOMBOL DIKLIK
                do.call(get(func_name, envir = .GlobalEnv), list())
              })
      }
    }
  }


  # 1.3 Main window guide



#=============================================================================
# 2. Input Data
#=============================================================================

# 2.1 Central data memory (Need initiated first)
if (!exists("APP_STATE")) {
  APP_STATE <- new.env()
  APP_STATE$data <- data.frame()        # Data aktual (baris = observasi, kolom = variabel)
  APP_STATE$variable_info <- data.frame( # Metadata untuk "Variable View"
    name = character(),
    type = character(),      # "numeric", "character", "factor"
    label = character(),     # Label deskriptif
    stringsAsFactors = FALSE
  )
  APP_STATE$is_dirty <- FALSE # Penanda apakah ada perubahan yang belum disimpan
}

# Data View 2.2

  # 2.1.1 Data view body function
create_data_view <- function(parent, state) {
  # Frame pembungkus
  frame_data <- tkframe(parent, relief = "groove", borderwidth = 2)
  tkpack(frame_data, fill = "both", expand = TRUE, padx = 5, pady = 5)

  # Buat tclArray sebagai jembatan data
  data_array <- tclArray()
  
  # Isi array dengan data dari memori pusat
  refresh_data_table(data_array, state$data)

  # Buat widget Tktable
  # Gunakan 'titlerows=1' dan 'titlecols=1' untuk header
  data_table <- tkwidget(
    frame_data, "table",
    variable = data_array,
    rows = max(1, nrow(state$data) + 1), # +1 untuk header
    cols = max(1, ncol(state$data) + 1),
    titlerows = 1,
    titlecols = 1,
    selectmode = "extended",
    colwidth = 10,
    rowheight = 2,
    background = "#f5f7fa"
  )
  tkpack(data_table, fill = "both", expand = TRUE)

  # Simpan referensi agar bisa diakses fungsi lain
  state$data_table_widget <- data_table
  state$data_array <- data_array
}

# Fungsi untuk menyegarkan tabel dari data.frame
refresh_data_table <- function(data_array, df) {
  # Bersihkan array (kecuali header)
  # ... (implementasi pembersihan)
  
  # Isi header kolom (baris 0, kolom 1..n)
  if (ncol(df) > 0) {
    for (j in 1:ncol(df)) {
      data_array[[0, j]] <- as.tclObj(names(df)[j])
    }
  }
  
  # Isi data (baris 1..n, kolom 1..n)
  for (i in 1:nrow(df)) {
    for (j in 1:ncol(df)) {
      data_array[[i, j]] <- as.tclObj(as.character(df[i, j]))
    }
  }
}

  # 2.1.2 menyimpan dari Tktable ke APP_STATE$data (memory central)
sync_data_view_to_state <- function(state) {
  n_rows <- as.integer(tclvalue(tkindex(state$data_table_widget, "end", "row"))) - 1
  n_cols <- as.integer(tclvalue(tkindex(state$data_table_widget, "end", "col"))) - 1
  
  if (n_rows < 1 || n_cols < 1) return()
  
  new_df <- data.frame(matrix(NA, nrow = n_rows, ncol = n_cols))
  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      new_df[i, j] <- tclvalue(state$data_array[[i, j]])
    }
  }
  colnames(new_df) <- sapply(1:n_cols, function(j) tclvalue(state$data_array[[0, j]]))
  
  # Konversi tipe data berdasarkan metadata
  new_df <- coerce_types(new_df, state$variable_info)
  
  state$data <- new_df
  state$is_dirty <- TRUE
}


# 2.2 Variable view

  # 2.2.1 Var view body function
create_variable_view <- function(parent, state) {
  frame_var <- tkframe(parent, relief = "groove", borderwidth = 2)
  tkpack(frame_var, fill = "both", expand = TRUE, padx = 5, pady = 5)
  
  var_array <- tclArray()
  refresh_variable_table(var_array, state$variable_info)
  
  var_table <- tkwidget(
    frame_var, "table",
    variable = var_array,
    rows = max(1, nrow(state$variable_info) + 1),
    cols = 4, # name, type, label, dll.
    titlerows = 1,
    titlecols = 1,
    selectmode = "browse",
    colwidth = 15,
    background = "#f5f7fa"
  )
  tkpack(var_table, fill = "both", expand = TRUE)
  
  # Validasi sel (contoh untuk kolom 'type')
  # (Implementasi vcmd akan dijelaskan di bawah)
  
  state$var_table_widget <- var_table
  state$var_array <- var_array
}

  # 2.2.2 Var view input validation
# validasi 'type'
validate_type <- function(S) {
  valid_types <- c("numeric", "character", "factor", "integer", "logical")
  if (S %in% valid_types || S == "") {
    return(tcl("expr", TRUE))
  } else {
    tkmessageBox(message = "Tipe data tidak valid! Pilih: numeric, character, factor, dll.")
    return(tcl("expr", FALSE))
  }
}


# 2.3 Konfirmasi impor data
import_data_with_confirmation <- function(file_path, file_type, state) {
  # 1. Cek apakah ada data
  if (nrow(state$data) > 0 || ncol(state$data) > 0) {
    response <- tkmessageBox(
      title = "Konfirmasi Impor",
      message = "Data yang ada akan ditimpa. Lanjutkan?",
      type = "yesno",
      icon = "warning"
    )
    if (tclvalue(response) != "yes") return(FALSE)
  }
  
  # 2. Baca file sesuai tipe
  new_data <- switch(file_type,
    "xlsx" = readxl::read_excel(file_path),
    "csv"  = readr::read_csv(file_path),
    "spss" = foreign::read.spss(file_path, to.data.frame = TRUE),
    "txt"  = readr::read_tsv(file_path)
  )
  
  # 3. Perbarui memori pusat
  state$data <- new_data
  state$variable_info <- infer_variable_info(new_data) # Fungsi inferensi tipe
  
  # 4. Segarkan UI
  refresh_data_table(state$data_array, state$data)
  refresh_variable_table(state$var_array, state$variable_info)
  
  return(TRUE)
}

# 2.4 Sinkron tipe data jika ada perubahan numeric ~ character (berdasarkan variable info)
coerce_types <- function(df, var_info) {
  for (i in seq_len(ncol(df))) {
    col_name <- names(df)[i]
    target_type <- var_info$type[var_info$name == col_name]
    
    if (length(target_type) == 0) next
    
    df[[i]] <- switch(target_type,
      "numeric"   = as.numeric(df[[i]]),
      "integer"   = as.integer(df[[i]]),
      "factor"    = as.factor(df[[i]]),
      "character" = as.character(df[[i]]),
      "logical"   = as.logical(df[[i]]),
      df[[i]] # default: biarkan
    )
  }
  return(df)
}

#=============================================================================
# 3.  Pengujian Hipotesis Var 1-population
#=============================================================================

#------------SATU POPULASI----------------#
tkadd(topmenu1, "cascade", label="Satu Populasi", menu=sub_m1.1)

#-------SUMMARY------#
sumvar1=function()
{
  require(tcltk)
  require(tcltk2)
  
  w1 = tktoplevel(bg = getOption("Rgui.theme_bl")$background)
  tktitle(w1) = "Summary Data"
  tkgrid(tklabel(w1,text="Summa<F12>ry Data Pengujian Hipotesis Variansi Satu Populasi",font=teks1,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  
  banyak = tclVar("  ")
  eb1 = tkentry(w1,width="20",textvariable = banyak,bg = getOption("Rgui.theme_bl")$primary)
  tkgrid(tklabel(w1,text="Banyak sampel :",font=teks2,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(eb1)
  
  s2 = tclVar(" ")
  eb2 = tkentry(w1,width="20",textvariable = s2,bg = getOption("Rgui.theme_bl")$primary)
  tkgrid(tklabel(w1,text="Variansi sampel (S^2) :",font=teks2,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(eb2)
  
  sig2 = tclVar(" ")
  eb3 = tkentry(w1,width="20",textvariable=sig2,bg = getOption("Rgui.theme_bl")$primary)
  tkgrid(tklabel(w1,text="Variansi populasi (σ0^2) :",font=teks2,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(eb3)
  
  alphaa = tclVar(" ")
  eb4=tkentry(w1,width="20",textvariable=alphaa,bg = getOption("Rgui.theme_bl")$primary)
  tkgrid(tklabel(w1,text="Alpha (dalam %) :",font=teks2,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(eb4)
  
  outputsum1 = function()
  {
    h0 = "Variansi populasi ="
    h1 = "Variansi populasi <"
    dk = "\n2. Daerah Kritis \t: Tolak H0 jika Chi-square Hitung < Chi-square Tabel "
    n = as.numeric(tclvalue(banyak))
    vs = as.numeric(tclvalue(s2))
    vp = as.numeric(tclvalue(sig2))
    alpha = as.numeric(tclvalue(alphaa))/100
    cch =((n-1)*vs)/vp
    chh = round(cch,4)
    cht = round(qchisq(1-alpha,n-1,lower.tail = FALSE),4)
    
    if(chh<cht)
    {
      w11 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
      tktitle(w11) = "Hasil Analisis Data"
      insert = tktext(w11,font=teks2,width=82,height=18,bg = getOption("Rgui.theme_bl")$primary)
      tkgrid(tklabel(w11,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
      tkgrid(insert)
      tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,vp,"
     H1 :",h1,vp," ",dk,"
     
3. Statistik Uji\t:
     Chi-Square hitung \t=",chh,"
     Derajat bebast \t\t=",n-1,"
     Chi-square tabel \t\t=",cht,"
     
4. Keputusan \t\t: Tolak H0
5. Kesimpulan \t\t: ",h1,vp))
    }
    else
    {
      w11 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
      tktitle(w11) = "Hasil Analisis Data"
      insert = tktext(w11,font=teks2,width=82,height=18,bg = getOption("Rgui.theme_bl")$primary)
      tkgrid(tklabel(w11,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
      tkgrid(insert)
      tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,vp,"
     H1 :",h1,vp," ",dk,"
     
3. Statistik Uji \t:
     Chi-Square \t\t=",chh,"
     Derajat bebas  \t\t=",n-1,"
     Chi-square tabel \t=",cht,"
     
4. Keputusan \t\t: Terima H0
5. Kesimpulan \t\t: ",h0,vp))
    }
    tombol=tkbutton(w11,text="Kembali",command=function()tkdestroy(w11),bg = getOption("Rgui.theme_bl")$background)
    tkgrid(tombol)
    tkgrid(tklabel(w11,text=" ",bg = getOption("Rgui.theme_bl")$primary))
  }
  
  outputsum2 = function()
  {
    h0 = "Variansi populasi ="
    h1 = "Variansi populasi ≠"
    dk = "\n2. Daerah Kritis \t: Tolak H0 jika Chi-square Hitung < Chi-square Tabel 1 atau tolak H0 jika Chi-Square hitung > Chi-Square tabel 2 "
    n = as.numeric(tclvalue(banyak))
    vs = as.numeric(tclvalue(s2))
    vp = as.numeric(tclvalue(sig2))
    alpha = as.numeric(tclvalue(alphaa))/100
    chh = ((n-1)*vs)/vp
    cht1 = round(qchisq(1-(alpha/2),n-1,lower.tail = FALSE),4)
    cht2 = round(qchisq((alpha/2),n-1,lower.tail = FALSE),4)
    
    if(chh<cht1)
    {
      w11 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
      tktitle(w11) = "Hasil Analisis Data"
      insert = tktext(w11,font=teks2,width=85,height=20,bg = getOption("Rgui.theme_bl")$primary)
      tkgrid(tklabel(w11,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
      tkgrid(insert)
      tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,vp,"
     H1 :",h1,vp," 
     ",dk,"

3. Statistik Uji \t:
     Chi-Square hitung\t\t=",chh,"
     Derajat bebas  \t\t  =",n-1,"
     Chi-Square tabel 1 \t=",cht1,"
     Chi-Square tabel 2 \t=",cht2,"
     
4. Keputusan \t\t: Tolak H0
5. Kesimpulan \t\t: ",h1,vp))
    }
    else if(chh>cht2)
    {
      w11 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
      tktitle(w11) = "Hasil Analisis Data"
      insert = tktext(w11,font=teks2,width=85,height=20,bg = getOption("Rgui.theme_bl")$primary)
      tkgrid(tklabel(w11,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
      tkgrid(insert)
      tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,vp,"
     H1 :",h1,vp," 
     ",dk,"

3. Statistik Uji \t:
     Chi-Square hitung\t\t=",chh,"
     Derajat bebas \t\t   =",n-1,"
     Chi-Square tabel 1 \t=",cht1,"
     Chi-Square tabel 2 \t=",cht2,"
     P-value \t\t=", round(pvalue,4),"
     
4. Keputusan \t\t: Tolak H0
5. Kesimpulan \t\t: ",h1,vp))
    }
    else
    {
      w11 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
      tktitle(w11) = "Hasil Analisis Data"
      insert = tktext(w11,font=teks2,width=85,height=20,bg = getOption("Rgui.theme_bl")$primary)
      tkgrid(tklabel(w11,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
      tkgrid(insert)
      tkinsert(insert,"end",paste("1. Hipotesis
     ",h0,vp,"
     ",h1,vp," 
     ",dk,"
     
3. Statistik Uji \t:
     Chi-Square hitung\t\t=",chh,"
     Derajat bebas \t     =",n-1,"
     Chi-Square tabel 1 \t=",cht1,"
     Chi-Square tabel 2 \t=",cht2,"
     
4. Keputusan \t\t: Terima H0
5. Kesimpulan \t\t: ",h0,vp))
    }
    tombol=tkbutton(w11,text="Kembali",command=function()tkdestroy(w11),bg = getOption("Rgui.theme_bl")$background)
    tkgrid(tombol)
    tkgrid(tklabel(w11,text=" ",bg = getOption("Rgui.theme_bl")$primary))
  }
  
  outputsum3 = function()
  {
    h0 = "Variansi populasi ="
    h1 = "Variansi populasi >"
    dk = "\n2. Daerah Kritis: Tolak H0 jika Chi-square Hitung > Chi-square Tabel "
    n = as.numeric(tclvalue(banyak))
    vs = as.numeric(tclvalue(s2))
    vp = as.numeric(tclvalue(sig2))
    alpha = as.numeric(tclvalue(alphaa))/100
    chh = round(((n-1)*vs)/vp,4)
    cht = round(qchisq(alpha,n-1,lower.tail = FALSE),4)
    
    if(chh > cht)
    {
      w11 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
      tktitle(w11) = "Hasil Analisis Data"
      insert = tktext(w11,font=teks2,width=82,height=18,bg = getOption("Rgui.theme_bl")$primary)
      tkgrid(tklabel(w11,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
      tkgrid(insert)
      tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,vp,"
     H1 :",h1,vp," ",dk,"
     
3. Statistik Uji \t:
     Chi-Square hitung\t\t=",chh,"
     Derajat bebas \t\t=",n-1,"
     Chi-square tabel \t=",cht,"
     
4. Keputusan \t\t: Tolak H0
5. Kesimpulan \t\t: ",h1,vp))
    }
    else
    {
      w11 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
      tktitle(w11) = "Hasil Analisis Data"
      insert = tktext(w11,font=teks2,width=82,height=18,bg = getOption("Rgui.theme_bl")$primary)
      tkgrid(tklabel(w11,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
      tkgrid(insert)
      tkinsert(insert,"end",paste("1. Hipotesis
     H1 :",h0,vp,"
     H0 :",h1,vp," ",dk,"
     
3. Statistik Uji \t:
     Chi-Square hitung\t\t=",chh,"
     Derajat bebas \t\t=",n-1,"
     Chi-square tabel \t=",cht,"
     
4. Keputusan \t\t: Terima H0
5. Kesimpulan \t\t: ",h0,vp))
    }
    tombol=tkbutton(w11,text="Kembali",command=function()tkdestroy(w11),bg = getOption("Rgui.theme_bl")$background)
    tkgrid(tombol)
    tkgrid(tklabel(w11,text=" ",bg = getOption("Rgui.theme_bl")$primary))
  }
  #pilihan arah uji hipotesis satu populasi
  tkgrid(tklabel(w1,text="",bg = getOption("Rgui.theme_bl")$background))
  tkgrid(tklabel(w1,text="Pilih uji hipotesis",font=teks3,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  kiri =tkbutton(w1,text="Sisi Kiri",command = function()outputsum1(),bg = getOption("Rgui.theme_bl")$secondary)
  tkgrid(kiri,sticky="news")
  sisi2=tkbutton(w1,text="2 Sisi",command=function()outputsum2(),bg = getOption("Rgui.theme_bl")$secondary)
  tkgrid(sisi2,sticky="news")
  kanan=tkbutton(w1,text="Sisi Kanan",command=function()outputsum3(),bg = getOption("Rgui.theme_bl")$secondary)
  tkgrid(kanan,sticky="news")
}

tkadd(sub_m1.1, "command", label="Summary", command = sumvar1)

#---------INPUT MANUAL---------#
manualvar1 = function()
{
  require(tcltk)
  require(tcltk2)
  
  w2 = tktoplevel(bg = getOption("Rgui.theme_bl")$background)
  tktitle(w2) = "Input Manual Data"
  tkgrid(tklabel(w2,text="Input Data Pengujian Hipotesis Variansi Satu Populasi",font=teks1,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  
  banyak1 = tclVar("  ")
  eb1 = tkentry(w2,width="20",textvariable = banyak1,bg = getOption("Rgui.theme_bl")$primary)
  tkgrid(tklabel(w2,text="Banyak sampel :",font=teks2,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(eb1)
  
  alfa = tclVar(" ")
  eb2=tkentry(w2,width="20",textvariable=alfa,bg = getOption("Rgui.theme_bl")$primary)
  tkgrid(tklabel(w2,text="Alpha (dalam %) :",font=teks2,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(eb2)
  tkgrid(tklabel(w2,text=" ",bg = getOption("Rgui.theme_bl")$background))
  input = tkbutton(w2,text="Input",command = function()inputvar(),bg = getOption("Rgui.theme_bl")$secondary)
  tkgrid(input)
  
  inputvar = function()
  {
    vp = tclVar(" ")
    n = as.numeric(tclvalue(banyak1))
    w21 = tktoplevel(bg = getOption("Rgui.theme_bl")$secondary)
    tktitle(w21) = "Input Data"
    tkgrid(tklabel(w21,text="Input Data",font=teks1,bg = getOption("Rgui.theme_bl")$secondary),sticky="news")
    tkgrid(tklabel(w21,text="",bg = getOption("Rgui.theme_bl")$secondary))
    
    #layout tabel
    tclRequire("Tktable")
    x = vector(length=n)
    layoutinput = tclArray()
    for(i in 1:n){
      layoutinput[[i,0]]=i
    }
    layoutinput[[0,1]]="n"
    
    #tabel
    tabel=tkwidget(w21,"table",variable=layoutinput,rows=n+1,cols=2,titlerows=1,titlecols=1,selectmode="extended  ",colwidth=8,rowheight=2)
    tkconfigure(tabel,selectmode="extended",rowseparator="\n",colseparator="\t")
    tkgrid(tklabel(w21,text="Data: ",bg = getOption("Rgui.theme_bl")$secondary),tabel)
    tkgrid(tklabel(w21,text=" ",bg = getOption("Rgui.theme_bl")$secondary))
    tkgrid(tklabel(w21,text="Varians Populasi: ",bg = getOption("Rgui.theme_bl")$secondary),tkentry(w21,width="10",textvariable=vp))
    tkgrid(tklabel(w21,text="",bg = getOption("Rgui.theme_bl")$secondary))
    
    outputmanual1 = function()
    {
      h0 = "Variansi populasi ="
      h1 = "Variansi populasi <"
      dk = "\n2. Daerah Kritis \t: Tolak H0 jika Chi-square Hitung < Chi-square Tabel atau Pvalue < Alpha"
      n = as.numeric(tclvalue(banyak1))
      
      tclRequire("Tktable")
      for(i in 1:n)
      {
        x[i]=as.numeric(layoutinput[[i,1]])
      }
      varpop = as.numeric(tclvalue(vp))
      alpha = as.numeric(tclvalue(alfa))/100
      chisq = round(varTest(x,alternative="less",conf.level=(1-alpha),sigma.squared=varpop)$statistic,4)
      pvalue = round(varTest(x,alternative="less",conf.level=(1-alpha),sigma.squared=varpop)$p.value,4)
      cht = round(qchisq(alpha,n-1),4)
      
      if(pvalue<alpha)
      {
        w211 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(w211) = "Hasil Analisis Data"
        insert = tktext(w211,font=teks2,bg = getOption("Rgui.theme_bl")$primary,width=82,height=15)
        tkgrid(tklabel(w211,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,varpop,"
     H1 :",h1,varpop," ",dk,"
     
3. Statistik Uji   :
     Chi-Square hitung \t=",chisq,"
     Chi-square tabel \t=",cht,"
     P-Value \t\t=",pvalue,"
     
4. Keputusan        : Tolak H0
5. Kesimpulan       : ",h1,varpop))
      }
      else
      {
        w211 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(w211) = "Hasil Analisis Data"
        insert = tktext(w211,font=teks2,bg = getOption("Rgui.theme_bl")$primary,width=82,height=15)
        tkgrid(tklabel(w211,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,varpop,"
     H1 :",h1,varpop," ",dk,"
     
3. Statistik Uji   :
     Chi-Square hitung \t=",chisq,"
     Chi-Square tabel \t=",cht,"
     P-value \t\t=",pvalue,"
     
4. Keputusan         : Terima H0
5. Kesimpulan        : ",h0,varpop))
      }
      tombol=tkbutton(w211,text="Kembali",command=function()tkdestroy(w211),bg = getOption("Rgui.theme_bl")$background)
      tkgrid(tombol)
      tkgrid(tklabel(w211,text=" ",bg = getOption("Rgui.theme_bl")$primary))
    }
    
    outputmanual2 = function()
    {
      h0 = "Variansi populasi ="
      h1 = "Variansi populasi ≠"
      dk = "\n2. Daerah Kritis \t: Tolak H0 jika Pvalue < Alpha"
      n = as.numeric(tclvalue(banyak1))
      
      tclRequire("Tktable")
      for(i in 1:n)
      {
        x[i]=as.numeric(layoutinput[[i,1]])
      }
      varpop = as.numeric(tclvalue(vp))
      alpha = as.numeric(tclvalue(alfa))/100
      chisq = round(varTest(x,alternative="two.sided",conf.level=(1-alpha),sigma.squared=varpop)$statistic,4)
      pvalue = round(varTest(x,alternative="two.sided",conf.level=(1-alpha),sigma.squared=varpop)$p.value,4)
      cht2 = round(qchisq((1-(alpha/2)),n-1),4)
      cht1 = round(qchisq((alpha/2),n-1),4)
      
      if(pvalue<alpha/2)
      {
        w211 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(w211) = "Hasil Analisis Data"
        insert = tktext(w211,font=teks2,width=85,height=17,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(w211,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,varpop,"
     H1 :",h1,varpop," ",dk,"
     
3. Statistik Uji   :
     Chi-Square         =",chisq,"
     Chi-Square tabel 1 =",cht1,"
     Chi-Square tabel 2 =",cht2,"
     P-Value            =",pvalue,"
     
4. Keputusan          : Tolak H0
5. Kesimpulan         : ",h1,varpop))
      }
      else if(pvalue>(1-(alpha/2)))
      {
        w211 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(w211) = "Hasil Analisis Data"
        insert = tktext(w211,font=teks2,width=85,height=17,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(w211,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,varpop,"
     H1 :",h1,varpop," ",dk,"
     
3. Statistik Uji   :
     Chi-Square         =",chisq,"
     Chi-Square tabel 1 =",cht1,"
     Chi-Square tabel 2 =",cht2,"
     P-Value            =",pvalue,"
     
4. Keputusan          : Tolak H0
5. Kesimpulan         : ",h1,varpop))
      }
      else
      {
        w211 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(w211) = "Hasil Analisis Data"
        insert = tktext(w211,font=teks2,width=85,height=17,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(w211,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,varpop,"
     H1 :",h1,varpop," ",dk,"
     
3. Statistik Uji   :
     Chi-Square hitung \t=",chisq,"
     Chi-Square tabel 1 \t=",cht1,"
     Chi-Square tabel 2 \t=",cht2,"
     P-Value \t\t =",pvalue,"
     
4. Keputusan          : Terima H0
5. Kesimpulan         : ",h0,varpop))
      }
      tombol=tkbutton(w211,text="Kembali",command=function()tkdestroy(w211),bg = getOption("Rgui.theme_bl")$background)
      tkgrid(tombol)
      tkgrid(tklabel(w211,text=" ",bg = getOption("Rgui.theme_bl")$primary))
    }
    
    outputmanual3 = function()
    {
      h0 = "Variansi populasi ="
      h1 = "Variansi populasi >"
      dk = "\n2. Daerah Kritis: Tolak H0 jika Chi-square Hitung > Chi-square Tabel atau tolak H0 jika Pvalue < Alpha"
      n = as.numeric(tclvalue(banyak1))
      
      tclRequire("Tktable")
      for(i in 1:n)
      {
        x[i]=as.numeric(layoutinput[[i,1]])
      }
      varpop = as.numeric(tclvalue(vp))
      alpha = as.numeric(tclvalue(alfa))/100
      chisq = round(varTest(x,alternative="greater",conf.level=(1-alpha),sigma.squared=varpop)$statistic,4)
      pvalue = round(varTest(x,alternative="greater",conf.level=(1-alpha),sigma.squared=varpop)$p.value,4)
      cht = round(qchisq(1-alpha,n-1),4)
      
      if(pvalue<alpha)
      {
        w211 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(w211) = "Hasil Analisis Data"
        insert = tktext(w211,font=teks2,width=82,height=15,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(w211,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,varpop,"
     H1 :",h1,varpop," ",dk,"
     
3. Statistik Uji   :
     Chi-Square hitung \t=",chisq,"
     Chi-square tabel \t=",cht,"
     P-Value \t\t=",pvalue,"
     
4. Keputusan        : Tolak H0
5. Kesimpulan       : ",h1,varpop))
      }
      else
      {
        w211 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(w211) = "Hasil Analisis Data"
        insert = tktext(w211,font=teks2,bg = getOption("Rgui.theme_bl")$primary,width=82,height=15)
        tkgrid(tklabel(w211,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,varpop,"
     H1 :",h1,varpop," ",dk,"
     
3. Statistik Uji   :
     Chi-Square hitung \t=",chisq,"
     Chi-square tabel \t=",cht,"
     P-Value \t\t=",pvalue,"
     
4. Keputusan        : Terima H0
5. Kesimpulan       : ",h0,varpop))
      }
      tombol=tkbutton(w211,text="Kembali",command=function()tkdestroy(w211),bg = getOption("Rgui.theme_bl")$background)
      tkgrid(tombol)
      tkgrid(tklabel(w211,text=" ",bg = getOption("Rgui.theme_bl")$primary))
    }
    
    bkiri =tkbutton(w21,text="Sisi Kiri",command = function()outputmanual1(),bg = getOption("Rgui.theme_bl")$secondary)
    b2sisi=tkbutton(w21,text="2 Sisi",command=function()outputmanual2(),bg = getOption("Rgui.theme_bl")$secondary)
    bkanan=tkbutton(w21,text="Sisi Kanan",command=function()outputmanual3(),bg = getOption("Rgui.theme_bl")$secondary)
    tkgrid(bkiri,b2sisi,bkanan,sticky="we")
  }
}

tkadd(sub_m1.1, "command", label="Input Data", command= manualvar1)

#---------IMPORT DATA---------#
tkadd(sub_m1.1, "cascade", label="Import Data", menu=sub2_m1.1)

import1pop = function()
{
  jendelaimport1 = tktoplevel(bg = getOption("Rgui.theme_bl")$background)
  tktitle(jendelaimport1) = "Import Data"
  tkgrid(tklabel(jendelaimport1, text="Import Data SPSS Pengujian Hipotesis Variansi Satu Populasi", font = teks1,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(tklabel(jendelaimport1,text=" ",bg = getOption("Rgui.theme_bl")$background))
  tkgrid(tklabel(jendelaimport1,text="Pastikan data format Anda berupa .sav",font=teks3,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(tklabel(jendelaimport1,text=" ",bg = getOption("Rgui.theme_bl")$background))
  
  alphaa = tclVar(" ")
  eb1 = tkentry(jendelaimport1,width="20",textvariable=alphaa,bg = getOption("Rgui.theme_bl")$primary)
  tkgrid(tklabel(jendelaimport1,text="Alpha (dalam %) :",font=teks2,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(eb1)
  
  varpop = tclVar(" ")
  eb2 = tkentry(jendelaimport1,width="20",textvariable=varpop,bg = getOption("Rgui.theme_bl")$primary)
  tkgrid(tklabel(jendelaimport1,text="Variansi Populasi :",font=teks2,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(eb2)
  
  tkgrid(tklabel(jendelaimport1,text=" ",bg = getOption("Rgui.theme_bl")$background))
  import=tkbutton(jendelaimport1,text="Import",command = function()import1(),bg = getOption("Rgui.theme_bl")$secondary)
  tkgrid(import)
  tkgrid(tklabel(jendelaimport1,text=" ",bg = getOption("Rgui.theme_bl")$background))
  
  import1 = function()
  {
    library(tcltk)
    library(foreign)
    data = read.spss(choose.files(),use.value.labels = TRUE, max.value.labels = Inf, to.data.frame = TRUE)
    alpha = as.numeric(tclvalue(alphaa))/100
    vp = as.numeric(tclvalue(varpop))
    m = as.matrix(data)
    x = as.numeric(m[,1])
    n = length(x[!is.na(x)])
    
    
    lihatdata = function()
    {
      View(data,"DATA")
    }
    
    ujisisikiri = function()
    {
      h0 = "Variansi populasi ="
      h1 = "Variansi populasi <"
      dk = "\n2. Daerah Kritis: Tolak H0 jika Pvalue < Alpha "
      chisq = round(varTest(x,alternative="less",conf.level=(1-alpha),sigma.squared=vp)$statistic,4)
      pvalue = round(varTest(x,alternative="less",conf.level=(1-alpha),sigma.squared=vp)$p.value,4)
      
      if(pvalue<alpha)
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=15,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,vp,"
     H1 :",h1,vp,"",dk,"
     
3. Statistik Uji   :
     Rata-rata Populasi \t\t: ",round(sum(x, na.rm=TRUE)/n,4),"
     Standar Deviasi    \t\t: ",round(sd(x, na.rm=TRUE),4),"
     Chi-Square hitung \t=",chisq,"
     Alpha  \t\t=",alpha,"
     P-Value  \t\t=",pvalue,"
     
4. Keputusan \t\t: Tolak H0
5. Kesimpulan \t\t:",h1,vp))
      }
      else
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=15,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,vp,"
     H1 :",h1,vp,"",dk,"
     
3. Statistik Uji   :
     Rata-rata Populasi \t\t: ",round(sum(x, na.rm=TRUE)/n,4),"
     Standar Deviasi    \t\t: ",round(sd(x, na.rm=TRUE),4),"
     Chi-Square hitung \t=",chisq,"
     Alpha  \t\t=",alpha,"
     P-Value  \t\t=",pvalue,"
     
4. Keputusan \t\t: Terima H0
5. Kesimpulan \t\t:",h0,vp))
      }
      tombol=tkbutton(jendelaoutput,text="Kembali",command=function()tkdestroy(jendelaoutput),bg = getOption("Rgui.theme_bl")$background)
      tkgrid(tombol)
      tkgrid(tklabel(jendelaoutput,text=" ",bg = getOption("Rgui.theme_bl")$primary))
    }
    
    ujiduasisi = function()
    {
      h0 = "Variansi populasi ="
      h1 = "Variansi populasi ≠"
      dk = "\n2. Daerah Kritis: Tolak H0 jika Pvalue < Alpha"
      chisq = round(varTest(x,alternative="two.sided",conf.level=(1-alpha),sigma.squared=vp)$statistic,4)
      pvalue = round(varTest(x,alternative="two.sided",conf.level=(1-alpha),sigma.squared=vp)$p.value,4)
      
      if(pvalue<(alpha/2))
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=85,height=15,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,vp,"
     H1 :",h1,vp,"",dk,"
     
3. Statistik Uji   :
     Rata-rata Populasi \t\t: ",round(sum(x, na.rm=TRUE)/n,4),"
     standar Deviasi    \t\t: ",round(sd(x, na.rm=TRUE),4),"
     Chi-Square hitung \t=",chisq,"
     Alpha \t\t=",alpha,"
     P-Value \t\t=",pvalue,"
     
4. Keputusan \t\t: Tolak H0
5. Kesimpulan \t\t:",h1,vp))
      }
      else if(pvalue<(alpha/2))
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=85,height=15,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,vp,"
     H1 :",h1,vp,"",dk,"
     
3. Statistik Uji   :
     Rata-rata Populasi \t\t: ",round(sum(x, na.rm=TRUE)/n,4),"
     Standar Deviasi    \t\t: ",round(sd(x, na.rm=TRUE),4),"
     Chi-Square hitung \t=",chisq,"
     Alpha \t\t=",alpha,"
     P-Value \t\t=",pvalue,"
     
4. Keputusan \t\t: Tolak H0
5. Kesimpulan \t\t:",h1,vp))
      }
      else
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=85,height=15,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,vp,"
     H1 :",h1,vp,"",dk,"
     
3. Statistik Uji   :
     Rata-rata Populasi \t\t: ",round(sum(x, na.rm=TRUE)/n,4),"
     Standar Deviasi    \t\t: ",round(sd(x, na.rm=TRUE),4),"
     Chi-Square hitung \t=",chisq,"
     Alpha \t\t=",alpha,"
     P-Value \t\t=",pvalue,"
     
4. Keputusan \t\t: Terima H0
5. Kesimpulan \t\t:",h0,vp))
      }
      tombol=tkbutton(jendelaoutput,text="Kembali",command=function()tkdestroy(jendelaoutput),bg = getOption("Rgui.theme_bl")$background)
      tkgrid(tombol)
      tkgrid(tklabel(jendelaoutput,text=" ",bg = getOption("Rgui.theme_bl")$primary))
    }
    
    ujisisikanan = function()
    {
      h0 = "Variansi populasi ="
      h1 = "Variansi populasi >"
      dk = "\n2. Daerah Kritis: Tolak H0 jika Pvalue < Alpha"
      chisq = round(varTest(x,alternative="greater",conf.level=(1-alpha),sigma.squared=vp)$statistic,4)
      pvalue = round(varTest(x,alternative="greater",conf.level=(1-alpha),sigma.squared=vp)$p.value,4)
      
      if(pvalue<alpha)
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=15,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,vp,"
     H1 :",h1,vp,"",dk,"
     
3. Statistik Uji   :
     Rata-rata populasi \t\t: ",round(sum(x, na.rm=TRUE)/n,4),"
     Standar Deviasi   \t\t: ",round(sd(x, na.rm=TRUE),4),"
     Chi-Square hitung \t\t=",chisq,"
     Alpha \t\t=",alpha,"
     P-Value \t\t=",pvalue,"
     
4. Keputusan \t\t: Tolak H0
5. Kesimpulan \t\t:",h1,vp))
      }
      else
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=15,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,vp,"
     H1 :",h1,vp,"",dk,"
     
3. Statistik Uji   :
     Rata-rata Populasi \t\t: ",round(sum(x, na.rm=TRUE)/n,4),"
     Standar Deviasi    \t\t: ",round(sd(x, na.rm=TRUE),4),"
     Chi-Square hitung \t=",chisq,"
     Alpha \t\t=",alpha,"
     P-Value \t\t=",pvalue,"
     
4. Keputusan \t\t: Terima H0
5. Kesimpulan \t\t:",h0,vp))
      }
      tombol=tkbutton(jendelaoutput,text="Kembali",command=function()tkdestroy(jendelaoutput),bg = getOption("Rgui.theme_bl")$background)
      tkgrid(tombol)
      tkgrid(tklabel(jendelaoutput,text=" ",bg = getOption("Rgui.theme_bl")$primary))
    }
    
    tombollihat = tkbutton(jendelaimport1,text="Lihat Data",command = lihatdata,bg = getOption("Rgui.theme_bl")$secondary)
    tkgrid(tombollihat)
    tkgrid(tklabel(jendelaimport1,text=" ",bg = getOption("Rgui.theme_bl")$background))
    
    bkiri =tkbutton(jendelaimport1,text="Sisi Kiri",command = ujisisikiri,bg = getOption("Rgui.theme_bl")$secondary)
    b2sisi=tkbutton(jendelaimport1,text="2 Sisi",command = ujiduasisi,bg = getOption("Rgui.theme_bl")$secondary)
    bkanan=tkbutton(jendelaimport1,text="Sisi Kanan",command = ujisisikanan,bg = getOption("Rgui.theme_bl")$secondary)
    tkgrid(bkiri,sticky="news")
    tkgrid(b2sisi,sticky="news")
    tkgrid(bkanan,sticky="news")
    
  }
  
}
tkadd(sub2_m1.1, "command", label="SPSS (.sav)", command=function()import1pop())

import1popex = function()
{
  jendelaimport1 = tktoplevel(bg = getOption("Rgui.theme_bl")$background)
  tktitle(jendelaimport1) = "Import Data"
  tkgrid(tklabel(jendelaimport1, text="Import Data Excel Pengujian Hipotesis Variansi Satu Populasi", font = teks1,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(tklabel(jendelaimport1,text=" ",bg = getOption("Rgui.theme_bl")$background))
  tkgrid(tklabel(jendelaimport1,text="Pastikan data format Anda berupa .xlsx",font=teks3,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(tklabel(jendelaimport1,text=" ",bg = getOption("Rgui.theme_bl")$background))
  
  alphaa = tclVar(" ")
  eb1 = tkentry(jendelaimport1,width="20",textvariable=alphaa,bg = getOption("Rgui.theme_bl")$primary)
  tkgrid(tklabel(jendelaimport1,text="Alpha (dalam %) :",font=teks2,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(eb1)
  
  varpop = tclVar(" ")
  eb2 = tkentry(jendelaimport1,width="20",textvariable=varpop,bg = getOption("Rgui.theme_bl")$primary)
  tkgrid(tklabel(jendelaimport1,text="Variansi Populasi :",font=teks2,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(eb2)
  
  tkgrid(tklabel(jendelaimport1,text=" ",bg = getOption("Rgui.theme_bl")$background))
  import=tkbutton(jendelaimport1,text="Import",command = function()import1(),bg = getOption("Rgui.theme_bl")$secondary)
  tkgrid(import)
  tkgrid(tklabel(jendelaimport1,text=" ",bg = getOption("Rgui.theme_bl")$background))
  
  import1 = function()
  {
    library(tcltk)
    library(readxl)
    data = read_excel(choose.files(),col_names = TRUE)
    alpha = as.numeric(tclvalue(alphaa))/100
    vp = as.numeric(tclvalue(varpop))
    m = as.matrix(data)
    x = as.numeric(m[,1])
    n = length(x[!is.na(x)])
    
    
    lihatdata = function()
    {
      View(data,"DATA")
    }
    
    ujisisikiri = function()
    {
      h0 = "Variansi populasi ="
      h1 = "Variansi populasi <"
      dk = "\n2. Daerah Kritis: Tolak H0 jika Pvalue < Alpha "
      chisq = round(varTest(x,alternative="less",conf.level=(1-alpha),sigma.squared=vp)$statistic,4)
      pvalue = round(varTest(x,alternative="less",conf.level=(1-alpha),sigma.squared=vp)$p.value,4)
      
      if(pvalue<alpha)
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=15,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,vp,"
     H1 :",h1,vp,"",dk,"
     
3. Statistik Uji   :
     Rata-rata Populasi \t\t: ",round(sum(x, na.rm=TRUE)/n,4),"
     Standar Deviasi    \t\t: ",round(sd(x, na.rm=TRUE),4),"
     Chi-Square hitung \t=",chisq,"
     Alpha  \t\t=",alpha,"
     P-Value  \t\t=",pvalue,"
     
4. Keputusan \t\t: Tolak H0
5. Kesimpulan \t\t:",h1,vp))
      }
      else
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=15,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,vp,"
     H1 :",h1,vp,"",dk,"
     
3. Statistik Uji   :
     Rata-rata Populasi \t\t: ",round(sum(x, na.rm=TRUE)/n,4),"
     Standar Deviasi    \t\t: ",round(sd(x, na.rm=TRUE),4),"
     Chi-Square hitung \t=",chisq,"
     Alpha  \t\t=",alpha,"
     P-Value  \t\t=",pvalue,"
     
4. Keputusan \t\t: Terima H0
5. Kesimpulan \t\t:",h0,vp))
      }
      tombol=tkbutton(jendelaoutput,text="Kembali",command=function()tkdestroy(jendelaoutput),bg = getOption("Rgui.theme_bl")$background)
      tkgrid(tombol)
      tkgrid(tklabel(jendelaoutput,text=" ",bg = getOption("Rgui.theme_bl")$primary))
    }
    
    ujiduasisi = function()
    {
      h0 = "Variansi populasi ="
      h1 = "Variansi populasi ≠"
      dk = "\n2. Daerah Kritis: Tolak H0 jika Pvalue < Alpha"
      chisq = round(varTest(x,alternative="two.sided",conf.level=(1-alpha),sigma.squared=vp)$statistic,4)
      pvalue = round(varTest(x,alternative="two.sided",conf.level=(1-alpha),sigma.squared=vp)$p.value,4)
      
      if(pvalue<(alpha/2))
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=85,height=15,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,vp,"
     H1 :",h1,vp,"",dk,"
     
3. Statistik Uji   :
     Rata-rata Populasi \t\t: ",round(sum(x, na.rm=TRUE)/n,4),"
     standar Deviasi    \t\t: ",round(sd(x, na.rm=TRUE),4),"
     Chi-Square hitung \t=",chisq,"
     Alpha \t\t=",alpha,"
     P-Value \t\t=",pvalue,"
     
4. Keputusan \t\t: Tolak H0
5. Kesimpulan \t\t:",h1,vp))
      }
      else if(pvalue<(alpha/2))
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=85,height=15,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,vp,"
     H1 :",h1,vp,"",dk,"
     
3. Statistik Uji   :
     Rata-rata Populasi \t\t: ",round(sum(x, na.rm=TRUE)/n,4),"
     Standar Deviasi    \t\t: ",round(sd(x, na.rm=TRUE),4),"
     Chi-Square hitung \t=",chisq,"
     Alpha \t\t=",alpha,"
     P-Value \t\t=",pvalue,"
     
4. Keputusan \t\t: Tolak H0
5. Kesimpulan \t\t:",h1,vp))
      }
      else
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=85,height=15,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,vp,"
     H1 :",h1,vp,"",dk,"
     
3. Statistik Uji   :
     Rata-rata Populasi \t\t: ",round(sum(x, na.rm=TRUE)/n,4),"
     Standar Deviasi    \t\t: ",round(sd(x, na.rm=TRUE),4),"
     Chi-Square hitung \t=",chisq,"
     Alpha \t\t=",alpha,"
     P-Value \t\t=",pvalue,"
     
4. Keputusan \t\t: Terima H0
5. Kesimpulan \t\t:",h0,vp))
      }
      tombol=tkbutton(jendelaoutput,text="Kembali",command=function()tkdestroy(jendelaoutput),bg = getOption("Rgui.theme_bl")$background)
      tkgrid(tombol)
      tkgrid(tklabel(jendelaoutput,text=" ",bg = getOption("Rgui.theme_bl")$primary))
    }
    
    ujisisikanan = function()
    {
      h0 = "Variansi populasi ="
      h1 = "Variansi populasi >"
      dk = "\n2. Daerah Kritis: Tolak H0 jika Pvalue < Alpha"
      chisq = round(varTest(x,alternative="greater",conf.level=(1-alpha),sigma.squared=vp)$statistic,4)
      pvalue = round(varTest(x,alternative="greater",conf.level=(1-alpha),sigma.squared=vp)$p.value,4)
      
      if(pvalue<alpha)
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=15,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,vp,"
     H1 :",h1,vp,"",dk,"
     
3. Statistik Uji   :
     Rata-rata populasi \t\t: ",round(sum(x, na.rm=TRUE)/n,4),"
     Standar Deviasi   \t\t: ",round(sd(x, na.rm=TRUE),4),"
     Chi-Square hitung \t\t=",chisq,"
     Alpha \t\t=",alpha,"
     P-Value \t\t=",pvalue,"
     
4. Keputusan \t\t: Tolak H0
5. Kesimpulan \t\t:",h1,vp))
      }
      else
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=15,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,vp,"
     H1 :",h1,vp,"",dk,"
     
3. Statistik Uji   :
     Rata-rata Populasi \t\t: ",round(sum(x, na.rm=TRUE)/n,4),"
     Standar Deviasi    \t\t: ",round(sd(x, na.rm=TRUE),4),"
     Chi-Square hitung \t=",chisq,"
     Alpha \t\t=",alpha,"
     P-Value \t\t=",pvalue,"
     
4. Keputusan \t\t: Terima H0
5. Kesimpulan \t\t:",h0,vp))
      }
      tombol=tkbutton(jendelaoutput,text="Kembali",command=function()tkdestroy(jendelaoutput),bg = getOption("Rgui.theme_bl")$background)
      tkgrid(tombol)
      tkgrid(tklabel(jendelaoutput,text=" ",bg = getOption("Rgui.theme_bl")$primary))
    }
    
    tombollihat = tkbutton(jendelaimport1,text="Lihat Data",command = lihatdata,bg = getOption("Rgui.theme_bl")$secondary)
    tkgrid(tombollihat)
    tkgrid(tklabel(jendelaimport1,text=" ",bg = getOption("Rgui.theme_bl")$background))
    
    bkiri =tkbutton(jendelaimport1,text="Sisi Kiri",command = ujisisikiri,bg = getOption("Rgui.theme_bl")$secondary)
    b2sisi=tkbutton(jendelaimport1,text="2 Sisi",command = ujiduasisi,bg = getOption("Rgui.theme_bl")$secondary)
    bkanan=tkbutton(jendelaimport1,text="Sisi Kanan",command = ujisisikanan,bg = getOption("Rgui.theme_bl")$secondary)
    tkgrid(bkiri,sticky="news")
    tkgrid(b2sisi,sticky="news")
    tkgrid(bkanan,sticky="news")
    
  }
  
}
tkadd(sub2_m1.1, "command", label="Excel (.xlsx)", command=function()import1popex())

#=============================================================================
# 4. Pengujian Hipotesis Var 2-population
#=============================================================================

#--------------------DUA POPULASI---------------------------#
tkadd(topmenu1, "cascade", label="Dua Populasi", menu=sub_m1.2)
#-----SUMMARY-----#
sumvar2 = function()
{
  require(tcltk)
  require(tcltk2)
  
  w3 = tktoplevel(bg = getOption("Rgui.theme_bl")$background)
  tktitle(w3) = "Summary Data"
  tkgrid(tklabel(w3,text="Summary Data Pengujian Hipotesis Variansi Dua Populasi",font=teks1,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  
  tkgrid(tklabel(w3,text="Sampel 1",font=teks3,bg = getOption("Rgui.theme_bl")$background))
  banyak1 = tclVar("  ")
  eb1 = tkentry(w3,width="20",textvariable = banyak1,bg = getOption("Rgui.theme_bl")$primary)
  tkgrid(tklabel(w3,text="Banyak sampel 1 :",font=teks2,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(eb1)
  
  varsam1 = tclVar(" ")
  eb2 = tkentry(w3,width="20",textvariable = varsam1,bg = getOption("Rgui.theme_bl")$primary)
  tkgrid(tklabel(w3,text="Variansi sampel 1 (S1^2) :",font=teks2,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(eb2)
  
  tkgrid(tklabel(w3,text="",bg = getOption("Rgui.theme_bl")$background))
  tkgrid(tklabel(w3,text="Sampel 2",font=teks3,bg = getOption("Rgui.theme_bl")$background))
  
  banyak2 = tclVar("  ")
  eb4 = tkentry(w3,width="20",textvariable = banyak2,bg = getOption("Rgui.theme_bl")$primary)
  tkgrid(tklabel(w3,text="Banyak sampel 2 :",font=teks2,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(eb4)
  
  varsam2 = tclVar(" ")
  eb5 = tkentry(w3,width="20",textvariable = varsam2,bg = getOption("Rgui.theme_bl")$primary)
  tkgrid(tklabel(w3,text="Variansi sampel 2 (S2^2) :",font=teks2,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(eb5)
  
  tkgrid(tklabel(w3,text="",bg = getOption("Rgui.theme_bl")$background))
  alphaa = tclVar(" ")
  eb7=tkentry(w3,width="20",textvariable=alphaa,bg = getOption("Rgui.theme_bl")$primary)
  tkgrid(tklabel(w3,text="Alpha (dalam %) :",font=teks2,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(eb7)
  
  outputsumvar1 = function()
  {
    h0 = "Variansi populasi 1 = Variansi populasi 2"
    h1 = "Variansi populasi 1 < Variansi populasi 2"
    dk = "\n2. Daerah Kritis : Tolak H0 jika F hitung < F tabel"
    n1 = as.numeric(tclvalue(banyak1))
    n2 = as.numeric(tclvalue(banyak2))
    vs1 = as.numeric(tclvalue(varsam1))
    vs2 = as.numeric(tclvalue(varsam2))
    alpha = as.numeric(tclvalue(alphaa))/100
    fh = round(vs1/vs2,4)
    ftb = round(qf(1-alpha,n1-1,n2-1,lower.tail = FALSE),4)
    
    if(fh<ftb)
    {
      w31 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
      tktitle(w31) = "Hasil Analisis Data"
      insert = tktext(w31,font=teks2,width=82,height=12,bg = getOption("Rgui.theme_bl")$primary)
      tkgrid(tklabel(w31,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
      tkgrid(insert)
      tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,"
     H1 :",h1,"",dk,"
3. Statistik Uji   :
     F-hitung\t\t=",fh,"
     F-tabel\t\t=",ftb,"
     Keputusan\t\t: Tolak H0
     Kesimpulan\t\t:",h1))
    }
    
    else
    {
      w31 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
      tktitle(w31) = "Hasil Analisis Data"
      insert = tktext(w31,font=teks2,width=82,height=12,bg = getOption("Rgui.theme_bl")$primary)
      tkgrid(tklabel(w31,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
      tkgrid(insert)
      tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,"
     H1 :",h1,"",dk,"
3. Statistik Uji   :
     F-hitung\t\t=",fh,"
     F-tabel\t\t=",ftb,"
     Keputusan\t\t: Terima H0
     Kesimpulan\t\t:",h0))
    }
    tombol=tkbutton(w31,text="Kembali",command=function()tkdestroy(w31),bg = getOption("Rgui.theme_bl")$background)
    tkgrid(tombol)
    tkgrid(tklabel(w31,text=" ",bg = getOption("Rgui.theme_bl")$primary))
  }
  
  outputsumvar2 = function()
  {
    h0 = "Variansi populasi 1 = Variansi populasi 2"
    h1 = "Variansi populasi 1 ≠ Variansi populasi 2"
    dk = "\n2. Daerah Kritis : Tolak H0 jika F hitung < F tabel 1 atau tolak H0 jika F hitung > F tabel 2"
    n1 = as.numeric(tclvalue(banyak1))
    n2 = as.numeric(tclvalue(banyak2))
    vs1 = as.numeric(tclvalue(varsam1))
    vs2 = as.numeric(tclvalue(varsam2))
    alpha = as.numeric(tclvalue(alphaa))/100
    fh = round(vs1/vs2,4)
    ft2 = round(qf(1-(alpha/2),n1-1,n2-1),4)
    ft1 = round(qf(alpha/2,n1-1,n2-1),4)
    
    if(fh<ft1)
    {
      w31 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
      tktitle(w31) = "Hasil Analisis Data"
      insert = tktext(w31,font=teks2,width=82,height=12,bg = getOption("Rgui.theme_bl")$primary)
      tkgrid(tklabel(w31,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
      tkgrid(insert)
      tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,"
     H1 :",h1,"",dk,"
3. Statistik Uji   :
     F-hitung\t\t=",fh,"
     F-tabel 1\t\t=",ft1,"
     F-tabel 2\t\t=",ft2,"
     Keputusan\t\t: Tolak H0
     Kesimpulan\t\t:",h1))
    }
    else if(fh>ft2)
    {
      w31 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
      tktitle(w31) = "Hasil Analisis Data"
      insert = tktext(w31,font=teks2,width=82,height=12,bg = getOption("Rgui.theme_bl")$primary)
      tkgrid(tklabel(w31,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
      tkgrid(insert)
      tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,"
     H1 :",h1,"",dk,"
3. Statistik Uji   :
     F-hitung\t\t=",fh,"
     F-tabel 1\t\t=",ft1,"
     F-tabel 2\t\t=",ft2,"
     Keputusan\t\t: Tolak H0
     Kesimpulan\t\t:",h1))
    }
    else
    {
      w31 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
      tktitle(w31) = "Hasil Analisis Data"
      insert = tktext(w31,font=teks2,width=82,height=12,bg = getOption("Rgui.theme_bl")$primary)
      tkgrid(tklabel(w31,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
      tkgrid(insert)
      tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,"
     H1 :",h1,"",dk,"
3. Statistik Uji   :
     F-hitung\t\t=",fh,"
     F-tabel 1\t\t=",ft1,"
     F-tabel 2\t\t=",ft2,"
     Keputusan\t\t: Terima H0
     Kesimpulan\t\t:",h0))
    }
    tombol=tkbutton(w31,text="Kembali",command=function()tkdestroy(w31),bg = getOption("Rgui.theme_bl")$background)
    tkgrid(tombol)
    tkgrid(tklabel(w31,text=" ",bg = getOption("Rgui.theme_bl")$primary))
  }
  
  outputsumvar3 = function()
  {
    h0 = "Variansi populasi 1 = Variansi populasi 2"
    h1 = "Variansi populasi 1 > Variansi populasi 2"
    dk = "\n2. Daerah Kritis : Tolak H0 jika F hitung > F tabel"
    n1 = as.numeric(tclvalue(banyak1))
    n2 = as.numeric(tclvalue(banyak2))
    vs1 = as.numeric(tclvalue(varsam1))
    vs2 = as.numeric(tclvalue(varsam2))
    alpha = as.numeric(tclvalue(alphaa))/100
    fh = round(vs1/vs2,4)
    ft = round(qf(alpha,n1-1,n2-1,lower.tail=FALSE),4)
    
    if(fh>ft)
    {
      w31 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
      tktitle(w31) = "Hasil Analisis Data"
      insert = tktext(w31,font=teks2,width=82,height=12,bg = getOption("Rgui.theme_bl")$primary)
      tkgrid(tklabel(w31,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
      tkgrid(insert)
      tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,"
     H1 :",h1,"",dk,"
3. Statistik Uji   :
     F-hitung\t\t=",fh,"
     F-tabel\t\t=",ft,"
     Keputusan\t\t: Tolak H0
     Kesimpulan\t\t:",h1))
    }
    
    else
    {
      w31 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
      tktitle(w31) = "Hasil Analisis Data"
      insert = tktext(w31,font=teks2,width=82,height=12,bg = getOption("Rgui.theme_bl")$primary)
      tkgrid(tklabel(w31,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
      tkgrid(insert)
      tkinsert(insert,"end",paste("1. Hipotesis
     H0 :",h0,"
     H1 :",h1,"",dk,"
3. Statistik Uji   :
     F-hitung\t\t=",fh,"
     F-tabel\t\t=",ft,"
     Keputusan\t\t: Terima H0
     Kesimpulan\t\t:",h0))
    }
    tombol=tkbutton(w31,text="Kembali",command=function()tkdestroy(w31),bg = getOption("Rgui.theme_bl")$background)
    tkgrid(tombol)
    tkgrid(tklabel(w31,text=" ",bg = getOption("Rgui.theme_bl")$primary))
  }
  
  #pilihan arah uji hipotesis satu populasi
  tkgrid(tklabel(w3,text="",bg = getOption("Rgui.theme_bl")$background))
  tkgrid(tklabel(w3,text="Pilih uji hipotesis",font=teks3,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  kiri =tkbutton(w3,text="Sisi Kiri",command = function()outputsumvar1(),bg = getOption("Rgui.theme_bl")$secondary)
  tkgrid(kiri,sticky="news")
  sisi2=tkbutton(w3,text="2 Sisi",command=function()outputsumvar2(),bg = getOption("Rgui.theme_bl")$secondary)
  tkgrid(sisi2,sticky="news")
  kanan=tkbutton(w3,text="Sisi Kanan",command=function()outputsumvar3(),bg = getOption("Rgui.theme_bl")$secondary)
  tkgrid(kanan,sticky="news")
}
tkadd(sub_m1.2, "command", label="Summary", command=function()sumvar2())

#------INPUT MANUAL----#
manualvar2 = function()
{
  w4 = tktoplevel(bg = getOption("Rgui.theme_bl")$background)
  tktitle(w4) = "Input Manual"
  tkgrid(tklabel(w4,text="Input Data Pengujian Hipotesis Variansi Dua Populasi",font=teks1,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  
  banyak1 = tclVar("  ")
  eb1 = tkentry(w4,width="20",textvariable = banyak1,bg = getOption("Rgui.theme_bl")$primary)
  tkgrid(tklabel(w4,text="Banyak sampel 1 :",font=teks2,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(eb1)
  
  banyak2 = tclVar("  ")
  eb2 = tkentry(w4,width="20",textvariable = banyak2,bg = getOption("Rgui.theme_bl")$primary)
  tkgrid(tklabel(w4,text="Banyak sampel 2 :",font=teks2,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(eb2)
  
  alfa = tclVar(" ")
  eb3=tkentry(w4,width="20",textvariable=alfa,bg = getOption("Rgui.theme_bl")$primary)
  tkgrid(tklabel(w4,text="Alpha (dalam %) :",font=teks2,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(eb3)
  tkgrid(tklabel(w4,text=" ",bg = getOption("Rgui.theme_bl")$background))
  
  input=tkbutton(w4,text="Input",command = function()inputvarman(),bg = getOption("Rgui.theme_bl")$secondary)
  tkgrid(input)
  tkgrid(tklabel(w4,text=" ",bg = getOption("Rgui.theme_bl")$background))
  
  inputvarman = function()
  {
    n1 = as.numeric(tclvalue(banyak1))
    n2 = as.numeric(tclvalue(banyak2))
    w41 = tktoplevel(bg = getOption("Rgui.theme_bl")$secondary)
    tktitle(w41) = "Input data"
    tkgrid(tklabel(w41,text="Masukkan data",font=teks1,bg = getOption("Rgui.theme_bl")$secondary),sticky="n")
    tkgrid(tklabel(w41,text="",bg = getOption("Rgui.theme_bl")$secondary))
    
    #Layout tabel
    tclRequire("Tktable")
    x = matrix(0,1,n1)
    layoutinput1 = tclArray()
    for(i in 1:n1){
      layoutinput1[[i,0]]=i
    }
    layoutinput1[[0,1]]="n1"
    
    y = matrix(0,1,n2)
    layoutinput2 = tclArray()
    for(i in 1:n2){
      layoutinput2[[i,0]]=i
    }
    layoutinput2[[0,1]]="n2"
    
    #Tabelnya
    tabelp1=tkwidget(w41,"table",variable=layoutinput1,rows=n1+1,cols=2,titlerows=1,titlecols=1,selectmode="extended  ",colwidth=8,rowheight=2,bg = getOption("Rgui.theme_bl")$primary)
    tkconfigure(tabelp1,selectmode="extended",rowseparator="\n",colseparator="\t")
    tabelp2=tkwidget(w41,"table",variable=layoutinput2,rows=n2+1,cols=2,titlerows=1,titlecols=1,selectmode="extended  ",colwidth=8,rowheight=2,bg = getOption("Rgui.theme_bl")$primary)
    tkconfigure(tabelp2,selectmode="extended",rowseparator="\n",colseparator="\t")
    tkgrid(tklabel(w41,text="Sampel 1:",font=teks3,bg = getOption("Rgui.theme_bl")$secondary),tabelp1,tklabel(w41,text="Sampel 2:",font=teks3,bg = getOption("Rgui.theme_bl")$secondary),tabelp2,sticky="n")
    
    outputmanualv1 = function()
    {
      h0 = "Variansi populasi 1 = Variansi populasi 2"
      h1 = "Variansi populasi 1 < Variansi populasi 2"
      dk = "\n2. Daerah Kritis : Tolak H0 jika P-Value < alpha"
      alpha = as.numeric(tclvalue(alfa))/100
      
      for(i in 0:n1){
        x[1,i]=as.numeric(layoutinput1[[i,1]])
      }
      for(i in 0:n2){
        y[1,i]=as.numeric(layoutinput2[[i,1]])
      }
      
      Fhit=round(var.test(x,y,ratio=1,alternative="less",conf.level=(1-alpha))$statistic,4)
      pvalue=round(var.test(x,y,ratio=1,alternative="less",conf.level=(1-alpha))$p.value,4)
      ft = round(qf(alpha,n1-1,n2-1),4)
      
      if(pvalue<alpha)
      {
        w411 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(w411) = "Hasil Analisis Data"
        insert = tktext(w411,font=teks2,width=82,height=12,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(w411,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     ",h0,"
     ",h1,"",dk,"
3. Statistik Uji   :
     F-hitung\t\t=",Fhit,"
     alpha\t\t=",alpha,"
     F-tabel\t\t=",ft,"
     P-Value\t\t=",pvalue,"
     Keputusan\t\t: Tolak H0
     Kesimpulan\t\t:",h1))
      }
      
      else
      {
        w411 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(w411) = "Hasil Analisis Data"
        insert = tktext(w411,font=teks2,width=82,height=12,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(w411,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     ",h0,"
     ",h1,"",dk,"
3. Statistik Uji   :
     F-hitung\t\t=",Fhit,"
     alpha\t\t=",alpha,"
     F-tabel\t\t=",ft,"
     P-Value\t\t=",pvalue,"
     Keputusan\t\t: Terima H0
     Kesimpulan\t\t:",h0))
      }
      tombol=tkbutton(w411,text="Kembali",command=function()tkdestroy(w411),bg = getOption("Rgui.theme_bl")$background)
      tkgrid(tombol)
      tkgrid(tklabel(w411,text=" ",bg = getOption("Rgui.theme_bl")$primary))
    }
    
    outputmanualv2 = function()
    {
      h0 = "Variansi populasi 1 = Variansi populasi 2"
      h1 = "Variansi populasi 1 ≠ Variansi populasi 2"
      dk = "\n2. Daerah Kritis : Tolak H0 jika P-Value < alpha"
      alpha = as.numeric(tclvalue(alfa))/100
      
      for(i in 0:n1){
        x[1,i]=as.numeric(layoutinput1[[i,1]])
      }
      for(i in 0:n2){
        y[1,i]=as.numeric(layoutinput2[[i,1]])
      }
      
      Fhit=round(var.test(x,y,ratio=1,alternative="two.sided",conf.level=1-alpha)$statistic,4)
      pvalue=round(var.test(x,y,ratio=1,alternative="two.sided",conf.level=1-alpha)$p.value,4)
      ft2 = round(qf(1-(alpha/2),n1-1,n2-1),4)
      ft1 = round(qf(alpha/2,n1-1,n2-1),4)
      
      
      if(pvalue<(alpha/2))
      {
        w411 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(w411) = "Hasil Analisis Data"
        insert = tktext(w411,font=teks2,width=82,height=12,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(w411,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     ",h0,"
     ",h1,"",dk,"
3. Statistik Uji   :
     F-hitung\t\t=",Fhit,"
     alpha\t\t=",alpha,"
     F-tabel 1\t\t=",ft1,"
     F-tabel 2\t\t=",ft2,"
     P-Value\t\t=",pvalue,"
     Keputusan\t\t: Tolak H0
     Kesimpulan\t\t:",h1))
      }
      
      else
      {
        w411 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(w411) = "Hasil Analisis Data"
        insert = tktext(w411,font=teks2,width=82,height=12,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(w411,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     ",h0,"
     ",h1,"",dk,"
3. Statistik Uji   :
     F-hitung\t\t=",Fhit,"
     alpha\t\t=",alpha,"
     F-tabel 1\t\t=",ft1,"
     F-tabel 2\t\t=",ft2,"
     P-Value\t\t=",pvalue,"
     Keputusan\t\t: Terima H0
     Kesimpulan\t\t:",h0))
      }
      tombol=tkbutton(w411,text="Kembali",command=function()tkdestroy(w411),bg = getOption("Rgui.theme_bl")$background)
      tkgrid(tombol)
      tkgrid(tklabel(w411,text=" ",bg = getOption("Rgui.theme_bl")$primary))
    }
    
    outputmanualv3 = function()
    {
      h0 = "Variansi populasi 1 = Variansi populasi 2"
      h1 = "Variansi populasi 1 > Variansi populasi 2"
      dk = "\n2. Daerah Kritis : Tolak H0 jika P-Value < alpha"
      alpha = as.numeric(tclvalue(alfa))/100
      
      for(i in 0:n1){
        x[1,i]=as.numeric(layoutinput1[[i,1]])
      }
      for(i in 0:n2){
        y[1,i]=as.numeric(layoutinput2[[i,1]])
      }
      
      Fhit=round(var.test(x,y,ratio=1,alternative="greater",conf.level=1-alpha)$statistic,4)
      pvalue=round(var.test(x,y,ratio=1,alternative="greater",conf.level=1-alpha)$p.value,4)
      ft = round(qf(1-alpha,n1-1,n2-1),4)
      
      if(pvalue<alpha)
      {
        w411 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(w411) = "Hasil Analisis Data"
        insert = tktext(w411,font=teks2,width=82,height=12,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(w411,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     ",h0,"
     ",h1,"",dk,"
3. Statistik Uji   :
     F-hitung\t\t=",Fhit,"
     alpha\t\t=",alpha,"
     F-tabel\t\t=",ft,"
     P-Value\t\t=",pvalue,"
     Keputusan\t\t: Tolak H0
     Kesimpulan\t\t:",h1))
      }
      
      else
      {
        w411 = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(w411) = "Hasil Analisis Data"
        insert = tktext(w411,font=teks2,width=82,height=12,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(w411,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     ",h0,"
     ",h1,"",dk,"
3. Statistik Uji   :
     F-hitung\t\t=",Fhit,"
     alpha\t\t=",alpha,"
     F-tabel\t\t=",ft,"
     P-Value\t\t=",pvalue,"
     Keputusan\t\t: Terima H0
     Kesimpulan\t\t:",h0))
      }
      tombol=tkbutton(w411,text="Kembali",command=function()tkdestroy(w411),bg = getOption("Rgui.theme_bl")$background)
      tkgrid(tombol)
      tkgrid(tklabel(w411,text=" ",bg = getOption("Rgui.theme_bl")$primary))
    }
    tkgrid(tklabel(w41,text="",bg = getOption("Rgui.theme_bl")$secondary))
    tombolkiri =tkbutton(w41,text="Sisi Kiri",command = function()outputmanualv1(),bg = getOption("Rgui.theme_bl")$secondary)
    tkgrid(tombolkiri,sticky="news")
    tombolkanan=tkbutton(w41,text="2 Sisi",command=function()outputmanualv2(),bg = getOption("Rgui.theme_bl")$secondary)
    tkgrid(tombolkanan,sticky="news")
    tombolkanan=tkbutton(w41,text="Sisi Kanan",command=function()outputmanualv3(),bg = getOption("Rgui.theme_bl")$secondary)
    tkgrid(tombolkanan,sticky="news")
    
  }
}
tkadd(sub_m1.2, "command", label="Input Data", command=function()manualvar2())

#IMPORT DATA
tkadd(sub_m1.2, "cascade", label="Import Data", menu=sub2_m1.2)

import2pop = function()
{
  jendelaimport2 = tktoplevel(bg = getOption("Rgui.theme_bl")$background)
  tktitle(jendelaimport2) = "Import Data"
  tkgrid(tklabel(jendelaimport2, text="Import Data SPSS Pengujian Hipotesis Variansi Dua Populasi", font = teks1,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(tklabel(jendelaimport2,text="Pastikan data anda berupa SPSS",font=teks3,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(tklabel(jendelaimport2,text=" ",bg = getOption("Rgui.theme_bl")$background))
  
  alphaa = tclVar(" ")
  eb4 = tkentry(jendelaimport2,width="20",textvariable=alphaa,bg = getOption("Rgui.theme_bl")$primary)
  tkgrid(tklabel(jendelaimport2,text="Alpha (dalam %) :",font=teks2,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(eb4)
  tkgrid(tklabel(jendelaimport2,text=" ",bg = getOption("Rgui.theme_bl")$background))
  tkgrid(tkbutton(jendelaimport2,text="Import",command = function()import2()))
  tkgrid(tklabel(jendelaimport2,text=" ",bg = getOption("Rgui.theme_bl")$background))
  
  import2 = function()
  {
    library(readxl)
    data = read.spss(choose.files(),use.value.labels = TRUE, max.value.labels = Inf, to.data.frame = TRUE)
    alpha = as.numeric(tclvalue(alphaa))/100
    m = as.matrix(data)
    x1 = as.numeric(m[,1])
    x2 = as.numeric(m[,2])
    n1 = length(x1[!is.na(x1)])
    n2 = length(x2[!is.na(x2)])
    
    
    lihatdata = function()
    {
      View(data,"DATA")
    }
    
    ujisisikiri = function()
    {
      h0 = "Variansi populasi 1 = Variansi populasi 2"
      h1 = "Variansi populasi 1 < Variansi populasi 2"
      dk = "\n2. Daerah Kritis : Tolak H0 jika P-Value < alpha"
      Fhit=round(var.test(x1,x2,ratio=1,alternative=c("less"), conf.level = 1-alpha)$statistic,4)
      pvalue=round(var.test(x1,x2,ratio=1,alternative=c("less"), conf.level = 1-alpha)$p.value,4)
      
      if(pvalue<alpha)
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=17,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     ",h0,"
     ",h1,"",dk,"
3. Statistik Uji   :
   rata-rata populasi 1\t: ",round(sum(x1, na.rm=TRUE)/n1,4),"
   rata-rata populasi 2\t: ",round(sum(x2, na.rm=TRUE)/n2,4),"
   standar deviasi 1\t\t: ",round(sd(x1, na.rm=TRUE),4),"
   standar deviasi 2\t\t: ",round(sd(x2, na.rm=TRUE),4),"
   F-hitung\t\t=",Fhit,"
   alpha\t\t=",alpha,"
   P-Value\t\t=",pvalue,"
   Keputusan\t\t: Tolak H0
   Kesimpulan\t\t:",h1))
      }
      else
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=17,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     ",h0,"
     ",h1,"",dk,"
3. Statistik Uji   :
   rata-rata populasi 1\t: ",round(sum(x1, na.rm=TRUE)/n1,4),"
   rata-rata populasi 2\t: ",round(sum(x2, na.rm=TRUE)/n2,4),"
   standar deviasi 1\t\t: ",round(sd(x1, na.rm=TRUE),4),"
   standar deviasi 2\t\t: ",round(sd(x2, na.rm=TRUE),4),"
   F-hitung\t\t=",Fhit,"
   alpha\t\t=",alpha,"
   P-Value\t\t=",pvalue,"
   Keputusan\t\t: Terima H0
   Kesimpulan\t\t:",h0))
      }
      tombol=tkbutton(jendelaoutput,text="Kembali",command=function()tkdestroy(jendelaoutput),bg = getOption("Rgui.theme_bl")$background)
      tkgrid(tombol)
      tkgrid(tklabel(jendelaoutput,text=" ",bg = getOption("Rgui.theme_bl")$primary))
    }
    
    ujiduasisi = function()
    {
      h0 = "Variansi populasi 1 = Variansi populasi 2"
      h1 = "Variansi populasi 1 ≠ Variansi populasi 2"
      dk = "\n2. Daerah Kritis : Tolak H0 jika P-Value < alpha"
      Fhit=round(var.test(x1,x2,ratio=1,alternative=c("two.sided"), conf.level = 1-alpha)$statistic,4)
      pvalue=round(var.test(x1,x2,ratio=1,alternative=c("two.sided"), conf.level = 1-alpha)$p.value,4)
      
      if(pvalue<(alpha/2))
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=17,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     ",h0,"
     ",h1,"",dk,"
3. Statistik Uji   :
   rata-rata populasi 1\t: ",round(sum(x1, na.rm=TRUE)/n1,4),"
   rata-rata populasi 2\t: ",round(sum(x2, na.rm=TRUE)/n2,4),"
   standar deviasi 1\t\t: ",round(sd(x1, na.rm=TRUE),4),"
   standar deviasi 2\t\t: ",round(sd(x2, na.rm=TRUE),4),"
   F-hitung\t\t=",Fhit,"
   alpha\t\t=",alpha,"
   P-Value\t\t=",pvalue,"
   Keputusan\t\t: Tolak H0
   Kesimpulan\t\t:",h1))
      }
      else
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=17,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     ",h0,"
     ",h1,"",dk,"
3. Statistik Uji   :
   rata-rata populasi 1\t: ",round(sum(x1, na.rm=TRUE)/n1,4),"
   rata-rata populasi 2\t: ",round(sum(x2, na.rm=TRUE)/n2,4),"
   standar deviasi 1\t\t: ",round(sd(x1, na.rm=TRUE),4),"
   standar deviasi 2\t\t: ",round(sd(x2, na.rm=TRUE),4),"
   F-hitung\t\t=",Fhit,"
   alpha\t\t=",alpha,"
   P-Value\t\t=",pvalue,"
   Keputusan\t\t: Terima H0
   Kesimpulan\t\t:",h0))
      }
      tombol=tkbutton(jendelaoutput,text="Kembali",command=function()tkdestroy(jendelaoutput),bg = getOption("Rgui.theme_bl")$background)
      tkgrid(tombol)
      tkgrid(tklabel(jendelaoutput,text=" ",bg = getOption("Rgui.theme_bl")$primary))
    }
    
    ujisisikanan = function()
    {
      h0 = "Variansi populasi 1 = Variansi populasi 2"
      h1 = "Variansi populasi 1 ≠ Variansi populasi 2"
      dk = "\n2. Daerah Kritis : Tolak H0 jika P-Value < alpha"
      Fhit=round(var.test(x1,x2,ratio=1,alternative=c("greater"), conf.level = 1-alpha)$statistic,4)
      pvalue=round(var.test(x1,x2,ratio=1,alternative=c("greater"), conf.level = 1-alpha)$p.value,4)
      
      if(pvalue<alpha)
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=17,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     ",h0,"
     ",h1,"",dk,"
3. Statistik Uji   :
   rata-rata populasi 1\t: ",round(sum(x1, na.rm=TRUE)/n1,4),"
   rata-rata populasi 2\t: ",round(sum(x2, na.rm=TRUE)/n2,4),"
   standar deviasi 1\t\t: ",round(sd(x1, na.rm=TRUE),4),"
   standar deviasi 2\t\t: ",round(sd(x2, na.rm=TRUE),4),"
   F-hitung\t\t=",Fhit,"
   alpha\t\t=",alpha,"
   P-Value\t\t=",pvalue,"
   Keputusan\t\t: Tolak H0
   Kesimpulan\t\t:",h1))
      }
      else
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=17,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     ",h0,"
     ",h1,"",dk,"
3. Statistik Uji   :
   rata-rata populasi 1\t: ",round(sum(x1, na.rm=TRUE)/n1,4),"
   rata-rata populasi 2\t: ",round(sum(x2, na.rm=TRUE)/n2,4),"
   standar deviasi 1\t\t: ",round(sd(x1, na.rm=TRUE),4),"
   standar deviasi 2\t\t: ",round(sd(x2, na.rm=TRUE),4),"
   F-hitung\t\t=",Fhit,"
   alpha\t\t=",alpha,"
   P-Value\t\t=",pvalue,"
   Keputusan\t\t: Terima H0
   Kesimpulan\t\t:",h0))
      }
      tombol=tkbutton(jendelaoutput,text="Kembali",command=function()tkdestroy(jendelaoutput),bg = getOption("Rgui.theme_bl")$background)
      tkgrid(tombol)
      tkgrid(tklabel(jendelaoutput,text=" ",bg = getOption("Rgui.theme_bl")$primary))
    }
    
    tombollihat = tkbutton(jendelaimport2,text="Lihat Data",command = lihatdata,bg = getOption("Rgui.theme_bl")$secondary)
    tkgrid(tombollihat)
    tkgrid(tklabel(jendelaimport2,text=" ",bg = getOption("Rgui.theme_bl")$background))
    
    bkiri =tkbutton(jendelaimport2,text="Sisi Kiri",command = ujisisikiri,bg = getOption("Rgui.theme_bl")$secondary)
    b2sisi=tkbutton(jendelaimport2,text="2 Sisi",command = ujiduasisi,bg = getOption("Rgui.theme_bl")$secondary)
    bkanan=tkbutton(jendelaimport2,text="Sisi Kanan",command = ujisisikanan,bg = getOption("Rgui.theme_bl")$secondary)
    tkgrid(bkiri,sticky="news")
    tkgrid(b2sisi,sticky="news")
    tkgrid(bkanan,sticky="news")
  }
}
tkadd(sub2_m1.2, "command", label="SPSS (.sav)", command=function()import2pop())

import2popex = function()
{
  jendelaimport2 = tktoplevel(bg = getOption("Rgui.theme_bl")$background)
  tktitle(jendelaimport2) = "Import Data"
  tkgrid(tklabel(jendelaimport2, text="Import Data Excel Pengujian Hipotesis Variansi Dua Populasi", font = teks1,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(tklabel(jendelaimport2,text="Pastikan format data anda berupa .xlsx",font=teks3,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(tklabel(jendelaimport2,text=" ",bg = getOption("Rgui.theme_bl")$background))
  
  alphaa = tclVar(" ")
  eb4 = tkentry(jendelaimport2,width="20",textvariable=alphaa,bg = getOption("Rgui.theme_bl")$primary)
  tkgrid(tklabel(jendelaimport2,text="Alpha (dalam %) :",font=teks2,bg = getOption("Rgui.theme_bl")$background),sticky="news")
  tkgrid(eb4)
  tkgrid(tklabel(jendelaimport2,text=" ",bg = getOption("Rgui.theme_bl")$background))
  tkgrid(tkbutton(jendelaimport2,text="Import",command = function()import2()))
  tkgrid(tklabel(jendelaimport2,text=" ",bg = getOption("Rgui.theme_bl")$background))
  
  import2 = function()
  {
    library(tcltk)
    library(readxl)
    data = read_excel(choose.files(),col_names = TRUE)
    alpha = as.numeric(tclvalue(alphaa))/100
    m = as.matrix(data)
    x1 = as.numeric(m[,1])
    x2 = as.numeric(m[,2])
    n1 = length(x1[!is.na(x1)])
    n2 = length(x2[!is.na(x2)])
    
    
    lihatdata = function()
    {
      View(data,"DATA")
    }
    
    ujisisikiri = function()
    {
      h0 = "Variansi populasi 1 = Variansi populasi 2"
      h1 = "Variansi populasi 1 < Variansi populasi 2"
      dk = "\n2. Daerah Kritis : Tolak H0 jika P-Value < alpha"
      Fhit=round(var.test(x1,x2,ratio=1,alternative=c("less"), conf.level = 1-alpha)$statistic,4)
      pvalue=round(var.test(x1,x2,ratio=1,alternative=c("less"), conf.level = 1-alpha)$p.value,4)
      
      if(pvalue<alpha)
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=17,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     ",h0,"
     ",h1,"",dk,"
3. Statistik Uji   :
   rata-rata populasi 1\t: ",round(sum(x1, na.rm=TRUE)/n1,4),"
   rata-rata populasi 2\t: ",round(sum(x2, na.rm=TRUE)/n2,4),"
   standar deviasi 1\t\t: ",round(sd(x1, na.rm=TRUE),4),"
   standar deviasi 2\t\t: ",round(sd(x2, na.rm=TRUE),4),"
   F-hitung\t\t=",Fhit,"
   alpha\t\t=",alpha,"
   P-Value\t\t=",pvalue,"
   Keputusan\t\t: Tolak H0
   Kesimpulan\t\t:",h1))
      }
      else
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=17,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     ",h0,"
     ",h1,"",dk,"
3. Statistik Uji   :
   rata-rata populasi 1\t: ",round(sum(x1, na.rm=TRUE)/n1,4),"
   rata-rata populasi 2\t: ",round(sum(x2, na.rm=TRUE)/n2,4),"
   standar deviasi 1\t\t: ",round(sd(x1, na.rm=TRUE),4),"
   standar deviasi 2\t\t: ",round(sd(x2, na.rm=TRUE),4),"
   F-hitung\t\t=",Fhit,"
   alpha\t\t=",alpha,"
   P-Value\t\t=",pvalue,"
   Keputusan\t\t: Terima H0
   Kesimpulan\t\t:",h0))
      }
      tombol=tkbutton(jendelaoutput,text="Kembali",command=function()tkdestroy(jendelaoutput),bg = getOption("Rgui.theme_bl")$background)
      tkgrid(tombol)
      tkgrid(tklabel(jendelaoutput,text=" ",bg = getOption("Rgui.theme_bl")$primary))
    }
    
    ujiduasisi = function()
    {
      h0 = "Variansi populasi 1 = Variansi populasi 2"
      h1 = "Variansi populasi 1 ≠ Variansi populasi 2"
      dk = "\n2. Daerah Kritis : Tolak H0 jika P-Value < alpha"
      Fhit=round(var.test(x1,x2,ratio=1,alternative=c("two.sided"), conf.level = 1-alpha)$statistic,4)
      pvalue=round(var.test(x1,x2,ratio=1,alternative=c("two.sided"), conf.level = 1-alpha)$p.value,4)
      
      if(pvalue<(alpha/2))
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=17,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     ",h0,"
     ",h1,"",dk,"
3. Statistik Uji   :
   rata-rata populasi 1\t: ",round(sum(x1, na.rm=TRUE)/n1,4),"
   rata-rata populasi 2\t: ",round(sum(x2, na.rm=TRUE)/n2,4),"
   standar deviasi 1\t\t: ",round(sd(x1, na.rm=TRUE),4),"
   standar deviasi 2\t\t: ",round(sd(x2, na.rm=TRUE),4),"
   F-hitung\t\t=",Fhit,"
   alpha\t\t=",alpha,"
   P-Value\t\t=",pvalue,"
   Keputusan\t\t: Tolak H0
   Kesimpulan\t\t:",h1))
      }
      else
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=17,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     ",h0,"
     ",h1,"",dk,"
3. Statistik Uji   :
   rata-rata populasi 1\t: ",round(sum(x1, na.rm=TRUE)/n1,4),"
   rata-rata populasi 2\t: ",round(sum(x2, na.rm=TRUE)/n2,4),"
   standar deviasi 1\t\t: ",round(sd(x1, na.rm=TRUE),4),"
   standar deviasi 2\t\t: ",round(sd(x2, na.rm=TRUE),4),"
   F-hitung\t\t=",Fhit,"
   alpha\t\t=",alpha,"
   P-Value\t\t=",pvalue,"
   Keputusan\t\t: Terima H0
   Kesimpulan\t\t:",h0))
      }
      tombol=tkbutton(jendelaoutput,text="Kembali",command=function()tkdestroy(jendelaoutput),bg = getOption("Rgui.theme_bl")$background)
      tkgrid(tombol)
      tkgrid(tklabel(jendelaoutput,text=" ",bg = getOption("Rgui.theme_bl")$primary))
    }
    
    ujisisikanan = function()
    {
      h0 = "Variansi populasi 1 = Variansi populasi 2"
      h1 = "Variansi populasi 1 ≠ Variansi populasi 2"
      dk = "\n2. Daerah Kritis : Tolak H0 jika P-Value < alpha"
      Fhit=round(var.test(x1,x2,ratio=1,alternative=c("greater"), conf.level = 1-alpha)$statistic,4)
      pvalue=round(var.test(x1,x2,ratio=1,alternative=c("greater"), conf.level = 1-alpha)$p.value,4)
      
      if(pvalue<alpha)
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=17,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     ",h0,"
     ",h1,"",dk,"
3. Statistik Uji   :
   rata-rata populasi 1\t: ",round(sum(x1, na.rm=TRUE)/n1,4),"
   rata-rata populasi 2\t: ",round(sum(x2, na.rm=TRUE)/n2,4),"
   standar deviasi 1\t\t: ",round(sd(x1, na.rm=TRUE),4),"
   standar deviasi 2\t\t: ",round(sd(x2, na.rm=TRUE),4),"
   F-hitung\t\t=",Fhit,"
   alpha\t\t=",alpha,"
   P-Value\t\t=",pvalue,"
   Keputusan\t\t: Tolak H0
   Kesimpulan\t\t:",h1))
      }
      else
      {
        jendelaoutput = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=17,bg = getOption("Rgui.theme_bl")$primary)
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
        tkgrid(insert)
        tkinsert(insert,"end",paste("1. Hipotesis
     ",h0,"
     ",h1,"",dk,"
3. Statistik Uji   :
   rata-rata populasi 1\t: ",round(sum(x1, na.rm=TRUE)/n1,4),"
   rata-rata populasi 2\t: ",round(sum(x2, na.rm=TRUE)/n2,4),"
   standar deviasi 1\t\t: ",round(sd(x1, na.rm=TRUE),4),"
   standar deviasi 2\t\t: ",round(sd(x2, na.rm=TRUE),4),"
   F-hitung\t\t=",Fhit,"
   alpha\t\t=",alpha,"
   P-Value\t\t=",pvalue,"
   Keputusan\t\t: Terima H0
   Kesimpulan\t\t:",h0))
      }
      tombol=tkbutton(jendelaoutput,text="Kembali",command=function()tkdestroy(jendelaoutput),bg = getOption("Rgui.theme_bl")$background)
      tkgrid(tombol)
      tkgrid(tklabel(jendelaoutput,text=" ",bg = getOption("Rgui.theme_bl")$primary))
    }
    
    tombollihat = tkbutton(jendelaimport2,text="Lihat Data",command = lihatdata,bg = getOption("Rgui.theme_bl")$secondary)
    tkgrid(tombollihat)
    tkgrid(tklabel(jendelaimport2,text=" ",bg = getOption("Rgui.theme_bl")$background))
    
    bkiri =tkbutton(jendelaimport2,text="Sisi Kiri",command = ujisisikiri,bg = getOption("Rgui.theme_bl")$secondary)
    b2sisi=tkbutton(jendelaimport2,text="2 Sisi",command = ujiduasisi,bg = getOption("Rgui.theme_bl")$secondary)
    bkanan=tkbutton(jendelaimport2,text="Sisi Kanan",command = ujisisikanan,bg = getOption("Rgui.theme_bl")$secondary)
    tkgrid(bkiri,sticky="news")
    tkgrid(b2sisi,sticky="news")
    tkgrid(bkanan,sticky="news")
  }
}
tkadd(sub2_m1.2, "command", label="Excel (.xlsx)", command=function()import2popex())

#=============================================================================
# 5. Help menu
#=============================================================================

#Petunjuk Pengujian Hipotesis Variansi
petunjuk_varians<-function()
{
  jendelapetunjukvar = tktoplevel(bg = getOption("Rgui.theme_bl")$primary)
  tkwm.title(jendelapetunjukvar,"Petunjuk Pengujian Hipotesis Varians")
  insertpetunjukvar=tktext(jendelapetunjukvar,font = teks2,width=85,height=32,bg = getOption("Rgui.theme_bl")$primary)
  tkgrid(tklabel(jendelapetunjukvar,text="Petunjuk Pengujian Hipotesis Variansi",font=teks1,bg = getOption("Rgui.theme_bl")$primary))
  tkgrid(insertpetunjukvar)
  tkinsert(insertpetunjukvar,"end",paste(
    "1) Pengujian Hipotesis Variansi Satu Populasi
      a. Summary
         - masukkan banyak sampel, variansi sampel, variansi populasi, dan alpha ke dalam kotak 
         - klik salah satu pilihan uji hipotesis untuk menampilkan output
      b. Input manual
         - masukkan banyak sampel dan alpha ke dalam kotak 
         - klik input, lalu akan muncul sebuah tabel yang kolomnya sebanyak sampel yang telah diinput
         - masukkan data ke dalam tabel
         - masukkan variansi populasi
         - klik salah satu pilihan uji hipotesis untuk menampilkan output
      c. Import
         - masukkan alpha dan variansi populasi ke dalam kotak
         - klik input lalu import data yang akan digunakan (pastikan data berbentuk SPSS atau Excel)
         - klik lihat data untuk memastikan data
         - klik salah satu uji hipotesis untuk menampilkan output
      
   2) Pengujian Hipotesis Variansi Dua Populasi
      a. Summary
         - masukkan banyak sampel 1, variansi sampel 1, banyak sampel 2, variansi sampel 2, dan alpha
           ke dalam kotak
         - klik salah satu pilihan uji hipotesis untuk menampilkan output
      b. Input manual
         - masukkan banyak sampel 1, banyak sampel 2, dan alpha ke dalam kotak
         - klik input, lalu akan muncul dua tabel yang kolomnnya sebanyak sampel yang telah diinput
         - masukkan data ke dalam tabel
         - klik salah satu pilihan uji hipotesis untuk menampilkan output
      c. Import
         - masukkan alpha ke dalam kotak
         - klik input lalu import data yang akan digunakan (pastikan data berbentuk SPSS atau Excel)
         - klik lihat data untuk memastikan data
         - klik salah satu uji hipotesis untuk menampilkan output
"))
  back = tkbutton(jendelapetunjukvar, text="Kembali",command = function()tkdestroy(jendelapetunjukvar),bg = getOption("Rgui.theme_bl")$background)
  tkgrid(back)
}
