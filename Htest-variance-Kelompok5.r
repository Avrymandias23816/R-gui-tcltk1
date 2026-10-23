#=============================================================================
# Program Uji Hipotesis Variansi Satu dan Dua Populasi
# Mata kuliah Compt Stat
#=============================================================================

library(tcltk)
library(readxl)

# Wanra Global ; pakai 60-30-10 rule
tcl("set", ":: primary_color", ) # or White for text
tcl("set", ":: 2nd_color", )
tcl("set", ":: 3rd_color", )

#=============================================================================
# 1. Struktur utama
#=============================================================================

# 1.1 Main window
Start_open <- function() {
  Start_page <- tktoplevel()

  # Title bar
  tktitle(Start_page) <- "... - Uji Hipotesis μ 1|2 Populasi"
  tkconfigure(Start_page, background = tclvalue(tcl("set", "::primary_color")))

  #  Frame size
  tkwm.geometry(Start_page, "")
  tkwm.minsize(Start_page, "")
  tkwm.maxsize(Start_page, "")

  # 1.2 Main window buttons

button_header <- tkframe((Start_page, background = tclvalue(tcl("set", "::primary_color")))
  tkpack(button_header, padx = 20, pady = 40)

  # Button Header
  tkpack(tklabel(
    frame_atas,
    text = "...",
    font = "Helvetica 20 bold",
    background = tclvalue(tcl("set", "::primary_color")),
    foreground = tclvalue(tcl("set", "::2nd_color"))
  ))
  tkpack(
    tklabel(
      frame_atas,
      text = "...",
      font = "Helvetica 10 italic",
    background = tclvalue(tcl("set", "::primary_color")),
    foreground = tclvalue(tcl("set", "::2nd_color"))
    ),
    pady = 5
  )
}

#=============================================================================
# 2. Input Data
#=============================================================================

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
  
  w1 = tktoplevel(bg="#c9a98d")
  tktitle(w1) = "Summary Data"
  tkgrid(tklabel(w1,text="Summary Data Pengujian Hipotesis Variansi Satu Populasi",font=teks1,bg="#c9a98d"),sticky="news")
  
  banyak = tclVar("  ")
  eb1 = tkentry(w1,width="20",textvariable = banyak,bg="#d3c3b2")
  tkgrid(tklabel(w1,text="Banyak sampel :",font=teks2,bg="#c9a98d"),sticky="news")
  tkgrid(eb1)
  
  s2 = tclVar(" ")
  eb2 = tkentry(w1,width="20",textvariable = s2,bg="#d3c3b2")
  tkgrid(tklabel(w1,text="Variansi sampel (S^2) :",font=teks2,bg="#c9a98d"),sticky="news")
  tkgrid(eb2)
  
  sig2 = tclVar(" ")
  eb3 = tkentry(w1,width="20",textvariable=sig2,bg="#d3c3b2")
  tkgrid(tklabel(w1,text="Variansi populasi (σ0^2) :",font=teks2,bg="#c9a98d"),sticky="news")
  tkgrid(eb3)
  
  alphaa = tclVar(" ")
  eb4=tkentry(w1,width="20",textvariable=alphaa,bg="#d3c3b2")
  tkgrid(tklabel(w1,text="Alpha (dalam %) :",font=teks2,bg="#c9a98d"),sticky="news")
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
      w11 = tktoplevel(bg="#d3c3b2")
      tktitle(w11) = "Hasil Analisis Data"
      insert = tktext(w11,font=teks2,width=82,height=18,bg="#d3c3b2")
      tkgrid(tklabel(w11,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      w11 = tktoplevel(bg="#d3c3b2")
      tktitle(w11) = "Hasil Analisis Data"
      insert = tktext(w11,font=teks2,width=82,height=18,bg="#d3c3b2")
      tkgrid(tklabel(w11,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
    tombol=tkbutton(w11,text="Kembali",command=function()tkdestroy(w11),bg="#c9a98d")
    tkgrid(tombol)
    tkgrid(tklabel(w11,text=" ",bg="#d3c3b2"))
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
      w11 = tktoplevel(bg="#d3c3b2")
      tktitle(w11) = "Hasil Analisis Data"
      insert = tktext(w11,font=teks2,width=85,height=20,bg="#d3c3b2")
      tkgrid(tklabel(w11,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      w11 = tktoplevel(bg="#d3c3b2")
      tktitle(w11) = "Hasil Analisis Data"
      insert = tktext(w11,font=teks2,width=85,height=20,bg="#d3c3b2")
      tkgrid(tklabel(w11,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      w11 = tktoplevel(bg="#d3c3b2")
      tktitle(w11) = "Hasil Analisis Data"
      insert = tktext(w11,font=teks2,width=85,height=20,bg="#d3c3b2")
      tkgrid(tklabel(w11,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
    tombol=tkbutton(w11,text="Kembali",command=function()tkdestroy(w11),bg="#c9a98d")
    tkgrid(tombol)
    tkgrid(tklabel(w11,text=" ",bg="#d3c3b2"))
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
      w11 = tktoplevel(bg="#d3c3b2")
      tktitle(w11) = "Hasil Analisis Data"
      insert = tktext(w11,font=teks2,width=82,height=18,bg="#d3c3b2")
      tkgrid(tklabel(w11,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      w11 = tktoplevel(bg="#d3c3b2")
      tktitle(w11) = "Hasil Analisis Data"
      insert = tktext(w11,font=teks2,width=82,height=18,bg="#d3c3b2")
      tkgrid(tklabel(w11,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
    tombol=tkbutton(w11,text="Kembali",command=function()tkdestroy(w11),bg="#c9a98d")
    tkgrid(tombol)
    tkgrid(tklabel(w11,text=" ",bg="#d3c3b2"))
  }
  #pilihan arah uji hipotesis satu populasi
  tkgrid(tklabel(w1,text="",bg="#c9a98d"))
  tkgrid(tklabel(w1,text="Pilih uji hipotesis",font=teks3,bg="#c9a98d"),sticky="news")
  kiri =tkbutton(w1,text="Sisi Kiri",command = function()outputsum1(),bg="#ebe1d1")
  tkgrid(kiri,sticky="news")
  sisi2=tkbutton(w1,text="2 Sisi",command=function()outputsum2(),bg="#ebe1d1")
  tkgrid(sisi2,sticky="news")
  kanan=tkbutton(w1,text="Sisi Kanan",command=function()outputsum3(),bg="#ebe1d1")
  tkgrid(kanan,sticky="news")
}

tkadd(sub_m1.1, "command", label="Summary", command = sumvar1)

#---------INPUT MANUAL---------#
manualvar1 = function()
{
  require(tcltk)
  require(tcltk2)
  
  w2 = tktoplevel(bg="#c9a98d")
  tktitle(w2) = "Input Manual Data"
  tkgrid(tklabel(w2,text="Input Data Pengujian Hipotesis Variansi Satu Populasi",font=teks1,bg="#c9a98d"),sticky="news")
  
  banyak1 = tclVar("  ")
  eb1 = tkentry(w2,width="20",textvariable = banyak1,bg="#d3c3b2")
  tkgrid(tklabel(w2,text="Banyak sampel :",font=teks2,bg="#c9a98d"),sticky="news")
  tkgrid(eb1)
  
  alfa = tclVar(" ")
  eb2=tkentry(w2,width="20",textvariable=alfa,bg="#d3c3b2")
  tkgrid(tklabel(w2,text="Alpha (dalam %) :",font=teks2,bg="#c9a98d"),sticky="news")
  tkgrid(eb2)
  tkgrid(tklabel(w2,text=" ",bg="#c9a98d"))
  input = tkbutton(w2,text="Input",command = function()inputvar(),bg="#ebe1d1")
  tkgrid(input)
  
  inputvar = function()
  {
    vp = tclVar(" ")
    n = as.numeric(tclvalue(banyak1))
    w21 = tktoplevel(bg="#ebe1d1")
    tktitle(w21) = "Input Data"
    tkgrid(tklabel(w21,text="Input Data",font=teks1,bg="#ebe1d1"),sticky="news")
    tkgrid(tklabel(w21,text="",bg="#ebe1d1"))
    
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
    tkgrid(tklabel(w21,text="Data: ",bg="#ebe1d1"),tabel)
    tkgrid(tklabel(w21,text=" ",bg="#ebe1d1"))
    tkgrid(tklabel(w21,text="Varians Populasi: ",bg="#ebe1d1"),tkentry(w21,width="10",textvariable=vp))
    tkgrid(tklabel(w21,text="",bg="#ebe1d1"))
    
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
        w211 = tktoplevel(bg="#d3c3b2")
        tktitle(w211) = "Hasil Analisis Data"
        insert = tktext(w211,font=teks2,bg="#d3c3b2",width=82,height=15)
        tkgrid(tklabel(w211,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
        w211 = tktoplevel(bg="#d3c3b2")
        tktitle(w211) = "Hasil Analisis Data"
        insert = tktext(w211,font=teks2,bg="#d3c3b2",width=82,height=15)
        tkgrid(tklabel(w211,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      tombol=tkbutton(w211,text="Kembali",command=function()tkdestroy(w211),bg="#c9a98d")
      tkgrid(tombol)
      tkgrid(tklabel(w211,text=" ",bg="#d3c3b2"))
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
        w211 = tktoplevel(bg="#d3c3b2")
        tktitle(w211) = "Hasil Analisis Data"
        insert = tktext(w211,font=teks2,width=85,height=17,bg="#d3c3b2")
        tkgrid(tklabel(w211,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
        w211 = tktoplevel(bg="#d3c3b2")
        tktitle(w211) = "Hasil Analisis Data"
        insert = tktext(w211,font=teks2,width=85,height=17,bg="#d3c3b2")
        tkgrid(tklabel(w211,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
        w211 = tktoplevel(bg="#d3c3b2")
        tktitle(w211) = "Hasil Analisis Data"
        insert = tktext(w211,font=teks2,width=85,height=17,bg="#d3c3b2")
        tkgrid(tklabel(w211,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      tombol=tkbutton(w211,text="Kembali",command=function()tkdestroy(w211),bg="#c9a98d")
      tkgrid(tombol)
      tkgrid(tklabel(w211,text=" ",bg="#d3c3b2"))
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
        w211 = tktoplevel(bg="#d3c3b2")
        tktitle(w211) = "Hasil Analisis Data"
        insert = tktext(w211,font=teks2,width=82,height=15,bg="#d3c3b2")
        tkgrid(tklabel(w211,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
        w211 = tktoplevel(bg="#d3c3b2")
        tktitle(w211) = "Hasil Analisis Data"
        insert = tktext(w211,font=teks2,bg="#d3c3b2",width=82,height=15)
        tkgrid(tklabel(w211,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      tombol=tkbutton(w211,text="Kembali",command=function()tkdestroy(w211),bg="#c9a98d")
      tkgrid(tombol)
      tkgrid(tklabel(w211,text=" ",bg="#d3c3b2"))
    }
    
    bkiri =tkbutton(w21,text="Sisi Kiri",command = function()outputmanual1(),bg="#ebe1d1")
    b2sisi=tkbutton(w21,text="2 Sisi",command=function()outputmanual2(),bg="#ebe1d1")
    bkanan=tkbutton(w21,text="Sisi Kanan",command=function()outputmanual3(),bg="#ebe1d1")
    tkgrid(bkiri,b2sisi,bkanan,sticky="we")
  }
}

tkadd(sub_m1.1, "command", label="Input Data", command= manualvar1)

#---------IMPORT DATA---------#
tkadd(sub_m1.1, "cascade", label="Import Data", menu=sub2_m1.1)

import1pop = function()
{
  jendelaimport1 = tktoplevel(bg="#c9a98d")
  tktitle(jendelaimport1) = "Import Data"
  tkgrid(tklabel(jendelaimport1, text="Import Data SPSS Pengujian Hipotesis Variansi Satu Populasi", font = teks1,bg="#c9a98d"),sticky="news")
  tkgrid(tklabel(jendelaimport1,text=" ",bg="#c9a98d"))
  tkgrid(tklabel(jendelaimport1,text="Pastikan data format Anda berupa .sav",font=teks3,bg="#c9a98d"),sticky="news")
  tkgrid(tklabel(jendelaimport1,text=" ",bg="#c9a98d"))
  
  alphaa = tclVar(" ")
  eb1 = tkentry(jendelaimport1,width="20",textvariable=alphaa,bg="#d3c3b2")
  tkgrid(tklabel(jendelaimport1,text="Alpha (dalam %) :",font=teks2,bg="#c9a98d"),sticky="news")
  tkgrid(eb1)
  
  varpop = tclVar(" ")
  eb2 = tkentry(jendelaimport1,width="20",textvariable=varpop,bg="#d3c3b2")
  tkgrid(tklabel(jendelaimport1,text="Variansi Populasi :",font=teks2,bg="#c9a98d"),sticky="news")
  tkgrid(eb2)
  
  tkgrid(tklabel(jendelaimport1,text=" ",bg="#c9a98d"))
  import=tkbutton(jendelaimport1,text="Import",command = function()import1(),bg="#ebe1d1")
  tkgrid(import)
  tkgrid(tklabel(jendelaimport1,text=" ",bg="#c9a98d"))
  
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=15,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=15,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      tombol=tkbutton(jendelaoutput,text="Kembali",command=function()tkdestroy(jendelaoutput),bg="#c9a98d")
      tkgrid(tombol)
      tkgrid(tklabel(jendelaoutput,text=" ",bg="#d3c3b2"))
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=85,height=15,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=85,height=15,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=85,height=15,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      tombol=tkbutton(jendelaoutput,text="Kembali",command=function()tkdestroy(jendelaoutput),bg="#c9a98d")
      tkgrid(tombol)
      tkgrid(tklabel(jendelaoutput,text=" ",bg="#d3c3b2"))
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=15,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=15,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      tombol=tkbutton(jendelaoutput,text="Kembali",command=function()tkdestroy(jendelaoutput),bg="#c9a98d")
      tkgrid(tombol)
      tkgrid(tklabel(jendelaoutput,text=" ",bg="#d3c3b2"))
    }
    
    tombollihat = tkbutton(jendelaimport1,text="Lihat Data",command = lihatdata,bg="#ebe1d1")
    tkgrid(tombollihat)
    tkgrid(tklabel(jendelaimport1,text=" ",bg="#c9a98d"))
    
    bkiri =tkbutton(jendelaimport1,text="Sisi Kiri",command = ujisisikiri,bg="#ebe1d1")
    b2sisi=tkbutton(jendelaimport1,text="2 Sisi",command = ujiduasisi,bg="#ebe1d1")
    bkanan=tkbutton(jendelaimport1,text="Sisi Kanan",command = ujisisikanan,bg="#ebe1d1")
    tkgrid(bkiri,sticky="news")
    tkgrid(b2sisi,sticky="news")
    tkgrid(bkanan,sticky="news")
    
  }
  
}
tkadd(sub2_m1.1, "command", label="SPSS (.sav)", command=function()import1pop())

import1popex = function()
{
  jendelaimport1 = tktoplevel(bg="#c9a98d")
  tktitle(jendelaimport1) = "Import Data"
  tkgrid(tklabel(jendelaimport1, text="Import Data Excel Pengujian Hipotesis Variansi Satu Populasi", font = teks1,bg="#c9a98d"),sticky="news")
  tkgrid(tklabel(jendelaimport1,text=" ",bg="#c9a98d"))
  tkgrid(tklabel(jendelaimport1,text="Pastikan data format Anda berupa .xlsx",font=teks3,bg="#c9a98d"),sticky="news")
  tkgrid(tklabel(jendelaimport1,text=" ",bg="#c9a98d"))
  
  alphaa = tclVar(" ")
  eb1 = tkentry(jendelaimport1,width="20",textvariable=alphaa,bg="#d3c3b2")
  tkgrid(tklabel(jendelaimport1,text="Alpha (dalam %) :",font=teks2,bg="#c9a98d"),sticky="news")
  tkgrid(eb1)
  
  varpop = tclVar(" ")
  eb2 = tkentry(jendelaimport1,width="20",textvariable=varpop,bg="#d3c3b2")
  tkgrid(tklabel(jendelaimport1,text="Variansi Populasi :",font=teks2,bg="#c9a98d"),sticky="news")
  tkgrid(eb2)
  
  tkgrid(tklabel(jendelaimport1,text=" ",bg="#c9a98d"))
  import=tkbutton(jendelaimport1,text="Import",command = function()import1(),bg="#ebe1d1")
  tkgrid(import)
  tkgrid(tklabel(jendelaimport1,text=" ",bg="#c9a98d"))
  
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=15,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=15,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      tombol=tkbutton(jendelaoutput,text="Kembali",command=function()tkdestroy(jendelaoutput),bg="#c9a98d")
      tkgrid(tombol)
      tkgrid(tklabel(jendelaoutput,text=" ",bg="#d3c3b2"))
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=85,height=15,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=85,height=15,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=85,height=15,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      tombol=tkbutton(jendelaoutput,text="Kembali",command=function()tkdestroy(jendelaoutput),bg="#c9a98d")
      tkgrid(tombol)
      tkgrid(tklabel(jendelaoutput,text=" ",bg="#d3c3b2"))
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=15,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=15,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      tombol=tkbutton(jendelaoutput,text="Kembali",command=function()tkdestroy(jendelaoutput),bg="#c9a98d")
      tkgrid(tombol)
      tkgrid(tklabel(jendelaoutput,text=" ",bg="#d3c3b2"))
    }
    
    tombollihat = tkbutton(jendelaimport1,text="Lihat Data",command = lihatdata,bg="#ebe1d1")
    tkgrid(tombollihat)
    tkgrid(tklabel(jendelaimport1,text=" ",bg="#c9a98d"))
    
    bkiri =tkbutton(jendelaimport1,text="Sisi Kiri",command = ujisisikiri,bg="#ebe1d1")
    b2sisi=tkbutton(jendelaimport1,text="2 Sisi",command = ujiduasisi,bg="#ebe1d1")
    bkanan=tkbutton(jendelaimport1,text="Sisi Kanan",command = ujisisikanan,bg="#ebe1d1")
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
  
  w3 = tktoplevel(bg="#c9a98d")
  tktitle(w3) = "Summary Data"
  tkgrid(tklabel(w3,text="Summary Data Pengujian Hipotesis Variansi Dua Populasi",font=teks1,bg="#c9a98d"),sticky="news")
  
  tkgrid(tklabel(w3,text="Sampel 1",font=teks3,bg="#c9a98d"))
  banyak1 = tclVar("  ")
  eb1 = tkentry(w3,width="20",textvariable = banyak1,bg="#d3c3b2")
  tkgrid(tklabel(w3,text="Banyak sampel 1 :",font=teks2,bg="#c9a98d"),sticky="news")
  tkgrid(eb1)
  
  varsam1 = tclVar(" ")
  eb2 = tkentry(w3,width="20",textvariable = varsam1,bg="#d3c3b2")
  tkgrid(tklabel(w3,text="Variansi sampel 1 (S1^2) :",font=teks2,bg="#c9a98d"),sticky="news")
  tkgrid(eb2)
  
  tkgrid(tklabel(w3,text="",bg="#c9a98d"))
  tkgrid(tklabel(w3,text="Sampel 2",font=teks3,bg="#c9a98d"))
  
  banyak2 = tclVar("  ")
  eb4 = tkentry(w3,width="20",textvariable = banyak2,bg="#d3c3b2")
  tkgrid(tklabel(w3,text="Banyak sampel 2 :",font=teks2,bg="#c9a98d"),sticky="news")
  tkgrid(eb4)
  
  varsam2 = tclVar(" ")
  eb5 = tkentry(w3,width="20",textvariable = varsam2,bg="#d3c3b2")
  tkgrid(tklabel(w3,text="Variansi sampel 2 (S2^2) :",font=teks2,bg="#c9a98d"),sticky="news")
  tkgrid(eb5)
  
  tkgrid(tklabel(w3,text="",bg="#c9a98d"))
  alphaa = tclVar(" ")
  eb7=tkentry(w3,width="20",textvariable=alphaa,bg="#d3c3b2")
  tkgrid(tklabel(w3,text="Alpha (dalam %) :",font=teks2,bg="#c9a98d"),sticky="news")
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
      w31 = tktoplevel(bg="#d3c3b2")
      tktitle(w31) = "Hasil Analisis Data"
      insert = tktext(w31,font=teks2,width=82,height=12,bg="#d3c3b2")
      tkgrid(tklabel(w31,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      w31 = tktoplevel(bg="#d3c3b2")
      tktitle(w31) = "Hasil Analisis Data"
      insert = tktext(w31,font=teks2,width=82,height=12,bg="#d3c3b2")
      tkgrid(tklabel(w31,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
    tombol=tkbutton(w31,text="Kembali",command=function()tkdestroy(w31),bg="#c9a98d")
    tkgrid(tombol)
    tkgrid(tklabel(w31,text=" ",bg="#d3c3b2"))
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
      w31 = tktoplevel(bg="#d3c3b2")
      tktitle(w31) = "Hasil Analisis Data"
      insert = tktext(w31,font=teks2,width=82,height=12,bg="#d3c3b2")
      tkgrid(tklabel(w31,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      w31 = tktoplevel(bg="#d3c3b2")
      tktitle(w31) = "Hasil Analisis Data"
      insert = tktext(w31,font=teks2,width=82,height=12,bg="#d3c3b2")
      tkgrid(tklabel(w31,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      w31 = tktoplevel(bg="#d3c3b2")
      tktitle(w31) = "Hasil Analisis Data"
      insert = tktext(w31,font=teks2,width=82,height=12,bg="#d3c3b2")
      tkgrid(tklabel(w31,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
    tombol=tkbutton(w31,text="Kembali",command=function()tkdestroy(w31),bg="#c9a98d")
    tkgrid(tombol)
    tkgrid(tklabel(w31,text=" ",bg="#d3c3b2"))
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
      w31 = tktoplevel(bg="#d3c3b2")
      tktitle(w31) = "Hasil Analisis Data"
      insert = tktext(w31,font=teks2,width=82,height=12,bg="#d3c3b2")
      tkgrid(tklabel(w31,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      w31 = tktoplevel(bg="#d3c3b2")
      tktitle(w31) = "Hasil Analisis Data"
      insert = tktext(w31,font=teks2,width=82,height=12,bg="#d3c3b2")
      tkgrid(tklabel(w31,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
    tombol=tkbutton(w31,text="Kembali",command=function()tkdestroy(w31),bg="#c9a98d")
    tkgrid(tombol)
    tkgrid(tklabel(w31,text=" ",bg="#d3c3b2"))
  }
  
  #pilihan arah uji hipotesis satu populasi
  tkgrid(tklabel(w3,text="",bg="#c9a98d"))
  tkgrid(tklabel(w3,text="Pilih uji hipotesis",font=teks3,bg="#c9a98d"),sticky="news")
  kiri =tkbutton(w3,text="Sisi Kiri",command = function()outputsumvar1(),bg="#ebe1d1")
  tkgrid(kiri,sticky="news")
  sisi2=tkbutton(w3,text="2 Sisi",command=function()outputsumvar2(),bg="#ebe1d1")
  tkgrid(sisi2,sticky="news")
  kanan=tkbutton(w3,text="Sisi Kanan",command=function()outputsumvar3(),bg="#ebe1d1")
  tkgrid(kanan,sticky="news")
}
tkadd(sub_m1.2, "command", label="Summary", command=function()sumvar2())

#------INPUT MANUAL----#
manualvar2 = function()
{
  w4 = tktoplevel(bg="#c9a98d")
  tktitle(w4) = "Input Manual"
  tkgrid(tklabel(w4,text="Input Data Pengujian Hipotesis Variansi Dua Populasi",font=teks1,bg="#c9a98d"),sticky="news")
  
  banyak1 = tclVar("  ")
  eb1 = tkentry(w4,width="20",textvariable = banyak1,bg="#d3c3b2")
  tkgrid(tklabel(w4,text="Banyak sampel 1 :",font=teks2,bg="#c9a98d"),sticky="news")
  tkgrid(eb1)
  
  banyak2 = tclVar("  ")
  eb2 = tkentry(w4,width="20",textvariable = banyak2,bg="#d3c3b2")
  tkgrid(tklabel(w4,text="Banyak sampel 2 :",font=teks2,bg="#c9a98d"),sticky="news")
  tkgrid(eb2)
  
  alfa = tclVar(" ")
  eb3=tkentry(w4,width="20",textvariable=alfa,bg="#d3c3b2")
  tkgrid(tklabel(w4,text="Alpha (dalam %) :",font=teks2,bg="#c9a98d"),sticky="news")
  tkgrid(eb3)
  tkgrid(tklabel(w4,text=" ",bg="#c9a98d"))
  
  input=tkbutton(w4,text="Input",command = function()inputvarman(),bg="#ebe1d1")
  tkgrid(input)
  tkgrid(tklabel(w4,text=" ",bg="#c9a98d"))
  
  inputvarman = function()
  {
    n1 = as.numeric(tclvalue(banyak1))
    n2 = as.numeric(tclvalue(banyak2))
    w41 = tktoplevel(bg="#ebe1d1")
    tktitle(w41) = "Input data"
    tkgrid(tklabel(w41,text="Masukkan data",font=teks1,bg="#ebe1d1"),sticky="n")
    tkgrid(tklabel(w41,text="",bg="#ebe1d1"))
    
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
    tabelp1=tkwidget(w41,"table",variable=layoutinput1,rows=n1+1,cols=2,titlerows=1,titlecols=1,selectmode="extended  ",colwidth=8,rowheight=2,bg="#d3c3b2")
    tkconfigure(tabelp1,selectmode="extended",rowseparator="\n",colseparator="\t")
    tabelp2=tkwidget(w41,"table",variable=layoutinput2,rows=n2+1,cols=2,titlerows=1,titlecols=1,selectmode="extended  ",colwidth=8,rowheight=2,bg="#d3c3b2")
    tkconfigure(tabelp2,selectmode="extended",rowseparator="\n",colseparator="\t")
    tkgrid(tklabel(w41,text="Sampel 1:",font=teks3,bg="#ebe1d1"),tabelp1,tklabel(w41,text="Sampel 2:",font=teks3,bg="#ebe1d1"),tabelp2,sticky="n")
    
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
        w411 = tktoplevel(bg="#d3c3b2")
        tktitle(w411) = "Hasil Analisis Data"
        insert = tktext(w411,font=teks2,width=82,height=12,bg="#d3c3b2")
        tkgrid(tklabel(w411,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
        w411 = tktoplevel(bg="#d3c3b2")
        tktitle(w411) = "Hasil Analisis Data"
        insert = tktext(w411,font=teks2,width=82,height=12,bg="#d3c3b2")
        tkgrid(tklabel(w411,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      tombol=tkbutton(w411,text="Kembali",command=function()tkdestroy(w411),bg="#c9a98d")
      tkgrid(tombol)
      tkgrid(tklabel(w411,text=" ",bg="#d3c3b2"))
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
        w411 = tktoplevel(bg="#d3c3b2")
        tktitle(w411) = "Hasil Analisis Data"
        insert = tktext(w411,font=teks2,width=82,height=12,bg="#d3c3b2")
        tkgrid(tklabel(w411,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
        w411 = tktoplevel(bg="#d3c3b2")
        tktitle(w411) = "Hasil Analisis Data"
        insert = tktext(w411,font=teks2,width=82,height=12,bg="#d3c3b2")
        tkgrid(tklabel(w411,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      tombol=tkbutton(w411,text="Kembali",command=function()tkdestroy(w411),bg="#c9a98d")
      tkgrid(tombol)
      tkgrid(tklabel(w411,text=" ",bg="#d3c3b2"))
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
        w411 = tktoplevel(bg="#d3c3b2")
        tktitle(w411) = "Hasil Analisis Data"
        insert = tktext(w411,font=teks2,width=82,height=12,bg="#d3c3b2")
        tkgrid(tklabel(w411,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
        w411 = tktoplevel(bg="#d3c3b2")
        tktitle(w411) = "Hasil Analisis Data"
        insert = tktext(w411,font=teks2,width=82,height=12,bg="#d3c3b2")
        tkgrid(tklabel(w411,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      tombol=tkbutton(w411,text="Kembali",command=function()tkdestroy(w411),bg="#c9a98d")
      tkgrid(tombol)
      tkgrid(tklabel(w411,text=" ",bg="#d3c3b2"))
    }
    tkgrid(tklabel(w41,text="",bg="#ebe1d1"))
    tombolkiri =tkbutton(w41,text="Sisi Kiri",command = function()outputmanualv1(),bg="#ebe1d1")
    tkgrid(tombolkiri,sticky="news")
    tombolkanan=tkbutton(w41,text="2 Sisi",command=function()outputmanualv2(),bg="#ebe1d1")
    tkgrid(tombolkanan,sticky="news")
    tombolkanan=tkbutton(w41,text="Sisi Kanan",command=function()outputmanualv3(),bg="#ebe1d1")
    tkgrid(tombolkanan,sticky="news")
    
  }
}
tkadd(sub_m1.2, "command", label="Input Data", command=function()manualvar2())

#IMPORT DATA
tkadd(sub_m1.2, "cascade", label="Import Data", menu=sub2_m1.2)

import2pop = function()
{
  jendelaimport2 = tktoplevel(bg="#c9a98d")
  tktitle(jendelaimport2) = "Import Data"
  tkgrid(tklabel(jendelaimport2, text="Import Data SPSS Pengujian Hipotesis Variansi Dua Populasi", font = teks1,bg="#c9a98d"),sticky="news")
  tkgrid(tklabel(jendelaimport2,text="Pastikan data anda berupa SPSS",font=teks3,bg="#c9a98d"),sticky="news")
  tkgrid(tklabel(jendelaimport2,text=" ",bg="#c9a98d"))
  
  alphaa = tclVar(" ")
  eb4 = tkentry(jendelaimport2,width="20",textvariable=alphaa,bg="#d3c3b2")
  tkgrid(tklabel(jendelaimport2,text="Alpha (dalam %) :",font=teks2,bg="#c9a98d"),sticky="news")
  tkgrid(eb4)
  tkgrid(tklabel(jendelaimport2,text=" ",bg="#c9a98d"))
  tkgrid(tkbutton(jendelaimport2,text="Import",command = function()import2()))
  tkgrid(tklabel(jendelaimport2,text=" ",bg="#c9a98d"))
  
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=17,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=17,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      tombol=tkbutton(jendelaoutput,text="Kembali",command=function()tkdestroy(jendelaoutput),bg="#c9a98d")
      tkgrid(tombol)
      tkgrid(tklabel(jendelaoutput,text=" ",bg="#d3c3b2"))
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=17,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=17,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      tombol=tkbutton(jendelaoutput,text="Kembali",command=function()tkdestroy(jendelaoutput),bg="#c9a98d")
      tkgrid(tombol)
      tkgrid(tklabel(jendelaoutput,text=" ",bg="#d3c3b2"))
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=17,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=17,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      tombol=tkbutton(jendelaoutput,text="Kembali",command=function()tkdestroy(jendelaoutput),bg="#c9a98d")
      tkgrid(tombol)
      tkgrid(tklabel(jendelaoutput,text=" ",bg="#d3c3b2"))
    }
    
    tombollihat = tkbutton(jendelaimport2,text="Lihat Data",command = lihatdata,bg="#ebe1d1")
    tkgrid(tombollihat)
    tkgrid(tklabel(jendelaimport2,text=" ",bg="#c9a98d"))
    
    bkiri =tkbutton(jendelaimport2,text="Sisi Kiri",command = ujisisikiri,bg="#ebe1d1")
    b2sisi=tkbutton(jendelaimport2,text="2 Sisi",command = ujiduasisi,bg="#ebe1d1")
    bkanan=tkbutton(jendelaimport2,text="Sisi Kanan",command = ujisisikanan,bg="#ebe1d1")
    tkgrid(bkiri,sticky="news")
    tkgrid(b2sisi,sticky="news")
    tkgrid(bkanan,sticky="news")
  }
}
tkadd(sub2_m1.2, "command", label="SPSS (.sav)", command=function()import2pop())

import2popex = function()
{
  jendelaimport2 = tktoplevel(bg="#c9a98d")
  tktitle(jendelaimport2) = "Import Data"
  tkgrid(tklabel(jendelaimport2, text="Import Data Excel Pengujian Hipotesis Variansi Dua Populasi", font = teks1,bg="#c9a98d"),sticky="news")
  tkgrid(tklabel(jendelaimport2,text="Pastikan format data anda berupa .xlsx",font=teks3,bg="#c9a98d"),sticky="news")
  tkgrid(tklabel(jendelaimport2,text=" ",bg="#c9a98d"))
  
  alphaa = tclVar(" ")
  eb4 = tkentry(jendelaimport2,width="20",textvariable=alphaa,bg="#d3c3b2")
  tkgrid(tklabel(jendelaimport2,text="Alpha (dalam %) :",font=teks2,bg="#c9a98d"),sticky="news")
  tkgrid(eb4)
  tkgrid(tklabel(jendelaimport2,text=" ",bg="#c9a98d"))
  tkgrid(tkbutton(jendelaimport2,text="Import",command = function()import2()))
  tkgrid(tklabel(jendelaimport2,text=" ",bg="#c9a98d"))
  
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=17,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=17,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      tombol=tkbutton(jendelaoutput,text="Kembali",command=function()tkdestroy(jendelaoutput),bg="#c9a98d")
      tkgrid(tombol)
      tkgrid(tklabel(jendelaoutput,text=" ",bg="#d3c3b2"))
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=17,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=17,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      tombol=tkbutton(jendelaoutput,text="Kembali",command=function()tkdestroy(jendelaoutput),bg="#c9a98d")
      tkgrid(tombol)
      tkgrid(tklabel(jendelaoutput,text=" ",bg="#d3c3b2"))
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=17,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
        jendelaoutput = tktoplevel(bg="#d3c3b2")
        tktitle(jendelaoutput) = "Hasil Analisis Data"
        insert = tktext(jendelaoutput,font=teks2,width=82,height=17,bg="#d3c3b2")
        tkgrid(tklabel(jendelaoutput,text="Hasil Analisis Data",font=teks1,bg="#d3c3b2"))
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
      tombol=tkbutton(jendelaoutput,text="Kembali",command=function()tkdestroy(jendelaoutput),bg="#c9a98d")
      tkgrid(tombol)
      tkgrid(tklabel(jendelaoutput,text=" ",bg="#d3c3b2"))
    }
    
    tombollihat = tkbutton(jendelaimport2,text="Lihat Data",command = lihatdata,bg="#ebe1d1")
    tkgrid(tombollihat)
    tkgrid(tklabel(jendelaimport2,text=" ",bg="#c9a98d"))
    
    bkiri =tkbutton(jendelaimport2,text="Sisi Kiri",command = ujisisikiri,bg="#ebe1d1")
    b2sisi=tkbutton(jendelaimport2,text="2 Sisi",command = ujiduasisi,bg="#ebe1d1")
    bkanan=tkbutton(jendelaimport2,text="Sisi Kanan",command = ujisisikanan,bg="#ebe1d1")
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
  jendelapetunjukvar = tktoplevel(bg="#d3c3b2")
  tkwm.title(jendelapetunjukvar,"Petunjuk Pengujian Hipotesis Varians")
  insertpetunjukvar=tktext(jendelapetunjukvar,font = teks2,width=85,height=32,bg="#d3c3b2")
  tkgrid(tklabel(jendelapetunjukvar,text="Petunjuk Pengujian Hipotesis Variansi",font=teks1,bg="#d3c3b2"))
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
  back = tkbutton(jendelapetunjukvar, text="Kembali",command = function()tkdestroy(jendelapetunjukvar),bg="#c9a98d")
  tkgrid(back)
}
tkadd(topmenu4, "command", label="Pengujian Hipotesis Variansi", command=petunjuk_varians)		
