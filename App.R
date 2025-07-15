library(shiny)
library(shinydashboard)
library(readxl)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
rsconnect::writeManifest()
ui <- dashboardPage(
  dashboardHeader(title = "Sumatra Agroclimate"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Tren Tahunan", tabName = "tren", icon = icon("chart-line")),
      menuItem("Perbandingan Komoditas", tabName = "perbandingan", icon = icon("balance-scale")),
      menuItem("Analisis Hubungan", tabName = "analisis", icon = icon("bar-chart")),
      menuItem("Peta", tabName = "peta", icon = icon("globe-asia")),
      menuItem("Tabel Data", tabName = "tabel", icon = icon("table")),
      menuItem("Metadata", tabName = "metadata", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "beranda",
              fluidRow(
                box(
                  title = "📊 Visualisasi Produksi Perkebunan di Pulau Sumatera (2017–2023)",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  p("Dashboard ini menyajikan data produksi komoditas perkebunan utama di wilayah Sumatera, 
               serta informasi iklim yang berkaitan (suhu dan curah hujan). Beberapa fitur utama dashboard ini antara lain:"),
                  tags$ul(
                    tags$li(strong("Tren Tahunan:"), " Memantau perkembangan produksi setiap tahun."),
                    tags$li(strong("Perbandingan Komoditas:"), " Membandingkan produksi antar jenis atau wilayah."),
                    tags$li(strong("Analisis Hubungan:"), " Melihat pengaruh iklim terhadap produksi komoditas."),
                    tags$li(strong("Peta Interaktif:"), " Menjelajahi produksi antar provinsi di Sumatera."),
                    tags$li(strong("Tabel Data:"), " Akses langsung ke data produksi dan iklim."),
                    tags$li(strong("Metadata:"), " Menampilkan sifat dan informasi penting dari setiap variabel data.")
                  ),
                  p("Gunakan menu di sebelah kiri untuk menjelajahi fitur dashboard.")
                )
              ),
              fluidRow(
                valueBoxOutput("box_total_produksi"),
                valueBoxOutput("box_jumlah_provinsi"),
                valueBoxOutput("box_jumlah_komoditas"),
                valueBoxOutput("box_provinsi_terbesar"),
                valueBoxOutput("box_komoditas_terbanyak"),
                valueBoxOutput("box_rata_rata_produksi")
              ),
              fluidRow(
                box(
                title = "Video Tutorial Penggunaan Dashboard",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                tags$iframe(
                  src = "https://drive.google.com/file/d/1i--87QUTajG-IOwoX_9uGizmJjL86FQQ/preview",
                  width = "100%",
                  height = "400px",
                  frameborder = "0",
                  allow = "autoplay"
                )
                )
              )
      ),
      tabItem(tabName = "tren",
              h2("Tren Tahunan"),
              fluidRow(
                box(
                  title = "Filter", status = "primary", solidHeader = TRUE, width = 12,
                  selectInput("tren_provinsi", "Pilih Provinsi:",
                              choices = c("Semua"), selected = "Semua"),
                  selectInput("tren_komoditas", "Pilih Komoditas:",
                              choices = c("Semua"), selected = "Semua")
                )
              ),
              fluidRow(
                box(
                  title = "Tren Produksi Tahunan", status = "info", solidHeader = TRUE, width = 12,
                  plotlyOutput("plot_tren_tahunan", height = "400px")
                )
              ),
              fluidRow(
                box(
                  title = "Korelasi Produksi dengan Iklim dan Curah Hujan", status = "warning", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("korelasi_output")
                )
              )
      ),
      tabItem(tabName = "analisis",
              h2("Analisis Hubungan: Pengaruh Suhu dan Curah Hujan terhadap Produksi Perkebunan"),
              fluidRow(
                box(
                  title = "Pengaturan Analisis", status = "primary", solidHeader = TRUE, width = 12,
                  
                  selectInput("analisis_provinsi", "Pilih Provinsi:",
                              choices = c("Semua")),
                  
                  selectInput("analisis_komoditas", "Pilih Komoditas:",
                              choices = c("Semua")),
                  
                  uiOutput("analisis_tahun_ui"),
                  
                  actionButton("jalankan_regresi", "Jalankan Analisis", icon = icon("play"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Scatter Plot: Produksi vs Suhu", status = "info", solidHeader = TRUE, width = 6,
                  plotOutput("plot_suhu"),
                  verbatimTextOutput("interpretasi_suhu")
                ),
                
                box(
                  title = "Scatter Plot: Produksi vs Curah Hujan", status = "info", solidHeader = TRUE, width = 6,
                  plotOutput("plot_hujan"),
                  verbatimTextOutput("interpretasi_hujan")
                )
              ),
              
              fluidRow(
                box(
                  title = "Hasil Regresi Linear", status = "success", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("regresi_output"),
                  verbatimTextOutput("interpretasi_regresi")
                )
              )
      ),
      tabItem(tabName = "peta",
              h2("Peta Interaktif Produksi Perkebunan"),
              fluidRow(
                box(
                  title = "Filter Peta", status = "primary", solidHeader = TRUE, width = 12,
                  sliderInput("peta_tahun", "Pilih Tahun:",
                              min = 2017, max = 2023, value = 2020, step = 1, sep = ""),
                  uiOutput("peta_jenis_ui"),
                  uiOutput("peta_detail_ui")
                )
              ),
              fluidRow(
                box(
                  title = "Peta Wilayah Sumatera", status = "info", solidHeader = TRUE, width = 12,
                  leafletOutput("peta_produksi", height = "500px")
                )
              )
      ),
      tabItem(tabName = "perbandingan",
              h2("Perbandingan Komoditas"),
              fluidRow(
                box(
                  title = "Filter", status = "primary", solidHeader = TRUE, width = 12,
                  selectInput("perbandingan_mode", "Opsi Menu Tampilan:",
                              choices = c("Jenis", "Wilayah"), selected = "Jenis"),
                  sliderInput("perbandingan_tahun", "Pilih Tahun:",
                              min = 2017, max = 2023, value = 2020, step = 1, sep = ""),
                  conditionalPanel(
                    condition = "input.perbandingan_mode == 'Wilayah'",
                    selectInput("perbandingan_komoditas", "Pilih Komoditas:",
                                choices = c("Semua"), selected = "Semua")
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Perbandingan Produksi Komoditas", status = "info", solidHeader = TRUE, width = 12,
                  plotlyOutput("plot_perbandingan_komoditas", height = "400px"),
                  verbatimTextOutput("info_perbandingan_komoditas")
                )
              )
      ),
      tabItem(tabName = "tabel",
              h2("Tabel Data"),
              tabsetPanel(
                tabPanel("Produksi",
                         downloadButton("download_produksi", "Unduh Data Produksi"),
                         DTOutput("tabel_produksi")
                ),
                tabPanel("Iklim (Monthly)",
                         downloadButton("download_iklim", "Unduh Data Iklim"),
                         DTOutput("tabel_iklim")
                ),
                tabPanel("Gabungan (Yearly)",
                         downloadButton("download_gabungan", "Unduh Data Gabungan"),
                         DTOutput("tabel_gabungan")
                )
              )
      ),
      tabItem(tabName = "metadata",
              h2("Metadata Variabel"),
              tabsetPanel(
                tabPanel("Metadata Produksi", 
                         fluidRow(
                           box(
                             title = "Informasi Metadata Variabel Produksi",
                             status = "primary",
                             solidHeader = TRUE,
                             width = 12,
                             DTOutput("tabel_metadata_produksi")
                           )
                         )
                ),
                tabPanel("Metadata Iklim",
                         fluidRow(
                           box(
                             title = "Informasi Metadata Variabel Iklim",
                             status = "info",
                             solidHeader = TRUE,
                             width = 12,
                             DTOutput("tabel_metadata_iklim")
                           )
                         )
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  produksi_data <- reactive({
    read_excel("Perkebunan_Sumatra.xlsx")
  })
  iklim_data <- reactive({
    read_excel("Iklim_Sumatra.xlsx")
  })
  shape_sumatera <- reactive({
    st_read("indonesia-prov.geojson")
  })

  iklim_tahunan <- reactive({
    iklim_data() %>%
      group_by(Provinsi, Tahun) %>%
      summarise(
        Suhu = mean(Suhu, na.rm = TRUE),
        CurahHujan = sum(CurahHujan, na.rm = TRUE),
        .groups = "drop"
      )
  })
  # Daftar provinsi di Sumatera
  provinsi_sumatera <- c(
    "Aceh", "Sumatera Utara", "Sumatera Barat", "Riau", "Kepulauan Riau",
    "Jambi", "Bengkulu", "Sumatera Selatan", "Kepulauan Bangka Belitung", "Lampung"
  )
  
  # Join Iklim dan produksi
  gabung_data <- reactive({
    produksi_data() %>%
      left_join(iklim_tahunan(), by = c("Provinsi", "Tahun")) %>%
      filter(Provinsi %in% provinsi_sumatera)
  })
  output$box_total_produksi <- renderValueBox({
    total <- sum(gabung_data()$Produksi, na.rm = TRUE)
    valueBox(
      value = paste0(round(total, 2), " Ribu Ton"),
      subtitle = "Total Produksi (2017–2023)",
      icon = icon("leaf"),
      color = "green"
    )
  })
  
  output$box_jumlah_provinsi <- renderValueBox({
    jumlah <- length(unique(gabung_data()$Provinsi))
    valueBox(
      value = jumlah,
      subtitle = "Jumlah Provinsi",
      icon = icon("map"),
      color = "blue"
    )
  })
  
  output$box_jumlah_komoditas <- renderValueBox({
    jumlah <- length(unique(gabung_data()$Komoditas))
    valueBox(
      value = jumlah,
      subtitle = "Jumlah Komoditas",
      icon = icon("seedling"),
      color = "olive"
    )
  })
  output$box_provinsi_terbesar <- renderValueBox({
    df <- produksi_data() %>%
      group_by(Provinsi) %>%
      summarise(TotalProduksi = sum(Produksi, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(TotalProduksi))
    
    provinsi_top <- df$Provinsi[1]
    produksi_top <- df$TotalProduksi[1]
    
    valueBox(
      value = provinsi_top,
      subtitle = paste("Provinsi Produksi Tertinggi (", format(round(produksi_top,0), big.mark=","), " Ribu Ton)", sep=""),
      icon = icon("trophy"),
      color = "red"
    )
  })
  
  output$box_komoditas_terbanyak <- renderValueBox({
    df <- produksi_data() %>%
      group_by(Komoditas) %>%
      summarise(TotalProduksi = sum(Produksi, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(TotalProduksi))
    
    komoditas_top <- df$Komoditas[1]
    produksi_top <- df$TotalProduksi[1]
    
    valueBox(
      value = komoditas_top,
      subtitle = paste("Komoditas Utama Sumatra (", format(round(produksi_top,0), big.mark=","), " Ribu Ton)", sep=""),
      icon = icon("leaf"),
      color = "orange"
    )
  })
  
  output$box_rata_rata_produksi <- renderValueBox({
    total_produksi <- sum(produksi_data()$Produksi, na.rm = TRUE)
    jumlah_provinsi <- produksi_data() %>% distinct(Provinsi) %>% nrow()
    rata2 <- total_produksi / jumlah_provinsi
    
    valueBox(
      value = paste0(format(round(rata2, 0), big.mark = ","), " Ribu Ton"),
      subtitle = "Rata-rata Produksi per Provinsi",
      icon = icon("balance-scale"),
      color = "purple"
    )
  })
  
  # Output Tabel
  output$tabel_produksi <- renderDT({
    datatable(produksi_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  output$tabel_iklim <- renderDT({
    datatable(iklim_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  output$tabel_gabungan <- renderDT({
    datatable(gabung_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  observe({
    df <- gabung_data()
    updateSelectInput(
      session,
      "analisis_provinsi",
      choices = c("Semua", sort(unique(df$Provinsi)))
    )
    updateSelectInput(
      session,
      "analisis_komoditas",
      choices = c("Semua", sort(unique(df$Komoditas)))
    )
      print("Isi gabung_data()")
      print(head(gabung_data()))
    
  })
  
  data_analisis <- reactive({
    df <- gabung_data()
    if (input$analisis_provinsi != "Semua") {
      df <- df %>% filter(Provinsi == input$analisis_provinsi)
    }
    if (input$analisis_komoditas != "Semua") {
      df <- df %>% filter(Komoditas == input$analisis_komoditas)
    }
    req(input$analisis_tahun)
    df <- df %>% filter(Tahun >= input$analisis_tahun[1], Tahun <= input$analisis_tahun[2])
    validate(
      need(nrow(df) > 2, "Data tidak cukup untuk analisis. Pilih komoditas lain.")
    )
    df
  })
  
  # Event: Jalankan regresi
  model_regresi <- eventReactive(input$jalankan_regresi, {
    lm(Produksi ~ Suhu + CurahHujan, data = data_analisis())
  })
  
  # Output ringkasan regresi
  output$regresi_output <- renderPrint({
    req(model_regresi())
    summary(model_regresi())
  })
  output$interpretasi_regresi <- renderText({
    model <- model_regresi()
    if (is.null(model)) return("Belum ada model yang dihitung")
    
    coef <- summary(model)$coefficients
    
    if (!all(c("Suhu", "CurahHujan") %in% rownames(coef))) {
      return("Model tidak valid: Suhu atau CurahHujan tidak masuk ke dalam model. Pastikan data memiliki variasi yang cukup.")
    }
    
    suhu_coef <- coef["Suhu", "Estimate"]
    hujan_coef <- coef["CurahHujan", "Estimate"]
    r_squared <- summary(model)$r.squared
    
    arah_suhu <- ifelse(suhu_coef > 0, "meningkatkan", "menurunkan")
    arah_hujan <- ifelse(hujan_coef > 0, "meningkatkan", "menurunkan")
    
    paste0(
      "Berdasarkan model regresi:\n",
      "- Suhu cenderung ", arah_suhu, " produksi sebesar ", round(suhu_coef, 2), " untuk setiap 1°C kenaikan.\n",
      "- Curah hujan cenderung ", arah_hujan, " produksi sebesar ", round(hujan_coef, 2), " untuk setiap 1 mm tambahan.\n",
      "Model ini menjelaskan sekitar ", round(r_squared * 100, 2), "% variasi produksi (R² = ", round(r_squared, 3), ")."
    )
  })
  
  output$plot_suhu <- renderPlot({
    df <- data_analisis() %>%
      filter(!is.na(Suhu), !is.na(Produksi))
    validate(
      need(nrow(df) > 2, "Data tidak cukup untuk membuat plot.")
    )
    ggplot(df, aes(x = Suhu, y = Produksi)) +
      geom_point(color = "steelblue") +
      geom_smooth(method = "lm", se = TRUE, color = "red") +
      labs(title = "Produksi vs Suhu",
           x = "Suhu (Celsius)",
           y = "Produksi (Ribu Ton)") +
      theme_minimal()
  })
  
  output$interpretasi_suhu <- renderText({
    df <- data_analisis()
    
    if (nrow(df) < 3) return("Tidak cukup data untuk menghitung korelasi.")
    
    if (sd(df$Suhu, na.rm = TRUE) == 0 || sd(df$Produksi, na.rm = TRUE) == 0) {
      return("Korelasi tidak dapat dihitung karena variabel konstan atau tidak bervariasi.")
    }
    
    cor_suhu <- cor(df$Suhu, df$Produksi, use = "complete.obs")
    
    if (is.na(cor_suhu)) return("Tidak dapat dihitung karena data tidak cukup.")
    
    hubungan <- ifelse(cor_suhu > 0, "positif", "negatif")
    kekuatan <- case_when(
      abs(cor_suhu) >= 0.7 ~ "kuat",
      abs(cor_suhu) >= 0.4 ~ "sedang",
      TRUE ~ "lemah"
    )
    
    paste0(
      "Terdapat hubungan ", hubungan, " yang ", kekuatan,
      " antara suhu dan produksi komoditas. Nilai korelasi: ",
      round(cor_suhu, 2), "."
    )
  })
  
  
  output$plot_hujan <- renderPlot({
    df <- data_analisis() %>%
      filter(!is.na(CurahHujan), !is.na(Produksi))
    validate(
      need(nrow(df) > 2, "Data tidak cukup untuk membuat plot.")
    )
    ggplot(df, aes(x = CurahHujan, y = Produksi)) +
      geom_point(color = "darkgreen") +
      geom_smooth(method = "lm", se = TRUE, color = "red") +
      labs(title = "Produksi vs Curah Hujan",
           x = "Curah Hujan (mm)",
           y = "Produksi (Ribu Ton)") +
      theme_minimal()
  })
  
  output$interpretasi_hujan <- renderText({
    df <- data_analisis()
    
    if (nrow(df) < 3) return("Tidak cukup data untuk menghitung korelasi.")
    
    if (sd(df$CurahHujan, na.rm = TRUE) == 0 || sd(df$Produksi, na.rm = TRUE) == 0) {
      return("Korelasi tidak dapat dihitung karena variabel konstan atau tidak bervariasi.")
    }
    
    cor_hujan <- cor(df$CurahHujan, df$Produksi, use = "complete.obs")
    
    if (is.na(cor_hujan)) return("Tidak dapat dihitung karena data tidak cukup.")
    
    hubungan <- ifelse(cor_hujan > 0, "positif", "negatif")
    kekuatan <- case_when(
      abs(cor_hujan) >= 0.7 ~ "kuat",
      abs(cor_hujan) >= 0.4 ~ "sedang",
      TRUE ~ "lemah"
    )
    
    paste0(
      "Terdapat hubungan ", hubungan, " yang ", kekuatan,
      " antara curah hujan dan produksi komoditas. Nilai korelasi: ",
      round(cor_hujan, 2), "."
    )
  })
  
  output$analisis_tahun_ui <- renderUI({
    df <- gabung_data()
    tahun_min <- min(df$Tahun, na.rm = TRUE)
    tahun_max <- max(df$Tahun, na.rm = TRUE)
    
    sliderInput(
      "analisis_tahun",
      "Rentang Tahun:",
      min = tahun_min,
      max = tahun_max,
      value = c(tahun_min, tahun_max),
      sep = ""
    )
  })
  
  # Update filter choices for Tren Tahunan tab
  observe({
    df <- gabung_data()
    updateSelectInput(
      session,
      "tren_provinsi",
      choices = c("Semua", sort(unique(df$Provinsi)))
    )
    updateSelectInput(
      session,
      "tren_komoditas",
      choices = c("Semua", sort(unique(df$Komoditas)))
    )
  })
  
  tren_data <- reactive({
    df <- gabung_data()

    if (input$tren_provinsi != "Semua") {
      df <- df %>% filter(Provinsi == input$tren_provinsi)
    }    
    if (input$tren_komoditas != "Semua") {
      df <- df %>% filter(Komoditas == input$tren_komoditas)
    }
    df %>%
      group_by(Tahun) %>%
      summarise(Produksi = sum(Produksi, na.rm = TRUE), .groups = "drop")
  })
  
  korelasi_data <- reactive({
    df <- gabung_data()
    if (input$tren_provinsi != "Semua") {
      df <- df %>% filter(Provinsi == input$tren_provinsi)
    }
    if (input$tren_komoditas != "Semua") {
      df <- df %>% filter(Komoditas == input$tren_komoditas)
    }
    df <- df %>%
      group_by(Tahun) %>%
      summarise(
        Produksi = sum(Produksi, na.rm = TRUE),
        Suhu = mean(Suhu, na.rm = TRUE),
        CurahHujan = sum(CurahHujan, na.rm = TRUE)
      )
    df
  })
  
  output$korelasi_output <- renderPrint({
    df <- korelasi_data()
    if (nrow(df) < 3) {
      cat("Data tidak cukup untuk menghitung korelasi.")
      return()
    }
    cor_suhu <- cor(df$Produksi, df$Suhu, use = "complete.obs")
    cor_hujan <- cor(df$Produksi, df$CurahHujan, use = "complete.obs")
    cat("Korelasi Produksi dengan Suhu:", round(cor_suhu, 3), "\n")
    if (abs(cor_suhu) > 0.8) {
      cat("Interpretasi: Korelasi kuat.\n")
    } else if (abs(cor_suhu) > 0.5) {
      cat("Interpretasi: Korelasi sedang.\n")
    } else if (abs(cor_suhu) > 0.1) {
      cat("Interpretasi: Korelasi lemah.\n")
    } else {
      cat("Interpretasi: Cenderung tidak ada korelasi.\n")
    }
    cat("\nKorelasi Produksi dengan Curah Hujan:", round(cor_hujan, 3), "\n")
    if (abs(cor_hujan) > 0.8) {
      cat("Interpretasi: Korelasi kuat.\n")
    } else if (abs(cor_hujan) > 0.5) {
      cat("Interpretasi: Korelasi sedang.\n")
    } else if (abs(cor_hujan) > 0.1) {
      cat("Interpretasi: Korelasi lemah.\n")
    } else {
      cat("Interpretasi: Cenderung tidak ada korelasi.\n")
    }
  })
  
  output$plot_tren_tahunan <- renderPlotly({
    df <- tren_data()
    
    validate(
      need(nrow(df) > 1, "Data tidak cukup untuk membuat grafik tren tahunan.")
    )
    
    p <- ggplot(df, aes(x = Tahun, y = Produksi, group = 1, text = paste("Tahun:", Tahun, "<br>Produksi:", Produksi))) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "blue", size = 2) +
      labs(title = "Tren Produksi Tahunan",
           x = "Tahun",
           y = "Produksi (Ribu Ton)") +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  output$table_frekuensi_kumulatif <- renderDT({
    df <- tren_data() %>%
      arrange(Tahun) %>%
      mutate(Frekuensi = Produksi,
             Frekuensi_Kumulatif = cumsum(Produksi)) %>%
      select(Tahun, Frekuensi, Frekuensi_Kumulatif)
    
    datatable(df, options = list(pageLength = 10, searching = FALSE))
  })
  
  # Update filter choices for Perbandingan Komoditas tab
  observe({
    df <- gabung_data()
    
    updateSelectInput(
      session,
      "perbandingan_provinsi",
      choices = c("Semua", sort(unique(df$Provinsi)))
    )
    
    updateSelectInput(
      session,
      "perbandingan_komoditas",
      choices = c("Semua", sort(unique(df$Komoditas)))
    )
  })
  
  perbandingan_data <- reactive({
    df <- gabung_data()
    if (input$perbandingan_mode == "Jenis") {
      df <- df %>% filter(Tahun == input$perbandingan_tahun)
      df %>%
        group_by(Komoditas) %>%
        summarise(Produksi = sum(Produksi, na.rm = TRUE), .groups = "drop")
    } else {
      if (input$perbandingan_komoditas != "Semua") {
        df <- df %>% filter(Komoditas == input$perbandingan_komoditas)
      }
      df <- df %>% filter(Tahun == input$perbandingan_tahun)
      df %>%
        group_by(Provinsi, Komoditas) %>%
        summarise(Produksi = sum(Produksi, na.rm = TRUE), .groups = "drop")
    }
  })
  
  output$plot_perbandingan_komoditas <- renderPlotly({
    df <- perbandingan_data()
    validate(
      need(nrow(df) > 0, "Data tidak cukup untuk membuat grafik perbandingan komoditas.")
    )
    if (input$perbandingan_mode == "Jenis") {
      p <- ggplot(df, aes(x = reorder(Komoditas, Produksi), y = Produksi, fill = Komoditas,
                          text = paste("Komoditas:", Komoditas, "<br>Produksi:", Produksi))) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = paste("Perbandingan Produksi Komoditas Tahun", input$perbandingan_tahun),
             x = "Komoditas",
             y = "Produksi (Ribu Ton)") +
        theme_minimal() +
        theme(legend.position = "none")
    } else {
      p <- ggplot(df, aes(x = Provinsi, y = Produksi, fill = Komoditas,
                          text = paste("Wilayah:", Provinsi, "<br>Komoditas:", Komoditas, "<br>Produksi:", Produksi))) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = paste("Perbandingan Produksi Komoditas per Wilayah Tahun", input$perbandingan_tahun),
             x = "Wilayah",
             y = "Produksi (Ribu Ton)") +
        theme_minimal()
    }
    ggplotly(p, tooltip = "text")
  })
  
  observeEvent(event_data("plotly_click", source = "tren"), {
    click_data <- event_data("plotly_click", source = "tren")
    if (is.null(click_data)) {
      output$info_tren_tahunan <- renderText({ "" })
    } else {
      tahun_clicked <- click_data$x
      produksi_clicked <- click_data$y
      output$info_tren_tahunan <- renderText({
        paste("Tahun:", tahun_clicked, "\nProduksi:", produksi_clicked)
      })
    }
  })
  
  observeEvent(event_data("plotly_click", source = "perbandingan"), {
    click_data <- event_data("plotly_click", source = "perbandingan")
    if (is.null(click_data)) {
      output$info_perbandingan_komoditas <- renderText({ "" })
    } else {
      komoditas_clicked <- click_data$x
      produksi_clicked <- click_data$y
      tahun_selected <- input$perbandingan_tahun
      if (input$perbandingan_mode == "Jenis") {
        output$info_perbandingan_komoditas <- renderText({
          paste("Komoditas:", komoditas_clicked, "\nTahun:", tahun_selected, "\nProduksi:", produksi_clicked)
        })
      } else {
        wilayah_clicked <- click_data$pointNumber + 1
        df <- perbandingan_data()
        wilayah_name <- unique(df$Provinsi)[wilayah_clicked]
        output$info_perbandingan_komoditas <- renderText({
          paste("Komoditas:", komoditas_clicked, "\nWilayah:", wilayah_name, "\nTahun:", tahun_selected, "\nProduksi:", produksi_clicked)
        })
      }
    }
  })
  observe({
    df <- gabung_data()
    updateSelectInput(session, "peta_komoditas",
                      choices = c("Semua", sort(unique(df$Komoditas))))
  })
  output$peta_produksi <- renderLeaflet({
    df <- gabung_data()
    tahun <- input$peta_tahun
    jenis <- input$peta_jenis
    komoditas_input <- input$peta_komoditas
    
    df <- df %>% filter(Tahun == tahun)
    
    if (jenis == "Produksi") {
      if (komoditas_input != "Semua") {
        df <- df %>% filter(Komoditas == komoditas_input)
      }
    }
    
    df_agg <- df %>%
      group_by(Provinsi) %>%
      summarise(
        Produksi = sum(Produksi, na.rm = TRUE),
        Suhu = mean(Suhu, na.rm = TRUE),
        CurahHujan = sum(CurahHujan, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Provinsi_Join = case_when(
        Provinsi == "Aceh" ~ "DI. ACEH",
        Provinsi == "Sumatera Utara" ~ "SUMATERA UTARA",
        Provinsi == "Sumatera Barat" ~ "SUMATERA BARAT",
        Provinsi == "Riau" ~ "RIAU",
        Provinsi == "Jambi" ~ "JAMBI",
        Provinsi == "Sumatera Selatan" ~ "SUMATERA SELATAN",
        Provinsi == "Bengkulu" ~ "BENGKULU",
        Provinsi == "Lampung" ~ "LAMPUNG",
        Provinsi == "Kepulauan Riau" ~ "KEPULAUAN RIAU",
        Provinsi == "Kepulauan Bangka Belitung" ~ "BANGKA BELITUNG",
        TRUE ~ Provinsi
      ))
    
    provinsi_sumatera <- c(
      "Aceh", "Sumatera Utara", "Sumatera Barat", "Riau", "Kepulauan Riau",
      "Jambi", "Bengkulu", "Sumatera Selatan", "Kepulauan Bangka Belitung", "Lampung"
    )
    
    shp <- shape_sumatera() %>%
      mutate(Propinsi = trimws(Propinsi)) %>%
      mutate(Propinsi = recode(Propinsi,
                               "DI. ACEH" = "Aceh",
                               "SUMATERA UTARA" = "Sumatera Utara",
                               "SUMATERA BARAT" = "Sumatera Barat",
                               "RIAU" = "Riau",
                               "JAMBI" = "Jambi",
                               "SUMATERA SELATAN" = "Sumatera Selatan",
                               "BENGKULU" = "Bengkulu",
                               "LAMPUNG" = "Lampung",
                               "KEPULAUAN RIAU" = "Kepulauan Riau",
                               "BANGKA BELITUNG" = "Kepulauan Bangka Belitung"
      )) %>%
      filter(Propinsi %in% provinsi_sumatera) %>%  # <- Tambahkan ini!
      left_join(df_agg, by = c("Propinsi" = "Provinsi"))
    
    
    layer_value <- if (jenis == "Produksi") {
      df_agg$Produksi
    } else if (komoditas_input == "Suhu") {
      df_agg$Suhu
    } else if (komoditas_input == "CurahHujan") {
      df_agg$CurahHujan
    } else {
      rep(NA_real_, nrow(shp))
    }
    
    # VALIDASI
    validate(
      need(!is.null(layer_value) && any(!is.na(layer_value)), "Data untuk peta tidak tersedia.")
    )
    if (all(is.na(layer_value))) {
      showNotification("Data tidak tersedia atau kosong untuk pilihan ini.", type = "error")
      return(NULL)
    }
    
    pal <- colorNumeric("YlOrRd", domain = layer_value, na.color = "transparent")
    
    label_unit <- case_when(
      jenis == "Produksi" ~ " Ribu Ton",
      komoditas_input == "Suhu" ~ " °C",
      komoditas_input == "CurahHujan" ~ " mm",
      TRUE ~ ""
    )
    
    label_popup <- paste0(shp$Propinsi, ": ", round(layer_value, 2), label_unit)
    
    
    leaflet(shp) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(layer_value),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = label_popup
      ) %>%
      addLegend(
        pal = pal,
        values = layer_value,
        opacity = 0.7,
        position = "bottomright",
        title = if (jenis == "Produksi") "Produksi (ton)" 
        else if (komoditas_input == "Suhu") "Suhu (°C)" 
        else "Curah Hujan (mm)"
      ) %>%
      fitBounds(lng1 = 95, lat1 = -6.5, lng2 = 106, lat2 = 6)
  })
  output$peta_jenis_ui <- renderUI({
    selectInput("peta_jenis", "Jenis Peta:",
                choices = c("Produksi", "Iklim"),
                selected = "Produksi")
  })
  output$peta_detail_ui <- renderUI({
    if (input$peta_jenis == "Produksi") {
      df <- gabung_data()
      komoditas <- sort(unique(df$Komoditas))
      selectInput("peta_komoditas", "Pilih Komoditas:",
                  choices = c("Semua", komoditas),
                  selected = "Semua")
    } else if (input$peta_jenis == "Iklim") {
      selectInput("peta_komoditas", "Pilih Jenis Iklim:",
                  choices = c("Suhu" = "Suhu", "Curah Hujan" = "CurahHujan"),
                  selected = "Suhu")
    }
  })
  
  output$download_produksi <- downloadHandler(
    filename = function() {
      paste("Data_Produksi_Sumatra.csv")
    },
    content = function(file) {
      write.csv(produksi_data(), file, row.names = FALSE)
    }
  )
  
  output$download_iklim <- downloadHandler(
    filename = function() {
      paste("Data_Iklim_Sumatra.csv")
    },
    content = function(file) {
      write.csv(iklim_data(), file, row.names = FALSE)
    }
  )
  
  output$download_gabungan <- downloadHandler(
    filename = function() {
      paste("Data_Gabungan_Sumatra.csv")
    },
    content = function(file) {
      write.csv(gabung_data(), file, row.names = FALSE)
    })
  
output$tabel_metadata_produksi <- renderDT({
  df <- produksi_data()
  metadata <- data.frame(
    Label = names(df),
    Tipe_Data = sapply(df, function(x) class(x)[1]),
    Jumlah = sapply(df, function(x) length(unique(x))),
    Data_Kosong = sapply(df, function(x) sum(is.na(x))),
    Contoh_Nilai = sapply(df, function(x) paste(head(unique(x), 3), collapse = ", "))
  )
  datatable(metadata, options = list(pageLength = 10, scrollX = TRUE))
})

output$tabel_metadata_iklim <- renderDT({
  df <- iklim_data()
  metadata <- data.frame(
    Label = names(df),
    Tipe_Data = sapply(df, function(x) class(x)[1]),
    Jumlah = sapply(df, function(x) length(unique(x))),
    Data_Kosong = sapply(df, function(x) sum(is.na(x))),
    Contoh_Nilai = sapply(df, function(x) paste(head(unique(x), 3), collapse = ", "))
  )
  datatable(metadata, options = list(pageLength = 10, scrollX = TRUE))
})
}
shinyApp(ui, server)