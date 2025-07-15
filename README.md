# Sumatra Agroclimate Dashboard

## Deskripsi
Dashboard ini dibuat menggunakan **R Shiny** untuk memvisualisasikan data produksi komoditas perkebunan dan data iklim (suhu dan curah hujan) di wilayah **Sumatera** pada rentang waktu **2017–2023**. Dashboard ini memuat analisis visual serta regresi antara faktor iklim dengan produksi perkebunan.

## Struktur Menu
1. **Beranda**
   - Ringkasan dashboard, penjelasan fitur, dan video tutorial.
2. **Tren Tahunan**
   - Visualisasi tren produksi per tahun dengan filter komoditas dan provinsi.
   - Korelasi suhu dan curah hujan terhadap produksi.
3. **Perbandingan Komoditas**
   - Perbandingan produksi antar komoditas atau antar wilayah.
4. **Analisis Hubungan**
   - Analisis regresi linear antara suhu, curah hujan, dan produksi.
   - Scatter plot dan interpretasi hasil analisis.
5. **Peta**
   - Peta interaktif produksi, suhu, dan curah hujan berdasarkan provinsi.
6. **Tabel Data**
   - Tabel untuk data produksi, iklim, dan gabungan, serta fitur unduh.
7. **Metadata**
   - Metadata untuk semua variabel dalam data produksi dan iklim.

## Data Input
- `Perkebunan_Sumatra.xlsx`: data produksi perkebunan.
- `Iklim_Sumatra.xlsx`: data suhu dan curah hujan per bulan.
- `indonesia-prov.geojson`: data batas wilayah provinsi untuk peta.

## Library yang Digunakan
- shiny  
- shinydashboard  
- readxl  
- DT  
- dplyr  
- ggplot2  
- plotly  
- leaflet  
- sf  

## Fitur Khusus
- Filter interaktif provinsi, komoditas, dan tahun.
- Korelasi otomatis dan interpretasi korelasi.
- Regresi linear dengan output interpretatif.
- Peta berwarna berdasarkan nilai numerik (produksi/suhu/hujan).
- Unduhan data .csv untuk semua tabel.
- Metadata otomatis dari struktur data.

## Petunjuk Penggunaan
1. Jalankan aplikasi dengan `shiny::runApp()`.
2. Pastikan file Excel dan geojson tersedia di direktori kerja.
3. Gunakan menu samping untuk menjelajahi dashboard.

## Catatan Pengembangan
- Pastikan format nama provinsi pada shapefile sesuai dengan nama pada data.
- Validasi data sangat penting untuk setiap input user.
- Dapat dikembangkan lebih lanjut untuk fitur prediksi atau penambahan jenis analisis iklim.

---

**Pembuat**: AMIR,ALIFIA,FERDI  
**Tanggal**: Juli 2025
