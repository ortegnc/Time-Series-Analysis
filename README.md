# Time-Series-Analysis

# 📊 Zaman Serisi Analizi - Londra'da Satılan Ev Sayıları

Bu projede, **1995-2010 yılları arasındaki Londra'da satılan ev sayıları** üzerine zaman serisi analizi gerçekleştirilmiştir.

## 📌 Veri Kümesi
- **192 gözlemden oluşan aylık veri** kullanılmıştır.
- **Trend ve mevsimsellik** etkilerini gidermek için **iki kez fark alma işlemi** uygulanmıştır.
- **ACF & PACF grafikleri** ile durağanlık kontrolü yapılmıştır.

## 🔍 Ayrıştırma Yöntemleri
- **Toplamsal ayrıştırma modeli**: Trend, mevsimsellik ve hata bileşeni içeren bir modeldir.
- **Çarpımsal ayrıştırma modeli**: Bileşenler çarpımsal olarak modellenmiştir.
- **Her iki modelin de artıkların beyaz gürültü olmaması** nedeniyle uygun olmadığı sonucuna varılmıştır.

## 📈 Regresyon Analizi
- **Toplamsal ve çarpımsal regresyon modelleri** kullanılmıştır.
- Katsayıların **istatistiksel olarak anlamsız çıkması nedeniyle** bu modeller uygun bulunmamıştır.

## 📉 Üstel Düzleştirme Yöntemi (Holt-Winters)
- **Toplamsal Winters modeli** (%39 MAPE) ve **Çarpımsal Winters modeli** (%40 MAPE) test edilmiştir.
- **Her iki modelin de tahmin performansı düşük** bulunmuştur.

## ⚙️ Box-Jenkins (SARIMA) Modelleri
- **(3,1,0)(2,1,0)[12] modeli** en uygun model olarak seçilmiştir (BIC = 1504.8).
- **Artıkların beyaz gürültü olması**, modelin uygunluğunu doğrulamaktadır.
- **Bu model kullanılarak 5 dönemlik tahminler üretilmiştir.**

## 🔮 Sonuçlar & Çıkarımlar
- **SARIMA modeli**, diğer yöntemlerden daha başarılı bulunmuştur.
- **Winters ve regresyon modelleri**, mevsimsellik ve trend etkilerini iyi yakalayamamıştır.
- **Gelecek tahminlerde SARIMA modeli** kullanılabilir.

## 📂 Kodlar & Kullanım
Bu projeyi çalıştırmak için aşağıdaki Python/R paketlerine ihtiyacınız vardır:

```r
install.packages("fpp")
install.packages("forecast")
library(fpp)
library(forecast)
```

Detaylı analiz ve kodlar için proje dosyalarını inceleyebilirsiniz.

---

📌 **Geliştiren:** Onur Günenç  
📅 **Tarih:** Mart 2025  

