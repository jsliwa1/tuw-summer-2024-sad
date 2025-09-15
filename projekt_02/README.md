# Statystyka w Analizie Danych – Projekt 2

**Autorzy:** 
- Jakub Śliwa
- Jakub Smela

## Opis projektu
Projekt obejmuje trzy główne zagadnienia z zakresu statystyki i analizy danych:

### Problem 1 – Porównanie rozkładów
- Zastosowano test t-Studenta oraz test Wilcoxona do porównania dwóch rozkładów.  
- Badano moc testów w zależności od przesunięcia wartości oczekiwanej (Δ) i odchyleń standardowych.  
- Wnioski:  
  - Test t-Studenta lepiej wykrywa różnice przy mniejszych przesunięciach.  
  - Wilcoxon jest bardziej odporny na odstępstwa od normalności.  
  - Różnice w odchyleniach standardowych są lepiej wykrywane przez test t-Studenta.  

### Problem 2 – Inflacja studencka vs oficjalna
- Porównano miesięczne dane inflacji studenckiej (IX 2022 – III 2024) z oficjalną.  
- Analiza wykresów pudełkowych i różnic wskazała, że inflacja studencka jest wyższa.  
- Test Wilcoxona dla par potwierdził istotną różnicę (p-value ≪ 0.05).  
- Wnioski: studenci doświadczają wyższej inflacji, szczególnie ze względu na wyższe ceny dóbr podstawowych.  

### Problem 3 – Asymptotyczna normalność estymatora MNW
- Zbadano zachowanie estymatora największej wiarygodności (MNW) dla rozkładu Bernoulliego z parametrem p=0.7.  
- Dla liczebności prób 10, 1000 i 100 000 przeprowadzono symulacje i porównano rozkłady estymatora.  
- Wyniki: wraz ze wzrostem próby estymator MNW zbiega do rozkładu normalnego.  

## Wnioski końcowe
- Testy statystyczne różnią się czułością i odpornością w zależności od założeń.  
- Inflacja studencka istotnie przewyższa inflację oficjalną w badanym okresie.  
- Estymator MNW jest asymptotycznie normalny, co potwierdzają symulacje.

W ramach pracy nad projektem odpowiedzialny byłem za rozwiązanie problemów 2 i 3.
