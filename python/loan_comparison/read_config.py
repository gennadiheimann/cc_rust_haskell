import tomllib 

with open("config.toml", "rb") as f:
    config = tomllib.load(f)

# print(config["title"])
# print(config["bsv"]["bs_sum"])
# print(config["bsv"]["deposit_rates_decimal"])
# print(config["bsv"]["minimum_saving_amount_decimal"])
# print(config["bsv"]["acquisition_fee_decimal"])
# print(config["bsv"]["saving_time"])
# print(config["bsv"]["saving_rate"])

#==============================================================================
# Read Configs
#==============================================================================

bs_summe = float(config["bsv"]["bs_sum"])
guthaben_zins = float(config["bsv"]["deposit_rates_decimal"])
minsest_ansparung = float(config["bsv"]["minimum_saving_amount_decimal"])
abschlus_gebuehr = float(config["bsv"]["acquisition_fee_decimal"])
ansparzeit = int(config["bsv"]["saving_time"])
ansparrate = float(config["bsv"]["saving_rate"])

def calc_saving_amount(ansparzeit, ansparrate, bs_summe, guthaben_zins, abschlus_gebuehr):
    monatszins = guthaben_zins / 12
    kapital = 0.0
    for _ in range(ansparzeit):
        kapital = kapital * (1 + monatszins) + ansparrate
    return kapital - (bs_summe * abschlus_gebuehr)

# Guthaben nach Ansparzeit
kapital = calc_saving_amount(
    ansparzeit=ansparzeit, 
    ansparrate=ansparrate, 
    bs_summe=bs_summe, 
    guthaben_zins=guthaben_zins, 
    abschlus_gebuehr=abschlus_gebuehr)

# Finazierung für die Teilung
rest_zu_finanzieren = (bs_summe * minsest_ansparung) - kapital

print(f"Kapital: {kapital:.2f}")
print(f"Rest zu Finanzieren: {rest_zu_finanzieren:.2f}")

#=======================================================================================
# Kredit
#=======================================================================================

# Pramietern für Kredit für Teilung
# Zins

# Rest zu Finanzieren 

def berechne_kredit(kreditsumme, zinssatz_jahr, laufzeit_jahre):
    i = zinssatz_jahr / 100 / 12        # monatlicher Zinssatz
    n = laufzeit_jahre * 12             # Anzahl Monate

    # Annuität (monatliche Rate)
    rate = kreditsumme * (i * (1 + i) ** n) / ((1 + i) ** n - 1)

    restschuld = kreditsumme

    print(f"Monatliche Rate: {rate:.2f} €\n")
    print("Monat | Rate   | Zinsen | Tilgung | Restschuld")
    
    zinsen_gesamt_zeit = 0.0
    
    for monat in range(1, n + 1):
        zinsen = restschuld * i
        tilgung = rate - zinsen
        restschuld -= tilgung
        zinsen_gesamt_zeit += zinsen

        print(f"{monat:5d} | {rate:7.2f} | {zinsen:7.2f} | {tilgung:8.2f} | {restschuld:10.2f}")

    print(f"Zinsen über gesamte Zeit: {zinsen_gesamt_zeit:.2f}")

berechne_kredit(kreditsumme=rest_zu_finanzieren, zinssatz_jahr=6.0, laufzeit_jahre=int(ansparzeit / 12)) 

#==============================================================================
# Darlehenphase
#==============================================================================

#========================================================================================
# export
#========================================================================================
"""
import csv

daten = [
    {
        "Bausparsumme": bs_summe, 
        "Ansparungszeit in Monaten": ansparzeit, 
        "Ansparbetrag - Anschlussgebühr" : kapital_mit_abschlussgebuer},
]

with open("export.csv", mode="w", newline="", encoding="utf-8") as file:
    # fieldnames = ["Vorname", "Nachname", "Alter"]
    # writer = csv.DictWriter(file, fieldnames=fieldnames)
    # writer.writeheader()
    # writer.writerows(daten)
    
    writer = csv.writer(file)
    # Kopfzeile
    #writer.writerow(["Vorname", "Nachname", "Alter"])

    # Daten
    writer.writerow(["Bausparsumme", bs_summe])
    writer.writerow(["Ansparungszeit in Monaten", ansparzeit])
    writer.writerow(["Ansparbetrag - Anschlussgebühr", kapital_mit_abschlussgebuer])
""" 
