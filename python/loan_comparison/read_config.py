try:
    import tomllib
except ImportError:
    import tomli as tomllib

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
darlehen_zins = float(config["bsv"]["fixed_interest_rate_decimal"])
darlehensrate = float(config["bsv"]["interest_prinipal_paymants_mounthly_decimal"])

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
print(f"########################################################")
print(f"Ansparzeit")
print(f"Kapital: {kapital:.2f}")
print(f"Rest zu Finanzieren: {rest_zu_finanzieren:.2f}")
print(f"########################################################")

#=======================================================================================
# Kredit
#=======================================================================================

# Pramietern für Kredit für Teilung
# Zins

# Rest zu Finanzieren 

print(f"########################################################")
print(f"Rest 10 Jare finanzieren ")

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

        #print(f"{monat:5d} | {rate:7.2f} | {zinsen:7.2f} | {tilgung:8.2f} | {restschuld:10.2f}")

    print(f"Zinsen über gesamte Zeit: {zinsen_gesamt_zeit:.2f}")
    return (rate, zinsen_gesamt_zeit)

(rest_finanzieren_rate, rest_gesamt_zinsen) = berechne_kredit(kreditsumme=rest_zu_finanzieren, zinssatz_jahr=6.0, laufzeit_jahre=int(ansparzeit / 12)) 

print(f"########################################################")

#==============================================================================
# Darlehenphase
#==============================================================================

print(f"########################################################")
import math

def kredit_plan(kreditsumme, zinssatz_jahr, rate):
    i = zinssatz_jahr / 100 / 12  # monatlicher Zinssatz

    # Laufzeit berechnen
    n = math.log(rate / (rate - kreditsumme * i)) / math.log(1 + i)
    monate = math.ceil(n)

    restschuld = kreditsumme

    print(f"Laufzeit: {monate} Monate (~{monate/12:.1f} Jahre)\n")
    #print("Monat | Rate   | Zinsen | Tilgung | Restschuld")
    #print("--------------------------------------------------")

    zinsen_ueber_gesamte_zeit = 0.0
    
    for monat in range(1, monate + 1):
        zinsen = restschuld * i
        zinsen_ueber_gesamte_zeit += zinsen
        tilgung = rate - zinsen
        restschuld -= tilgung

        # Rundungsfehler am Ende korrigieren
        if restschuld < 0:
            tilgung += restschuld
            restschuld = 0

        #print(f"{monat:5d} | {rate:7.2f} | {zinsen:7.2f} | {tilgung:8.2f} | {restschuld:10.2f}")

        if restschuld <= 0:
            break
        
    print(f"Zinsen ueber gesamte Lufzeit: {zinsen_ueber_gesamte_zeit:.2f}")
    return zinsen_ueber_gesamte_zeit


# Beispiel
darlehen_gesamt_zinsen = kredit_plan(
    kreditsumme=bs_summe - (bs_summe * minsest_ansparung),
    zinssatz_jahr= darlehen_zins,     
    rate=bs_summe * darlehensrate
)

print(f"########################################################")

zinsen_belastungfrei_darlehen = bs_summe * 0.045 * ansparzeit / 12
print(f"Belastung Tilgungsfreier Darlehen: {zinsen_belastungfrei_darlehen:.2f}")
belastung_tilgungsfrei_darlehen = bs_summe * 0.045 / 12
print(f"Belastung monatlich: {belastung_tilgungsfrei_darlehen:.2f}")
belastung_0_10 = belastung_tilgungsfrei_darlehen + ansparrate
print(f"Belastung 0-10 Jahre: {belastung_0_10:.2f}")
belastung_11_20 = rest_finanzieren_rate + (bs_summe*darlehensrate)
print(f"Belastung 11-20 Jahre: {belastung_11_20:.2f}")
print(f"Belastung 11-30 Jahre: {bs_summe*darlehensrate}")
gesamt_zinsen = zinsen_belastungfrei_darlehen + rest_gesamt_zinsen + darlehen_gesamt_zinsen
print(f"Geasmt Zinsen: {gesamt_zinsen:.2f}")


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
