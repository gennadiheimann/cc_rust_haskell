from configuration import ConfigReader
from bauspar import Bauspar
from loan import Loan

config = ConfigReader("config_v2.toml")

print(f"Tilgungsfreie Finanzierung")
tilgungsfreie_darlehen_zins_gesamt = config.bausparsumme * config.tilgungsfreie_darlehen_zins * config.ansparen_zeit / 12
print(f"Tilgungsfreie Darlehen: Gesamte Zinsen für {config.ansparen_zeit} Monate: {tilgungsfreie_darlehen_zins_gesamt:.2f} EUR")
tilgungsfreie_darlehen_rate_monat = tilgungsfreie_darlehen_zins_gesamt / config.ansparen_zeit
print(f"Tilgungsfreie Darlehen: Monatliche Rate: {tilgungsfreie_darlehen_rate_monat:.2f} EUR\n")


bauspar = Bauspar(config)
saving_amount = bauspar.calculate_saving_amount()
print(f"Gesamte Ansparung für {config.bausparsumme} EUR in {config.ansparen_zeit} Monaten: {saving_amount:.2f} EUR")
print(f"Ansparrate: {config.ansparen_rate:.2f} EUR pro Monat\n")

print(f"Rest zu finanzieren: {(config.bausparsumme * config.mindest_ansparung) - saving_amount:.2f} EUR")
loan = Loan(config)
zwischenfinanzierung_rate_monat = loan.calculate_monthly_rate((config.bausparsumme * config.mindest_ansparung) - saving_amount)
print(f"Restfinanzierung für {config.ansparen_zeit} Monaten: {zwischenfinanzierung_rate_monat:.2f} EUR pro Monat")
zwischenfinanzierung_zinsen_gesamt = loan.calculate_total_interest((config.bausparsumme * config.mindest_ansparung) - saving_amount, zwischenfinanzierung_rate_monat)
print(f"Zinsen für Restfinanzierung über {config.ansparen_zeit} Monate: {zwischenfinanzierung_zinsen_gesamt:.2f} EUR\n")

darlehen_bausparen = config.bausparsumme - (config.bausparsumme * config.mindest_ansparung)
print(f"Darlehen nache dem Ansparen: {darlehen_bausparen:.2f} EUR")
print(f"Darlehensrate: {config.bausparsumme * config.darlehen_rate:.2f} EUR pro Monat")
darlehen_bausparen_rate_monat = config.bausparsumme * config.darlehen_rate
(darlehen_gesamt_monate, darlehen_gesamt_zinsen) = loan.calculate_total_interest_with_fix_payment(darlehen_bausparen, darlehen_bausparen_rate_monat)
print(f"Darlehen: Gesamte Zinsen für {darlehen_gesamt_monate} Monate: {darlehen_gesamt_zinsen:.2f} EUR\n")

gesamtbelastung_0_10 = config.ansparen_rate + tilgungsfreie_darlehen_rate_monat
print(f"Gesamtbelastung (monatlich) für 0-10 Jahre: {gesamtbelastung_0_10:.2f} EUR\n")

print(f"Gesamtbelastung (monatlich) für 11-20 Jahre: {(config.bausparsumme * config.darlehen_rate) + zwischenfinanzierung_rate_monat:.2f} EUR\n")

print(f"Gesamtbelastung (monatlich) für 21-30 Jahre: {(config.bausparsumme * config.darlehen_rate):.2f} EUR\n")

print(f"Zinsen über gesamte Lufzeit: {(tilgungsfreie_darlehen_zins_gesamt + zwischenfinanzierung_zinsen_gesamt + darlehen_gesamt_zinsen):.2f} EUR\n")

print(f"Berechnung der Darlehen ohne BSV, Darlehen: {config.bausparsumme:.2f} EUR")
print(f"Überneheme die Monatsraten aus dem Finanzierung mit BSV")
# print(f"Monatsrate 0-10 Jahre: {gesamtbelastung_0_10:.2f} EUR")

gesamtbelastung_11_20 = (config.bausparsumme * config.darlehen_rate) + zwischenfinanzierung_rate_monat
gesamtbelastung_21_30 = config.bausparsumme * config.darlehen_rate

darlehens_ohne_bsv = [
    (gesamtbelastung_0_10, config.darlehen_ohne_bsv_laufzeit_0_10, config.darlehen_ohne_bsv_zins_prognose_0_10),
    (gesamtbelastung_11_20, config.darlehen_ohne_bsv_laufzeit_11_20, config.darlehen_ohne_bsv_zins_prognose_11_20), 
    (gesamtbelastung_21_30, config.darlehen_ohne_bsv_laufzeit_21_30, config.darlehen_ohne_bsv_zins_prognose_21_30),
    (-1, config.darlehen_ohne_bsv_laufzeit_31_40, config.darlehen_ohne_bsv_zins_prognose_31_40)
]

restschuld_gesamtzinsen = loan.calculate_remaining_debt_and_total_interest(config.bausparsumme, darlehens_ohne_bsv, print_on = True)
for (restschuld, gesamtzinsen_periode, gesamtzinsen) in restschuld_gesamtzinsen:
    print(f"Restschuld: {restschuld:.2f} EUR und gezahlte Zinsen in Periode: {gesamtzinsen_periode:.2f} EUR, Gesamtzinsen: {gesamtzinsen:.2f} EUR\n")
# print(f"Restschuld nach 0-10 Jahre: {darlehen_ohne_bsv_0_10_restschuld:.2f} EUR und gezahlte Zinsen: {darlehen_ohne_bsv_0_10_gesamtzinsen:.2f} EUR\n")
