from configuration import ConfigReader
from bauspar import Bauspar
from loan import Loan

config = ConfigReader("config.toml")

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
(darlehen_gesamt_monate, darlehen_gesamt_zinsen) = loan.calculate_total_interest_with_fix_payment(darlehen_bausparen)
print(f"Darlehen: Gesamte Zinsen für {darlehen_gesamt_monate} Monate: {darlehen_gesamt_zinsen:.2f} EUR\n")

gesamtbelastung_0_10 = config.ansparen_rate + tilgungsfreie_darlehen_rate_monat
print(f"Gesamtbelastung (monatlich) für 0-10 Jahre: {gesamtbelastung_0_10:.2f} EUR\n")

print(f"Gesamtbelastung (monatlich) für 11-20 Jahre: {(config.bausparsumme * config.darlehen_rate) + zwischenfinanzierung_rate_monat:.2f} EUR\n")

print(f"Gesamtbelastung (monatlich) für 21-30 Jahre: {(config.bausparsumme * config.darlehen_rate):.2f} EUR\n")

print(f"Zinsen über gesamte Lufzeit: {(tilgungsfreie_darlehen_zins_gesamt + zwischenfinanzierung_zinsen_gesamt + darlehen_gesamt_zinsen):.2f} EUR\n")



