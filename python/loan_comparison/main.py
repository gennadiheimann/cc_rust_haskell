from configuration import ConfigReader
from bauspar import Bauspar
from loan import Loan
from export import Exporter

config = ConfigReader("config_v2.toml")

#==Tilgungsfreies Darlehen==

loan = Loan(config)
(interest_only_mortgage_interest_total, interest_only_mortgage_loan_payment_monthly) = \
  loan.calc_interest_only_mortage()

#loan.print_interest_only_mortgage(
#  interest_only_mortgage_interest_total, 
#  interest_only_mortgage_loan_payment_monthly)

"""
Verlängerung kann nur berechnet werden wenn Sparphase beim Bausparvertrag
länger als duration_in_months bei tilgungsfreien Darlehn ist
"""
# ==Tilgungsfreies Darlehen Verlängerung==
loan.calc_interest_only_mortage_extension()
#loan.print_interest_only_mortage_extension()

# ==Sparphase==
# Zusätzliche Raten Zwischenfinnanzieren?
# Monatliche Rate automatisch für erreicheung der Bewertungszahl ausrechnen
# Spahrphase soll die Dauer solange berechen bis die Bewertungszahl erreicht wird
# BSV Spahrphase mit allen Parametern berechnen und dann Kostenloses Darlehen.
# Optionen für Parametern
# Zusätzliche Ansparrate (wiederholbar)
# Zwischenfinanzierung für Guthaben, damit der BSV schneller geteilt werden kann

bauspar = Bauspar(config)
(total_interest, total_saving_amount, months, evaluation_score) = \
  bauspar.calc_bsv_saving_time_to_reach_evaluation_score()

bauspar.print_bsv_saving_time_to_reach_evaluation_score(months, total_saving_amount, evaluation_score)

bauspar.calc_bsv_saving_with_additional_payment()
bauspar.print_bsv_seving_with_additional_payment()

# ==Zwischenfinanzierung für Teilung==
(is_interim_loan_exist, interim_loan, interim_loan_monthly_paymant, interim_loan_total_interest) = \
  bauspar.calc_interim_loan()
if is_interim_loan_exist:
   bauspar.print_interim_loan(interim_loan, interim_loan_monthly_paymant, interim_loan_total_interest)

# Berechnung wenn Bausparsumme kleiner als Darlehen
if config.interest_only_mortgage_loan_amount > config.bsv_amount :
    print(f"Rest von Darlehensumme : {config.interest_only_mortgage_loan_amount - config.bsv_amount:.2f} EUR")
    
    rest_from_loan_payment_monthly_rate = \
      loan.calculate_monthly_rate(
         config.interest_only_mortgage_loan_amount - config.bsv_amount
      )

    print(f"Monatliche Rate {rest_from_loan_payment_monthly_rate:.2f}")

    total_interest_for_rest_from_loan = \
      loan.calculate_total_interest((config.interest_only_mortgage_loan_amount - config.bsv_amount),
                                     rest_from_loan_payment_monthly_rate)
    print(f"Gesamtzins: {total_interest_for_rest_from_loan:.2f} EUR\n")

# ==Darlehensphase==
(bsv_loan_amount, bsv_loan_total_month, bsv_loan_total_interest) = bauspar.calc_loan_phase()
bauspar.print_loan_phase(bsv_loan_amount, bsv_loan_total_month, bsv_loan_total_interest)

gesamtbelastung_0_10 = config.bsv_saving_rate + interest_only_mortgage_loan_payment_monthly
print(f"Gesamtbelastung (monatlich) für 0-10 Jahre: {gesamtbelastung_0_10:.2f} EUR\n")
if loan.interest_only_mortgage_extension_loan_payment_monthly != 0.0:
  gesamtbelastung_11_15 = config.bsv_saving_rate + loan.interest_only_mortgage_extension_loan_payment_monthly
  print(f" Zusätzliche Gesamtbelastung (monatlich) für 11-20 Jahre: {gesamtbelastung_11_15} EUR , für {config.bsv_saving_time_mounthly - config.interest_only_mortgage_duration_in_months} Moante\n")

print(f"Gesamtbelastung (monatlich) für 11-20 Jahre: {(config.bsv_amount * config.interest_prinipal_paymants_mounthly) + interim_loan_monthly_paymant:.2f} EUR\n")

print(f"Gesamtbelastung (monatlich) für 21-30 Jahre: {(config.bsv_amount * config.interest_prinipal_paymants_mounthly):.2f} EUR\n")

interest_total_for_all_time = \
  interest_only_mortgage_interest_total  + loan.interest_only_mortgage_extension_interest_total + interim_loan_monthly_paymant + bsv_loan_total_interest

print(f"Zinsen über gesamte Lufzeit: {(interest_total_for_all_time):.2f} EUR\n")

print(f"Berechnung der Darlehen ohne BSV, Darlehen: {config.bsv_amount:.2f} EUR")
print(f"Überneheme die Monatsraten aus dem Finanzierung mit BSV")
# print(f"Monatsrate 0-10 Jahre: {gesamtbelastung_0_10:.2f} EUR")

gesamtbelastung_11_20 = (config.bsv_amount * config.interest_prinipal_paymants_mounthly) + interim_loan_monthly_paymant
gesamtbelastung_21_30 = config.bsv_amount * config.interest_prinipal_paymants_mounthly

darlehens_ohne_bsv = [
    (gesamtbelastung_0_10, config.darlehen_ohne_bsv_laufzeit_0_10, config.darlehen_ohne_bsv_zins_prognose_0_10),
    (gesamtbelastung_11_20, config.darlehen_ohne_bsv_laufzeit_11_20, config.darlehen_ohne_bsv_zins_prognose_11_20), 
    (gesamtbelastung_21_30, config.darlehen_ohne_bsv_laufzeit_21_30, config.darlehen_ohne_bsv_zins_prognose_21_30),
    (-1, config.darlehen_ohne_bsv_laufzeit_31_40, config.darlehen_ohne_bsv_zins_prognose_31_40)
]

restschuld_gesamtzinsen = loan.calculate_remaining_debt_and_total_interest(config.bsv_amount, darlehens_ohne_bsv, print_on = False)
for (restschuld, gesamtzinsen_periode, gesamtzinsen) in restschuld_gesamtzinsen:
    print(f"Restschuld: {restschuld:.2f} EUR und gezahlte Zinsen in Periode: {gesamtzinsen_periode:.2f} EUR, Gesamtzinsen: {gesamtzinsen:.2f} EUR\n")

# Export der Daten
"""
export_data = {
    f"Darlehenssumme": f"{config.bsv_amount:.2f}",
    f"Tilgungsfreie Darlehen: Gesamte Zinsen für {config.bsv_saving_time} Monate": f"{interest_only_mortgage_interest_total:.2f}",
    f"Tilgungsfreie Darlehen: Monatliche Rate in EUR": f"{loan.interest_only_mortgage_loan_payment_monthly:.2f}",
    f"Ansparenphase: Gesamte Ansparung für {config.bsv_amount:.2f} EUR in {config.bsv_saving_time} Monaten": f"{bauspar.saving_amount_with_additional_payment:.2f}",
    f"Ansparenphase: Ansparrate pro Monat in EUR": f"{config.ansparen_rate:.2f}",
    f"Ansparenphase: Rest zu finanzieren in EUR": f"{(config.bsv_amount * config.bsv_minimum_saving_amount) - bauspar.saving_amount_with_additional_payment:.2f}",
    f"Zwischenfinanzierung: Monatliche Rate in EUR": f"{interim_loan_monthly_paymant:.2f}",
    f"Zwischenfinanzierung: Gesamtzinsen in EUR": f"{interim_loan_total_interest:.2f}",
    f"Darlehenphase: Dallehensumme in EUR": f"{bsv_loan_amount:.2f}",
    f"Darlehenphase: Monatliche Rate in EUR": f"{bsv_loan_total_month:.2f}",
    f"Darlehenphase: Gesamte Zinsen in EUR": f"{bsv_loan_total_interest:.2f}",
    f"Gesamtbelastung Ansparphase in EUR": f"{gesamtbelastung_0_10:.2f}",
    f"Gesamtbelastung Darlehenphase mit Zwischenfinazierung in EUR": f"{gesamtbelastung_11_20:.2f}",
    f"Gesamtbelastung Darlehenphase in EUR": f"{gesamtbelastung_21_30:.2f}",
    f"Gesamtzinsen über gesamte Lufzeit in EUR": f"{(loan.interest_only_mortgage_interest_total + interim_loan_total_interest + bsv_loan_total_interest):.2f}",
    f"Darlehen ohne BSV: Darlehen in EUR": f"{gesamtbelastung_0_10:.2f}",
}

exporter = Exporter(export_data)
exporter.export_to_csv("export.csv")
"""