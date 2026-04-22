from configuration import ConfigReader
from bauspar import Bauspar
from loan import Loan

config = ConfigReader("config_only_bsv.toml")

bauspar = Bauspar(config)
bauspar.calc_bsv_saving()
bauspar.print_bsv_seving()

loan = Loan(config)

(total_interest, total_saving_amount) = bauspar.calculate_saving_amount_with_one_time_payment()

print(f"Gasamt Guthaben: {total_saving_amount:.2f}")
print(f"Bewertungszahl: {bauspar.calc_bewertungszahl(total_interest, total_saving_amount):.2f}\n")


darlehen_bausparen = config.bsv_amount - (config.bsv_amount * config.bsv_minimum_saving_amount)
print(f"Darlehen nache dem Ansparen: {darlehen_bausparen:.2f} EUR")
print(f"Darlehensrate: {config.bsv_amount * config.interest_prinipal_paymants_mounthly:.2f} EUR pro Monat")
darlehen_bausparen_rate_monat = config.bsv_amount * config.interest_prinipal_paymants_mounthly
(darlehen_gesamt_monate, darlehen_gesamt_zinsen) = loan.calculate_total_interest_with_fix_payment(darlehen_bausparen, darlehen_bausparen_rate_monat)
print(f"Darlehen: Gesamte Zinsen für {darlehen_gesamt_monate} Monate: {darlehen_gesamt_zinsen:.2f} EUR\n")



