from configuration import ConfigReader
import math

class Loan:
    def __init__(self, config: ConfigReader):
        self.config = config
    
    def calculate_monthly_rate(self, loan_sum):
        i = self.config.ratenkredit_zins / 12        # monatlicher Zinssatz
        n = self.config.ansparen_zeit
        monthly_rate = loan_sum * (i * (1 + i) ** n) / ((1 + i) ** n - 1)
        #print(f"Monatliche Rate: {monthly_rate:.2f} €\n")
        return monthly_rate

    def calculate_total_interest(self, loan_sum, loan_payment_monthly):
        total_interest = 0.0

        #print("Monat | Rate   | Zinsen | Tilgung | Restschuld")

        restschuld = loan_sum
        n = self.config.ansparen_zeit
        i = self.config.ratenkredit_zins / 12 

        for monat in range(1, n + 1):
            zinsen = restschuld * i
            tilgung = loan_payment_monthly - zinsen
            restschuld -= tilgung
            total_interest += zinsen
            #print(f"{monat:5d} | {loan_payment_monthly:7.2f} | {zinsen:7.2f} | {tilgung:8.2f} | {restschuld:10.2f}")

        # print(f"Zinsen über gesamte Zeit: {zinsen_gesamt_zeit:.2f}")
        return total_interest

    def calculate_total_interest_with_fix_payment(self, loan_sum, loan_payment_monthly, print_on = False):
        i = self.config.darlehen_zins / 12
        
        # Laufzeit berechnen
        n = math.log(loan_payment_monthly / (loan_payment_monthly - loan_sum * i)) / math.log(1 + i)
        total_month = math.ceil(n)

        restschuld = loan_sum

        print(f"Laufzeit: {total_month} Monate ({int(total_month/12)} Jahre und {total_month%12} Monate)")
        
        if print_on:
            print("Monat | Rate   | Zinsen | Tilgung | Restschuld")
            print("--------------------------------------------------")

        total_interest = 0.0
    
        for monat in range(1, total_month + 1):
            zinsen = restschuld * i
            total_interest += zinsen
            tilgung = loan_payment_monthly - zinsen
            restschuld -= tilgung

            # Rundungsfehler am Ende korrigieren
            if restschuld < 0:
                tilgung += restschuld
                restschuld = 0

            if print_on:
                print(f"{monat:5d} | {loan_payment_monthly:7.2f} | {zinsen:7.2f} | {tilgung:8.2f} | {restschuld:10.2f}")

            if restschuld <= 0:
                break
        
        # print(f"Zinsen ueber gesamte Lufzeit: {total_interest:.2f}")
        return (total_month, total_interest)
    
    def calculate_remaining_debt_and_total_interest(self, loan_sum, loan_without_bsv, print_on = False):
        restschuld_ = loan_sum
        total_interest_ = 0.0
        restschuld_and_total_interest = []
        for loan_payment_monthly, laufzeit, zins in loan_without_bsv:
            if loan_payment_monthly == -1:
                break

            i = zins / 12
            restschuld_tmp = 0.0
            total_interest_tmp = 0.0
            
            if print_on:
                print("Monat | Rate   | Zinsen | Tilgung | Restschuld")
                print("--------------------------------------------------")

            for monat in range(1, laufzeit + 1):
                zinsen = restschuld_ * i
                total_interest_ += zinsen
                tilgung = loan_payment_monthly - zinsen
                restschuld_ -= tilgung
                total_interest_tmp += zinsen

                if restschuld_ < 0:
                    # tilgung += restschuld_
                    restschuld_ += tilgung
                    break

                if print_on:
                    print(f"{monat:5d} | {loan_payment_monthly:7.2f} | {zinsen:7.2f} | {tilgung:8.2f} | {restschuld_:10.2f}")

            if print_on:
                print(f"Restschuld nach {laufzeit} Monaten: {restschuld_:.2f} EUR und gezahlte Zinsen: {total_interest_tmp:.2f} EUR\n")

            # if restschuld_ > 0:
            restschuld_and_total_interest.append((restschuld_, total_interest_tmp, total_interest_))

        return restschuld_and_total_interest
        
        # i = self.config.darlehen_ohne_bsv_zins_prognose_0_10 / 12

        # restschuld = loan_sum
        # total_interest = 0.0
        # if print_on:
        #     print("Monat | Rate   | Zinsen | Tilgung | Restschuld")
        #     print("--------------------------------------------------")

        # for monat in range(1, self.config.darlehen_ohne_bsv_laufzeit_0_10 + 1):
        #     zinsen = restschuld * i
        #     total_interest += zinsen
        #     tilgung = loan_payment_monthly - zinsen
        #     restschuld -= tilgung
        #     if print_on:
        #         print(f"{monat:5d} | {loan_payment_monthly:7.2f} | {zinsen:7.2f} | {tilgung:8.2f} | {restschuld:10.2f}")
