from configuration import ConfigReader


class Bauspar:
    def __init__(self, config: ConfigReader):
        self.config = config
    
    def calculate_saving_amount(self):
        monthly_rate = self.config.bsv_deposit_rates_decimal
        day_factor = 365/360

        total_saving_amount = 0 -  (self.config.bsv_amount * self.config.bsv_acquisition_fee_decimal)
        total_interest = 0

        for _ in range(self.config.bsv_saving_time_mounthly):
            total_saving_amount += self.config.bsv_saving_rate
            if total_saving_amount > 0:
                interest = total_saving_amount * monthly_rate * day_factor / 12
                total_interest += interest
                total_saving_amount += interest
        return (total_interest, total_saving_amount)
    
    def calculate_saving_amount_with_additional_payment(self): 
        monthly_rate = self.config.bsv_deposit_rates_decimal
        day_factor = 365/360

        total_saving_amount = (0 - 
          (self.config.bsv_amount * self.config.bsv_acquisition_fee_decimal)
        )
        total_interest = 0

        for m in range(self.config.bsv_saving_time_mounthly):
            if m in  self.config.bsv_additional_payments.keys():
                total_saving_amount += self.config.bsv_additional_payments[m]
            else: 
              total_saving_amount += self.config.bsv_saving_rate
            if total_saving_amount > 0:
                interest = total_saving_amount * monthly_rate * day_factor / 12
                total_interest += interest
                total_saving_amount += interest
        return (total_interest, total_saving_amount)
    
    def calc_bewertungszahl(self, total_interest, saving_amount):
        return (saving_amount + (total_interest * self.config.zinsfaktor)) / (4 * self.config.bsv_amount / 1000)
    

    def calc_bsv_saving(self):
      (interest_of_saving, saving_amount) = self.calculate_saving_amount()
      self.saving_amount = saving_amount
      self.interest_of_saving = interest_of_saving
      self.rating_nummber = self.calc_bewertungszahl(interest_of_saving, saving_amount)

    def calc_bsv_saving_with_additional_payment(self):
      (interest_of_saving, saving_amount) = self.calculate_saving_amount_with_additional_payment()
      self.saving_amount_with_additional_payment = saving_amount
      self.interest_of_saving_with_additional_payment = interest_of_saving
      self.rating_nummber_with_additional_payment = self.calc_bewertungszahl(interest_of_saving, saving_amount)
    
    def calc_bsv_saving_time_to_reach_evaluation_score(self):
      monthly_rate = self.config.bsv_deposit_rates_decimal
      day_factor = 365/360
      # Anschlussgebuehr
      total_saving_amount = (0 - 
        (self.config.bsv_amount * self.config.bsv_acquisition_fee_decimal)
      )
      total_interest = 0.0
      evaluation_score = 0.0 
      # Maximum 100 Jahre
      for m in range(1, 12000):
        if m in  self.config.bsv_additional_payments.keys():
          total_saving_amount += self.config.bsv_additional_payments[m]
        else: 
          total_saving_amount += self.config.bsv_saving_rate
        if total_saving_amount > 0:
          interest = total_saving_amount * monthly_rate * day_factor / 12
          total_interest += interest
          total_saving_amount += interest
          evaluation_score = \
            self.calc_bewertungszahl(total_interest=total_interest, saving_amount=total_saving_amount)
          
        if self.config.bsv_minimum_evaluation_score <= evaluation_score:
          break
          
      return (total_interest, total_saving_amount, m, evaluation_score)
      

    def print_bsv_seving(self):
      print(f"==Sparphase==")
      month = self.config.bsv_saving_time_mounthly % 12
      years = int(self.config.bsv_saving_time_mounthly / 12)
      print(f"\tAnsparzeit: {years} Jahre und {month} Monate")
      print(f"\tGesamte Ansparung für {self.config.bsv_amount} EUR in {self.config.bsv_saving_time_mounthly} Monaten: {self.saving_amount:.2f} EUR")
      print(f"\tAnsparrate: {self.config.bsv_saving_rate:.2f} EUR pro Monat")
      print(f"\tZinsen: {self.interest_of_saving:.2f} EUR")
      print(f"\tBewertungszahl: {self.rating_nummber:.2f} \n")

    def print_bsv_seving_with_additional_payment(self):
      print(f"==Sparphase==")
      month = self.config.bsv_saving_time_mounthly % 12
      years = int(self.config.bsv_saving_time_mounthly / 12)
      print(f"\tAnsparzeit: {years} Jahre und {month} Monate")
      print(f"\tGesamte Ansparung für {self.config.bsv_amount} EUR in {self.config.bsv_saving_time_mounthly} Monaten: {self.saving_amount_with_additional_payment:.2f} EUR")
      print(f"\tRegulare Ansparrate: {self.config.bsv_saving_rate:.2f} EUR pro Monat")
      for moment, payment in self.config.bsv_additional_payments.items():
         print(f"\tZusatzliche Ansparrate: {payment} EUR in {moment}. Monat")
      print(f"\tZinsen: {self.interest_of_saving_with_additional_payment:.2f} EUR")
      print(f"\tBewertungszahl: {self.rating_nummber_with_additional_payment:.2f} \n")

    def calc_interim_loan(self):
       interim_loan = (self.config.bsv_amount * self.config.bsv_minimum_saving_amount) - self.saving_amount_with_additional_payment
       if interim_loan > 0 :
          from loan import Loan
          loan = Loan(self.config)
          interim_loan_monthly_paymant = loan.calculate_monthly_rate(
            interim_loan, 
            self.config.bsv_interim_loan_duration_in_month, 
            self.config.bsv_interim_loan_interest_rate
          )
          interim_loan_total_interest = loan.calculate_total_interest(
            interim_loan, 
            interim_loan_monthly_paymant, 
            self.config.bsv_interim_loan_duration_in_month, 
            self.config.bsv_interim_loan_interest_rate
          )
          return (True, interim_loan, interim_loan_monthly_paymant, interim_loan_total_interest)
       return (False, 0, 0, 0)
    
    def print_interim_loan(self, interim_loan, interim_loan_monthly_paymant, interim_loan_total_interest):
      print(f"==Zwischenfinanzierung für Teilung==")
      print(f"\tRest zu finanzieren: {interim_loan:.2f} EUR")
      print(f"\tRestfinanzierung für {self.config.bsv_interim_loan_duration_in_month} Monaten: {interim_loan_monthly_paymant:.2f} EUR pro Monat")
      print(f"\tZinsen für Restfinanzierung über {self.config.bsv_saving_time_mounthly} Monate: {interim_loan_total_interest:.2f} EUR\n")

    def calc_loan_phase(self):
      bsv_minimum_saving_amount = self.config.bsv_amount * self.config.bsv_minimum_saving_amount
      loan_amount = self.config.bsv_amount - bsv_minimum_saving_amount
      if self.saving_amount_with_additional_payment > bsv_minimum_saving_amount:
        loan_amount = self.config.bsv_amount - self.saving_amount
      darlehen_bausparen_rate_monat = self.config.bsv_amount * self.config.interest_prinipal_paymants_mounthly
      from loan import Loan
      loan = Loan(self.config)
      (loan_total_month, loan_total_interest) = loan.calculate_total_interest_with_fix_payment(loan_amount, darlehen_bausparen_rate_monat)
      return (loan_amount, loan_total_month, loan_total_interest)
    
    def print_loan_phase(self, loan_amount, loan_total_month, loan_total_interest):
      print(f"==Darlehensphase==")
      print(f"\tDarlehen nache dem Ansparen: {loan_amount:.2f} EUR")
      print(f"\tLaufzeit: {loan_total_month} Monate ({int(loan_total_month/12)} Jahre und {loan_total_month%12} Monate)")
      print(f"\tDarlehensrate: {self.config.bsv_amount * self.config.interest_prinipal_paymants_mounthly:.2f} EUR pro Monat")
      print(f"\tDarlehen: Gesamte Zinsen für {loan_total_month} Monate: {loan_total_interest:.2f} EUR\n")
          
    def print_bsv_saving_time_to_reach_evaluation_score(self, months, total_saving_amount, evaluation_score):
      print(f"==Sparphase Bewertungszahl (Bausparsumme: {self.config.bsv_amount:.2f} EUR)==")
      print(f"\tRegulare Ansparrate: {self.config.bsv_saving_rate:.2f} EUR pro Monat")
      for moment, payment in self.config.bsv_additional_payments.items() :
        print(f"\tZusätzliche Ansparraten im Monat {moment} : {payment:.2f} EUR")
      print(f"\tErrechnete Ansparzeit: {months} Monaten ({int(months / 12)} Jahre und {months % 12} Monate)")
      print(f"\tMindestansparrung: {(self.config.bsv_amount * self.config.bsv_minimum_saving_amount):.2f} EUR")
      print(f"\tGesamte Ansparung: {total_saving_amount:.2f} EUR")
      print(f"\tErreichte Bewerttungszahl: {evaluation_score:.2f}")
      print(f"\n")


      