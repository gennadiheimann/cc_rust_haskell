from configuration import ConfigReader
import math

class Loan:
  def __init__(self, config: ConfigReader):
    self.config = config
  
  def calculate_monthly_rate(self, loan_sum, duration_in_month, interest_rate):
    i = interest_rate / 12        # monatlicher Zinssatz
    n = int(duration_in_month)
    monthly_rate = loan_sum * (i * (1 + i) ** n) / ((1 + i) ** n - 1)
    #print(f"Monatliche Rate: {monthly_rate:.2f} €\n")
    return monthly_rate

  def calculate_total_interest(self, loan_sum, loan_payment_monthly, duration_in_month, interest_rate):
    total_interest = 0.0

    #print("Monat | Rate   | Zinsen | Tilgung | Restschuld")

    restschuld = loan_sum
    n = int(duration_in_month)
    i = interest_rate / 12 

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

    #print(f"Laufzeit: {total_month} Monate ({int(total_month/12)} Jahre und {total_month%12} Monate)")
    
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
  """
    Berechnung des tilgungsfreien Darlehen
  """
  def calc_interest_only_mortage(self):
    interest_only_mortgage_interest_total = (
      self.config.interest_only_mortgage_loan_amount * 
      self.config.interest_only_mortgage_interest_rate_decimal * 
      self.config.interest_only_mortgage_duration_in_months / 12
    )

    interest_only_mortgage_loan_payment_monthly = (
      interest_only_mortgage_interest_total / self.config.interest_only_mortgage_duration_in_months
    )
    return (interest_only_mortgage_interest_total, interest_only_mortgage_loan_payment_monthly)
  

  def print_interest_only_mortgage(self, interest_only_mortgage_interest_total, interest_only_mortgage_loan_payment_monthly):
    print(f"==Tilgungsfreies Darlehen==")

    print(f"\tDarlehen: {self.config.interest_only_mortgage_loan_amount:.2f} EUR")
    
    months = self.config.interest_only_mortgage_duration_in_months
    interest_total = interest_only_mortgage_interest_total

    print(f"\tGesamte Zinsen für {months} Monate: {interest_total:.2f} EUR")

    print(f"\tMonatliche Rate: {interest_only_mortgage_loan_payment_monthly:.2f} EUR\n")

  
  def calc_interest_only_mortage_extension(self):
    if self.config.bsv_saving_time_mounthly - self.config.interest_only_mortgage_duration_in_months > 0:
      self.extension_time_after_saving_time = self.config.bsv_saving_time_mounthly - self.config.interest_only_mortgage_duration_in_months

      self.interest_only_mortgage_extension_interest_total = \
        self.config.interest_only_mortgage_loan_amount * self.config.interest_only_mortage_extension_interest_rate_decimal * self.extension_time_after_saving_time / 12
  
      self.interest_only_mortgage_extension_loan_payment_monthly = self.interest_only_mortgage_extension_interest_total / self.extension_time_after_saving_time
    else:
      self.interest_only_mortgage_extension_interest_total = 0.0
      self.interest_only_mortgage_extension_loan_payment_monthly = 0.0 

  def print_interest_only_mortage_extension(self):
    if self.config.bsv_saving_time_mounthly - self.config.interest_only_mortgage_duration_in_months > 0:
      print("==Tilgungsfreies Darlehen Verlängerung==")
      
      print(f"\tGesamte Zinsen für {self.extension_time_after_saving_time} Monate: {self.interest_only_mortgage_extension_interest_total:.2f} EUR")

      print(f"\tMonatliche Rate: {self.interest_only_mortgage_extension_loan_payment_monthly:.2f} EUR\n")
    
