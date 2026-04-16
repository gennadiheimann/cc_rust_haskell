try:
    import tomllib
except ImportError:
    import tomli as tomllib

class ConfigReader:
    def __init__(self, config_file_name):
        with open(config_file_name, "rb") as f:
            config = tomllib.load(f)
        # Tilgungsfreie Finanzierung
        self.tilgungsfreie_darlehen_zins = float(config["interest_only_mortgage"]["interest_rate_decimal"])
        # Algemein Bausparvertrag
        self.bausparsumme = float(config["bsv"]["bs_sum"])
        self.mindest_ansparung = float(config["bsv"]["minimum_saving_amount_decimal"])
        self.abschlussgebuehr = float(config["bsv"]["acquisition_fee_decimal"])
        # Ansparen
        self.ansparen_guthabenzins = float(config["bsv"]["deposit_rates_decimal"])
        self.ansparen_zeit = int(config["bsv"]["saving_time"])
        self.ansparen_rate = float(config["bsv"]["saving_rate"])
        # Zwischenfinanzierung
        self.ratenkredit_zins = float(config["instalment_loan"]["interest_rate_decimal"])
        # Darlehen
        self.darlehen_zins = float(config["bsv"]["fixed_interest_rate_decimal"])
        self.darlehen_rate = float(config["bsv"]["interest_prinipal_paymants_mounthly_decimal"])
        # Darlehen ohne BSV
        self.darlehen_ohne_bsv_zins_prognose_0_10 = float(config["loan_without_bsv"]["interest_rate_forcast_0_10_decimal"])
        self.darlehen_ohne_bsv_laufzeit_0_10 = int(config["loan_without_bsv"]["duration_in_months_0_10"])
        self.darlehen_ohne_bsv_zins_prognose_11_20 = float(config["loan_without_bsv"]["interest_rate_forcast_11_20_decimal"])
        self.darlehen_ohne_bsv_laufzeit_11_20 = int(config["loan_without_bsv"]["duration_in_months_11_20"])
        self.darlehen_ohne_bsv_zins_prognose_21_30 = float(config["loan_without_bsv"]["interest_rate_forcast_21_30_decimal"])
        self.darlehen_ohne_bsv_laufzeit_21_30 = int(config["loan_without_bsv"]["duration_in_months_21_30"])
        self.darlehen_ohne_bsv_zins_prognose_31_40 = float(config["loan_without_bsv"]["interest_rate_forcast_31_40_decimal"])
        self.darlehen_ohne_bsv_laufzeit_31_40 = int(config["loan_without_bsv"]["duration_in_months_31_40"])