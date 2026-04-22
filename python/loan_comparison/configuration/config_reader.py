try:
    import tomllib
except ImportError:
    import tomli as tomllib

class ConfigReader:
    def __init__(self, config_file_name):
        with open(config_file_name, "rb") as f:
            config = tomllib.load(f)
        # Tilgungsfreie Finanzierung
        self.interest_only_mortgage_interest_rate_decimal = float(config["interest_only_mortgage"]["interest_rate_decimal"])
        self.interest_only_mortgage_duration_in_months = float(config["interest_only_mortgage"]["duration_in_months"])
        self.interest_only_mortgage_loan_amount = float(config["interest_only_mortgage"]["loan_amount"])
        self.interest_only_mortage_extension_interest_rate_decimal = float(config["interest_only_mortage_extension"]["interest_rate_decimal"])
        # Algemein Bausparvertrag
        bsv = config["bsv"]
        self.bsv_amount = float(bsv["amount"])
        self.bsv_minimum_saving_amount = float(bsv["minimum_saving_amount_decimal"])
        self.abschlussgebuehr = float(config["bsv"]["acquisition_fee_decimal"])
        # Ansparen
        self.ansparen_guthabenzins = float(config["bsv"]["deposit_rates_decimal"])
        self.bsv_saving_time = int(config["bsv"]["saving_time"])
        #self.bsv_additional_payment = config["bsv"]["additional_payment"]
        #self.bsv_additional_payment_moment = config["bsv"]["additional_payment_moment"]
        a_p = bsv["additional_payments"]
        self.bsv_additional_payments = {p["moment"]: p["payment"] for p in a_p}
        self.ansparen_rate = float(config["bsv"]["saving_rate"])
        self.zinsfaktor = float(config["bsv"]["interest_factor"])
        # Zwischenfinanzierung
        self.bsv_interim_loan_interest_rate = float(bsv["interim_loan"]["interest_rate_decimal"])
        self.bsv_interim_loan_duration_in_month = float(bsv["interim_loan"]["duration_in_month"])
        # Darlehen
        self.darlehen_zins = float(config["bsv"]["fixed_interest_rate_decimal"])
        self.interest_prinipal_paymants_mounthly = float(config["bsv"]["interest_prinipal_paymants_mounthly_decimal"])
        # Darlehen ohne BSV
        self.darlehen_ohne_bsv_zins_prognose_0_10 = float(config["loan_without_bsv"]["interest_rate_forcast_0_10_decimal"])
        self.darlehen_ohne_bsv_laufzeit_0_10 = int(config["loan_without_bsv"]["duration_in_months_0_10"])
        self.darlehen_ohne_bsv_zins_prognose_11_20 = float(config["loan_without_bsv"]["interest_rate_forcast_11_20_decimal"])
        self.darlehen_ohne_bsv_laufzeit_11_20 = int(config["loan_without_bsv"]["duration_in_months_11_20"])
        self.darlehen_ohne_bsv_zins_prognose_21_30 = float(config["loan_without_bsv"]["interest_rate_forcast_21_30_decimal"])
        self.darlehen_ohne_bsv_laufzeit_21_30 = int(config["loan_without_bsv"]["duration_in_months_21_30"])
        self.darlehen_ohne_bsv_zins_prognose_31_40 = float(config["loan_without_bsv"]["interest_rate_forcast_31_40_decimal"])
        self.darlehen_ohne_bsv_laufzeit_31_40 = int(config["loan_without_bsv"]["duration_in_months_31_40"])