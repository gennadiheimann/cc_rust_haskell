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