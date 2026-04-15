from configuration import ConfigReader

class Bauspar:
    def __init__(self, config: ConfigReader):
        self.config = config
    
    def calculate_saving_amount(self):
        monthly_interest = self.config.ansparen_guthabenzins / 12
        total_saving_amount = 0.0
        for _ in range(self.config.ansparen_zeit):
            total_saving_amount = total_saving_amount * (1 + monthly_interest) + self.config.ansparen_rate
        return total_saving_amount - (self.config.bausparsumme * self.config.abschlussgebuehr)