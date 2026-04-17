from configuration import ConfigReader

class Bauspar:
    def __init__(self, config: ConfigReader):
        self.config = config
    
    def calculate_saving_amount(self):
        monthly_rate = self.config.ansparen_guthabenzins
        day_factor = 365/360

        total_saving_amount = 0 -  (self.config.bausparsumme * self.config.abschlussgebuehr)

        for _ in range(self.config.ansparen_zeit):
            total_saving_amount += self.config.ansparen_rate
            if total_saving_amount > 0:
                interest = total_saving_amount * monthly_rate * day_factor / 12
                total_saving_amount += interest
        return total_saving_amount