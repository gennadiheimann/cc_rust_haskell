class Exporter:
    def __init__(self, export_data):
        self.export_data = export_data

    def export_to_csv(self, filename):
        import csv

        with open(filename, mode="w", newline="", encoding="utf-8") as file:
            # fieldnames = ["Vorname", "Nachname", "Alter"]
            # writer = csv.DictWriter(file, fieldnames=fieldnames)
            # writer.writeheader()
            # writer.writerows(daten)
            
            writer = csv.writer(file)
            # Kopfzeile
            #writer.writerow(["Vorname", "Nachname", "Alter"])

            for key, value in self.export_data.items():
                writer.writerow([key, value])
            # writer.writerow(["Bausparsumme", bs_summe])
            # writer.writerow(["Ansparungszeit in Monaten", ansparzeit])
            # writer.writerow(["Ansparbetrag - Anschlussgebühr", kapital_mit_abschlussgebuer])