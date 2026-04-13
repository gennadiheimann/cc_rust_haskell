import tomllib 

with open("config.toml", "rb") as f:
    config = tomllib.load(f)

print(config["title"])
print(config["bsv"]["bs_sum"])
print(config["bsv"]["deposit_rates_decimal"])
print(config["bsv"]["minimum_saving_amount_decimal"])
print(config["bsv"]["acquisition_fee_decimal"])
print(config["bsv"]["saving_time"])
print(config["bsv"]["saving_rate"])


bs_sum = float(config["bsv"]["bs_sum"])
deposit_rates_decimal = float(config["bsv"]["deposit_rates_decimal"])
minimum_saving_amount_decimal = float(config["bsv"]["minimum_saving_amount_decimal"])
acquisition_fee_decimal = float(config["bsv"]["acquisition_fee_decimal"])
saving_time = float(config["bsv"]["saving_time"])
saving_rate = float(config["bsv"]["saving_rate"])

saving_amount_after_saving_time = (saving_time * saving_rate) -  (bs_sum * acquisition_fee_decimal)
print(saving_amount_after_saving_time)
saving_amount_after_saving_time_with_deposit_rate = saving_amount_after_saving_time + (saving_amount_after_saving_time * deposit_rates_decimal)



rate = saving_rate
jahreszins = deposit_rates_decimal
jahre = saving_time / 12

monatszins = jahreszins / 12
monate = jahre * 12

kapital = 0.0

for _ in range(int(monate)):
    kapital = kapital * (1 + monatszins) + rate

eingezahlt = rate * monate
zinsen = kapital - eingezahlt
kapital_mit_abschlussgebuer = kapital - (bs_sum * acquisition_fee_decimal)
kredit = bs_sum * minimum_saving_amount_decimal - kapital_mit_abschlussgebuer

print(f"Kapital: {kapital:.2f}")
print(f"Zinsen: {zinsen:.2f}")
print(f"Abschlussgebühr: {bs_sum * acquisition_fee_decimal:.2f}")
print(f"Kapital mit Abschlussgebühr: {kapital_mit_abschlussgebuer:.2f}")
print(f"Kredit: {kredit:.2f}")
