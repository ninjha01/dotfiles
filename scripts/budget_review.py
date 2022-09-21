import pandas as pd
from collections import defaultdict
from pprint import pprint

def cprint(cost):
    print('${:,.2f}'.format(cost))

csv_path = "/Users/nishantjha/Downloads/2022-09-21_transaction_download.csv"
df = pd.read_csv(csv_path)
# replace nan with 0
df = df.fillna(0)

# get each row as a dict
rows = df.to_dict(orient="records")
sums_by_category = defaultdict(lambda: 0.0)
for row in rows:
    category, debit, credit = row["Category"], float(row["Debit"]), float(row["Credit"])
    sums_by_category[category] += debit - credit

print("Total spend")
# sum debits for all categories except Purchases
total_spend = sum([cost for category, cost in sums_by_category.items() if category != "Payment/Credit"])
cprint(total_spend)

print("Total spend per month")
cprint(total_spend / 12)

print("Summary")
pprint(sorted(sums_by_category.items(), key=lambda x: x[1], reverse=True))

print("Max Expense")
# get max expense in rows
expenses_by_cost = sorted(rows, key=lambda x: float(x["Debit"]), reverse=True)
pprint(expenses_by_cost[:5])

print("Money spent on amazon")
# get money spent on amazon
amazon_rows = [row for row in rows if row["Description"].lower().find("amazon") != -1]
# sum up the money spent on amazon
total_spent_on_amazon = sum([float(row["Debit"]) for row in amazon_rows])
cprint(total_spent_on_amazon)

print("Money spent on uber or lyft")
# get money spent on uber or lyft
uber_or_lyft_rows = [
    row
    for row in rows
    if row["Description"].lower().find("uber") != -1
    or row["Description"].lower().find("lyft") != -1
]
# sum up the money spent on amazon
total_spent_on_ubers = sum([float(row["Debit"]) for row in uber_or_lyft_rows])
cprint(total_spent_on_ubers)
