import csv

with open("T_T100D_SEGMENT_ALL_CARRIER.csv") as csvfile:
    reader = csv.reader(csvfile)

    data = []

    for row in reader:
        data.append(row)

airports = {}

for row in data[1:]:
    origin = row[6]
    if origin not in airports:
        airports[origin] = {
            "name": origin,
            "id": [row[5]],
            "depts": float(row[1]),
            "pass": float(row[3])
        }
    else:
        if row[5] not in airports[origin]["id"]:
            print(origin, "has multiple ids")
            airports[origin]["id"].append(row[5])
        airports[origin]["depts"] += float(row[1])
        airports[origin]["pass"] += float(row[3])

def get_depts(airport):
    return airport["depts"]

def get_pass(airport):
    return airport["pass"]

pass500 = [airport["name"] for airport in sorted(list(airports.values()), key=get_pass, reverse=True)[:500]]
# print(pass500)

airport_to_node = {}

for i in range(len(pass500)):
    airport_to_node[pass500[i]] = i

with open("nodeAirportName.csv", 'w', newline='', encoding='utf-8') as file:
    writer = csv.writer(file,)

    writer.writerow(["nodeID", "airportCode"])

    for airport in airport_to_node:
        writer.writerow([airport_to_node[airport], airport])

# [['DEPARTURES_SCHEDULED', 'DEPARTURES_PERFORMED', 'SEATS', 'PASSENGERS', 'DISTANCE', 'ORIGIN_AIRPORT_ID', 'ORIGIN', 'DEST_AIRPORT_ID', 'DEST', 'MONTH']

network_dict = {}

for row in data:
    origin = row[6]
    dest = row[8]
    passengers = row[3]

    if origin in pass500 and dest in pass500:
        if (origin, dest) not in network_dict:
            network_dict[(origin, dest)] = {
                "origin": airport_to_node[origin],
                "destination": airport_to_node[dest],
                "o_code": origin,
                "d_code": dest,
                "totalPassengers": float(passengers)
            }
        else:
            network_dict[(origin, dest)]["totalPassengers"] += float(passengers)

with open("passengersEdgelist.csv", 'w', newline='', encoding='utf-8') as file:
    writer = csv.writer(file)

    writer.writerow(["origin", "destination", "passengers"])

    for route in network_dict.values():
        if route["totalPassengers"] != 0:
            writer.writerow([route["origin"], route["destination"], int(route["totalPassengers"])])

with open("airportNetwork.csv", 'w', newline='', encoding='utf-8') as file:
    writer = csv.writer(file)

    writer.writerow(["origin", "destination"])

    for route in network_dict.values():
        if route["totalPassengers"] != 0 and route["origin"] != route["destination"]:
            writer.writerow([route["o_code"], route["d_code"]])





