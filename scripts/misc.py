import csv
import json

with open("../data/T_T100D_SEGMENT_ALL_CARRIER.csv") as csvfile:
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

for i in range(1, len(pass500) + 1):
    airport_to_node[pass500[i-1]] = i

# with open("nodeAirportName.csv", 'w', newline='', encoding='utf-8') as file:
#     writer = csv.writer(file,)

#     writer.writerow(["nodeID", "airportCode"])

#     for airport in airport_to_node:
#         writer.writerow([airport_to_node[airport], airport])

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


# import pandas as pd

# xls = pd.ExcelFile("../data/cy10_all_enplanements.xls") # use r before absolute file path 

# sheetX = xls.parse() #2 is the sheet number+1 thus if the file has only 1 sheet write 0 in paranthesis

# locid = sheetX['Locid']
# enplanements = sheetX['CY 10 Enplanements']

# with open("enplanements.csv", 'w', newline='', encoding='utf-8') as file:
#     writer = csv.writer(file)

#     writer.writerow(["airport", "enplanements"])

#     for airport in airport_to_node:
#         num = 0

#         for i in range(len(locid)):
#             if locid[i] == airport:
#                 num = enplanements[i]

#         if num == 0:
#             writer.writerow([airport, "MISSING!!!!!"])
#         else:
#             writer.writerow([airport, num])







# with open("passengersEdgelist.csv", 'w', newline='', encoding='utf-8') as file:
#     writer = csv.writer(file)

#     writer.writerow(["origin", "destination", "passengers"])

#     for route in network_dict.values():
#         if route["totalPassengers"] != 0:
#             writer.writerow([route["origin"], route["destination"], int(route["totalPassengers"])])

# with open("airportNetwork.csv", 'w', newline='', encoding='utf-8') as file:
#     writer = csv.writer(file)

#     writer.writerow(["origin", "destination"])

#     for route in network_dict.values():
#         if route["totalPassengers"] != 0 and route["origin"] != route["destination"]:
#             writer.writerow([route["o_code"], route["d_code"]])

# with open("airportNetworkNumeric.csv", 'w', newline='', encoding='utf-8') as file:
#     writer = csv.writer(file)

#     writer.writerow(["origin", "destination"])

#     for route in network_dict.values():
#         if route["totalPassengers"] != 0 and route["origin"] != route["destination"]:
#             writer.writerow([route["origin"], route["destination"]])



# with open("airport-codes_csv.csv", encoding="utf-8") as file:
#     reader = csv.reader(file)

#     data = []

#     for row in reader:
#         data.append(row)

# with open("all_places.txt") as file:
#     places = json.load(file)

# print(places[0])

# with open("airport_census.json") as file:
#     airport_census = json.load(file)

# for airport in airport_to_node:
#     # airport_census[airport] = {
#     #     "city": "",
#     #     "place": []
#     # }
#     airport_census[airport]["place"] = []

# for airport in airport_census:
#     # for row in data:
#     #     if row[9] == airport:
#     #         airport_census[airport]["city"] = row[7]
#     for row in places:
#         if airport_census[airport]["city"] != "" and airport_census[airport]["city"] in row[0]:
#             airport_census[airport]["place"].append(row)

# for airport in airport_census:
#     if len(airport_census[airport]["place"]) == 0:
#         print(airport, "no city found")
#     elif len(airport_census[airport]["place"]) > 1:
#         print(airport, "multiple cities found")
# #         # airport_census[airport]["city"] = input("Enter city name: ")

# with open("airport_census.json", 'w') as file:
#     json.dump(airport_census, file)
    

# with open("Airports2.csv") as file:
#     reader = csv.reader(file)

#     kaggle = []

#     for row in reader:
#         kaggle.append(row)

# print(kaggle[0])
# print(kaggle[1])

# print(airport_to_node.values())

# airport_pop = {}

# for airport in airport_to_node:
#     airport_pop[airport] = {
#         "pop": None,
#         "year": 0
#     }

# for row in kaggle[1:]:
#     if row[0] in airport_pop:
#         # print("airport found")
#         # if airport_pop[row[0]] != None and airport_pop[row[0]] != row[9]:
#         #     print(row[0], "has different populations", airport_pop[row[0]], row[9])
#         if int(row[8][:4]) > airport_pop[row[0]]["year"]:
#             airport_pop[row[0]]["pop"] = row[9]
#             airport_pop[row[0]]["year"] = int(row[8][:4])

# count = 0
# for airport in airport_pop:
#     if airport_pop[airport]["pop"] == None:
#         print(airport, "missing population")
#         count += 1

# print("missing", count, "populations")

# with open("airportPopulation.csv", 'w', newline='', encoding='utf-8') as file:
#     writer = csv.writer(file)

#     writer.writerow("population")

#     for pop in airport_pop.values():
#         writer.writerow(pop)

# print(airport_pop.values())



# airport_census = {}

# for airport in pass500:
#     airport_census[airport] = {
#         "city": "",
#         "state": "",
#         "places": [],
#         "place": "",
#         "pop": None
#     }

# with open("airport_census.json", 'w') as file:
#     json.dump(airport_census, file)

# with open("airport_census.json") as file:
#     airport_census = json.load(file)

# with open("../data/BTS_T-100_v2.csv") as file:
#     reader = csv.reader(file)

#     data = []

#     for row in reader:
#         data.append(row)

# with open("../data/all_places.txt") as file:
#     places = json.load(file)

# print(data[0])
# ['DEPARTURES_PERFORMED', 'SEATS', 'PASSENGERS', 'DISTANCE', 'ORIGIN', 'ORIGIN_CITY_NAME', 'ORIGIN_STATE_NM', 'DEST', 'DEST_CITY_NAME', 'DEST_STATE_NM', 'MONTH']

# for row in places[:5]:
#     print(row[0])

# for row in data[1:]:
#     origin = row[4]
#     city = row[5][:row[5].index(',')]
#     state = row[6]

#     if origin in airport_census and airport_census[origin]["city"] == "":
#         airport_census[origin]["city"] = city
#         airport_census[origin]["state"] = state
        
#         for row in places:
#             place = row[0]

#             if place.startswith(city) and place.endswith(state):
#                 airport_census[origin]["places"].append(place)
        
#         if len(airport_census[origin]["places"]) == 0:
#             print(origin, "no place")
#         elif len(airport_census[origin]["places"]) == 1:
#             print(origin, "place found")
#             airport_census[origin]["place"] = airport_census[origin]["places"][0]
#         else:
#             print(origin, "multiple places - ", " | ".join(airport_census[origin]["places"]))

# for key in airport_census:
#     airport = airport_census[key]
#     if len(airport["places"]) > 1 and airport["place"] == "":
#         print(key, " - ", airport["city"], airport["state"], "has multiple places - ", " | ".join(airport["places"]))
#         i = input("Enter place index: ")
#         if i == 's':
#             print("skipping")
#         elif i == 'e':
#             print("exiting")
#             break
#         else:
#             airport["place"] = airport["places"][int(i)]

# for key in airport_census:
#     airport = airport_census[key]
#     if airport["place"] == "":
#         print(key, " - ", airport["city"], airport["state"], "has no place")
#         place = input("Enter place: ")
#         if place == 's':
#             print("skipping")
#         elif place == 'e':
#             print("exiting")
#             break
#         else:
#             airport["place"] = place

# count = 0
# for airport in airport_census.values():
#     if airport["place"] == "":
#         count += 1

# for key in airport_census:
#     airport = airport_census[key]
#     if airport["place"] != "skip":
#         for row in places:
#             if airport["place"] == row[0]:
#                 airport["pop"] = row[1]
#                 break
#         if airport["pop"] == None or airport["pop"] == "null":
#             print(airport, "missing population")



# for key in airport_census:
#     airport = airport_census[key]
#     if airport["pop"] == None or airport["pop"] == "null":
#         print(key, " - ", airport["city"], airport["state"], "has no population")
#         pop = input("Enter population: ")
#         if pop == 's':
#             print("skipping")
#         elif pop == 'e':
#             print("exiting")
#             break
#         else:
#             airport["pop"] = pop

# count = 0

# for key in airport_census:
#     airport = airport_census[key]
#     if airport["pop"] == None or airport["pop"] == "null":
#         count += 1

# print(count)
        

                                                 
                                        
# if input("save? ") == 'y':
#     with open("airport_census.json", 'w') as file:
#         json.dump(airport_census, file)

# with open("airportPopulation.csv", 'w', newline='', encoding='utf-8') as file:
#     writer = csv.writer(file)

#     writer.writerow(["population"])

#     for airport in airport_to_node:
#         writer.writerow([airport_census[airport]["pop"]])

with open("airportPopulation.csv") as file:
    reader = csv.reader(file)

    populations = []

    for row in reader:
        populations.append(row[0])

    populations = [int(num) for num in populations[1:]]

    mean = sum(populations)/len(populations)
    print(mean)

    with open("populationBool.csv", 'w', newline='', encoding='utf-8') as new_file:
        writer = csv.writer(new_file)

        writer.writerow(["Population>Mean("+str(mean)+")"])

        for num in populations:
            if num > mean:
                writer.writerow(["1"])
            else:
                writer.writerow(["0"])





    




