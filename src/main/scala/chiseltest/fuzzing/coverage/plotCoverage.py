import sys
import json
import matplotlib.pyplot as plt

def loadJSON(JSON_filename):
    file = open(JSON_filename)
    input_data = json.load(file)
    file.close()
    
    cumulative_coverage = []
    creation_times = []
    for input in input_data:
        cumulative_coverage.append(input["cumulative_coverage"] * 100)
        creation_times.append((input['creation_time']))

    assert len(cumulative_coverage) == len(creation_times), "BRANDON: JSON FILE HAS BAD FORMATTING, UNEQUAL NUMBER OF INPUT DATA POINTS"

    plt.plot(creation_times, cumulative_coverage)
    plt.ylabel("Cumulative coverage (% false mux toggle coverage)")
    plt.xlabel("Time (sec)")
    plt.show()


if __name__ == "__main__":
    JSON_filename = sys.argv[1]
    loadJSON(JSON_filename)
