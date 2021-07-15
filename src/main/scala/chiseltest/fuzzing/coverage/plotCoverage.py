import sys
import json
import matplotlib.pyplot as plt

"""Plot inputted JSON files"""
def plotJSON(JSON_filenames):
    # Get JSON file(s) data
    files = [open(file) for file in JSON_filenames]
    input_data = [json.load(file) for file in files]
    [file.close() for file in files]

    # Convert JSON data to plotting data
    plottingData = [extractPlottingData(input) for input in input_data]

    # Plot data
    for i in range(len(plottingData)):
        (creation_time, cumulative_coverage) = plottingData[i]
        plt.plot(creation_time, cumulative_coverage, label = JSON_filenames[i])

    plt.title("Cumulative coverage over time")
    plt.ylabel("% of false mux toggle coverage")
    plt.xlabel("Seconds")
    plt.legend()
    plt.show()


"""Extract data from INPUT_DATA in JSON format and convert to matplotlib plotting format"""
def extractPlottingData(input_data):
    cumulative_coverage = []
    creation_times = []
    for input in input_data:
        creation_times.append((input['creation_time']))
        cumulative_coverage.append(input["cumulative_coverage"] * 100)

    assert len(creation_times) == len(cumulative_coverage), "JSON FILE HAS BAD FORMATTING, UNEQUAL NUMBER OF INPUT DATA POINTS"

    return (creation_times, cumulative_coverage)


if __name__ == "__main__":
    JSON_filenames = sys.argv[1:]
    assert len(JSON_filenames) > 0, "MUST INPUT JSON FILE(S) AS COMMAND LINE ARGUMENT(S)"
    plotJSON(JSON_filenames)
