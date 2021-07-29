import sys
import json
import matplotlib.pyplot as plt

"""Plot inputted JSON files"""
def plotJSON(JSON_filenames):
    files = [open(file, 'r') for file in JSON_filenames]
    # List of data dictionaries for each input file
    input_data = [json.load(file) for file in files]
    [file.close() for file in files]

    # Convert JSON data to plotting data
    plottingData = [extractPlottingData(input) for input in input_data]

    # Plot data
    for i in range(len(plottingData)):
        (creation_time, cumulative_coverage) = plottingData[i]
        plt.plot(creation_time, cumulative_coverage, label = JSON_filenames[i])

    plt.title("Coverage Over Time")
    plt.ylabel("Cumulative coverage %")
    plt.yticks([x for x in range(0, 110, 10)])
    plt.xlabel("Seconds")
    plt.legend()
    plt.savefig("coveragePlot.png")
    plt.show()


"""Extract data from a single INPUT_DATA dictionary in JSON format and convert to matplotlib plotting format"""
def extractPlottingData(input_data):
    creation_times = []
    cumulative_coverage = []
    for input in input_data['coverage_data']:
        creation_times.append((input['creation_time']))
        cumulative_coverage.append(input["cumulative_coverage"] * 100)

    assert len(creation_times) == len(cumulative_coverage), "JSON FILE HAS BAD FORMATTING, UNEQUAL NUMBER OF INPUT DATA POINTS"

    # Extract end time from JSON file and add it to plotting data
    creation_times.append(input_data['end_time'])
    cumulative_coverage.append(cumulative_coverage[-1])

    return (creation_times, cumulative_coverage)


if __name__ == "__main__":
    JSON_filenames = sys.argv[1:]
    assert len(JSON_filenames) > 0, "MUST INPUT JSON FILE(S) AS COMMAND LINE ARGUMENT(S)"
    plotJSON(JSON_filenames)
