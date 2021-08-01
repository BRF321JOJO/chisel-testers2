import sys
import json
import matplotlib.pyplot as plt
import numpy as np

# Code to generate flattened list of lists variable flattened_times sourced from the following link:
# https://stackoverflow.com/questions/952914/how-to-make-a-flat-list-out-of-a-list-of-lists

"""Plot inputted JSON files"""
def plotJSON(perform_average, JSON_filenames):
    files = [open(file, 'r') for file in JSON_filenames]
    # List of data dictionaries for each input file
    input_data = [json.load(file) for file in files]
    [file.close() for file in files]

    # Convert JSON data to plotting data
    plottingData = [extractPlottingData(input) for input in input_data]

    # Plot data
    if perform_average:
        all_times = [creation_times for (creation_times, _) in plottingData]
        all_times = [item for sublist in all_times for item in sublist]
        all_times.sort()

        # Idea for this averaging modeled from RFUZZ analysis.py script: https://github.com/ekiwi/rfuzz
        number_of_inputs = len(plottingData)
        all_percentages = np.zeros((number_of_inputs, len(all_times)))
        for i in range(number_of_inputs):
            all_percentages[i] = np.interp(all_times, plottingData[i][0], plottingData[i][1])
        means = np.mean(all_percentages, axis=0)
        plt.step(all_times, means, label = "Averaged: " + ", ".join([str(name) for name in JSON_filenames]))

    else:
        for i in range(len(plottingData)):
            (creation_time, cumulative_coverage) = plottingData[i]
            plt.step(creation_time, cumulative_coverage, label = JSON_filenames[i])

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
    assert len(sys.argv) > 1, "MUST INPUT ARGUMENTS: PERFORM_AVERAGE JSON_FILES ..."
    perform_average = True if sys.argv[1].lower() == "true" else False

    JSON_filenames = sys.argv[2:]
    assert len(JSON_filenames) > 0, "MUST INPUT JSON FILE(S) AS COMMAND LINE ARGUMENT(S)"
    plotJSON(perform_average, JSON_filenames)
