import sys
import json
import matplotlib.pyplot as plt
import numpy as np
from scipy.interpolate import interp1d


"""Plot inputted JSON files"""
def plotJSON(perform_average, JSON_filenames):
    # Load and parse plotting data from JSON files
    json_data = loadJSON(JSON_filenames)
    plottingData = [extractPlottingData(input) for input in json_data]

    # Plot data (Averaging code modeled from RFUZZ analysis.py script: https://github.com/ekiwi/rfuzz)
    if perform_average:
        # Collects all times seen across passed in JSON files
        all_times = []
        [all_times.extend(creation_times) for (creation_times, _) in plottingData]
        all_times = sorted(set(all_times))

        all_coverage = np.zeros((len(plottingData), len(all_times)))
        for i, (creation_times, cumulative_coverage) in enumerate(plottingData):
            # Returns function which interpolates y-value(s) when passed x-value(s). Obeys step function, using previous value when interpolating.
            interp_function = interp1d(creation_times, cumulative_coverage, kind='previous', bounds_error=False, assume_sorted=True)
            # Interpolates coverage value for each time in all_times. Saved to all_coverage matrix
            all_coverage[i] = interp_function(all_times)
        means = np.mean(all_coverage, axis=0)
        plt.step(all_times, means, where='post', label="Averaged: " + ", ".join([str(name) for name in JSON_filenames]))

    else:
        for i in range(len(plottingData)):
            (creation_time, cumulative_coverage) = plottingData[i]
            plt.step(creation_time, cumulative_coverage, where='post', label = JSON_filenames[i])

    plt.title("Coverage Over Time")
    plt.ylabel("Cumulative coverage %")
    plt.yticks([x for x in range(0, 110, 10)])
    plt.xlabel("Seconds")
    plt.legend()
    plt.savefig("coveragePlot.png")
    plt.show()


"""Loads in data from JSON files"""
def loadJSON(JSON_filenames):
    files = [open(file, 'r') for file in JSON_filenames]
    input_data = [json.load(file) for file in files]
    [file.close() for file in files]
    return input_data


"""Extract plotting data from a single JSON file's data"""
def extractPlottingData(input_data):
    creation_times = []
    cumulative_coverage = []
    for input in input_data['coverage_data']:
        creation_times.append((input['creation_time']))
        cumulative_coverage.append(input["cumulative_coverage"] * 100)

    # Extract end time from JSON file and add it to plotting data
    creation_times.append(input_data['end_time'])
    cumulative_coverage.append(cumulative_coverage[-1])

    assert len(creation_times) == len(cumulative_coverage), "NUMBER OF TIMES SHOULD EQUAL NUMBER OF COVERAGE READINGS"

    return (creation_times, cumulative_coverage)


if __name__ == "__main__":
    assert len(sys.argv) > 0, "MUST INPUT ARGUMENTS: PERFORM_AVERAGE JSON_FILES ..."
    perform_average = True if sys.argv[1].lower() == "true" else False

    JSON_filenames = sys.argv[2:]
    assert len(JSON_filenames) > 0, "MUST INPUT JSON FILE(S) AS COMMAND LINE ARGUMENT(S)"
    plotJSON(perform_average, JSON_filenames)
