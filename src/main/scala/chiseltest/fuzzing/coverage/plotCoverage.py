import argparse
import json
import matplotlib.pyplot as plt
import numpy as np
import os
from scipy.interpolate import interp1d

"""Plot inputted JSON files"""
def plotJSON(do_average, JSON_filepaths):
    # Load and parse plotting data from JSON files
    (json_data, JSON_filenames) = loadJSON(JSON_filepaths)
    plottingData = [extractPlottingData(input) for input in json_data]

    # Plot data (Averaging code modeled from RFUZZ analysis.py script: https://github.com/ekiwi/rfuzz)
    if do_average:
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
            plt.step(creation_time, cumulative_coverage, where='post', label=JSON_filenames[i])

    plt.title("Coverage Over Time")
    plt.ylabel("Cumulative coverage %")
    plt.yticks([x for x in range(0, 110, 10)])
    plt.xlabel("Seconds")
    plt.legend()
    plt.savefig("coveragePlot.png")
    plt.show()


"""Loads in data from JSON files. 
   Input (JSON_LOCATIONS): List of JSON files and folders that contain JSON files.
   Return: List of JSON data and list of their filenames (INPUT_DATA, JSON_FILENAMES) """
def loadJSON(JSON_filepaths):
    JSON_filenames = recursiveLocateJSON(JSON_filepaths)
    assert JSON_filenames, "NO JSON FILES FOUND WITHIN PROVIDED FILEPATHS: {}".format(JSON_filepaths)

    files = [open(file, 'r') for file in JSON_filenames]
    input_data = [json.load(file) for file in files]
    [file.close() for file in files]

    return input_data, JSON_filenames


"""Locates all paths to JSON files. Searches recursively within folders.
   Input (JSON_LOCATIONS): List of files and folders that contain JSON files. 
   Return: List of all JSON files at JSON_FILEPATHS."""
def recursiveLocateJSON(JSON_filepaths):
    JSON_filenames = []

    for filepath in JSON_filepaths:
        if os.path.isfile(filepath) and filepath.split(".")[-1].lower() == "json":
            JSON_filenames.append(filepath)
        elif os.path.isdir(filepath):
            subPaths = [os.path.join(filepath, subPath) for subPath in os.listdir(filepath)]
            JSON_filenames.extend(recursiveLocateJSON(subPaths))

    return JSON_filenames


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
    parser = argparse.ArgumentParser(description='Plotting script for fuzzing results')
    parser.add_argument('do_average', help='Indicates whether the plotted data should be averaged or not')
    parser.add_argument('JSON_filepaths', metavar='Path', nargs='+', help='Paths where JSON files exist, recursively searches')
    args = parser.parse_args()

    if args.do_average.lower() == "true":
        do_average = True
    elif args.do_average.lower() == "false":
        do_average = False
    else:
        raise argparse.ArgumentTypeError("DO_AVERAGE ARGUMENT MUST BE TRUE/FALSE, NOT: {}".format(args.do_average))

    for filepath in args.JSON_filepaths:
        if not (os.path.isfile(filepath) or os.path.isdir(filepath)):
            raise argparse.ArgumentTypeError("INPUT JSON FILEPATH DOES NOT EXIST: {}".format(filepath))

    plotJSON(do_average, args.JSON_filepaths)
