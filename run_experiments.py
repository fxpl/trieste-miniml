#!/usr/bin/python

# Not my best work, but it gets the job done.
#
# This script generates random MiniML-programs of configurable sizes and compiles them to LLVM IR using Trieste.
# Compiler runtime information is gathered from Trieste's Info mode and is consolidated into a latex-formatted table,
# and plotted as a graph.

import subprocess
import re
import pandas
import matplotlib.pyplot as plt


def main():

    # Experiment parameters
    runs = 25
    topexprs = [1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000]
    h = 3

    output_file = "compiler_output.txt"
    all_results = []
    for w in topexprs:
        print(f"Compiling program with width: {w}, height: {h}")
        for i in range(runs):
            run_compiler(w, h, output_file)

            results = get_run_results(output_file)
            for result in results:
                result["loc"] = w
                result["run"] = i
                all_results.append(result)

    df = pandas.DataFrame(all_results)

    stages = [
        "Parsing",
        "Frontend",
        "Pre-Compilation",
        "Compilation",
        "Code Generation",
    ]
    stage_symbols = [
        "o",
        "v",
        "s",
        "^",
        "D",
    ]

    df_stages = group_passes_into_stages(df, stages)
    avg_runtime_per_stage = calc_runtime_avg_stddev_ms(df_stages)

    print(make_table(avg_runtime_per_stage, stages))
    make_plot(avg_runtime_per_stage, stages, stage_symbols)


def calc_runtime_avg_stddev_ms(df: pandas.DataFrame):
    """Calculate runtime average and standard deviation for each combination of stage and program size"""

    grouped = df.groupby(["loc", "stage"], as_index=False).agg(
        avg_time_us=("total_time_us", "mean"),
        std_time_us=("total_time_us", "std"),
    )

    grouped["avg_time_ms"] = grouped["avg_time_us"] / 1000
    grouped["std_time_ms"] = grouped["std_time_us"] / 1000

    return grouped


def group_passes_into_stages(
    df: pandas.DataFrame, stages: list[str]
) -> pandas.DataFrame:
    """Sums the runtime of all passes belonging to a compiler stage, per run"""

    parse_pass = ["parse"]
    frontend_passes = [
        "parse_cleanup",
        "fun",
        "parens",
        "conditionals",
        "let",
        "exprs",
        "funapp",
        "mul",
        "addsub",
        "comparison",
        "cleanup",
        "inf_fresh",
        "inf_exprs",
        "let_constr",
        "solve_constraints",
        "cleanup_constraints",
        "resolve_print",
    ]
    precompile_passes = [
        "resolve_polymorphism",
        "free_variables",
        "propagate_free_variables",
        "globals",
        "main_function",
        "closure_conversion",
        "closure_globals",
    ]
    compile_passes = ["compile", "blockify"]
    codegen_passes = ["generateLLVMIR"]

    pass_to_stage = {}
    for p in parse_pass:
        pass_to_stage[p] = stages[0]
    for p in frontend_passes:
        pass_to_stage[p] = stages[1]
    for p in precompile_passes:
        pass_to_stage[p] = stages[2]
    for p in compile_passes:
        pass_to_stage[p] = stages[3]
    for p in codegen_passes:
        pass_to_stage[p] = stages[4]

    df["stage"] = df["pass"].map(pass_to_stage)

    sum_by_run = df.groupby(["loc", "run", "stage"], as_index=False).agg(
        total_time_us=("time_us", "sum")
    )

    return sum_by_run


def make_table(df: pandas.DataFrame, categories: list[str]) -> str:
    """Creates a latex-formatted table containing the results"""

    pivot = df.pivot(
        index="loc",
        columns="stage",
        values=["avg_time_ms", "std_time_ms"],
    )

    avg = pivot["avg_time_ms"].round(0).astype(int).map("{:,}".format)
    std = pivot["std_time_ms"].round(0).astype(int).map("{:,}".format)

    table_df = avg + r"$\pm$" + std
    table_df = table_df[categories]

    tabular = []
    col_format = "r" + "c" * len(categories)

    tabular.append(r"\begin{tabular}{" + col_format + "}")
    tabular.append(r"\toprule")
    tabular.append("LOC & " + " & ".join(categories) + r" \\")
    tabular.append(r"\midrule")

    for loc in table_df.index:
        row = []
        for cat in categories:
            row.append(table_df.at[loc, cat])

        tabular.append(f"{loc} & " + " & ".join(row) + r" \\")

    tabular.append(r"\bottomrule")
    tabular.append(r"\end{tabular}")

    return "\n".join(tabular)


def make_plot(df: pandas.DataFrame, categories: list[str], symbols: list[str]):
    """Plots the results onto a graph"""

    assert len(categories) == len(symbols)

    marker_size = 10
    plot_font_size = 16
    tick_font_size = 16
    legend_font_size = 18
    plt.figure(figsize=(12, 7))

    for i, stage in enumerate(categories):
        mask = df["stage"] == stage
        stage_values = df[mask]
        plt.errorbar(
            x=stage_values["loc"],
            y=stage_values["avg_time_ms"],
            yerr=stage_values["std_time_ms"],
            label=stage,
            marker=symbols[i],
            ms=marker_size,
            capsize=4,
        )

    plt.xlabel("Lines of code", fontsize=plot_font_size)
    plt.ylabel("Average Time (ms)", fontsize=plot_font_size)

    plt.xticks(fontsize=tick_font_size)
    plt.yticks(fontsize=tick_font_size)

    plt.legend(fontsize=legend_font_size)
    plt.grid(True)
    plt.tight_layout()
    plt.show()


def run_compiler(width, height, output_path):
    """Generates a MiniML-program of a given width and height and compiles it into LLVM IR,
    using Trieste with Info mode enabled.
    Info mode output is written to output_path"""

    generated_file = "examples/generated.miniml"
    with open(output_path, "w") as f:
        try:
            subprocess.run(
                [
                    "python",
                    "miniml_generator.py",
                    "--width",
                    str(width),
                    "--height",
                    str(height),
                    "-o",
                    generated_file,
                ],
                check=True,
            )

            subprocess.run(
                ["./build/miniml", "build", generated_file, "-l", "Info"],
                stdout=f,
                check=True,
            )
        except subprocess.CalledProcessError as e:
            print(f"Error running compiler: {e}")


def get_run_results(results_path):
    """Parses the pass results from the output of Trieste's Info mode"""

    results = []

    with open(results_path, "r") as f:
        block = re.search(r"---------\n(.*?)\n---------", f.read(), re.DOTALL)
        if not block:
            raise ValueError("Could not find table generated by Trieste's Info mode.")
        block_text = block.group(1)

        parse_pattern = re.compile(r"^Parse time\s+\(us\):\s+(\d+)")
        parse_time_us = parse_pattern.match(block_text).groups()[0]
        results.append(
            {
                "pass": "parse",
                "iterations": 0,
                "changes": 0,
                "time_us": int(parse_time_us),
            }
        )

        pattern = re.compile(r"(\w+)\s+(\d+)\s+(\d+)\s+(\d+)")
        for match in pattern.finditer(block_text):
            name, iterations, changes, time_us = match.groups()
            results.append(
                {
                    "pass": name,
                    "iterations": int(iterations),
                    "changes": int(changes),
                    "time_us": int(time_us),
                }
            )

    return results


if __name__ == "__main__":
    main()
