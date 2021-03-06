#!/usr/bin/python3

import sys
from pathlib import Path
import re
import subprocess


goblint_root = Path(".")
goblint_regression = goblint_root / "tests" / "regression"

EXCLUDE_TASKS = [
    "04-mutex_13-failed_locking",
    "04-mutex_31-uninitialized",
    "04-mutex_49-type-invariants",
    "04-mutex_29-funstruct_rc",
    "04-mutex_30-funstruct_nr",
    "04-mutex_30-funstruct_nr",

    "06-symbeq_16-type_rc", # extern
    "06-symbeq_17-type_nr", # extern
    "06-symbeq_20-mult_accs_nr", # extern
    "06-symbeq_21-mult_accs_rc", # extern
    "10-synch_04-two_mainfuns", # no main
    "05-lval_ls_17-per_elem_simp", # no pthread include, locksmith pragma

    "09-regions_29-malloc_race_cp", # duplicate of 02/25
    "09-regions_30-list2alloc-offsets", # duplicate of 09/28
    "09-regions_31-equ_rc", # duplicate of 06/10
    "09-regions_32-equ_nr", # duplicate of 06/11
    "09-regions_34-escape_rc", # duplicate of 04/45
    "09-regions_35-list2_rc-offsets-thread", # duplicate of 09/03
    "10-synch_17-glob_fld_nr", # duplicate of 05/08
    "19-spec_02-mutex_rc", # duplicate of 04/01

    "29-svcomp_01-race-2_3b-container_of", # duplicate sv-benchmarks
    "29-svcomp_01-race-2_4b-container_of", # duplicate sv-benchmarks
    "29-svcomp_01-race-2_5b-container_of", # duplicate sv-benchmarks
]

target_root = Path(sys.argv[1])

for goblint_f in sorted(goblint_regression.glob("**/*.c")):
    print(goblint_f, end=": ")

    content = goblint_f.read_text()
    # handle & strip Goblint param hints
    m = re.match(r"^//(.*?)\n(.*)$", content, flags=re.DOTALL)
    if m:
        top_comment = m.group(1)
        content = m.group(2)
    else:
        top_comment = None

    if top_comment is not None:
        if "SKIP" in top_comment:
            print("skip")
            continue
        elif "--set kernel true" in top_comment:
            print("kernel")
            continue
        elif "osek" in top_comment:
            print("osek")
            continue
        elif "--set allfuns true" in top_comment:
            print("allfuns")
            continue

    task_name = Path(goblint_f.parent.name + "_" + goblint_f.name).stem
    if task_name in EXCLUDE_TASKS:
        print("exclude")
        continue

    properties = {}

    content = re.sub(r"//\s*RACE(?!!)", "// NORACE", content)
    if re.search(r"//\s*RACE!", content):
        properties["../properties/no-data-race.prp"] = False
    elif re.search(r"//\s*NORACE", content):
        # if didn't contain RACE!, must be race-free
        properties["../properties/no-data-race.prp"] = True

    if re.search(r"assert_racefree[^\n]*//\s*UNKNOWN", content):
        properties["../properties/unreach-call.prp"] = False
    elif "assert_racefree" in content:
        # if didn't contain UNKNOWN assert_racefree, must be race-free
        properties["../properties/unreach-call.prp"] = True

    # TODO: unreach-call property based on asserts

    if properties:
        print()
        for property_file, expected_verdict in properties.items():
            print(f"  {property_file}: {expected_verdict}")

        # copy file
        target_f = target_root / (task_name + ".c")
        print(f"  -> {target_f}")
        target_f.write_text(content)

        # preprocess file
        preprocessed_f = target_root / (task_name + ".i")
        print(f"  -> {preprocessed_f}")
        preprocessed_f.touch()
        with preprocessed_f.open("w") as f:
            # running gcc in target_root with relative path avoid absolute paths in preprocessor
            subprocess.run(["gcc", "-E", "-P", "-m32", str(target_f.relative_to(target_root))], stdout=f, check=True, cwd=target_root)

        # create task definition
        task_definition_f = target_root / (task_name + ".yml")
        task_definition_f.touch()
        # write yml manually to get consistent formatting
        with task_definition_f.open("w") as f:
            f.write("format_version: '2.0'\n")
            f.write("\n")
            if top_comment:
                f.write(f"# original top comment: {top_comment}\n")
            f.write(f"input_files: '{preprocessed_f.relative_to(target_root)}'\n")
            f.write("\n")
            f.write("properties:\n")
            for property_file, expected_verdict in properties.items():
                f.write(f"  - property_file: {property_file}\n")
                f.write(f"    expected_verdict: {'true' if expected_verdict else 'false'}\n")
            f.write("\n")
            f.write("options:\n")
            f.write("  language: C\n")
            f.write("  data_model: ILP32\n") # TODO: is this right for Goblint tests?

    else:
        print("no properties")
