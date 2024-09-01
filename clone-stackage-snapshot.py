#!/usr/bin/env python3

import requests
import os
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor, as_completed

# Sample snapshot for testing
snapshot0 = '''-- NOTE: Due to revisions, this file may not work. See:
-- https://github.com/fpco/stackage-server/issues/232

-- Stackage snapshot from: http://www.stackage.org/snapshot/nightly-2024-08-25
-- Please place this file next to your .cabal file as cabal.config
-- To only use tested packages, uncomment the following line:
-- remote-repo: stackage-nightly-2024-08-25:http://www.stackage.org/nightly-2024-08-25
with-compiler: ghc-9.8.2
constraints: abstract-deque ==0.3,
             abstract-deque-tests ==0.3,
             abstract-par ==0.3.3,
             AC-Angle ==1.0,
             acc ==0.2.0.3,
             ace ==0.6,
             acid-state ==0.16.1.3,
'''

def setup_logging(log_file_path):
    # Overwrites the log on every run, use "a" if you want to append
    log_file = open(log_file_path, "w")
    sys.stdout = log_file
    sys.stderr = log_file

# May require
# git config --global url."https://".insteadOf git://
# to work, as lots of packages have git://github.com instead of
# http://github.com in their source repo urls
def main(target_dir):
    # Replace with your desired snapshot URL
    snapshot_url = 'https://www.stackage.org/lts-22.33/cabal.config'

    print("Downloading stackage snapshot")
    response = requests.get(snapshot_url)
    snapshot = response.text

    if os.path.exists(target_dir):
        print(f"Target directory '{target_dir}' exists")
    else:
        print(f"Target directory '{target_dir}' does not exist. Creating it...")
        os.makedirs(target_dir, exist_ok=True)
        print(f"Directory '{target_dir}' created successfully")

    print("Parsing packages")
    packages = read_packages(snapshot)

    os.chdir(target_dir)

    print("Start cloning all packages")
    clone_packages(packages)
    print("Cloned all packages")

def read_constraints(snapshot):
    return [
        s.replace("constraints: ", "").strip()
        for s in snapshot.splitlines()
        if s.strip() and not s.startswith("--") and not s.startswith("with-compiler:")
    ]

def read_packages(snapshot):
    constraints = read_constraints(snapshot)
    packages = [
        s.split("==")[0].strip().rstrip(",")
        for s in constraints
    ]
    return packages

def clone_packages(packages):
    with ThreadPoolExecutor(max_workers=2) as executor:
        futures = {executor.submit(clone_package, package): package for package in packages}

        for future in as_completed(futures):
            package = futures[future]
            try:
                future.result()
            except Exception as e:
                print(f"An error occurred with package '{package}': {e}", file=sys.stderr)


def clone_package(package):
    command = ['cabal', 'get', '-s', package]

    if os.path.isdir(package):
        print(f"Directory '{package}' already exists. Skipping '{package}'.")
        return
    
    try:
        print(f"Clone package: {' '.join(command)}")
        result = subprocess.run(command, check=True, capture_output=True, text=True)
        
        print(result.stdout)
        if result.stderr:
            print(result.stderr, file=sys.stderr)
    
    except subprocess.CalledProcessError as e:
        print(f"Error occurred: {e}", file=sys.stderr)
        if e.stderr:
            print(f"Error message:\n{e.stderr}", file=sys.stderr)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python clone-stackage-snapshot.py <target_dir>")
        sys.exit(1)
    target_dir = sys.argv[1]
    setup_logging('clone.log')
    main(target_dir)
