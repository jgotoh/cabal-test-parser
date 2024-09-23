#!/usr/bin/env python3

import sys
import os
import requests

def setup_logging(log_file_path):
    # Overwrites the log on every run, use "a" if you want to append
    log_file = open(log_file_path, "w")
    sys.stdout = log_file
    sys.stderr = log_file

def main():
    github_token = os.getenv('GITHUB_TOKEN')

    if not github_token:
        raise ValueError("GITHUB_TOKEN environment variable is not set")

    search_response = search_project_files(github_token)
    if search_response.status_code != 200:
        print(f"Searching github failed '{search_response.status_code}, {search_response.text}'", file=sys.stderr)
        return
    search_data = search_response.json()

    print(f"Total items:{search_data.get('total_count')}")
    items = search_data.get('items', [])

    # TODO mit cabal-test-parser herausfinden wie sich das verhält wenn
    # ein cabal.project.dev in einem Ordner liegt -wirds geparsed?
    # am besten einen Fehler einbauen (bspw mit mixed styles , und ohne), damit
    # auf jeden fall legacy != parser vorkommt

    # for item in data.get('items', []):
        # print(f"File: {item['name']}, Path: {item['path']}, Repository: {item['repository']['full_name']}")

def search_project_files(github_token):
    url = "https://api.github.com/search/code"
    headers = {
        "Authorization": f"Bearer {github_token}",
    }
    query = "filename:cabal.project"
    params = {
        "q": query
    }
    response = requests.get(url, headers=headers, params=params, timeout=10)
    return response

if __name__ == "__main__":
    setup_logging('dl-github.log')
    main()