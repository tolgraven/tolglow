#!/usr/bin/env bash
set -euo pipefail
export GH_REPO=tolgraven/tolglow GH_PAGER=cat
gh issue list --state open --label 'project: afterglow' --limit 4
gh issue list --state closed --label 'project: tolglow' --search 'sort:created-asc' --limit 3
gh issue view 227 --json state,title,url,body --jq '.state, .title, .url, .body' | sed '/^## Original requests/,$d'
gh issue view 200 --json state,title,url,body --jq '.state, .title, .url, .body' | sed '/^## Original requests/,$d'
gh issue view 199 --json state,title,url --jq '.state, .title, .url'
