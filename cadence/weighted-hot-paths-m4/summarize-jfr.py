import collections
import json
import re
import sys

def milliseconds(value):
    h, m, s, ms = [int(part) for part in re.split(r"[:.]", value)]
    return ((60*h+m)*60+s)*1000+ms

def samples(lines):
    current = []
    for line in lines:
        if line.strip() == "}":
            block = "".join(current)
            thread = re.search(r'sampledThread = "([^"]+)"', block)
            time = re.search(r'startTime = ([0-9:.]+)', block)
            frames = [match[1] for match in re.finditer(r"^    ([^\s(]+)\(", block, re.M)]
            if time and thread and frames:
                yield milliseconds(time[1]), thread[1], frames
            current.clear()
        else:
            current.append(line)

def frame_name(frame):
    return re.sub(r"\.(?:invokeStatic|invoke|doInvoke)$", "", frame)

data = list(samples(sys.stdin))
first_workers = min(time for time, name, _ in data if name.startswith("afterglow-runtime-"))
names = collections.Counter()
owners = collections.defaultdict(collections.Counter)
sites = collections.defaultdict(collections.Counter)
containing = collections.defaultdict(collections.Counter)
for time, name, frames in data:
    if not first_workers + 10000 <= time <= first_workers + 30500:
        continue
    group = re.sub(r"\d+$", "*", name)
    names[group] += 1
    sites[group][frame_name(frames[0])] += 1
    project_frames = [frame_name(frame) for frame in frames if frame.startswith(("afterglow.", "tolglow."))]
    owners[group][project_frames[0] if project_frames else "(unattributed)"] += 1
    containing[group].update(set(project_frames))
print(json.dumps({
    "filteredSamples": names,
    "nearestProjectMethod": {group: counts.most_common(18) for group, counts in owners.items()},
    "stackContains": {group: counts.most_common(18) for group, counts in containing.items()},
    "leaf": {group: counts.most_common(12) for group, counts in sites.items()},
}, indent=2))
