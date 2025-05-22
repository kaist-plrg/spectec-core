import os
import re
from pathlib import Path
from typing import List, Tuple, Optional
from datetime import datetime, timedelta

LogEntry = Tuple[datetime, str]


def parse_timestamp(ts: str) -> datetime:
    return datetime.strptime(ts, "%H:%M:%S")


def assign_synthetic_times(times: List[str], base_time: datetime) -> List[datetime]:
    """Assign synthetic datetime values with monotonic offsets, accounting for rollover."""
    parsed = [parse_timestamp(t) for t in times]
    result = []
    offset = timedelta()
    last = parsed[0]

    for t in parsed:
        if t < last:
            offset += timedelta(days=1)
        result.append(base_time + (t - parsed[0]) + offset)
        last = t

    return result


def fuzz_log_sort_key(file: Path) -> Tuple[int, str]:
    name = file.name
    if name == "init.log":
        return (0, "")  # highest priority
    if name.startswith("fuel"):
        num = int(re.findall(r"\d+", name)[0])
        return (1, -num)  # reverse fuel order: fuel10 before fuel1
    return (2, name)  # fallback


def parse_fuzzer_log(log_file: Path, phase_start: datetime) -> List[LogEntry]:
    """Parse fuzzer logs like fuzz*/log/init.log or fuelX.log"""
    coverage_pattern = re.compile(r"\[\[(\d{2}:\d{2}:\d{2})\]\].*overage (\d+)/(\d+)")
    entries: List[LogEntry] = []
    times = []
    messages = []

    with open(log_file) as f:
        for line in f:
            match = coverage_pattern.search(line)
            if match:
                t = match.group(1)
                cov = int(match.group(2))
                times.append(t)
                messages.append(f"Fuzzer phase: {cov}")

    synthetic_times = assign_synthetic_times(times, phase_start)
    return list(zip(synthetic_times, messages))


def parse_reducer_log(log_file: Path, phase_start: datetime) -> List[LogEntry]:
    """Parse reducer logs like reduce*/reducer.log"""
    time_pattern = re.compile(r"\[(\d{2}:\d{2}:\d{2})\]")
    times = []

    with open(log_file) as f:
        for line in f:
            match = time_pattern.search(line)
            if match:
                times.append(match.group(1))

    synthetic_times = assign_synthetic_times(times, phase_start)
    return [(t, "Reducer phase") for t in synthetic_times]


def get_start_time(base_dir: Path) -> Optional[datetime]:
    reducer_log = base_dir / "reduce0" / "reducer.log"
    if reducer_log.exists():
        with open(reducer_log) as f:
            for line in f:
                match = re.search(r"\[(\d{2}:\d{2}:\d{2})\]", line)
                if match:
                    return parse_timestamp(match.group(1))
    else:
        fuzzer_log = base_dir / "fuzz0" / "log" / "init.log"
        with open(fuzzer_log) as f:
            for line in f:
                match = re.search(r"\[\[(\d{2}:\d{2}:\d{2})\]\]", line)
                if match:
                    return parse_timestamp(match.group(1))
    return None


def summarize_interval(entries: List[LogEntry], interval_minutes: int) -> None:
    end_time = max(ts for ts, _ in entries)
    start_time = min(ts for ts, _ in entries)
    idx = 0
    current_time = start_time

    reduce = False
    if entries[0][1].startswith("Reducer"):
        reduce = True

    while current_time <= end_time:
        window_entries = []

        while idx < len(entries) and entries[idx][0] <= current_time:
            if entries[idx][1].startswith("Fuzzer") and entries[idx - 1][1].startswith(
                "Reducer"
            ):
                coverage = int(entries[idx][1].split(":")[1].strip())
                print(f"{entries[idx][0] - start_time}, {coverage}")
            elif entries[idx][1].startswith("Reducer") and entries[idx - 1][
                1
            ].startswith("Fuzzer"):
                coverage = int(entries[idx - 1][1].split(":")[1].strip())
                print(f"{entries[idx-1][0] - start_time}, {coverage}")
            window_entries.append(entries[idx])
            idx += 1

        last_time, last_msg = window_entries[-1]

        timestamp = current_time - start_time
        if last_msg.startswith("Fuzzer"):
            coverage = int(last_msg.split(":")[1].strip())
            print(f"{timestamp}, {coverage}")

        current_time += timedelta(minutes=interval_minutes)


def load_phase_logs(base_dir: Path, start_time: datetime) -> List[LogEntry]:
    """Load and combine logs in phase order: reduce0 → fuzz0 → reduce1 → fuzz1 → ..."""
    all_entries: List[LogEntry] = []
    current_time = start_time

    i = 0
    while True:
        reduce_log = base_dir / f"reduce{i}" / "reducer.log"
        fuzz_dir = base_dir / f"fuzz{i}" / "log"
        phase_had_data = False

        # Reducer phase
        if reduce_log.exists():
            entries = parse_reducer_log(reduce_log, current_time)
            if entries:
                all_entries.extend(entries)
                current_time = entries[-1][0] + timedelta(seconds=1)
                phase_had_data = True

        # Fuzzer phase
        if fuzz_dir.exists():
            for log_file in sorted(fuzz_dir.glob("*.log"), key=fuzz_log_sort_key):
                entries = parse_fuzzer_log(log_file, current_time)
                if entries:
                    all_entries.extend(entries)
                    current_time = entries[-1][0] + timedelta(seconds=1)
                    phase_had_data = True

        if not phase_had_data:
            break

        i += 1

    return all_entries


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description="Summarize fuzzer coverage by interval."
    )
    parser.add_argument(
        "dir", type=Path, help="Top-level directory (contains fuzz*/ and reduce*/)"
    )
    args = parser.parse_args()

    base_dir = args.dir
    start_time = get_start_time(base_dir)
    # print(f"\n[INFO] Start time: {start_time.strftime('%H:%M:%S') if start_time else 'Unknown'}")
    entries = load_phase_logs(base_dir, start_time)
    # for entry in entries:
    #     print(f"[{entry[0].strftime('%Y-%m-%d %H:%M:%S')}] {entry[1]}")
    # print(f"\n[INFO] Collected {len(entries)} log entries.")
    summarize_interval(entries, interval_minutes=5)


