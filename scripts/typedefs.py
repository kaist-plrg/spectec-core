from enum import Enum
from typing import Dict, List, Tuple, NewType, Optional, TypedDict

#
# Type aliases
#
PID = int
Origin = str  # function or relation that the PID belongs to
Filepath = NewType("Filepath", str)
Basename = NewType("Basename", str)
Directory = NewType("Directory", str)


class Status(Enum):
    HIT_LIKELY = 1
    HIT_UNLIKELY = 2
    CLOSE_MISS = 3
    COMPLETE_MISS = 4


CoverageEntry = Tuple[Status, List[Filepath]]
Coverage = Dict[Origin, Dict[PID, CoverageEntry]]
Reductions = Dict[PID, List[Filepath]]


class CReduceConfigs(TypedDict):
    p4spectec_dir: Directory
    cores: Optional[int]
    timeout_interesting: int
    timeout_creduce: int
