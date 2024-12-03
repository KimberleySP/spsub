from typing import Dict, Any
import ctypes
from pathlib import Path
import json

# 加载各语言的metrics收集库
class MetricsBridge:
    def __init__(self):
        self.rust_metrics = ctypes.CDLL(
            str(Path(__file__).parent / "../../target/release/libmetrics_rs.so")
        )
        self.scala_stats = ctypes.CDLL(
            str(Path(__file__).parent / "../../lib/libstats_scala.so")
        )
        self.go_limiter = ctypes.CDLL(
            str(Path(__file__).parent / "../../lib/libratelimit.so")
        )
        
    def collect_all_metrics(self) -> Dict[str, Any]:
        metrics = {}
        
        # 收集Rust核心指标
        rust_data = self.rust_metrics.get_metrics()
        metrics["core"] = json.loads(ctypes.string_at(rust_data))
        
        # 收集Scala统计数据
        scala_data = self.scala_stats.get_windowed_stats()
        metrics["stats"] = json.loads(ctypes.string_at(scala_data))
        
        # 收集Go限流器状态
        limiter_data = self.go_limiter.get_limiter_status()
        metrics["rate_limit"] = json.loads(ctypes.string_at(limiter_data))
        
        return metrics 