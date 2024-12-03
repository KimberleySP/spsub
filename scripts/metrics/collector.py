from typing import Dict, List
import prometheus_client as prom
import time

class MetricsCollector:
    def __init__(self):
        self.request_counter = prom.Counter(
            'proxy_requests_total',
            'Total number of proxy requests'
        )
        
        self.latency_histogram = prom.Histogram(
            'proxy_request_duration_seconds',
            'Request duration in seconds',
            buckets=[.005, .01, .025, .05, .075, .1, .25, .5, .75, 1.0]
        ) 