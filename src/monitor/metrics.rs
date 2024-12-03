use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use tokio::time::{Duration, interval};
use prometheus::{
    IntCounter, IntGauge, Histogram, HistogramOpts,
    register_int_counter, register_int_gauge, register_histogram,
};
use lazy_static::lazy_static;

lazy_static! {
    pub static ref PROXY_REQUESTS: IntCounter = register_int_counter!(
        "proxy_requests_total",
        "Total number of proxy requests"
    ).unwrap();

    pub static ref ACTIVE_CONNECTIONS: IntGauge = register_int_gauge!(
        "active_connections",
        "Number of active proxy connections"
    ).unwrap();

    pub static ref REQUEST_DURATION: Histogram = register_histogram!(
        "request_duration_seconds",
        "Request duration in seconds",
        vec![0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0]
    ).unwrap();
}

#[derive(Clone)]
pub struct MetricsCollector {
    pub total_bytes: Arc<AtomicU64>,
    pub error_count: Arc<AtomicU64>,
}

impl MetricsCollector {
    pub fn new() -> Self {
        Self {
            total_bytes: Arc::new(AtomicU64::new(0)),
            error_count: Arc::new(AtomicU64::new(0)),
        }
    }

    pub async fn start_collector(&self) {
        let bytes = self.total_bytes.clone();
        let errors = self.error_count.clone();
        
        tokio::spawn(async move {
            let mut interval = interval(Duration::from_secs(15));
            loop {
                interval.tick().await;
                let current_bytes = bytes.load(Ordering::Relaxed);
                let current_errors = errors.load(Ordering::Relaxed);
                
                // 发送指标到 Prometheus
                PROXY_REQUESTS.inc();
                REQUEST_DURATION.observe(current_bytes as f64 / 1_000_000.0);
                
                if current_errors > 0 {
                    log::error!("Detected {} errors in last interval", current_errors);
                }
            }
        });
    }
} 