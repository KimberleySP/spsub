use tokio;
use clap::{App, Arg};
use std::net::SocketAddr;
use crate::proxy::forward::ProxyForwarder;
use crate::monitor::metrics::MetricsCollector;
use crate::config::parser::{YamlConfigLoader, ConfigLoader};

mod proxy;
mod monitor;
mod config;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let matches = App::new("Advanced Proxy")
        .version("1.0")
        .author("Your Name")
        .about("Advanced proxy with metrics collection")
        .arg(
            Arg::with_name("config")
                .short('c')
                .long("config")
                .value_name("FILE")
                .help("Sets a custom config file")
                .takes_value(true),
        )
        .get_matches();

    // 初始化日志
    env_logger::init();

    // 加载配置
    let config_path = matches.value_of("config").unwrap_or("config.yml");
    let loader = YamlConfigLoader { path: config_path.to_string() };
    let config = loader.load().await?;

    // 初始化指标收集器
    let collector = MetricsCollector::new();
    collector.start_collector().await;

    // 启动代理服务
    let addr: SocketAddr = "127.0.0.1:25500".parse()?;
    let forwarder = ProxyForwarder::new(
        addr,
        config.default_target,
        25500,
    ).await?;

    forwarder.start().await?;

    Ok(())
} 