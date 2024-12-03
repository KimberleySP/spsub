use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;
use tokio::fs::File;
use tokio::io::AsyncReadExt;
use async_trait::async_trait;

#[derive(Debug, Serialize, Deserialize)]
pub struct ProxyRule {
    pub domain: String,
    pub target: String,
    pub priority: i32,
    #[serde(default)]
    pub headers: HashMap<String, String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Config {
    pub rules: Vec<ProxyRule>,
    pub default_target: String,
    pub metrics_enabled: bool,
    pub log_level: String,
    #[serde(default)]
    pub advanced: AdvancedConfig,
}

#[derive(Debug, Serialize, Deserialize, Default)]
pub struct AdvancedConfig {
    pub tcp_keepalive: Option<u64>,
    pub buffer_size: Option<usize>,
    pub timeout: Option<u64>,
    pub retry_count: Option<u32>,
}

#[async_trait]
pub trait ConfigLoader {
    async fn load(&self) -> Result<Config, ConfigError>;
    async fn validate(&self, config: &Config) -> Result<(), ConfigError>;
}

#[derive(Debug)]
pub enum ConfigError {
    IoError(std::io::Error),
    ParseError(serde_yaml::Error),
    ValidationError(String),
}

pub struct YamlConfigLoader {
    path: String,
}

#[async_trait]
impl ConfigLoader for YamlConfigLoader {
    async fn load(&self) -> Result<Config, ConfigError> {
        let path = Path::new(&self.path);
        let mut file = File::open(path).await.map_err(ConfigError::IoError)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents).await.map_err(ConfigError::IoError)?;
        
        let config: Config = serde_yaml::from_str(&contents).map_err(ConfigError::ParseError)?;
        self.validate(&config).await?;
        Ok(config)
    }

    async fn validate(&self, config: &Config) -> Result<(), ConfigError> {
        if config.rules.is_empty() {
            return Err(ConfigError::ValidationError("No rules defined".into()));
        }

        for rule in &config.rules {
            if rule.domain.is_empty() {
                return Err(ConfigError::ValidationError("Empty domain in rule".into()));
            }
            // 更多验证逻辑...
        }

        Ok(())
    }
} 