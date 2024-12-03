use tokio::net::{TcpListener, TcpStream};
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use futures::future::try_join;
use std::error::Error;
use std::net::SocketAddr;
use bytes::{BytesMut, BufMut};
use trust_dns_resolver::AsyncResolver;
use trust_dns_resolver::config::*;

const BUFFER_SIZE: usize = 32 * 1024; // 32KB

pub struct ProxyForwarder {
    listen_addr: SocketAddr,
    target_host: String,
    target_port: u16,
    resolver: AsyncResolver,
}

impl ProxyForwarder {
    pub async fn new(listen: SocketAddr, host: String, port: u16) -> Result<Self, Box<dyn Error>> {
        let resolver = AsyncResolver::tokio(
            ResolverConfig::default(),
            ResolverOpts::default(),
        )?;

        Ok(Self {
            listen_addr: listen,
            target_host: host,
            target_port: port,
            resolver,
        })
    }

    pub async fn start(&self) -> Result<(), Box<dyn Error>> {
        let listener = TcpListener::bind(self.listen_addr).await?;
        log::info!("Proxy listening on {}", self.listen_addr);

        loop {
            let (inbound, peer_addr) = listener.accept().await?;
            log::debug!("New connection from {}", peer_addr);

            let target_host = self.target_host.clone();
            let target_port = self.target_port;
            let resolver = self.resolver.clone();

            tokio::spawn(async move {
                if let Err(e) = handle_connection(inbound, target_host, target_port, resolver).await {
                    log::error!("Connection error: {}", e);
                }
            });
        }
    }
}

async fn handle_connection(
    mut inbound: TcpStream,
    target_host: String,
    target_port: u16,
    resolver: AsyncResolver,
) -> Result<(), Box<dyn Error>> {
    // DNS解析
    let ips = resolver.lookup_ip(target_host.as_str()).await?;
    let ip = ips.iter().next().ok_or("No IP resolved")?;
    
    // 连接目标服务器
    let mut outbound = TcpStream::connect((ip, target_port)).await?;

    let (mut ri, mut wi) = inbound.split();
    let (mut ro, mut wo) = outbound.split();

    let client_to_server = async {
        let mut buffer = BytesMut::with_capacity(BUFFER_SIZE);
        loop {
            let n = ri.read_buf(&mut buffer).await?;
            if n == 0 { break; }
            
            wo.write_all(&buffer[..n]).await?;
            buffer.clear();
        }
        wo.shutdown().await?;
        Ok::<_, Box<dyn Error>>(())
    };

    let server_to_client = async {
        let mut buffer = BytesMut::with_capacity(BUFFER_SIZE);
        loop {
            let n = ro.read_buf(&mut buffer).await?;
            if n == 0 { break; }
            
            wi.write_all(&buffer[..n]).await?;
            buffer.clear();
        }
        wi.shutdown().await?;
        Ok::<_, Box<dyn Error>>(())
    };

    try_join(client_to_server, server_to_client).await?;
    Ok(())
} 