declare namespace Protocol {
  interface BaseConfig {
    version: string;
    timeout: number;
  }

  interface HTTP extends BaseConfig {
    keepAlive: boolean;
    maxHeaderSize: number;
  }

  interface SOCKS5 extends BaseConfig {
    auth: AuthMethod[];
    udpAssociate: boolean;
  }
} 