# JavDB 预告片观看助手

<p align="center">
  <img src="https://img.shields.io/github/license/KimberleySP/spsub" alt="license">
  <img src="https://img.shields.io/github/stars/KimberleySP/spsub" alt="stars">
  <img src="https://img.shields.io/github/forks/KimberleySP/spsub" alt="forks">
</p>

> 一个基于 Clash 的 JavDB 预告片访问解决方案

[English](./README_EN.md) | 简体中文

## 📖 目录

- [项目简介](#-项目简介)
- [功能特性](#-功能特性)
- [快速开始](#-快速开始)
- [详细部署](#-详细部署)
- [使用指南](#-使用指南)
- [常见问题](#-常见问题)
- [贡献指南](#-贡献指南)
- [开源协议](#-开源协议)

## 📝 项目简介

本项目旨在解决 JavDB 网站无法正常观看预告片的问题。通过配置特定的分流规则，实现流畅访问预告片资源。项目基于 [sub-web](https://github.com/CareyWang/sub-web) 和 [subconverter](https://github.com/tindy2013/subconverter) 开发。

### 系统要求
- Clash 代理工具
- Node.js >= 14 (仅开发环境需要)
- Docker (可选)

## ✨ 功能特性

- 🚀 支持 Clash 代理工具
- 🔄 自定义分流规则配置
- 🎯 针对性优化日本媒体资源访问
- 📱 支持多平台（Windows/macOS/Linux）
- 🔒 安全的 API 访问控制

## 🚀 快速开始

### 使用 Docker 快速部署（推荐）

1. 部署 sub-web：
```bash
docker run -d --name=sub-web \
  -p 80:80 \
  -e API_URL=http://你的subconverter地址:25500 \
  --restart=always \
  careywong/sub-web:latest
```

2. 部署 subconverter：
```bash
docker run -d --name=subconverter \
  -p 25500:25500 \
  -v /path/to/config:/base/config \
  --restart=always \
  tindy2013/subconverter:latest
```

3. 配置 Clash：
   - 访问部署的 sub-web 站点
   - 填入订阅地址
   - 使用远程配置：`https://raw.githubusercontent.com/KimberleySP/spsub/refs/heads/main/spsub.ini`
   - 生成并导入 Clash 配置

## 📖 详细部署

### 1. 搭建 sub-web

#### 1.1 环境要求
- Node.js 版本 >= 14
- Git
- yarn 或 npm 包管理器

#### 1.2 手动部署步骤

```bash
# 克隆项目
git clone https://github.com/CareyWang/sub-web

# 进入项目目录
cd sub-web

# 安装依赖（二选一）
yarn install
# 或
npm install

# 修改后端地址（重要）
```

编辑 `src/views/SubConvert.vue` 文件，找到 `backendUrl` 变量，修改为你的 subconverter 地址：
```javascript
const backendUrl = "http://你的域名:25500";
```

```bash
# 开发环境运行（二选一）
yarn serve
# 或
npm run serve

# 生产环境构建（二选一）
yarn build
# 或
npm run build
```

#### 1.3 部署说明

开发环境：
- 运行后访问 http://localhost:8080
- 修改代码后自动热重载

生产环境：
1. 将 dist 目录下的文件部署到网站根目录
2. 推荐使用 Nginx 进行部署，基本配置如下：

```nginx
server {
    listen 80;
    server_name sub-web.你的域名.com;
    root /path/to/sub-web/dist;
    index index.html;
    
    # 启用 gzip 压缩
    gzip on;
    gzip_types text/plain text/css application/json application/javascript text/xml application/xml application/xml+rss text/javascript;
    
    # 处理前端路由
    location / {
        try_files $uri $uri/ /index.html;
    }
    
    # 反向代理到 subconverter（可选）
    location /sub {
        proxy_pass http://127.0.0.1:25500;
        proxy_set_header Host $http_host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    }
}
```

### 2. 搭建 subconverter

#### 2.1 环境要求
- 支持 amd64/arm64 的 Linux/macOS/Windows 系统
- 如果要自行编译，需要 C++ 编译环境

#### 2.2 手动部署步骤

1. 下载预编译文件：
   - 访问 [releases 页面](https://github.com/tindy2013/subconverter/releases)
   - 下载对应系统版本：
     - Windows: subconverter_windows64.7z
     - Linux: subconverter_linux64.tar.gz
     - macOS: subconverter_darwin64.tar.gz

2. 解压文件：
```bash
# Linux/macOS
tar -zxvf subconverter_linux64.tar.gz

# Windows
使用解压软件解压 subconverter_windows64.7z
```

3. 运行程序：
```bash
# Linux/macOS
cd subconverter
chmod +x subconverter   # 添加执行权限
./subconverter          # 运行

# Windows
进入解压目录，双击运行 subconverter.exe
```

#### 2.3 配置说明
主配置文件 pref.ini 重要配置项：
```ini
[common]
# API模式，设置为true可以防止直接访问本地文件
api_mode=false

# API访问密钥，如果设置了api_mode，需要配置此项
api_access_token=password

# 监听地址，建议使用0.0.0.0以允许远程访问
listen=0.0.0.0

# 监听端口
port=25500
```

## 📚 使用指南

1. 打开已部署的 sub-web 网站
2. 在订阅链接处填入你的代理服务订阅地址
3. 在远程配置中填入：
   ```
   https://raw.githubusercontent.com/KimberleySP/spsub/refs/heads/main/spsub.ini
   ```
4. 点击"生成订阅"按钮，获取转换后的订阅地址
5. 在 Clash 中配置：
   - 导入生成的订阅地址
   - 在 Proxy 分流规则中选择除日本以外的节点
   - 在 DMM 分流规则中选择日本节点

## ❓ 常见问题

### sub-web 相关问题
- Node.js 版本不兼容：确保版本 >= 14
- 依赖安装失败：尝试清除 node_modules 后重新安装
- 端口被占用：修改配置或关闭占用端口的程序

### subconverter 相关问题
- 启动失败：
  - 检查程序权限：`chmod +x subconverter`
  - 检查端口占用：`netstat -tunlp | grep 25500`
  - 查看日志：`tail -f subconverter.log`
- 转换失败：
  - 确认原始订阅可访问
  - 检查服务运行状态
  - 查看错误日志

## 🤝 贡献指南

欢迎提交 Issue 和 Pull Request 来帮助改进项目。

### 提交 Issue
- 使用清晰的标题描述问题
- 详细描述问题或建议
- 如果可能，提供截图或错误日志

### 提交 PR
1. Fork 本仓库
2. 创建新的分支：`git checkout -b feature/xxxx`
3. 提交改动：`git commit -am 'Add some feature'`
4. 推送到分支：`git push origin feature/xxxx`
5. 提交 Pull Request

## 📄 开源协议

本项目采用 [MIT](LICENSE) 协议开源。

## 🙏 鸣谢

- [sub-web](https://github.com/CareyWang/sub-web)
- [subconverter](https://github.com/tindy2013/subconverter)
- [Clash](https://github.com/Dreamacro/clash)

---

> 注意：本项目仅供学习研究使用，请遵守当地法律法规。