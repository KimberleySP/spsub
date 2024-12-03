#!/bin/bash

# 检查系统环境
check_system() {
    if [ -f /etc/os-release ]; then
        . /etc/os-release
        OS=$NAME
    else
        echo "无法确定操作系统类型"
        exit 1
    fi
}

# 安装依赖
install_dependencies() {
    case $OS in
        "Ubuntu"|"Debian GNU/Linux")
            apt update
            apt install -y docker.io docker-compose
            ;;
        "CentOS Linux")
            yum install -y docker docker-compose
            ;;
        *)
            echo "不支持的操作系统: $OS"
            exit 1
            ;;
    esac
}

# 启动服务
start_services() {
    docker-compose up -d
}

main() {
    check_system
    install_dependencies
    start_services
}

main 