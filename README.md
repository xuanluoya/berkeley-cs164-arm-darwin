# Berkeley CS164 Arm64/Darwin

本项目是对加州大学伯克利分校CS164-2025《编程语言与编译器》课程（2025年春季）的重构，专门针对Apple Silicon（ARM64）macOS系统

本项目实现了一个简单的 Scheme like 编译器，目标平台为 Apple Silicon（ARM64）汇编

## 与课程差异

1. 目标架构：ARM64 ，与X86的实现方法基本相同但使用Arm64重构
2. 改为类 Scheme 语法

## 构建和安装

### 环境

- 搭载Apple Silicon（ARM64）处理器的macOS系统
- OCaml 4.14+（包含 dune ）
- Clang/LLVM

### 从源码构建和运行

```bash
# 克隆仓库
git clone --depth=1 https://github.com/xuanluoya/berkeley-cs164-arm-darwin.git
cd berkeley-cs164-arm-darwin

# 构建编译器
dune build

# 运行实例
dune exec berkeley-cs164-arm-darwin ./example/rec_func.scm

# REPL
dune exec berkeley-cs164-arm-darwin
```

## 资源

- **原课程**：[编程语言与编译器 - CS 164 @ 加州大学伯克利分校，2025年春季](https://berkeley-cs164-sp25.github.io/)
- **课程仓库**：[berkeley-cs164-2025](https://github.com/berkeley-cs164-2025/)
- **课程视频**：[BiliBili录播](https://www.bilibili.com/video/BV1EWqrBBEwM)

## LICENSE

[MIT](LICENSE)
