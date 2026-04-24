#!/bin/bash

# 检查 example 目录
if [ ! -d "./example" ]; then
    echo "错误: 找不到 ./example 目录"
    exit 1
fi

total=0
passed=0
failed=0

echo "开始自动化测试"
echo "--------------------------------------"

for file in ./example/*.scm; do
    [ -e "$file" ] || continue
    total=$((total + 1))
    filename=$(basename "$file")

    echo -n "正在测试 [$total]: $filename ... "

    input_content=""
    if [[ "$filename" == *"readnum"* ]]; then
        input_content="2"
    fi

    # 执行命令
    # echo "$input_content" 会通过管道传给你的程序
    # 如果 input_content 为空，程序会等待输入（或者如果程序没调用 read-num 则直接结束）
    output=$(echo "$input_content" | dune exec berkeley-cs164-arm-darwin -- "$file" 2>&1)
    exit_code=$?

    if [ $exit_code -eq 0 ]; then
        echo -e "\033[32m[OK]\033[0m"
        # 如果你想看 readnum 的输出结果，可以取消下面这行的注释
        # echo "  输出: $output"
        passed=$((passed + 1))
    else
        echo -e "\033[31m[FAIL]\033[0m"
        echo "  错误详情: $output"
        failed=$((failed + 1))
    fi
done

echo "--------------------------------------"
echo -e "测试总结: 总计 $total | 通过 \033[32m$passed\033[0m | 失败 \033[31m$failed\033[0m"
