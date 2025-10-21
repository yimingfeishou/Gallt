//Copyright (c) Gallt Developer.
//版权所有（c）Gallt 开发者。

#include "common.h"
#include "tokenizer.h"
#include "parser.h"
#include "semantic_analyzer.h"
#include "interpreter.h"
#include <fstream>
#include <iostream>
#include <string>
#include <filesystem>
#include <atomic>

namespace fs = std::filesystem;

// 读取源代码文件内容（保持原有逻辑，严格检查.glt扩展名）
std::optional<std::string> read_source_file(const std::string& file_path) {
    // 检查文件扩展名是否为.glt
    if (fs::path(file_path).extension() != GalltConstants::FILE_EXTENSION) {
        std::cerr << "错误：文件必须是" << GalltConstants::FILE_EXTENSION << "格式\n";
        return std::nullopt;
    }

    // 检查文件是否存在且可读取
    if (!fs::exists(file_path) || !fs::is_regular_file(file_path)) {
        std::cerr << "错误：文件 " << file_path << " 不存在或不是普通文件\n";
        return std::nullopt;
    }

    // 打开文件（二进制模式避免换行符转换问题）
    std::ifstream file(file_path, std::ios::in | std::ios::binary);
    if (!file.is_open()) {
        std::cerr << "错误：无法打开文件 " << file_path << " 权限不足或文件损坏\n";
        return std::nullopt;
    }

    // 读取文件内容（全部读取到字符串）
    std::string source((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    file.close();

    if (source.empty()) {
        std::cerr << "警告：文件 " << file_path << " 为空，无代码可执行\n";
    }

    return source;
}

// 打印错误信息（统一格式，包含阶段、错误码、位置和详情）
void print_error(const std::string& stage, ErrorCode code, const SourceLocation& loc, const std::string& msg) {
    std::cerr << "\n[" << stage << "错误] "
        << CommonUtils::get_error_message(code) << "\n"
        << "位置：" << loc.to_string() << "\n"
        << "详情：" << msg << "\n\n";
}

// 信号处理函数（兜底：确保Ctrl+C能正确中断主程序）
void sigint_handler_main(int signum) {
    if (signum == SIGINT) {
        g_is_ctrl_c = true;
        std::cout << "\n\n[提示] 收到Ctrl+C中断信号，正在停止程序...\n";
    }
}

int main(int argc, char* argv[]) {
    // 1. 初始化：打印编译器信息+注册全局信号处理
    std::cout << "=============================================\n";
    std::cout << "版权所有（c）Gallt开发者\n";
    std::cout << "Copyright (c) Gallt Developer\n";
    std::cout << "Gallt版本 Gallt Preview Version 0.0.0.1\n";
    std::cout << "Pisces版本 Pisces-glt-pre.ver-0.0.0.1";
    std::cout << "支持文件格式：" << GalltConstants::FILE_EXTENSION << "\n";
    std::cout << "按Ctrl+C可中断无限循环或事件监听\n";
    std::cout << "=============================================\n\n";

    // 注册主程序Ctrl+C信号处理（与tokenizer的信号处理互补）
    std::signal(SIGINT, sigint_handler_main);

    // 2. 检查命令行参数（必须传入1个源文件路径）
    if (argc != 2) {
        std::cerr << "用法错误：" << argv[0] << " <源文件路径" << GalltConstants::FILE_EXTENSION << ">\n";
        std::cerr << "示例：" << argv[0] << " test.glt\n";
        return 1;
    }
    std::string file_path = argv[1];

    // 3. 读取源文件
    std::cout << "[步骤1/5] 读取源文件：" << file_path << "\n";
    auto source_opt = read_source_file(file_path);
    if (!source_opt.has_value()) {
        std::cerr << "[失败] 源文件读取失败\n";
        return 1;
    }
    std::string source_code = source_opt.value();
    std::cout << "[成功] 读取完成（大小：" << source_code.size() << " 字节）\n\n";

    // 4. 词法分析（生成Token流）
    std::cout << "[步骤2/5] 执行词法分析...\n";
    Tokenizer tokenizer(source_code);
    // 词法分析无致命错误（语法分析阶段会捕获无效Token）
    std::cout << "[成功] 词法分析完成，无无效字符或未闭合注释\n\n";

    // 5. 语法分析（生成AST）
    std::cout << "[步骤3/5] 执行语法分析...\n";
    Parser parser(tokenizer);
    std::unique_ptr<ProgramNode> program;
    try {
        program = parser.parse_program();
    }
    catch (const ParseError& e) {
        print_error("语法", e.error_code, e.location, e.what());
        std::cerr << "[失败] 语法分析中断\n";
        return 1;
    }
    const auto& parse_errors = parser.get_errors();
    if (!parse_errors.empty()) {
        std::cerr << "[失败] 语法分析发现" << parse_errors.size() << "个错误：\n";
        for (const auto& err : parse_errors) {
            print_error("语法", err.error_code, err.location, err.what());
        }
        return 1;
    }
    std::cout << "[成功] 语法分析完成，AST构建成功\n\n";

    // 6. 语义分析（类型检查、符号校验）
    std::cout << "[步骤4/5] 执行语义分析...\n";
    SemanticAnalyzer semantic_analyzer;
    semantic_analyzer.analyze(program.get());
    const auto& semantic_errors = semantic_analyzer.get_errors();
    if (!semantic_errors.empty()) {
        std::cerr << "[失败] 语义分析发现" << semantic_errors.size() << "个错误：\n";
        for (const auto& err : semantic_errors) {
            print_error("语义", err.code, err.location, err.message);
        }
        return 1;
    }
    std::cout << "[成功] 语义分析完成，无类型错误或未定义符号\n\n";

    // 7. 解释执行（运行AST）
    std::cout << "[步骤5/5] 开始解释执行...\n";
    std::cout << "---------------------------------------------\n";
    Interpreter interpreter;
    interpreter.execute(program.get());
    const auto& runtime_errors = interpreter.get_errors();
    if (!runtime_errors.empty()) {
        std::cerr << "\n[失败] 执行过程中发现" << runtime_errors.size() << "个运行时错误：\n";
        for (const auto& err : runtime_errors) {
            print_error("运行时", err.code, err.location, err.what());
        }
        std::cout << "\n程序异常退出\n";
        return 1;
    }

    // 8. 执行完成
    std::cout << "\n---------------------------------------------\n";
    std::cout << "[成功] 程序执行完成，无错误\n";
    return 0;
}
