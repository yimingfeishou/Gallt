//Copyright (c) Gallt Developer.
//版权所有（c）Gallt 开发者。

#ifndef COMMON_H
#define COMMON_H
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>
#include <set>
#include <optional>

// -------------- 1. 源码位置信息（用于错误定位）--------------
// 记录代码在源文件中的行号和列号，报错时提供精准定位
struct SourceLocation {
    int line;   // 行号（从1开始）
    int column; // 列号（从1开始）
    // 构造函数
    SourceLocation(int l = 0, int c = 0) : line(l), column(c) {}
    // 输出位置信息（用于日志/报错）
    std::string to_string() const {
        return "Line: " + std::to_string(line) + ", Column: " + std::to_string(column);
    }
};

// -------------- 2. 错误码枚举（对应Gallt文档报错表，补充ER 0006）--------------
// 所有编译器支持的错误类型，与文档中ER编码一一对应
enum class ErrorCode {
    ER_0001, // 多main应用程序未用r.main释放前一个main
    ER_0002, // otherwise后无执行代码块
    ER_0003, // 未定义的代码、值或函数
    ER_0004, // main/thread命名不合法
    ER_0005, // re重复执行次数超出普通上限
    ER_0006, // 函数命名冲突（新增：文档明确要求）
    ER_1001, // 数据类型不匹配
    ER_2001, // 数组索引超出范围
    NO_ERROR  // 无错误
};

// -------------- 3. 数据类型枚举（对应Gallt支持的变量/参数类型）--------------
enum class DataType {
    DATA_TYPE_INT,    // 整数类型（对应Gallt的int）
    DATA_TYPE_FLOAT,  // 单精度浮点数（对应Gallt的float）
    DATA_TYPE_DOUBLE, // 双精度浮点数（对应Gallt的double）
    DATA_TYPE_STRING, // 字符串类型（对应Gallt的string）
    DATA_TYPE_UNKNOWN // 未定义类型（语义分析时用于临时标记）
};

// -------------- 4. Token类型枚举（补充多行注释、事件检测相关Token）--------------
// Token的各个子类型（补充多行注释、事件检测相关Token）
// 关键字（Gallt文档中定义的核心语法关键字）
enum class TokenType {
    TOKEN_START,    // "start"
    TOKEN_END,      // "end"
    TOKEN_MAIN,     // "main"
    TOKEN_INPUT,    // "input"
    TOKEN_OUTPUT,   // "output"
    TOKEN_WAIT,     // "wait"
    TOKEN_THREAD,   // "thread"
    TOKEN_RE,       // "re"
    TOKEN_WILL,     // "will"
    TOKEN_INFINITE, // "infinite"
    TOKEN_DETECT,   // "detect"（事件检测关键字）
    TOKEN_RUN,      // "run"（事件触发执行关键字）
    TOKEN_DEFINE,   // "define"
    TOKEN_INT,      // "int"
    TOKEN_FLOAT,    // "float"
    TOKEN_DOUBLE,   // "double"
    TOKEN_STRING,   // "string"
    TOKEN_FUNCTION, // "function"
    TOKEN_RETURN,   // "return"
    TOKEN_IF,       // "if"
    TOKEN_OTHERWISE,// "otherwise"
    TOKEN_FOR,      // "for"
    TOKEN_WHILE,    // "while"
    TOKEN_ARR,      // "arr"
    TOKEN_VAR,      // "var"（特殊变量声明关键字）
    TOKEN_VAR_NAME, // "var.name"（特殊变量命名标记，新增）
    TOKEN_R_MAIN,   // "r.main"（释放前一个main的关键字）
    TOKEN_EXIT,     // "exit"（多main程序退出关键字）
    TOKEN_PI,       // "pi"（π计算关键字）
    // 标识符（变量名、函数名、数组名等）
    TOKEN_IDENTIFIER,
    // 字面量（具体的值）
    TOKEN_INT_LITERAL,    // 整数字面量（如10、-5）
    TOKEN_FLOAT_LITERAL,  // 浮点数字面量（如3.14、-0.5）
    TOKEN_STRING_LITERAL, // 字符串字面量（如"Hello"）
    // 运算符（Gallt支持的运算/赋值符号）
    TOKEN_EQUALS, // "="（赋值/相等判断，语义分析时区分场景）
    TOKEN_PLUS,   // "+"（加法/连接）
    TOKEN_MINUS,  // "-"（减法）
    TOKEN_MUL,    // "*"（乘法）
    TOKEN_DIV,    // "/"（除法）
    TOKEN_LT,     // "<"（小于）
    TOKEN_GT,     // ">"（大于）
    // 分隔符（语法结构分隔符号）
    TOKEN_LPAREN,    // "("（左圆括号，如input参数）
    TOKEN_RPAREN,    // ")"（右圆括号）
    TOKEN_LBRACE,    // "{"（左大括号，代码块边界）
    TOKEN_RBRACE,    // "}"（右大括号）
    TOKEN_LBRACKET,  // "["（左方括号，数组索引/参数列表）
    TOKEN_RBRACKET,  // "]"（右方括号）
    TOKEN_SEMICOLON, // ";"（参数分隔符，如函数参数a;b）
    TOKEN_DOT,       // "."（成员访问，如nums.4、var.1）
    TOKEN_COMMA,     // ","（函数调用参数分隔，如value(5,3)）
    TOKEN_AT,        // "@"（事件类型分隔符，如detect @click，新增）
    // 特殊Token
    TOKEN_EOF,          // 文件结束符
    TOKEN_SINGLE_COMMENT, // 单行注释（//，新增明确区分）
    TOKEN_MULTI_COMMENT,  // 多行注释（/*...*/，新增）
    TOKEN_WAIT_UNIT,      // wait时间单位（ms/s/min/hour，新增）
    TOKEN_CTRL_C         // Ctrl+C中断信号标记（用于无限re循环，新增）
};

// -------------- 5. 全局常量（补充wait单位、多行注释标记等）--------------
namespace GalltConstants {
    const int DEFAULT_PI_DIGITS = 7;       // π默认计算位数（文档规定）
    const int MAX_RE_NORMAL_COUNT = 65536; // 普通re最大重复次数（文档规定）
    const std::string DEFAULT_WAIT_UNIT = "ms"; // 等待默认单位（文档规定）
    const std::string FILE_EXTENSION = ".glt";  // Gallt代码文件扩展名
    // 支持的wait时间单位集合（新增，用于解析验证）
    const std::set<std::string> WAIT_SUPPORTED_UNITS = { "ms", "s", "min", "hour" };
    // 多行注释起始/结束标记（新增）
    const std::string MULTI_COMMENT_START = "/*";
    const std::string MULTI_COMMENT_END = "*/";
    // 事件类型集合（新增，对应文档detect支持的事件）
    const std::set<std::string> SUPPORTED_EVENTS = { "input", "click", "enter", "keyboard.click" };
}

// -------------- 6. 关键字映射表（补充var.name、事件检测相关关键字）--------------
// 静态映射：关键字字符串 → TokenType
static const std::unordered_map<std::string, TokenType> KEYWORD_MAP = {
    {"start", TokenType::TOKEN_START},
    {"end", TokenType::TOKEN_END},
    {"main", TokenType::TOKEN_MAIN},
    {"input", TokenType::TOKEN_INPUT},
    {"output", TokenType::TOKEN_OUTPUT},
    {"wait", TokenType::TOKEN_WAIT},
    {"thread", TokenType::TOKEN_THREAD},
    {"re", TokenType::TOKEN_RE},
    {"will", TokenType::TOKEN_WILL},
    {"infinite", TokenType::TOKEN_INFINITE},
    {"detect", TokenType::TOKEN_DETECT},
    {"run", TokenType::TOKEN_RUN},
    {"define", TokenType::TOKEN_DEFINE},
    {"int", TokenType::TOKEN_INT},
    {"float", TokenType::TOKEN_FLOAT},
    {"double", TokenType::TOKEN_DOUBLE},
    {"string", TokenType::TOKEN_STRING},
    {"function", TokenType::TOKEN_FUNCTION},
    {"return", TokenType::TOKEN_RETURN},
    {"if", TokenType::TOKEN_IF},
    {"otherwise", TokenType::TOKEN_OTHERWISE},
    {"for", TokenType::TOKEN_FOR},
    {"while", TokenType::TOKEN_WHILE},
    {"arr", TokenType::TOKEN_ARR},
    {"var", TokenType::TOKEN_VAR},
    {"var.name", TokenType::TOKEN_VAR_NAME}, // 补充特殊变量命名关键字
    {"r.main", TokenType::TOKEN_R_MAIN},
    {"exit", TokenType::TOKEN_EXIT},
    {"pi", TokenType::TOKEN_PI}
};

// -------------- 7. 工具函数（补充ER 0006错误描述、wait单位转换等）--------------
namespace CommonUtils {
    // 根据字符串判断是否为关键字，是则返回对应TokenType，否则返回标识符
    inline TokenType get_keyword_token(const std::string& str) {
        auto it = KEYWORD_MAP.find(str);
        return (it != KEYWORD_MAP.end()) ? it->second : TokenType::TOKEN_IDENTIFIER;
    }

    // 根据错误码获取错误描述（补充ER 0006，与文档一致）
    inline std::string get_error_message(ErrorCode err) {
        switch (err) {
        case ErrorCode::ER_0001:
            return "ER 0001：多main应用程序在编写下一个main时使用r.main释放前一个main";
        case ErrorCode::ER_0002:
            return "ER 0002：otherwise后没有执行方式";
        case ErrorCode::ER_0003:
            return "ER 0003：未定义的代码、值或函数";
        case ErrorCode::ER_0004:
            return "ER 0004：main或thread命名错误，检查命名是否合法";
        case ErrorCode::ER_0005:
            return "ER 0005：re执行超限";
        case ErrorCode::ER_0006:
            return "ER 0006：函数命名冲突"; 
        case ErrorCode::ER_1001:
            return "ER 1001：类型不匹配";
        case ErrorCode::ER_2001:
            return "ER 2001：数组索引超出范围";
        case ErrorCode::NO_ERROR:
            return "无错误";
        default:
            return "未知错误";
        }
    }

    // 将DataType转换为字符串（用于日志/报错）
    inline std::string data_type_to_string(DataType type) {
        switch (type) {
        case DataType::DATA_TYPE_INT: return "int";
        case DataType::DATA_TYPE_FLOAT: return "float";
        case DataType::DATA_TYPE_DOUBLE: return "double";
        case DataType::DATA_TYPE_STRING: return "string";
        default: return "unknown";
        }
    }

    // 新增：将wait时间单位转换为毫秒（统一计算基准）
    inline std::optional<int> convert_time_to_ms(int duration, const std::string& unit) {
        if (GalltConstants::WAIT_SUPPORTED_UNITS.find(unit) == GalltConstants::WAIT_SUPPORTED_UNITS.end()) {
            return std::nullopt; // 不支持的单位
        }
        if (unit == "ms") return duration;
        if (unit == "s") return duration * 1000;
        if (unit == "min") return duration * 60 * 1000;
        if (unit == "hour") return duration * 3600 * 1000;
        return std::nullopt;
    }

    // 新增：判断事件类型是否支持
    inline bool is_supported_event(const std::string& event) {
        return GalltConstants::SUPPORTED_EVENTS.find(event) != GalltConstants::SUPPORTED_EVENTS.end();
    }
}

#endif // COMMON_H