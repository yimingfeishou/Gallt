//Copyright (c) Gallt Developer.
//版权所有（c）Gallt 开发者。

#ifndef TOKENIZER_H
#define TOKENIZER_H
#include "common.h"
#include <string>
#include <cctype>
#include <atomic>
#include <csignal>

// 全局原子变量：标记是否收到Ctrl+C中断信号（用于无限re循环）
std::atomic<bool> g_is_ctrl_c = false;

// 信号处理函数：收到SIGINT（Ctrl+C）时设置中断标记
void sigint_handler(int signum) {
    if (signum == SIGINT) {
        g_is_ctrl_c = true;
    }
}

// 词法单元结构：包含类型、文本值和源码位置
struct Token {
    TokenType type;          // Token类型（来自common.h的枚举）
    std::string value;       // Token的文本值（如标识符名称、数字值、单位）
    SourceLocation location; // 在源码中的位置（行号+列号）
    // 默认构造函数
    Token() : type(TokenType::TOKEN_EOF), value(""), location() {}
    // 构造函数
    Token(TokenType t, const std::string& v, const SourceLocation& loc)
        : type(t), value(v), location(loc) {
    }
};

// 词法分析器类
class Tokenizer {
private:
    std::string source_code;  // 源代码字符串
    size_t current_pos;       // 当前解析位置（索引）
    SourceLocation current_loc; // 当前位置信息（行号、列号）
    char current_char;        // 当前字符
    std::vector<Token> token_buffer; // Token缓冲区（用于预存Ctrl+C标记、回退的Token）

    // 前进到下一个字符，并更新位置信息
    void advance() {
        if (current_pos < source_code.size()) {
            current_pos++;
            current_loc.column++;
            // 处理换行：行号+1，列号重置为1
            if (current_pos < source_code.size() && source_code[current_pos - 1] == '\n') {
                current_loc.line++;
                current_loc.column = 1;
            }
            current_char = (current_pos < source_code.size()) ? source_code[current_pos] : '\0';
        }
        else {
            current_char = '\0'; // 已到达文件末尾
        }
    }

    // 跳过空白字符（空格、制表符、换行符等）
    void skip_whitespace() {
        while (current_char != '\0' && std::isspace(current_char)) {
            advance();
        }
    }

    // 跳过注释（支持单行//和多行/*...*/）
    void skip_comment() {
        // 1. 处理单行注释//
        if (current_char == '/' && peek() == '/') {
            // 跳过直到换行或文件结束
            while (current_char != '\0' && current_char != '\n') {
                advance();
            }
            return;
        }
        // 2. 处理多行注释/*...*/
        if (current_char == '/' && peek() == '*') {
            SourceLocation comment_start_loc = current_loc;
            advance(); // 跳过第一个/
            advance(); // 跳过*
            // 跳过直到*/或文件结束
            while (current_char != '\0') {
                if (current_char == '*' && peek() == '/') {
                    advance(); // 跳过*
                    advance(); // 跳过/
                    return;
                }
                advance();
            }
            // 多行注释未闭合：生成注释Token（后续语法分析报错）
            token_buffer.emplace_back(
                TokenType::TOKEN_MULTI_COMMENT,
                "未闭合的多行注释",
                comment_start_loc
            );
            return;
        }
    }

    // 预览下一个字符（不移动当前位置）
    char peek() const {
        return (current_pos + 1 < source_code.size()) ? source_code[current_pos + 1] : '\0';
    }

    // 预览后续n个字符组成的字符串（不移动当前位置）
    std::string peek_n(size_t n) const {
        std::string res;
        for (size_t i = 0; i < n && (current_pos + i) < source_code.size(); i++) {
            res += source_code[current_pos + i];
        }
        return res;
    }

    // 处理带点关键字（如var.name、r.main）
    std::optional<Token> parse_dotted_keyword() {
        if (current_char == '\0') return std::nullopt;

        // 步骤1：读取第一个部分（如var、r）
        std::string first_part;
        while (current_char != '\0' && (std::isalnum(current_char) || current_char == '_')) {
            first_part += current_char;
            advance();
        }
        // 步骤2：检查是否存在.和后续部分
        if (current_char != '.' || peek() == '\0' || !std::isalpha(peek())) {
            // 无.或.后非字母：回退第一个部分的位置，返回空（交给普通标识符处理）
            current_pos -= first_part.size();
            current_loc.column -= first_part.size();
            current_char = source_code[current_pos];
            return std::nullopt;
        }
        // 步骤3：读取.和第二个部分（如name、main）
        advance(); // 跳过.
        std::string second_part;
        while (current_char != '\0' && (std::isalnum(current_char) || current_char == '_')) {
            second_part += current_char;
            advance();
        }
        // 步骤4：组合为带点字符串，判断是否为关键字
        std::string dotted_str = first_part + "." + second_part;
        TokenType type = CommonUtils::get_keyword_token(dotted_str);
        if (type != TokenType::TOKEN_IDENTIFIER) {
            // 是带点关键字（如var.name→TOKEN_VAR_NAME，r.main→TOKEN_R_MAIN）
            return Token(type, dotted_str, current_loc);
        }
        // 不是关键字：回退所有位置，返回空
        current_pos -= (first_part.size() + 1 + second_part.size()); // 1是.的长度
        current_loc.column -= (first_part.size() + 1 + second_part.size());
        current_char = source_code[current_pos];
        return std::nullopt;
    }

    // 解析标识符或关键字（含带点关键字）
    Token parse_identifier() {
        // 先尝试解析带点关键字（如var.name、r.main）
        if (auto dotted_token = parse_dotted_keyword()) {
            return dotted_token.value();
        }
        // 普通标识符/关键字
        SourceLocation start_loc = current_loc;
        std::string id_str;
        // 收集标识符字符（字母/数字/下划线）
        while (current_char != '\0' && (std::isalnum(current_char) || current_char == '_')) {
            id_str += current_char;
            advance();
        }
        // 判断是否为关键字（使用common.h中的工具函数）
        TokenType type = CommonUtils::get_keyword_token(id_str);
        return Token(type, id_str, start_loc);
    }

    // 解析数字字面量（整数或浮点数）+ 可选的wait单位（如3000ms、5s）
    Token parse_number(bool is_wait_context = false) {
        SourceLocation start_loc = current_loc;
        std::string num_str;
        bool has_dot = false;

        // 收集数字部分
        while (current_char != '\0' && std::isdigit(current_char)) {
            num_str += current_char;
            advance();
        }
        // 处理小数部分（如果有.且后面跟数字）
        if (current_char == '.' && std::isdigit(peek())) {
            has_dot = true;
            num_str += current_char;
            advance(); // 跳过.
            while (current_char != '\0' && std::isdigit(current_char)) {
                num_str += current_char;
                advance();
            }
        }

        // 步骤1：生成数字Token
        TokenType num_type = has_dot ? TokenType::TOKEN_FLOAT_LITERAL : TokenType::TOKEN_INT_LITERAL;
        Token num_token(num_type, num_str, start_loc);

        // 步骤2：如果是wait上下文，尝试解析后续单位（如ms、s）
        if (!is_wait_context || current_char == '\0' || !std::isalpha(current_char)) {
            return num_token;
        }
        // 收集单位字符（仅字母）
        std::string unit_str;
        SourceLocation unit_loc = current_loc;
        while (current_char != '\0' && std::isalpha(current_char)) {
            unit_str += current_char;
            advance();
        }
        // 验证单位是否支持（仅保留支持的单位，不支持的视为标识符）
        if (GalltConstants::WAIT_SUPPORTED_UNITS.find(unit_str) != GalltConstants::WAIT_SUPPORTED_UNITS.end()) {
            // 先将单位Token存入缓冲区（后续next_token会优先返回）
            token_buffer.emplace_back(TokenType::TOKEN_WAIT_UNIT, unit_str, unit_loc);
        }
        else {
            // 不支持的单位：回退单位字符，视为后续的标识符
            current_pos -= unit_str.size();
            current_loc.column -= unit_str.size();
            current_char = source_code[current_pos];
        }
        return num_token;
    }

    // 解析字符串字面量（以"开头和结尾，支持内部包含/*（不视为注释））
    Token parse_string() {
        SourceLocation start_loc = current_loc;
        std::string str_val;
        advance(); // 跳过开头的"

        // 收集直到 closing "（不处理转义字符，符合文档简化需求）
        while (current_char != '\0' && current_char != '"') {
            str_val += current_char;
            advance();
        }
        if (current_char == '"') {
            advance(); // 跳过结尾的"
        }
        else {
            // 未闭合的字符串：生成字符串Token（后续语义分析报错）
            return Token(TokenType::TOKEN_STRING_LITERAL, str_val, start_loc);
        }
        return Token(TokenType::TOKEN_STRING_LITERAL, str_val, start_loc);
    }

    // 解析wait语句中的时间单位（被parse_number调用，或单独调用）
    Token parse_wait_unit() {
        SourceLocation start_loc = current_loc;
        std::string unit_str;
        // 收集单位字符（仅字母）
        while (current_char != '\0' && std::isalpha(current_char)) {
            unit_str += current_char;
            advance();
        }
        return Token(TokenType::TOKEN_WAIT_UNIT, unit_str, start_loc);
    }

public:
    // 构造函数：初始化源代码、位置信息，并注册信号处理函数
    Tokenizer(const std::string& code)
        : source_code(code), current_pos(0), current_loc(1, 1) {
        current_char = (source_code.empty()) ? '\0' : source_code[0];
        // 注册Ctrl+C信号处理函数
        std::signal(SIGINT, sigint_handler);
    }

    // 获取下一个Token（核心方法：优先返回缓冲区Token，再解析源码）
    Token next_token() {
        // 1. 优先处理缓冲区中的Token（如wait单位、未闭合注释）
        if (!token_buffer.empty()) {
            Token buf_token = token_buffer.front();
            token_buffer.erase(token_buffer.begin());
            return buf_token;
        }

        // 2. 检查是否收到Ctrl+C中断信号
        if (g_is_ctrl_c) {
            g_is_ctrl_c = false; // 重置标记，避免重复触发
            return Token(TokenType::TOKEN_CTRL_C, "Ctrl+C中断", current_loc);
        }

        // 3. 循环解析源码，生成新Token
        while (current_char != '\0') {
            // 跳过空白字符
            if (std::isspace(current_char)) {
                skip_whitespace();
                continue;
            }

            // 跳过注释（单行//或多行/*...*/）
            if (current_char == '/' && (peek() == '/' || peek() == '*')) {
                skip_comment();
                continue;
            }

            // 解析带点关键字（如var.name、r.main）：已在parse_identifier中处理

            // 解析标识符或关键字
            if (std::isalpha(current_char) || current_char == '_') {
                return parse_identifier();
            }

            // 解析数字字面量（区分普通数字和wait上下文数字）
            if (std::isdigit(current_char)) {
                // 判断是否为wait上下文（前一个Token是wait且当前为时间值）
                bool is_wait_context = false;
                return parse_number(is_wait_context);
            }

            // 解析字符串字面量
            if (current_char == '"') {
                return parse_string();
            }

            // 解析wait单位（单独出现时，如ms、s，视为标识符，此处仅在wait上下文处理）
            if (std::isalpha(current_char)) {
                return parse_identifier();
            }

            // 处理运算符和分隔符
            switch (current_char) {
            case '=': {
                SourceLocation loc = current_loc;
                advance();
                return Token(TokenType::TOKEN_EQUALS, "=", loc);
            }
            case '+': {
                SourceLocation loc = current_loc;
                advance();
                return Token(TokenType::TOKEN_PLUS, "+", loc);
            }
            case '-': {
                SourceLocation loc = current_loc;
                advance();
                return Token(TokenType::TOKEN_MINUS, "-", loc);
            }
            case '*': {
                SourceLocation loc = current_loc;
                advance();
                return Token(TokenType::TOKEN_MUL, "*", loc);
            }
            case '/': {
                SourceLocation loc = current_loc;
                advance();
                return Token(TokenType::TOKEN_DIV, "/", loc);
            }
            case '<': {
                SourceLocation loc = current_loc;
                advance();
                return Token(TokenType::TOKEN_LT, "<", loc);
            }
            case '>': {
                SourceLocation loc = current_loc;
                advance();
                return Token(TokenType::TOKEN_GT, ">", loc);
            }
            case '(': {
                SourceLocation loc = current_loc;
                advance();
                return Token(TokenType::TOKEN_LPAREN, "(", loc);
            }
            case ')': {
                SourceLocation loc = current_loc;
                advance();
                return Token(TokenType::TOKEN_RPAREN, ")", loc);
            }
            case '{': {
                SourceLocation loc = current_loc;
                advance();
                return Token(TokenType::TOKEN_LBRACE, "{", loc);
            }
            case '}': {
                SourceLocation loc = current_loc;
                advance();
                return Token(TokenType::TOKEN_RBRACE, "}", loc);
            }
            case '[': {
                SourceLocation loc = current_loc;
                advance();
                return Token(TokenType::TOKEN_LBRACKET, "[", loc);
            }
            case ']': {
                SourceLocation loc = current_loc;
                advance();
                return Token(TokenType::TOKEN_RBRACKET, "]", loc);
            }
            case ';': {
                SourceLocation loc = current_loc;
                advance();
                return Token(TokenType::TOKEN_SEMICOLON, ";", loc);
            }
            case '.': {
                SourceLocation loc = current_loc;
                advance();
                return Token(TokenType::TOKEN_DOT, ".", loc);
            }
            case ',': {
                SourceLocation loc = current_loc;
                advance();
                return Token(TokenType::TOKEN_COMMA, ",", loc);
            }
            case '@': { // 事件类型分隔符（如detect @click）
                SourceLocation loc = current_loc;
                advance();
                return Token(TokenType::TOKEN_AT, "@", loc);
            }
            default: {
                // 未知字符：生成EOF Token（后续语法分析报错）
                SourceLocation loc = current_loc;
                std::string unknown(1, current_char);
                advance();
                return Token(TokenType::TOKEN_EOF, unknown, loc);
            }
            }
        }

        // 4. 到达文件末尾
        return Token(TokenType::TOKEN_EOF, "", current_loc);
    }

    // 新增：供外部调用（如parser.h），回退Token到缓冲区（解决预读逻辑）
    void push_back_token(const Token& token) {
        token_buffer.insert(token_buffer.begin(), token);
    }

    // 供外部调用（如parse_wait），标记当前数字为wait上下文（需解析单位）
    Token parse_wait_number() {
        return parse_number(true); // is_wait_context=true，解析数字后尝试提取单位
    }
};

#endif // TOKENIZER_H