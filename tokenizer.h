//Copyright (c) Gallt Developer.
//��Ȩ���У�c��Gallt �����ߡ�

#ifndef TOKENIZER_H
#define TOKENIZER_H
#include "common.h"
#include <string>
#include <cctype>
#include <atomic>
#include <csignal>

// ȫ��ԭ�ӱ���������Ƿ��յ�Ctrl+C�ж��źţ���������reѭ����
std::atomic<bool> g_is_ctrl_c = false;

// �źŴ��������յ�SIGINT��Ctrl+C��ʱ�����жϱ��
void sigint_handler(int signum) {
    if (signum == SIGINT) {
        g_is_ctrl_c = true;
    }
}

// �ʷ���Ԫ�ṹ���������͡��ı�ֵ��Դ��λ��
struct Token {
    TokenType type;          // Token���ͣ�����common.h��ö�٣�
    std::string value;       // Token���ı�ֵ�����ʶ�����ơ�����ֵ����λ��
    SourceLocation location; // ��Դ���е�λ�ã��к�+�кţ�
    // Ĭ�Ϲ��캯��
    Token() : type(TokenType::TOKEN_EOF), value(""), location() {}
    // ���캯��
    Token(TokenType t, const std::string& v, const SourceLocation& loc)
        : type(t), value(v), location(loc) {
    }
};

// �ʷ���������
class Tokenizer {
private:
    std::string source_code;  // Դ�����ַ���
    size_t current_pos;       // ��ǰ����λ�ã�������
    SourceLocation current_loc; // ��ǰλ����Ϣ���кš��кţ�
    char current_char;        // ��ǰ�ַ�
    std::vector<Token> token_buffer; // Token������������Ԥ��Ctrl+C��ǡ����˵�Token��

    // ǰ������һ���ַ���������λ����Ϣ
    void advance() {
        if (current_pos < source_code.size()) {
            current_pos++;
            current_loc.column++;
            // �����У��к�+1���к�����Ϊ1
            if (current_pos < source_code.size() && source_code[current_pos - 1] == '\n') {
                current_loc.line++;
                current_loc.column = 1;
            }
            current_char = (current_pos < source_code.size()) ? source_code[current_pos] : '\0';
        }
        else {
            current_char = '\0'; // �ѵ����ļ�ĩβ
        }
    }

    // �����հ��ַ����ո��Ʊ�������з��ȣ�
    void skip_whitespace() {
        while (current_char != '\0' && std::isspace(current_char)) {
            advance();
        }
    }

    // ����ע�ͣ�֧�ֵ���//�Ͷ���/*...*/��
    void skip_comment() {
        // 1. ������ע��//
        if (current_char == '/' && peek() == '/') {
            // ����ֱ�����л��ļ�����
            while (current_char != '\0' && current_char != '\n') {
                advance();
            }
            return;
        }
        // 2. �������ע��/*...*/
        if (current_char == '/' && peek() == '*') {
            SourceLocation comment_start_loc = current_loc;
            advance(); // ������һ��/
            advance(); // ����*
            // ����ֱ��*/���ļ�����
            while (current_char != '\0') {
                if (current_char == '*' && peek() == '/') {
                    advance(); // ����*
                    advance(); // ����/
                    return;
                }
                advance();
            }
            // ����ע��δ�պϣ�����ע��Token�������﷨��������
            token_buffer.emplace_back(
                TokenType::TOKEN_MULTI_COMMENT,
                "δ�պϵĶ���ע��",
                comment_start_loc
            );
            return;
        }
    }

    // Ԥ����һ���ַ������ƶ���ǰλ�ã�
    char peek() const {
        return (current_pos + 1 < source_code.size()) ? source_code[current_pos + 1] : '\0';
    }

    // Ԥ������n���ַ���ɵ��ַ��������ƶ���ǰλ�ã�
    std::string peek_n(size_t n) const {
        std::string res;
        for (size_t i = 0; i < n && (current_pos + i) < source_code.size(); i++) {
            res += source_code[current_pos + i];
        }
        return res;
    }

    // �������ؼ��֣���var.name��r.main��
    std::optional<Token> parse_dotted_keyword() {
        if (current_char == '\0') return std::nullopt;

        // ����1����ȡ��һ�����֣���var��r��
        std::string first_part;
        while (current_char != '\0' && (std::isalnum(current_char) || current_char == '_')) {
            first_part += current_char;
            advance();
        }
        // ����2������Ƿ����.�ͺ�������
        if (current_char != '.' || peek() == '\0' || !std::isalpha(peek())) {
            // ��.��.�����ĸ�����˵�һ�����ֵ�λ�ã����ؿգ�������ͨ��ʶ������
            current_pos -= first_part.size();
            current_loc.column -= first_part.size();
            current_char = source_code[current_pos];
            return std::nullopt;
        }
        // ����3����ȡ.�͵ڶ������֣���name��main��
        advance(); // ����.
        std::string second_part;
        while (current_char != '\0' && (std::isalnum(current_char) || current_char == '_')) {
            second_part += current_char;
            advance();
        }
        // ����4�����Ϊ�����ַ������ж��Ƿ�Ϊ�ؼ���
        std::string dotted_str = first_part + "." + second_part;
        TokenType type = CommonUtils::get_keyword_token(dotted_str);
        if (type != TokenType::TOKEN_IDENTIFIER) {
            // �Ǵ���ؼ��֣���var.name��TOKEN_VAR_NAME��r.main��TOKEN_R_MAIN��
            return Token(type, dotted_str, current_loc);
        }
        // ���ǹؼ��֣���������λ�ã����ؿ�
        current_pos -= (first_part.size() + 1 + second_part.size()); // 1��.�ĳ���
        current_loc.column -= (first_part.size() + 1 + second_part.size());
        current_char = source_code[current_pos];
        return std::nullopt;
    }

    // ������ʶ����ؼ��֣�������ؼ��֣�
    Token parse_identifier() {
        // �ȳ��Խ�������ؼ��֣���var.name��r.main��
        if (auto dotted_token = parse_dotted_keyword()) {
            return dotted_token.value();
        }
        // ��ͨ��ʶ��/�ؼ���
        SourceLocation start_loc = current_loc;
        std::string id_str;
        // �ռ���ʶ���ַ�����ĸ/����/�»��ߣ�
        while (current_char != '\0' && (std::isalnum(current_char) || current_char == '_')) {
            id_str += current_char;
            advance();
        }
        // �ж��Ƿ�Ϊ�ؼ��֣�ʹ��common.h�еĹ��ߺ�����
        TokenType type = CommonUtils::get_keyword_token(id_str);
        return Token(type, id_str, start_loc);
    }

    // ���������������������򸡵�����+ ��ѡ��wait��λ����3000ms��5s��
    Token parse_number(bool is_wait_context = false) {
        SourceLocation start_loc = current_loc;
        std::string num_str;
        bool has_dot = false;

        // �ռ����ֲ���
        while (current_char != '\0' && std::isdigit(current_char)) {
            num_str += current_char;
            advance();
        }
        // ����С�����֣������.�Һ�������֣�
        if (current_char == '.' && std::isdigit(peek())) {
            has_dot = true;
            num_str += current_char;
            advance(); // ����.
            while (current_char != '\0' && std::isdigit(current_char)) {
                num_str += current_char;
                advance();
            }
        }

        // ����1����������Token
        TokenType num_type = has_dot ? TokenType::TOKEN_FLOAT_LITERAL : TokenType::TOKEN_INT_LITERAL;
        Token num_token(num_type, num_str, start_loc);

        // ����2�������wait�����ģ����Խ���������λ����ms��s��
        if (!is_wait_context || current_char == '\0' || !std::isalpha(current_char)) {
            return num_token;
        }
        // �ռ���λ�ַ�������ĸ��
        std::string unit_str;
        SourceLocation unit_loc = current_loc;
        while (current_char != '\0' && std::isalpha(current_char)) {
            unit_str += current_char;
            advance();
        }
        // ��֤��λ�Ƿ�֧�֣�������֧�ֵĵ�λ����֧�ֵ���Ϊ��ʶ����
        if (GalltConstants::WAIT_SUPPORTED_UNITS.find(unit_str) != GalltConstants::WAIT_SUPPORTED_UNITS.end()) {
            // �Ƚ���λToken���뻺����������next_token�����ȷ��أ�
            token_buffer.emplace_back(TokenType::TOKEN_WAIT_UNIT, unit_str, unit_loc);
        }
        else {
            // ��֧�ֵĵ�λ�����˵�λ�ַ�����Ϊ�����ı�ʶ��
            current_pos -= unit_str.size();
            current_loc.column -= unit_str.size();
            current_char = source_code[current_pos];
        }
        return num_token;
    }

    // �����ַ�������������"��ͷ�ͽ�β��֧���ڲ�����/*������Ϊע�ͣ���
    Token parse_string() {
        SourceLocation start_loc = current_loc;
        std::string str_val;
        advance(); // ������ͷ��"

        // �ռ�ֱ�� closing "��������ת���ַ��������ĵ�������
        while (current_char != '\0' && current_char != '"') {
            str_val += current_char;
            advance();
        }
        if (current_char == '"') {
            advance(); // ������β��"
        }
        else {
            // δ�պϵ��ַ����������ַ���Token�����������������
            return Token(TokenType::TOKEN_STRING_LITERAL, str_val, start_loc);
        }
        return Token(TokenType::TOKEN_STRING_LITERAL, str_val, start_loc);
    }

    // ����wait����е�ʱ�䵥λ����parse_number���ã��򵥶����ã�
    Token parse_wait_unit() {
        SourceLocation start_loc = current_loc;
        std::string unit_str;
        // �ռ���λ�ַ�������ĸ��
        while (current_char != '\0' && std::isalpha(current_char)) {
            unit_str += current_char;
            advance();
        }
        return Token(TokenType::TOKEN_WAIT_UNIT, unit_str, start_loc);
    }

public:
    // ���캯������ʼ��Դ���롢λ����Ϣ����ע���źŴ�����
    Tokenizer(const std::string& code)
        : source_code(code), current_pos(0), current_loc(1, 1) {
        current_char = (source_code.empty()) ? '\0' : source_code[0];
        // ע��Ctrl+C�źŴ�����
        std::signal(SIGINT, sigint_handler);
    }

    // ��ȡ��һ��Token�����ķ��������ȷ��ػ�����Token���ٽ���Դ�룩
    Token next_token() {
        // 1. ���ȴ��������е�Token����wait��λ��δ�պ�ע�ͣ�
        if (!token_buffer.empty()) {
            Token buf_token = token_buffer.front();
            token_buffer.erase(token_buffer.begin());
            return buf_token;
        }

        // 2. ����Ƿ��յ�Ctrl+C�ж��ź�
        if (g_is_ctrl_c) {
            g_is_ctrl_c = false; // ���ñ�ǣ������ظ�����
            return Token(TokenType::TOKEN_CTRL_C, "Ctrl+C�ж�", current_loc);
        }

        // 3. ѭ������Դ�룬������Token
        while (current_char != '\0') {
            // �����հ��ַ�
            if (std::isspace(current_char)) {
                skip_whitespace();
                continue;
            }

            // ����ע�ͣ�����//�����/*...*/��
            if (current_char == '/' && (peek() == '/' || peek() == '*')) {
                skip_comment();
                continue;
            }

            // ��������ؼ��֣���var.name��r.main��������parse_identifier�д���

            // ������ʶ����ؼ���
            if (std::isalpha(current_char) || current_char == '_') {
                return parse_identifier();
            }

            // ����������������������ͨ���ֺ�wait���������֣�
            if (std::isdigit(current_char)) {
                // �ж��Ƿ�Ϊwait�����ģ�ǰһ��Token��wait�ҵ�ǰΪʱ��ֵ��
                bool is_wait_context = false;
                return parse_number(is_wait_context);
            }

            // �����ַ���������
            if (current_char == '"') {
                return parse_string();
            }

            // ����wait��λ����������ʱ����ms��s����Ϊ��ʶ�����˴�����wait�����Ĵ���
            if (std::isalpha(current_char)) {
                return parse_identifier();
            }

            // ����������ͷָ���
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
            case '@': { // �¼����ͷָ�������detect @click��
                SourceLocation loc = current_loc;
                advance();
                return Token(TokenType::TOKEN_AT, "@", loc);
            }
            default: {
                // δ֪�ַ�������EOF Token�������﷨��������
                SourceLocation loc = current_loc;
                std::string unknown(1, current_char);
                advance();
                return Token(TokenType::TOKEN_EOF, unknown, loc);
            }
            }
        }

        // 4. �����ļ�ĩβ
        return Token(TokenType::TOKEN_EOF, "", current_loc);
    }

    // ���������ⲿ���ã���parser.h��������Token�������������Ԥ���߼���
    void push_back_token(const Token& token) {
        token_buffer.insert(token_buffer.begin(), token);
    }

    // ���ⲿ���ã���parse_wait������ǵ�ǰ����Ϊwait�����ģ��������λ��
    Token parse_wait_number() {
        return parse_number(true); // is_wait_context=true���������ֺ�����ȡ��λ
    }
};

#endif // TOKENIZER_H