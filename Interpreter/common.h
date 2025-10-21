//Copyright (c) Gallt Developer.
//��Ȩ���У�c��Gallt �����ߡ�

#ifndef COMMON_H
#define COMMON_H
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>
#include <set>
#include <optional>

// -------------- 1. Դ��λ����Ϣ�����ڴ���λ��--------------
// ��¼������Դ�ļ��е��кź��кţ�����ʱ�ṩ��׼��λ
struct SourceLocation {
    int line;   // �кţ���1��ʼ��
    int column; // �кţ���1��ʼ��
    // ���캯��
    SourceLocation(int l = 0, int c = 0) : line(l), column(c) {}
    // ���λ����Ϣ��������־/����
    std::string to_string() const {
        return "Line: " + std::to_string(line) + ", Column: " + std::to_string(column);
    }
};

// -------------- 2. ������ö�٣���ӦGallt�ĵ����������ER 0006��--------------
// ���б�����֧�ֵĴ������ͣ����ĵ���ER����һһ��Ӧ
enum class ErrorCode {
    ER_0001, // ��mainӦ�ó���δ��r.main�ͷ�ǰһ��main
    ER_0002, // otherwise����ִ�д����
    ER_0003, // δ����Ĵ��롢ֵ����
    ER_0004, // main/thread�������Ϸ�
    ER_0005, // re�ظ�ִ�д���������ͨ����
    ER_0006, // ����������ͻ���������ĵ���ȷҪ��
    ER_1001, // �������Ͳ�ƥ��
    ER_2001, // ��������������Χ
    NO_ERROR  // �޴���
};

// -------------- 3. ��������ö�٣���ӦGallt֧�ֵı���/�������ͣ�--------------
enum class DataType {
    DATA_TYPE_INT,    // �������ͣ���ӦGallt��int��
    DATA_TYPE_FLOAT,  // �����ȸ���������ӦGallt��float��
    DATA_TYPE_DOUBLE, // ˫���ȸ���������ӦGallt��double��
    DATA_TYPE_STRING, // �ַ������ͣ���ӦGallt��string��
    DATA_TYPE_UNKNOWN // δ�������ͣ��������ʱ������ʱ��ǣ�
};

// -------------- 4. Token����ö�٣��������ע�͡��¼�������Token��--------------
// Token�ĸ��������ͣ��������ע�͡��¼�������Token��
// �ؼ��֣�Gallt�ĵ��ж���ĺ����﷨�ؼ��֣�
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
    TOKEN_DETECT,   // "detect"���¼����ؼ��֣�
    TOKEN_RUN,      // "run"���¼�����ִ�йؼ��֣�
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
    TOKEN_VAR,      // "var"��������������ؼ��֣�
    TOKEN_VAR_NAME, // "var.name"���������������ǣ�������
    TOKEN_R_MAIN,   // "r.main"���ͷ�ǰһ��main�Ĺؼ��֣�
    TOKEN_EXIT,     // "exit"����main�����˳��ؼ��֣�
    TOKEN_PI,       // "pi"���м���ؼ��֣�
    // ��ʶ���������������������������ȣ�
    TOKEN_IDENTIFIER,
    // �������������ֵ��
    TOKEN_INT_LITERAL,    // ��������������10��-5��
    TOKEN_FLOAT_LITERAL,  // ����������������3.14��-0.5��
    TOKEN_STRING_LITERAL, // �ַ�������������"Hello"��
    // �������Gallt֧�ֵ�����/��ֵ���ţ�
    TOKEN_EQUALS, // "="����ֵ/����жϣ��������ʱ���ֳ�����
    TOKEN_PLUS,   // "+"���ӷ�/���ӣ�
    TOKEN_MINUS,  // "-"��������
    TOKEN_MUL,    // "*"���˷���
    TOKEN_DIV,    // "/"��������
    TOKEN_LT,     // "<"��С�ڣ�
    TOKEN_GT,     // ">"�����ڣ�
    // �ָ������﷨�ṹ�ָ����ţ�
    TOKEN_LPAREN,    // "("����Բ���ţ���input������
    TOKEN_RPAREN,    // ")"����Բ���ţ�
    TOKEN_LBRACE,    // "{"��������ţ������߽磩
    TOKEN_RBRACE,    // "}"���Ҵ����ţ�
    TOKEN_LBRACKET,  // "["�������ţ���������/�����б�
    TOKEN_RBRACKET,  // "]"���ҷ����ţ�
    TOKEN_SEMICOLON, // ";"�������ָ������纯������a;b��
    TOKEN_DOT,       // "."����Ա���ʣ���nums.4��var.1��
    TOKEN_COMMA,     // ","���������ò����ָ�����value(5,3)��
    TOKEN_AT,        // "@"���¼����ͷָ�������detect @click��������
    // ����Token
    TOKEN_EOF,          // �ļ�������
    TOKEN_SINGLE_COMMENT, // ����ע�ͣ�//��������ȷ���֣�
    TOKEN_MULTI_COMMENT,  // ����ע�ͣ�/*...*/��������
    TOKEN_WAIT_UNIT,      // waitʱ�䵥λ��ms/s/min/hour��������
    TOKEN_CTRL_C         // Ctrl+C�ж��źű�ǣ���������reѭ����������
};

// -------------- 5. ȫ�ֳ���������wait��λ������ע�ͱ�ǵȣ�--------------
namespace GalltConstants {
    const int DEFAULT_PI_DIGITS = 7;       // ��Ĭ�ϼ���λ�����ĵ��涨��
    const int MAX_RE_NORMAL_COUNT = 65536; // ��ͨre����ظ��������ĵ��涨��
    const std::string DEFAULT_WAIT_UNIT = "ms"; // �ȴ�Ĭ�ϵ�λ���ĵ��涨��
    const std::string FILE_EXTENSION = ".glt";  // Gallt�����ļ���չ��
    // ֧�ֵ�waitʱ�䵥λ���ϣ����������ڽ�����֤��
    const std::set<std::string> WAIT_SUPPORTED_UNITS = { "ms", "s", "min", "hour" };
    // ����ע����ʼ/������ǣ�������
    const std::string MULTI_COMMENT_START = "/*";
    const std::string MULTI_COMMENT_END = "*/";
    // �¼����ͼ��ϣ���������Ӧ�ĵ�detect֧�ֵ��¼���
    const std::set<std::string> SUPPORTED_EVENTS = { "input", "click", "enter", "keyboard.click" };
}

// -------------- 6. �ؼ���ӳ�������var.name���¼������عؼ��֣�--------------
// ��̬ӳ�䣺�ؼ����ַ��� �� TokenType
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
    {"var.name", TokenType::TOKEN_VAR_NAME}, // ����������������ؼ���
    {"r.main", TokenType::TOKEN_R_MAIN},
    {"exit", TokenType::TOKEN_EXIT},
    {"pi", TokenType::TOKEN_PI}
};

// -------------- 7. ���ߺ���������ER 0006����������wait��λת���ȣ�--------------
namespace CommonUtils {
    // �����ַ����ж��Ƿ�Ϊ�ؼ��֣����򷵻ض�ӦTokenType�����򷵻ر�ʶ��
    inline TokenType get_keyword_token(const std::string& str) {
        auto it = KEYWORD_MAP.find(str);
        return (it != KEYWORD_MAP.end()) ? it->second : TokenType::TOKEN_IDENTIFIER;
    }

    // ���ݴ������ȡ��������������ER 0006�����ĵ�һ�£�
    inline std::string get_error_message(ErrorCode err) {
        switch (err) {
        case ErrorCode::ER_0001:
            return "ER 0001����mainӦ�ó����ڱ�д��һ��mainʱʹ��r.main�ͷ�ǰһ��main";
        case ErrorCode::ER_0002:
            return "ER 0002��otherwise��û��ִ�з�ʽ";
        case ErrorCode::ER_0003:
            return "ER 0003��δ����Ĵ��롢ֵ����";
        case ErrorCode::ER_0004:
            return "ER 0004��main��thread�������󣬼�������Ƿ�Ϸ�";
        case ErrorCode::ER_0005:
            return "ER 0005��reִ�г���";
        case ErrorCode::ER_0006:
            return "ER 0006������������ͻ"; 
        case ErrorCode::ER_1001:
            return "ER 1001�����Ͳ�ƥ��";
        case ErrorCode::ER_2001:
            return "ER 2001����������������Χ";
        case ErrorCode::NO_ERROR:
            return "�޴���";
        default:
            return "δ֪����";
        }
    }

    // ��DataTypeת��Ϊ�ַ�����������־/����
    inline std::string data_type_to_string(DataType type) {
        switch (type) {
        case DataType::DATA_TYPE_INT: return "int";
        case DataType::DATA_TYPE_FLOAT: return "float";
        case DataType::DATA_TYPE_DOUBLE: return "double";
        case DataType::DATA_TYPE_STRING: return "string";
        default: return "unknown";
        }
    }

    // ��������waitʱ�䵥λת��Ϊ���루ͳһ�����׼��
    inline std::optional<int> convert_time_to_ms(int duration, const std::string& unit) {
        if (GalltConstants::WAIT_SUPPORTED_UNITS.find(unit) == GalltConstants::WAIT_SUPPORTED_UNITS.end()) {
            return std::nullopt; // ��֧�ֵĵ�λ
        }
        if (unit == "ms") return duration;
        if (unit == "s") return duration * 1000;
        if (unit == "min") return duration * 60 * 1000;
        if (unit == "hour") return duration * 3600 * 1000;
        return std::nullopt;
    }

    // �������ж��¼������Ƿ�֧��
    inline bool is_supported_event(const std::string& event) {
        return GalltConstants::SUPPORTED_EVENTS.find(event) != GalltConstants::SUPPORTED_EVENTS.end();
    }
}

#endif // COMMON_H