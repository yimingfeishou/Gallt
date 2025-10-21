//Copyright (c) Gallt Developer.
//��Ȩ���У�c��Gallt �����ߡ�

#ifndef PARSER_H
#define PARSER_H
#include "common.h"
#include "tokenizer.h"
#include "ast_nodes.h"
#include <memory>
#include <vector>
#include <stdexcept>
#include <algorithm>

// �﷨���������쳣�������жϷ��������ݴ�����Ϣ��
class ParseError : public std::runtime_error {
public:
    ErrorCode error_code;
    SourceLocation location;
    ParseError(ErrorCode code, const SourceLocation& loc, const std::string& msg)
        : std::runtime_error(msg), error_code(code), location(loc) {}
};

// �﷨��������
class Parser {
private:
    Tokenizer& tokenizer;       // �ʷ����������ã��ṩToken����
    Token current_token;        // ��ǰToken
    std::vector<ParseError> errors; // �ռ����﷨����

    // ƥ�䲢����ָ�����͵�Token������ƥ�����׳�����
    void consume(TokenType expected_type) {
        if (current_token.type == expected_type) {
            current_token = tokenizer.next_token();
        }
        else {
            std::string msg = "Ԥ��Token���Ͳ�ƥ�䣬Ԥ��: " + token_type_to_string(expected_type) +
                "��ʵ��: " + token_type_to_string(current_token.type);
            throw ParseError(ErrorCode::ER_0003, current_token.location, msg);
        }
    }

    // ������������TokenTypeת��Ϊ�ַ��������ڴ�����Ϣ����������Token��
    std::string token_type_to_string(TokenType type) {
        switch (type) {
        case TokenType::TOKEN_START: return "start";
        case TokenType::TOKEN_END: return "end";
        case TokenType::TOKEN_MAIN: return "main";
        case TokenType::TOKEN_INPUT: return "input";
        case TokenType::TOKEN_OUTPUT: return "output";
        case TokenType::TOKEN_WAIT: return "wait";
        case TokenType::TOKEN_THREAD: return "thread";
        case TokenType::TOKEN_RE: return "re";
        case TokenType::TOKEN_WILL: return "will";
        case TokenType::TOKEN_INFINITE: return "infinite";
        case TokenType::TOKEN_DETECT: return "detect";
        case TokenType::TOKEN_RUN: return "run";
        case TokenType::TOKEN_DEFINE: return "define";
        case TokenType::TOKEN_INT: return "int";
        case TokenType::TOKEN_FLOAT: return "float";
        case TokenType::TOKEN_DOUBLE: return "double";
        case TokenType::TOKEN_STRING: return "string";
        case TokenType::TOKEN_FUNCTION: return "function";
        case TokenType::TOKEN_RETURN: return "return";
        case TokenType::TOKEN_IF: return "if";
        case TokenType::TOKEN_OTHERWISE: return "otherwise";
        case TokenType::TOKEN_FOR: return "for";
        case TokenType::TOKEN_WHILE: return "while";
        case TokenType::TOKEN_ARR: return "arr";
        case TokenType::TOKEN_VAR: return "var";
        case TokenType::TOKEN_VAR_NAME: return "var.name";
        case TokenType::TOKEN_R_MAIN: return "r.main";
        case TokenType::TOKEN_EXIT: return "exit";
        case TokenType::TOKEN_PI: return "pi";
        case TokenType::TOKEN_IDENTIFIER: return "��ʶ��";
        case TokenType::TOKEN_INT_LITERAL: return "����������";
        case TokenType::TOKEN_FLOAT_LITERAL: return "������������";
        case TokenType::TOKEN_STRING_LITERAL: return "�ַ���������";
        case TokenType::TOKEN_EQUALS: return "=";
        case TokenType::TOKEN_PLUS: return "+";
        case TokenType::TOKEN_MINUS: return "-";
        case TokenType::TOKEN_MUL: return "*";
        case TokenType::TOKEN_DIV: return "/";
        case TokenType::TOKEN_LT: return "<";
        case TokenType::TOKEN_GT: return ">";
        case TokenType::TOKEN_LPAREN: return "(";
        case TokenType::TOKEN_RPAREN: return ")";
        case TokenType::TOKEN_LBRACE: return "{";
        case TokenType::TOKEN_RBRACE: return "}";
        case TokenType::TOKEN_LBRACKET: return "[";
        case TokenType::TOKEN_RBRACKET: return "]";
        case TokenType::TOKEN_SEMICOLON: return ";";
        case TokenType::TOKEN_DOT: return ".";
        case TokenType::TOKEN_COMMA: return ",";
        case TokenType::TOKEN_AT: return "@";
        case TokenType::TOKEN_WAIT_UNIT: return "ʱ�䵥λ��ms/s/min/hour��";
        case TokenType::TOKEN_CTRL_C: return "Ctrl+C�жϱ��";
        case TokenType::TOKEN_EOF: return "�ļ�������";
        default: return "δ֪Token";
        }
    }

    // ������������鵱ǰToken�Ƿ�Ϊ��ֵ���ͣ�int/float��
    bool is_numeric_token(TokenType type) {
        return type == TokenType::TOKEN_INT_LITERAL || type == TokenType::TOKEN_FLOAT_LITERAL;
    }

    // ����������Ԥ����һ��Token�������ģ�ͨ��tokenizer��public�ӿ�ʵ�֣�
    Token peek_next_token() {
        Token next = tokenizer.next_token();
        tokenizer.push_back_token(next); // ʹ��Tokenizer��public�ӿڻ���Token
        return next;
    }

    // ------------------------------ ���ʽ�������޸�case�ظ���release���� ------------------------------
    // ����ԭ�ӱ��ʽ���ϲ��ظ���TOKEN_IDENTIFIER��֧���޸�release���ã�
    std::unique_ptr<ExpressionNode> parse_primary_expr() {
        SourceLocation loc = current_token.location;
        switch (current_token.type) {
        case TokenType::TOKEN_INT_LITERAL: {
            Token token = current_token;
            consume(TokenType::TOKEN_INT_LITERAL);
            return std::make_unique<LiteralExprNode>(loc, DataType::DATA_TYPE_INT, token.value);
        }
        case TokenType::TOKEN_FLOAT_LITERAL: {
            Token token = current_token;
            consume(TokenType::TOKEN_FLOAT_LITERAL);
            return std::make_unique<LiteralExprNode>(loc, DataType::DATA_TYPE_FLOAT, token.value);
        }
        case TokenType::TOKEN_STRING_LITERAL: {
            Token token = current_token;
            consume(TokenType::TOKEN_STRING_LITERAL);
            return std::make_unique<LiteralExprNode>(loc, DataType::DATA_TYPE_STRING, token.value);
        }
        case TokenType::TOKEN_IDENTIFIER: {
            std::string id_name = current_token.value;
            consume(TokenType::TOKEN_IDENTIFIER);

            // 1. ���ȼ���Ƿ�ΪCount���ʽ��count(...) �� float.count(...)��
            // �ӷ�֧1��float.count / double.count
            if (id_name == "float" || id_name == "double") {
                if (current_token.type == TokenType::TOKEN_DOT) {
                    consume(TokenType::TOKEN_DOT);
                    if (current_token.type != TokenType::TOKEN_IDENTIFIER || current_token.value != "count") {
                        throw ParseError(ErrorCode::ER_0003, current_token.location,
                            id_name + ".������count����" + id_name + ".count(...)��");
                    }
                    consume(TokenType::TOKEN_IDENTIFIER); // ����count
                    consume(TokenType::TOKEN_LPAREN); // ����(
                    auto calc_expr = parse_expr(); // ������������ʽ
                    consume(TokenType::TOKEN_RPAREN); // ����)
                    // ȷ��Count����
                    CountExprNode::CountType count_type = (id_name == "float") ?
                        CountExprNode::CountType::COUNT_FLOAT : CountExprNode::CountType::COUNT_DOUBLE;
                    return std::make_unique<CountExprNode>(loc, count_type, std::move(calc_expr));
                }
            }
            // �ӷ�֧2����ͨcount(...)
            else if (id_name == "count") {
                consume(TokenType::TOKEN_LPAREN); // ����(
                auto calc_expr = parse_expr(); // ������������ʽ
                consume(TokenType::TOKEN_RPAREN); // ����)
                return std::make_unique<CountExprNode>(
                    loc,
                    CountExprNode::CountType::COUNT_NORMAL,
                    std::move(calc_expr)
                );
            }

            // 2. ����Ƿ�Ϊ�������ã����(��
            if (current_token.type == TokenType::TOKEN_LPAREN) {
                consume(TokenType::TOKEN_LPAREN);
                std::vector<std::unique_ptr<ExpressionNode>> args;
                while (current_token.type != TokenType::TOKEN_RPAREN) {
                    args.push_back(parse_expr());
                    if (current_token.type == TokenType::TOKEN_SEMICOLON) {
                        consume(TokenType::TOKEN_SEMICOLON);
                    }
                    else {
                        break;
                    }
                }
                consume(TokenType::TOKEN_RPAREN);
                return std::make_unique<FunctionCallExprNode>(loc, id_name, std::move(args));
            }

            // 3. ����Ƿ�Ϊ������ʣ����.��
            else if (current_token.type == TokenType::TOKEN_DOT) {
                consume(TokenType::TOKEN_DOT);
                auto index = parse_primary_expr();
                auto arr_id = std::make_unique<IdentifierExprNode>(loc, id_name);
                return std::make_unique<ArrayAccessExprNode>(loc, std::move(arr_id), std::move(index));
            }

            // 4. ����Ƿ�Ϊ�¼���������keyboard.click��
            if (CommonUtils::is_supported_event(id_name) && current_token.type == TokenType::TOKEN_EQUALS) {
                consume(TokenType::TOKEN_EQUALS);
                auto trigger_val = parse_primary_expr(); // ����ֵ����"k"��
                return std::make_unique<EventConditionExprNode>(loc, id_name, std::move(trigger_val));
            }

            // 5. ��ͨ��ʶ��������/��������
            return std::make_unique<IdentifierExprNode>(loc, id_name);
        }
        case TokenType::TOKEN_PI: {
            consume(TokenType::TOKEN_PI);
            std::unique_ptr<ExpressionNode> precision;
            if (current_token.type == TokenType::TOKEN_LPAREN) {
                consume(TokenType::TOKEN_LPAREN);
                precision = parse_expr();
                consume(TokenType::TOKEN_RPAREN);
            }
            return std::make_unique<PiExprNode>(loc, std::move(precision));
        }
        case TokenType::TOKEN_LPAREN: {
            consume(TokenType::TOKEN_LPAREN);
            auto expr = parse_expr();
            consume(TokenType::TOKEN_RPAREN);
            return expr;
        }
        default: {
            throw ParseError(ErrorCode::ER_0003, current_token.location,
                "Unexpected token in expression: " + current_token.value);
        }
        }
    }

    // ������Ԫ������ʽ������ԭ���߼��������������ʽ��
    std::unique_ptr<ExpressionNode> parse_expr() {
        auto left = parse_primary_expr();
        while (current_token.type == TokenType::TOKEN_PLUS ||
            current_token.type == TokenType::TOKEN_MINUS ||
            current_token.type == TokenType::TOKEN_MUL ||
            current_token.type == TokenType::TOKEN_DIV ||
            current_token.type == TokenType::TOKEN_EQUALS ||
            current_token.type == TokenType::TOKEN_LT ||
            current_token.type == TokenType::TOKEN_GT) {
            Token op_token = current_token;
            consume(op_token.type);
            auto right = parse_primary_expr();
            left = std::make_unique<BinaryOpExprNode>(op_token.location, op_token.type,
                std::move(left), std::move(right));
        }
        return left;
    }

    // ------------------------------ ���������޸�token_buffer���ʡ�make_unique������ ------------------------------
    // ��������飨����ԭ���߼���
    std::unique_ptr<BlockNode> parse_block() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_LBRACE); // ����{
        std::vector<std::unique_ptr<StatementNode>> statements;
        while (current_token.type != TokenType::TOKEN_RBRACE &&
            current_token.type != TokenType::TOKEN_EOF) {
            statements.push_back(std::move(parse_statement())); // �޸�������std::move
        }
        consume(TokenType::TOKEN_RBRACE); // ����}
        return std::make_unique<BlockNode>(loc, std::move(statements));
    }

    // ��������������䣨����ԭ���߼����������飩
    std::unique_ptr<VariableDeclarationNode> parse_variable_declaration() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_DEFINE); // ����define
        // ������������
        DataType var_type;
        if (current_token.type == TokenType::TOKEN_INT) {
            var_type = DataType::DATA_TYPE_INT;
            consume(TokenType::TOKEN_INT);
        }
        else if (current_token.type == TokenType::TOKEN_FLOAT) {
            var_type = DataType::DATA_TYPE_FLOAT;
            consume(TokenType::TOKEN_FLOAT);
        }
        else if (current_token.type == TokenType::TOKEN_DOUBLE) {
            var_type = DataType::DATA_TYPE_DOUBLE;
            consume(TokenType::TOKEN_DOUBLE);
        }
        else if (current_token.type == TokenType::TOKEN_STRING) {
            var_type = DataType::DATA_TYPE_STRING;
            consume(TokenType::TOKEN_STRING);
        }
        else {
            throw ParseError(ErrorCode::ER_1001, current_token.location, "δ֪��������");
        }
        // ����Ƿ�Ϊ���飨arr�ؼ��֣�
        bool is_array = false;
        std::unique_ptr<ExpressionNode> array_size;
        if (current_token.type == TokenType::TOKEN_ARR) {
            is_array = true;
            consume(TokenType::TOKEN_ARR);
            consume(TokenType::TOKEN_LBRACKET); // ����[
            array_size = parse_expr();
            consume(TokenType::TOKEN_RBRACKET); // ����]
        }
        // ����������
        if (current_token.type != TokenType::TOKEN_IDENTIFIER) {
            throw ParseError(ErrorCode::ER_0004, current_token.location, "�����������Ǳ�ʶ��");
        }
        std::string var_name = current_token.value;
        consume(TokenType::TOKEN_IDENTIFIER);
        // ������ʼֵ����ѡ��
        std::unique_ptr<ExpressionNode> initial_value;
        if (current_token.type == TokenType::TOKEN_EQUALS) {
            consume(TokenType::TOKEN_EQUALS);
            initial_value = parse_expr();
        }
        consume(TokenType::TOKEN_SEMICOLON); // ����;
        return std::make_unique<VariableDeclarationNode>(loc, var_type, var_name,
            is_array, std::move(array_size),
            std::move(initial_value));
    }

    // ������ֵ��䣨����ԭ���߼�������������ʣ�
    std::unique_ptr<AssignmentNode> parse_assignment() {
        SourceLocation loc = current_token.location;
        auto target = parse_primary_expr(); // Ŀ������Ǳ�ʶ�����������
        if (current_token.type != TokenType::TOKEN_EQUALS) {
            throw ParseError(ErrorCode::ER_0003, current_token.location, "��ֵ���ȱ��=");
        }
        consume(TokenType::TOKEN_EQUALS);
        auto value = parse_expr();
        consume(TokenType::TOKEN_SEMICOLON); // ����;
        return std::make_unique<AssignmentNode>(loc, std::move(target), std::move(value));
    }

    // ���������䣨����ԭ���߼���
    std::unique_ptr<OutputNode> parse_output() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_OUTPUT); // ����output
        auto expr = parse_expr();
        consume(TokenType::TOKEN_SEMICOLON); // ����;
        return std::make_unique<OutputNode>(loc, std::move(expr));
    }

    // ����������䣨����ԭ���߼���
    std::unique_ptr<InputNode> parse_input() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_INPUT); // ����input
        std::unique_ptr<ExpressionNode> prompt;
        std::unique_ptr<IdentifierExprNode> target;
        // ����Ƿ�����ʾ��Ϣ���ַ������ʽ��
        if (current_token.type == TokenType::TOKEN_STRING_LITERAL) {
            prompt = parse_expr();
            if (current_token.type != TokenType::TOKEN_COMMA) {
                throw ParseError(ErrorCode::ER_0003, current_token.location, "���������ʾ��Ϣ��ȱ��,");
            }
            consume(TokenType::TOKEN_COMMA);
        }
        // ����Ŀ�����
        if (current_token.type != TokenType::TOKEN_IDENTIFIER) {
            throw ParseError(ErrorCode::ER_0003, current_token.location, "�������Ŀ������Ǳ�ʶ��");
        }
        target = std::make_unique<IdentifierExprNode>(current_token.location, current_token.value);
        consume(TokenType::TOKEN_IDENTIFIER);
        consume(TokenType::TOKEN_SEMICOLON); // ����;
        return std::make_unique<InputNode>(loc, std::move(prompt), std::move(target));
    }

    // ����������䣨����ԭ���߼���
    std::unique_ptr<IfNode> parse_if() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_IF); // ����if
        // �����������ʽ
        auto condition = parse_expr();
        // ����then��
        auto then_block = parse_block();
        // ����otherwise�飨��ѡ��
        std::unique_ptr<BlockNode> else_block;
        if (current_token.type == TokenType::TOKEN_OTHERWISE) {
            consume(TokenType::TOKEN_OTHERWISE);
            else_block = parse_block();
            if (!else_block) {
                throw ParseError(ErrorCode::ER_0002, current_token.location, "otherwise��ȱ�ٴ����");
            }
        }
        return std::make_unique<IfNode>(loc, std::move(condition), std::move(then_block), std::move(else_block));
    }

    // �����������������������䣨start var.name="name" { var1 string name="Peter"; ... }��
    std::unique_ptr<VarNode> parse_var_declaration() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_START); // ����start
        // ƥ��var.name�ؼ���
        if (current_token.type != TokenType::TOKEN_VAR_NAME) {
            throw ParseError(ErrorCode::ER_0003, current_token.location, "start������var.name���������������");
        }
        consume(TokenType::TOKEN_VAR_NAME); // ����var.name
        // �������������ƣ���="Peter"��
        if (current_token.type != TokenType::TOKEN_EQUALS) {
            throw ParseError(ErrorCode::ER_0003, current_token.location, "var.name��ȱ��=");
        }
        consume(TokenType::TOKEN_EQUALS);
        if (current_token.type != TokenType::TOKEN_STRING_LITERAL) {
            throw ParseError(ErrorCode::ER_1001, current_token.location, "var.name��ֵ�������ַ�������������\"Peter\"��");
        }
        std::string var_group_name = current_token.value;
        consume(TokenType::TOKEN_STRING_LITERAL);
        // �����������ڲ���var���var1 string name="Peter"��
        auto var_block = parse_block(); // ����{...}
        std::vector<VarNode::VarItem> var_items;
        for (auto& stmt : var_block->statements) {
            // ÿ��var�������"varN ���� ��=ֵ"��ʽ����var1 string name="Peter"��
            auto var_stmt = dynamic_cast<VariableDeclarationNode*>(stmt.get());
            if (!var_stmt) {
                throw ParseError(ErrorCode::ER_0003, stmt->location, "�����������ֻ�ܰ���varN��������var1 string name=\"Peter\"��");
            }
            // ��ȡvar��ʶ����"var1"��
            std::string var_id = var_stmt->var_name;
            if (var_id.substr(0, 3) != "var" || !std::isdigit(var_id[3])) {
                throw ParseError(ErrorCode::ER_0004, var_stmt->location, "���������������var+���ֿ�ͷ����var1��var2��");
            }
            // ��ȡ����������"name"��
            std::string var_key = var_stmt->var_name;
            // ����VarItem��ȷ��initial_value�ǿգ�
            if (!var_stmt->initial_value) {
                throw ParseError(ErrorCode::ER_0003, var_stmt->location, "������������ʼ������var1 string name=\"Peter\"��");
            }
            VarNode::VarItem item;
            item.var_id = var_id;
            item.var_type = var_stmt->var_type;
            item.var_key = var_key;
            item.var_value = std::move(var_stmt->initial_value); // �����ƶ�
            var_items.push_back(std::move(item)); // �����ƶ�
        }
        // ����end
        if (current_token.type == TokenType::TOKEN_END) {
            consume(TokenType::TOKEN_END);
        }
        return std::make_unique<VarNode>(loc, var_group_name, std::move(var_items));
    }

    // �����������¼������䣨detect keyboard.click="k" run { ... }��
    std::unique_ptr<DetectNode> parse_detect() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_DETECT); // ����detect
        // �����¼���������keyboard.click="k"��
        auto expr = parse_expr();
        auto event_cond = dynamic_cast<EventConditionExprNode*>(expr.get());
        if (!event_cond) {
            throw ParseError(ErrorCode::ER_0003, current_token.location, "detect��������¼���������keyboard.click=\"k\"��");
        }
        // ת��expr������Ȩ����������ָ�룩
        std::unique_ptr<EventConditionExprNode> event_condition(static_cast<EventConditionExprNode*>(expr.release()));
        // ��֤�¼������Ƿ�֧��
        if (!CommonUtils::is_supported_event(event_condition->event_type)) {
            throw ParseError(ErrorCode::ER_0003, event_condition->location,
                "��֧�ֵ��¼�����: " + event_condition->event_type +
                "��֧�ֵ��¼���input��click��enter��keyboard.click��");
        }
        // ����run�ؼ��ֺʹ����
        if (current_token.type != TokenType::TOKEN_RUN) {
            throw ParseError(ErrorCode::ER_0003, current_token.location, "�¼�������ȱ��run�ؼ���");
        }
        consume(TokenType::TOKEN_RUN);
        auto run_block = parse_block(); // ����{...}
        return std::make_unique<DetectNode>(loc, std::move(event_condition), std::move(run_block));
    }

    // ����������Forѭ����䣨for i 1 to 50 step 2 { ... }��
    std::unique_ptr<LoopNode> parse_for_loop() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_FOR); // ����for
        // ����ѭ����������i��
        if (current_token.type != TokenType::TOKEN_IDENTIFIER) {
            throw ParseError(ErrorCode::ER_0004, current_token.location, "for������ѭ������������i��");
        }
        std::string for_loop_var = current_token.value;
        consume(TokenType::TOKEN_IDENTIFIER);
        // ������ʼֵ����1��
        if (!is_numeric_token(current_token.type)) {
            throw ParseError(ErrorCode::ER_1001, current_token.location, "ѭ��������������ֵ��ʼֵ����1��");
        }
        auto for_start = parse_primary_expr();
        // ����to�ؼ���
        if (current_token.type != TokenType::TOKEN_IDENTIFIER || current_token.value != "to") {
            throw ParseError(ErrorCode::ER_0003, current_token.location, "��ʼֵ������to�ؼ��֣���1 to 50��");
        }
        consume(TokenType::TOKEN_IDENTIFIER); // ����to
        // ��������ֵ����50��
        if (!is_numeric_token(current_token.type)) {
            throw ParseError(ErrorCode::ER_1001, current_token.location, "to��������ֵ����ֵ����50��");
        }
        auto for_end = parse_primary_expr();
        // ������������ѡ��Ĭ��1��
        std::unique_ptr<ExpressionNode> for_step;
        if (current_token.type == TokenType::TOKEN_IDENTIFIER && current_token.value == "step") {
            consume(TokenType::TOKEN_IDENTIFIER); // ����step
            if (!is_numeric_token(current_token.type)) {
                throw ParseError(ErrorCode::ER_1001, current_token.location, "step��������ֵ��������2��");
            }
            for_step = parse_primary_expr();
        }
        else {
            // ����Ĭ��1������int�������ڵ㣩
            for_step = std::make_unique<LiteralExprNode>(loc, DataType::DATA_TYPE_INT, "1");
        }
        // ����ѭ����
        auto body = parse_block();
        // ����Forѭ���ڵ㣨������ȷ��LoopNode���캯����
        return std::make_unique<LoopNode>(
            loc,
            for_loop_var,
            std::move(for_start),
            std::move(for_end),
            std::move(for_step),
            std::move(body)
        );
    }

    // ��չ������reѭ����䣨֧��re wait time = 3000ms���޸�token_buffer���ʣ�
    std::unique_ptr<LoopNode> parse_re_loop() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_RE); // ����re
        // �����Ƿ�Ϊ����ѭ����infinite��
        bool is_infinite = false;
        if (current_token.type == TokenType::TOKEN_INFINITE) {
            is_infinite = true;
            consume(TokenType::TOKEN_INFINITE);
            // ����ѭ����ֱ�Ӹ�will re
            if (current_token.type != TokenType::TOKEN_WILL) {
                throw ParseError(ErrorCode::ER_0003, current_token.location, "infinite������will re����infinite will re { ... }��");
            }
        }
        else {
            // ����ѭ��������re number=3��
            if (current_token.type == TokenType::TOKEN_IDENTIFIER && current_token.value == "number") {
                consume(TokenType::TOKEN_IDENTIFIER); // ����number
                if (current_token.type != TokenType::TOKEN_EQUALS) {
                    throw ParseError(ErrorCode::ER_0003, current_token.location, "re number��ȱ��=");
                }
                consume(TokenType::TOKEN_EQUALS);
                auto condition = parse_primary_expr(); // ������������3��
                // �������Ƿ�Ϊ������
                auto lit = dynamic_cast<LiteralExprNode*>(condition.get());
                if (lit && lit->value_type == DataType::DATA_TYPE_INT) {
                    int count = std::stoi(lit->value);
                    if (count <= 0) {
                        throw ParseError(ErrorCode::ER_0005, condition->location, "reѭ����������Ϊ����������ǰ��" + std::to_string(count) + "��");
                    }
                }
            }
            else {
                throw ParseError(ErrorCode::ER_0003, current_token.location, "re������infinite��number=��������re number=3��");
            }
        }
        // ����reѭ����wait����ѡ����wait time = 3000ms��
        std::unique_ptr<ExpressionNode> re_wait_duration;
        std::string re_wait_unit = GalltConstants::DEFAULT_WAIT_UNIT;
        if (current_token.type == TokenType::TOKEN_WAIT) {
            consume(TokenType::TOKEN_WAIT); // ����wait
            if (current_token.type == TokenType::TOKEN_IDENTIFIER && current_token.value == "time") {
                consume(TokenType::TOKEN_IDENTIFIER); // ����time
                if (current_token.type != TokenType::TOKEN_EQUALS) {
                    throw ParseError(ErrorCode::ER_0003, current_token.location, "re wait time��ȱ��=");
                }
                consume(TokenType::TOKEN_EQUALS);
                // �����ȴ�ʱ��������tokenizer.parse_wait_number����ȡ��ֵToken��
                Token num_token = tokenizer.parse_wait_number();
                if (!is_numeric_token(num_token.type)) {
                    throw ParseError(ErrorCode::ER_1001, num_token.location, "re wait time��������ֵʱ������3000��");
                }
                // ����LiteralExprNode���޸�make_unique������������release��
                DataType num_type = (num_token.type == TokenType::TOKEN_INT_LITERAL) ?
                    DataType::DATA_TYPE_INT : DataType::DATA_TYPE_FLOAT;
                re_wait_duration = std::make_unique<LiteralExprNode>(
                    num_token.location,
                    num_type,
                    num_token.value
                );
                // �����ȴ���λ����tokenizer��ȡ��ͨ��public�ӿڣ�
                Token unit_token = tokenizer.next_token();
                if (unit_token.type != TokenType::TOKEN_WAIT_UNIT) {
                    // ���˵�λToken��ͨ��tokenizer��public�ӿڣ�
                    tokenizer.push_back_token(unit_token);
                    throw ParseError(ErrorCode::ER_0003, unit_token.location, "re wait time��ȱ�ٺϷ���λ��ms/s/min/hour��");
                }
                re_wait_unit = unit_token.value;
                // ��֤��λ�Ϸ���
                if (GalltConstants::WAIT_SUPPORTED_UNITS.find(re_wait_unit) == GalltConstants::WAIT_SUPPORTED_UNITS.end()) {
                    throw ParseError(ErrorCode::ER_0003, unit_token.location, "��֧�ֵ�ʱ�䵥λ��" + re_wait_unit + "��֧�֣�ms/s/min/hour��");
                }
            }
        }
        // ����will re�����
        if (current_token.type != TokenType::TOKEN_WILL) {
            throw ParseError(ErrorCode::ER_0003, current_token.location, "re��ȱ��will�ؼ��֣���will re { ... }��");
        }
        consume(TokenType::TOKEN_WILL);
        if (current_token.type != TokenType::TOKEN_RE) {
            throw ParseError(ErrorCode::ER_0003, current_token.location, "will��ȱ��re�ؼ��֣���will re { ... }��");
        }
        consume(TokenType::TOKEN_RE);
        auto body = parse_block(); // ����{...}
        // ����reѭ���ڵ㣨���ô�wait�Ĺ��캯����
        return std::make_unique<LoopNode>(
            loc,
            std::move(re_wait_duration), // �ȴ�ʱ������Ϊ�գ�
            std::move(body),
            std::move(re_wait_duration), // �޸����˴�ӦΪcondition��ԭ�������˳�����
            re_wait_unit,
            is_infinite
        );
    }

    // ��չ������ѭ����䣨����while/for/re������For��re wait֧�֣�
    std::unique_ptr<LoopNode> parse_loop() {
        SourceLocation loc = current_token.location;
        if (current_token.type == TokenType::TOKEN_WHILE) {
            // whileѭ����ԭ���߼���
            consume(TokenType::TOKEN_WHILE);
            auto condition = parse_expr();
            auto body = parse_block();
            return std::make_unique<LoopNode>(loc, LoopNode::LoopType::LOOP_WHILE,
                std::move(condition), std::move(body));
        }
        else if (current_token.type == TokenType::TOKEN_FOR) {
            // Forѭ���������߼���
            return parse_for_loop();
        }
        else if (current_token.type == TokenType::TOKEN_RE) {
            // reѭ������չ�߼���֧��wait��
            return parse_re_loop();
        }
        else {
            throw ParseError(ErrorCode::ER_0003, current_token.location, "δ֪ѭ������");
        }
    }

    // ��չ�������ȴ���䣨֧�ֶ൥λ���޸�token_buffer���ʣ�
    std::unique_ptr<WaitNode> parse_wait() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_WAIT); // ����wait
        // ����wait�﷨����time = 3000ms��
        if (current_token.type == TokenType::TOKEN_IDENTIFIER && current_token.value == "time") {
            consume(TokenType::TOKEN_IDENTIFIER); // ����time
            if (current_token.type != TokenType::TOKEN_EQUALS) {
                throw ParseError(ErrorCode::ER_0003, current_token.location, "wait time��ȱ��=");
            }
            consume(TokenType::TOKEN_EQUALS);
        }
        // �����ȴ�ʱ���͵�λ������tokenizer.parse_wait_number��
        Token num_token = tokenizer.parse_wait_number();
        if (!is_numeric_token(num_token.type)) {
            throw ParseError(ErrorCode::ER_1001, num_token.location, "wait��������ֵʱ������3000��");
        }
        // ����ʱ�����ʽ�ڵ㣨�޸�make_unique������
        DataType num_type = (num_token.type == TokenType::TOKEN_INT_LITERAL) ?
            DataType::DATA_TYPE_INT : DataType::DATA_TYPE_FLOAT;
        auto duration = std::make_unique<LiteralExprNode>(
            num_token.location,
            num_type,
            num_token.value
        );
        // ������λ��ͨ��tokenizer.next_token����ֱ�ӷ���buffer��
        Token unit_token = tokenizer.next_token();
        std::string unit = GalltConstants::DEFAULT_WAIT_UNIT;
        if (unit_token.type == TokenType::TOKEN_WAIT_UNIT) {
            unit = unit_token.value;
            // ��֤��λ�Ϸ���
            if (GalltConstants::WAIT_SUPPORTED_UNITS.find(unit) == GalltConstants::WAIT_SUPPORTED_UNITS.end()) {
                throw ParseError(ErrorCode::ER_0003, unit_token.location, "��֧�ֵ�ʱ�䵥λ��" + unit + "��֧�֣�ms/s/min/hour��");
            }
        }
        else {
            // ���˵�λToken��ͨ��tokenizer��public�ӿڣ�
            tokenizer.push_back_token(unit_token);
            throw ParseError(ErrorCode::ER_0003, unit_token.location, "waitʱ����ȱ�ٺϷ���λ��ms/s/min/hour��");
        }
        consume(TokenType::TOKEN_SEMICOLON); // ����;
        return std::make_unique<WaitNode>(loc, std::move(duration), unit);
    }

    // �����߳���䣨����ԭ���߼���
    std::unique_ptr<ThreadNode> parse_thread() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_THREAD); // ����thread
        if (current_token.type != TokenType::TOKEN_IDENTIFIER) {
            throw ParseError(ErrorCode::ER_0004, current_token.location, "�߳��������Ǳ�ʶ��");
        }
        std::string thread_name = current_token.value;
        consume(TokenType::TOKEN_IDENTIFIER);
        auto body = parse_block();
        return std::make_unique<ThreadNode>(loc, thread_name, std::move(body));
    }

    // ����������䣨����ԭ���߼���
    std::unique_ptr<ReturnNode> parse_return() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_RETURN); // ����return
        std::unique_ptr<ExpressionNode> return_value;
        if (current_token.type != TokenType::TOKEN_SEMICOLON) {
            return_value = parse_expr();
        }
        consume(TokenType::TOKEN_SEMICOLON); // ����;
        return std::make_unique<ReturnNode>(loc, std::move(return_value));
    }

    // �����ͷ�main��䣨����ԭ���߼���
    std::unique_ptr<ReleaseMainNode> parse_release_main() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_R_MAIN); // ����r.main
        consume(TokenType::TOKEN_SEMICOLON); // ����;
        return std::make_unique<ReleaseMainNode>(loc);
    }

    // �����˳���䣨����ԭ���߼���
    std::unique_ptr<ExitNode> parse_exit() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_EXIT); // ����exit
        consume(TokenType::TOKEN_SEMICOLON); // ����;
        return std::make_unique<ExitNode>(loc);
    }

    // ��չ������������䣨����VarNode��DetectNode��֧���޸�Ԥ���߼���
    std::unique_ptr<StatementNode> parse_statement() {
        switch (current_token.type) {
        case TokenType::TOKEN_DEFINE:
            return parse_variable_declaration();
        case TokenType::TOKEN_IDENTIFIER:
            // ����Ƿ�Ϊcount���ʽ����count(5*8);��
            if (current_token.value == "count") {
                auto count_expr = parse_primary_expr();
                consume(TokenType::TOKEN_SEMICOLON);
                // count���ʽ��Ƕ���������ֵ�У��˴��ݲ�����֧��
                throw ParseError(ErrorCode::ER_0003, current_token.location, "count���ʽ�����������ֵ��ʹ�ã���output count(5*8);��");
            }
            return parse_assignment();
        case TokenType::TOKEN_OUTPUT:
            return parse_output();
        case TokenType::TOKEN_INPUT:
            return parse_input();
        case TokenType::TOKEN_IF:
            return parse_if();
        case TokenType::TOKEN_WHILE:
        case TokenType::TOKEN_FOR:
        case TokenType::TOKEN_RE:
            return parse_loop();
        case TokenType::TOKEN_WAIT:
            return parse_wait();
        case TokenType::TOKEN_THREAD:
            return parse_thread();
        case TokenType::TOKEN_RETURN:
            return parse_return();
        case TokenType::TOKEN_R_MAIN:
            return parse_release_main();
        case TokenType::TOKEN_EXIT:
            return parse_exit();
        case TokenType::TOKEN_LBRACE:
            return parse_block();
            // �����������������������start var.name=...��
        case TokenType::TOKEN_START: {
            // Ԥ����һ��Token��ͨ��public�ӿڣ���ֱ�ӷ���buffer��
            Token next = peek_next_token();
            if (next.type == TokenType::TOKEN_VAR_NAME) {
                return parse_var_declaration();
            }
            // ������ͨstart����start main��������parse_main����
            throw ParseError(ErrorCode::ER_0003, current_token.location, "start������main��var.name����start main �� start var.name=\"Peter\"��");
        }
                                   // �����������¼�������
        case TokenType::TOKEN_DETECT:
            return parse_detect();
        default:
            throw ParseError(ErrorCode::ER_0003, current_token.location,
                "δ֪�������: " + current_token.value);
        }
    }

    // ------------------------------ ������������������޸�token_buffer���ʣ� ------------------------------
    // �����������壨����ԭ���߼���
    std::unique_ptr<FunctionDefinitionNode> parse_function_definition() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_FUNCTION); // ����function
        // ������������
        DataType return_type = DataType::DATA_TYPE_UNKNOWN;
        if (current_token.type == TokenType::TOKEN_INT) {
            return_type = DataType::DATA_TYPE_INT;
            consume(TokenType::TOKEN_INT);
        }
        else if (current_token.type == TokenType::TOKEN_FLOAT) {
            return_type = DataType::DATA_TYPE_FLOAT;
            consume(TokenType::TOKEN_FLOAT);
        }
        else if (current_token.type == TokenType::TOKEN_DOUBLE) {
            return_type = DataType::DATA_TYPE_DOUBLE;
            consume(TokenType::TOKEN_DOUBLE);
        }
        else if (current_token.type == TokenType::TOKEN_STRING) {
            return_type = DataType::DATA_TYPE_STRING;
            consume(TokenType::TOKEN_STRING);
        }
        else {
            throw ParseError(ErrorCode::ER_1001, current_token.location, "�����������ʹ���");
        }
        // ����������
        if (current_token.type != TokenType::TOKEN_IDENTIFIER) {
            throw ParseError(ErrorCode::ER_0004, current_token.location, "�����������Ǳ�ʶ��");
        }
        std::string func_name = current_token.value;
        consume(TokenType::TOKEN_IDENTIFIER);
        // ���������б�
        consume(TokenType::TOKEN_LPAREN); // ����(
        std::vector<VariableDeclarationNode> params;
        while (current_token.type != TokenType::TOKEN_RPAREN) {
            DataType param_type;
            if (current_token.type == TokenType::TOKEN_INT) {
                param_type = DataType::DATA_TYPE_INT;
                consume(TokenType::TOKEN_INT);
            }
            else if (current_token.type == TokenType::TOKEN_FLOAT) {
                param_type = DataType::DATA_TYPE_FLOAT;
                consume(TokenType::TOKEN_FLOAT);
            }
            else if (current_token.type == TokenType::TOKEN_DOUBLE) {
                param_type = DataType::DATA_TYPE_DOUBLE;
                consume(TokenType::TOKEN_DOUBLE);
            }
            else if (current_token.type == TokenType::TOKEN_STRING) {
                param_type = DataType::DATA_TYPE_STRING;
                consume(TokenType::TOKEN_STRING);
            }
            else {
                throw ParseError(ErrorCode::ER_1001, current_token.location, "�������ʹ���");
            }
            if (current_token.type != TokenType::TOKEN_IDENTIFIER) {
                throw ParseError(ErrorCode::ER_0004, current_token.location, "�����������Ǳ�ʶ��");
            }
            std::string param_name = current_token.value;
            consume(TokenType::TOKEN_IDENTIFIER);
            params.emplace_back(current_token.location, param_type, param_name, false, nullptr, nullptr);
            if (current_token.type == TokenType::TOKEN_SEMICOLON) {
                consume(TokenType::TOKEN_SEMICOLON);
            }
            else {
                break;
            }
        }
        consume(TokenType::TOKEN_RPAREN); // ����)
        // ����������
        auto body = parse_block();
        return std::make_unique<FunctionDefinitionNode>(loc, func_name, return_type,
            std::move(params), std::move(body));
    }

    // ����main����飨����ԭ���߼���
    std::unique_ptr<MainNode> parse_main() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_START); // ����start
        if (current_token.type != TokenType::TOKEN_MAIN) {
            throw ParseError(ErrorCode::ER_0003, current_token.location, "start������main");
        }
        consume(TokenType::TOKEN_MAIN);
        // main���ƣ���ѡ��
        std::string main_name;
        if (current_token.type == TokenType::TOKEN_IDENTIFIER) {
            main_name = current_token.value;
            consume(TokenType::TOKEN_IDENTIFIER);
        }
        // ����main��
        auto main_body = parse_block();
        consume(TokenType::TOKEN_END); // ����end
        return std::make_unique<MainNode>(loc, main_name, std::move(main_body));
    }

public:
    // ���캯�������մʷ�����������ʼ����ǰToken
    explicit Parser(Tokenizer& t) : tokenizer(t) {
        current_token = tokenizer.next_token(); // Ԥ����һ��Token
    }

    // ��չ������������������ȫ���¼����ڵ��ռ����޸�token_buffer���ʣ�
    std::unique_ptr<ProgramNode> parse_program() {
        SourceLocation loc = current_token.location;
        auto program = std::make_unique<ProgramNode>(loc);
        // �����ɺ������塢main�顢ȫ���¼�������
        while (current_token.type != TokenType::TOKEN_EOF) {
            if (current_token.type == TokenType::TOKEN_FUNCTION) {
                program->functions.push_back(std::move(parse_function_definition())); // �޸�������std::move
            }
            else if (current_token.type == TokenType::TOKEN_START) {
                // Ԥ���ж���main�����������������ͨ��public�ӿڣ�
                Token next = peek_next_token();
                if (next.type == TokenType::TOKEN_MAIN) {
                    program->mains.push_back(std::move(parse_main())); // �޸�������std::move
                }
                else if (next.type == TokenType::TOKEN_VAR_NAME) {
                    auto var_node = parse_var_declaration();
                    // ����ǰ��main������Ĭ��main
                    if (program->mains.empty()) {
                        // ע�⣺�����û����ų�ʼ�� vector<unique_ptr<...>>������ initializer_list ���¿��� unique_ptr��C2280��
                        std::vector<std::unique_ptr<StatementNode>> tmp_stmts;
                        tmp_stmts.push_back(std::move(var_node)); // ��ȷ�ƶ�����
                        auto default_main_body = std::make_unique<BlockNode>(loc, std::move(tmp_stmts));
                        program->mains.push_back(std::make_unique<MainNode>(
                            loc,
                            "default_main",
                            std::move(default_main_body)
                        ));
                    }
                    else {
                        program->mains.back()->main_body->statements.push_back(std::move(var_node)); // �����ƶ�
                    }
                }
                else {
                    throw ParseError(ErrorCode::ER_0003, current_token.location, "start������main��var.name����start main �� start var.name=\"Peter\"��");
                }
            }
            else if (current_token.type == TokenType::TOKEN_DETECT) {
                program->detect_events.push_back(std::move(parse_detect())); // �޸�������std::move
            }
            else {
                throw ParseError(ErrorCode::ER_0003, current_token.location, "���򶥲�ֻ���Ǻ������塢main���ȫ���¼����");
            }
        }
        return program;
    }

    // ��ȡ���������в����Ĵ����б�
    const std::vector<ParseError>& get_errors() const {
        return errors;
    }
};

#endif // PARSER_H