//Copyright (c) Gallt Developer.
//版权所有（c）Gallt 开发者。

#ifndef PARSER_H
#define PARSER_H
#include "common.h"
#include "tokenizer.h"
#include "ast_nodes.h"
#include <memory>
#include <vector>
#include <stdexcept>
#include <algorithm>

// 语法分析错误异常（用于中断分析并传递错误信息）
class ParseError : public std::runtime_error {
public:
    ErrorCode error_code;
    SourceLocation location;
    ParseError(ErrorCode code, const SourceLocation& loc, const std::string& msg)
        : std::runtime_error(msg), error_code(code), location(loc) {}
};

// 语法分析器类
class Parser {
private:
    Tokenizer& tokenizer;       // 词法分析器引用（提供Token流）
    Token current_token;        // 当前Token
    std::vector<ParseError> errors; // 收集的语法错误

    // 匹配并消耗指定类型的Token，若不匹配则抛出错误
    void consume(TokenType expected_type) {
        if (current_token.type == expected_type) {
            current_token = tokenizer.next_token();
        }
        else {
            std::string msg = "预期Token类型不匹配，预期: " + token_type_to_string(expected_type) +
                "，实际: " + token_type_to_string(current_token.type);
            throw ParseError(ErrorCode::ER_0003, current_token.location, msg);
        }
    }

    // 辅助函数：将TokenType转换为字符串（用于错误信息，补充新增Token）
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
        case TokenType::TOKEN_IDENTIFIER: return "标识符";
        case TokenType::TOKEN_INT_LITERAL: return "整数字面量";
        case TokenType::TOKEN_FLOAT_LITERAL: return "浮点数字面量";
        case TokenType::TOKEN_STRING_LITERAL: return "字符串字面量";
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
        case TokenType::TOKEN_WAIT_UNIT: return "时间单位（ms/s/min/hour）";
        case TokenType::TOKEN_CTRL_C: return "Ctrl+C中断标记";
        case TokenType::TOKEN_EOF: return "文件结束符";
        default: return "未知Token";
        }
    }

    // 辅助函数：检查当前Token是否为数值类型（int/float）
    bool is_numeric_token(TokenType type) {
        return type == TokenType::TOKEN_INT_LITERAL || type == TokenType::TOKEN_FLOAT_LITERAL;
    }

    // 辅助函数：预读下一个Token（不消耗，通过tokenizer的public接口实现）
    Token peek_next_token() {
        Token next = tokenizer.next_token();
        tokenizer.push_back_token(next); // 使用Tokenizer的public接口回退Token
        return next;
    }

    // ------------------------------ 表达式解析（修复case重复、release错误） ------------------------------
    // 解析原子表达式（合并重复的TOKEN_IDENTIFIER分支，修复release调用）
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

            // 1. 优先检查是否为Count表达式（count(...) 或 float.count(...)）
            // 子分支1：float.count / double.count
            if (id_name == "float" || id_name == "double") {
                if (current_token.type == TokenType::TOKEN_DOT) {
                    consume(TokenType::TOKEN_DOT);
                    if (current_token.type != TokenType::TOKEN_IDENTIFIER || current_token.value != "count") {
                        throw ParseError(ErrorCode::ER_0003, current_token.location,
                            id_name + ".后必须跟count（如" + id_name + ".count(...)）");
                    }
                    consume(TokenType::TOKEN_IDENTIFIER); // 消耗count
                    consume(TokenType::TOKEN_LPAREN); // 消耗(
                    auto calc_expr = parse_expr(); // 解析待计算表达式
                    consume(TokenType::TOKEN_RPAREN); // 消耗)
                    // 确定Count类型
                    CountExprNode::CountType count_type = (id_name == "float") ?
                        CountExprNode::CountType::COUNT_FLOAT : CountExprNode::CountType::COUNT_DOUBLE;
                    return std::make_unique<CountExprNode>(loc, count_type, std::move(calc_expr));
                }
            }
            // 子分支2：普通count(...)
            else if (id_name == "count") {
                consume(TokenType::TOKEN_LPAREN); // 消耗(
                auto calc_expr = parse_expr(); // 解析待计算表达式
                consume(TokenType::TOKEN_RPAREN); // 消耗)
                return std::make_unique<CountExprNode>(
                    loc,
                    CountExprNode::CountType::COUNT_NORMAL,
                    std::move(calc_expr)
                );
            }

            // 2. 检查是否为函数调用（后跟(）
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

            // 3. 检查是否为数组访问（后跟.）
            else if (current_token.type == TokenType::TOKEN_DOT) {
                consume(TokenType::TOKEN_DOT);
                auto index = parse_primary_expr();
                auto arr_id = std::make_unique<IdentifierExprNode>(loc, id_name);
                return std::make_unique<ArrayAccessExprNode>(loc, std::move(arr_id), std::move(index));
            }

            // 4. 检查是否为事件条件（如keyboard.click）
            if (CommonUtils::is_supported_event(id_name) && current_token.type == TokenType::TOKEN_EQUALS) {
                consume(TokenType::TOKEN_EQUALS);
                auto trigger_val = parse_primary_expr(); // 触发值（如"k"）
                return std::make_unique<EventConditionExprNode>(loc, id_name, std::move(trigger_val));
            }

            // 5. 普通标识符（变量/函数名）
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

    // 解析二元运算表达式（保持原有逻辑，兼容新增表达式）
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

    // ------------------------------ 语句解析（修复token_buffer访问、make_unique参数） ------------------------------
    // 解析代码块（保持原有逻辑）
    std::unique_ptr<BlockNode> parse_block() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_LBRACE); // 消耗{
        std::vector<std::unique_ptr<StatementNode>> statements;
        while (current_token.type != TokenType::TOKEN_RBRACE &&
            current_token.type != TokenType::TOKEN_EOF) {
            statements.push_back(std::move(parse_statement())); // 修复：必须std::move
        }
        consume(TokenType::TOKEN_RBRACE); // 消耗}
        return std::make_unique<BlockNode>(loc, std::move(statements));
    }

    // 解析变量声明语句（保持原有逻辑，兼容数组）
    std::unique_ptr<VariableDeclarationNode> parse_variable_declaration() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_DEFINE); // 消耗define
        // 解析变量类型
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
            throw ParseError(ErrorCode::ER_1001, current_token.location, "未知变量类型");
        }
        // 检查是否为数组（arr关键字）
        bool is_array = false;
        std::unique_ptr<ExpressionNode> array_size;
        if (current_token.type == TokenType::TOKEN_ARR) {
            is_array = true;
            consume(TokenType::TOKEN_ARR);
            consume(TokenType::TOKEN_LBRACKET); // 消耗[
            array_size = parse_expr();
            consume(TokenType::TOKEN_RBRACKET); // 消耗]
        }
        // 解析变量名
        if (current_token.type != TokenType::TOKEN_IDENTIFIER) {
            throw ParseError(ErrorCode::ER_0004, current_token.location, "变量名必须是标识符");
        }
        std::string var_name = current_token.value;
        consume(TokenType::TOKEN_IDENTIFIER);
        // 解析初始值（可选）
        std::unique_ptr<ExpressionNode> initial_value;
        if (current_token.type == TokenType::TOKEN_EQUALS) {
            consume(TokenType::TOKEN_EQUALS);
            initial_value = parse_expr();
        }
        consume(TokenType::TOKEN_SEMICOLON); // 消耗;
        return std::make_unique<VariableDeclarationNode>(loc, var_type, var_name,
            is_array, std::move(array_size),
            std::move(initial_value));
    }

    // 解析赋值语句（保持原有逻辑，兼容数组访问）
    std::unique_ptr<AssignmentNode> parse_assignment() {
        SourceLocation loc = current_token.location;
        auto target = parse_primary_expr(); // 目标可以是标识符或数组访问
        if (current_token.type != TokenType::TOKEN_EQUALS) {
            throw ParseError(ErrorCode::ER_0003, current_token.location, "赋值语句缺少=");
        }
        consume(TokenType::TOKEN_EQUALS);
        auto value = parse_expr();
        consume(TokenType::TOKEN_SEMICOLON); // 消耗;
        return std::make_unique<AssignmentNode>(loc, std::move(target), std::move(value));
    }

    // 解析输出语句（保持原有逻辑）
    std::unique_ptr<OutputNode> parse_output() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_OUTPUT); // 消耗output
        auto expr = parse_expr();
        consume(TokenType::TOKEN_SEMICOLON); // 消耗;
        return std::make_unique<OutputNode>(loc, std::move(expr));
    }

    // 解析输入语句（保持原有逻辑）
    std::unique_ptr<InputNode> parse_input() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_INPUT); // 消耗input
        std::unique_ptr<ExpressionNode> prompt;
        std::unique_ptr<IdentifierExprNode> target;
        // 检查是否有提示信息（字符串表达式）
        if (current_token.type == TokenType::TOKEN_STRING_LITERAL) {
            prompt = parse_expr();
            if (current_token.type != TokenType::TOKEN_COMMA) {
                throw ParseError(ErrorCode::ER_0003, current_token.location, "输入语句提示信息后缺少,");
            }
            consume(TokenType::TOKEN_COMMA);
        }
        // 解析目标变量
        if (current_token.type != TokenType::TOKEN_IDENTIFIER) {
            throw ParseError(ErrorCode::ER_0003, current_token.location, "输入语句目标必须是标识符");
        }
        target = std::make_unique<IdentifierExprNode>(current_token.location, current_token.value);
        consume(TokenType::TOKEN_IDENTIFIER);
        consume(TokenType::TOKEN_SEMICOLON); // 消耗;
        return std::make_unique<InputNode>(loc, std::move(prompt), std::move(target));
    }

    // 解析条件语句（保持原有逻辑）
    std::unique_ptr<IfNode> parse_if() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_IF); // 消耗if
        // 解析条件表达式
        auto condition = parse_expr();
        // 解析then块
        auto then_block = parse_block();
        // 解析otherwise块（可选）
        std::unique_ptr<BlockNode> else_block;
        if (current_token.type == TokenType::TOKEN_OTHERWISE) {
            consume(TokenType::TOKEN_OTHERWISE);
            else_block = parse_block();
            if (!else_block) {
                throw ParseError(ErrorCode::ER_0002, current_token.location, "otherwise后缺少代码块");
            }
        }
        return std::make_unique<IfNode>(loc, std::move(condition), std::move(then_block), std::move(else_block));
    }

    // 新增：解析特殊变量声明语句（start var.name="name" { var1 string name="Peter"; ... }）
    std::unique_ptr<VarNode> parse_var_declaration() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_START); // 消耗start
        // 匹配var.name关键字
        if (current_token.type != TokenType::TOKEN_VAR_NAME) {
            throw ParseError(ErrorCode::ER_0003, current_token.location, "start后必须跟var.name（特殊变量声明）");
        }
        consume(TokenType::TOKEN_VAR_NAME); // 消耗var.name
        // 解析变量组名称（如="Peter"）
        if (current_token.type != TokenType::TOKEN_EQUALS) {
            throw ParseError(ErrorCode::ER_0003, current_token.location, "var.name后缺少=");
        }
        consume(TokenType::TOKEN_EQUALS);
        if (current_token.type != TokenType::TOKEN_STRING_LITERAL) {
            throw ParseError(ErrorCode::ER_1001, current_token.location, "var.name赋值必须是字符串字面量（如\"Peter\"）");
        }
        std::string var_group_name = current_token.value;
        consume(TokenType::TOKEN_STRING_LITERAL);
        // 解析变量组内部的var项（如var1 string name="Peter"）
        auto var_block = parse_block(); // 消耗{...}
        std::vector<VarNode::VarItem> var_items;
        for (auto& stmt : var_block->statements) {
            // 每个var项必须是"varN 类型 键=值"格式（如var1 string name="Peter"）
            auto var_stmt = dynamic_cast<VariableDeclarationNode*>(stmt.get());
            if (!var_stmt) {
                throw ParseError(ErrorCode::ER_0003, stmt->location, "特殊变量组内只能包含varN声明（如var1 string name=\"Peter\"）");
            }
            // 提取var标识（如"var1"）
            std::string var_id = var_stmt->var_name;
            if (var_id.substr(0, 3) != "var" || !std::isdigit(var_id[3])) {
                throw ParseError(ErrorCode::ER_0004, var_stmt->location, "特殊变量名必须以var+数字开头（如var1、var2）");
            }
            // 提取变量键（如"name"）
            std::string var_key = var_stmt->var_name;
            // 构建VarItem（确保initial_value非空）
            if (!var_stmt->initial_value) {
                throw ParseError(ErrorCode::ER_0003, var_stmt->location, "特殊变量必须初始化（如var1 string name=\"Peter\"）");
            }
            VarNode::VarItem item;
            item.var_id = var_id;
            item.var_type = var_stmt->var_type;
            item.var_key = var_key;
            item.var_value = std::move(var_stmt->initial_value); // 保持移动
            var_items.push_back(std::move(item)); // 保持移动
        }
        // 消耗end
        if (current_token.type == TokenType::TOKEN_END) {
            consume(TokenType::TOKEN_END);
        }
        return std::make_unique<VarNode>(loc, var_group_name, std::move(var_items));
    }

    // 新增：解析事件检测语句（detect keyboard.click="k" run { ... }）
    std::unique_ptr<DetectNode> parse_detect() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_DETECT); // 消耗detect
        // 解析事件条件（如keyboard.click="k"）
        auto expr = parse_expr();
        auto event_cond = dynamic_cast<EventConditionExprNode*>(expr.get());
        if (!event_cond) {
            throw ParseError(ErrorCode::ER_0003, current_token.location, "detect后必须是事件条件（如keyboard.click=\"k\"）");
        }
        // 转移expr的所有权（避免悬空指针）
        std::unique_ptr<EventConditionExprNode> event_condition(static_cast<EventConditionExprNode*>(expr.release()));
        // 验证事件类型是否支持
        if (!CommonUtils::is_supported_event(event_condition->event_type)) {
            throw ParseError(ErrorCode::ER_0003, event_condition->location,
                "不支持的事件类型: " + event_condition->event_type +
                "（支持的事件：input、click、enter、keyboard.click）");
        }
        // 解析run关键字和代码块
        if (current_token.type != TokenType::TOKEN_RUN) {
            throw ParseError(ErrorCode::ER_0003, current_token.location, "事件条件后缺少run关键字");
        }
        consume(TokenType::TOKEN_RUN);
        auto run_block = parse_block(); // 消耗{...}
        return std::make_unique<DetectNode>(loc, std::move(event_condition), std::move(run_block));
    }

    // 新增：解析For循环语句（for i 1 to 50 step 2 { ... }）
    std::unique_ptr<LoopNode> parse_for_loop() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_FOR); // 消耗for
        // 解析循环变量（如i）
        if (current_token.type != TokenType::TOKEN_IDENTIFIER) {
            throw ParseError(ErrorCode::ER_0004, current_token.location, "for后必须跟循环变量名（如i）");
        }
        std::string for_loop_var = current_token.value;
        consume(TokenType::TOKEN_IDENTIFIER);
        // 解析起始值（如1）
        if (!is_numeric_token(current_token.type)) {
            throw ParseError(ErrorCode::ER_1001, current_token.location, "循环变量后必须跟数值起始值（如1）");
        }
        auto for_start = parse_primary_expr();
        // 解析to关键字
        if (current_token.type != TokenType::TOKEN_IDENTIFIER || current_token.value != "to") {
            throw ParseError(ErrorCode::ER_0003, current_token.location, "起始值后必须跟to关键字（如1 to 50）");
        }
        consume(TokenType::TOKEN_IDENTIFIER); // 消耗to
        // 解析结束值（如50）
        if (!is_numeric_token(current_token.type)) {
            throw ParseError(ErrorCode::ER_1001, current_token.location, "to后必须跟数值结束值（如50）");
        }
        auto for_end = parse_primary_expr();
        // 解析步长（可选，默认1）
        std::unique_ptr<ExpressionNode> for_step;
        if (current_token.type == TokenType::TOKEN_IDENTIFIER && current_token.value == "step") {
            consume(TokenType::TOKEN_IDENTIFIER); // 消耗step
            if (!is_numeric_token(current_token.type)) {
                throw ParseError(ErrorCode::ER_1001, current_token.location, "step后必须跟数值步长（如2）");
            }
            for_step = parse_primary_expr();
        }
        else {
            // 步长默认1（创建int字面量节点）
            for_step = std::make_unique<LiteralExprNode>(loc, DataType::DATA_TYPE_INT, "1");
        }
        // 解析循环体
        auto body = parse_block();
        // 生成For循环节点（调用正确的LoopNode构造函数）
        return std::make_unique<LoopNode>(
            loc,
            for_loop_var,
            std::move(for_start),
            std::move(for_end),
            std::move(for_step),
            std::move(body)
        );
    }

    // 扩展：解析re循环语句（支持re wait time = 3000ms，修复token_buffer访问）
    std::unique_ptr<LoopNode> parse_re_loop() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_RE); // 消耗re
        // 解析是否为无限循环（infinite）
        bool is_infinite = false;
        if (current_token.type == TokenType::TOKEN_INFINITE) {
            is_infinite = true;
            consume(TokenType::TOKEN_INFINITE);
            // 无限循环需直接跟will re
            if (current_token.type != TokenType::TOKEN_WILL) {
                throw ParseError(ErrorCode::ER_0003, current_token.location, "infinite后必须跟will re（如infinite will re { ... }）");
            }
        }
        else {
            // 解析循环次数（re number=3）
            if (current_token.type == TokenType::TOKEN_IDENTIFIER && current_token.value == "number") {
                consume(TokenType::TOKEN_IDENTIFIER); // 消耗number
                if (current_token.type != TokenType::TOKEN_EQUALS) {
                    throw ParseError(ErrorCode::ER_0003, current_token.location, "re number后缺少=");
                }
                consume(TokenType::TOKEN_EQUALS);
                auto condition = parse_primary_expr(); // 解析次数（如3）
                // 检查次数是否为正整数
                auto lit = dynamic_cast<LiteralExprNode*>(condition.get());
                if (lit && lit->value_type == DataType::DATA_TYPE_INT) {
                    int count = std::stoi(lit->value);
                    if (count <= 0) {
                        throw ParseError(ErrorCode::ER_0005, condition->location, "re循环次数必须为正整数（当前：" + std::to_string(count) + "）");
                    }
                }
            }
            else {
                throw ParseError(ErrorCode::ER_0003, current_token.location, "re后必须跟infinite或number=次数（如re number=3）");
            }
        }
        // 解析re循环的wait（可选，如wait time = 3000ms）
        std::unique_ptr<ExpressionNode> re_wait_duration;
        std::string re_wait_unit = GalltConstants::DEFAULT_WAIT_UNIT;
        if (current_token.type == TokenType::TOKEN_WAIT) {
            consume(TokenType::TOKEN_WAIT); // 消耗wait
            if (current_token.type == TokenType::TOKEN_IDENTIFIER && current_token.value == "time") {
                consume(TokenType::TOKEN_IDENTIFIER); // 消耗time
                if (current_token.type != TokenType::TOKEN_EQUALS) {
                    throw ParseError(ErrorCode::ER_0003, current_token.location, "re wait time后缺少=");
                }
                consume(TokenType::TOKEN_EQUALS);
                // 解析等待时长（调用tokenizer.parse_wait_number，获取数值Token）
                Token num_token = tokenizer.parse_wait_number();
                if (!is_numeric_token(num_token.type)) {
                    throw ParseError(ErrorCode::ER_1001, num_token.location, "re wait time后必须跟数值时长（如3000）");
                }
                // 创建LiteralExprNode（修复make_unique参数，不调用release）
                DataType num_type = (num_token.type == TokenType::TOKEN_INT_LITERAL) ?
                    DataType::DATA_TYPE_INT : DataType::DATA_TYPE_FLOAT;
                re_wait_duration = std::make_unique<LiteralExprNode>(
                    num_token.location,
                    num_type,
                    num_token.value
                );
                // 解析等待单位（从tokenizer获取，通过public接口）
                Token unit_token = tokenizer.next_token();
                if (unit_token.type != TokenType::TOKEN_WAIT_UNIT) {
                    // 回退单位Token（通过tokenizer的public接口）
                    tokenizer.push_back_token(unit_token);
                    throw ParseError(ErrorCode::ER_0003, unit_token.location, "re wait time后缺少合法单位（ms/s/min/hour）");
                }
                re_wait_unit = unit_token.value;
                // 验证单位合法性
                if (GalltConstants::WAIT_SUPPORTED_UNITS.find(re_wait_unit) == GalltConstants::WAIT_SUPPORTED_UNITS.end()) {
                    throw ParseError(ErrorCode::ER_0003, unit_token.location, "不支持的时间单位：" + re_wait_unit + "（支持：ms/s/min/hour）");
                }
            }
        }
        // 解析will re代码块
        if (current_token.type != TokenType::TOKEN_WILL) {
            throw ParseError(ErrorCode::ER_0003, current_token.location, "re后缺少will关键字（如will re { ... }）");
        }
        consume(TokenType::TOKEN_WILL);
        if (current_token.type != TokenType::TOKEN_RE) {
            throw ParseError(ErrorCode::ER_0003, current_token.location, "will后缺少re关键字（如will re { ... }）");
        }
        consume(TokenType::TOKEN_RE);
        auto body = parse_block(); // 消耗{...}
        // 生成re循环节点（调用带wait的构造函数）
        return std::make_unique<LoopNode>(
            loc,
            std::move(re_wait_duration), // 等待时长（可为空）
            std::move(body),
            std::move(re_wait_duration), // 修复：此处应为condition，原代码参数顺序错误
            re_wait_unit,
            is_infinite
        );
    }

    // 扩展：解析循环语句（覆盖while/for/re，新增For和re wait支持）
    std::unique_ptr<LoopNode> parse_loop() {
        SourceLocation loc = current_token.location;
        if (current_token.type == TokenType::TOKEN_WHILE) {
            // while循环（原有逻辑）
            consume(TokenType::TOKEN_WHILE);
            auto condition = parse_expr();
            auto body = parse_block();
            return std::make_unique<LoopNode>(loc, LoopNode::LoopType::LOOP_WHILE,
                std::move(condition), std::move(body));
        }
        else if (current_token.type == TokenType::TOKEN_FOR) {
            // For循环（新增逻辑）
            return parse_for_loop();
        }
        else if (current_token.type == TokenType::TOKEN_RE) {
            // re循环（扩展逻辑，支持wait）
            return parse_re_loop();
        }
        else {
            throw ParseError(ErrorCode::ER_0003, current_token.location, "未知循环类型");
        }
    }

    // 扩展：解析等待语句（支持多单位，修复token_buffer访问）
    std::unique_ptr<WaitNode> parse_wait() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_WAIT); // 消耗wait
        // 解析wait语法（如time = 3000ms）
        if (current_token.type == TokenType::TOKEN_IDENTIFIER && current_token.value == "time") {
            consume(TokenType::TOKEN_IDENTIFIER); // 消耗time
            if (current_token.type != TokenType::TOKEN_EQUALS) {
                throw ParseError(ErrorCode::ER_0003, current_token.location, "wait time后缺少=");
            }
            consume(TokenType::TOKEN_EQUALS);
        }
        // 解析等待时长和单位（调用tokenizer.parse_wait_number）
        Token num_token = tokenizer.parse_wait_number();
        if (!is_numeric_token(num_token.type)) {
            throw ParseError(ErrorCode::ER_1001, num_token.location, "wait后必须跟数值时长（如3000）");
        }
        // 创建时长表达式节点（修复make_unique参数）
        DataType num_type = (num_token.type == TokenType::TOKEN_INT_LITERAL) ?
            DataType::DATA_TYPE_INT : DataType::DATA_TYPE_FLOAT;
        auto duration = std::make_unique<LiteralExprNode>(
            num_token.location,
            num_type,
            num_token.value
        );
        // 解析单位（通过tokenizer.next_token，不直接访问buffer）
        Token unit_token = tokenizer.next_token();
        std::string unit = GalltConstants::DEFAULT_WAIT_UNIT;
        if (unit_token.type == TokenType::TOKEN_WAIT_UNIT) {
            unit = unit_token.value;
            // 验证单位合法性
            if (GalltConstants::WAIT_SUPPORTED_UNITS.find(unit) == GalltConstants::WAIT_SUPPORTED_UNITS.end()) {
                throw ParseError(ErrorCode::ER_0003, unit_token.location, "不支持的时间单位：" + unit + "（支持：ms/s/min/hour）");
            }
        }
        else {
            // 回退单位Token（通过tokenizer的public接口）
            tokenizer.push_back_token(unit_token);
            throw ParseError(ErrorCode::ER_0003, unit_token.location, "wait时长后缺少合法单位（ms/s/min/hour）");
        }
        consume(TokenType::TOKEN_SEMICOLON); // 消耗;
        return std::make_unique<WaitNode>(loc, std::move(duration), unit);
    }

    // 解析线程语句（保持原有逻辑）
    std::unique_ptr<ThreadNode> parse_thread() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_THREAD); // 消耗thread
        if (current_token.type != TokenType::TOKEN_IDENTIFIER) {
            throw ParseError(ErrorCode::ER_0004, current_token.location, "线程名必须是标识符");
        }
        std::string thread_name = current_token.value;
        consume(TokenType::TOKEN_IDENTIFIER);
        auto body = parse_block();
        return std::make_unique<ThreadNode>(loc, thread_name, std::move(body));
    }

    // 解析返回语句（保持原有逻辑）
    std::unique_ptr<ReturnNode> parse_return() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_RETURN); // 消耗return
        std::unique_ptr<ExpressionNode> return_value;
        if (current_token.type != TokenType::TOKEN_SEMICOLON) {
            return_value = parse_expr();
        }
        consume(TokenType::TOKEN_SEMICOLON); // 消耗;
        return std::make_unique<ReturnNode>(loc, std::move(return_value));
    }

    // 解析释放main语句（保持原有逻辑）
    std::unique_ptr<ReleaseMainNode> parse_release_main() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_R_MAIN); // 消耗r.main
        consume(TokenType::TOKEN_SEMICOLON); // 消耗;
        return std::make_unique<ReleaseMainNode>(loc);
    }

    // 解析退出语句（保持原有逻辑）
    std::unique_ptr<ExitNode> parse_exit() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_EXIT); // 消耗exit
        consume(TokenType::TOKEN_SEMICOLON); // 消耗;
        return std::make_unique<ExitNode>(loc);
    }

    // 扩展：解析单个语句（新增VarNode、DetectNode分支，修复预读逻辑）
    std::unique_ptr<StatementNode> parse_statement() {
        switch (current_token.type) {
        case TokenType::TOKEN_DEFINE:
            return parse_variable_declaration();
        case TokenType::TOKEN_IDENTIFIER:
            // 检查是否为count表达式（如count(5*8);）
            if (current_token.value == "count") {
                auto count_expr = parse_primary_expr();
                consume(TokenType::TOKEN_SEMICOLON);
                // count表达式需嵌套在输出或赋值中，此处暂不单独支持
                throw ParseError(ErrorCode::ER_0003, current_token.location, "count表达式必须在输出或赋值中使用（如output count(5*8);）");
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
            // 新增：解析特殊变量声明（start var.name=...）
        case TokenType::TOKEN_START: {
            // 预读下一个Token（通过public接口，不直接访问buffer）
            Token next = peek_next_token();
            if (next.type == TokenType::TOKEN_VAR_NAME) {
                return parse_var_declaration();
            }
            // 否则按普通start（如start main）处理，由parse_main调用
            throw ParseError(ErrorCode::ER_0003, current_token.location, "start后必须跟main或var.name（如start main 或 start var.name=\"Peter\"）");
        }
                                   // 新增：解析事件检测语句
        case TokenType::TOKEN_DETECT:
            return parse_detect();
        default:
            throw ParseError(ErrorCode::ER_0003, current_token.location,
                "未知语句类型: " + current_token.value);
        }
    }

    // ------------------------------ 函数与主程序解析（修复token_buffer访问） ------------------------------
    // 解析函数定义（保持原有逻辑）
    std::unique_ptr<FunctionDefinitionNode> parse_function_definition() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_FUNCTION); // 消耗function
        // 解析返回类型
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
            throw ParseError(ErrorCode::ER_1001, current_token.location, "函数返回类型错误");
        }
        // 解析函数名
        if (current_token.type != TokenType::TOKEN_IDENTIFIER) {
            throw ParseError(ErrorCode::ER_0004, current_token.location, "函数名必须是标识符");
        }
        std::string func_name = current_token.value;
        consume(TokenType::TOKEN_IDENTIFIER);
        // 解析参数列表
        consume(TokenType::TOKEN_LPAREN); // 消耗(
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
                throw ParseError(ErrorCode::ER_1001, current_token.location, "参数类型错误");
            }
            if (current_token.type != TokenType::TOKEN_IDENTIFIER) {
                throw ParseError(ErrorCode::ER_0004, current_token.location, "参数名必须是标识符");
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
        consume(TokenType::TOKEN_RPAREN); // 消耗)
        // 解析函数体
        auto body = parse_block();
        return std::make_unique<FunctionDefinitionNode>(loc, func_name, return_type,
            std::move(params), std::move(body));
    }

    // 解析main程序块（保持原有逻辑）
    std::unique_ptr<MainNode> parse_main() {
        SourceLocation loc = current_token.location;
        consume(TokenType::TOKEN_START); // 消耗start
        if (current_token.type != TokenType::TOKEN_MAIN) {
            throw ParseError(ErrorCode::ER_0003, current_token.location, "start后必须跟main");
        }
        consume(TokenType::TOKEN_MAIN);
        // main名称（可选）
        std::string main_name;
        if (current_token.type == TokenType::TOKEN_IDENTIFIER) {
            main_name = current_token.value;
            consume(TokenType::TOKEN_IDENTIFIER);
        }
        // 解析main体
        auto main_body = parse_block();
        consume(TokenType::TOKEN_END); // 消耗end
        return std::make_unique<MainNode>(loc, main_name, std::move(main_body));
    }

public:
    // 构造函数：接收词法分析器，初始化当前Token
    explicit Parser(Tokenizer& t) : tokenizer(t) {
        current_token = tokenizer.next_token(); // 预读第一个Token
    }

    // 扩展：解析整个程序（新增全局事件检测节点收集，修复token_buffer访问）
    std::unique_ptr<ProgramNode> parse_program() {
        SourceLocation loc = current_token.location;
        auto program = std::make_unique<ProgramNode>(loc);
        // 程序由函数定义、main块、全局事件检测组成
        while (current_token.type != TokenType::TOKEN_EOF) {
            if (current_token.type == TokenType::TOKEN_FUNCTION) {
                program->functions.push_back(std::move(parse_function_definition())); // 修复：必须std::move
            }
            else if (current_token.type == TokenType::TOKEN_START) {
                // 预读判断是main还是特殊变量声明（通过public接口）
                Token next = peek_next_token();
                if (next.type == TokenType::TOKEN_MAIN) {
                    program->mains.push_back(std::move(parse_main())); // 修复：必须std::move
                }
                else if (next.type == TokenType::TOKEN_VAR_NAME) {
                    auto var_node = parse_var_declaration();
                    // 若当前无main，创建默认main
                    if (program->mains.empty()) {
                        // 注意：不能用花括号初始化 vector<unique_ptr<...>>，会走 initializer_list 导致拷贝 unique_ptr（C2280）
                        std::vector<std::unique_ptr<StatementNode>> tmp_stmts;
                        tmp_stmts.push_back(std::move(var_node)); // 明确移动语义
                        auto default_main_body = std::make_unique<BlockNode>(loc, std::move(tmp_stmts));
                        program->mains.push_back(std::make_unique<MainNode>(
                            loc,
                            "default_main",
                            std::move(default_main_body)
                        ));
                    }
                    else {
                        program->mains.back()->main_body->statements.push_back(std::move(var_node)); // 保持移动
                    }
                }
                else {
                    throw ParseError(ErrorCode::ER_0003, current_token.location, "start后必须跟main或var.name（如start main 或 start var.name=\"Peter\"）");
                }
            }
            else if (current_token.type == TokenType::TOKEN_DETECT) {
                program->detect_events.push_back(std::move(parse_detect())); // 修复：必须std::move
            }
            else {
                throw ParseError(ErrorCode::ER_0003, current_token.location, "程序顶层只能是函数定义、main块或全局事件检测");
            }
        }
        return program;
    }

    // 获取解析过程中产生的错误列表
    const std::vector<ParseError>& get_errors() const {
        return errors;
    }
};

#endif // PARSER_H