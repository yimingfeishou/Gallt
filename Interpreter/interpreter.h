//Copyright (c) Gallt Developer.
//版权所有（c）Gallt 开发者。
#ifndef INTERPRETER_H
#define INTERPRETER_H
#include "common.h"
#include "ast_nodes.h"
#include "semantic_analyzer.h"
#include <variant>
#include <unordered_map>
#include <vector>
#include <iostream>
#include <string>
#include <cmath>
#include <chrono>
#include <thread>
#include <stdexcept>
#include <optional>
#include <conio.h>
#include <atomic>
#include <functional>
#include <mutex>
#include <memory> // 智能指针头文件

// 全局原子变量：继承自tokenizer的Ctrl+C中断标记
extern std::atomic<bool> g_is_ctrl_c;

// ------------------------------ 运行时值类型（Value） ------------------------------
struct RuntimeValue {
    using ValueVariant = std::variant<int, float, double, std::string>;
    DataType type;       // 值的类型
    ValueVariant value;  // 实际存储的值

    // 默认构造函数
    RuntimeValue() : type(DataType::DATA_TYPE_INT), value(0) {}
    // 类型构造函数
    RuntimeValue(int val) : type(DataType::DATA_TYPE_INT), value(val) {}
    RuntimeValue(float val) : type(DataType::DATA_TYPE_FLOAT), value(val) {}
    RuntimeValue(double val) : type(DataType::DATA_TYPE_DOUBLE), value(val) {}
    RuntimeValue(const std::string& val) : type(DataType::DATA_TYPE_STRING), value(val) {}

    // 获取值的字符串表示（用于输出）
    std::string to_string() const {
        switch (type) {
        case DataType::DATA_TYPE_INT:
            return std::to_string(std::get<int>(value));
        case DataType::DATA_TYPE_FLOAT:
            return std::to_string(std::get<float>(value));
        case DataType::DATA_TYPE_DOUBLE:
            return std::to_string(std::get<double>(value));
        case DataType::DATA_TYPE_STRING:
            return std::get<std::string>(value);
        default: return "unknown";
        }
    }

    // 类型转换（安全转换，失败返回空）
    template <typename T>
    std::optional<T> as() const {
        static_assert(
            std::is_same_v<T, int> || std::is_same_v<T, float> ||
            std::is_same_v<T, double> || std::is_same_v<T, std::string>,
            "as()只支持int/float/double/string类型"
            );
        try {
            return std::get<T>(value);
        }
        catch (...) {
            return std::nullopt;
        }
    }

    // 数值运算辅助：统一转换为double进行计算（兼容int/float/double）
    std::optional<double> to_numeric() const {
        if (type == DataType::DATA_TYPE_INT) return static_cast<double>(std::get<int>(value));
        if (type == DataType::DATA_TYPE_FLOAT) return static_cast<double>(std::get<float>(value));
        if (type == DataType::DATA_TYPE_DOUBLE) return std::get<double>(value);
        return std::nullopt; // 非数值类型
    }
};

// 数组值（存储RuntimeValue的列表）
struct ArrayValue {
    std::vector<RuntimeValue> elements; // 数组元素
    DataType element_type;              // 元素类型（确保数组同构）

    ArrayValue() : element_type(DataType::DATA_TYPE_INT) {}
    ArrayValue(size_t size, DataType type) : element_type(type) {
        elements.resize(size);
        switch (type) {
        case DataType::DATA_TYPE_INT: elements.assign(size, RuntimeValue(0)); break;
        case DataType::DATA_TYPE_FLOAT: elements.assign(size, RuntimeValue(0.0f)); break;
        case DataType::DATA_TYPE_DOUBLE: elements.assign(size, RuntimeValue(0.0)); break;
        case DataType::DATA_TYPE_STRING: elements.assign(size, RuntimeValue("")); break;
        default: break;
        }
    }
};

// ------------------------------ 事件类型定义（用于DetectNode执行） ------------------------------
enum class EventType {
    EVENT_INPUT,         // 输入事件（input）
    EVENT_CLICK,         // 点击事件（click，简化为键盘回车）
    EVENT_ENTER,         // 回车事件（enter）
    EVENT_KEYBOARD_CLICK // 键盘点击事件（keyboard.click）
};

// 事件数据结构（存储事件类型和触发值）
struct EventData {
    EventType type;
    RuntimeValue trigger_val; // 触发值（如键盘字符"k"）
};

// ------------------------------ 运行时环境（线程安全+智能指针） ------------------------------
class RuntimeEnvironment {
private:
    // 作用域结构：函数用shared_ptr管理，避免野指针
    struct Scope {
        std::unordered_map<std::string, RuntimeValue> variables;       // 普通变量+特殊变量
        std::unordered_map<std::string, ArrayValue> arrays;            // 数组变量
        std::unordered_map<std::string, std::shared_ptr<FunctionDefinitionNode>> functions; // 函数（shared_ptr）
    };

    std::vector<Scope> scopes;                  // 作用域栈
    std::vector<std::pair<EventType, std::function<void(const EventData&)>>> event_listeners; // 事件监听器
    mutable std::mutex env_mutex;               // 线程安全互斥锁

public:
    RuntimeEnvironment() {
        push_scope(); // 初始全局作用域
    }

    // 作用域管理（线程安全）
    void push_scope() {
        std::lock_guard<std::mutex> lock(env_mutex);
        scopes.emplace_back();
    }

    void pop_scope() {
        std::lock_guard<std::mutex> lock(env_mutex);
        if (!scopes.empty()) scopes.pop_back();
    }

    // 变量管理（线程安全）
    std::optional<RuntimeValue*> find_variable(const std::string& name) {
        std::lock_guard<std::mutex> lock(env_mutex);
        for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
            if (it->variables.count(name)) {
                return &(it->variables[name]);
            }
        }
        return std::nullopt;
    }

    void add_variable(const std::string& name, const RuntimeValue& value) {
        std::lock_guard<std::mutex> lock(env_mutex);
        if (scopes.empty()) push_scope();
        scopes.back().variables[name] = value;
    }

    // 数组管理（线程安全）
    std::optional<ArrayValue*> find_array(const std::string& name) {
        std::lock_guard<std::mutex> lock(env_mutex);
        for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
            if (it->arrays.count(name)) {
                return &(it->arrays[name]);
            }
        }
        return std::nullopt;
    }

    void add_array(const std::string& name, const ArrayValue& array) {
        std::lock_guard<std::mutex> lock(env_mutex);
        if (scopes.empty()) push_scope();
        scopes.back().arrays[name] = array;
    }

    // 函数管理（shared_ptr+线程安全）
    std::optional<std::shared_ptr<FunctionDefinitionNode>> find_function(const std::string& name) {
        std::lock_guard<std::mutex> lock(env_mutex);
        if (scopes.empty()) return std::nullopt;

        const auto& global_scope = scopes.front();
        auto found = global_scope.functions.find(name);
        if (found != global_scope.functions.end()) {
            return found->second;
        }
        return std::nullopt;
    }

    void add_function(const std::string& name, std::shared_ptr<FunctionDefinitionNode> func) {
        std::lock_guard<std::mutex> lock(env_mutex);
        if (scopes.empty()) push_scope();
        scopes.front().functions[name] = std::move(func); // 移动语义，避免拷贝
    }

    // 事件监听管理（线程安全）
    void add_event_listener(EventType type, std::function<void(const EventData&)> callback) {
        std::lock_guard<std::mutex> lock(env_mutex);
        event_listeners.emplace_back(type, std::move(callback));
    }

    void trigger_event(const EventData& event) {
        std::lock_guard<std::mutex> lock(env_mutex);
        for (const auto& listener : event_listeners) {
            if (listener.first == event.type) {
                listener.second(event);
            }
        }
    }

    void clear_event_listeners() {
        std::lock_guard<std::mutex> lock(env_mutex);
        event_listeners.clear();
    }
};

// ------------------------------ 解释器错误（保持原有） ------------------------------
class InterpreterError : public std::runtime_error {
public:
    ErrorCode code;
    SourceLocation location;

    InterpreterError(ErrorCode c, const SourceLocation& loc, const std::string& msg)
        : std::runtime_error(msg), code(c), location(loc) {}
};

// ------------------------------ 解释器类（修复unique_ptr拷贝问题） ------------------------------
class Interpreter {
private:
    RuntimeEnvironment env;                      // 运行时环境
    std::vector<InterpreterError> errors;        // 运行时错误
    std::atomic<bool> is_running = true;         // 程序运行标记（原子变量）
    std::thread event_thread;                    // 事件监听线程
    std::atomic<bool> event_thread_running = false; // 事件线程标记

    // 添加运行时错误
    void add_error(ErrorCode code, const SourceLocation& loc, const std::string& msg) {
        errors.emplace_back(code, loc, msg);
        std::cerr << "运行时错误: " << msg << "（" << loc.to_string() << "）\n";
    }

    // ------------------------------ 核心：表达式计算（适配智能指针） ------------------------------
    RuntimeValue evaluate_expression(ExpressionNode* expr) {
        if (!expr) {
            throw InterpreterError(ErrorCode::ER_0003, SourceLocation(), "空表达式");
        }

        // 1. 字面量表达式
        if (auto lit = dynamic_cast<LiteralExprNode*>(expr)) {
            switch (lit->value_type) {
            case DataType::DATA_TYPE_INT: return RuntimeValue(std::stoi(lit->value));
            case DataType::DATA_TYPE_FLOAT: return RuntimeValue(std::stof(lit->value));
            case DataType::DATA_TYPE_DOUBLE: return RuntimeValue(std::stod(lit->value));
            case DataType::DATA_TYPE_STRING: return RuntimeValue(lit->value);
            default: throw InterpreterError(ErrorCode::ER_1001, lit->location, "未知字面量类型");
            }
        }

        // 2. 标识符表达式（普通变量+特殊变量）
        else if (auto id = dynamic_cast<IdentifierExprNode*>(expr)) {
            if (auto var = env.find_variable(id->name)) {
                return **var;
            }
            // 拆分特殊变量（组名.varId）
            size_t dot_pos = id->name.find('.');
            if (dot_pos != std::string::npos) {
                std::string full_name = id->name;
                if (auto var = env.find_variable(full_name)) {
                    return **var;
                }
            }
            throw InterpreterError(ErrorCode::ER_0003, id->location, "未定义的变量: " + id->name);
        }

        // 3. 二元运算表达式
        else if (auto bin_op = dynamic_cast<BinaryOpExprNode*>(expr)) {
            RuntimeValue left = evaluate_expression(bin_op->left.get()); // 用get()获取原始指针，不拷贝
            RuntimeValue right = evaluate_expression(bin_op->right.get());
            switch (bin_op->op) {
            case TokenType::TOKEN_PLUS: return evaluate_addition(left, right, bin_op->location);
            case TokenType::TOKEN_MINUS: return evaluate_subtraction(left, right, bin_op->location);
            case TokenType::TOKEN_MUL: return evaluate_multiplication(left, right, bin_op->location);
            case TokenType::TOKEN_DIV: return evaluate_division(left, right, bin_op->location);
            case TokenType::TOKEN_EQUALS: return evaluate_equality(left, right, bin_op->location);
            case TokenType::TOKEN_LT: return evaluate_less_than(left, right, bin_op->location);
            case TokenType::TOKEN_GT: return evaluate_greater_than(left, right, bin_op->location);
            default: throw InterpreterError(ErrorCode::ER_0003, bin_op->location, "未知运算符");
            }
        }

        // 4. 数组访问表达式
        else if (auto arr_access = dynamic_cast<ArrayAccessExprNode*>(expr)) {
            std::string arr_name = arr_access->array_name->name;
            auto arr_opt = env.find_array(arr_name);
            if (!arr_opt) {
                throw InterpreterError(ErrorCode::ER_0003, arr_access->location, "未定义的数组: " + arr_name);
            }
            ArrayValue* arr = arr_opt.value();

            // 计算索引（必须是int）
            RuntimeValue index_val = evaluate_expression(arr_access->index.get());
            auto index_opt = index_val.as<int>();
            if (!index_opt) {
                throw InterpreterError(ErrorCode::ER_1001, arr_access->index->location, "数组索引必须是int");
            }
            int index = index_opt.value();

            // 检查越界
            if (index < 0 || static_cast<size_t>(index) >= arr->elements.size()) {
                throw InterpreterError(
                    ErrorCode::ER_2001,
                    arr_access->index->location,
                    "数组索引越界: " + std::to_string(index) + "（大小: " + std::to_string(arr->elements.size()) + "）"
                );
            }
            return arr->elements[index];
        }

        // 5. 函数调用表达式
        else if (auto func_call = dynamic_cast<FunctionCallExprNode*>(expr)) {
            return call_function(func_call->function_name, func_call->arguments, func_call->location);
        }

        // 6. π表达式
        else if (auto pi_expr = dynamic_cast<PiExprNode*>(expr)) {
            int precision = GalltConstants::DEFAULT_PI_DIGITS;
            if (pi_expr->precision) {
                RuntimeValue prec_val = evaluate_expression(pi_expr->precision.get());
                if (auto p_opt = prec_val.as<int>()) precision = p_opt.value();
            }
            double pi = std::acos(-1.0);
            std::string pi_str = std::to_string(pi);
            if (precision > 0 && static_cast<size_t>(precision + 1) < pi_str.size()) {
                pi_str = pi_str.substr(0, precision + 1);
            }
            return RuntimeValue(std::stod(pi_str));
        }

        // 7. Count表达式
        else if (auto count_expr = dynamic_cast<CountExprNode*>(expr)) {
            return evaluate_count_expression(count_expr);
        }

        // 8. 事件条件表达式
        else if (auto event_cond = dynamic_cast<EventConditionExprNode*>(expr)) {
            return evaluate_expression(event_cond->trigger_val.get());
        }

        throw InterpreterError(ErrorCode::ER_0003, expr->location, "不支持的表达式类型");
    }

    // Count表达式计算
    RuntimeValue evaluate_count_expression(CountExprNode* count_expr) {
        RuntimeValue calc_val = evaluate_expression(count_expr->calc_expr.get());
        auto numeric_val_opt = calc_val.to_numeric();
        if (!numeric_val_opt) {
            throw InterpreterError(
                ErrorCode::ER_1001,
                count_expr->calc_expr->location,
                "Count表达式计算内容必须是数值类型"
            );
        }
        double numeric_val = numeric_val_opt.value();

        switch (count_expr->count_type) {
        case CountExprNode::CountType::COUNT_NORMAL:
            return RuntimeValue(static_cast<int>(std::round(numeric_val)));
        case CountExprNode::CountType::COUNT_FLOAT:
            return RuntimeValue(static_cast<float>(numeric_val));
        case CountExprNode::CountType::COUNT_DOUBLE:
            return RuntimeValue(numeric_val);
        default:
            throw InterpreterError(ErrorCode::ER_0003, count_expr->location, "未知的Count类型");
        }
    }

    // ------------------------------ 二元运算实现 ------------------------------
    RuntimeValue evaluate_addition(const RuntimeValue& left, const RuntimeValue& right, const SourceLocation& loc) {
        if (left.type == DataType::DATA_TYPE_STRING || right.type == DataType::DATA_TYPE_STRING) {
            return RuntimeValue(left.to_string() + right.to_string());
        }
        auto l_opt = left.to_numeric(), r_opt = right.to_numeric();
        if (!l_opt || !r_opt) throw InterpreterError(ErrorCode::ER_1001, loc, "加法不支持非数值类型");
        return RuntimeValue(l_opt.value() + r_opt.value());
    }

    RuntimeValue evaluate_subtraction(const RuntimeValue& left, const RuntimeValue& right, const SourceLocation& loc) {
        auto l_opt = left.to_numeric(), r_opt = right.to_numeric();
        if (!l_opt || !r_opt) throw InterpreterError(ErrorCode::ER_1001, loc, "减法不支持非数值类型");
        return RuntimeValue(l_opt.value() - r_opt.value());
    }

    RuntimeValue evaluate_multiplication(const RuntimeValue& left, const RuntimeValue& right, const SourceLocation& loc) {
        auto l_opt = left.to_numeric(), r_opt = right.to_numeric();
        if (!l_opt || !r_opt) throw InterpreterError(ErrorCode::ER_1001, loc, "乘法不支持非数值类型");
        return RuntimeValue(l_opt.value() * r_opt.value());
    }

    RuntimeValue evaluate_division(const RuntimeValue& left, const RuntimeValue& right, const SourceLocation& loc) {
        auto l_opt = left.to_numeric(), r_opt = right.to_numeric();
        if (!l_opt || !r_opt) throw InterpreterError(ErrorCode::ER_1001, loc, "除法不支持非数值类型");
        if (r_opt.value() == 0) throw InterpreterError(ErrorCode::ER_0003, loc, "除数不能为0");
        return RuntimeValue(l_opt.value() / r_opt.value());
    }

    RuntimeValue evaluate_equality(const RuntimeValue& left, const RuntimeValue& right, const SourceLocation& loc) {
        if (left.type != right.type) return RuntimeValue(0);
        bool equal = false;
        if (left.type == DataType::DATA_TYPE_INT) {
            auto l_opt = left.as<int>(), r_opt = right.as<int>();
            if (l_opt && r_opt) equal = (l_opt.value() == r_opt.value());
        }
        else if (left.type == DataType::DATA_TYPE_FLOAT) {
            auto l_opt = left.as<float>(), r_opt = right.as<float>();
            if (l_opt && r_opt) equal = (l_opt.value() == r_opt.value());
        }
        else if (left.type == DataType::DATA_TYPE_DOUBLE) {
            auto l_opt = left.as<double>(), r_opt = right.as<double>();
            if (l_opt && r_opt) equal = (l_opt.value() == r_opt.value());
        }
        else if (left.type == DataType::DATA_TYPE_STRING) {
            auto l_opt = left.as<std::string>(), r_opt = right.as<std::string>();
            if (l_opt && r_opt) equal = (l_opt.value() == r_opt.value());
        }
        return RuntimeValue(equal ? 1 : 0);
    }

    RuntimeValue evaluate_less_than(const RuntimeValue& left, const RuntimeValue& right, const SourceLocation& loc) {
        auto l_opt = left.to_numeric(), r_opt = right.to_numeric();
        if (!l_opt || !r_opt) throw InterpreterError(ErrorCode::ER_1001, loc, "小于判断不支持非数值类型");
        return RuntimeValue(l_opt.value() < r_opt.value() ? 1 : 0);
    }

    RuntimeValue evaluate_greater_than(const RuntimeValue& left, const RuntimeValue& right, const SourceLocation& loc) {
        auto l_opt = left.to_numeric(), r_opt = right.to_numeric();
        if (!l_opt || !r_opt) throw InterpreterError(ErrorCode::ER_1001, loc, "大于判断不支持非数值类型");
        return RuntimeValue(l_opt.value() > r_opt.value() ? 1 : 0);
    }

    // ------------------------------ 函数调用（修复unique_ptr参数传递） ------------------------------
    RuntimeValue call_function(const std::string& name, const std::vector<std::unique_ptr<ExpressionNode>>& args, const SourceLocation& loc) {
        // 查找函数（shared_ptr）
        auto func_opt = env.find_function(name);
        if (!func_opt) throw InterpreterError(ErrorCode::ER_0003, loc, "未定义的函数: " + name);
        auto func = func_opt.value();

        // 检查参数数量
        if (args.size() != func->parameters.size()) {
            throw InterpreterError(
                ErrorCode::ER_1001,
                loc,
                "函数" + name + "参数数量不匹配（预期: " + std::to_string(func->parameters.size()) +
                ", 实际: " + std::to_string(args.size()) + "）"
            );
        }

        // 计算参数值（用引用遍历args，避免拷贝unique_ptr）
        std::vector<RuntimeValue> arg_values;
        for (const auto& arg : args) { // 关键：const& 引用遍历，不拷贝
            RuntimeValue arg_val = evaluate_expression(arg.get());
            DataType param_type = func->parameters[arg_values.size()].var_type;
            if (!is_type_compatible(arg_val.type, param_type)) {
                throw InterpreterError(
                    ErrorCode::ER_1001,
                    arg->location,
                    "函数" + name + "参数" + std::to_string(arg_values.size() + 1) + "类型不匹配（预期: " +
                    CommonUtils::data_type_to_string(param_type) + ", 实际: " +
                    CommonUtils::data_type_to_string(arg_val.type) + "）"
                );
            }
            arg_values.push_back(arg_val);
        }

        // 执行函数（作用域切换+参数绑定）
        env.push_scope();
        for (size_t i = 0; i < func->parameters.size(); ++i) {
            const auto& param = func->parameters[i];
            env.add_variable(param.var_name, arg_values[i]);
        }

        RuntimeValue return_val(0);
        try {
            execute_block(func->function_body.get()); // 用get()获取原始指针，不拷贝
        }
        catch (const ReturnValue& ret) {
            return_val = ret.value;
        }

        env.pop_scope();
        return return_val;
    }

    // 类型兼容性检查
    bool is_type_compatible(DataType actual, DataType expected) {
        if (actual == expected) return true;
        // 数值类型兼容（int→float→double）
        if ((actual == DataType::DATA_TYPE_INT && (expected == DataType::DATA_TYPE_FLOAT || expected == DataType::DATA_TYPE_DOUBLE)) ||
            (actual == DataType::DATA_TYPE_FLOAT && expected == DataType::DATA_TYPE_DOUBLE)) {
            return true;
        }
        return false;
    }

    // ------------------------------ 语句执行（核心修复：unique_ptr遍历/传递） ------------------------------
    // 1. 执行代码块（修复：引用遍历statements，避免拷贝unique_ptr）
    void execute_block(BlockNode* block) {
        if (!block || !is_running || g_is_ctrl_c) return;
        // 关键修复：for (auto& stmt : ...) 引用遍历，不拷贝unique_ptr<StatementNode>
        for (auto& stmt : block->statements) {
            if (!is_running || g_is_ctrl_c) break;
            execute_statement(stmt.get()); // 用get()获取原始指针，不拷贝
        }
    }

    // 2. 执行单个语句（参数用原始指针，不传递unique_ptr）
    void execute_statement(StatementNode* stmt) {
        if (!stmt || !is_running || g_is_ctrl_c) return;

        // 变量声明语句
        if (auto var_decl = dynamic_cast<VariableDeclarationNode*>(stmt)) {
            execute_variable_declaration(var_decl);
        }
        // 赋值语句
        else if (auto assign = dynamic_cast<AssignmentNode*>(stmt)) {
            execute_assignment(assign);
        }
        // 输出语句
        else if (auto output = dynamic_cast<OutputNode*>(stmt)) {
            execute_output(output);
        }
        // 输入语句
        else if (auto input = dynamic_cast<InputNode*>(stmt)) {
            execute_input(input);
        }
        // 条件语句
        else if (auto if_node = dynamic_cast<IfNode*>(stmt)) {
            execute_if(if_node);
        }
        // 循环语句
        else if (auto loop = dynamic_cast<LoopNode*>(stmt)) {
            execute_loop(loop);
        }
        // 等待语句
        else if (auto wait = dynamic_cast<WaitNode*>(stmt)) {
            execute_wait(wait);
        }
        // 线程语句（修复lambda捕获，不拷贝unique_ptr）
        else if (auto thread = dynamic_cast<ThreadNode*>(stmt)) {
            execute_thread(thread);
        }
        // 返回语句
        else if (auto ret = dynamic_cast<ReturnNode*>(stmt)) {
            execute_return(ret);
        }
        // 释放main语句
        else if (auto release_main = dynamic_cast<ReleaseMainNode*>(stmt)) {
            // 无运行时操作
        }
        // 退出语句
        else if (auto exit_node = dynamic_cast<ExitNode*>(stmt)) {
            is_running = false;
            stop_event_thread();
        }
        // 代码块
        else if (auto block_stmt = dynamic_cast<BlockNode*>(stmt)) {
            execute_block(block_stmt);
        }
        // 特殊变量声明语句
        else if (auto var_node = dynamic_cast<VarNode*>(stmt)) {
            execute_var_node(var_node);
        }
        // 事件检测语句（修复lambda捕获，不拷贝unique_ptr）
        else if (auto detect = dynamic_cast<DetectNode*>(stmt)) {
            execute_detect_node(detect);
        }
        else {
            add_error(ErrorCode::ER_0003, stmt->location, "不支持的语句类型");
        }
    }

    // 3. 执行特殊变量声明（VarNode）
    void execute_var_node(VarNode* node) {
        for (const auto& item : node->var_items) {
            std::string full_var_name = node->var_group_name + "." + item.var_id;
            RuntimeValue init_val = evaluate_expression(item.var_value.get()); // get()获取指针

            if (!is_type_compatible(init_val.type, item.var_type)) {
                add_error(
                    ErrorCode::ER_1001,
                    node->location,
                    "特殊变量" + full_var_name + "类型不匹配（预期: " +
                    CommonUtils::data_type_to_string(item.var_type) + ", 实际: " +
                    CommonUtils::data_type_to_string(init_val.type) + "）"
                );
                continue;
            }

            env.add_variable(full_var_name, init_val);
            std::cout << "[特殊变量注册] " << full_var_name << " = " << init_val.to_string() << "\n";
        }
    }

    // 4. 执行事件检测（DetectNode，修复lambda捕获）
    void execute_detect_node(DetectNode* node) {
        // 解析事件类型和触发值
        std::string event_type_str = node->event_condition->event_type;
        RuntimeValue trigger_val = evaluate_expression(node->event_condition->trigger_val.get());
        EventType event_type;

        if (event_type_str == "input") event_type = EventType::EVENT_INPUT;
        else if (event_type_str == "click") event_type = EventType::EVENT_CLICK;
        else if (event_type_str == "enter") event_type = EventType::EVENT_ENTER;
        else if (event_type_str == "keyboard.click") event_type = EventType::EVENT_KEYBOARD_CLICK;
        else {
            add_error(ErrorCode::ER_0003, node->location, "不支持的事件类型: " + event_type_str);
            return;
        }

        // 注册事件监听器（修复：lambda捕获node指针，不拷贝对象）
        env.add_event_listener(event_type,
            [this, node_ptr = node, trigger_val, event_type_str](const EventData& event) { // 捕获指针，不拷贝
                if (event.trigger_val.type != trigger_val.type) return;
                bool match = false;

                if (event.trigger_val.type == DataType::DATA_TYPE_STRING) {
                    auto event_val_opt = event.trigger_val.as<std::string>();
                    auto trigger_val_opt = trigger_val.as<std::string>();
                    if (event_val_opt && trigger_val_opt) {
                        match = (event_val_opt.value() == trigger_val_opt.value());
                    }
                }
                else if (event.trigger_val.type == DataType::DATA_TYPE_INT) {
                    auto event_val_opt = event.trigger_val.as<int>();
                    auto trigger_val_opt = trigger_val.as<int>();
                    if (event_val_opt && trigger_val_opt) {
                        match = (event_val_opt.value() == trigger_val_opt.value());
                    }
                }

                if (match && is_running && !g_is_ctrl_c) {
                    std::cout << "\n[事件触发] " << event_type_str << "（触发值: " << trigger_val.to_string() << "）\n";
                    execute_block(node_ptr->run_block.get()); // get()获取指针，不拷贝
                }
            });

        std::cout << "[事件监听注册] " << event_type_str << "（触发值: " << trigger_val.to_string() << "）\n";

        // 启动事件线程（仅一次）
        if (!event_thread_running) {
            event_thread_running = true;
            event_thread = std::thread(&Interpreter::event_listener_thread, this);
        }
    }

    // 事件监听线程
    void event_listener_thread() {
        while (event_thread_running && !g_is_ctrl_c) {
            if (_kbhit()) {
                char key = _getch();
                EventData event;

                if (key == 13) { // Enter键
                    event.type = EventType::EVENT_ENTER;
                    event.trigger_val = RuntimeValue(std::string("\n"));
                    env.trigger_event(event);
                }
                else if (key == 32) { // 空格键（模拟Click）
                    event.type = EventType::EVENT_CLICK;
                    event.trigger_val = RuntimeValue(1);
                    env.trigger_event(event);
                }
                else { // 普通字符（Keyboard.Click + Input）
                    event.type = EventType::EVENT_KEYBOARD_CLICK;
                    event.trigger_val = RuntimeValue(std::string(1, key));
                    env.trigger_event(event);

                    EventData input_event;
                    input_event.type = EventType::EVENT_INPUT;
                    input_event.trigger_val = RuntimeValue(std::string(1, key));
                    env.trigger_event(input_event);
                }
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
    }

    // 停止事件线程
    void stop_event_thread() {
        if (event_thread_running) {
            event_thread_running = false;
            if (event_thread.joinable()) {
                event_thread.join();
            }
        }
        env.clear_event_listeners();
    }

    // 执行变量声明
    void execute_variable_declaration(VariableDeclarationNode* node) {
        if (node->is_array) {
            size_t size = 0;
            if (node->array_size) {
                RuntimeValue size_val = evaluate_expression(node->array_size.get());
                auto s_opt = size_val.as<int>();
                if (s_opt && s_opt.value() > 0) {
                    size = static_cast<size_t>(s_opt.value());
                }
                else {
                    add_error(ErrorCode::ER_2001, node->array_size->location, "无效的数组大小");
                    size = 1;
                }
            }
            ArrayValue arr(size, node->var_type);
            env.add_array(node->var_name, arr);
            std::cout << "[数组创建] " << node->var_name << "（大小: " << size << "）\n";
        }
        else {
            RuntimeValue init_val(0);
            if (node->initial_value) {
                init_val = evaluate_expression(node->initial_value.get());
            }
            env.add_variable(node->var_name, init_val);
            std::cout << "[变量创建] " << node->var_name << " = " << init_val.to_string() << "\n";
        }
    }

    // 执行赋值语句
    void execute_assignment(AssignmentNode* node) {
        RuntimeValue value = evaluate_expression(node->value.get());

        // 赋值目标：普通变量/特殊变量
        if (auto id = dynamic_cast<IdentifierExprNode*>(node->target.get())) {
            auto var_opt = env.find_variable(id->name);
            if (var_opt) {
                RuntimeValue* var = var_opt.value();
                *var = value;
                std::cout << "[赋值] " << id->name << " = " << value.to_string() << "\n";
                return;
            }
            add_error(ErrorCode::ER_0003, id->location, "赋值目标未定义: " + id->name);
            return;
        }

        // 赋值目标：数组元素
        else if (auto arr_access = dynamic_cast<ArrayAccessExprNode*>(node->target.get())) {
            std::string arr_name = arr_access->array_name->name;
            auto arr_opt = env.find_array(arr_name);
            if (!arr_opt) {
                add_error(ErrorCode::ER_0003, arr_access->location, "数组未定义: " + arr_name);
                return;
            }
            ArrayValue* arr = arr_opt.value();

            RuntimeValue index_val = evaluate_expression(arr_access->index.get());
            auto index_opt = index_val.as<int>();
            if (!index_opt) {
                add_error(ErrorCode::ER_1001, arr_access->index->location, "数组索引必须是int");
                return;
            }
            int index = index_opt.value();

            if (index < 0 || static_cast<size_t>(index) >= arr->elements.size()) {
                add_error(ErrorCode::ER_2001, arr_access->index->location, "数组索引越界: " + std::to_string(index));
                return;
            }

            if (!is_type_compatible(value.type, arr->element_type)) {
                add_error(
                    ErrorCode::ER_1001,
                    node->value->location,
                    "数组元素类型不匹配（预期: " + CommonUtils::data_type_to_string(arr->element_type) +
                    ", 实际: " + CommonUtils::data_type_to_string(value.type) + "）"
                );
                return;
            }

            arr->elements[index] = value;
            std::cout << "[数组赋值] " << arr_name << "[" << index << "] = " << value.to_string() << "\n";
            return;
        }

        add_error(ErrorCode::ER_0003, node->target->location, "无效的赋值目标");
    }

    // 执行输出语句
    void execute_output(OutputNode* node) {
        RuntimeValue val = evaluate_expression(node->expr.get());
        std::cout << "[输出] " << val.to_string() << "\n";
    }

    // 执行输入语句
    void execute_input(InputNode* node) {
        if (node->prompt) {
            RuntimeValue prompt_val = evaluate_expression(node->prompt.get());
            std::cout << prompt_val.to_string();
        }

        std::string input_str;
        std::getline(std::cin, input_str);

        const std::string& var_name = node->target->name;
        auto var_opt = env.find_variable(var_name);
        if (var_opt) {
            RuntimeValue* var = var_opt.value();
            DataType target_type = var->type;

            try {
                switch (target_type) {
                case DataType::DATA_TYPE_INT:
                    var->value = std::stoi(input_str);
                    break;
                case DataType::DATA_TYPE_FLOAT:
                    var->value = std::stof(input_str);
                    break;
                case DataType::DATA_TYPE_DOUBLE:
                    var->value = std::stod(input_str);
                    break;
                case DataType::DATA_TYPE_STRING:
                    var->value = input_str;
                    break;
                default:
                    add_error(ErrorCode::ER_1001, node->location, "不支持的输入类型");
                    return;
                }
                std::cout << "[输入] " << var_name << " = " << input_str << "\n";
            }
            catch (...) {
                add_error(ErrorCode::ER_1001, node->location, "输入格式与目标类型不匹配");
            }
            return;
        }
        add_error(ErrorCode::ER_0003, node->target->location, "输入目标未定义: " + var_name);
    }

    // 执行条件语句
    void execute_if(IfNode* node) {
        RuntimeValue cond_val = evaluate_expression(node->condition.get());
        bool condition = false;

        if (cond_val.type == DataType::DATA_TYPE_INT) {
            auto val_opt = cond_val.as<int>();
            if (val_opt) condition = (val_opt.value() != 0);
        }
        else if (cond_val.type == DataType::DATA_TYPE_FLOAT) {
            auto val_opt = cond_val.as<float>();
            if (val_opt) condition = (val_opt.value() != 0);
        }
        else if (cond_val.type == DataType::DATA_TYPE_DOUBLE) {
            auto val_opt = cond_val.as<double>();
            if (val_opt) condition = (val_opt.value() != 0);
        }

        if (condition) {
            std::cout << "[条件满足] 执行if块\n";
            execute_block(node->then_block.get()); // get()获取指针，不拷贝
        }
        else if (node->else_block) {
            std::cout << "[条件不满足] 执行otherwise块\n";
            execute_block(node->else_block.get());
        }
    }

    // 执行循环语句
    void execute_loop(LoopNode* node) {
        switch (node->loop_type) {
        case LoopNode::LoopType::LOOP_WHILE:
            execute_while_loop(node);
            break;
        case LoopNode::LoopType::LOOP_FOR:
            execute_for_loop(node);
            break;
        case LoopNode::LoopType::LOOP_RE:
            execute_re_loop(node);
            break;
        }
    }

    // 执行While循环
    void execute_while_loop(LoopNode* node) {
        std::cout << "[While循环] 开始执行\n";
        int iter_count = 0;
        while (is_running && !g_is_ctrl_c) {
            RuntimeValue cond_val = evaluate_expression(node->condition.get());
            bool condition = false;

            if (cond_val.type == DataType::DATA_TYPE_INT) {
                auto val_opt = cond_val.as<int>();
                if (val_opt) condition = (val_opt.value() != 0);
            }
            else if (cond_val.type == DataType::DATA_TYPE_FLOAT) {
                auto val_opt = cond_val.as<float>();
                if (val_opt) condition = (val_opt.value() != 0);
            }
            else if (cond_val.type == DataType::DATA_TYPE_DOUBLE) {
                auto val_opt = cond_val.as<double>();
                if (val_opt) condition = (val_opt.value() != 0);
            }

            if (!condition) break;
            std::cout << "[While循环] 迭代" << ++iter_count << "\n";
            execute_block(node->body.get()); // get()获取指针，不拷贝
        }
        std::cout << "[While循环] 执行结束（共" << iter_count << "次迭代）\n";
    }

    // 执行For循环
    void execute_for_loop(LoopNode* node) {
        if (node->for_loop_var.empty() || !node->for_start || !node->for_end || !node->for_step) {
            add_error(ErrorCode::ER_0003, node->location, "For循环参数不完整");
            return;
        }

        RuntimeValue start_val = evaluate_expression(node->for_start.get());
        RuntimeValue end_val = evaluate_expression(node->for_end.get());
        RuntimeValue step_val = evaluate_expression(node->for_step.get());

        auto start_opt = start_val.as<int>();
        auto end_opt = end_val.as<int>();
        auto step_opt = step_val.as<int>();

        if (!start_opt || !end_opt || !step_opt || step_opt.value() <= 0) {
            add_error(ErrorCode::ER_1001, node->location, "For循环参数必须是正整数步长");
            return;
        }

        int start = start_opt.value();
        int end = end_opt.value();
        int step = step_opt.value();

        env.push_scope();
        RuntimeValue loop_var_val(start);
        env.add_variable(node->for_loop_var, loop_var_val);
        std::cout << "[For循环] 开始执行（" << node->for_loop_var << "从" << start << "到" << end << "，步长" << step << "）\n";

        int iter_count = 0;
        while (is_running && !g_is_ctrl_c) {
            auto current_var_opt = env.find_variable(node->for_loop_var);
            if (!current_var_opt) break;

            RuntimeValue* current_var = current_var_opt.value();
            auto current_opt = current_var->as<int>();
            if (!current_opt) break;

            int current = current_opt.value();
            if (current > end) break;

            std::cout << "[For循环] 迭代" << ++iter_count << "（" << node->for_loop_var << "=" << current << "）\n";
            execute_block(node->body.get()); // get()获取指针，不拷贝

            current_var->value = current + step;
        }

        env.pop_scope();
        std::cout << "[For循环] 执行结束（共" << iter_count << "次迭代）\n";
    }

    // 执行re循环
    void execute_re_loop(LoopNode* node) {
        if (node->is_infinite) {
            std::cout << "[re循环] 无限执行（按Ctrl+C停止）\n";
            int iter_count = 0;
            while (is_running && !g_is_ctrl_c) {
                std::cout << "[re循环] 迭代" << ++iter_count << "\n";
                execute_block(node->body.get()); // get()获取指针，不拷贝

                if (node->re_wait_duration) {
                    execute_re_wait(node);
                }
            }
            std::cout << "[re循环] 被中断（共" << iter_count << "次迭代）\n";
        }
        else {
            RuntimeValue count_val = evaluate_expression(node->condition.get());
            auto count_opt = count_val.as<int>();

            if (!count_opt || count_opt.value() <= 0 || count_opt.value() > GalltConstants::MAX_RE_NORMAL_COUNT) {
                add_error(ErrorCode::ER_0005, node->condition->location, "re循环次数无效（1~65536）");
                return;
            }

            int count = count_opt.value();
            std::cout << "[re循环] 开始执行（共" << count << "次迭代）\n";
            for (int i = 0; i < count && is_running && !g_is_ctrl_c; ++i) {
                std::cout << "[re循环] 迭代" << (i + 1) << "\n";
                execute_block(node->body.get()); // get()获取指针，不拷贝

                if (node->re_wait_duration) {
                    execute_re_wait(node);
                }
            }
            std::cout << "[re循环] 执行结束\n";
        }
    }

    // 执行re循环的wait
    void execute_re_wait(LoopNode* node) {
        if (!node->re_wait_duration) return;

        RuntimeValue dur_val = evaluate_expression(node->re_wait_duration.get());
        auto dur_opt = dur_val.as<int>();
        if (!dur_opt || dur_opt.value() <= 0) {
            add_error(ErrorCode::ER_0003, node->re_wait_duration->location, "re等待时长必须为正整数");
            return;
        }
        int dur = dur_opt.value();

        auto ms_opt = CommonUtils::convert_time_to_ms(dur, node->re_wait_unit);
        if (!ms_opt) {
            add_error(ErrorCode::ER_0003, node->location, "不支持的re等待单位: " + node->re_wait_unit);
            return;
        }
        int ms = ms_opt.value();

        std::cout << "[re等待] " << dur << node->re_wait_unit << "（" << ms << "ms）\n";
        std::this_thread::sleep_for(std::chrono::milliseconds(ms));
    }

    // 执行Wait语句
    void execute_wait(WaitNode* node) {
        if (!node->duration) {
            add_error(ErrorCode::ER_0003, node->location, "Wait语句缺少时长");
            return;
        }

        RuntimeValue dur_val = evaluate_expression(node->duration.get());
        auto dur_opt = dur_val.as<int>();
        if (!dur_opt || dur_opt.value() <= 0) {
            add_error(ErrorCode::ER_0003, node->duration->location, "Wait时长必须为正整数");
            return;
        }
        int dur = dur_opt.value();

        auto ms_opt = CommonUtils::convert_time_to_ms(dur, node->unit);
        if (!ms_opt) {
            add_error(ErrorCode::ER_0003, node->location, "不支持的Wait单位: " + node->unit);
            return;
        }
        int ms = ms_opt.value();

        std::cout << "[Wait] " << dur << node->unit << "（" << ms << "ms）\n";
        std::this_thread::sleep_for(std::chrono::milliseconds(ms));
    }

    // 执行线程语句（修复lambda捕获，不拷贝unique_ptr）
    void execute_thread(ThreadNode* node) {
        std::cout << "[线程] " << node->thread_name << " 开始创建\n";
        // 修复：捕获node指针+线程体指针，不拷贝unique_ptr
        std::thread t([this, thread_name = node->thread_name, body_ptr = node->thread_body.get()]() {
            std::cout << "[线程] " << thread_name << " 开始执行\n";
            execute_block(body_ptr); // 用提前获取的原始指针，不拷贝
            std::cout << "[线程] " << thread_name << " 执行结束\n";
            });
        t.detach();
    }

    // 函数返回异常
    struct ReturnValue {
        RuntimeValue value;
        ReturnValue(RuntimeValue v) : value(std::move(v)) {}
    };

    // 执行返回语句
    void execute_return(ReturnNode* node) {
        RuntimeValue ret_val(0);
        if (node->return_value) {
            ret_val = evaluate_expression(node->return_value.get());
        }
        throw ReturnValue(ret_val);
    }

public:
    Interpreter() = default;

    // 析构函数：清理事件线程
    ~Interpreter() {
        stop_event_thread();
    }

    // 执行程序（入口：接收原始指针，不传递unique_ptr）
    void execute(ProgramNode* program) {
        if (!program) return;
        try {
            // 注册全局函数（unique_ptr→shared_ptr，移动语义）
            for (auto& func_unique : program->functions) {
                if (!func_unique) {
                    add_error(ErrorCode::ER_0003, SourceLocation(), "无效的函数指针（nullptr）");
                    continue;
                }
                // 移动unique_ptr所有权到shared_ptr，避免拷贝
                auto func_shared = std::shared_ptr<FunctionDefinitionNode>(std::move(func_unique));
                env.add_function(func_shared->function_name, std::move(func_shared));
                std::cout << "[函数注册] " << func_shared->function_name << "\n";
            }

            // 执行main程序（引用遍历，不拷贝unique_ptr）
            for (auto& main : program->mains) { // 关键：auto& 引用遍历
                if (!is_running || g_is_ctrl_c) break;
                std::cout << "\n[开始执行main" << (main->main_name.empty() ? "" : (" " + main->main_name)) << "]\n";
                execute_block(main->main_body.get()); // get()获取指针，不拷贝
                std::cout << "[main" << (main->main_name.empty() ? "" : (" " + main->main_name)) << "执行结束]\n";
            }

            // 等待事件线程
            if (event_thread_running) {
                std::cout << "\n[事件监听中] 按Ctrl+C退出\n";
                while (event_thread_running && !g_is_ctrl_c) {
                    std::this_thread::sleep_for(std::chrono::milliseconds(100));
                }
            }
        }
        catch (const InterpreterError& e) {
            add_error(e.code, e.location, e.what());
        }
        catch (...) {
            add_error(ErrorCode::ER_0003, SourceLocation(), "未知运行时异常");
        }

        // 清理资源
        stop_event_thread();
    }

    // 获取运行时错误
    const std::vector<InterpreterError>& get_errors() const {
        return errors;
    }
};

#endif // INTERPRETER_H