//Copyright (c) Gallt Developer.
//��Ȩ���У�c��Gallt �����ߡ�
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
#include <memory> // ����ָ��ͷ�ļ�

// ȫ��ԭ�ӱ������̳���tokenizer��Ctrl+C�жϱ��
extern std::atomic<bool> g_is_ctrl_c;

// ------------------------------ ����ʱֵ���ͣ�Value�� ------------------------------
struct RuntimeValue {
    using ValueVariant = std::variant<int, float, double, std::string>;
    DataType type;       // ֵ������
    ValueVariant value;  // ʵ�ʴ洢��ֵ

    // Ĭ�Ϲ��캯��
    RuntimeValue() : type(DataType::DATA_TYPE_INT), value(0) {}
    // ���͹��캯��
    RuntimeValue(int val) : type(DataType::DATA_TYPE_INT), value(val) {}
    RuntimeValue(float val) : type(DataType::DATA_TYPE_FLOAT), value(val) {}
    RuntimeValue(double val) : type(DataType::DATA_TYPE_DOUBLE), value(val) {}
    RuntimeValue(const std::string& val) : type(DataType::DATA_TYPE_STRING), value(val) {}

    // ��ȡֵ���ַ�����ʾ�����������
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

    // ����ת������ȫת����ʧ�ܷ��ؿգ�
    template <typename T>
    std::optional<T> as() const {
        static_assert(
            std::is_same_v<T, int> || std::is_same_v<T, float> ||
            std::is_same_v<T, double> || std::is_same_v<T, std::string>,
            "as()ֻ֧��int/float/double/string����"
            );
        try {
            return std::get<T>(value);
        }
        catch (...) {
            return std::nullopt;
        }
    }

    // ��ֵ���㸨����ͳһת��Ϊdouble���м��㣨����int/float/double��
    std::optional<double> to_numeric() const {
        if (type == DataType::DATA_TYPE_INT) return static_cast<double>(std::get<int>(value));
        if (type == DataType::DATA_TYPE_FLOAT) return static_cast<double>(std::get<float>(value));
        if (type == DataType::DATA_TYPE_DOUBLE) return std::get<double>(value);
        return std::nullopt; // ����ֵ����
    }
};

// ����ֵ���洢RuntimeValue���б�
struct ArrayValue {
    std::vector<RuntimeValue> elements; // ����Ԫ��
    DataType element_type;              // Ԫ�����ͣ�ȷ������ͬ����

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

// ------------------------------ �¼����Ͷ��壨����DetectNodeִ�У� ------------------------------
enum class EventType {
    EVENT_INPUT,         // �����¼���input��
    EVENT_CLICK,         // ����¼���click����Ϊ���̻س���
    EVENT_ENTER,         // �س��¼���enter��
    EVENT_KEYBOARD_CLICK // ���̵���¼���keyboard.click��
};

// �¼����ݽṹ���洢�¼����ͺʹ���ֵ��
struct EventData {
    EventType type;
    RuntimeValue trigger_val; // ����ֵ��������ַ�"k"��
};

// ------------------------------ ����ʱ�������̰߳�ȫ+����ָ�룩 ------------------------------
class RuntimeEnvironment {
private:
    // ������ṹ��������shared_ptr��������Ұָ��
    struct Scope {
        std::unordered_map<std::string, RuntimeValue> variables;       // ��ͨ����+�������
        std::unordered_map<std::string, ArrayValue> arrays;            // �������
        std::unordered_map<std::string, std::shared_ptr<FunctionDefinitionNode>> functions; // ������shared_ptr��
    };

    std::vector<Scope> scopes;                  // ������ջ
    std::vector<std::pair<EventType, std::function<void(const EventData&)>>> event_listeners; // �¼�������
    mutable std::mutex env_mutex;               // �̰߳�ȫ������

public:
    RuntimeEnvironment() {
        push_scope(); // ��ʼȫ��������
    }

    // ����������̰߳�ȫ��
    void push_scope() {
        std::lock_guard<std::mutex> lock(env_mutex);
        scopes.emplace_back();
    }

    void pop_scope() {
        std::lock_guard<std::mutex> lock(env_mutex);
        if (!scopes.empty()) scopes.pop_back();
    }

    // ���������̰߳�ȫ��
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

    // ��������̰߳�ȫ��
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

    // ��������shared_ptr+�̰߳�ȫ��
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
        scopes.front().functions[name] = std::move(func); // �ƶ����壬���⿽��
    }

    // �¼����������̰߳�ȫ��
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

// ------------------------------ ���������󣨱���ԭ�У� ------------------------------
class InterpreterError : public std::runtime_error {
public:
    ErrorCode code;
    SourceLocation location;

    InterpreterError(ErrorCode c, const SourceLocation& loc, const std::string& msg)
        : std::runtime_error(msg), code(c), location(loc) {}
};

// ------------------------------ �������ࣨ�޸�unique_ptr�������⣩ ------------------------------
class Interpreter {
private:
    RuntimeEnvironment env;                      // ����ʱ����
    std::vector<InterpreterError> errors;        // ����ʱ����
    std::atomic<bool> is_running = true;         // �������б�ǣ�ԭ�ӱ�����
    std::thread event_thread;                    // �¼������߳�
    std::atomic<bool> event_thread_running = false; // �¼��̱߳��

    // �������ʱ����
    void add_error(ErrorCode code, const SourceLocation& loc, const std::string& msg) {
        errors.emplace_back(code, loc, msg);
        std::cerr << "����ʱ����: " << msg << "��" << loc.to_string() << "��\n";
    }

    // ------------------------------ ���ģ����ʽ���㣨��������ָ�룩 ------------------------------
    RuntimeValue evaluate_expression(ExpressionNode* expr) {
        if (!expr) {
            throw InterpreterError(ErrorCode::ER_0003, SourceLocation(), "�ձ��ʽ");
        }

        // 1. ���������ʽ
        if (auto lit = dynamic_cast<LiteralExprNode*>(expr)) {
            switch (lit->value_type) {
            case DataType::DATA_TYPE_INT: return RuntimeValue(std::stoi(lit->value));
            case DataType::DATA_TYPE_FLOAT: return RuntimeValue(std::stof(lit->value));
            case DataType::DATA_TYPE_DOUBLE: return RuntimeValue(std::stod(lit->value));
            case DataType::DATA_TYPE_STRING: return RuntimeValue(lit->value);
            default: throw InterpreterError(ErrorCode::ER_1001, lit->location, "δ֪����������");
            }
        }

        // 2. ��ʶ�����ʽ����ͨ����+���������
        else if (auto id = dynamic_cast<IdentifierExprNode*>(expr)) {
            if (auto var = env.find_variable(id->name)) {
                return **var;
            }
            // ����������������.varId��
            size_t dot_pos = id->name.find('.');
            if (dot_pos != std::string::npos) {
                std::string full_name = id->name;
                if (auto var = env.find_variable(full_name)) {
                    return **var;
                }
            }
            throw InterpreterError(ErrorCode::ER_0003, id->location, "δ����ı���: " + id->name);
        }

        // 3. ��Ԫ������ʽ
        else if (auto bin_op = dynamic_cast<BinaryOpExprNode*>(expr)) {
            RuntimeValue left = evaluate_expression(bin_op->left.get()); // ��get()��ȡԭʼָ�룬������
            RuntimeValue right = evaluate_expression(bin_op->right.get());
            switch (bin_op->op) {
            case TokenType::TOKEN_PLUS: return evaluate_addition(left, right, bin_op->location);
            case TokenType::TOKEN_MINUS: return evaluate_subtraction(left, right, bin_op->location);
            case TokenType::TOKEN_MUL: return evaluate_multiplication(left, right, bin_op->location);
            case TokenType::TOKEN_DIV: return evaluate_division(left, right, bin_op->location);
            case TokenType::TOKEN_EQUALS: return evaluate_equality(left, right, bin_op->location);
            case TokenType::TOKEN_LT: return evaluate_less_than(left, right, bin_op->location);
            case TokenType::TOKEN_GT: return evaluate_greater_than(left, right, bin_op->location);
            default: throw InterpreterError(ErrorCode::ER_0003, bin_op->location, "δ֪�����");
            }
        }

        // 4. ������ʱ��ʽ
        else if (auto arr_access = dynamic_cast<ArrayAccessExprNode*>(expr)) {
            std::string arr_name = arr_access->array_name->name;
            auto arr_opt = env.find_array(arr_name);
            if (!arr_opt) {
                throw InterpreterError(ErrorCode::ER_0003, arr_access->location, "δ���������: " + arr_name);
            }
            ArrayValue* arr = arr_opt.value();

            // ����������������int��
            RuntimeValue index_val = evaluate_expression(arr_access->index.get());
            auto index_opt = index_val.as<int>();
            if (!index_opt) {
                throw InterpreterError(ErrorCode::ER_1001, arr_access->index->location, "��������������int");
            }
            int index = index_opt.value();

            // ���Խ��
            if (index < 0 || static_cast<size_t>(index) >= arr->elements.size()) {
                throw InterpreterError(
                    ErrorCode::ER_2001,
                    arr_access->index->location,
                    "��������Խ��: " + std::to_string(index) + "����С: " + std::to_string(arr->elements.size()) + "��"
                );
            }
            return arr->elements[index];
        }

        // 5. �������ñ��ʽ
        else if (auto func_call = dynamic_cast<FunctionCallExprNode*>(expr)) {
            return call_function(func_call->function_name, func_call->arguments, func_call->location);
        }

        // 6. �б��ʽ
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

        // 7. Count���ʽ
        else if (auto count_expr = dynamic_cast<CountExprNode*>(expr)) {
            return evaluate_count_expression(count_expr);
        }

        // 8. �¼��������ʽ
        else if (auto event_cond = dynamic_cast<EventConditionExprNode*>(expr)) {
            return evaluate_expression(event_cond->trigger_val.get());
        }

        throw InterpreterError(ErrorCode::ER_0003, expr->location, "��֧�ֵı��ʽ����");
    }

    // Count���ʽ����
    RuntimeValue evaluate_count_expression(CountExprNode* count_expr) {
        RuntimeValue calc_val = evaluate_expression(count_expr->calc_expr.get());
        auto numeric_val_opt = calc_val.to_numeric();
        if (!numeric_val_opt) {
            throw InterpreterError(
                ErrorCode::ER_1001,
                count_expr->calc_expr->location,
                "Count���ʽ�������ݱ�������ֵ����"
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
            throw InterpreterError(ErrorCode::ER_0003, count_expr->location, "δ֪��Count����");
        }
    }

    // ------------------------------ ��Ԫ����ʵ�� ------------------------------
    RuntimeValue evaluate_addition(const RuntimeValue& left, const RuntimeValue& right, const SourceLocation& loc) {
        if (left.type == DataType::DATA_TYPE_STRING || right.type == DataType::DATA_TYPE_STRING) {
            return RuntimeValue(left.to_string() + right.to_string());
        }
        auto l_opt = left.to_numeric(), r_opt = right.to_numeric();
        if (!l_opt || !r_opt) throw InterpreterError(ErrorCode::ER_1001, loc, "�ӷ���֧�ַ���ֵ����");
        return RuntimeValue(l_opt.value() + r_opt.value());
    }

    RuntimeValue evaluate_subtraction(const RuntimeValue& left, const RuntimeValue& right, const SourceLocation& loc) {
        auto l_opt = left.to_numeric(), r_opt = right.to_numeric();
        if (!l_opt || !r_opt) throw InterpreterError(ErrorCode::ER_1001, loc, "������֧�ַ���ֵ����");
        return RuntimeValue(l_opt.value() - r_opt.value());
    }

    RuntimeValue evaluate_multiplication(const RuntimeValue& left, const RuntimeValue& right, const SourceLocation& loc) {
        auto l_opt = left.to_numeric(), r_opt = right.to_numeric();
        if (!l_opt || !r_opt) throw InterpreterError(ErrorCode::ER_1001, loc, "�˷���֧�ַ���ֵ����");
        return RuntimeValue(l_opt.value() * r_opt.value());
    }

    RuntimeValue evaluate_division(const RuntimeValue& left, const RuntimeValue& right, const SourceLocation& loc) {
        auto l_opt = left.to_numeric(), r_opt = right.to_numeric();
        if (!l_opt || !r_opt) throw InterpreterError(ErrorCode::ER_1001, loc, "������֧�ַ���ֵ����");
        if (r_opt.value() == 0) throw InterpreterError(ErrorCode::ER_0003, loc, "��������Ϊ0");
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
        if (!l_opt || !r_opt) throw InterpreterError(ErrorCode::ER_1001, loc, "С���жϲ�֧�ַ���ֵ����");
        return RuntimeValue(l_opt.value() < r_opt.value() ? 1 : 0);
    }

    RuntimeValue evaluate_greater_than(const RuntimeValue& left, const RuntimeValue& right, const SourceLocation& loc) {
        auto l_opt = left.to_numeric(), r_opt = right.to_numeric();
        if (!l_opt || !r_opt) throw InterpreterError(ErrorCode::ER_1001, loc, "�����жϲ�֧�ַ���ֵ����");
        return RuntimeValue(l_opt.value() > r_opt.value() ? 1 : 0);
    }

    // ------------------------------ �������ã��޸�unique_ptr�������ݣ� ------------------------------
    RuntimeValue call_function(const std::string& name, const std::vector<std::unique_ptr<ExpressionNode>>& args, const SourceLocation& loc) {
        // ���Һ�����shared_ptr��
        auto func_opt = env.find_function(name);
        if (!func_opt) throw InterpreterError(ErrorCode::ER_0003, loc, "δ����ĺ���: " + name);
        auto func = func_opt.value();

        // ����������
        if (args.size() != func->parameters.size()) {
            throw InterpreterError(
                ErrorCode::ER_1001,
                loc,
                "����" + name + "����������ƥ�䣨Ԥ��: " + std::to_string(func->parameters.size()) +
                ", ʵ��: " + std::to_string(args.size()) + "��"
            );
        }

        // �������ֵ�������ñ���args�����⿽��unique_ptr��
        std::vector<RuntimeValue> arg_values;
        for (const auto& arg : args) { // �ؼ���const& ���ñ�����������
            RuntimeValue arg_val = evaluate_expression(arg.get());
            DataType param_type = func->parameters[arg_values.size()].var_type;
            if (!is_type_compatible(arg_val.type, param_type)) {
                throw InterpreterError(
                    ErrorCode::ER_1001,
                    arg->location,
                    "����" + name + "����" + std::to_string(arg_values.size() + 1) + "���Ͳ�ƥ�䣨Ԥ��: " +
                    CommonUtils::data_type_to_string(param_type) + ", ʵ��: " +
                    CommonUtils::data_type_to_string(arg_val.type) + "��"
                );
            }
            arg_values.push_back(arg_val);
        }

        // ִ�к������������л�+�����󶨣�
        env.push_scope();
        for (size_t i = 0; i < func->parameters.size(); ++i) {
            const auto& param = func->parameters[i];
            env.add_variable(param.var_name, arg_values[i]);
        }

        RuntimeValue return_val(0);
        try {
            execute_block(func->function_body.get()); // ��get()��ȡԭʼָ�룬������
        }
        catch (const ReturnValue& ret) {
            return_val = ret.value;
        }

        env.pop_scope();
        return return_val;
    }

    // ���ͼ����Լ��
    bool is_type_compatible(DataType actual, DataType expected) {
        if (actual == expected) return true;
        // ��ֵ���ͼ��ݣ�int��float��double��
        if ((actual == DataType::DATA_TYPE_INT && (expected == DataType::DATA_TYPE_FLOAT || expected == DataType::DATA_TYPE_DOUBLE)) ||
            (actual == DataType::DATA_TYPE_FLOAT && expected == DataType::DATA_TYPE_DOUBLE)) {
            return true;
        }
        return false;
    }

    // ------------------------------ ���ִ�У������޸���unique_ptr����/���ݣ� ------------------------------
    // 1. ִ�д���飨�޸������ñ���statements�����⿽��unique_ptr��
    void execute_block(BlockNode* block) {
        if (!block || !is_running || g_is_ctrl_c) return;
        // �ؼ��޸���for (auto& stmt : ...) ���ñ�����������unique_ptr<StatementNode>
        for (auto& stmt : block->statements) {
            if (!is_running || g_is_ctrl_c) break;
            execute_statement(stmt.get()); // ��get()��ȡԭʼָ�룬������
        }
    }

    // 2. ִ�е�����䣨������ԭʼָ�룬������unique_ptr��
    void execute_statement(StatementNode* stmt) {
        if (!stmt || !is_running || g_is_ctrl_c) return;

        // �����������
        if (auto var_decl = dynamic_cast<VariableDeclarationNode*>(stmt)) {
            execute_variable_declaration(var_decl);
        }
        // ��ֵ���
        else if (auto assign = dynamic_cast<AssignmentNode*>(stmt)) {
            execute_assignment(assign);
        }
        // ������
        else if (auto output = dynamic_cast<OutputNode*>(stmt)) {
            execute_output(output);
        }
        // �������
        else if (auto input = dynamic_cast<InputNode*>(stmt)) {
            execute_input(input);
        }
        // �������
        else if (auto if_node = dynamic_cast<IfNode*>(stmt)) {
            execute_if(if_node);
        }
        // ѭ�����
        else if (auto loop = dynamic_cast<LoopNode*>(stmt)) {
            execute_loop(loop);
        }
        // �ȴ����
        else if (auto wait = dynamic_cast<WaitNode*>(stmt)) {
            execute_wait(wait);
        }
        // �߳���䣨�޸�lambda���񣬲�����unique_ptr��
        else if (auto thread = dynamic_cast<ThreadNode*>(stmt)) {
            execute_thread(thread);
        }
        // �������
        else if (auto ret = dynamic_cast<ReturnNode*>(stmt)) {
            execute_return(ret);
        }
        // �ͷ�main���
        else if (auto release_main = dynamic_cast<ReleaseMainNode*>(stmt)) {
            // ������ʱ����
        }
        // �˳����
        else if (auto exit_node = dynamic_cast<ExitNode*>(stmt)) {
            is_running = false;
            stop_event_thread();
        }
        // �����
        else if (auto block_stmt = dynamic_cast<BlockNode*>(stmt)) {
            execute_block(block_stmt);
        }
        // ��������������
        else if (auto var_node = dynamic_cast<VarNode*>(stmt)) {
            execute_var_node(var_node);
        }
        // �¼������䣨�޸�lambda���񣬲�����unique_ptr��
        else if (auto detect = dynamic_cast<DetectNode*>(stmt)) {
            execute_detect_node(detect);
        }
        else {
            add_error(ErrorCode::ER_0003, stmt->location, "��֧�ֵ��������");
        }
    }

    // 3. ִ���������������VarNode��
    void execute_var_node(VarNode* node) {
        for (const auto& item : node->var_items) {
            std::string full_var_name = node->var_group_name + "." + item.var_id;
            RuntimeValue init_val = evaluate_expression(item.var_value.get()); // get()��ȡָ��

            if (!is_type_compatible(init_val.type, item.var_type)) {
                add_error(
                    ErrorCode::ER_1001,
                    node->location,
                    "�������" + full_var_name + "���Ͳ�ƥ�䣨Ԥ��: " +
                    CommonUtils::data_type_to_string(item.var_type) + ", ʵ��: " +
                    CommonUtils::data_type_to_string(init_val.type) + "��"
                );
                continue;
            }

            env.add_variable(full_var_name, init_val);
            std::cout << "[�������ע��] " << full_var_name << " = " << init_val.to_string() << "\n";
        }
    }

    // 4. ִ���¼���⣨DetectNode���޸�lambda����
    void execute_detect_node(DetectNode* node) {
        // �����¼����ͺʹ���ֵ
        std::string event_type_str = node->event_condition->event_type;
        RuntimeValue trigger_val = evaluate_expression(node->event_condition->trigger_val.get());
        EventType event_type;

        if (event_type_str == "input") event_type = EventType::EVENT_INPUT;
        else if (event_type_str == "click") event_type = EventType::EVENT_CLICK;
        else if (event_type_str == "enter") event_type = EventType::EVENT_ENTER;
        else if (event_type_str == "keyboard.click") event_type = EventType::EVENT_KEYBOARD_CLICK;
        else {
            add_error(ErrorCode::ER_0003, node->location, "��֧�ֵ��¼�����: " + event_type_str);
            return;
        }

        // ע���¼����������޸���lambda����nodeָ�룬����������
        env.add_event_listener(event_type,
            [this, node_ptr = node, trigger_val, event_type_str](const EventData& event) { // ����ָ�룬������
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
                    std::cout << "\n[�¼�����] " << event_type_str << "������ֵ: " << trigger_val.to_string() << "��\n";
                    execute_block(node_ptr->run_block.get()); // get()��ȡָ�룬������
                }
            });

        std::cout << "[�¼�����ע��] " << event_type_str << "������ֵ: " << trigger_val.to_string() << "��\n";

        // �����¼��̣߳���һ�Σ�
        if (!event_thread_running) {
            event_thread_running = true;
            event_thread = std::thread(&Interpreter::event_listener_thread, this);
        }
    }

    // �¼������߳�
    void event_listener_thread() {
        while (event_thread_running && !g_is_ctrl_c) {
            if (_kbhit()) {
                char key = _getch();
                EventData event;

                if (key == 13) { // Enter��
                    event.type = EventType::EVENT_ENTER;
                    event.trigger_val = RuntimeValue(std::string("\n"));
                    env.trigger_event(event);
                }
                else if (key == 32) { // �ո����ģ��Click��
                    event.type = EventType::EVENT_CLICK;
                    event.trigger_val = RuntimeValue(1);
                    env.trigger_event(event);
                }
                else { // ��ͨ�ַ���Keyboard.Click + Input��
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

    // ֹͣ�¼��߳�
    void stop_event_thread() {
        if (event_thread_running) {
            event_thread_running = false;
            if (event_thread.joinable()) {
                event_thread.join();
            }
        }
        env.clear_event_listeners();
    }

    // ִ�б�������
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
                    add_error(ErrorCode::ER_2001, node->array_size->location, "��Ч�������С");
                    size = 1;
                }
            }
            ArrayValue arr(size, node->var_type);
            env.add_array(node->var_name, arr);
            std::cout << "[���鴴��] " << node->var_name << "����С: " << size << "��\n";
        }
        else {
            RuntimeValue init_val(0);
            if (node->initial_value) {
                init_val = evaluate_expression(node->initial_value.get());
            }
            env.add_variable(node->var_name, init_val);
            std::cout << "[��������] " << node->var_name << " = " << init_val.to_string() << "\n";
        }
    }

    // ִ�и�ֵ���
    void execute_assignment(AssignmentNode* node) {
        RuntimeValue value = evaluate_expression(node->value.get());

        // ��ֵĿ�꣺��ͨ����/�������
        if (auto id = dynamic_cast<IdentifierExprNode*>(node->target.get())) {
            auto var_opt = env.find_variable(id->name);
            if (var_opt) {
                RuntimeValue* var = var_opt.value();
                *var = value;
                std::cout << "[��ֵ] " << id->name << " = " << value.to_string() << "\n";
                return;
            }
            add_error(ErrorCode::ER_0003, id->location, "��ֵĿ��δ����: " + id->name);
            return;
        }

        // ��ֵĿ�꣺����Ԫ��
        else if (auto arr_access = dynamic_cast<ArrayAccessExprNode*>(node->target.get())) {
            std::string arr_name = arr_access->array_name->name;
            auto arr_opt = env.find_array(arr_name);
            if (!arr_opt) {
                add_error(ErrorCode::ER_0003, arr_access->location, "����δ����: " + arr_name);
                return;
            }
            ArrayValue* arr = arr_opt.value();

            RuntimeValue index_val = evaluate_expression(arr_access->index.get());
            auto index_opt = index_val.as<int>();
            if (!index_opt) {
                add_error(ErrorCode::ER_1001, arr_access->index->location, "��������������int");
                return;
            }
            int index = index_opt.value();

            if (index < 0 || static_cast<size_t>(index) >= arr->elements.size()) {
                add_error(ErrorCode::ER_2001, arr_access->index->location, "��������Խ��: " + std::to_string(index));
                return;
            }

            if (!is_type_compatible(value.type, arr->element_type)) {
                add_error(
                    ErrorCode::ER_1001,
                    node->value->location,
                    "����Ԫ�����Ͳ�ƥ�䣨Ԥ��: " + CommonUtils::data_type_to_string(arr->element_type) +
                    ", ʵ��: " + CommonUtils::data_type_to_string(value.type) + "��"
                );
                return;
            }

            arr->elements[index] = value;
            std::cout << "[���鸳ֵ] " << arr_name << "[" << index << "] = " << value.to_string() << "\n";
            return;
        }

        add_error(ErrorCode::ER_0003, node->target->location, "��Ч�ĸ�ֵĿ��");
    }

    // ִ��������
    void execute_output(OutputNode* node) {
        RuntimeValue val = evaluate_expression(node->expr.get());
        std::cout << "[���] " << val.to_string() << "\n";
    }

    // ִ���������
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
                    add_error(ErrorCode::ER_1001, node->location, "��֧�ֵ���������");
                    return;
                }
                std::cout << "[����] " << var_name << " = " << input_str << "\n";
            }
            catch (...) {
                add_error(ErrorCode::ER_1001, node->location, "�����ʽ��Ŀ�����Ͳ�ƥ��");
            }
            return;
        }
        add_error(ErrorCode::ER_0003, node->target->location, "����Ŀ��δ����: " + var_name);
    }

    // ִ���������
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
            std::cout << "[��������] ִ��if��\n";
            execute_block(node->then_block.get()); // get()��ȡָ�룬������
        }
        else if (node->else_block) {
            std::cout << "[����������] ִ��otherwise��\n";
            execute_block(node->else_block.get());
        }
    }

    // ִ��ѭ�����
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

    // ִ��Whileѭ��
    void execute_while_loop(LoopNode* node) {
        std::cout << "[Whileѭ��] ��ʼִ��\n";
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
            std::cout << "[Whileѭ��] ����" << ++iter_count << "\n";
            execute_block(node->body.get()); // get()��ȡָ�룬������
        }
        std::cout << "[Whileѭ��] ִ�н�������" << iter_count << "�ε�����\n";
    }

    // ִ��Forѭ��
    void execute_for_loop(LoopNode* node) {
        if (node->for_loop_var.empty() || !node->for_start || !node->for_end || !node->for_step) {
            add_error(ErrorCode::ER_0003, node->location, "Forѭ������������");
            return;
        }

        RuntimeValue start_val = evaluate_expression(node->for_start.get());
        RuntimeValue end_val = evaluate_expression(node->for_end.get());
        RuntimeValue step_val = evaluate_expression(node->for_step.get());

        auto start_opt = start_val.as<int>();
        auto end_opt = end_val.as<int>();
        auto step_opt = step_val.as<int>();

        if (!start_opt || !end_opt || !step_opt || step_opt.value() <= 0) {
            add_error(ErrorCode::ER_1001, node->location, "Forѭ����������������������");
            return;
        }

        int start = start_opt.value();
        int end = end_opt.value();
        int step = step_opt.value();

        env.push_scope();
        RuntimeValue loop_var_val(start);
        env.add_variable(node->for_loop_var, loop_var_val);
        std::cout << "[Forѭ��] ��ʼִ�У�" << node->for_loop_var << "��" << start << "��" << end << "������" << step << "��\n";

        int iter_count = 0;
        while (is_running && !g_is_ctrl_c) {
            auto current_var_opt = env.find_variable(node->for_loop_var);
            if (!current_var_opt) break;

            RuntimeValue* current_var = current_var_opt.value();
            auto current_opt = current_var->as<int>();
            if (!current_opt) break;

            int current = current_opt.value();
            if (current > end) break;

            std::cout << "[Forѭ��] ����" << ++iter_count << "��" << node->for_loop_var << "=" << current << "��\n";
            execute_block(node->body.get()); // get()��ȡָ�룬������

            current_var->value = current + step;
        }

        env.pop_scope();
        std::cout << "[Forѭ��] ִ�н�������" << iter_count << "�ε�����\n";
    }

    // ִ��reѭ��
    void execute_re_loop(LoopNode* node) {
        if (node->is_infinite) {
            std::cout << "[reѭ��] ����ִ�У���Ctrl+Cֹͣ��\n";
            int iter_count = 0;
            while (is_running && !g_is_ctrl_c) {
                std::cout << "[reѭ��] ����" << ++iter_count << "\n";
                execute_block(node->body.get()); // get()��ȡָ�룬������

                if (node->re_wait_duration) {
                    execute_re_wait(node);
                }
            }
            std::cout << "[reѭ��] ���жϣ���" << iter_count << "�ε�����\n";
        }
        else {
            RuntimeValue count_val = evaluate_expression(node->condition.get());
            auto count_opt = count_val.as<int>();

            if (!count_opt || count_opt.value() <= 0 || count_opt.value() > GalltConstants::MAX_RE_NORMAL_COUNT) {
                add_error(ErrorCode::ER_0005, node->condition->location, "reѭ��������Ч��1~65536��");
                return;
            }

            int count = count_opt.value();
            std::cout << "[reѭ��] ��ʼִ�У���" << count << "�ε�����\n";
            for (int i = 0; i < count && is_running && !g_is_ctrl_c; ++i) {
                std::cout << "[reѭ��] ����" << (i + 1) << "\n";
                execute_block(node->body.get()); // get()��ȡָ�룬������

                if (node->re_wait_duration) {
                    execute_re_wait(node);
                }
            }
            std::cout << "[reѭ��] ִ�н���\n";
        }
    }

    // ִ��reѭ����wait
    void execute_re_wait(LoopNode* node) {
        if (!node->re_wait_duration) return;

        RuntimeValue dur_val = evaluate_expression(node->re_wait_duration.get());
        auto dur_opt = dur_val.as<int>();
        if (!dur_opt || dur_opt.value() <= 0) {
            add_error(ErrorCode::ER_0003, node->re_wait_duration->location, "re�ȴ�ʱ������Ϊ������");
            return;
        }
        int dur = dur_opt.value();

        auto ms_opt = CommonUtils::convert_time_to_ms(dur, node->re_wait_unit);
        if (!ms_opt) {
            add_error(ErrorCode::ER_0003, node->location, "��֧�ֵ�re�ȴ���λ: " + node->re_wait_unit);
            return;
        }
        int ms = ms_opt.value();

        std::cout << "[re�ȴ�] " << dur << node->re_wait_unit << "��" << ms << "ms��\n";
        std::this_thread::sleep_for(std::chrono::milliseconds(ms));
    }

    // ִ��Wait���
    void execute_wait(WaitNode* node) {
        if (!node->duration) {
            add_error(ErrorCode::ER_0003, node->location, "Wait���ȱ��ʱ��");
            return;
        }

        RuntimeValue dur_val = evaluate_expression(node->duration.get());
        auto dur_opt = dur_val.as<int>();
        if (!dur_opt || dur_opt.value() <= 0) {
            add_error(ErrorCode::ER_0003, node->duration->location, "Waitʱ������Ϊ������");
            return;
        }
        int dur = dur_opt.value();

        auto ms_opt = CommonUtils::convert_time_to_ms(dur, node->unit);
        if (!ms_opt) {
            add_error(ErrorCode::ER_0003, node->location, "��֧�ֵ�Wait��λ: " + node->unit);
            return;
        }
        int ms = ms_opt.value();

        std::cout << "[Wait] " << dur << node->unit << "��" << ms << "ms��\n";
        std::this_thread::sleep_for(std::chrono::milliseconds(ms));
    }

    // ִ���߳���䣨�޸�lambda���񣬲�����unique_ptr��
    void execute_thread(ThreadNode* node) {
        std::cout << "[�߳�] " << node->thread_name << " ��ʼ����\n";
        // �޸�������nodeָ��+�߳���ָ�룬������unique_ptr
        std::thread t([this, thread_name = node->thread_name, body_ptr = node->thread_body.get()]() {
            std::cout << "[�߳�] " << thread_name << " ��ʼִ��\n";
            execute_block(body_ptr); // ����ǰ��ȡ��ԭʼָ�룬������
            std::cout << "[�߳�] " << thread_name << " ִ�н���\n";
            });
        t.detach();
    }

    // ���������쳣
    struct ReturnValue {
        RuntimeValue value;
        ReturnValue(RuntimeValue v) : value(std::move(v)) {}
    };

    // ִ�з������
    void execute_return(ReturnNode* node) {
        RuntimeValue ret_val(0);
        if (node->return_value) {
            ret_val = evaluate_expression(node->return_value.get());
        }
        throw ReturnValue(ret_val);
    }

public:
    Interpreter() = default;

    // ���������������¼��߳�
    ~Interpreter() {
        stop_event_thread();
    }

    // ִ�г�����ڣ�����ԭʼָ�룬������unique_ptr��
    void execute(ProgramNode* program) {
        if (!program) return;
        try {
            // ע��ȫ�ֺ�����unique_ptr��shared_ptr���ƶ����壩
            for (auto& func_unique : program->functions) {
                if (!func_unique) {
                    add_error(ErrorCode::ER_0003, SourceLocation(), "��Ч�ĺ���ָ�루nullptr��");
                    continue;
                }
                // �ƶ�unique_ptr����Ȩ��shared_ptr�����⿽��
                auto func_shared = std::shared_ptr<FunctionDefinitionNode>(std::move(func_unique));
                env.add_function(func_shared->function_name, std::move(func_shared));
                std::cout << "[����ע��] " << func_shared->function_name << "\n";
            }

            // ִ��main�������ñ�����������unique_ptr��
            for (auto& main : program->mains) { // �ؼ���auto& ���ñ���
                if (!is_running || g_is_ctrl_c) break;
                std::cout << "\n[��ʼִ��main" << (main->main_name.empty() ? "" : (" " + main->main_name)) << "]\n";
                execute_block(main->main_body.get()); // get()��ȡָ�룬������
                std::cout << "[main" << (main->main_name.empty() ? "" : (" " + main->main_name)) << "ִ�н���]\n";
            }

            // �ȴ��¼��߳�
            if (event_thread_running) {
                std::cout << "\n[�¼�������] ��Ctrl+C�˳�\n";
                while (event_thread_running && !g_is_ctrl_c) {
                    std::this_thread::sleep_for(std::chrono::milliseconds(100));
                }
            }
        }
        catch (const InterpreterError& e) {
            add_error(e.code, e.location, e.what());
        }
        catch (...) {
            add_error(ErrorCode::ER_0003, SourceLocation(), "δ֪����ʱ�쳣");
        }

        // ������Դ
        stop_event_thread();
    }

    // ��ȡ����ʱ����
    const std::vector<InterpreterError>& get_errors() const {
        return errors;
    }
};

#endif // INTERPRETER_H