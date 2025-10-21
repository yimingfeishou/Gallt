//Copyright (c) Gallt Developer.
//��Ȩ���У�c��Gallt �����ߡ�

#ifndef SEMANTIC_ANALYZER_H
#define SEMANTIC_ANALYZER_H
#include "common.h"
#include "ast_nodes.h"
#include <unordered_map>
#include <vector>
#include <optional>
#include <stdexcept>
#include <string>
#include <cmath>
#include <algorithm>

// ���ű���Ŀ����¼����/������������Ϣ����չ֧�����������
struct Symbol {
    enum class SymbolType { VARIABLE, FUNCTION } type; // �������ͣ�����/������
    std::string name;                                 // �������ƣ���ͨ���������������������������.varId��
    SourceLocation declaration_loc;                   // ����λ�ã����ڱ���

    // ����������Ϣ����չ���������ǣ�
    struct VarInfo {
        DataType data_type;      // ��������
        bool is_array;           // �Ƿ�Ϊ����
        size_t array_size;       // �����С������is_arrayΪtrue��
        bool is_initialized;     // �Ƿ��ѳ�ʼ��
        bool is_special_var;     // �Ƿ�Ϊ���������var.name�µ�var1/var2��
        std::string var_group;   // �������������������"Peter"����is_special_varΪtrueʱ��Ч��
    };

    // ����������Ϣ������ԭ�У�
    struct FuncInfo {
        DataType return_type;                          // ��������
        std::vector<DataType> param_types;             // ���������б�
        std::vector<std::string> param_names;          // ���������б����ڼ���ظ���
    };

    // ��std::optional���union��֧�ַ�POD����
    std::optional<VarInfo> var_info;   // ������Ϣ������typeΪVARIABLEʱ��Ч��
    std::optional<FuncInfo> func_info; // ������Ϣ������typeΪFUNCTIONʱ��Ч��

    // Ĭ�Ϲ��캯��
    Symbol() : type(SymbolType::VARIABLE), name(), declaration_loc(), var_info(), func_info() {}

    // ���캯������ͨ������
    Symbol(const std::string& n, const SourceLocation& loc, const VarInfo& vi)
        : type(SymbolType::VARIABLE), name(n), declaration_loc(loc), var_info(vi) {
    }

    // ���캯���������������������
    Symbol(const std::string& group_name, const std::string& var_id, const SourceLocation& loc, const VarInfo& vi)
        : type(SymbolType::VARIABLE), name(group_name + "." + var_id), declaration_loc(loc), var_info(vi) {
    }

    // ���캯����������
    Symbol(const std::string& n, const SourceLocation& loc, const FuncInfo& fi)
        : type(SymbolType::FUNCTION), name(n), declaration_loc(loc), func_info(fi) {
    }
};

// �����������ṹ������ԭ�У�
struct SemanticError {
    ErrorCode code;
    SourceLocation location;
    std::string message;
    SemanticError(ErrorCode c, const SourceLocation& loc, const std::string& msg)
        : code(c), location(loc), message(msg) {
    }
};

// ���ű�����ࣨ��չ����������ͻ��⣩
class SymbolTable {
private:
    std::vector<std::unordered_map<std::string, Symbol>> scopes; // ������ջ��vectorʵ�֣�

public:
    SymbolTable() {
        push_scope(); // ��ʼȫ��������
    }

    // ������������
    void push_scope() {
        scopes.emplace_back();
    }

    // �˳���ǰ������
    void pop_scope() {
        if (!scopes.empty()) {
            scopes.pop_back();
        }
    }

    // ���ҷ��ţ��ӵ�ǰ����������������
    const Symbol* find_symbol(const std::string& name) const {
        for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
            auto found = it->find(name);
            if (found != it->end()) {
                return &(found->second);
            }
        }
        return nullptr;
    }

    // ��չ����ӷ��ţ����ֺ���������ͻER 0006����ͨ���ų�ͻER 0003��
    bool add_symbol(const Symbol& symbol, SemanticError& error) {
        if (scopes.back().count(symbol.name)) {
            const Symbol& existing = scopes.back()[symbol.name];
            // ����������ͻ����ǰ���ź����з��ž�Ϊ����
            if (symbol.type == Symbol::SymbolType::FUNCTION && existing.type == Symbol::SymbolType::FUNCTION) {
                error = SemanticError(
                    ErrorCode::ER_0006, // ����������ͻר��������
                    symbol.declaration_loc,
                    "����������ͻ��" + symbol.name + "������" +
                    existing.declaration_loc.to_string() + "������"
                );
                return false;
            }
            // ��ͨ���ų�ͻ������-����������-������
            error = SemanticError(
                ErrorCode::ER_0003,
                symbol.declaration_loc,
                "�����ظ�����: " + symbol.name + "������" +
                existing.declaration_loc.to_string() + "������"
            );
            return false;
        }
        scopes.back()[symbol.name] = symbol;
        return true;
    }
};

// ����������ࣨ��չ�����ڵ������У�飩
class SemanticAnalyzer {
private:
    SymbolTable symbol_table;          // ���ű�
    std::vector<SemanticError> errors; // �����б�
    int main_count = 0;                // main����������ER 0001��
    bool last_main_released = true;    // ��һ��main�Ƿ��ͷ�
    std::unordered_map<std::string, std::vector<std::string>> special_var_groups; // ����������¼��������varId�б�

    // ��Ӵ����б�
    void add_error(ErrorCode code, const SourceLocation& loc, const std::string& msg) {
        errors.emplace_back(code, loc, msg);
    }

    // ����ʶ���Ƿ�������
    bool check_identifier_declared(const std::string& name, const SourceLocation& loc) {
        if (!symbol_table.find_symbol(name)) {
            add_error(ErrorCode::ER_0003, loc, "δ����ı�ʶ��: " + name);
            return false;
        }
        return true;
    }

    // ��������Ƿ���ݣ���չ��ֵ�����Զ�������
    bool check_type_compatibility(DataType actual, DataType expected, const SourceLocation& loc, const std::string& context) {
        if (actual == DataType::DATA_TYPE_UNKNOWN || expected == DataType::DATA_TYPE_UNKNOWN) {
            return true; // δ֪�����ݲ�����
        }
        // ��ȫƥ��
        if (actual == expected) {
            return true;
        }
        // ��ֵ���ͼ��ݣ�int��float��double��
        if ((actual == DataType::DATA_TYPE_INT && (expected == DataType::DATA_TYPE_FLOAT || expected == DataType::DATA_TYPE_DOUBLE)) ||
            (actual == DataType::DATA_TYPE_FLOAT && expected == DataType::DATA_TYPE_DOUBLE)) {
            return true;
        }
        // ������
        add_error(
            ErrorCode::ER_1001,
            loc,
            context + "���Ͳ�ƥ��: Ԥ��" + CommonUtils::data_type_to_string(expected) +
            "��ʵ��" + CommonUtils::data_type_to_string(actual)
        );
        return false;
    }

    // �����ʽ�Ƿ�Ϊ��ֵ���ͣ�int/float/double��
    bool check_numeric_expression(ExpressionNode* expr, const SourceLocation& loc, const std::string& context) {
        if (!expr) {
            add_error(ErrorCode::ER_0003, loc, context + "����Ϊ��");
            return false;
        }
        DataType expr_type = get_expression_type(expr);
        if (expr_type == DataType::DATA_TYPE_INT || expr_type == DataType::DATA_TYPE_FLOAT || expr_type == DataType::DATA_TYPE_DOUBLE) {
            return true;
        }
        add_error(
            ErrorCode::ER_1001,
            loc,
            context + "��������ֵ���ͣ�int/float/double����ʵ��Ϊ" + CommonUtils::data_type_to_string(expr_type)
        );
        return false;
    }

    // �������ʽ���ͣ���չ֧��CountExprNode��EventConditionExprNode��
    DataType get_expression_type(ExpressionNode* expr) {
        if (!expr) return DataType::DATA_TYPE_UNKNOWN;

        if (auto lit = dynamic_cast<LiteralExprNode*>(expr)) {
            return lit->value_type;
        }
        else if (auto id = dynamic_cast<IdentifierExprNode*>(expr)) {
            // ����Ƿ�Ϊ�����������Peter.var1��
            const Symbol* sym = symbol_table.find_symbol(id->name);
            if (sym && sym->type == Symbol::SymbolType::VARIABLE && sym->var_info.has_value()) {
                return sym->var_info.value().data_type;
            }
            return DataType::DATA_TYPE_UNKNOWN;
        }
        else if (auto bin_op = dynamic_cast<BinaryOpExprNode*>(expr)) {
            DataType left_type = get_expression_type(bin_op->left.get());
            DataType right_type = get_expression_type(bin_op->right.get());
            // �ַ���ƴ�ӣ����������Ϊ�ַ�����
            if (left_type == DataType::DATA_TYPE_STRING && right_type == DataType::DATA_TYPE_STRING) {
                return DataType::DATA_TYPE_STRING;
            }
            // ��ֵ���㣨ȡ�������ͣ�
            if (left_type == DataType::DATA_TYPE_DOUBLE || right_type == DataType::DATA_TYPE_DOUBLE) {
                return DataType::DATA_TYPE_DOUBLE;
            }
            else if (left_type == DataType::DATA_TYPE_FLOAT || right_type == DataType::DATA_TYPE_FLOAT) {
                return DataType::DATA_TYPE_FLOAT;
            }
            else if (left_type == DataType::DATA_TYPE_INT && right_type == DataType::DATA_TYPE_INT) {
                return DataType::DATA_TYPE_INT;
            }
            return DataType::DATA_TYPE_UNKNOWN;
        }
        else if (auto arr_access = dynamic_cast<ArrayAccessExprNode*>(expr)) {
            const Symbol* arr_sym = symbol_table.find_symbol(arr_access->array_name->name);
            if (arr_sym && arr_sym->type == Symbol::SymbolType::VARIABLE && arr_sym->var_info.has_value() && arr_sym->var_info.value().is_array) {
                // ��������Ƿ�Ϊint
                DataType index_type = get_expression_type(arr_access->index.get());
                if (index_type != DataType::DATA_TYPE_INT && index_type != DataType::DATA_TYPE_UNKNOWN) {
                    add_error(
                        ErrorCode::ER_1001,
                        arr_access->index->location,
                        "��������������int���ͣ�ʵ��Ϊ" + CommonUtils::data_type_to_string(index_type)
                    );
                }
                return arr_sym->var_info.value().data_type;
            }
            return DataType::DATA_TYPE_UNKNOWN;
        }
        else if (auto func_call = dynamic_cast<FunctionCallExprNode*>(expr)) {
            const Symbol* func_sym = symbol_table.find_symbol(func_call->function_name);
            if (func_sym && func_sym->type == Symbol::SymbolType::FUNCTION && func_sym->func_info.has_value()) {
                return func_sym->func_info.value().return_type;
            }
            return DataType::DATA_TYPE_UNKNOWN;
        }
        else if (auto pi_expr = dynamic_cast<PiExprNode*>(expr)) {
            return DataType::DATA_TYPE_DOUBLE; // ��Ĭ�Ϸ���double
        }
        // ����������Count���ʽ����
        else if (auto count_expr = dynamic_cast<CountExprNode*>(expr)) {
            return count_expr->get_type(); // ֱ��ʹ��CountExprNodeԤ����ķ�������
        }
        // �����������¼��������ʽ�������¼�����ֵ���ͣ�
        else if (auto event_cond = dynamic_cast<EventConditionExprNode*>(expr)) {
            return get_expression_type(event_cond->trigger_val.get());
        }
        return DataType::DATA_TYPE_UNKNOWN;
    }

    // ------------------------------ �����ڵ��visit���� ------------------------------
    // ����������������������ڵ㣨VarNode��
    void visit(VarNode* node) {
        // 1. �����������Ƿ��ظ�
        if (special_var_groups.count(node->var_group_name)) {
            add_error(
                ErrorCode::ER_0003,
                node->location,
                "������������ظ���" + node->var_group_name + "���Ѵ���ͬ�������飩"
            );
            return;
        }
        special_var_groups[node->var_group_name] = {}; // ��ʼ������varId�б�

        // 2. ����ÿ��VarItem
        for (const auto& item : node->var_items) {
            SourceLocation item_loc = node->location; // �򻯣�ʵ��Ӧȡitem��λ�ã�����չVarItem���location��
            std::string full_var_name = node->var_group_name + "." + item.var_id;

            // 2.1 ���varId�Ƿ��������ظ�
            auto& group_var_ids = special_var_groups[node->var_group_name];
            if (std::find(group_var_ids.begin(), group_var_ids.end(), item.var_id) != group_var_ids.end()) {
                add_error(
                    ErrorCode::ER_0003,
                    item_loc,
                    "������" + node->var_group_name + "��varId�ظ���" + item.var_id
                );
                continue;
            }
            group_var_ids.push_back(item.var_id);

            // 2.2 ����ʼֵ�����Ƿ�ƥ��
            if (!item.var_value) {
                add_error(ErrorCode::ER_0003, item_loc, "�������" + full_var_name + "�����ʼ��");
                continue;
            }
            DataType init_type = get_expression_type(item.var_value.get());
            if (!check_type_compatibility(init_type, item.var_type, item_loc, "�������" + full_var_name + "��ʼ��")) {
                continue;
            }

            // 2.3 ע��������������ű�
            Symbol::VarInfo var_info;
            var_info.data_type = item.var_type;
            var_info.is_array = false; // ��������ݲ�֧������
            var_info.array_size = 0;
            var_info.is_initialized = true;
            var_info.is_special_var = true;
            var_info.var_group = node->var_group_name;

            Symbol var_symbol(node->var_group_name, item.var_id, item_loc, var_info);
            SemanticError add_err(ErrorCode::NO_ERROR, item_loc, "");
            if (!symbol_table.add_symbol(var_symbol, add_err)) {
                errors.push_back(add_err);
            }
        }
    }

    // �����������¼����ڵ㣨DetectNode��
    void visit(DetectNode* node) {
        if (!node->event_condition) {
            add_error(ErrorCode::ER_0003, node->location, "�¼����ȱ����������detect keyboard.click=\"k\"��");
            return;
        }
        // 1. ����¼������Ƿ�֧��
        std::string event_type = node->event_condition->event_type;
        if (!CommonUtils::is_supported_event(event_type)) {
            add_error(
                ErrorCode::ER_0003,
                node->event_condition->location,
                "��֧�ֵ��¼����ͣ�" + event_type + "��֧�֣�input/click/enter/keyboard.click��"
            );
            return;
        }

        // 2. ��鴥��ֵ���ͣ������¼�����Լ����
        if (!node->event_condition->trigger_val) {
            add_error(ErrorCode::ER_0003, node->event_condition->location, event_type + "�¼�ȱ�ٴ���ֵ����\"k\"��");
            return;
        }
        DataType trigger_type = get_expression_type(node->event_condition->trigger_val.get());
        bool trigger_type_valid = false;
        if (event_type == "keyboard.click" || event_type == "input" || event_type == "enter") {
            // ����/�����¼�����ֵ�������ַ�������"k"��
            trigger_type_valid = (trigger_type == DataType::DATA_TYPE_STRING);
        }
        else if (event_type == "click") {
            // ����¼�����ֵ��ѡ�������꣬��֧��int��
            trigger_type_valid = (trigger_type == DataType::DATA_TYPE_INT || trigger_type == DataType::DATA_TYPE_UNKNOWN);
        }
        if (!trigger_type_valid) {
            add_error(
                ErrorCode::ER_1001,
                node->event_condition->trigger_val->location,
                event_type + "�¼��Ĵ���ֵ������" + (event_type == "click" ? "int" : "string") + "���ͣ�ʵ��Ϊ" + CommonUtils::data_type_to_string(trigger_type)
            );
        }

        // 3. ���run�����������
        if (node->run_block) {
            visit(node->run_block.get());
        }
    }

    // ����������Count��ֵ������ʽ��CountExprNode��
    void visit(CountExprNode* node) {
        // 1. ����������ʽ�Ƿ�Ϊ��ֵ����
        if (!node->calc_expr) {
            add_error(ErrorCode::ER_0003, node->location, "Count���ʽȱ�ٴ��������ݣ���count(5*8)��");
            return;
        }
        if (!check_numeric_expression(node->calc_expr.get(), node->calc_expr->location, "Count���ʽ�ļ�������")) {
            return;
        }

        // 2. ��֤Count���Ͷ�Ӧ�ļ����߼�����ѡ����COUNT_NORMAL��֧��int���㣩
        DataType calc_type = get_expression_type(node->calc_expr.get());
        if (node->count_type == CountExprNode::CountType::COUNT_NORMAL && calc_type != DataType::DATA_TYPE_INT) {
            add_error(
                ErrorCode::ER_1001,
                node->location,
                "��ͨcount(...)��֧��int���ͼ��㣬ʵ��Ϊ" + CommonUtils::data_type_to_string(calc_type) + "������float.count/double.count��"
            );
        }
    }

    // ------------------------------ ��չ���нڵ��visit���� ------------------------------
    // ��չ�����ʵȴ��ڵ㣨WaitNode��֧�ֶ൥λУ�飩
    void visit(WaitNode* node) {
        if (!node->duration) {
            add_error(ErrorCode::ER_0003, node->location, "Wait���ȱ��ʱ������wait time=3000ms��");
            return;
        }
        // 1. ���ȴ�ʱ���Ƿ�Ϊ����ֵ
        if (!check_numeric_expression(node->duration.get(), node->duration->location, "Waitʱ��")) {
            return;
        }
        // 2. ���ʱ���Ƿ�Ϊ���������������ɾ�̬��飩
        if (auto lit = dynamic_cast<LiteralExprNode*>(node->duration.get())) {
            double dur = 0.0;
            if (lit->value_type == DataType::DATA_TYPE_INT) {
                dur = std::stoi(lit->value);
            }
            else if (lit->value_type == DataType::DATA_TYPE_FLOAT) {
                dur = std::stof(lit->value);
            }
            else if (lit->value_type == DataType::DATA_TYPE_DOUBLE) {
                dur = std::stod(lit->value);
            }
            if (dur <= 0) {
                add_error(ErrorCode::ER_0003, node->duration->location, "Waitʱ������Ϊ��������ǰ��" + std::to_string(dur) + "��");
            }
        }

        // 3. ��鵥λ�Ƿ�Ϸ���ͨ��CommonUtils��֤��
        if (GalltConstants::WAIT_SUPPORTED_UNITS.find(node->unit) == GalltConstants::WAIT_SUPPORTED_UNITS.end()) {
            add_error(
                ErrorCode::ER_0003,
                node->location,
                "��֧�ֵ�Wait��λ��" + node->unit + "��֧�֣�ms/s/min/hour��"
            );
        }
    }

    // ��չ������ѭ���ڵ㣨LoopNode��֧��For��re waitУ�飩- �޸���������������
    void visit(LoopNode* node) {
        switch (node->loop_type) {
        case LoopNode::LoopType::LOOP_WHILE: {
            // ԭ���߼����������Ϊ��ֵ����
            if (!node->condition) {
                add_error(ErrorCode::ER_0003, node->location, "Whileѭ��ȱ����������while(a < 10)��");
                break;
            }
            if (!check_numeric_expression(node->condition.get(), node->condition->location, "Whileѭ������")) {
                break;
            }
            // ���ѭ����
            if (node->body) {
                visit(node->body.get());
            }
            break;
        }

        case LoopNode::LoopType::LOOP_FOR: {
            // 1. ���Forѭ����Ҫ�ֶΣ������ָ�룩
            if (node->for_loop_var.empty()) {
                add_error(ErrorCode::ER_0003, node->location, "Forѭ��ȱ�ٱ���������for i 1 to 50��");
                break;
            }
            if (!node->for_start) {
                add_error(ErrorCode::ER_0003, node->location, "Forѭ��ȱ����ʼֵ����for i 1 to 50��");
                break;
            }
            if (!node->for_end) {
                add_error(ErrorCode::ER_0003, node->location, "Forѭ��ȱ�ٽ���ֵ����for i 1 to 50��");
                break;
            }
            // ����Ϊ��ʱ��ʼ��Ĭ��ֵ���޸���ָ�룩
            if (!node->for_step) {
                node->for_step = std::make_unique<LiteralExprNode>(node->location, DataType::DATA_TYPE_INT, "1");
            }

            // 2. ���ѭ�������Ƿ��ظ�������ʹ��try-catchȷ���������˳���
            symbol_table.push_scope();
            bool scope_pushed = true;
            try {
                // 2.1 ��ʱע��ѭ����������ǰ������
                Symbol::VarInfo var_info;
                var_info.data_type = DataType::DATA_TYPE_INT; // ѭ������Ĭ��Ϊint
                var_info.is_array = false;
                var_info.array_size = 0;
                var_info.is_initialized = true;
                var_info.is_special_var = false;
                var_info.var_group = "";
                Symbol loop_var_sym(node->for_loop_var, node->location, var_info);
                SemanticError add_err(ErrorCode::NO_ERROR, node->location, "");
                if (!symbol_table.add_symbol(loop_var_sym, add_err)) {
                    errors.push_back(add_err);
                    // ��ʹ�����ظ����Լ�����������ֶΣ�����������жϣ�
                }

                // 2.2 �����ʼֵ������ֵ������Ϊ��ֵ����
                if (!check_numeric_expression(node->for_start.get(), node->for_start->location, "Forѭ����ʼֵ")) {
                    throw std::runtime_error("For��ʼֵ����"); // ����catch��ȷ��pop_scope
                }
                if (!check_numeric_expression(node->for_end.get(), node->for_end->location, "Forѭ������ֵ")) {
                    throw std::runtime_error("For����ֵ����");
                }
                if (!check_numeric_expression(node->for_step.get(), node->for_step->location, "Forѭ������")) {
                    throw std::runtime_error("For��������");
                }

                // 2.3 ��鲽��Ϊ��������������ѭ����
                if (auto step_lit = dynamic_cast<LiteralExprNode*>(node->for_step.get())) {
                    if (step_lit->value_type != DataType::DATA_TYPE_INT) {
                        add_error(ErrorCode::ER_1001, node->for_step->location, "Forѭ������������int���ͣ��������������������ѭ����");
                        throw std::runtime_error("For�������ʹ���");
                    }
                    int step = std::stoi(step_lit->value);
                    if (step <= 0) {
                        add_error(ErrorCode::ER_0003, node->for_step->location, "Forѭ����������Ϊ����������ǰ��" + std::to_string(step) + "��");
                        throw std::runtime_error("For����ֵ����");
                    }
                }

                // 2.4 ���ѭ����
                if (node->body) {
                    visit(node->body.get());
                }
            }
            catch (...) {
                // �����Ƿ񱨴���ȷ���˳��������޸�������������
                if (scope_pushed) {
                    symbol_table.pop_scope();
                    scope_pushed = false;
                }
            }
            // ���������˳�������
            if (scope_pushed) {
                symbol_table.pop_scope();
            }
            break;
        }

        case LoopNode::LoopType::LOOP_RE: {
            // 1. ������ѭ����������Ϊ������
            if (!node->is_infinite) {
                if (!node->condition) {
                    add_error(ErrorCode::ER_0003, node->location, "������reѭ��ȱ�ٴ�������re number=3��");
                    break;
                }
                if (!check_numeric_expression(node->condition.get(), node->condition->location, "reѭ������")) {
                    break;
                }
                if (auto count_lit = dynamic_cast<LiteralExprNode*>(node->condition.get())) {
                    if (count_lit->value_type != DataType::DATA_TYPE_INT) {
                        add_error(ErrorCode::ER_1001, node->condition->location, "reѭ������������int���ͣ���ǰ��" + CommonUtils::data_type_to_string(count_lit->value_type) + "��");
                        break;
                    }
                    int count = std::stoi(count_lit->value);
                    if (count <= 0) {
                        add_error(ErrorCode::ER_0005, node->condition->location, "reѭ����������Ϊ����������ǰ��" + std::to_string(count) + "��");
                    }
                    else if (count > GalltConstants::MAX_RE_NORMAL_COUNT) {
                        add_error(ErrorCode::ER_0005, node->condition->location, "reѭ�������������ޣ�" + std::to_string(count) + " > 65536��");
                    }
                }
            }

            // 2. ��wait��reѭ�������waitʱ���͵�λ
            if (node->re_wait_duration) {
                // ���waitʱ��
                if (!check_numeric_expression(node->re_wait_duration.get(), node->re_wait_duration->location, "reѭ���ȴ�ʱ��")) {
                    break;
                }
                if (auto dur_lit = dynamic_cast<LiteralExprNode*>(node->re_wait_duration.get())) {
                    double dur = 0.0;
                    if (dur_lit->value_type == DataType::DATA_TYPE_INT) dur = std::stoi(dur_lit->value);
                    else if (dur_lit->value_type == DataType::DATA_TYPE_FLOAT) dur = std::stof(dur_lit->value);
                    else if (dur_lit->value_type == DataType::DATA_TYPE_DOUBLE) dur = std::stod(dur_lit->value);
                    if (dur <= 0) {
                        add_error(ErrorCode::ER_0003, node->re_wait_duration->location, "reѭ���ȴ�ʱ������Ϊ��������ǰ��" + std::to_string(dur) + "��");
                    }
                }
                // ���wait��λ
                if (GalltConstants::WAIT_SUPPORTED_UNITS.find(node->re_wait_unit) == GalltConstants::WAIT_SUPPORTED_UNITS.end()) {
                    add_error(
                        ErrorCode::ER_0003,
                        node->location,
                        "reѭ����֧�ֵĵȴ���λ��" + node->re_wait_unit + "��֧�֣�ms/s/min/hour��"
                    );
                }
            }

            // 3. ���ѭ����
            if (node->body) {
                visit(node->body.get());
            }
            break;
        }
        }
    }

    // ------------------------------ ԭ�нڵ��visit������������չ�� ------------------------------
    // ���ʸ��ڵ㣨ProgramNode����չȫ���¼���⣩
    void visit(ProgramNode* node) {
        // 1. �ȴ��������壨ȫ��������
        for (auto& func : node->functions) {
            if (func) {
                visit(func.get());
            }
        }

        // 2. ����main����
        for (auto& main : node->mains) {
            if (main) {
                visit(main.get());
            }
        }

        // 3. ����ȫ���¼����
        for (auto& detect : node->detect_events) {
            if (detect) {
                visit(detect.get());
            }
        }
    }

    // ���ʺ�������ڵ㣨FunctionDefinitionNode���޸�����������ͻ��
    void visit(FunctionDefinitionNode* node) {
        if (!node) return;
        // 1. ��麯��������ӵ�ȫ�ַ��ű�����ER 0006��
        Symbol::FuncInfo func_info;
        func_info.return_type = node->return_type;
        std::unordered_map<std::string, bool> param_name_check;

        for (const auto& param : node->parameters) {
            func_info.param_types.push_back(param.var_type);
            func_info.param_names.push_back(param.var_name);
            if (param_name_check.count(param.var_name)) {
                add_error(ErrorCode::ER_0004, param.location, "����" + node->function_name + "�������ظ���" + param.var_name);
            }
            param_name_check[param.var_name] = true;
        }

        Symbol func_symbol(node->function_name, node->location, func_info);
        SemanticError add_err(ErrorCode::NO_ERROR, node->location, "");
        if (!symbol_table.add_symbol(func_symbol, add_err)) {
            errors.push_back(add_err);
        }

        // 2. ���뺯����������Ӳ���
        symbol_table.push_scope();
        bool scope_pushed = true;
        try {
            for (const auto& param : node->parameters) {
                Symbol::VarInfo var_info;
                var_info.data_type = param.var_type;
                var_info.is_array = param.is_array;
                var_info.is_initialized = true;
                var_info.is_special_var = false;
                var_info.var_group = "";

                if (param.is_array && param.array_size) {
                    if (auto lit = dynamic_cast<LiteralExprNode*>(param.array_size.get())) {
                        try {
                            var_info.array_size = std::stoull(lit->value);
                            if (var_info.array_size == 0) {
                                add_error(ErrorCode::ER_2001, param.array_size->location, "�������������С����Ϊ0");
                            }
                        }
                        catch (...) {
                            var_info.array_size = 0;
                            add_error(ErrorCode::ER_2001, param.array_size->location, "�������������С������������");
                        }
                    }
                    else {
                        var_info.array_size = 0;
                        add_error(ErrorCode::ER_2001, param.array_size->location, "�������������С����������������");
                    }
                }
                else {
                    var_info.array_size = 0;
                }

                Symbol param_sym(param.var_name, param.location, var_info);
                SemanticError param_err(ErrorCode::NO_ERROR, param.location, "");
                if (!symbol_table.add_symbol(param_sym, param_err)) {
                    errors.push_back(param_err);
                }
            }

            // 3. ��麯����
            if (node->function_body) {
                visit(node->function_body.get());
            }
        }
        catch (...) {
            // ȷ���������˳�
            if (scope_pushed) {
                symbol_table.pop_scope();
                scope_pushed = false;
            }
        }
        if (scope_pushed) {
            symbol_table.pop_scope();
        }
    }

    // ����Main�ڵ㣨MainNode������ԭ�����������
    void visit(MainNode* node) {
        if (!node) return;
        main_count++;
        if (main_count > 1 && !last_main_released) {
            add_error(ErrorCode::ER_0001, node->location, "��main����δ�ͷ�ǰһ��main����ʹ��r.main��");
        }
        last_main_released = false;

        symbol_table.push_scope();
        bool scope_pushed = true;
        try {
            if (node->main_body) {
                visit(node->main_body.get());
            }
        }
        catch (...) {
            if (scope_pushed) {
                symbol_table.pop_scope();
                scope_pushed = false;
            }
        }
        if (scope_pushed) {
            symbol_table.pop_scope();
        }
    }

    // ���ʴ����ڵ㣨BlockNode������ԭ��������
    void visit(BlockNode* node) {
        if (!node) return;
        symbol_table.push_scope();
        bool scope_pushed = true;
        try {
            for (auto& stmt : node->statements) {
                if (stmt) {
                    visit(stmt.get());
                }
            }
        }
        catch (...) {
            if (scope_pushed) {
                symbol_table.pop_scope();
                scope_pushed = false;
            }
        }
        if (scope_pushed) {
            symbol_table.pop_scope();
        }
    }

    // ���ʱ��������ڵ㣨VariableDeclarationNode������ԭ���߼���
    void visit(VariableDeclarationNode* node) {
        if (!node) return;
        Symbol::VarInfo var_info;
        var_info.data_type = node->var_type;
        var_info.is_array = node->is_array;
        var_info.is_initialized = (node->initial_value != nullptr);
        var_info.is_special_var = false;
        var_info.var_group = "";

        if (node->is_array) {
            if (!node->array_size) {
                add_error(ErrorCode::ER_0003, node->location, "��������ȱ�ٴ�С");
                var_info.array_size = 0;
            }
            else {
                DataType size_type = get_expression_type(node->array_size.get());
                if (size_type != DataType::DATA_TYPE_INT) {
                    add_error(ErrorCode::ER_1001, node->array_size->location, "�����С������int����");
                    var_info.array_size = 0;
                }
                else if (auto lit = dynamic_cast<LiteralExprNode*>(node->array_size.get())) {
                    try {
                        size_t size = std::stoull(lit->value);
                        if (size == 0) {
                            add_error(ErrorCode::ER_2001, node->array_size->location, "�����С����Ϊ0");
                        }
                        var_info.array_size = size;
                    }
                    catch (...) {
                        add_error(ErrorCode::ER_2001, node->array_size->location, "�����С������������");
                        var_info.array_size = 0;
                    }
                }
                else {
                    add_error(ErrorCode::ER_2001, node->array_size->location, "�����С����������������");
                    var_info.array_size = 0;
                }
            }
        }
        else {
            var_info.array_size = 0;
        }

        if (node->initial_value) {
            DataType init_type = get_expression_type(node->initial_value.get());
            check_type_compatibility(node->var_type, init_type, node->initial_value->location, "����" + node->var_name + "��ʼ��");
        }

        Symbol var_symbol(node->var_name, node->location, var_info);
        SemanticError add_err(ErrorCode::NO_ERROR, node->location, "");
        if (!symbol_table.add_symbol(var_symbol, add_err)) {
            errors.push_back(add_err);
        }
    }

    // ���ʸ�ֵ�ڵ㣨AssignmentNode��֧�����������ֵ��
    void visit(AssignmentNode* node) {
        if (!node || !node->target || !node->value) return;
        DataType target_type;
        std::string target_name;

        // 1. ��鸳ֵĿ�꣨��ͨ����/����/���������
        if (auto id = dynamic_cast<IdentifierExprNode*>(node->target.get())) {
            target_name = id->name;
            if (!check_identifier_declared(target_name, id->location)) return;

            const Symbol* var_sym = symbol_table.find_symbol(target_name);
            if (var_sym->type != Symbol::SymbolType::VARIABLE || !var_sym->var_info.has_value()) {
                add_error(ErrorCode::ER_0003, id->location, target_name + "���Ǳ��������ܸ�ֵ");
                return;
            }
            target_type = var_sym->var_info.value().data_type;
        }
        else if (auto arr_access = dynamic_cast<ArrayAccessExprNode*>(node->target.get())) {
            target_name = arr_access->array_name->name;
            if (!check_identifier_declared(target_name, arr_access->array_name->location)) return;

            const Symbol* arr_sym = symbol_table.find_symbol(target_name);
            if (arr_sym->type != Symbol::SymbolType::VARIABLE || !arr_sym->var_info.has_value() || !arr_sym->var_info.value().is_array) {
                add_error(ErrorCode::ER_0003, arr_access->array_name->location, target_name + "�������飬����ͨ��������ֵ");
                return;
            }
            target_type = arr_sym->var_info.value().data_type;

            // �����������
            DataType index_type = get_expression_type(arr_access->index.get());
            if (index_type != DataType::DATA_TYPE_INT && index_type != DataType::DATA_TYPE_UNKNOWN) {
                add_error(ErrorCode::ER_1001, arr_access->index->location, "��������������int����");
            }
        }
        else {
            add_error(ErrorCode::ER_0003, node->target->location, "��ֵĿ������Ǳ���������Ԫ��");
            return;
        }

        // 2. ��鸳ֵ���ʽ����
        DataType value_type = get_expression_type(node->value.get());
        check_type_compatibility(target_type, value_type, node->value->location, "��ֵ��" + target_name);
    }

    // ��������ڵ㣨OutputNode��֧��Count���ʽ�����
    void visit(OutputNode* node) {
        if (!node || !node->expr) return;
        // �������ʽ�Ϸ��ԣ�����ǿ�����ͣ��ַ���/��ֵ���������
        get_expression_type(node->expr.get());
    }

    // ��������ڵ㣨InputNode������ԭ���߼���
    void visit(InputNode* node) {
        if (!node || !node->target) return;
        if (!check_identifier_declared(node->target->name, node->target->location)) return;

        const Symbol* var_sym = symbol_table.find_symbol(node->target->name);
        if (var_sym->type != Symbol::SymbolType::VARIABLE || !var_sym->var_info.has_value()) {
            add_error(ErrorCode::ER_0003, node->target->location, node->target->name + "���Ǳ��������ܽ�������");
            return;
        }

        if (node->prompt) {
            DataType prompt_type = get_expression_type(node->prompt.get());
            if (prompt_type != DataType::DATA_TYPE_STRING && prompt_type != DataType::DATA_TYPE_UNKNOWN) {
                add_error(ErrorCode::ER_1001, node->prompt->location, "������ʾ������string����");
            }
        }
    }

    // ���������ڵ㣨IfNode������ԭ���߼���
    void visit(IfNode* node) {
        if (!node || !node->condition || !node->then_block) return;
        if (!check_numeric_expression(node->condition.get(), node->condition->location, "If����")) {
            return;
        }

        visit(node->then_block.get());
        if (node->else_block) {
            visit(node->else_block.get());
        }
    }

    // �����߳̽ڵ㣨ThreadNode������ԭ���߼���
    void visit(ThreadNode* node) {
        if (!node || !node->thread_body) return;
        visit(node->thread_body.get());
    }

    // ���ʷ��ؽڵ㣨ReturnNode������ԭ���߼���
    void visit(ReturnNode* node) {
        if (!node) return;
        if (!node->return_value) return;

        // ��鷵��ֵ���ͣ����Ϻ����������ͣ��˴���Ϊ��ֵ/�ַ������ɣ�
        get_expression_type(node->return_value.get());
    }

    // �����ͷ�main�ڵ㣨ReleaseMainNode������ԭ���߼���
    void visit(ReleaseMainNode* node) {
        if (!node) return;
        last_main_released = true;
    }

    // �����˳��ڵ㣨ExitNode���������飩
    void visit(ExitNode* node) {}

    // �������������ʽ��LiteralExprNode���������飩
    void visit(LiteralExprNode* node) {}

    // ���ʱ�ʶ�����ʽ��IdentifierExprNode�����������
    void visit(IdentifierExprNode* node) {
        if (!node) return;
        check_identifier_declared(node->name, node->location);
    }

    // ���ʶ�Ԫ������ʽ��BinaryOpExprNode�������������ͣ�
    void visit(BinaryOpExprNode* node) {
        if (!node || !node->left || !node->right) return;
        visit(node->left.get());
        visit(node->right.get());

        DataType left_type = get_expression_type(node->left.get());
        DataType right_type = get_expression_type(node->right.get());

        // �ַ�����֧��ƴ�ӣ�+��
        if ((left_type == DataType::DATA_TYPE_STRING || right_type == DataType::DATA_TYPE_STRING)) {
            if (node->op != TokenType::TOKEN_PLUS) {
                add_error(
                    ErrorCode::ER_1001,
                    node->location,
                    "�ַ�����֧��+���㣨ƴ�ӣ�����ǰ�������" + token_type_to_string(node->op)
                );
            }
            return;
        }

        // ��ֵ����֧��+��-��*��/��<��>��=
        if (!check_numeric_expression(node->left.get(), node->left->location, "��Ԫ�����������")) return;
        if (!check_numeric_expression(node->right.get(), node->right->location, "��Ԫ�����Ҳ�����")) return;
    }

    // ����������ʱ��ʽ��ArrayAccessExprNode�����������ں��������ͣ�
    void visit(ArrayAccessExprNode* node) {
        if (!node || !node->array_name || !node->index) return;
        visit(node->array_name.get()); // ����������Ƿ�����
        visit(node->index.get());      // ����������ʽ

        const Symbol* arr_sym = symbol_table.find_symbol(node->array_name->name);
        if (arr_sym && arr_sym->type == Symbol::SymbolType::VARIABLE && arr_sym->var_info.has_value() && arr_sym->var_info.value().is_array) {
            DataType index_type = get_expression_type(node->index.get());
            if (index_type != DataType::DATA_TYPE_INT && index_type != DataType::DATA_TYPE_UNKNOWN) {
                add_error(
                    ErrorCode::ER_1001,
                    node->index->location,
                    "����" + node->array_name->name + "������������int���ͣ�ʵ��Ϊ" + CommonUtils::data_type_to_string(index_type)
                );
            }
        }
    }

    // ���ʺ������ñ��ʽ��FunctionCallExprNode������ԭ���߼���
    void visit(FunctionCallExprNode* node) {
        if (!node) return;
        if (!check_identifier_declared(node->function_name, node->location)) return;

        const Symbol* func_sym = symbol_table.find_symbol(node->function_name);
        if (func_sym->type != Symbol::SymbolType::FUNCTION || !func_sym->func_info.has_value()) {
            add_error(ErrorCode::ER_0003, node->location, node->function_name + "���Ǻ���");
            return;
        }

        const auto& param_types = func_sym->func_info.value().param_types;
        if (node->arguments.size() != param_types.size()) {
            add_error(
                ErrorCode::ER_1001,
                node->location,
                "����" + node->function_name + "����������ƥ�䣨Ԥ�ڣ�" + std::to_string(param_types.size()) + "��ʵ�ʣ�" + std::to_string(node->arguments.size()) + "��"
            );
            return;
        }

        for (size_t i = 0; i < node->arguments.size(); ++i) {
            if (!node->arguments[i]) continue;
            visit(node->arguments[i].get());
            DataType arg_type = get_expression_type(node->arguments[i].get());
            check_type_compatibility(
                arg_type, param_types[i],
                node->arguments[i]->location,
                "����" + node->function_name + "�Ĳ���" + std::to_string(i + 1)
            );
        }
    }

    // ���ʦб��ʽ��PiExprNode����龫��Ϊ��������
    void visit(PiExprNode* node) {
        if (!node) return;
        if (node->precision) {
            visit(node->precision.get());
            if (!check_numeric_expression(node->precision.get(), node->precision->location, "�м��㾫��")) return;

            if (auto prec_lit = dynamic_cast<LiteralExprNode*>(node->precision.get())) {
                if (prec_lit->value_type != DataType::DATA_TYPE_INT) {
                    add_error(ErrorCode::ER_1001, node->precision->location, "�м��㾫�ȱ�����int���ͣ���ǰ��" + CommonUtils::data_type_to_string(prec_lit->value_type) + "��");
                }
                else {
                    int prec = std::stoi(prec_lit->value);
                    if (prec <= 0) {
                        add_error(ErrorCode::ER_0003, node->precision->location, "�м��㾫�ȱ���Ϊ����������ǰ��" + std::to_string(prec) + "��");
                    }
                }
            }
        }
    }

    // ͨ�ñ��ʽ�ڵ���ʣ����ף�
    void visit(ExpressionNode* node) {
        if (!node) return;
        if (auto lit = dynamic_cast<LiteralExprNode*>(node)) visit(lit);
        else if (auto id = dynamic_cast<IdentifierExprNode*>(node)) visit(id);
        else if (auto bin_op = dynamic_cast<BinaryOpExprNode*>(node)) visit(bin_op);
        else if (auto arr_access = dynamic_cast<ArrayAccessExprNode*>(node)) visit(arr_access);
        else if (auto func_call = dynamic_cast<FunctionCallExprNode*>(node)) visit(func_call);
        else if (auto pi_expr = dynamic_cast<PiExprNode*>(node)) visit(pi_expr);
        else if (auto count_expr = dynamic_cast<CountExprNode*>(node)) visit(count_expr);
        else if (auto event_cond = dynamic_cast<EventConditionExprNode*>(node)) visit(event_cond);
        else {
            add_error(ErrorCode::ER_0003, node->location, "��֧�ֵı��ʽ����");
        }
    }

    // ͨ�����ڵ���ʣ����ף�
    void visit(StatementNode* node) {
        if (!node) return;
        if (auto var_decl = dynamic_cast<VariableDeclarationNode*>(node)) visit(var_decl);
        else if (auto assign = dynamic_cast<AssignmentNode*>(node)) visit(assign);
        else if (auto output = dynamic_cast<OutputNode*>(node)) visit(output);
        else if (auto input = dynamic_cast<InputNode*>(node)) visit(input);
        else if (auto if_node = dynamic_cast<IfNode*>(node)) visit(if_node);
        else if (auto loop = dynamic_cast<LoopNode*>(node)) visit(loop);
        else if (auto wait = dynamic_cast<WaitNode*>(node)) visit(wait);
        else if (auto thread = dynamic_cast<ThreadNode*>(node)) visit(thread);
        else if (auto ret = dynamic_cast<ReturnNode*>(node)) visit(ret);
        else if (auto release_main = dynamic_cast<ReleaseMainNode*>(node)) visit(release_main);
        else if (auto exit_node = dynamic_cast<ExitNode*>(node)) visit(exit_node);
        else if (auto block = dynamic_cast<BlockNode*>(node)) visit(block);
        else if (auto var_node = dynamic_cast<VarNode*>(node)) visit(var_node);
        else if (auto detect = dynamic_cast<DetectNode*>(node)) visit(detect);
        else {
            add_error(ErrorCode::ER_0003, node->location, "��֧�ֵ��������");
        }
    }

    // ����������TokenTypeת�ַ��������ڴ�����Ϣ��
    std::string token_type_to_string(TokenType type) {
        switch (type) {
        case TokenType::TOKEN_PLUS: return "+";
        case TokenType::TOKEN_MINUS: return "-";
        case TokenType::TOKEN_MUL: return "*";
        case TokenType::TOKEN_DIV: return "/";
        case TokenType::TOKEN_EQUALS: return "=";
        case TokenType::TOKEN_LT: return "<";
        case TokenType::TOKEN_GT: return ">";
        default: return "δ֪�����";
        }
    }

public:
    SemanticAnalyzer() = default;

    // ִ�������������ڷ�����
    void analyze(ProgramNode* program) {
        if (program) {
            visit(program);
        }
    }

    // ��ȡ��������б�
    const std::vector<SemanticError>& get_errors() const {
        return errors;
    }
};

#endif // SEMANTIC_ANALYZER_H