//Copyright (c) Gallt Developer.
//版权所有（c）Gallt 开发者。

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

// 符号表条目：记录变量/函数的语义信息（扩展支持特殊变量）
struct Symbol {
    enum class SymbolType { VARIABLE, FUNCTION } type; // 符号类型（变量/函数）
    std::string name;                                 // 符号名称（普通变量：变量名；特殊变量：组名.varId）
    SourceLocation declaration_loc;                   // 声明位置（用于报错）

    // 变量特有信息（扩展特殊变量标记）
    struct VarInfo {
        DataType data_type;      // 数据类型
        bool is_array;           // 是否为数组
        size_t array_size;       // 数组大小（仅当is_array为true）
        bool is_initialized;     // 是否已初始化
        bool is_special_var;     // 是否为特殊变量（var.name下的var1/var2）
        std::string var_group;   // 特殊变量所属组名（如"Peter"，仅is_special_var为true时有效）
    };

    // 函数特有信息（保持原有）
    struct FuncInfo {
        DataType return_type;                          // 返回类型
        std::vector<DataType> param_types;             // 参数类型列表
        std::vector<std::string> param_names;          // 参数名称列表（用于检查重复）
    };

    // 用std::optional替代union，支持非POD类型
    std::optional<VarInfo> var_info;   // 变量信息（仅当type为VARIABLE时有效）
    std::optional<FuncInfo> func_info; // 函数信息（仅当type为FUNCTION时有效）

    // 默认构造函数
    Symbol() : type(SymbolType::VARIABLE), name(), declaration_loc(), var_info(), func_info() {}

    // 构造函数（普通变量）
    Symbol(const std::string& n, const SourceLocation& loc, const VarInfo& vi)
        : type(SymbolType::VARIABLE), name(n), declaration_loc(loc), var_info(vi) {
    }

    // 构造函数（特殊变量，带组名）
    Symbol(const std::string& group_name, const std::string& var_id, const SourceLocation& loc, const VarInfo& vi)
        : type(SymbolType::VARIABLE), name(group_name + "." + var_id), declaration_loc(loc), var_info(vi) {
    }

    // 构造函数（函数）
    Symbol(const std::string& n, const SourceLocation& loc, const FuncInfo& fi)
        : type(SymbolType::FUNCTION), name(n), declaration_loc(loc), func_info(fi) {
    }
};

// 语义分析错误结构（保持原有）
struct SemanticError {
    ErrorCode code;
    SourceLocation location;
    std::string message;
    SemanticError(ErrorCode c, const SourceLocation& loc, const std::string& msg)
        : code(c), location(loc), message(msg) {
    }
};

// 符号表管理类（扩展函数命名冲突检测）
class SymbolTable {
private:
    std::vector<std::unordered_map<std::string, Symbol>> scopes; // 作用域栈（vector实现）

public:
    SymbolTable() {
        push_scope(); // 初始全局作用域
    }

    // 进入新作用域
    void push_scope() {
        scopes.emplace_back();
    }

    // 退出当前作用域
    void pop_scope() {
        if (!scopes.empty()) {
            scopes.pop_back();
        }
    }

    // 查找符号（从当前作用域向上搜索）
    const Symbol* find_symbol(const std::string& name) const {
        for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
            auto found = it->find(name);
            if (found != it->end()) {
                return &(found->second);
            }
        }
        return nullptr;
    }

    // 扩展：添加符号（区分函数命名冲突ER 0006和普通符号冲突ER 0003）
    bool add_symbol(const Symbol& symbol, SemanticError& error) {
        if (scopes.back().count(symbol.name)) {
            const Symbol& existing = scopes.back()[symbol.name];
            // 函数命名冲突：当前符号和已有符号均为函数
            if (symbol.type == Symbol::SymbolType::FUNCTION && existing.type == Symbol::SymbolType::FUNCTION) {
                error = SemanticError(
                    ErrorCode::ER_0006, // 函数命名冲突专属错误码
                    symbol.declaration_loc,
                    "函数命名冲突：" + symbol.name + "（已在" +
                    existing.declaration_loc.to_string() + "声明）"
                );
                return false;
            }
            // 普通符号冲突（变量-变量、变量-函数）
            error = SemanticError(
                ErrorCode::ER_0003,
                symbol.declaration_loc,
                "符号重复声明: " + symbol.name + "（已在" +
                existing.declaration_loc.to_string() + "声明）"
            );
            return false;
        }
        scopes.back()[symbol.name] = symbol;
        return true;
    }
};

// 语义分析器类（扩展新增节点的语义校验）
class SemanticAnalyzer {
private:
    SymbolTable symbol_table;          // 符号表
    std::vector<SemanticError> errors; // 错误列表
    int main_count = 0;                // main计数（用于ER 0001）
    bool last_main_released = true;    // 上一个main是否释放
    std::unordered_map<std::string, std::vector<std::string>> special_var_groups; // 特殊变量组记录（组名→varId列表）

    // 添加错误到列表
    void add_error(ErrorCode code, const SourceLocation& loc, const std::string& msg) {
        errors.emplace_back(code, loc, msg);
    }

    // 检查标识符是否已声明
    bool check_identifier_declared(const std::string& name, const SourceLocation& loc) {
        if (!symbol_table.find_symbol(name)) {
            add_error(ErrorCode::ER_0003, loc, "未定义的标识符: " + name);
            return false;
        }
        return true;
    }

    // 检查类型是否兼容（扩展数值类型自动提升）
    bool check_type_compatibility(DataType actual, DataType expected, const SourceLocation& loc, const std::string& context) {
        if (actual == DataType::DATA_TYPE_UNKNOWN || expected == DataType::DATA_TYPE_UNKNOWN) {
            return true; // 未知类型暂不报错
        }
        // 完全匹配
        if (actual == expected) {
            return true;
        }
        // 数值类型兼容（int→float→double）
        if ((actual == DataType::DATA_TYPE_INT && (expected == DataType::DATA_TYPE_FLOAT || expected == DataType::DATA_TYPE_DOUBLE)) ||
            (actual == DataType::DATA_TYPE_FLOAT && expected == DataType::DATA_TYPE_DOUBLE)) {
            return true;
        }
        // 不兼容
        add_error(
            ErrorCode::ER_1001,
            loc,
            context + "类型不匹配: 预期" + CommonUtils::data_type_to_string(expected) +
            "，实际" + CommonUtils::data_type_to_string(actual)
        );
        return false;
    }

    // 检查表达式是否为数值类型（int/float/double）
    bool check_numeric_expression(ExpressionNode* expr, const SourceLocation& loc, const std::string& context) {
        if (!expr) {
            add_error(ErrorCode::ER_0003, loc, context + "不能为空");
            return false;
        }
        DataType expr_type = get_expression_type(expr);
        if (expr_type == DataType::DATA_TYPE_INT || expr_type == DataType::DATA_TYPE_FLOAT || expr_type == DataType::DATA_TYPE_DOUBLE) {
            return true;
        }
        add_error(
            ErrorCode::ER_1001,
            loc,
            context + "必须是数值类型（int/float/double），实际为" + CommonUtils::data_type_to_string(expr_type)
        );
        return false;
    }

    // 解析表达式类型（扩展支持CountExprNode、EventConditionExprNode）
    DataType get_expression_type(ExpressionNode* expr) {
        if (!expr) return DataType::DATA_TYPE_UNKNOWN;

        if (auto lit = dynamic_cast<LiteralExprNode*>(expr)) {
            return lit->value_type;
        }
        else if (auto id = dynamic_cast<IdentifierExprNode*>(expr)) {
            // 检查是否为特殊变量（如Peter.var1）
            const Symbol* sym = symbol_table.find_symbol(id->name);
            if (sym && sym->type == Symbol::SymbolType::VARIABLE && sym->var_info.has_value()) {
                return sym->var_info.value().data_type;
            }
            return DataType::DATA_TYPE_UNKNOWN;
        }
        else if (auto bin_op = dynamic_cast<BinaryOpExprNode*>(expr)) {
            DataType left_type = get_expression_type(bin_op->left.get());
            DataType right_type = get_expression_type(bin_op->right.get());
            // 字符串拼接（仅当两侧均为字符串）
            if (left_type == DataType::DATA_TYPE_STRING && right_type == DataType::DATA_TYPE_STRING) {
                return DataType::DATA_TYPE_STRING;
            }
            // 数值运算（取更宽类型）
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
                // 检查索引是否为int
                DataType index_type = get_expression_type(arr_access->index.get());
                if (index_type != DataType::DATA_TYPE_INT && index_type != DataType::DATA_TYPE_UNKNOWN) {
                    add_error(
                        ErrorCode::ER_1001,
                        arr_access->index->location,
                        "数组索引必须是int类型，实际为" + CommonUtils::data_type_to_string(index_type)
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
            return DataType::DATA_TYPE_DOUBLE; // π默认返回double
        }
        // 新增：处理Count表达式类型
        else if (auto count_expr = dynamic_cast<CountExprNode*>(expr)) {
            return count_expr->get_type(); // 直接使用CountExprNode预定义的返回类型
        }
        // 新增：处理事件条件表达式（返回事件触发值类型）
        else if (auto event_cond = dynamic_cast<EventConditionExprNode*>(expr)) {
            return get_expression_type(event_cond->trigger_val.get());
        }
        return DataType::DATA_TYPE_UNKNOWN;
    }

    // ------------------------------ 新增节点的visit方法 ------------------------------
    // 新增：访问特殊变量声明节点（VarNode）
    void visit(VarNode* node) {
        // 1. 检查变量组名是否重复
        if (special_var_groups.count(node->var_group_name)) {
            add_error(
                ErrorCode::ER_0003,
                node->location,
                "特殊变量组名重复：" + node->var_group_name + "（已存在同名变量组）"
            );
            return;
        }
        special_var_groups[node->var_group_name] = {}; // 初始化组内varId列表

        // 2. 处理每个VarItem
        for (const auto& item : node->var_items) {
            SourceLocation item_loc = node->location; // 简化：实际应取item的位置（可扩展VarItem添加location）
            std::string full_var_name = node->var_group_name + "." + item.var_id;

            // 2.1 检查varId是否在组内重复
            auto& group_var_ids = special_var_groups[node->var_group_name];
            if (std::find(group_var_ids.begin(), group_var_ids.end(), item.var_id) != group_var_ids.end()) {
                add_error(
                    ErrorCode::ER_0003,
                    item_loc,
                    "变量组" + node->var_group_name + "内varId重复：" + item.var_id
                );
                continue;
            }
            group_var_ids.push_back(item.var_id);

            // 2.2 检查初始值类型是否匹配
            if (!item.var_value) {
                add_error(ErrorCode::ER_0003, item_loc, "特殊变量" + full_var_name + "必须初始化");
                continue;
            }
            DataType init_type = get_expression_type(item.var_value.get());
            if (!check_type_compatibility(init_type, item.var_type, item_loc, "特殊变量" + full_var_name + "初始化")) {
                continue;
            }

            // 2.3 注册特殊变量到符号表
            Symbol::VarInfo var_info;
            var_info.data_type = item.var_type;
            var_info.is_array = false; // 特殊变量暂不支持数组
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

    // 新增：访问事件检测节点（DetectNode）
    void visit(DetectNode* node) {
        if (!node->event_condition) {
            add_error(ErrorCode::ER_0003, node->location, "事件检测缺少条件（如detect keyboard.click=\"k\"）");
            return;
        }
        // 1. 检查事件类型是否支持
        std::string event_type = node->event_condition->event_type;
        if (!CommonUtils::is_supported_event(event_type)) {
            add_error(
                ErrorCode::ER_0003,
                node->event_condition->location,
                "不支持的事件类型：" + event_type + "（支持：input/click/enter/keyboard.click）"
            );
            return;
        }

        // 2. 检查触发值类型（根据事件类型约定）
        if (!node->event_condition->trigger_val) {
            add_error(ErrorCode::ER_0003, node->event_condition->location, event_type + "事件缺少触发值（如\"k\"）");
            return;
        }
        DataType trigger_type = get_expression_type(node->event_condition->trigger_val.get());
        bool trigger_type_valid = false;
        if (event_type == "keyboard.click" || event_type == "input" || event_type == "enter") {
            // 键盘/输入事件触发值必须是字符串（如"k"）
            trigger_type_valid = (trigger_type == DataType::DATA_TYPE_STRING);
        }
        else if (event_type == "click") {
            // 点击事件触发值可选（如坐标，暂支持int）
            trigger_type_valid = (trigger_type == DataType::DATA_TYPE_INT || trigger_type == DataType::DATA_TYPE_UNKNOWN);
        }
        if (!trigger_type_valid) {
            add_error(
                ErrorCode::ER_1001,
                node->event_condition->trigger_val->location,
                event_type + "事件的触发值必须是" + (event_type == "click" ? "int" : "string") + "类型，实际为" + CommonUtils::data_type_to_string(trigger_type)
            );
        }

        // 3. 检查run块内语句语义
        if (node->run_block) {
            visit(node->run_block.get());
        }
    }

    // 新增：访问Count数值计算表达式（CountExprNode）
    void visit(CountExprNode* node) {
        // 1. 检查待计算表达式是否为数值类型
        if (!node->calc_expr) {
            add_error(ErrorCode::ER_0003, node->location, "Count表达式缺少待计算内容（如count(5*8)）");
            return;
        }
        if (!check_numeric_expression(node->calc_expr.get(), node->calc_expr->location, "Count表达式的计算内容")) {
            return;
        }

        // 2. 验证Count类型对应的计算逻辑（可选：如COUNT_NORMAL仅支持int计算）
        DataType calc_type = get_expression_type(node->calc_expr.get());
        if (node->count_type == CountExprNode::CountType::COUNT_NORMAL && calc_type != DataType::DATA_TYPE_INT) {
            add_error(
                ErrorCode::ER_1001,
                node->location,
                "普通count(...)仅支持int类型计算，实际为" + CommonUtils::data_type_to_string(calc_type) + "（需用float.count/double.count）"
            );
        }
    }

    // ------------------------------ 扩展现有节点的visit方法 ------------------------------
    // 扩展：访问等待节点（WaitNode，支持多单位校验）
    void visit(WaitNode* node) {
        if (!node->duration) {
            add_error(ErrorCode::ER_0003, node->location, "Wait语句缺少时长（如wait time=3000ms）");
            return;
        }
        // 1. 检查等待时长是否为正数值
        if (!check_numeric_expression(node->duration.get(), node->duration->location, "Wait时长")) {
            return;
        }
        // 2. 检查时长是否为正数（仅字面量可静态检查）
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
                add_error(ErrorCode::ER_0003, node->duration->location, "Wait时长必须为正数（当前：" + std::to_string(dur) + "）");
            }
        }

        // 3. 检查单位是否合法（通过CommonUtils验证）
        if (GalltConstants::WAIT_SUPPORTED_UNITS.find(node->unit) == GalltConstants::WAIT_SUPPORTED_UNITS.end()) {
            add_error(
                ErrorCode::ER_0003,
                node->location,
                "不支持的Wait单位：" + node->unit + "（支持：ms/s/min/hour）"
            );
        }
    }

    // 扩展：访问循环节点（LoopNode，支持For和re wait校验）- 修复控制流跳过问题
    void visit(LoopNode* node) {
        switch (node->loop_type) {
        case LoopNode::LoopType::LOOP_WHILE: {
            // 原有逻辑：检查条件为数值类型
            if (!node->condition) {
                add_error(ErrorCode::ER_0003, node->location, "While循环缺少条件（如while(a < 10)）");
                break;
            }
            if (!check_numeric_expression(node->condition.get(), node->condition->location, "While循环条件")) {
                break;
            }
            // 检查循环体
            if (node->body) {
                visit(node->body.get());
            }
            break;
        }

        case LoopNode::LoopType::LOOP_FOR: {
            // 1. 检查For循环必要字段（避免空指针）
            if (node->for_loop_var.empty()) {
                add_error(ErrorCode::ER_0003, node->location, "For循环缺少变量名（如for i 1 to 50）");
                break;
            }
            if (!node->for_start) {
                add_error(ErrorCode::ER_0003, node->location, "For循环缺少起始值（如for i 1 to 50）");
                break;
            }
            if (!node->for_end) {
                add_error(ErrorCode::ER_0003, node->location, "For循环缺少结束值（如for i 1 to 50）");
                break;
            }
            // 步长为空时初始化默认值（修复空指针）
            if (!node->for_step) {
                node->for_step = std::make_unique<LiteralExprNode>(node->location, DataType::DATA_TYPE_INT, "1");
            }

            // 2. 检查循环变量是否重复声明（使用try-catch确保作用域退出）
            symbol_table.push_scope();
            bool scope_pushed = true;
            try {
                // 2.1 临时注册循环变量到当前作用域
                Symbol::VarInfo var_info;
                var_info.data_type = DataType::DATA_TYPE_INT; // 循环变量默认为int
                var_info.is_array = false;
                var_info.array_size = 0;
                var_info.is_initialized = true;
                var_info.is_special_var = false;
                var_info.var_group = "";
                Symbol loop_var_sym(node->for_loop_var, node->location, var_info);
                SemanticError add_err(ErrorCode::NO_ERROR, node->location, "");
                if (!symbol_table.add_symbol(loop_var_sym, add_err)) {
                    errors.push_back(add_err);
                    // 即使变量重复，仍继续检查其他字段（避免控制流中断）
                }

                // 2.2 检查起始值、结束值、步长为数值类型
                if (!check_numeric_expression(node->for_start.get(), node->for_start->location, "For循环起始值")) {
                    throw std::runtime_error("For起始值错误"); // 触发catch，确保pop_scope
                }
                if (!check_numeric_expression(node->for_end.get(), node->for_end->location, "For循环结束值")) {
                    throw std::runtime_error("For结束值错误");
                }
                if (!check_numeric_expression(node->for_step.get(), node->for_step->location, "For循环步长")) {
                    throw std::runtime_error("For步长错误");
                }

                // 2.3 检查步长为正整数（避免死循环）
                if (auto step_lit = dynamic_cast<LiteralExprNode*>(node->for_step.get())) {
                    if (step_lit->value_type != DataType::DATA_TYPE_INT) {
                        add_error(ErrorCode::ER_1001, node->for_step->location, "For循环步长必须是int类型（避免非整数步长导致死循环）");
                        throw std::runtime_error("For步长类型错误");
                    }
                    int step = std::stoi(step_lit->value);
                    if (step <= 0) {
                        add_error(ErrorCode::ER_0003, node->for_step->location, "For循环步长必须为正整数（当前：" + std::to_string(step) + "）");
                        throw std::runtime_error("For步长值错误");
                    }
                }

                // 2.4 检查循环体
                if (node->body) {
                    visit(node->body.get());
                }
            }
            catch (...) {
                // 无论是否报错，都确保退出作用域（修复控制流跳过）
                if (scope_pushed) {
                    symbol_table.pop_scope();
                    scope_pushed = false;
                }
            }
            // 正常流程退出作用域
            if (scope_pushed) {
                symbol_table.pop_scope();
            }
            break;
        }

        case LoopNode::LoopType::LOOP_RE: {
            // 1. 非无限循环：检查次数为正整数
            if (!node->is_infinite) {
                if (!node->condition) {
                    add_error(ErrorCode::ER_0003, node->location, "非无限re循环缺少次数（如re number=3）");
                    break;
                }
                if (!check_numeric_expression(node->condition.get(), node->condition->location, "re循环次数")) {
                    break;
                }
                if (auto count_lit = dynamic_cast<LiteralExprNode*>(node->condition.get())) {
                    if (count_lit->value_type != DataType::DATA_TYPE_INT) {
                        add_error(ErrorCode::ER_1001, node->condition->location, "re循环次数必须是int类型（当前：" + CommonUtils::data_type_to_string(count_lit->value_type) + "）");
                        break;
                    }
                    int count = std::stoi(count_lit->value);
                    if (count <= 0) {
                        add_error(ErrorCode::ER_0005, node->condition->location, "re循环次数必须为正整数（当前：" + std::to_string(count) + "）");
                    }
                    else if (count > GalltConstants::MAX_RE_NORMAL_COUNT) {
                        add_error(ErrorCode::ER_0005, node->condition->location, "re循环次数超出上限（" + std::to_string(count) + " > 65536）");
                    }
                }
            }

            // 2. 带wait的re循环：检查wait时长和单位
            if (node->re_wait_duration) {
                // 检查wait时长
                if (!check_numeric_expression(node->re_wait_duration.get(), node->re_wait_duration->location, "re循环等待时长")) {
                    break;
                }
                if (auto dur_lit = dynamic_cast<LiteralExprNode*>(node->re_wait_duration.get())) {
                    double dur = 0.0;
                    if (dur_lit->value_type == DataType::DATA_TYPE_INT) dur = std::stoi(dur_lit->value);
                    else if (dur_lit->value_type == DataType::DATA_TYPE_FLOAT) dur = std::stof(dur_lit->value);
                    else if (dur_lit->value_type == DataType::DATA_TYPE_DOUBLE) dur = std::stod(dur_lit->value);
                    if (dur <= 0) {
                        add_error(ErrorCode::ER_0003, node->re_wait_duration->location, "re循环等待时长必须为正数（当前：" + std::to_string(dur) + "）");
                    }
                }
                // 检查wait单位
                if (GalltConstants::WAIT_SUPPORTED_UNITS.find(node->re_wait_unit) == GalltConstants::WAIT_SUPPORTED_UNITS.end()) {
                    add_error(
                        ErrorCode::ER_0003,
                        node->location,
                        "re循环不支持的等待单位：" + node->re_wait_unit + "（支持：ms/s/min/hour）"
                    );
                }
            }

            // 3. 检查循环体
            if (node->body) {
                visit(node->body.get());
            }
            break;
        }
        }
    }

    // ------------------------------ 原有节点的visit方法（兼容扩展） ------------------------------
    // 访问根节点（ProgramNode，扩展全局事件检测）
    void visit(ProgramNode* node) {
        // 1. 先处理函数定义（全局作用域）
        for (auto& func : node->functions) {
            if (func) {
                visit(func.get());
            }
        }

        // 2. 处理main程序
        for (auto& main : node->mains) {
            if (main) {
                visit(main.get());
            }
        }

        // 3. 处理全局事件检测
        for (auto& detect : node->detect_events) {
            if (detect) {
                visit(detect.get());
            }
        }
    }

    // 访问函数定义节点（FunctionDefinitionNode，修复函数命名冲突）
    void visit(FunctionDefinitionNode* node) {
        if (!node) return;
        // 1. 检查函数名并添加到全局符号表（触发ER 0006）
        Symbol::FuncInfo func_info;
        func_info.return_type = node->return_type;
        std::unordered_map<std::string, bool> param_name_check;

        for (const auto& param : node->parameters) {
            func_info.param_types.push_back(param.var_type);
            func_info.param_names.push_back(param.var_name);
            if (param_name_check.count(param.var_name)) {
                add_error(ErrorCode::ER_0004, param.location, "函数" + node->function_name + "参数名重复：" + param.var_name);
            }
            param_name_check[param.var_name] = true;
        }

        Symbol func_symbol(node->function_name, node->location, func_info);
        SemanticError add_err(ErrorCode::NO_ERROR, node->location, "");
        if (!symbol_table.add_symbol(func_symbol, add_err)) {
            errors.push_back(add_err);
        }

        // 2. 进入函数作用域，添加参数
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
                                add_error(ErrorCode::ER_2001, param.array_size->location, "函数参数数组大小不能为0");
                            }
                        }
                        catch (...) {
                            var_info.array_size = 0;
                            add_error(ErrorCode::ER_2001, param.array_size->location, "函数参数数组大小必须是正整数");
                        }
                    }
                    else {
                        var_info.array_size = 0;
                        add_error(ErrorCode::ER_2001, param.array_size->location, "函数参数数组大小必须是字面量整数");
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

            // 3. 检查函数体
            if (node->function_body) {
                visit(node->function_body.get());
            }
        }
        catch (...) {
            // 确保作用域退出
            if (scope_pushed) {
                symbol_table.pop_scope();
                scope_pushed = false;
            }
        }
        if (scope_pushed) {
            symbol_table.pop_scope();
        }
    }

    // 访问Main节点（MainNode，保持原有作用域管理）
    void visit(MainNode* node) {
        if (!node) return;
        main_count++;
        if (main_count > 1 && !last_main_released) {
            add_error(ErrorCode::ER_0001, node->location, "多main程序未释放前一个main（需使用r.main）");
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

    // 访问代码块节点（BlockNode，保持原有作用域）
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

    // 访问变量声明节点（VariableDeclarationNode，保持原有逻辑）
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
                add_error(ErrorCode::ER_0003, node->location, "数组声明缺少大小");
                var_info.array_size = 0;
            }
            else {
                DataType size_type = get_expression_type(node->array_size.get());
                if (size_type != DataType::DATA_TYPE_INT) {
                    add_error(ErrorCode::ER_1001, node->array_size->location, "数组大小必须是int类型");
                    var_info.array_size = 0;
                }
                else if (auto lit = dynamic_cast<LiteralExprNode*>(node->array_size.get())) {
                    try {
                        size_t size = std::stoull(lit->value);
                        if (size == 0) {
                            add_error(ErrorCode::ER_2001, node->array_size->location, "数组大小不能为0");
                        }
                        var_info.array_size = size;
                    }
                    catch (...) {
                        add_error(ErrorCode::ER_2001, node->array_size->location, "数组大小必须是正整数");
                        var_info.array_size = 0;
                    }
                }
                else {
                    add_error(ErrorCode::ER_2001, node->array_size->location, "数组大小必须是字面量整数");
                    var_info.array_size = 0;
                }
            }
        }
        else {
            var_info.array_size = 0;
        }

        if (node->initial_value) {
            DataType init_type = get_expression_type(node->initial_value.get());
            check_type_compatibility(node->var_type, init_type, node->initial_value->location, "变量" + node->var_name + "初始化");
        }

        Symbol var_symbol(node->var_name, node->location, var_info);
        SemanticError add_err(ErrorCode::NO_ERROR, node->location, "");
        if (!symbol_table.add_symbol(var_symbol, add_err)) {
            errors.push_back(add_err);
        }
    }

    // 访问赋值节点（AssignmentNode，支持特殊变量赋值）
    void visit(AssignmentNode* node) {
        if (!node || !node->target || !node->value) return;
        DataType target_type;
        std::string target_name;

        // 1. 检查赋值目标（普通变量/数组/特殊变量）
        if (auto id = dynamic_cast<IdentifierExprNode*>(node->target.get())) {
            target_name = id->name;
            if (!check_identifier_declared(target_name, id->location)) return;

            const Symbol* var_sym = symbol_table.find_symbol(target_name);
            if (var_sym->type != Symbol::SymbolType::VARIABLE || !var_sym->var_info.has_value()) {
                add_error(ErrorCode::ER_0003, id->location, target_name + "不是变量，不能赋值");
                return;
            }
            target_type = var_sym->var_info.value().data_type;
        }
        else if (auto arr_access = dynamic_cast<ArrayAccessExprNode*>(node->target.get())) {
            target_name = arr_access->array_name->name;
            if (!check_identifier_declared(target_name, arr_access->array_name->location)) return;

            const Symbol* arr_sym = symbol_table.find_symbol(target_name);
            if (arr_sym->type != Symbol::SymbolType::VARIABLE || !arr_sym->var_info.has_value() || !arr_sym->var_info.value().is_array) {
                add_error(ErrorCode::ER_0003, arr_access->array_name->location, target_name + "不是数组，不能通过索引赋值");
                return;
            }
            target_type = arr_sym->var_info.value().data_type;

            // 检查数组索引
            DataType index_type = get_expression_type(arr_access->index.get());
            if (index_type != DataType::DATA_TYPE_INT && index_type != DataType::DATA_TYPE_UNKNOWN) {
                add_error(ErrorCode::ER_1001, arr_access->index->location, "数组索引必须是int类型");
            }
        }
        else {
            add_error(ErrorCode::ER_0003, node->target->location, "赋值目标必须是变量或数组元素");
            return;
        }

        // 2. 检查赋值表达式类型
        DataType value_type = get_expression_type(node->value.get());
        check_type_compatibility(target_type, value_type, node->value->location, "赋值给" + target_name);
    }

    // 访问输出节点（OutputNode，支持Count表达式输出）
    void visit(OutputNode* node) {
        if (!node || !node->expr) return;
        // 仅检查表达式合法性（无需强制类型，字符串/数值均可输出）
        get_expression_type(node->expr.get());
    }

    // 访问输入节点（InputNode，保持原有逻辑）
    void visit(InputNode* node) {
        if (!node || !node->target) return;
        if (!check_identifier_declared(node->target->name, node->target->location)) return;

        const Symbol* var_sym = symbol_table.find_symbol(node->target->name);
        if (var_sym->type != Symbol::SymbolType::VARIABLE || !var_sym->var_info.has_value()) {
            add_error(ErrorCode::ER_0003, node->target->location, node->target->name + "不是变量，不能接收输入");
            return;
        }

        if (node->prompt) {
            DataType prompt_type = get_expression_type(node->prompt.get());
            if (prompt_type != DataType::DATA_TYPE_STRING && prompt_type != DataType::DATA_TYPE_UNKNOWN) {
                add_error(ErrorCode::ER_1001, node->prompt->location, "输入提示必须是string类型");
            }
        }
    }

    // 访问条件节点（IfNode，保持原有逻辑）
    void visit(IfNode* node) {
        if (!node || !node->condition || !node->then_block) return;
        if (!check_numeric_expression(node->condition.get(), node->condition->location, "If条件")) {
            return;
        }

        visit(node->then_block.get());
        if (node->else_block) {
            visit(node->else_block.get());
        }
    }

    // 访问线程节点（ThreadNode，保持原有逻辑）
    void visit(ThreadNode* node) {
        if (!node || !node->thread_body) return;
        visit(node->thread_body.get());
    }

    // 访问返回节点（ReturnNode，保持原有逻辑）
    void visit(ReturnNode* node) {
        if (!node) return;
        if (!node->return_value) return;

        // 检查返回值类型（需结合函数返回类型，此处简化为数值/字符串均可）
        get_expression_type(node->return_value.get());
    }

    // 访问释放main节点（ReleaseMainNode，保持原有逻辑）
    void visit(ReleaseMainNode* node) {
        if (!node) return;
        last_main_released = true;
    }

    // 访问退出节点（ExitNode，无特殊检查）
    void visit(ExitNode* node) {}

    // 访问字面量表达式（LiteralExprNode，无特殊检查）
    void visit(LiteralExprNode* node) {}

    // 访问标识符表达式（IdentifierExprNode，检查声明）
    void visit(IdentifierExprNode* node) {
        if (!node) return;
        check_identifier_declared(node->name, node->location);
    }

    // 访问二元运算表达式（BinaryOpExprNode，检查操作数类型）
    void visit(BinaryOpExprNode* node) {
        if (!node || !node->left || !node->right) return;
        visit(node->left.get());
        visit(node->right.get());

        DataType left_type = get_expression_type(node->left.get());
        DataType right_type = get_expression_type(node->right.get());

        // 字符串仅支持拼接（+）
        if ((left_type == DataType::DATA_TYPE_STRING || right_type == DataType::DATA_TYPE_STRING)) {
            if (node->op != TokenType::TOKEN_PLUS) {
                add_error(
                    ErrorCode::ER_1001,
                    node->location,
                    "字符串仅支持+运算（拼接），当前运算符：" + token_type_to_string(node->op)
                );
            }
            return;
        }

        // 数值运算支持+、-、*、/、<、>、=
        if (!check_numeric_expression(node->left.get(), node->left->location, "二元运算左操作数")) return;
        if (!check_numeric_expression(node->right.get(), node->right->location, "二元运算右操作数")) return;
    }

    // 访问数组访问表达式（ArrayAccessExprNode，检查数组存在和索引类型）
    void visit(ArrayAccessExprNode* node) {
        if (!node || !node->array_name || !node->index) return;
        visit(node->array_name.get()); // 检查数组名是否声明
        visit(node->index.get());      // 检查索引表达式

        const Symbol* arr_sym = symbol_table.find_symbol(node->array_name->name);
        if (arr_sym && arr_sym->type == Symbol::SymbolType::VARIABLE && arr_sym->var_info.has_value() && arr_sym->var_info.value().is_array) {
            DataType index_type = get_expression_type(node->index.get());
            if (index_type != DataType::DATA_TYPE_INT && index_type != DataType::DATA_TYPE_UNKNOWN) {
                add_error(
                    ErrorCode::ER_1001,
                    node->index->location,
                    "数组" + node->array_name->name + "的索引必须是int类型，实际为" + CommonUtils::data_type_to_string(index_type)
                );
            }
        }
    }

    // 访问函数调用表达式（FunctionCallExprNode，保持原有逻辑）
    void visit(FunctionCallExprNode* node) {
        if (!node) return;
        if (!check_identifier_declared(node->function_name, node->location)) return;

        const Symbol* func_sym = symbol_table.find_symbol(node->function_name);
        if (func_sym->type != Symbol::SymbolType::FUNCTION || !func_sym->func_info.has_value()) {
            add_error(ErrorCode::ER_0003, node->location, node->function_name + "不是函数");
            return;
        }

        const auto& param_types = func_sym->func_info.value().param_types;
        if (node->arguments.size() != param_types.size()) {
            add_error(
                ErrorCode::ER_1001,
                node->location,
                "函数" + node->function_name + "参数数量不匹配（预期：" + std::to_string(param_types.size()) + "，实际：" + std::to_string(node->arguments.size()) + "）"
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
                "函数" + node->function_name + "的参数" + std::to_string(i + 1)
            );
        }
    }

    // 访问π表达式（PiExprNode，检查精度为正整数）
    void visit(PiExprNode* node) {
        if (!node) return;
        if (node->precision) {
            visit(node->precision.get());
            if (!check_numeric_expression(node->precision.get(), node->precision->location, "π计算精度")) return;

            if (auto prec_lit = dynamic_cast<LiteralExprNode*>(node->precision.get())) {
                if (prec_lit->value_type != DataType::DATA_TYPE_INT) {
                    add_error(ErrorCode::ER_1001, node->precision->location, "π计算精度必须是int类型（当前：" + CommonUtils::data_type_to_string(prec_lit->value_type) + "）");
                }
                else {
                    int prec = std::stoi(prec_lit->value);
                    if (prec <= 0) {
                        add_error(ErrorCode::ER_0003, node->precision->location, "π计算精度必须为正整数（当前：" + std::to_string(prec) + "）");
                    }
                }
            }
        }
    }

    // 通用表达式节点访问（兜底）
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
            add_error(ErrorCode::ER_0003, node->location, "不支持的表达式类型");
        }
    }

    // 通用语句节点访问（兜底）
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
            add_error(ErrorCode::ER_0003, node->location, "不支持的语句类型");
        }
    }

    // 辅助函数：TokenType转字符串（用于错误信息）
    std::string token_type_to_string(TokenType type) {
        switch (type) {
        case TokenType::TOKEN_PLUS: return "+";
        case TokenType::TOKEN_MINUS: return "-";
        case TokenType::TOKEN_MUL: return "*";
        case TokenType::TOKEN_DIV: return "/";
        case TokenType::TOKEN_EQUALS: return "=";
        case TokenType::TOKEN_LT: return "<";
        case TokenType::TOKEN_GT: return ">";
        default: return "未知运算符";
        }
    }

public:
    SemanticAnalyzer() = default;

    // 执行语义分析（入口方法）
    void analyze(ProgramNode* program) {
        if (program) {
            visit(program);
        }
    }

    // 获取语义错误列表
    const std::vector<SemanticError>& get_errors() const {
        return errors;
    }
};

#endif // SEMANTIC_ANALYZER_H