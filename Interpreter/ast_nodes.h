//Copyright (c) Gallt Developer.
//版权所有（c）Gallt 开发者。

#ifndef AST_NODES_H
#define AST_NODES_H
#include "common.h"
#include <memory>
#include <vector>

// 前向声明（用于节点间引用）
class ASTNode;
class ExpressionNode;
class StatementNode;
class BlockNode;

// ------------------------------ 基类定义 ------------------------------
// AST所有节点的基类，包含位置信息
class ASTNode {
public:
    SourceLocation location; // 节点在源码中的位置（用于错误定位）
    explicit ASTNode(const SourceLocation& loc) : location(loc) {}
    virtual ~ASTNode() = default; // 虚析构函数，确保子类正确析构
};

// ------------------------------ 表达式节点（Expression） ------------------------------
// 表达式基类（所有表达式节点的父类）
class ExpressionNode : public ASTNode {
public:
    using ASTNode::ASTNode;
    // 类型推断方法（语义分析阶段使用）
    virtual DataType get_type() const { return DataType::DATA_TYPE_UNKNOWN; }
};

// 1. 字面量表达式（如123、3.14、"hello"）
class LiteralExprNode : public ExpressionNode {
public:
    DataType value_type; // 字面量类型
    std::string value;   // 字面量字符串形式（统一存储，解释时转换）
    LiteralExprNode(const SourceLocation& loc, DataType type, const std::string& val)
        : ExpressionNode(loc), value_type(type), value(val) {
    }
    DataType get_type() const override { return value_type; }
};

// 2. 标识符表达式（变量名、函数名等引用，如x、add）
class IdentifierExprNode : public ExpressionNode {
public:
    std::string name; // 标识符名称
    explicit IdentifierExprNode(const SourceLocation& loc, const std::string& n)
        : ExpressionNode(loc), name(n) {
    }
};

// 3. 二元运算表达式（如a + b、x * 3）
class BinaryOpExprNode : public ExpressionNode {
public:
    TokenType op; // 运算符（+、-、*、/、=、<、>等）
    std::unique_ptr<ExpressionNode> left;  // 左操作数
    std::unique_ptr<ExpressionNode> right; // 右操作数
    BinaryOpExprNode(const SourceLocation& loc, TokenType o,
        std::unique_ptr<ExpressionNode> l,
        std::unique_ptr<ExpressionNode> r)
        : ExpressionNode(loc), op(o), left(std::move(l)), right(std::move(r)) {
    }
};

// 4. 数组访问表达式（如arr.1、nums.5）
class ArrayAccessExprNode : public ExpressionNode {
public:
    std::unique_ptr<IdentifierExprNode> array_name; // 数组名
    std::unique_ptr<ExpressionNode> index;          // 索引表达式（如1、x+2）
    ArrayAccessExprNode(const SourceLocation& loc,
        std::unique_ptr<IdentifierExprNode> arr,
        std::unique_ptr<ExpressionNode> idx)
        : ExpressionNode(loc), array_name(std::move(arr)), index(std::move(idx)) {
    }
};

// 5. 函数调用表达式（如add(1;2)、input("name")）
class FunctionCallExprNode : public ExpressionNode {
public:
    std::string function_name; // 函数名
    std::vector<std::unique_ptr<ExpressionNode>> arguments; // 参数列表（用;分隔）
    FunctionCallExprNode(const SourceLocation& loc, const std::string& name,
        std::vector<std::unique_ptr<ExpressionNode>> args)
        : ExpressionNode(loc), function_name(name), arguments(std::move(args)) {
    }
};

// 6. π表达式（如pi(5)，指定精度）
class PiExprNode : public ExpressionNode {
public:
    std::unique_ptr<ExpressionNode> precision; // 精度表达式（可选，默认7位）
    explicit PiExprNode(const SourceLocation& loc, std::unique_ptr<ExpressionNode> prec = nullptr)
        : ExpressionNode(loc), precision(std::move(prec)) {
    }
};

// 新增：7. Count数值计算表达式（对应文档count语法：count(运算)、float.count(运算)）
class CountExprNode : public ExpressionNode {
public:
    enum class CountType { // Count类型：普通int、float、double
        COUNT_NORMAL,  // 对应 count(...)
        COUNT_FLOAT,   // 对应 float.count(...)
        COUNT_DOUBLE   // 对应 double.count(...)
    };
    CountType count_type; // Count计算类型
    std::unique_ptr<ExpressionNode> calc_expr; // 待计算的表达式（如5*8、1.5+0.7）
    CountExprNode(const SourceLocation& loc, CountType type, std::unique_ptr<ExpressionNode> expr)
        : ExpressionNode(loc), count_type(type), calc_expr(std::move(expr)) {
    }
    DataType get_type() const override { // 根据Count类型返回对应数据类型
        switch (count_type) {
        case CountType::COUNT_NORMAL: return DataType::DATA_TYPE_INT;
        case CountType::COUNT_FLOAT: return DataType::DATA_TYPE_FLOAT;
        case CountType::COUNT_DOUBLE: return DataType::DATA_TYPE_DOUBLE;
        default: return DataType::DATA_TYPE_UNKNOWN;
        }
    }
};

// 新增：8. 事件条件表达式（如detect keyboard.click="k"中的条件）
class EventConditionExprNode : public ExpressionNode {
public:
    std::string event_type; // 事件类型（如"keyboard.click"）
    std::unique_ptr<ExpressionNode> trigger_val; // 触发值（如"k"）
    EventConditionExprNode(const SourceLocation& loc, const std::string& evt_type, std::unique_ptr<ExpressionNode> trig_val)
        : ExpressionNode(loc), event_type(evt_type), trigger_val(std::move(trig_val)) {
    }
};

// ------------------------------ 语句节点（Statement） ------------------------------
// 语句基类（所有语句节点的父类）
class StatementNode : public ASTNode {
public:
    using ASTNode::ASTNode;
};

// 1. 代码块节点（用{...}包裹的语句集合）
class BlockNode : public StatementNode {
public:
    std::vector<std::unique_ptr<StatementNode>> statements; // 块内语句列表

    // 构造函数：使用移动语义接收参数，正确
    explicit BlockNode(const SourceLocation& loc,
        std::vector<std::unique_ptr<StatementNode>> stmts)
        : StatementNode(loc), statements(std::move(stmts)) {
    }

    // 禁止拷贝（核心修改）
    BlockNode(const BlockNode&) = delete;
    BlockNode& operator=(const BlockNode&) = delete;

    // 允许移动（可选，但建议显式默认，避免编译器因删除拷贝而不生成移动）
    BlockNode(BlockNode&&) = default;
    BlockNode& operator=(BlockNode&&) = default;
};

// 2. 变量声明语句（如define int x; 或 define arr int nums[5];）
class VariableDeclarationNode : public StatementNode {
public:
    DataType var_type;          // 变量类型（int/float等）
    std::string var_name;       // 变量名
    bool is_array;              // 是否为数组
    std::unique_ptr<ExpressionNode> array_size; // 数组大小（仅当is_array为true时有效）
    std::unique_ptr<ExpressionNode> initial_value; // 初始值（可选）
    VariableDeclarationNode(const SourceLocation& loc, DataType type, const std::string& name,
        bool is_arr = false,
        std::unique_ptr<ExpressionNode> size = nullptr,
        std::unique_ptr<ExpressionNode> init_val = nullptr)
        : StatementNode(loc), var_type(type), var_name(name), is_array(is_arr),
        array_size(std::move(size)), initial_value(std::move(init_val)) {
    }
};

// 3. 赋值语句（如x = 5; 或 arr.2 = "hello";）
class AssignmentNode : public StatementNode {
public:
    std::unique_ptr<ExpressionNode> target; // 赋值目标（标识符或数组访问）
    std::unique_ptr<ExpressionNode> value;  // 赋值表达式
    AssignmentNode(const SourceLocation& loc,
        std::unique_ptr<ExpressionNode> tgt,
        std::unique_ptr<ExpressionNode> val)
        : StatementNode(loc), target(std::move(tgt)), value(std::move(val)) {
    }
};

// 4. 输出语句（如output x; 或 output "result: " + str(y);）
class OutputNode : public StatementNode {
public:
    std::unique_ptr<ExpressionNode> expr; // 输出的表达式
    explicit OutputNode(const SourceLocation& loc, std::unique_ptr<ExpressionNode> e)
        : StatementNode(loc), expr(std::move(e)) {
    }
};

// 5. 输入语句（如input x; 或 input "name: ", name;）
class InputNode : public StatementNode {
public:
    std::unique_ptr<ExpressionNode> prompt; // 提示信息（可选，字符串表达式）
    std::unique_ptr<IdentifierExprNode> target; // 接收输入的变量
    InputNode(const SourceLocation& loc,
        std::unique_ptr<ExpressionNode> p,
        std::unique_ptr<IdentifierExprNode> tgt)
        : StatementNode(loc), prompt(std::move(p)), target(std::move(tgt)) {
    }
};

// 6. 条件语句（if...otherwise...）
class IfNode : public StatementNode {
public:
    std::unique_ptr<ExpressionNode> condition; // 条件表达式
    std::unique_ptr<BlockNode> then_block;     // if成立时执行的块
    std::unique_ptr<BlockNode> else_block;     // otherwise块（可选）
    IfNode(const SourceLocation& loc,
        std::unique_ptr<ExpressionNode> cond,
        std::unique_ptr<BlockNode> then_blk,
        std::unique_ptr<BlockNode> else_blk = nullptr)
        : StatementNode(loc), condition(std::move(cond)),
        then_block(std::move(then_blk)), else_block(std::move(else_blk)) {
    }
};

// 新增：7. 特殊变量声明节点（对应文档start var.name="name" { ... }）
class VarNode : public StatementNode {
public:
    // 单个特殊变量项（如var1 string name="Peter"）
    struct VarItem {
        std::string var_id; // 变量标识（如"var1"、"var2"）
        DataType var_type;  // 变量类型（int/float/double/string）
        std::string var_key; // 变量键（如"name"）
        std::unique_ptr<ExpressionNode> var_value; // 变量值（如"Peter"、12）
    };
    std::string var_group_name; // 变量组名称（如"Peter"，对应var.name="Peter"）
    std::vector<VarItem> var_items; // 特殊变量列表（包含多个VarItem）
    VarNode(const SourceLocation& loc, const std::string& group_name, std::vector<VarItem> items)
        : StatementNode(loc), var_group_name(group_name), var_items(std::move(items)) {
    }
};

// 新增：8. 事件检测语句节点（对应文档detect [名称] run { ... }）
class DetectNode : public StatementNode {
public:
    std::unique_ptr<EventConditionExprNode> event_condition; // 事件条件（如keyboard.click="k"）
    std::unique_ptr<BlockNode> run_block; // 事件触发后执行的代码块（run { ... }）
    DetectNode(const SourceLocation& loc,
        std::unique_ptr<EventConditionExprNode> evt_cond,
        std::unique_ptr<BlockNode> run_blk)
        : StatementNode(loc), event_condition(std::move(evt_cond)), run_block(std::move(run_blk)) {
    }
};

// 9. 循环语句（while/for/re，扩展re等待和For循环字段）
class LoopNode : public StatementNode {
public:
    enum class LoopType {
        LOOP_WHILE,  // while循环（条件循环）
        LOOP_FOR,    // for循环（计数循环，如for i 1 to 50）
        LOOP_RE      // re循环（重复执行，支持wait）
    };
    LoopType loop_type;
    // 通用循环条件（while条件/re次数）
    std::unique_ptr<ExpressionNode> condition;
    bool is_infinite;                          // re是否为无限循环（infinite）
    std::unique_ptr<BlockNode> body;           // 循环体

    // 新增：For循环专属字段（对应for i 1 to 50）
    std::string for_loop_var;                  // 循环变量名（如"i"）
    std::unique_ptr<ExpressionNode> for_start; // 起始值（如1）
    std::unique_ptr<ExpressionNode> for_end;   // 结束值（如50）
    std::unique_ptr<ExpressionNode> for_step;  // 步长（默认1，可选）

    // 新增：re循环专属字段（对应re wait time = 3000ms）
    std::unique_ptr<ExpressionNode> re_wait_duration; // re每次循环后的等待时长
    std::string re_wait_unit;                         // re等待单位（ms/s/min/hour，默认ms）

    // 构造函数：通用循环（while/re）
    LoopNode(const SourceLocation& loc, LoopType type,
        std::unique_ptr<ExpressionNode> cond,
        std::unique_ptr<BlockNode> bdy,
        bool infinite = false)
        : StatementNode(loc), loop_type(type), condition(std::move(cond)),
        body(std::move(bdy)), is_infinite(infinite),
        for_step(std::make_unique<LiteralExprNode>(loc, DataType::DATA_TYPE_INT, "1")), // For步长默认1
        re_wait_unit(GalltConstants::DEFAULT_WAIT_UNIT) { // re等待单位默认ms
    }

    // 构造函数：For循环（专用）
    LoopNode(const SourceLocation& loc,
        const std::string& loop_var,
        std::unique_ptr<ExpressionNode> start,
        std::unique_ptr<ExpressionNode> end,
        std::unique_ptr<ExpressionNode> step,
        std::unique_ptr<BlockNode> bdy)
        : StatementNode(loc), loop_type(LoopType::LOOP_FOR), body(std::move(bdy)),
        is_infinite(false), for_loop_var(loop_var),
        for_start(std::move(start)), for_end(std::move(end)),
        for_step(step ? std::move(step) : std::make_unique<LiteralExprNode>(loc, DataType::DATA_TYPE_INT, "1")),
        re_wait_unit(GalltConstants::DEFAULT_WAIT_UNIT) {
    }

    // 构造函数：带wait的re循环（专用）
    LoopNode(const SourceLocation& loc,
        std::unique_ptr<ExpressionNode> cond,
        std::unique_ptr<BlockNode> bdy,
        std::unique_ptr<ExpressionNode> wait_duration,
        const std::string& wait_unit,
        bool infinite = false)
        : StatementNode(loc), loop_type(LoopType::LOOP_RE), condition(std::move(cond)),
        body(std::move(bdy)), is_infinite(infinite),
        re_wait_duration(std::move(wait_duration)), re_wait_unit(wait_unit),
        for_step(std::make_unique<LiteralExprNode>(loc, DataType::DATA_TYPE_INT, "1")) {
    }
};

// 10. 等待语句（如wait 100; 等待100ms，扩展多单位支持）
class WaitNode : public StatementNode {
public:
    std::unique_ptr<ExpressionNode> duration; // 等待时长（数值表达式）
    std::string unit;                         // 等待单位（ms/s/min/hour，默认ms）
    WaitNode(const SourceLocation& loc,
        std::unique_ptr<ExpressionNode> dur,
        const std::string& u = GalltConstants::DEFAULT_WAIT_UNIT)
        : StatementNode(loc), duration(std::move(dur)), unit(u) {
    }
};

// 11. 线程语句（如thread t1 { ... }，完善线程信息）
class ThreadNode : public StatementNode {
public:
    std::string thread_name; // 线程名
    std::unique_ptr<BlockNode> thread_body; // 线程执行体
    ThreadNode(const SourceLocation& loc, const std::string& name,
        std::unique_ptr<BlockNode> body)
        : StatementNode(loc), thread_name(name), thread_body(std::move(body)) {
    }
};

// 12. 函数定义语句（如function int add(int a; int b) { ... }）
class FunctionDefinitionNode : public StatementNode {
public:
    std::string function_name;                // 函数名
    DataType return_type;                     // 返回类型（无返回值可设为UNKNOWN）
    std::vector<VariableDeclarationNode> parameters; // 参数列表（含类型）
    std::unique_ptr<BlockNode> function_body; // 函数体
    FunctionDefinitionNode(const SourceLocation& loc, const std::string& name,
        DataType ret_type,
        std::vector<VariableDeclarationNode> params,
        std::unique_ptr<BlockNode> body)
        : StatementNode(loc), function_name(name), return_type(ret_type),
        parameters(std::move(params)), function_body(std::move(body)) {
    }
};

// 13. 返回语句（如return x + y;）
class ReturnNode : public StatementNode {
public:
    std::unique_ptr<ExpressionNode> return_value; // 返回值表达式（可选）
    explicit ReturnNode(const SourceLocation& loc, std::unique_ptr<ExpressionNode> val = nullptr)
        : StatementNode(loc), return_value(std::move(val)) {
    }
};

// 14. 释放main语句（r.main）
class ReleaseMainNode : public StatementNode {
public:
    using StatementNode::StatementNode; // 继承构造函数
};

// 15. 退出语句（exit）
class ExitNode : public StatementNode {
public:
    using StatementNode::StatementNode; // 继承构造函数
};

// ------------------------------ 程序结构节点 ------------------------------
// 主程序节点（main块）
class MainNode : public ASTNode {
public:
    std::string main_name; // main名称（可选，多main程序用）
    std::unique_ptr<BlockNode> main_body; // main执行体
    MainNode(const SourceLocation& loc, const std::string& name,
        std::unique_ptr<BlockNode> body)
        : ASTNode(loc), main_name(name), main_body(std::move(body)) {
    }
};

// 整个程序的根节点（扩展事件列表，支持事件检测）
class ProgramNode : public ASTNode {
public:
    std::vector<std::unique_ptr<FunctionDefinitionNode>> functions; // 函数定义列表
    std::vector<std::unique_ptr<MainNode>> mains; // main程序列表（多main支持）
    std::vector<std::unique_ptr<DetectNode>> detect_events; // 全局事件检测列表（新增）
    explicit ProgramNode(const SourceLocation& loc) : ASTNode(loc) {}
};

#endif // AST_NODES_H