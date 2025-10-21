//Copyright (c) Gallt Developer.
//��Ȩ���У�c��Gallt �����ߡ�

#ifndef AST_NODES_H
#define AST_NODES_H
#include "common.h"
#include <memory>
#include <vector>

// ǰ�����������ڽڵ�����ã�
class ASTNode;
class ExpressionNode;
class StatementNode;
class BlockNode;

// ------------------------------ ���ඨ�� ------------------------------
// AST���нڵ�Ļ��࣬����λ����Ϣ
class ASTNode {
public:
    SourceLocation location; // �ڵ���Դ���е�λ�ã����ڴ���λ��
    explicit ASTNode(const SourceLocation& loc) : location(loc) {}
    virtual ~ASTNode() = default; // ������������ȷ��������ȷ����
};

// ------------------------------ ���ʽ�ڵ㣨Expression�� ------------------------------
// ���ʽ���ࣨ���б��ʽ�ڵ�ĸ��ࣩ
class ExpressionNode : public ASTNode {
public:
    using ASTNode::ASTNode;
    // �����ƶϷ�������������׶�ʹ�ã�
    virtual DataType get_type() const { return DataType::DATA_TYPE_UNKNOWN; }
};

// 1. ���������ʽ����123��3.14��"hello"��
class LiteralExprNode : public ExpressionNode {
public:
    DataType value_type; // ����������
    std::string value;   // �������ַ�����ʽ��ͳһ�洢������ʱת����
    LiteralExprNode(const SourceLocation& loc, DataType type, const std::string& val)
        : ExpressionNode(loc), value_type(type), value(val) {
    }
    DataType get_type() const override { return value_type; }
};

// 2. ��ʶ�����ʽ���������������������ã���x��add��
class IdentifierExprNode : public ExpressionNode {
public:
    std::string name; // ��ʶ������
    explicit IdentifierExprNode(const SourceLocation& loc, const std::string& n)
        : ExpressionNode(loc), name(n) {
    }
};

// 3. ��Ԫ������ʽ����a + b��x * 3��
class BinaryOpExprNode : public ExpressionNode {
public:
    TokenType op; // �������+��-��*��/��=��<��>�ȣ�
    std::unique_ptr<ExpressionNode> left;  // �������
    std::unique_ptr<ExpressionNode> right; // �Ҳ�����
    BinaryOpExprNode(const SourceLocation& loc, TokenType o,
        std::unique_ptr<ExpressionNode> l,
        std::unique_ptr<ExpressionNode> r)
        : ExpressionNode(loc), op(o), left(std::move(l)), right(std::move(r)) {
    }
};

// 4. ������ʱ��ʽ����arr.1��nums.5��
class ArrayAccessExprNode : public ExpressionNode {
public:
    std::unique_ptr<IdentifierExprNode> array_name; // ������
    std::unique_ptr<ExpressionNode> index;          // �������ʽ����1��x+2��
    ArrayAccessExprNode(const SourceLocation& loc,
        std::unique_ptr<IdentifierExprNode> arr,
        std::unique_ptr<ExpressionNode> idx)
        : ExpressionNode(loc), array_name(std::move(arr)), index(std::move(idx)) {
    }
};

// 5. �������ñ��ʽ����add(1;2)��input("name")��
class FunctionCallExprNode : public ExpressionNode {
public:
    std::string function_name; // ������
    std::vector<std::unique_ptr<ExpressionNode>> arguments; // �����б���;�ָ���
    FunctionCallExprNode(const SourceLocation& loc, const std::string& name,
        std::vector<std::unique_ptr<ExpressionNode>> args)
        : ExpressionNode(loc), function_name(name), arguments(std::move(args)) {
    }
};

// 6. �б��ʽ����pi(5)��ָ�����ȣ�
class PiExprNode : public ExpressionNode {
public:
    std::unique_ptr<ExpressionNode> precision; // ���ȱ��ʽ����ѡ��Ĭ��7λ��
    explicit PiExprNode(const SourceLocation& loc, std::unique_ptr<ExpressionNode> prec = nullptr)
        : ExpressionNode(loc), precision(std::move(prec)) {
    }
};

// ������7. Count��ֵ������ʽ����Ӧ�ĵ�count�﷨��count(����)��float.count(����)��
class CountExprNode : public ExpressionNode {
public:
    enum class CountType { // Count���ͣ���ͨint��float��double
        COUNT_NORMAL,  // ��Ӧ count(...)
        COUNT_FLOAT,   // ��Ӧ float.count(...)
        COUNT_DOUBLE   // ��Ӧ double.count(...)
    };
    CountType count_type; // Count��������
    std::unique_ptr<ExpressionNode> calc_expr; // ������ı��ʽ����5*8��1.5+0.7��
    CountExprNode(const SourceLocation& loc, CountType type, std::unique_ptr<ExpressionNode> expr)
        : ExpressionNode(loc), count_type(type), calc_expr(std::move(expr)) {
    }
    DataType get_type() const override { // ����Count���ͷ��ض�Ӧ��������
        switch (count_type) {
        case CountType::COUNT_NORMAL: return DataType::DATA_TYPE_INT;
        case CountType::COUNT_FLOAT: return DataType::DATA_TYPE_FLOAT;
        case CountType::COUNT_DOUBLE: return DataType::DATA_TYPE_DOUBLE;
        default: return DataType::DATA_TYPE_UNKNOWN;
        }
    }
};

// ������8. �¼��������ʽ����detect keyboard.click="k"�е�������
class EventConditionExprNode : public ExpressionNode {
public:
    std::string event_type; // �¼����ͣ���"keyboard.click"��
    std::unique_ptr<ExpressionNode> trigger_val; // ����ֵ����"k"��
    EventConditionExprNode(const SourceLocation& loc, const std::string& evt_type, std::unique_ptr<ExpressionNode> trig_val)
        : ExpressionNode(loc), event_type(evt_type), trigger_val(std::move(trig_val)) {
    }
};

// ------------------------------ ���ڵ㣨Statement�� ------------------------------
// �����ࣨ�������ڵ�ĸ��ࣩ
class StatementNode : public ASTNode {
public:
    using ASTNode::ASTNode;
};

// 1. �����ڵ㣨��{...}��������伯�ϣ�
class BlockNode : public StatementNode {
public:
    std::vector<std::unique_ptr<StatementNode>> statements; // ��������б�

    // ���캯����ʹ���ƶ�������ղ�������ȷ
    explicit BlockNode(const SourceLocation& loc,
        std::vector<std::unique_ptr<StatementNode>> stmts)
        : StatementNode(loc), statements(std::move(stmts)) {
    }

    // ��ֹ�����������޸ģ�
    BlockNode(const BlockNode&) = delete;
    BlockNode& operator=(const BlockNode&) = delete;

    // �����ƶ�����ѡ����������ʽĬ�ϣ������������ɾ���������������ƶ���
    BlockNode(BlockNode&&) = default;
    BlockNode& operator=(BlockNode&&) = default;
};

// 2. ����������䣨��define int x; �� define arr int nums[5];��
class VariableDeclarationNode : public StatementNode {
public:
    DataType var_type;          // �������ͣ�int/float�ȣ�
    std::string var_name;       // ������
    bool is_array;              // �Ƿ�Ϊ����
    std::unique_ptr<ExpressionNode> array_size; // �����С������is_arrayΪtrueʱ��Ч��
    std::unique_ptr<ExpressionNode> initial_value; // ��ʼֵ����ѡ��
    VariableDeclarationNode(const SourceLocation& loc, DataType type, const std::string& name,
        bool is_arr = false,
        std::unique_ptr<ExpressionNode> size = nullptr,
        std::unique_ptr<ExpressionNode> init_val = nullptr)
        : StatementNode(loc), var_type(type), var_name(name), is_array(is_arr),
        array_size(std::move(size)), initial_value(std::move(init_val)) {
    }
};

// 3. ��ֵ��䣨��x = 5; �� arr.2 = "hello";��
class AssignmentNode : public StatementNode {
public:
    std::unique_ptr<ExpressionNode> target; // ��ֵĿ�꣨��ʶ����������ʣ�
    std::unique_ptr<ExpressionNode> value;  // ��ֵ���ʽ
    AssignmentNode(const SourceLocation& loc,
        std::unique_ptr<ExpressionNode> tgt,
        std::unique_ptr<ExpressionNode> val)
        : StatementNode(loc), target(std::move(tgt)), value(std::move(val)) {
    }
};

// 4. �����䣨��output x; �� output "result: " + str(y);��
class OutputNode : public StatementNode {
public:
    std::unique_ptr<ExpressionNode> expr; // ����ı��ʽ
    explicit OutputNode(const SourceLocation& loc, std::unique_ptr<ExpressionNode> e)
        : StatementNode(loc), expr(std::move(e)) {
    }
};

// 5. ������䣨��input x; �� input "name: ", name;��
class InputNode : public StatementNode {
public:
    std::unique_ptr<ExpressionNode> prompt; // ��ʾ��Ϣ����ѡ���ַ������ʽ��
    std::unique_ptr<IdentifierExprNode> target; // ��������ı���
    InputNode(const SourceLocation& loc,
        std::unique_ptr<ExpressionNode> p,
        std::unique_ptr<IdentifierExprNode> tgt)
        : StatementNode(loc), prompt(std::move(p)), target(std::move(tgt)) {
    }
};

// 6. ������䣨if...otherwise...��
class IfNode : public StatementNode {
public:
    std::unique_ptr<ExpressionNode> condition; // �������ʽ
    std::unique_ptr<BlockNode> then_block;     // if����ʱִ�еĿ�
    std::unique_ptr<BlockNode> else_block;     // otherwise�飨��ѡ��
    IfNode(const SourceLocation& loc,
        std::unique_ptr<ExpressionNode> cond,
        std::unique_ptr<BlockNode> then_blk,
        std::unique_ptr<BlockNode> else_blk = nullptr)
        : StatementNode(loc), condition(std::move(cond)),
        then_block(std::move(then_blk)), else_block(std::move(else_blk)) {
    }
};

// ������7. ������������ڵ㣨��Ӧ�ĵ�start var.name="name" { ... }��
class VarNode : public StatementNode {
public:
    // ��������������var1 string name="Peter"��
    struct VarItem {
        std::string var_id; // ������ʶ����"var1"��"var2"��
        DataType var_type;  // �������ͣ�int/float/double/string��
        std::string var_key; // ����������"name"��
        std::unique_ptr<ExpressionNode> var_value; // ����ֵ����"Peter"��12��
    };
    std::string var_group_name; // ���������ƣ���"Peter"����Ӧvar.name="Peter"��
    std::vector<VarItem> var_items; // ��������б��������VarItem��
    VarNode(const SourceLocation& loc, const std::string& group_name, std::vector<VarItem> items)
        : StatementNode(loc), var_group_name(group_name), var_items(std::move(items)) {
    }
};

// ������8. �¼�������ڵ㣨��Ӧ�ĵ�detect [����] run { ... }��
class DetectNode : public StatementNode {
public:
    std::unique_ptr<EventConditionExprNode> event_condition; // �¼���������keyboard.click="k"��
    std::unique_ptr<BlockNode> run_block; // �¼�������ִ�еĴ���飨run { ... }��
    DetectNode(const SourceLocation& loc,
        std::unique_ptr<EventConditionExprNode> evt_cond,
        std::unique_ptr<BlockNode> run_blk)
        : StatementNode(loc), event_condition(std::move(evt_cond)), run_block(std::move(run_blk)) {
    }
};

// 9. ѭ����䣨while/for/re����չre�ȴ���Forѭ���ֶΣ�
class LoopNode : public StatementNode {
public:
    enum class LoopType {
        LOOP_WHILE,  // whileѭ��������ѭ����
        LOOP_FOR,    // forѭ��������ѭ������for i 1 to 50��
        LOOP_RE      // reѭ�����ظ�ִ�У�֧��wait��
    };
    LoopType loop_type;
    // ͨ��ѭ��������while����/re������
    std::unique_ptr<ExpressionNode> condition;
    bool is_infinite;                          // re�Ƿ�Ϊ����ѭ����infinite��
    std::unique_ptr<BlockNode> body;           // ѭ����

    // ������Forѭ��ר���ֶΣ���Ӧfor i 1 to 50��
    std::string for_loop_var;                  // ѭ������������"i"��
    std::unique_ptr<ExpressionNode> for_start; // ��ʼֵ����1��
    std::unique_ptr<ExpressionNode> for_end;   // ����ֵ����50��
    std::unique_ptr<ExpressionNode> for_step;  // ������Ĭ��1����ѡ��

    // ������reѭ��ר���ֶΣ���Ӧre wait time = 3000ms��
    std::unique_ptr<ExpressionNode> re_wait_duration; // reÿ��ѭ����ĵȴ�ʱ��
    std::string re_wait_unit;                         // re�ȴ���λ��ms/s/min/hour��Ĭ��ms��

    // ���캯����ͨ��ѭ����while/re��
    LoopNode(const SourceLocation& loc, LoopType type,
        std::unique_ptr<ExpressionNode> cond,
        std::unique_ptr<BlockNode> bdy,
        bool infinite = false)
        : StatementNode(loc), loop_type(type), condition(std::move(cond)),
        body(std::move(bdy)), is_infinite(infinite),
        for_step(std::make_unique<LiteralExprNode>(loc, DataType::DATA_TYPE_INT, "1")), // For����Ĭ��1
        re_wait_unit(GalltConstants::DEFAULT_WAIT_UNIT) { // re�ȴ���λĬ��ms
    }

    // ���캯����Forѭ����ר�ã�
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

    // ���캯������wait��reѭ����ר�ã�
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

// 10. �ȴ���䣨��wait 100; �ȴ�100ms����չ�൥λ֧�֣�
class WaitNode : public StatementNode {
public:
    std::unique_ptr<ExpressionNode> duration; // �ȴ�ʱ������ֵ���ʽ��
    std::string unit;                         // �ȴ���λ��ms/s/min/hour��Ĭ��ms��
    WaitNode(const SourceLocation& loc,
        std::unique_ptr<ExpressionNode> dur,
        const std::string& u = GalltConstants::DEFAULT_WAIT_UNIT)
        : StatementNode(loc), duration(std::move(dur)), unit(u) {
    }
};

// 11. �߳���䣨��thread t1 { ... }�������߳���Ϣ��
class ThreadNode : public StatementNode {
public:
    std::string thread_name; // �߳���
    std::unique_ptr<BlockNode> thread_body; // �߳�ִ����
    ThreadNode(const SourceLocation& loc, const std::string& name,
        std::unique_ptr<BlockNode> body)
        : StatementNode(loc), thread_name(name), thread_body(std::move(body)) {
    }
};

// 12. ����������䣨��function int add(int a; int b) { ... }��
class FunctionDefinitionNode : public StatementNode {
public:
    std::string function_name;                // ������
    DataType return_type;                     // �������ͣ��޷���ֵ����ΪUNKNOWN��
    std::vector<VariableDeclarationNode> parameters; // �����б������ͣ�
    std::unique_ptr<BlockNode> function_body; // ������
    FunctionDefinitionNode(const SourceLocation& loc, const std::string& name,
        DataType ret_type,
        std::vector<VariableDeclarationNode> params,
        std::unique_ptr<BlockNode> body)
        : StatementNode(loc), function_name(name), return_type(ret_type),
        parameters(std::move(params)), function_body(std::move(body)) {
    }
};

// 13. ������䣨��return x + y;��
class ReturnNode : public StatementNode {
public:
    std::unique_ptr<ExpressionNode> return_value; // ����ֵ���ʽ����ѡ��
    explicit ReturnNode(const SourceLocation& loc, std::unique_ptr<ExpressionNode> val = nullptr)
        : StatementNode(loc), return_value(std::move(val)) {
    }
};

// 14. �ͷ�main��䣨r.main��
class ReleaseMainNode : public StatementNode {
public:
    using StatementNode::StatementNode; // �̳й��캯��
};

// 15. �˳���䣨exit��
class ExitNode : public StatementNode {
public:
    using StatementNode::StatementNode; // �̳й��캯��
};

// ------------------------------ ����ṹ�ڵ� ------------------------------
// ������ڵ㣨main�飩
class MainNode : public ASTNode {
public:
    std::string main_name; // main���ƣ���ѡ����main�����ã�
    std::unique_ptr<BlockNode> main_body; // mainִ����
    MainNode(const SourceLocation& loc, const std::string& name,
        std::unique_ptr<BlockNode> body)
        : ASTNode(loc), main_name(name), main_body(std::move(body)) {
    }
};

// ��������ĸ��ڵ㣨��չ�¼��б�֧���¼���⣩
class ProgramNode : public ASTNode {
public:
    std::vector<std::unique_ptr<FunctionDefinitionNode>> functions; // ���������б�
    std::vector<std::unique_ptr<MainNode>> mains; // main�����б���main֧�֣�
    std::vector<std::unique_ptr<DetectNode>> detect_events; // ȫ���¼�����б�������
    explicit ProgramNode(const SourceLocation& loc) : ASTNode(loc) {}
};

#endif // AST_NODES_H