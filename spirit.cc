#include <iostream>
using std::cout;
auto const nl = "\n";
#include <fstream>
#include <boost/spirit/home/x3.hpp>
#include <boost/spirit/home/x3/support/ast/variant.hpp>
namespace x3 = boost::spirit::x3;

#include <variant>
#include <vector>
#include <boost/fusion/include/std_tuple.hpp>
#include <tuple>
#include <map>
#include <optional>
#include <functional>

// Language Ops, like the bytecode or intermediate representation
enum OpCode {
    NOP,
    PUSH,
    END,
    ASSIGN,
    ADD, 
    SUB,
    MUL,
    DIV,
    CALL,
    DEF,
};

const char* code_names[]{
    "nop",
    "push",
    "end",
    "assign",
    "add",
    "sub",
    "mul",
    "div",
    "call",
    "def",
};

// a value type
struct Value{
    enum Type{
        INT,
        VAR,
        NONE
    } type = NONE;
    std::variant<int, std::string> value;
};

std::ostream& operator<<(std::ostream& o, Value& v){
    if (v.type != Value::NONE) {
        auto is_int = v.type == Value::INT;
        o << "value: ";
        if (is_int)
            o << "int " << std::get<int>(v.value);
        else
            o << "var " << std::get<std::string>(v.value);
    }

    return o;
}

struct Op {
    OpCode code;
    Value value;
};

std::ostream& operator<<(std::ostream& o, Op& op){
    o << code_names[op.code] << " " << op.value;
    return o;
}


std::vector<Op> program;

// SYNTAX

auto last_op = NOP;
std::vector<std::string> deferred_functions;
auto statement_end = (x3::lit("\n") | x3::lit("\r") | x3::lit("\0"))[([](){
    // cout << "FUNCS SIZE: " << deferred_functions.size() <<nl;
    // cout << "LAST OP: " << code_names[last_op] << nl;
    if (last_op == CALL && !deferred_functions.empty()){
        auto f = deferred_functions.back();
        // cout << "GETTING FUNCTION: "<< f << nl;
        deferred_functions.pop_back();
        program.push_back({
            .code = CALL,
            .value = {Value::VAR, f}
        });
    }
    program.push_back({END});
    last_op = NOP;
})];

auto reserved = (   
                    (x3::lit("print_num")>>(x3::lit("(") | statement_end))   |
                    (x3::lit("print_char")>>(x3::lit("(")| statement_end))   |
                    x3::lexeme[x3::lit("def") >> (x3::space | statement_end)]
                );
auto identifier = x3::lexeme[+x3::alpha]-reserved;

auto wrap_atom = [](Value::Type type){
    return [type](auto& ctx){
        // if (type == Value::VAR) {
        //     cout << "VAR!" << x3::_pass(ctx) << nl;
        // }
        auto attr = x3::_attr(ctx);
        x3::_val(ctx) = attr;
        program.push_back({
            .code = PUSH,
            .value = {type, attr}
        });
        // last_op = PUSH;
    };
};
// numbers
auto number = x3::lexeme[ x3::int_ ] [wrap_atom(Value::INT)];
auto var_identifier = (identifier >> !x3::lit("("))[wrap_atom(Value::VAR)];

auto atom = number | var_identifier;
// BOOST_SPIRIT_DEFINE( number, identifier, atom );


auto push_operator = [](auto& ctx){
    auto attr = x3::_attr(ctx);
    x3::_val(ctx)=attr;
    OpCode code = NOP;

    switch (attr){
    case '=':
        code = ASSIGN;
        break;
    case '+':
        code = ADD;
        break;
    case '-':
        code = SUB;
        break;
    case '*':
        code = MUL;
        break;
    case '/':
        code = DIV;
        break;
    }
    // last_op = code;
    program.push_back({code});
};
// operators
auto symbol_assign = x3::char_("=")[push_operator];
auto symbol_add = x3::char_("+")[push_operator];
auto symbol_sub = x3::char_("-")[push_operator];
auto symbol_mul = x3::char_("*")[push_operator];
auto symbol_div = x3::char_("/")[push_operator];
auto math_ops = symbol_add | symbol_sub | symbol_mul | symbol_div;

x3::rule<struct expression> expression;

// built in functions
auto print_num = x3::lit("print_num")[([](){
    last_op = CALL;
    deferred_functions.push_back("print_num");
})];

auto print_char = x3::lit("print_char")[([](){
    last_op = CALL;
    deferred_functions.push_back("print_char");
})];


auto call = (((print_num | print_char | identifier)-(x3::lit("def"))) > x3::lit("(") > expression > x3::lit(")") > statement_end)[([](auto& ctx){
    last_op = CALL;
    std::string name = x3::_attr(ctx);
    if (!name.empty())
        deferred_functions.push_back(name);
})];


auto expression_def = (atom | call) >> *(math_ops >> (atom | call));

BOOST_SPIRIT_DEFINE(expression);

auto op_assign = var_identifier >> (symbol_assign > expression > statement_end);


auto keyword_def = (x3::lexeme[(x3::lit("def")>>x3::space)])[([](){
    throw "\x1b[93mDEF VARIABLES TODO\x1b[0m\n";
    // last_op = DEF;
    // cout << "DEF!" << nl;
    program.push_back({
        .code = DEF,
    });
})];

auto def = (keyword_def > identifier > x3::lit("(") > (identifier % ",") > x3::lit(")"))[([](auto& ctx){
    throw "\x1b[93mDEF VARIABLES TODO\x1b[0m\n";
    // last_op = DEF;
    cout << "FUNCTION DEF" << nl;
    auto attr = x3::_attr(ctx);

    auto name = boost::fusion::at_c<1>(attr);

    auto args = boost::fusion::at_c<2>(attr);
    cout << "NAME: " << name << nl;
    for (auto a: args)
        cout << "ARG: "  << a << nl;
    // cout << x3::_attr(ctx);
})];



auto syntax =   op_assign   |
                // def         |
                call        ;

auto valid_program = +syntax >> statement_end;

std::string src;
auto file_name = "test1.spirit";

int main() {
    // read file
    {
        std::ifstream src_file{file_name};
        src.assign(std::istreambuf_iterator<char>{src_file}, std::istreambuf_iterator<char>{});
    }

    auto iter = src.begin();
    auto iter_end = src.end();
    bool success;

    using atom_type = std::variant<int,std::string>;
    std::tuple<std::string, char, atom_type> expr;

    try{

        success = x3::phrase_parse(iter, iter_end, valid_program, x3::blank);
    } catch(boost::spirit::x3::expectation_failure<std::string::iterator> e) {
        // distance(begin(src), e.where()) give's the position in the string where the error occurs
        // could iterate through src incrementing col and row (at newline) until it equals e.where()
        // this would give the correct row and col
        auto location = std::distance(src.begin(), e.where());


        if(*(e.where()-1) == '('){
            cout << "Missing Expression for function call at " << location << nl;
        } else {
            cout << "ERROR!: expected " << e.which() << " at " << location  << nl;
        }
        
        return 1;
    }


    cout << "parsed all: " << std::boolalpha << (iter == iter_end) << nl;
    cout << std::boolalpha << success << nl;

    // cout << "** PROGRAM **" << nl;
    // for (auto& op: program)
    //     cout << op << nl;

    std::map<std::string, int> variables;

    auto get_val = [&](Value& v)->std::optional<int> {
        if (v.type == Value::INT) {
            return std::get<Value::INT>(v.value);
        } else {
            auto name = std::get<Value::VAR>(v.value);
            if (variables.count(name)){
                return variables[name];
                // if (v.type == Value::INT){
                // } else {
                //     return get_val(v);
                // }
            } else
                return std::nullopt;
        }
    };

    auto peek = [](int i)->Op{ 
        if (program.size() > i + 1) {
            return program[i+1];
        } else 
            return {NOP};
    };

    std::vector<int> stack{};
    int top = -1;

    auto push = [&](int v) {
        int size = stack.size();
        ++top;
        if(top >= size){
            // cout << "ADDING FOR: " << top << " stacK: " << size << nl;
            stack.push_back(0);
        }
        stack[top]=v;

    };

    auto pop = [&]()->std::optional<int>{ 
        if (top > -1) {
            return stack[top--];
        } else
            return std::nullopt;
    };

    auto pop_binary = [&](int& i)->std::optional<std::tuple<int,int>>{
        using std::nullopt;

        int a; 
        int b; 
        if (auto a_opt = pop()){
            a = *a_opt; 
            auto next = peek(i++); 
            if (next.code == PUSH){ 
                i++; 
                if(auto b_opt = get_val(next.value)) {
                    b = *b_opt;
                    top++;
                } else {
                    cout << "UNKNOWN VARIABLE: " << std::get<Value::VAR>(next.value.value);
                    return nullopt;
                }

            } else {
                cout << "INSUFFICIENT ARGS FOR BINARY OP" << nl; 
                return nullopt;
            } 
        } else {
            cout << "INSUFFICIENT ARGS FOR BINARY OP" << nl; 
            return nullopt; 
        }

        return std::tuple{ a, b };
    };

    cout << "\n** EXECUTE PROGRAM ** \n";
    for (auto i = 0; i < program.size();) {


        auto op = program[i];
        switch(op.code) {
        case PUSH: {
            auto next = peek(i++);
            if (next.code == ASSIGN){
                next = peek(i++);
                // NOTE, here I'm only getting the next value, I should be going until the next END and then evaluating all that
                if (next.code == PUSH){
                    i++;
                    auto var_name = std::get<Value::VAR>(op.value.value);
                    // cout << "VAR NAME: " << var_name << nl;
                    if(auto val = get_val(next.value)){
                        // cout << "VALUE: " << *val  << nl;
                        variables[var_name]=*val;
                    } else {
                        cout << "UNKNOWN VARIABLE IN ASSIGNMENT: " << std::get<Value::VAR>(next.value.value) << nl;
                        return 1;
                    }
                }
            } else {
                if (auto val = get_val(op.value)){
                    // cout << "PUSHING VALUE: " << *val << nl;
                    push(*val);
                } else {
                    cout << "REFERENCED UNKNOWN VARIABLE: " << std::get<Value::VAR>(op.value.value) << nl;
                    return 1;
                }
            }
            break;
        }
        case CALL: {
            auto func_name = std::get<Value::VAR>(op.value.value);
            i++;
            // cout << "FUNC: " <<func_name << nl;
            if (func_name == "print_num") {
                if(auto arg = pop()){
                    cout << *arg << nl;
                } else  {
                    cout << "MISSING ARG FOR PRINT_NUM" << nl;
                    return 1;
                }
            } else if(func_name == "print_char") {
                // cout << "CHAR" << nl;
                if(auto arg = pop()){ {
                    // cout << "ARG: " << *arg << nl;
                    cout << (char)*arg;
                }
                } else  {
                    cout << "MISSING ARG FOR PRINT_CHAR" << nl;
                    return 1;
                }
            }
            break;
        }
        case ADD: {
            if( auto args = pop_binary(i)){
                auto [a, b] = *args;
                push(a + b);
            }
            else return 1;
            break;
        }
        case SUB: {
            if( auto args = pop_binary(i)){
                auto [a, b] = *args;
                push(a - b);
            }
            else return 1;
            break;
        }
        case MUL: {
            if( auto args = pop_binary(i)){
                auto [a, b] = *args;
                push(a * b);
            }
            else return 1;
            break;
        }
        case DIV: {
            if( auto args = pop_binary(i)){
                auto [a, b] = *args;
                if (b == 0) {
                    cout << "DIVIDE BY 0!" << nl;
                    return 1;
                }
                push(a / b);
            }
            else return 1;
            break;
        }
        case END:
            i++;
            break;
        default:
            i++;
            cout << "\x1b[93mUNHANDLED: " << op << nl << "\x1b[0m";
        }

    }
    // cout << "** VARS ** " << nl;
    // for (auto& [k, v]: variables)
    //     cout << k << ": "  << v <<nl;

    // cout << "\n** STACK ** " << nl;
    // cout << "TOP: " << top << nl;
    // for (auto i = 0; i < stack.size(); i++) {
    //     auto v = stack[i];
    //     cout << v; 
    //     if(i == top) 
    //         cout << " <- top";
    //     cout<<nl;
        
    // }
}

