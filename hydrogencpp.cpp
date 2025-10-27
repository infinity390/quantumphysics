#include <stack>
#include <iostream>
#include <vector>
#include <map>
#include <cmath>
#include <string>
#include <algorithm>
#include <functional>
#include <sstream>
#include <stdexcept>
#include <unordered_map>
#include <tuple>

class BigInt {
private:
    // digits stored least-significant first, each char '0'..'9'
    std::string digits;
    bool negative = false;

    void trim() {
        while (digits.size() > 1 && digits.back() == '0') digits.pop_back();
        if (digits.empty()) digits = "0";
        if (digits == "0") negative = false;
    }

    // compare absolute values: -1 if |a|<|b|, 0 if equal, +1 if |a|>|b|
    static int abs_compare(const BigInt& a, const BigInt& b) {
        if (a.digits.size() != b.digits.size())
            return a.digits.size() < b.digits.size() ? -1 : 1;
        for (int i = static_cast<int>(a.digits.size()) - 1; i >= 0; --i) {
            if (a.digits[i] != b.digits[i])
                return (a.digits[i] < b.digits[i]) ? -1 : 1;
        }
        return 0;
    }

public:
    int to_int(){
        if (negative){
            return -std::stoi(digits);
        } else {
            return std::stoi(digits);
        }
    }
    double to_double(){
        if (negative){
            return -std::stod(digits);
        } else {
            return std::stod(digits);
        }
    }
    // ----- constructors -----
    BigInt(long long val = 0) {
        negative = val < 0;
        unsigned long long v = static_cast<unsigned long long>(negative ? -val : val);
        if (v == 0) digits = "0";
        else {
            digits.clear();
            while (v > 0) {
                digits.push_back(char('0' + (v % 10)));
                v /= 10;
            }
        }
        trim();
    }

    BigInt(const std::string& s) {
        if (s.empty()) throw std::invalid_argument("Empty string");
        std::string t = s;
        negative = false;
        if (t[0] == '-') {
            negative = true;
            t = t.substr(1);
            if (t.empty()) throw std::invalid_argument("Invalid string");
        }
        for (char c : t) if (c < '0' || c > '9') throw std::invalid_argument("Invalid character in string");
        digits = t;
        std::reverse(digits.begin(), digits.end());
        trim();
    }

    // copy/move default OK

    // ----- helpers -----
    BigInt abs() const {
        BigInt r = *this;
        r.negative = false;
        return r;
    }
    static BigInt abs(const BigInt& x) { return x.abs(); }

    std::string str() const {
        std::string s = digits;
        std::reverse(s.begin(), s.end());
        if (negative && s != "0") s.insert(s.begin(), '-');
        return s;
    }

    // quick constructor from single digit (0..9)
    static BigInt from_digit(int d) {
        if (d < 0 || d > 9) throw std::invalid_argument("digit out of range");
        BigInt r;
        r.digits = std::string(1, char('0' + d));
        r.negative = false;
        r.trim();
        return r;
    }

    // multiply by small int (0..99...) - returns new BigInt
    BigInt mul_small(int m) const {
        if (m == 0) return BigInt(0);
        if (m < 0) return (*this).mul_small(-m).operator-();
        BigInt res;
        res.negative = negative;
        res.digits.assign(digits.size() + 10, '0'); // allocate enough
        int carry = 0;
        size_t k = 0;
        for (size_t i = 0; i < digits.size() || carry; ++i) {
            int d = (i < digits.size()) ? (digits[i] - '0') : 0;
            long long cur = 1LL * d * m + carry;
            res.digits[k++] = char('0' + (cur % 10));
            carry = static_cast<int>(cur / 10);
        }
        res.digits.resize(k);
        res.trim();
        return res;
    }

    // ----- comparison operators -----
    bool operator==(const BigInt& other) const { return negative == other.negative && digits == other.digits; }
    bool operator!=(const BigInt& other) const { return !(*this == other); }

    bool operator<(const BigInt& other) const {
        if (negative != other.negative) return negative;
        int cmp = abs_compare(*this, other);
        return (!negative) ? (cmp < 0) : (cmp > 0);
    }
    bool operator>(const BigInt& other) const { return other < *this; }
    bool operator<=(const BigInt& other) const { return !(*this > other); }
    bool operator>=(const BigInt& other) const { return !(*this < other); }

    // ----- unary -----
    BigInt operator-() const {
        BigInt r = *this;
        if (r.digits == "0") r.negative = false;
        else r.negative = !r.negative;
        return r;
    }

    // ----- addition -----
    BigInt operator+(const BigInt& other) const {
        if (negative == other.negative) {
            BigInt res;
            res.negative = negative;
            res.digits.clear();

            int carry = 0;
            size_t n = std::max(digits.size(), other.digits.size());
            for (size_t i = 0; i < n || carry; ++i) {
                int d1 = (i < digits.size()) ? (digits[i] - '0') : 0;
                int d2 = (i < other.digits.size()) ? (other.digits[i] - '0') : 0;
                int sum = d1 + d2 + carry;
                res.digits.push_back(char('0' + (sum % 10)));
                carry = sum / 10;
            }
            res.trim();

            return res;
        } else {
            BigInt tmp = other;
            tmp.negative = !tmp.negative;
            BigInt f = *this - tmp;

            return f;
        }
    }

    // ----- subtraction -----
    BigInt operator-(const BigInt& other) const {
        if (negative != other.negative) {
            BigInt tmp = other;
            tmp.negative = !tmp.negative;
            return *this + tmp;
        }

        int cmp = abs_compare(*this, other);
        if (cmp == 0) return BigInt(0);

        const BigInt *larger, *smaller;
        bool result_neg;
        if (cmp > 0) { larger = this; smaller = &other; result_neg = negative; }
        else         { larger = &other; smaller = this; result_neg = !negative; }

        BigInt res;
        res.negative = result_neg;
        res.digits.assign(larger->digits.size(), '0');

        int borrow = 0;
        for (size_t i = 0; i < larger->digits.size(); ++i) {
            int ld = larger->digits[i] - '0';
            int sd = (i < smaller->digits.size()) ? (smaller->digits[i] - '0') : 0;
            int diff = ld - sd - borrow;
            if (diff < 0) { diff += 10; borrow = 1; } else borrow = 0;
            res.digits[i] = char('0' + diff);
        }
        res.trim();
        return res;
    }

    // ----- multiplication (big * big) -----
    BigInt operator*(const BigInt& other) const {
        BigInt res;
        res.digits.assign(digits.size() + other.digits.size(), '0');
        for (size_t i = 0; i < digits.size(); ++i) {
            int carry = 0;
            int d1 = digits[i] - '0';
            for (size_t j = 0; j < other.digits.size() || carry; ++j) {
                int d2 = (j < other.digits.size()) ? (other.digits[j] - '0') : 0;
                int cur = (res.digits[i + j] - '0') + d1 * d2 + carry;
                res.digits[i + j] = char('0' + (cur % 10));
                carry = cur / 10;
            }
        }
        res.negative = negative != other.negative;
        res.trim();
        return res;
    }

    // ----- division (exact) -----
    // performs integer division; throws if remainder != 0
    BigInt operator/(const BigInt& divisor) const {
        if (divisor == BigInt(0)) throw std::runtime_error("Divide by zero");

        BigInt a = this->abs();
        BigInt b = divisor.abs();
        if (abs_compare(a, b) < 0) {
            if (a == BigInt(0)) return BigInt(0);
            throw std::runtime_error("Not divisible exactly");
        }

        BigInt q;
        q.digits.assign(a.digits.size(), '0');
        BigInt r(0);

        for (int i = static_cast<int>(a.digits.size()) - 1; i >= 0; --i) {
            // r = r * 10 + digit(a, i)
            r = r.mul_small(10);
            int addd = a.digits[i] - '0';
            if (addd) r = r + BigInt(addd);

            // find x in [0..9] such that b * x <= r < b*(x+1)
            int x = 0, lo = 0, hi = 9;
            while (lo <= hi) {
                int mid = (lo + hi) >> 1;
                BigInt t = b.mul_small(mid);
                if (t <= r) { x = mid; lo = mid + 1; }
                else hi = mid - 1;
            }
            q.digits[i] = char('0' + x);
            if (x) r = r - b.mul_small(x);
        }

        q.trim();
        r.trim();
        if (!(r.digits.size() == 1 && r.digits[0] == '0')) throw std::runtime_error("Not divisible exactly");
        q.negative = negative != divisor.negative;
        return q;
    }

    // ----- modulo -----
    BigInt operator%(const BigInt& m) const {
        BigInt dividend = this->abs();
        BigInt divisor = m.abs();
        if (abs_compare(dividend, divisor) < 0) return dividend;

        BigInt rem(0);
        for (int i = static_cast<int>(dividend.digits.size()) - 1; i >= 0; --i) {
            rem = rem.mul_small(10);
            int addd = dividend.digits[i] - '0';
            if (addd) rem = rem + BigInt(addd);

            // subtract as many times as possible
            int x = 0;
            while (abs_compare(rem, divisor) >= 0) {
                // we can speed this by using mul_small + binary search but simple loop is fine
                rem = rem - divisor;
                ++x;
                // If many subtracts are needed, consider using binary search instead
            }
        }
        rem.trim();
        return rem;
    }

    // ----- gcd -----
    static BigInt gcd(BigInt a, BigInt b) {
        a = a.abs();
        b = b.abs();
        while (!(b.digits.size() == 1 && b.digits[0] == '0')) {
            BigInt r = a % b;
            a = b;
            b = r;
        }
        return a;
    }
};

// ostream helper
inline std::ostream& operator<<(std::ostream& os, const BigInt& x) {
    os << x.str();
    return os;
}

BigInt ipow(BigInt base, int exp) {
    BigInt result = BigInt(1);
    int orig = exp;
    BigInt origbase = base;
    if (exp == 0) return result;
    while (exp > 0) {
        if (exp % 2 == 1) result = result * base;
        base = base * base;
        exp /= 2;
    }

    return result;
}
int ipow(int base, int exp) {
    int result = 1;
    if (exp == 0) return result;
    for (int i=0; i<exp; ++i) result = result * base;
    return result;
}

template <typename T = int>
class Fraction {
public:
    T num;
    T den;

    bool none = false;

    bool is_none(){
        return none;
    }
    Fraction(){
        none = true;
    }
    Fraction(const std::string& v){
        if (v == "none"){
            none = true;
        }
    }
    // Constructors
    Fraction(int n, int d) {
        if (d == 0) throw std::invalid_argument("Denominator cannot be zero");
        if (d < 0) { n = -n; d = -d; }

        int g = gcd(n, d);
        num = n / g;
        den = d / g;
    }
    Fraction(BigInt n, BigInt d) {
        if (d == BigInt(0)) throw std::invalid_argument("Denominator cannot be zero");
        if (d < BigInt(0)) { n = -n; d = -d; }
        BigInt g = gcdfx(n, d);
        num = n / g;
        den = d / g;
    }
    Fraction(BigInt n, int d) {
        Fraction x = Fraction(n,BigInt(1))*gen_zero(d);

        num = x.num;
        den = x.den;
    }
    Fraction operator^(int exp) const {

        Fraction<T> tmp = equal_to_zero(num);

        if (!(tmp == Fraction<T>())) return tmp;

        if (exp >= 0)
            return Fraction(ipow(num, exp), ipow(den, exp));
        else
            return Fraction(ipow(den, -exp), ipow(num, -exp));
    }
    // Addition
    Fraction operator+(const Fraction& other) const {
        T n = num * other.den + other.num * den;
        T d = den * other.den;

        return Fraction(n, d);
    }

    // Subtraction
    Fraction operator-(const Fraction& other) const {
        T n = num * other.den - other.num * den;
        T d = den * other.den;
        return Fraction(n, d);
    }

    // Multiplication
    Fraction operator*(const Fraction& other) const {
        T n = num * other.num;
        T d = den * other.den;
        return Fraction(n, d);
    }

    // Division
    Fraction operator/(const Fraction& other) const {
        if (other.num == 0) throw std::invalid_argument("Division by zero");
        T n = num * other.den;
        T d = den * other.num;
        return Fraction(n, d);
    }

    // Equality
    bool operator==(const Fraction& other) const {
        if (none && other.none) return true;
        return num == other.num && den == other.den;
    }

    bool operator!=(const Fraction& other) const {
        return !((*this)==other);
    }
    // Less-than (useful for sorting)
    bool operator<(const Fraction& other) const {
        return num * other.den < other.num * den;
    }

    // Convert to double
    double to_double() const {
        return static_cast<double>(num) / static_cast<double>(den);
    }
private:
    static int gcd(int a, int b) {
        while (b != 0) {
            int temp = b;
            b = a % b;
            a = temp;
        }
        return (a < 0) ? -a : a; // always positive
    }

    static Fraction<BigInt> equal_to_zero(BigInt x) {
        if (x == BigInt(0)) return Fraction<BigInt>(BigInt(0),BigInt(1));
        return Fraction<BigInt>("none");
    }
    static Fraction<int> equal_to_zero(int x){
        if (x == 0) return Fraction<int>(0,1);
        return Fraction<int>("none");
    }
    Fraction gen_zero(int n){
        BigInt x = BigInt(1);
        if (n > 0){
            for (int i=0; i<n; ++i){
                x = x * BigInt(10);
            }
        } else if (n < 0){
            for (int i=0; i<(-n); ++i){
                x = x * BigInt(10);
            }
            return Fraction(BigInt(1), x);
        }
        return Fraction(x, BigInt(1));

    }
    int gcdfx(int a, int b){
        return gcd(a, b);
    }
    BigInt gcdfx(BigInt a, BigInt b){
        return BigInt::gcd(a, b);
    }

};


class TreeNode {
public:
    enum NodeType { CONSTANT, NUMBER, VARIABLE, FUNCTION };
    NodeType type;
    std::string value;
    std::vector<TreeNode> children;

    TreeNode() : type(VARIABLE), value("none") {}
    TreeNode(NodeType t, const std::string& v) : type(t), value(v) {}
    TreeNode(const std::string& v, const std::vector<TreeNode>& ch) : type(FUNCTION), value(v), children(ch) {}
    TreeNode(NodeType t, const std::string& v, const std::vector<TreeNode>& ch) : type(t), value(v), children(ch) {}

    void add_child(const TreeNode& child) { children.push_back(child); }

    TreeNode operator+(const TreeNode& other) const { return make_op("+", other); }
    TreeNode operator-(const TreeNode& other) const { return make_op("-", other); }
    TreeNode operator-() const { return (*this)*TreeNode(NUMBER, "-1"); }
    TreeNode operator*(const TreeNode& other) const { return make_op("*", other); }
    TreeNode operator/(const TreeNode& other) const { return (*this)*(other^TreeNode(NUMBER, "-1")); }
    TreeNode operator^(const TreeNode& other) const { return make_op("^", other); }
    TreeNode fx(std::string s) const {
        std::vector<TreeNode> new_children;
        new_children.push_back(*this);
        return TreeNode(TreeNode::FUNCTION, s, new_children);
    }

    std::string str(int depth = 0) const {
        std::string indent(depth, ' ');
        std::string s;

        if (type == FUNCTION) {
            s += indent + "f_" + value + "\n";
        } else if (type == VARIABLE) {
            s += indent + "v_" + value + "\n";
        } else if (type == NUMBER) {
            s += indent + "d_" + value + "\n";
        } else if (type == CONSTANT) {
            s += indent + "s_" + value + "\n";
        }

        for (const auto& child : children) {
            s += child.str(depth + 1);
        }

        return s;
    }


    std::string render() const {
        if (type == NUMBER || type == VARIABLE)
            return value;
        if (type == FUNCTION && !(value == "+" || value == "-" || value == "*" || value == "/" || value == "^")) {
            std::string result = value + "(";
            for (size_t i = 0; i < children.size(); ++i) {
                std::string arg = children[i].render();
                if (arg.size() > 1 && arg.front() == '(' && arg.back() == ')') {
                    arg = arg.substr(1, arg.size() - 2);
                }
                result += arg;
                if (i < children.size() - 1)
                    result += ",";
            }
            result += ")";
            return result;
        }
        if (children.empty()) return value;
        if (children.size() == 1) return value + children[0].render();

        std::string result;
        for (size_t i = 0; i < children.size(); ++i) {
            result += children[i].render();
            if (i < children.size() - 1)
                result += value;
        }
        return "(" + result + ")";
    }
    bool operator==(const TreeNode& other) const {
        TreeNode lhs = *this;
        TreeNode rhs = other;
        lhs.recursive_sort();
        rhs.recursive_sort();
        return lhs.str() == rhs.str();
    }
    void recursive_sort() {
        for (auto& child : children) child.recursive_sort();
        if (value == "+" || value == "*") {
            std::sort(children.begin(), children.end(), [](const TreeNode& a, const TreeNode& b) {
                return a.str() < b.str();
            });
        }
    }

private:
    TreeNode make_op(const std::string& op, const TreeNode& other) const {
        TreeNode node(FUNCTION, op);
        node.add_child(*this);
        node.add_child(other);
        return node;
    }
};

class Parser {
    std::string s;
    size_t pos = 0;

    char peek() { return pos < s.size() ? s[pos] : '\0'; }
    char get() { return pos < s.size() ? s[pos++] : '\0'; }
    void skip() { while (isspace(peek())) get(); }

    TreeNode number() {
        skip();
        std::string num;
        while (isdigit(peek()) || peek() == '.') num += get();
        return TreeNode(TreeNode::NUMBER, num);
    }

    TreeNode variable_or_function() {
        skip();
        std::string name;
        while (isalpha(peek()) || isdigit(peek()) || peek() == '_')
            name += get();

        skip();
        // If the next character is '(', it’s a function
        if (peek() == '(') {
            get(); // consume '('
            std::vector<TreeNode> args;

            // parse comma-separated arguments
            skip();
            if (peek() != ')') {
                while (true) {
                    args.push_back(expr());
                    skip();
                    if (peek() == ',') {
                        get();
                        skip();
                    } else break;
                }
            }
            if (peek() == ')') get(); // consume ')'
            return TreeNode(TreeNode::FUNCTION, name, args);
        }
        // Otherwise, it’s just a variable
        if (name == "e" || name == "pi" || name == "inf" || name == "hbar"
            || (name.size() == 2 && name[0] == 'e')){
            return TreeNode(TreeNode::CONSTANT, name);
        }
        return TreeNode(TreeNode::VARIABLE, name);
    }

    TreeNode factor() {
        skip();
        if (isdigit(peek())) return number();
        if (isalpha(peek())) return variable_or_function();
        if (peek() == '(') {
            get();
            TreeNode n = expr();
            if (peek() == ')') get();
            return n;
        }
        return TreeNode(); // default
    }

    TreeNode power() {
        TreeNode left = factor();
        skip();
        if (peek() == '^') {
            get();
            left = left ^ power();
        }
        return left;
    }

    TreeNode term() {
        TreeNode node = power();
        skip();
        while (peek() == '*' || peek() == '/') {
            char op = get();
            node = (op == '*') ? node * power() : node / power();
            skip();
        }
        return node;
    }

    TreeNode expr() {
        TreeNode node = term();
        skip();
        while (peek() == '+' || peek() == '-') {
            char op = get();
            node = (op == '+') ? node + term() : node - term();
            skip();
        }
        return node;
    }

public:
    Parser(const std::string& str) : s(str) {}

    TreeNode parse() {
        return expr();
    }
};
struct PyLike {
    TreeNode node;
    std::string str_val;  // holds "const"
    int coef = 1;
    bool is_const = false;

    PyLike() {}
    PyLike(const int x, const TreeNode& n) : coef(x), node(n), is_const(false) {}
    PyLike(const int x, const std::string& s) : coef(x), str_val(s), is_const(true) {}
};

// Utility to get key for map
std::string key(const PyLike& x) {
    if (x.is_const) return x.str_val;
    return x.node.value;
}

Fraction<> calculate(const TreeNode& eq);
TreeNode solve(const TreeNode& eq);
bool illegal_expr(const TreeNode& eq);
TreeNode tree_from_str(const std::string& s) {
    std::istringstream iss(s);
    std::string line;

    struct StackItem {
        TreeNode* node;
        int depth;
    };
    std::vector<StackItem> stack;

    TreeNode root;

    while (std::getline(iss, line)) {
        // Count leading spaces to determine depth
        int spaces = 0;
        while (spaces < line.size() && line[spaces] == ' ') ++spaces;
        int depth = spaces; // 1 space = 1 depth

        line = line.substr(spaces);

        if (line.size() < 2 || line[1] != '_')
            throw std::runtime_error("Invalid format");

        char prefix = line[0];
        std::string val = line.substr(2);

        TreeNode::NodeType type;
        if (prefix == 'f') type = TreeNode::FUNCTION;
        else if (prefix == 'v') type = TreeNode::VARIABLE;
        else if (prefix == 'd') type = TreeNode::NUMBER;
        else if (prefix == 's') type = TreeNode::CONSTANT;
        else throw std::runtime_error("Unknown prefix");

        TreeNode node(type, val);

        if (stack.empty()) {
            root = node;
            stack.push_back({&root, depth});
        } else {
            // Pop stack until we find parent at depth-1
            while (!stack.empty() && stack.back().depth >= depth)
                stack.pop_back();

            if (stack.empty())
                throw std::runtime_error("Invalid indentation");

            stack.back().node->add_child(node);

            // **Store pointer to the newly added child safely**
            stack.push_back({&(stack.back().node->children.back()), depth});
        }
    }

    return root;
}

template<typename T>
void permute_helper(std::vector<T>& arr, int l, int r, std::vector<std::vector<T>>& result) {
    if (l == r) {
        result.push_back(arr);
        return;
    }
    for (int i = l; i <= r; ++i) {
        std::swap(arr[l], arr[i]);
        permute_helper(arr, l + 1, r, result);
        std::swap(arr[l], arr[i]); // backtrack
    }
}

template<typename T>
std::vector<std::vector<T>> permutations(const std::vector<T>& vec) {
    std::vector<std::vector<T>> result;
    if (vec.empty()) return result;
    std::vector<T> arr = vec;
    permute_helper(arr, 0, arr.size() - 1, result);
    return result;
}
bool all_lower(const std::string& s){
    for (size_t i = 0; i < s.size(); ++i) {
        if (!std::islower(static_cast<unsigned char>(s[i]))) {
            return false;
        }
    }
    return true;
}
TreeNode number(int n){
    return TreeNode(TreeNode::NUMBER, std::to_string(n));
}
TreeNode variable(std::string n){
    return TreeNode(TreeNode::VARIABLE, n);
}
TreeNode constant(std::string n){
    return TreeNode(TreeNode::CONSTANT, n);
}
TreeNode conversion(const TreeNode& node) {
    TreeNode n = node;
    if (n.value == "+") n.value = "w+";
    else if (n.value == "*") n.value = "w*";

    std::vector<TreeNode> children;
    for (auto& c : n.children)
        children.push_back(conversion(c));
    return TreeNode(n.type, n.value, children);
}

// Reverse conversion
TreeNode conversionrev(const TreeNode& node) {
    TreeNode n = node;
    if (n.value == "w+") n.value = "+";
    else if (n.value == "w*") n.value = "*";

    std::vector<TreeNode> children;
    for (auto& c : n.children)
        children.push_back(conversionrev(c));
    return TreeNode(n.type, n.value, children);
}
bool helper2(const TreeNode& equation, const TreeNode& formula,
            std::map<std::string, TreeNode>& varlist) {

    // Check for variable placeholder
    if (formula.type == TreeNode::VARIABLE) {

        if (!all_lower(formula.value)) {
            auto it = varlist.find(formula.str());
            if (it != varlist.end()) {
                return it->second == equation;
            } else {
                // Assign variable
                varlist[formula.str()] = equation;
                return true;
            }
        }
    }

    // Name mismatch
    if (equation.value != formula.value) return false;

    // Children count mismatch
    if (equation.children.size() != formula.children.size()) return false;

    for (size_t i = 0; i < equation.children.size(); ++i) {
        if (!helper2(equation.children[i], formula.children[i], varlist))
            return false;
    }
    return true;
}

// Recursive helper to get all combinations
std::vector<TreeNode> helper(const TreeNode& node) {
    if (node.children.empty())
        return {node};

    std::vector<std::vector<TreeNode>> child_perms;
    std::vector<std::vector<TreeNode>> groups;

    if (node.value == "w+" || node.value == "w*") {
        groups = permutations(node.children); // Use our permutation function
    } else {
        groups.push_back(node.children);
    }

    std::vector<TreeNode> results;

    for (auto& children_group : groups) {
        std::vector<std::vector<TreeNode>> child_results;
        for (auto& child : children_group)
            child_results.push_back(helper(child));

        std::vector<std::vector<TreeNode>> combos = {{}}; // start with empty combo
        for (auto& vec : child_results) {
            std::vector<std::vector<TreeNode>> new_combos;
            for (auto& c1 : combos) {
                for (auto& c2 : vec) {
                    auto tmp = c1;
                    tmp.push_back(c2);
                    new_combos.push_back(tmp);
                }
            }
            combos = new_combos;
        }

        for (auto& combo : combos)
            results.push_back(TreeNode(node.type, node.value, combo));
    }

    return results;
}

std::vector<TreeNode> lst(const TreeNode& formula) {
    TreeNode formula2 = conversion(formula);
    std::vector<TreeNode> out;
    static std::map<std::string, std::vector<TreeNode>> database;
    std::string fs = formula2.str();
    if (database.count(fs)){
        return database[fs];
    }
    std::vector<TreeNode> data = helper(formula2);
    for (int i=0; i<data.size(); ++i){
        bool pass = true;
        for (int j=0; j<out.size(); ++j){
            if (out[j] == data[i]){
                pass = false;
                break;
            }
        }
        if (pass){
            out.push_back(data[i]);
        }
    }
    database[fs] = out;
    return out;
}

TreeNode replace_node(const TreeNode& root, const TreeNode& target, const TreeNode& replacement) {

    if (root == target) return replacement;

    // Otherwise, rebuild the node with replaced children
    std::vector<TreeNode> newChildren;
    for (size_t i = 0; i < root.children.size(); ++i) {
        newChildren.push_back(replace_node(root.children[i], target, replacement));
    }

    // Return a new node with the same value and replaced children
    return TreeNode(root.type, root.value, newChildren);
}
typedef std::map<std::string, TreeNode> VarMap;

TreeNode structure(const TreeNode& equation,
                   const TreeNode& formula,
                   const TreeNode& formula_out,
                   bool only_const = false,
                   std::function<bool(const TreeNode&)> const_fx = [](const TreeNode& x){
                       return x.str().find("v_") == std::string::npos;
                   })
{
    VarMap varlist;

    // Convert equation and output formula
    TreeNode eq_conv = conversion(equation);
    TreeNode fo_conv;
    fo_conv = conversion(formula_out);

    // Iterate over all permutations of formula
    for (TreeNode& item : lst(formula)) {

        varlist.clear();
        if (helper2(eq_conv, item, varlist)) { // pass varlist by reference
            if (only_const) {
                bool all_const = true;
                for (auto& kv : varlist) {
                    if (!const_fx(kv.second)) {
                        all_const = false;
                        break;
                    }
                }
                if (!all_const)
                    continue;
            }
            TreeNode fo_temp = fo_conv;
            for (auto& kv : varlist) {
                fo_temp = replace_node(fo_temp, tree_from_str(kv.first), kv.second);
            }
            return conversionrev(fo_temp);
        }
    }
    return TreeNode();
}

// Assuming permutation/product helpers exist
typedef std::vector<TreeNode> TreeList;
typedef std::vector<TreeList> TreeMatrix;

TreeNode product(const std::vector<TreeNode>& lst) {
    if (lst.empty())
        return TreeNode(TreeNode::NUMBER, "1");

    TreeNode s = lst[0];
    for (size_t i = 1; i < lst.size(); ++i)
        s = s * lst[i];

    return s;
}

std::vector<std::vector<TreeNode>> cart_product(const std::vector<std::vector<TreeNode>>& lists) {
    std::vector<std::vector<TreeNode>> result;

    if (lists.empty()) return result;
    result.push_back(std::vector<TreeNode>());

    for (const auto& lst : lists) {
        std::vector<std::vector<TreeNode>> temp;
        for (const auto& res_vec : result) {
            for (const auto& elem : lst) {
                std::vector<TreeNode> new_vec = res_vec;
                new_vec.push_back(elem);
                temp.push_back(new_vec);
            }
        }
        result = temp;
    }

    return result;
}

TreeNode transform_formula(TreeNode equation,
                           const TreeNode& wrt,
                           TreeMatrix formula_list,
                           const std::string& var,
                           TreeMatrix expr,
                           std::function<bool(const TreeNode&)> const_fx =
                               [](const TreeNode& x){ return x.render().find("v_") == std::string::npos; })
{
    std::string var2 = wrt.value;

    if (var != var2) {
        for (int i=0; i<formula_list.size(); ++i){
            for (int j=0; j<formula_list[i].size(); ++j){

                formula_list[i][j] = replace_node(formula_list[i][j], TreeNode(TreeNode::VARIABLE, "x"), wrt);
            }
        }
        for (int i=0; i<expr.size(); ++i){
            for (int j=0; j<expr[i].size(); ++j){
                expr[i][j] = replace_node(expr[i][j], TreeNode(TreeNode::VARIABLE, "x"), wrt);
            }
        }
    }

    for (auto& item_row : formula_list) {
        TreeList item = item_row;
        TreeList orig = item; // deep copy

        // Iterate over all combinations from expr
        for (auto& item2_combo : cart_product(expr)) { // product() should generate all combinations

            for (int i = 0; i < 2; ++i) {
                for (size_t j = 0; j < expr.size(); ++j) {
                    item[i] = replace_node(item[i], expr[j][0], item2_combo[j]);
                }
            }

            for (int i = 0; i < 2; ++i) {
                item[i] = solve(item[i]);
            }

            TreeNode out;
            bool p = !(var == "");
            out = structure(equation, item[0], item[1], p, const_fx);

            if (out.value != "none") { // non-empty result
                out = solve(out);
            }
            if (illegal_expr(out)){
                out = TreeNode();
            }

            if (out.value != "none") {

                return out;
            }

            item = orig; // reset item to original
        }
    }

    return TreeNode(); // corresponds to None in Python
}

// ---------------- Differentiation ----------------
bool contain(const TreeNode& eq, const TreeNode& what) {
    if (eq == what) return true;
    for (auto& child : eq.children) if (contain(child, what)) return true;
    return false;
}
bool contain(const std::vector<TreeNode>& eq, const TreeNode& what) {
    return contain(TreeNode(TreeNode::FUNCTION, "dummy", {eq}), what);
}
TreeNode diff(const TreeNode& eq, const std::string& var) {
    if (!contain(eq, TreeNode(TreeNode::VARIABLE, var)))
        return TreeNode(TreeNode::NUMBER, "0");

    if (eq.value == "+") {
        TreeNode r(TreeNode::NUMBER, "0");
        for (auto& c : eq.children)
            r = r + diff(c, var);
        return r;
    }

    if (eq.value == "*") {
        TreeNode r(TreeNode::NUMBER, "0");
        for (size_t i = 0; i < eq.children.size(); ++i) {
            TreeNode rest(TreeNode::NUMBER, "1");
            for (size_t j = 0; j < eq.children.size(); ++j)
                if (i != j) rest = rest * eq.children[j];
            r = r + (diff(eq.children[i], var) * rest);
        }
        return r;
    }

    if (eq.value == "^") {
        const TreeNode& base = eq.children[0];
        const TreeNode& power = eq.children[1];

        if (contain(power, TreeNode(TreeNode::VARIABLE, var))) {
            return (base ^ power) *
                   ((diff(base, var) * power / base) +
                    diff(power, var) * TreeNode("log", { base }));
        }

        if (power.type != TreeNode::NUMBER || std::stoi(power.value) != 1) {
            return TreeNode(TreeNode::NUMBER, power.value) *
                   (base ^ TreeNode(TreeNode::NUMBER, std::to_string(std::stoi(power.value) - 1))) *
                   diff(base, var);
        } else {
            return diff(eq.children[0], var);
        }
    }

    // --- FUNCTION DERIVATIVES ---
    if (eq.type == TreeNode::FUNCTION) {
        const std::string& f = eq.value;
        const TreeNode& g = eq.children[0];
        TreeNode gprime = diff(g, var);

        if (f == "sin")  return TreeNode("cos", { g }) * gprime;
        if (f == "cos")  return TreeNode(TreeNode::NUMBER, "-1") * TreeNode("sin", { g }) * gprime;
        if (f == "tan")  return (TreeNode("sec", { g }) ^ TreeNode(TreeNode::NUMBER, "2")) * gprime;
        if (f == "log")  return gprime / g;
        if (f == "sqrt") return gprime / (TreeNode(TreeNode::NUMBER, "2") * TreeNode("sqrt", { g }));
        if (f == "sec")  return TreeNode("sec", { g }) * TreeNode("tan", { g }) * gprime;
        if (f == "cosec")  return TreeNode(TreeNode::NUMBER, "-1") * TreeNode("cosec", { g }) * TreeNode("cot", { g }) * gprime;
        if (f == "cot")  return TreeNode(TreeNode::NUMBER, "-1") * (TreeNode("cosec", { g }) ^ TreeNode(TreeNode::NUMBER, "2")) * gprime;

        // unknown function
        return TreeNode("diff", { eq, TreeNode(TreeNode::VARIABLE, var) });
    }

    // Variable itself
    if (eq.type == TreeNode::VARIABLE)
        return (eq.value == var)
            ? TreeNode(TreeNode::NUMBER, "1")
            : TreeNode(TreeNode::NUMBER, "0");

    // Fallback for unrecognized cases
    return TreeNode("diff", { eq, TreeNode(TreeNode::VARIABLE, var) });
}

// ---------------- Utilities ----------------
void convert_to_basic(TreeNode& node) {
    if(node.type!=TreeNode::FUNCTION) return;
    for(auto& c: node.children) convert_to_basic(c);
    if (node.value == "e" || node.value == "pi" || node.value == "inf" || node.value == "hbar"
        || (node.value.size() == 2 && node.value[0] == 'e')) node.type = TreeNode::CONSTANT;
    if(node.value=="-") { node.value="+"; if(node.children.size()==2) node.children[1] = TreeNode("*",{TreeNode(TreeNode::NUMBER,"-1"), node.children[1]}); }
    if(node.value=="/") { node.value="*"; node.children[1]=TreeNode("^",{node.children[1], TreeNode(TreeNode::NUMBER,"-1")}); }
    if(node.value=="sqrt") node = TreeNode("^",{node.children[0], TreeNode(TreeNode::NUMBER,"2")^TreeNode(TreeNode::NUMBER,"-1")});
}

TreeNode flatten(const TreeNode& eq) {
    if(eq.value!="+" && eq.value!="*") return eq;
    std::vector<TreeNode> new_children;
    for(auto& c: eq.children){ TreeNode f = flatten(c); if(f.value==eq.value) new_children.insert(new_children.end(), f.children.begin(), f.children.end()); else new_children.push_back(f);}
    if(new_children.size()==1) return new_children[0];
    return TreeNode(eq.type,eq.value,new_children);
}

bool illegal_expr(const TreeNode& eq){
    if (eq.value == "^" && eq.children[0].value == "0"){
        Fraction<> n = calculate(eq.children[1]);
        if (!n.is_none()){
            if (n<Fraction<>(0,1)){
                return true;
            }
        }
    }
    for (int i=0; i<eq.children.size(); ++i){
        if (illegal_expr(eq.children[i])){
            return true;
        }
    }
    return false;
}
bool perfect_root(int n, int r, int &root) {
    if (r <= 0 || n < 0 && r % 2 == 0) return false; // invalid cases

    int lo = 0, hi = n > 1 ? n : 1;
    while (lo <= hi) {
        int mid = lo + (hi - lo) / 2;

        int pow_val = 1;
        for (int i = 0; i < r; ++i) {
            pow_val *= mid;
            if (pow_val > n) break;
        }

        if (pow_val == n) {
            root = mid;
            return true;
        } else if (pow_val < n) {
            lo = mid + 1;
        } else {
            hi = mid - 1;
        }
    }
    return false;
}
Fraction<> calculate(const TreeNode& eq) {
    if (eq.type == TreeNode::NUMBER) {
        return Fraction<>(std::stoi(eq.value), 1);
    }

    if (eq.value == "+") {
        Fraction<> result = calculate(eq.children[0]);

        if (result.is_none()) return Fraction<>("none");

        for (int i = 1; i < eq.children.size(); ++i) {
            Fraction<> tmp = calculate(eq.children[i]);

            if (tmp.is_none()) return Fraction<>("none");
            result = result + tmp;
        }
        return result;
    }

    else if (eq.value == "*") {
        Fraction<> result = calculate(eq.children[0]);

        if (result.is_none()) return Fraction<>("none");

        for (int i = 1; i < eq.children.size(); ++i) {
            Fraction<> tmp = calculate(eq.children[i]);

            if (tmp.is_none()) return Fraction<>("none");
            result = result * tmp;
        }
        return result;
    }

    else if (eq.value == "^") {
        Fraction<> base = calculate(eq.children[0]);
        Fraction<> exp  = calculate(eq.children[1]);

        if (base.is_none() || exp.is_none())
            return Fraction<>("none");

        // Handle invalid 0^0 or 0^negative cases
        if (base == Fraction<>(0,1) && (exp == Fraction<>(0,1) || exp < Fraction<>(0,1)))
            return Fraction<>("none");

        // Integer exponent
        if (exp.den == 1) {

            Fraction<> x = (base ^ (exp.num));

            return base ^ exp.num;
        }

        // Fractional exponent: try to extract roots
        int c, d;
        if (perfect_root(base.num, exp.den, c) && perfect_root(base.den, exp.den, d)) {
            Fraction<> rooted(c, d);
            return rooted ^ exp.num;
        }

        // Not a perfect root — cannot express as rational
        return Fraction<>("none");
    }

    return Fraction<>("none");
}


TreeNode frac_to_tree(Fraction<> g){
    Fraction<> f= Fraction<>(g.num, g.den);
    if (f.num == 0){
        return TreeNode(TreeNode::NUMBER, "0");
    }
    if (f.num == 1){
        return TreeNode(TreeNode::NUMBER, std::to_string(f.den))^number(-1);
    }
    if (f.den == 1){
        return TreeNode(TreeNode::NUMBER, std::to_string(f.num));
    } else {

        return TreeNode(TreeNode::NUMBER, std::to_string(f.num))/TreeNode(TreeNode::NUMBER, std::to_string(f.den));
    }
}
// ------------------ _solve ------------------
TreeNode _solve(const TreeNode& eq_input) {
    std::function<TreeNode(const TreeNode&)> solve_add;
    std::function<TreeNode(const TreeNode&)> solve_mul;
    std::function<TreeNode(const TreeNode&)> solve_u;
    std::function<TreeNode(const TreeNode&)> recur_solve;

    // ---------------- solve_add ----------------
    solve_add = [&](const TreeNode& equation) -> TreeNode {
        auto multiplied = [&](const TreeNode& eq) -> PyLike {
            if (eq.type == TreeNode::NUMBER) return PyLike(std::stoi(eq.value), "const");
            if (eq.value == "*") {
                int coef = 1;
                std::vector<TreeNode> new_children = eq.children;
                for (int i = new_children.size() - 1; i >= 0; --i) {
                    if (new_children[i].type == TreeNode::NUMBER) {
                        coef *= std::stoi(new_children[i].value);
                        new_children.erase(new_children.begin() + i);
                    }
                }
                if (new_children.empty()) return PyLike(coef, "const");
                if (new_children.size() == 1) {
                    PyLike res(coef, new_children[0]);
                    return res;
                }
                PyLike res(coef, TreeNode("*", new_children));
                return res;
            }
            PyLike res(1, eq);
            return res;
        };
        TreeNode eq = equation;
        if (eq.value != "+") return eq;
        for (int i=eq.children.size()-1; i>=0; --i){
            if (eq.children[i] == TreeNode(TreeNode::NUMBER, "0")){
                eq.children.erase(eq.children.begin() + i);
            }
        }
        if (eq.children.size() == 1){
            return eq.children[0];
        }
        if (eq.children.size() == 0){
            return TreeNode(TreeNode::NUMBER, "0");
        }
        std::map<std::string, int> dic;
        dic["const"] = 0;
        for (const auto& child : eq.children) {
            PyLike tmp = multiplied(child);
            if (tmp.is_const) dic["const"] += tmp.coef;
            else if (dic.count(tmp.node.str())) dic[tmp.node.str()] += tmp.coef;
            else dic[tmp.node.str()] = tmp.coef;
        }
        std::vector<TreeNode> children;
        for (auto& it : dic) {
            const std::string& k = it.first;
            int n = it.second;
            if (n == 0) continue;
            if (k == "const") children.push_back(TreeNode(TreeNode::NUMBER, std::to_string(n)));
            else {
                TreeNode tmp = tree_from_str(k);

                if (n != 1) children.push_back(tmp * TreeNode(TreeNode::NUMBER, std::to_string(n)));
                else children.push_back(tmp);
            }
        }
        if (children.size() == 1){
            if (children[0].type == TreeNode::FUNCTION && children.empty()) return TreeNode(TreeNode::NUMBER, "0");
            return children[0];
        }
        if (children.empty()) return TreeNode(TreeNode::NUMBER, "0");
        return TreeNode("+", children);
    };
    solve_mul = [&](const TreeNode& equation) -> TreeNode {
        auto multiplied = [&](const TreeNode& eq) -> PyLike {
            if (eq.type == TreeNode::NUMBER) return PyLike(std::stoi(eq.value), "const");
            if (eq.value != "^" || eq.children[1].type != TreeNode::NUMBER) {
                PyLike res(1, eq);
                return res;
            }
            PyLike res(std::stoi(eq.children[1].value), eq.children[0]);
            return res;
        };
        TreeNode eq = equation;
        if (eq.value == "^") {
            if (eq.children[1].value == "1") return eq.children[0];
            return eq;
        }
        if (eq.value != "*") return eq;

        for (int i=eq.children.size()-1; i>=0; --i){
            if (eq.children[i] == TreeNode(TreeNode::NUMBER, "0")){
                return TreeNode(TreeNode::NUMBER, "0");
            } else if (eq.children[i] == TreeNode(TreeNode::NUMBER, "1")){
                eq.children.erase(eq.children.begin() + i);
            }
        }

        if (eq.children.size() == 1){

            return eq.children[0];
        }
        if (eq.children.size() == 0){

            return TreeNode(TreeNode::NUMBER, "1");
        }
        std::map<std::string, int> dic;
        dic["const"] = 1;

        for (const auto& child : eq.children) {
            PyLike tmp = multiplied(child);
            if (tmp.is_const) dic["const"] *= tmp.coef;
            else if (dic.count(tmp.node.str())) dic[tmp.node.str()] += tmp.coef;
            else dic[tmp.node.str()] = tmp.coef;
        }

        if (dic["const"] == 0) return TreeNode(TreeNode::NUMBER, "0");

        std::vector<TreeNode> children;
        for (auto& it : dic) {
            const std::string& k = it.first;
            int n = it.second;
            if (n == 0) continue;
            if (k == "const") {
                if (n != 1) children.push_back(TreeNode(TreeNode::NUMBER, std::to_string(n)));
            } else {
                TreeNode tmp = tree_from_str(k);
                if (n != 1) children.push_back(tmp ^ TreeNode(TreeNode::NUMBER, std::to_string(n)));
                else children.push_back(tmp);
            }
        }
        if (children.size() == 1) return children[0];
        return TreeNode("*", children);
    };
    solve_u = [&](const TreeNode& eq) -> TreeNode {
        if (eq.value == "*" || eq.value == "^") return solve_mul(eq);
        TreeNode tmp = solve_add(eq);
        return tmp;
    };
    recur_solve = [&](const TreeNode& eq) -> TreeNode {

        TreeNode eq_solved = solve_u(eq);

        eq_solved = flatten(eq_solved);

        std::vector<TreeNode> rec_children;
        for (auto& child : eq_solved.children)
            rec_children.push_back(recur_solve(child));

        return TreeNode(eq_solved.type, eq_solved.value, rec_children);
    };

    return recur_solve(eq_input);
}
TreeNode solve(const TreeNode& eq){
    TreeNode current = eq;
    while(true){
        TreeNode simplified = _solve(current);
        if(simplified==current) break;
        current = simplified;
    }
    return current;
}
typedef Fraction<BigInt> SciNumber;
SciNumber compute2(const TreeNode &eq) {
    if (eq.children.empty()) {
        if (eq.type == TreeNode::CONSTANT && eq.value == "hbar") return SciNumber(BigInt("1054571817"), -43);
        if (eq.type == TreeNode::CONSTANT && eq.value == "ek")   return SciNumber(BigInt("8987551787"), 0);
        if (eq.type == TreeNode::CONSTANT && eq.value == "ec")   return SciNumber(BigInt("1602176634"), -28);  // 1.602176634e-19 C
        if (eq.type == TreeNode::CONSTANT && eq.value == "em")   return SciNumber(BigInt("9109383701"), -40);  // 9.1093837015e-31 kg
        if (eq.type == TreeNode::CONSTANT && eq.value == "pi")   return SciNumber(BigInt("3141592653"), -9); // 3.141592653589793238e0
        if (eq.type == TreeNode::CONSTANT && eq.value == "e")    return SciNumber(BigInt("2718281828"), -9);
        if (eq.type == TreeNode::NUMBER)
            return SciNumber(BigInt(std::stoll(eq.value)),BigInt(1));
        return SciNumber("none"); // None
    }


    if (eq.value == "+") {
        SciNumber sum = SciNumber(BigInt(0), BigInt(1));

        for (const auto &child : eq.children) {
            SciNumber val = compute2(child);

            if (val.is_none()) return SciNumber("none");
            sum = sum + val;
        }
        return sum;
    }

    else if (eq.value == "*") {
        SciNumber sum = SciNumber(BigInt(1), BigInt(1));

        for (const auto &child : eq.children) {
            SciNumber val = compute2(child);

            if (val.is_none()) return SciNumber("none");
            sum = sum * val;

        }
        return sum;
    }

    else if (eq.value == "^"){
        SciNumber base = compute2(eq.children[0]);
        int power = compute2(eq.children[1]).num.to_int();
        return base^power;
    }

    return SciNumber("none"); // None
}
struct OptionalNumber {
    bool is_none;
    double value;

    OptionalNumber() : is_none(true), value(0) {}  // default is None
    OptionalNumber(double v) : is_none(false), value(v) {}

    double to_double(){
        return value;
    }
};
OptionalNumber compute(const TreeNode &eq) {
    if (eq.children.empty()) {
        if (eq.type == TreeNode::CONSTANT && eq.value == "e") return OptionalNumber(M_E);
        if (eq.type == TreeNode::CONSTANT && eq.value == "pi") return OptionalNumber(M_PI);
        if (eq.type == TreeNode::CONSTANT && eq.value == "hbar") return OptionalNumber(1.054571817e-34);
        if (eq.type == TreeNode::CONSTANT && eq.value == "ek") return OptionalNumber(8.9875517873681764e9);
        if (eq.type == TreeNode::CONSTANT && eq.value == "ec") return OptionalNumber(1.602176634e-19);
        if (eq.type == TreeNode::CONSTANT && eq.value == "em") return OptionalNumber(9.1093837015e-31);
        if (eq.type == TreeNode::NUMBER)
            return OptionalNumber(std::stod(eq.value));
        return OptionalNumber(); // None
    }

    std::vector<OptionalNumber> values;
    for (const auto &child : eq.children) {
        OptionalNumber val = compute(child);
        if (val.is_none) return OptionalNumber();
        values.push_back(val);
    }

    if (eq.value == "+") {
        double sum = 0;
        for (auto &v : values) sum += v.value;
        return OptionalNumber(sum);
    }
    else if (eq.value == "-") return OptionalNumber(values[0].value - values[1].value);
    else if (eq.value == "rad") return OptionalNumber(values[0].value * M_PI / 180.0);
    else if (eq.value == "*") {
        double prod = 1.0;
        for (auto &v : values) prod *= v.value;
        return OptionalNumber(prod);
    }
    else if (eq.value == "neg") return OptionalNumber(-values[0].value);
    else if (eq.value == "/") return OptionalNumber(values[0].value / values[1].value);
    else if (eq.value == "^") return OptionalNumber(pow(values[0].value, values[1].value));
    else if (eq.value == "sin") return OptionalNumber(sin(values[0].value));
    else if (eq.value == "cos") return OptionalNumber(cos(values[0].value));
    else if (eq.value == "tan") return OptionalNumber(tan(values[0].value));
    else if (eq.value == "arcsin") return OptionalNumber(asin(values[0].value));
    else if (eq.value == "arccos") return OptionalNumber(acos(values[0].value));
    else if (eq.value == "arctan") return OptionalNumber(atan(values[0].value));
    else if (eq.value == "log") return OptionalNumber(log(values[0].value));

    return OptionalNumber(); // None
}
TreeNode parse(std::string s){
    Parser p(s);
    return p.parse();
}
std::map<std::pair<int,int>, TreeNode> trig_sin_table = {
    {{0,1}, parse("0")},
    {{1,6}, parse("1/2")},
    {{1,4}, parse("2^(1/2)/2")},
    {{1,3}, parse("3^(1/2)/2")},
    {{1,2}, parse("1")},
    {{2,3}, parse("3^(1/2)/2")},
    {{3,4}, parse("2^(1/2)/2")},
    {{5,6}, parse("1/2")},
    {{1,1}, parse("0")}
};

std::map<std::pair<int,int>, TreeNode> trig_cos_table = {
    {{0,1}, parse("1")},
    {{1,6}, parse("3^(1/2)/2")},
    {{1,4}, parse("2^(1/2)/2")},
    {{1,3}, parse("1/2")},
    {{1,2}, parse("0")},
    {{2,3}, parse("-1/2")},
    {{3,4}, parse("-2^(1/2)/2")},
    {{5,6}, parse("-1/2")},
    {{1,1}, parse("-1")}
};

// Simplify all entries
void simplify_trig_tables() {
    for (auto &p : trig_cos_table)
        p.second = solve(p.second);
    for (auto &p : trig_sin_table)
        p.second = solve(p.second);
}

// --------------------------------------
std::vector<TreeNode> factor_generation(const TreeNode& equation){
    std::vector<TreeNode> output;
    TreeNode eq = equation;
    if (eq.value != "*"){
        eq = TreeNode("*", {eq});
    }
    if (eq.value == "*"){
        for (TreeNode child : eq.children){
            if (child.value == "^"){
                if (child.children[1].type != TreeNode::NUMBER){
                    output.push_back(child);
                    continue;
                }
                Fraction<> n = calculate(child.children[1]);
                if (!n.is_none() && n.den == 1){
                    int c = n.num;
                    if (c < 0){
                        for (int i=0; i<-c; ++i){
                            output.push_back(child.children[0]^number(-1));
                        }
                    } else {
                        if (c==0){
                            output.push_back(number(0));
                        }
                        for (int i=0; i<c; ++i){
                            output.push_back(child.children[0]);
                        }
                    }
                } else {
                    output.push_back(child);
                }
            } else {
                output.push_back(child);
            }
        }
    }
    return output;
}
TreeNode solve2(const TreeNode &eq) {
    TreeNode eq2 = eq;

    // Recursively simplify children first
    for (auto &child : eq2.children)
        child = solve2(child);

    // --- Simplify Multiplication ---
    if (eq2.value == "*") {
        Fraction<> con(1, 1);
        for (int i = static_cast<int>(eq2.children.size()) - 1; i >= 0; --i) {
            Fraction<> n = calculate(eq2.children[i]);
            if (!n.is_none()) {
                con = con * n;
                eq2.children.erase(eq2.children.begin() + i);
            }
        }

        if (con == Fraction<>(0, 1)) {
            return number(0);
        } else if (con != Fraction<>(1, 1)) {
            eq2.children.push_back(frac_to_tree(con));
        }

        if (eq2.children.empty()) {
            return number(1);
        }
        if (eq2.children.size() == 1) {
            return eq2.children[0];
        }
    }

    // --- Simplify Addition ---
    else if (eq2.value == "+") {
        Fraction<> con(0, 1);
        for (int i = static_cast<int>(eq2.children.size()) - 1; i >= 0; --i) {
            Fraction<> n = calculate(eq2.children[i]);
            if (!n.is_none()) {
                con = con + n;
                eq2.children.erase(eq2.children.begin() + i);
            }
        }

        if (con != Fraction<>(0, 1)) {
            eq2.children.push_back(frac_to_tree(con));
        }

        if (eq2.children.empty()) {
            return number(0);
        }
        if (eq2.children.size() == 1) {
            return eq2.children[0];
        }
    }

    // --- Simplify Logs ---
    else if (eq2.value == "log") {
        if (eq2.children.size() == 1) {
            if (eq2.children[0].value == "1") return number(0);
            if (eq2.children[0].value == "e") return number(1);
        }
    }

    // --- Simplify Powers ---
    else if (eq2.value == "^" && eq2.children.size() == 2) {
        if (eq2.children[1] == number(0)) return number(1);
        if (eq2.children[1] == number(1)) return eq2.children[0];
        if (eq2.children[0] == number(1)) return number(1);
    }

    return eq2;
}


TreeNode trig0(const TreeNode &eq) {

    auto isneg = [](const TreeNode &eq) -> bool {
        if (eq.type != TreeNode::NUMBER) return false;
        int val = std::stoi(eq.value);
        return val < 0;
    };

    auto single_pi = [&](const std::vector<TreeNode> &lst) -> std::pair<int,int> {
        if (std::find(lst.begin(), lst.end(), number(0)) != lst.end())
            return {0, 1};

        int count = 0;
        for (auto &item : lst)
            if (item == constant("pi")) count++;

        if (count != 1)
            return {-1, -1}; // invalid marker

        TreeNode eq_local = solve(product(lst) / constant("pi"));
        auto out = calculate(eq_local); // returns a Fraction object

        if (out.is_none()) return {-1, -1};
        if (out.num < 0) return {-1, -1};

        int a = out.num;
        int b = out.den;
        a %= 2 * b;
        if (a > b) a = 2*b - a;

        return {a, b};
    };

    // Function dispatch
    if (eq.value == "arccosec")
        return (number(1) / eq.children[0]).fx("arcsin");

    if (eq.value == "arctan") {
        if (eq.children[0].value == "0")
            return number(0);
    }

    if (eq.value == "log") {
        if (eq.children[0].value == "1")
            return number(0);
    }

    if (eq.value == "tan") {
        if (eq.children[0].value == "arctan")
            return eq.children[0].children[0];
        return eq.children[0].fx("sin") / eq.children[0].fx("cos");
    }

    if (eq.value == "sec")
        return (eq.children[0].fx("cos")) ^ number(-1);

    if (eq.value == "cosec")
        return (eq.children[0].fx("sin")) ^ number(-1);

    if (eq.value == "cot")
        return eq.children[0].fx("cos") / eq.children[0].fx("sin");

    // sin
    if (eq.value == "sin") {
        if (eq.children[0].value == "arcsin")
            return eq.children[0].children[0];

        auto lst = factor_generation(eq.children[0]);
        for (auto &item : lst) {
            if (isneg(item))
                return -((eq.children[0] * number(-1)).fx("sin"));
        }

        auto out = single_pi(lst);
        if (out.first != -1) {
            auto it = trig_sin_table.find(out);
            if (it != trig_sin_table.end())
                return it->second;
        }
    }

    // cos
    if (eq.value == "cos") {
        if (eq.children[0].value == "arccos")
            return eq.children[0].children[0];

        auto lst = factor_generation(eq.children[0]);
        for (auto &item : lst) {
            if (isneg(item))
                return ((eq.children[0] * number(-1)).fx("cos"));
        }

        auto out = single_pi(lst);
        if (out.first != -1) {
            auto it = trig_cos_table.find(out);
            if (it != trig_cos_table.end())
                return it->second;
        }
    }

    std::vector<TreeNode> new_children;

    for (auto &child : eq.children)
        new_children.push_back(trig0(child));

    return TreeNode(eq.type, eq.value, new_children);
}


TreeNode solve_trig(const TreeNode& eq){
    TreeNode current = eq;
    while(true){
        TreeNode simplified = trig0(_solve(current));
        if(simplified==current) break;
        current = simplified;
    }
    return current;
}
TreeNode expand(const TreeNode& eq){
    if (eq.value == "^"){
        Fraction<> n = calculate(eq.children[1]);
        if (!n.is_none() && n.den == 1 && n.num > 1){
            std::vector<TreeNode> power_children;
            for (int i=0; i<n.num; ++i){
                power_children.push_back(eq.children[0]);
            }
            return expand(flatten(TreeNode("*", power_children)));
        }
    }
    if (eq.value == "*"){
        TreeNode lone_children = TreeNode(TreeNode::NUMBER, "1");
        std::vector<TreeNode> bracket_children;
        for (int i=eq.children.size()-1; i>=0; --i){
            if (eq.children[i].value == "+"){
                bracket_children.push_back(eq.children[i]);
            } else if (eq.children[i].value == "^" && eq.children[i].children[0].value == "+"){
                Fraction<> n = calculate(eq.children[i].children[1]);
                if (!n.is_none() && n.den == 1 && n.num > 1){
                    for (int j=0; j<n.num; ++j){
                        bracket_children.push_back(eq.children[i].children[0]);
                    }
                } else {
                    lone_children = lone_children * eq.children[i];
                }
            } else {
                lone_children = lone_children * eq.children[i];
            }
        }

        lone_children = solve(lone_children);

        while (!bracket_children.empty()){
            TreeNode tmp = TreeNode(TreeNode::NUMBER, "0");
            for (int i=0; i<bracket_children[0].children.size(); ++i){
                if (lone_children.value == "+"){
                    for (int j=0; j<lone_children.children.size(); ++j){
                        tmp = tmp + bracket_children[0].children[i] * lone_children.children[j];
                    }
                } else {
                    tmp = tmp + lone_children * bracket_children[0].children[i];
                }
            }
            lone_children = flatten(solve(tmp));

            bracket_children.erase(bracket_children.begin());
        }
        return lone_children;
    }
    std::vector<TreeNode> rec_children;
    for (auto& child : eq.children)
        rec_children.push_back(expand(flatten(child)));
    return TreeNode(eq.type, eq.value, rec_children);
}
TreeNode summation(const std::vector<TreeNode>& lst) {
    if (lst.empty())
        return TreeNode(TreeNode::NUMBER, "0");
    TreeNode s = lst[0];
    for (int i = 1; i < lst.size(); ++i)
        s = s + lst[i];
    return s;
}

void replace_all(std::string& str, const std::string& from, const std::string& to) {
    if(from.empty()) return;
    size_t pos = 0;
    while((pos = str.find(from, pos)) != std::string::npos) {
        str.replace(pos, from.length(), to);
        pos += to.length();
    }
}
std::tuple<TreeMatrix, std::string, TreeMatrix> integrate_formula_list(){
    static std::tuple<TreeMatrix, std::string, TreeMatrix> out;
    static bool visited = false;
    if (visited){
        return out;
    }
    std::string var = "x";
    std::vector<std::vector<std::string>> formula = {
        {
            "sin(A*{var}+B)", "-cos(A*{var}+B)/A"
        },
        {
            "cos(A*{var}+B)", "sin(A*{var}+B)/A"
        },
        {
            "(C*e)^(A*{var}+B)*{var}^2", "(C*e)^(A*{var}+B)*({var}^2/(A*(log(C)+1))-2*{var}/(A*(log(C)+1))^2+2/(A*(log(C)+1))^3)"
        },
        {
            "(C*e)^(A*{var}+B)", "(C*e)^(A*{var}+B)/(A*(log(C)+1))"
        },
        {
            "(C*e)^(A*{var}+B)*{var}", "(C*e)^(A*{var}+B)*({var}/(A*(log(C)+1))-1/(A*(log(C)+1))^2)"
        }
    };
    TreeMatrix formula_list;

    for (int i=0; i<formula.size(); ++i){
        TreeList tmp;
        for (int j=0; j<formula[i].size(); ++j){
            std::string s = formula[i][j];
            replace_all(s, "{var}", var);
            Parser p(s);
            TreeNode q = p.parse();
            convert_to_basic(q);
            tmp.push_back(solve(q));
        }
        formula_list.push_back(tmp);
    }
    TreeMatrix expr;
    std::vector<std::vector<std::string>> expr_list = {
        {
            "A", "1"
        },
        {
            "B", "0"
        },
        {
            "C", "1"
        }
    };
    for (int i=0; i<expr_list.size(); ++i){
        TreeList tmp;
        for (int j=0; j<expr_list[i].size(); ++j){
            Parser p(expr_list[i][j]);
            tmp.push_back(p.parse());
        }
        expr.push_back(tmp);
    }
    out = std::make_tuple(formula_list, var, expr);
    visited = true;
    return out;
}
bool not_linear_x(const TreeNode& eq, const TreeNode& wrt){
    if (eq.value == "^"){
        if (contain(eq.children[0], wrt) && !(solve(eq.children[0]) == number(1))){
            return true;
        }
        if (contain(eq.children[1], wrt)){
            TreeNode eq2 = solve(eq.children[0]);
            if (!((eq2 == number(0)) || (eq2 == number(1)))) return true;
        }
    }
    for (auto &child : eq.children){
        if (not_linear_x(child, wrt)){
            return true;
        }
    }
    return false; //not sure
}
TreeNode helper3(const TreeNode& equation, const TreeNode& wrt, std::vector<TreeNode>& alloclst, std::map<std::string,TreeNode>& dic){
    /*if (eq.value == "integrate"){
        std::vector<TreeNode> new_children;
        for (TreeNode item : eq.children){

            new_children.push_back(helper3(item, wrt, alloclst, dic));
        }
        return TreeNode(eq.type, eq.value, new_children);
    }*/
    TreeNode eq = equation;
    bool conste = false;
    if (!contain(eq, wrt) && contain(eq, constant("e"))){
        eq = replace_node(eq, constant("e"), wrt);
        conste = true;
    }
    if (contain(eq, wrt) && !not_linear_x(eq, wrt)){
        TreeNode a = diff(eq, wrt.value);
        a = solve(a);
        TreeNode b = diff(a, wrt.value);
        b = solve(b);
        TreeNode c = diff(b, wrt.value);
        c = solve(c);

        if (c == TreeNode(TreeNode::NUMBER, "0")){

            b = replace_node(eq, wrt, TreeNode(TreeNode::NUMBER, "0"));
            a = solve(a);
            b = solve(b);

            if (!(a == TreeNode(TreeNode::NUMBER, "1")) && a.children.size()>1 && !dic.count(a.str())){
                TreeNode a2 = alloclst[0];
                dic[a.str()] = a2;
                alloclst.erase(alloclst.begin());
            }
            if (!(b == TreeNode(TreeNode::NUMBER, "0")) && b.children.size()>1 && !dic.count(b.str())){
                TreeNode b2 = alloclst[0];
                dic[b.str()] = b2;

                alloclst.erase(alloclst.begin());
            }

            if (dic.count(a.str())){
                a = dic[a.str()];
            }
            if (dic.count(b.str())){
                b = dic[b.str()];
            }
            if (conste){
                return solve(a*constant("e") + b);
            }
            return solve(a*wrt + b);
        }
    }
    if (conste){
        eq = replace_node(eq, wrt, constant("e"));
    }
    std::vector<TreeNode> new_children;
    for (auto &item : eq.children){
        new_children.push_back(helper3(item, wrt, alloclst, dic));
    }
    return TreeNode(eq.type, eq.value, new_children);
}
TreeNode req(const TreeNode& eq, std::map<std::string,TreeNode>& dic){
    TreeNode eq2 = eq;
    for (auto& kv : dic){
        eq2 = replace_node(eq2, kv.second, tree_from_str(kv.first));
    }
    return eq2;
}

std::tuple<TreeNode,std::map<std::string,TreeNode>> group_const(const TreeNode& eq, const TreeNode& wrt){
    std::vector<TreeNode> alloclst;
    for (int i=0; i<26; ++i){
        char c = 'a';
        TreeNode v = TreeNode(TreeNode::VARIABLE, std::string(1,c+i));
        if (!contain(eq, v)){
            alloclst.push_back(v);
        }
    }
    std::map<std::string,TreeNode> dic;
    TreeNode eq2 = helper3(eq, wrt, alloclst, dic);
    return std::make_tuple(eq2, dic);
}
TreeNode integrate_formula(const TreeNode& eq){
    if (eq.value == "integrate"){
        if (eq.children[0] == eq.children[1]){
            return (eq.children[1]^number(2))/number(2);
        }
        if (!contain(eq.children[0], eq.children[1])){
            return eq.children[0]*eq.children[1];
        }
        if (eq.value == "^"){
            Fraction<> n = calculate(eq.children[1]);
            if (!n.is_none() && !(n == Fraction<>(1,1))){
                TreeNode m = solve(frac_to_tree(n)+number(1));
                return (eq.children[0]^m)/m;
            }
        }
        std::tuple<TreeMatrix, std::string, TreeMatrix> out = integrate_formula_list();
        TreeNode out2 = transform_formula(eq.children[0], eq.children[1],
                                          std::get<0>(out), std::get<1>(out), std::get<2>(out),
                                          [&eq](const TreeNode& x){return !contain(x, eq.children[1]);});
        if (!(out2 == TreeNode())){
            return out2;
        }
    }
    std::vector<TreeNode> new_children;
    for (TreeNode item : eq.children){
        new_children.push_back(integrate_formula(item));
    }
    return TreeNode(eq.type, eq.value, new_children);
}
TreeNode integrate_summation(const TreeNode& equation){
    TreeNode eq = expand(equation);
    if ((eq.value == "integrate" || eq.value == "limitpinf" || eq.value == "limit") && eq.children[0].value == "+"){
        TreeNode new_children = number(0);
        for (auto &item : eq.children[0].children){
            new_children = new_children + TreeNode(eq.type, eq.value, {item, eq.children[1]});
        }
        return new_children;
    }
    std::vector<TreeNode> new_children;
    for (TreeNode item : eq.children){
        new_children.push_back(integrate_summation(item));
    }
    return TreeNode(eq.type, eq.value, new_children);
}

TreeNode psi(const TreeNode& r, const TreeNode& c1, const TreeNode& c2){
    return c1 * (constant("e")^(-c2 * r));
}
TreeNode laplace_psi(const TreeNode& r, const TreeNode& c1, const TreeNode& c2){
    return diff((r^number(2)) * diff(psi(r,c1,c2), r.value), r.value)/(r^number(2));
}
TreeNode potential(const TreeNode& r){
    return -(constant("ek")*number(1)*(constant("ec")^number(2)))/r;
}
TreeNode Hpsi(const TreeNode& r, const TreeNode& c1, const TreeNode& c2){
    return -(constant("hbar")^number(2))/(number(2)*constant("em")) * laplace_psi(r, c1, c2) + potential(r) * psi(r, c1, c2);
}
TreeNode psiHpsi(const TreeNode& r, const TreeNode& c1, const TreeNode& c2){
    return psi(r, c1, c2)*Hpsi(r, c1, c2);
}
TreeNode psi2(const TreeNode& r, const TreeNode& c1, const TreeNode& c2){
    return psi(r, c1, c2)^number(2);
}
std::vector<TreeNode> num_dem(const TreeNode& equation){
    TreeNode eq = equation;

    if (eq.value == "^"){
        eq = TreeNode("*", {eq});
    }
    if (eq.value == "*"){
        TreeNode num = number(1);
        TreeNode dem = number(1);
        for (TreeNode item : eq.children){
            if (item.value == "^"){
                Fraction<> n = calculate(item.children[1]);
                if (!n.is_none() && n < Fraction<>(0,1)){

                    dem = dem * item.children[0]^(-item.children[1]);
                } else {
                    num = num * item;
                }
            } else {
                num = num * item;
            }
        }

        std::vector<TreeNode> out;
        out.push_back(solve(num));

        out.push_back(solve(dem));
        return out;
    }
    return {eq, number(1)};
}
std::tuple<TreeNode,int> check(const TreeNode& num, const TreeNode& den, const TreeNode& var){
    TreeNode n = solve_trig(replace_node(num, var, number(0)));
    TreeNode d = solve_trig(replace_node(den, var, number(0)));

    if (illegal_expr(n) || illegal_expr(d)){
        return std::make_tuple(TreeNode(),0);
    }
    if (n == number(0) && d == number(0)){
        return std::make_tuple(TreeNode(),1);
    }
    if (!(d == number(0))){
        return std::make_tuple(solve(n/d),2);
    }
    return std::make_tuple(TreeNode(),0);
}
TreeNode lhospital(const TreeNode& num, const TreeNode& den, const TreeNode& var){
    std::tuple<TreeNode,bool> out = check(num, den, var);
    if (std::get<1>(out) == 2){
        return std::get<0>(out);
    }
    TreeNode n2 = num;
    TreeNode d2 = den;

    while(true){
        TreeNode n = diff(n2, var.value);
        TreeNode d = diff(d2, var.value);
        std::tuple<TreeNode,int> out = check(n, d, var);
        if (std::get<1>(out)==1){
            n2 = n;
            d2 = d;
            continue;
        }
        if (std::get<1>(out)==0){

            return solve(n2/d2);
        }
        return std::get<0>(out);
    }
}
TreeNode lhospital2(const TreeNode& equation, const TreeNode& var){
    TreeNode eq = solve_trig(equation);

    if (illegal_expr(eq)){
        return TreeNode();
    }
    if (!contain(eq, var)){
        return eq;
    }
    std::vector<TreeNode> out = num_dem(equation);

    if (illegal_expr(out[0]) || illegal_expr(out[1])){
        return eq;
    }

    return lhospital(out[0], out[1], var);
}

bool present(const std::vector<TreeNode>& lst, const TreeNode &eq){
    for (auto &child : lst){
        if (child == eq) return true;
    }
    return false;
}

int sign_eq(const TreeNode& eq){
    if (eq.type == TreeNode::CONSTANT){
        if (eq.value == "inf") return -2;
        if (compute(eq).to_double() > 0){
            return 1;
        }
        return -1;
    }
    if (eq.type == TreeNode::NUMBER){
        if (eq.value == "0"){
            return 0;
        }
        if (compute(eq).to_double() > 0){
            return 1;
        }
        return -1;
    }
    if (eq.value == "+"){
        bool all_pos = true;
        bool all_neg = true;
        bool all_zero = true;
        for (auto &child : eq.children){
            int n = sign_eq(child);
            if (n == -2) return -2;
            if (n != 0) all_zero = false;
            if (n == -1) all_pos = false;
            if (n == 1) all_neg = false;
        }
        if (all_zero) return 0;
        if (all_neg) return -1;
        if (all_pos) return 1;
    }
    if (eq.value == "*"){
        int ans_sign = 1;
        for (auto &child : eq.children){
            int n = sign_eq(child);
            if (n == -2) return -2;
            else if (n == 0) ans_sign = 0;
            else ans_sign = ans_sign * n;
        }
        return ans_sign;
    }
    return -2;
}

TreeNode fxinf(const TreeNode &eq) {

    if (eq == TreeNode()) // assume TreeNode has is_none() or a similar check
        return TreeNode(); // use whatever you use to represent None

    if (eq.value == "+") {
        if (present(eq.children, constant("inf")) &&
            present(eq.children, -constant("inf"))){

            return TreeNode();
        }

        if (present(eq.children, constant("inf")))
            return constant("inf");

        if (present(eq.children, -constant("inf")))
            return -constant("inf");
    }

    if (eq.value == "*") {
        std::vector<TreeNode> lst = factor_generation(flatten(eq));

        if (present(lst, constant("inf"))) {
            // recursively apply fxinf to all children

            std::vector<TreeNode> new_children;

            for (auto &child : eq.children)
                new_children.push_back(fxinf(child));

            for (auto &child : new_children)
                if (child == TreeNode())
                    return TreeNode();

            TreeNode new_eq(eq.type, eq.value, new_children);

            lst = factor_generation(new_eq);

            if (present(lst, number(0))){
                return number(0);
            }

            std::vector<TreeNode> lst2;
            for (auto &item : lst)
                if (item.str().find("v_") != std::string::npos)
                    lst2.push_back(item);

            bool sign = true;
            int neg_count = 0;
            for (auto &item : lst) {
                if (item.str().find("v_") == std::string::npos &&
                    !contain(item, constant("inf")) &&
                    !compute(item).is_none && compute(item).to_double() < 0)
                    neg_count++;
            }
            if (neg_count % 2 == 1) sign = false;

            if (lst2.empty()) {
                if (sign)
                    return constant("inf");
                else
                    return -constant("inf");
            }
        }
    }

    if (eq.value == "^") {
        int a = sign_eq(eq.children[0]);
        int b = sign_eq(eq.children[1]);

        if (a == 1) {
            if (eq.children[1] == -constant("inf"))
                return number(0);
            else if (eq.children[1] == constant("inf"))
                return constant("inf");
        }
        if (b == 1){
            if (eq.children[0] == constant("inf"))
                return constant("inf");
        }
        if (b == -1){
            if (eq.children[0] == constant("inf"))
                return number(0);
        }
    }

    // recurse
    std::vector<TreeNode> new_children;

    for (auto &child : eq.children)
        new_children.push_back(fxinf(child));

    for (auto &child : new_children)
        if (child == TreeNode())
            return TreeNode();

    return TreeNode(eq.type, eq.value, new_children);
}
TreeNode dowhilesolve2(const TreeNode& eq){
    TreeNode current = eq;
    while(true){
        TreeNode simplified = solve2(current);

        if(simplified==current) break;
        current = simplified;
    }
    return current;
}

TreeNode dowhilefxint(const TreeNode& eq){
    TreeNode current = eq;
    while(true){
        TreeNode simplified = fxinf(current);
        if(simplified==current) break;
        current = simplified;
    }

    return current;
}

std::tuple<TreeNode,bool> limit(const TreeNode& eq, const TreeNode& var){

    TreeNode eq2 = solve(replace_node(eq, var, number(0)));

    if (!(eq2 == TreeNode()) && !contain(eq2, var)){

        return std::make_tuple(eq2, true);
    }

    TreeNode equation = lhospital2(eq, var);
    equation = solve(expand(solve(equation)));
    if (!contain(equation, var)){
        return std::make_tuple(equation, true);
    }
    return std::make_tuple(equation, false);
}
TreeNode power_split(const TreeNode& eq){
    if (eq.value == "^" && eq.children[0].value == "*"){
        std::vector<TreeNode> lst_term;
        for (auto &child : eq.children[0].children){
            lst_term.push_back(child^eq.children[1]);
        }
        return product(lst_term);
    }
    std::vector<TreeNode> new_children;
    for (auto &item : eq.children){
        new_children.push_back(power_split(item));
    }
    return TreeNode(eq.type, eq.value, new_children);
}
TreeNode power_merge(const TreeNode& eq){
    if (eq.value == "^" && eq.children[0].value == "^"){
        std::vector<TreeNode> lst_term;
        for (auto &child : eq.children[0].children){
            lst_term.push_back(child^eq.children[1]);
        }
        return eq.children[0].children[0]^(eq.children[1]*eq.children[0].children[1]);
    }
    std::vector<TreeNode> new_children;
    for (auto &item : eq.children){
        new_children.push_back(power_merge(item));
    }
    return TreeNode(eq.type, eq.value, new_children);
}
TreeNode dowhilepower(const TreeNode& eq){
    TreeNode current = eq;
    while(true){
        TreeNode simplified = power_split(power_merge(current));

        if(simplified==current) break;
        current = simplified;
    }
    return current;
}
TreeNode fraction(const TreeNode &eq) {
    using NodePtr = const TreeNode*;
    std::stack<std::pair<NodePtr, NodePtr>> stack; // node, parent_info (not used)
    stack.push(std::make_pair(&eq, nullptr));

    std::unordered_map<NodePtr, TreeNode> result_map;

    while (!stack.empty()) {
        auto top_elem = stack.top();
        stack.pop();
        const TreeNode* node = top_elem.first;
        const TreeNode* parent_info = top_elem.second;

        if (result_map.count(node)) continue;

        // Leaf node
        if (node->children.empty()) {
            result_map[node] = TreeNode(node->type, node->value, {});
            continue;
        }

        // Check if all children are processed
        bool all_done = true;
        for (const auto &child : node->children) {
            if (result_map.find(&child) == result_map.end()) {
                all_done = false;
                break;
            }
        }

        if (!all_done) {
            stack.push(std::make_pair(node, parent_info));
            for (auto it = node->children.rbegin(); it != node->children.rend(); ++it) {
                if (result_map.find(&(*it)) == result_map.end())
                    stack.push(std::make_pair(&(*it), node));
            }
            continue;
        }

        // Process node
        if (node->value == "=") {
            TreeNode left = result_map[&node->children[0]];
            TreeNode right = result_map[&node->children[1]];
            result_map[node] = TreeNode("=", {left, right});
            continue;
        } else if (node->value == "+") {
            std::vector<std::pair<std::vector<TreeNode>, std::vector<TreeNode>>> con;

            for (const auto &child : node->children) {
                TreeNode child_proc = result_map[&child];

                if (child_proc.value == "^" && child_proc.children[1].type == TreeNode::NUMBER &&
                    std::stoi(child_proc.children[1].value) < 0) {

                    std::vector<TreeNode> den;
                    int n = std::stoi(child_proc.children[1].value);
                    if (n == -1) den.push_back(child_proc.children[0]);
                    else den.push_back(TreeNode("^", {child_proc.children[0], number(-n)}));
                    con.push_back({{}, den});

                } else if (child_proc.value == "*") {
                    std::vector<TreeNode> num;
                    std::vector<TreeNode> den;
                    for (const auto &child2 : child_proc.children) {
                        if (child2.value == "^" && child2.children[1].type == TreeNode::NUMBER &&
                            std::stoi(child2.children[1].value) < 0) {

                            int n = std::stoi(child2.children[1].value);
                            if (n == -1) den.push_back(child2.children[0]);
                            else den.push_back(TreeNode("^", {child2.children[0], number(-n)}));
                        } else {
                            num.push_back(child2);
                        }
                    }
                    con.push_back({num, den});
                } else {
                    con.push_back({{child_proc}, {}});
                }
            }

            // If there is a denominator anywhere
            bool has_den = false;
            for (auto &p : con) if (!p.second.empty()) has_den = true;

            if (con.size() > 1 && has_den) {
                // numerator
                std::vector<TreeNode> a_children;
                for (size_t i = 0; i < con.size(); ++i) {
                    std::vector<TreeNode> b_children = con[i].first;
                    for (size_t j = 0; j < con.size(); ++j) {
                        if (i == j) continue;
                        b_children.insert(b_children.end(), con[j].second.begin(), con[j].second.end());
                    }
                    if (b_children.empty()) b_children.push_back(number(1));
                    else if (b_children.size() > 1) b_children = {TreeNode("*", b_children)};
                    a_children.insert(a_children.end(), b_children.begin(), b_children.end());
                }
                TreeNode a = TreeNode("+", a_children);

                // denominator
                std::vector<TreeNode> c_children;
                for (auto &p : con) c_children.insert(c_children.end(), p.second.begin(), p.second.end());
                TreeNode c;
                if (c_children.size() == 1) c = c_children[0];
                else c = TreeNode("*", c_children);
                c = TreeNode("^", {c, number(-1)});

                result_map[node] = TreeNode("*", {solve(expand(a)), c});
                continue;
            }
        }

        // Default: reconstruct node
        std::vector<TreeNode> children_proc;
        for (const auto &child : node->children)
            children_proc.push_back(result_map[&child]);
        result_map[node] = TreeNode(node->type, node->value, children_proc);
    }

    return solve(result_map[&eq]);
}
TreeNode to_inf(const TreeNode& eq, const TreeNode& wrt){
    TreeNode expr = eq;
    expr = replace_node(expr, wrt, constant("inf"));
    expr = flatten(expr);

    expr = dowhilefxint(expr);
    return expr;
}
TreeNode limit3(const TreeNode &eq) {
    // Handle the specific limit case
    if (eq.value == "limitpinf") {
        // If the variable is not in the expression, return the expression itself
        if (!contain(eq, eq.children[1])) {
            return eq.children[0];
        }
        TreeNode eq2 = to_inf(eq.children[0], eq.children[1]);

        if (!contain(eq2, constant("inf")) && !contain(eq2, eq.children[1])) {
            return solve(eq2);
        }

        std::vector<TreeNode> out = num_dem(dowhilesolve2(dowhilepower(fraction(eq.children[0]))));
        TreeNode a = to_inf(out[0], eq.children[1]);

        TreeNode b = to_inf(out[1], eq.children[1]);
        if ((a == constant("inf") && b == constant("inf")) ||
            (a == number(0) && b == number(0))){

            TreeNode eq3 = solve(diff(out[0],eq.children[1].value)/diff(out[1],eq.children[1].value));
            if (contain(eq3, eq.children[1])) return TreeNode("limitpinf", {eq3, eq.children[1]});
            else return eq3;
        }
    }

    // Recurse over children
    std::vector<TreeNode> new_children;

    for (const auto &child : eq.children) {
        new_children.push_back(limit3(child));
    }

    return TreeNode(eq.type, eq.value, new_children);
}

TreeNode limit1(const TreeNode& eq){
    if (eq.value == "limit"){

        std::tuple<TreeNode,bool> out = limit(eq.children[0], eq.children[1]);
        if (std::get<1>(out)){

            return std::get<0>(out);
        } else {
            return TreeNode(eq.value, {std::get<0>(out), eq.children[1]});
        }
    }
    std::vector<TreeNode> new_children;
    for (TreeNode item : eq.children){
        new_children.push_back(limit1(item));
    }
    return TreeNode(eq.type, eq.value, new_children);
}
TreeNode integrate_const(const TreeNode& equation){
    TreeNode eq = flatten(equation);
    if ((eq.value == "integrate" || eq.value == "limit" || eq.value == "limitpinf") && contain(eq.children[0], eq.children[1]) && eq.children[0].value == "*"){

        std::vector<TreeNode> lst_const;
        for (auto &child : eq.children[0].children){
            if (!contain(child, eq.children[1])){

                lst_const.push_back(child);
            }
        }
        if (!lst_const.empty()){
            std::vector<TreeNode> lst_const2;
            for (auto &child : eq.children[0].children){
                if (contain(child, eq.children[1])) lst_const2.push_back(child);
            }
            TreeNode eq2 = product(lst_const2);

            TreeNode con = solve(product(lst_const));
            if (!(con == number(1))) return integrate_const(TreeNode(eq.value, {eq2, eq.children[1]}))*con;
        }
    }
    std::vector<TreeNode> new_children;
    for (auto &item : eq.children){
        new_children.push_back(integrate_const(item));
    }
    return TreeNode(eq.type, eq.value, new_children);
}
TreeNode cancel(const TreeNode& eq){
    if (eq.value == "*"){
        std::vector<TreeNode> out = factor_generation(eq);
        std::vector<TreeNode> cancelled;
        for (int i=0; i<out.size(); ++i){
            out[i] = solve(out[i]);
        }
        while (true){
            int a=-1;
            int b=-1;
            for (int i=0; i<out.size(); ++i){
                for (int j=0; j<out.size(); ++j){
                    if (i == j) continue;
                    if (out[i].value == "^" && out[i].children[1] == number(-1) && out[i].children[0] == out[j]){
                        a = i;
                        b = j;
                        break;
                    }
                }
                if (a != -1 || b != -1) break;
            }
            if (a == -1 && b == -1) break;
            else {
                out.erase(out.begin()+std::max(a,b));
                out.erase(out.begin()+std::min(a,b));
            }
        }
        TreeNode out2 = solve(product(out));
        if (out2.children.size() < eq.children.size()) return out2;
    }
    std::vector<TreeNode> new_children;
    for (auto &item : eq.children){
        new_children.push_back(cancel(item));
    }
    return TreeNode(eq.type, eq.value, new_children);
}
TreeNode dowhilecancel(const TreeNode& eq){
    TreeNode current = eq;
    while(true){
        TreeNode simplified = expand(cancel(current));

        if(simplified==current) break;
        current = simplified;
    }
    return current;
}
TreeNode def_integrate(const TreeNode& eq, const TreeNode& wrt, const TreeNode& a, const TreeNode& b, int mode){
    TreeNode eq2 = TreeNode("integrate", {eq, wrt});
    eq2 = solve(eq2);
    eq2 = integrate_const(eq2);
    eq2 = power_merge(eq2);
    eq2 = fraction(eq2);
    eq2 = dowhilesolve2(eq2);
    eq2 = dowhilepower(eq2);
    std::tuple<TreeNode,std::map<std::string,TreeNode>> out = group_const(eq2, wrt);
    eq2 = std::get<0>(out);
    std::map<std::string,TreeNode> fx = std::get<1>(out);
    eq2 = integrate_const(eq2);
    eq2 = integrate_formula(eq2);
    eq2 = dowhilesolve2(eq2);
    eq2 = solve(eq2);
    eq2 = integrate_const(eq2);
    if (mode == 1){
        eq2 = integrate_summation(eq2);
        eq2 = integrate_const(eq2);
        eq2 = integrate_formula(eq2);
    }
    eq2 = req(eq2, fx);
    TreeNode a2;
    if (a == constant("inf")){
        a2 = TreeNode("limitpinf", {eq2, wrt});
    } else {
        a2 = TreeNode("limit", {replace_node(eq2, wrt, wrt+a), wrt});
    }
    TreeNode b2;
    if (b == constant("inf")){
        b2 = TreeNode("limitpinf", {eq2, wrt});
    } else {
        b2 = TreeNode("limit", {replace_node(eq2, wrt, wrt+b), wrt});
    }

    a2 = solve(a2);
    a2 = limit1(a2);
    convert_to_basic(a2);
    a2 = flatten(a2);
    a2 = dowhilesolve2(a2);
    a2 = fraction(a2);
    a2 = dowhilepower(a2);
    a2 = dowhilecancel(a2);
    a2 = dowhilesolve2(a2);

    b2 = fraction(b2);
    b2 = dowhilesolve2(b2);
    b2 = solve(b2);
    b2 = limit3(b2);
    b2 = dowhilesolve2(b2);

    TreeNode eq3 = solve(b2-a2);
    convert_to_basic(eq3);
    eq3 = dowhilepower(eq3);
    eq3 = solve(eq3);
    eq3 = dowhilesolve2(eq3);
    eq3 = dowhilecancel(eq3);
    return eq3;
}
TreeNode integrate_function(const TreeNode& eq, const TreeNode& r, int mode){
    return def_integrate(eq * number(4) * constant("pi") * (r^number(2)), r, number(0), constant("inf"), mode);
}
std::string divideBigInt(const BigInt& a, const BigInt& b, int precision = 20) {
    if (b == BigInt(0)) return "NaN"; // Division by zero

    BigInt ten(10);
    BigInt zero(0);

    BigInt dividend = a;
    BigInt divisor = b;
    bool negative = false;

    if (dividend < zero) { negative = !negative; dividend = -dividend; }
    if (divisor < zero)  { negative = !negative; divisor = -divisor; }

    std::string result = "";

    // --- Integer Part ---
    BigInt current(0);
    std::string s = dividend.str();

    for (char ch : s) {
        current = current * ten + BigInt(ch - '0');

        int count = 0;
        while (current >= divisor) {
            current = current - divisor;
            count++;
        }
        result.push_back('0' + count);
    }

    // Remove leading zeros
    int pos = 0;
    while (pos + 1 < result.size() && result[pos] == '0') pos++;
    result = result.substr(pos);

    // --- Fractional Part ---
    if (current == BigInt(0)) {
        if (negative && result != "0") result = "-" + result;
        return result;
    }

    result.push_back('.');

    for (int i = 0; i < precision; ++i) {
        current = current * ten;

        int count = 0;
        while (current >= divisor) {
            current = current - divisor;
            count++;
        }
        result.push_back('0' + count);
        if (current == BigInt(0)) break;
    }

    if (negative && result != "0") result = "-" + result;
    return result;
}

void hydrogen(){
    TreeNode z, k, m, e1, hbar, pi, r, a0, c2, c1, expr, expr2, result;
    z = number(1);
    k = constant("ek");
    m = constant("em");
    e1 = constant("ec");
    hbar = constant("hbar");
    pi = constant("pi");
    r = variable("r");

    a0 = (hbar^number(2))/(k * (e1^number(2)) * m);
    c2 = z/a0;
    c1 = ((z^number(3))/(pi * (a0^number(3)))).fx("sqrt");
    convert_to_basic(c1);

    expr = expand(solve(psi2(r,c1,c2)));
    expr = integrate_function(expr, r, 0);

    expr2 = expand(solve(psiHpsi(r,c1,c2)));
    expr2 = dowhilesolve2(expr2);
    expr2 = integrate_function(expr2, r, 1);

    result = expr2/expr;

    result = result/e1;
    result = expand(result);
    result = dowhilesolve2(result);
    std::cout << result.render() << "\n\n";
    SciNumber x = compute2(result);
    std::cout << divideBigInt(x.num, x.den) << "\n";
}

int main(){
    hydrogen();
    return 0;
}
