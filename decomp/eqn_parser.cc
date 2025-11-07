/*************************************************************************
 *
 *  Copyright (c) 2025 Karthi Srinivasan
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either version 2
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor,
 *  Boston, MA  02110-1301, USA.
 *
 **************************************************************************
 */

#include "eqn_parser.h"

Token EqnLexer::next() {
    skipSpaceAndComments();
    if (eof()) return make(TokKind::End, "");

    char c = peek();
    if (c=='=') { bump(); return make(TokKind::Eq, "="); }
    if (c==';') { bump(); return make(TokKind::Semi, ";"); }
    if (c=='(') { bump(); return make(TokKind::LParen, "("); }
    if (c==')') { bump(); return make(TokKind::RParen, ")"); }
    if (c=='!') { bump(); return make(TokKind::Bang, "!"); }
    if (c=='*') { bump(); return make(TokKind::Star, "*"); }
    if (c=='+') { bump(); return make(TokKind::Plus, "+"); }
    if (c=='^') { bump(); return make(TokKind::Caret, "^"); }
    if (c==',') { bump(); return make(TokKind::Comma, ","); }

    if (std::isdigit(static_cast<unsigned char>(c))) {
        // Single 0/1 literal if not followed by ident-ish char or '['
        size_t save_i=i, save_line=line, save_col=col;
        char d = c;
        bump();
        if ((d=='0' || d=='1') && !isIdentCont(peek()) && peek()!='[') {
            return Token{TokKind::Const, std::string(1, d), (int)save_line, (int)save_col};
        }
        // Otherwise treat as identifier starting with digits (e.g., "71_")
        i=save_i; line=save_line; col=save_col;
        return lexId();
    }

    if (isIdentStart(c)) return lexId();

    // Fallback to ident until delimiter
    return lexId();
}

bool EqnLexer::eof() const { 
    return i>=s.size(); 
}

char EqnLexer::peek() const { 
    return eof()? '\0': s[i]; 
}

char EqnLexer::bump() {
    char c = peek();
    i++;
    if (c=='\n') { line++; col=1; } else { col++; }
    return c;
}

bool EqnLexer::isIdentStart(char c) {
    return std::isalpha(static_cast<unsigned char>(c)) || 
            c=='_' || 
            std::isdigit(static_cast<unsigned char>(c));
}

bool EqnLexer::isIdentCont(char c) {
    return std::isalnum(static_cast<unsigned char>(c)) 
            || c=='_' 
            || c=='.';
}

Token EqnLexer::make(TokKind k, std::string t) const {
    return Token{k, std::move(t), (int)line, (int)col-1};
}

void EqnLexer::skipSpaceAndComments() {
    while (!eof()) {
        char c = peek();
        if (std::isspace(static_cast<unsigned char>(c))) { bump(); continue; }
        if (c == '#') {
            while (!eof() && bump() != '\n') {}
            continue;
        }
        break;
    }
}

Token EqnLexer::lexId() {
    size_t start_i=i, start_line=line, start_col=col;
    while (!eof() && isIdentCont(peek())) bump();
    while (!eof() && peek()=='[') { // bracketed indices like out[7] or in_0[0]
        size_t j=i; bump();
        bool ok=true;
        if (eof() || !std::isdigit(static_cast<unsigned char>(peek()))) ok=false;
        while (!eof() && std::isdigit(static_cast<unsigned char>(peek()))) bump();
        if (eof() || peek()!=']') ok=false;
        if (!ok) { i=j; col--; break; }
        bump(); // ]
    }
    std::string text(s.substr(start_i, i-start_i));
    return Token{TokKind::Id, std::move(text), (int)start_line, (int)start_col};
}



void EqnParser::parseFile() {
    while (tok.kind != TokKind::End) {
        if (tok.kind == TokKind::Semi) { advance(); continue; }
        if (isKeyword(tok, "INORDER")) { advance(); parseOrderList(ef.inorder); continue; }
        if (isKeyword(tok, "OUTORDER")) { advance(); parseOrderList(ef.outorder); continue; }
        ef.stmts.insert(std::move(parseStmt()));
    }
}

std::string EqnParser::get_name (VarId v) {
    Assert (nm.count(v), "var not found");
    return nm.at(v);
}

Bimap<std::string, VarId> 
EqnParser::get_name_map () { 
    return nm; 
}

std::unordered_set<VarId> 
EqnParser::get_inorder () { 
    return ef.inorder; 
}

std::unordered_set<VarId> 
EqnParser::get_outorder () { 
    return ef.outorder; 
}

std::unordered_map<VarId, ChpExpr> 
EqnParser::get_stmts () { 
    std::unordered_map<VarId, ChpExpr> ret = {};
    for ( const auto &[x,y] : ef.stmts ) {
        ret.insert({x,ChpExpr::deep_copy(y)});
    }
    return ret;
}


std::vector<Block *> EqnParser::get_assigns (ChpGraph &g) {
    std::vector<Block *> ret = {};
    for (const auto &[lhs,rhs]: ef.stmts) {
        auto b = g.blockAllocator().newBlock(Block::makeBasicBlock(
            Statement::makeAssignment(lhs, ChpExprSingleRootDag::of_expr(ChpExpr::deep_copy(rhs)) ) ) );
        ret.push_back(b);
    }
    return ret;
}

ChpExpr EqnParser::rec (ChpExpr &&e) {
    switch (e.type()) {
    case IRExprTypeKind::BinaryOp: {
        return ChpExpr::makeBinaryOp(e.u_e2().op_type, 
            std::make_unique<ChpExpr>(std::move(rec(std::move(*(e.u_e2().l))))),
            std::make_unique<ChpExpr>(std::move(rec(std::move(*(e.u_e2().r))))));
    }
    break;
    case IRExprTypeKind::UnaryOp: {
        return ChpExpr::makeUnaryOp(e.u_e1().op_type, 
            std::make_unique<ChpExpr>(std::move(rec(std::move(*(e.u_e1().l))))));
    }
    break;
    case IRExprTypeKind::Var: {
        auto v = e.u_var().id;
        if (ef.inorder.count(v)) {
            return ChpExpr::makeVariableAccess(v,1);
        }
        return rec(ChpExpr::deep_copy(ef.stmts.at(v)));
    }
    break;
    case IRExprTypeKind::Const: {
        return ChpExpr::deep_copy(e);
    }
    break;
    default:
    Assert (false, "unexpected type");
    break;
    }
    return ChpExpr::deep_copy(e);
}

std::string EqnParser::file_to_str(const std::string& path) {
    std::ifstream in(path);
    if (!in) throw std::runtime_error("could not find eqn file");
    std::ostringstream sstr;
    sstr << in.rdbuf();
    return sstr.str();
}
    
VarId EqnParser::get_var (std::string name) {
    if (!nm.count(name)) {
        auto vi = idpool->makeUniqueVar(1,false);
        nm.insert(name,vi);
    }
    return nm.at(name);
}

bool EqnParser::ieq (char a, char b) { 
    return std::tolower((unsigned char)a)==std::tolower((unsigned char)b); 
}

bool EqnParser::isKeyword(const Token& t, std::string kw) {
    if (t.kind != TokKind::Id || t.text.size()!=kw.size()) return false;
    for (size_t i=0;i<kw.size();++i) if(!ieq(t.text[i], kw[i])) return false;
    return true;
}

void EqnParser::errorHere(std::string msg) const {
    std::ostringstream oss;
    oss << "Parse error at " << tok.line << ":" << tok.col << " near '" << tok.text << "': " << msg;
    throw std::runtime_error(oss.str());
}

void EqnParser::advance () { 
    tok = lex.next(); 
}

void EqnParser::parseOrderList (std::unordered_set<VarId> &out) {
    if (tok.kind != TokKind::Eq) errorHere("expected '=' after INORDER/OUTORDER");
    advance();
    // Accept optional parentheses: INORDER = (a, b, c);
    bool hadLP = false;
    if (tok.kind == TokKind::LParen) { hadLP = true; advance(); }

    bool needItem = true;
    while (true) {
        if (tok.kind == TokKind::Semi || (hadLP && tok.kind==TokKind::RParen)) {
            if (needItem) { /* allow empty if needed */ }
            if (hadLP && tok.kind==TokKind::RParen) { 
                advance(); /* consume ) */ 
                if (tok.kind != TokKind::Semi) errorHere("expected ';' after ')'"); 
            }
            if (tok.kind == TokKind::Semi) advance();
            else if (!hadLP) errorHere("expected ';' after order list");
            break;
        }
        if (tok.kind == TokKind::Comma) { advance(); continue; }
        if (tok.kind != TokKind::Id) errorHere("expected identifier in order list");
        out.insert(get_var(tok.text));
        advance();
        needItem = false;
        if (tok.kind == TokKind::Comma) { advance(); needItem = true; }
    }
}

std::pair<VarId, ChpExpr> EqnParser::parseStmt() {
    if (tok.kind != TokKind::Id) { 
        fprintf(stdout, "\ntok : %s", tok.text.c_str());
        errorHere("expected identifier at start of statement");
    }
    std::string lhs = tok.text; int lline=tok.line, lcol=tok.col;
    advance();
    if (tok.kind != TokKind::Eq) {
        fprintf(stdout, "\ntok : %s", tok.text.c_str());
        errorHere("expected '=' after LHS");
    }
    advance();
    auto rhs = parseExpr();
    if (tok.kind != TokKind::Semi) {
        fprintf(stdout, "\ntok : %s", tok.text.c_str());
        errorHere("expected ';' at end of statement");
    }
    advance();
    rhs = rec(std::move(rhs));
    return std::make_pair(get_var(lhs), std::move(rhs));
}

ChpExpr EqnParser::parseExpr() { 
    return parseOr(); 
}

ChpExpr EqnParser::parseOr() { 
    auto e = parseXor(); 
    while(tok.kind==TokKind::Plus) { 
        advance(); 
        e = ChpExpr::makeBinaryOp(
            IRBinaryOpType::Or, std::make_unique<ChpExpr>(std::move(e)), 
            std::make_unique<ChpExpr>(std::move(parseXor())));
    } 
    return e; 
}

ChpExpr EqnParser::parseXor() { 
    auto e = parseAnd(); 
    while(tok.kind==TokKind::Caret) {
        advance(); 
        e = ChpExpr::makeBinaryOp(
            IRBinaryOpType::Xor, std::make_unique<ChpExpr>(std::move(e)), 
            std::make_unique<ChpExpr>(std::move(parseAnd())));
    } 
    return std::move(e); 
}

ChpExpr EqnParser::parseAnd() { 
    auto e = parseUnary();
    while(tok.kind==TokKind::Star) { 
        advance(); 
        e = ChpExpr::makeBinaryOp(
            IRBinaryOpType::And, std::make_unique<ChpExpr>(std::move(e)),
            std::make_unique<ChpExpr>(std::move(parseUnary())));
    } 
    return std::move(e); 
}

ChpExpr EqnParser::parseUnary() {
    if (tok.kind==TokKind::Bang) { 
        advance(); 
        return ChpExpr::makeUnaryOp(
            IRUnaryOpType::Not, 
            std::make_unique<ChpExpr>(std::move(parseUnary())));
    }
    return parsePrimary();
}

ChpExpr EqnParser::parsePrimary() {
    if (tok.kind==TokKind::Const) { 
        uint64_t v = (tok.text=="1") ? (1) : (0); 
        advance(); 
        auto bval = ChpOptimize::BigInt{v};
        return ChpExpr::makeConstant(bval,1);
    }
    if (tok.kind==TokKind::Id) { 
        std::string n = tok.text; 
        advance(); 
        auto v = get_var(n);
        return ChpExpr::makeVariableAccess(v,1); 
    }
    if (tok.kind==TokKind::LParen) { 
        advance(); 
        auto e = parseExpr(); 
        if(tok.kind!=TokKind::RParen) errorHere("expected ')'"); 
        advance(); 
        return e; 
    }
    errorHere("expected primary expression");
    return ChpExpr::makeConstant(ChpOptimize::BigInt{0},1);
}