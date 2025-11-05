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

#include <string>
#include <vector>
#include <iostream>
#include <fstream>
#include <sstream>

#include <act/chp/chp-graph.h>
// ---------- Tokens ----------

using namespace ChpOptimize;

enum class TokKind {
    End, Id, Const, Eq, Semi,
    LParen, RParen,
    Bang, Star, Plus, Caret, Comma
};

struct Token {
    TokKind kind{};
    std::string text;   // ident text or "0"/"1"
    int line=1, col=1;
};

class EqnLexer {
public:
    explicit EqnLexer(std::string src): s(src) {}
    Token next();

private:
    std::string s;
    size_t i=0;
    size_t line=1, col=1;

    bool eof() const;
    char peek() const;
    char bump();

    static bool isIdentStart(char c);
    static bool isIdentCont(char c);

    Token make(TokKind, std::string) const;

    void skipSpaceAndComments();

    Token lexId();
};

struct EqnFile {
    std::unordered_set<VarId>   inorder{};
    std::unordered_set<VarId>   outorder{};
    std::unordered_map<VarId, ChpExpr> stmts{};
};

class EqnParser {
public:
    explicit EqnParser(std::string path, ChpOptimize::IdPool &idp)
    : lex(file_to_str(path)), idpool(&idp), ef(), nm(), nmi() { 
        advance(); 
    }

    void parseFile();

    std::string get_name (VarId);

    std::vector<Block *> get_assigns (ChpGraph &g);

    std::unordered_map<std::string, VarId> get_name_map () { return nm; }
    std::unordered_map<VarId, std::string> get_name_map_inv () { return nmi; }

private:
    EqnLexer lex; 
    Token tok;
    EqnFile ef;
    ChpOptimize::IdPool *idpool;
    std::unordered_map<std::string, VarId> nm;
    std::unordered_map<VarId, std::string> nmi;

    ChpExpr rec (ChpExpr &&);

    std::string file_to_str(const std::string&);

    VarId get_var (std::string);

    static bool ieq (char, char);
    
    static bool isKeyword(const Token&, std::string);

    void errorHere(std::string) const;

    void advance ();

    // INORDER/OUTORDER = name [name|, name]* ;
    void parseOrderList (std::unordered_set<VarId> &);

    std::pair<VarId, ChpExpr> parseStmt();

    // Precedence: ! > * > ^ > +
    ChpExpr parseExpr();

    ChpExpr parseOr();

    ChpExpr parseXor();

    ChpExpr parseAnd();
    
    ChpExpr parseUnary();

    ChpExpr parsePrimary();
};

