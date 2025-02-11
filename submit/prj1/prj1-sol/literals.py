#!/usr/bin/env python3

import re
import sys
from collections import namedtuple
import json

"""
literals
 : literal*
 ;
literal
 : NIL
 | BOOL
 | INT
 | STR
 | array
 | hash
 | range
 ;
array
 : '[' ( literal',' )* ']'
 ;
hash
 : '{' ( literal '=>' literal',' )* '}'
 ;
range
 : int_range
 | str_range
 ;
int_range
 : '('int'..'int')'
 | '('int'...'int')'
 ;
str_range
 : '('str'..'str')'
 | '('str'...'str')'
 ;
"""

def parse(text):

    def peek(kind): return lookahead.kind == kind
    def consume(kind):
        nonlocal lookahead
        if (lookahead.kind == kind):
            lookahead = nextToken()
        else:
            print(f'expecting {kind} at {lookahead.lexeme}',
                  file=sys.stderr)
            sys.exit(1)
    def nextToken():
        nonlocal index
        if (index >= len(tokens)):
            return Token('EOF', '<EOF>')
        else:
            tok = tokens[index]
            index += 1
            return tok

    def program():
        asts = []
        while (not peek('EOF')):
            asts.append(expr())
            consume(';')
        return asts

    def expr():
        t = term()
        while (peek('+') or (peek('-'))):
            kind = lookahead.kind
            consume(kind)
            t1 = term()
            t = Ast(kind, t, t1)
        return t
    
    def expn():
        t = factor()
        if (peek('EXPN')):
            consume('EXPN')
            t1 = expn()
            return Ast('**', t, t1)
        return t
            

    def term():
        if (peek('-')):
            consume('-')
            return Ast('-', term())
        else:
            return expn()

    def factor():
        if (peek('INT')):
            value = int(lookahead.lexeme)
            consume('INT')
            ast = Ast('INT')
            ast['value'] = value
            return ast
        else:
            consume('(')
            value = expr()
            consume(')')
            return value

    #begin parse()
    tokens = scan(text)
    index = 0
    lookahead = nextToken()
    value = program()
    if (not peek('EOF')):
        print(f'expecting <EOF>, got {lookahead.lexeme}', file=sys.stderr)
        sys.exit(1)
    return value

def scan(text):
    SPACE_RE = re.compile(r'\s+')
    NIL_RE = re.compile(r'nil')
    BOOL_RE = re.compile(r'true|false')
    INT_RE = re.compile(r'^(\d+_)*\d+$')
    STR_RE = re.compile(r"^'([^'\\]|\\.)*'$")
    CHAR_RE = re.compile(r'.')

    def next_match(text):
        if m := SPACE_RE.match(text):
            return (m, None)
        elif m := NIL_RE.match(text):
            return (m, 'NIL')
        elif m := BOOL_RE.match(text):
            return (m, 'BOOL')
        elif m := INT_RE.match(text):
            return (m, 'INT')
        elif m := STR_RE.match(text):
            return (m, 'STR')
        elif m := INT_RE.match(text):
            return (m, 'INT')
        elif m := CHAR_RE.match(text):  #must be last: match any char
            return (m, m.group())
    tokens = []
    while (len(text) > 0):
        (match, kind) = next_match(text)
        lexeme = match.group()
        if (kind): tokens.append(Token(kind, lexeme))
        text = text[len(lexeme):]
    return tokens

def main():
    if (len(sys.argv) != 2): usage();
    contents = readFile(sys.argv[1]);
    asts = parse(contents)
    print(json.dumps(asts, separators=(',', ':'))) #no whitespace

def readFile(path):
    with open(path, 'r') as file:
        content = file.read()
    return content


def usage():
    print(f'usage: {sys.argv[0]} DATA_FILE')
    sys.exit(1)

#use a dict so that we can add attributes dynamically
def Ast(tag, *kids):
    return { 'tag': tag, } if len(kids) == 0 else { 'tag': tag, 'kids': kids }

Token = namedtuple('Token', ['kind', 'lexeme'])

if __name__ == "__main__":
    main()
