#!/usr/bin/env python3

import re
import sys
import json

from collections import namedtuple


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

#use nested function for encapsulation.
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
        values = []
        while (not peek('EOF')):
            values.append(expr())
            consume(';')
        return values

    def expr():
        t = term()
        while (peek('+') or (peek('-'))):
            kind = lookahead.kind
            consume(kind)
            t1 = term()
            t += (t1 if (kind == '+') else -t1)
        return t

    def term():
        if (peek('-')):
            consume('-')
            return - term()
        else:
            return expn()

    def expn():
        t = factor()
        if (peek('EXP')):
            consume('EXP')
            t = t ** expn()
        return t

    def factor():
        if (peek('INT')):
            value = int(lookahead.lexeme)
            consume('INT')
            return value
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
    result = parse(contents)
    print(json.dumps(result, separators=(',', ':')))

def readFile(path):
    with open(path, 'r') as file:
        content = file.read()
    return content

Token = namedtuple('Token', ['kind', 'lexeme'])

def usage():
    print(f'usage: {sys.argv[0]} DATA_FILE')
    sys.exit(1)

if __name__ == "__main__":
    main()
