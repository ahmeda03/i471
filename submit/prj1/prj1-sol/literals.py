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
 : '('INT'..'INT')'
 | '('INT'...'INT')'
 ;
str_range
 : '('STR'..'STR')'
 | '('STR'...'STR')'
 ;
"""

def parse(text):

    def peek(kind): return lookahead.kind == kind
    def consume(kind):
        nonlocal lookahead
        if (lookahead.kind == kind):
            lookahead = nextToken()
        else:
            sys.stderr.write('INCORRECT TOKEN FOUND\n')
            sys.exit(1)
    def nextToken():
        nonlocal index
        if (index >= len(tokens)):
            return Token('EOF', '<EOF>')
        else:
            tok = tokens[index]
            index += 1
            return tok

    def literals():
        objects = []
        while (not peek('EOF')):
            objects.append(literal())
        return objects
    
    # add error handling everywhere ASAP
    
    def literal():
        if (peek('NIL')):
            return nil_token()
        elif peek('BOOL'):
            return bool_token()
        elif peek('INT'):
            return int_token()
        elif peek('STR'):
            return str_token()
        elif peek('['):
            return Array()
        elif peek('{'):
            return Hash()
        elif peek('('):
            return Range()
        else:
            sys.stderr.write("NO MATCH\n")
            sys.exit(1)

    def nil_token():
        obj = Literal_Object('nil', None)
        if peek('NIL'):
            consume('NIL')
        else:
            sys.stderr.write('NIL TOKEN NOT FOUND\n')
            sys.exit(1)
        return obj
    
    def bool_token():
        val = True if (lookahead.lexeme).lower() == "true" else False
        obj = Literal_Object('bool', val)
        if peek('BOOL'):
            consume('BOOL')
        else:
            sys.stderr.write('BOOL TOKEN NOT FOUND\n')
            sys.exit(1)
        return obj

    def int_token():
        val = int(lookahead.lexeme)
        obj = Literal_Object('int', val)
        if peek('INT'):
            consume('INT')
        else:
            sys.stderr.write('INT TOKEN NOT FOUND\n')
            sys.exit(1)
        return obj
   
    def str_token():
        val = lookahead.lexeme[1:-1]
        obj = Literal_Object('str', val)
        if peek('STR'):
            consume('STR')
        else:
            sys.stderr.write('STR TOKEN NOT FOUND\n')
            sys.exit(1)
        return obj
    
    def Array():
        if peek('['):
            consume('[')
        else:
            sys.stderr.write('OPENING BRACES NOT FOUND\n')
            sys.exit(1)
    
        arr = []
        while (not peek(']')):
            val = literal()
            arr.append(val)

            if peek(','):
                consume(',')
            else:
                sys.stderr.write('COMMA NOT FOUND\n')
                sys.exit(1)

        if peek(']'):
            consume(']')
        else:
            sys.stderr.write('CLOSING BRACES NOT FOUND\n')
            sys.exit(1)

        obj = Literal_Object("array", arr)
        return obj

    def Hash():
        if peek ('{'):
            consume('{')
        else:
            sys.stderr.write('OPENING CURLY BRACES NOT FOUND\n')
            sys.exit(1)
        
        hashs = []
        while (not peek('}')):
            hash_key = literal()

            if peek('HASH_DELIMITER'):
                consume('HASH_DELIMITER')
            else:
                sys.stderr.write('HASH DELIMITER NOT FOUND\n')
                sys.exit(1)

            hash_val = literal()

            hashs.append({'key': hash_key, 'val': hash_val})

            if peek(','):
                consume(',')
            else:
                sys.stderr.write('COMMA NOT FOUND\n')
                sys.exit(1)

        if peek ('}'):
            consume('}')
        else:
            sys.stderr.write('CLOSING CURLY BRACES NOT FOUND\n')
            sys.exit(1)

        obj = Literal_Object("hash", hashs)

        return obj

    def Range():
        if peek('('):
            consume('(')
        else:
            sys.stderr.write('OPENING PARENTHESIS NOT FOUND\n')
            sys.exit(1)

        if peek('INT'):
            return int_range()
        elif peek('STR'):
            return str_range()
        else:
            sys.stderr.write('INT OR STR TOKEN NOT FOUND\n')
            sys.exit(1)

    def int_range():
        first_val = int_token()
        
        if peek('CLOSED_RANGE_DELIMITER'):
            obj = Literal_Object('closed_range', None)
            consume('CLOSED_RANGE_DELIMITER')
        elif peek('HALF_OPEN_RANGE_DELIMITER'):
            obj = Literal_Object('half_open_range', None)
            consume('HALF_OPEN_RANGE_DELIMITER')
        else:
            sys.stderr.write('INCORRECT TOKEN, EXPECTING \'..\' or \'...\'\n')
            sys.exit(1)

        if peek('INT'):
            second_val = int_token()
        else:
            sys.stderr.write('INT TOKEN NOT FOUND')
            sys.exit(1)

        obj['val'] = [first_val, second_val]

        if peek(')'):
            consume(')')
        else:
            sys.stderr.write('CLOSING PARENTHESIS NOT FOUND\n')
            sys.exit(1)

        return obj
        

    def str_range():
        first_val = str_token()
        
        if peek('CLOSED_RANGE_DELIMITER'):
            obj = Literal_Object('closed_range', None)
            consume('CLOSED_RANGE_DELIMITER')
        elif peek('HALF_OPEN_RANGE_DELIMITER'):
            obj = Literal_Object('half_open_range', None)
            consume('HALF_OPEN_RANGE_DELIMITER')
        else:
            sys.stderr.write('INCORRECT TOKEN, EXPECTING \'..\' or \'...\'\n')
            sys.exit(1)

        if peek('STR'):
            second_val = str_token()
        else:
            sys.stderr.write('INT TOKEN NOT FOUND')
            sys.exit(1)

        obj['val'] = [first_val, second_val]

        if peek(')'):
            consume(')')
        else:
            sys.stderr.write('CLOSING PARENTHESIS NOT FOUND\n')
            sys.exit(1)

        return obj

    #begin parse()
    tokens = scan(text)
    index = 0
    lookahead = nextToken()
    val = literals()
    if (not peek('EOF')):
        print(f'expecting <EOF>, got {lookahead.lexeme}', file=sys.stderr)
        sys.exit(1)
    return val

def scan(text):
    SPACE_AND_COMMENTS_RE = re.compile(r'\s+|#.*|\n')
    NIL_RE = re.compile(r'nil')
    BOOL_RE = re.compile(r'true|false')
    INT_RE = re.compile(r'(\d+_)*\d+')
    STR_RE = re.compile(r"'([^'\\]|\\.)*'", re.S)
    HASH_DELIMITER = re.compile(r'=>')
    HALF_OPEN_RANGE_DELIMITER = re.compile(r'\.\.\.')
    CLOSED_RANGE_DELIMITER = re.compile(r'\.\.')
    CHAR_RE = re.compile(r'.')

    def next_match(text):
        m = SPACE_AND_COMMENTS_RE.match(text)
        if (m): return (m, None)
        
        m = NIL_RE.match(text)
        if (m): return (m, 'NIL')
        
        m = BOOL_RE.match(text)
        if (m): return (m, 'BOOL')

        m = INT_RE.match(text)
        if (m): return (m, 'INT')
        
        m = STR_RE.match(text)
        if (m): return (m, 'STR')
        
        m = HASH_DELIMITER.match(text)
        if (m): return (m, 'HASH_DELIMITER')
        
        m = HALF_OPEN_RANGE_DELIMITER.match(text)
        if (m): return (m, 'HALF_OPEN_RANGE_DELIMITER')
        
        m = CLOSED_RANGE_DELIMITER.match(text)
        if (m): return (m, 'CLOSED_RANGE_DELIMITER')

        m = CHAR_RE.match(text)  #must be last: match any char
        if (m): return (m, m.group())
    
    tokens = []
    while (len(text) > 0):
        (match, kind) = next_match(text)
        lexeme = match.group()
        if (kind): tokens.append(Token(kind, lexeme))
        text = text[len(lexeme):]
    return tokens

def main():
    contents = sys.stdin.read()
    result = parse(contents)
    print(json.dumps(result, separators=(',', ':'))) #no whitespace 

def Literal_Object(tag, val):
    return { 'tag': tag, 'val': val }

Token = namedtuple('Token', ['kind', 'lexeme'])

if __name__ == "__main__":
    main()
