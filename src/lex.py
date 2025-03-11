import ply.lex as lex
import ply.yacc as yacc
import os

# Reserved words
reserved = {
    'Book': 'BOOK',
    'Confirm': 'CONFIRM',
    'Pay': 'PAY',
    'Cancel': 'CANCEL',
    'List': 'LIST',
    'reservation': 'RESERVATION',
    'reserve': 'RESERVE',
    'schedule': 'SCHEDULE',
    'from': 'FROM',
    'to': 'TO',
    'on': 'ON',
    'at': 'AT',
    'for': 'FOR',
    '.': 'END',
}

# Token list
tokens = [
             # Time/date values
             'DATE',
             'TIME',
             # Numeric values
             'INTEGER',
             'STRING',
             'FLOAT',
             # Identifiers
             'IDENTIFIER',
         ] + list(reserved.values())


# Comments (ignored)
def t_comment(t):
    r'\#.*'
    pass


# Date values (Month Day, Year)
def t_date(t):
    r'(January|February|March|April|May|June|July|August|September|October|November|December)\s?\d{1,2},\s?\d{4}'
    return t


# Time values (12 hour time with AM/PM, 24 hour time)
def t_time(t):
    r'((0?[1-9]|1[0-2]):[0-5][0-9]\s?(AM|PM))|(([01]?[0-9]|2[0-3]):[0-5][0-9])'
    return t


# Dot at the end of a statement
def t_end(t):
    r'\.$'
    return t


# Float values
def t_float(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t


# Integer values
def t_integer(t):
    r'\d+'
    t.value = int(t.value)
    return t


# String values
def t_string(t):
    r'\".*?\"'
    t.value = t.value[1:-1]  # Remove quotes
    return t


# Identifiers (including reserved words)
def t_identifier(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'IDENTIFIER')  # Check for reserved words
    return t


# Newline tracking
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


# Ignore whitespace and tabs
t_ignore = ' \t'


# Error handling
def t_error(t):
    print(f"Illegal character '{t.value[0]}' at line {t.lineno}")
    t.lexer.skip(1)


# Build the lexer
lexer = lex.lex()


# --- Parser ---

# Grammar rules
def p_command(p):
    '''
    command : search_command
            | list_command
            | book_command
            | confirm_command
            | pay_command
            | cancel_command
            | view_command
            | history_command
            | help_command
            | exit_command
    '''

    print("Valid command:", p[1])


def p_search_command(p):
    'search_command : SEARCH EVENT STRING'
    p[0] = f"Searching for event: {p[3]}"


def p_list_command(p):
    'list_command : LIST SERVICE STRING FROM STRING TO STRING'
    p[0] = f"Listing service: {p[3]} from {p[5]} to {p[7]}"


def p_book_command(p):
    'book_command : BOOK SERVICE STRING FROM STRING TO STRING ON DATE AT TIME FOR STRING'
    p[0] = f"Booking service: {p[3]} from {p[5]} to {p[7]} on {p[9]} at {p[11]} for {p[13]}"


def p_confirm_command(p):
    'confirm_command : CONFIRM RESERVATION FOR STRING'
    p[0] = f"Confirming reservation for: {p[4]}"


def p_pay_command(p):
    'pay_command : PAY RESERVATION FOR STRING'
    p[0] = f"Paying for reservation: {p[4]}"


def p_cancel_command(p):
    'cancel_command : CANCEL RESERVATION FOR IDENTIFIER'
    p[0] = f"Canceling reservation for: {p[4]}"


def p_view_command(p):
    'view_command : VIEW AVAILABLE SERVICE STRING FROM STRING TO STRING ON DATE'
    p[0] = f"Viewing available service: {p[4]} from {p[6]} to {p[8]} on {p[10]}"


def p_history_command(p):
    'history_command : HISTORY FOR STRING'
    p[0] = f"Viewing history for: {p[3]}"


def p_help_command(p):
    'help_command : HELP'
    p[0] = "Displaying help information"


def p_exit_command(p):
    'exit_command : EXIT'
    p[0] = "Exiting the system"


# Error handling for syntax errors
def p_error(p):
    print("Syntax error in input!")


# Build the parser
parser = yacc.yacc()


def main():
    print("Welcome to APL Booking Project Language (APBL Version 1.0)\n")

    while True:
        try:
            statement = input('APBL> ')
        except EOFError:
            break
        except KeyboardInterrupt:
            print('\n')
            break
        if not statement:
            continue
        elif statement == 'clear' | statement == 'cls':
            os.system('clear')
            print("Welcome to APL Booking Project Language (APBL Version 1.0)\n")
            continue

        # lexer.input(statement)
        parser.parse(input=statement, lexer=lexer)

        # Iterate through all tokens
        for tok in lexer:
            print(tok)
        print('\n')


if __name__ == '__main__':
    main()
