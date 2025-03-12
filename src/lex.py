import ply.lex as lex
import ply.yacc as yacc
import os
import calendar

# Reserved words
reserved = {
    # Commands
    "Book": "KEYWORD_BOOK",
    "Ticket": "KEYWORD_TICKET",
    "Tickets": "KEYWORD_TICKETS",
    "Confirm": "KEYWORD_CONFIRM",
    "Pay": "KEYWORD_PAY",
    "Cancel": "KEYWORD_CANCEL",
    "List": "KEYWORD_LIST",
    "View": "KEYWORD_VIEW",
    "History": "KEYWORD_HISTORY",
    "Help": "KEYWORD_HELP",
    "Exit": "KEYWORD_EXIT",
    # Reserved words
    "reservation": "RESERVATION",
    "reservations": "RESERVATIONS",
    "schedule": "SCHEDULE",
    "from": "FROM",
    "to": "TO",
    "on": "ON",
    "at": "AT",
    "for": "FOR",
    ".": "SYM_END",
}

# Token list
tokens = [
    # Time/date values
    "DATE",
    "TIME",
    # Numeric values
    "INTEGER",
    "STRING",
    "FLOAT",
    # Identifiers
    "IDENTIFIER",
] + list(reserved.values())


# Comments (ignored)
def t_comment(t):
    r"""\#.*"""
    pass


# Date values (Month Day, Year)
def t_date(t):
    r"""(January|February|March|April|May|June|July|August|September|October|November|December)\s?\d{1,2},\s?\d{4}"""
    t.type = "DATE"
    return t


# Time values (12 hour time with AM/PM, 24 hour time)
def t_time(t):
    r"""((0?[1-9]|1[0-2]):[0-5][0-9]\s?(AM|PM))|(([01]?[0-9]|2[0-3]):[0-5][0-9])"""
    t.type = "TIME"
    return t


# Dot at the end of a statement
def t_end(t):
    r"""\.$"""
    t.type = "SYM_END"
    return t


# Float values
def t_float(t):
    r"""\d+\.\d+"""
    t.value = float(t.value)
    return t


# Integer values
def t_integer(t):
    r"""-?\d+"""
    t.value = int(t.value)
    t.type = "INTEGER"
    return t


# String values
def t_string(t):
    r"""\".*?\" """
    t.value = t.value[1:-1]  # Remove quotes
    t.type = "STRING"
    return t


# Identifiers (including reserved words)
def t_identifier(t):
    r"""[a-zA-Z_][a-zA-Z_0-9]*"""
    t.type = reserved.get(t.value, "IDENTIFIER")  # Check for reserved words
    return t


# Newline tracking
def t_newline(t):
    r"""\n+"""
    t.lexer.lineno += len(t.value)


# Ignore whitespace and tabs
t_ignore = " \t"


# Error handling
def t_error(t):
    print(f"Illegal character '{t.value[0]}' at line {t.lineno}")
    t.lexer.skip(1)


# Build the lexer
lexer = lex.lex()


# Map of months to their corresponding numbers
months = {
    "January": 1,
    "February": 2,
    "March": 3,
    "April": 4,
    "May": 5,
    "June": 6,
    "July": 7,
    "August": 8,
    "September": 9,
    "October": 10,
    "November": 11,
    "December": 12,
}


# --- Parser ---


# Grammar rules
def p_command(p):
    """
    command : book_command
            | confirm_command
            | pay_command
            | cancel_command
            | list_command
            | view_command
            | history_command
            | help_command
            | exit_command
    """

    print("Valid command:", p[1])


def p_identifier_list(p):
    """
    identifier_list : IDENTIFIER identifier_list
                    | IDENTIFIER
                    | STRING
    """

    if len(p) == 3:
        p[0] = f"{p[1]} {p[2]}"
    else:
        p[0] = p[1]

    if len(p) == 3:
        p[0] = f"{p[1]} {p[2]}"  # Accumulate identifiers


# Example commands
# Book Ticket for Knutsford Express from Montego Bay to Kingston on February 17, 2025 at 8:30 AM for Joy Reynolds.
# Book 2 Tickets for Knutsford Express from Montego Bay to Kingston on February 17, 2025 at 8:30 AM for Joy Reynolds.

# Testing the date validation
# Book 2 Tickets for Knutsford Express from Montego Bay to Kingston on February 29, 2025 at 8:30 AM for Joy Reynolds.
# Book 2 Tickets for Knutsford Express from Montego Bay to Kingston on February 29, 2025 at 8:30 AM for Joy Reynolds.


def p_book_command(p):
    """
    book_command : KEYWORD_BOOK KEYWORD_TICKET FOR identifier_list FROM identifier_list TO identifier_list ON DATE AT TIME FOR identifier_list SYM_END
                 | KEYWORD_BOOK INTEGER KEYWORD_TICKETS FOR identifier_list FROM identifier_list TO identifier_list ON DATE AT TIME FOR identifier_list SYM_END
    """

    # Specific the numbers of tickets the user wants
    if type(p[2]) is int:

        # Validation for date
        month_day, year = p[11].split(",")
        month, day = month_day.split(" ")

        day = int(day)
        month = months[month]
        year = int(year)

        if p[2] < 1:
            print(
                "Error: The number of tickets MUST be a positive number. Great than 0!"
            )
        else:
            if day not in range(1, calendar.monthrange(year, month)[1] + 1):
                print(
                    f"Error: The day {day} does not exist in {calendar.month_name[month]} {year}."
                )
            else:
                p[0] = (
                    f"Booking {p[2]} tickets for {p[5]} from {p[7]} to {p[9]} on {p[11]} at {p[13]} for {p[15]}."
                )
    # The user only wants on ticket
    else:
        # Validation for date
        month_day, year = p[10].split(",")
        month, day = month_day.split(" ")

        day = int(day)
        month = months[month]
        year = int(year)

        if day not in range(1, calendar.monthrange(year, month)[1] + 1):
            print(
                f"Error: The day {day} does not exist in {calendar.month_name[month]} {year}."
            )
        else:
            if day not in range(1, calendar.monthrange(year, month)[1] + 1):
                print(
                    f"Error: The day {day} does not exist in {calendar.month_name[month]} {year}."
                )
            else:
                p[0] = (
                    f"Booking a ticket for {p[2]} from {p[4]} to {p[6]} on {p[8]} at {p[10]} for {p[12]}."
                )


# Examples:
#  Confirm reservation for Knutsford Express for Joy Reynolds.
#  Confirm 3 reservations for Knutsford Express for Joy Reynolds.


def p_confirm_command(p):
    """
    confirm_command : KEYWORD_CONFIRM RESERVATION FOR identifier_list FOR identifier_list SYM_END
                    | KEYWORD_CONFIRM INTEGER RESERVATIONS FOR identifier_list FOR identifier_list SYM_END
    """

    if type(p[2]) is int:
        if p[2] < 1:
            print(
                "Error: The number of reservations MUST be a positive number. Great than 0!"
            )
        else:
            p[0] = f"Confirming {p[2]} reservations for {p[5]} for {p[7]}."
    else:
        p[0] = f"Confirming reservation for {p[4]} for {p[6]}."

    # Examples:
    #  Pay reservation for Knutsford Express for Joy Reynolds.


def p_pay_command(p):
    """pay_command : KEYWORD_PAY RESERVATION FOR identifier_list FOR identifier_list SYM_END"""

    p[0] = f"Paying reservation for {p[4]} for {p[6]}."


# Cancel reservations for a particular person.
def p_cancel_command(p):
    """cancel_command : KEYWORD_CANCEL RESERVATION FOR identifier_list FOR identifier_list SYM_END"""

    p[0] = f"Cancelling reservation for {p[4]} for {p[6]}."


# List all the available schedules from a hotel/company.
def p_list_command(p):
    """
    list_command : KEYWORD_LIST identifier_list SCHEDULE SYM_END
    """

    p[0] = f"List of available schedules for {p[2]}"


# Displays all the current schedules for a person.
def p_view_command(p):
    """view_command : KEYWORD_VIEW SCHEDULE FOR identifier_list SYM_END"""

    p[0] = f"Viewing current schedules for {p[4]}."


# Views all the schedules for a person.
def p_history_command(p):
    """history_command : KEYWORD_HISTORY FOR identifier_list SYM_END"""
    p[0] = f"Viewing history for {p[3]}"


def p_help_command(p):
    """help_command : KEYWORD_HELP SYM_END"""

    p[0] = (
        "Displaying help information:\n"
        "  book_command: book <identifiers> from <identifiers> to <identifiers> on <date> at <time> for <identifiers>.\n"
        "  confirm_command: confirm reservation for <identifiers> for <identifiers>.\n"
        "  pay_command: pay reservation for <identifiers> for <identifiers>.\n"
        "  cancel_command: cancel reservation for <identifiers> for <identifiers>.\n"
        "  list_command: list <identifiers> schedule.\n"
        "  view_command: view schedule for <identifiers>.\n"
        "  history_command: history for <identifiers>.\n"
        "  help_command: help.\n"
        "  exit_command: exit."
    )


def p_exit_command(p):
    """exit_command : KEYWORD_EXIT SYM_END"""

    p[0] = "Exiting the system"
    exit()


# FIXME : Need to output the corresponding error message for each command
# Error handling
def p_error(p):
    if p:
        if (
            p.type == "FROM"
            or p.type == "TO"
            or p.type == "ON"
            or p.type == "AT"
            or p.type == "DATE"
            or p.type == "TIME"
        ):
            print(
                "Syntax error: Incorrect order. The correct format is: KEYWORD_BOOK <service> FROM <location> TO <location> ON <date> AT <time> FOR <person> SYM_END"
            )
        elif p.type == "RESERVATION":
            if (
                p.lexer.lexpos > 0
                and p.lexer.lexdata[p.lexer.lexpos - 1 : p.lexer.lexpos] == " "
            ):
                if p.lexer.lexdata[p.lexer.lexpos - 2 : p.lexer.lexpos - 1] == "L":
                    print(
                        "Syntax error: Incorrect order. The correct format is: KEYWORD_CANCEL RESERVATION FOR <identifier> FOR <identifier> SYM_END"
                    )
                elif p.lexer.lexdata[p.lexer.lexpos - 2 : p.lexer.lexpos - 1] == "Y":
                    print(
                        "Syntax error: Incorrect order. The correct format is: KEYWORD_PAY RESERVATION FOR <identifier> FOR <identifier> SYM_END"
                    )
                elif p.lexer.lexdata[p.lexer.lexpos - 2 : p.lexer.lexpos - 1] == "M":
                    print(
                        "Syntax error: Incorrect order. The correct format is: KEYWORD_CONFIRM RESERVATION FOR <identifier> FOR <identifier> SYM_END"
                    )
                else:
                    print(
                        "Syntax error: Incorrect order. The correct format is: KEYWORD_CONFIRM/PAY/CANCEL RESERVATION FOR <identifier> FOR <identifier> SYM_END"
                    )
            else:
                print(
                    "Syntax error: Incorrect order. The correct format is: KEYWORD_CONFIRM/PAY/CANCEL RESERVATION FOR <identifier> FOR <identifier> SYM_END"
                )
        elif p.type == "SCHEDULE":
            if (
                p.lexer.lexpos > 0
                and p.lexer.lexdata[p.lexer.lexpos - 1 : p.lexer.lexpos] == " "
            ):
                if p.lexer.lexdata[p.lexer.lexpos - 2 : p.lexer.lexpos - 1] == "T":
                    print(
                        "Syntax error: Incorrect order. The correct format is: KEYWORD_VIEW SCHEDULE FOR <person> SYM_END"
                    )
                elif p.lexer.lexdata[p.lexer.lexpos - 2 : p.lexer.lexpos - 1] == "T":
                    print(
                        "Syntax error: Incorrect order. The correct format is: KEYWORD_LIST <service> SCHEDULE SYM_END"
                    )
                else:
                    print(
                        "Syntax error: Incorrect order. The correct format is: KEYWORD_LIST/VIEW <service> SCHEDULE SYM_END"
                    )
            else:
                print(
                    "Syntax error: Incorrect order. The correct format is: KEYWORD_LIST/VIEW <service> SCHEDULE SYM_END"
                )

        elif p.type == "FOR":
            if (
                p.lexer.lexpos > 0
                and p.lexer.lexdata[p.lexer.lexpos - 1 : p.lexer.lexpos] == " "
            ):
                if p.lexer.lexdata[p.lexer.lexpos - 2 : p.lexer.lexpos - 1] == "Y":
                    print(
                        "Syntax error: Incorrect order. The correct format is: KEYWORD_HISTORY FOR <person> SYM_END"
                    )
                else:
                    print(
                        "Syntax error: Incorrect order. The correct format is: KEYWORD_HISTORY FOR <person> SYM_END"
                    )
            else:
                print(
                    "Syntax error: Incorrect order. The correct format is: KEYWORD_HISTORY FOR <person> SYM_END"
                )

        elif p.type == "SYM_END":
            if (
                p.lexer.lexpos > 0
                and p.lexer.lexdata[p.lexer.lexpos - 1 : p.lexer.lexpos] == " "
            ):
                if p.lexer.lexdata[p.lexer.lexpos - 2 : p.lexer.lexpos - 1] == "P":
                    print(
                        "Syntax error: Incorrect order. The correct format is: KEYWORD_HELP SYM_END"
                    )
                elif p.lexer.lexdata[p.lexer.lexpos - 2 : p.lexer.lexpos - 1] == "T":
                    print(
                        "Syntax error: Incorrect order. The correct format is: KEYWORD_EXIT SYM_END"
                    )
                else:
                    print(
                        "Syntax error: Incorrect order. The correct format is: KEYWORD_HELP/EXIT SYM_END"
                    )
            else:
                print(
                    "Syntax error: Incorrect order. The correct format is: KEYWORD_HELP/EXIT SYM_END"
                )

        elif p.type == "IDENTIFIER":
            print(
                "Syntax error: Missing keywords. The correct format is: KEYWORD_BOOK <service> FROM <location> TO <location> ON <date> AT <time> FOR <person> SYM_END"
            )
        else:
            print(f"Syntax error: Unexpected token '{p.type}' at line {p.lineno}")
    else:
        print("Syntax error: Unexpected end of input")


# Build the parser
parser = yacc.yacc()


def main():
    print("Welcome to APL Booking Project Language (APBL Version 1.0)\n")

    while True:
        try:
            s = input("APBL> ")
        except EOFError:
            break
        except KeyboardInterrupt:
            print("\n")
            break
        if not s:
            continue
        elif s == "clear" or s == "cls":
            os.system("clear")
            print("Welcome to APL Booking Project Language (APBL Version 1.0)\n")
            continue

        # lexer.input(s)
        parser.parse(input=s, lexer=lexer)
        print("\n")

    # Iterate through all tokens
    # for tok in lexer:
    #     print(tok)
    # print('\n')


if __name__ == "__main__":
    main()
