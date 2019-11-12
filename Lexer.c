/* Lexical Analyzer for B-Minor Language */
/* Developed by: Ali Alimohammadi */

#define _CRT_SECURE_NO_WARNINGS

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>
#include <stdbool.h>
#include <limits.h>

#define NELEMS(arr) (sizeof(arr) / sizeof(arr[0]))

#define da_dim(name, type)  type *name = NULL;          \
                            int _qy_ ## name ## _p = 0;  \
                            int _qy_ ## name ## _max = 0
#define da_rewind(name)     _qy_ ## name ## _p = 0
#define da_redim(name)      do {if (_qy_ ## name ## _p >= _qy_ ## name ## _max) \
                                name = realloc(name, (_qy_ ## name ## _max += 32) * sizeof(name[0]));} while (0)
#define da_append(name, x)  do {da_redim(name); name[_qy_ ## name ## _p++] = x;} while (0)
#define da_len(name)        _qy_ ## name ## _p

#define MAX_ID 1000		/* maximum number of identifiers */

#define NONE "None"

/* Output format for each type */
#define IDENTIFIER "Identifier\tID: %d ---> %s"
#define DELIMITER "Delimiter\t%s"
#define KEYWORD "Keyword\t\t%d"
#define OPERAND "Operand\t\t%s"
#define NUMERICAL "Number\t\t%d"
#define STRING "String\t\t%d ---> Address of \"%s\" in strings buffer"


typedef enum {
	tk_Array = 1, tk_Bool, tk_Char, tk_Else, tk_False, tk_For, tk_Function, tk_If, tk_Integer, tk_Print,
	tk_Return, tk_String, tk_True, tk_Void, tk_While, tk_Colon, tk_EOI, tk_Exp, tk_Mul, tk_Div, tk_Mod,
	tk_Add, tk_Sub, tk_Negate, tk_Not, tk_Lss, tk_Leq, tk_Gtr, tk_Geq, tk_Eq, tk_Neq, tk_Inc, tk_Dec,
	tk_Assign, tk_And, tk_Or, tk_LParen, tk_RParen, tk_CLBrace, tk_CRBrace, tk_SLBrace, tk_SRBrace,
	tk_Semicolon, tk_Comma, tk_Num, tk_Str, tk_Identifier
} TokenType;

typedef struct {
	TokenType token;
	int err_ln, err_col;
	union {
		int n;                  /* value for constants */
		char *text;             /* text for identifiers */
	};
} Token;

static FILE *source_fp, *dest_fp;
static char symbol_table[MAX_ID][32] = { 0 }, current_char = ' ';
static int top_id = 0, line = 1, col = 0;
da_dim(text, char);

Token getTokenType();

static void error(int err_line, int err_col, const char *fmt, ...)
{
	char buffer[1000];
	va_list ap;

	va_start(ap, fmt);
	vsprintf(buffer, fmt, ap);
	va_end(ap);

	printf("(%d, %d) Error: %s\n", err_line, err_col, buffer);
	exit(1);
}

static int getNextChar()	/* get next char from input file */
{	
	current_char = getc(source_fp);
	col++;
	if (current_char == '\n')
	{
		line++;
		col = 0;
	}
	return current_char;
}

static Token charLiteral(int n, int err_line, int err_col)		/* 'c' */
{
	if (current_char == '\'')
		error(err_line, err_col, "getTokenType: Empty character constant!");
	if (current_char == '\\') {
		getNextChar();
		if (current_char == 'n')
			n = 10;
		else if (current_char == '\\')
			n = '\\';
		else error(err_line, err_col, "getTokenType: Unknown escape sequence \\%c", current_char);
	}
	if (getNextChar() != '\'')
		error(err_line, err_col, "Multi-character constant");
	getNextChar();
	return (Token) { tk_Integer, err_line, err_col, { n } };
}

static Token divisionOrComment(int err_line, int err_col)		/* differentiate division and comments */
{
	/* single line comment */
	if (current_char == '/')
	{
		getNextChar();
		while (true)
		{
			if (current_char == '\n')
			{
				getNextChar();
				return getTokenType();
			}
			getNextChar();
		}
	}

	/* division */
	if (current_char != '*')
		return (Token) { tk_Div, err_line, err_col, { 0 } };

	/* multi-line comment */
	getNextChar();
	while (true)
	{
		if (current_char == '*')
		{
			if (getNextChar() == '/')
			{
				getNextChar();
				return getTokenType();
			}
		}
		else if (current_char == EOF)
			error(err_line, err_col, "EOF in comment");
		else
			getNextChar();
	}
}

static Token stringLiteral(int start, int err_line, int err_col)		/* "string" */
{
	da_rewind(text);
	while (getNextChar() != start)
	{
		if (current_char == '\n') error(err_line, err_col, "EOL in string");
		if (current_char == EOF)  error(err_line, err_col, "EOF in string");
		da_append(text, (char)current_char);
	}
	da_append(text, '\0');

	getNextChar();
	return (Token) { tk_Str, err_line, err_col, { .n = start, .text = text } };
}

static int kwd_cmp(const void *p1, const void *p2)
{
	return strcmp(*(char **)p1, *(char **)p2);
}

static TokenType getIdentifierType(const char *identifier)
{
	static struct {
		char *s;
		TokenType sym;
	} keywords[] = {
		{ "array", tk_Array },
		{ "boolean", tk_Bool },
		{ "char", tk_Char },
		{ "else", tk_Else },
		{ "false", tk_False },
		{ "for" , tk_For },
		{ "function", tk_Function },
		{ "if", tk_If },
		{ "integer", tk_Integer },
		{ "print", tk_Print },
		{ "return", tk_Return },
		{ "string", tk_String },
		{ "true", tk_True },
		{ "void", tk_Void },
		{ "while", tk_While }
	}, *kwp;

	return (kwp = bsearch(&identifier, keywords, NELEMS(keywords), sizeof(keywords[0]), kwd_cmp)) == NULL ? tk_Identifier : kwp->sym;
}

static Token identifierOrInteger(int err_line, int err_col)
{
	int n, is_number = true;

	da_rewind(text);
	while (isalnum(current_char) || current_char == '_')
	{
		da_append(text, (char)current_char);
		if (!isdigit(current_char))
			is_number = false;
		getNextChar();
	}
	if (da_len(text) == 0)
		error(err_line, err_col, "getTokenType: Unrecognized character (%d) '%c'", current_char, current_char);
	da_append(text, '\0');
	if (isdigit(text[0]))
	{
		if (!is_number)
			error(err_line, err_col, "Invalid number: %s", text);
		n = strtol(text, NULL, 0);
		if (n == LONG_MAX && errno == ERANGE)
			error(err_line, err_col, "Number exceeds maximum value.");
		return (Token) { tk_Num, err_line, err_col, { n } };
	}
	return (Token) { getIdentifierType(text), err_line, err_col, { .text = text } };
}

static Token followedBy(char expected, TokenType ifyes, TokenType ifno, int err_line, int err_col)		/* look ahead for '>=', etc. */
{
	if (current_char == expected)
	{
		getNextChar();
		return (Token) { ifyes, err_line, err_col, NONE };
	}
	if (ifno == tk_EOI)
		error(err_line, err_col, "follow: Unrecognized character '%c' (%d)\n", current_char, current_char);
	return (Token) { ifno, err_line, err_col, NONE };
}

Token getTokenType()
{            /* return the token type */
							/* skip white space */
	while (isspace(current_char))
		getNextChar();
	int err_line = line;
	int err_col = col;
	switch (current_char)
	{
		case '{':  getNextChar(); return (Token) { tk_CLBrace, err_line, err_col, NONE };
		case '}':  getNextChar(); return (Token) { tk_CRBrace, err_line, err_col, NONE };
		case '[':  getNextChar(); return (Token) { tk_SLBrace, err_line, err_col, NONE };
		case ']':  getNextChar(); return (Token) { tk_SRBrace, err_line, err_col, NONE };
		case '(':  getNextChar(); return (Token) { tk_LParen, err_line, err_col, NONE };
		case ')':  getNextChar(); return (Token) { tk_RParen, err_line, err_col, NONE };
		case '^':  getNextChar(); return (Token) { tk_Exp, err_line, err_col, NONE };
		case '*':  getNextChar(); return (Token) { tk_Mul, err_line, err_col, NONE };
		case '%':  getNextChar(); return (Token) { tk_Mod, err_line, err_col, NONE };
		case ':':  getNextChar(); return (Token) { tk_Colon, err_line, err_col, NONE };
		case ';':  getNextChar(); return (Token) { tk_Semicolon, err_line, err_col, NONE };
		case ',':  getNextChar(); return (Token) { tk_Comma, err_line, err_col, NONE };
		case '/':  getNextChar(); return divisionOrComment(err_line, err_col);
		case '\'': getNextChar(); return charLiteral(current_char, err_line, err_col);
		case '+':  getNextChar(); return followedBy('+', tk_Inc, tk_Add, err_line, err_col);
		case '-':  getNextChar(); return followedBy('-', tk_Dec, tk_Sub, err_line, err_col);
		case '<':  getNextChar(); return followedBy('=', tk_Leq, tk_Lss, err_line, err_col);
		case '>':  getNextChar(); return followedBy('=', tk_Geq, tk_Gtr, err_line, err_col);
		case '=':  getNextChar(); return followedBy('=', tk_Eq, tk_Assign, err_line, err_col);
		case '!':  getNextChar(); return followedBy('=', tk_Neq, tk_Not, err_line, err_col);
		case '&':  getNextChar(); return followedBy('&', tk_And, tk_EOI, err_line, err_col);
		case '|':  getNextChar(); return followedBy('|', tk_Or, tk_EOI, err_line, err_col);
		case '"':  return stringLiteral(current_char, err_line, err_col);
		default:   return identifierOrInteger(err_line, err_col);
		case EOF:  return (Token) { tk_EOI, err_line, err_col, NONE };
	}
}

void tokenize()				/* recognize the tokens and their type */
{
	Token token;
	do {
		token = getTokenType();
		
		switch (token.token)
		{
			case tk_Array:		fprintf(dest_fp, KEYWORD, tk_Array);			break;
			case tk_Char:		fprintf(dest_fp, KEYWORD, tk_Char);				break;
			case tk_Bool:		fprintf(dest_fp, KEYWORD, tk_Bool);				break;
			case tk_Else:		fprintf(dest_fp, KEYWORD, tk_Else);				break;
			case tk_False:		fprintf(dest_fp, KEYWORD, tk_False);			break;
			case tk_For:		fprintf(dest_fp, KEYWORD, tk_For);				break;
			case tk_Function:	fprintf(dest_fp, KEYWORD, tk_Function);			break;
			case tk_If:			fprintf(dest_fp, KEYWORD, tk_If);				break;
			case tk_Integer:	fprintf(dest_fp, KEYWORD, tk_Integer);			break;
			case tk_Print:		fprintf(dest_fp, KEYWORD, tk_Print);			break;
			case tk_Return:		fprintf(dest_fp, KEYWORD, tk_Return);			break;
			case tk_String:		fprintf(dest_fp, KEYWORD, tk_String);			break;
			case tk_True:		fprintf(dest_fp, KEYWORD, tk_True);				break;
			case tk_Void:		fprintf(dest_fp, KEYWORD, tk_Void);				break;
			case tk_While:		fprintf(dest_fp, KEYWORD, tk_While);			break;
			case tk_Colon:		fprintf(dest_fp, DELIMITER, ":");				break;
			case tk_Semicolon:	fprintf(dest_fp, DELIMITER, ";");				break;
			case tk_Comma:		fprintf(dest_fp, DELIMITER, ",");				break;
			case tk_SLBrace:	fprintf(dest_fp, OPERAND, "[");					break;
			case tk_SRBrace:	fprintf(dest_fp, OPERAND, "]");					break;
			case tk_CLBrace:	fprintf(dest_fp, DELIMITER, "{");				break;
			case tk_CRBrace:	fprintf(dest_fp, DELIMITER, "}");				break;
			case tk_LParen:		fprintf(dest_fp, DELIMITER, "(");				break;
			case tk_RParen:		fprintf(dest_fp, DELIMITER, ")");				break;
			case tk_Assign:		fprintf(dest_fp, OPERAND, "=");					break;
			case tk_Exp:		fprintf(dest_fp, OPERAND, "^");					break;
			case tk_Add:		fprintf(dest_fp, OPERAND, "+");					break;
			case tk_Sub:		fprintf(dest_fp, OPERAND, "-");					break;
			case tk_Inc:		fprintf(dest_fp, OPERAND, "++");				break;
			case tk_Dec:		fprintf(dest_fp, OPERAND, "--");				break;
			case tk_Mul:		fprintf(dest_fp, OPERAND, "*");					break;
			case tk_Div:		fprintf(dest_fp, OPERAND, "/");					break;
			case tk_Mod:		fprintf(dest_fp, OPERAND, "%");					break;
			case tk_Eq:			fprintf(dest_fp, OPERAND, "==");				break;
			case tk_Geq:		fprintf(dest_fp, OPERAND, ">=");				break;
			case tk_Leq:		fprintf(dest_fp, OPERAND, "<=");				break;
			case tk_Gtr:		fprintf(dest_fp, OPERAND, ">");					break;
			case tk_Lss:		fprintf(dest_fp, OPERAND, "<");					break;
			case tk_Neq:		fprintf(dest_fp, OPERAND, "!=");				break;
			case tk_Not:		fprintf(dest_fp, OPERAND, "!");					break;
			case tk_And:		fprintf(dest_fp, OPERAND, "&&");				break;
			case tk_Or:			fprintf(dest_fp, OPERAND, "||");				break;
			case tk_Num:		fprintf(dest_fp, NUMERICAL, token.n);			break;
			case tk_Str:		fprintf(dest_fp, STRING, token.n, token.text);	break;
			case tk_Identifier:
			{
				bool exists = false;
				for (int id = 1; id <= top_id; id++)
					if (!strcmp(token.text, symbol_table[id]))
					{
						fprintf(dest_fp, IDENTIFIER, id, token.text);
						exists = true;
						break;
					}
				if (!exists)
				{
					strcpy(symbol_table[++top_id], token.text);
					fprintf(dest_fp, IDENTIFIER, top_id, token.text);
				}
			}
		}
		fprintf(dest_fp, "\n");
	} while (token.token != tk_EOI);
	if (dest_fp != stdout)
		fclose(dest_fp);
}

void initializeIO(FILE **fp, FILE *std, const char mode[], const char fn[])
{
	if (fn[0] == '\0')
		*fp = std;
	else if ((*fp = fopen(fn, mode)) == NULL)
		error(0, 0, "Can't open %s\n", fn);
}

int main(int argc, char *argv[])
{
	/* specify input file path */
	initializeIO(&source_fp, stdin, "r", argc > 0 ? argv[0] : "");
	/* specify output file path */
	initializeIO(&dest_fp, stdout, "w", argc > 1 ? argv[1] : "");
	/* tokenize the given input */
	tokenize();
	return 0;
}