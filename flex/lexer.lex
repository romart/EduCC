


B [0-1]
O [0-7]
D [0-9]
NZ [1-9]
L [a-zA-Z_]
A [a-zA-Z_0-9]
H [a-fA-F0-9]
HP (0[xX])
E ([Ee][+-]?{D}+)
P ([Pp][+-]?{D}+)
BP (b|B)
FS (f|F|l|L)
IS (((u|U)(l|L|ll|LL)?)|((l|L|ll|LL)(u|U)?)) 
CP (L)
SP (u8|u|U|L)
ES (\\(['"\?\\abfnrtv]|[0-7]{1,3}|x[a-fA-F0-9]+))
WS [ \t\v\f]


%option reentrant bison-locations

%{

#include "tokens.h"

int fileno(FILE *stream);

#define YY_USER_ACTION         \
  *yylloc += yyleng;


%}

%x COMMENT
%x PP
%x STRING

%%

"/"+"*"                  { BEGIN(COMMENT); }
<COMMENT>[^*\n]*
<COMMENT>[^*\n]*\n         { return NEWLINE; }
<COMMENT>"*"+[^*/\n]*
<COMMENT>"*"+[^*/\n]*\n    { return NEWLINE; }
<COMMENT>"*"+"/"           { BEGIN(INITIAL); }
<COMMENT><<EOF>>           { BEGIN(INITIAL); return(UNTERMINATED_COMMENT); }


"/"+"/"+[^\n]*   { }

"\""                    { BEGIN(STRING); }
<STRING>[^\"\n]*        { return(STRING_LITERAL); }
<STRING>"\n"            { BEGIN(INITIAL); return(DANGLING_NEWLINE); }
<STRING>"\""            { BEGIN(INITIAL); }

"#"                     { BEGIN(PP); }
<PP>[^\n\\]*            { return(PP_TOKEN); }
<PP>"\\"+(WS)*\n        { return NEWLINE; }
<PP>"\n"                { BEGIN(INITIAL); return NEWLINE; }


"break"					{ return(BREAK); }
"case"					{ return(CASE); }
"char"					{ return(CHAR); }
"const"				  { return(CONST); }
"continue"			{ return(CONTINUE); }
"default"				{ return(DEFAULT); }
"do"				  	{ return(DO); }
"double"				{ return(DOUBLE); }
"else"					{ return(ELSE); }
"enum"					{ return(ENUM); }
"extern"				{ return(EXTERN); }
"float"					{ return(FLOAT); }
"for"				  	{ return(FOR); }
"goto"					{ return(GOTO); }
"if"					  { return(IF); }
"int"					  { return(INT); }
"long"					{ return(LONG); }
"register"			{ return(REGISTER); }
"volatile"			{ return(VOLATILE); }
"return"				{ return(RETURN); }
"short"					{ return(SHORT); }
"signed"				{ return(SIGNED); }
"sizeof"				{ return(SIZEOF); }
"static"				{ return(STATIC); }
"struct"				{ return(STRUCT); }
"switch"				{ return(SWITCH); }
"typedef"				{ return(TYPEDEF); }
"union"					{ return(UNION); }
"unsigned"			{ return(UNSIGNED); }
"void"					{ return(VOID); }
"while"					{ return(WHILE); }


{L}{A}*         { return(IDENTIFIER); }


{HP}{H}+{IS}?				{ return I_CONSTANT_RAW; }
{NZ}{D}*{IS}?				{ return I_CONSTANT_RAW; }
"0"{O}*{IS}?				{ return I_CONSTANT_RAW; }
"0"{BP}{B}+                             { return I_CONSTANT_RAW; }
{CP}?"'"([^'\\\n]|{ES})+"'"		{ return I_CONSTANT_RAW; }

{D}+{E}{FS}?				{ return F_CONSTANT_RAW; }
{D}*"."{D}+{E}?{FS}?			{ return F_CONSTANT_RAW; }
{D}+"."{E}?{FS}?			{ return F_CONSTANT_RAW; }
{HP}{H}+{P}{FS}?			{ return F_CONSTANT_RAW; }
{HP}{H}*"."{H}+{P}{FS}?			{ return F_CONSTANT_RAW; }
{HP}{H}+"."{P}{FS}?			{ return F_CONSTANT_RAW; }


"..."					{ return ELLIPSIS; }
">>="					{ return RIGHT_ASSIGN; }
"<<="					{ return LEFT_ASSIGN; }
"+="					{ return ADD_ASSIGN; }
"-="					{ return SUB_ASSIGN; }
"*="					{ return MUL_ASSIGN; }
"/="					{ return DIV_ASSIGN; }
"%="					{ return MOD_ASSIGN; }
"&="					{ return AND_ASSIGN; }
"^="					{ return XOR_ASSIGN; }
"|="					{ return OR_ASSIGN; }
">>"					{ return RIGHT_OP; }
"<<"					{ return LEFT_OP; }
"++"					{ return INC_OP; }
"--"					{ return DEC_OP; }
"->"					{ return PTR_OP; }
"&&"					{ return AND_OP; }
"||"					{ return OR_OP; }
"<="					{ return LE_OP; }
">="					{ return GE_OP; }
"=="					{ return EQ_OP; }
"!="					{ return NE_OP; }
";"					{ return ';'; }
("{"|"<%")	{ return '{'; }
("}"|"%>")	{ return '}'; }
","					{ return ','; }
":"					{ return ':'; }
"="					{ return '='; }
"("					{ return '('; }
")"					{ return ')'; }
("["|"<:")				{ return '['; }
("]"|":>")				{ return ']'; }
"."					{ return '.'; }
"&"					{ return '&'; }
"!"					{ return '!'; }
"~"					{ return '~'; }
"-"					{ return '-'; }
"+"					{ return '+'; }
"*"					{ return '*'; }
"/"					{ return '/'; }
"%"					{ return '%'; }
"<"					{ return '<'; }
">"					{ return '>'; }
"^"					{ return '^'; }
"|"					{ return '|'; }
"?"					{ return '?'; }

\n					{ return NEWLINE; }
{WS}+				{ }
.           { return(BAD_CHARACTER); }
%%
