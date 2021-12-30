


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
FS (f|F|l|L)
IS ((u|U)(l|L|ll|LL)?)|((l|L|ll|LL)(u|U)?)) 
CP (u|U|L)
SP (u8|u|U|L)
ES (\\(['"\?\\abfnrtv]|[0-7]{1,3}|x[a-fA-F0-9]+))
WS [ \t\v\n\f]



%{

extern int num_chars;
extern int num_lines;
// extern YYLTYPE yylloc;

#define YY_USER_ACTION         \
  num_chars += yyleng; 


%}


%%
\n      ++num_lines;
.      
%%
