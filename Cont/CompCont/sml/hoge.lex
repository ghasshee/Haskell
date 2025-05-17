type lexresult = Tokens.token
fun eof() = Tokens.EOF(0,0)

%%
digits=[0-9]*; 
%%

if          => (Tokens.IF(yypos,yypos+2)); 

