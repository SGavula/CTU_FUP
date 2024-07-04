<program> -> (<definition> <sep>)* <expr> <space>*

<definition> -> <var> <sep> ":=" <sep> <expr>

<expr> -> <var>
        | "(\\" <var> '.' <expr> ')'
        | '(' <expr> <sep> <expr> ')'

<var> -> <alphanum>+
<alphanum> -> any alphanumeric character recognized by isAlphaNum

<sep> -> <space>+
<space> -> any whitespace recognized by isSpace, e.g. ' ', '\n'

"I := (\\x.x)\n (I I)"
((\\x.x)(\\x.x))  





<expr> -> <space>* <expr'> <space>*
<expr'> -> <var>
         | <val>
         | <add>
         | <mul>

<var> -> <lower> <alphanum>*
<val> -> <int> "." <digit>+ | <int>
<int> -> "-" <digit>+ | <digit>+ 

<add> -> "(" <expr> ("+" <expr>)+ ")"
<mul> -> "(" <expr> ("*" <expr>)+ ")"