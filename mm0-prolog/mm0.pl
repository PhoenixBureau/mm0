

mm0_file(Statements) --> z(statement, Statements).


% Lexxer.

lex([LEX|Rest]) --> lexeme(LEX), lex(Rest).
lex(     Rest ) --> whitespace,  lex(Rest).
lex([])         --> [], !.

% lexeme ::= symbol | identifier | number | math-string

% identifier([Ch|Rest]) --> [a-zA-Z_][a-zA-Z0-9_]*
lexeme(ident(ID)) --> identifier(Codes), { atom_codes(ID, Codes) }.

% try identifier//3 first, to parse e.g.:
% def __: string = $ s1 (ch x2 x0) $; -- space
% where the ident is '__' whoch otherwise gets lexed as
% ident(def), symbol('_'), symbol('_'), symbol(:), ...

lexeme(symbol('*')) --> "*".
lexeme(symbol('.')) --> ".".
lexeme(symbol(':')) --> ":".
lexeme(symbol(';')) --> ";".
lexeme(symbol('(')) --> "(".
lexeme(symbol(')')) --> ")".
lexeme(symbol('>')) --> ">".
lexeme(symbol('{')) --> "{".
lexeme(symbol('}')) --> "}".
lexeme(symbol('=')) --> "=".
lexeme(symbol('_')) --> "_".

% number ::= 0 | [1-9][0-9]*
lexeme(number(0)) --> "0".
lexeme(number(N)) --> number(Codes), { number_codes(N, Codes) }.

% math-string ::= '$' [^\$]* '$'
lexeme(mstr(MathStr)) --> "$", mstr(Codes), "$", { atom_codes(MathStr, Codes) }.


% Character patterns.

identifier([Ch|Rest]) --> (alpha(Ch) | underscore(Ch)), ident_chars(Rest).

ident_chars([Ch|Rest]) --> (alpha(Ch) | underscore(Ch) | digit(Ch)), ident_chars(Rest).
ident_chars([]) --> [], !.

number([Ch|Rest]) --> digit_non_zero(Ch), digits(Rest).

digits(Digits) --> z(digit, Digits).

mstr([Ch|Rest]) --> [Ch], { nonvar(Ch), Ch =\= "$" }, mstr(Rest).
mstr([]) --> [], !.


% whitespace ::= whitestuff+
% whitestuff ::= whitechar | line-comment
% whitechar ::= ' ' | '\n'
% line-comment ::= '--' [^\n]* '\n'

whitespace --> whitestuff, whitespace.
whitespace --> whitestuff, !.

whitestuff --> whitechar | line_comment.

whitechar --> " " | "\n".

line_comment --> "--", non_newlines, "\n".

non_newlines --> [Ch], { nonvar(Ch), Ch =\= "\n" }, non_newlines.
non_newlines --> [], !.
% Compare with mstr//3.


% Character recognizers

digit(Ch)          --> [Ch], { nonvar(Ch), between(0'0, 0'9, Ch) }.
digit_non_zero(Ch) --> [Ch], { nonvar(Ch), between(0'1, 0'9, Ch) }.

alpha(Ch) --> [Ch], { nonvar(Ch), (between(0'a, 0'z, Ch) | between(0'A, 0'Z, Ch))}.

underscore(0'_) --> "_".



% Parser (or wahtever)

statement(Statement) --> sort_stmt(Statement)
    |  term_stmt(Statement)
    |  notation_stmt(Statement)
    |  def_stmt(Statement)
    |  inout_stmt(Statement).
    % |  assert_stmt(Statement)


% sort-stmt ::= ('pure')? ('strict')? ('provable')? ('free')? 'sort' identifier ';'

sort_stmt(sort(Name, Opts)) --> opts(Opts), [ident(sort), ident(Name), symbol(;)].

opts([    pure|Rest]) --> [ident(pure)], opts(Rest).
opts([  strict|Rest]) --> [ident(strict)], opts(Rest).
opts([provable|Rest]) --> [ident(provable)], opts(Rest).
opts([    free|Rest]) --> [ident(free)], opts(Rest).
opts([]) --> [], !.


% term-stmt ::= 'term' identifier (type-binder)* ':' arrow-type ';'
% identifier_ ::= identifier | '_'
% type ::= identifier (identifier)*
% type-binder ::= '{' (identifier)* ':' type '}'
%              |  '(' (identifier_)* ':' type ')'
% arrow-type ::= type | type '>' arrow-type

term_stmt(term(Name, TypeBinders, ArrowType)) -->
    [ident(term), ident(Name)],
    z(type_binder,TypeBinders),
    [symbol(:)],
    arrow_type(ArrowType),
    [symbol(;)].


ident(ID) --> [ident(ID)].

ident_(ID) --> ident(ID) | [symbol(_)], { ID='_' }.

% type ::= identifier (identifier)*
type([T|Ts]) --> [ident(T)], type(Ts).
type([T]   ) --> [ident(T)], !.

% type-binder ::= '{' (identifier )* ':' type '}'
%              |  '(' (identifier_)* ':' type ')'
type_binder(type(Names, Type)) --> [symbol('{')], z(ident,  Names), [symbol(:)], type(Type), [symbol('}')].
type_binder(type(Names, Type)) --> [symbol('(')], z(ident_, Names), [symbol(:)], type(Type), [symbol(')')].

% arrow-type ::= type | type '>' arrow-type
arrow_type(T)      --> type(T).
arrow_type(T > AT) --> type(T), [symbol(>)], arrow_type(AT).



    % notation-stmt ::= delimiter-stmt
    % |  simple-notation-stmt
    % |  coercion-stmt
    % |  gen-notation-stmt
    % delimiter-stmt ::= 'delimiter' math-string ';'
    % simple-notation-stmt ::= ('infixl' | 'infixr' | 'prefix') identifier ':'
    % constant 'prec' precedence-lvl ';'
    % constant ::= math-string
    % precedence-lvl ::= number | 'max'
    % coercion-stmt ::= 'coercion' identifier ':' identifier '>' identifier ';'
    % gen-notation-stmt ::= 'notation' identifier (type-binder)* ':'
    % type '=' prec-constant (notation-literal)* ';'
    % notation-literal ::= prec-constant | identifier
    % prec-constant ::= '(' constant ':' precedence-lvl ')'





notation_stmt(Statement) --> delimiter_stmt(Statement)
    |  simple_notation_stmt(Statement).
                %    |  coercion_stmt
                %    |  gen_notation_stmt


delimiter_stmt(delimiter(Delimiter)) --> [ident(delimiter), mstr(Delimiter), symbol(;)].


% simple-notation-stmt ::= ('infixl' | 'infixr' | 'prefix') identifier ':' constant 'prec' precedence-lvl ';'

    % constant ::= math-string
    % precedence-lvl ::= number | 'max'

simple_notation_stmt(infixl(Name, N, P)) --> [ident(infixl), ident(Name), symbol(:)], constant(N), [ident(prec)], precedence_lvl(P), [symbol(;)].
simple_notation_stmt(infixr(Name, N, P)) --> [ident(infixr), ident(Name), symbol(:)], constant(N), [ident(prec)], precedence_lvl(P), [symbol(;)].
simple_notation_stmt(prefix(Name, N, P)) --> [ident(prefix), ident(Name), symbol(:)], constant(N), [ident(prec)], precedence_lvl(P), [symbol(;)].

constant(N) --> [mstr(N)].

precedence_lvl(P) --> [number(P)], !.
precedence_lvl(max) --> [ident(max)].






% def-stmt ::= 'def' identifier (dummy-binder)* ':' type ('=' formula)? ';'

def_stmt(def(Name, Bs, T, F)) -->
    [ident(def), ident(Name)],
    z(dummy_binder, Bs),
    [symbol(:)],
    type(T),
    ([symbol(=)], formula(F) | [], { F=nil }),
    [symbol(;)].

% dummy-binder ::= '{' (dummy-identifier)* ':' type '}'
%               |  '(' (dummy-identifier)* ':' type ')'

% dummy-identifier ::= '.' identifier | identifier_

dummy_binder(type(Names, Type)) --> [symbol('{')], z(dummy_identifier, Names), [symbol(:)], type(Type), [symbol('}')].
dummy_binder(type(Names, Type)) --> [symbol('(')], z(dummy_identifier, Names), [symbol(:)], type(Type), [symbol(')')].

dummy_identifier(ID) --> [symbol(.), ident(ID)] | ident_(ID).

formula(F) --> [mstr(F)].




% inout-stmt ::= input-stmt | output-stmt
% input-stmt ::= 'input' input-kind ':' (identifier | math-string)* ';'
% output-stmt ::= 'output' output-kind ':' (identifier | math-string)* ';'
% input-kind ::= identifier
% output-kind ::= identifier


inout_stmt(Statement) --> input_stmt(Statement) | output_stmt(Statement).

input_stmt(  input(Kind, Content)) --> [ ident(input), ident(Kind), symbol(:)], z(io_content, Content), [symbol(;)].
output_stmt(output(Kind, Content)) --> [ident(output), ident(Kind), symbol(:)], z(io_content, Content), [symbol(;)].

io_content(ident(Bar)) --> [ident(Bar)], !.
io_content( mstr(Bar)) --> [mstr(Bar)].



% Try it out...

mm0_filename("../examples/hello.mm0").

do :-
    mm0_filename(FN),
    read_file_to_codes(FN, Codes, []),
    phrase(lex(Tokens), Codes),
    phrase(mm0_file(MM0), Tokens),
    maplist(portray_clause, MM0),
    !.


% zero_or_more aka  foo*

z(F, [Term|List])  --> call(F, Term), z(F, List).
z(_, [])  --> [], !.
