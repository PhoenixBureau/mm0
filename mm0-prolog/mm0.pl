

mm0_file(Statements) --> z(statement, Statements).


% Lexxer.

lex([LEX|Rest]) --> lexeme(LEX), lex(Rest).
lex(     Rest ) --> whitespace,  lex(Rest).
lex([])         --> [], !.

% lexeme ::= symbol | identifier | number | math-string

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

% identifier([Ch|Rest]) --> [a-zA-Z_][a-zA-Z0-9_]*
lexeme(ident(ID)) --> identifier(Codes), { atom_codes(ID, Codes) }.

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

underscore("_") --> "_".



% Parser (or wahtever)

statement(Statement) --> sort_stmt(Statement)
    |  term_stmt(Statement)
    |  notation_stmt(Statement).
    % |  assert_stmt(Statement)
    % |  def_stmt(Statement)
    % |  inout_stmt(Statement).


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

term_stmt(term(Name, TypeBinder, ArrowType)) -->
    [ident(term), ident(Name)],
    type_binder(TypeBinder),
    [symbol(:)],
    arrow_type(ArrowType),
    [symbol(;)].


% identifier_(ID) --> identifier(ID).

% type ::= identifier (identifier)*
type([T|Ts]) --> [ident(T)], type(Ts).
type([T]   ) --> [ident(T)], !.

identity(ID) --> [ident(ID)].

% type-binder ::= '{' (identifier )* ':' type '}'
%              |  '(' (identifier_)* ':' type ')'
type_binder(type(Names, Type)) --> [symbol('{')], z(identity, Names), [symbol(:)], type(Type), [symbol('}')].
type_binder(type(Names, Type)) --> [symbol('(')], z(identity, Names), [symbol(:)], type(Type), [symbol(')')].

% arrow-type ::= type | type '>' arrow-type
arrow_type(T)      --> type(T).
arrow_type(T > AT) --> type(T), [symbol(>)], arrow_type(AT).






notation_stmt(Statement) --> delimiter_stmt(Statement).
                %    |  simple_notation_stmt
                %    |  coercion_stmt
                %    |  gen_notation_stmt


delimiter_stmt(delimiter(Delimiter)) --> [ident(delimiter), mstr(Delimiter), symbol(;)].















% Try it out...

mm0_filename("../examples/hello.mm0").

do(MM0, R) :-
    mm0_filename(FN),
    read_file_to_codes(FN, Codes, []),
    phrase(lex(Tokens), Codes),
    phrase(mm0_file(MM0), Tokens, R).


% zero_or_more aka  foo*

z(F, [Term|List])  --> call(F, Term), z(F, List).
z(_, [])  --> [], !.
