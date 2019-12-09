

mm0_file([Statement|Rest]) --> statement(Statement), mm0_file(Rest).
mm0_file([]) --> [], !.


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

digits([Ch|Rest]) --> digit(Ch), digits(Rest).
digits([]) --> [], !.

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
    |  assert_stmt(Statement)
    |  def_stmt(Statement)
    |  notation_stmt(Statement)
    |  inout_stmt(Statement).


% sort-stmt ::= ('pure')? ('strict')? ('provable')? ('free')? 'sort' identifier ';'

sort_stmt(sort(Name, Opts)) --> opts(Opts), [ident(sort), ident(Name), symbol(;)].

opts([    pure|Rest]) --> [ident(pure)], opts(Rest).
opts([  strict|Rest]) --> [ident(strict)], opts(Rest).
opts([provable|Rest]) --> [ident(provable)], opts(Rest).
opts([    free|Rest]) --> [ident(free)], opts(Rest).
opts([]) --> [], !.


% Try it out...

mm0_filename("../examples/hello.mm0").

do(MM0, R) :-
    mm0_filename(FN),
    read_file_to_codes(FN, Codes, []),
    phrase(lex(MM0), Codes, Rest),
    atom_codes(R, Rest).
