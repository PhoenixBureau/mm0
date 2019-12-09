% :- use_module(library(dcg/basics)).
% high_order


mm0_file([LEX|Rest]) --> lexeme(LEX), mm0_file(Rest).
mm0_file(     Rest ) --> whitespace,  mm0_file(Rest).
mm0_file([])         --> [], !.


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


% Character recognizers

digit(Ch)          --> [Ch], { nonvar(Ch), between(0'0, 0'9, Ch) }.
digit_non_zero(Ch) --> [Ch], { nonvar(Ch), between(0'1, 0'9, Ch) }.

alpha(Ch) --> [Ch], { nonvar(Ch), (between(0'a, 0'z, Ch) | between(0'A, 0'Z, Ch))}.

underscore("_") --> "_".


% Try it out...

mm0_filename("../examples/hello.mm0").

do(MM0, R) :-
    mm0_filename(FN),
    read_file_to_codes(FN, Codes, []),
    phrase(mm0_file(MM0), Codes, Rest),
    atom_codes(R, Rest).
