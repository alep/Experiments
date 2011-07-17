%% Parsing with parsing combinators
%% From chapter 1 in Implementing Func. Lang. by Peyton-Jones
%% http://research.microsoft.com/en-us/um/people/simonpj/papers/pj-lester-book/
-module(parser).
-import(lists, [all/2, sublist/2, takewhile/2, dropwhile/2, 
		nthtail/2, reverse/1]).
-export([clex/1, pThen/3, pAlt/2, mkPair/2,
	 pZeroOrMore/1, pOneOrMore/1, pEmpty/1, pVar/0, 
	 pHelloOrGoodbye/0, pGreetings/1, pGreetings2/1, 
	 pGreetingsN/1, pNum/1, pParens/1, pExpr6/0, parse/1]).

%% ------------------------- Lexer ------------------------------- 

isWhitespace(Symbol) ->
    case Symbol of
	$ -> true;
	_ -> false
    end.

isDigit(Symbol) ->
    ($0 =< Symbol) and (Symbol =< $9).

isAlpha(Symbol) ->
    ($A =< Symbol) and (Symbol =< $z).


clex([], Tokens) -> Tokens;
clex([H|T], Tokens) ->
    if
	($0 =< H) and (H =< $9) ->
	    NumToken = [H|takewhile(fun isDigit/1, T)],
	    Rest = dropwhile(fun isDigit/1, T),
	    TokensPrime = [NumToken|Tokens],
	    clex(Rest, TokensPrime);
        ($ == H) ->
	    Rest = dropwhile(fun isWhitespace/1, T),
	    clex(Rest, Tokens);
	(40 =< H) and (H =< 47) -> %% [40, .. ,47] == "()*+,-./"
	    SymbolToken = [H],					    
	    Rest = T,
	    TokensPrime = [SymbolToken|Tokens],
	    clex(Rest, TokensPrime);
	($A =< H) and (H =< $z) ->
	    AlphaToks = [H|takewhile(fun isAlpha/1, T)],
	    Rest = dropwhile(fun isAlpha/1, T),
	    TokensPrime = [AlphaToks|Tokens],
	    clex(Rest, TokensPrime);
	true ->
	    TokensPrime = [[H]|Tokens],
	    clex(T, TokensPrime)
    end.


clex(Str) ->
    reverse(clex(Str, [])).
 
%% ----------------------- Parsers -----------------------------
%% We try to build a big parser by glueing together smaller parsers.
%% A parser returns the remaining list of tokens. 
%% The grammar may be ambiguous, so there is more than one way to parse the input;
%% or the input may not conform to the grammar, in which case there is no way to
%% successfully parse the input. An elegant way to accommodate these possibilities is
%% to return a list of possible parses.
%% So what's a parser? A parser is a function which takes as input a list of tokens
%% and returns a list of pairs of where the _second_ component is the remaining tokens
%% and the _first_ components it the parsed token of some type A.



pSat(Predicate, [Tok|Toks]) ->
    case Predicate(Tok) of 
	true ->
	    [{Tok, Toks}];
	false -> []
    end;
pSat(Predicate, []) ->
    [].
    

pLit(String) ->
    fun(Toks) ->
	    pSat(fun(OtherString) ->
			 String == OtherString end,
		 Toks)
    end.

pVar() -> 
    fun(Toks) ->
	    pSat(fun(Tok) ->
			 Head = hd(Tok),
			 ($A =< Head) and (Head =< $Z) end, 
		 Toks)
    end.


%% Alternative combines two parses into one. pAlt would correspond to 
%% | of the BNF grammar. pAlt works by first using the first parser with
%% list of tokens and then the second parser with same list of toknes.
%% This is easily implemented by concatenating the result of the parses.
%% remember why we used a list in first place! 
pAlt(P1, P2) ->
    fun(Toks) ->
	    P1(Toks) ++ P2(Toks)
    end.

%% --
pHelloOrGoodbye() ->
    fun(Toks) ->
	    P = pAlt(pLit("hello"), pLit("goodbye")), 
	    P(Toks)
    end.

%% pThen is another parser combiner which corresponds to sequencing in BNF (<digit> <number>)
%% It works in the following way it takes parser p1, with p1 the list of tokens is parsed
%% p2 takes the left over list of tokens after p1 was applied and parses the rest of the tokens
%% the first component of each of the results is combined with some function
pThen(Func, P1, P2) ->
    fun(Toks) ->
	    [{Func(V1, V2), Toks2} 
	     || {V1, Toks1} <- P1(Toks),
		{V2, Toks2} <- P2(Toks1)]
    end.

pThen3(Func, P1, P2, P3) ->
    fun(Toks) ->
	    [{Func(V1, V2, V3), Toks3}
	     || {V1, Toks1} <- P1(Toks),
		{V2, Toks2} <- P2(Toks1),
		{V3, Toks3} <- P3(Toks2)]
    end.


%% Couple of test functions for pThen -------
keepFirst(X, Y) -> X.
keepTwo(X, Y, Z) -> {X, Y}.
mkPair(X, Y) -> {X, Y}.

pGreetings(Toks) ->
    P = pThen(fun keepFirst/2,
	       pThen(fun mkPair/2, pHelloOrGoodbye(), pVar()),
	       pLit("!")),
    P(Toks).

pGreetings2(Toks) ->
    P = pThen3(fun keepTwo/3, pHelloOrGoodbye(), pVar(), pLit("!")),
    P(Toks).


%% -------


%% Always succeeds removes nothing from the impout and returs whatever value we passed it.
pEmpty(Val) -> fun(Toks) -> [{Val, Toks}] end.

%% Another very common feature of grammars is to require zer or more repetitions of a symbol
%% To reflect this we would like a function, pZeroOrMore, which takes a parser, p and returns
%% a new parser which recognises zero or more occurrences of whatever p recognises.

%% pZeroOrMore must either be an empty parse or one or more occurrences of p

pZeroOrMore(Parser) ->
    fun(Toks) ->
	    P = pAlt(pOneOrMore(Parser), pEmpty([])),
	    %% Just take the first one, get rid of the rest.
	    case P(Toks) of
		[] -> [];
		[H|_] -> [H]
	    end
    end.

combine(X, XS) -> [X|XS].

pOneOrMore(Parser) -> 
    fun(Toks) ->
	    P = pThen(fun combine/2, Parser, pZeroOrMore(Parser)),
	    P(Toks)
    end.


pGreetingsN(Toks) ->
    P = pZeroOrMore(fun pGreetings2/1),
    P(Toks).


%% --- Grammar for expresion parser ---

removeParens(X, Y, Z) ->
    Y. %% where X = '(' and Z = ')'

pParens(Parser) ->
    fun(Toks) ->
	    P = pThen3(fun removeParens/3, pLit("("), Parser, pLit(")")),
	    P(Toks)
    end.
   
pNum([]) -> [];
pNum([Token|TokenList]) ->
    All = all(fun(X) -> ($0 =< X) and (X =< $9) end, Token),
    case All of
	true -> [{{number, Token}, TokenList}];
        false -> []
    end.

    
aexpr() ->
    fun(Toks) ->
	    P = pAlt(fun pNum/1, pParens(pExpr4())),
	    P(Toks)
    end.

assembleOp(E1, no_op) -> E1;
assembleOp(E1, {found_op, Op, E2}) -> 
    V = {var, Op},
    Ap = {ap, V, E1},
    {ap, Ap, E2}.

foundOp(Op, Expr) ->
    {found_op, Op, Expr}.

pExpr4() ->
    fun(Toks) ->
	    P = pThen(fun assembleOp/2, pExpr5(), pExpr4c()),
	    P(Toks)
    end.

pExpr4c() ->
    fun(Toks) ->
	    P1 = pThen(fun foundOp/2, pLit("+"), pExpr4()),
	    P2 = pThen(fun foundOp/2, pLit("-"), pExpr5()),
	    P3 = pEmpty(no_op),
	    P = pAlt(P1, pAlt(P2, P3)),
	    P(Toks)
    end.

pExpr5() ->
    fun(Toks) ->
	    P = pThen(fun assembleOp/2, pExpr6(), pExpr5c()),
	    P(Toks)
    end.

pExpr5c() ->
    fun(Toks) ->
	    P1 = pThen(fun foundOp/2, pLit("*"), pExpr5()),
	    P2 = pThen(fun foundOp/2, pLit("/"), pExpr6()),
	    P3 = pEmpty(no_op),
	    P = pAlt(P1, pAlt(P2, P3)),
	    P(Toks)
    end.

mkApChain([X]) -> X;
mkApChain([X|XS]) -> 
    Res = mkApChain(XS),
    {ap, Res, X}.

pApply(Parser, Fun, Toks) ->
    [{Fun(Val), Toks1} || {Val, Toks1} <- Parser(Toks)].


pExpr6() ->
    fun(Toks) ->
	    pApply(pOneOrMore(aexpr()), fun mkApChain/1, Toks)
    end.
    
parse(Str) ->
    NewStr = "(" ++ Str ++ ")",
    P = pExpr6(),
    P(clex(NewStr)).
