:- module(prov_diff, [prov_diff/4]).

:- use_module(library(option)).
:- use_module(library(semweb/rdf11)).

prov_diff(G1, G2, Results, Options) :-
    Results = diff{entities:Entities,
                  activities:Activities},
    rdf_equal(prov:'Entity', ProvEnt),
    rdf_equal(prov:'Activity', ProvAct),
    ActComp = compare(props([rdfs:label, rdfs:comment, prov:used])),
    compare_on_id(G1, G2, Entities,   [type(ProvEnt) | Options]),
    compare_on_props(G1, G2, Activities, [type(ProvAct), ActComp|Options]).

compare_on_id(G1, G2, Results, Options) :-
    Results = diff{g1:G1Report, g2:G2Report, diff:Diff},
    G1Report = g{count:G1Ecount},
    G2Report = g{count:G2Ecount},
    graph_entities(G1, EntsG1, Options),
    graph_entities(G2, EntsG2, Options),
    subject_id_diff(EntsG1, EntsG2, Diff, Options),
    length(EntsG1, G1Ecount),
    length(EntsG2, G2Ecount).

compare_on_props(G1, G2, Results, Options) :-
    Results = diff{g1:G1Report, g2:G2Report, diff:Diff},
    G1Report = g{count:G1Ecount},
    G2Report = g{count:G2Ecount},
    graph_entities(G1, EntsG1, Options),
    graph_entities(G2, EntsG2, Options),
    subject_prop_diff(EntsG1, G1, G2, Diff, Options),
    length(EntsG1, G1Ecount),
    length(EntsG2, G2Ecount).

graph_entities(G, Entities, Options):-
    option(type(Type), Options),
    findall(E, rdf(E, rdf:type, Type, G), Entities0),
    sort(Entities0, Entities).

subject_id_diff(E1, E2, Result, Options) :-
    classify_id(E1, E2, [g1:[], g2:[], both:[]], Result, Options).

subject_prop_diff(E1, G1, G2, Result, Options) :-
    classify_prop(E1, G1, G2, [g:[], both:[]], Result, Options).

classify_prop(  [], _G1, _G2, R, R, _):- !.
classify_prop([H|T], G1, G2, In0, Out, Options) :-
    option(compare(props(Props)), Options),
    (   rdf_equal(H, provx:f4e5e8378570a8c22023697da3da4bbe57cb14cc) -> gtrace; true),
    classify_props(H, G1, G2, Props, In0, In1, Options),
    classify_prop(T, G1, G2, In1, Out, Options).

classify_id([], [], R, R, _):- !.

classify_id([], [H|T], In0, Out, Options) :-
    !,
    In0= [g1:G1, g2:G2, both:Both],
    In1= [g1:G1, g2:[H|G2], both:Both],
    classify_id([], T, In1, Out, Options).

classify_id([H|T], [], In0, Out, Options) :-
    !,
    In0 = [g1:G1, g2:G2, both:Both],
    In1 = [g1:[H|G1], g2:G2, both:Both],
    classify_id(T, [], In1, Out, Options).

classify_id([H|T1], [H|T2], In0, Out, Options) :-
    !,
    In0 = [g1:G1, g2:G2, both:Both],
    In1 = [g1:G1, g2:G2, both:[H|Both]],
    classify_id(T1, T2, In1, Out, Options).

classify_id([H1|T1], [H2|T2], In0, Out, Options) :-
    H1 @< H2,
    !,
    In0 = [g1:G1, g2:G2, both:Both],
    In1 = [g1:[H1|G1], g2:G2, both:Both],
    classify_id(T1, [H2|T2], In1, Out, Options).

classify_id([H1|T1], [H2|T2], In0, Out, Options) :-
    H1 @> H2,
    !,
    In0 = [g1:G1, g2:G2, both:Both],
    In1 = [g1:G1, g2:G2, both:[H1,H2|Both]],
    classify_id([H1|T1], T2, In1, Out, Options).


classify_props(S1, _G1, _G2, [], In, Out, _Options) :-
    !,
    In  = [g:Gonly, both:Both],
    Out = [g:Gonly, both:[S1 | Both]].

classify_props(S1, G1, G2, [P|T], In, Out, Options) :-
    rdf_global_id(P, Pred),
    (   rdf(S1, Pred, O, G1)
    ->  rdf(S2, Pred, O, G2),
        classify_props_list(S1, S2, T, G1, G2, In, Out, Options)
    ;   classify_props(S1, G1, G2, T, In, Out, Options)
    ).

classify_props_list(S1, S2, [], _G1, _G2, In, Out, _Options) :-
    !,
    In  = [g:Gonly, both:Both],
    Out = [g:Gonly, both:[S1, S2 | Both]].

classify_props_list(S1, S2, [P|T], G1, G2, In, Out, Options) :-
    rdf_global_id(P, Pred),
    findall(O, rdf(S1, Pred, O, G1), O1s),
    findall(O, rdf(S2, Pred, O, G2), O2s),
    sort(O1s, O1sorted),
    sort(O2s, O2sorted),
    (   O1sorted == O2sorted
    ->  classify_props_list(S1, S2, T, G1, G2, In, Out, Options)
    ;   In = [g:Gonly, both:Both],
        Out = [g:[S1|Gonly], both:Both]
    ).















