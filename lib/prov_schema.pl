:- module(prov_schema, [
              prov_init/1,
              default_provenance_graph/1,
              log_start_activity/3,
              log_end_activity/3,
              log_entity_use/2,
              log_entity_create/2,
              prov_uri/3,
              xsd_now/1
          ]).
:- use_module(library(semweb/rdf_library)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_persistency)).

:- use_module(library(git)).
:- use_module(library(option)).
:- use_module(library(socket)).
:- use_module(library(uri)).
:- use_module(library(version)).

/** <module> Provide PROV schema, namespace and visualization hooks.

This module provides the PROV schema and the prefix =prov= for use in
Prolog.
*/

:- rdf_register_ns(provx,  'http://cliopatria.swi-prolog.org/ns/prov#').
:- rdf_register_ns(doap,   'http://usefulinc.com/ns/doap#').

:- rdf_attach_library(prov(rdf)).
:- rdf_load_library(prov).

:- dynamic
    current_prov_uri/2.

:- rdf_meta
    default_provenance_graph(r),
    current_prov_uri(r,r).

default_provenance_graph(provbundle).

%!  prov_init(+Options) is det.
%
%   Clear current_prov_uri cached assertions
%   and initialize prov bundle
prov_init(Options) :-
    retractall(current_prov_uri(_,_)),
    default_provenance_graph(DefaultBundle),
    option(prov(ProvBundle), Options, DefaultBundle),
    option(persistency(Persistency), Options, false),
    rdf_unload_graph(ProvBundle),
    rdf_persistency(ProvBundle, Persistency),
    rdf_assert('', rdf:type, prov:'Bundle', ProvBundle),
    prov_uri(ProvBundle, program(Program), Options),
    prov_uri(ProvBundle, person(Person), Options),
    rdf_assert('', prov:wasAttributedTo, Person, ProvBundle),
    rdf_assert('', prov:wasAttributedTo, Program, ProvBundle).


%!  prov_uri(+Graph, -URI, +Options) is det.
%
%   URI is unified with the URI associated with the current
%   SoftwareAgent (program) or Person in the prov bundle Graph.
prov_uri(Graph, UriTerm, _Options) :-
    current_prov_uri(Graph, UriTerm),!.

prov_uri(Graph, program(Program), Options) :-
    prov_program(Graph, Program, Options), !.

prov_uri(Graph, person(Person), Options) :-
    prov_person(Graph, Person, Options), !.


prov_program(Graph, Program, Options)  :-
    (   current_prolog_flag(version_git, PL_version)
    ->  true
    ;   current_prolog_flag(version, PL_version)
    ),
    current_prolog_flag(executable, PrologExec),
    uri_file_name(PrologExecURL, PrologExec),
    working_directory(CWD,CWD),
    uri_file_name(CWDF,CWD),
    gethostname(LocalHost),
    variant_sha1(CWDF-LocalHost, LocHash),
    rdf_global_id(provx:LocHash, Location),
    format(atom(LocationLabel), '~w:~w', [LocalHost, CWD]),
    findall(M-U-V-F,
            (   git_module_property(M, home_url(U)),
                git_module_property(M, version(V)),
                git_module_property(M, directory(D)),
                uri_file_name(F, D)
            ),
            MUVs
           ),
    Prolog = 'swi-prolog'-'http://www.swi-prolog.org'-PL_version-PrologExecURL,
    AllModules = [Prolog|MUVs],
    sort(AllModules, SortedModules),
    variant_sha1(SortedModules, Hash),
    rdf_global_id(provx:Hash, Program),
    assert(current_prov_uri(Graph, program(Program))),
    option(program_label(Label:Lang), Options, 'Local ClioPatria instance':en),
    rdf_assert(Program, rdfs:label, Label@Lang, Graph),
    rdf_assert(Program, rdf:type,   prov:'SoftwareAgent', Graph),
    rdf_assert(Program, prov:atLocation, Location, Graph),
    rdf_assert(Location, rdf:type, prov:'Location', Graph),
    rdf_assert(Location, provx:host, LocalHost^^xsd:string, Graph),
    rdf_assert(Location, provx:cwd, CWDF, Graph),
    rdf_assert(Location, rdfs:label, LocationLabel^^xsd:string, Graph),
    forall(member(M-U-V-D, SortedModules),
           (   variant_sha1(M-U-V-D, CompHash),
               rdf_global_id(provx:CompHash, Comp),
               rdf_assert(Program, provx:component, Comp, Graph),
               rdf_assert(Comp, rdf:type, doap:'Project', Graph),
               rdf_assert(D, rdf:type, doap:'GitBranch', Graph),
               rdf_assert(Comp, doap:revision, V^^xsd:string, Graph),
               rdf_assert(Comp, doap:name, M@en, Graph),
               rdf_assert(Comp, doap:repository, D, Graph),
               rdf_assert(Comp, doap:homepage, U, Graph),
               prov_module_settings(Comp, M, Options)
           )
          ),
    !.

prov_person(Graph, Person, Options) :-
    default_user_name(DefaultUserName),
    option(user(UserName), Options, DefaultUserName),
    variant_sha1(UserName, Hash),
    rdf_global_id(provx:Hash, DefaultPerson),
    option(person(Person), Options, DefaultPerson),
    rdf_assert(Person, foaf:name, UserName^^xsd:string, Graph),
    rdf_assert(Person, rdf:type, prov:'Person', Graph),
    assert(current_prov_uri(Graph, person(Person))).

default_user_name(UserName) :-
    git(['config','--get','user.name'], [output(Codes)]),
    atom_codes(Atom, Codes),
    normalize_space(atom(UserName), Atom),
    !.


xsd_now(TimeStamp) :-
    get_time(Time),
    xsd_timestamp(Time, TimeStamp).

xsd_timestamp(Time, TimeStamp) :-
    stamp_date_time(Time, Date, 'UTC'),
    format_time(atom(TimeStamp), '%FT%T%:z', Date, posix).

log_start_activity(Activity, ProvBundle, Options0) :-
    option(label(Label), Options0),
    (   ground(ProvBundle)
    ->  true
    ;   default_provenance_graph(DefaultBundle),
        option(prov(ProvBundle), Options0, DefaultBundle)
    ),
    Options = [prov(ProvBundle) | Options0],
    prov_uri(ProvBundle, program(Program), Options),
    prov_uri(ProvBundle, person(Person), Options),
    xsd_now(TimeStamp),
    variant_sha1(TimeStamp:Program:Person:Label, Hash),
    rdf_global_id(provx:Hash, Activity),
    rdf_assert(Activity, rdf:type, prov:'Activity', ProvBundle),
    rdf_assert(Activity, rdfs:label, Label@en, ProvBundle),
    rdf_assert(Activity, prov:startedAtTime, TimeStamp^^xsd:dateTime, ProvBundle),
    rdf_assert(Activity, prov:wasAssociatedWith, Person, ProvBundle),
    rdf_assert(Activity, prov:wasAssociatedWith, Program, ProvBundle).

log_end_activity(Activity, ProvBundle, _Options) :-
    xsd_now(TimeStamp),
    rdf_assert(Activity, prov:endedAtTime, TimeStamp^^xsd:dateTime, ProvBundle).

log_entity_use(Spec, Options) :-
    default_provenance_graph(DefaultBundle),
    option(prov(ProvBundle), Options, DefaultBundle),
    option(activity(Activity), Options),
    spec_entity_file(Spec, Entity, File),
    rdf_assert(Entity, rdf:type, prov:'Entity', ProvBundle),
    rdf_assert(Activity, prov:used, Entity, ProvBundle),
    (   access_file(File, read)
    ->  size_file(File, Size),
        time_file(File, Time),
        xsd_timestamp(Time, Stamp),
        rdf_assert(Entity, provx:file_size, Size^^xsd:integer, ProvBundle),
        rdf_assert(Entity, prov:generatedAtTime, Stamp^^xsd:dateTime, ProvBundle)
    ;   true
    ).

spec_entity_file(Spec, Entity, File) :-
    uri_is_global(Spec),
    Spec = Entity,
    uri_file_name(Entity, File),
    !.

spec_entity_file(Spec, Entity, false) :-
    uri_is_global(Spec),
    Spec = Entity,
    !.

spec_entity_file(Spec, Entity, File) :-
    uri_file_name(Entity, Spec),
    Spec = File,
    !.

log_entity_create(File, Options) :-
    option(activity(Activity), Options),
    option(prov(ProvBundle), Options),
    option(graph(Graph), Options, none),
    (   uri_is_global(File)
    ->  Entity = File,
        xsd_now(TimeStamp)
    ;   uri_file_name(Entity, File),
        (   access_file(File, read),
            time_file(File, Time)
        ->  xsd_timestamp(Time, TimeStamp)
	;   xsd_now(TimeStamp)
        )
    ),
    xsd_now(TimeStamp),
    rdf_assert(Entity, rdf:type, prov:'Entity', ProvBundle),
    rdf_assert(Entity, prov:generatedAtTime, TimeStamp^^xsd:dateTime,  ProvBundle),
    rdf_assert(Entity, prov:wasGeneratedBy, Activity, ProvBundle),
    log_derivation(Entity, Options),
    log_entity_graph_properties(Entity, Graph, ProvBundle).

log_derivation(Entity, Options) :-
    option(was_derived_from(Sources), Options),!,
    option(prov(ProvBundle), Options),
    forall(member(Source, Sources),
           (   uri_file_name(SourceUri, Source),
               rdf_assert(Entity, prov:wasDerivedFrom, SourceUri, ProvBundle)
           )
          ).

log_derivation(_,_). % skip

prov_module_settings(Comp, Module, Options) :-
    option(prov(ProvBundle), Options),
    forall(setting(Module:Key, Value),
           assert_key_value_pair(Comp, Key, Value, ProvBundle)
          ).
log_entity_graph_properties(Entity, Graph, ProvBundle) :-
    forall(rdf_graph_property(Graph, Property),
           (   Property =.. [ Local, LValue ],
               assert_key_value_pair(Entity, Local, LValue, ProvBundle)
           )).

assert_key_value_pair(Entity, Key0, Value0, Graph) :-
    rdf_equal(xsd:string, XsdString),
    (   Key0 = triples
    ->  rdf_global_id(void:triples, Pred)
    ;   Key0 = source_last_modified
    ->  rdf_global_id(dcterms:modified, Pred)
    ;   Key0 = source
    ->  rdf_global_id(prov:wasDerivedFrom, Pred)
    ;   rdf_global_id(provx:Key0, Pred)
    ),
    (   Key0 = source_last_modified
    ->  xsd_timestamp(Value0, Value)
    ;   Value0 == []
    ->  rdf_equal(rdf:nil, Value)
    ;   compound(Value0)
    ->  format(atom(Atom), '~p', [Value0]),
        Value = Atom^^XsdString
    ;   number(Value0)
    ->  Value = Value0
    ;   Value = Value0^^XsdString
    ),
    rdf_assert(Entity, Pred, Value, Graph).
