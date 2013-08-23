:- module(prov_schema, []).
:- use_module(library(semweb/rdf_library)).

/** <module> Provide PROV schema, namespace and visualization hooks.

This module provides the PROV schema and the prefix =prov= for use in
Prolog.
*/

% :- rdf_register_ns(prov,  'http://www.w3..org/net/opmv/ns#').
:- rdf_attach_library(prov(rdf)).
:- rdf_load_library(prov).
