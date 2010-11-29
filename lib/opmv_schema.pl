:- module(opmv_schema, []).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_library)).

/** <module> Provide OPMV schema, namespace and visualization hooks.

This module provides the SKOS schema and   the  prefix =opmv= for use in
Prolog.
*/

:- rdf_register_ns(opmv,  'http://purl.org/net/opmv/ns#').
:- rdf_attach_library(opmv(rdf)).
:- rdf_load_library(opmv).
