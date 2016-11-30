-module(file1_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

prop_dummy() ->
    'EQC PROPERTY'.
