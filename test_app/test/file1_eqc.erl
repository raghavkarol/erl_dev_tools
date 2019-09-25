-module(file1_eqc).

-compile([export_all, nowarn_export_all]).

prop_dummy() ->
    'EQC PROPERTY'.
