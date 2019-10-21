-ifndef(__pprof__).
-define(__pprof__, 42).

%% Profile for a process
-record(process,
        {
          pid   = undefined,
          stack = [],
          calls = #{},
          count = #{},
          time  = #{}
        }).

-endif.

%% Function all by a process
-record(call,
        {
          id     = 0,
          t1     = 0,
          t2     = 0,
          time   = 0,
          caller = undefined,
          mfa    = undefined,
          result = undefined
        }).
