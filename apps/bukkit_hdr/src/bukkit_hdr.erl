-module(bukkit_hdr).
-on_load(init/0).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([
    new/0,
    update/2,
    read/1
]).

-opaque hdr() :: term().
-export_type([hdr/0]).

-define(
    NOTLOADED,
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]})
).


-spec new() -> Hdr when
    Hdr :: hdr().

new() ->
    new(erlang:system_info(schedulers), 1, 1000000, 3).

-spec new(Count, Lowest, Highest, SigFig) -> Hdr when
    Count :: pos_integer(),
    Lowest :: non_neg_integer(),
    Highest :: pos_integer(),
    SigFig :: pos_integer(),
    Hdr :: hdr().

new(_, _, _, _) ->
    ?NOTLOADED.


-spec update(Hdr, Value) -> ok when
    Hdr :: hdr(),
    Value :: non_neg_integer().

update(Hdr, Value) ->
    Index = erlang:system_info(scheduler_id),
    update(Hdr, Index, Value).

-spec update(Hdr, Index, Value) -> ok when
    Hdr :: hdr(),
    Index :: pos_integer(),
    Value :: non_neg_integer().

update(_, _, _) ->
    ?NOTLOADED.


-spec read(Hdr) -> Read | Error when
    Hdr :: hdr(),
    Read :: {Min, Max, P50, P75, P90, P99, P999, Mean, StdDev},
    Min :: non_neg_integer(),
    Max :: non_neg_integer(),
    P50 :: non_neg_integer(),
    P75 :: non_neg_integer(),
    P90 :: non_neg_integer(),
    P99 :: non_neg_integer(),
    P999 :: non_neg_integer(),
    Mean :: float(),
    StdDev :: float(),
    Error :: undefined | error.

read(Hdr) ->
    ?NOTLOADED.


init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EBinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EBinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, "bukkit_hdr"), 0).
