-module(bukkit_hdr).
-on_load(init/0).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([
    new/0,
    new/3,
    update/2,
    read/1
]).

-opaque hdr() :: term().
-export_type([hdr/0]).

-define(
    NOTLOADED,
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]})
).

-spec new() -> {ok, Hdr} | Error when
    Hdr :: hdr(),
    Error :: badarg | undefined.

new() ->
    new(1, 9223372036854775807, 5).

-spec new(Lowest, Highest, SigFig) -> {ok, Hdr} | Error when
    Lowest :: non_neg_integer(),
    Highest :: pos_integer(),
    SigFig :: pos_integer(),
    Hdr :: hdr(),
    Error :: badarg | undefined.

new(_, _, _) ->
    ?NOTLOADED.


-spec update(Hdr, Value) -> ok when
    Hdr :: hdr(),
    Value :: non_neg_integer().

update(Hdr, Value) ->
    ?NOTLOADED.


-spec read(Hdr) -> Read | Error when
    Hdr :: hdr() | [hdr()],
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

read(Hdrs) when is_list(Hdrs) ->
    read_int(Hdrs);
read(Hdr) ->
    read_int([Hdr]).


read_int(_) ->
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
