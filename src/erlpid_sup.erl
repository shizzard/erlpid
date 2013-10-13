-module(erlpid_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).



%% Interface



start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    write_pid(),
    {ok, { {one_for_one, 5, 10}, []} }.



%% Internals



write_pid() ->
    case application:get_env(erlpid, destination) of
        {ok, Dest} ->
            check_rights(Dest),
            write_pid(Dest);
        undefined ->
            throw({misconfiguration, destination_is_undefined})
    end.

write_pid(File) ->
    Pid = os:getpid(),
    {ok, Ptr} = file:open(File, [write]),
    ok = file:write(Ptr, Pid),
    ok = file:close(Ptr).



check_rights(_File) ->
    %% TODO: make rights check here
    ok.