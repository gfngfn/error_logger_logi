%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc error_logger handler
%% @private
-module(error_logger_logi_h).

-behaviour(gen_event).

%%------------------------------------------------------------------------------------------------------------------------
%% 'gen_event' Callback API
%%------------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros & Records
%%------------------------------------------------------------------------------------------------------------------------
-define(MAX_DEPTH, 16).
-define(MAX_QUEUE_LEN, 8).

-record(state,
        {
          logger :: logi:logger()
        }).

%%------------------------------------------------------------------------------------------------------------------------
%% 'gen_event' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @hidden
init([Logger]) ->
    State = #state{logger = Logger},
    {ok, State}.

%% @hidden
handle_event(Event, State) ->
    {_, Len} = erlang:process_info(self(), message_queue_len),
    _ = case Len > ?MAX_QUEUE_LEN of
            false -> do_log(Event, State);
            true  -> logi:warning_opt("overload: queue_len=~p", [Len], [{frequency, {interval, 3 * 60 * 1000}}])
        end,
    {ok, State}.

%% @hidden
handle_call(Request, State) ->
    _ = logi:warning_opt(State#state.logger, "unknown call: request=~p", [Request], [{frequency, {interval, 60 * 1000}}]),
    {ok, undefined, State}.

%% @hidden
handle_info(Info, State) ->
    _ = logi:warning_opt(State#state.logger, "unknown info: info=~p", [Info], [{frequency, {interval, 60 * 1000}}]),
    {ok, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec do_log(term(), #state{}) -> any().
do_log({error, Gleader, {Pid, Format, Data}}, State) ->
    logi:error_opt(State#state.logger, Format, Data, [{headers, [{gleader, Gleader}, {sender, Pid}]}]);
do_log({warning, Gleader, {Pid, Format, Data}}, State) ->
    logi:warning_opt(State#state.logger, Format, Data, [{headers, [{gleader, Gleader}, {sender, Pid}]}]);
do_log({info, Gleader, {Pid, Format, Data}}, State) ->
    logi:info_opt(State#state.logger, Format, Data, [{headers, [{gleader, Gleader}, {sender, Pid}]}]);
do_log({error_report, Gleader, {Pid, Type, Report}}, State) ->
    logi:error_opt(State#state.logger, "~P", [Report, ?MAX_DEPTH], [{headers, [{gleader, Gleader}, {sender, Pid}, {type, Type}]}]);
do_log({warning_report, Gleader, {Pid, Type, Report}}, State) ->
    logi:warning_opt(State#state.logger, "~P", [Report, ?MAX_DEPTH], [{headers, [{gleader, Gleader}, {sender, Pid}, {type, Type}]}]);
do_log({info_report, Gleader, {Pid, Type, Report}}, State) ->
    logi:info_opt(State#state.logger, "~P", [Report, ?MAX_DEPTH], [{headers, [{gleader, Gleader}, {sender, Pid}, {type, Type}]}]);
do_log(Event, State) ->
    logi:verbose_opt(State#state.logger, "unknown call: event=~p", [Event], [{frequency, {interval, 3 * 60 * 1000}}]).
