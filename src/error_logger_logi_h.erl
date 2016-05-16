%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc error_logger handler
%% @private
%%
%% TODO: refactoring
-module(error_logger_logi_h).

-behaviour(gen_event).

%%------------------------------------------------------------------------------------------------------------------------
%% 'gen_event' Callback API
%%------------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%%------------------------------------------------------------------------------------------------------------------------
%% Internal API
%%------------------------------------------------------------------------------------------------------------------------
-export([do_log/2]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros & Records
%%------------------------------------------------------------------------------------------------------------------------
-define(MAX_DEPTH, 16).
-define(MAX_WRITERS, 100).
-define(WRITE_TIMEOUT, 1000).
-define(WARNING_LOG_FREQUENCY, {frequency, {interval, 60 * 1000}}).

-record(state,
        {
          logger       :: logi:logger(),
          writers  = 0 :: non_neg_integer(),
          omit_report_types = [] :: list()
        }).

%%------------------------------------------------------------------------------------------------------------------------
%% 'gen_event' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @hidden
init({Logger, OmitReportTypes}) ->
    State =
        #state{
           logger = Logger,
           omit_report_types = OmitReportTypes
          },
    {ok, State}.

%% @hidden
handle_event(Event, State) ->
    case State#state.writers =< ?MAX_WRITERS of
        false ->
            {_, Len} = erlang:process_info(self(), message_queue_len),
            _ = logi:warning_opt("overload: queue_len=~w, workers=~w", [Len, State#state.writers], [?WARNING_LOG_FREQUENCY]),
            {ok, State};
        true ->
            ok = start_writer(Event, State),
            {ok, State#state{writers = State#state.writers + 1}}
    end.

%% @hidden
handle_call(Request, State) ->
    _ = logi:warning_opt(State#state.logger, "unknown call: request=~W", [Request, ?MAX_WRITERS], [?WARNING_LOG_FREQUENCY]),
    {ok, undefined, State}.

%% @hidden
handle_info({?MODULE, timeout, Pid}, State) ->
    _ = case erlang:is_process_alive(Pid) of
            false -> ok;
            true  ->
                _ = exit(Pid, kill),
                logi:warning_opt(State#state.logger, "log write timeout: writer=~p", [Pid], [?WARNING_LOG_FREQUENCY])
        end,
    {ok, State#state{writers = State#state.writers - 1}};
handle_info(Info, State) ->
    _ = logi:warning_opt(State#state.logger, "unknown info: info=~W", [Info, ?MAX_WRITERS], [?WARNING_LOG_FREQUENCY]),
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
-spec start_writer(term(), #state{}) -> ok.
start_writer(Event, State = #state{logger = Logger}) ->
    case is_omitted(Event, State) of
        true  -> ok;
        false ->
            Pid = spawn(?MODULE, do_log, [Event, Logger]),
            _ = erlang:send_after(?WRITE_TIMEOUT, self(), {?MODULE, timeout, Pid}),
            ok
    end.

-spec do_log(term(), logi:logger()) -> any().
do_log({error, Gleader, {Pid, Format, Data}}, Logger) ->
    logi:error_opt(Logger, Format, Data, [{headers, [{gleader, Gleader}, {sender, Pid}]}]);
do_log({warning_msg, Gleader, {Pid, Format, Data}}, Logger) ->
    logi:warning_opt(Logger, Format, Data, [{headers, [{gleader, Gleader}, {sender, Pid}]}]);
do_log({info_msg, Gleader, {Pid, Format, Data}}, Logger) ->
    logi:info_opt(Logger, Format, Data, [{headers, [{gleader, Gleader}, {sender, Pid}]}]);
do_log({error_report, Gleader, {Pid, Type, Report}}, Logger) ->
    logi:error_opt(Logger, "~W", [Report, ?MAX_DEPTH], [{headers, [{gleader, Gleader}, {sender, Pid}, {type, Type}]}]);
do_log({warning_report, Gleader, {Pid, Type, Report}}, Logger) ->
    logi:warning_opt(Logger, "~W", [Report, ?MAX_DEPTH], [{headers, [{gleader, Gleader}, {sender, Pid}, {type, Type}]}]);
do_log({info_report, Gleader, {Pid, Type, Report}}, Logger) ->
    logi:info_opt(Logger, "~W", [Report, ?MAX_DEPTH], [{headers, [{gleader, Gleader}, {sender, Pid}, {type, Type}]}]);
do_log(Event, Logger) ->
    logi:notice(Logger, "unknown call: event=~W", [Event, ?MAX_DEPTH]).

-spec is_omitted(term(), #state{}) -> boolean().
is_omitted({info_report, _, {_, Type, _}}, #state{omit_report_types = Types}) ->
    lists:member(Type, Types);
is_omitted({warning_report, _, {_, Type, _}}, #state{omit_report_types = Types}) ->
    lists:member(Type, Types);
is_omitted({error_report, _, {_, Type, _}}, #state{omit_report_types = Types}) ->
    lists:member(Type, Types);
is_omitted(_, _) ->
    false.
