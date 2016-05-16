%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc API module
-module(error_logger_logi).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         install/0, install/1,
         install_opt/1,
         uninstall/0, uninstall/1
        ]).

-export_type([options/0, option/0]).

%%------------------------------------------------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------------------------------------------------
-type options() :: [option()].
-type option() :: {logger, logi:logger()}
                | {omit_report_types, list()}.
%% logger:
%% - error_logger経由で出力されたメッセージ群の出力に用いるロガー
%% - デフォルト: `logi:default_logger()'
%%
%% omit_report_types:
%% - 指定のリスト内に含まれるタイプのレポートがログ出力から除外される
%% - 例えば進捗レポートを除外したい場合には `[progress]` を指定する
%% - デフォルト: `[]'

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @equiv install(logi:default_logger())
-spec install() -> ok.
install() ->
    install(logi:default_logger()).

%% @equiv install_opt([{logger, Logger}])
-spec install(logi:logger()) -> ok.
install(Logger) ->
    install_opt([{logger, Logger}]).

-spec install_opt(options()) -> ok.
install_opt(Options) ->
    Logger = proplists:get_value(logger, Options, logi:default_logger()),
    OmitReportTypes = proplists:get_value(omit_report_types, Options, []),
    _ = is_list(OmitReportTypes) orelse error(badarg, [Options]),
    case error_logger:add_report_handler(error_logger_logi_h, {Logger, OmitReportTypes}) of
        ok    -> ok;
        Other -> exit({install_failed, Other}, [Logger])
    end.

%% @equiv uninstall(logi:default_logger())
-spec uninstall() -> ok.
uninstall() ->
    uninstall(logi:default_logger()).

-spec uninstall(logi:logger()) -> ok.
uninstall(Logger) ->
    case error_logger:delete_report_handler(error_logger_logi_h) of
        ok                        -> ok;
        {error, module_not_found} -> ok;
        Other                     -> exit({uninstall_failed, Other}, [Logger])
    end.
