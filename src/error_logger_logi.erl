%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc API module
-module(error_logger_logi).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         install/0, install/1,
         uninstall/0, uninstall/1
        ]).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @equiv install(logi:default_logger())
-spec install() -> ok.
install() ->
    install(logi:default_logger()).

-spec install(logi:logger()) -> ok.
install(Logger) ->
    case error_logger:add_report_handler(error_logger_logi_h, [Logger]) of
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
