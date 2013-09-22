%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(httpStatusCodes_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
	 stop/1]).

-export([getDescription/1,
	getReference/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
	httpStatusCodes_sup:start_link().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
	ok.

getDescription(Code) ->
	{Code, _Description, _Reference} = getFromList(Code),
	_Description.

getReference(Code) ->
	{Code, _Description, _Reference} = getFromList(Code),
	_Reference.

%%%===================================================================
%%% Internal functions
%%%===================================================================

getFromList(Code) ->
	HttpCodes = [
		{100,<<"Continue">>,<<"RFC2616">>},
		{101,<<"Switching Protocols">>,<<"RFC2616">>},
		{102,<<"Processing">>,<<"RFC2518">>},
		{200,<<"OK">>,<<"RFC2616">>},
		{201,<<"Created">>,<<"RFC2616">>},
		{202,<<"Accepted">>,<<"RFC2616">>},
		{203,<<"Non-Authoritative Information">>,<<"RFC2616">>},
		{204,<<"No Content">>,<<"RFC2616">>},
		{205,<<"Reset Content">>,<<"RFC2616">>},
		{206,<<"Partial Content">>,<<"RFC2616">>},
		{207,<<"Multi-Status">>,<<"RFC4918">>},
		{208,<<"Already Reported">>,<<"RFC5842">>},
		{226,<<"IM Used">>,<<"RFC3229">>},
		{300,<<"Multiple Choices">>,<<"RFC2616">>},
		{301,<<"Moved Permanently">>,<<"RFC2616">>},
		{302,<<"Found">>,<<"RFC2616">>},
		{303,<<"See Other">>,<<"RFC2616">>},
		{304,<<"Not Modified">>,<<"RFC2616">>},
		{305,<<"Use Proxy">>,<<"RFC2616">>},
		{306,<<"Reserved">>,<<"RFC2616">>},
		{307,<<"Temporary Redirect">>,<<"RFC2616">>},
		{308,<<"Permanent Redirect">>,<<"RFC-reschke-http-status-308-07">>},
		{400,<<"Bad Request">>,<<"RFC2616">>},
		{401,<<"Unauthorized">>,<<"RFC2616">>},
		{402,<<"Payment Required">>,<<"RFC2616">>},
		{403,<<"Forbidden">>,<<"RFC2616">>},
		{404,<<"Not Found">>,<<"RFC2616">>},
		{405,<<"Method Not Allowed">>,<<"RFC2616">>},
		{406,<<"Not Acceptable">>,<<"RFC2616">>},
		{407,<<"Proxy Authentication Required">>,<<"RFC2616">>},
		{408,<<"Request Timeout">>,<<"RFC2616">>},
		{409,<<"Conflict">>,<<"RFC2616">>},
		{410,<<"Gone">>,<<"RFC2616">>},
		{411,<<"Length Required">>,<<"RFC2616">>},
		{412,<<"Precondition Failed">>,<<"RFC2616">>},
		{413,<<"Request Entity Too Large">>,<<"RFC2616">>},
		{414,<<"Request-URI Too Long">>,<<"RFC2616">>},
		{415,<<"Unsupported Media Type">>,<<"RFC2616">>},
		{416,<<"Requested Range Not Satisfiable">>,<<"RFC2616">>},
		{417,<<"Expectation Failed">>,<<"RFC2616">>},
		{422,<<"Unprocessable Entity">>,<<"RFC4918">>},
		{423,<<"Locked">>,<<"RFC4918">>},
		{424,<<"Failed Dependency">>,<<"RFC4918">>},
		{426,<<"Upgrade Required">>,<<"RFC2817">>},
		{428,<<"Precondition Required">>,<<"RFC6585">>},
		{429,<<"Too Many Requests">>,<<"RFC6585">>},
		{431,<<"Request Header Fields Too Large">>,<<"RFC6585">>},
		{500,<<"Internal Server Error">>,<<"RFC2616">>},
		{501,<<"Not Implemented">>,<<"RFC2616">>},
		{502,<<"Bad Gateway">>,<<"RFC2616">>},
		{503,<<"Service Unavailable">>,<<"RFC2616">>},
		{504,<<"Gateway Timeout">>,<<"RFC2616">>},
		{505,<<"HTTP Version Not Supported">>,<<"RFC2616">>},
		{506,<<"Variant Also Negotiates (Experimental)">>,<<"RFC2295">>},
		{507,<<"Insufficient Storage">>,<<"RFC4918">>},
		{508,<<"Loop Detected">>,<<"RFC5842">>},
		{510,<<"Not Extended">>,<<"RFC2774">>},
		{511,<<"Network Authentication Required">>,<<"RFC6585">>}
		],
	case lists:keyfind(Code,1,HttpCodes) of
		false ->
			{Code,<<"Unassigned">>,<<"">>};
		Ret ->
			Ret
	end.
