%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%% Erlang http status code library
%%%
%%% this library provides simple methods to get the description or
%%% reference to an http status code.
%%%
%%% The list is taken from:
%%%   http://www.iana.org/assignments/http-status-codes/http-status-codes.xhtml
%%% @end
%%%-------------------------------------------------------------------
-module(httpStatusCodes).

-export([getDescription/1,
         getReference/1,
	 getHttpStatus/1]).


-type httpStatusCode() :: 100..599.
-type httpStatusDescription() :: bitstring().
-type httpStatusReference() :: bitstring().
-export_type([httpStatusCode/0,
              httpStatusDescription/0,
              httpStatusReference/0]).


%% @doc returns a httpStatusDescription() to an httpStatusCode()
%% @end
-spec getDescription(httpStatusCode()) -> httpStatusDescription().
getDescription(Code) ->
	{Code, Description, _Reference} = getHttpStatus(Code),
	Description.


%% @doc returns a httpStatusReference() to an httpStatusCode()
%% @end
-spec getReference(httpStatusCode()) -> httpStatusReference().
getReference(Code) ->
	{Code, _Description, Reference} = getHttpStatus(Code),
	Reference.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc
%% return a tuple, which contains the httpStatusCode(),
%% the httpStatusDescription() and the httpStatusReference()
%% to an httpStatusCode()
%% @end
-spec getHttpStatus(httpStatusCode()) ->
	{httpStatusCode(), httpStatusDescription(), httpStatusReference()}.
getHttpStatus(100) -> {100,<<"Continue">>,<<"RFC2616">>};
getHttpStatus(101) -> {101,<<"Switching Protocols">>,<<"RFC2616">>};
getHttpStatus(102) -> {102,<<"Processing">>,<<"RFC2518">>};
getHttpStatus(200) -> {200,<<"OK">>,<<"RFC2616">>};
getHttpStatus(201) -> {201,<<"Created">>,<<"RFC2616">>};
getHttpStatus(202) -> {202,<<"Accepted">>,<<"RFC2616">>};
getHttpStatus(203) -> {203,<<"Non-Authoritative Information">>,<<"RFC2616">>};
getHttpStatus(204) -> {204,<<"No Content">>,<<"RFC2616">>};
getHttpStatus(205) -> {205,<<"Reset Content">>,<<"RFC2616">>};
getHttpStatus(206) -> {206,<<"Partial Content">>,<<"RFC2616">>};
getHttpStatus(207) -> {207,<<"Multi-Status">>,<<"RFC4918">>};
getHttpStatus(208) -> {208,<<"Already Reported">>,<<"RFC5842">>};
getHttpStatus(226) -> {226,<<"IM Used">>,<<"RFC3229">>};
getHttpStatus(300) -> {300,<<"Multiple Choices">>,<<"RFC2616">>};
getHttpStatus(301) -> {301,<<"Moved Permanently">>,<<"RFC2616">>};
getHttpStatus(302) -> {302,<<"Found">>,<<"RFC2616">>};
getHttpStatus(303) -> {303,<<"See Other">>,<<"RFC2616">>};
getHttpStatus(304) -> {304,<<"Not Modified">>,<<"RFC2616">>};
getHttpStatus(305) -> {305,<<"Use Proxy">>,<<"RFC2616">>};
getHttpStatus(306) -> {306,<<"Reserved">>,<<"RFC2616">>};
getHttpStatus(307) -> {307,<<"Temporary Redirect">>,<<"RFC2616">>};
getHttpStatus(308) -> {308,<<"Permanent Redirect">>,<<"RFC-reschke-http-status-308-07">>};
getHttpStatus(400) -> {400,<<"Bad Request">>,<<"RFC2616">>};
getHttpStatus(401) -> {401,<<"Unauthorized">>,<<"RFC2616">>};
getHttpStatus(402) -> {402,<<"Payment Required">>,<<"RFC2616">>};
getHttpStatus(403) -> {403,<<"Forbidden">>,<<"RFC2616">>};
getHttpStatus(404) -> {404,<<"Not Found">>,<<"RFC2616">>};
getHttpStatus(405) -> {405,<<"Method Not Allowed">>,<<"RFC2616">>};
getHttpStatus(406) -> {406,<<"Not Acceptable">>,<<"RFC2616">>};
getHttpStatus(407) -> {407,<<"Proxy Authentication Required">>,<<"RFC2616">>};
getHttpStatus(408) -> {408,<<"Request Timeout">>,<<"RFC2616">>};
getHttpStatus(409) -> {409,<<"Conflict">>,<<"RFC2616">>};
getHttpStatus(410) -> {410,<<"Gone">>,<<"RFC2616">>};
getHttpStatus(411) -> {411,<<"Length Required">>,<<"RFC2616">>};
getHttpStatus(412) -> {412,<<"Precondition Failed">>,<<"RFC2616">>};
getHttpStatus(413) -> {413,<<"Request Entity Too Large">>,<<"RFC2616">>};
getHttpStatus(414) -> {414,<<"Request-URI Too Long">>,<<"RFC2616">>};
getHttpStatus(415) -> {415,<<"Unsupported Media Type">>,<<"RFC2616">>};
getHttpStatus(416) -> {416,<<"Requested Range Not Satisfiable">>,<<"RFC2616">>};
getHttpStatus(417) -> {417,<<"Expectation Failed">>,<<"RFC2616">>};
getHttpStatus(422) -> {422,<<"Unprocessable Entity">>,<<"RFC4918">>};
getHttpStatus(423) -> {423,<<"Locked">>,<<"RFC4918">>};
getHttpStatus(424) -> {424,<<"Failed Dependency">>,<<"RFC4918">>};
getHttpStatus(426) -> {426,<<"Upgrade Required">>,<<"RFC2817">>};
getHttpStatus(428) -> {428,<<"Precondition Required">>,<<"RFC6585">>};
getHttpStatus(429) -> {429,<<"Too Many Requests">>,<<"RFC6585">>};
getHttpStatus(431) -> {431,<<"Request Header Fields Too Large">>,<<"RFC6585">>};
getHttpStatus(500) -> {500,<<"Internal Server Error">>,<<"RFC2616">>};
getHttpStatus(501) -> {501,<<"Not Implemented">>,<<"RFC2616">>};
getHttpStatus(502) -> {502,<<"Bad Gateway">>,<<"RFC2616">>};
getHttpStatus(503) -> {503,<<"Service Unavailable">>,<<"RFC2616">>};
getHttpStatus(504) -> {504,<<"Gateway Timeout">>,<<"RFC2616">>};
getHttpStatus(505) -> {505,<<"HTTP Version Not Supported">>,<<"RFC2616">>};
getHttpStatus(506) -> {506,<<"Variant Also Negotiates (Experimental)">>,<<"RFC2295">>};
getHttpStatus(507) -> {507,<<"Insufficient Storage">>,<<"RFC4918">>};
getHttpStatus(508) -> {508,<<"Loop Detected">>,<<"RFC5842">>};
getHttpStatus(510) -> {510,<<"Not Extended">>,<<"RFC2774">>};
getHttpStatus(511) -> {511,<<"Network Authentication Required">>,<<"RFC6585">>};
getHttpStatus(Code) when is_integer(Code), Code<600, Code>=100 -> {Code,<<"Unassigned">>,<<"">>};
getHttpStatus(_Code) -> throw(undefined).
