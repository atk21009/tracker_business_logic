-module(auth_key).
-export([check_auth_key/1]).

check_auth_key(CompareString) ->
    AuthKey = env_variables:auth_key(),
    CompareString == AuthKey.