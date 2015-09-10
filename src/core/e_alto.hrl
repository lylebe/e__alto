% License: Apache License, Version 2.0
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
%% @author Lyle Bertz <lyleb551144@gmail.com>
%% @copyright Copyright 2015 Lyle Bertz
%%
%% This is a generic header file for common macros and defaults.
%%

-compile([{parse_transform, lager_transform}]).

-define(APPLICATIONNAME, e_alto).
-define(ALTOSCHEMAKEY, schema).
-define(JESSE_ETS, jesse_ets).

-define(E_SYNTAX, e_syntax).
-define(E_MISSING_FIELD, e_missing_field).
-define(E_INVALID_FIELD_TYPE, e_invalid_field_type).
-define(E_INVALID_FIELD_VALUE, e_invalid_field_value).
-define(ALTO_ERR, alto_error).

-record(costmetric, { name, mode, metric, description=undefined }). 
-record(resourceentry, { name, type, uri, mediatype, accepts=undefined, capabilities=undefined, uses=[] }).
