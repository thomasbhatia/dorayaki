% -record(diameter_header,
%         {version,            %%  8-bit unsigned
%          length,             %% 24-bit unsigned
%          is_request,         %% boolean() R flag
%          is_proxiable,       %% boolean() P flag
%          is_error,           %% boolean() E flag
%          is_retransmitted,
%          cmd_code,           %% 24-bit unsigned
%          application_id,     %% 32-bit unsigned
%          hop_by_hop_id,      %% 32-bit unsigned
%          end_to_end_id,      %% 32-bit unsigned
%          raw_data,
%          avps
%          }). %% boolean() T flag


% -record(diameter_avp,
%         {code,      %% 32-bit unsigned
%          vendor_id, %% 32-bit unsigned OPTIONAL
%          v = false, %% boolean() V flag
%          m = false, %% boolean() M flag
%          p = false, %% boolean() P flag
%          length,
%          data,      %% encoded binary()
%          value,     %% decoded term() decoded | undefined
%          type,       %% atom() type name,
%          padding    %% Padding = length - 8
%          }).  





% -define(AVP_PADDING, [3, 263, 264, 283, 293, 296]).
% -define(AVP_Type_Unsigned32, [268]).bit
% -define(AVP_SUPPORT, [268, 456]).