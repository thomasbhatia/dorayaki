%% AVP definitions
-define(User_Name,							1).

-define(Class, 								25).
-define(Session_Timeout,					27).

-define(Proxy_State,						33).

-define(Accounting_Session_Id,				44).

-define(Acct_Multi_Session_Id,				50).
-define(Event_Timestamp,					55).

-define(Acct_Interim_Interval,				85).

-define(Host_IP_Address,					257).
-define(Auth_Application_Id,				258).
-define(Acct_Application_Id,				259).

-define(Vendor_Specific_Application_Id,		260).
-define(Redirect_Host_Usage,				261).
-define(Redirect_Max_Cache_Time,			262).
-define(Session_Id,							263).
-define(Origin_Host,						264).
-define(Supported_Vendor_Id,				265).
-define(Vendor_Id,							266).
-define(Firmware_Revision,					267).
-define(Result_Code,						268).
-define(Product_Name,						269).

-define(Session_Binding,					270).
-define(Session_Server_Failover,			271).
-define(Multi_Round_Time_Out, 				272).
-define(Disconnect_Cause,					273).
-define(Auth_Request_Type,					274).
-define(Auth_Grace_Period,					276).
-define(Auth_Session_State,					277).
-define(Origin_State_Id,					278).
-define(Failed_AVP,							279).

-define(Proxy_Host,							280).
-define(Error_Message,						281).
-define(Route_Record,						282).
-define(Destination_Realm,					283).
-define(Proxy_Info,							284).
-define(Re_Auth_Request_Type,				285).
-define(Accounting_Sub_Session_Id,			287).

-define(Authorization_Lifetime,				291).
-define(Redirect_Host,						292).
-define(Destination_Host,					293).
-define(Error_Reporting_Host,				294).
-define(Termination_Cause,					295).
-define(Origin_Realm,						296).
-define(Experimental_Result,				297).
-define(Experimental_Result_Code,			298).
-define(Inband_Security_Id,					299).

-define(E2E_Sequence,						300).

-define(CC_Request_Number,					415).
-define(CC_Request_Type,					416).

-define(Granted_Service_Unit,				431).
-define(Rating_Group,						432).
-define(Service_Identifier,					439).

-define(Subscription_Id,					443).
-define(Validity_Time,						448).

-define(Multiple_Services_Credit_Control,	456).
-define(Accounting_Record_Type,				480).

-define(Accounting_Realtime_Required,		483).
-define(Accounting_Record_Number,			485).

-define(Volume_Quota_Threshold,				869).

% Type to 
-define(Integer32,		12).
-define(Integer64,		16).
-define(Unsigned32,		12).
-define(Unsigned64,		16).
-define(Float32,		12).
-define(Float64,		16).
-define(OctetString,	arb).
-define(UTF8String,	arb).
-define(Time,	arb).
-define(Address,	arb).
-define(Grouped,	gro).
-define(Enumerated,	12).
-define(DiamIdent,	arb).
-define(DiamURI,	arb).


