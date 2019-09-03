-module(pm).

%%% @doc

-behaviour(gen_server).

-include("pm.hrl").

%% API
-export([start_link/0, new_session/2, end_session/1,
	 switch_assignment_between_uas/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export_types([id_tag/0, id/0, u/0, ua/0, o/0, oa/0, pc/0, pe/0,
	       assign/0, association/0, prohibition/0, obligation/0, op/0, ar/0]).

-type id_tag() :: u | ua | o | oa | pc | arset | atset | pattern | response.
-type id() :: {id_tag(), term()}.
-type u() :: #u{}.
-type ua() :: #ua{}.
-type o() :: #o{}.
-type oa() :: #oa{}.
-type pc() :: #pc{}.
-type pe() :: u() | ua() | o() | oa() | pc().
-type assign() :: #assign{}.
-type association() :: #association{}.
-type prohibition() :: #prohibition{}.
-type obligation() :: #obligation{}.
-type op() :: #op{}.
-type ar() :: #ar{}.

-define(SERVER, ?MODULE).
-define(LDAP_HOST, "rpi2.fritz.box").
-define(LDAP_PORT, 636).
-define(LDAP_BASE, "ou=pm,dc=de-bleek,dc=demon,dc=nl").

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

-spec new_session(Username, Password) -> {ok, Pid :: pid()} | {error, Reason :: term()} when
      Username :: string(),
      Password :: string().
%% @doc
new_session(Username, Password) ->
    DN = "uid=" ++ Username ++ "," ++ ?LDAP_BASE,
    {ok, Handle} = eldap:open([?LDAP_HOST], [{port, ?LDAP_PORT}, {ssl, true}]),
    ok = eldap:simple_bind(Handle, DN, Password),
    eldap:close(Handle),
    case pm_pep_sup:add(Username) of
	{ok, Pid} ->
	    {ok, Pid};
	{error, {already_started, Pid}} ->
	    {ok, Pid};
	Error ->
	    Error
    end.

-spec end_session(Pid :: pid()) -> ok | {error, Reason :: term()}.
%% @doc
end_session(Pid) ->
    pm_pep_sup:delete(Pid).
    
%% Defining Personas
%% E.1 Via Model Extension
-spec switch_assignment_between_uas(P :: pid(), U :: u(), Ua_current :: ua(), Ua_new :: ua()) -> ok.
%% @doc
switch_assignment_between_uas(_P, _U, _Ua_current, _Ua_new) ->
    ok.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
    {ok, #state{}}.

%% @private
handle_call(Request, _From, State) ->
    Reply = {error, {not_implemented, Request}},
    {reply, Reply, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% cmds() ->
%%     add_host_app,
%%     add_oattr,
%%     add_open_obj,
%%     add_prop,
%%     add_record_keys,
%%     add_script,
%%     add_template,
%%     add_uattr,
%%     assign,
%%     assign_obj_to_oattr,
%%     assign_obj_to_oattr_with_prop,
%%     build_clipboard,
%%     change_password,
%%     assign_obj_to_container,
%%     assign_obj_to_inbox_of,
%%     assign_obj_to_outbox_of,
%%     assign_obj_to_winbox_of,
%%     bye,
%%     close_object,
%%     compile_script_and_add_to_enabled,
%%     compile_script_and_enable,
%%     compute_fast_vos,
%%     compute_vos,
%%     connect,
%%     copy_object,
%%     copy_to_clipboard,
%%     create_linked_objects,
%%     create_object_3,
%%     create_process,
%%     create_record,
%%     create_record_in_entity_with_prop,
%%     create_session,
%%     deassign_obj_from_home_of,
%%     deassign_obj_from_inbox_of,
%%     deassign_obj_from_oattr_with_prop,
%%     deassign_obj_from_outbox_of,
%%     delete_assignment,
%%     delete_container_objects,
%%     delete_node,
%%     delete_open_obj,
%%     delete_opsets_between,
%%     delete_session,
%%     do_create_process,
%%     exit_process,
%%     find_a_name,
%%     get_all_ops,
%%     get_app_path,
%%     get_connector,
%%     get_container_with_prop,
%%     get_containers_of,
%%     get_mell_containers_of,
%%     get_dasc_objects,
%%     get_devices,
%%     get_email_acct,
%%     get_email_recipients,
%%     get_entity_id,
%%     get_entity_name,
%%     get_entity_with_prop,
%%     get_file_content,
%%     get_host_app_paths,
%%     get_host_repository,
%%     get_inbox_messages,
%%     get_installed_app_names,
%%     get_k_store_paths,
%%     get_last_error,
%%     get_members_of,
%%     get_mell_members_of,
%%     get_name_of_entity_with_id_and_type,
%%     get_id_of_entity_with_name_and_type,
%%     get_oattrs,
%%     get_obj_attrs_proper,
%%     get_obj_email_props,
%%     get_obj_info,
%%     get_object_in_inbox_of,
%%     get_obj_in_outbox_of,
%%     get_obj_properties,
%%     get_obj_name_path,
%%     get_opsets_between,
%%     get_opset_oattrs,
%%     get_opset_ops,
%%     get_outbox_messages,
%%     get_permitted_ops,
%%     get_policy_classes,
%%     get_proper_containers,
%%     get_record_info,
%%     get_records,
%%     get_sel_vos_graph,
%%     get_sel_vos_graph_2,
%%     get_session_id,
%%     get_session_user,
%%     get_simple_vos_graph,
%%     get_templates,
%%     get_template_info,
%%     get_users,
%%     get_users_and_attrs,
%%     get_users_of,
%%     get_user_attributes,
%%     get_vos_graph,
%%     get_vos_id_properties,
%%     get_pos_node_properties,
%%     get_vos_properties,
%%     is_in_pos,
%%     is_obj_in_oattr_with_prop,
%%     is_pasting_allowed,
%%     is_time_to_refresh,
%%     make_cmd,
%%     may_session_close,
%%     new_open_object,
%%     new_read_file,
%%     new_read_object,
%%     new_write_file,
%%     new_write_object,
%%     open_object,
%%     pass_cmd_to_engine,
%%     process_event,
%%     read_new_file,
%%     open_object_3,
%%     open_object_4,
%%     read_object_3,
%%     request_perms,
%%     send_object,
%%     send_simple_msg,
%%     set_k_store_paths,
%%     set_host_app_paths,
%%     set_perms,
%%     set_record_keys,
%%     set_startups,
%%     spawn_session,
%%     would_open_prevent_save,
%%     write_object_3.

%% From Harmonia-1.6/src/gov/nist/csd/pm/server/packet/SQLPacketHandler.java

%% addDeny(sSessId, sDenyName, sDenyType, sUserOrAttrName, sUserOrAttrId, sOp, sOattrName, sOattrId, sIsInters)
%% addEmailAcct(sSessId, sPmUser, sFullName, sEmailAddr
%% addHost(sSessId, sName, sRepo)
%% addHost(sSessId, sName, sRepo, sReserved, sIpa, sDescr, sPdc)
%% addHostApp(sSessId, sHost, appName, appPath, mainClassName, appPrefix)
%% addOattr(crtSessionId, sProcId, sName, sDescr, sInfo, sBaseId, sBaseType, sBaseIsVos, sAssocObjId, sProps)
%% addObjClassAndOp(sClientId, sClass, sDescr, sInfo, sOp)
%% addObject3(sSessId, sProcId, sName, sDescr, sInfo, sContainers, sClass, sType, sHost, sPath, sOrigName, sOrigId, sInh, sSender, sRecip, sSubject, sAttached, null, null, null)
%% addOpenObj(sSessId, sObjName)
%% addOpsetAndOp(sSessId, sOpset, sDescr, sInfo, sOp, sBaseId, sBaseType)
%% addPc(sSessId, sProcId, sName, sDescr, sInfo, sProps)
%% addProp(sSessId, sId, sType, sIsVos, sProp)
%% addRecordKeys(sSessId, sRecName, sKeys)
%% addSchemaOattr(sSessId, oattrType, name, baseName, baseType)
%% addTemplate(sSessId, sTplName, sContainers, sKeys)
%% addUattr(sClientId, crtSessionId, sProcId, sName, sDescr, sInfo, sBaseId, sBaseType, sBaseIsVos, sProps)
%% addUser(sSessId, sProcId, sName, sFull, sInfo, sHash, sBaseId, sBaseType, sBaseIsVos)
%% assign(sSessId, sProcId, sId1, sType1, sId2, sType2)
%% assignObjToOattr(sSessId, sProcId, sObjName, sOattrName)
%% assignObjToOattrWithProp(sSessId, sProcId, sObjName, sProp)
%% audit(sSessId, sEvent, sObjId, sResult)
%% buildClipboard(sSessId, sOattrName)
%% calcPriv(sSessId, sUserName, sUserId, sUserType, sTgtName, sTgtId, sTgtType)
%% changePassword(sClientId, sUser, sOldPass, sNewPass, sConPass)
%% connect()
%% copyObject(sSessId, sProcId, sObjName)
%% createContForFolder(sSessId, sFolderPath, sBaseContName)
%% createObjForFile(sSessId, sFilePath, sContName)
%% createObject3(sSessId, sProcId, sObjName, sObjClass, sObjType, sContainers, sPerms, sSender, sReceiver, sSubject, sAttached)
%% createRecord(sSessId, sProcId, sRecordName, sBase, sBaseType, sTplId, sComponents, sKeys)
%% createRecordInEntityWithProp(sSessId, sProcId, sRecordName, sProp, sBaseType, sTplId, sComponents, sKeys)
%% createSchemaPC(sSessId, sProcId, policyType oattrType, sPolicyClass, sUattr)
%% createSession(sClientId, sName, sHost, sUser, sHash)
%% deassignObjFromOattrWithProp(sSessId, sProcId, sObjName
%% deleteAssignment(sSessId, sProcId, sId1, sType1, sId2, sType2, sIsAdminVos)
%% deleteContainerObjects(sSessId, sContainerName, sContainerType)
%% deleteDeny(sSessId, sDenyName, sOp, sOattrName, sOattrId)
%% deleteHost(sSessId, sHostId)
%% deleteNode(sSessId, sId, sType, sIsVos)
%% deleteObjClassAndOp(sSessId, sClass, sOp)
%% deleteObject(sSessId, sObjId)
%% deleteObjectStrong(sSessId, sObjId)
%% deleteOpenObj(sSessId, sObjName)
%% deleteOpsetAndOp(sClientId, sOpsetId, sOp)
%% deleteOpsetsBetween(sSessId, sProcId, sUattrName, sOattrName)
%% deleteProperty(sSessId, sPropName)
%% deleteSession(sClientId, sId)
%% deleteTemplate(sSessId, sTplId, sTplName)
%% doDacConfinement(sSessId, sUser, sPc, sAttr, sCont)
%% export(sClientId, sCrtSessId)
%% findBorderOaPrivRelaxed(sSessId, sName, sId, sType)
%% findBorderOaPrivRestricted(sSessId, sName, sId, sType)
%% genConfForDacUser(sSessId, sUser, sPc, sAttr, sCont)
%% genDacUserWithConf(sSessId, sUser, sFullName, sPc
%% getAllOps(sSessId)
%% getAsets(sClientId)
%% getAssocOattr1(sId)
%% getAssocObj(sClientId, sOaId)
%% getAttrInfo(sSessId, sAttrId, sAttrType, sIsVos)
%% getConnector()
%% getContainers(id, type)
%% getContainersOf(sSessId, sBaseName, sBaseId, sBaseType, sGraphType)
%% getDascOattrs(sBaseName, sBaseType)
%% getDascObjects(sBaseName, sBaseType)
%% getDascOpsets(sBaseName, sBaseType)
%% getDascUattrs(sBaseName, sBaseType)
%% getDascUsers(sBaseName, sBaseType)
%% getDenies(sSessId)
%% getDenyInfo(sSessId, sDenyId)
%% getEmailAcct(sSessId, sPmUser)
%% getEmailRecipients(sSessId, sReceiver)
%% getEntityId(sSessId, sEntityName, sEntityType)
%% getEntityId(sSessId, sName, sType)
%% getEntityName(sSessId, sEntityId, sEntityType)
%% getEntityName(sSessId, sId, sType)
%% getEntityWithProp(sSessId, sEntType, sProp)
%% getFileContent(sSessId, sFileProp, bisFromClient, bosToClient)
%% getFromAttrs1(sSessId, sOpsetId)
%% getFromAttrs1(sSessId, sOpsetId)
%% getFromOpsets1(sSessId, sOpsetId)
%% getFromUserAttrsPacket(sBaseId, sBaseType)
%% getHostAppPaths(sClientId, sCrtSessId, sHost, appname)
%% getHostInfo(sSessId, sHostId)
%% getHostRepository(sCrtSessId)
%% getHosts(sClientId)
%% getInboxMessages(sSessId)
%% getInstalledApps(sHost)
%% getKStorePaths(sClientId, sSessId)
%% getMellContainersOf(sSessId, sBaseName, sBaseId, sBaseType, sGraphType)
%% getMellMembersOf(sSessId, sBaseName, sBaseId, sBaseType, sGraphType)
%% getMembersOf(sSessId, sBaseName, sBaseId, sBaseType, sGraphType)
%% getOattrs(sSessId)
%% getOattrs(sSessId)
%% getObjAttrsProper(sSessId)
%% getObjClassOps(sClientId, sClass)
%% getObjClasses(sClientId)
%% getObjEmailProps(sCrtSessId, sObjName)
%% getObjInfo(sObjId)
%% getObjNamePath(sObjName)
%% getObjProperties(sObjId)
%% getObjects(sClientId)
%% getOpsetInfo(sClientId, sOpsetId)
%% getOpsetOattrs(sSessId, sOpsetName)
%% getOpsetOps(sClientId, sOpset)
%% getOpsets(sClientId)
%% getOpsetsBetween(sSessId, sUattrName, sEntityName, sEntityType)
%% getOutboxMessages(sSessId)
%% getPcInfo(sSessId, sPcId, sIsVos)
%% getPermittedOps1(sSessId, sProcId, sBaseId, sBaseType)
%% getPermittedOpsOnEntity(sSessId, sProcId, sId, sType)
%% getPmEntitiesOfClass(sSessId, sClass)
%% getPolicyClasses(sClientId)
%% getPosNodeProperties(sCrtSessId, sPresType, sNodeId, sNodeLabel, sNodeType)
%% getProperties(sSessId)
%% getProperty(sSessId, sPropName)
%% getPropertyValue(sSessId, sPropName)
%% getRecordInfo(sSessId, sOattrId)
%% getRecords(sSessId, sTplId, sKey)
%% getReps(sId, sType)
%% getSessionInfo(sClientId, sSessId)
%% getSessionName(sSessId)
%% getSessionUser(sSessId)
%% getSessions(sClientId)
%% getTemplateInfo(sSessId, sTplId)
%% getTemplates(sSessId)
%% getToAttrs1(sSessId, sOpsetId)
%% getToAttrsUser(sId, sType)
%% getUserAttributes(sClientId)
%% getUserDescendants(sClientId, sUserId)
%% getUserInfo(sSessId, sUserId)
%% getUsers(sClientId)
%% getUsersAndAttrs(sSessId)
%% getUsersOf(sUattrName)
%% importConfiguration(sSessId, cmdPacket)
%% initialOas(sSessId, sName, sId, sType)
%% initialOasWithLabels(sSessId, sName, sId, sType)
%% interpretCmd(sSessId, sCmd)
%% isAssigned(sId1, sType1, sId2, sType2)
%% isObjInOattrWithProp(sSessId, sObjName, sProp)
%% isRecordPacket(sId)
%% isolateOattr(sAttrName, sAttrType)
%% newGetPermittedOps(sCrtSessId, sUserId, sProcId, sObjName)
%% removeProp(sSessId, sId, sType, sIsVos, sProp)
%% replaceProp(sSessId, sId, sType, sIsVos, sOldProp, sNewProp)
%% reset(crtSessionId)
%% sendSimpleMsg(sSessId, sMsgName, sReceiver)
%% setHostAppPaths(sCrtSessId, sHost, sAtoolPath, sRtfedPath, sWkfPath, sEmlPath, sExpPath, sLauncherPath, sMsofficePath, sMedrecPath, sAcctrecPath, soldWkfPath, sSchemaPath)
%% setKStorePaths(sCrtSessId, sUserId, sHost, sKsPath
%% setPerms(sSessId, sProcId, sUattrName, sOpset, sSuggOattr, sSuggBase, sSuggBaseType, sPerms, sEntName, sEntType, sInclAscs)
%% setProperty(sSessId, sPropName, sPropValue)
%% setRecordKeys(sSessId, sRecName, sKeys)
%% setSchemaPerms(sSessId, sProcId, sBaseName, sBaseType, sAttrName, uattr)
%% setTablePerms(sSessId, sProcId, sBaseName, sBaseType, sAttrName, sAttrType, sPerms, uattr, inh.equalsIgnoreCase("yes"))
%% showAccessibleObjects(sSessId, sName, sId, sType)
%% subsequentOas(sSessId, sUserName, sUserId, sUserType, sTgtName, sTgtId, sTgtType)
%% successorOas(sSessId, sUserName, sUserId, sUserType, sTgtName, sTgtId, sTgtType)
%% testExportRecords()
%% testGetDeniedPerms(sSessId, sObjName)
%% testGetMemberObjects(sSessId, sContId, sContType)
%% testIsContained(sId1, sClass1, sId2, sClass2)
%% testSynchro(sClientId)
%% updateAllNodes()
%% updateHost(sSessId, sHostId, sHostName, sRepo)
%% updateHost(sSessId, sHostId, sHostName, sRepo, sIpa, sDescr, sPdc)
