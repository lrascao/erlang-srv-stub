
-record(request, {
    socket,
    message_id, 
    is_reply, 
    flags, 
    module_name, 
    proc_name, 
    proc_id,
    data
    }).
