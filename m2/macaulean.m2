stderr << "Macaulean M2 Startup" << endl

needsPackage "JSONRPC"
needsPackage "MRDI"
needsPackage "JSON"
needsPackage "Parsing"
--get the actual parser from the JSON package so we can run it character by character
importFrom_JSON {"jsonTextP"}
--get the helper function from JSONRPC that can take already parsed JSON objects
importFrom_JSONRPC {"handleRequestHelper"}

stderr << "Packages Loaded" << endl



--read in a single JSON object from the stream
--currently this uses getc which is inefficient, but means we don't have to deal
--with buffering ourselves
--will block if there's nothing to read
--We need either this, a message oriented socket api, or something like what LSP does with Content-Length
fromJSONStream = method();
fromJSONStream File := (file) -> (
    currParser := jsonTextP;
    while true do (
        --TODO use utf8
        currParser = currParser (getc file);
        if currParser === null then error "JSON parsing failed";
        --poor man's check of whether the parser is complete, see if passing null returns a value
        parseResult := currParser null;
        if parseResult =!= null then return parseResult;
        )
    )

--main loop, reading one JSON expression at a time
--right now it uses the same file for input and output
macauleanMainLoop = method();
macauleanMainLoop (JSONRPCServer, File) := (server, file) -> (
    while true do (
        wait file;
        if atEndOfFile file then return;
        if isReady file then (
            local response;
            try request := fromJSONStream file
            then response = handleRequestHelper_server request
            else response = makeResponse(server, JSONRPCError(-32700, "Parse error"), null);
            file << toJSON response;
            )
        )
    )

--setup the server, copied from example.m2
server = new JSONRPCServer
server#"logger" = (str) -> (stderr << str << endl)
registerMethod(server, "quotientRemainder", (polymrdi, idealmrdi) -> (
	f := loadMRDI polymrdi;
	I := loadMRDI idealmrdi;
	(q, r) := quotientRemainder(matrix f, gens I);
	(saveMRDI q, saveMRDI r)))

registerMethod(server, "factor", (nmrdi) -> (
	n := loadMRDI nmrdi;
	saveMRDI(toList \ toList factor n)))

registerMethod(server, "testMethod", (expr) -> (
        toExternalString value expr
        ))


macauleanMainLoop(server, stdio);
-- inputJSON = fromJSONStream stdio;
-- stdio << toExternalString sum inputJSON << endl;

stderr << "Macaualy2 Finished" << endl
