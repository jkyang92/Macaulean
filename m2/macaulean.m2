stderr << "Macaulean M2 Startup" << endl

needsPackage "JSONRPC"
needsPackage "MRDI"
needsPackage "JSON"
needsPackage "Parsing"
--get the actual parser from the JSON package so we can run it character by character
importFrom_JSON {"jsonTextP"}

stderr << "Packages Loaded" << endl


--read in a single JSON object from the stream
--currently this uses getc which is inefficient, but means we don't have to deal
--with buffering ourselves
--will block if there's nothing to read
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

wait stdio;
inputJSON = fromJSONStream stdio;
stdio << toExternalString sum inputJSON << endl;

stderr << "Macaualy2 Finished" << endl
