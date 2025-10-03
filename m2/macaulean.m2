stderr << "Macaulean M2 Startup" << endl

needsPackage "JSONRPC"
needsPackage "MRDI"

wait stdio;
inputString = read stdio;
stdio << toExternalString value inputString << endl;

stderr << "Packages Loaded" << endl
