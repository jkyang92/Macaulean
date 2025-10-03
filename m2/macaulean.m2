stderr << "Macaulean M2 Startup" << endl

needsPackage "JSONRPC"
needsPackage "MRDI"

stderr << "Packages Loaded" << endl

wait stdio;
inputString = read stdio;
stdio << toExternalString value inputString << endl;

stderr << "Macaualy2 Finished" << endl
