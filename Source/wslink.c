#include "wstp.h"
#include "WolframLibrary.h"
#include "WolframIOLibraryFunctions.h"
#include "WolframNumericArrayLibrary.h"

DLLEXPORT mint WolframLibrary_getVersion() {
    return WolframLibraryVersion;
}

DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData) {
    return LIBRARY_NO_ERROR;
}

DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData \
libData) {
    return;
}

DLLEXPORT int f1(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    int pkt; 

    WSLINK link = libData->getWSLINK(libData);
    /*WSPutFunction(link, "EvaluatePacket", 1);
    WSPutFunction(link, "Message", 2);
    WSPutFunction(link, "MessageName", 2);
    WSPutSymbol(link, "MyFunction");
    WSPutString(link, "info");
    WSPutString(link, "Message called from within Library function.");*/

    unsigned char arr[4] = {0, 1, 2, 3}; 
    long dims[1] = {4};     

    WSPutFunction(link, "EvaluatePacket", 1);
    WSPutFunction(link, "Print", 1);

    //WSPutNext(link, WSTK_64BIT_UNSIGNED_LITTLEENDIAN_INTEGER);
    //WSPutSize(link, 4); 
    //WSPutData(link, "1234", 4); 

    WSPutBinaryNumberArray(link, arr, dims, (char **)0, 1, WSTK_8BIT_UNSIGNED_INTEGER);

    //WSPutString(link, "Hello client program.");
    
    WSEndPacket(link);
    //WSFlush(link);

    libData->processWSLINK(link);
    pkt = WSNextPacket(link);
    if ( pkt == RETURNPKT) {
        WSNewPacket(link);
    }

    return LIBRARY_NO_ERROR; 
}
