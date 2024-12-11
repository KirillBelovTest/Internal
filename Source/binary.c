#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "WolframLibrary.h"
#include "WolframNumericArrayLibrary.h"

DLLEXPORT mint WolframLibrary_getVersion() {
    return WolframLibraryVersion;
}

DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData) {
    return LIBRARY_NO_ERROR;
}

DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData) {
    return;
}

DLLEXPORT int byteMask(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    MNumericArray nMask = MArgument_getMNumericArray(Args[0]);
    
    uint8_t* mask = (uint8_t *)libData->numericarrayLibraryFunctions->MNumericArray_getData(nMask);
    const mint maskLen = MArgument_getInteger(Args[1]);
    
    MNumericArray nArr = MArgument_getMNumericArray(Args[2]);
    uint8_t* arr = (uint8_t *)libData->numericarrayLibraryFunctions->MNumericArray_getData(nArr); 
    const mint arrLen = MArgument_getInteger(Args[3]);

    MNumericArray nResult;
    libData->numericarrayLibraryFunctions->MNumericArray_new(MNumericArray_Type_UBit8, 1, &arrLen, &nResult);
    uint8_t *result = (uint8_t*)libData->numericarrayLibraryFunctions->MNumericArray_getData(nResult); 

    const int len = arrLen - arrLen % maskLen; 
    int k; 

    for (size_t i = 0; i < len; i = i + maskLen) {
        result[i] = arr[i] ^ mask[i]; 
        for (size_t j = 1; j < maskLen; j++) { 
            k = i + j; 
            result[k] = arr[k] ^ mask[j]; 
        }
    }
    
    for (size_t i = len; i < arrLen; i++) {
        result[i] = arr[i] ^ mask[i % maskLen];
    }

    MArgument_setMNumericArray(Res, nResult);
    return LIBRARY_NO_ERROR;
}

DLLEXPORT int bytesPosition(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    MNumericArray ndata = MArgument_getMNumericArray(Args[0]);
    uint8_t* data = (uint8_t *)libData->numericarrayLibraryFunctions->MNumericArray_getData(ndata);
    const mint dataLen = MArgument_getInteger(Args[1]);
    
    MNumericArray nsep = MArgument_getMNumericArray(Args[2]);
    uint8_t* sep = (uint8_t *)libData->numericarrayLibraryFunctions->MNumericArray_getData(nsep); 
    const mint sepLen = MArgument_getInteger(Args[3]);

    const mint count = MArgument_getInteger(Args[4]);

    MTensor npositions;  
    libData->MTensor_new(MType_Integer, 1, &count, &npositions);
    mint* positions = libData->numericarrayLibraryFunctions->MNumericArray_getData(npositions); 

    mint i = 0; 

    for (size_t j = 0; j < dataLen; j++) {
        if (data[j] == sep[0]) {
            if (sepLen == 1) {
                positions[i] = j + 1; 
                i++;   
            } else {
                positions[i] = j + 1; 
                i++; 
                for (size_t k = 1; k < sepLen; k++){
                    if (data[j + k] != sep[k]){
                        i--; 
                        positions[i] = 0; 
                        break; 
                    }
                }
            }
        }
        if (i > count) break;
    }

    MArgument_setMTensor(Res, npositions); 
    return LIBRARY_NO_ERROR;     
}