#include "WolframLibrary.h"
#include "WolframIOLibraryFunctions.h"
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

typedef struct {
    WolframLibraryData libData;
    mint id;
    mint timeoutMSec;
    mint runCount; 
    int shouldContinue; 
} ThreadArgs;

static void runBackgroundTask(mint taskId, void* args) 
{
    ThreadArgs* threadArgs = (ThreadArgs*)args;
    WolframLibraryData libData = threadArgs->libData;
    mint id = threadArgs->id;
    mint timeoutMSec = threadArgs->timeoutMSec;
    mint runCount = threadArgs->runCount;

    DataStore ds;
    mint n = 0;

    while (libData->ioLibraryFunctions->asynchronousTaskAliveQ(taskId)) {
        ds = libData->ioLibraryFunctions->createDataStore();
        libData->ioLibraryFunctions->DataStore_addInteger(ds, n++);
        libData->ioLibraryFunctions->DataStore_addInteger(ds, runCount);
        libData->ioLibraryFunctions->DataStore_addInteger(ds, id);
        libData->ioLibraryFunctions->raiseAsyncEvent(taskId, "BackgroundTask", ds);
        #ifdef _WIN32
        Sleep(timeoutMSec); 
        #else
        struct timespec req = {0, timeoutMSec * 1000000L}; 
        nanosleep(&req, NULL);
        #endif
    }
 
    free(threadArgs); 

    return LIBRARY_NO_ERROR;
}

DLLEXPORT int startBackgroundTask(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    if (Argc != 3) {
        return LIBRARY_FUNCTION_ERROR;
    }

    mint id = MArgument_getInteger(Args[0]);
    int timeoutMSec = MArgument_getInteger(Args[1]);
    int runCount = MArgument_getInteger(Args[2]);
    
    if (id <= 0 || timeoutMSec <= 0 || runCount <= 0) {
        return LIBRARY_FUNCTION_ERROR;
    }

    ThreadArgs* threadArgs = (ThreadArgs*)malloc(sizeof(ThreadArgs));
    if (threadArgs == NULL) {
        return LIBRARY_FUNCTION_ERROR;
    }

    threadArgs->libData = libData;
    threadArgs->id = id;
    threadArgs->timeoutMSec = timeoutMSec;
    threadArgs->runCount = runCount;

    int taskId = libData->ioLibraryFunctions->createAsynchronousTaskWithThread(runBackgroundTask, threadArgs);

    MArgument_setInteger(Res, taskId); 
    return LIBRARY_NO_ERROR;
}

DLLEXPORT int stopBackgroundTask(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    if (Argc != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }

    mint taskId = MArgument_getInteger(Args[0]);
    
    if (libData->ioLibraryFunctions->asynchronousTaskAliveQ(taskId)) {
        int result = libData->ioLibraryFunctions->removeAsynchronousTask(taskId);
    }

    MArgument_setInteger(Res, taskId); 
    
    return LIBRARY_NO_ERROR;
}