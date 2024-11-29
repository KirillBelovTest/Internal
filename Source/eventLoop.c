#include "WolframLibrary.h"
#include "WolframIOLibraryFunctions.h"
#include "WolframNumericArrayLibrary.h"
#include "wstp.h"

#if defined(_WIN32) || defined(_WIN64)
#include <windows.h>
    typedef HANDLE thread_t;
#else
#include <pthread.h>
#include <unistd.h>
    typedef pthread_t thread_t;
#endif

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

void event(WolframLibraryData libData, int id)
{
    WSLINK link = libData->getWSLINK(libData);
    int pkt;

    WSNewPacket(link); 
    
    WSPutFunction(link, "EvaluatePacket", 1);
    WSPutFunction(link, "Global`event", 1);
    WSPutInteger(link, id);
    WSEndPacket(link);

    libData->processWSLINK(link);

    pkt = WSNextPacket(link);

    wsint64 result;

    printf("EVENT CALL: event(%d)\n", id);

    while (True)
    {
        if (pkt == RETURNPKT){
            printf("return pkt = %d\n", pkt);
            while (!WSReady(link))
            {
                Sleep(1); 
            }
            
            break;
        } else if (pkt == EVALUATEPKT){
            printf("eval pkt = %d\n", pkt);
            pkt = WSNextPacket(link); 
        } else if (pkt == TEXTPKT){
            printf("text pkt = %d\n", pkt);
            pkt = WSNextPacket(link);
        } else if (pkt == ILLEGALPKT){
            printf("illegal pkt = %d\n", pkt);
            break;
        } else {
            printf("unexpected pkt = %d\n", pkt);
            Sleep(10000); 
            break;
        }
    }

    if (WSReady(link))
    {
        WSGetInteger64(link, &result);
    }

    printf("EVENT RETURN: event(%d) = %d\n", id, result);
}

#if defined(_WIN32) || defined(_WIN64)
DWORD WINAPI runBackgroundTask2(LPVOID args)
#else
void* runBackgroundTask2(void* args)
#endif
{
    ThreadArgs* threadArgs = (ThreadArgs*)args;
    WolframLibraryData libData = threadArgs->libData;
    mint id = threadArgs->id;
    mint timeoutMSec = threadArgs->timeoutMSec;
    mint runCount = threadArgs->runCount;
    WSLINK link = threadArgs->libData->getWSLINK(threadArgs->libData);

    int n = 0; 

    int pkt; 

    for (int i = 0; i < runCount; i++) {
        
        event(libData, id); 

        printf("run number %d\n", n);

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

DLLEXPORT int startBackgroundTask2(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
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

    thread_t thread;

    #if defined(_WIN32) || defined(_WIN64)
    thread = CreateThread(NULL, 0, runBackgroundTask2, threadArgs, 0, NULL);
    if (thread == NULL) {
        WSClose(threadArgs->libData->getWSLINK(threadArgs->libData));
        free(threadArgs);
        return LIBRARY_FUNCTION_ERROR;
    }
    #else
    if (pthread_create(&thread, NULL, runBackgroundTask, threadArgs) != 0) {
        WSClose(threadArgs->libData->getWSLINK(threadArgs->libData));
        free(threadArgs);
        return LIBRARY_FUNCTION_ERROR;
    }
    #endif

    MArgument_setInteger(Res, thread); 
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
