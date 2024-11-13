#include <stdio.h>

#include "WolframLanguageRuntimeV1SDK.h"

int main(int argc, char **argv){
    wlr_runtime_conf configuration; 
    char *result; 

    wlr_InitializeRuntimeConfiguration(&configuration);
    configuration.containmentSetting = WLR_UNCONTAINED; 

    WLR_SDK_START_RUNTIME(
        0, 
        WLR_LICENSE_OR_SIGNED_CODE_MODE, 
        "C:\\Program Files\\Wolfram Research\\Wolfram\\14.1", 
        &configuration
    ); 

    wlr_StringData(wlr_Eval(
        wlr_E(
            wlr_Symbol("WolframAlpha"), 
            wlr_String("Wheather in Saratov"), 
            wlr_String("ShortAnswer")
        )
    ), &result, NULL); 
    
    printf("%s\r\n", result);

    wlr_Release(&result); 
    return 0; 
}
