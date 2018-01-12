&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Winn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Winn 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

DEFINE INPUT PARAMETER parentProcedure AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER parentFrame     AS HANDLE NO-UNDO.

&GLOBAL-DEFINE VK_SCROLL   145
&GLOBAL-DEFINE VK_NUMLOCK  144
&GLOBAL-DEFINE VK_CAPITAL  20

DEFINE VARIABLE intResult AS INTEGER NO-UNDO.

PROCEDURE GetKeyState EXTERNAL "USER32.DLL":
    DEFINE INPUT  PARAMETER intVirtKey AS LONG.
    DEFINE RETURN PARAMETER intResult  AS SHORT.
END PROCEDURE.



/* PROCEDURE keybd_event EXTERNAL "USER32.DLL":     */
/*   DEF INPUT  PARAM bVk         AS SHORT NO-UNDO. */
/*   DEF INPUT  PARAM bScan       AS SHORT NO-UNDO. */
/*   DEF INPUT  PARAM dwFlags     AS LONG  NO-UNDO. */
/*   DEF INPUT  PARAM dwExtraInfo AS LONG  NO-UNDO. */
/*   DEF RETURN PARAM intResult   AS LONG  NO-UNDO. */
/* END PROCEDURE.                                   */

PROCEDURE LockWindowUpdate EXTERNAL "user32.dll" :
  DEFINE INPUT  PARAMETER hWndLock AS LONG.
  DEFINE RETURN PARAMETER IsLocked AS LONG.
END PROCEDURE.

   DEFINE VARIABLE but1 AS HANDLE NO-UNDO.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE keyboardLayer NO-UNDO SERIALIZE-NAME "keyboardLayer"
    FIELD Layer        AS INTEGER SERIALIZE-NAME "layer"        XML-NODE-TYPE "ATTRIBUTE"
    FIELD keyboardDesc AS CHARACTER SERIALIZE-NAME "keyboard-desc". 

DEFINE TEMP-TABLE keyboard NO-UNDO SERIALIZE-NAME "keyboard"
    FIELD KeyCaption  AS CHARACTER SERIALIZE-NAME "key-label"  XML-NODE-TYPE "ATTRIBUTE" 
    FIELD keyValue    AS CHARACTER CASE-SENSITIVE SERIALIZE-NAME "key-value"  XML-NODE-TYPE "ATTRIBUTE"
    FIELD shiftKey    AS LOGICAL SERIALIZE-NAME "shift-key"    XML-NODE-TYPE "ATTRIBUTE"
    FIELD ctrlKey     AS LOGICAL SERIALIZE-NAME "ctrl-key"    XML-NODE-TYPE "ATTRIBUTE"
    FIELD altKey      AS LOGICAL SERIALIZE-NAME "alt-key"    XML-NODE-TYPE "ATTRIBUTE"
    FIELD Layer       AS INTEGER SERIALIZE-NAME "layer"        XML-NODE-TYPE "HIDDEN"
    FIELD KeyboardRow AS INTEGER SERIALIZE-NAME "row"          XML-NODE-TYPE "ATTRIBUTE" 
    FIELD KeyboardPos AS INTEGER SERIALIZE-NAME "pos"          XML-NODE-TYPE "ATTRIBUTE"
    FIELD KeyWidth    AS INTEGER SERIALIZE-NAME "width"
    FIELD KeyHeight   AS INTEGER SERIALIZE-NAME "height"
    FIELD indent      AS INTEGER SERIALIZE-NAME "indent"    
    FIELD SENSITIVE   AS LOGICAL SERIALIZE-NAME "sensitive"  INITIAL TRUE
    FIELD imageUp     AS CHARACTER SERIALIZE-NAME "image-up"
    FIELD imageDown   AS CHARACTER SERIALIZE-NAME "image-dwn"
    FIELD WIDGET-HANDLE   AS HANDLE XML-NODE-TYPE "HIDDEN"
    INDEX idxPosition IS PRIMARY
        Layer
        KeyboardRow
        KeyboardPos.
        
/* DEFINE TEMP-TABLE layer1 SERIALIZE-NAME "keyboard" LIKE  keyboard .  */
/* DEFINE TEMP-TABLE layer2 SERIALIZE-NAME "keyboard" LIKE  keyboard  . */
/* DEFINE TEMP-TABLE layer3 SERIALIZE-NAME "keyboard" LIKE  keyboard.   */
/* DEFINE TEMP-TABLE layer4 SERIALIZE-NAME "keyboard" LIKE  keyboard .  */


    DEFINE DATASET keyboardLayout SERIALIZE-NAME "keyboardLayout" FOR keyboardLayer, keyboard DATA-RELATION FOR keyboardLayer, keyboard RELATION-FIELDS(Layer, Layer) NESTED.
    
    
    DATASET keyboardLayout:READ-XML("FILE", "keyboardLayoutLayer1.xml", "EMPTY", ?, ?).

    DATASET keyboardLayout:READ-XML("FILE", "keyboardLayoutLayer2.xml", "APPEND", ?, ?).

    DATASET keyboardLayout:READ-XML("FILE", "keyboardLayoutLayer3.xml", "APPEND", ?, ?).

    DATASET keyboardLayout:READ-XML("FILE", "keyboardLayoutLayer4.xml", "APPEND", ?, ?).

    DATASET keyboardLayout:WRITE-XML("FILE", "keyboardLayoutDebug.xml", TRUE).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-24 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-24 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCapsLockState C-Winn 
FUNCTION getCapsLockState RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Winn AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE FILL-IN-24 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN-24 AT ROW 13.86 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 18 NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 190.8 BY 13.95 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Winn ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 20.14
         WIDTH              = 213.8
         MAX-HEIGHT         = 48.43
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 48.43
         VIRTUAL-WIDTH      = 384
         SHOW-IN-TASKBAR    = no
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Winn = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Winn
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       FILL-IN-24:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Winn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Winn C-Winn
ON END-ERROR OF C-Winn /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Winn C-Winn
ON WINDOW-CLOSE OF C-Winn /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Winn 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   
   /** Possition the virtual keyboard into a place 
       holder inside it's parenent's window. **/
   FRAME {&Frame-name}:X = parentFrame:X.
   FRAME {&Frame-name}:Y = parentFrame:Y.
   
  RUN enable_UI.
  
  

    RUN createLayout (INPUT ?).
    
/*   IF NOT THIS-PROCEDURE:PERSISTENT THEN */
/*     WAIT-FOR CLOSE OF THIS-PROCEDURE.   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearDynamicKeys C-Winn 
PROCEDURE clearDynamicKeys :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
              
    FOR EACH keyboard:
        
        
        IF VALID-HANDLE(keyboard.WIDGET-HANDLE) THEN
            DELETE WIDGET keyboard.WIDGET-HANDLE.    
        
        
    
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createLayout C-Winn 
PROCEDURE createLayout :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE INPUT PARAMETER showLayer AS INTEGER NO-UNDO.
    DEF VAR IsLocked  AS INTEGER NO-UNDO INITIAL 0.
    
    DEFINE VARIABLE xpos AS INTEGER     NO-UNDO.
    DEFINE VARIABLE ypos AS INTEGER     NO-UNDO.    
    
    DEFINE VARIABLE keyHeightAtStartofRow AS INTEGER     NO-UNDO.
    
    
    
    RUN LockWindowUpdate(FRAME {&frame-name}:HWND, 
                         OUTPUT IsLocked).
    
    FRAME {&frame-name}:VISIBLE = NO.
    
    RUN clearDynamicKeys. 
                             
                             
    IF showLayer EQ ? THEN
    DO:
        /** If "caps lock" is 'on' show 
            layer 1 else layer 2 **/
        IF getCapsLockState() = 1 THEN
            showLayer = 1.
        ELSE
            showLayer = 2.
    END.
    
    
    FOR EACH keyboard
        WHERE Keyboard.Layer EQ ABS(showLayer)
        BREAK BY KeyboardRow 
              BY KeyboardPos:
        
        xpos = xpos + keyboard.indent.
        
        CREATE BUTTON but1 
            ASSIGN
              Y             = ypos
              X             = xpos
              LABEL         = keyboard.KeyCaption
              WIDTH-PIXELS  = keyboard.KeyWidth  
              HEIGHT-PIXELS = keyboard.KeyHeight
              FRAME     = FRAME {&FRAME-NAME}:HANDLE
              NO-FOCUS  = TRUE
              TAB-STOP  = FALSE
              SENSITIVE = keyboard.SENSITIVE
              BGCOLOR   = (IF keyboard.KeyValue EQ "" THEN 12 ELSE ?)
              VISIBLE   = TRUE
              TRIGGERS:
                ON CHOOSE PERSISTENT RUN Keyboard-Process-Key IN THIS-PROCEDURE ( INPUT keyboard.KeyValue, INPUT keyboard.shiftKey, INPUT keyboard.ctrlKey, INPUT keyboard.altKey).
              END TRIGGERS.
              
         ASSIGN
            keyboard.WIDGET-HANDLE = but1:HANDLE.
            
        IF FIRST-OF(keyboard.KeyboardRow) THEN
        
                   keyHeightAtStartofRow = keyboard.KeyHeight.
        
              
        IF NOT LAST-OF(keyboard.KeyboardRow) THEN
            xpos = xpos + keyboard.KeyWidth + 2.
        ELSE           
        DO:
            xpos = 0.
            ypos = ypos + keyHeightAtStartofRow + 2.
        END.
            
              
              
     END.
     
     RUN LockWindowUpdate( 0, OUTPUT IsLocked).
     FRAME {&frame-name}:VISIBLE = YES.
     
     RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Winn  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME DEFAULT-FRAME.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Winn  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY FILL-IN-24 
      WITH FRAME DEFAULT-FRAME.
  ENABLE FILL-IN-24 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Keyboard-Process-Key C-Winn 
PROCEDURE Keyboard-Process-Key :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER pKeyValue        AS CHARACTER CASE-SENSITIVE NO-UNDO.
    DEFINE INPUT PARAMETER pShiftKey        AS LOGICAL      NO-UNDO.
    DEFINE INPUT PARAMETER pCtrlKey         AS LOGICAL      NO-UNDO.
    DEFINE INPUT PARAMETER pAltKey          AS LOGICAL      NO-UNDO.
    
    DEFINE VARIABLE intResult        AS INTEGER     NO-UNDO.
    
    DEF VAR mi_vk_result AS INT NO-UNDO.
   
   FILL-IN-24:SCREEN-VALUE IN FRAME {&FRAME-NAME} = pKeyValue.
   
   
    IF pKeyValue EQ "" THEN
    DO:
        IF DYNAMIC-FUNCTION('getCapsLockState':U) EQ 0  THEN
            RUN createLayout (INPUT 1).

        IF DYNAMIC-FUNCTION('getCapsLockState':U) EQ 1  THEN
            RUN createLayout (INPUT 2).
    END.

    
    CASE TRUE:
        WHEN pKeyValue EQ '-1' THEN
        DO:
            RUN createLayout (INPUT 1).
            RETURN.
        END.
        WHEN pKeyValue EQ '-2' THEN
        DO:
            RUN createLayout (INPUT 2).
            RETURN.
        END.
        WHEN pKeyValue EQ '-3' THEN
        DO:
            RUN createLayout (INPUT 3).
            RETURN.
        END.
        WHEN pKeyValue EQ '-4' THEN
        DO:
            RUN createLayout (INPUT 4).
            RETURN.
        END.        
        

    END CASE.
    
/*     IF pi_vk_code LT "0" OR pi_vk_code EQ ? THEN */
/*     DO:                                          */
        

/*                                                                  */
/*         /** Uppcase Keys **/                                     */
/*         IF ABS(pi_vk_code) EQ 1 THEN                             */
/*         DO:                                                      */
/*             IF DYNAMIC-FUNCTION('getCapsLockState':U) EQ 0  THEN */
/*                 RUN toggleCapsLocks.                             */
/*                                                                  */
/*                                                                  */
/*         END.                                                     */
/*                                                                  */
/*         /** Lowercase Keys **/                                   */
/*         IF ABS(pi_vk_code) EQ 2 THEN                             */
/*         DO:                                                      */
/*                                                                  */
/*             IF DYNAMIC-FUNCTION('getCapsLockState':U) EQ 1  THEN */
/*                 RUN toggleCapsLocks.                             */
/*                                                                  */
/*                                                                  */
/*         END.                                                     */
        
        
        
/*                 */
/*         RETURN. */
/*     END.        */
    

   
    
/*     &scoped_define keyporessdown 0x01 */
/*     &scoped_define keyporessup 0x02   */

    DEFINE VARIABLE keyPress AS CHARACTER   NO-UNDO.
    
    /** OVERRIDE THE CUSTOM SPACE command.**/
    IF pKeyValue EQ "~{SPACE}":U THEN
        pKeyValue = " ".
        
    IF pCtrlKey OR pShiftKey OR pAltKey THEN
    DO:
            
        IF pCtrlKey THEN
            keyPress = "^":U.
            
        IF pShiftKey THEN
            keyPress = keyPress + "+":U. 
            
        IF pAltKey THEN
            keyPress = keyPress + "%":U.      
            
        keyPress = keyPress + "(" + pKeyValue + ")".    
     END.
     ELSE
        keyPress = pKeyValue.
     
     FILL-IN-24:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FILL-IN-24:SCREEN-VALUE IN FRAME {&FRAME-NAME} + " " + keyPress.
     
    /* THE MAGIC DOT NET */ 
    System.Windows.Forms.SendKeys:Send(keyPress).

/*     IF pShiftKey THEN                                               */
/*     DO:                                                             */
/*         RUN keybd_event (16, 0, 0, 0, OUTPUT mi_vk_result).         */
/*         RUN keybd_event (pi_vk_code, 0, 0, 0, OUTPUT mi_vk_result). */
/*         RUN keybd_event (pi_vk_code, 0, 2, 0, OUTPUT mi_vk_result). */
/*         RUN keybd_event (16, 0, 2, 0, OUTPUT mi_vk_result).         */
/*     END.                                                            */
/*     ELSE                                                            */
/*     DO:                                                             */
/*                                                                     */
/*                                                                     */
/*         IF FOCUS:TYPE EQ "FILL-IN" THEN                             */
/*         DO:                                                         */
/*             CASE pi_vk_code:                                        */
/*                WHEN 37 THEN                                         */
/*                     APPLY "CURSOR-LEFT" TO FOCUS.                   */
/*                WHEN 39 THEN                                         */
/*                     APPLY "CURSOR-RIGHT"  TO FOCUS.                 */
/*             END CASE.                                               */
/*         END.                                                        */
        
    
    
/*         RUN keybd_event (pi_vk_code, 0, 1, 0, OUTPUT mi_vk_result).  */
/*          RUN keybd_event (pi_vk_code, 0, 2, 0, OUTPUT mi_vk_result). */
/*         RUN keybd_event (pi_vk_code, 0, 0, 0, OUTPUT mi_vk_result). */

       
    
/*     END. */
    
/*          PROCESS EVENTS.                                                                   */
/*       DO WITH FRAME {&Frame-name}:                                                         */
/*                                                                                            */
/*          FILL-IN-22:SCREEN-VALUE =  STRING(KEYLABEL(LASTKEY))  + ' ' + STRING(pi_vk_code). */
/*       END.                                                                                 */
  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE toggleCapsLocks C-Winn 
PROCEDURE toggleCapsLocks PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE mi_vk_result AS INTEGER     NO-UNDO.
    
    RUN keybd_event (0x14, 0, 0x0, 0, OUTPUT mi_vk_result).        
    RUN keybd_event (0x14, 0, 0x2, 0, OUTPUT mi_vk_result).
    
    PROCESS EVENTS.
    
    RETURN.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCapsLockState C-Winn 
FUNCTION getCapsLockState RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE VARIABLE intResult AS INTEGER     NO-UNDO.
    RUN GetKeyState (INPUT {&VK_CAPITAL}, OUTPUT intResult).
     
  RETURN intResult.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

