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

  Author: James Bowen

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





/* PROCEDURE keybd_event EXTERNAL "USER32.DLL":     */
/*   DEF INPUT  PARAM bVk         AS SHORT NO-UNDO. */
/*   DEF INPUT  PARAM bScan       AS SHORT NO-UNDO. */
/*   DEF INPUT  PARAM dwFlags     AS LONG  NO-UNDO. */
/*   DEF INPUT  PARAM dwExtraInfo AS LONG  NO-UNDO. */
/*   DEF RETURN PARAM intResult   AS LONG  NO-UNDO. */
/* END PROCEDURE.                                   */



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

    
DEFINE TEMP-TABLE Updatekeyboard NO-UNDO like keyboard.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-24 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
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
     FILL-IN-24 AT ROW 13.88 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 18 NO-TAB-STOP 
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
         HEIGHT             = 20.08
         WIDTH              = 213.72
         MAX-HEIGHT         = 48.42
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 48.42
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
/* SETTINGS FOR FILL-IN FILL-IN-24 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-24:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE
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
  
    

    //RUN createLayout (INPUT ?).
    
      
    
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
    
    
    

     
    FRAME {&frame-name}:VISIBLE = NO.
    

                             
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
              VISIBLE   = TRUE
              selectable = true
              SENSITIVE =  true
              private-data = string(xpos - keyboard.indent)
              BGCOLOR   = (IF keyboard.KeyValue EQ "" THEN 12 ELSE ?)
              TRIGGERS:
                ON selection persistent RUN Keyboard-Get-Key IN THIS-PROCEDURE ( input rowid(keyboard) ).
              END TRIGGERS.
         
         ASSIGN
            keyboard.WIDGET-HANDLE = but1:HANDLE.
         
        IF keyboard.imageUp NE "" AND SEARCH(keyboard.imageUp) NE "" THEN 
            but1:LOAD-IMAGE-UP(SEARCH(keyboard.imageUp)).                  
             
        IF keyboard.imageDown NE "" AND SEARCH(keyboard.imageDown) NE "" THEN 
            but1:LOAD-IMAGE-DOWN(SEARCH(keyboard.imageDown)).    
           
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
  VIEW FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Keyboard-Get-Key C-Winn 
PROCEDURE Keyboard-Get-Key :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    define input parameter keyboardROWID as rowid no-undo.
    
    define buffer buffKeyboard for temp-table Keyboard .
    
    empty temp-table Updatekeyboard.
    
    for first buffKeyboard
        where rowid(buffKeyboard) eq keyboardROWID:
        
        run Keyboard-Get-Key in source-procedure (buffer buffKeyboard).
        
        run updateSatus in source-procedure (input "Get Keyboard config" ).
        
    end.
  
    return.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoadKeyboardLayout C-Winn 
PROCEDURE LoadKeyboardLayout :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE input parameter pchFullPathname  AS CHARACTER   NO-UNDO.
    DEFINE input parameter pchFileName      AS CHARACTER   NO-UNDO.
    
    run clearDynamicKeys in this-procedure.
    
    run updateSatus in source-procedure (input "Loaded: " + pchFileName ). 
    
    pchFileName =  pchFullPathname + '\' + pchFileName. 
    
    DATASET keyboardLayout:READ-XML("FILE", pchFileName, "EMPTY", ?, ?).

    FOR first keyboard:
        RUN createLayout (INPUT keyboard.Layer ).
    end.
    
    run clearScreen in source-procedure.
    
    RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveKeyboardXML C-Winn 
PROCEDURE SaveKeyboardXML :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE input parameter pchFullPathname  AS CHARACTER   NO-UNDO.
    DEFINE input parameter pchFileName      AS CHARACTER   NO-UNDO.
    
    run updateSatus in source-procedure (input "Saved: " + pchFileName ). 
    
    pchFileName =  pchFullPathname + '\' + pchFileName. 
    
    for each keyboard
        break by keyboard.Layer:
    
        if first-of(keyboard.Layer) then
        do:
            
            find keyboardLayer
                where keyboardLayer.Layer eq keyboard.Layer
                no-error.
                
            if not available keyboardLayer then
            do:
            
                create keyboardLayer.
                
                assign
                    keyboardLayer.Layer = keyboard.Layer.
                
            end.
        end.
    end.
    
    
    dataset keyboardLayout:write-xml("FILE", pchFileName, true).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateKeyboardRecord C-Winn 
PROCEDURE updateKeyboardRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    define input parameter keyboardRowID as rowid no-undo.
    
    define PARAMETER BUFFER updateKeyboard for temp-table Keyboard.
    
    define BUFFER buffKeyboard for temp-table keyboard.
    
    for first buffKeyboard
        where rowid(buffKeyboard) eq keyboardRowID:
            
        buffer-copy updateKeyboard to buffKeyboard.
        
        run updateSatus in source-procedure (input "Updated Keyboard config" ).
        
    end.
    
    return.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

