&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE virtualKeyboard AS HANDLE NO-UNDO.
DEFINE variable currentButton   AS HANDLE NO-UNDO.


DEFINE temp-table Keyboard NO-UNDO SERIALIZE-NAME "keyboard"
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS imgImageUp imgImageDown fiXMLFolder ~
BUTTON-13 fiLayer fiRow fiPosition fiKeyWidth XMLFileLayer fiKeyLabel ~
fiKeyHeight fiKeyValue tbCtrlKey tbAltKey tbShiftKey fiKeyIndent ~
tbSensitive fiImageUp fiImageDown bntSave fiRowid 
&Scoped-Define DISPLAYED-OBJECTS fiXMLFolder fiLayer fiRow fiPosition ~
fiKeyWidth XMLFileLayer fiKeyLabel fiKeyHeight fiKeyValue tbCtrlKey ~
tbAltKey tbShiftKey fiKeyIndent tbSensitive fiImageUp fiImageDown fiRowid 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bntSave 
     LABEL "Save XML" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-13 
     LABEL "Folder" 
     SIZE 7 BY 1.12.

DEFINE VARIABLE fiImageDown AS CHARACTER FORMAT "X(256)":U 
     LABEL "Image Down" 
     VIEW-AS FILL-IN 
     SIZE 77 BY 1 NO-UNDO.

DEFINE VARIABLE fiImageUp AS CHARACTER FORMAT "X(256)":U 
     LABEL "Image Up" 
     VIEW-AS FILL-IN 
     SIZE 77 BY 1 NO-UNDO.

DEFINE VARIABLE fiKeyHeight AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Height" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiKeyIndent AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Indent" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiKeyLabel AS CHARACTER FORMAT "X(256)":U 
     LABEL "Key Label" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE fiKeyValue AS CHARACTER FORMAT "X(256)":U 
     LABEL "Key value" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE fiKeyWidth AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Width" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiLayer AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Layer" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE fiPosition AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Pos" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE fiRow AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Row" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE fiRowid AS CHARACTER FORMAT "X(256)":U 
     LABEL "Row ID" 
      VIEW-AS TEXT 
     SIZE 20 BY .62 NO-UNDO.

DEFINE VARIABLE fiXMLFolder AS CHARACTER FORMAT "X(256)":U 
     LABEL "Folder" 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1 NO-UNDO.

DEFINE IMAGE imgImageDown
     FILENAME "adeicon/blank":U
     SIZE 9.14 BY 2.46.

DEFINE IMAGE imgImageUp
     FILENAME "adeicon/blank":U
     SIZE 9.14 BY 2.46.

DEFINE VARIABLE XMLFileLayer AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 55 BY 6.73 NO-UNDO.

DEFINE VARIABLE tbAltKey AS LOGICAL INITIAL no 
     LABEL "Alt Key" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .77 NO-UNDO.

DEFINE VARIABLE tbCtrlKey AS LOGICAL INITIAL no 
     LABEL "Cltr Key" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .77 NO-UNDO.

DEFINE VARIABLE tbSensitive AS LOGICAL INITIAL no 
     LABEL "Sensitive" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.14 BY .77 NO-UNDO.

DEFINE VARIABLE tbShiftKey AS LOGICAL INITIAL no 
     LABEL "Shift Key" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.14 BY .77 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiXMLFolder AT ROW 2.08 COL 20 COLON-ALIGNED WIDGET-ID 40
     BUTTON-13 AT ROW 2.08 COL 70 WIDGET-ID 42
     fiLayer AT ROW 2.35 COL 90 COLON-ALIGNED WIDGET-ID 18 NO-TAB-STOP 
     fiRow AT ROW 2.35 COL 105 COLON-ALIGNED WIDGET-ID 54 NO-TAB-STOP 
     fiPosition AT ROW 2.35 COL 120 COLON-ALIGNED WIDGET-ID 56 NO-TAB-STOP 
     fiKeyWidth AT ROW 2.35 COL 153 COLON-ALIGNED WIDGET-ID 34
     XMLFileLayer AT ROW 3.42 COL 22 NO-LABEL WIDGET-ID 30
     fiKeyLabel AT ROW 3.42 COL 90 COLON-ALIGNED WIDGET-ID 46
     fiKeyHeight AT ROW 3.42 COL 153 COLON-ALIGNED WIDGET-ID 38
     fiKeyValue AT ROW 4.5 COL 90 COLON-ALIGNED WIDGET-ID 16
     tbCtrlKey AT ROW 4.5 COL 117 WIDGET-ID 10
     tbAltKey AT ROW 4.5 COL 127 WIDGET-ID 12
     tbShiftKey AT ROW 4.5 COL 137 WIDGET-ID 14
     fiKeyIndent AT ROW 4.5 COL 153 COLON-ALIGNED WIDGET-ID 48
     tbSensitive AT ROW 5.58 COL 92 WIDGET-ID 20
     fiImageUp AT ROW 6.38 COL 90 COLON-ALIGNED WIDGET-ID 22
     fiImageDown AT ROW 7.46 COL 90 COLON-ALIGNED WIDGET-ID 26
     bntSave AT ROW 8.81 COL 154 WIDGET-ID 44
     fiRowid AT ROW 9.08 COL 90 COLON-ALIGNED WIDGET-ID 52 NO-TAB-STOP 
     imgImageUp AT ROW 4.5 COL 172 WIDGET-ID 24
     imgImageDown AT ROW 7.19 COL 172 WIDGET-ID 32
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 203.57 BY 36.96 WIDGET-ID 100.

DEFINE FRAME FRAME-I
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 7 ROW 11.23
         SIZE 194 BY 25.58
         BGCOLOR 7  WIDGET-ID 200.


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
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Virtual Keyboard Maintenance"
         HEIGHT             = 36.04
         WIDTH              = 203.57
         MAX-HEIGHT         = 40.42
         MAX-WIDTH          = 248.29
         VIRTUAL-HEIGHT     = 40.42
         VIRTUAL-WIDTH      = 248.29
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-I:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
ASSIGN 
       fiLayer:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiPosition:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiRow:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiRowid:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FRAME FRAME-I
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-I
/* Query rebuild information for FRAME FRAME-I
     _Query            is NOT OPENED
*/  /* FRAME FRAME-I */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Virtual Keyboard Maintenance */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Virtual Keyboard Maintenance */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bntSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bntSave C-Win
ON CHOOSE OF bntSave IN FRAME DEFAULT-FRAME /* Save XML */
DO:
   run SaveKeyboardXML in virtualKeyboard (input XMLFileLayer:private-data, input XMLFileLayer:screen-value).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-13 C-Win
ON CHOOSE OF BUTTON-13 IN FRAME DEFAULT-FRAME /* Folder */
DO:
  
  do with frame {&frame-name}:
  
        assign
            fiXMLFolder.
  
        SYSTEM-DIALOG GET-DIR fiXMLFolder  INITIAL-DIR fiXMLFolder  TITLE "XML Folder".
  
        run scanfolder in this-procedure.
  
  end.
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiImageDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiImageDown C-Win
ON VALUE-CHANGED OF fiImageDown IN FRAME DEFAULT-FRAME /* Image Down */
DO:
  
    run updateKeyboardRecord in this-procedure.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiImageUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiImageUp C-Win
ON VALUE-CHANGED OF fiImageUp IN FRAME DEFAULT-FRAME /* Image Up */
DO:
  run updateKeyboardRecord in this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiKeyHeight
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiKeyHeight C-Win
ON CURSOR-DOWN OF fiKeyHeight IN FRAME DEFAULT-FRAME /* Height */
DO:
    self:screen-value = string(integer(self:screen-value) - 1).
  
    if valid-handle(currentButton) and integer(self:screen-value) ge 1 then
        currentButton:height-pixels = integer(self:screen-value).
    
    run updateKeyboardRecord in this-procedure.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiKeyHeight C-Win
ON CURSOR-UP OF fiKeyHeight IN FRAME DEFAULT-FRAME /* Height */
DO:
    self:screen-value = string(integer(self:screen-value) + 1).
  
    if valid-handle(currentButton) then
        currentButton:height-pixels = integer(self:screen-value).
    
    run updateKeyboardRecord in this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiKeyHeight C-Win
ON VALUE-CHANGED OF fiKeyHeight IN FRAME DEFAULT-FRAME /* Height */
DO:
  run updateKeyboardRecord in this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiKeyIndent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiKeyIndent C-Win
ON CURSOR-LEFT OF fiKeyIndent IN FRAME DEFAULT-FRAME /* Indent */
DO:
    DEFINE VARIABLE inIndent AS INTEGER     NO-UNDO.
    
    inIndent = integer(self:screen-value) - 1.
    
    inIndent = maximum(inIndent, 0).
    
    self:screen-value = string(inIndent).
  
/*     message currentButton:type  */
/*         view-as alert-box info. */
  
    if valid-handle(currentButton) and inIndent ge 0 then
        currentButton:x = integer(currentButton:private-data) + inIndent. 
        
    run updateKeyboardRecord in this-procedure.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiKeyIndent C-Win
ON CURSOR-RIGHT OF fiKeyIndent IN FRAME DEFAULT-FRAME /* Indent */
DO:
    DEFINE VARIABLE inIndent AS INTEGER     NO-UNDO.
    
    inIndent = integer(self:screen-value) + 1.
    inIndent = maximum(inIndent, 0).
    
    self:screen-value = string(inIndent).
  
    if valid-handle(currentButton) and inIndent ge 0 then
        currentButton:x = integer(currentButton:private-data) + inIndent. 
        
    run updateKeyboardRecord in this-procedure.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiKeyIndent C-Win
ON VALUE-CHANGED OF fiKeyIndent IN FRAME DEFAULT-FRAME /* Indent */
DO:
  run updateKeyboardRecord in this-procedure.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiKeyLabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiKeyLabel C-Win
ON VALUE-CHANGED OF fiKeyLabel IN FRAME DEFAULT-FRAME /* Key Label */
DO:

    if valid-handle(currentButton) then
    do:
        currentButton:label = self:screen-value.
    end.
    
    run updateKeyboardRecord in this-procedure.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiKeyValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiKeyValue C-Win
ON VALUE-CHANGED OF fiKeyValue IN FRAME DEFAULT-FRAME /* Key value */
DO:
  run updateKeyboardRecord in this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiKeyWidth
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiKeyWidth C-Win
ON CURSOR-LEFT OF fiKeyWidth IN FRAME DEFAULT-FRAME /* Width */
DO:
    self:screen-value = string(integer(self:screen-value) - 1).
  
    if valid-handle(currentButton) and integer(self:screen-value) ge 1 then
    do:
        currentButton:width-pixels = integer(self:screen-value).
    end.
    
    run updateKeyboardRecord in this-procedure.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiKeyWidth C-Win
ON CURSOR-RIGHT OF fiKeyWidth IN FRAME DEFAULT-FRAME /* Width */
DO:
    self:screen-value = string(integer(self:screen-value) + 1).
  
    if valid-handle(currentButton) then
    do:
        currentButton:width-pixels = integer(self:screen-value).
    end. 
    
    run updateKeyboardRecord in this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiKeyWidth C-Win
ON VALUE-CHANGED OF fiKeyWidth IN FRAME DEFAULT-FRAME /* Width */
DO:
  run updateKeyboardRecord in this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiXMLFolder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiXMLFolder C-Win
ON RETURN OF fiXMLFolder IN FRAME DEFAULT-FRAME /* Folder */
DO:
     run scanfolder in this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAltKey
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAltKey C-Win
ON VALUE-CHANGED OF tbAltKey IN FRAME DEFAULT-FRAME /* Alt Key */
DO:
  run updateKeyboardRecord in this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbCtrlKey
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbCtrlKey C-Win
ON VALUE-CHANGED OF tbCtrlKey IN FRAME DEFAULT-FRAME /* Cltr Key */
DO:
  run updateKeyboardRecord in this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbSensitive
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbSensitive C-Win
ON VALUE-CHANGED OF tbSensitive IN FRAME DEFAULT-FRAME /* Sensitive */
DO:
  run updateKeyboardRecord in this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbShiftKey
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbShiftKey C-Win
ON VALUE-CHANGED OF tbShiftKey IN FRAME DEFAULT-FRAME /* Shift Key */
DO:
  run updateKeyboardRecord in this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME XMLFileLayer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL XMLFileLayer C-Win
ON VALUE-CHANGED OF XMLFileLayer IN FRAME DEFAULT-FRAME
DO:
  run LoadKeyboardLayout in virtualKeyboard (input self:private-data, input self:screen-value).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


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
  RUN enable_UI.
  
  run initialise in this-procedure.
  
  RUN DynamicKeyboardEdit.w PERSISTENT SET virtualKeyboard ( INPUT FRAME {&frame-name}:HANDLE,
                                                             INPUT FRAME FRAME-I:HANDLE).
  

  WAIT-FOR System.Windows.Forms.Application:Run().

     
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearScreen C-Win 
PROCEDURE clearScreen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    do with frame {&frame-name}:
    
        assign
            fiRowid = ""
            fiLayer         = ?
            fiKeyLabel      = ""
            fiKeyValue      = ""
            fiImageUp       = ""
            fiImageDown     = ""
            fiKeyWidth      = ?
            fiKeyHeight     = ?
            fiKeyIndent     = ?
            fiRow           = ?
            fiPosition      = ?
            .
            
        tbSensitive:checked = false.
        tbCtrlKey:checked   = false.
        tbAltKey:checked    = false.
        tbShiftKey:checked  = false.
            
        imgImageUp:LOAD-IMAGE("") .
        imgImageDown:LOAD-IMAGE("") .
            
        display 
            fiRowid
            fiLayer
            fiRow
            fiPosition
            fiKeyLabel
            fiKeyValue
            fiImageUp
            fiImageDown
            fiKeyWidth
            fiKeyHeight
            fiKeyIndent
            .
            
        disable
            fiRowid
            fiLayer
            fiRow
            fiPosition
            fiKeyLabel
            fiKeyValue
            fiImageUp
            fiImageDown
            fiKeyWidth
            fiKeyHeight
            fiKeyIndent
            tbSensitive
            tbCtrlKey
            tbAltKey
            tbShiftKey
            .            
        
    
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY fiXMLFolder fiLayer fiRow fiPosition fiKeyWidth XMLFileLayer 
          fiKeyLabel fiKeyHeight fiKeyValue tbCtrlKey tbAltKey tbShiftKey 
          fiKeyIndent tbSensitive fiImageUp fiImageDown fiRowid 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE imgImageUp imgImageDown fiXMLFolder BUTTON-13 fiLayer fiRow fiPosition 
         fiKeyWidth XMLFileLayer fiKeyLabel fiKeyHeight fiKeyValue tbCtrlKey 
         tbAltKey tbShiftKey fiKeyIndent tbSensitive fiImageUp fiImageDown 
         bntSave fiRowid 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW FRAME FRAME-I IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-I}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialise C-Win 
PROCEDURE initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    do with frame {&frame-name}:
    
        file-info:file-name = '.'.
        
        assign
            fiXMLFolder = file-info:full-pathname.
        
        display
            fiXMLFolder.
    
        run scanfolder in this-procedure.
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Keyboard-Get-Key C-Win 
PROCEDURE Keyboard-Get-Key :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE PARAMETER BUFFER buffKeyboard for temp-table Keyboard.
    
    define buffer buffEditKeyboard for temp-table Keyboard.
  
    do with frame {&frame-name}:

        fiRowid:screen-value = string(rowid(buffKeyboard)).
    
        assign
            currentButton   = buffKeyboard.WIDGET-HANDLE
            fiLayer         = buffKeyboard.layer
            fiKeyLabel      = buffKeyboard.KeyCaption
            fiKeyValue      = buffKeyboard.keyValue
            fiImageUp       = buffKeyboard.imageUp
            fiImageDown     = buffKeyboard.imageDown
            fiKeyWidth      = buffKeyboard.KeyWidth
            fiKeyHeight     = buffKeyboard.KeyHeight
            fiKeyIndent     = buffKeyboard.Indent
            fiRow           = buffKeyboard.KeyboardRow
            fiPosition      = buffKeyboard.KeyboardPos
            .
            
        tbSensitive:checked = buffKeyboard.sensitive.
        tbCtrlKey:checked   = buffKeyboard.ctrlKey.
        tbAltKey:checked    = buffKeyboard.altKey.
        tbShiftKey:checked  = buffKeyboard.shiftKey.
            
        imgImageUp:LOAD-IMAGE(fiImageUp) .
        imgImageDown:LOAD-IMAGE(fiImageDown) .
            
        display 
            fiLayer
            fiRow
            fiPosition
            fiKeyLabel
            fiKeyValue
            fiImageUp
            fiImageDown
            fiKeyWidth
            fiKeyHeight
            fiKeyIndent
            .
            
         enable
            fiRowid
            fiLayer
            fiRow
            fiPosition
            fiKeyLabel
            fiKeyValue
            fiImageUp
            fiImageDown
            fiKeyWidth
            fiKeyHeight
            fiKeyIndent
            tbSensitive
            tbCtrlKey
            tbAltKey
            tbShiftKey
            .            
         
         //buffer-copy Keyboard to buffEditKeyboard.
            
    end.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE scanfolder C-Win 
PROCEDURE scanfolder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    do with frame {&frame-name}:
    
        DEFINE VARIABLE chFolderFile AS CHARACTER   NO-UNDO.
        DEFINE VARIABLE chFullPath AS CHARACTER   NO-UNDO.
        DEFINE VARIABLE iNumItems AS INTEGER     NO-UNDO.
        
        XMLFileLayer:private-data = fiXMLFolder.          
        
        
        do iNumItems = 1 to XMLFileLayer:NUM-ITEMS:
            XMLFileLayer:DELETE(iNumItems).
        end.
        
        XMLFileLayer:LIST-ITEMS = "".
        
        input from os-dir(fiXMLFolder) NO-ATTR-LIST.
               
       
               
        repeat:
        
            import chFolderFile chFullPath NO-ERROR.
            
            if chFolderFile eq '.' or chFolderFile eq '..' then
                next.
            
            if not chFolderFile matches '*.xml' then
                next.

              
             XMLFileLayer:ADD-LAST(chFolderFile).    
        
        end. 
        
        input close. 
    
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateKeyboardRecord C-Win 
PROCEDURE updateKeyboardRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    define buffer updateKeyboard for temp-table Keyboard.
    DEFINE VARIABLE editKeyboardROWID AS ROWID       NO-UNDO.

    do with frame {&frame-name}:
    
        editKeyboardROWID = to-rowid( fiRowid:screen-value ).

/*          find first Keyboard where                */
/*              rowid(Keyboard) eq editKeyboardROWID */
/*              no-error.                            */

        create updateKeyboard.
             
        assign 
            updateKeyboard.widget-handle = currentButton
            updateKeyboard.KeyCaption  = fiKeyLabel:screen-value
            updateKeyboard.keyValue    = fiKeyValue:screen-value
            updateKeyboard.shiftKey    = tbShiftKey:checked
            updateKeyboard.ctrlKey     = tbCtrlKey:checked
            updateKeyboard.altKey      = tbAltKey:checked
            updateKeyboard.Layer       = integer(fiLayer:screen-value)
            updateKeyboard.KeyboardRow = integer(fiRow:screen-value)
            updateKeyboard.KeyboardPos = integer(fiPosition:screen-value)
            updateKeyboard.KeyWidth    = integer(fiKeyWidth:screen-value)
            updateKeyboard.KeyHeight   = integer(fiKeyHeight:screen-value)
            updateKeyboard.indent      = integer(fiKeyIndent:screen-value)
            updateKeyboard.SENSITIVE   = tbSensitive:checked
            updateKeyboard.imageUp     = fiImageUp:screen-value
            updateKeyboard.imageDown   = fiImageDown:screen-value.
            
            
        run updateKeyboardRecord in virtualKeyboard (input editKeyboardROWID, buffer updateKeyboard).
           
        empty temp-table updateKeyboard.           
           
    
    end.
    
    return.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateSatus C-Win 
PROCEDURE updateSatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE input parameter pchFullPathname  AS CHARACTER   NO-UNDO.
    
    status  INPUT pchFullPathname.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

