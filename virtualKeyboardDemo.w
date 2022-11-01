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

DEFINE VARIABLE virtualKeyboard AS HANDLE NO-UNDO.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-5 BUTTON-8 FILL-IN-1 FILL-IN-2 ~
FILL-IN-21 FILL-IN-23 FILL-IN-3 EDITOR-2 BUTTON-6 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-1 FILL-IN-2 FILL-IN-21 FILL-IN-23 ~
FILL-IN-3 EDITOR-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-5  NO-FOCUS
     LABEL "<" 
     SIZE 7 BY 1.14.

DEFINE BUTTON BUTTON-6  NO-FOCUS
     LABEL ">" 
     SIZE 7 BY 1.14.

DEFINE BUTTON BUTTON-8 
     IMAGE-UP FILE "images/keyboard keys/clavier/q.png":U
     LABEL "Button 8" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE EDITOR-2 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 106 BY 5.71 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 1" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 105 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Fill 2" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-21 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 21" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-23 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 23" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Fill 3" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-5 AT ROW 1.48 COL 53 WIDGET-ID 14 NO-TAB-STOP 
     BUTTON-8 AT ROW 1.71 COL 10 WIDGET-ID 26
     FILL-IN-1 AT ROW 3.14 COL 28 COLON-ALIGNED WIDGET-ID 2
     FILL-IN-2 AT ROW 4.57 COL 28 COLON-ALIGNED WIDGET-ID 4
     FILL-IN-21 AT ROW 4.81 COL 84 COLON-ALIGNED WIDGET-ID 12 NO-TAB-STOP 
     FILL-IN-23 AT ROW 4.81 COL 124 COLON-ALIGNED WIDGET-ID 22
     FILL-IN-3 AT ROW 6 COL 28 COLON-ALIGNED WIDGET-ID 6
     EDITOR-2 AT ROW 7.19 COL 30 NO-LABEL WIDGET-ID 10
     BUTTON-6 AT ROW 1.48 COL 60 WIDGET-ID 16 NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 159.6 BY 36.29 WIDGET-ID 100.

DEFINE FRAME FRAME-F
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         THREE-D 
         AT COL 6 ROW 13.62
         SCROLLABLE SIZE 216 BY 17.38
         BGCOLOR 10  WIDGET-ID 200.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 36.29
         WIDTH              = 159.6
         MAX-HEIGHT         = 48.43
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 48.43
         VIRTUAL-WIDTH      = 384
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-F:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-F:MOVE-AFTER-TAB-ITEM (EDITOR-2:HANDLE IN FRAME DEFAULT-FRAME)
/* END-ASSIGN-TABS */.

ASSIGN 
       EDITOR-2:RETURN-INSERTED IN FRAME DEFAULT-FRAME  = TRUE.

/* SETTINGS FOR FRAME FRAME-F
   UNDERLINE Size-to-Fit                                                */
ASSIGN 
       FRAME FRAME-F:HEIGHT           = 17.38
       FRAME FRAME-F:WIDTH            = 154.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-F
/* Query rebuild information for FRAME FRAME-F
     _Query            is NOT OPENED
*/  /* FRAME FRAME-F */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  
  
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 C-Win
ON CHOOSE OF BUTTON-5 IN FRAME DEFAULT-FRAME /* < */
DO:
  APPLY "CURSOR-LEFT".
  
      DO WITH FRAME {&Frame-name}:
      
         FILL-IN-21:SCREEN-VALUE = STRING(LASTKEY)  .
      END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 C-Win
ON CHOOSE OF BUTTON-6 IN FRAME DEFAULT-FRAME /* > */
DO:
  APPLY "CURSOR-RIGHT".
  
      DO WITH FRAME {&Frame-name}:
      
         FILL-IN-21:SCREEN-VALUE = STRING(LASTKEY)  .
      END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EDITOR-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDITOR-2 C-Win
ON ENTRY OF EDITOR-2 IN FRAME DEFAULT-FRAME
DO:
  RUN createLayout IN virtualKeyboard (INPUT ?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-1 C-Win
ON ANY-KEY OF FILL-IN-1 IN FRAME DEFAULT-FRAME /* Fill 1 */
DO:
        DO WITH FRAME {&Frame-name}:

         FILL-IN-21:SCREEN-VALUE = STRING(KEYLABEL(LASTKEY))  .
      END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-1 C-Win
ON ENTRY OF FILL-IN-1 IN FRAME DEFAULT-FRAME /* Fill 1 */
DO:
   RUN createLayout IN virtualKeyboard (INPUT ?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-2 C-Win
ON ENTRY OF FILL-IN-2 IN FRAME DEFAULT-FRAME /* Fill 2 */
DO:
  RUN createLayout IN virtualKeyboard (INPUT 3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-23
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-23 C-Win
ON CTRL-SHIFT-ALT-F2 OF FILL-IN-23 IN FRAME DEFAULT-FRAME /* Fill 23 */
DO:
  MESSAGE "Ctr SHIFT ALT F2".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-3 C-Win
ON ENTRY OF FILL-IN-3 IN FRAME DEFAULT-FRAME /* Fill 3 */
DO:
  RUN createLayout IN virtualKeyboard (INPUT 3).
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
  
  RUN DynamicKeyboard.w PERSISTENT SET virtualKeyboard ( INPUT FRAME {&frame-name}:HANDLE,
                                                         INPUT FRAME FRAME-F:HANDLE).
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN .
/*     WAIT-FOR CLOSE OF THIS-PROCEDURE. */
    WAIT-FOR System.Windows.Forms.Application:Run().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY FILL-IN-1 FILL-IN-2 FILL-IN-21 FILL-IN-23 FILL-IN-3 EDITOR-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BUTTON-5 BUTTON-8 FILL-IN-1 FILL-IN-2 FILL-IN-21 FILL-IN-23 FILL-IN-3 
         EDITOR-2 BUTTON-6 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW FRAME FRAME-F IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-F}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

