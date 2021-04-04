      * Generation Parameters - SCRVER(02)            Do Not Delete|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PVZZ5.
      *
      * Copyright 1986-2020, Computer Sciences Corporation.
      *
      *
      *REMARKS.
      *
      * The remarks section should detail the main processes of the
      * program and any special functions.
      *
      ***********************************************************************
      *           AMENDMENT  HISTORY                                        *
      ***********************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....                               *
      *                                                                     *
      * 27/11/20  01/01   DUMMY        IT-Nguyen Huu Tri                    *
      *           AGENT POLICY IMFORMATION                                  *
      *                                                                     *
      **DD/MM/YY*************************************************************
      /
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WSAA-PROG                   PIC X(05) VALUE 'PVZZ5'.
       01  WSAA-VERSION                PIC X(02) VALUE '01'.
       01  WSAA-AGNT-NAME              PIC X(50)           .
       01  WSAA-END-FILE               PIC X(01)           .
      *
       01  ERRORS.
           03  E005                    PIC X(04) VALUE 'E005'.
           03  E058                    PIC X(04) VALUE 'E058'.
      *
       01  TABLES.
           03  TNNNN                   PIC X(05) VALUE 'TNNNN'.
      *
       01  FORMATS.
           03  XXXXREC                 PIC X(10) VALUE 'XXXXREC'.
           03  CLNTREC                 PIC X(10) VALUE 'CLNTREC'.
           03  AGNTLAGREC              PIC X(10) VALUE 'AGNTLAGREC'.
           03  CHDRAGTREC              PIC X(10) VALUE 'CHDRAGTREC'.
           03  COVRREC                 PIC X(10) VALUE 'COVRREC'.
           03  COVTREC                 PIC X(10) VALUE 'COVTREC'.


      /
           COPY VARCOM.
      *
           COPY SYSERRREC.
      *
           COPY OPSTATSREC.
      *
           COPY CLNTSKM.
      *
           COPY AGNTLAGSKM.
      *
           COPY CHDRAGTSKM.
      *
           COPY COVRSKM.
      *
           COPY COVTSKM.
      *


      /
       LINKAGE SECTION.

           COPY WSSPCOMN.

       01  WSSP-USER-AREA              PIC X(768).

           COPY SCRNPARAMS.

           COPY SVZZ5SCR.
      /
       PROCEDURE DIVISION           USING WSSP-COMMON-AREA
                                          WSSP-USER-AREA
                                          SCRN-SCREEN-PARAMS
                                          SVZZ5-DATA-AREA
                                          SVZZ5-SUBFILE-AREA.

           COPY MAING.
      /
      *****************************************************************
      *      Initialise Fields for showing on Screen
      *****************************************************************
      *
       1000-INITIALISE SECTION.
      *************************
      *
       1010-INITIALISE.
      *
      *
      * Initialise Fields
      *
           MOVE SPACES                 TO SVZZ5-DATA-AREA.
           MOVE SPACES                 TO SVZZ5-SUBFILE-AREA.
           MOVE SPACES                 TO WSAA-AGNT-NAME.
           MOVE 'N'                    TO WSAA-END-FILE.

      * Dummy subfile initialisation for prototype - replace with SCLR
      *    MOVE SINIT                  TO SCRN-FUNCTION.

      *    CALL 'SVZZ5IO'           USING SCRN-SCREEN-PARAMS
      *                                   SVZZ5-DATA-AREA
      *                                   SVZZ5-SUBFILE-AREA.

      *    IF  SCRN-STATUZ          NOT = O-K
      *        MOVE SCRN-STATUZ        TO SYSR-STATUZ
      *        PERFORM 600-FATAL-ERROR
      *    END-IF.

      *    MOVE 1                      TO SCRN-SUBFILE-RRN.
      *
           MOVE SCLR                   TO    SCRN-FUNCTION.

           CALL 'SVZZ5IO'              USING SCRN-SCREEN-PARAMS
                                             SVZZ5-DATA-AREA
                                             SVZZ5-SUBFILE-AREA.

           IF  SCRN-STATUZ             NOT   = O-K
               MOVE SCRN-STATUZ        TO    SYSR-STATUZ
               PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE 1                      TO SCRN-SUBFILE-RRN.




      *    Dummy field initialisation for prototype version.
******

      *
      *    Set screen fields
      *
       1020-RETRV-CLNT.
      *
           MOVE 'RETRV'                TO     AGNTLAG-FUNCTION.
           CALL 'AGNTLAGIO'            USING  AGNTLAG-PARAMS.

           IF AGNTLAG-STATUZ           NOT =  O-K AND MRNF

              MOVE AGNTLAG-PARAMS      TO     SYSR-PARAMS
              PERFORM 600-FATAL-ERROR

           END-IF.

           MOVE AGNTLAG-AGNTNUM        TO     SVZZ5-AGENTNO.

           MOVE 'CN'                   TO     CLNT-CLNTPFX.
           MOVE '9'                    TO     CLNT-CLNTCOY.
           MOVE AGNTLAG-CLNTNUM        TO     CLNT-CLNTNUM.

           MOVE CLNTREC                TO     CLNT-FORMAT.
           MOVE READR                  TO     CLNT-FUNCTION.
           CALL 'CLNTIO'               USING  CLNT-PARAMS.
           IF CLNT-STATUZ              NOT =  O-K AND MRNF

              MOVE CLNT-PARAMS         TO     SYSR-PARAMS
              PERFORM 600-FATAL-ERROR

           END-IF.

           IF CLNT-STATUZ                  = MRNF

              MOVE E058                TO    SVZZ5-CFRNAM-ERR
              GO TO 1090-EXIT

           END-IF.

           STRING
                  CLNT-LGIVNAME DELIMITED BY ' '
                  SPACES CLNT-LSURNAME DELIMITED BY SIZE
                  INTO WSAA-AGNT-NAME
           END-STRING.

           MOVE WSAA-AGNT-NAME         TO SVZZ5-CFRNAM.

           MOVE 'AG'                   TO CHDRAGT-AGNTPFX.
           MOVE '2'                    TO CHDRAGT-AGNTCOY.
           MOVE SVZZ5-AGENTNO          TO CHDRAGT-AGNTNUM.
           MOVE SPACES                 TO CHDRAGT-VALIDFLAG.
           MOVE '2'                    TO CHDRAGT-CHDRCOY.
           MOVE SPACES                 TO CHDRAGT-CHDRNUM.

           MOVE BEGN                   TO CHDRAGT-FUNCTION.
           PERFORM  1100-LOAD-SUBFILE-PAGE
                                       UNTIL WSAA-END-FILE = 'Y'.
      *    MOVE 'N'                    TO WSAA-END-FILE.
           MOVE 1                      TO SCRN-SUBFILE-RRN.



      *
      *    Perform section to load first page of subfile
      *    Example:
      *    PERFORM  1200-LOAD-SUBFILE
      *               UNTIL ABCD-STATUZ = ENDP OR
      *                      WSAA-COUNT = SXXXX-SUBFILE-PAGE.
      *

      *
       1090-EXIT.
            EXIT.
      /

      *
      *    Sections performed from the 1000 section above.
      *      (including subfile load section)
      * e.g. 1200-LOAD-SUBILE SECTION.
      *
       1100-LOAD-SUBFILE-PAGE SECTION.
      ********************************
       1110-WRITE-TO-SUBFILE.

           MOVE SPACES                 TO SVZZ5-SUBFILE-FIELDS.
           CALL 'CHDRAGTIO'            USING CHDRAGT-PARAMS.
           MOVE NEXTR                  TO CHDRAGT-FUNCTION.
           IF CHDRAGT-STATUZ           NOT = O-K AND
                                       NOT = ENDP
              MOVE CHDRAGT-PARAMS      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR

           END-IF.

           IF CHDRAGT-STATUZ               = ENDP
              OR NOT (CHDRAGT-VALIDFLAG    = '1' OR '3')
              MOVE 'Y'                 TO WSAA-END-FILE
              GO TO  1190-EXIT

           END-IF.

           MOVE CHDRAGT-STATCODE       TO SVZZ5-ACCTYPE.
           MOVE CHDRAGT-SINSTAMT06     TO SVZZ5-BNFTAMT.
           MOVE CHDRAGT-COWNNUM        TO SVZZ5-COWNNUM.

           IF CHDRAGT-VALIDFLAG        = '1'

              PERFORM 1200-READ-COVR

           ELSE

              PERFORM 1300-READ-COVT

           END-IF.

           MOVE CHDRAGT-OCCDATE        TO SVZZ5-ISSDATE.
           MOVE CHDRAGT-CHDRNUM        TO SVZZ5-POLICNO.

           MOVE SADD                   TO SCRN-FUNCTION.

           CALL 'SVZZ5IO'              USING SCRN-SCREEN-PARAMS
                                             SVZZ5-DATA-AREA
                                             SVZZ5-SUBFILE-AREA.
           IF  SCRN-STATUZ             NOT = O-K
               MOVE SCRN-STATUZ        TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR
           END-IF.


       1190-EXIT.
           EXIT.
      *
       1200-READ-COVR SECTION.
      ************************


       1210-START.

           MOVE WSSP-COMPANY           TO COVR-CHDRCOY.
           MOVE CHDRAGT-CHDRNUM        TO COVR-CHDRNUM.
           MOVE '01'                   TO COVR-LIFE.
           MOVE '01'                   TO COVR-COVERAGE.
           MOVE '00'                   TO COVR-RIDER.
           MOVE '0'                    TO COVR-PLAN-SUFFIX.
           MOVE 'READR'                TO COVR-FUNCTION.
           CALL 'COVRIO'               USING COVR-PARAMS.

           IF COVR-STATUZ              NOT = O-K AND MRNF

              MOVE COVR-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR

           END-IF.

           IF COVR-STATUZ              = MRNF

              MOVE '0'                 TO SVZZ5-DORGSI
              GO TO 1290-EXIT

           END-IF.
           MOVE COVR-SUMINS            TO SVZZ5-DORGSI.



       1290-EXIT.
           EXIT.

      *
      /

       1300-READ-COVT SECTION.
      ************************
      *

       1310-START.

           MOVE WSSP-COMPANY           TO COVT-CHDRCOY.
           MOVE CHDRAGT-CHDRNUM        TO COVT-CHDRNUM.
           MOVE '01'                   TO COVT-LIFE.
           MOVE '01'                   TO COVT-COVERAGE.
           MOVE '00'                   TO COVT-RIDER.
           MOVE '0'                    TO COVT-PLAN-SUFFIX.
           MOVE 'READR'                TO COVT-FUNCTION.
           CALL 'COVTIO'               USING COVT-PARAMS.

           IF COVT-STATUZ              NOT = O-K AND MRNF

              MOVE COVT-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR

           END-IF.

           IF COVT-STATUZ              = MRNF

              MOVE '0'                 TO SVZZ5-DORGSI
              GO TO 1390-EXIT

           END-IF.
           MOVE COVT-SUMINS            TO SVZZ5-DORGSI.



       1390-EXIT.
           EXIT.
      *

      /
       PRE-SCREEN-EDIT SECTION.
      *************************
      *
       PRE-START.
      *---------------------------------------------------------------*
      *    This section will handle any action required on the screen *
      *    before the screen is painted.                              *
      *---------------------------------------------------------------*
            GO TO PRE-EXIT.
      *
       PRE-EXIT.
           EXIT.
      /
      *****************************************************************
      *     Retrieve Screen fields and Edit
      *****************************************************************
      *
       2000-SCREEN-EDIT SECTION.
      **************************
      *
       2010-SCREEN-IO.
      *
      *  Perform Screen Validation
      *

      *
       2010-VALIDATE-SCREEN.
      *


      *
      *    Validate fields
      *



      *
       2050-CHECK-FOR-ERRORS.
      *
           IF  SVZZ5-ERROR-INDICATORS
                                    NOT = SPACES
               MOVE 'Y'                TO WSSP-EDTERROR
           END-IF.
      *
       2060-VALIDATE-SUBFILE.
      *
           MOVE SRNCH                  TO SCRN-FUNCTION.

           CALL 'SVZZ5IO'           USING SCRN-SCREEN-PARAMS
                                          SVZZ5-DATA-AREA
                                          SVZZ5-SUBFILE-AREA.

           IF  SCRN-STATUZ          NOT = O-K
           AND SCRN-STATUZ          NOT = ENDP
               MOVE SCRN-STATUZ        TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR
           END-IF.

           PERFORM 2600-VALIDATE-SUBFILE
                      UNTIL SCRN-STATUZ = ENDP.
      *
       2090-EXIT.
            EXIT.
      /
      *****************************************************************

      *
      *    Sections performed from the 2000 section above.
      *          (Screen validation)
      *

      /
       2600-VALIDATE-SUBFILE SECTION.
      *******************************
      *
       2610-VALIDATION.
      *



      *
      *    Validate subfile fields
      *
           IF SVZZ5-SLT             NOT = SPACE AND
              SVZZ5-SLT             NOT = 1

              MOVE E005             TO SVZZ5-SLT-ERR
              MOVE 'Y'              TO WSSP-EDTERROR

           END-IF.        


      *
       2670-UPDATE-ERROR-INDICATORS.
      *
           IF  SVZZ5-ERROR-SUBFILE  NOT = SPACES
               MOVE 'Y'                TO WSSP-EDTERROR
           END-IF.

           MOVE SUPD                   TO SCRN-FUNCTION.

           CALL 'SVZZ5IO'           USING SCRN-SCREEN-PARAMS
                                          SVZZ5-DATA-AREA
                                          SVZZ5-SUBFILE-AREA.
           IF  SCRN-STATUZ          NOT = O-K
               MOVE SCRN-STATUZ        TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR
           END-IF.
      *
       2680-READ-NEXT-MODIFIED-RECORD.
      *
           MOVE SRNCH                  TO SCRN-FUNCTION.

           CALL 'SVZZ5IO'           USING SCRN-SCREEN-PARAMS
                                          SVZZ5-DATA-AREA
                                          SVZZ5-SUBFILE-AREA.

           IF  SCRN-STATUZ          NOT = O-K
           AND SCRN-STATUZ          NOT = ENDP
               MOVE SCRN-STATUZ        TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF SCRN-STATUZ          NOT = ENDP

              GO TO  2610-VALIDATION.
      *
       2690-EXIT.
            EXIT.
      /
      *****************************************************************

      *
      *    Sections performed from the 2600 section above.
      *

      /
      *****************************************************************
      *     Update Database if required and Log Transaction
      *****************************************************************
      *
       3000-UPDATE SECTION.
      *********************
      *
       3010-UPDATE-DATABASE.
      *
           IF SVZZ5-SLT           = '1'

              MOVE SPACE                  TO S2473-SLT.
              MOVE SUPD                   TO SCRN-FUNCTION. 
              CALL 'S2473IO'              USING SCRN-SCREEN-PARAMS
                                             S2473-DATA-AREA
                                             S2473-SUBFILE-AREA.
              IF SCRN-STATUZ              NOT = O-K
                                          AND NOT = ENDP
                 MOVE SCRN-STATUZ         TO SYSR-STATUZ
                 PERFORM 600-FATAL-ERROR
           
             END-IF.


           END-IF.


      *
      *  Update database files as required
      *

      *
       3090-EXIT.
            EXIT.

      /

      *
      *    Sections performed from the 3000 section above.
      *

      /
      *****************************************************************
      *     Decise which Transaction Program is Next
      *****************************************************************
      *
       4000-WHERE-NEXT SECTION.
      *************************
      *
       4010-NEXT-PROGRAM.
      *
      *  Set Next Program
      *
           ADD 1                       TO WSSP-PROGRAM-PTR.
      *
      *
       4090-EXIT.
            EXIT.
