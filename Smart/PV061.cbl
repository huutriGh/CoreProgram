      * Generation Parameters - SCRVER(02)            Do Not Delete|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PV061.
      *
      *(C) Copyright CSC Corporation Limited 1986 - 2000.
      *    All rights reserved. CSC Confidential.
      *
      *REMARKS.
      *
      * The remarks section should detail the main processes of the
      * program, and any special functions.
      *
      ***********************************************************************
      *           AMENDMENT  HISTORY                                        *
      ***********************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....                               *
      *                                                                     *
      * 20/05/14  01/01   PHLRMS       Phuong Le Dev                        *
      *           Payment Request Information                               *
      *                                                                     *
      **DD/MM/YY*************************************************************
      /
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       01  WSAA-PROG                   PIC X(05) VALUE 'PV061'.
       01  WSAA-VERSION                PIC X(02) VALUE '01'.
      *
       01  ERRORS.
           03  E005                    PIC X(04) VALUE 'E005'.
           03  H093                    PIC X(04) VALUE 'H093'.
           03  G615                    PIC X(04) VALUE 'G615'.
      *
       01  TABLES.
           03  TNNNN                   PIC X(05) VALUE 'TNNNN'.
      *
       01  FORMATS.
           03  RMPRCHKREC              PIC X(10) VALUE 'RMPRCHKREC'.
           03  RMPRINFREC              PIC X(10) VALUE 'RMPRINFREC'.
           03  STIHINFREC              PIC X(10) VALUE 'STIHINFREC'.

       01  FILLER.
           03  WSAA-SEC-PROG           PIC X(05) OCCURS 8.

       01  WSAA-BATCKEY.
           COPY BATCKEY.
      *
       01  WSAA-TOTAL-STOCK-IN         PIC S9(10).
       01  WSAA-TOTAL-ORDER            PIC S9(10).
       01  WSAA-X                      PIC S9(03) COMP-3 VALUE 0.
       01  WSAA-Y                      PIC S9(03) COMP-3 VALUE 0.
       01  WSAA-SUBF-RRN               PIC S9(02) VALUE ZEROES.
      /
           COPY VARCOM.
           COPY GENSSWREC.
      ***  COPY OPTSWCHREC.
      *
           COPY SYSERRREC.
      *
           COPY OPSTATSREC.
           COPY RMPRCHKSKM.
           COPY RMPRINFSKM.
           COPY STIHINFSKM.
      /
       LINKAGE SECTION.

           COPY WSSPCOMN.

      *01  WSSP-USER-AREA              PIC X(768).
           COPY WSSPWINDOW.
           COPY SCRNPARAMS.

           COPY SV061SCR.
      /
       PROCEDURE DIVISION USING WSSP-COMMON-AREA WSSP-USER-AREA
                                SCRN-SCREEN-PARAMS SV061-DATA-AREA
                                SV061-SUBFILE-AREA.

           COPY MAING.
      /
      *****************************************************************
      *      INITIALISE FIELDS FOR SHOWING ON SCREEN
      *****************************************************************
      *
       1000-INITIALISE SECTION.
      *************************
      *
       1010-INITIALISE.

           MOVE SPACES                 TO SV061-DATA-AREA.
           MOVE SPACES                 TO SV061-SUBFILE-AREA.
           MOVE WSSP-BATCHKEY          TO WSAA-BATCKEY.
           MOVE SPACES                 TO WSSP-VALUE.

           IF  WSSP-PROGRAM-PTR        = 0
               ADD 1                   TO WSSP-PROGRAM-PTR
           END-IF.

           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'
              GO TO 1090-EXIT
           END-IF.

      * Dummy subfile initalisation for prototype - relpace with SCLR
           MOVE SCLR                   TO SCRN-FUNCTION.

           CALL 'SV061IO' USING SCRN-SCREEN-PARAMS
                                SV061-DATA-AREA
                                SV061-SUBFILE-AREA.

           IF SCRN-STATUZ NOT = O-K
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE 1                      TO SCRN-SUBFILE-RRN.

      *    Dummy field initilisation for prototype version.
******
      *
      *    Set screen fields
      *
           MOVE VRCM-MAX-DATE          TO SV061-REQDATE    .
           MOVE SPACES                 TO SV061-PAYMRQNO   .
           MOVE ZEROES                 TO SV061-RCSERNUM-01.
           MOVE ZEROES                 TO SV061-RCSERNUM-02.
           MOVE SPACES                 TO SV061-RCTYPE     .
           MOVE SPACES                 TO SV061-VIEWPLAN   .
           MOVE SPACES                 TO SV061-STFLAG-01  .
           MOVE SPACES                 TO SV061-STFLAG-02  .
           MOVE SPACES                 TO SV061-STOCKDEC   .
           MOVE SPACES                 TO SV061-XOPT       .

           PERFORM  1200-LOAD-SUBFILE.
           MOVE 1                      TO SCRN-SUBFILE-RRN.
      *
       1090-EXIT.
           EXIT.
      /
       1200-LOAD-SUBFILE SECTION.
      ***************************
      *
       1210-INIT.
      *
           INITIALIZE                     RMPRINF-PARAMS.
           MOVE VRCM-MAX-DATE          TO RMPRINF-REQDATE.
           IF  WSSP-FLAG               = 'I'
               MOVE WSSP-UND-FUNCTION  TO RMPRINF-RCTYPE
           ELSE
               MOVE WSSP-DEFINITM      TO RMPRINF-RCTYPE
           END-IF.

           MOVE WSSP-CLNTKEY(1:10)     TO RMPRINF-PAYMRQNO.
           MOVE RMPRINFREC             TO RMPRINF-FORMAT.
           MOVE BEGN                   TO RMPRINF-FUNCTION.
      *
       1220-START.
      *
           CALL 'RMPRINFIO'            USING RMPRINF-PARAMS.

           IF  RMPRINF-STATUZ          NOT = O-K
           AND                         NOT = ENDP
               MOVE RMPRINF-STATUZ     TO SYSR-STATUZ
               MOVE RMPRINF-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF  RMPRINF-STATUZ          = ENDP
           OR (RMPRINF-RCTYPE          NOT = WSSP-UND-FUNCTION
           AND WSSP-UND-FUNCTION       NOT = SPACES)
           OR (RMPRINF-RCTYPE          NOT = WSSP-DEFINITM
           AND WSSP-DEFINITM           NOT = SPACES)
           OR (WSSP-CLNTKEY(1:10)      NOT = SPACES
           AND RMPRINF-PAYMRQNO        NOT = WSSP-CLNTKEY(1:10))
               GO TO 1290-EXIT
           END-IF.

           PERFORM 1300-ADD-SCREEN.
      *
       1280-NEXT.
      *
           MOVE NEXTR                  TO RMPRINF-FUNCTION.
           GO TO 1220-START.
      *
       1290-EXIT.
           EXIT.
      /
       1300-ADD-SCREEN SECTION.
      *************************
      *
       1310-START.
      *
           MOVE RMPRINF-REQDATE        TO SV061-REQDATE    .
           MOVE RMPRINF-PAYMRQNO       TO SV061-PAYMRQNO   .
           MOVE RMPRINF-RCSERNUM01     TO SV061-RCSERNUM-01.
           MOVE RMPRINF-RCSERNUM02     TO SV061-RCSERNUM-02.
           MOVE RMPRINF-RCTYPE         TO SV061-RCTYPE     .
           MOVE SPACES                 TO SV061-VIEWPLAN   .

           PERFORM 1400-GET-TOTAL-STCK-IN.

           IF  WSAA-TOTAL-STOCK-IN     = 0
               MOVE 'N'                TO SV061-STFLAG-01
           ELSE
               MOVE 'Y'                TO SV061-STFLAG-01
           END-IF.

           COMPUTE WSAA-TOTAL-ORDER    = RMPRINF-RCSERNUM02
                                       - RMPRINF-RCSERNUM01 + 1.

           IF  WSAA-TOTAL-STOCK-IN     = WSAA-TOTAL-ORDER
               MOVE 'N'                TO SV061-STFLAG-02
           ELSE
               MOVE 'Y'                TO SV061-STFLAG-02
           END-IF.

           MOVE SPACES                 TO SV061-STOCKDEC   .
           MOVE SPACES                 TO SV061-XOPT       .
      *
       1320-ADD-TO-SCREEN.
      *
           MOVE SADD                   TO SCRN-FUNCTION.

           CALL 'SV061IO'              USING SCRN-SCREEN-PARAMS
                                             SV061-DATA-AREA
                                             SV061-SUBFILE-AREA.

           IF  SCRN-STATUZ             NOT = O-K
           AND                         NOT = ENDP
               MOVE SCRN-STATUZ        TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR
           END-IF.
      *
       1390-EXIT.
           EXIT.
      /
       1400-GET-TOTAL-STCK-IN SECTION.
      ********************************
      *
       1410-INIT.
      *
           MOVE ZEROS                  TO WSAA-TOTAL-STOCK-IN.
           INITIALIZE                     STIHINF-PARAMS.
           MOVE RMPRINF-PAYMRQNO       TO STIHINF-PAYMRQNO.
           MOVE RMPRINF-RCTYPE         TO STIHINF-RCTYPE.
           MOVE STIHINFREC             TO STIHINF-FORMAT.
           MOVE BEGN                   TO STIHINF-FUNCTION.
      *
       1420-START.
      *
           CALL 'STIHINFIO'            USING STIHINF-PARAMS.

           IF  STIHINF-STATUZ          NOT = O-K
           AND                         NOT = ENDP
               MOVE STIHINF-STATUZ     TO SYSR-STATUZ
               MOVE STIHINF-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF  STIHINF-STATUZ          = ENDP
           OR  STIHINF-PAYMRQNO        NOT = RMPRINF-PAYMRQNO
           OR  STIHINF-RCTYPE          NOT = RMPRINF-RCTYPE
               GO TO 1490-EXIT
           END-IF.

           ADD STIHINF-TOTSTI          TO WSAA-TOTAL-STOCK-IN.
      *
       1480-NEXT.
      *
           MOVE NEXTR                  TO STIHINF-FUNCTION.
           GO TO 1420-START.
      *
       1490-EXIT.
           EXIT.
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
      *     RETRIEVE SCREEN FIELDS AND EDIT
      *****************************************************************
      *
       2000-SCREEN-EDIT SECTION.
      **************************
      *
       2010-SCREEN-IO.
      *
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'
              GO TO 2090-EXIT
           END-IF.

      *
       2010-VALIDATE-SCREEN.
      *


      *
      *    Validate fields
      *



      *
       2050-CHECK-FOR-ERRORS.
      *
           IF SV061-ERROR-INDICATORS   NOT = SPACES
              MOVE 'Y'                 TO WSSP-EDTERROR
           END-IF.
      *
       2060-VALIDATE-SUBFILE.
      *
           MOVE SRNCH                  TO SCRN-FUNCTION.

           CALL 'SV061IO' USING SCRN-SCREEN-PARAMS
                                SV061-DATA-AREA
                                SV061-SUBFILE-AREA.

           IF SCRN-STATUZ              NOT = O-K
                                   AND NOT = ENDP
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.

           PERFORM 2600-VALIDATE-SUBFILE
                                       UNTIL SCRN-STATUZ = ENDP.

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
      *    Validate subfile fields
      *
       2670-UPDATE-ERROR-INDICATORS.
      *
           IF SV061-ERROR-SUBFILE      NOT = SPACES
              MOVE 'Y'                 TO WSSP-EDTERROR
           END-IF.

           MOVE SUPD                   TO SCRN-FUNCTION.

           CALL 'SV061IO' USING SCRN-SCREEN-PARAMS
                                SV061-DATA-AREA
                                SV061-SUBFILE-AREA.
           IF SCRN-STATUZ              NOT = O-K
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.
      *
       2680-READ-NEXT-MODIFIED-RECORD.
      *
           MOVE SRNCH                  TO SCRN-FUNCTION.

           CALL 'SV061IO' USING SCRN-SCREEN-PARAMS
                                SV061-DATA-AREA
                                SV061-SUBFILE-AREA.

           IF SCRN-STATUZ              NOT = O-K
                                   AND NOT = ENDP
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.
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
      *     UPDATE DATABASE IF REQUIRED AND LOG TRANSACTION
      *****************************************************************
      *
       3000-UPDATE SECTION.
      **********************
      *
       3010-UPDATE-DATABASE.

      *
      *  Update database files as required
      *
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'
              GO TO 3090-EXIT
           END-IF.

       3090-EXIT.
            EXIT.

      /
      *
      *    Sections performed from the 3000 section above.
      *

      /
      *****************************************************************
      *     DECIDE WHICH TRANSACTION PROGRAM IS NEXT
      *****************************************************************
      *
       4000-WHERE-NEXT SECTION.
      *************************
      *
       4010-NEXT-PROGRAM.
      *
           MOVE WSAA-PROG              TO WSSP-NEXTPROG.

           MOVE WSSP-COMPANY           TO GENS-COMPANY.
           MOVE WSAA-PROG              TO GENS-PROG-IN.
           MOVE WSKY-BATC-BATCTRCDE    TO GENS-TRANSACT.

           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = SPACE
              MOVE WSSP-PROGRAM-PTR    TO WSAA-X
              MOVE 0                   TO WSAA-Y
              PERFORM 4100-SAVE-PROGRAM 8 TIMES
           END-IF.

           MOVE SSTRT                  TO SCRN-FUNCTION.

           CALL 'SV061IO'              USING SCRN-SCREEN-PARAMS
                                             SV061-DATA-AREA
                                             SV061-SUBFILE-AREA.

           IF SCRN-STATUZ              NOT = O-K
           AND                         NOT = ENDP
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.

           PERFORM 4500-LOOP-SUBFILE   UNTIL SCRN-STATUZ = ENDP.
      *
       4080-NEXT.
      *
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'
              MOVE WSSP-PROGRAM-PTR    TO WSAA-X
              MOVE 0                   TO WSAA-Y
              PERFORM 4200-RESTORE-PROGRAM 8 TIMES
           END-IF.

           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'
               MOVE WSAA-SUBF-RRN    TO SCRN-SUBFILE-RRN
               MOVE SPACE            TO WSSP-SEC-ACTN(WSSP-PROGRAM-PTR)
               MOVE SCRN-SCRNAME     TO WSSP-NEXTPROG
           ELSE
               ADD 1                 TO WSSP-PROGRAM-PTR
           END-IF.
      *
       4090-EXIT.
            EXIT.
      *
       4100-SAVE-PROGRAM SECTION.
      ***************************
      *
       4110-SAVE.
      *
           ADD 1                       TO WSAA-X.
           ADD 1                       TO WSAA-Y.
           MOVE WSSP-SEC-PROG (WSAA-X) TO WSAA-SEC-PROG(WSAA-Y).
      *
       4190-EXIT.
            EXIT.
      /
      *****************************************************************
       4200-RESTORE-PROGRAM      SECTION.
      ***********************************
      *
       4210-RESTORE.
      *
           ADD 1                       TO WSAA-X.
           ADD 1                       TO WSAA-Y.
           MOVE WSAA-SEC-PROG (WSAA-Y) TO WSSP-SEC-PROG(WSAA-X).
      *
       4290-EXIT.
            EXIT.

      *****************************************************************
       4300-CALL-GENSSW SECTION.
      **************************
      *
       4310-CALL-SUBROUTINE.
      *
           CALL 'GENSSW' USING GENS-GENSSW-REC.

           IF  GENS-STATUZ             NOT = O-K
           AND                         NOT = MRNF
               MOVE GENS-STATUZ        TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR
           END-IF.

      *****
      * IF AN ENTRY ON T1675 WAS NOT FOUND BY GENSWCH REDISPLAY THE SCR
      * WITH AN ERROR AND THE OPTIONS AND EXTRAS INDICATOR
      * WITH ITS INITIAL LOAD VALUE
      *****
           IF GENS-STATUZ               = MRNF
              MOVE ' '                 TO WSSP-SEC-ACTN
                                              (WSSP-PROGRAM-PTR)
              MOVE H093                TO SCRN-ERROR-CODE
              MOVE SCRN-SCRNAME        TO WSSP-NEXTPROG
              GO TO 4390-EXIT
           END-IF.

      *****
      *    LOAD FROM GENSW TO WSSP.
      *****

           ADD 1, WSSP-PROGRAM-PTR GIVING WSAA-X
           MOVE 1                      TO WSAA-Y
           PERFORM 4400-LOAD-PROGRAM 8 TIMES.

           MOVE '*'                    TO
                                      WSSP-SEC-ACTN (WSSP-PROGRAM-PTR).
           ADD 1                       TO WSSP-PROGRAM-PTR.
      *
       4390-EXIT.
            EXIT.
      /
      *****************************************************************
       4400-LOAD-PROGRAM SECTION.
      ***************************
      *
       4410-RESTORE.
      *
           MOVE GENS-PROG-OUT (WSAA-Y) TO WSSP-SEC-PROG(WSAA-X).
           ADD 1                       TO WSAA-X.
           ADD 1                       TO WSAA-Y.
      *
       4490-EXIT.
            EXIT.
      /
       4500-LOOP-SUBFILE  SECTION.
      ****************************
      *
       4510-START.
      *
           PERFORM 4600-READ-RMPRCHK.

           MOVE KEEPS                  TO RMPRCHK-FUNCTION.

           CALL 'RMPRCHKIO'            USING RMPRCHK-PARAMS.

           IF  RMPRCHK-STATUZ          NOT = O-K
               MOVE RMPRCHK-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF  SV061-VIEWPLAN          NOT = SPACES                     <PHLRMS>
               MOVE SCRN-SUBFILE-RRN   TO WSAA-SUBF-RRN
               MOVE 'I'                TO WSSP-FLAG                     <PHLRMS>
               MOVE 'A'                TO GENS-FUNCTION                 <PHLRMS>
               PERFORM 4300-CALL-GENSSW                                 <PHLRMS>
                                                                        <PHLRMS>
               MOVE SPACES             TO SV061-VIEWPLAN                <PHLRMS>
               MOVE SUPD               TO SCRN-FUNCTION

               CALL 'SV061IO'          USING SCRN-SCREEN-PARAMS
                                             SV061-DATA-AREA
                                             SV061-SUBFILE-AREA

               IF SCRN-STATUZ          NOT = O-K AND ENDP
                  MOVE SCRN-STATUZ     TO SYSR-STATUZ
                  PERFORM 600-FATAL-ERROR
               END-IF
               GO TO 4090-EXIT                                          <PHLRMS>
           END-IF.                                                      <PHLRMS>

           IF  SV061-STOCKDEC          NOT = SPACES                     <PHLRMS>
               MOVE SCRN-SUBFILE-RRN   TO WSAA-SUBF-RRN
               MOVE 'I'                TO WSSP-FLAG                     <PHLRMS>
               MOVE 'B'                TO GENS-FUNCTION                 <PHLRMS>
               PERFORM 4300-CALL-GENSSW                                 <PHLRMS>
                                                                        <PHLRMS>
               MOVE SPACES             TO SV061-STOCKDEC                <PHLRMS>
               MOVE SUPD               TO SCRN-FUNCTION

               CALL 'SV061IO'          USING SCRN-SCREEN-PARAMS
                                             SV061-DATA-AREA
                                             SV061-SUBFILE-AREA

               IF SCRN-STATUZ          NOT = O-K AND ENDP
                  MOVE SCRN-STATUZ     TO SYSR-STATUZ
                  PERFORM 600-FATAL-ERROR
               END-IF
               GO TO 4090-EXIT                                          <PHLRMS>
           END-IF.                                                      <PHLRMS>

           IF  SV061-XOPT              NOT = SPACES                     <PHLRMS>
               MOVE SCRN-SUBFILE-RRN   TO WSAA-SUBF-RRN
               MOVE 'I'                TO WSSP-FLAG                     <PHLRMS>
               MOVE 'C'                TO GENS-FUNCTION                 <PHLRMS>
               PERFORM 4300-CALL-GENSSW                                 <PHLRMS>
                                                                        <PHLRMS>
               MOVE SPACES             TO SV061-XOPT                    <PHLRMS>
               MOVE SUPD               TO SCRN-FUNCTION

               CALL 'SV061IO'          USING SCRN-SCREEN-PARAMS
                                             SV061-DATA-AREA
                                             SV061-SUBFILE-AREA

               IF SCRN-STATUZ          NOT = O-K AND ENDP
                  MOVE SCRN-STATUZ     TO SYSR-STATUZ
                  PERFORM 600-FATAL-ERROR
               END-IF
               GO TO 4090-EXIT                                          <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *
       4580-READ-NEXT-MODIFIED-RECORD.
      *
           MOVE SRDN                   TO SCRN-FUNCTION.

           CALL 'SV061IO'              USING SCRN-SCREEN-PARAMS
                                             SV061-DATA-AREA
                                             SV061-SUBFILE-AREA.

           IF  SCRN-STATUZ             NOT = O-K
           AND                         NOT = ENDP
               MOVE SCRN-STATUZ        TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR
           END-IF.
      *
       4590-EXIT.
           EXIT.
      *                                                                 <PHLRMS>
       4600-READ-RMPRCHK SECTION.                                       <PHLRMS>
      ***************************                                       <PHLRMS>
      *
       4610-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                     RMPRCHK-PARAMS.
           MOVE SV061-PAYMRQNO         TO RMPRCHK-PAYMRQNO.
           MOVE SV061-RCTYPE           TO RMPRCHK-RCTYPE.
           MOVE RMPRCHKREC             TO RMPRCHK-FORMAT.
           MOVE READR                  TO RMPRCHK-FUNCTION.

           CALL 'RMPRCHKIO'            USING RMPRCHK-PARAMS.

           IF  RMPRCHK-STATUZ          NOT = O-K
               MOVE RMPRCHK-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.
      *
       4690-EXIT.                                                       <PHLRMS>
           EXIT.
