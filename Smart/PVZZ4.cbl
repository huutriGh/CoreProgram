      * Generation Parameters - SCRVER(02)            Do Not Delete|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PVZZ4.
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
      * 26/11/20  01/01   DUMMY        IT-Nguyen Huu Tri                    *
      *           CRU AGENT POLICY INFORMATION                              *
      *                                                                     *
      **DD/MM/YY*************************************************************
      /
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WSAA-PROG                   PIC X(05) VALUE 'PVZZ4'.
       01  WSAA-VERSION                PIC X(02) VALUE '01'.
      *
       01  ERRORS.
           03  E070                    PIC X(04) VALUE 'E070'.
           03  E073                    PIC X(04) VALUE 'E073'.
           03  E132                    PIC X(04) VALUE 'E132'.
           03  F692                    PIC X(04) VALUE 'F692'.
           03  E305                    PIC X(04) VALUE 'E305'.

      *
       01  TABLES.
           03  TNNNN                   PIC X(05) VALUE 'TNNNN'.
      *
       01  FORMATS.
           03  XXXXREC                 PIC X(10) VALUE 'XXXXREC'.
           03  AGNTLAGREC              PIC X(10) VALUE 'AGNTLAGREC'.
      *
       01  WSAA-BATCHKEY.
           COPY BATCKEY.
      /
           COPY VARCOM.
      *
           COPY SYSERRREC.
      *
           COPY OPSTATSREC.
      *
           COPY SANCTNREC.
      *
           COPY SUBPROGREC.
      *
           COPY BCBPROGREC.
      *
           COPY BATCDORREC.
      *
           COPY AGNTLAGSKM.

      /
       LINKAGE SECTION.

           COPY WSSPCOMN.

       01  WSSP-USER-AREA              PIC X(768).

           COPY SCRNPARAMS.

           COPY SVZZ4SCR.
      /
       PROCEDURE DIVISION           USING WSSP-COMMON-AREA
                                          WSSP-USER-AREA
                                          SCRN-SCREEN-PARAMS
                                          SVZZ4-DATA-AREA.

           COPY MAING.
      /
      *****************************************************************
      *      Initialise fields for Showing on Screen
      *****************************************************************
      *
       1000-INITIALISE SECTION.
      *************************
      *
       1010-INITIALISE.
      *
      *  Initialise Fields
      *
           MOVE SPACES                 TO SVZZ4-DATA-AREA.
           MOVE WSSP-SBMACTION         TO SVZZ4-ACTION.
           MOVE WSSP-BATCHKEY          TO WSKY-BATC-KEY.

           IF  WSKY-BATC-BATCACTMN  NOT = WSSP-ACCTMONTH
           OR  WSKY-BATC-BATCACTYR  NOT = WSSP-ACCTYEAR
               MOVE E070               TO SCRN-ERROR-CODE
           END-IF.

           MOVE RLSE                   TO AGNTLAG-FUNCTION.
           CALL 'AGNTLAGIO'            USING AGNTLAG-PARAMS.
           IF AGNTLAG-STATUZ           NOT = O-K

              MOVE AGNTLAG-PARAMS      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR

           END-IF.

******
      *
       1090-EXIT.
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
      *     Retrieve Screen Fields and Edit
      *****************************************************************
      *
       2000-SCREEN-EDIT SECTION.
      **************************
      *
       2010-VALIDATE.
      *
      *  Validate the Screen
      *
           PERFORM 2100-VALIDATE-ACTION.

           IF  SVZZ4-ACTION-ERR         = SPACES
               IF  SCRN-STATUZ      NOT = 'BACH'
                   PERFORM 2200-VALIDATE-KEYS
               ELSE
                   PERFORM 2900-VERIFY-BATCH-CONTROL
               END-IF
           END-IF.
      *
       2080-CHECK-FOR-ERRORS.
      *
           IF  SVZZ4-ERROR-INDICATORS
                                    NOT = SPACES
               MOVE 'Y'                TO WSSP-EDTERROR
           END-IF.
      *
       2090-EXIT.
            EXIT.
      /
       2100-VALIDATE-ACTION SECTION.
      ******************************
      *
       2110-CHECK-AGAINST-TABLE.
      *
      *  Validate Selected Action
      *
           MOVE SCRN-ACTION            TO SUBP-ACTION.
           MOVE WSSP-COMPANY           TO SUBP-COMPANY.
           MOVE WSAA-PROG              TO SUBP-PROG-CODE.

           CALL 'SUBPROG'           USING SUBP-SUBPROG-REC.

           IF  SUBP-STATUZ          NOT = O-K
               MOVE SUBP-STATUZ        TO SVZZ4-ACTION-ERR
               GO TO 2190-EXIT
           END-IF.
      *
       2120-CHECK-SANCTIONS.
      *
           MOVE 'SUBM'                 TO SNCT-FUNCTION.
           MOVE WSSP-USERID            TO SNCT-USERID.
           MOVE WSSP-COMPANY           TO SNCT-COMPANY.
           MOVE WSSP-BRANCH            TO SNCT-BRANCH.
           MOVE SUBP-TRANSCD           TO SNCT-TRANSCD.

           CALL 'SANCTN'            USING WSSP-COMMON-AREA
                                          SNCT-SANCTN-REC.

           IF  SNCT-STATUZ              = BOMB
               MOVE SNCT-SANCTN-REC    TO SYSR-PARAMS
               MOVE '2'                TO SYSR-SYSERR-TYPE
               MOVE SNCT-STATUZ        TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF  SNCT-STATUZ          NOT = O-K
               MOVE SNCT-STATUZ        TO SVZZ4-ACTION-ERR
           END-IF.
      *
       2190-EXIT.
            EXIT.
      /
       2200-VALIDATE-KEYS SECTION.
      ****************************
      *
       2210-VALIDATE-KEY1.
      *

      *---------------------------------------------------------------*
      *    Validate fields                                            *
      *---------------------------------------------------------------*

           IF SUBP-KEY1                = SPACES
           OR SUBP-KEY1                = 'N'

              GO TO 2290-EXIT

           END-IF.


           IF   SVZZ4-AGENTNO          NOT     = SPACES

                MOVE WSSP-COMPANY      TO AGNTLAG-AGNTCOY
                MOVE SVZZ4-AGENTNO     TO AGNTLAG-AGNTNUM
                MOVE READR             TO AGNTLAG-FUNCTION
                CALL 'AGNTLAGIO'       USING     AGNTLAG-PARAMS

                IF AGNTLAG-STATUZ      NOT     = O-K
                                       AND NOT = MRNF

                   MOVE AGNTLAG-PARAMS TO SYSR-PARAMS
                   PERFORM 600-FATAL-ERROR


                END-IF

                IF AGNTLAG-STATUZ               = MRNF
                AND SUBP-KEY1                   = 'Y'

                   MOVE E305           TO       SVZZ4-AGENTNO-ERR
                   GO TO 2290-EXIT

                END-IF
           ELSE
      *         IF SUBP-KEY1                    = 'Y'
      *         AND (SCRN-ACTION                = 'A' OR 'D' OR 'C')

                   MOVE F692           TO       SVZZ4-AGENTNO-ERR
      *            GO TO 2290-EXIT

      *         END-IF
           END-IF.


      *
       2290-EXIT.
            EXIT.
      /
      
       2900-VERIFY-BATCH-CONTROL SECTION.
      ***********************************
      *
       2910-VALIDATE-REQUEST.
      *
      *  Validate Request for Batch Control
      *
           IF  SUBP-BCHRQD          NOT = 'Y'
               MOVE E073               TO SVZZ4-ACTION-ERR
               GO TO 2990-EXIT
           END-IF.
      *
       2920-RETRIEVE-BATCH-PROGS.
      *
           MOVE SUBP-TRANSCD           TO BCBP-TRANSCD.
           MOVE WSSP-COMPANY           TO BCBP-COMPANY.

           CALL 'BCBPROG'           USING BCBP-BCBPROG-REC.

           IF  BCBP-STATUZ          NOT = O-K
               MOVE BCBP-STATUZ        TO SVZZ4-ACTION-ERR
               GO TO 2990-EXIT
           END-IF.

           MOVE BCBP-NXTPROG1          TO WSSP-NEXT1PROG.
           MOVE BCBP-NXTPROG2          TO WSSP-NEXT2PROG.
           MOVE BCBP-NXTPROG3          TO WSSP-NEXT3PROG.
           MOVE BCBP-NXTPROG4          TO WSSP-NEXT4PROG.
      *
       2990-EXIT.
            EXIT.
      /
      *****************************************************************
      *     Update Database if required and Log Transaction
      *****************************************************************
      *
       3000-UPDATE SECTION.
      *********************
      *
       3010-UPDATE-WSSP.
      *
      *  Perform Updates
      *
           MOVE SCRN-ACTION            TO WSSP-SBMACTION.
           MOVE WSSP-BATCHKEY          TO WSAA-BATCHKEY.
           MOVE SUBP-TRANSCD           TO WSKY-BATC-BATCTRCDE.
           MOVE WSAA-BATCHKEY          TO WSSP-BATCHKEY.
           MOVE WSAA-PROG              TO WSSP-SUBMENU.

           IF  SCRN-STATUZ              = 'BACH'
               GO TO 3090-EXIT
           END-IF.

           MOVE SUBP-NXT1PROG          TO WSSP-SEC-PROG (1).
           MOVE SUBP-NXT2PROG          TO WSSP-SEC-PROG (2).
           MOVE SUBP-NXT3PROG          TO WSSP-SEC-PROG (3).
           MOVE SUBP-NXT4PROG          TO WSSP-SEC-PROG (4).



      *---------------------------------------------------------------*
      *  Update WSSP Key details                                      *
      *---------------------------------------------------------------*


           EVALUATE SCRN-ACTION


              WHEN 'A'
              WHEN 'D'

                   MOVE 'I'            TO WSSP-FLAG

              WHEN 'B'

                   MOVE 'A'            TO WSSP-FLAG

              WHEN 'C'

                   MOVE 'M'            TO WSSP-FLAG

           END-EVALUATE.

           IF SVZZ4-AGENTNO               = SPACES
           AND SUBP-KEY1                  = 'N'

              PERFORM 3200-ALLOCATE-NUMBER

           END-IF.

           IF SVZZ4-ERROR-INDICATORS   NOT = SPACES

              GO TO 3080-BATCHING

           END-IF.

           IF WSSP-FLAG = 'I'

              GO TO 3020-PASS-AGENT-DETAILS

           END-IF.

      *
       3020-PASS-AGENT-DETAILS.
      *

           IF SCRN-ACTION              NOT = 'B'

              MOVE 'KEEPS'             TO     AGNTLAG-FUNCTION
              MOVE AGNTLAGREC          TO     AGNTLAG-FORMAT

              CALL 'AGNTLAGIO'         USING  AGNTLAG-PARAMS

              IF AGNTLAG-STATUZ        NOT =  O-K AND MRNF

                 MOVE AGNTLAG-PARAMS   TO     SYSR-PARAMS
                 PERFORM 600-FATAL-ERROR

              END-IF

           END-IF.

      *
       3080-BATCHING.
      *
           IF  SUBP-BCHRQD              = 'Y'
           AND SVZZ4-ERROR-INDICATORS   = SPACES
               PERFORM 3100-UPDATE-BATCH-CONTROL
           END-IF.

           IF  SVZZ4-ERROR-INDICATORS
                                    NOT = SPACES
               MOVE 'Y'                TO WSSP-EDTERROR
               ROLLBACK
           END-IF.
      *
       3090-EXIT.
            EXIT.
      /
       3100-UPDATE-BATCH-CONTROL SECTION.
      ***********************************
      *
       3110-AUTOMATIC-BATCHING.
      *
      * Set up Batching
      *
           MOVE 'AUTO'                 TO    BATD-FUNCTION.
           MOVE WSSP-TRANID            TO    BATD-TRANID.
           MOVE WSSP-BATCHKEY          TO    BATD-BATCHKEY.

           CALL 'BATCDOR'              USING BATD-BATCDOR-REC.

           IF  BATD-STATUZ             NOT = O-K
               MOVE BATD-STATUZ        TO    SVZZ4-ACTION-ERR
           END-IF.

           MOVE BATD-BATCHKEY          TO WSSP-BATCHKEY.
      *
       3190-EXIT.
            EXIT.
      /
      *
       3200-ALLOCATE-NUMBER SECTION.
      *****************************
       3210-START.



       3290-EXIT.
           EXIT.
      /
      *****************************************************************
      *     Decide which Transaction Program is Next
      *****************************************************************
      *
       4000-WHERE-NEXT SECTION.
      *************************
      *
       4010-NEXT-PROGRAM.
      *
      *  Set Next Program
      *
           IF  SCRN-STATUZ          NOT = 'BACH'
               MOVE 1                  TO WSSP-PROGRAM-PTR
           ELSE
              MOVE 0                   TO WSSP-PROGRAM-PTR
              MOVE WSSP-NEXT1PROG      TO WSSP-NEXTPROG
           END-IF.
      *
      *
       4090-EXIT.
            EXIT.
