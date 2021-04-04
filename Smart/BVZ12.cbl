       IDENTIFICATION DIVISION.
       PROGRAM-ID. BVZ12.
      *
      * Copyright 1986-2020, Computer Sciences Corporation.
      *
      *
      ***********************************************************************
      *           AMENDMENT  HISTORY                                        *
      ***********************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....                               *
      *                                                                     *
      * 23/11/20  01/01   DUMMY        IT-Nguyen Huu Tri                    *
      *           INSERT PROCESS AND BATCH PROGRAM FOLLOW SCHEDULE          *
      *                                                                     *
      **DD/MM/YY*************************************************************
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.                                IBM-AS400.
       OBJECT-COMPUTER.                                IBM-AS400.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      /
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.
      *
       01  WSAA-PROG                   PIC X(05) VALUE 'BVZ12'.
       01  WSAA-VERSION                PIC X(02) VALUE '01'.
       01  WSAA-COMPANY                PIC X(01) VALUE '2'.
       01  WSAA-PROCS-EXIST            PIC 9(02)          .
       01  WSAA-PROCESS-NAME           PIC X(0010)        .
       01  WSAA-PROCESS-IX             PIC 9(02)          .
       01  WSAA-READ-SCH-DONE          PIC X(01) VALUE 'N'.


      *
      *  These fields are required by MAINB processing and should not
      *   be deleted.
      *
       01  WSAA-COMMIT-CNT             PIC S9(08) COMP-3.
       01  WSAA-CYCLE-CNT              PIC S9(08) COMP-3.
       01  WSAA-CNT                    PIC 9(02).
       01  WSSP-EDTERROR               PIC X(04).
      *
      ****************************************************************
      *
      * The formats BUPA BSSC BPRD BSPR and BMSG are required by MAINB
      *  processing and should not be deleted.
      *
       01  FORMATS.
           03  BMSGREC                 PIC X(10) VALUE 'BMSGREC'.
           03  BPRDREC                 PIC X(10) VALUE 'BPRDREC'.
           03  BSPRREC                 PIC X(10) VALUE 'BSPRREC'.
           03  BSSCREC                 PIC X(10) VALUE 'BSSCREC'.
           03  BUPAREC                 PIC X(10) VALUE 'BUPAREC'.
           03  DESCREC                 PIC X(10) VALUE 'DESCREC'.
           03  BSPDREC                 PIC X(10) VALUE 'BSPDREC'.
           03  BPSRCPYREC              PIC X(10) VALUE 'BPSRCPYREC'.
           03  BSPDCPYREC              PIC X(10) VALUE 'BSPDCPYREC'.
           03  ZNHTREC                 PIC X(10) VALUE 'ZNHTREC'.

      *
       01  TABLES.
           03  T1692                   PIC X(06) VALUE 'T1692'.
           03  T1693                   PIC X(06) VALUE 'T1693'.
           03  T3629                   PIC X(06) VALUE 'T3629'.
      *
       01  CONTROL-TOTALS.
           03  CT01                    PIC 9(02) VALUE 01.
      *
       01  WSAA-OVERFLOW               PIC X(01) VALUE 'Y'.
       88  NEW-PAGE-REQ                          VALUE 'Y'.
      *
       01  WSAA-EOF                    PIC X(01) VALUE 'N'.
       88  END-OF-FILE                           VALUE 'Y'.
      *
      *
      *   Main, standard page headings
      *
      *
      *  Detail line - add as many detail and total lines as required.
      *              - use redefines to save WS space where applicable.
      *
      /
           COPY BATCDORREC.
      /
           COPY BATCUPREC.
      /
           COPY BSSCSKM.
      /
           COPY BSPRSKM.
      /
           COPY BUPASKM.
      /
           COPY BPRDSKM.
      /
           COPY CONLOGREC.
      /
           COPY CONTOTREC.
      /
           COPY DATCON1REC.
      /
           COPY DESCSKM.
      /
           COPY SFTLOCKREC.
      /
           COPY SMTPFXCPY.
      /
           COPY SYSERRREC.
      /
           COPY VARCOM.
      /
           COPY BSPDSKM.
      /
           COPY BPSRCPYSKM.
      /
           COPY BSPDCPYSKM.
      /
           COPY ZNHTSKM.


      *
       LINKAGE SECTION.
      *****************
      *
        01  LSAA-STATUZ                PIC X(04).
        01  LSAA-BSSCREC               PIC X(1024).
        01  LSAA-BSPRREC               PIC X(1024).
        01  LSAA-BPRDREC               PIC X(1024).
        01  LSAA-BUPAREC               PIC X(1024).
      /
       PROCEDURE DIVISION           USING LSAA-STATUZ
                                          LSAA-BSSCREC
                                          LSAA-BSPRREC
                                          LSAA-BPRDREC
                                          LSAA-BUPAREC.
      *
           COPY MAINB.
      /
       0900-RESTART SECTION.
      **********************
      *
       0910-RESTART.
      *
      * Place any additional restart processing in here.
      *
           CONTINUE.

       0990-EXIT.
           EXIT.
      /
       1000-INITIALISE SECTION.
      *************************
      *
       1010-INITIALISE.
      *
           MOVE SPACES                   TO BSPDCPY-SCHEDULE-NAME.
           MOVE '2'                      TO BSPDCPY-COMPANY.
           MOVE SPACES                   TO BSPDCPY-PROCESS-NAME.
           MOVE BEGN                     TO BSPDCPY-FUNCTION.
           MOVE BSPDCPYREC               TO BSPDCPY-FORMAT.

      *
       1090-EXIT.
           EXIT.
      /
       2000-READ-FILE SECTION.
      ************************
      *
       2010-READ-FILE.
      *
           MOVE 0                        TO WSAA-PROCESS-IX.
           MOVE 0                        TO WSAA-PROCS-EXIST.
           MOVE 'N'                      TO WSAA-READ-SCH-DONE.
           CALL 'BSPDCPYIO'              USING   BSPDCPY-PARAMS.

           IF  BSPDCPY-STATUZ              NOT = O-K
           AND BSPDCPY-STATUZ              NOT = ENDP

              MOVE BSPDCPY-PARAMS           TO   SYSR-PARAMS
              PERFORM 600-FATAL-ERROR

           END-IF.

           IF BSPDCPY-STATUZ                   = ENDP

              MOVE ENDP                     TO   WSSP-EDTERROR
              GO TO 2090-EXIT

           ELSE

               MOVE BSPDCPY-COMPANY         TO   WSAA-COMPANY
               MOVE BSPDCPY-PROCESS-NAME    TO   WSAA-PROCESS-NAME

           END-IF.

           MOVE NEXTR                       TO   BSPDCPY-FUNCTION.

      *
       2090-EXIT.
           EXIT.
      /
       2500-EDIT SECTION.
      *******************
      *
       2510-EDIT.
      *
      *
           MOVE O-K                    TO WSSP-EDTERROR.

           IF   BSPDCPY-COMPANY        NOT = '2'

                MOVE SPACES            TO WSSP-EDTERROR
                GO TO 2590-EXIT

           END-IF.

      *
       2590-EXIT.
           EXIT.
      /


       2100-READ-FILE-BPRDPF SECTION.
      *******************************
       2110-START.

           ADD 1                         TO WSAA-PROCESS-IX.
           MOVE WSAA-COMPANY             TO BPRD-COMPANY.
           MOVE WSAA-PROCESS-NAME        TO BPRD-PROCESS-NAME.
           MOVE BPRDREC                  TO BPRD-FORMAT.
           MOVE READR                    TO BPRD-FUNCTION.
           CALL 'BPRDIO'              USING BPRD-PARAMS.

           IF BPRD-STATUZ             NOT = O-K
           AND BPRD-STATUZ            NOT = MRNF

               MOVE BPRD-PARAMS          TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR

           END-IF.

           IF BPRD-STATUZ                 = MRNF

              MOVE 'Y'                   TO WSAA-READ-SCH-DONE
              GO TO  2150-EXIT

           ELSE

              ADD  1                   TO WSAA-PROCS-EXIST
              MOVE BPRD-PROCESS-NAME   TO ZNHT-TBACPRO(WSAA-PROCESS-IX)
              MOVE BPRD-BATCH-PROGRAM  TO ZNHT-TPROGRM(WSAA-PROCESS-IX)
              PERFORM 2200-READ-FILE-BPSRPF


           END-IF.

       2150-EXIT.
           EXIT.
      /
       2200-READ-FILE-BPSRPF SECTION.
      *******************************
       2210-START.

           MOVE BPRD-COMPANY             TO BPSRCPY-PRIOR-COMPANY.
           MOVE BPRD-PROCESS-NAME        TO BPSRCPY-PRIOR-PROCESS.
           MOVE BPRD-COMPANY             TO BPSRCPY-SUBSEQUENT-COMPANY.
           MOVE SPACES                   TO BPSRCPY-SUBSEQUENT-PROCESS.


           MOVE BPSRCPYREC               TO BPSRCPY-FORMAT.
           MOVE BEGN                     TO BPSRCPY-FUNCTION.
           CALL 'BPSRCPYIO'           USING BPSRCPY-PARAMS.

           IF  BPSRCPY-STATUZ         NOT = O-K
           AND BPSRCPY-STATUZ         NOT = ENDP

               MOVE BPSRCPY-PARAMS       TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR

           END-IF.
           IF  BPRD-COMPANY           NOT = BPSRCPY-PRIOR-COMPANY
           OR  BPRD-PROCESS-NAME      NOT = BPSRCPY-PRIOR-PROCESS
           OR  BPRD-COMPANY           NOT = BPSRCPY-SUBSEQUENT-COMPANY

               MOVE 'Y'                  TO WSAA-READ-SCH-DONE
               GO TO 2250-EXIT

           END-IF.

           IF BPSRCPY-STATUZ              = ENDP
              OR WSAA-PROCESS-IX          > 80

              MOVE 'Y'                    TO     WSAA-READ-SCH-DONE
              GO TO  2250-EXIT

           ELSE

              MOVE BPSRCPY-SUBSEQUENT-PROCESS    TO WSAA-PROCESS-NAME
              MOVE BPSRCPY-SUBSEQUENT-COMPANY    TO WSAA-COMPANY

           END-IF.

       2250-EXIT.
           EXIT.
      /
       2300-ASIGN-VALUE-TO-ZNHT SECTION.
      **********************************
       2310-START.


           PERFORM   2100-READ-FILE-BPRDPF
                               UNTIL   WSAA-READ-SCH-DONE = 'Y'.


       2350-EXIT.
           EXIT.

      /

       3000-UPDATE SECTION.
      *********************
      *
       3010-UPDATE.
      *

           INITIALIZE  ZNHT-PARAMS.
           MOVE BSPDCPY-COMPANY                 TO ZNHT-TCOMPAPY.
           MOVE BSPDCPY-SCHEDULE-NAME           TO ZNHT-TSCHNME.
           IF ZNHT-TCOMPAPY                  NOT = '2'
              GO TO 3090-EXIT
           END-IF.
           PERFORM 2300-ASIGN-VALUE-TO-ZNHT.
           IF WSAA-PROCS-EXIST               < 1

              GO TO 3090-EXIT

           END-IF.
           MOVE WRITR                  TO ZNHT-FUNCTION.
           MOVE ZNHTREC                TO ZNHT-FORMAT.
           CALL 'ZNHTIO'            USING ZNHT-PARAMS.

           IF ZNHT-STATUZ             NOT = O-K

              MOVE ZNHT-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR

           END-IF.




      *
       3090-EXIT.
           EXIT.
      /
       3500-COMMIT SECTION.
      *********************
      *
       3510-COMMIT.
      *
      * Place any additional commitment processing in here.
      *
      *    CONTINUE.

       3590-EXIT.
           EXIT.
      /
       3600-ROLLBACK SECTION.
      ***********************
      *
       3610-ROLLBACK.
      *
      * Place any additional rollback processing in here.
      *
      *    CONTINUE.

       3690-EXIT.
           EXIT.
      /
       4000-CLOSE SECTION.
      ********************
      *
       4010-CLOSE-FILES.
      *



           MOVE O-K                    TO LSAA-STATUZ.
      *
       4090-EXIT.
           EXIT.
