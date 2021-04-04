       IDENTIFICATION DIVISION.
       PROGRAM-ID. BVZZ3.
      *
      * Copyright 1986-2020, Computer Sciences Corporation.
      *
      *
      *REMARKS.
      *
      ***********************************************************************
      *           AMENDMENT  HISTORY                                        *
      ***********************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....                               *
      *                                                                     *
      * 19/11/20  01/01   DUMMY        IT-Nguyen Huu Tri                    *
      *           INSERT AGENT DATA TO ZZZ2PF FROM AGLFPF                   *
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
      *    SELECT PRINTER-FILE  ASSIGN TO FORMATFILE-RXXXX-SI.
      /
       DATA DIVISION.
       FILE SECTION.
      *FD  PRINTER-FILE                LABEL RECORDS STANDARD.
      *01  PRINTER-REC                 PIC X(001).
      *      ------------>  Change to largest record format size
      /
       WORKING-STORAGE SECTION.
      *
       01  WSAA-PROG                   PIC X(05) VALUE 'BVZZ3'.
       01  WSAA-VERSION                PIC X(02) VALUE '01'.
      *
      *  These fields are required by MAINB processing and should not
      *   be deleted.
      *
       01  WSAA-COMMIT-CNT             PIC S9(08) COMP-3.
       01  WSAA-CYCLE-CNT              PIC S9(08) COMP-3.
       01  WSAA-CNT                    PIC 9(02).
       01  WSSP-EDTERROR               PIC X(04).
       01  HAS-AGNT-CLNTNUM            PIC X(01).
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
           03  AGLFREC                 PIC X(10) VALUE 'AGLFREC'.
           03  AGNTREC                 PIC X(10) VALUE 'AGNTREC'.
           03  CLNTREC                 PIC X(10) VALUE 'CLNTREC'.
           03  ZZZ2REC                 PIC X(10) VALUE 'ZZZ2REC'.
      *    03  TVZZ3REC                PIC X(10) VALUE 'TVZZ3REC'.
           03  ITEMREC                 PIC X(10) VALUE 'ITEMREC'.
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
      *01  INDIC-AREA.
      *    03  INDIC-TABLE  OCCURS 99  PIC 1 INDICATOR 1.
      *        88  IND-OFF  VALUE B'0'.
      *        88  IND-ON   VALUE B'1'.
      *
      *   Main, standard page headings
      *
      *01  RXXXX-H01.
      *    COPY DD-RXXXXH01-O OF RXXXX.
      *
      *  Detail line - add as many detail and total lines as required.
      *              - use redefines to save WS space where applicable.
      *
      *01  RXXXX-D01.
      *    COPY DD-RXXXXD01-O OF RXXXX.
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
           COPY ITEMSKM.
      /
           COPY AGLFSKM.
      /
           COPY AGNTSKM.
      /
           COPY CLNTSKM.
      /
           COPY ZZZ2SKM.
      /
           COPY TVZZ3REC.



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
      *

           MOVE '1'                   TO HAS-AGNT-CLNTNUM.
           MOVE 'IT'                  TO ITEM-ITEMPFX.
           MOVE '2'                   TO ITEM-ITEMCOY.
           MOVE 'TVZZ3'               TO ITEM-ITEMTABL.
           MOVE '****'                TO ITEM-ITEMITEM.
           MOVE SPACES                TO ITEM-ITEMSEQ.
           MOVE READR                 TO ITEM-FUNCTION.
           MOVE ITEMREC               TO ITEM-FORMAT.


           CALL 'ITEMIO'               USING ITEM-PARAMS

           IF ITEM-STATUZ              NOT = O-K
           AND ITEM-STATUZ             NOT = MRNF

               MOVE ITEM-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR

           END-IF.

           IF ITEM-STATUZ              = MRNF

               MOVE ENDP                TO WSSP-EDTERROR
               GO TO 2090-EXIT

           ELSE

               MOVE ITEM-GENAREA        TO TVZZ3-TVZZ3-REC
               MOVE BEGN                TO AGLF-FUNCTION

           END-IF.

      *
       1090-EXIT.
           EXIT.
      /
       2000-READ-FILE SECTION.
      ************************
      *
       2010-READ-FILE.
      *

           PERFORM 2100-READ-FILE-AGLF.

      *
       2090-EXIT.
           EXIT.
       2100-READ-FILE-AGLF SECTION.
      *****************************
       2110-START.

           MOVE '2'                 TO AGLF-AGNTCOY.
           MOVE SPACES              TO AGLF-AGNTNUM.
           MOVE AGLFREC             TO AGLF-FORMAT.
           CALL 'AGLFIO'            USING AGLF-PARAMS.

           IF AGLF-STATUZ           NOT = O-K
           AND AGLF-STATUZ          NOT = ENDP

               MOVE AGLF-PARAMS     TO  SYSR-PARAMS
               PERFORM 600-FATAL-ERROR

           END-IF.

           IF AGLF-STATUZ               = ENDP

               MOVE  ENDP           TO WSSP-EDTERROR
               GO TO 2150-EXIT

           END-IF.
           MOVE NEXTR               TO AGLF-FUNCTION.

       2150-EXIT.
           EXIT.
      /
       2200-READ-FILE-AGNT SECTION.
      *****************************
       2210-START.
            MOVE '1'                TO HAS-AGNT-CLNTNUM.
            MOVE 'AG'               TO AGNT-AGNTPFX.
            MOVE '2'                TO AGNT-AGNTCOY.
            MOVE AGLF-AGNTNUM       TO AGNT-AGNTNUM.
            MOVE READR              TO AGNT-FUNCTION.
            MOVE AGNTREC            TO AGNT-FORMAT.
            CALL 'AGNTIO'           USING AGNT-PARAMS.

            IF AGNT-STATUZ          NOT = O-K
            AND AGNT-STATUZ             = MRNF

               MOVE AGNT-PARAMS    TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR

            END-IF.

            IF AGNT-STATUZ              = MRNF

               MOVE '0'                TO HAS-AGNT-CLNTNUM

            ELSE

               PERFORM 2300-READ-FILE-CLNTPF

            END-IF.

       2250-EXIT.
           EXIT.
      /
       2300-READ-FILE-CLNTPF SECTION.
      *******************************
       2310-START.
           MOVE 'CN'               TO CLNT-CLNTPFX
           MOVE '9'                TO CLNT-CLNTCOY.
           MOVE AGNT-CLNTNUM       TO CLNT-CLNTNUM.
           MOVE READR              TO CLNT-FUNCTION.
           MOVE CLNTREC            TO CLNT-FORMAT.
           CALL 'CLNTIO'           USING CLNT-PARAMS.

           IF CLNT-STATUZ          NOT = O-K
           AND CLNT-STATUZ         NOT = MRNF

               MOVE CLNT-PARAMS    TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR

           END-IF.

           IF CLNT-STATUZ              = MRNF

               MOVE '0'            TO HAS-AGNT-CLNTNUM

           END-IF.

       2350-EXIT.
           EXIT.
      /
       2400-ASIGN-VALUE-TO-ZZZ2PF SECTION.
      ************************************
       2400-START.

           MOVE AGLF-AGNTNUM           TO ZZZ2-AGNTNUM.
           MOVE CLNT-SURNAME           TO ZZZ2-LSURNAME.
           MOVE CLNT-GIVNAME           TO ZZZ2-LGIVNAME.
           MOVE AGLF-TSALESUNT         TO ZZZ2-TSALESUNT.
           MOVE AGLF-ARACDE            TO ZZZ2-ARACDE.
           MOVE AGLF-DTEAPP            TO ZZZ2-DTEAPP.
           MOVE CLNT-CLTDOD            TO ZZZ2-CLTDOB.

       2400-EXIT.
           EXIT.

     /
       2500-EDIT SECTION.
      *******************
      *
       2510-EDIT.
      *
           MOVE O-K                    TO WSSP-EDTERROR.

           MOVE NEXTR                  TO AGLF-FUNCTION.

           IF AGLF-ARACDE             NOT = TVZZ3-ARACDEN
           OR  AGLF-TSALESUNT         NOT = TVZZ3-TSALESUNT

               MOVE SPACES             TO WSSP-EDTERROR
               GO TO 2590-EXIT

           END-IF.

      *
       2590-EXIT.
           EXIT.
      /
       3000-UPDATE SECTION.
      *********************
      *
       3010-UPDATE.
      *
      * Update database records.
      *
           PERFORM 2200-READ-FILE-AGNT.

           MOVE NEXTR                  TO  AGLF-FORMAT.

           IF HAS-AGNT-CLNTNUM            = '0'

              GO TO 3090-EXIT

           END-IF.

           PERFORM 2400-ASIGN-VALUE-TO-ZZZ2PF.
           MOVE WRITR                  TO ZZZ2-FUNCTION.
           MOVE ZZZ2REC                TO ZZZ2-FORMAT.
           CALL 'ZZZ2IO'            USING ZZZ2-PARAMS.

           IF ZZZ2-STATUZ             NOT = O-K

              MOVE ZZZ2-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR

           END-IF.


      *
      *
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
           CONTINUE.

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
           CONTINUE.

       3690-EXIT.
           EXIT.
      /
       4000-CLOSE SECTION.
      ********************
      *
       4010-CLOSE-FILES.
      *
      *  Close any open files.
      *
           MOVE O-K                    TO LSAA-STATUZ.
      *
       4090-EXIT.
           EXIT.
