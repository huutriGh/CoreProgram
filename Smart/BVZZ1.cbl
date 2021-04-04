       IDENTIFICATION DIVISION.
       PROGRAM-ID. BVZZ1.
      *
      * Copyright 1986-2020, Computer Sciences Corporation.
      *
      *REMARKS.
      *
      ***********************************************************************
      *           AMENDMENT  HISTORY                                        *
      ***********************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....                               *
      *                                                                     *
      * 11/11/20  01/01   DUMMY        Nguyen Huu Tri - IT                  *
      *           GENERATE DATA TO FILE ZZZ1PF BASE ON PAR INPUT            *
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
       01  WSAA-PROG                   PIC X(05) VALUE 'BVZZ1'.
       01  WSAA-VERSION                PIC X(02) VALUE '01'.
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
           03  CHDRLNBREC              PIC X(10) VALUE 'CHDRLNBREC'.
           03  HPADREC                 PIC X(10) VALUE 'HPADREC'.
           03  ZZZ1REC                 PIC X(10) VALUE 'ZZZ1REC'.
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
           COPY CHDRLNBSKM.
      /
           COPY HPADSKM.
      /
           COPY ZZZ1SKM.
      /
           COPY PVZZ2PAR.
      /
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
      * Open required files.
      *
      ***  OPEN OUTPUT PRINTER-FILE.
           MOVE BUPA-PARMAREA          TO PVZZ2-PARM-RECORD.
           MOVE O-K                    TO WSSP-EDTERROR.
      *
      **** MOVE BSSC-EFFECTIVE-DATE    TO WSAA-TODAY.
           MOVE 2                      TO CHDRLNB-CHDRCOY.
           MOVE SPACES                 TO CHDRLNB-CHDRNUM.
           MOVE BEGN                   TO CHDRLNB-FUNCTION.
           MOVE CHDRLNBREC             TO CHDRLNB-FORMAT.
      *
       1090-EXIT.
           EXIT.
      /
       2000-READ-FILE SECTION.
      ************************
      *
       2010-READ-FILE.

           CALL 'CHDRLNBIO'            USING CHDRLNB-PARAMS.

           IF CHDRLNB-STATUZ           NOT = O-K
           AND CHDRLNB-STATUZ          NOT = ENDP
             MOVE CHDRLNB-PARAMS       TO  SYSR-PARAMS
             PERFORM 600-FATAL-ERROR
           END-IF.

           IF CHDRLNB-STATUZ               = ENDP

              MOVE ENDP                TO   WSSP-EDTERROR
              GO TO 2090-EXIT

           END-IF.
           MOVE NEXTR                  TO CHDRLNB-FUNCTION.

      *
       2090-EXIT.
           EXIT.
      /
       2500-EDIT SECTION.
      *******************
      *
       2510-EDIT.
      *
      *  Check record is required for processing.
      *  Softlock the record if it is to be updated.
      *
           MOVE O-K                    TO WSSP-EDTERROR.
           IF CHDRLNB-STATCODE         NOT = PVZZ2-ACCTYP
               MOVE SPACES                 TO WSSP-EDTERROR
               GO TO 2590-EXIT
           END-IF.
           IF CHDRLNB-VALIDFLAG            = '3'
              IF CHDRLNB-STATCODE NOT = 'PS'

                 MOVE SPACES                  TO WSSP-EDTERROR
                 GO TO 2590-EXIT

              ELSE

                 GO TO 2590-EXIT

              END-IF

           END-IF.
           IF CHDRLNB-VALIDFLAG       NOT  = '1'
              MOVE SPACES                  TO WSSP-EDTERROR
              GO TO 2590-EXIT
           END-IF.
      *
       2590-EXIT.
           EXIT.
      /
       2600-ASIGN-VALUE-TO-ZZZ1PF SECTION.
      ************************************
       2610-START.

              MOVE CHDRLNB-CHDRCOY     TO  ZZZ1-TCTRCOM.
              MOVE CHDRLNB-CHDRNUM     TO  ZZZ1-TCHDRNUM.
              MOVE CHDRLNB-CNTTYPE     TO  ZZZ1-TCTRTYPE.
              MOVE CHDRLNB-STATCODE    TO  ZZZ1-TCTRSTA.
              MOVE CHDRLNB-PSTATCODE   TO  ZZZ1-TPREMSTA.
              MOVE CHDRLNB-TRANNO      TO  ZZZ1-TTRANNUM.
              MOVE CHDRLNB-AGNTNUM     TO  ZZZ1-TAGNTNUM.
              MOVE CHDRLNB-COWNNUM     TO  ZZZ1-TPLOWNER.

       2630-EXIT.
           EXIT.
      /

       2700-READ-HPAD-FILE SECTION.
      *****************************
       2710-START.

           MOVE CHDRLNB-CHDRCOY               TO  HPAD-CHDRCOY.
           MOVE CHDRLNB-CHDRNUM               TO  HPAD-CHDRNUM.
           MOVE READR                         TO  HPAD-FUNCTION.
           MOVE HPADREC                       TO  HPAD-FORMAT.
           CALL 'HPADIO'                   USING  HPAD-PARAMS.

           IF HPAD-STATUZ                     NOT = O-K
           AND HPAD-STATUZ                    NOT = MRNF

               MOVE HPAD-PARAMS               TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR

           END-IF.

           IF HPAD-STATUZ                         = MRNF
              MOVE SPACES                        TO WSSP-EDTERROR
              GO TO 2730-EXIT
           ELSE

               MOVE HPAD-HPROPDTE                 TO ZZZ1-TPRODATE
               MOVE HPAD-HISSDTE                  TO ZZZ1-TISSEUDT

           END-IF.

       2730-EXIT.
           EXIT.
      /

       3000-UPDATE SECTION.
      *********************
      *
       3010-UPDATE-START.
      *
      * Update database records.
      *
           PERFORM 2600-ASIGN-VALUE-TO-ZZZ1PF
           PERFORM 2700-READ-HPAD-FILE
           MOVE WRITR                         TO ZZZ1-FUNCTION.
           MOVE ZZZ1REC                       TO ZZZ1-FORMAT.
           CALL 'ZZZ1IO'                   USING ZZZ1-PARAMS.

           IF ZZZ1-STATUZ                     NOT = O-K
             MOVE ZZZ1-PARAMS                 TO    SYSR-PARAMS
             PERFORM 600-FATAL-ERROR

           END-IF.
           MOVE NEXTR                         TO    CHDRLNB-FUNCTION.

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
      **** CLOSE PRINTER-FILE.
           MOVE O-K                    TO LSAA-STATUZ.
      *
       4090-EXIT.
           EXIT.
