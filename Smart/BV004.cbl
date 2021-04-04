       IDENTIFICATION DIVISION.
       PROGRAM-ID.     BV004.
      *
      *(C) Copyright CSC Corporation Limited 1986 - 2000.
      *    All rights reserved. CSC Confidential.
      *
      *REMARKS.
      *   This is Tax Invoice Report Downloading program.
      *
      ***********************************************************************
      *           AMENDMENT  HISTORY                                        *
      ***********************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....                               *
      *                                                                     *
      * 17/10/13  01/01   GAPPH1       Thanh Do                             *
      *           Tax Invoice Reporting batch program.                      *
      *                                                                     *
      * 19/02/14  01/01   PHE001       Thanh Do                             *
      *           Add Premium Discount Amount to report files.              *
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
           SELECT ZIVRPF               ASSIGN TO DATABASE-ZIVRPF.
      /
       DATA DIVISION.
       FILE SECTION.
       FD  ZIVRPF                      LABEL RECORDS STANDARD.
       01  ZIVRPF-REC.
           COPY DDS-ALL-FORMATS OF ZIVRPF.
      *
      /
       WORKING-STORAGE SECTION.
      *
       01  WSAA-PROG                   PIC X(05) VALUE 'BV004'.
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
           03  ITEMREC                 PIC X(10) VALUE 'ITEMREC'.
           03  DESCREC                 PIC X(10) VALUE 'DESCREC'.
           03  ZTAXRPTREC              PIC X(10) VALUE 'ZTAXRPTREC'.
           03  CLNTREC                 PIC X(10) VALUE 'CLNTREC'.
           03  CHDRLNBREC              PIC X(10) VALUE 'CHDRLNBREC'.
      *
       01  TABLES.
           03  TH558                   PIC X(06) VALUE 'TH558'.
           03  T5687                   PIC X(06) VALUE 'T5687'.
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
      *  ZIVR member parametres.

       01  WSAA-ZIVR-FN.
           03  FILLER                  PIC X(04) VALUE 'ZIVR'.
           03  WSAA-ZIVR-RUNID         PIC X(02) VALUE 'RP'.
           03  WSAA-ZIVR-JOBNO         PIC 9(04).

       01  WSAA-THREAD-MEMBER.
           03  FILLER                  PIC X(06) VALUE 'THREAD'.
           03  WSAA-THREAD-NUMBER      PIC 9(03).
           03  FILLER                  PIC X.

       01  WSAA-QCMDEXC              PIC X(100).
       01  WSAA-QCMDEXC-LENGTH       PIC S9(10)V9(05) COMP-3 VALUE 100.
      *
       01  WSAA-COMPANY                PIC X(01).
       01  WSAA-COUNT                  PIC 9(05) VALUE 0.
       01  WSAA-LSURNAME               PIC X(60).
       01  WSAA-LGIVNAME               PIC X(60).
       01  WSAA-PRODDESC               PIC X(30).
       01  WSAA-DATECFROM              PIC X(10).
       01  WSAA-DATECTO                PIC X(10).
      *
       01  INDIC-AREA.
           03  INDIC-TABLE  OCCURS 99  PIC 1 INDICATOR 1.
               88  IND-OFF  VALUE B'0'.
               88  IND-ON   VALUE B'1'.
      *
      /
           COPY BATCDORREC.
           COPY BATCUPREC.
           COPY BSSCSKM.
           COPY BSPRSKM.
           COPY BUPASKM.
           COPY BPRDSKM.
           COPY CONLOGREC.
           COPY CONTOTREC.
           COPY DATCON1REC.
           COPY DESCSKM.
           COPY SFTLOCKREC.
           COPY SYSERRREC.
           COPY VARCOM.
      /
           COPY FSUPFXCPY.
           COPY SMTPFXCPY.
           COPY PV004PAR.
           COPY ZTAXRPTSKM.
           COPY CLNTSKM.
           COPY ITEMSKM.
           COPY TH558REC.
           COPY CHDRLNBSKM.
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
           MOVE BUPA-PARMAREA          TO PV004-PARM-RECORD.
           MOVE O-K                    TO WSSP-EDTERROR.
      *
           IF BPRD-SYSTEM-PARAM04      NOT = SPACES
              MOVE BPRD-SYSTEM-PARAM04 TO WSAA-ZIVR-RUNID.

           MOVE BPRD-COMPANY           TO WSAA-COMPANY.                 <LA2108>
           MOVE BSSC-SCHEDULE-NUMBER   TO WSAA-ZIVR-JOBNO.
           MOVE BSPR-PROCESS-OCC-NUM   TO WSAA-THREAD-NUMBER.

      *    Do the override.

           STRING
               'OVRDBF FILE(ZIVRPF)    TOFILE('
                                       DELIMITED BY SIZE
                BPRD-RUN-LIBRARY       DELIMITED BY SPACES
               '/' WSAA-ZIVR-FN
               ') MBR(' WSAA-THREAD-MEMBER ')'
               ' SEQONLY(*YES 1000)'
                                       DELIMITED BY SIZE
                                       INTO WSAA-QCMDEXC
           END-STRING.

           CALL 'QCMDEXC' USING WSAA-QCMDEXC WSAA-QCMDEXC-LENGTH.

           OPEN OUTPUT ZIVRPF.
      *
           PERFORM 1100-CONV-DATE-RANGE.

           MOVE ZEROES                 TO WSAA-COUNT.

           INITIALIZE                  ZTAXRPT-PARAMS.
           MOVE PV004-DATEFRM          TO ZTAXRPT-DINVOICEDT.
           MOVE SPACES                 TO ZTAXRPT-ZCODE.
           MOVE ZEROES                 TO ZTAXRPT-INVSEQ.
           MOVE ZTAXRPTREC             TO ZTAXRPT-FORMAT.
           MOVE BEGN                   TO ZTAXRPT-FUNCTION.
      *
      *
       1090-EXIT.
           EXIT.
      /
       1100-CONV-DATE-RANGE SECTION.
      ******************************
       1101-START.
      *
           INITIALIZE                  DTC1-DATCON1-REC.
           MOVE PV004-DATEFRM          TO DTC1-INT-DATE.
           PERFORM 2900-DATCON1-DATE.
           MOVE DTC1-EXT-DATE          TO WSAA-DATECFROM.

           INITIALIZE                  DTC1-DATCON1-REC.
           MOVE PV004-DATETO           TO DTC1-INT-DATE.
           PERFORM 2900-DATCON1-DATE.
           MOVE DTC1-EXT-DATE          TO WSAA-DATECTO.
      *
       1119-EXIT.
           EXIT.
      /
       2000-READ-FILE SECTION.
      ************************
      *
       2010-READ-FILE.
      *
           CALL 'ZTAXRPTIO'            USING ZTAXRPT-PARAMS.

           IF ZTAXRPT-STATUZ           NOT = O-K
           AND                         NOT = ENDP
               MOVE ZTAXRPT-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF ZTAXRPT-STATUZ           = ENDP
           OR ZTAXRPT-DINVOICEDT       > PV004-DATETO
               MOVE ENDP               TO WSSP-EDTERROR
               GO TO 2090-EXIT
           END-IF.
      *
           MOVE NEXTR                  TO ZTAXRPT-FUNCTION.
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

           IF ZTAXRPT-DINVOICEDT       < PV004-DATEFRM
           OR ZTAXRPT-DINVOICEDT       > PV004-DATETO
               MOVE SPACES             TO WSSP-EDTERROR
               GO TO 2590-EXIT
           END-IF.

      * Process for Contract Level only. Check Basic Product:

           IF ZTAXRPT-BASEPFLAG        NOT = 'Y'
               MOVE SPACES             TO WSSP-EDTERROR
               GO TO 2590-EXIT
           END-IF.

           PERFORM 2600-READ-CONTRACT-HEADER.
      *
       2590-EXIT.
           EXIT.
      /
       2600-READ-CONTRACT-HEADER SECTION.
      ***********************************
       2601-START.
      *
           INITIALIZE                  CHDRLNB-PARAMS.
           MOVE WSAA-COMPANY           TO CHDRLNB-CHDRCOY.
           MOVE ZTAXRPT-CHDRNUM        TO CHDRLNB-CHDRNUM.
           MOVE CHDRLNBREC             TO CHDRLNB-FORMAT.
           MOVE READR                  TO CHDRLNB-FUNCTION.

           CALL 'CHDRLNBIO'            USING CHDRLNB-PARAMS.

           IF CHDRLNB-STATUZ           NOT = O-K
               MOVE CHDRLNB-STATUZ     TO SYSR-STATUZ
               MOVE CHDRLNB-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.
      *
       2619-EXIT.
           EXIT.
      /
       2900-DATCON1-DATE SECTION.
      ***************************
       2901-START.
      *
           MOVE 'CONV'                 TO DTC1-FUNCTION.
           MOVE O-K                    TO DTC1-STATUZ.
           CALL 'DATCON1'              USING DTC1-DATCON1-REC.
           IF DTC1-STATUZ              NOT = O-K
               STRING DTC1-DATCON1-REC DELIMITED BY SIZE
                      WSAA-PROG        DELIMITED BY SIZE
                                  INTO SYSR-PARAMS
               END-STRING
               PERFORM 600-FATAL-ERROR
           END-IF.
      *
       2919-EXIT.
           EXIT.
      /
       3000-UPDATE SECTION.
      *********************
       3010-UPDATE.
      *
           ADD 1                       TO WSAA-COUNT.

           MOVE WSAA-COUNT             TO SEQNUM      OF ZIVRPF.
           MOVE ZTAXRPT-ZCODE          TO ZCODE       OF ZIVRPF.
           MOVE ZTAXRPT-INVSEQ         TO INVSEQ      OF ZIVRPF.        <GVL209>

           INITIALIZE                  DTC1-DATCON1-REC.
           MOVE ZTAXRPT-DINVOICEDT     TO DTC1-INT-DATE.
           PERFORM 2900-DATCON1-DATE.
           MOVE DTC1-EXT-DATE          TO DATEX       OF ZIVRPF.

           PERFORM 3100-GET-POLICYHOLDER-NAME.
           MOVE WSAA-LSURNAME          TO LSURNAME    OF ZIVRPF.
           MOVE WSAA-LGIVNAME          TO LGIVNAME    OF ZIVRPF.

           MOVE ZTAXRPT-TAXIDNUM       TO TAXIDNUM    OF ZIVRPF.

           PERFORM 3200-GET-PRODUCT-DESC.
           MOVE WSAA-PRODDESC          TO PDESC       OF ZIVRPF.

           MOVE ZTAXRPT-TOTLPREM       TO TOTLPREM    OF ZIVRPF.
           MOVE ZTAXRPT-TRANCD         TO TRANCD      OF ZIVRPF.
           MOVE ZTAXRPT-DISCOUNT       TO DISCOUNT    OF ZIVRPF.        <PHE001>

           MOVE WSAA-DATECFROM         TO DATECFROM   OF ZIVRPF.
           MOVE WSAA-DATECTO           TO DATECTO     OF ZIVRPF.

           MOVE ZTAXRPT-CHDRNUM        TO CHDRNUM     OF ZIVRPF.
           MOVE ZTAXRPT-COWNNUM        TO COWNNUM     OF ZIVRPF.

           MOVE CHDRLNB-STATCODE       TO STATCODE    OF ZIVRPF.

           INITIALIZE                  DTC1-DATCON1-REC.
           MOVE ZTAXRPT-PTDATE         TO DTC1-INT-DATE.
           PERFORM 2900-DATCON1-DATE.
           MOVE DTC1-EXT-DATE          TO RPTDATE     OF ZIVRPF.

           MOVE ZTAXRPT-BILLFREQ       TO BILLFREQ    OF ZIVRPF.
           MOVE ZTAXRPT-STFLAG         TO STFLAG      OF ZIVRPF.

           WRITE ZIVRPF-REC.
      *
           MOVE NEXTR                  TO ZTAXRPT-FUNCTION.
      *
       3090-EXIT.
           EXIT.
      /
       3100-GET-POLICYHOLDER-NAME SECTION.
      ************************************
       3101-START.
      *
           MOVE SPACES                 TO WSAA-LSURNAME
                                          WSAA-LGIVNAME.

           INITIALIZE                  CLNT-PARAMS.
           MOVE PRFX-CLNT              TO CLNT-CLNTPFX.
           MOVE BSPR-FSUCO             TO CLNT-CLNTCOY.
           MOVE ZTAXRPT-COWNNUM        TO CLNT-CLNTNUM.
           MOVE CLNTREC                TO CLNT-FORMAT.
           MOVE READR                  TO CLNT-FUNCTION.

           CALL 'CLNTIO'               USING CLNT-PARAMS.

           IF CLNT-STATUZ              NOT = O-K
               MOVE CLNT-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE CLNT-LSURNAME          TO WSAA-LSURNAME.
           MOVE CLNT-LGIVNAME          TO WSAA-LGIVNAME.
      *
       3190-EXIT.
           EXIT.
      /
       3200-GET-PRODUCT-DESC SECTION.
      *******************************
       3201-START.
      *
           MOVE SPACES                 TO WSAA-PRODDESC.

      * Read TH558 for extendend description for contract type, if not  <GVLPB3>
      * found, read T5688.                                              <GVLPB3>
      *                                                                 <GVLPB3>
           INITIALIZE                  ITEM-DATA-KEY.                   <GVLPB3>
           MOVE O-K                    TO ITEM-STATUZ.                  <GVLPB3>
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.                 <GVLPB3>
           MOVE WSAA-COMPANY           TO ITEM-ITEMCOY.                 <GVLPB3>
           MOVE TH558                  TO ITEM-ITEMTABL.                <GVLPB3>
           STRING BSSC-LANGUAGE        DELIMITED BY SIZE                <GVLPB3>
                  CHDRLNB-CNTTYPE      DELIMITED BY SIZE                <GVLPB3>
                                  INTO ITEM-ITEMITEM.                   <GVLPB3>
           MOVE SPACES                 TO ITEM-ITEMSEQ.
           MOVE ITEMREC                TO ITEM-FORMAT.                  <GVLPB3>
           MOVE READR                  TO ITEM-FUNCTION.                <GVLPB3>
                                                                        <GVLPB3>
           CALL 'ITEMIO'               USING ITEM-PARAMS.               <GVLPB3>
                                                                        <GVLPB3>
           IF  ITEM-STATUZ             NOT = O-K                        <GVLPB3>
           AND ITEM-STATUZ             NOT = MRNF                       <GVLPB3>
               MOVE ITEM-STATUZ        TO SYSR-STATUZ                   <GVLPB3>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <GVLPB3>
               PERFORM 600-FATAL-ERROR                                  <GVLPB3>
           END-IF.                                                      <GVLPB3>
                                                                        <GVLPB3>
           IF ITEM-STATUZ              = O-K                            <GVLPB3>
              MOVE ITEM-GENAREA        TO TH558-TH558-REC               <GVLPB3>
              IF  TH558-ADSC           NOT = SPACES                     <GVLPB3>
                  MOVE TH558-ADSC      TO WSAA-PRODDESC                 <GVLPB3>
              END-IF                                                    <GVLPB3>
           ELSE                                                         <GVLPB3>
               INITIALIZE              DESC-PARAMS
               MOVE SMTP-ITEM          TO DESC-DESCPFX
               MOVE WSAA-COMPANY       TO DESC-DESCCOY
               MOVE T5687              TO DESC-DESCTABL
               MOVE ZTAXRPT-CRCODE     TO DESC-DESCITEM
               MOVE SPACES             TO DESC-ITEMSEQ
               MOVE BSSC-LANGUAGE      TO DESC-LANGUAGE
               MOVE DESCREC            TO DESC-FORMAT
               MOVE READR              TO DESC-FUNCTION

               CALL 'DESCIO'           USING DESC-PARAMS

               IF DESC-STATUZ          NOT = O-K
                   MOVE DESC-PARAMS    TO SYSR-PARAMS
                   PERFORM 600-FATAL-ERROR
               END-IF

               MOVE DESC-LONGDESC      TO WSAA-PRODDESC
           END-IF.
      *
       3290-EXIT.
           EXIT.
      /
       3500-COMMIT SECTION.
      *********************
      *
       3510-COMMIT.
      *
      * Place any additional commitment processing in here.
      *
       3590-EXIT.
           EXIT.
      /
       3600-ROLLBACK SECTION.
      **********************
      *
       3610-ROLLBACK.
      *
      * Place any additional rollback processing in here.
      *
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
           CLOSE ZIVRPF.
           MOVE O-K                    TO LSAA-STATUZ.
      *
       4090-EXIT.
           EXIT.
