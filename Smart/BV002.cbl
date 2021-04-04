       IDENTIFICATION DIVISION.
       PROGRAM-ID.     BV002.
      *(C) Copyright CSC Corporation Limited 1986 - 2000.
      *    All rights reserved. CSC Confidential.
      *
      *REMARKS.
      *   This is Tax Invoice Report Download program.
      *
      *   The program will read the report file ZIVRDLxxxx and
      *   1. Prepare & copy data for the 1st Invoice Report
      *       "BANG KE HOA DON, CHUNG TU HANG HOA, D-VU BAN RA"
      *   2. Prepare & copy data for the 2nd Invoice Report
      *       "DANH SACH HOA DON GTGT" - (Internal usage)
      *   3. Prepare & copy data for the 3rd Invoice Report
      *       "BAO CAO TINH HINH SU DUNG HOA DON"
      *
      ***********************************************************************
      *           AMENDMENT  HISTORY                                        *
      ***********************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....                               *
      *                                                                     *
      * 21/10/13  01/01   GAPPH1       Thanh Do                             *
      *           Prepare & Copy reports to CSV files.                      *
      *                                                                     *
      * 19/02/14  01/01   PHE001       Thanh Do                             *
      *           Add premium discount to reports.                          *
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
           SELECT ZCSVPF               ASSIGN TO DATABASE-ZCSVPF.
           SELECT ZIVR01               ASSIGN TO DISK-ZIVR01.
           SELECT ZIVR02               ASSIGN TO DISK-ZIVR02.
           SELECT ZIVR03               ASSIGN TO DISK-ZIVR03.
      /
       DATA DIVISION.
       FILE SECTION.
       FD  ZIVRPF                      LABEL RECORDS STANDARD.
       01  ZIVRPF-REC.
           COPY DDS-ALL-FORMATS OF ZIVRPF.

       FD  ZCSVPF                      LABEL RECORDS STANDARD.
       01  ZCSVPF-REC.
           COPY DDS-ALL-FORMATS OF ZCSVPF.
      *
       FD  ZIVR01                      LABEL RECORDS STANDARD.
       01  ZIVR01-REC                  PIC X(1024).
      *
       FD  ZIVR02                      LABEL RECORDS STANDARD.
       01  ZIVR02-REC                  PIC X(1024).
      *
       FD  ZIVR03                      LABEL RECORDS STANDARD.
       01  ZIVR03-REC                  PIC X(1024).
      /
       WORKING-STORAGE SECTION.
      *
       01  WSAA-PROG                   PIC X(05) VALUE 'BV002'.
       01  WSAA-VERSION                PIC X(02) VALUE '01'.
      *
      *01  ZCSVPF-REC.
      *    COPY DDS-ALL-FORMATS OF ZCSVPF.
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
      *
       01  TABLES.
           03  T1692                   PIC X(06) VALUE 'T1692'.
           03  T1693                   PIC X(06) VALUE 'T1693'.
           03  T3629                   PIC X(06) VALUE 'T3629'.
      *
       01  CONTROL-TOTALS.
           03  CT01                    PIC 9(02) VALUE 01.
      *
       01  WSAA-ZIVR-FN.
           03  FILLER                  PIC X(04) VALUE 'ZIVR'.
           03  WSAA-ZIVR-RUNID         PIC X(02) VALUE 'RP'.
           03  WSAA-ZIVR-JOBNO         PIC 9(04).
      *
       01  WSAA-ZIV1-FN.
           03  FILLER                  PIC X(04) VALUE 'ZCSV'.
           03  WSAA-ZIV1-RUNID         PIC X(02) VALUE '1R'.
           03  WSAA-ZIV1-JOBNO         PIC 9(04).
      *
       01  WSAA-ZIV2-FN.
           03  FILLER                  PIC X(04) VALUE 'ZCSV'.
           03  WSAA-ZIV2-RUNID         PIC X(02) VALUE '2R'.
           03  WSAA-ZIV2-JOBNO         PIC 9(04).
      *
       01  WSAA-ZIV3-FN.
           03  FILLER                  PIC X(04) VALUE 'ZCSV'.
           03  WSAA-ZIV3-RUNID         PIC X(02) VALUE '3R'.
           03  WSAA-ZIV3-JOBNO         PIC 9(04).
      *
       01  WSAA-THREAD-MEMBER.
           03  FILLER                  PIC X(06)   VALUE 'THREAD'.
           03  WSAA-THREAD-NUMBER      PIC 9(03).
           03  FILLER                  PIC X.
      *
       01  WSAA-QCMDEXC                PIC X(200).
       01  WSAA-QCMDEXC-LENGTH         PIC S9(10)V9(05)
                                           COMP-3 VALUE 200.
       01  WSAA-REC                    PIC X(1024).
       01  WSAA-HEADER-WRITTEN         PIC X(01).
       01  WSAA-ACTIVE                 PIC X(10) VALUE 'Active'.
       01  WSAA-CANCEL                 PIC X(10) VALUE 'Cancelled'.
      *
       01  WSAA-HEADER-H1.
           03  H1-NO                   PIC X(03) VALUE 'NO.'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H1-SERIES               PIC X(08) VALUE 'NOTATION'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H1-SEQUENCE             PIC X(10) VALUE 'INVOICE-NO'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H1-ISS-DATE             PIC X(11) VALUE 'ISSUED-DATE'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H1-OWNERNAME            PIC X(10) VALUE 'OWNER-NAME'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H1-PO-TAXID             PIC X(11) VALUE 'PO-TAX-CODE'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H1-PROD-DESC            PIC X(11) VALUE 'PRODUCTNAME'.   <PHE001>
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H1-TOT-AMT              PIC X(07) VALUE 'PREMIUM'.
           03  FILLER                  PIC X(01) VALUE ';'.             <PHE001>
           03  H1-TOT-DISCOUNT         PIC X(08) VALUE 'DISCOUNT'.      <PHE001>
      *
       01  WSAA-HEADER-H2.
           03  H2-NO                   PIC X(03) VALUE 'NO.'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H2-SERIES               PIC X(08) VALUE 'NOTATION'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H2-SEQUENCE             PIC X(10) VALUE 'INVOICE-NO'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H2-ISS-DATE             PIC X(11) VALUE 'ISSUED-DATE'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H2-OWNERNAME            PIC X(10) VALUE 'OWNER-NAME'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H2-PO-TAXID             PIC X(11) VALUE 'PO-TAX-CODE'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H2-PROD-DESC            PIC X(11) VALUE 'PRODUCTNAME'.   <PHE001>
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H2-TOT-AMT              PIC X(07) VALUE 'PREMIUM'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H2-VAT                  PIC X(10) VALUE 'VAT-AMOUNT'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H2-NOTES                PIC X(05) VALUE 'NOTES'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H2-CHDRNUM              PIC X(10) VALUE 'POLICY-NO.'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H2-STATCODE             PIC X(10) VALUE 'POL-STATUS'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H2-PTDATE               PIC X(08) VALUE 'DUE-DATE'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H2-BILLFREQ             PIC X(09) VALUE 'FREQUENCY'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H2-INV-STATUS           PIC X(10) VALUE 'INV-STATUS'.
           03  FILLER                  PIC X(01) VALUE ';'.             <PHE001>
           03  H2-INV-DISCOUNT         PIC X(08) VALUE 'DISCOUNT'.      <PHE001>
      *
       01  WSAA-HEADER-H3.
           03  H3-NO                   PIC X(03) VALUE 'NO.'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H3-INV-NAME             PIC X(08) VALUE 'INV-NAME'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H3-INV-FORM             PIC X(08) VALUE 'INV-FORM'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H3-SERIES               PIC X(10) VALUE 'INV-SERIES'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H3-SERI-FROM            PIC X(11) VALUE 'FROM-SERIES'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H3-SERI-TO              PIC X(09) VALUE 'TO-SERIES'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H3-NO-OF-USED           PIC X(10) VALUE 'NO-OF-USED'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H3-NO-OF-CANC       PIC X(15) VALUE 'NO-OF-CANCELLED'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H3-SERI-CANC        PIC X(16) VALUE 'CANCELLED-SERIES'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  H3-TOTAL                PIC X(05) VALUE 'TOTAL'.

      * Data Details:
       01  WSAA-DETAIL-D1.
           03  D1-NO                   PIC 9(07).
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D1-SERIES               PIC X(06).
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D1-SEQUENCE             PIC X(07).
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D1-ISS-DATE             PIC X(10).
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D1-OWNERNAME            PIC X(80).
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D1-PO-TAXID             PIC X(10).
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D1-PROD-DESC            PIC X(50).
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D1-TOT-AMT              PIC 9(15).
           03  FILLER                  PIC X(01) VALUE ';'.             <PHE001>
           03  D1-TOT-DISCOUNT         PIC 9(15).                       <PHE001>
      *
       01  WSAA-DETAIL-D2.
           03  D2-NO                   PIC 9(07).
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D2-SERIES               PIC X(06).
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D2-SEQUENCE             PIC X(07).
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D2-ISS-DATE             PIC X(10).
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D2-OWNERNAME            PIC X(80).
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D2-PO-TAXID             PIC X(10).
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D2-PROD-DESC            PIC X(50).
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D2-TOT-AMT              PIC 9(15).
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D2-VAT                  PIC X(15) VALUE SPACES.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D2-NOTES                PIC X(05) VALUE SPACES.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D2-CHDRNUM              PIC X(08).
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D2-STATCODE             PIC X(02).
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D2-PTDATE               PIC X(10).
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D2-BILLFREQ             PIC X(02).
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D2-INV-STATUS           PIC X(10).
           03  FILLER                  PIC X(01) VALUE ';'.             <PHE001>
           03  D2-TOT-DISCOUNT         PIC 9(15).                       <PHE001>
      *
       01  WSAA-DETAIL-D3.
           03  D3-NO                   PIC 9(07).
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D3-INV-NAME             PIC X(20) VALUE 'Hoïa Ðín GTGT'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D3-INV-FORM             PIC X(11) VALUE '01GTGT2/001'.
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D3-SERIES               PIC X(06).
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D3-SERI-FROM            PIC X(07).                       <GAPPH1>
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D3-SERI-TO              PIC X(07).                       <GAPPH1>
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D3-NO-OF-USED           PIC 9(07).
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D3-NO-OF-CANC           PIC 9(07).
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D3-SERI-CANC            PIC X(200).
           03  FILLER                  PIC X(01) VALUE ';'.
           03  D3-TOTAL                PIC 9(07).
      *
       01  WSAA-SERIES                 PIC X(06).
       01  WSAA-INVSEQ-START           PIC X(07).
       01  WSAA-INVSEQ-END             PIC X(07).
       01  WSAA-INV-USED               PIC 9(07) VALUE 0.
       01  WSAA-INV-CANCEL             PIC 9(07) VALUE 0.
       01  WSAA-INV-TOTAL              PIC 9(07) VALUE 0.
       01  WSAA-CANC-INVNO             PIC X(200).
       01  WSAA-FIRST-TIME             PIC X(01) VALUE 'Y'.
       01  WSAA-INVSEQ                 PIC 9(07) VALUE 0.
      *
       01  WSAA-OVERFLOW               PIC X(01) VALUE 'Y'.
       88  NEW-PAGE-REQ                          VALUE 'Y'.
      *
       01  WSAA-EOF                    PIC X(01) VALUE 'N'.
       88  END-OF-FILE                           VALUE 'Y'.
      *
       01  INDIC-AREA.
           03  INDIC-TABLE  OCCURS 99  PIC 1 INDICATOR 1.
               88  IND-OFF  VALUE B'0'.
               88  IND-ON   VALUE B'1'.
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
           MOVE O-K                    TO WSSP-EDTERROR.
      *
           PERFORM 1100-OPEN-OUTPUT-FILES.
           PERFORM 1120-OPEN-INPUT-FILE.

      * Write Header Info:
           IF WSAA-HEADER-WRITTEN      NOT = 'Y'
               PERFORM 1200-WRITE-HEADERS
               MOVE 'Y'                TO WSAA-HEADER-WRITTEN
           END-IF.

           MOVE ZEROES                 TO WSAA-INV-USED
                                          WSAA-INV-CANCEL
                                          WSAA-INV-TOTAL.

           MOVE SPACES                 TO WSAA-CANC-INVNO
                                          WSAA-INVSEQ-START
                                          WSAA-INVSEQ-END.
      *
       1090-EXIT.
           EXIT.
      /
       1100-OPEN-OUTPUT-FILES SECTION.
      ********************************
       1101-START.
      *
      * Report 1: BANG KE HOA DON, CHUNG TU HANG HOA, DICH VU BAN RA:
      *
      **** MOVE BPRD-SYSTEM-PARAM04    TO WSAA-ZIV1-RUNID.
           MOVE BSSC-SCHEDULE-NUMBER   TO WSAA-ZIV1-JOBNO.
           MOVE BSPR-PROCESS-OCC-NUM   TO WSAA-THREAD-NUMBER.

           MOVE SPACES                 TO WSAA-QCMDEXC.
           STRING
               'OVRDBF FILE(ZIVR01) TOFILE('
                                       DELIMITED BY SIZE
                BPRD-RUN-LIBRARY       DELIMITED BY SPACES
               '/' WSAA-ZIV1-FN ') '
               'MBR(' WSAA-THREAD-MEMBER ')'
               ' SEQONLY(*YES 1000)'
                                       DELIMITED BY SIZE
                                       INTO WSAA-QCMDEXC
           END-STRING.

           CALL 'QCMDEXC' USING WSAA-QCMDEXC WSAA-QCMDEXC-LENGTH.

           OPEN OUTPUT ZIVR01.
      *
      * Report 2: DANH SACH HOA DON GTGT (for Internal usage):
      *
      **** MOVE BPRD-SYSTEM-PARAM04    TO WSAA-ZIV2-RUNID.
           MOVE BSSC-SCHEDULE-NUMBER   TO WSAA-ZIV2-JOBNO.
           MOVE BSPR-PROCESS-OCC-NUM   TO WSAA-THREAD-NUMBER.

           MOVE SPACES                 TO WSAA-QCMDEXC.
           STRING
               'OVRDBF FILE(ZIVR02) TOFILE('
                                       DELIMITED BY SIZE
                BPRD-RUN-LIBRARY       DELIMITED BY SPACES
               '/' WSAA-ZIV2-FN ') '
               'MBR(' WSAA-THREAD-MEMBER ')'
               ' SEQONLY(*YES 1000)'
                                       DELIMITED BY SIZE
                                       INTO WSAA-QCMDEXC
           END-STRING.

           CALL 'QCMDEXC' USING WSAA-QCMDEXC WSAA-QCMDEXC-LENGTH.

           OPEN OUTPUT ZIVR02.
      *
      * Report 3: TINH HINH SU DUNG HOA DON (SUMMARY REPORT-01 ROW)
      *
      **** MOVE BPRD-SYSTEM-PARAM04    TO WSAA-ZIV3-RUNID.
           MOVE BSSC-SCHEDULE-NUMBER   TO WSAA-ZIV3-JOBNO.
           MOVE BSPR-PROCESS-OCC-NUM   TO WSAA-THREAD-NUMBER.

           MOVE SPACES                 TO WSAA-QCMDEXC.
           STRING
               'OVRDBF FILE(ZIVR03) TOFILE('
                                       DELIMITED BY SIZE
                BPRD-RUN-LIBRARY       DELIMITED BY SPACES
               '/' WSAA-ZIV3-FN ') '
               'MBR(' WSAA-THREAD-MEMBER ')'
               ' SEQONLY(*YES 1000)'
                                       DELIMITED BY SIZE
                                       INTO WSAA-QCMDEXC
           END-STRING.

           CALL 'QCMDEXC' USING WSAA-QCMDEXC WSAA-QCMDEXC-LENGTH.

           OPEN OUTPUT ZIVR03.

      *
       1119-EXIT.
           EXIT.
      /
       1120-OPEN-INPUT-FILE SECTION.
      ******************************
       1121-START.
      *
      **** MOVE BPRD-SYSTEM-PARAM04    TO WSAA-ZIVR-RUNID.
           MOVE BSSC-SCHEDULE-NUMBER   TO WSAA-ZIVR-JOBNO.
           MOVE BSPR-PROCESS-OCC-NUM   TO WSAA-THREAD-NUMBER.

           MOVE SPACES                 TO WSAA-QCMDEXC.
           STRING
               'OVRDBF FILE(ZIVRPF) TOFILE('
                                       DELIMITED BY SIZE
                BPRD-RUN-LIBRARY       DELIMITED BY SPACES
               '/' WSAA-ZIVR-FN ') '
               'MBR(' WSAA-THREAD-MEMBER ')'
               ' SEQONLY(*YES 1000)'
                                       DELIMITED BY SIZE
                                       INTO WSAA-QCMDEXC
           END-STRING.

           CALL 'QCMDEXC' USING WSAA-QCMDEXC WSAA-QCMDEXC-LENGTH.

           OPEN INPUT ZIVRPF.
      *
       1129-EXIT.
           EXIT.
      /
       1200-WRITE-HEADERS SECTION.
      ****************************
       1201-START.
      *
      * Header 1:
      *
           MOVE SPACES                 TO WSAA-REC.
           MOVE WSAA-HEADER-H1         TO WSAA-REC.
           WRITE ZIVR01-REC       FROM WSAA-REC.
      *
      * Header 2:
      *
           MOVE SPACES                 TO WSAA-REC.
           MOVE WSAA-HEADER-H2         TO WSAA-REC.
           WRITE ZIVR02-REC       FROM WSAA-REC.
      *
      * Header 3:
      *
           MOVE SPACES                 TO WSAA-REC.
           MOVE WSAA-HEADER-H3         TO WSAA-REC.
           WRITE ZIVR03-REC       FROM WSAA-REC.

      *
       1219-EXIT.
           EXIT.
      /
       2000-READ-FILE SECTION.
      ************************
       2010-READ-FILE.
      *
           READ ZIVRPF NEXT RECORD
               AT END
                   IF WSAA-INVSEQ-START NOT = SPACES                    <GAPPH1>
                      PERFORM 3200-WRITE-REPORT3-SUMMARY                <GAPPH1>
                   END-IF                                               <GAPPH1>
                   MOVE ENDP           TO WSSP-EDTERROR
                   GO TO 2090-EXIT.
      *
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

           IF ZCODE    OF ZIVRPF       = SPACES
           OR CHDRNUM  OF ZIVRPF       = SPACES
           OR INVSEQ   OF ZIVRPF       = ZEROES
               MOVE SPACES             TO WSSP-EDTERROR
               GO TO 2590-EXIT
           END-IF.

           IF WSAA-FIRST-TIME          = 'Y'
               MOVE ZCODE  OF ZIVRPF   TO WSAA-SERIES
               MOVE INVSEQ OF ZIVRPF   TO WSAA-INVSEQ-START
               MOVE 'N'                TO WSAA-FIRST-TIME
           END-IF.

           MOVE INVSEQ  OF ZIVRPF      TO WSAA-INVSEQ-END.
      *
       2590-EXIT.
           EXIT.
      /
       3000-UPDATE SECTION.
      *********************
       3010-UPDATE.
      *
           IF STFLAG  OF ZIVRPF        = 'A'
               ADD 1                   TO WSAA-INV-USED
           ELSE
               ADD 1                   TO WSAA-INV-CANCEL
               MOVE  INVSEQ OF ZIVRPF  TO WSAA-INVSEQ
               STRING
                     WSAA-CANC-INVNO   DELIMITED BY '  '
                     WSAA-INVSEQ       DELIMITED BY SIZE
                     ', '              DELIMITED BY '  '
                                  INTO WSAA-CANC-INVNO
               END-STRING
           END-IF.

           ADD 1                       TO WSAA-INV-TOTAL.

           PERFORM 3100-PREPARE-DATA-REPORT1.
           PERFORM 3120-PREPARE-DATA-REPORT2.

      *
       3090-EXIT.
           EXIT.
      /
       3100-PREPARE-DATA-REPORT1 SECTION.
      ***********************************
       3101-START.
      *
           MOVE SEQNUM    OF ZIVRPF    TO D1-NO.
           MOVE ZCODE     OF ZIVRPF    TO D1-SERIES.
           MOVE INVSEQ    OF ZIVRPF    TO D1-SEQUENCE.
           MOVE DATEX     OF ZIVRPF    TO D1-ISS-DATE.
           MOVE SPACES                 TO D1-OWNERNAME.                 <PHE001>
           STRING
               LSURNAME   OF ZIVRPF    DELIMITED BY '  '
               ' '                     DELIMITED BY SIZE
               LGIVNAME   OF ZIVRPF    DELIMITED BY '  '
                                       INTO D1-OWNERNAME
           END-STRING.
           MOVE TAXIDNUM  OF ZIVRPF    TO D1-PO-TAXID.
           MOVE PDESC     OF ZIVRPF    TO D1-PROD-DESC.
           MOVE TOTLPREM  OF ZIVRPF    TO D1-TOT-AMT.
           MOVE DISCOUNT  OF ZIVRPF    TO D1-TOT-DISCOUNT.              <PHE001>

      * Write Report Data:

           MOVE SPACES                 TO WSAA-REC.
           MOVE WSAA-DETAIL-D1         TO WSAA-REC.
           WRITE ZIVR01-REC       FROM WSAA-REC.
      *
       3119-EXIT.
           EXIT.
      /
       3120-PREPARE-DATA-REPORT2 SECTION.
      ***********************************
       3121-START.
      *
           MOVE SEQNUM    OF ZIVRPF    TO D2-NO.
           MOVE ZCODE     OF ZIVRPF    TO D2-SERIES.
           MOVE INVSEQ    OF ZIVRPF    TO D2-SEQUENCE.
           MOVE DATEX     OF ZIVRPF    TO D2-ISS-DATE.
           MOVE SPACES                 TO D2-OWNERNAME.                 <PHE001>
           STRING
               LSURNAME   OF ZIVRPF    DELIMITED BY '  '
               ' '                     DELIMITED BY SIZE
               LGIVNAME   OF ZIVRPF    DELIMITED BY '  '
                                       INTO D2-OWNERNAME
           END-STRING.
           MOVE TAXIDNUM  OF ZIVRPF    TO D2-PO-TAXID.
           MOVE PDESC     OF ZIVRPF    TO D2-PROD-DESC.
           MOVE TOTLPREM  OF ZIVRPF    TO D2-TOT-AMT.
           MOVE DISCOUNT  OF ZIVRPF    TO D2-TOT-DISCOUNT.              <PHE001>

      * Additional Info:

           MOVE SPACES                 TO D2-VAT
                                          D2-NOTES.
           MOVE CHDRNUM   OF ZIVRPF    TO D2-CHDRNUM.
           MOVE STATCODE  OF ZIVRPF    TO D2-STATCODE.
           MOVE RPTDATE   OF ZIVRPF    TO D2-PTDATE.
           MOVE BILLFREQ  OF ZIVRPF    TO D2-BILLFREQ.
           IF  STFLAG     OF ZIVRPF    = 'A'
               MOVE WSAA-ACTIVE        TO D2-INV-STATUS
           ELSE
               MOVE WSAA-CANCEL        TO D2-INV-STATUS
           END-IF.

      * Write Report Data:

           MOVE SPACES                 TO WSAA-REC.
           MOVE WSAA-DETAIL-D2         TO WSAA-REC.
           WRITE ZIVR02-REC       FROM WSAA-REC.
      *
       3129-EXIT.
           EXIT.
      /
       3200-WRITE-REPORT3-SUMMARY SECTION.
      ************************************
       3201-START.
      *
           MOVE 1                      TO D3-NO.
           MOVE WSAA-SERIES            TO D3-SERIES.
           MOVE WSAA-INVSEQ-START      TO D3-SERI-FROM.
           MOVE WSAA-INVSEQ-END        TO D3-SERI-TO.
           MOVE WSAA-INV-USED          TO D3-NO-OF-USED.
           MOVE WSAA-INV-CANCEL        TO D3-NO-OF-CANC.
           MOVE WSAA-CANC-INVNO        TO D3-SERI-CANC.
           MOVE WSAA-INV-TOTAL         TO D3-TOTAL.

      * Write Report Data:

           MOVE SPACES                 TO WSAA-REC.
           MOVE WSAA-DETAIL-D3         TO WSAA-REC.
           WRITE ZIVR03-REC       FROM WSAA-REC.
      *
       3219-EXIT.
           EXIT.
      /
       3500-COMMIT SECTION.
      **********************
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
           CLOSE ZIVR01.
           CLOSE ZIVR02.
           CLOSE ZIVR03.
           MOVE O-K                    TO LSAA-STATUZ.
      *
       4090-EXIT.
           EXIT.
