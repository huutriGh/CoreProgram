       IDENTIFICATION DIVISION.
       PROGRAM-ID.     BV001.
      *
      *
      * Copyright 1986-2020, Computer Sciences Corporation.
      *
      *REMARKS.
      *                NEW PAYMENT REPORT LISTING
      *
      *   This program will create a report of all type of payment
      *   requests based on input parameters.
      *
      *   The report will generate a download file to PC.
      *   File name: ZPMRDLxxxx WHERE xxxx is the schedule no.
      *   Batch job: L2NEWPAYRP - Trans Code: BV02.
      *   Parameter screen: SV001.
      *
      ***********************************************************************
      *           AMENDMENT  HISTORY                                        *
      ***********************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....                               *
      *                                                                     *
      * 01/10/13  01/01   GAPPH1       Thanh Do                             *
      *           New Payment Report Listing.                               *
      *                                                                     *
      * 18/12/13  01/01   GAPPH2       Thanh Do                             *
      *           Check Extraction condition.                               *
      *                                                                     *
      * 09/05/14  01/01   PHLRMS       Phuong Le Dev                        *
      *           Recompile                                                 *
      *                                                                     *
      * 11/08/16  01/01   POP001       Phi Tran - IT DEV                    *
      *           Filter Data base on Office.                               *
      *                                                                     *
      * 07/05/18  01/01   PHFX30       Phi Tran - IT DEV                    *
      *           Loop TV023 to get Manager Case.                           *
      *                                                                     *
      * 18/12/18  01/01   PS036        Tuyet Huynh IT - DEV                 *
      *           Add fields for file : ZPMRPF                              *
      *           if Payee ID = Servicing Agent ID ,Authorize Note =
      *           'Authorize to IC' .Else note 'Authorize to Other'.
      *           ID is Security number.
      *                                                                     *
      * 20/09/19  01/01   PHFX49       Thanh Do                             *
      *           Fix Null IDDATE in Payment Report.                        *
      *                                                                     *
      * 04/11/20  01/01   CLM14        Van Bao Tuyen - IT                   *
      *           Increase length id place from 30 to 100 character.        *
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
           SELECT ZPMRPF               ASSIGN TO DATABASE-ZPMRPF.
      /
       DATA DIVISION.
       FILE SECTION.
       FD  ZPMRPF                          LABEL RECORDS STANDARD
           DATA RECORDS                    ARE ZPMRPF-REC.
       01  ZPMRPF-REC.
           COPY DDS-ALL-FORMATS            OF ZPMRPF.
      /
       WORKING-STORAGE SECTION.
      *
       01  WSAA-PROG                   PIC X(05) VALUE 'BV001'.
       01  WSAA-VERSION                PIC X(02) VALUE '01'.
      *
      *
       01  WSAA-COMMIT-CNT             PIC S9(08) COMP-3.
       01  WSAA-CYCLE-CNT              PIC S9(08) COMP-3.
       01  WSAA-CNT                    PIC 9(02).
       01  WSSP-EDTERROR               PIC X(04).
      *
       01  WSAA-NAMES-FUNC             PIC X(05)  VALUE 'LGNMN'.
      * The following line has been changed & reinstated
       01  WSAA-SHORTDESCA-LEN         PIC S9(02) COMP-3 VALUE 9.
       01  WSAA-SURNAME-LEN            PIC S9(02) COMP-3 VALUE 60.
      *
       01  WSAA-NAME.
           03  WSAA-SURNAME            PIC X(60).
           03  WSAA-GIVNAME            PIC X(60).
      *                                                                 <PHFX30>
       01  IDX                         PIC 9(03).                       <PHFX30>
       01  WSAA-TV023-FOUND            PIC X(01).                       <PHFX30>
      *
       01  FORMATS.
           03  BMSGREC                 PIC X(10) VALUE 'BMSGREC'.
           03  BPRDREC                 PIC X(10) VALUE 'BPRDREC'.
           03  BSPRREC                 PIC X(10) VALUE 'BSPRREC'.
           03  BSSCREC                 PIC X(10) VALUE 'BSSCREC'.
           03  BUPAREC                 PIC X(10) VALUE 'BUPAREC'.
           03  DESCREC                 PIC X(10) VALUE 'DESCREC'.
           03  ITEMREC                 PIC X(10) VALUE 'ITEMREC'.
           03  CLTSREC                 PIC X(10) VALUE 'CLTSREC'.
           03  CLEXREC                 PIC X(10) VALUE 'CLEXREC'.
      *    03  SANCREC                 PIC X(10) VALUE 'SANCREC'.
           03  CHEQRPTREC              PIC X(10) VALUE 'CHEQRPTREC'.
           03  USRDREC                 PIC X(10) VALUE 'USRDREC'.
           03  ACMVPMRREC              PIC X(10) VALUE 'ACMVPMRREC'.
           03  PREQREC                 PIC X(10) VALUE 'PREQREC'.
           03  BABRREC                 PIC X(10) VALUE 'BABRREC'.
           03  RMRKUPDREC              PIC X(10) VALUE 'RMRKUPDREC'.    <POP001>
           03  CHDRLIFREC              PIC X(10) VALUE 'CHDRLIFREC'.    <PS036>
           03  AGNTREC                 PIC X(10) VALUE 'AGNTREC'.       <PS036>
           03  ZCLEREC                 PIC X(10) VALUE 'ZCLEREC'.       <CLM14>
      *
       01  TABLES.
           03  T1692                   PIC X(06) VALUE 'T1692'.
           03  T1693                   PIC X(06) VALUE 'T1693'.
           03  T3629                   PIC X(06) VALUE 'T3629'.
           03  T3672                   PIC X(06) VALUE 'T3672'.
           03  T3695                   PIC X(06) VALUE 'T3695'.
           03  T3593                   PIC X(06) VALUE 'T3593'.
           03  T3688                   PIC X(06) VALUE 'T3688'.
           03  TV023                   PIC X(06) VALUE 'TV023'.         <POP001>
           03  TV022                   PIC X(06) VALUE 'TV022'.         <POP001>
      *
       01  CONTROL-TOTALS.
           03  CT01                    PIC 9(02) VALUE 01.
      *
       01  WSAA-OVERFLOW               PIC X(01) VALUE 'N'.
       88  NEW-PAGE-REQ                          VALUE 'Y'.
      *
       01  WSAA-USRPRF                 PIC X(10) VALUE SPACES.
       01  WSAA-TODAY                  PIC 9(08) VALUE 0.
       01  WSAA-TODAY-T                PIC X(10).
       01  WSAA-DATE-T                 PIC X(10).
       01  WSAA-SIZE                   PIC 9(03) VALUE 200.
       01  WSAA-IX                     PIC 9(03) VALUE 0.
       01  WSAA-IY                     PIC 9(03) VALUE 0.
       01  IX                          PIC 9(02).                       <POP001>
       01  WSAA-IDPLACE                PIC X(30).                       <CLM14>
       01  WSAA-IDPLACEXT              PIC X(70).                       <CLM14>
       01  WSAA-ID-PLACE               PIC X(30).                       <CLM14>
       01  FILLER  REDEFINES WSAA-ID-PLACE.                             <CLM14>
           03 WSAA-IDPLACE-VAL         PIC X(28).                       <CLM14>
           03 WSAA-IDPLACE-SPACE.                                       <CLM14>
              05 IDPLACE-1             PIC X(01).                       <CLM14>
              05 IDPLACE-2             PIC X(01).                       <CLM14>
       01  WSAA-SPACES                 PIC X(01) VALUE SPACES.          <CLM14>
      *
       01  AP                          PIC X(01) VALUE "'".
       01  WSAA-SQL-SMT                PIC X(400).
       01  FILLER REDEFINES WSAA-SQL-SMT.
           03  WSAA-SQL-LINE1          PIC X(100).
           03  WSAA-SQL-LINE2          PIC X(100).
           03  WSAA-SQL-LINE3          PIC X(100).
           03  WSAA-SQL-LINE4          PIC X(100).
      *                                                                 <POP001>
       01  WSAA-NOTES.                                                  <POP001>
           03  WSAA-NOTE               PIC X(61) OCCURS 12.             <POP001>
      *                                                                 <POP001>
       01  WSAA-WHERE-SMT              VALUE ALL '*'.
           03  TEST-CHAR               PIC X(01) OCCURS 200.
       01  FILLER REDEFINES WSAA-WHERE-SMT.
           03  TEST-COMPANY            PIC X(50).
           03  TEST-PAYMETHOD          PIC X(50).
           03  TEST-PAYSTATUS          PIC X(50).
           03  TEST-DATE               PIC X(50).
      *
       01  WSAA-EOF                    PIC X(01) VALUE 'N'.
       88  END-OF-FILE                           VALUE 'Y'.
      *
       01  WSAA-PAY-STATS              PIC X(02) VALUE '  '.
       88  PAY-ALLSTATUS               VALUE '  '.
       88  PAY-CREATED                 VALUE 'RQ'.
       88  PAY-APPROVED                VALUE 'AQ'.
       88  PAY-AUTHORISED              VALUE 'AU'.
       88  PAY-PROCESSED               VALUE 'PR'.
       88  PAY-CANCELLED               VALUE 'RC'.
      *
       01  WSAA-USERNUM                PIC 9(06) VALUE 0.
       01  WSAA-WOFFCODE               PIC X(05) VALUE SPACES.          <POP001>
       01  WSAA-DATE-BEG               PIC 9(08).
       01  FILLER REDEFINES WSAA-DATE-BEG.
           03  WSAA-DATE-BEG-CC        PIC 9(02).
           03  WSAA-DATE-BEG-YMD       PIC 9(06).

       01  WSAA-DATE-END               PIC 9(08).
       01  FILLER REDEFINES WSAA-DATE-END.
           03  WSAA-DATE-END-CC        PIC 9(02).
           03  WSAA-DATE-END-YMD       PIC 9(06).
      *
       01  INDIC-AREA.
           03  INDIC-TABLE  OCCURS 99  PIC 1 INDICATOR 1.
               88  IND-OFF  VALUE B'0'.
               88  IND-ON   VALUE B'1'.
      *
      *   SQL control data structure
      *
           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.

       01  FILLER.
           03  SQL-ERROR-CODE          PIC S9(09).
           03  FILLER REDEFINES SQL-ERROR-CODE.
               05  FILLER              PIC X(05).
               05  SQL-STATUZ          PIC X(04).
      *
       01  SQL-CHEQPF.
           05  SQL-CHEQREC.
               10  SQL-REQNCOY         PIC X(00001).
               10  SQL-PROCIND         PIC X(00002).
               10  SQL-REQNNO          PIC X(00009).
               10  SQL-REQNTYPE        PIC X(00001).
               10  SQL-PAYAMT          PIC S9(16)V9(02) COMP-3.
               10  SQL-REQNBCDE        PIC X(00002).
               10  SQL-CLNTCOY         PIC X(00001).
               10  SQL-CLNTNUM01       PIC X(00008).
               10  SQL-CLTTYPE01       PIC X(00001).
               10  SQL-CHEQNO          PIC X(00009).
               10  SQL-CHEQDUPN        PIC X(00001).
               10  SQL-BANKKEY         PIC X(00010).
               10  SQL-BANKACCKEY      PIC X(00020).
               10  SQL-REQUSER         PIC S9(06) COMP-3.
               10  SQL-REQDATE         PIC S9(08) COMP-3.
               10  SQL-AUTHUSER        PIC S9(06) COMP-3.
               10  SQL-AUTHDATE        PIC S9(08) COMP-3.
               10  SQL-APPRUSER        PIC S9(06) COMP-3.
               10  SQL-APPRDTE         PIC S9(08) COMP-3.
               10  SQL-USER            PIC S9(06) COMP-3.
               10  SQL-PAYDATE         PIC S9(08) COMP-3.
               10  SQL-BRANCH          PIC X(00002).
               10  SQL-REQNREV         PIC X(00009).
               10  SQL-REVERSAL-IND    PIC X(00001).
               10  SQL-ZPRNNO          PIC S9(02) COMP-3.
               10  SQL-TRDT            PIC S9(06) COMP-3.
               10  SQL-PAYCURR         PIC X(03).
      *
       01  WSAA-TABLE-NAME             PIC X(05).
       01  WSAA-TABLE-ITEM             PIC X(08).
       01  WSAA-QCMDEXC                PIC X(400).
       01  WSAA-QCMDEXC-LENGTH         PIC S9(10)V9(05) COMP-3
                                                     VALUE 400.
      *
       01  WSAA-ZPMR-FN.
           03  FILLER                  PIC X(04)   VALUE 'ZPMR'.
           03  WSAA-ZPMR-RUNID         PIC X(02).
           03  WSAA-ZPMR-JOBNO         PIC 9(04).

       01  WSAA-THREAD-MEMBER.
           03  FILLER                  PIC X(06)   VALUE 'THREAD'.
           03  WSAA-THREAD-NUMBER      PIC 9(03).
           03  FILLER                  PIC X.
      *
       01  WSAA-DATA.
           03  WSAA-CHDRNUM            PIC X(08).
           03  WSAA-PAYEE-NAME         PIC X(60).
           03  WSAA-PAYEE-IDNO         PIC X(24).
           03  WSAA-LONGDESC           PIC X(30).
           03  WSAA-BANKKEY            PIC X(10).
           03  WSAA-BANKDESC           PIC X(60).
           03  WSAA-BRANCHDESC         PIC X(60).                       <CLM14>
           03  WSAA-TRANSOFFICE        PIC X(50).                       <CLM14>
           03  WSAA-BANKCITY           PIC X(30).                       <CLM14>
           03  WSAA-BANKACCKEY         PIC X(20).
           03  WSAA-IDDATE             PIC 9(08).                       <CLM14>
           03  WSAA-IDDATE-T           PIC X(10).
      **** 03  WSAA-IDPLACE            PIC X(30).                       <CLM14>
           03  WSAA-IDPLACE-FULL       PIC X(100).                      <CLM14>
           03  WSAA-DATEFROM           PIC X(10).
           03  WSAA-DATETO             PIC X(10).
           03  WSAA-REPDATE            PIC X(10).
           03  WSAA-STATDETS           PIC X(30).
           03  WSAA-USERID             PIC X(10).
           03  WSAA-REMARK             PIC X(200).                      <POP001>
           03  WSAA-SACSCODE           PIC X(02).
           03  WSAA-SACSTYPE           PIC X(02).
           03  WSAA-CLNTNUM            PIC X(08).                       <PS036>
           03  WSAA-CLTNAME            PIC X(47).                       <PS036>
           03  WSAA-AGNT-ID            PIC X(24).                       <PS036>
           03  WSAA-PO-ID              PIC X(24).                       <PS036>
                                                                        <PS036>
       01  WSAA-AGNOTES-1              PIC X(25) VALUE                  <PS036>
                                           'Authorize to IC'.           <PS036>
       01  WSAA-AGNOTES-2              PIC X(25) VALUE                  <PS036>
                                           'Authorize to Other'.        <PS036>
       01  WSAA-SECUITYNO              PIC X(24).                       <PS036>
       01  WSAA-AGN                    PIC X(25).                       <PS036>
       01  WSAA-SANCKEY.
           COPY SANCKYR.
      *
      ***  COPY COMMONPAR.
      ***  COPY CONTROLREC.
      ***  COPY CONJOBREC.
           COPY CONLOGREC.
           COPY CONERRREC.
           COPY TV023REC.                                               <POP001>
           COPY TV022REC.                                               <POP001>
           COPY VARCOM.
      *    COPY DBCSTRNCPY.
           COPY DBCSTRCPY2.
           COPY NAMADRSREC.
           COPY SPCOUTREC.
           COPY CONTOTREC.
           COPY DATCON1REC.
           COPY CLTSSKM.
           COPY CLEXSKM.
           COPY USRDSKM.
           COPY USRNAMEREC.
      /
           COPY SFTLOCKREC.
           COPY SYSERRREC.
           COPY SMTPFXCPY.
           COPY BATCDORREC.
           COPY BATCUPREC.
           COPY BPRDSKM.
           COPY BSPRSKM.
           COPY BSSCSKM.
           COPY BUPASKM.
           COPY DESCSKM.
           COPY ITEMSKM.
      /
           COPY RMRKUPDSKM.                                             <POP001>
           COPY CHEQRPTSKM.
           COPY PV001PAR.
           COPY ZRGTUSRREC.
           COPY ACMVPMRSKM.
           COPY PREQSKM.
           COPY BABRSKM.
           COPY CHDRLIFSKM.                                             <PS036>
           COPY AGNTSKM.                                                <PS036>
           COPY ZCLESKM.                                                <CLM14>
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

           COPY MAINB.
      /
       500-SQL-ERROR SECTION.
      ***********************
      *
       510-CALL-SYSTEM-ERROR.
      *
           MOVE SQLCODE                TO SQL-ERROR-CODE.
           MOVE SQL-STATUZ             TO SYSR-SYSERR-STATUZ.
           PERFORM 600-FATAL-ERROR.
      *
       590-EXIT-SQL-ERROR.
           EXIT.
      /
       800-PREPARE-CONDITION SECTION.
      *******************************
       801-START.
      *
           STRING 'REQNCOY = ' AP BSPR-COMPANY AP
                  DELIMITED BY SIZE INTO TEST-COMPANY.

           IF  PV001-PYMCHQ            NOT = SPACES
               STRING ' AND '
                      'REQNTYPE = ' AP PV001-PYMCHQ AP
                      DELIMITED BY SIZE INTO TEST-PAYMETHOD
           END-IF.

           IF  PV001-PAYSTATZ          NOT = SPACES
               STRING ' AND '
                      'PROCIND = ' AP PV001-PAYSTATZ AP                 <GAPPH2>
                      DELIMITED BY SIZE INTO TEST-PAYSTATUS
           END-IF.
      *
      *  Remove all '*' from the test condition.
      *
           MOVE 1                          TO WSAA-IY.
           PERFORM VARYING WSAA-IX         FROM 1 BY 1
                   UNTIL WSAA-IX           > WSAA-SIZE
              IF  TEST-CHAR (WSAA-IX)      NOT = '*'
                  MOVE TEST-CHAR (WSAA-IX) TO TEST-CHAR (WSAA-IY)
                  ADD 1                    TO WSAA-IY
              END-IF
           END-PERFORM.
      *
           PERFORM VARYING WSAA-IX         FROM WSAA-IY BY 1
                   UNTIL WSAA-IX           > WSAA-SIZE
              MOVE '*'                     TO TEST-CHAR (WSAA-IX)
           END-PERFORM.

           MOVE SPACES                     TO WSAA-SQL-SMT.
           STRING 'SELECT REQNCOY, '
                         'PROCIND, '
                         'REQNNO, '
                         'REQNTYPE, '
                         'PAYAMT, '
                         'REQNBCDE, '
                         'CLNTCOY, '
                         'CLNTNUM01, '
                         'CLTTYPE01, '
                         'CHEQNO, '
                         'CHEQDUPN, '
                         'BANKKEY, '
                         'BANKACCKEY, '
                         'REQUSER, '
                         'REQDATE, '
                         'AUTHUSER, '
                         'AUTHDATE, '
                         'APPRUSER, '
                         'APPRDTE, '
                         '"USER", '
                         'PAYDATE, '
                         'BRANCH, '
                         'REQNREV, '
                         'REVIND, '
                         'ZPRNNO, '
                         'TRDT, '
                         'PAYCURR '
                         'FROM CHEQRPT '
                  'WHERE ' DELIMITED BY SIZE
                         WSAA-WHERE-SMT DELIMITED BY '*'
                   INTO  WSAA-SQL-SMT
           END-STRING.

           DISPLAY WSAA-SQL-LINE1.
           DISPLAY WSAA-SQL-LINE2.
           DISPLAY WSAA-SQL-LINE3.
           DISPLAY WSAA-SQL-LINE4.

      *
       819-EXIT.
           EXIT.
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
           EXEC SQL
              WHENEVER SQLERROR GO TO 500-SQL-ERROR
           END-EXEC.

           MOVE O-K                    TO WSSP-EDTERROR.
           MOVE BUPA-PARMAREA          TO PV001-PARM-RECORD.
      *
           MOVE TDAY                   TO DTC1-FUNCTION.
           CALL 'DATCON1'           USING DTC1-DATCON1-REC.
           MOVE DTC1-EXT-DATE          TO WSAA-TODAY.

           MOVE CONV                   TO DTC1-FUNCTION.
           MOVE BSSC-EFFECTIVE-DATE    TO DTC1-INT-DATE.
           CALL 'DATCON1'           USING DTC1-DATCON1-REC.
           MOVE DTC1-EXT-DATE          TO WSAA-TODAY-T.
      *
       1060-DEFINE-CURSOR.
      *
      *  Define the query required by declaring a cursor
      *
      *
           PERFORM 1100-OPEN-DOWNLOAD-FILE.
           PERFORM  800-PREPARE-CONDITION.
      *
      *  Prepare cursor for query
      *
           EXEC SQL
             PREPARE S1 FROM :WSAA-SQL-SMT
           END-EXEC.
      *
      *  Define the query required by declaring a cursor
      *
           EXEC SQL
              DECLARE CHEQPF1 CURSOR FOR S1
           END-EXEC.
      *
      *   Open the cursor (this runs the query)
      *
           EXEC SQL
              OPEN CHEQPF1
           END-EXEC.
      *
       1090-EXIT.
           EXIT.
      /
       1100-OPEN-DOWNLOAD-FILE SECTION.
      *********************************
       1101-START.
      *
           MOVE BPRD-SYSTEM-PARAM04    TO WSAA-ZPMR-RUNID.
           MOVE BSSC-SCHEDULE-NUMBER   TO WSAA-ZPMR-JOBNO.
           MOVE BSPR-PROCESS-OCC-NUM   TO WSAA-THREAD-NUMBER.

           STRING
               'OVRDBF FILE(ZPMRPF) TOFILE('
                                       DELIMITED BY SIZE
                BPRD-RUN-LIBRARY       DELIMITED BY SPACES
               '/' WSAA-ZPMR-FN ') '
               'MBR(' WSAA-THREAD-MEMBER ')'
               ' SEQONLY(*YES 1000)'
                                       DELIMITED BY SIZE
                                       INTO WSAA-QCMDEXC
           END-STRING.

           CALL 'QCMDEXC' USING WSAA-QCMDEXC WSAA-QCMDEXC-LENGTH.

           OPEN OUTPUT ZPMRPF.

      *
       1190-EXIT.
           EXIT.
      /
       2000-READ-FILE SECTION.
      ************************
      *
       2010-READ-FILE.
      *
      *   Fetch record
      *
           EXEC SQL
             WHENEVER NOT FOUND GO TO 2080-END-OF-FILE
           END-EXEC.

           EXEC SQL
             FETCH CHEQPF1 INTO :SQL-CHEQREC
           END-EXEC.

           GO TO 2090-EXIT.
      *
       2080-END-OF-FILE.
      *
           MOVE ENDP                   TO WSSP-EDTERROR.
      *
       2090-EXIT.
           EXIT.
      /
       2500-EDIT SECTION.
      *******************
      *
       2510-EDIT.
      *
      * Check record is required for processing.
      * Softlock the record if it is to be updated.
      *
           MOVE O-K                    TO WSSP-EDTERROR.
           MOVE 'N'                    TO WSAA-TV023-FOUND.             <PHFX30>
           MOVE ZERO                   TO IDX.                          <PHFX30>

           IF SQL-REQNNO                = SPACES
               MOVE SPACES             TO WSSP-EDTERROR
               GO TO 2590-EXIT
           END-IF.

           IF PV001-PROCUSR            NOT = SPACES
               PERFORM 2600-GET-USER-NUMBER
           END-IF.

           IF PV001-PAYSTATZ           NOT = SPACES
               MOVE PV001-PAYSTATZ     TO WSAA-PAY-STATS
           ELSE
               MOVE SQL-PROCIND        TO WSAA-PAY-STATS
           END-IF.

      * If having UserID and with/without Payment Status:
      * (If Payment Status <> SPACES, already filttered by SQL cmd).

           IF PV001-PROCUSR            NOT = SPACES
      * Payment Created cases:
              IF PAY-CREATED
              AND (SQL-REQUSER         NOT = WSAA-USERNUM)
                 MOVE SPACES           TO WSSP-EDTERROR
                 GO TO 2590-EXIT
              END-IF

      * Payment Approved cases:
              IF PAY-APPROVED
              AND (SQL-APPRUSER        NOT = WSAA-USERNUM)
                 MOVE SPACES           TO WSSP-EDTERROR
                 GO TO 2590-EXIT
              END-IF

      * Payment Authorised cases:
              IF PAY-AUTHORISED
              AND (SQL-AUTHUSER        NOT = WSAA-USERNUM)
                 MOVE SPACES           TO WSSP-EDTERROR
                 GO TO 2590-EXIT
              END-IF

      * Payment Cancelled cases:
              IF PAY-CANCELLED
              AND (SQL-USER            NOT = WSAA-USERNUM)
                 MOVE SPACES           TO WSSP-EDTERROR
                 GO TO 2590-EXIT
              END-IF

      * Payment Processed cases:
              IF PAY-PROCESSED
              AND (SQL-AUTHUSER        NOT = WSAA-USERNUM)
                 MOVE SPACES           TO WSSP-EDTERROR
                 GO TO 2590-EXIT
              END-IF
           END-IF.
      *
      * If having Date Range:
      *
           MOVE PV001-DATEFRM          TO WSAA-DATE-BEG.
           MOVE PV001-DATETO           TO WSAA-DATE-END.

           IF PV001-DATEFRM            NOT = VRCM-MAX-DATE
           AND PV001-DATEFRM           NOT = ZEROES
           AND PV001-DATETO            NOT = VRCM-MAX-DATE
           AND PV001-DATETO            NOT = ZEROES
      * Payment Created cases:
              IF PAY-CREATED
              AND NOT (SQL-REQDATE     >= PV001-DATEFRM
                   AND SQL-REQDATE     <= PV001-DATETO )
                 MOVE SPACES           TO WSSP-EDTERROR
                 GO TO 2590-EXIT
              END-IF

      * Payment Approved cases:
              IF PAY-APPROVED
              AND NOT (SQL-APPRDTE     >= PV001-DATEFRM
                   AND SQL-APPRDTE     <= PV001-DATETO )
                 MOVE SPACES           TO WSSP-EDTERROR
                 GO TO 2590-EXIT
              END-IF

      * Payment Authorised cases:
              IF PAY-AUTHORISED
              AND NOT (SQL-AUTHDATE     >= PV001-DATEFRM
                   AND SQL-AUTHDATE     <= PV001-DATETO )
                 MOVE SPACES           TO WSSP-EDTERROR
                 GO TO 2590-EXIT
              END-IF

      * Payment Cancelled cases:
              IF PAY-CANCELLED
              AND NOT (SQL-TRDT         >= WSAA-DATE-BEG-YMD
                   AND SQL-TRDT         <= WSAA-DATE-END-YMD )
                 MOVE SPACES           TO WSSP-EDTERROR
                 GO TO 2590-EXIT
              END-IF

      * Payment Processed cases:
              IF PAY-PROCESSED
              AND NOT (SQL-AUTHDATE     >= PV001-DATEFRM
                   AND SQL-AUTHDATE     <= PV001-DATETO )
                 MOVE SPACES           TO WSSP-EDTERROR
                 GO TO 2590-EXIT
              END-IF
           END-IF.
P     *                                                                 <POP001>
      * If having Office.                                               <POP001>
      *                                                                 <POP001>
           IF PV001-WOFFCODE        NOT = SPACES                        <PHFX30>
              PERFORM 3300-GET-USERID                                   <POP001>
              INITIALIZE                  ITEM-PARAMS                   <POP001>
              MOVE BSPR-COMPANY        TO ITEM-ITEMCOY                  <POP001>
              MOVE 'IT'                TO ITEM-ITEMPFX                  <POP001>
              MOVE TV023               TO ITEM-ITEMTABL                 <POP001>
              MOVE WSAA-USERID         TO ITEM-ITEMITEM                 <POP001>
              MOVE ITEMREC             TO ITEM-FORMAT                   <POP001>
              MOVE READR               TO ITEM-FUNCTION                 <POP001>
                                                                        <POP001>
              CALL  'ITEMIO'        USING ITEM-PARAMS                   <POP001>
                                                                        <POP001>
              IF ITEM-STATUZ        NOT = O-K                           <POP001>
              AND ITEM-STATUZ       NOT = MRNF                          <POP001>
                  MOVE ITEM-PARAMS     TO SYSR-PARAMS                   <POP001>
                  PERFORM 600-FATAL-ERROR                               <POP001>
              END-IF                                                    <POP001>
      *                                                                 <PHFX30>
              IF ITEM-STATUZ            = O-K                           <PHFX30>
                 MOVE ITEM-GENAREA     TO TV023-TV023-REC               <PHFX30>
                 PERFORM A1000-LOOP-TV023                               <PHFX30>
                 IF WSAA-TV023-FOUND   NOT = 'Y'                        <PHFX30>
                    MOVE SPACES        TO WSSP-EDTERROR                 <PHFX30>
                    GO TO 2590-EXIT                                     <PHFX30>
                 END-IF                                                 <PHFX30>
              ELSE                                                      <PHFX30>
                 MOVE SPACES           TO WSSP-EDTERROR                 <PHFX30>
                 GO TO 2590-EXIT                                        <PHFX30>
              END-IF                                                    <PHFX30>
           END-IF.                                                      <PHFX30>
      *
       2590-EXIT.
           EXIT.
      /
       2600-GET-USER-NUMBER SECTION.
      ******************************
       2601-START.
      *
           MOVE ZEROES                 TO WSAA-USERNUM.
           INITIALIZE                  ZRGETUSR-REC.
           MOVE 'USID'                 TO ZRGETUSR-FUNCTION.
           MOVE O-K                    TO ZRGETUSR-STATUZ.
           MOVE PV001-PROCUSR          TO ZRGETUSR-USERID.
           CALL 'ZRGETUSR'       USING ZRGETUSR-REC.

           IF ZRGETUSR-STATUZ          NOT = O-K
               MOVE ZRGETUSR-REC       TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE ZRGETUSR-USERNUM       TO WSAA-USERNUM.
      *
       2619-EXIT.
           EXIT.
      /
       3000-UPDATE SECTION.
      *********************
       3010-UPDATE.
      *
           PERFORM 3100-GET-POLICY-NUMBER.
           PERFORM 3140-CONVERT-DATE.
           PERFORM 3200-GET-PAYEE-NAME-ID.

           MOVE SQL-REQNNO             TO REQNNO     OF ZPMRPF.
           MOVE WSAA-CHDRNUM           TO CHDRNUM    OF ZPMRPF.
           MOVE WSAA-PAYEE-NAME        TO PAYEENME   OF ZPMRPF.
           MOVE WSAA-PAYEE-IDNO        TO SECUITYNO  OF ZPMRPF.
           MOVE SQL-PAYAMT             TO XAMTPAY    OF ZPMRPF.

           MOVE WSAA-SACSTYPE          TO SACSTYPE   OF ZPMRPF.
           MOVE WSAA-SACSTYPE          TO WSAA-TABLE-ITEM.
           MOVE T3695                  TO WSAA-TABLE-NAME.
           PERFORM 3220-GET-ITEM-DESC.
           MOVE WSAA-LONGDESC          TO DESCR      OF ZPMRPF.
PHI                                                                     <POP001>
PHI        MOVE WSAA-SACSCODE          TO SACSCODE   OF ZPMRPF.         <POP001>

           MOVE SQL-REQNTYPE           TO PAYMMETH   OF ZPMRPF.
           MOVE SQL-REQNTYPE           TO WSAA-TABLE-ITEM.
           MOVE T3672                  TO WSAA-TABLE-NAME.
           PERFORM 3220-GET-ITEM-DESC.
           MOVE WSAA-LONGDESC          TO PYMDESC    OF ZPMRPF.

           MOVE SQL-REQNBCDE           TO BANKCODE   OF ZPMRPF.
           MOVE SQL-BANKKEY            TO BANKKEY    OF ZPMRPF.
           IF SQL-BANKKEY              NOT = SPACES
               PERFORM 3230-GET-BANKDESC
           ELSE
               PERFORM 3240-GET-BANKCODE-DESC
           END-IF.
           MOVE WSAA-BANKDESC          TO BABRDC     OF ZPMRPF.
           MOVE WSAA-BRANCHDESC        TO BRANCHDC   OF ZPMRPF.         <CLM14>
           MOVE WSAA-TRANSOFFICE       TO TRANSOFF   OF ZPMRPF.         <CLM14>
           MOVE WSAA-BANKCITY          TO BANKCITY   OF ZPMRPF.         <CLM14>

           MOVE SQL-BANKACCKEY         TO BANKACCKEY OF ZPMRPF.
           MOVE WSAA-IDDATE-T          TO TEXTDATE   OF ZPMRPF.
      **** MOVE WSAA-IDPLACE           TO IDPLACE    OF ZPMRPF.         <CLM14>
           MOVE WSAA-IDPLACE-FULL      TO ZIDPLACE   OF ZPMRPF.         <CLM14>
           MOVE WSAA-DATEFROM          TO DATECFROM  OF ZPMRPF.
           MOVE WSAA-DATETO            TO DATECTO    OF ZPMRPF.
           MOVE WSAA-TODAY-T           TO REPDATE    OF ZPMRPF.

           MOVE SQL-PROCIND            TO WSAA-TABLE-ITEM.
           MOVE T3593                  TO WSAA-TABLE-NAME.
           PERFORM 3220-GET-ITEM-DESC.
           MOVE WSAA-LONGDESC          TO STATDETS   OF ZPMRPF.

           PERFORM 3300-GET-USERID.
           MOVE WSAA-USERID            TO PROCUSR    OF ZPMRPF.
           MOVE SQL-AUTHDATE           TO AUTHDATE   OF ZPMRPF.         <POP001>
                                                                        <POP001>
           IF PV001-WOFFCODE        NOT = SPACES                        <POP001>
              MOVE PV001-WOFFCODE      TO WOFFCODE   OF  ZPMRPF         <POP001>
           ELSE                                                         <POP001>
              INITIALIZE                  ITEM-PARAMS                   <POP001>
              MOVE BSPR-COMPANY        TO ITEM-ITEMCOY                  <POP001>
              MOVE 'IT'                TO ITEM-ITEMPFX                  <POP001>
              MOVE TV023               TO ITEM-ITEMTABL                 <POP001>
              MOVE WSAA-USERID         TO ITEM-ITEMITEM                 <POP001>
              MOVE ITEMREC             TO ITEM-FORMAT                   <POP001>
              MOVE READR               TO ITEM-FUNCTION                 <POP001>
                                                                        <POP001>
              CALL  'ITEMIO'        USING ITEM-PARAMS                   <POP001>
                                                                        <POP001>
              IF ITEM-STATUZ        NOT = O-K                           <POP001>
              AND ITEM-STATUZ       NOT = MRNF                          <POP001>
                  MOVE ITEM-STATUZ     TO SYSR-STATUZ                   <POP001>
                  MOVE ITEM-PARAMS     TO SYSR-PARAMS                   <POP001>
                  PERFORM 600-FATAL-ERROR                               <POP001>
              END-IF                                                    <POP001>
              IF   ITEM-STATUZ          = O-K                           <POP001>
                   MOVE ITEM-GENAREA        TO TV023-TV023-REC          <POP001>
                   MOVE TV023-WOFFCODE      TO WOFFCODE  OF  ZPMRPF     <POP001>
              ELSE                                                      <POP001>
                   MOVE SPACES              TO WOFFCODE  OF  ZPMRPF     <POP001>
           END-IF.                                                      <POP001>
                                                                        <POP001>
           MOVE WOFFCODE               TO WSAA-TABLE-ITEM.              <POP001>
           MOVE TV022                  TO WSAA-TABLE-NAME.              <POP001>
           PERFORM 3220-GET-ITEM-DESC.                                  <POP001>
           IF DESC-STATUZ              = O-K                            <POP001>
              MOVE DESC-LONGDESC       TO WSAA-LONGDESC                 <POP001>
              MOVE WSAA-LONGDESC       TO OFFDES     OF ZPMRPF          <POP001>
           ELSE                                                         <POP001>
              MOVE SPACES              TO OFFDES     OF ZPMRPF          <POP001>
           END-IF.                                                      <POP001>
                                                                        <POP001>
           PERFORM 3400-GET-REMARK.                                     <POP001>
           MOVE WSAA-NOTES             TO PMNOTE     OF ZPMRPF.         <POP001>

           IF PV001-PYMCHQ             NOT = '0'                        <PS036>
              MOVE SPACES              TO OWNNAM     OF ZPMRPF          <PS036>
                                          ZRSECNO    OF ZPMRPF          <PS036>
                                          SERVAGNAM  OF ZPMRPF          <PS036>
                                          AGTLICNO   OF ZPMRPF          <PS036>
                                          AGNOTES    OF ZPMRPF          <PS036>
           ELSE                                                         <PS036>
              PERFORM 3800-METHOD-CASH                                  <PS036>
           END-IF.                                                      <PS036>
                                                                        <PS036>
           WRITE ZPMRPF-REC.
      *
       3090-EXIT.
           EXIT.
      /
       3100-GET-POLICY-NUMBER SECTION.
      ********************************
       3101-START.
      *
           IF SQL-PROCIND              NOT = 'PR'
               PERFORM 3120-READ-PREQ-FILE
           ELSE
               PERFORM 3130-READ-ACMVPMR-FILE
           END-IF.
      *
       3119-EXIT.
           EXIT.
      /
       3120-READ-PREQ-FILE SECTION.
      *****************************
       3121-START.
      *
           MOVE SPACES                 TO WSAA-CHDRNUM
                                          WSAA-SACSTYPE
                                          WSAA-SACSCODE.

           INITIALIZE                  PREQ-PARAMS.
           MOVE 'RQ'                   TO PREQ-RDOCPFX.
           MOVE SQL-REQNCOY            TO PREQ-RDOCCOY.
           MOVE SQL-REQNNO             TO PREQ-RDOCNUM.
           MOVE 1                      TO PREQ-JRNSEQ.
           MOVE PREQREC                TO PREQ-FORMAT.
           MOVE READR                  TO PREQ-FUNCTION.

           CALL 'PREQIO'               USING PREQ-PARAMS.

           IF PREQ-STATUZ              NOT = O-K
           AND                         NOT = MRNF
               MOVE PREQ-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF PREQ-STATUZ               = O-K
               MOVE PREQ-RLDGACCT       TO WSAA-CHDRNUM
               MOVE PREQ-SACSTYP        TO WSAA-SACSTYPE
               MOVE PREQ-SACSCODE       TO WSAA-SACSCODE
           END-IF.
      *
       3129-EXIT.
           EXIT.
      /
       3130-READ-ACMVPMR-FILE SECTION.
      ********************************
       3131-START.
      *
           MOVE SPACES                 TO WSAA-CHDRNUM
                                          WSAA-SACSTYPE
                                          WSAA-SACSCODE.

           INITIALIZE                  ACMVPMR-PARAMS.
           MOVE 'RQ'                   TO ACMVPMR-RDOCPFX.
           MOVE SQL-REQNCOY            TO ACMVPMR-RDOCCOY.
           MOVE SQL-REQNNO             TO ACMVPMR-RDOCNUM.
           MOVE 1                      TO ACMVPMR-JRNSEQ.
           MOVE ACMVPMRREC             TO ACMVPMR-FORMAT.
           MOVE READR                  TO ACMVPMR-FUNCTION.

           CALL 'ACMVPMRIO'            USING ACMVPMR-PARAMS.

           IF ACMVPMR-STATUZ           NOT = O-K
           AND                         NOT = MRNF
               MOVE ACMVPMR-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF ACMVPMR-STATUZ           = O-K
               MOVE ACMVPMR-RLDGACCT   TO WSAA-CHDRNUM
               MOVE ACMVPMR-SACSCODE   TO WSAA-SACSCODE
               MOVE ACMVPMR-SACSTYP    TO WSAA-SACSTYPE
           END-IF.
      *
       3139-EXIT.
           EXIT.
      /
       3140-CONVERT-DATE SECTION.
      ***************************
       3141-START.
      *
           MOVE SPACES                 TO WSAA-DATEFROM
                                          WSAA-DATETO.

           IF PV001-DATEFRM            NOT = VRCM-MAX-DATE
           AND PV001-DATEFRM           NOT = ZEROES
              MOVE CONV                TO DTC1-FUNCTION
              MOVE PV001-DATEFRM       TO DTC1-INT-DATE
              CALL 'DATCON1'        USING DTC1-DATCON1-REC
              MOVE DTC1-EXT-DATE       TO WSAA-DATEFROM
           END-IF.

           IF PV001-DATETO             NOT = VRCM-MAX-DATE
           AND PV001-DATETO            NOT = ZEROES
              MOVE CONV                TO DTC1-FUNCTION
              MOVE PV001-DATETO        TO DTC1-INT-DATE
              CALL 'DATCON1'        USING DTC1-DATCON1-REC
              MOVE DTC1-EXT-DATE       TO WSAA-DATETO
           END-IF.
      *
       3149-EXIT.
           EXIT.
      /
       3200-GET-PAYEE-NAME-ID SECTION.
      ********************************
       3201-START.
      *
           INITIALIZE                  CLTS-DATA-KEY.
           MOVE 'CN'                   TO CLTS-CLNTPFX.
           MOVE SQL-CLNTCOY            TO CLTS-CLNTCOY.
           MOVE SQL-CLNTNUM01          TO CLTS-CLNTNUM.
           MOVE READR                  TO CLTS-FUNCTION.
           MOVE CLTSREC                TO CLTS-FORMAT.

           CALL 'CLTSIO'               USING CLTS-PARAMS.

           IF  CLTS-STATUZ             NOT = O-K
               MOVE CLTS-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.
      *
           MOVE SPACES                 TO NMAD-NAMADRS-REC.
      *
           MOVE BSSC-LANGUAGE          TO NMAD-LANGUAGE.
           MOVE CLTS-CLNTNUM           TO NMAD-CLNT-NUMBER.
           MOVE CLTS-CLNTPFX           TO NMAD-CLNT-PREFIX.
           MOVE CLTS-CLNTCOY           TO NMAD-CLNT-COMPANY.
      *
           MOVE WSAA-NAMES-FUNC        TO NMAD-FUNCTION.
           CALL 'NAMADRS'              USING  NMAD-NAMADRS-REC.
           IF NMAD-STATUZ              NOT = O-K
              MOVE NMAD-STATUZ         TO CONR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.
           MOVE NMAD-NAME              TO WSAA-PAYEE-NAME.
           MOVE CLTS-SECUITYNO         TO WSAA-PAYEE-IDNO.
      *
      * Read Extra details:
      *
           MOVE SPACES                 TO WSAA-SPACES.                  <CLM14>
           INITIALIZE                  CLEX-PARAMS.
           MOVE CLTS-CLNTPFX           TO CLEX-CLNTPFX.
           MOVE CLTS-CLNTCOY           TO CLEX-CLNTCOY.
           MOVE CLTS-CLNTNUM           TO CLEX-CLNTNUM.
           MOVE CLEXREC                TO CLEX-FORMAT.
           MOVE READR                  TO CLEX-FUNCTION.

           CALL 'CLEXIO'               USING CLEX-PARAMS.

           IF CLEX-STATUZ              NOT = O-K
           AND                         NOT = MRNF
               MOVE CLEX-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF CLEX-STATUZ              = O-K
             MOVE CLEX-IDPLACE         TO WSAA-IDPLACE
                                          WSAA-ID-PLACE                 <CLM14>
      ****   MOVE CLEX-IDDATE          TO WSAA-IDDATE                   <PHFX49>
             IF CLTS-CLTTYPE           = 'C'                            <PHFX49>
                IF CLTS-CLTDOB         IS NUMERIC                       <PHFX49>
                AND CLTS-CLTDOB        NOT = ZEROES                     <PHFX49>
                AND CLTS-CLTDOB        NOT = 99999999                   <PHFX49>
                   MOVE CLTS-CLTDOB    TO WSAA-IDDATE                   <PHFX49>
                ELSE                                                    <PHFX49>
                   MOVE WSAA-TODAY     TO WSAA-IDDATE                   <PHFX49>
                END-IF                                                  <PHFX49>
             ELSE                                                       <PHFX49>
                MOVE CLEX-IDDATE       TO WSAA-IDDATE                   <PHFX49>
             END-IF                                                     <PHFX49>

             IF CLEX-IDDATE            IS NUMERIC
             AND CLEX-IDDATE           NOT = VRCM-MAX-DATE
             AND CLEX-IDDATE           NOT = ZEROES
                INITIALIZE             DTC1-DATCON1-REC
                MOVE O-K               TO DTC1-STATUZ
                MOVE WSAA-IDDATE       TO DTC1-INT-DATE
                MOVE 'CONV'            TO DTC1-FUNCTION
                CALL 'DATCON1'         USING DTC1-DATCON1-REC
                IF DTC1-STATUZ         NOT = O-K
                   STRING WSAA-PROG DTC1-DATCON1-REC
                       DELIMITED BY SIZE   INTO SYSR-PARAMS
                   END-STRING
                   PERFORM 600-FATAL-ERROR
                END-IF
                MOVE DTC1-EXT-DATE     TO WSAA-IDDATE-T
             ELSE
                MOVE '??/??/????'      TO WSAA-IDDATE-T
             END-IF
                                                                        <CLM14>
             PERFORM A3200-READ-ZCLE                                    <CLM14>
             IF WSAA-IDPLACE-SPACE        = '  '                        <CLM14>
                STRING WSAA-ID-PLACE     DELIMITED BY '  '              <CLM14>
                       WSAA-SPACES       DELIMITED BY SIZE              <CLM14>
                       WSAA-IDPLACEXT    DELIMITED BY SIZE              <CLM14>
                                INTO  WSAA-IDPLACE-FULL                 <CLM14>
                END-STRING                                              <CLM14>
             ELSE                                                       <CLM14>
                IF IDPLACE-2              = ' '                         <CLM14>
                   STRING WSAA-ID-PLACE     DELIMITED BY SIZE           <CLM14>
                          WSAA-IDPLACEXT    DELIMITED BY SIZE           <CLM14>
                                INTO  WSAA-IDPLACE-FULL                 <CLM14>
                   END-STRING                                           <CLM14>
                ELSE                                                    <CLM14>
                   STRING WSAA-ID-PLACE     DELIMITED BY SIZE           <CLM14>
                          WSAA-SPACES       DELIMITED BY SIZE           <CLM14>
                          WSAA-IDPLACEXT    DELIMITED BY SIZE           <CLM14>
                                   INTO  WSAA-IDPLACE-FULL              <CLM14>
                   END-STRING                                           <CLM14>
                END-IF                                                  <CLM14>
             END-IF                                                     <CLM14>
           ELSE
               MOVE SPACES             TO WSAA-IDDATE-T
               MOVE SPACES             TO WSAA-IDPLACE
               MOVE SPACES             TO WSAA-IDPLACE-FULL             <CLM14>
           END-IF.
      *
       3219-EXIT.
           EXIT.
      /
       3220-GET-ITEM-DESC SECTION.
      ****************************
       3221-START.
      *
           MOVE SPACES                 TO WSAA-LONGDESC.
           INITIALIZE                  DESC-PARAMS.
           MOVE 'IT'                   TO DESC-DESCPFX.
           MOVE SQL-REQNCOY            TO DESC-DESCCOY.
           MOVE WSAA-TABLE-NAME        TO DESC-DESCTABL.
           MOVE WSAA-TABLE-ITEM        TO DESC-DESCITEM.
           MOVE SPACES                 TO DESC-ITEMSEQ.
           MOVE BSSC-LANGUAGE          TO DESC-LANGUAGE.
           MOVE DESCREC                TO DESC-FORMAT.
           MOVE READR                  TO DESC-FUNCTION.

           CALL 'DESCIO'               USING DESC-PARAMS.

           IF DESC-STATUZ              NOT = O-K
           AND                         NOT = MRNF
               MOVE DESC-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF DESC-STATUZ              = O-K
               MOVE DESC-LONGDESC      TO WSAA-LONGDESC
           ELSE
               MOVE ALL '?'            TO WSAA-LONGDESC
           END-IF.

      *
       3229-EXIT.
           EXIT.
      /
       3230-GET-BANKDESC SECTION.
      ***************************
       3231-START.
      *
           IF SQL-BANKKEY              = SPACES
               MOVE SPACES             TO WSAA-BANKDESC
                                          WSAA-BRANCHDESC               <CLM14>
                                          WSAA-TRANSOFFICE              <CLM14>
                                          WSAA-BANKCITY                 <CLM14>
               GO TO 3239-EXIT
           END-IF.

           INITIALIZE                  BABR-PARAMS.
           MOVE SQL-BANKKEY            TO BABR-BANKKEY.
           MOVE BABRREC                TO BABR-FORMAT.
           MOVE READR                  TO BABR-FUNCTION.

           CALL 'BABRIO'               USING BABR-PARAMS.

           IF BABR-STATUZ              NOT = O-K
           AND                         NOT = MRNF
               MOVE BABR-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF BABR-STATUZ              = O-K
               MOVE BABR-BANKDESC      TO WSAA-BANKDESC
               MOVE BABR-ZDETAL        TO WSAA-BRANCHDESC               <CLM14>
               MOVE BABR-ZTRDDS        TO WSAA-TRANSOFFICE              <CLM14>
               MOVE BABR-BANK-ADDR05   TO WSAA-BANKCITY                 <CLM14>
           ELSE
               MOVE SPACES             TO WSAA-BANKDESC
                                          WSAA-BRANCHDESC               <CLM14>
                                          WSAA-TRANSOFFICE              <CLM14>
                                          WSAA-BANKCITY                 <CLM14>
           END-IF.
      *
       3239-EXIT.
           EXIT.
      /
       3240-GET-BANKCODE-DESC SECTION.
      ********************************
       3241-START.
      *
           MOVE SPACES                 TO WSAA-BRANCHDESC               <CLM14>
                                          WSAA-TRANSOFFICE              <CLM14>
                                          WSAA-BANKCITY.                <CLM14>
           IF SQL-REQNBCDE             = SPACES
               MOVE SPACES             TO WSAA-BANKDESC
               GO TO 3249-EXIT
           END-IF.

           MOVE SQL-REQNBCDE           TO WSAA-TABLE-ITEM.
           MOVE T3688                  TO WSAA-TABLE-NAME.
           PERFORM 3220-GET-ITEM-DESC.
           MOVE WSAA-LONGDESC          TO WSAA-BANKDESC.
      *
       3249-EXIT.
           EXIT.
      /
       3300-GET-USERID SECTION.
      *************************
       3301-START.
      *
      * Get corresponding User Number for each Payment Status:
      *
           IF PV001-PAYSTATZ           NOT = SPACES
               MOVE PV001-PAYSTATZ     TO WSAA-PAY-STATS
           ELSE
               MOVE SQL-PROCIND        TO WSAA-PAY-STATS
           END-IF.
      ***
           IF PV001-PROCUSR            = SPACES
               IF PAY-CREATED
                   MOVE SQL-REQUSER    TO WSAA-USERNUM
               END-IF

               IF PAY-APPROVED
                   MOVE SQL-APPRUSER   TO WSAA-USERNUM
               END-IF

               IF PAY-AUTHORISED
                   MOVE SQL-AUTHUSER   TO WSAA-USERNUM
               END-IF

               IF PAY-CANCELLED
                   MOVE SQL-USER       TO WSAA-USERNUM
               END-IF

               IF PAY-PROCESSED
                   MOVE SQL-AUTHUSER   TO WSAA-USERNUM
               END-IF
           END-IF.
      *
      * Use WSAA-USERNUM variable above:
      *
           IF WSAA-USERNUM             = 0
               MOVE SPACES             TO WSAA-USERID
               GO TO 3319-EXIT
           END-IF.

           INITIALIZE                  ZRGETUSR-REC.
           MOVE 'USNO'                 TO ZRGETUSR-FUNCTION.
           MOVE O-K                    TO ZRGETUSR-STATUZ.
           MOVE WSAA-USERNUM           TO ZRGETUSR-USERNUM.
           CALL 'ZRGETUSR'       USING ZRGETUSR-REC.

           IF ZRGETUSR-STATUZ          NOT = O-K
               MOVE ZRGETUSR-REC       TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE ZRGETUSR-USERID        TO WSAA-USERID.
      *
       3319-EXIT.
           EXIT.
      /
       3400-GET-REMARK SECTION.                                         <POP001>
      *************************                                         <POP001>
       3410-START.                                                      <POP001>
      *                                                                 <POP001>
           MOVE SPACES                 TO WSAA-NOTES.                   <POP001>
           MOVE ZERO                   TO IX.                           <POP001>
           MOVE SPACES                 TO RMRKUPD-DATA-KEY.             <POP001>
           MOVE 'RQ'                   TO RMRKUPD-RDOCPFX.              <POP001>
           MOVE BSPR-COMPANY           TO RMRKUPD-RDOCCOY.              <POP001>
           MOVE SQL-REQNNO             TO RMRKUPD-RDOCNUM.              <POP001>
           MOVE ZERO                   TO RMRKUPD-SEQNBR.               <POP001>
           MOVE RMRKUPDREC             TO RMRKUPD-FORMAT.               <POP001>
           MOVE BEGN                   TO RMRKUPD-FUNCTION.             <POP001>
      *                                                                 <POP001>
       3420-CALL-IO.                                                    <POP001>
      *                                                                 <POP001>
           CALL 'RMRKUPDIO'         USING RMRKUPD-PARAMS.               <POP001>
                                                                        <POP001>
           IF RMRKUPD-STATUZ        NOT = O-K AND ENDP                  <POP001>
              MOVE RMRKUPD-STATUZ      TO SYSR-STATUZ                   <POP001>
              MOVE RMRKUPD-PARAMS      TO SYSR-PARAMS                   <POP001>
              PERFORM 600-FATAL-ERROR                                   <POP001>
           END-IF.                                                      <POP001>
                                                                        <POP001>
           IF RMRKUPD-STATUZ        NOT = O-K                           <POP001>
           OR RMRKUPD-RDOCPFX       NOT = 'RQ'                          <POP001>
           OR RMRKUPD-RDOCCOY       NOT = BSPR-COMPANY                  <POP001>
           OR RMRKUPD-RDOCNUM       NOT = SQL-REQNNO                    <POP001>
              MOVE ENDP                TO RMRKUPD-STATUZ                <POP001>
              GO TO 3490-EXIT                                           <POP001>
           END-IF.                                                      <POP001>
                                                                        <POP001>
           ADD 1                       TO IX.                           <POP001>
                                                                        <POP001>
           MOVE RMRKUPD-ALINE          TO WSAA-NOTE(IX).                <POP001>
                                                                        <POP001>
           MOVE NEXTR                  TO RMRKUPD-FUNCTION.             <POP001>
           GO TO 3420-CALL-IO.                                          <POP001>
      *                                                                 <POP001>
       3490-EXIT.                                                       <POP001>
           EXIT.                                                        <POP001>
      /                                                                 <POP001>
       3700-TAKE-OWNER-POLICY SECTION.                                  <PS036>
      ********************************                                  <PS036>
       3701-START.                                                      <PS036>
      *                                                                 <PS036>
           MOVE SPACES                 TO WSAA-CLTNAME                  <PS036>
                                          WSAA-SECUITYNO                <PS036>
                                          WSAA-CLNTNUM.                 <PS036>
                                                                        <PS036>
           INITIALIZE                  CHDRLIF-PARAMS.                  <PS036>
           MOVE 'CH'                   TO CHDRLIF-CHDRPFX.              <PS036>
           MOVE '2'                    TO CHDRLIF-CHDRCOY.              <PS036>
           MOVE WSAA-CHDRNUM           TO CHDRLIF-CHDRNUM.              <PS036>
           MOVE CHDRLIFREC             TO CHDRLIF-FORMAT.               <PS036>
           MOVE READR                  TO CHDRLIF-FUNCTION.             <PS036>
                                                                        <PS036>
           CALL 'CHDRLIFIO'            USING CHDRLIF-PARAMS.            <PS036>
                                                                        <PS036>
           IF  CHDRLIF-STATUZ          NOT = O-K                        <PS036>
           AND                         NOT = MRNF                       <PS036>
               MOVE CHDRLIF-PARAMS     TO SYSR-PARAMS                   <PS036>
               PERFORM 600-FATAL-ERROR                                  <PS036>
           END-IF.                                                      <PS036>
                                                                        <PS036>
           IF CHDRLIF-STATUZ           NOT = O-K                        <PS036>
              GO TO 3709-EXIT                                           <PS036>
           END-IF.                                                      <PS036>
                                                                        <PS036>
           MOVE CHDRLIF-COWNNUM        TO WSAA-CLNTNUM.                 <PS036>
           PERFORM 3710-READ-CLTS.                                      <PS036>
      *                                                                 <PS036>
       3709-EXIT.                                                       <PS036>
           EXIT.                                                        <PS036>
      /                                                                 <PS036>
       3710-READ-CLTS SECTION.                                          <PS036>
      ******************************                                    <PS036>
       3711-START.                                                      <PS036>
      *                                                                 <PS036>
           INITIALIZE                  CLTS-DATA-KEY.                   <PS036>
           MOVE 'CN'                   TO CLTS-CLNTPFX.                 <PS036>
           MOVE '9'                    TO CLTS-CLNTCOY.                 <PS036>
           MOVE WSAA-CLNTNUM           TO CLTS-CLNTNUM.                 <PS036>
           MOVE READR                  TO CLTS-FUNCTION.                <PS036>
           MOVE CLTSREC                TO CLTS-FORMAT.                  <PS036>
                                                                        <PS036>
           CALL 'CLTSIO'               USING CLTS-PARAMS.               <PS036>
                                                                        <PS036>
           IF  CLTS-STATUZ             NOT = O-K                        <PS036>
           AND                         NOT = MRNF                       <PS036>
               MOVE CLTS-PARAMS        TO SYSR-PARAMS                   <PS036>
               PERFORM 600-FATAL-ERROR                                  <PS036>
           END-IF.                                                      <PS036>
                                                                        <PS036>
           IF  CLTS-STATUZ             = O-K                            <PS036>
               MOVE CLTS-SECUITYNO     TO WSAA-SECUITYNO                <PS036>
               PERFORM 3720-GET-CLIENT-NAME                             <PS036>
           END-IF.                                                      <PS036>
      *                                                                 <PS036>
       3719-EXIT.                                                       <PS036>
           EXIT.                                                        <PS036>
      /                                                                 <PS036>
       3720-GET-CLIENT-NAME SECTION.                                    <PS036>
      ******************************                                    <PS036>
       3721-START.                                                      <PS036>
      *                                                                 <PS036>
           MOVE SPACES                 TO NMAD-NAMADRS-REC.             <PS036>
           MOVE BSSC-LANGUAGE          TO NMAD-LANGUAGE.                <PS036>
           MOVE CLTS-CLNTNUM           TO NMAD-CLNT-NUMBER.             <PS036>
           MOVE CLTS-CLNTPFX           TO NMAD-CLNT-PREFIX.             <PS036>
           MOVE CLTS-CLNTCOY           TO NMAD-CLNT-COMPANY.            <PS036>
           MOVE WSAA-NAMES-FUNC        TO NMAD-FUNCTION.                <PS036>
                                                                        <PS036>
           CALL 'NAMADRS'              USING  NMAD-NAMADRS-REC.         <PS036>
                                                                        <PS036>
           IF NMAD-STATUZ              NOT = O-K                        <PS036>
           AND                         NOT = MRNF                       <PS036>
              MOVE NMAD-STATUZ         TO CONR-STATUZ                   <PS036>
              PERFORM 600-FATAL-ERROR                                   <PS036>
           END-IF.                                                      <PS036>
           IF NMAD-STATUZ              = O-K                            <PS036>
              MOVE NMAD-NAME           TO WSAA-CLTNAME                  <PS036>
           END-IF.                                                      <PS036>
      *                                                                 <PS036>
       3729-EXIT.                                                       <PS036>
           EXIT.                                                        <PS036>
      /                                                                 <PS036>
       3730-READ-AGNT SECTION.                                          <PS036>
      ************************                                          <PS036>
       3731-START.                                                      <PS036>
      *                                                                 <PS036>
           MOVE SPACES                 TO WSAA-CLTNAME                  <PS036>
                                          WSAA-SECUITYNO                <PS036>
                                          WSAA-CLNTNUM.                 <PS036>
                                                                        <PS036>
           INITIALIZE                    AGNT-PARAMS.                   <PS036>
           MOVE 'AG'                     TO AGNT-AGNTPFX.               <PS036>
           MOVE '2'                      TO AGNT-AGNTCOY.               <PS036>
           MOVE CHDRLIF-AGNTNUM          TO AGNT-AGNTNUM.               <PS036>
           MOVE AGNTREC                  TO AGNT-FORMAT.                <PS036>
           MOVE READR                    TO AGNT-FUNCTION.              <PS036>
                                                                        <PS036>
           CALL 'AGNTIO'                 USING AGNT-FUNCTION.           <PS036>
                                                                        <PS036>
           IF AGNT-STATUZ                NOT = O-K                      <PS036>
           AND                           NOT = MRNF                     <PS036>
               MOVE AGNT-PARAMS          TO SYSR-PARAMS                 <PS036>
               PERFORM 600-FATAL-ERROR                                  <PS036>
           END-IF.                                                      <PS036>
                                                                        <PS036>
           IF AGNT-STATUZ              NOT = O-K                        <PS036>
              GO TO 3739-EXIT                                           <PS036>
           END-IF.                                                      <PS036>
                                                                        <PS036>
           MOVE AGNT-CLNTNUM           TO WSAA-CLNTNUM.                 <PS036>
           PERFORM 3710-READ-CLTS.                                      <PS036>
      *                                                                 <PS036>
       3739-EXIT.                                                       <PS036>
           EXIT.                                                        <PS036>
      /                                                                 <PS036>
       3800-METHOD-CASH SECTION.                                        <PS036>
      **************************                                        <PS036>
       3801-START.                                                      <PS036>
      *                                                                 <PS036>
           MOVE SPACES                 TO WSAA-CLNTNUM                  <PS036>
                                          WSAA-PO-ID                    <PS036>
                                          WSAA-AGNT-ID                  <PS036>
                                          WSAA-AGN.                     <PS036>
                                                                        <PS036>
           PERFORM 3700-TAKE-OWNER-POLICY.                              <PS036>
           MOVE WSAA-CLTNAME           TO OWNNAM     OF ZPMRPF.         <PS036>
           MOVE WSAA-SECUITYNO         TO ZRSECNO    OF ZPMRPF.         <PS036>
           MOVE WSAA-SECUITYNO         TO WSAA-PO-ID.                   <PS036>
                                                                        <PS036>
           PERFORM 3730-READ-AGNT.                                      <PS036>
           MOVE WSAA-CLTNAME           TO SERVAGNAM  OF ZPMRPF.         <PS036>
           MOVE WSAA-SECUITYNO         TO AGTLICNO   OF ZPMRPF.         <PS036>
           MOVE WSAA-SECUITYNO         TO WSAA-AGNT-ID.                 <PS036>
                                                                        <PS036>
           IF WSAA-PAYEE-IDNO          = WSAA-AGNT-ID                   <PS036>
              MOVE WSAA-AGNOTES-1      TO WSAA-AGN                      <PS036>
           ELSE                                                         <PS036>
              MOVE WSAA-AGNOTES-2      TO WSAA-AGN                      <PS036>
           END-IF.                                                      <PS036>
                                                                        <PS036>
           IF WSAA-PAYEE-IDNO          = WSAA-PO-ID                     <PS036>
              MOVE SPACES              TO WSAA-AGN                      <PS036>
           END-IF.                                                      <PS036>
                                                                        <PS036>
           MOVE WSAA-AGN               TO AGNOTES    OF ZPMRPF.         <PS036>
      *                                                                 <PS036>
       3809-EXIT.                                                       <PS036>
           EXIT.                                                        <PS036>
      /                                                                 <PS036>
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
      *   Close the cursor
      *
           EXEC SQL
              CLOSE CHEQPF1
           END-EXEC.
      *
      *  Close any open files.
      *
           CLOSE ZPMRPF.
           MOVE O-K                    TO LSAA-STATUZ.
      *
       4090-EXIT.
           EXIT.
      /
       A1000-LOOP-TV023 SECTION.                                        <PHFX30>
      **************************                                        <PHFX30>
       A1100-START.                                                     <PHFX30>
      *                                                                 <PHFX30>
      *-- Offices which report user manages                             <PHFX30>
      *                                                                 <PHFX30>
           MOVE 'N'                    TO WSAA-TV023-FOUND.             <PHFX30>
      *                                                                 <PHFX30>
           IF TV023-WOFFCODE            = PV001-WOFFCODE                <PHFX30>
              MOVE 'Y'                 TO WSAA-TV023-FOUND              <PHFX30>
              GO TO A1900-EXIT                                          <PHFX30>
           END-IF.                                                      <PHFX30>
      *                                                                 <PHFX30>
           IF TV023-YNFLAG          NOT = 'Y'                           <PHFX30>
             PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 100            <PHFX30>
                                  OR TV023-OFFCDE(IDX) = SPACES         <PHFX30>
                                  OR WSAA-TV023-FOUND  = 'Y'            <PHFX30>
               IF TV023-OFFCDE(IDX) = PV001-WOFFCODE                    <PHFX30>
                 MOVE 'Y'         TO WSAA-TV023-FOUND                   <PHFX30>
               END-IF                                                   <PHFX30>
             END-PERFORM                                                <PHFX30>
           ELSE                                                         <PHFX30>
             MOVE 'Y'         TO WSAA-TV023-FOUND                       <PHFX30>
           END-IF.                                                      <PHFX30>
      *                                                                 <PHFX30>
       A1900-EXIT.                                                      <PHFX30>
            EXIT.                                                       <PHFX30>
      /                                                                 <PHFX30>
       A3200-READ-ZCLE SECTION.                                         <CLM14>
      **************************                                        <CLM14>
       A3210-START.                                                     <CLM14>
      *                                                                 <CLM14>
           MOVE SPACES                 TO ZCLE-PARAMS.                  <CLM14>
           MOVE CLEX-CLNTPFX           TO ZCLE-CLNTPFX.                 <CLM14>
           MOVE CLEX-CLNTCOY           TO ZCLE-CLNTCOY.                 <CLM14>
           MOVE CLEX-CLNTNUM           TO ZCLE-CLNTNUM.                 <CLM14>
           MOVE ZCLEREC                TO ZCLE-FORMAT.                  <CLM14>
           MOVE READR                  TO ZCLE-FUNCTION.                <CLM14>
                                                                        <CLM14>
           CALL 'ZCLEIO'            USING ZCLE-PARAMS.                  <CLM14>
                                                                        <CLM14>
           IF ZCLE-STATUZ           NOT = O-K AND MRNF                  <CLM14>
               MOVE ZCLE-PARAMS        TO SYSR-PARAMS                   <CLM14>
               PERFORM 600-FATAL-ERROR                                  <CLM14>
           END-IF.                                                      <CLM14>
                                                                        <CLM14>
           IF ZCLE-STATUZ               = O-K                           <CLM14>
              MOVE ZCLE-IDPLACEXT      TO WSAA-IDPLACEXT                <CLM14>
           ELSE                                                         <CLM14>
              MOVE SPACES              TO WSAA-IDPLACEXT                <CLM14>
           END-IF.                                                      <CLM14>
                                                                        <CLM14>
       A3290-EXIT.                                                      <CLM14>
             EXIT.                                                      <CLM14>
      /                                                                 <CLM14>
