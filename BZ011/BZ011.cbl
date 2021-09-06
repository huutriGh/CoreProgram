      * COMPILE-OPTIONS-SQL   CSRSQLCSR(*ENDJOB) COMMIT(*NONE) <Do Not Delete>
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     BZ011.
      *
      *(C) Copyright CSC Corporation Limited 1986 - 2000.
      *    All rights reserved. CSC Confidential.
      *
      *REMARKS.
      *   This is a skeleton for a batch mainline program.
      *
      *   The basic procedure division logic is for reading via SQL and
      *     printing a simple input primary file. The overall structure
      *     is as follows:
      *
      *   Initialise
      *     - retrieve and set up standard report headings.
      *
      *   Read
      *     - read first primary file record
      *
      *   Perform     Until End of File
      *
      *      Edit
      *       - Check if the primary file record is required
      *       - Softlock it if the record is to be updated
      *
      *      Update
      *       - update database files
      *       - write details to report while not primary file EOF
      *       - look up referred to records for output details
      *       - if new page, write headings
      *       - write details
      *
      *      Read next primary file records
      *
      *    End Perform
      *
      *   Control totals:
      *     01  -  Number of pages printed
      *
      *   Error Processing:
      *     If a system error move the error code into the SYSR-STATUZ
      *     If a database error move the XXXX-PARAMS to SYSR-PARAMS.
      *     Perform the 600-FATAL-ERROR section.
      *
      *   These remarks must be replaced by what the program actually
      *     does.
      *
      ***********************************************************************
      *           AMENDMENT  HISTORY                                        *
      ***********************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....                               *
      *                                                                     *
      * 22/08/16  01/01   POP002       Phi Tran - IT DEV                    *
      *           Initial Version.                                          *
      *                                                                     *
      * 08/09/17  01/01   PS010        Phi Tran - IT DEV                    *
      *           Enhcane for LOAN Repayment.                               *
      *                                                                     *
      * 16/11/18  01/01   CS013        Phi Tran - IT DEV                    *
      *           ADD Field USERID.                                         *
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
           SELECT ZCRPPF               ASSIGN TO DATABASE-ZCRPPF.
      /
       DATA DIVISION.
       FILE SECTION.
       FD  ZCRPPF                          LABEL RECORDS STANDARD
           DATA RECORDS                    ARE ZCRPPF-REC.
       01  ZCRPPF-REC.
           COPY DDS-ALL-FORMATS            OF ZCRPPF.
      /
       WORKING-STORAGE SECTION.
      *
       01  WSAA-PROG                   PIC X(05) VALUE 'BZ011'.
       01  WSAA-VERSION                PIC X(02) VALUE '01'.
      *
      *
       01  WSAA-COMMIT-CNT             PIC S9(08) COMP-3.
       01  WSAA-CYCLE-CNT              PIC S9(08) COMP-3.
       01  WSAA-CNT                    PIC 9(02).
       01  WSSP-EDTERROR               PIC X(04).
       01  WSAA-T204                   PIC X(04) VALUE 'T204'.
       01  WSAA-T205                   PIC X(04) VALUE 'T205'.
       01  WSAA-T2A3                   PIC X(04) VALUE 'T2A3'.
       01  WSAA-CN                     PIC X(02) VALUE 'CN'.
       01  WSAA-WOFFCODE               PIC X(05).                       <POP002>
       01  WSAA-CLNTNUM                PIC X(08).                       <POP002>
      *01  WSAA-PAYRNAME               PIC X(30).               <CS013> <POP002>
       01  WSAA-PAYRNAME               PIC X(80).                       <CS013>
       01  WSAA-OFFDES                 PIC X(30).                       <POP002>
       01  WSAA-ZTRNDATE               PIC X(10).                       <POP002>
       01  WSAA-EFFDATES               PIC X(10).                       <POP002>
       01  WSAA-DATE-FROM              PIC S9(08).
       01  WSAA-DATE-TO                PIC S9(08).
       01  WSAA-TIME-ACCEPT.                                            <POP002>
           03  WSAA-HH                 PIC 9(02).                       <POP002>
           03  WSAA-MM                 PIC 9(02).                       <POP002>
           03  WSAA-SS                 PIC 9(02).                       <POP002>
       01  WSAA-TIME-OUT               PIC X(10).                       <POP002>
       01  WSAA-TRRN-NUM               PIC X(11).
       01  WSAA-TIME                   PIC 9(6)  VALUE 0.
       01  WSAA-DEPDATE                PIC S9(08) COMP-3.               <POP002>
       01  WSAA-NOTES.                                                  <POP001>
           03  WSAA-NOTE               PIC X(61) OCCURS 12.             <POP001>
       01  IX                          PIC 9(02).                       <POP001>
      *
       01  WSAA-TV088-KEY.
           03  WSAA-TV088-SACSCODE     PIC X(02).                       <POP002>
           03  WSAA-TV088-SACSTYP      PIC X(02).                       <POP002>
      *
       01  WSAA-NAMES-FUNC             PIC X(05)  VALUE 'LGNMN'.
      * The following line has been changed & reinstated
       01  WSAA-SHORTDESCA-LEN         PIC S9(02) COMP-3 VALUE 9.
       01  WSAA-SURNAME-LEN            PIC S9(02) COMP-3 VALUE 60.
      *
       01  WSAA-NAME.
           03  WSAA-SURNAME            PIC X(60).
           03  WSAA-GIVNAME            PIC X(60).
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
           03  ZPPRREC                 PIC X(10) VALUE 'ZPPRREC'.
           03  RMRKUPDREC              PIC X(10) VALUE 'RMRKUPDREC'.    <POP001>
           03  RBNKREC                 PIC X(10) VALUE 'RBNKREC'.       <POP001>
           03  RTRNCDEREC              PIC X(10) VALUE 'RTRNCDEREC'.    <POP001>
           03  CHDRENQREC              PIC X(10) VALUE 'CHDRENQREC'.    <POP001>
           03  ACMVREVREC              PIC X(10) VALUE 'ACMVREVREC'.    <POP002>
      *
       01  TABLES.
           03  T1692                   PIC X(06) VALUE 'T1692'.
           03  T1693                   PIC X(06) VALUE 'T1693'.
           03  T3629                   PIC X(06) VALUE 'T3629'.
           03  T3672                   PIC X(06) VALUE 'T3672'.
           03  T3695                   PIC X(06) VALUE 'T3695'.
           03  T3593                   PIC X(06) VALUE 'T3593'.
           03  T3688                   PIC X(06) VALUE 'T3688'.
           03  TV023                   PIC X(06) VALUE 'TV023'.
           03  TV022                   PIC X(06) VALUE 'TV022'.         <POP002>
           03  TV088                   PIC X(06) VALUE 'TV088'.         <POP002>
           03  T3676                   PIC X(06) VALUE 'T3676'.
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
       01  WSAA-DATE-F                 PIC X(10).                       <POP002>
       01  WSAA-SIZE                   PIC 9(03) VALUE 200.
       01  WSAA-IX                     PIC 9(03) VALUE 0.
       01  WSAA-IY                     PIC 9(03) VALUE 0.
      *
       01  AP                          PIC X(01) VALUE "'".
       01  WSAA-SQL-SMT                PIC X(400).
       01  FILLER REDEFINES WSAA-SQL-SMT.
           03  WSAA-SQL-LINE1          PIC X(100).
           03  WSAA-SQL-LINE2          PIC X(100).
           03  WSAA-SQL-LINE3          PIC X(100).
           03  WSAA-SQL-LINE4          PIC X(100).
      *
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
       01  SQL-RTRNPF.
           05  SQL-RTRNREC.
               10  SQL-BATCTRCDE       PIC X(00004).
               10  SQL-RDOCCOY         PIC X(00001).
               10  SQL-RDOCNUM         PIC X(00009).
               10  SQL-RLDGPFX         PIC X(00002).
               10  SQL-RLDGCOY         PIC X(00001).
               10  SQL-RLDGACCT        PIC X(00016).
               10  SQL-EFFDATE         PIC S9(08).
               10  SQL-TRANDESC        PIC X(30).
               10  SQL-BANKCODE        PIC X(02).
               10  SQL-SACSCODE        PIC X(02).
               10  SQL-SACSTYPE        PIC X(02).
               10  SQL-USER-PROFILE    PIC X(10).
               10  SQL-TRANNO          PIC S9(05) COMP-3.               <POP002>
               10  SQL-RDOCPFX         PIC X(0002).                     <POP002>
      *
       01  WSAA-TABLE-NAME             PIC X(05).
       01  WSAA-TABLE-ITEM             PIC X(08).
       01  WSAA-QCMDEXC                PIC X(400).
       01  WSAA-QCMDEXC-LENGTH         PIC S9(10)V9(05) COMP-3
                                                     VALUE 400.
      *
       01  WSAA-ZCRP-FN.
           03  FILLER                  PIC X(04)   VALUE 'ZCRP'.
           03  WSAA-ZCRP-RUNID         PIC X(02).
           03  WSAA-ZCRP-JOBNO         PIC 9(04).

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
           03  WSAA-BANKACCKEY         PIC X(20).
           03  WSAA-IDDATE             PIC 9(08).
           03  WSAA-IDDATE-T           PIC X(10).
           03  WSAA-IDPLACE            PIC X(30).
           03  WSAA-DATEFROM           PIC X(10).
           03  WSAA-DATETO             PIC X(10).
           03  WSAA-REPDATE            PIC X(10).
           03  WSAA-STATDETS           PIC X(30).
           03  WSAA-USERID             PIC X(10).
           03  WSAA-REMARK             PIC X(60).
           03  WSAA-SACSCODE           PIC X(02).
           03  WSAA-SACSTYPE           PIC X(02).

       01  WSAA-SANCKEY.
           COPY SANCKYR.
      *
      ***  COPY COMMONPAR.
      ***  COPY CONTROLREC.
      ***  COPY CONJOBREC.
           COPY CONLOGREC.
           COPY CONERRREC.
           COPY VARCOM.
      *    COPY DBCSTRNCPY.
           COPY DBCSTRCPY2.
           COPY NAMADRSREC.
           COPY SPCOUTREC.
           COPY CONTOTREC.
           COPY ACMVREVSKM.                                             <POP002>
           COPY DATCON1REC.
           COPY CLTSSKM.
           COPY CLEXSKM.
           COPY RMRKUPDSKM.                                             <POP001>
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
           COPY ZPPRSKM.
           COPY RBNKSKM.
           COPY RTRNCDESKM.
           COPY TV023REC.
           COPY TV022REC.                                               <POP002>
           COPY TV088REC.                                               <POP002>
      /
           COPY CHEQRPTSKM.
           COPY CHDRENQSKM.
           COPY PZ012PAR.
           COPY ZRGTUSRREC.
           COPY ACMVPMRSKM.
           COPY PREQSKM.
           COPY BABRSKM.
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
           ACCEPT WSAA-TIME-ACCEPT     FROM TIME.                       <POP002>
           MOVE BUPA-PARMAREA          TO PZ012-PARM-RECORD.
           ACCEPT WSAA-TIME            FROM TIME.                       <POP002>
      *
           MOVE TDAY                   TO DTC1-FUNCTION.
           CALL 'DATCON1'           USING DTC1-DATCON1-REC.
           MOVE DTC1-EXT-DATE          TO WSAA-TODAY.

           MOVE CONV                   TO DTC1-FUNCTION.
           MOVE BSSC-EFFECTIVE-DATE    TO DTC1-INT-DATE.
           CALL 'DATCON1'           USING DTC1-DATCON1-REC.
           MOVE DTC1-EXT-DATE          TO WSAA-TODAY-T.
           MOVE PZ012-FROMDATE         TO WSAA-DATE-FROM.
           MOVE PZ012-TODATE           TO WSAA-DATE-TO.
                                                                        <POP002>
           MOVE SPACES                 TO DTC1-DATCON1-REC.             <POP002>
           MOVE CONV                   TO DTC1-FUNCTION.                <POP002>
           MOVE PZ012-FROMDATE         TO DTC1-INT-DATE.                <POP002>
           CALL 'DATCON1'           USING DTC1-DATCON1-REC.             <POP002>
           MOVE DTC1-EXT-DATE          TO WSAA-DATE-F.                  <POP002>
                                                                        <POP002>
           MOVE SPACES                 TO DTC1-DATCON1-REC.             <POP002>
           MOVE CONV                   TO DTC1-FUNCTION.                <POP002>
           MOVE PZ012-TODATE           TO DTC1-INT-DATE.                <POP002>
           CALL 'DATCON1'           USING DTC1-DATCON1-REC.             <POP002>
           MOVE DTC1-EXT-DATE          TO WSAA-DATE-T.                  <POP002>
                                                                        <POP002>
      *
       1060-DEFINE-CURSOR.
      *
      *  Define the query required by declaring a cursor
      *
      *
           PERFORM 1100-OPEN-DOWNLOAD-FILE.
      *
      *  Define the query required by declaring a cursor
      *
           EXEC SQL
              DECLARE RTRNPF1 CURSOR FOR
              SELECT BATCTRCDE, RDOCCOY, RDOCNUM, RLDGPFX, RLDGCOY,
                     RLDGACCT, EFFDATE, TRANDESC, BANKCODE, SACSCODE,
                     SACSTYP, USRPRF, TRANNO, RDOCPFX                   <POP002>
                     FROM RTRNPF
              WHERE ( BATCTRCDE         =:WSAA-T204
              OR      BATCTRCDE         =:WSAA-T205
              OR      BATCTRCDE         =:WSAA-T2A3 )
              AND     SACSCODE          = 'CN'                          <POP002>
              ORDER BY BANKCODE, EFFDATE, TRANNO                        <POP002>
                                                                        <POP002>
           END-EXEC.
      *
      *   Open the cursor (this runs the query)
      *
           EXEC SQL
              OPEN RTRNPF1
           END-EXEC.
      *
       1090-EXIT.
           EXIT.
      /
       1100-OPEN-DOWNLOAD-FILE SECTION.
      *********************************
       1101-START.
      *
           MOVE BPRD-SYSTEM-PARAM04    TO WSAA-ZCRP-RUNID.
           MOVE BSSC-SCHEDULE-NUMBER   TO WSAA-ZCRP-JOBNO.
           MOVE BSPR-PROCESS-OCC-NUM   TO WSAA-THREAD-NUMBER.

           STRING
               'OVRDBF FILE(ZCRPPF) TOFILE('
                                       DELIMITED BY SIZE
                BPRD-RUN-LIBRARY       DELIMITED BY SPACES
               '/' WSAA-ZCRP-FN ') '
               'MBR(' WSAA-THREAD-MEMBER ')'
               ' SEQONLY(*YES 1000)'
                                       DELIMITED BY SIZE
                                       INTO WSAA-QCMDEXC
           END-STRING.

           CALL 'QCMDEXC' USING WSAA-QCMDEXC WSAA-QCMDEXC-LENGTH.

           OPEN OUTPUT ZCRPPF.

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
      *
           EXEC SQL
             FETCH RTRNPF1 INTO :SQL-RTRNREC
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
      *
      * If having OFFICE.                                               <POP002>
      *
      *    IF PZ012-WOFFCODE           NOT = SPACES                     <POP002>
      *       INITIALIZE                  ITEM-PARAMS                   <POP002>
      *       MOVE BSPR-COMPANY        TO ITEM-ITEMCOY                  <POP002>
      *       MOVE 'IT'                TO ITEM-ITEMPFX                  <POP002>
      *       MOVE TV023               TO ITEM-ITEMTABL                 <POP002>
      *       MOVE SQL-USER-PROFILE    TO ITEM-ITEMITEM                 <POP002>
      *       MOVE ITEMREC             TO ITEM-FORMAT                   <POP002>
      *       MOVE READR               TO ITEM-FUNCTION                 <POP002>
      *                                                                 <POP002>
      *       CALL  'ITEMIO'        USING ITEM-PARAMS                   <POP002>
      *                                                                 <POP002>
      *       IF ITEM-STATUZ        NOT = O-K                           <POP002>
      *       AND ITEM-STATUZ       NOT = MRNF                          <POP002>
      *           MOVE ITEM-PARAMS     TO SYSR-PARAMS                   <POP002>
      *           PERFORM 600-FATAL-ERROR                               <POP002>
      *       END-IF                                                    <POP002>
      *       MOVE ITEM-GENAREA        TO TV023-TV023-REC               <POP002>
      *       IF TV023-WOFFCODE     NOT = PZ012-WOFFCODE                <POP002>
      *          MOVE SPACES           TO WSSP-EDTERROR                 <POP002>
      *          GO TO 2590-EXIT                                        <POP002>
      *       END-IF                                                    <POP002>
      *    END-IF.                                                      <POP002>
                                                                        <POP002>
      *                                                                 <POP002>
      * If having BANKCODE.                                             <POP002>
      *                                                                 <POP002>
           IF  PZ012-BANKCODE       NOT = SPACES                        <POP002>
           AND SQL-BANKCODE         NOT = PZ012-BANKCODE                <POP002>
               MOVE SPACES         TO WSSP-EDTERROR                     <POP002>
               GO TO 2590-EXIT                                          <POP002>
           END-IF.                                                      <POP002>
      *
      * If having Date Range:
      *
           INITIALIZE                  RBNK-PARAMS.                     <POP002>
           MOVE ZEROES                 TO WSAA-DEPDATE.                 <POP002>
                                                                        <POP002>
           MOVE 'CA'                   TO RBNK-RDOCPFX.                 <POP002>
           MOVE SQL-RDOCCOY            TO RBNK-RDOCCOY.                 <POP002>
           MOVE SQL-RDOCNUM            TO RBNK-RDOCNUM.                 <POP002>
           MOVE RBNKREC                TO RBNK-FORMAT.                  <POP002>
           MOVE READR                  TO RBNK-FUNCTION.                <POP002>
                                                                        <POP002>
           CALL 'RBNKIO'  USING        RBNK-PARAMS.                     <POP002>
                                                                        <POP002>
           IF RBNK-STATUZ           NOT = O-K AND MRNF                  <POP002>
              MOVE RBNK-STATUZ         TO SYSR-STATUZ                   <POP002>
              MOVE RBNK-PARAMS         TO SYSR-PARAMS                   <POP002>
           END-IF.                                                      <POP002>
                                                                        <POP002>
           IF  RBNK-STATUZ              = O-K                           <POP002>
               MOVE RBNK-DEPDATE        TO WSAA-DEPDATE                 <POP002>
           END-IF.                                                      <POP002>
                                                                        <POP002>
           IF  PZ012-FROMDATE       NOT = VRCM-MAX-DATE                 <POP002>
           AND PZ012-FROMDATE       NOT = ZEROES                        <POP002>
           AND PZ012-TODATE         NOT = VRCM-MAX-DATE                 <POP002>
           AND PZ012-TODATE         NOT = ZEROES                        <POP002>
               IF NOT( RBNK-DEPDATE    >= PZ012-FROMDATE                <POP002>
               AND  RBNK-DEPDATE       <= PZ012-TODATE )                <POP002>
                    MOVE SPACES           TO WSSP-EDTERROR              <POP002>
                    GO TO 2590-EXIT                                     <POP002>
               END-IF                                                   <POP002>
           END-IF.                                                      <POP002>
      *                                                                 <CS013>
      * If Having USERID.                                               <CS013>
      *                                                                 <CS013>
           IF  PZ012-USERID         NOT = SPACES                        <CS013>
           AND SQL-USER-PROFILE     NOT = PZ012-USERID                  <CS013>
               MOVE SPACES         TO WSSP-EDTERROR                     <CS013>
               GO TO 2590-EXIT                                          <CS013>
           END-IF.                                                      <CS013>
      *
       2590-EXIT.
           EXIT.
      /
       3000-UPDATE SECTION.
      *********************
       3010-UPDATE.
      *                                                                 <POP002>
           STRING WSAA-ZCRP-JOBNO      DELIMITED BY SIZE                <POP002>
                  WSAA-TIME-ACCEPT     DELIMITED BY SIZE                <POP002>
                                       INTO      WSAA-TIME-OUT          <POP002>
           END-STRING.                                                  <POP002>
                                                                        <POP002>
           MOVE WSAA-TIME-OUT          TO TIMEJOB   OF ZCRPPF.          <POP002>
           MOVE WSAA-DATE-F            TO DATECFROM OF ZCRPPF.          <POP002>
           MOVE WSAA-DATE-T            TO DATECTO   OF ZCRPPF.          <POP002>
                                                                        <POP002>
           MOVE SPACES                 TO DTC1-DATCON1-REC.             <POP002>
           MOVE CONV                   TO DTC1-FUNCTION.                <POP002>
           MOVE SQL-EFFDATE            TO DTC1-INT-DATE.                <POP002>
           CALL 'DATCON1'           USING DTC1-DATCON1-REC.             <POP002>
           MOVE DTC1-EXT-DATE          TO EFFDATES  OF ZCRPPF.          <POP002>
           MOVE DTC1-EXT-DATE          TO WSAA-EFFDATES.                <POP002>
      *                                                                 <POP002>
           INITIALIZE                  ITEM-PARAMS                      <POP002>
           MOVE BSPR-COMPANY        TO ITEM-ITEMCOY                     <POP002>
           MOVE 'IT'                TO ITEM-ITEMPFX                     <POP002>
           MOVE TV023               TO ITEM-ITEMTABL                    <POP002>
           MOVE SQL-USER-PROFILE    TO ITEM-ITEMITEM                    <POP002>
           MOVE ITEMREC             TO ITEM-FORMAT                      <POP002>
           MOVE READR               TO ITEM-FUNCTION                    <POP002>
                                                                        <POP002>
           CALL  'ITEMIO'        USING ITEM-PARAMS                      <POP002>
                                                                        <POP002>
           IF  ITEM-STATUZ          NOT = O-K                           <POP002>
           AND ITEM-STATUZ          NOT = MRNF                          <POP002>
               MOVE ITEM-STATUZ        TO SYSR-STATUZ                   <POP002>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <POP002>
               PERFORM 600-FATAL-ERROR                                  <POP002>
           END-IF.                                                      <POP002>
                                                                        <POP002>
           IF  ITEM-STATUZ              = O-K                           <POP002>
               MOVE ITEM-GENAREA       TO TV023-TV023-REC               <POP002>
               MOVE TV023-WOFFCODE     TO WOFFCODE  OF  ZCRPPF          <POP002>
               MOVE TV023-WOFFCODE     TO WSAA-WOFFCODE                 <POP002>
           ELSE                                                         <POP002>
               MOVE SPACES         TO WOFFCODE  OF  ZCRPPF              <POP002>
           END-IF.                                                      <POP002>
      *                                                                 <POP002>
           MOVE SQL-BANKCODE           TO BANKCODE  OF  ZCRPPF.         <POP002>
           PERFORM A100-GET-OFFICE-DESC.                                <POP002>
      *
      * Get TR-RN Number
      *
           MOVE SPACES                 TO WSAA-TRRN-NUM.

           INITIALIZE                     ZPPR-PARAMS.                  <POP002>
           MOVE 'CA'                   TO ZPPR-PREFIX.                  <POP002>
           MOVE SQL-RDOCCOY            TO ZPPR-COMPANY.                 <POP002>
           MOVE SQL-BANKCODE           TO ZPPR-BANKCODE.                <POP002>
           MOVE SQL-RDOCNUM            TO ZPPR-RECEIPT.                 <POP002>
           MOVE ZPPRREC                TO ZPPR-FORMAT.
           MOVE READR                  TO ZPPR-FUNCTION.

           CALL 'ZPPRIO'            USING ZPPR-PARAMS.                  <POP002>

           IF ZPPR-STATUZ            NOT = O-K AND MRNF                 <POP002>
               MOVE ZPPR-STATUZ        TO SYSR-STATUZ
               MOVE ZPPR-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF ZPPR-STATUZ               = O-K                           <POP002>
              STRING ZPPR-RCPREPRTCD '-'                                <POP002>
                     ZPPR-RCPREPRNT  DELIMITED BY SIZE                  <POP002>
                                     INTO WSAA-TRRN-NUM                 <POP002>
              END-STRING                                                <POP002>
           END-IF.

           MOVE WSAA-TRRN-NUM          TO ZVARIABLE OF ZCRPPF.
           MOVE SQL-RDOCNUM            TO RECEIPT   OF ZCRPPF.
                                                                        <POP002>
           IF  SQL-RLDGACCT(1:8)    NOT = SPACES                        <POP002>
               MOVE SQL-RLDGACCT(1:8)  TO CLNTNUM   OF ZCRPPF           <POP002>
               MOVE SQL-RLDGACCT(1:8)  TO WSAA-CLNTNUM                  <POP002>
               INITIALIZE                 CLTS-PARAMS                   <POP002>
               MOVE 'CN'               TO CLTS-CLNTPFX                  <POP002>
               MOVE '9'                TO CLTS-CLNTCOY                  <POP002>
               MOVE SQL-RLDGACCT(1:8)  TO CLTS-CLNTNUM                  <POP002>
               MOVE READR              TO CLTS-FUNCTION                 <POP002>
               MOVE CLTSREC            TO CLTS-FORMAT                   <POP002>
                                                                        <POP002>
               CALL 'CLTSIO'        USING CLTS-PARAMS                   <POP002>
                                                                        <POP002>
               IF  CLTS-STATUZ      NOT = O-K AND MRNF                  <POP002>
                   MOVE CLTS-STATUZ    TO SYSR-PARAMS                   <POP002>
                   MOVE CLTS-PARAMS    TO SYSR-PARAMS                   <POP002>
                   PERFORM 600-FATAL-ERROR                              <POP002>
               END-IF                                                   <POP002>
                                                                        <POP002>
               MOVE SPACES             TO NMAD-NAMADRS-REC              <POP002>
               MOVE CLTS-CLNTPFX       TO NMAD-CLNT-PREFIX              <POP002>
               MOVE '9'                TO NMAD-CLNT-COMPANY             <POP002>
               MOVE SQL-RLDGACCT(1:8)  TO NMAD-CLNT-NUMBER              <POP002>
                                                                        <POP002>
               MOVE 'E'                TO NMAD-LANGUAGE                 <POP002>
               MOVE 'PYNMN'            TO NMAD-FUNCTION                 <POP002>
               CALL 'NAMADRS'       USING NMAD-NAMADRS-REC              <POP002>
               IF NMAD-STATUZ              NOT = O-K AND MRNF           <POP002>
                  MOVE NMAD-NAMADRS-REC    TO SYSR-PARAMS               <POP002>
                  MOVE NMAD-STATUZ         TO SYSR-STATUZ               <POP002>
                  PERFORM 600-FATAL-ERROR                               <POP002>
               END-IF                                                   <POP002>
               IF NMAD-STATUZ              = MRNF                       <POP002>
                  MOVE SPACES              TO PAYRNAME   OF  ZCRPPF     <CS013>
                  MOVE SPACES              TO CLNTNMXL   OF  ZCRPPF     <CS013>
               ELSE                                                     <CS013>
                  MOVE NMAD-NAME           TO PAYRNAME   OF  ZCRPPF     <CS013>
                  MOVE NMAD-NAME           TO CLNTNMXL   OF  ZCRPPF     <CS013>
                  MOVE NMAD-NAME           TO WSAA-PAYRNAME             <POP002>
           ELSE                                                         <POP002>
               MOVE SPACES             TO CLNTNUM   OF ZCRPPF           <POP002>
               MOVE SPACES             TO PAYRNAME  OF ZCRPPF           <CS013>
               MOVE SPACES             TO CLNTNMXL  OF ZCRPPF           <CS013>
           END-IF.                                                      <POP002>
      *
           MOVE SQL-USER-PROFILE       TO USERID    OF ZCRPPF.
      *
      * Get Remark.
      *
           PERFORM 3400-GET-REMARK.                                     <POP001>
           MOVE WSAA-NOTES             TO PMNOTE  OF ZCRPPF.
      *
      * Read RBNK
      *
           INITIALIZE                  RBNK-PARAMS.

           MOVE 'CA'                   TO RBNK-RDOCPFX.
           MOVE SQL-RDOCCOY            TO RBNK-RDOCCOY.
           MOVE SQL-RDOCNUM            TO RBNK-RDOCNUM.
           MOVE RBNKREC                TO RBNK-FORMAT.
           MOVE READR                  TO RBNK-FUNCTION.

           CALL 'RBNKIO'  USING        RBNK-PARAMS.

           IF RBNK-STATUZ           NOT = O-K AND MRNF
              MOVE RBNK-STATUZ         TO SYSR-STATUZ
              MOVE RBNK-PARAMS         TO SYSR-PARAMS
           END-IF.
      *                                                                 <POP002>
           MOVE SPACES                 TO DTC1-DATCON1-REC.             <POP002>
           MOVE CONV                   TO DTC1-FUNCTION.                <POP002>
           MOVE RBNK-DEPDATE           TO DTC1-INT-DATE.                <POP002>
           CALL 'DATCON1'           USING DTC1-DATCON1-REC.             <POP002>
           MOVE DTC1-EXT-DATE          TO ZTRNDATE  OF ZCRPPF.          <POP002>
           MOVE DTC1-EXT-DATE          TO WSAA-ZTRNDATE.                <POP002>
      *                                                                 <POP002>
           MOVE RBNK-BANKDESC01        TO BNKNAME    OF ZCRPPF.
           MOVE RBNK-BANKDESC02        TO BRCHNAME   OF ZCRPPF.
           MOVE RBNK-DOCORIGAMT        TO DOCORIGAMT OF ZCRPPF.
           MOVE RBNK-INSREFNO          TO INSREFNO   OF ZCRPPF.
      *                                                                 <POP002>
      *  Read RTRNCDE to get Payor.                                     <POP002>
      *                                                                 <POP002>
           PERFORM 3700-LOOP-RTRNCDE.                                   <PS010>
      *                                                                 <PS010>
      *    INITIALIZE                     RTRNCDE-PARAMS.       <PS010> <POP002>
      *    MOVE SQL-RDOCPFX            TO RTRNCDE-RDOCPFX.      <PS010> <POP002>
      *    MOVE '2'                    TO RTRNCDE-RLDGCOY.      <PS010> <POP002>
      *    MOVE SQL-RDOCCOY            TO RTRNCDE-RDOCCOY.      <PS010> <POP002>
      *    MOVE SQL-RDOCNUM            TO RTRNCDE-RDOCNUM.      <PS010> <POP002>
      *    MOVE READR                  TO RTRNCDE-FUNCTION.     <PS010> <POP002>
      *    MOVE RTRNCDEREC             TO RTRNCDE-FORMAT.       <PS010> <POP002>
      *                                                         <PS010> <POP002>
      *    CALL 'RTRNCDEIO'         USING RTRNCDE-PARAMS.       <PS010> <POP002>
      *                                                         <PS010> <POP002>
      *    IF  RTRNCDE-STATUZ       NOT = O-K AND MRNF          <PS010> <POP002>
      *        MOVE RTRNCDE-STATUZ     TO SYSR-STATUZ           <PS010> <POP002>
      *        MOVE RTRNCDE-PARAMS     TO SYSR-PARAMS           <PS010> <POP002>
      *        PERFORM 600-FATAL-ERROR                          <PS010> <POP002>
      *    END-IF.                                              <PS010> <POP002>
      *                                                         <PS010> <POP002>
      *    IF RTRNCDE-STATUZ            = O-K                   <PS010> <POP002>
      *       MOVE RTRNCDE-SACSCODE    TO SACSCODE  OF ZCRPPF   <PS010> <POP002>
      *       MOVE RTRNCDE-SACSCODE    TO WSAA-TV088-SACSCODE   <PS010> <POP002>
      *       MOVE RTRNCDE-SACSTYP     TO SACSTYPE  OF ZCRPPF   <PS010> <POP002>
      *       MOVE RTRNCDE-SACSTYP     TO WSAA-TV088-SACSTYP    <PS010> <POP002>
      *       MOVE RTRNCDE-TRANDESC    TO TRANDESC OF ZCRPPF    <PS010> <POP002>
      *       MOVE RTRNCDE-ORIGAMT     TO DOCORIGAMT OF ZCRPPF          <PS010>
      *       INITIALIZE                  CHDRENQ-PARAMS        <PS010> <POP002>
      *       MOVE RTRNCDE-RDOCCOY     TO CHDRENQ-CHDRCOY       <PS010> <POP002>
      *       MOVE RTRNCDE-RLDGACCT(1:8)   TO CHDRENQ-CHDRNUM   <PS010> <POP002>
      *       MOVE READR               TO CHDRENQ-FUNCTION      <PS010> <POP002>
      *       MOVE CHDRENQREC          TO CHDRENQ-FORMAT        <PS010> <POP002>
      *       CALL 'CHDRENQIO'      USING CHDRENQ-PARAMS        <PS010> <POP002>
      *       IF  CHDRENQ-STATUZ    NOT = O-K AND MRNF          <PS010> <POP002>
      *           MOVE CHDRENQ-STATUZ  TO SYSR-STATUZ           <PS010> <POP002>
      *           MOVE CHDRENQ-PARAMS  TO SYSR-PARAMS           <PS010> <POP002>
      *           PERFORM 600-FATAL-ERROR                       <PS010> <POP002>
      *       END-IF                                            <PS010> <POP002>
      *       IF  CHDRENQ-STATUZ        = O-K                   <PS010> <POP002>
      *           MOVE RTRNCDE-RLDGACCT(1:8) TO CHDRNUM  OF ZCRPPF      <PS010>
      *           INITIALIZE              CLTS-PARAMS           <PS010> <POP002>
      *           MOVE 'CN'            TO CLTS-CLNTPFX          <PS010> <POP002>
      *           MOVE CHDRENQ-COWNCOY TO CLTS-CLNTCOY          <PS010> <POP002>
      *           MOVE CHDRENQ-COWNNUM TO CLTS-CLNTNUM          <PS010> <POP002>
      *           MOVE READR           TO CLTS-FUNCTION         <PS010> <POP002>
      *           MOVE CLTSREC         TO CLTS-FORMAT           <PS010> <POP002>
      *                                                         <PS010> <POP002>
      *           CALL 'CLTSIO'     USING CLTS-PARAMS           <PS010> <POP002>
      *                                                         <PS010> <POP002>
      *           IF  CLTS-STATUZ             NOT = O-K AND MRNF<PS010> <POP002>
      *               MOVE CLTS-STATUZ TO SYSR-PARAMS           <PS010> <POP002>
      *               MOVE CLTS-PARAMS TO SYSR-PARAMS           <PS010> <POP002>
      *               PERFORM 600-FATAL-ERROR                   <PS010> <POP002>
      *           END-IF                                        <PS010> <POP002>
      *           MOVE SPACES          TO NMAD-NAMADRS-REC      <PS010> <POP002>
      *           MOVE CLTS-CLNTPFX    TO NMAD-CLNT-PREFIX      <PS010> <POP002>
      *           MOVE CHDRENQ-COWNCOY TO NMAD-CLNT-COMPANY     <PS010> <POP002>
      *           MOVE CHDRENQ-COWNNUM TO NMAD-CLNT-NUMBER      <PS010> <POP002>
      *           MOVE 'E'             TO NMAD-LANGUAGE         <PS010> <POP002>
      *           MOVE 'PYNMN'         TO NMAD-FUNCTION         <PS010> <POP002>
      *           CALL 'NAMADRS'       USING NMAD-NAMADRS-REC   <PS010> <POP002>
      *           IF NMAD-STATUZ              NOT = O-K         <PS010> <POP002>
      *           AND                         NOT = MRNF        <PS010> <POP002>
      *              MOVE NMAD-NAMADRS-REC    TO SYSR-PARAMS    <PS010> <POP002>
      *              MOVE NMAD-STATUZ         TO SYSR-STATUZ    <PS010> <POP002>
      *              PERFORM 600-FATAL-ERROR                    <PS010> <POP002>
      *           END-IF                                        <PS010> <POP002>
      *           IF NMAD-STATUZ              = MRNF            <PS010> <POP002>
      *              MOVE SPACES              TO OWNERNAME  OF  ZCRPPF  <PS010>
      *           ELSE                                                  <PS010>
      *              MOVE NMAD-NAME           TO OWNERNAME  OF  ZCRPPF  <PS010>
      *           END-IF                                        <PS010> <POP002>
      *       ELSE                                              <PS010> <POP002>
      *           MOVE SPACES          TO CHDRNUM   OF ZCRPPF   <PS010> <POP002>
      *           MOVE SPACES          TO OWNERNAME OF ZCRPPF   <PS010> <POP002>
      *       END-IF                                            <PS010> <POP002>
      *       PERFORM A2000-READ-TV088                          <PS010> <POP002>
      *    END-IF.                                                      <PS010>
                                                                        <PS010>
PHI        PERFORM A1000-READ-ACMV.                                     <PS010>
      *
       3090-EXIT.
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
      *   Close the cursor
      *
           EXEC SQL
              CLOSE RTRNPF1
           END-EXEC.
      *
      *  Close any open files.
      *
           CLOSE ZCRPPF.
           MOVE O-K                    TO LSAA-STATUZ.
      *
       4090-EXIT.
           EXIT.
      /
       3400-GET-REMARK SECTION.                                         <POP001>
      *************************                                         <POP001>
       3410-START.                                                      <POP001>
      *                                                                 <POP001>
           MOVE SPACES                 TO WSAA-NOTES.                   <POP001>
           MOVE ZERO                   TO IX.                           <POP001>
           MOVE SPACES                 TO RMRKUPD-DATA-KEY.             <POP001>
           MOVE 'CA'                   TO RMRKUPD-RDOCPFX.              <POP001>
           MOVE BSPR-COMPANY           TO RMRKUPD-RDOCCOY.              <POP001>
           MOVE SQL-RDOCNUM            TO RMRKUPD-RDOCNUM.              <POP001>
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
           OR RMRKUPD-RDOCNUM       NOT = SQL-RDOCNUM                   <POP001>
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
       A100-GET-OFFICE-DESC SECTION.                                    <POP002>
      ******************************                                    <POP002>
       A110-START.                                                      <POP002>
      *                                                                 <POP002>
      * Get Office Description.                                         <POP002>
      *                                                                 <POP002>
           INITIALIZE                     DESC-PARAMS.                  <POP002>
           MOVE 'IT'                   TO DESC-DESCPFX.                 <POP002>
           MOVE BSPR-COMPANY           TO DESC-DESCCOY.                 <POP002>
           MOVE T3688                  TO DESC-DESCTABL.                <POP002>
           MOVE SQL-BANKCODE           TO DESC-DESCITEM.                <POP002>
           MOVE SPACES                 TO DESC-ITEMSEQ                  <POP002>
           MOVE 'E'                    TO DESC-LANGUAGE.                <POP002>
           MOVE READR                  TO DESC-FUNCTION.                <POP002>
                                                                        <POP002>
           CALL  'DESCIO'           USING DESC-PARAMS.                  <POP002>
                                                                        <POP002>
           IF DESC-STATUZ           NOT = O-K AND MRNF                  <POP002>
              MOVE DESC-STATUZ         TO SYSR-STATUZ                   <POP002>
              MOVE DESC-PARAMS         TO SYSR-PARAMS                   <POP002>
              PERFORM 600-FATAL-ERROR                                   <POP002>
           END-IF.                                                      <POP002>
                                                                        <POP002>
           IF DESC-STATUZ               = O-K                           <POP002>
              MOVE DESC-LONGDESC       TO OFFDES OF  ZCRPPF             <POP002>
              MOVE DESC-LONGDESC       TO WSAA-OFFDES                   <PS010>
           ELSE                                                         <POP002>
              MOVE SPACES              TO OFFDES OF  ZCRPPF             <POP002>
           END-IF.                                                      <POP002>
      *                                                                 <POP002>
       A190-EXIT.                                                       <POP002>
           EXIT.                                                        <POP002>
      /                                                                 <POP002>
       A1000-READ-ACMV SECTION.                                         <POP002>
      *************************                                         <POP002>
       A1010-START.                                                     <POP002>
      *                                                                 <POP002>
           INITIALIZE                     ACMVREV-PARAMS.               <POP002>
           MOVE SQL-RDOCCOY            TO ACMVREV-RLDGCOY.              <POP002>
           MOVE SQL-RDOCNUM            TO ACMVREV-RDOCNUM.              <POP002>
           MOVE 0                      TO ACMVREV-TRANNO.               <POP002>
           MOVE BEGN                   TO ACMVREV-FUNCTION.             <POP002>
      *                                                                 <POP002>
       A1020-CALL-IO.                                                   <POP002>
      *                                                                 <POP002>
           CALL 'ACMVREVIO'         USING ACMVREV-PARAMS.               <POP002>
                                                                        <POP002>
           IF  ACMVREV-STATUZ       NOT = O-K                           <POP002>
           AND ACMVREV-STATUZ       NOT = ENDP                          <POP002>
               MOVE ACMVREV-STATUZ     TO SYSR-STATUZ                   <POP002>
               MOVE ACMVREV-PARAMS     TO SYSR-PARAMS                   <POP002>
               PERFORM 600-FATAL-ERROR                                  <POP002>
           END-IF.                                                      <POP002>
      *                                                                 <POP002>
           IF  ACMVREV-STATUZ       NOT = O-K                           <POP002>
           OR  ACMVREV-RLDGCOY      NOT = SQL-RDOCCOY                   <POP002>
           OR  ACMVREV-RDOCNUM      NOT = SQL-RDOCNUM                   <POP002>
               MOVE ENDP               TO ACMVREV-STATUZ                <POP002>
               GO TO A1090-EXIT                                         <POP002>
           END-IF.                                                      <POP002>
      *                                                                 <PS010>
           IF  ACMVREV-SACSTYP          = 'AA'                          <PS010>
           OR  ACMVREV-GLSIGN       NOT = '-'                           <PS010>
               MOVE NEXTR              TO ACMVREV-FUNCTION              <PS010>
               GO TO A1020-CALL-IO                                      <PS010>
           END-IF.                                                      <PS010>
      *                                                                 <CS013>
           IF  ACMVREV-SACSTYP          = 'LA'                          <CS013>
           OR  ACMVREV-GLSIGN       NOT = '-'                           <CS013>
               MOVE NEXTR              TO ACMVREV-FUNCTION              <CS013>
               GO TO A1020-CALL-IO                                      <CS013>
           END-IF.                                                      <CS013>
      *                                                                 <CS013>
           IF ACMVREV-STATUZ            = O-K                           <POP002>
              MOVE ACMVREV-ORIGAMT     TO DOCORIGAMT OF ZCRPPF          <PS010>
              MOVE ACMVREV-SACSCODE    TO SACSCODE  OF ZCRPPF           <POP002>
              MOVE ACMVREV-SACSCODE    TO WSAA-TV088-SACSCODE           <POP002>
              MOVE ACMVREV-SACSTYP     TO SACSTYPE  OF ZCRPPF           <POP002>
              MOVE ACMVREV-SACSTYP     TO WSAA-TV088-SACSTYP            <POP002>
              MOVE ACMVREV-TRANDESC    TO TRANDESC OF ZCRPPF            <POP002>
              INITIALIZE                  CHDRENQ-PARAMS                <POP002>
              MOVE ACMVREV-RLDGCOY     TO CHDRENQ-CHDRCOY               <POP002>
              MOVE ACMVREV-RLDGACCT(1:8)   TO CHDRENQ-CHDRNUM           <POP002>
              MOVE READR               TO CHDRENQ-FUNCTION              <POP002>
              MOVE CHDRENQREC          TO CHDRENQ-FORMAT                <POP002>
              CALL 'CHDRENQIO'      USING CHDRENQ-PARAMS                <POP002>
              IF  CHDRENQ-STATUZ    NOT = O-K AND MRNF                  <POP002>
                  MOVE CHDRENQ-STATUZ  TO SYSR-STATUZ                   <POP002>
                  MOVE CHDRENQ-PARAMS  TO SYSR-PARAMS                   <POP002>
                  PERFORM 600-FATAL-ERROR                               <POP002>
              END-IF                                                    <POP002>
              IF  CHDRENQ-STATUZ        = O-K                           <POP002>
                  MOVE ACMVREV-RLDGACCT(1:8) TO CHDRNUM  OF ZCRPPF      <POP002>
                  INITIALIZE              CLTS-PARAMS                   <POP002>
                  MOVE 'CN'            TO CLTS-CLNTPFX                  <POP002>
                  MOVE CHDRENQ-COWNCOY TO CLTS-CLNTCOY                  <POP002>
                  MOVE CHDRENQ-COWNNUM TO CLTS-CLNTNUM                  <POP002>
                  MOVE READR           TO CLTS-FUNCTION                 <POP002>
                  MOVE CLTSREC         TO CLTS-FORMAT                   <POP002>
                                                                        <POP002>
                  CALL 'CLTSIO'     USING CLTS-PARAMS                   <POP002>
                                                                        <POP002>
                  IF  CLTS-STATUZ             NOT = O-K AND MRNF        <POP002>
                      MOVE CLTS-STATUZ TO SYSR-PARAMS                   <POP002>
                      MOVE CLTS-PARAMS TO SYSR-PARAMS                   <POP002>
                      PERFORM 600-FATAL-ERROR                           <POP002>
                  END-IF                                                <POP002>
                  MOVE SPACES          TO NMAD-NAMADRS-REC              <POP002>
                  MOVE CLTS-CLNTPFX    TO NMAD-CLNT-PREFIX              <POP002>
                  MOVE CHDRENQ-COWNCOY TO NMAD-CLNT-COMPANY             <POP002>
                  MOVE CHDRENQ-COWNNUM TO NMAD-CLNT-NUMBER              <POP002>
                  MOVE 'E'             TO NMAD-LANGUAGE                 <POP002>
                  MOVE 'PYNMN'         TO NMAD-FUNCTION                 <POP002>
                  CALL 'NAMADRS'       USING NMAD-NAMADRS-REC           <POP002>
                  IF NMAD-STATUZ              NOT = O-K                 <POP002>
                  AND                         NOT = MRNF                <POP002>
                     MOVE NMAD-NAMADRS-REC    TO SYSR-PARAMS            <POP002>
                     MOVE NMAD-STATUZ         TO SYSR-STATUZ            <POP002>
                     PERFORM 600-FATAL-ERROR                            <POP002>
                  END-IF                                                <POP002>
                  IF NMAD-STATUZ              = MRNF                    <POP002>
                     MOVE SPACES              TO OWNERNAME  OF  ZCRPPF  <POP002>
                  ELSE                                                  <POP002>
                     MOVE NMAD-NAME           TO OWNERNAME  OF  ZCRPPF  <POP002>
                  END-IF                                                <POP002>
              ELSE                                                      <POP002>
                  MOVE SPACES          TO CHDRNUM   OF ZCRPPF           <POP002>
                  MOVE SPACES          TO OWNERNAME OF ZCRPPF           <POP002>
           ELSE                                                         <POP002>
              MOVE SPACES              TO CHDRNUM   OF ZCRPPF           <POP002>
           END-IF.                                                      <POP002>
           PERFORM A3000-MOVE-VALUE-AGAIN.                              <PS010>
           PERFORM A2000-READ-TV088.                                    <POP002>
           MOVE NEXTR                  TO ACMVREV-FUNCTION.             <POP002>
           GO TO A1020-CALL-IO.                                         <POP002>
      *                                                                 <POP002>
       A1090-EXIT.                                                      <POP002>
            EXIT.                                                       <POP002>
      /                                                                 <POP002>
       A2000-READ-TV088 SECTION.                                        <POP002>
      **************************                                        <POP002>
       A2010-START.                                                     <POP002>
      *                                                                 <POP002>
           IF  CHDRENQ-STATCODE         = 'LA'                          <POP002>
           AND SACSCODE                 = 'LP'                          <POP002>
           AND SACSTYPE                 = 'S'                           <POP002>
               MOVE 'SR'             TO   WSAA-TV088-SACSTYP            <POP002>
           END-IF.                                                      <POP002>
                                                                        <POP002>
           INITIALIZE                     ITEM-PARAMS.                  <POP002>
           MOVE BSPR-COMPANY           TO ITEM-ITEMCOY.                 <POP002>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <POP002>
           MOVE TV088                  TO ITEM-ITEMTABL.                <POP002>
           MOVE WSAA-TV088-KEY         TO ITEM-ITEMITEM.                <POP002>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <POP002>
           MOVE READR                  TO ITEM-FUNCTION.                <POP002>
                                                                        <POP002>
           CALL  'ITEMIO'        USING ITEM-PARAMS.                     <POP002>
                                                                        <POP002>
           IF ITEM-STATUZ           NOT = O-K                           <POP002>
           AND ITEM-STATUZ          NOT = MRNF                          <POP002>
               MOVE ITEM-STATUZ        TO SYSR-STATUZ                   <POP002>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <POP002>
               PERFORM 600-FATAL-ERROR                                  <POP002>
           END-IF.                                                      <POP002>
                                                                        <POP002>
           IF  ITEM-STATUZ              = O-K                           <POP002>
               MOVE ITEM-GENAREA       TO TV088-TV088-REC               <POP002>
               MOVE TV088-ADSC         TO LONGDSC  OF ZCRPPF            <POP002>
           ELSE                                                         <POP002>
                MOVE SPACES            TO LONGDSC  OF ZCRPPF            <POP002>
           END-IF.                                                      <POP002>
                                                                        <POP002>
           WRITE ZCRPPF-REC.                                            <POP002>
      *                                                                 <POP002>
       A2090-EXIT.                                                      <POP002>
            EXIT.                                                       <POP002>
      /                                                                 <POP002>
       A3000-MOVE-VALUE-AGAIN SECTION.                                  <POP002>
      ********************************                                  <POP002>
       A3010-START.                                                     <POP002>
      *                                                                 <POP002>
           MOVE WSAA-TIME-OUT          TO TIMEJOB   OF ZCRPPF.          <POP002>
           MOVE WSAA-DATE-F            TO DATECFROM OF ZCRPPF.          <POP002>
           MOVE WSAA-DATE-T            TO DATECTO   OF ZCRPPF.          <POP002>
           MOVE WSAA-EFFDATES          TO EFFDATES  OF ZCRPPF.          <POP002>
           MOVE WSAA-WOFFCODE          TO WOFFCODE  OF ZCRPPF.          <POP002>
           MOVE WSAA-TRRN-NUM          TO ZVARIABLE OF ZCRPPF.          <POP002>
           MOVE SQL-RDOCNUM            TO RECEIPT   OF ZCRPPF.          <POP002>
           MOVE SQL-BANKCODE           TO BANKCODE  OF ZCRPPF.          <POP002>
           MOVE WSAA-CLNTNUM           TO CLNTNUM   OF ZCRPPF.          <POP002>
           MOVE WSAA-PAYRNAME          TO PAYRNAME  OF ZCRPPF.          <CS013>
           MOVE WSAA-PAYRNAME          TO CLNTNMXL  OF ZCRPPF.          <CS013>
           MOVE WSAA-OFFDES            TO OFFDES    OF ZCRPPF.          <POP002>
           MOVE SQL-USER-PROFILE       TO USERID    OF ZCRPPF.          <POP002>
           MOVE RBNK-INSREFNO          TO INSREFNO   OF ZCRPPF.         <POP002>
           MOVE WSAA-NOTES             TO PMNOTE  OF ZCRPPF.            <POP002>
           MOVE WSAA-ZTRNDATE          TO ZTRNDATE  OF ZCRPPF.          <POP002>
      *                                                                 <POP002>
       A2090-EXIT.                                                      <POP002>
      /                                                                 <POP002>
       3700-LOOP-RTRNCDE SECTION.                                       <PS010>
      ***************************                                       <PS010>
       3710-START.                                                      <PS010>
      *                                                                 <PS010>
           INITIALIZE                     RTRNCDE-PARAMS.               <PS010>
           MOVE SQL-RDOCPFX            TO RTRNCDE-RDOCPFX.              <PS010>
           MOVE '2'                    TO RTRNCDE-RLDGCOY.              <PS010>
           MOVE SQL-RDOCCOY            TO RTRNCDE-RDOCCOY.              <PS010>
           MOVE SQL-RDOCNUM            TO RTRNCDE-RDOCNUM.              <PS010>
           MOVE BEGN                   TO RTRNCDE-FUNCTION.             <PS010>
           MOVE RTRNCDEREC             TO RTRNCDE-FORMAT.               <PS010>
      *                                                                 <PS010>
       3720-CALL-IO.                                                    <PS010>
      *                                                                 <PS010>
           CALL 'RTRNCDEIO'         USING RTRNCDE-PARAMS.               <PS010>
                                                                        <PS010>
           IF  RTRNCDE-STATUZ       NOT = O-K AND ENDP                  <PS010>
               MOVE RTRNCDE-STATUZ     TO SYSR-STATUZ                   <PS010>
               MOVE RTRNCDE-PARAMS     TO SYSR-PARAMS                   <PS010>
               PERFORM 600-FATAL-ERROR                                  <PS010>
           END-IF.                                                      <PS010>
                                                                        <PS010>
           IF RTRNCDE-RDOCPFX       NOT = SQL-RDOCPFX                   <PS010>
           OR RTRNCDE-RLDGCOY       NOT = '2'                           <PS010>
           OR RTRNCDE-RDOCCOY       NOT = SQL-RDOCCOY                   <PS010>
           OR RTRNCDE-RDOCNUM       NOT = SQL-RDOCNUM                   <PS010>
              MOVE ENDP                TO RTRNCDE-STATUZ                <PS010>
              GO TO 3790-EXIT                                           <PS010>
           END-IF.                                                      <PS010>
                                                                        <PS010>
           IF RTRNCDE-STATUZ            = O-K                           <PS010>
              MOVE RTRNCDE-SACSCODE    TO SACSCODE  OF ZCRPPF           <PS010>
              MOVE RTRNCDE-SACSCODE    TO WSAA-TV088-SACSCODE           <PS010>
              MOVE RTRNCDE-SACSTYP     TO SACSTYPE  OF ZCRPPF           <PS010>
              MOVE RTRNCDE-SACSTYP     TO WSAA-TV088-SACSTYP            <PS010>
              MOVE RTRNCDE-TRANDESC    TO TRANDESC OF ZCRPPF            <PS010>
              MOVE RTRNCDE-ORIGAMT     TO DOCORIGAMT OF ZCRPPF          <PS010>
              INITIALIZE                  CHDRENQ-PARAMS                <PS010>
              MOVE RTRNCDE-RDOCCOY     TO CHDRENQ-CHDRCOY               <PS010>
              MOVE RTRNCDE-RLDGACCT(1:8)   TO CHDRENQ-CHDRNUM           <PS010>
              MOVE READR               TO CHDRENQ-FUNCTION              <PS010>
              MOVE CHDRENQREC          TO CHDRENQ-FORMAT                <PS010>
              CALL 'CHDRENQIO'      USING CHDRENQ-PARAMS                <PS010>
              IF  CHDRENQ-STATUZ    NOT = O-K AND MRNF                  <PS010>
                  MOVE CHDRENQ-STATUZ  TO SYSR-STATUZ                   <PS010>
                  MOVE CHDRENQ-PARAMS  TO SYSR-PARAMS                   <PS010>
                  PERFORM 600-FATAL-ERROR                               <PS010>
              END-IF                                                    <PS010>
              IF  CHDRENQ-STATUZ        = O-K                           <PS010>
                  MOVE RTRNCDE-RLDGACCT(1:8) TO CHDRNUM  OF ZCRPPF      <PS010>
                  INITIALIZE              CLTS-PARAMS                   <PS010>
                  MOVE 'CN'            TO CLTS-CLNTPFX                  <PS010>
                  MOVE CHDRENQ-COWNCOY TO CLTS-CLNTCOY                  <PS010>
                  MOVE CHDRENQ-COWNNUM TO CLTS-CLNTNUM                  <PS010>
                  MOVE READR           TO CLTS-FUNCTION                 <PS010>
                  MOVE CLTSREC         TO CLTS-FORMAT                   <PS010>
                                                                        <PS010>
                  CALL 'CLTSIO'     USING CLTS-PARAMS                   <PS010>
                                                                        <PS010>
                  IF  CLTS-STATUZ             NOT = O-K AND MRNF        <PS010>
                      MOVE CLTS-STATUZ TO SYSR-PARAMS                   <PS010>
                      MOVE CLTS-PARAMS TO SYSR-PARAMS                   <PS010>
                      PERFORM 600-FATAL-ERROR                           <PS010>
                  END-IF                                                <PS010>
                  MOVE SPACES          TO NMAD-NAMADRS-REC              <PS010>
                  MOVE CLTS-CLNTPFX    TO NMAD-CLNT-PREFIX              <PS010>
                  MOVE CHDRENQ-COWNCOY TO NMAD-CLNT-COMPANY             <PS010>
                  MOVE CHDRENQ-COWNNUM TO NMAD-CLNT-NUMBER              <PS010>
                  MOVE 'E'             TO NMAD-LANGUAGE                 <PS010>
                  MOVE 'PYNMN'         TO NMAD-FUNCTION                 <PS010>
                  CALL 'NAMADRS'       USING NMAD-NAMADRS-REC           <PS010>
                  IF NMAD-STATUZ              NOT = O-K                 <PS010>
                  AND                         NOT = MRNF                <PS010>
                     MOVE NMAD-NAMADRS-REC    TO SYSR-PARAMS            <PS010>
                     MOVE NMAD-STATUZ         TO SYSR-STATUZ            <PS010>
                     PERFORM 600-FATAL-ERROR                            <PS010>
                  END-IF                                                <PS010>
                  IF NMAD-STATUZ              = MRNF                    <PS010>
                     MOVE SPACES              TO OWNERNAME  OF  ZCRPPF  <PS010>
                  ELSE                                                  <PS010>
                     MOVE NMAD-NAME           TO OWNERNAME  OF  ZCRPPF  <PS010>
                  END-IF                                                <PS010>
              ELSE                                                      <PS010>
                  MOVE SPACES          TO CHDRNUM   OF ZCRPPF           <PS010>
                  MOVE SPACES          TO OWNERNAME OF ZCRPPF           <PS010>
              END-IF                                                    <PS010>
              PERFORM A2000-READ-TV088                                  <PS010>
              PERFORM A3000-MOVE-VALUE-AGAIN                            <PS010>
           END-IF.                                                      <PS010>
                                                                        <PS010>
           MOVE NEXTR                  TO RTRNCDE-FUNCTION.             <PS010>
           GO TO 3720-CALL-IO.                                          <PS010>
      *                                                                 <PS010>
       3790-EXIT.                                                       <PS010>
           EXIT.                                                        <PS010>
      /                                                                 <PS010>
