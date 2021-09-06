      * Generation Parameters SCRVER(02)               Do Not Delete!   <S9503>
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P5140.
      *
      * Copyright 1986-2020, Computer Sciences Corporation.
      *
      *REMARKS.
      *
      * This program is called from Contract Enquiries Extra Details.
      * Details are only shown for Flexible Premium contracts.
      *
      * The screen will show details of prem billed, received and the
      * minimum amount required (of the premium billed) before overdue
      * processing is activated.
      *
      * Details are cumulative as they are from the FPRM record not
      * the FPCO (the FPCO is held at coverage/target year level).
      *                                                                     *
      *****************************************************************
      *              AMENDMENT  HISTORY                               *
      *****************************************************************
      * DATE.....   BY..   AMENDMENT...............  NUMBER
      *
      * DD/MM/YY    X.X.   xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  NNN
      ***********************************************************************
      * ......... New Version of the Amendment History.                     *
      *                                                                     *
      ***********************************************************************
      *           AMENDMENT  HISTORY                                        *
      ***********************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....                               *
      *                                                                     *
      * 10/01/96  01/01   D9604        Peter Evans                          *
      *           New screen for enquiry on flexible premium contracts      *
      *                                                                     *
      * 17/10/97  01/01   AY2K         Chuan Heok of Continuum Singap       *
      *           Code standardisation for client/server conversion.        *
      *                                                                     *
      * 29/11/97    DUNC  SMART 9503 Conv for Client/Server.        <S9503>
      *                                                                     *
      * 30/03/20  01/01   CS020        Van Bao Tuyen - IT                   *
      *           Add field total apply premium for 1 year on screen.       *
      *                                                                     *
      * 23/06/20  01/01   CS020        Thanh Do                             *
      *           Show Oldest Billed Amount on screen.                      *
      *                                                                     *
      * 22/09/20  01/01   CS023        Mai Yen Phi - IT                     *
      *           Recompile                                                 *
      *                                                                     *
      **DD/MM/YY*************************************************************
      *
      *
      /
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       01  WSAA-PROG                   PIC X(05) VALUE 'P5140'.
       01  WSAA-VERSION                PIC X(02) VALUE '01'.
       01  WSAA-BILLFREQ               PIC 9(02).
      *01  WSAA-T5729-SUB              PIC S9(5) COMP-3.
      *01       T5729-IX               PIC S9(2) COMP-3.

      *01  WSAA-DURATION-ARRAY.
      *    03 WSAA-DURATIONS   OCCURS 4.
      *       05 WSAA-DURATION         PIC 9(4).
      *
      *01      WSAA-OVERDUE-MIN             PIC S9(03).
      *01      WSAA-TARGET-MAX              PIC S9(05).
      *01      WSAA-TARGET-MIN              PIC S9(03).
                                                                        <CS020>
       01  WSAA-POLYR-IF               PIC S9(02).                      <CS020>
       01  WSAA-DAEXPY                 PIC S9(03) VALUE 0.              <CS020>
       01  WSAA-TODAY                  PIC 9(08).                       <CS020>
       01  WSAA-LAST-AS                PIC 9(08).                       <CS020>
       01  WSAA-LAST-AS-TMP            PIC 9(08).                       <CS020>
       01  WSAA-LAST-AS-TEMP           PIC 9(08).                       <CS020>
       01  WSAA-TOTPREAMT              PIC S9(15)V9(02).                <CS020>
       01  WSAA-SACSCURBAL             PIC S9(15)V9(02).                <CS020>
       01  WSAA-OLDESTBILAMT           PIC S9(15)V9(02).                <CS020>
       01  WSAA-RLDGACCT.                                               <CS020>
           03  WSAA-CHDRNUM            PIC X(08).                       <CS020>
           03  WSAA-LIFE               PIC X(02).                       <CS020>
           03  WSAA-COVERAGE           PIC X(02).                       <CS020>
           03  WSAA-RIDER              PIC X(02).                       <CS020>
           03  WSAA-PLSFX              PIC X(02).                       <CS020>
       01  WSAA-FLEX-PREM-FLAG         PIC X(01).                       <CS020>
           88  WSAA-FLEX-PREM          VALUE 'Y'.                       <CS020>
           88  WSAA-NON-FLEX-PREM      VALUE 'N'.                       <CS020>
       01  WSAA-UL34-FLAG              PIC X(01).                       <CS020>
           88  WSAA-UL34-Y             VALUE 'Y'.                       <CS020>
           88  WSAA-UL34-N             VALUE 'N'.                       <CS020>

       01  ERRORS.
           03  E005                    PIC X(04) VALUE 'E005'.
      *
       01  TABLES.
           03  T5688                   PIC X(05) VALUE 'T5688'.
           03  T3588                   PIC X(05) VALUE 'T3588'.
           03  T3623                   PIC X(05) VALUE 'T3623'.
           03  T5729                   PIC X(05) VALUE 'T5729'.
           03  TV078                   PIC X(05) VALUE 'TV078'.         <CS020>
           03  TV103                   PIC X(05) VALUE 'TV103'.         <CS020>
           03  TZ028                   PIC X(05) VALUE 'TZ028'.         <CS020>
      *
       01  FORMATS.
           03  PAYRREC                 PIC X(10) VALUE 'PAYRREC'.
           03  FPRMREC                 PIC X(10) VALUE 'FPRMREC'.
           03  CHDRENQREC              PIC X(10) VALUE 'CHDRENQREC'.
           03  DESCREC                 PIC X(10) VALUE 'DESCREC'.
           03  ITEMREC                 PIC X(10) VALUE 'ITEMREC'.
           03  ACMVENQREC              PIC X(10) VALUE 'ACMVENQREC'.    <CS020>
           03  ACBLREC                 PIC X(10) VALUE 'ACBLREC'.       <CS020>
           03  LINSRNLREC              PIC X(10) VALUE 'LINSRNLREC'.    <CS020>
           03  COVRREC                 PIC X(10) VALUE 'COVRREC'.       <CS020>
      /
           COPY VARCOM.
      *
           COPY PAYRSKM.
           COPY FPRMSKM.
           COPY CHDRENQSKM.
           COPY DESCSKM.
           COPY ITEMSKM.
           COPY ITDMSKM.
           COPY COVRSKM.                                                <CS020>
           COPY ACBLENQSKM.                                             <CS020>
           COPY ACMVENQSKM.                                             <CS020>
           COPY DATCON1REC.                                             <CS020>
           COPY DATCON2REC.                                             <CS020>
           COPY LINSRNLSKM.                                             <CS020>

           COPY SYSERRREC.
      *
           COPY OPSTATSREC.
      *
           COPY T5729REC.
           COPY TV078REC.                                               <CS020>
           COPY TZ028REC.                                               <CS020>
           COPY DATCON3REC.

      ***  COPY SCRNPARAMS.                                             <S9503>
      /
      ***  COPY S5140SCR.                                               <S9503>
      /

       LINKAGE SECTION.
      * Screen copybooks are now part of the linkage.                   <S9503>
      /                                                                 <S9503>
           COPY SCRNPARAMS.                                             <S9503>
      /                                                                 <S9503>
           COPY S5140SCR.                                               <S9503>

           COPY WSSPCOMN.

       01  WSSP-USER-AREA              PIC X(768).
      /
      * Statement now includes screen copybooks.                        <S9503>
       PROCEDURE DIVISION USING WSSP-COMMON-AREA WSSP-USER-AREA         <S9503>
                                               SCRN-SCREEN-PARAMS       <S9503>
                                               S5140-DATA-AREA      .   <S9503>

      *                                                                 <S9503>
      * MAINF has been replaced by MAING as the screen                  <S9503>
      * or driver now calls the program.                                <S9503>
      *                                                                 <S9503>
           COPY MAING.                                                  <S9503>
      /
      *****************************************************************
      *      INITIALISE FIELDS FOR SHOWING ON SCREEN
      *****************************************************************
      *
       1000-INITIALISE SECTION.
      *************************
      *
       1010-INITIALISE.

      **** MOVE SPACES                 TO S5140-DATA-AREA.              <CS020>
           INITIALIZE                  S5140-DATA-AREA.                 <CS020>

      *    Dummy field initilisation for prototype version.
           MOVE ZERO
                TO S5140-ANNTARPRM                .
           MOVE MAXDATE
                TO S5140-BTDATE                   .
           MOVE ZERO
                TO S5140-INSTALMENT               .
           MOVE ZERO
                TO S5140-MIN-PRM-REQD            .
           MOVE ZERO
                TO S5140-TOTAL-BILLED             .
           MOVE ZERO
                TO S5140-TOTAL-RECD               .
           MOVE ZERO
                TO S5140-VARNCE                   .
           MOVE ZERO                                                    <CS020>
                TO S5140-ZTOTPREMYR               .                     <CS020>
           MOVE ZERO                                                    <CS020>
                TO S5140-UNBILPRM                 .                     <CS020>
      *    Set screen fields

      * Read CHDRENQ (RETRV)  in  order to obtain the contract header
      * information.

           MOVE 'RETRV'                TO CHDRENQ-FUNCTION.
           CALL 'CHDRENQIO'         USING CHDRENQ-PARAMS.
      *
           IF CHDRENQ-STATUZ          NOT = O-K
               MOVE CHDRENQ-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
           MOVE CHDRENQ-CHDRNUM        TO S5140-CHDRNUM.
           MOVE CHDRENQ-CNTTYPE        TO S5140-CNTTYPE.
           MOVE CHDRENQ-CNTCURR        TO S5140-CNTCURR.
           MOVE CHDRENQ-REGISTER       TO S5140-REGISTER.

      *    Obtain the Contract Type description from T5688.

           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE 'IT'                   TO DESC-DESCPFX.
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.
           MOVE T5688                  TO DESC-DESCTABL.
           MOVE CHDRENQ-CNTTYPE        TO DESC-DESCITEM.
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.
           MOVE READR                  TO DESC-FUNCTION.
      *
           CALL 'DESCIO' USING DESC-PARAMS.
           IF   DESC-STATUZ            NOT = O-K
                                   AND NOT = MRNF
                MOVE DESC-PARAMS       TO SYSR-PARAMS
                PERFORM 600-FATAL-ERROR.
      *
           IF   DESC-STATUZ            = MRNF
                MOVE ALL '?'           TO S5140-LONGDESC
           ELSE
                MOVE DESC-LONGDESC     TO S5140-LONGDESC.
      *
      *    Obtain the Contract Status description from T3623.
      *
           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE 'IT'                   TO DESC-DESCPFX.
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.
           MOVE T3623                  TO DESC-DESCTABL.
           MOVE CHDRENQ-STATCODE       TO DESC-DESCITEM.
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.
           MOVE READR                  TO DESC-FUNCTION.
      *
           CALL 'DESCIO' USING DESC-PARAMS.
           IF   DESC-STATUZ            NOT = O-K
                                   AND NOT = MRNF
                MOVE DESC-PARAMS       TO SYSR-PARAMS
                PERFORM 600-FATAL-ERROR.
      *
           IF   DESC-STATUZ            = MRNF
                MOVE ALL '?'           TO S5140-CHDRSTATUS
           ELSE
                MOVE DESC-SHORTDESC    TO S5140-CHDRSTATUS.
      *
      *    Obtain the Premuim Status description from T3588.
      *
           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE 'IT'                   TO DESC-DESCPFX.
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.
           MOVE T3588                  TO DESC-DESCTABL.
           MOVE CHDRENQ-PSTATCODE      TO DESC-DESCITEM.
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.
           MOVE READR                  TO DESC-FUNCTION.
      *
           CALL 'DESCIO' USING DESC-PARAMS.
           IF   DESC-STATUZ            NOT = O-K
                                   AND NOT = MRNF
                MOVE DESC-PARAMS       TO SYSR-PARAMS
                PERFORM 600-FATAL-ERROR.
      *
           IF   DESC-STATUZ            = MRNF
                MOVE ALL '?'           TO S5140-PREMSTATUS
           ELSE
                MOVE DESC-SHORTDESC    TO S5140-PREMSTATUS.

      * Read PAYR file:

           MOVE SPACES                TO PAYR-DATA-KEY.
           MOVE WSSP-CHDR-CHDRCOY     TO PAYR-CHDRCOY.
           MOVE WSSP-CHDR-CHDRNUM     TO PAYR-CHDRNUM.
           MOVE 1                     TO PAYR-PAYRSEQNO.
           MOVE 1                     TO PAYR-VALIDFLAG.
           MOVE PAYRREC               TO PAYR-FORMAT.
           MOVE READR                 TO PAYR-FUNCTION.

           CALL 'PAYRIO'              USING PAYR-PARAMS.

           IF  PAYR-STATUZ            NOT = O-K
              MOVE PAYR-STATUZ         TO SYSR-STATUZ
              MOVE PAYR-PARAMS        TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.


      * Now read the FPRM file:

           MOVE SPACES                TO FPRM-DATA-KEY.
           MOVE PAYR-CHDRCOY          TO FPRM-CHDRCOY.
           MOVE PAYR-CHDRNUM          TO FPRM-CHDRNUM.
           MOVE 1                     TO FPRM-PAYRSEQNO.
           MOVE FPRMREC               TO FPRM-FORMAT.
           MOVE READR                 TO FPRM-FUNCTION.

           CALL 'FPRMIO'              USING FPRM-PARAMS.

           IF  FPRM-STATUZ            NOT = O-K
              MOVE FPRM-STATUZ        TO SYSR-STATUZ
              MOVE FPRM-PARAMS        TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

      * Now set up the rest of the fields:

           MOVE PAYR-BILLFREQ         TO WSAA-BILLFREQ.

           COMPUTE S5140-ANNTARPRM = PAYR-SINSTAMT06
                                   * WSAA-BILLFREQ
           END-COMPUTE.

           MOVE PAYR-SINSTAMT06       TO S5140-INSTALMENT.
           MOVE PAYR-BILLFREQ         TO S5140-BILLFREQ.
           MOVE PAYR-BTDATE           TO S5140-BTDATE.

           IF FPRM-TOTAL-RECD >= FPRM-MIN-PRM-REQD
              MOVE ZEROS              TO S5140-MIN-PRM-REQD
           ELSE
              COMPUTE S5140-MIN-PRM-REQD = FPRM-MIN-PRM-REQD
                                        - FPRM-TOTAL-RECD
              END-COMPUTE
           END-IF.

           MOVE FPRM-TOTAL-BILLED     TO S5140-TOTAL-BILLED.
           MOVE FPRM-TOTAL-RECD       TO S5140-TOTAL-RECD.

           COMPUTE S5140-VARNCE  = FPRM-TOTAL-RECD
                                 - FPRM-TOTAL-BILLED
           END-COMPUTE.
      *                                                                 <CS020>
      * Calculation total premium apply for contract in year contract   <CS020>
      *                                                                 <CS020>
           PERFORM 1100-GET-LAST-AS.                                    <CS020>
                                                                        <CS020>
           PERFORM 1200-READ-COVR.                                      <CS020>
                                                                        <CS020>
           PERFORM 1400-READ-ACBL.                                      <CS020>
           ADD WSAA-SACSCURBAL         TO WSAA-TOTPREAMT.               <CS020>
                                                                        <CS020>
           MOVE WSAA-TOTPREAMT         TO S5140-ZTOTPREMYR.             <CS020>

           PERFORM 1500-GET-OLDEST-BILLAMT.                             <CS020>

       1090-EXIT.
            EXIT.
      /
       1100-GET-LAST-AS SECTION.                                        <CS020>
      ***************************                                       <CS020>
       1110-START.                                                      <CS020>
           MOVE TDAY                   TO DTC1-FUNCTION.                <CS020>
           CALL 'DATCON1'           USING DTC1-DATCON1-REC.             <CS020>
           MOVE DTC1-INT-DATE          TO WSAA-TODAY.                   <CS020>
                                                                        <CS020>
           MOVE CHDRENQ-OCCDATE   TO DTC3-INT-DATE-1.                   <CS020>
           MOVE WSAA-TODAY        TO DTC3-INT-DATE-2.                   <CS020>
           MOVE '01'              TO DTC3-FREQUENCY.                    <CS020>
           CALL 'DATCON3' USING        DTC3-DATCON3-REC.                <CS020>
           IF DTC3-STATUZ              NOT = O-K                        <CS020>
               MOVE DTC3-STATUZ        TO SYSR-STATUZ                   <CS020>
               MOVE DTC3-DATCON3-REC   TO SYSR-PARAMS                   <CS020>
               PERFORM 600-FATAL-ERROR                                  <CS020>
           END-IF.                                                      <CS020>
           MOVE DTC3-FREQ-FACTOR  TO WSAA-POLYR-IF.                     <CS020>
                                                                        <CS020>
           INITIALIZE                DTC2-DATCON2-REC.                  <CS020>
           MOVE WSAA-POLYR-IF     TO DTC2-FREQ-FACTOR.                  <CS020>
           MOVE '01'              TO DTC2-FREQUENCY.                    <CS020>
           MOVE CHDRENQ-OCCDATE   TO DTC2-INT-DATE-1.                   <CS020>
           CALL 'DATCON2' USING        DTC2-DATCON2-REC.                <CS020>
           IF DTC2-STATUZ              = BOMB                           <CS020>
               MOVE DTC2-STATUZ        TO SYSR-STATUZ                   <CS020>
               MOVE DTC2-DATCON2-REC   TO SYSR-PARAMS                   <CS020>
               PERFORM 600-FATAL-ERROR                                  <CS020>
           END-IF.                                                      <CS020>
           MOVE DTC2-INT-DATE-2   TO WSAA-LAST-AS-TEMP.                 <CS020>
                                                                        <CS020>
           PERFORM A1100-READ-TV078.                                    <CS020>
                                                                        <CS020>
       1190-EXIT.                                                       <CS020>
            EXIT.                                                       <CS020>
      /                                                                 <CS020>
                                                                        <CS020>
       1200-READ-COVR SECTION.                                          <CS020>
      *************************                                         <CS020>
       1210-START.                                                      <CS020>
           MOVE ZEROES                 TO WSAA-TOTPREAMT.               <CS020>
           INITIALIZE                     COVR-PARAMS.                  <CS020>
           MOVE CHDRENQ-CHDRCOY        TO COVR-CHDRCOY.                 <CS020>
           MOVE CHDRENQ-CHDRNUM        TO COVR-CHDRNUM.                 <CS020>
           MOVE SPACES                 TO COVR-PLAN-SUFFIX.             <CS020>
           MOVE COVRREC                TO COVR-FORMAT.                  <CS020>
           MOVE BEGN                   TO COVR-FUNCTION.                <CS020>
                                                                        <CS020>
       1220-CALL.                                                       <CS020>
           CALL 'COVRIO'           USING COVR-PARAMS.                   <CS020>
                                                                        <CS020>
           IF COVR-STATUZ           NOT = O-K                           <CS020>
           AND COVR-STATUZ          NOT = ENDP                          <CS020>
               MOVE COVR-STATUZ        TO SYSR-STATUZ                   <CS020>
               MOVE COVR-PARAMS        TO SYSR-PARAMS                   <CS020>
               PERFORM 600-FATAL-ERROR                                  <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
           IF COVR-CHDRCOY          NOT = CHDRENQ-CHDRCOY               <CS020>
           OR COVR-CHDRNUM          NOT = CHDRENQ-CHDRNUM               <CS020>
           OR COVR-STATUZ               = ENDP                          <CS020>
              MOVE ENDP                TO COVR-STATUZ                   <CS020>
              GO TO 1290-EXIT                                           <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
           IF COVR-VALIDFLAG            = '2'                           <CS020>
              GO TO 1280-NEXTR                                          <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
           MOVE SPACES                 TO WSAA-RLDGACCT.                <CS020>
           MOVE COVR-CHDRNUM           TO WSAA-CHDRNUM.                 <CS020>
           MOVE COVR-LIFE              TO WSAA-LIFE.                    <CS020>
           MOVE COVR-COVERAGE          TO WSAA-COVERAGE.                <CS020>
           MOVE COVR-RIDER             TO WSAA-RIDER.                   <CS020>
           MOVE ZEROES                 TO WSAA-PLSFX.                   <CS020>
                                                                        <CS020>
           IF WSAA-FLEX-PREM                                            <CS020>
              IF COVR-STAT-FUND        NOT = 'U'                        <CS020>
                 PERFORM A1200-RECALC-LAST-AS                           <CS020>
                 MOVE WSAA-LAST-AS-TMP    TO WSAA-LAST-AS               <CS020>
              ELSE                                                      <CS020>
                 MOVE WSAA-LAST-AS-TEMP   TO WSAA-LAST-AS               <CS020>
              END-IF                                                    <CS020>
           ELSE                                                         <CS020>
              PERFORM B1200-READ-TV103-TZ028                            <CS020>
              IF WSAA-UL34-Y                                            <CS020>
                 IF COVR-STAT-FUND        NOT = 'U'                     <CS020>
                    PERFORM A1200-RECALC-LAST-AS                        <CS020>
                    MOVE WSAA-LAST-AS-TMP    TO WSAA-LAST-AS            <CS020>
                 ELSE                                                   <CS020>
                    MOVE WSAA-LAST-AS-TEMP   TO WSAA-LAST-AS            <CS020>
                 END-IF                                                 <CS020>
              ELSE                                                      <CS020>
                 MOVE WSAA-LAST-AS-TEMP   TO WSAA-LAST-AS               <CS020>
              END-IF                                                    <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
           PERFORM 1300-SUM-ACMV.                                       <CS020>
                                                                        <CS020>
       1280-NEXTR.                                                      <CS020>
           MOVE NEXTR               TO COVR-FUNCTION.                   <CS020>
           GO TO 1220-CALL.                                             <CS020>
                                                                        <CS020>
       1290-EXIT.                                                       <CS020>
            EXIT.                                                       <CS020>
      /                                                                 <CS020>
       1300-SUM-ACMV SECTION.                                           <CS020>
      ************************                                          <CS020>
       1310-START.                                                      <CS020>
      *                                                                 <CS020>
      *    Lopp ACMVENQ to sum total premium apply in 1 year contract.  <CS020>
      *                                                                 <CS020>
           INITIALIZE                     ACMVENQ-PARAMS.               <CS020>
           MOVE CHDRENQ-CHDRCOY        TO ACMVENQ-RLDGCOY.              <CS020>
           MOVE WSAA-RLDGACCT          TO ACMVENQ-RLDGACCT.             <CS020>
           MOVE WSAA-LAST-AS           TO ACMVENQ-EFFDATE.              <CS020>
           MOVE BEGN                   TO ACMVENQ-FUNCTION.             <CS020>
                                                                        <CS020>
       1320-CALL.                                                       <CS020>
           CALL 'ACMVENQIO'         USING ACMVENQ-PARAMS.               <CS020>
                                                                        <CS020>
           IF  ACMVENQ-STATUZ       NOT = O-K                           <CS020>
           AND ACMVENQ-STATUZ       NOT = ENDP                          <CS020>
               MOVE ACMVENQ-PARAMS     TO SYSR-PARAMS                   <CS020>
               MOVE ACMVENQ-STATUZ     TO SYSR-STATUZ                   <CS020>
               PERFORM 600-FATAL-ERROR                                  <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
           IF  ACMVENQ-RLDGCOY      NOT = CHDRENQ-CHDRCOY               <CS020>
           OR  ACMVENQ-RLDGACCT     NOT = WSAA-RLDGACCT                 <CS020>
           OR  ACMVENQ-STATUZ           = ENDP                          <CS020>
           OR  ACMVENQ-EFFDATE          > WSAA-TODAY                    <CS020>
               MOVE ENDP               TO ACMVENQ-STATUZ                <CS020>
               GO TO 1390-EXIT                                          <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
           IF ACMVENQ-SACSCODE          = 'LE' AND                      <CS020>
            ( ACMVENQ-SACSTYP           = 'LP' OR 'SP' )                <CS020>
              ADD ACMVENQ-ACCTAMT      TO WSAA-TOTPREAMT                <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
       1380-NEXTR.                                                      <CS020>
           MOVE NEXTR               TO ACMVENQ-FUNCTION.                <CS020>
           GO TO 1320-CALL.                                             <CS020>
                                                                        <CS020>
       1390-EXIT.                                                       <CS020>
            EXIT.                                                       <CS020>
      /                                                                 <CS020>
       1400-READ-ACBL SECTION.                                          <CS020>
      *************************                                         <CS020>
       1410-START.                                                      <CS020>
           MOVE ZEROES                 TO WSAA-SACSCURBAL.              <CS020>
           MOVE SPACES                 TO ACBLENQ-PARAMS.               <CS020>
           MOVE CHDRENQ-CHDRCOY        TO ACBLENQ-RLDGCOY.              <CS020>
           MOVE SPACES                 TO WSAA-RLDGACCT.                <CS020>
           MOVE CHDRENQ-CHDRNUM        TO WSAA-CHDRNUM.                 <CS020>
           MOVE WSAA-RLDGACCT          TO ACBLENQ-RLDGACCT.             <CS020>
           MOVE 'LP'                   TO ACBLENQ-SACSCODE.             <CS020>
           MOVE 'S '                   TO ACBLENQ-SACSTYP.              <CS020>
           MOVE 'VND'                  TO ACBLENQ-ORIGCURR.             <CS020>
           MOVE READR                  TO ACBLENQ-FUNCTION.             <CS020>
           MOVE ACBLREC                TO ACBLENQ-FORMAT.               <CS020>
                                                                        <CS020>
           CALL 'ACBLENQIO'         USING ACBLENQ-PARAMS.               <CS020>
                                                                        <CS020>
           IF  ACBLENQ-STATUZ       NOT = O-K                           <CS020>
           AND                      NOT = MRNF                          <CS020>
               MOVE ACBLENQ-PARAMS     TO SYSR-PARAMS                   <CS020>
               PERFORM 600-FATAL-ERROR                                  <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
           IF  ACBLENQ-STATUZ           = MRNF                          <CS020>
               GO TO 1490-EXIT                                          <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
           COMPUTE WSAA-SACSCURBAL      = ACBLENQ-SACSCURBAL * -1.      <CS020>
                                                                        <CS020>
       1490-EXIT.                                                       <CS020>
            EXIT.                                                       <CS020>
      /                                                                 <CS020>
       1500-GET-OLDEST-BILLAMT SECTION.                                 <CS020>
      *********************************                                 <CS020>
       1500-START.                                                      <CS020>
      *                                                                 <CS020>
           MOVE ZEROES                 TO S5140-UNBILPRM.               <CS020>
                                                                        <CS020>
           INITIALIZE                  LINSRNL-PARAMS.                  <CS020>
           MOVE CHDRENQ-CHDRCOY        TO LINSRNL-CHDRCOY.              <CS020>
           MOVE CHDRENQ-CHDRNUM        TO LINSRNL-CHDRNUM.              <CS020>
           MOVE ZEROES                 TO LINSRNL-INSTFROM.             <CS020>
           MOVE LINSRNLREC             TO LINSRNL-FORMAT.               <CS020>
           MOVE BEGN                   TO LINSRNL-FUNCTION.             <CS020>
                                                                        <CS020>
           CALL 'LINSRNLIO'            USING LINSRNL-PARAMS.            <CS020>
                                                                        <CS020>
           IF LINSRNL-STATUZ           NOT = O-K                        <CS020>
           AND                         NOT = ENDP                       <CS020>
               MOVE LINSRNL-PARAMS     TO SYSR-PARAMS                   <CS020>
               PERFORM 600-FATAL-ERROR                                  <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
           IF LINSRNL-STATUZ           = ENDP                           <CS020>
           OR LINSRNL-CHDRCOY      NOT = CHDRENQ-CHDRCOY                <CS020>
           OR LINSRNL-CHDRNUM      NOT = CHDRENQ-CHDRNUM                <CS020>
               GO TO 1519-EXIT                                          <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
           MOVE LINSRNL-INSTAMT06      TO S5140-UNBILPRM.               <CS020>
      *                                                                 <CS020>
       1519-EXIT.                                                       <CS020>
           EXIT.                                                        <CS020>
      /                                                                 <CS020>
      *
      *    Sections performed from the 1000 section above.
      *

      /
      *****************************************************************
      *     RETRIEVE SCREEN FIELDS AND EDIT
      *****************************************************************
      *
       PRE-SCREEN-EDIT SECTION.                                         <S9503>
      ************************                                          <S9503>
      *                                                                 <S9503>
       PRE-START.                                                       <S9503>
      *                                                                 <S9503>
                                                                        <S9503>
           GO TO PRE-EXIT.                                              <S9503>
      *                                                                 <S9503>
       PRE-EXIT.                                                        <S9503>
           EXIT.                                                        <S9503>
      /                                                                 <S9503>
       2000-SCREEN-EDIT SECTION.
      **************************
      *
       2010-SCREEN-IO.
      *    CALL 'S5140IO' USING SCRN-SCREEN-PARAMS                      <S9503>
      *                          S5140-DATA-AREA.                       <S9503>
      * Screen errors are now handled in the calling program.           <S9503>
      *    PERFORM 200-SCREEN-ERRORS.                                   <S9503>
           MOVE O-K                    TO WSSP-EDTERROR.

       2020-VALIDATE.

      *
      *    Validate fields
      *

       2080-CHECK-FOR-ERRORS.
           IF S5140-ERROR-INDICATORS NOT = SPACES
              MOVE 'Y'                 TO WSSP-EDTERROR.

       2090-EXIT.
            EXIT.
      /

      *
      *    Sections performed from the 2000 section above.
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
      *  Update database files as required / WSSP
      *

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
           ADD 1                       TO WSSP-PROGRAM-PTR.
      *
       4090-EXIT.
            EXIT.
      /                                                                 <CS020>
       A1100-READ-TV078 SECTION.                                        <CS020>
      ***************************                                       <CS020>
       A1110-START.                                                     <CS020>
      *                                                                 <CS020>
           SET WSAA-NON-FLEX-PREM      TO TRUE.                         <CS020>
           INITIALIZE                     ITDM-PARAMS.                  <CS020>
           MOVE WSSP-COMPANY           TO ITDM-ITEMCOY.                 <CS020>
           MOVE 'IT'                   TO ITDM-ITEMPFX.                 <CS020>
           MOVE TV078                  TO ITDM-ITEMTABL.                <CS020>
           MOVE CHDRENQ-CNTTYPE        TO ITDM-ITEMITEM.                <CS020>
           MOVE 99999999               TO ITDM-ITMFRM.                  <CS020>
           MOVE BEGN                   TO ITDM-FUNCTION.                <CS020>
                                                                        <CS020>
           CALL 'ITDMIO'  USING ITDM-PARAMS.                            <CS020>
                                                                        <CS020>
           IF  ITDM-STATUZ          NOT = O-K                           <CS020>
           AND ITDM-STATUZ          NOT = ENDP                          <CS020>
              MOVE ITDM-PARAMS      TO SYSR-PARAMS                      <CS020>
              PERFORM 600-FATAL-ERROR                                   <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
           IF ITDM-ITEMCOY          NOT = WSSP-COMPANY                  <CS020>
           OR ITDM-ITEMTABL         NOT = TV078                         <CS020>
           OR ITDM-ITEMITEM         NOT = CHDRENQ-CNTTYPE               <CS020>
           OR ITDM-STATUZ               = ENDP                          <CS020>
              MOVE ENDP             TO ITDM-STATUZ                      <CS020>
              GO TO A1190-EXIT                                          <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
           MOVE ITDM-GENAREA        TO TV078-TV078-REC.                 <CS020>
           IF WSAA-POLYR-IF         >= TV078-NOFYEAR                    <CS020>
              SET WSAA-FLEX-PREM    TO TRUE                             <CS020>
           END-IF.                                                      <CS020>
      *                                                                 <CS020>
       A1190-EXIT.                                                      <CS020>
             EXIT.                                                      <CS020>
      /                                                                 <CS020>
       A1200-RECALC-LAST-AS SECTION.                                    <CS020>
      *******************************                                   <CS020>
       A1210-START.                                                     <CS020>
      *                                                                 <CS020>
      * Rider traditional collection premium before 15 days so then     <CS020>
      * Last AS = Last AS - 15.                                         <CS020>
      *                                                                 <CS020>
           MOVE 15                     TO WSAA-DAEXPY.                  <CS020>
           COMPUTE WSAA-DAEXPY          = WSAA-DAEXPY * (-1).           <CS020>
           INITIALIZE                     DTC2-DATCON2-REC.             <CS020>
           MOVE WSAA-DAEXPY            TO DTC2-FREQ-FACTOR.             <CS020>
           MOVE 'DY'                   TO DTC2-FREQUENCY.               <CS020>
           MOVE WSAA-LAST-AS-TEMP      TO DTC2-INT-DATE-1.              <CS020>
                                                                        <CS020>
           CALL 'DATCON2'              USING DTC2-DATCON2-REC.          <CS020>
                                                                        <CS020>
           IF DTC2-STATUZ           NOT = O-K                           <CS020>
              MOVE DTC2-DATCON2-REC    TO SYSR-PARAMS                   <CS020>
              MOVE DTC2-STATUZ         TO SYSR-STATUZ                   <CS020>
              PERFORM 600-FATAL-ERROR                                   <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
           MOVE DTC2-INT-DATE-2        TO WSAA-LAST-AS-TMP.             <CS020>
      *                                                                 <CS020>
       A1290-EXIT.                                                      <CS020>
             EXIT.                                                      <CS020>
      /                                                                 <CS020>
       B1200-READ-TV103-TZ028 SECTION.                                  <CS020>
      *********************************                                 <CS020>
       B1210-START.                                                     <CS020>
      *                                                                 <CS020>
           SET WSAA-UL34-N             TO TRUE.                         <CS020>
           INITIALIZE                     ITEM-PARAMS.                  <CS020>
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.                 <CS020>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <CS020>
           MOVE TV103                  TO ITEM-ITEMTABL.                <CS020>
           MOVE CHDRENQ-CNTTYPE        TO ITEM-ITEMITEM.                <CS020>
           MOVE READR                  TO ITEM-FUNCTION.                <CS020>
                                                                        <CS020>
           CALL 'ITEMIO'  USING ITEM-PARAMS.                            <CS020>
                                                                        <CS020>
           IF  ITEM-STATUZ          NOT = O-K                           <CS020>
           AND ITEM-STATUZ          NOT = MRNF                          <CS020>
              MOVE ITEM-PARAMS      TO SYSR-PARAMS                      <CS020>
              PERFORM 600-FATAL-ERROR                                   <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
           IF ITEM-STATUZ               = O-K                           <CS020>
              SET WSAA-UL34-Y       TO TRUE                             <CS020>
              GO TO B1290-EXIT                                          <CS020>
           ELSE                                                         <CS020>
              INITIALIZE                     ITDM-PARAMS                <CS020>
              MOVE WSSP-COMPANY           TO ITDM-ITEMCOY               <CS020>
              MOVE 'IT'                   TO ITDM-ITEMPFX               <CS020>
              MOVE TZ028                  TO ITDM-ITEMTABL              <CS020>
              MOVE CHDRENQ-CNTTYPE        TO ITDM-ITEMITEM              <CS020>
              MOVE 99999999               TO ITDM-ITMFRM                <CS020>
              MOVE BEGN                   TO ITDM-FUNCTION              <CS020>
                                                                        <CS020>
              CALL 'ITDMIO'  USING ITDM-PARAMS                          <CS020>
                                                                        <CS020>
              IF  ITDM-STATUZ          NOT = O-K                        <CS020>
              AND ITDM-STATUZ          NOT = ENDP                       <CS020>
                 MOVE ITDM-PARAMS      TO SYSR-PARAMS                   <CS020>
                 PERFORM 600-FATAL-ERROR                                <CS020>
              END-IF                                                    <CS020>
                                                                        <CS020>
              IF ITDM-ITEMCOY          NOT = WSSP-COMPANY               <CS020>
              OR ITDM-ITEMTABL         NOT = TZ028                      <CS020>
              OR ITDM-ITEMITEM         NOT = CHDRENQ-CNTTYPE            <CS020>
              OR ITDM-STATUZ               = ENDP                       <CS020>
                 MOVE ENDP             TO ITDM-STATUZ                   <CS020>
                 GO TO B1290-EXIT                                       <CS020>
              END-IF                                                    <CS020>
              SET WSAA-UL34-Y          TO TRUE                          <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
      *                                                                 <CS020>
       B1290-EXIT.                                                      <CS020>
             EXIT.                                                      <CS020>
      /                                                                 <CS020>
