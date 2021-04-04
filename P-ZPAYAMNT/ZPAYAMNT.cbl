       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ZPAYAMNT.
      *
      *(c) Copyright Continuum Corporation Ltd.  1986....1995.
      *    All rights reserved.  Continuum Confidential.
      *
      *REMARKS.
      *
      *
      *  Input Values :
      * --------------
      *    Contract Number , Payment purpose
      *
      *  Output Values:
      * --------------
      *    Due Date (if any) , Amount for each payment purpose in TV041
      *
      ***********************************************************************
      *                                                                     *
      * ......... New Version of the Amendment History.                     *
      *                                                                     *
      ***********************************************************************
      *           AMENDMENT  HISTORY                                        *
      ***********************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....                               *
      *                                                                     *
      *                                                                     *
      * 04/04/14  01/01   PHLRMS       Phuong Le Dev                        *
      *           Get amount and due date for payment purpose in TV041      *
      *                                                                     *
      * 02/02/15  01/01   PHE003       Phuong Le Dev                        *
      *           Fix : Check same due date                                 *
      *                                                                     *
      * 22/07/15  01/01   PHE003       Phuong Le Dev                        *
      *           New Status 'UB'                                           *
      *                                                                     *
      * 30/05/18  01/01   NB012        Thanh Do                             *
      *           Add Planned Premium to Premium Due for Purpose= PP.       *
      *                                                                     *
      * 15/10/18  01/01   PHFX23       Phi Tran - IT DEV                    *
      *           Don't Calculate Interest Pending for Coupon and           *
      *           IAV Amt.
      *                                                                     *
      **DD/MM/YY*************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-AS400.
       OBJECT-COMPUTER.    IBM-AS400.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WSAA-SUBR                   PIC X(10) VALUE 'ZPAYAMNT'.
       01  WSAA-VERSION                PIC X(02) VALUE '01'.
       01  WSAA-COMPANY                PIC X(01) VALUE '2'.
       01  WSAA-CURRENCY               PIC X(03) VALUE 'VND'.
       01  WSAA-MAX-DATE               PIC S9(08) VALUE 99999999.       <PHLRMS>
       01  WSAA-FIRST-TIME             PIC X(01).                       <PHLRMS>
       01  WSAA-FIRST-DUEDTE           PIC S9(08).                      <PHLRMS>
       01  WSAA-YEARDUE                PIC 9(03) VALUE 0.               <NB012>
                                                                        <PHLRMS>
       01  WSAA-ARR.                                                    <PHLRMS>
           03  WSAA-ARR-PRMDUE         OCCURS 10.                       <PHLRMS>
               05  WSAA-ITEM-DUE       PIC S9(08).                      <PHLRMS>
               05  WSAA-ITEM-AMOUNT    PIC S9(15)V9(02) COMP-3.         <PHLRMS>

       01  WSAA-VARIABLES.
           03  WSAA-USED-RCPT          PIC X(01).                       <PHLRMS>
           03  WSAA-SKIP-DUE           PIC X(01).                       <PHLRMS>
           03  WSAA-DUE-AMOUNT         PIC S9(15)V9(02) COMP-3.
           03  WSAA-AMOUNT             PIC S9(15)V9(02) COMP-3.
           03  WSAA-COUNT-DUE          PIC 9(02).
           03  IDX                     PIC 9(02).
           03  IDY                     PIC 9(02).                       <PHLRMS>
           03  WSAA-TABLE              PIC X(05).
           03  WSAA-ITEM               PIC X(08).
           03  WSAA-DUEDTE             PIC S9(08).
           03  WSAA-TODAY              PIC S9(08).                      <PHLRMS>
           03  WSAA-PND-TOTAL          PIC S9(10)V9(02) COMP-3.         <PHLRMS>
           03  WSAA-TOPUP-YR1          PIC S9(12).                      <NB012>
           03  WSAA-TOPUP-YR2          PIC S9(12).                      <NB012>
           03  WSAA-TOPUP-YR3          PIC S9(12).                      <NB012>
           03  WSAA-TOPUP-YR4          PIC S9(12).                      <NB012>
           03  WSAA-FOUND              PIC X(01).                       <PHLRMS>
      *
       01  FORMATS.
           03  ITEMREC                 PIC X(10) VALUE 'ITEMREC'.
           03  LINSDRYREC              PIC X(10) VALUE 'LINSDRYREC'.
           03  CHDRLNBREC              PIC X(10) VALUE 'CHDRLNBREC'.
           03  ACBLREC                 PIC X(10) VALUE 'ACBLREC'.       <PHLRMS>
           03  LOANREC                 PIC X(10) VALUE 'LOANREC'.       <PHLRMS>
           03  RCPYPOLREC              PIC X(10) VALUE 'RCPYPOLREC'.
           03  RCPYINFREC              PIC X(10) VALUE 'RCPYINFREC'.
           03  TRRNINFREC              PIC X(10) VALUE 'TRRNINFREC'.
           03  ZPPIENQREC              PIC X(10) VALUE 'ZPPIENQREC'.    <NB012>
      *
       01  TABLES.
           03  TV041                   PIC X(10) VALUE 'TV041'.

           COPY ITEMSKM.
      *
           COPY CHDRLNBSKM.
           COPY INTCALCREC.                                             <PHLRMS>
           COPY ZRDECPLREC.                                             <PHLRMS>
      *
           COPY VARCOM.
           COPY SMTPFXCPY.
      *
           COPY SYSERRREC.
      *
           COPY DATCON1REC.                                             <PHLRMS>
           COPY DATCON3REC.                                             <PHLRMS>
           COPY DATCON4REC.                                             <PHLRMS>
      *
           COPY TV041REC.
      *
           COPY LINSDRYSKM.
           COPY RCPYPOLSKM.
           COPY ACBLENQSKM.
           COPY LOANSKM.                                                <PHLRMS>
           COPY TRRNINFSKM.
           COPY ZPPIENQSKM.                                             <NB012>
      *
       LINKAGE SECTION.

           COPY ZPAYAMTREC.

       PROCEDURE DIVISION           USING ZPAYAMT-RECORD.

      ************************
       000-MAINLINE   SECTION.
      ************************
      *                                                                 <PHLRMS>
       000-MAIN.
      *                                                                 <PHLRMS>
           INITIALIZE                     WSAA-VARIABLES.
                                                                        <PHLRMS>
           MOVE WSAA-SUBR              TO SYSR-SUBRNAME.                <PHLRMS>
           MOVE ZERO                   TO WSAA-COUNT-DUE.               <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Today                                                        <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE TDAY                   TO DTC1-FUNCTION.                <PHLRMS>
           CALL 'DATCON1'              USING DTC1-DATCON1-REC.          <PHLRMS>
           MOVE DTC1-INT-DATE          TO WSAA-TODAY.                   <PHLRMS>

           MOVE O-K                    TO ZPAYAMT-STATUZ.
           MOVE ZEROES                 TO ZPAYAMT-AMOUNT.
           MOVE VRCM-MAX-DATE          TO ZPAYAMT-FIRST-DUEDTE.         <PHLRMS>

           EVALUATE ZPAYAMT-PURCODE
           WHEN 'AP'                                                    <PHLRMS>
               PERFORM A10-ADVANCE-PREMIUM                              <PHLRMS>
           WHEN 'PD'                                                    <PHLRMS>
               PERFORM A00-PREMIUM-DUE                                  <PHLRMS>
           WHEN 'PP'                                                    <NB012>
               PERFORM A00-PREMIUM-DUE                                  <NB012>
      ***  WHEN 'RI'
      ***      PERFORM 300-LOAD-CHDRLNB
      ***      IF  WSAA-COUNT-DUE      > 0
      ***          MOVE WSAA-DUEDTE     TO ZPAYAMT-DUEDTE
      ***          MOVE WSAA-DUE-AMOUNT TO ZPAYAMT-AMOUNT
      ***      END-IF
           WHEN OTHER
               PERFORM B00-OTHERS                                       <PHLRMS>
           END-EVALUATE.
      *
       000-EXIT.
           EXIT PROGRAM.
      /                                                                 <PHLRMS>
       A00-PREMIUM-DUE SECTION.                                         <PHLRMS>
      *************************                                         <PHLRMS>
      *                                                                 <PHLRMS>
       A01-START.                                                       <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Initialize array                                             <PHLRMS>
      *                                                                 <PHLRMS>
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 10               <PHLRMS>
               MOVE WSAA-MAX-DATE      TO WSAA-ITEM-DUE    (IDX)        <PHLRMS>
               MOVE ZEROES             TO WSAA-ITEM-AMOUNT (IDX)        <PHLRMS>
           END-PERFORM.                                                 <PHLRMS>
                                                                        <PHLRMS>
           MOVE ZEROES                 TO WSAA-COUNT-DUE.               <PHLRMS>
           MOVE ZEROES                 TO IDY.                          <PHLRMS>
           MOVE 'Y'                    TO WSAA-FIRST-TIME.              <PHLRMS>
                                                                        <PHLRMS>
           PERFORM 200-LOAD-LINSDRY.                                    <PHLRMS>
                                                                        <PHLRMS>
           IF  WSAA-COUNT-DUE          = 0                              <PHLRMS>
               PERFORM 300-LOAD-CHDRLNB                                 <PHLRMS>
               ADD 1                   TO IDY                           <PHLRMS>
               MOVE WSAA-DUEDTE        TO WSAA-ITEM-DUE    (IDY)        <PHLRMS>
               MOVE WSAA-DUE-AMOUNT    TO WSAA-ITEM-AMOUNT (IDY)        <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  ZPAYAMT-DUEDTE          = WSAA-MAX-DATE                  <PHLRMS>
               MOVE WSAA-ITEM-DUE(1)   TO ZPAYAMT-DUEDTE                <PHLRMS>
               MOVE WSAA-ITEM-AMOUNT(1)                                 <PHLRMS>
                                       TO ZPAYAMT-AMOUNT                <PHLRMS>
           ELSE                                                         <PHLRMS>
               MOVE 'N'                TO WSAA-FOUND                    <PHLRMS>
               PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 10           <PHLRMS>
                             OR WSAA-ITEM-DUE (IDX) = WSAA-MAX-DATE     <PHLRMS>
                   IF  WSAA-ITEM-DUE (IDX) = ZPAYAMT-DUEDTE             <PHLRMS>
                       MOVE WSAA-ITEM-DUE(IDX)    TO ZPAYAMT-DUEDTE     <PHLRMS>
                       MOVE WSAA-ITEM-AMOUNT(IDX) TO ZPAYAMT-AMOUNT     <PHLRMS>
                       MOVE 'Y'        TO WSAA-FOUND                    <PHLRMS>
                   END-IF                                               <PHLRMS>
               END-PERFORM                                              <PHLRMS>
               IF  WSAA-FOUND          = 'N'                            <PHLRMS>
                   MOVE ZEROES         TO ZPAYAMT-AMOUNT                <PHLRMS>
                   MOVE MRNF           TO ZPAYAMT-STATUZ                <PHLRMS>
               END-IF                                                   <PHLRMS>
           END-IF.                                                      <PHLRMS>
           MOVE WSAA-FIRST-DUEDTE      TO ZPAYAMT-FIRST-DUEDTE.         <PHLRMS>
                                                                        <NB012>
      * Add Planned Premium to Premium Due for UL Only:                 <NB012>
                                                                        <NB012>
           PERFORM 900-READ-CHDRLNB.                                    <NB012>
           IF  CHDRLNB-CNTTYPE(1:1)    NOT = 'U'                        <NB012>
               GO TO A09-EXIT                                           <NB012>
           END-IF.                                                      <NB012>
                                                                        <NB012>
      * Go here if UL Product:                                          <NB012>
                                                                        <NB012>
           MOVE ZEROES                 TO WSAA-TOPUP-YR1                <NB012>
                                          WSAA-TOPUP-YR2                <NB012>
                                          WSAA-TOPUP-YR3                <NB012>
                                          WSAA-TOPUP-YR4.               <NB012>
                                                                        <NB012>
           IF ZPAYAMT-PURCODE          = 'PP'                           <NB012>
               PERFORM A20-GET-PLANNED-PREMIUM                          <NB012>
               IF ZPAYAMT-AMOUNT       > ZEROES                         <NB012>
                  PERFORM A30-CALC-DUE-IN-YEAR                          <NB012>
                  PERFORM A40-ADD-PLANNED-PREM                          <NB012>
               END-IF                                                   <NB012>
           END-IF.                                                      <NB012>
      *                                                                 <PHLRMS>
       A09-EXIT.                                                        <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /                                                                 <PHLRMS>
       A10-ADVANCE-PREMIUM SECTION.                                     <PHLRMS>
      *****************************                                     <PHLRMS>
      *                                                                 <PHLRMS>
       A11-START.                                                       <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                     CHDRLNB-PARAMS.               <PHLRMS>
           MOVE WSAA-COMPANY           TO CHDRLNB-CHDRCOY.              <PHLRMS>
           MOVE ZPAYAMT-CHDRNUM        TO CHDRLNB-CHDRNUM.              <PHLRMS>
           MOVE CHDRLNBREC             TO CHDRLNB-FORMAT.               <PHLRMS>
           MOVE READR                  TO CHDRLNB-FUNCTION.             <PHLRMS>
                                                                        <PHLRMS>
           CALL 'CHDRLNBIO'            USING CHDRLNB-PARAMS.            <PHLRMS>
                                                                        <PHLRMS>
           IF  CHDRLNB-STATUZ          NOT = O-K                        <PHLRMS>
               MOVE CHDRLNB-STATUZ     TO SYSR-STATUZ                   <PHLRMS>
               MOVE CHDRLNB-PARAMS     TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           MOVE CHDRLNB-PTDATE         TO ZPAYAMT-DUEDTE.               <PHLRMS>
           MOVE CHDRLNB-PTDATE         TO ZPAYAMT-FIRST-DUEDTE.         <PHLRMS>
           MOVE CHDRLNB-SINSTAMT06     TO ZPAYAMT-AMOUNT.               <PHLRMS>
      *                                                                 <PHLRMS>
       A19-EXIT.                                                        <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /                                                                 <NB012>
       A20-GET-PLANNED-PREMIUM SECTION.                                 <NB012>
      *********************************                                 <NB012>
       A21-START.                                                       <NB012>
      *                                                                 <NB012>
           MOVE ZEROES                 TO WSAA-TOPUP-YR1                <NB012>
                                          WSAA-TOPUP-YR2                <NB012>
                                          WSAA-TOPUP-YR3                <NB012>
                                          WSAA-TOPUP-YR4.               <NB012>
                                                                        <NB012>
           INITIALIZE                  ZPPIENQ-PARAMS.                  <NB012>
           MOVE ZPAYAMT-CHDRNUM        TO ZPPIENQ-CHDRNUM.              <NB012>
           MOVE ZPPIENQREC             TO ZPPIENQ-FORMAT.               <NB012>
           MOVE READR                  TO ZPPIENQ-FUNCTION.             <NB012>
                                                                        <NB012>
           CALL 'ZPPIENQIO'            USING ZPPIENQ-PARAMS.            <NB012>
                                                                        <NB012>
           IF ZPPIENQ-STATUZ           NOT = O-K                        <NB012>
           AND                         NOT = MRNF                       <NB012>
               MOVE ZPPIENQ-STATUZ     TO SYSR-STATUZ                   <NB012>
               MOVE ZPPIENQ-PARAMS     TO SYSR-PARAMS                   <NB012>
               PERFORM 600-FATAL-ERROR                                  <NB012>
           END-IF.                                                      <NB012>
                                                                        <NB012>
           IF ZPPIENQ-STATUZ           = O-K                            <NB012>
               MOVE ZPPIENQ-PREMESTA   TO WSAA-TOPUP-YR1                <NB012>
               MOVE ZPPIENQ-PREMESTB   TO WSAA-TOPUP-YR2                <NB012>
               MOVE ZPPIENQ-PREMESTC   TO WSAA-TOPUP-YR3                <NB012>
               MOVE ZPPIENQ-PREMESTD   TO WSAA-TOPUP-YR4                <NB012>
           END-IF.                                                      <NB012>
      *                                                                 <NB012>
       A29-EXIT.                                                        <NB012>
           EXIT.                                                        <NB012>
      /                                                                 <NB012>
       A30-CALC-DUE-IN-YEAR SECTION.                                    <NB012>
      ******************************                                    <NB012>
       A31-START.                                                       <NB012>
      *                                                                 <NB012>
           MOVE 1                      TO WSAA-YEARDUE.                 <NB012>
           PERFORM 900-READ-CHDRLNB.                                    <NB012>
                                                                        <NB012>
           INITIALIZE                  DTC3-DATCON3-REC.                <NB012>
           MOVE O-K                    TO DTC3-STATUZ.                  <NB012>
           MOVE '01'                   TO DTC3-FREQUENCY.               <NB012>
           MOVE CHDRLNB-OCCDATE        TO DTC3-INT-DATE-1.              <NB012>
           MOVE ZPAYAMT-DUEDTE         TO DTC3-INT-DATE-2.              <NB012>
           MOVE ZEROES                 TO DTC3-FREQ-FACTOR.             <NB012>
                                                                        <NB012>
           CALL 'DATCON3'              USING DTC3-DATCON3-REC.          <NB012>
                                                                        <NB012>
           IF  DTC3-STATUZ             NOT = O-K                        <NB012>
               MOVE DTC3-STATUZ        TO SYSR-STATUZ                   <NB012>
               MOVE DTC3-DATCON3-REC   TO SYSR-PARAMS                   <NB012>
               PERFORM 600-FATAL-ERROR                                  <NB012>
           END-IF.                                                      <NB012>
                                                                        <NB012>
           ADD 1                       TO DTC3-FREQ-FACTOR.             <NB012>
           MOVE DTC3-FREQ-FACTOR       TO WSAA-YEARDUE.                 <NB012>
      *                                                                 <NB012>
       A39-EXIT.                                                        <NB012>
           EXIT.                                                        <NB012>
      /                                                                 <NB012>
       A40-ADD-PLANNED-PREM SECTION.                                    <NB012>
      ******************************                                    <NB012>
       A41-START.                                                       <NB012>
      *                                                                 <NB012>
           IF  WSAA-YEARDUE            <= 1                             <NB012>
               COMPUTE ZPAYAMT-AMOUNT  = ZPAYAMT-AMOUNT                 <NB012>
                                       + WSAA-TOPUP-YR1                 <NB012>
           END-IF.                                                      <NB012>
                                                                        <NB012>
           IF  WSAA-YEARDUE             = 2                             <NB012>
               COMPUTE ZPAYAMT-AMOUNT  = ZPAYAMT-AMOUNT                 <NB012>
                                       + WSAA-TOPUP-YR2                 <NB012>
           END-IF.                                                      <NB012>
                                                                        <NB012>
           IF  WSAA-YEARDUE             = 3                             <NB012>
               COMPUTE ZPAYAMT-AMOUNT  = ZPAYAMT-AMOUNT                 <NB012>
                                       + WSAA-TOPUP-YR3                 <NB012>
           END-IF.                                                      <NB012>
                                                                        <NB012>
           IF  WSAA-YEARDUE            >= 4                             <NB012>
               COMPUTE ZPAYAMT-AMOUNT  = ZPAYAMT-AMOUNT                 <NB012>
                                       + WSAA-TOPUP-YR4                 <NB012>
           END-IF.                                                      <NB012>
      *                                                                 <NB012>
       A49-EXIT.                                                        <NB012>
           EXIT.                                                        <NB012>
      /                                                                 <PHLRMS>
       B00-OTHERS SECTION.                                              <PHLRMS>
      *************************                                         <PHLRMS>
      *                                                                 <PHLRMS>
       B10-START.                                                       <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE TV041                  TO WSAA-TABLE.                   <PHLRMS>
           MOVE ZPAYAMT-PURCODE        TO WSAA-ITEM.                    <PHLRMS>
           PERFORM 800-READ-TABLE.                                      <PHLRMS>
           MOVE ITEM-GENAREA           TO TV041-TV041-REC.              <PHLRMS>
                                                                        <PHLRMS>
           MOVE ZERO                   TO WSAA-AMOUNT.                  <PHLRMS>
           MOVE 1                      TO IDX.                          <PHLRMS>
           PERFORM 700-GET-AMOUNT      UNTIL IDX > 18                   <PHLRMS>
                                       OR TV041-SACSCODE(IDX) = SPACES  <PHLRMS>
                                       OR TV041-SACSTYPE(IDX) = SPACES. <PHLRMS>
           MOVE WSAA-MAX-DATE          TO ZPAYAMT-DUEDTE.               <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Calculate pending interest relevant to Policy Loan or APL    <PHLRMS>
      *                                                                 <PHLRMS>
           IF  ZPAYAMT-PURCODE         = 'RL'                           <PHLRMS>
           OR                            'RA'                           <PHLRMS>
           OR                            'PF'                           <PHLRMS>
               PERFORM 100-CALC-PEND-INTEREST                           <PHLRMS>
               ADD WSAA-PND-TOTAL      TO WSAA-AMOUNT                   <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           MOVE WSAA-AMOUNT            TO ZPAYAMT-AMOUNT.               <PHLRMS>
      *                                                                 <PHLRMS>
       B90-EXIT.                                                        <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /                                                                 <PHLRMS>
       100-CALC-PEND-INTEREST SECTION.                                  <PHLRMS>
      ********************************                                  <PHLRMS>
      *                                                                 <PHLRMS>
       110-INIT.                                                        <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                     LOAN-PARAMS.                  <PHLRMS>
           MOVE WSAA-COMPANY           TO LOAN-CHDRCOY.                 <PHLRMS>
           MOVE ZPAYAMT-CHDRNUM        TO LOAN-CHDRNUM.                 <PHLRMS>
           MOVE 0                      TO LOAN-LOAN-NUMBER.             <PHLRMS>
                                                                        <PHLRMS>
           MOVE LOANREC                TO LOAN-FORMAT.                  <PHLRMS>
           MOVE BEGN                   TO LOAN-FUNCTION.                <PHLRMS>
      *                                                                 <PHLRMS>
       120-CALL.                                                        <PHLRMS>
      *                                                                 <PHLRMS>
           CALL 'LOANIO'            USING LOAN-PARAMS.                  <PHLRMS>
                                                                        <PHLRMS>
           IF  LOAN-STATUZ          NOT = O-K                           <PHLRMS>
           AND                      NOT = ENDP                          <PHLRMS>
               MOVE LOAN-PARAMS        TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  LOAN-CHDRCOY         NOT = WSAA-COMPANY                  <PHLRMS>
           OR  LOAN-CHDRNUM         NOT = ZPAYAMT-CHDRNUM               <PHLRMS>
           OR  LOAN-STATUZ              = ENDP                          <PHLRMS>
               GO TO 190-EXIT                                           <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  ZPAYAMT-PURCODE     = 'RL'                               <PHLRMS>
               IF  LOAN-LOAN-TYPE  NOT = 'P'                            <PHLRMS>
                   GO TO 180-NEXT                                       <PHLRMS>
               END-IF                                                   <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  ZPAYAMT-PURCODE     = 'RA'                               <PHLRMS>
               IF  LOAN-LOAN-TYPE  NOT = 'A'                            <PHLRMS>
                   GO TO 180-NEXT                                       <PHLRMS>
               END-IF                                                   <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHFX23>
           IF  ZPAYAMT-PURCODE     = 'PF'                               <PHFX23>
               IF  LOAN-LOAN-TYPE  = 'E' OR 'D'                         <PHFX23>
                   GO TO 180-NEXT                                       <PHFX23>
               END-IF                                                   <PHFX23>
           END-IF.                                                      <PHFX23>
                                                                        <PHLRMS>
      * Work out Pending Interest Since last Billing date.              <PHLRMS>
      *                                                                 <PHLRMS>
           IF LOAN-LAST-INT-BILL-DATE   < WSAA-TODAY                    <PHLRMS>
              PERFORM 900-READ-CHDRLNB                                  <PHLRMS>
              MOVE SPACES              TO INTC-INTCALC-REC              <PHLRMS>
              MOVE LOAN-LOAN-NUMBER    TO INTC-LOAN-NUMBER              <PHLRMS>
              MOVE LOAN-CHDRCOY        TO INTC-CHDRCOY                  <PHLRMS>
              MOVE LOAN-CHDRNUM        TO INTC-CHDRNUM                  <PHLRMS>
              MOVE CHDRLNB-CNTTYPE     TO INTC-CNTTYPE                  <PHLRMS>
              MOVE WSAA-TODAY          TO INTC-INTEREST-TO              <PHLRMS>
              MOVE LOAN-LAST-INT-BILL-DATE                              <PHLRMS>
                                       TO INTC-INTEREST-FROM            <PHLRMS>
              MOVE LOAN-LAST-CAPN-LOAN-AMT                              <PHLRMS>
                                       TO INTC-LOANORIGAM               <PHLRMS>
              MOVE LOAN-LAST-CAPN-DATE                                  <PHLRMS>
                                       TO INTC-LAST-CAPLSN-DATE         <PHLRMS>
              MOVE LOAN-LOAN-START-DATE                                 <PHLRMS>
                                       TO INTC-LOAN-START-DATE          <PHLRMS>
              MOVE ZEROES              TO INTC-INTEREST-AMOUNT          <PHLRMS>
              MOVE LOAN-LOAN-CURRENCY  TO INTC-LOAN-CURRENCY            <PHLRMS>
              MOVE LOAN-LOAN-TYPE      TO INTC-LOAN-TYPE                <PHLRMS>
                                                                        <PHLRMS>
              CALL 'INTCALC'        USING INTC-INTCALC-REC              <PHLRMS>
                                                                        <PHLRMS>
              IF INTC-STATUZ        NOT = O-K                           <PHLRMS>
                 MOVE INTC-INTCALC-REC TO SYSR-PARAMS                   <PHLRMS>
                 MOVE INTC-STATUZ      TO SYSR-STATUZ                   <PHLRMS>
                 PERFORM 600-FATAL-ERROR                                <PHLRMS>
              END-IF                                                    <PHLRMS>
           ELSE                                                         <PHLRMS>
              MOVE 0                   TO INTC-INTEREST-AMOUNT          <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           MOVE INTC-INTEREST-AMOUNT   TO ZRDP-AMOUNT-IN.               <PHLRMS>
           MOVE LOAN-LOAN-CURRENCY     TO ZRDP-CURRENCY.                <PHLRMS>
           PERFORM 1000-CALL-ROUNDING.                                  <PHLRMS>
           MOVE ZRDP-AMOUNT-OUT        TO INTC-INTEREST-AMOUNT.         <PHLRMS>
                                                                        <PHLRMS>
           ADD  INTC-INTEREST-AMOUNT   TO WSAA-PND-TOTAL.               <PHLRMS>
      *                                                                 <PHLRMS>
       180-NEXT.                                                        <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE NEXTR                  TO LOAN-FUNCTION.                <PHLRMS>
           GO TO 120-CALL.                                              <PHLRMS>
      *                                                                 <PHLRMS>
       190-EXIT.                                                        <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /
       200-LOAD-LINSDRY SECTION.
      ***************************
      *
       210-INIT.
      *
           INITIALIZE                     LINSDRY-PARAMS   .
           MOVE WSAA-COMPANY           TO LINSDRY-CHDRCOY  .
           MOVE ZPAYAMT-CHDRNUM        TO LINSDRY-CHDRNUM  .
           MOVE ZERO                   TO LINSDRY-INSTFROM .
           MOVE LINSDRYREC             TO LINSDRY-FORMAT   .
           MOVE BEGN                   TO LINSDRY-FUNCTION .
      *
       220-START.
      *
           CALL 'LINSDRYIO'            USING LINSDRY-PARAMS.

           IF  LINSDRY-STATUZ          NOT = O-K
           AND                         NOT = ENDP
               MOVE LINSDRY-STATUZ     TO SYSR-STATUZ
               MOVE LINSDRY-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF  LINSDRY-STATUZ          = ENDP
           OR  LINSDRY-CHDRCOY         NOT = WSAA-COMPANY
           OR  LINSDRY-CHDRNUM         NOT = ZPAYAMT-CHDRNUM
               GO TO 290-EXIT
           END-IF.
                                                                        <PHLRMS>
           MOVE LINSDRY-INSTFROM       TO WSAA-DUEDTE.                  <PHLRMS>
           PERFORM 400-CHECK-AVAIL-DUE.                                 <PHLRMS>
                                                                        <PHLRMS>
           IF  WSAA-SKIP-DUE           = 'Y'                            <PHLRMS>
               GO TO 280-NEXT                                           <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  WSAA-FIRST-TIME         = 'Y'                            <PHLRMS>
               MOVE 'N'                TO WSAA-FIRST-TIME               <PHLRMS>
               MOVE LINSDRY-INSTFROM   TO WSAA-FIRST-DUEDTE             <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           ADD 1                       TO WSAA-COUNT-DUE.               <PHLRMS>
           ADD 1                       TO IDY.                          <PHLRMS>
           MOVE LINSDRY-INSTFROM       TO WSAA-ITEM-DUE    (IDY).       <PHLRMS>
           MOVE LINSDRY-INSTAMT06      TO WSAA-ITEM-AMOUNT (IDY).       <PHLRMS>
      *
       280-NEXT.
      *
           MOVE NEXTR                  TO LINSDRY-FUNCTION .
           GO TO 220-START.
      *
       290-EXIT.
           EXIT.
      /
       300-LOAD-CHDRLNB SECTION.
      ***************************
      *
       310-START.
      *
           INITIALIZE                     CHDRLNB-PARAMS.
           MOVE WSAA-COMPANY           TO CHDRLNB-CHDRCOY.
           MOVE ZPAYAMT-CHDRNUM        TO CHDRLNB-CHDRNUM.
           MOVE CHDRLNBREC             TO CHDRLNB-FORMAT.
           MOVE READR                  TO CHDRLNB-FUNCTION.

           CALL 'CHDRLNBIO'            USING CHDRLNB-PARAMS.

           IF  CHDRLNB-STATUZ          NOT = O-K
               MOVE CHDRLNB-STATUZ     TO SYSR-STATUZ
               MOVE CHDRLNB-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE CHDRLNB-PTDATE         TO WSAA-DUEDTE.                  <PHLRMS>
           PERFORM 400-CHECK-AVAIL-DUE.                                 <PHLRMS>
                                                                        <PHLRMS>
           IF  WSAA-SKIP-DUE           = 'Y'                            <PHLRMS>
               MOVE WSAA-MAX-DATE      TO WSAA-DUEDTE                   <PHLRMS>
               MOVE ZEROES             TO WSAA-DUE-AMOUNT               <PHLRMS>
               GO TO 390-EXIT                                           <PHLRMS>
           END-IF.                                                      <PHLRMS>

           IF  WSAA-FIRST-TIME         = 'Y'                            <PHLRMS>
               MOVE 'N'                TO WSAA-FIRST-TIME               <PHLRMS>
               MOVE CHDRLNB-PTDATE     TO WSAA-FIRST-DUEDTE             <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           ADD 1                       TO WSAA-COUNT-DUE.               <PHLRMS>
           MOVE CHDRLNB-SINSTAMT06     TO WSAA-DUE-AMOUNT.
      *
       390-EXIT.
           EXIT.
      /                                                                 <PHLRMS>
       400-CHECK-AVAIL-DUE SECTION.                                     <PHLRMS>
      ******************************                                    <PHLRMS>
      *                                                                 <PHLRMS>
       410-INIT.                                                        <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE 'N'                    TO WSAA-SKIP-DUE.                <PHLRMS>
           INITIALIZE                     RCPYPOL-PARAMS.               <PHLRMS>
           MOVE ZPAYAMT-CHDRNUM        TO RCPYPOL-CHDRNUM.              <PHLRMS>
           MOVE WSAA-DUEDTE            TO RCPYPOL-DATEDUE.              <PHLRMS>
           MOVE RCPYPOLREC             TO RCPYPOL-FORMAT.               <PHLRMS>
           MOVE BEGN                   TO RCPYPOL-FUNCTION.             <PHLRMS>
      *                                                                 <PHLRMS>
       420-START.                                                       <PHLRMS>
      *                                                                 <PHLRMS>
           CALL 'RCPYPOLIO'            USING RCPYPOL-PARAMS.            <PHLRMS>
                                                                        <PHLRMS>
           IF  RCPYPOL-STATUZ          NOT = O-K                        <PHLRMS>
           AND                         NOT = ENDP                       <PHLRMS>
               MOVE RCPYPOL-STATUZ     TO SYSR-STATUZ                   <PHLRMS>
               MOVE RCPYPOL-PARAMS     TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  RCPYPOL-STATUZ          = ENDP                           <PHLRMS>
           OR  RCPYPOL-CHDRNUM         NOT = ZPAYAMT-CHDRNUM            <PHLRMS>
               GO TO 490-EXIT                                           <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  RCPYPOL-DATEDUE         NOT = WSAA-DUEDTE                <PHE003>
               GO TO 480-NEXT                                           <PHE003>
           END-IF.                                                      <PHE003>
                                                                        <PHE003>
           IF  RCPYPOL-VALIDFLAG       = '1'                            <PHE003>
               PERFORM 500-CHECK-USED-RCPT                              <PHE003>
               IF  WSAA-USED-RCPT      = 'Y'                            <PHE003>
                   MOVE 'Y'            TO WSAA-SKIP-DUE                 <PHE003>
                   GO TO 490-EXIT                                       <PHE003>
               END-IF                                                   <PHE003>
           END-IF.                                                      <PHE003>
      *                                                                 <PHLRMS>
       480-NEXT.                                                        <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE NEXTR                  TO RCPYPOL-FUNCTION.             <PHLRMS>
           GO TO 420-START.                                             <PHLRMS>
      *                                                                 <PHLRMS>
       490-EXIT.                                                        <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /                                                                 <PHLRMS>
       500-CHECK-USED-RCPT SECTION.                                     <PHLRMS>
      ******************************                                    <PHLRMS>
      *                                                                 <PHLRMS>
       510-START.                                                       <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE 'N'                    TO WSAA-USED-RCPT.               <PHLRMS>
                                                                        <PHLRMS>
           INITIALIZE                     TRRNINF-PARAMS.               <PHLRMS>
           MOVE RCPYPOL-RCTYPE         TO TRRNINF-RCTYPE.               <PHLRMS>
           MOVE RCPYPOL-RCSERNUM       TO TRRNINF-RCSERNUM.             <PHLRMS>
           MOVE TRRNINFREC             TO TRRNINF-FORMAT.               <PHLRMS>
           MOVE READR                  TO TRRNINF-FUNCTION.             <PHLRMS>
                                                                        <PHLRMS>
           CALL 'TRRNINFIO'            USING TRRNINF-PARAMS.            <PHLRMS>
                                                                        <PHLRMS>
           IF  TRRNINF-STATUZ          NOT = O-K                        <PHLRMS>
           AND                         NOT = MRNF                       <PHLRMS>
               MOVE TRRNINF-STATUZ     TO SYSR-STATUZ                   <PHLRMS>
               MOVE TRRNINF-PARAMS     TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  TRRNINF-STATUZ          = O-K                            <PHLRMS>
               IF  TRRNINF-RCSTAT      = 'US'                           <PHLRMS>
               OR  TRRNINF-RCSTAT      = 'UB'                           <PHE003>
                   MOVE 'Y'            TO WSAA-USED-RCPT                <PHLRMS>
               END-IF                                                   <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       590-EXIT.                                                        <PHLRMS>
           EXIT.                                                        <PHLRMS>
      *
       600-FATAL-ERROR SECTION.
      *************************
      *
       610-START.
      *
           IF SYSR-STATUZ              = BOMB
               GO TO 690-EXIT
           END-IF.
      *
           MOVE SYSR-STATUZ            TO SYSR-SYSERR-STATUZ.
           IF  SYSR-SYSERR-TYPE        NOT = '2'
               MOVE 1                  TO SYSR-SYSERR-TYPE
           END-IF.
      *
           CALL 'SYSERR'               USING SYSR-SYSERR-REC.
      *
       690-EXIT.
           MOVE BOMB                   TO ZPAYAMT-STATUZ.
           EXIT PROGRAM.
      /
       700-GET-AMOUNT SECTION.
      *************************
      *
       710-INIT.
      *                                                                 <PHLRMS>
           INITIALIZE                    ACBLENQ-PARAMS  .
           MOVE WSAA-COMPANY          TO ACBLENQ-RLDGCOY .
           MOVE TV041-SACSCODE(IDX)   TO ACBLENQ-SACSCODE.
           MOVE TV041-SACSTYPE(IDX)   TO ACBLENQ-SACSTYP .
           MOVE ZPAYAMT-CHDRNUM       TO ACBLENQ-RLDGACCT(1:8).
           MOVE WSAA-CURRENCY         TO ACBLENQ-ORIGCURR.
           MOVE ACBLREC               TO ACBLENQ-FORMAT  .
           MOVE BEGN                  TO ACBLENQ-FUNCTION.
      *
       720-START.
      *
           CALL  'ACBLENQIO'          USING ACBLENQ-PARAMS.

           IF  ACBLENQ-STATUZ         NOT = O-K
           AND ACBLENQ-STATUZ         NOT = ENDP
               MOVE ACBLENQ-PARAMS    TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF  ACBLENQ-STATUZ         NOT = O-K
           OR  ACBLENQ-RLDGCOY        NOT = WSAA-COMPANY
           OR  ACBLENQ-SACSCODE       NOT = TV041-SACSCODE(IDX)
           OR  ACBLENQ-SACSTYP        NOT = TV041-SACSTYPE(IDX)
           OR  ACBLENQ-RLDGACCT(1:8)  NOT = ZPAYAMT-CHDRNUM
           OR  ACBLENQ-ORIGCURR       NOT = WSAA-CURRENCY
               ADD 1                  TO IDX
               GO TO 790-EXIT
           END-IF.

           ADD  ACBLENQ-SACSCURBAL    TO WSAA-AMOUNT.
      *
       780-NEXT.
      *
           MOVE NEXTR                 TO ACBLENQ-FUNCTION.
           GO TO 720-START.
      *
       790-EXIT.
           EXIT.
      /                                                                 <PHLRMS>
       800-READ-TABLE SECTION.                                          <PHLRMS>
      *************************                                         <PHLRMS>
      *                                                                 <PHLRMS>
       810-START.                                                       <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                    ITEM-PARAMS.                   <PHLRMS>
           MOVE SMTP-ITEM             TO ITEM-ITEMPFX.                  <PHLRMS>
           MOVE WSAA-COMPANY          TO ITEM-ITEMCOY.                  <PHLRMS>
           MOVE WSAA-TABLE            TO ITEM-ITEMTABL.                 <PHLRMS>
           MOVE WSAA-ITEM             TO ITEM-ITEMITEM.                 <PHLRMS>
           MOVE ITEMREC               TO ITEM-FORMAT.                   <PHLRMS>
           MOVE READR                 TO ITEM-FUNCTION.                 <PHLRMS>
                                                                        <PHLRMS>
           CALL  'ITEMIO'             USING ITEM-PARAMS.                <PHLRMS>
                                                                        <PHLRMS>
           IF  ITEM-STATUZ            NOT = O-K                         <PHLRMS>
           AND ITEM-STATUZ            NOT = MRNF                        <PHLRMS>
               MOVE ITEM-PARAMS       TO SYSR-PARAMS                    <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       890-EXIT.                                                        <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /                                                                 <PHLRMS>
       900-READ-CHDRLNB SECTION.                                        <PHLRMS>
      ***************************                                       <PHLRMS>
      *                                                                 <PHLRMS>
       910-START.                                                       <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                     CHDRLNB-PARAMS.               <PHLRMS>
           MOVE WSAA-COMPANY           TO CHDRLNB-CHDRCOY.              <PHLRMS>
           MOVE ZPAYAMT-CHDRNUM        TO CHDRLNB-CHDRNUM.              <PHLRMS>
           MOVE CHDRLNBREC             TO CHDRLNB-FORMAT.               <PHLRMS>
           MOVE READR                  TO CHDRLNB-FUNCTION.             <PHLRMS>
                                                                        <PHLRMS>
           CALL 'CHDRLNBIO'            USING CHDRLNB-PARAMS.            <PHLRMS>
                                                                        <PHLRMS>
           IF  CHDRLNB-STATUZ          NOT = O-K                        <PHLRMS>
               MOVE CHDRLNB-STATUZ     TO SYSR-STATUZ                   <PHLRMS>
               MOVE CHDRLNB-PARAMS     TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       990-EXIT.                                                        <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /                                                                 <PHLRMS>
       1000-CALL-ROUNDING SECTION.                                      <PHLRMS>
      ****************************                                      <PHLRMS>
      *                                                                 <PHLRMS>
       1010-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE SPACES                 TO ZRDP-FUNCTION                 <PHLRMS>
           MOVE WSAA-COMPANY           TO ZRDP-COMPANY.                 <PHLRMS>
           MOVE O-K                    TO ZRDP-STATUZ.                  <PHLRMS>
           MOVE SPACES                 TO ZRDP-BATCTRCDE.               <PHLRMS>
                                                                        <PHLRMS>
           CALL 'ZRDECPLC'             USING ZRDP-ZRDECPL-REC.          <PHLRMS>
                                                                        <PHLRMS>
           IF  ZRDP-STATUZ             NOT = O-K                        <PHLRMS>
               MOVE ZRDP-STATUZ        TO SYSR-STATUZ                   <PHLRMS>
               MOVE ZRDP-ZRDECPL-REC   TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       1090-EXIT.                                                       <PHLRMS>
           EXIT.                                                        <PHLRMS>
