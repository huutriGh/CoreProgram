      * Generation Parameters SCRVER(02)               Do Not Delete!   <S9503>
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P5073.
      *REMARKS.
      *
      *                  New Contract Issue Submenu
      *                  ==========================
      *
      *
      * Initialisation
      * --------------
      *
      *  Retrieve todays date (DATCON1) and store for later use.
      *
      *
      * Validation
      * ----------
      *
      *  Key 1 - Contract proposal number (CHDRLNB)
      *
      *       Y = mandatory, must exist on file.
      *            - CHDRLNB  -  Life  new  business  contract  header
      *                 logical view  (LIFO,  keyed  on  company   and
      *                 contract number, select service unit = LP).
      *            - must be correct status for transaction (T5679).
      *            - for maintenece  transactions,   servicing  branch
      *              must be sign-on branch
      *
      *       N = optional, if entered must not exist on file, must be
      *            within correct number range (call ALOCNO to check).
      *
      *       Blank = irrelevant.
      *
      *  Key 3 - Contract number.
      *
      *       Y = mandatory, must exist on file (CHDRLNB).
      *
      *       N = irrelevant.
      *
      *       Blank = irrelevant.
      *
      *
      * Updating
      * --------
      *
      *  Set up WSSP-FLAG:
      *       - "I" for actions E and F, otherwise "M".
      *  For enqury transactions ("I"), nothing else is required.
      *
      *  For maintenance transactions  (WSSP-FLAG  set to  'M' above),
      *  soft lock the contract header (call SFTLOCK). If the contract
      *  is already locked, display an error message.
      *
      ******************Enhancements for Life Asia 1.0****************
      *
      * A new option is now available to postpone a proposal. This is
      * in addition to the options of decline and withdrawing a
      * proposal. The processing of this option is all done via tables.
      *
      *****************************************************************
      *              AMENDMENT  HISTORY                               *
      *****************************************************************
      * DATE.....   BY..   AMENDMENT...............  NUMBER
      * DD/MM/YY    X.X.   xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  NNN
      *
      * 15/02/90    P.F    Removed code handling action F which is  001
      *                    no longer on screen.
      * 19/03/90    T.S    Command F8 (Batch control) to work.      002
      * 08/05/90    B.C.   Allow Contract Inquiry to work. P6353    003
      *                    uses CHDRENQ not CHDRSUR.(IOM FIX)
      * 22/06/90    BPM    Change to wording on options B and C on
      *                    screen - No change to program.
      * 04/09/92    M.P.   AQR 3364.                                    004
      *                    Commented out unused T5677 and
      *                    associated objects.  Amended source and
      *                    object computer to AS400.
      * 31/09/93    M.S    AQR 4783.                                005.
      *                    Prevent missing table item forcing fatal
      *                    error.
      *
      * 04/05/94    TSG.   MAY9405. SANCTN conversion.              SE1
      *
      *****************************************************************
      /
      ***********************************************************************
      *                                                                     *
      * ......... New Version of the Amendment History.                     *
      *                                                                     *
      ***********************************************************************
      *           AMENDMENT  HISTORY                                        *
      ***********************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....                               *
      *                                                                     *
      * 05/02/96  01/01   D96NUM       Rob Yates                            *
      *                   SINSTAMT                                          *
      *                   ISAM                                              *
      *                   INSTTOT                                           *
      *                                                                     *
      *                   The above field(s) have been increased in         *
      *                   size as part of the 1996 Numeric Field            *
      *                   Increase project. This is to enable users         *
      *                   to then convert (if they should wish to do        *
      *                   so) the fields to 'Monetary' and use the          *
      *                   SMART Variable Decimal Places technology          *
      *                   accordingly.                                      *
      *                                                                     *
      ***********************************************************************
      *                                                                     *
      * ......... New Version of the Amendment History.                     *
      *                                                                     *
      ***********************************************************************
      *           AMENDMENT  HISTORY                                        *
      ***********************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....                               *
      *                                                                     *
      * 08/07/96  01/01   CAS1.0       Sunil Patel                          *
      *                                                                     *
      *                   A new option is added to postpone proposals.      *
      *                   This is now option D.Therefore only F is now      *
      *                   allowed for enquiry.                              *
      * 29/11/97    DUNC  SMART 9503 Conv for Client/Server.        <S9503> *
      *                                                                     *
      * 17/12/98  01/01   V4L011       Daniel Wong                          *
      *                   If action = 'E' assign 'E' to WSSP-FLAG           *
      *                                                                     *
      * 18/05/98  01/01   A06633       Kevin Duffy                          *
      *           It is possible to enter an entity with a spurious char    *
      *           at the end. This is truncated when accessing entity but   *
      *           SFTLOCK has entity moved from screen field. Therefore     *
      *           it locks a 'different' entity and its possible for two    *
      *           people to work on same entity. Set up SFTLOCK entity      *
      *           from field/key used on existence check.                   *
      *                                                                     *
      * 08/03/07  01/01   V71L11       Lewis Liu Gang  - CSC Beijing        *
      *           - Change option  'E' to NTU proposal                      *
      *           - Create option  'G' for                                  *
      *             Reverse Decline/Withdraw/Postpone/NTU                   *
      *                                                                     *
      * 27/02/08  01/01   V72L11       Xu Chen/ASIA/CSC (Beijing)           *
      *           Since T5661 is not be used, comment it out.               *
      *                                                                     *
      * 17/11/09  01/01   V75F01       Saw Hoong Ong/FSG/CSC (Malaysia)     *
      *           Secured Data Access                                       *
      *           If the sign-on user is a restricted user, only allow      *
      *           access to contract numbers that are sanctioned.           *
      *                                                                     *
      **DD/MM/YY*************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER. IBM-S38.                                        <004>
       SOURCE-COMPUTER. IBM-AS400.                                      <004>
      *OBJECT-COMPUTER. IBM-S38.                                        <004>
       OBJECT-COMPUTER. IBM-AS400.                                      <004>
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       01  WSAA-PROG                   PIC X(05) VALUE 'P5073'.
       01  WSAA-VERSION                PIC X(02) VALUE '02'.
      *
       01  ERRORS.
           03  E070                    PIC X(04) VALUE 'E070'.
           03  E073                    PIC X(04) VALUE 'E073'.
           03  E132                    PIC X(04) VALUE 'E132'.
           03  E186                    PIC X(04) VALUE 'E186'.
           03  E455                    PIC X(04) VALUE 'E455'.
           03  E544                    PIC X(04) VALUE 'E544'.
           03  E558                    PIC X(04) VALUE 'E558'.
           03  E767                    PIC X(04) VALUE 'E767'.
           03  F235                    PIC X(04) VALUE 'F235'.
           03  F321                    PIC X(04) VALUE 'F321'.
           03  F910                    PIC X(04) VALUE 'F910'.
           03  F918                    PIC X(04) VALUE 'F918'.
           03  G619                    PIC X(04) VALUE 'G619'.
      *
       01  TABLES.
           03  T5688                   PIC X(05) VALUE 'T5688'.
           03  T5679                   PIC X(05) VALUE 'T5679'.
      **** 03  T5677                   PIC X(05) VALUE 'T5677'.         <004>
      *    03  T5661                   PIC X(05) VALUE 'T5661'.         <V72L11>
      *
       01  FORMATS.
           03  CHDRLNBREC              PIC X(10) VALUE 'CHDRLNBREC'.
           03  LIFELNBREC              PIC X(10) VALUE 'LIFELNBREC'.
           03  CHDRENQREC              PIC X(10) VALUE 'CHDRENQREC'.    <003>
           03  FLUPLNBREC              PIC X(10) VALUE 'FLUPLNBREC'.
           03  ITEMREC                 PIC X(10) VALUE 'ITEMREC'.
           03  DESCREC                 PIC X(10) VALUE 'DESCREC'.
      *
       01  WSAA-GENKEY.
           03  WSAA-SERVUNIT           PIC X(02) VALUE 'LP'.
           03  WSAA-BRANCH             PIC X(02).
           03  FILLER                  PIC X(02).
      *
      *01  WSAA-T5677-KEY.                                              <004>
      ***  03  WSAA-T5677-TRANNO       PIC X(4).                        <004>
      ***  03  WSAA-T5677-FOLLOWUP     PIC X(4).                        <004>
      *
       01  WSAA-TODAY                  PIC 9(08) COMP-3 VALUE 0.
       01  WSAA-X                      PIC 9(02) COMP-3 VALUE 0.
       01  WSAA-SUB                    PIC 9(02) COMP-3 VALUE 0.
      *
           COPY ALOCNOREC.
      *
           COPY DATCON1REC.
      *
           COPY SFTLOCKREC.
      *
           COPY T5688REC.
      *
           COPY T5679REC.
      *
      **** COPY T5677REC.                                               <004>
      *
      *    COPY T5661REC.                                               <V72L11>
      *
       01  WSAA-BATCHKEY.
           COPY BATCKEY.
      *
           COPY CHDRLNBSKM.
      *
           COPY CHDRENQSKM.                                             <003>
      *
           COPY LIFELNBSKM.
      *
           COPY FLUPLNBSKM.
      *
           COPY ITEMSKM.
      *
           COPY DESCSKM.
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
      *                                                                 <V75F01>
           COPY FSUPFXCPY.                                              <V75F01>
      *                                                                 <V75F01>
           COPY SDASANCREC.                                             <V75F01>
      *
      ***  COPY SCRNPARAMS.                                             <S9503>
      /
      ***  COPY S5073SCR.                                               <S9503>
      /
       LINKAGE SECTION.
      * Screen copybooks are now part of the linkage.                   <S9503>
      /                                                                 <S9503>
           COPY SCRNPARAMS.                                             <S9503>
      /                                                                 <S9503>
           COPY S5073SCR.                                               <S9503>

           COPY WSSPCOMN.

           COPY WSSPLIFE.
      /
      * Statement now includes screen copybooks.                        <S9503>
       PROCEDURE DIVISION USING WSSP-COMMON-AREA WSSP-USER-AREA         <S9503>
                                               SCRN-SCREEN-PARAMS       <S9503>
                                               S5073-DATA-AREA      .   <S9503>

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

           MOVE SPACES                 TO S5073-DATA-AREA.
           MOVE WSSP-SBMACTION         TO S5073-ACTION.
           MOVE WSSP-BATCHKEY          TO WSKY-BATC-KEY.
           IF WSKY-BATC-BATCACTMN      NOT = WSSP-ACCTMONTH
           OR WSKY-BATC-BATCACTYR      NOT = WSSP-ACCTYEAR
               MOVE E070               TO SCRN-ERROR-CODE.

           IF WSAA-TODAY = 0
              MOVE TDAY                   TO DTC1-FUNCTION
              CALL 'DATCON1' USING DTC1-DATCON1-REC
              MOVE DTC1-INT-DATE          TO WSAA-TODAY.


       1090-EXIT.
            EXIT.
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
      *    CALL 'S5073IO' USING SCRN-SCREEN-PARAMS                      <S9503>
      *                          S5073-DATA-AREA.                       <S9503>
      * Screen errors are now handled in the calling program.           <S9503>
      *    PERFORM 200-SCREEN-ERRORS.                                   <S9503>
           MOVE O-K                    TO WSSP-EDTERROR.

       2020-VALIDATE.
           PERFORM 2100-VALIDATE-ACTION.

           IF S5073-ACTION-ERR         = SPACES
              IF SCRN-STATUZ           NOT = 'BACH'
                  PERFORM 2200-VALIDATE-KEYS
              ELSE
                  PERFORM 2900-VERIFY-BATCH-CONTROL.

       2080-CHECK-FOR-ERRORS.
           IF S5073-ERROR-INDICATORS NOT = SPACES
              MOVE 'Y'                 TO WSSP-EDTERROR.
      *
       2090-EXIT.
            EXIT.
      /
       2100-VALIDATE-ACTION SECTION.
      ******************************
      *
       2110-CHECK-AGAINST-TABLE.
           MOVE SCRN-ACTION            TO SUBP-ACTION.
           MOVE WSSP-COMPANY           TO SUBP-COMPANY.
           MOVE WSAA-PROG              TO SUBP-PROG-CODE.
           CALL 'SUBPROG' USING SUBP-SUBPROG-REC.
           IF SUBP-STATUZ NOT = O-K
              MOVE SUBP-STATUZ         TO S5073-ACTION-ERR
              GO TO 2190-EXIT.
      *
       2120-CHECK-SANCTIONS.
           MOVE 'SUBM'                 TO SNCT-FUNCTION.
      **** MOVE WSSP-PASSWORD          TO SNCT-PASSWORD.                <SE1>
      **** MOVE WSSP-USERID            TO SNCT-PASSWORD.                <SE1>
           MOVE WSSP-USERID            TO SNCT-USERID  .                <SE1>
           MOVE WSSP-COMPANY           TO SNCT-COMPANY.
           MOVE WSSP-BRANCH            TO SNCT-BRANCH.
           MOVE SUBP-TRANSCD           TO SNCT-TRANSCD.
           CALL 'SANCTN' USING WSSP-COMMON-AREA SNCT-SANCTN-REC.
           IF SNCT-STATUZ              = BOMB
               MOVE SNCT-STATUZ        TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR.
           IF SNCT-STATUZ              NOT = O-K
               MOVE SNCT-STATUZ        TO S5073-ACTION-ERR.

       2190-EXIT.
            EXIT.
      /
       2200-VALIDATE-KEYS SECTION.
      ****************************
      *
       2210-VALIDATE-KEY1.
      *
           IF SUBP-KEY1                  = SPACES
              GO TO 2220-VALIDATE-KEY2.
      *
           MOVE WSSP-COMPANY           TO CHDRLNB-CHDRCOY.
           MOVE S5073-CHDRSEL          TO CHDRLNB-CHDRNUM.
           MOVE READR                  TO CHDRLNB-FUNCTION.
      *
           IF S5073-CHDRSEL            NOT = SPACES
              CALL 'CHDRLNBIO' USING CHDRLNB-PARAMS
           ELSE
              MOVE MRNF                TO CHDRLNB-STATUZ.
      *
           IF CHDRLNB-STATUZ           NOT = O-K AND
                                       NOT = MRNF
              MOVE CHDRLNB-PARAMS      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.
      *
           IF CHDRLNB-STATUZ           = MRNF
            AND SUBP-KEY1 = 'Y'
              MOVE E544                TO S5073-CHDRSEL-ERR.
      *
           IF CHDRLNB-STATUZ           = O-K
            AND SUBP-KEY1 = 'N'
              MOVE F918                TO S5073-CHDRSEL-ERR.
      *
           IF  S5073-CHDRSEL-ERR       = SPACES                         <V75F01>
               INITIALIZE                 SDAS-SANC-REC                 <V75F01>
               MOVE 'VENTY'            TO SDAS-FUNCTION                 <V75F01>
               MOVE WSSP-USERID        TO SDAS-USERID                   <V75F01>
               MOVE PRFX-CHDR          TO SDAS-ENTYPFX                  <V75F01>
               MOVE WSSP-COMPANY       TO SDAS-ENTYCOY                  <V75F01>
               MOVE S5073-CHDRSEL      TO SDAS-ENTYNUM                  <V75F01>
      *                                                                 <V75F01>
               CALL 'SDASANC'          USING SDAS-SANC-REC              <V75F01>
      *                                                                 <V75F01>
               IF  SDAS-STATUZ         NOT = O-K                        <V75F01>
                   MOVE SDAS-STATUZ    TO S5073-CHDRSEL-ERR             <V75F01>
                   GO TO 2290-EXIT                                      <V75F01>
               END-IF                                                   <V75F01>
           END-IF.                                                      <V75F01>
      *
      *    For transactions on existing proposals,
      *       check the contract selected is of the correct status
      *       and from correct branck.
      *
           IF SUBP-KEY1                = 'Y'
            AND S5073-CHDRSEL-ERR      = SPACES
              PERFORM 2400-CHECK-STATUS.
           IF SUBP-KEY1                = 'Y'
            AND S5073-CHDRSEL-ERR      = SPACES
            AND WSSP-BRANCH NOT = CHDRLNB-CNTBRANCH
              MOVE E455                TO S5073-CHDRSEL-ERR.

      *
       2220-VALIDATE-KEY2.
      *
           IF SUBP-KEY2                  = SPACES OR 'N'
              GO TO 2290-EXIT.
      *
           MOVE WSSP-COMPANY           TO CHDRLNB-CHDRCOY.
           MOVE S5073-CHDRSEL          TO CHDRLNB-CHDRNUM.
           MOVE READR                  TO CHDRLNB-FUNCTION.
      *
           IF S5073-CHDRSEL            NOT = SPACES
              CALL 'CHDRLNBIO' USING CHDRLNB-PARAMS
           ELSE
              MOVE MRNF                TO CHDRLNB-STATUZ.
      *
           IF CHDRLNB-STATUZ           NOT = O-K AND
                                       NOT = MRNF
              MOVE CHDRLNB-PARAMS      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.
      *
           IF CHDRLNB-STATUZ           = MRNF
              MOVE E544                TO S5073-CHDRSEL-ERR.

       2290-EXIT.
            EXIT.
      *
      *
       2400-CHECK-STATUS SECTION.
      ***************************
      *
       2410-READ-STATUS-TABLE.
           MOVE SPACES                 TO ITEM-DATA-KEY.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.
           MOVE T5679                  TO ITEM-ITEMTABL.
           MOVE SUBP-TRANSCD           TO ITEM-ITEMITEM.
           MOVE READR                  TO ITEM-FUNCTION.
      *
           CALL 'ITEMIO' USING ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = O-K
                                       AND   MRNF
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.
      *
           IF ITEM-STATUZ              NOT = O-K                        <005>
              MOVE F321                TO S5073-CHDRSEL-ERR             <005>
              GO                       TO 2490-EXIT.                    <005>
      *
           MOVE ITEM-GENAREA           TO T5679-T5679-REC.
           MOVE ZERO TO WSAA-SUB.
           PERFORM 2500-LOOK-FOR-STAT.
      *
       2490-EXIT.
            EXIT.
      /
       2500-LOOK-FOR-STAT        SECTION.
      ***********************************
       2510-SEARCH.
              ADD 1 TO WSAA-SUB.
              IF WSAA-SUB > 12
                 MOVE E767                TO S5073-CHDRSEL-ERR
                 MOVE 'Y'                 TO WSSP-EDTERROR
              ELSE
              IF CHDRLNB-STATCODE NOT = T5679-CN-RISK-STAT(WSAA-SUB)
                 GO TO 2510-SEARCH.
      *
      *
       2590-EXIT.
            EXIT.
      /
       2900-VERIFY-BATCH-CONTROL SECTION.
      ***********************************
      *
       2910-VALIDATE-REQUEST.
           IF SUBP-BCHRQD              NOT = 'Y'
<002>**        MOVE E073               TO S5073-ACTION-ERR.             <002>
<002>          MOVE E073               TO S5073-ACTION-ERR              <002>
      *  The exit paragraph name will be standardised to 2090 -EXIT.    <S9503>
      *        GO TO 2990-EXIT.                                         <S9503>
               GO TO 2090-EXIT.                                         <S9503>
      *
       2920-RETRIEVE-BATCH-PROGS.
           MOVE SUBP-TRANSCD           TO BCBP-TRANSCD.
           MOVE WSSP-COMPANY           TO BCBP-COMPANY.
           CALL 'BCBPROG' USING BCBP-BCBPROG-REC.
           IF BCBP-STATUZ              NOT = O-K
               MOVE BCBP-STATUZ        TO S5073-ACTION-ERR
      *  The exit paragraph name will be standardised to 2090 -EXIT.    <S9503>
      *        GO TO 2990-EXIT.                                         <S9503>
               GO TO 2090-EXIT.                                         <S9503>

           MOVE BCBP-NXTPROG1          TO WSSP-NEXT1PROG.
           MOVE BCBP-NXTPROG2          TO WSSP-NEXT2PROG.
           MOVE BCBP-NXTPROG3          TO WSSP-NEXT3PROG.
           MOVE BCBP-NXTPROG4          TO WSSP-NEXT4PROG.

      *  The exit paragraph name will be standardised to 2090 -EXIT.    <S9503>
      *2990-EXIT.                                                       <S9503>
       2090-EXIT.                                                       <S9503>
            EXIT.
      /
      *****************************************************************
      *     UPDATE DATABASE IF REQUIRED AND LOG TRANSACTION
      *****************************************************************
      *
       3000-UPDATE SECTION.
      **********************
      *
       3010-UPDATE-WSSP.
           MOVE SCRN-ACTION            TO WSSP-SBMACTION.
           MOVE WSSP-BATCHKEY          TO WSAA-BATCHKEY.
           MOVE SUBP-TRANSCD           TO WSKY-BATC-BATCTRCDE.
           MOVE WSAA-BATCHKEY          TO WSSP-BATCHKEY.
           MOVE WSAA-PROG              TO WSSP-SUBMENU.
           IF SCRN-STATUZ              = 'BACH'
              GO TO 3090-EXIT.
      *
           MOVE SUBP-NXT1PROG          TO WSSP-SEC-PROG (1).
           MOVE SUBP-NXT2PROG          TO WSSP-SEC-PROG (2).
           MOVE SUBP-NXT3PROG          TO WSSP-SEC-PROG (3).
           MOVE SUBP-NXT4PROG          TO WSSP-SEC-PROG (4).
      *
      *  Update WSSP Key details
      *
      *****IF SCRN-ACTION = 'E'                                         <001>
  *****    IF SCRN-ACTION = 'E' OR 'F'                          <CAS1.0><003>
           IF SCRN-ACTION = 'F'                                         <CAS1.0>
              MOVE 'I'                 TO WSSP-FLAG
               MOVE WSSP-COMPANY       TO CHDRENQ-CHDRCOY               <003>
               MOVE S5073-CHDRSEL      TO CHDRENQ-CHDRNUM               <003>
               MOVE READS              TO CHDRENQ-FUNCTION              <003>
               MOVE CHDRENQREC         TO CHDRENQ-FORMAT                <003>
               CALL 'CHDRENQIO' USING CHDRENQ-PARAMS                    <003>
              GO TO 3070-KEEPS
           ELSE
              MOVE 'M'                 TO WSSP-FLAG.
      *
      *    IF S5073-ACTION              = 'E'                   <V71L11><V4L011>
      *       MOVE 'E'                 TO WSSP-FLAG             <V71L11><V4L011>
      *    END-IF.                                              <V71L11><V4L011>
      *                                                         <V71L11><PCA122>
      * The following 'E' is new for NTU                        <V71L11><PCA122>
      *                                                                 <V71L11>
           IF S5073-ACTION              = 'E'                           <V71L11>
              MOVE 'E'                 TO WSSP-FLAG                     <V71L11>
              MOVE WSSP-COMPANY        TO CHDRENQ-CHDRCOY               <V71L11>
              MOVE S5073-CHDRSEL       TO CHDRENQ-CHDRNUM               <V71L11>
              MOVE READS               TO CHDRENQ-FUNCTION              <V71L11>
              MOVE CHDRENQREC          TO CHDRENQ-FORMAT                <V71L11>
              CALL 'CHDRENQIO' USING CHDRENQ-PARAMS                     <V71L11>
           END-IF.                                                      <V71L11>
           IF S5073-ACTION              = 'G'                           <V71L11>
              MOVE 'G'                 TO WSSP-FLAG                     <V71L11>
           END-IF.                                                      <V71L11>
                                                                        <V71L11>

           IF S5073-ERROR-INDICATORS   NOT = SPACES
              GO TO 3080-BATCHING.
      *
      *    Soft lock contract.
      *
           MOVE 'LOCK'                 TO SFTL-FUNCTION.
           MOVE WSSP-COMPANY           TO SFTL-COMPANY.
           MOVE 'CH'                   TO SFTL-ENTTYP.
           IF S5073-CHDRSEL            NOT = SPACES
      ****    MOVE S5073-CHDRSEL       TO SFTL-ENTITY                   <A06633>
              MOVE CHDRLNB-CHDRNUM     TO SFTL-ENTITY                   <A06633>
           ELSE
              MOVE ALNO-ALOC-NO        TO SFTL-ENTITY.
           MOVE SUBP-TRANSCD           TO SFTL-TRANSACTION.
           MOVE VRCM-USER              TO SFTL-USER.

           CALL 'SFTLOCK' USING SFTL-SFTLOCK-REC.
           IF SFTL-STATUZ              NOT = O-K
                                   AND NOT = 'LOCK'
              MOVE SFTL-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.

           IF SFTL-STATUZ              = 'LOCK'
              MOVE F910                TO S5073-CHDRSEL-ERR.
      *
      *    For existing proposals, add one to the transaction number.
      *
           IF SUBP-KEY1                = 'Y'
            AND S5073-ERROR-INDICATORS = SPACES
              ADD 1                    TO CHDRLNB-TRANNO.
      *
      *
       3070-KEEPS.
      *   Store the contract header for use by the transaction programs
      *
           MOVE 'KEEPS'                TO CHDRLNB-FUNCTION.
           MOVE CHDRLNBREC             TO CHDRLNB-FORMAT.
      *
           CALL 'CHDRLNBIO' USING CHDRLNB-PARAMS.
      *
           IF CHDRLNB-STATUZ           NOT = O-K
              MOVE CHDRLNB-PARAMS      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.
      *
       3080-BATCHING.
           IF   SUBP-BCHRQD            = 'Y'
            AND S5073-ERROR-INDICATORS = SPACES
              PERFORM 3100-UPDATE-BATCH-CONTROL.
      *
           IF S5073-ERROR-INDICATORS NOT = SPACES
              MOVE 'Y'                 TO WSSP-EDTERROR
              ROLLBACK.
      *
       3090-EXIT.
            EXIT.

      /
       3100-UPDATE-BATCH-CONTROL SECTION.
      ***********************************
      *
       3110-AUTOMATIC-BATCHING.
           MOVE 'AUTO'                 TO BATD-FUNCTION.
           MOVE WSSP-TRANID            TO BATD-TRANID.
           MOVE WSSP-BATCHKEY          TO BATD-BATCHKEY.
           CALL 'BATCDOR' USING BATD-BATCDOR-REC.
           IF BATD-STATUZ              NOT = O-K
               MOVE BATD-STATUZ        TO S5073-ACTION-ERR.

           MOVE BATD-BATCHKEY          TO WSSP-BATCHKEY.
      *
       3190-EXIT.
            EXIT.
      *
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
           IF SCRN-STATUZ              NOT = 'BACH'
              MOVE 1                   TO WSSP-PROGRAM-PTR
           ELSE
              MOVE 0                   TO WSSP-PROGRAM-PTR
              MOVE WSSP-NEXT1PROG      TO WSSP-NEXTPROG.
      *
       4090-EXIT.
            EXIT.
