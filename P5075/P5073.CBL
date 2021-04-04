      * Generation Parameters SCRVER(02)               Do Not Delete!   <S9503>
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P5075.
      *REMARKS.
      *
      *        Withdraw/Decline/Reinstate Contract
      *
      * Initialise
      * ----------
      *
      *
      * The details of the contract being worked on will be stored in
      * the CHDRLNB I/O module.  Retrieve the details.
      *
      * Look up the following always:
      *       Contract type from T5688,
      *       Contract status from T3623.
      *       Servicing branch from T1692 (default from sign-on branch)
      *
      * Look up the  contract  definition  details from T5688. If the
      * minimum  number of  policies  in  the  plan  is  blank,  plan
      * processing is not  applicable  to the contract. In this case,
      * non-display and protect the prompt for the number of policies
      * in the plan.  Otherwise,  if the number of policies is blank,
      * move in the default value from the table.
      *
      *
      *  Look up the following:
      *       Premium status from T5075, (***NO, FROM T3588!!!)         <003>
      *       The owner's client (CLTS) details.  Call FMTCLTN to
      *            format the client name for confirmation.
      *       Count the number of life (LIFELNB) records attached
      *            to the proposal.  For each life and joint life
      *            record read, check to see if any are a life on
      *            another proposal (use LFCLLNB),
      *       The agent (AGNT)  to  get  the  client  number, and
      *            hence the  client (agent's) name. Call FMTCLTN
      *            to format the client name for confirmation.
      *
      *
      * Validation
      * ----------
      *
      * All information entered is optional, so no validation is
      * required.
      *
      * Updating
      * --------
      *
      *
      * Update the Contract header details as follows:
      *
      *   Add one to the contract transaction number
      *   Contract status to status on T5679
      *   Contract status to today
      *   Contract status transaction no to contract transaction no.
      *   Transaction Number last used to contract transaction no.
      *   Use contract number to read MLIACHD, if the indicator is
      *   'Y' then update the record in MLIACHD else delete the
      *   contract record in MLIACHD (for reversal transacton).
      *   If the reason code or narrative was entered, write a resn
      *   record with the information entered .
      *
      * Next Program
      * ------------
      *
      * Add 1 to the program pointer and exit.
      *
      *
      *****************************************************************
      *              AMENDMENT  HISTORY                               *
      *****************************************************************
      * DATE.....   BY..   AMENDMENT...............  NUMBER
      *
      * DD/MM/YY    X.X.   xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  NNN
      * 16/03/90    T.S    Set up the correct status in CHDR record 001
      *                    (SDF 138).
      *                    Commented out the unused coding.
      *                    Set up the transaction code in RESN record.
      * 23/05/91    N.F.   (SDF 1654)                               002
      *                    Set PTRN-BATCPFX to 'BA'
      * 30/05/91    B.C.   (SDF 1906)                               003
      *                    Premium Status Code comes from T3588
      *                    NOT T5075 (which does not exist). Change
      *                    to use correct table.
      * 23/07/91    I.W.   No longer use the SACS........
      *
      * 30/12/92    J.B.   AQR 4344                                 004
      *                    Change source computer to AS400.
      *
      * 19/08/93    H.P.   AQR 4640.                                005
      *                    As VRCM-TERMID contains the User Terminal ID
      *                    and not VRCM-TERM, all instances of
      *                    VRCM-TERM replaced by VRCM-TERMID.
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
      * 06/02/96  01/01   D96NUM       Rob Yates                            *
      *                   SINSTAMT                                          *
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
      * 04/09/96   01/01  CAS1.0       Kristin McLeish                      *
      *                   Check for concurrent proposals is not             *
      *                   excluding the proposal actually being             *
      *                   processed.                                        *
      *                                                                     *
      * 02/10/96  01/01   RA9606       David ODriscoll                      *
      *           Refit of A9606.                                           *
      *           Includes UK Base work units: A06191.                      *
      *                   A06191       Christopher Dobson
      *           PCDDLNB logical and copybook commented out as
      *           no longer used.
      *                                                                     *
      * 06/03/97  01/01   FUPLET       Dominic Dunbar                       *
      *           Add notepad functionality and write LETC Record.          *
      *                                                                     *
      * 11/09/97  01/01   D9703        Tony Tang - CSC Hong Kong            *
      *           Life Asia Ver. 2.1                                        *
      *           Set up new date field DATESUB  added to PTRN for the      *
      *           Windforward after Reversal development.                   *
      *                                                                     *
      * 29/11/97    DUNC  SMART 9503 Conv for Client/Server.        <S9503>
      *                                                                     *
      * 17/12/98  01/01   V4L011       Daniel Wong                          *
      *           Update Substandard & Hospitalisation details on MLIA      *
      *                                                                     *
      *                                                                     *
      * 17/08/01  01/01   PCPPRT       Lai AT    - CSC Malaysia             *
      *                   Table T6634 is changed to TR384.                  *
      *                                                                     *
      * 14/01/02  01/01   PCPPRT       Saw Geok Tin                         *
      *                   If item cannot be found in TR384, re-read         *
      *                   again using the generic contract type.            *
      *                                                                     *
      * 24/06/04  01/01   SMU002       Simon Wong - CSC HK                  *
      *                   Update program remarks for scribe manual.         *
      *                                                                     *
      *                                                                     *
      * 08/07/04  01/01   LA3383       Nancie lin - FSG Taiwan              *
      *      a)  instead in 2000- section, if comment is blank, should ??   *
      *          retreive from default T5500.                               *
      *      b)  Set CHDRCOY and CHDRNUM to LETRQST                         *
      *                                                                     *
      * 11/11/04  01/01   LA3425       Fred Chow - CSC HK                   *
      *           - Remove all UNDRPF records when Withdraw/Decline/   ??   *
      *             Postpone and re-generate when reversal.                 *
      *                                                                     *
      * 26/11/04  01/01   LA3380       Fred Chow - CSC HK                   *
      *           - Enhance error handling when calling RLLIADB.       ??   *
      *                                                                     *
      * 01/06/06  01/01   LA3998       Chen Xu - CSC Beijing                *
      *           Pass value to the MLLI-LANGUAGE field of linkage.         *
      *                                                                     *
      * 09/10/06  01/01   V71L01       Chen Xu - CSC Beijing                *
      *           Initialize PTRN-PARAMS.                                   *
      *                                                                     *
      * 12/01/07  01/01   V71L08       Xu Chen/ASIA/CSC (Beijing)           *
      *           Check underwriter authority limit.                        *
      *                                                                     *
      * 08/03/07  01/01   V71L11       Lewis Liu Gang - CSC Beijing         *
      *           Updated for new action:"Not taken Up Proposal"            *
      *           and "Reverse Decline/Withdraw/Postphone/NTU"              *
      *                                                                     *
      * 26/03/07  01/01   V71L08       Nath Mathmaluwe/FSG/CSC (Singa       *
      *           Pass TRSA and UW limit to error line.                     *
      *                                                                     *
      * 29/06/09  Retrofitted by Helen Cui                                  *
      *           01/01   A06859       Lynne Dornan                         *
      *           The reason description is not a mandatory field and       *
      *           is also free format.  However, even if the user           *
      *           enters a free format description then T5500 is read       *
      *           using the Reason Code (if entered or not) and the         *
      *           description is overwritten with the Table Item desc       *
      *           if the item is found or '?' if it is not.  This is        *
      *           now changed to ensure that if the reason description      *
      *           contains a value the table is not read.                   *
      *                                                                     *
      *           Also, do not write a reason record (RESN) if              *
      *           Reason Code is blank.                                     *
      *                                                                     *
      * 15/07/09  Retrofitted by Helen Cui                                  *
      *           01/01   LA4541       CSC - Oisin Greene                   *
      *           Fix F5 refresh as currently transaction is processed      *
      *           when F5 is pressed.                                       *
      *                                                                     *
      * 20/07/09  Retrofitted by Helen Cui                                  *
      *           01/01   LA5057       CSC - Aidan Galligan                 *
      *           Recompile for changes to S5075_RESNDESC to allow          *
      *           lowercase entry.                                          *
      *                                                                     *
      **DD/MM/YY*************************************************************
      *
      /
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.                                      <004>
       OBJECT-COMPUTER. IBM-AS400.                                      <004>
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       01  WSAA-PROG                   PIC X(05) VALUE 'P5075'.
       01  WSAA-VERSION                PIC X(02) VALUE '02'.
      *
       01  ERRORS.
           03  CURS                    PIC X(04) VALUE 'CURS'.
           03  E186                    PIC X(04) VALUE 'E186'.
           03  E268                    PIC X(04) VALUE 'E268'.
           03  E304                    PIC X(04) VALUE 'E304'.
           03  E305                    PIC X(04) VALUE 'E305'.
           03  E455                    PIC X(04) VALUE 'E455'.
           03  E475                    PIC X(04) VALUE 'E475'.
           03  E480                    PIC X(04) VALUE 'E480'.
           03  E481                    PIC X(04) VALUE 'E481'.
           03  E484                    PIC X(04) VALUE 'E484'.
           03  E487                    PIC X(04) VALUE 'E487'.
           03  E492                    PIC X(04) VALUE 'E492'.
           03  E525                    PIC X(04) VALUE 'E525'.
           03  E767                    PIC X(04) VALUE 'E767'.
           03  F290                    PIC X(04) VALUE 'F290'.
           03  F321                    PIC X(04) VALUE 'F321'.
           03  F849                    PIC X(04) VALUE 'F849'.
           03  F915                    PIC X(04) VALUE 'F915'.
           03  G620                    PIC X(04) VALUE 'G620'.
           03  G709                    PIC X(04) VALUE 'G709'.
      *
       01  TABLES.
           03  T1692                   PIC X(05) VALUE 'T1692'.
           03  T3623                   PIC X(05) VALUE 'T3623'.
           03  T5679                   PIC X(05) VALUE 'T5679'.
           03  T5688                   PIC X(05) VALUE 'T5688'.
           03  T5689                   PIC X(05) VALUE 'T5689'.
           03  T5500                   PIC X(05) VALUE 'T5500'.
           03  T3588                   PIC X(05) VALUE 'T3588'.         <003>
      **** 03  T6634                   PIC X(05) VALUE 'T6634'. <PCPPRT><FUPLET>
           03  TR384                   PIC X(05) VALUE 'TR384'.         <PCPPRT>
      *                                                                 <003>
<001>**    03  T3625                   PIC X(05) VALUE 'T3625'.
<001>**    03  T5620                   PIC X(05) VALUE 'T5620'.
<001>**    03  T5645                   PIC X(05) VALUE 'T5645'.
<001>**    03  T5661                   PIC X(05) VALUE 'T5661'.
      ***  03  T5075                   PIC X(05) VALUE 'T5075'.         <003>
      *
       01  FORMATS.
           03  DESCREC                 PIC X(10) VALUE 'DESCREC'.
           03  ITEMREC                 PIC X(10) VALUE 'ITEMREC'.
           03  CLTSREC                 PIC X(10) VALUE 'CLTSREC'.
           03  ITDMREC                 PIC X(10) VALUE 'ITDMREC'.
           03  PTRNREC                 PIC X(10) VALUE 'PTRNREC'.
           03  RESNREC                 PIC X(10) VALUE 'RESNREC'.
           03  BATCUPREC               PIC X(10) VALUE 'BATCUPREC'.
           03  BNFYLNBREC              PIC X(10) VALUE 'BNFYLNBREC'.
      **** 03  PCDDLNBREC              PIC X(10) VALUE 'PCDDLNBREC'.    <RA9606>
      *****03  SACSLNBREC              PIC X(10) VALUE 'SACSLNBREC'.    <004>
           03  CHDRLNBREC              PIC X(10) VALUE 'CHDRLNBREC'.
           03  LIFELNBREC              PIC X(10) VALUE 'LIFELNBREC'.
           03  LFCLLNBREC              PIC X(10) VALUE 'LFCLLNBREC'.
           03  AGLFLNBREC              PIC X(10) VALUE 'AGLFLNBREC'.
           03  MLIACHDREC              PIC X(10) VALUE 'MLIACHDREC'.    <V4L011>
           03  MLIAAFIREC              PIC X(10) VALUE 'MLIAAFIREC'.    <V4L011>
           03  COVTCSNREC              PIC X(10) VALUE 'COVTCSNREC'.    <LA3425>
      /
       01  WSAA-ALL                    PIC S9(02) VALUE 99.
       01  WSAA-X                      PIC S9(03) COMP-3 VALUE 0.
       01  WSAA-Y                      PIC S9(03) COMP-3 VALUE 0.
       01  WSAA-Z                      PIC S9(03) COMP-3 VALUE 0.
       01  WSAA-MAX-OCC                PIC S9(03) COMP-3 VALUE 30.
       01  WSAA-TODAY           PIC S9(08).
      *
       01  WSAA-KILL                   PIC X     VALUE SPACES.
           88  FIRST-TIME              VALUE 'N'.
      *
       01  WSER-OWNERSEL               PIC X(01).
           88  WSER-OWNERSEL-INVALID   VALUE 'Y'.
       01  WSER-AGNTSEL                PIC X(01).
           88  WSER-AGNTSEL-INVALID    VALUE 'Y'.
       01  WSER-CNTBRANCH              PIC X(01).
           88  WSER-CNTBRANCH-INVALID  VALUE 'Y'.
       01  WSER-BILLING                PIC X(01).
           88  WSER-BILLING-INVALID    VALUE 'Y'.
      *
       01  FILLER.
           03  WSAA-SEC-PROG           PIC X(05) OCCURS 8.
                                                                        <LA3425>
       01  WSAA-JLIFE                  PIC X(02).                       <LA3425>
       01  WSAA-JLIFE-NUM REDEFINES WSAA-JLIFE.                         <LA3425>
           03  WSAA-JLIFE-N            PIC 9(02).                       <LA3425>
      *                                                                 <LA3425>
      *
      *  Working storage to build screen headings
      *
       01  WSAA-END-HEX.
           03  WSAA-HEX-20             PIC 9(03) COMP-3 VALUE 200.
           03  FILLER REDEFINES WSAA-HEX-20.
               05  WSAA-END-UNDERLINE  PIC X(01).
               05  FILLER              PIC X(01).
      *
       01  WSAA-START-HEX.
           03  WSAA-HEX-26             PIC 9(03) COMP-3 VALUE 260.
           03  FILLER REDEFINES WSAA-HEX-26.
               05  WSAA-START-UNDERLINE PIC X(01).
               05  FILLER              PIC X(01).
      *
       01  WSAA-HEADING.
           03  WSAA-HEADING-CHAR       PIC X(01) OCCURS 26.
      *
       01  WSAA-HEDLINE.
           03  WSAA-HEAD               PIC X(01) OCCURS 24.
      *                                                                 <V71L08>
       01  WSAA-ERRORLINE-SS.                                           <V71L08>
           03  WSAA-LEVELDSS           PIC X(06).                       <V71L08>
           03  WSAA-LEVELSS            PIC X(03).                       <V71L08>
           03  WSAA-ERRSS1             PIC X(20).                       <V71L08>
           03  WSAA-TRSA-SS            PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99.        <V71L08>
           03  WSAA-ERRSS2             PIC X(19).                       <V71L08>
           03  WSAA-UWLMT-SS           PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99.        <V71L08>
      *                                                                 <V71L08>
       01  WSAA-ERRORLINE-S.                                            <V71L08>
           03  WSAA-LEVELDS            PIC X(06).                       <V71L08>
           03  WSAA-LEVELS             PIC X(03).                       <V71L08>
           03  WSAA-ERRS1              PIC X(16).                       <V71L08>
           03  WSAA-TRSA-S             PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99.        <V71L08>
           03  WSAA-ERRS2              PIC X(15).                       <V71L08>
           03  WSAA-UWLMT-S            PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99.        <V71L08>
      *
      *  Working storage for billing validation
      *
       01  WSAA-DEFAULT-BILLCD         PIC X(01).
       01  WSAA-END-OF-MONTH           PIC X(01).
       01  WSAA-BILLCD-OK              PIC X(01).
       01  WSAA-RENDD                  PIC X(02) VALUE SPACES.
       01  WSAA-MOP-FREQ-OK            PIC X(01) VALUE SPACES.
       01  WSAA-RCD                    PIC 9(08) VALUE 0.
       01  WSAA-DATE REDEFINES WSAA-RCD.
           03  WSAA-YR                 PIC 9(04).
           03  WSAA-MTH                PIC 9(02).
           03  WSAA-DAY                PIC 9(02).
      *
       01  WSDD-BILLFREQ               PIC X(02).
       01  WSDD-MOP                    PIC X(01).
      *
      *   Batch key to obtain transaction number
      *
       01  WSAA-BATCKEY.
           COPY BATCKEY.
      /
      *****************************************************************
      *  Tables
      *****************************************************************
<001>**    COPY T3625REC.
      *
<001>**    COPY T5620REC.
      *
<001>**    COPY T5645REC.
      *
<001>**    COPY T5661REC.
      *                                                                 <V4L011>
           COPY T5500REC.                                               <V4L011>
      *                                                                 <V4L011>
           COPY T5679REC.
      *
           COPY T5688REC.
      *
           COPY T5689REC.
      *                                                                 <FUPLET>
      **** COPY T6634REC.                                       <PCPPRT><FUPLET>
           COPY TR384REC.                                               <PCPPRT>
      /
      *****************************************************************
      *  Subroutines
      *****************************************************************
           COPY GENSSWREC.
      *
           COPY DATCON1REC.
      *
           COPY DATCON2REC.
      *
           COPY BATCUPREC.
      *
           COPY SFTLOCKREC.
      *                                                                 <FUPLET>
           COPY LETRQSTREC.                                             <FUPLET>
      *
           COPY UWLMTREC.                                               <V71L08>
      *                                                                 <V71L08>
      /
      *****************************************************************
      *  Database files
      *****************************************************************
      /
           COPY ITEMSKM.
      *
           COPY ITDMSKM.
      *
           COPY CLTSSKM.
      *
           COPY DESCSKM.
      *
           COPY PTRNSKM.
      *
           COPY RESNSKM.
      *
      **** COPY PCDDLNBSKM.                                             <RA9606>
      *
      *****COPY SACSLNBSKM.                                             <004>
      *
           COPY LFCLLNBSKM.
      *
           COPY AGLFLNBSKM.
      *
           COPY CHDRLNBSKM.
      *
           COPY FLUPLNBSKM.
      *
           COPY LIFELNBSKM.
      /
           COPY VARCOM.
      /                                                                 <A06859>
           COPY SMTPFXCPY.                                              <A06859>
      /
           COPY SYSERRREC.
      *
           COPY OPSTATSREC.
      *
      ***  COPY SCRNPARAMS.                                             <S9503>
      /
      ***  COPY S5075SCR.                                               <S9503>
      /
      **** COPY RLLIAREC.                                       <LA3380><V4L011>
           COPY RLLIADBREC.                                             <LA3380>
      /                                                                 <V4L011>
           COPY MLIACHDSKM.                                             <V4L011>
      /                                                                 <V4L011>
           COPY MLIAAFISKM.                                             <V4L011>
      /                                                                 <V4L011>
           COPY CRTUNDWREC.                                             <LA3425>
      /                                                                 <LA3425>
           COPY COVTCSNSKM.                                             <LA3425>
      /                                                                 <LA3425>
       LINKAGE SECTION.
      * Screen copybooks are now part of the linkage.                   <S9503>
      /                                                                 <S9503>
           COPY SCRNPARAMS.                                             <S9503>
      /                                                                 <S9503>
           COPY S5075SCR.                                               <S9503>

           COPY WSSPCOMN.

           COPY WSSPLIFE.
      /
      * Statement now includes screen copybooks.                        <S9503>
       PROCEDURE DIVISION USING WSSP-COMMON-AREA WSSP-USER-AREA         <S9503>
                                               SCRN-SCREEN-PARAMS       <S9503>
                                               S5075-DATA-AREA      .   <S9503>

      *                                                                 <S9503>
      * MAINF has been replaced by MAING as the screen                  <S9503>
      * or driver now calls the program.                                <S9503>
      *                                                                 <S9503>
           COPY MAING.                                                  <S9503>
      *
      *****************************************************************
      *      INITIALISE FIELDS FOR SHOWING ON SCREEN
      *****************************************************************
      *
       1000-INITIALISE SECTION.
      *************************
      *
       1010-INITIALISE.
      *
      *
      * First time in only, read accounting rules for transaction
      *                     look up the singed-on branch description
      *                     get todays date
      *
<001>**    IF WSAA-BATCKEY NOT = SPACES
<001>**       GO TO 1020-BLANK-OUT-FIELDS.
      *
      * Set up the batch key fields.
           MOVE WSSP-BATCHKEY          TO WSKY-BATC-KEY.
      *
<001>**    MOVE WSSP-BATCHKEY          TO WSAA-BATCKEY.
<001>**    MOVE SPACES                 TO ITEM-DATA-KEY.
<001>**    MOVE 'IT'                   TO ITEM-ITEMPFX.
<001>**    MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.
<001>**    MOVE T5645                  TO ITEM-ITEMTABL.
<001>**    MOVE WSKY-BATC-BATCTRCDE    TO ITEM-ITEMITEM.
<001>**    MOVE 'READR'                TO ITEM-FUNCTION.
      *
<001>**    CALL 'ITEMIO' USING ITEM-PARAMS.
<001>**    IF ITEM-STATUZ              NOT = O-K
<001>**                            AND NOT = MRNF
<001>**        MOVE ITEM-STATUZ        TO SYSR-STATUZ
<001>**        PERFORM 600-FATAL-ERROR.
      *
<001>**    MOVE ITEM-GENAREA TO T5645-T5645-REC.
      *
<001>**1890-EXIT.
<001>**    EXIT.
      *
      *
           MOVE TDAY                   TO DTC1-FUNCTION.
           CALL 'DATCON1' USING DTC1-DATCON1-REC.
           MOVE DTC1-INT-DATE          TO WSAA-TODAY.
      *
       1020-BLANK-OUT-FIELDS.

           MOVE SPACES                 TO S5075-DATA-AREA.

           MOVE VRCM-MAX-DATE
                TO S5075-BILLCD                   .
           MOVE ZERO
                TO S5075-LASSURED                 .
           MOVE VRCM-MAX-DATE
                TO S5075-OCCDATE                  .
           MOVE ZERO
                TO S5075-POLINC                   .
      *
           MOVE WSKY-BATC-BATCTRCDE    TO S5075-BATCTRCDE.
      *
           MOVE 'Y'                    TO WSER-OWNERSEL,
                                          WSER-AGNTSEL,
                                          WSER-CNTBRANCH,
                                          WSER-BILLING.
      *
      * Retrieve contract fields from I/O module

           MOVE 'RETRV'                TO CHDRLNB-FUNCTION.
      *
           CALL 'CHDRLNBIO'       USING CHDRLNB-PARAMS.
           IF CHDRLNB-STATUZ           NOT = O-K
              MOVE CHDRLNB-PARAMS      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.
      *
           MOVE CHDRLNB-CHDRNUM        TO S5075-CHDRNUM.
           IF CHDRLNB-OCCDATE NOT = ZERO
              MOVE CHDRLNB-OCCDATE     TO S5075-OCCDATE.
           MOVE ZERO                   TO T5688-POLMIN.
      *
      *
      * get servicing branch description
      *
           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE 'IT'                   TO DESC-DESCPFX.
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.
           MOVE T1692                  TO DESC-DESCTABL.                <V71L11>
      *    MOVE WSSP-BRANCH            TO DESC-DESCITEM,                <V71L11>
      *                                   S5075-CNTBRANCH.              <V71L11>
           MOVE CHDRLNB-CNTBRANCH      TO DESC-DESCITEM,                <V71L11>
                                          S5075-CNTBRANCH.              <V71L11>
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.
           MOVE READR                  TO DESC-FUNCTION.
      *
           CALL 'DESCIO' USING DESC-PARAMS.
           IF DESC-STATUZ              NOT = O-K
                                   AND NOT = MRNF
               MOVE DESC-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
           IF DESC-STATUZ              = MRNF
               MOVE ALL '?'            TO S5075-SBRDESC
           ELSE
               MOVE DESC-LONGDESC      TO S5075-SBRDESC.
      *
      * Retrieve contract type from T5688
      *       validation rules from T5689
      *
           PERFORM 1600-READ-CONTRACT-TABLES.
      *
           IF S5075-OCCDATE-ERR NOT = SPACES
              MOVE 'Y'                 TO WSER-BILLING.
      *
           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE 'IT'                   TO DESC-DESCPFX.
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.
           MOVE T5688                  TO DESC-DESCTABL.
           MOVE CHDRLNB-CNTTYPE        TO DESC-DESCITEM.
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.
           MOVE READR                  TO DESC-FUNCTION.

           CALL 'DESCIO' USING DESC-PARAMS.
           IF DESC-STATUZ              NOT = O-K
                                   AND NOT = MRNF
               MOVE DESC-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.

           IF DESC-STATUZ              = O-K
              MOVE DESC-LONGDESC       TO WSAA-HEDLINE
           ELSE
              MOVE ALL '?'             TO WSAA-HEDLINE.
      *
           PERFORM 1100-LOAD-HEADING.
      *
      *    Retrieve contract status from T3623
      *

           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE 'IT'                   TO DESC-DESCPFX.
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.
           MOVE T3623                  TO DESC-DESCTABL.
           MOVE CHDRLNB-STATCODE       TO DESC-DESCITEM.
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.
           MOVE READR                  TO DESC-FUNCTION.

           CALL 'DESCIO' USING DESC-PARAMS.
           IF DESC-STATUZ              NOT = O-K
                                   AND NOT = MRNF
               MOVE DESC-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
           IF DESC-STATUZ              = O-K
              MOVE DESC-SHORTDESC      TO S5075-STATDSC
           ELSE
              MOVE ALL '?'             TO S5075-STATDSC.
      *
      *   Check to see if plan processing is applicable.
      *
           IF T5688-POLMIN = ZERO
               MOVE 'Y'                TO S5075-POLINC-OUT(ND)
               MOVE 'Y'                TO S5075-POLINC-OUT(PR)
           ELSE
               IF CHDRLNB-POLINC = ZERO
                  MOVE T5688-POLDEF    TO S5075-POLINC.
      *
           MOVE SPACE                  TO WSER-OWNERSEL,
                                          WSER-AGNTSEL,
                                          WSER-CNTBRANCH,
                                          WSER-BILLING.
      *
      *  Load all fields from the Contract Header to the Screen
      *
           MOVE CHDRLNB-POLINC         TO S5075-POLINC.
           MOVE CHDRLNB-COWNNUM        TO S5075-OWNERSEL.
           MOVE CHDRLNB-OCCDATE        TO S5075-OCCDATE.
           MOVE CHDRLNB-BILLFREQ       TO S5075-BILLFREQ.
           MOVE CHDRLNB-BILLCHNL       TO S5075-MOP.
           MOVE CHDRLNB-BILLCD         TO S5075-BILLCD.
           MOVE CHDRLNB-CNTCURR        TO S5075-CNTCURR.
           MOVE CHDRLNB-REGISTER       TO S5075-REGISTER.
           MOVE CHDRLNB-SRCEBUS        TO S5075-SRCEBUS.
           MOVE CHDRLNB-REPTYPE        TO S5075-REPTYPE.
           MOVE CHDRLNB-REPNUM         TO S5075-LREPNUM.
           MOVE CHDRLNB-CNTBRANCH      TO S5075-CNTBRANCH.
           MOVE CHDRLNB-AGNTNUM        TO S5075-AGNTSEL.
           MOVE CHDRLNB-CAMPAIGN       TO S5075-CAMPAIGN.
           MOVE CHDRLNB-CHDRSTCDA      TO S5075-STCAL.
           MOVE CHDRLNB-CHDRSTCDB      TO S5075-STCBL.
           MOVE CHDRLNB-CHDRSTCDC      TO S5075-STCCL.
           MOVE CHDRLNB-CHDRSTCDD      TO S5075-STCDL.
           MOVE CHDRLNB-CHDRSTCDE      TO S5075-STCEL.
      *
      *  Look up premium status
      *
           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE 'IT'                   TO DESC-DESCPFX.
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.
      ***  MOVE T5075                  TO DESC-DESCTABL.                <003>
           MOVE T3588                  TO DESC-DESCTABL.                <003>
           MOVE CHDRLNB-PSTATCODE      TO DESC-DESCITEM.
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.
           MOVE READR                  TO DESC-FUNCTION.
      *
           CALL 'DESCIO' USING DESC-PARAMS.
           IF DESC-STATUZ              NOT = O-K
                                   AND NOT = MRNF
               MOVE DESC-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
           IF DESC-STATUZ              = O-K
              MOVE DESC-SHORTDESC      TO S5075-PREM-STAT-DESC
           ELSE
              MOVE ALL '?'             TO S5075-PREM-STAT-DESC.
      *
      *   Look up all other descriptions
      *       - owner name
      *       - branch name
      *       - agent name
      *
           PERFORM 1700-FORMAT-CLIENT-NAME.
      *
           IF S5075-OWNERSEL-ERR NOT = SPACES
              MOVE 'Y'                 TO WSER-OWNERSEL.
      *
           IF S5075-CNTBRANCH-ERR NOT = SPACES
              MOVE 'Y'                 TO WSER-CNTBRANCH.
      *
           PERFORM 1900-FORMAT-AGENT-NAME.
      *
           IF S5075-AGNTSEL-ERR NOT = SPACES
              MOVE 'Y'                 TO WSER-AGNTSEL.
      *
      *   Count no of lives assured.
      *
            MOVE SPACES                TO LIFELNB-DATA-KEY.
            MOVE WSSP-COMPANY          TO LIFELNB-CHDRCOY.
            MOVE CHDRLNB-CHDRNUM       TO LIFELNB-CHDRNUM.
            MOVE BEGN                  TO LIFELNB-FUNCTION.
      *
           CALL 'LIFELNBIO'  USING LIFELNB-PARAMS.
           IF (LIFELNB-STATUZ          NOT = O-K) AND
              (LIFELNB-STATUZ          NOT = ENDP)
               MOVE LIFELNB-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
           MOVE 'N'                    TO S5075-CONPROSAL.
           PERFORM 1300-COUNT-LIVES UNTIL
               (LIFELNB-CHDRCOY    NOT = WSSP-COMPANY) OR
               (LIFELNB-CHDRNUM    NOT = CHDRLNB-CHDRNUM) OR
               (LIFELNB-STATUZ          = ENDP).
       1090-EXIT.
            EXIT.
      *
       1100-LOAD-HEADING          SECTION.
      ************************************
      *
       1110-LOAD-SCREEN.
           MOVE SPACES                 TO WSAA-HEADING.
      *
      *   Count the number of spaces at the end of the line
      *     (this is assuming there are none at the beginning)
      *
           PERFORM 1120-NOTHING
                   VARYING WSAA-X FROM 24 BY -1
                   UNTIL WSAA-X < 1
                      OR WSAA-HEAD(WSAA-X) NOT = SPACE.
           SUBTRACT WSAA-X FROM 24     GIVING WSAA-Y.
           IF WSAA-Y NOT = ZERO
              DIVIDE 2                 INTO WSAA-Y.
           ADD 1                       TO WSAA-Y.
      *
      *   WSAA-X is the size of the heading string
      *   WSAA-Y is the number of spaces in the front
      *   WSAA-Z is the position in the FROM string.
      *
           MOVE ZERO                   TO WSAA-Z.
           MOVE WSAA-START-UNDERLINE   TO WSAA-HEADING-CHAR(WSAA-Y).
           PERFORM 1130-MOVE-CHAR WSAA-X TIMES.
      *
      *   WSAA-X reused as the position in the TO string.
      *
           ADD 1                       TO WSAA-X.
           MOVE WSAA-END-UNDERLINE     TO WSAA-HEADING-CHAR(WSAA-X).
      *
           MOVE WSAA-HEADING           TO S5075-HEADING.
      *
           GO TO 1190-EXIT.
      *
       1120-NOTHING.
      *
       1130-MOVE-CHAR.
           ADD 1                       TO WSAA-Z.
           ADD WSAA-Y, WSAA-Z          GIVING WSAA-X.
           MOVE WSAA-HEAD(WSAA-Z)      TO WSAA-HEADING-CHAR(WSAA-X).
      *
       1190-EXIT.
           EXIT.
      *
      *
       1300-COUNT-LIVES           SECTION.
      ************************************
      *
       1300-COUNT.
           ADD 1                       TO S5075-LASSURED.
           IF S5075-CONPROSAL = 'Y'
              GO TO 1380-READ-NEXT-LIFE.
      *
      *  Check for a current proposal for the same client no.
      *
           MOVE SPACES                 TO LFCLLNB-DATA-KEY.
           MOVE WSSP-COMPANY           TO LFCLLNB-CHDRCOY.
           MOVE LIFELNB-LIFCNUM        TO LFCLLNB-LIFCNUM.
           MOVE BEGN                   TO LFCLLNB-FUNCTION.             <CAS1.0>
           MOVE O-K                    TO LFCLLNB-STATUZ.               <CAS1.0>
      **** MOVE READR                  TO LFCLLNB-FUNCTION.             <CAS1.0>
      ****                                                              <CAS1.0>
      **** CALL 'LFCLLNBIO'  USING LFCLLNB-PARAMS.                      <CAS1.0>
      **** IF LFCLLNB-STATUZ           NOT = O-K                        <CAS1.0>
      ****                         AND NOT = MRNF                       <CAS1.0>
      ****     MOVE LFCLLNB-PARAMS     TO SYSR-PARAMS                   <CAS1.0>
      ****     PERFORM 600-FATAL-ERROR.                                 <CAS1.0>
      ****                                                              <CAS1.0>
      **** IF LFCLLNB-STATUZ = O-K                                      <CAS1.0>
      ****     MOVE 'Y'                TO S5075-CONPROSAL.              <CAS1.0>
                                                                        <CAS1.0>
           PERFORM WITH TEST AFTER UNTIL LFCLLNB-STATUZ = ENDP          <CAS1.0>
                                                                        <CAS1.0>
              CALL 'LFCLLNBIO'         USING LFCLLNB-PARAMS             <CAS1.0>
                                                                        <CAS1.0>
              IF  LFCLLNB-STATUZ       NOT = O-K                        <CAS1.0>
              AND LFCLLNB-STATUZ       NOT = ENDP                       <CAS1.0>
                 MOVE LFCLLNB-PARAMS   TO SYSR-PARAMS                   <CAS1.0>
                 PERFORM 600-FATAL-ERROR                                <CAS1.0>
              END-IF                                                    <CAS1.0>
                                                                        <CAS1.0>
              IF LFCLLNB-CHDRCOY       NOT = WSSP-COMPANY               <CAS1.0>
              OR LFCLLNB-LIFCNUM       NOT = LIFELNB-LIFCNUM            <CAS1.0>
                 MOVE ENDP             TO LFCLLNB-STATUZ                <CAS1.0>
              END-IF                                                    <CAS1.0>
                                                                        <CAS1.0>
              IF  LFCLLNB-STATUZ       = O-K                            <CAS1.0>
              AND LFCLLNB-CHDRNUM      NOT = LIFELNB-CHDRNUM            <CAS1.0>
                 MOVE 'Y'              TO S5075-CONPROSAL               <CAS1.0>
                 MOVE ENDP             TO LFCLLNB-STATUZ                <CAS1.0>
              ELSE                                                      <CAS1.0>
                 MOVE NEXTR            TO LFCLLNB-FUNCTION              <CAS1.0>
              END-IF                                                    <CAS1.0>
                                                                        <CAS1.0>
           END-PERFORM.                                                 <CAS1.0>
      *                                                                 <001>
       1380-READ-NEXT-LIFE.                                             <001>
           MOVE NEXTR                  TO LIFELNB-FUNCTION.
           CALL 'LIFELNBIO'  USING LIFELNB-PARAMS.                      <001>
           IF (LIFELNB-STATUZ          NOT = O-K) AND                   <001>
              (LIFELNB-STATUZ          NOT = ENDP)                      <001>
               MOVE LIFELNB-PARAMS     TO SYSR-PARAMS                   <001>
               PERFORM 600-FATAL-ERROR.                                 <001>
      *
       1390-EXIT.
           EXIT.
      *
       1600-READ-CONTRACT-TABLES  SECTION.
      ************************************
      *
       1610-READ-CONTRACT-DEFINITION.
           MOVE SPACES                 TO ITDM-DATA-KEY.
           MOVE WSSP-COMPANY           TO ITDM-ITEMCOY.
           MOVE T5688                  TO ITDM-ITEMTABL.
           MOVE CHDRLNB-CNTTYPE        TO ITDM-ITEMITEM.
           MOVE S5075-OCCDATE          TO ITDM-ITMFRM.
           MOVE BEGN                   TO ITDM-FUNCTION.
           CALL 'ITDMIO' USING ITDM-PARAMS.
      *

           IF ITDM-STATUZ              NOT = O-K
                                   AND NOT = ENDP
               MOVE ITDM-STATUZ        TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR.
      *
           IF ITDM-ITEMCOY NOT = WSSP-COMPANY
            OR ITDM-ITEMTABL NOT = T5688
            OR ITDM-ITEMITEM NOT = CHDRLNB-CNTTYPE
            OR ITDM-STATUZ = ENDP
              MOVE F290                TO S5075-OCCDATE-ERR
           ELSE
              MOVE ITDM-GENAREA        TO T5688-T5688-REC.
      *
       1650-READ-BILLING-VALIDATION.
           MOVE SPACES                 TO ITDM-DATA-KEY.
           MOVE WSSP-COMPANY           TO ITDM-ITEMCOY.
           MOVE T5689                  TO ITDM-ITEMTABL.
           MOVE CHDRLNB-CNTTYPE        TO ITDM-ITEMITEM.
           MOVE S5075-OCCDATE          TO ITDM-ITMFRM.
           MOVE BEGN                   TO ITDM-FUNCTION.
      *
           CALL 'ITDMIO' USING ITDM-PARAMS.
           IF ITDM-STATUZ              NOT = O-K
                                   AND NOT = ENDP
               MOVE ITDM-STATUZ        TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR.
      *
           IF ITDM-ITEMCOY NOT = WSSP-COMPANY
            OR ITDM-ITEMTABL NOT = T5689
            OR ITDM-ITEMITEM NOT = CHDRLNB-CNTTYPE
            OR ITDM-STATUZ = ENDP
              MOVE E268                TO S5075-OCCDATE-ERR
              GO TO 1690-EXIT
           ELSE
              MOVE ITDM-GENAREA        TO T5689-T5689-REC.
      *
      *  Check for single MoP and/or frequency
      *
           MOVE SPACES                 TO WSDD-BILLFREQ
                                          WSDD-MOP.
           MOVE 1                      TO WSAA-X.
           PERFORM 1660-CHECK-TABLE 30 TIMES.
      *
           IF WSDD-BILLFREQ NOT = '??'
              MOVE WSDD-BILLFREQ       TO S5075-BILLFREQ
              MOVE 'Y'                 TO S5075-BILLFREQ-OUT (PR)
           ELSE
              MOVE SPACE               TO S5075-BILLFREQ-OUT (PR).
      *
           IF WSDD-MOP NOT = '?'
              MOVE WSDD-MOP            TO S5075-MOP
              MOVE 'Y'                 TO S5075-MOP-OUT (PR)
           ELSE
              MOVE SPACE               TO S5075-MOP-OUT (PR).
      *
           GO TO 1690-EXIT.
      *
       1660-CHECK-TABLE.
           IF T5689-BILLFREQ (WSAA-X) NOT = SPACES
                    AND WSDD-BILLFREQ NOT = '??'
             IF WSDD-BILLFREQ = SPACES
                MOVE T5689-BILLFREQ (WSAA-X) TO WSDD-BILLFREQ
             ELSE
                IF T5689-BILLFREQ (WSAA-X) NOT = WSDD-BILLFREQ
                   MOVE '??'           TO WSDD-BILLFREQ.
      *
           IF T5689-MOP (WSAA-X) NOT = SPACES
                    AND WSDD-MOP NOT = '?'
             IF WSDD-MOP = SPACES
                MOVE T5689-MOP (WSAA-X) TO WSDD-MOP
             ELSE
                IF T5689-MOP (WSAA-X) NOT = WSDD-MOP
                   MOVE '?'            TO WSDD-MOP.
      *
           ADD 1                       TO WSAA-X.
      *
       1690-EXIT.
           EXIT.
      *
       1700-FORMAT-CLIENT-NAME    SECTION.
      ************************************
      *
       1710-READ-CLIENT-RECORD.
           IF S5075-OWNERSEL           = SPACES
               MOVE E186               TO S5075-OWNERSEL-ERR
               GO TO 1790-EXIT.
      *
           MOVE SPACES                 TO CLTS-DATA-AREA.
           MOVE WSSP-FSUCO             TO CLTS-CLNTCOY.
           MOVE 'CN'                   TO CLTS-CLNTPFX.
           MOVE S5075-OWNERSEL         TO CLTS-CLNTNUM.
      *
           MOVE READR                  TO CLTS-FUNCTION.
      *
           CALL 'CLTSIO' USING CLTS-PARAMS.
      *
           IF CLTS-STATUZ              NOT = O-K
                                  AND  NOT = MRNF
                   MOVE CLTS-PARAMS    TO SYSR-PARAMS
                   PERFORM 600-FATAL-ERROR.
      *
           IF CLTS-STATUZ             = MRNF
              OR CLTS-VALIDFLAG       NOT = 1
               MOVE E304               TO S5075-OWNERSEL-ERR
               MOVE SPACES             TO S5075-OWNERNAME
           ELSE
               PERFORM PLAINNAME
               MOVE WSSP-LONGCONFNAME  TO S5075-OWNERNAME.
      *
       1790-EXIT.
           EXIT.
       1900-FORMAT-AGENT-NAME     SECTION.                              <001>
      ************************************
      *
       1910-READ-AGENT.
      *
           IF S5075-AGNTSEL            = SPACES
               MOVE E186               TO S5075-AGNTSEL-ERR
               GO TO 1990-EXIT.
      *
           MOVE SPACES                 TO AGLFLNB-DATA-KEY.
           MOVE WSSP-COMPANY           TO AGLFLNB-AGNTCOY.
           MOVE S5075-AGNTSEL          TO AGLFLNB-AGNTNUM.
           MOVE READR                  TO AGLFLNB-FUNCTION.
      *
           CALL 'AGLFLNBIO' USING AGLFLNB-PARAMS.
           IF AGLFLNB-STATUZ           NOT = O-K
                                  AND  NOT = MRNF
                MOVE AGLFLNB-PARAMS    TO SYSR-PARAMS
                PERFORM 600-FATAL-ERROR.
      *
           MOVE SPACES                 TO S5075-AGENTNAME.
           IF AGLFLNB-STATUZ = MRNF
              MOVE E305                TO S5075-AGNTSEL-ERR
              GO TO 1990-EXIT.
      *
           IF S5075-OCCDATE NOT = ZERO
                       AND  NOT = VRCM-MAX-DATE
              IF AGLFLNB-DTETRM        < S5075-OCCDATE
               OR AGLFLNB-DTEEXP       < S5075-OCCDATE
               OR AGLFLNB-DTEAPP       > S5075-OCCDATE
                 MOVE E475             TO S5075-AGNTSEL-ERR.
      *
           IF AGLFLNB-AGNTBR NOT = S5075-CNTBRANCH
              MOVE E455                TO S5075-AGNTSEL-ERR.
      *
           MOVE SPACES                 TO CLTS-DATA-AREA.
           MOVE WSSP-FSUCO             TO CLTS-CLNTCOY.
           MOVE 'CN'                   TO CLTS-CLNTPFX.
           MOVE AGLFLNB-CLNTNUM        TO CLTS-CLNTNUM.
      *
           MOVE READR                  TO CLTS-FUNCTION.
      *
           CALL 'CLTSIO' USING CLTS-PARAMS.
      *
           IF CLTS-STATUZ              NOT = O-K
                   MOVE CLTS-PARAMS    TO SYSR-PARAMS
                   PERFORM 600-FATAL-ERROR.
      *
           PERFORM PLAINNAME
           MOVE WSSP-LONGCONFNAME      TO S5075-AGENTNAME.
      *
       1990-EXIT.
           EXIT.
      /
           COPY CONFNAME.
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
      *                                                                 <S9503>
      *                                                                 <S9503>
           MOVE O-K                    TO WSSP-EDTERROR.                <S9503>
      *                                                                 <S9503>
           GO TO PRE-EXIT.                                              <S9503>
      *                                                                 <S9503>
       PRE-EXIT.                                                        <S9503>
           EXIT.                                                        <S9503>
      /                                                                 <S9503>
       2000-SCREEN-EDIT SECTION.
      **************************
      *
       2010-SCREEN-IO.
      *    CALL 'S5075IO' USING SCRN-SCREEN-PARAMS                      <S9503>
      *                          S5075-DATA-AREA.                       <S9503>
      * Screen errors are now handled in the calling program.           <S9503>
      *    PERFORM 200-SCREEN-ERRORS.                                   <S9503>
           MOVE O-K                    TO WSSP-EDTERROR.
      *
           IF SCRN-STATUZ              = CALC                           <LA4541>
              MOVE 'Y'                 TO WSSP-EDTERROR                 <LA4541>
           END-IF.                                                      <LA4541>
      *                                                                 <LA4541>
           IF WSSP-FLAG                = 'M'                            <V71L08>
           OR WSSP-FLAG                = 'E'                            <V71L08>
           OR WSSP-FLAG                = 'G'                            <V71L08>
              PERFORM 2100-CHECK-AUTHORITY                              <V71L08>
           END-IF.                                                      <V71L08>
                                                                        <V71L08>
      **** IF  S5075-REASONCD          = SPACES                 <A06859><LA3383>
      ****     IF S5075-RESNDESC       = SPACES                 <A06859><LA3383>
      ****          MOVE '?'           TO S5075-RESNDESC        <A06859><LA3383>
      ****     END-IF                                           <A06859><LA3383>
      ****     GO TO 2090-EXIT                                  <A06859><LA3383>
      **** END-IF.                                              <A06859><LA3383>
      ****                                                      <A06859><LA3383>
      **** IF  S5075-RESNDESC      NOT = SPACES                 <A06859><LA3383>
      ****       GO TO 2090-EXIT                                <A06859><LA3383>
      **** END-IF.                                              <A06859><LA3383>
      *                                                                 <LA3383>
      **** IF SCRN-STATUZ              = CALC                           <LA4541>
      ****    MOVE 'Y'                 TO WSSP-EDTERROR.                <LA4541>
      ****                                                              <LA4541>
      * Only read T5500 for the description if the user has not already <A06859>
      * provided one.                                                   <A06859>
      *                                                                 <A06859>
           IF  S5075-RESNDESC           = SPACES                        <A06859>
               PERFORM 2200-READ-T5500-DESC                             <A06859>
           END-IF.                                                      <A06859>
                                                                        <A06859>
           IF  S5075-ERROR-INDICATORS                                   <A06859>
                                    NOT = SPACES                        <A06859>
               MOVE 'Y'                TO WSSP-EDTERROR                 <A06859>
           END-IF.                                                      <A06859>
                                                                        <A06859>
      **** MOVE SPACES                 TO DESC-DATA-KEY.                <A06859>
      **** MOVE 'IT'                   TO DESC-DESCPFX.                 <A06859>
      **** MOVE WSSP-COMPANY           TO DESC-DESCCOY.                 <A06859>
      **** MOVE T5500                  TO DESC-DESCTABL.                <A06859>
      **** MOVE S5075-REASONCD         TO DESC-DESCITEM.                <A06859>
      **** MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.                <A06859>
      **** MOVE READR                  TO DESC-FUNCTION.                <A06859>
      ****                                                              <A06859>
      **** CALL 'DESCIO' USING DESC-PARAMS.                             <A06859>
      **** IF DESC-STATUZ              NOT = O-K                        <A06859>
      ****                         AND NOT = MRNF                       <A06859>
      ****     MOVE DESC-PARAMS        TO SYSR-PARAMS                   <A06859>
      ****     PERFORM 600-FATAL-ERROR.                                 <A06859>
      ****                                                              <A06859>
      **** IF DESC-STATUZ              = O-K                            <A06859>
      ****    MOVE DESC-LONGDESC       TO S5075-RESNDESC                <A06859>
      **** ELSE                                                         <A06859>
      ****    MOVE '?'                 TO S5075-RESNDESC.               <A06859>

       2090-EXIT.
            EXIT.
      *
      /
       2100-CHECK-AUTHORITY SECTION.                                    <V71L08>
      ******************************                                    <V71L08>
       2110-START.                                                      <V71L08>
                                                                        <V71L08>
           MOVE 'CHCK'                 TO UWLMT-FUNCTION.               <V71L08>
           MOVE CHDRLNB-CHDRCOY        TO UWLMT-CHDRCOY.                <V71L08>
           MOVE CHDRLNB-CHDRNUM        TO UWLMT-CHDRNUM.                <V71L08>
           MOVE CHDRLNB-CNTTYPE        TO UWLMT-CNTTYP.                 <V71L08>
           MOVE CHDRLNB-COWNPFX        TO UWLMT-COWNPFX.                <V71L08>
           MOVE S5075-OWNERSEL         TO UWLMT-LIFCNUM.                <V71L08>
           MOVE WSSP-LANGUAGE          TO UWLMT-LANGUAGE.               <V71L08>
           MOVE WSSP-FSUCO             TO UWLMT-FSUCO.                  <V71L08>
           MOVE WSSP-USERID            TO UWLMT-USERID.                 <V71L08>
                                                                        <V71L08>
           CALL 'UWLMTCHK'             USING UWLMT-REC.                 <V71L08>
           IF  UWLMT-STATUZ            NOT = O-K                        <V71L08>
               IF UWLMT-STATUZ             = 'SS'                       <V71L08>
                  MOVE UWLMT-UWLEVEL      TO WSAA-LEVELSS               <V71L08>
                  MOVE 'Level:'           TO WSAA-LEVELDSS              <V71L08>
                  MOVE '. Sub Stnd TRSA Amt '     TO WSAA-ERRSS1        <V71L08>
                  MOVE UWLMT-SUMINS               TO WSAA-TRSA-SS       <V71L08>
                  MOVE ' > Sub Stnd UW Lmt '      TO WSAA-ERRSS2        <V71L08>
                  MOVE UWLMT-UNDWRLIM             TO WSAA-UWLMT-SS      <V71L08>
                  MOVE WSAA-ERRORLINE-SS          TO SCRN-ERRORLINE     <V71L08>
               ELSE                                                     <V71L08>
                  MOVE UWLMT-UWLEVEL              TO WSAA-LEVELS        <V71L08>
                  MOVE 'Level:'                   TO WSAA-LEVELDS       <V71L08>
                  MOVE '. Stnd TRSA Amt '         TO WSAA-ERRS1         <V71L08>
                  MOVE UWLMT-SUMINS               TO WSAA-TRSA-S        <V71L08>
                  MOVE ' > Stnd UW Lmt '          TO WSAA-ERRS2         <V71L08>
                  MOVE UWLMT-UNDWRLIM             TO WSAA-UWLMT-S       <V71L08>
                  MOVE WSAA-ERRORLINE-S           TO SCRN-ERRORLINE     <V71L08>
               END-IF                                                   <V71L08>
               MOVE 'Y'                TO WSSP-EDTERROR                 <V71L08>
           END-IF.                                                      <V71L08>
                                                                        <V71L08>
       2190-EXIT.                                                       <V71L08>
           EXIT.                                                        <V71L08>
      /                                                                 <A06859>
       2200-READ-T5500-DESC SECTION.                                    <A06859>
      ******************************                                    <A06859>
      *                                                                 <A06859>
       2210-READ.                                                       <A06859>
                                                                        <A06859>
           INITIALIZE DESCREC-KEY-DATA     OF DESC-PARAMS.              <A06859>
           INITIALIZE DESCREC-NON-KEY-DATA OF DESC-PARAMS.              <A06859>
                                                                        <A06859>
           MOVE SMTP-ITEM              TO DESC-DESCPFX.                 <A06859>
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.                 <A06859>
           MOVE T5500                  TO DESC-DESCTABL.                <A06859>
           MOVE S5075-REASONCD         TO DESC-DESCITEM.                <A06859>
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.                <A06859>
           MOVE READR                  TO DESC-FUNCTION.                <A06859>
                                                                        <A06859>
           MOVE DESCREC                TO DESC-FORMAT.                  <A06859>
                                                                        <A06859>
           CALL 'DESCIO'            USING DESC-PARAMS.                  <A06859>
                                                                        <A06859>
           IF  DESC-STATUZ          NOT = O-K                           <A06859>
           AND DESC-STATUZ          NOT = MRNF                          <A06859>
               MOVE DESC-PARAMS        TO SYSR-PARAMS                   <A06859>
               MOVE DESC-STATUZ        TO SYSR-STATUZ                   <A06859>
               PERFORM 600-FATAL-ERROR                                  <A06859>
           END-IF.                                                      <A06859>
                                                                        <A06859>
           IF  DESC-STATUZ              = O-K                           <A06859>
               MOVE DESC-LONGDESC      TO S5075-RESNDESC                <A06859>
           ELSE                                                         <A06859>
               MOVE SPACES             TO S5075-RESNDESC                <A06859>
           END-IF.                                                      <A06859>
                                                                        <A06859>
       2290-EXIT.                                                       <A06859>
           EXIT.                                                        <A06859>
      /                                                                 <V71L08>
      *****************************************************************
      *     UPDATE DATABASE IF REQUIRED AND LOG TRANSACTION
      *****************************************************************
      *
       3000-UPDATE SECTION.
      **********************
      *
       3010-UPDATE-DATABASE.
      *
      *  Update contract header fields as follows
      *
      *  contract status date to today's date
      *
           MOVE WSAA-TODAY             TO CHDRLNB-STATDATE.
      *
      *  read T5679 for contract status
      *
           MOVE SPACES                 TO ITEM-DATA-KEY.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.
           MOVE T5679                  TO ITEM-ITEMTABL.
           MOVE WSKY-BATC-BATCTRCDE    TO ITEM-ITEMITEM.
           MOVE READR                  TO ITEM-FUNCTION.
      *
           CALL 'ITEMIO' USING ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = O-K
                                       AND   MRNF
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.
      *
           IF ITEM-STATUZ              NOT = O-K
              MOVE F321                TO SCRN-ERROR-CODE
              GO TO 3090-EXIT.
      *
           MOVE ITEM-GENAREA           TO T5679-T5679-REC.
           MOVE T5679-SET-CN-RISK-STAT TO CHDRLNB-STATCODE.
      *
      *
      *  contract status transaction no to contract tran no
      *
           MOVE CHDRLNB-TRANNO         TO CHDRLNB-STATTRAN,
                                          CHDRLNB-PSTATTRAN.
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
           MOVE 'WRITS'                TO CHDRLNB-FUNCTION.
           MOVE CHDRLNBREC             TO CHDRLNB-FORMAT.
      *
           CALL 'CHDRLNBIO' USING CHDRLNB-PARAMS.
      *
           IF CHDRLNB-STATUZ           NOT = O-K
              MOVE CHDRLNB-PARAMS      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.
      *                                                                 <V4L011>
      * Write MLIA record.                                              <V4L011>
      *    'G' is reversal, changed from 'E' originally         <V71L11><V4L011>
      *                                                         <V71L11><V4L011>
      *    IF WSSP-FLAG                 = 'E'                   <V71L11><V4L011>
           IF WSSP-FLAG                 = 'G'                           <V71L11>
              PERFORM M100-READ-MLIACHD                                 <V4L011>
           ELSE                                                         <V4L011>
              PERFORM 3500-CHECK-SUBSTD                                 <V4L011>
              IF T5500-MIND = 'Y' THEN                                  <V4L011>
                 MOVE CHDRLNB-CHDRCOY     TO MLLI-CHDRCOY               <V4L011>
                 MOVE CHDRLNB-CHDRNUM     TO MLLI-CHDRNUM               <V4L011>
                 MOVE WSSP-FSUCO          TO MLLI-FSUCOY                <V4L011>
                 MOVE WSKY-BATC-BATCTRCDE TO MLLI-BATCTRCDE             <V4L011>
                 MOVE WSSP-LANGUAGE       TO MLLI-LANGUAGE              <LA3998>
                 CALL 'RLLIADB'       USING  MLLI-RLLIA-REC             <V4L011>
                 IF MLLI-STATUZ       NOT = O-K                         <LA3380>
                     MOVE MLLI-RLLIA-REC      TO SYSR-PARAMS            <LA3380>
                     MOVE MLLI-STATUZ         TO SYSR-STATUZ            <LA3380>
                     PERFORM 600-FATAL-ERROR                            <LA3380>
                 END-IF                                                 <LA3380>
              END-IF                                                    <V4L011>
           END-IF.                                                      <V4L011>
      *                                                                 <V4L011>
      * Write a PTRN record.
           INITIALIZE                  PTRN-PARAMS.                     <V71L01>
           MOVE SPACES                 TO PTRN-DATA-KEY.
           MOVE 'BA'                   TO PTRN-BATCPFX.                 <002>
           MOVE WSKY-BATC-BATCCOY      TO PTRN-BATCCOY.
           MOVE WSKY-BATC-BATCBRN      TO PTRN-BATCBRN.
           MOVE WSKY-BATC-BATCACTYR    TO PTRN-BATCACTYR.
           MOVE WSKY-BATC-BATCACTMN    TO PTRN-BATCACTMN.
           MOVE WSKY-BATC-BATCTRCDE    TO PTRN-BATCTRCDE.
           MOVE WSKY-BATC-BATCBATCH    TO PTRN-BATCBATCH.
           MOVE CHDRLNB-CHDRCOY        TO PTRN-CHDRCOY.
           MOVE CHDRLNB-CHDRNUM        TO PTRN-CHDRNUM.
           MOVE CHDRLNB-TRANNO         TO PTRN-TRANNO.
           MOVE VRCM-DATE              TO PTRN-TRANSACTION-DATE.
           MOVE VRCM-TIME              TO PTRN-TRANSACTION-TIME.
      **** MOVE VRCM-TERM              TO PTRN-TERMID.                  <005>
           MOVE VRCM-TERMID            TO PTRN-TERMID.                  <005>
           MOVE VRCM-USER              TO PTRN-USER.
           MOVE TDAY                   TO DTC1-FUNCTION.
           CALL 'DATCON1' USING DTC1-DATCON1-REC.
           MOVE DTC1-INT-DATE          TO PTRN-PTRNEFF.
           MOVE DTC1-INT-DATE          TO PTRN-DATESUB.                 <D9703>
           MOVE PTRNREC                TO PTRN-FORMAT.
           MOVE WRITR                  TO PTRN-FUNCTION.
           CALL 'PTRNIO'               USING PTRN-PARAMS.
           IF PTRN-STATUZ              NOT = O-K
              MOVE PTRN-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.


      * Update the batch header using BATCUP.
           MOVE SPACES                 TO BCUP-BATCUP-REC.
           MOVE WSKY-BATC-KEY          TO BCUP-BATCHKEY.
           MOVE WSKY-BATC-BATCBATCH    TO BCUP-BATCBATCH.
           MOVE 1                      TO BCUP-TRANCNT.
           MOVE ZEROS                  TO BCUP-ETREQCNT
                                          BCUP-SUB
                                          BCUP-BCNT
                                          BCUP-BVAL
                                          BCUP-ASCNT
                                          BCUP-STATUZ.
           MOVE WRITS                  TO BCUP-FUNCTION.
           CALL 'BATCUP'               USING BCUP-BATCUP-REC.
           IF BCUP-STATUZ              NOT = O-K
              MOVE BCUP-BATCUP-REC     TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.

      * Create a RESN record if reason code or narrative was entered

           IF S5075-RESNDESC NOT = SPACES OR
              S5075-REASONCD NOT = SPACES
              MOVE SPACES          TO   RESN-DATA-KEY
              MOVE CHDRLNB-CHDRCOY TO   RESN-CHDRCOY
              MOVE CHDRLNB-CHDRNUM TO   RESN-CHDRNUM
              MOVE CHDRLNB-TRANNO  TO   RESN-TRANNO
              MOVE S5075-REASONCD  TO   RESN-REASONCD
              MOVE S5075-RESNDESC  TO   RESN-RESNDESC
<001> ****    MOVE WSKY-BATC-BATCTRCDE TO RESN-TRANCDE.                 <A06859>
              MOVE WSKY-BATC-BATCTRCDE TO RESN-TRANCDE                  <A06859>
              MOVE VRCM-DATE       TO   RESN-TRANSACTION-DATE
              MOVE VRCM-TIME       TO   RESN-TRANSACTION-TIME
              MOVE VRCM-USER       TO   RESN-USER
              MOVE RESNREC         TO   RESN-FORMAT
              MOVE WRITR           TO   RESN-FUNCTION
              CALL 'RESNIO'               USING RESN-PARAMS
              IF RESN-STATUZ              NOT = O-K
                 MOVE RESN-PARAMS         TO SYSR-PARAMS
                 PERFORM 600-FATAL-ERROR.
      ****                                                              <LA3425>
      **** If not inquiry mode, remove all UNDRPF records.              <LA3425>
      ****                                                              <LA3425>
           IF WSSP-FLAG            NOT = 'I'                            <LA3425>
              PERFORM 3700-DELETE-UNDRPF                                <LA3425>
           END-IF.                                                      <LA3425>
                                                                        <FUPLET>
           IF WSSP-FLAG = 'M'                                           <Q39>
           OR WSSP-FLAG                 = 'E'                           <V71L11>
           PERFORM 6000-WRITE-LETTER.                                   <FUPLET>

      * Release the soft lock on the contract.
           IF WSSP-FLAG = 'I'
              GO TO 3090-EXIT.
                                                                        <LA3425>
      ****                                                              <LA3425>
      **** If reversing Withdraw/Decline/Postpone, generate UNDRPF rec  <LA3425>
      ****                                                              <LA3425>
      *    IF WSSP-FLAG                = 'E'                    <V71L11><LA3425>
           IF WSSP-FLAG                = 'G'                            <V71L11>
              PERFORM 3800-CREATE-UNDRPF                                <LA3425>
           END-IF.                                                      <LA3425>
                                                                        <LA3425>
           MOVE SPACES                 TO SFTL-SFTLOCK-REC.
           MOVE WSSP-COMPANY           TO SFTL-COMPANY.
           MOVE CHDRLNB-CHDRNUM        TO SFTL-ENTITY.
           MOVE 'CH'                   TO SFTL-ENTTYP.
           MOVE VRCM-USER              TO SFTL-USER.
           MOVE WSKY-BATC-BATCTRCDE    TO SFTL-TRANSACTION.
           MOVE SPACES                 TO SFTL-STATUZ.
           MOVE 'UNLK'                 TO SFTL-FUNCTION.
           CALL 'SFTLOCK'              USING SFTL-SFTLOCK-REC.
           IF SFTL-STATUZ              NOT = O-K
              MOVE SFTL-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.


       3090-EXIT.
            EXIT.
      /
       3500-CHECK-SUBSTD SECTION.
      ************************
       3510-CHECK-SUBSTD-FLAG.
      *
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.
           MOVE T5500                  TO ITEM-ITEMTABL.
           MOVE S5075-REASONCD         TO ITEM-ITEMITEM.
           MOVE READR                  TO ITEM-FUNCTION.
           CALL 'ITEMIO' USING ITEM-PARAMS.


           IF ITEM-STATUZ              NOT = O-K
                                   AND NOT = MRNF
               MOVE ITEM-STATUZ        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.

           IF ITEM-STATUZ              = 'MRNF'
              MOVE E268                TO S5075-REASONCD-ERR
           ELSE
              MOVE ITEM-GENAREA        TO T5500-T5500-REC.
      *       IF ITEM-GENAREA NOT = SPACES
      *          MOVE T5500-MIND TO ABC.
       3600-EXIT.
            EXIT.
                                                                        <LA3425>
       3700-DELETE-UNDRPF SECTION.                                      <LA3425>
      ****************************                                      <LA3425>
       3710-DELETE.                                                     <LA3425>
      *                                                                 <LA3425>
           INITIALIZE UNDW-PARM-REC.                                    <LA3425>
           MOVE CHDRLNB-CHDRCOY        TO UNDW-COY.                     <LA3425>
           MOVE CHDRLNB-CNTCURR        TO UNDW-CURRCODE.                <LA3425>
           MOVE CHDRLNB-CHDRNUM        TO UNDW-CHDRNUM.                 <LA3425>
           MOVE ALL '*'                TO UNDW-CRTABLE.                 <LA3425>
           MOVE 'DEL'                  TO UNDW-FUNCTION.                <LA3425>
           CALL 'CRTUNDWRT'            USING UNDW-PARM-REC.             <LA3425>
           IF UNDW-STATUS              NOT = O-K                        <LA3425>
              MOVE UNDW-PARM-REC       TO SYSR-PARAMS                   <LA3425>
              MOVE UNDW-STATUS         TO SYSR-STATUZ                   <LA3425>
              MOVE 'CRTUNDWRT'         TO SYSR-IOMOD                    <LA3425>
              PERFORM 600-FATAL-ERROR                                   <LA3425>
           END-IF.                                                      <LA3425>
      *                                                                 <LA3425>
       3790-EXIT.                                                       <LA3425>
            EXIT.                                                       <LA3425>
                                                                        <LA3425>
       3800-CREATE-UNDRPF SECTION.                                      <LA3425>
      ****************************                                      <LA3425>
       3810-CREATE.                                                     <LA3425>
      *                                                                 <LA3425>
           INITIALIZE COVTCSNREC-KEY-DATA.                              <LA3425>
           MOVE CHDRLNB-CHDRCOY        TO COVTCSN-CHDRCOY               <LA3425>
           MOVE CHDRLNB-CHDRNUM        TO COVTCSN-CHDRNUM               <LA3425>
           MOVE BEGN                   TO COVTCSN-FUNCTION.             <LA3425>
           MOVE COVTCSNREC             TO COVTCSN-FORMAT.               <LA3425>
           MOVE O-K                    TO COVTCSN-STATUZ.               <LA3425>
           PERFORM 3820-CREATE-UNDR-COVT                                <LA3425>
             UNTIL COVTCSN-STATUZ NOT = O-K.                            <LA3425>
      *                                                                 <LA3425>
       3890-EXIT.                                                       <LA3425>
            EXIT.                                                       <LA3425>
                                                                        <LA3425>
       3820-CREATE-UNDR-COVT SECTION.                                   <LA3425>
      *******************************                                   <LA3425>
       3825-CREATE.                                                     <LA3425>
      *                                                                 <LA3425>
           CALL 'COVTCSNIO'            USING COVTCSN-PARAMS.            <LA3425>
                                                                        <LA3425>
           IF COVTCSN-STATUZ           = ENDP OR                        <LA3425>
               CHDRLNB-CHDRCOY     NOT = COVTCSN-CHDRCOY OR             <LA3425>
               CHDRLNB-CHDRNUM     NOT = COVTCSN-CHDRNUM                <LA3425>
               MOVE ENDP               TO COVTCSN-STATUZ                <LA3425>
               GO TO 3829-EXIT                                          <LA3425>
           END-IF.                                                      <LA3425>
                                                                        <LA3425>
           MOVE NEXTR                  TO COVTCSN-FUNCTION.             <LA3425>
                                                                        <LA3425>
           INITIALIZE UNDW-PARM-REC.                                    <LA3425>
           MOVE ZEROES                 TO WSAA-JLIFE-N.                 <LA3425>
                                                                        <LA3425>
       3826-NEXT-LIFE.                                                  <LA3425>
                                                                        <LA3425>
           PERFORM M300-READ-LIFE.                                      <LA3425>
                                                                        <LA3425>
           IF LIFELNB-STATUZ           = MRNF                           <LA3425>
               GO TO 3829-EXIT                                          <LA3425>
           END-IF.                                                      <LA3425>
                                                                        <LA3425>
           MOVE LIFELNB-LIFCNUM        TO UNDW-CLNTNUM.                 <LA3425>
           MOVE COVTCSN-CHDRCOY        TO UNDW-COY.                     <LA3425>
           MOVE COVTCSN-CHDRNUM        TO UNDW-CHDRNUM.                 <LA3425>
           MOVE COVTCSN-LIFE           TO UNDW-LIFE.                    <LA3425>
           MOVE COVTCSN-CRTABLE        TO UNDW-CRTABLE.                 <LA3425>
           MOVE WSKY-BATC-BATCTRCDE    TO UNDW-BATCTRCDE.               <LA3425>
           MOVE COVTCSN-SUMINS         TO UNDW-SUMINS.                  <LA3425>
           MOVE CHDRLNB-CNTTYPE        TO UNDW-CNTTYP.                  <LA3425>
           MOVE CHDRLNB-CNTCURR        TO UNDW-CURRCODE.                <LA3425>
           MOVE 'ADD'                  TO UNDW-FUNCTION.                <LA3425>
                                                                        <LA3425>
           CALL 'CRTUNDWRT'            USING UNDW-PARM-REC.             <LA3425>
                                                                        <LA3425>
           IF UNDW-STATUS              NOT = O-K                        <LA3425>
              MOVE UNDW-PARM-REC       TO SYSR-PARAMS                   <LA3425>
              MOVE UNDW-STATUS         TO SYSR-STATUZ                   <LA3425>
              MOVE 'CRTUNDWRT'         TO SYSR-IOMOD                    <LA3425>
              PERFORM 600-FATAL-ERROR.                                  <LA3425>
                                                                        <LA3425>
           ADD 1                       TO WSAA-JLIFE-N.                 <LA3425>
                                                                        <LA3425>
           GO TO 3826-NEXT-LIFE.                                        <LA3425>
      *                                                                 <LA3425>
       3829-EXIT.                                                       <LA3425>
            EXIT.                                                       <LA3425>
                                                                        <LA3425>
      /
       M100-READ-MLIACHD SECTION.                                       <L04>
      ***************************                                       <L04>
                                                                        <L04>
       M100-START.                                                      <L04>
                                                                        <L04>
           MOVE O-K                    TO MLIACHD-STATUZ.               <L04>
           MOVE CHDRLNB-CHDRNUM        TO MLIACHD-MLENTITY.             <L04>
           MOVE MLIACHDREC             TO MLIACHD-FORMAT.               <L04>
           MOVE BEGNH                  TO MLIACHD-FUNCTION.             <L04>
                                                                        <L04>
           PERFORM M200-READ-MLIACHDIO                                  <L04>
                     UNTIL MLIACHD-STATUZ = ENDP.                       <L04>
                                                                        <L04>
       M100-EXIT.                                                       <L04>
           EXIT.                                                        <L04>
                                                                        <L04>
       M200-READ-MLIACHDIO SECTION.                                     <L04>
      *****************************                                     <L04>
                                                                        <L04>
       M200-START.                                                      <L04>
                                                                        <L04>
           CALL 'MLIACHDIO'       USING   MLIACHD-PARAMS.               <L04>
           IF MLIACHD-STATUZ        NOT = O-K AND ENDP                  <L04>
              MOVE MLIACHD-STATUZ      TO SYSR-STATUZ                   <L04>
              MOVE MLIACHD-PARAMS      TO SYSR-PARAMS                   <L04>
              PERFORM 600-FATAL-ERROR                                   <L04>
           END-IF.                                                      <L04>
                                                                        <L04>
           IF CHDRLNB-CHDRNUM       NOT = MLIACHD-MLENTITY              <L04>
              MOVE ENDP                TO MLIACHD-STATUZ                <L04>
           END-IF.                                                      <L04>
                                                                        <L04>
           IF MLIACHD-STATUZ            = ENDP                          <L04>
              GO TO M200-EXIT                                           <L04>
           END-IF.                                                      <L04>
                                                                        <L04>
           IF MLIACHD-MIND              = 'Y'                           <L04>
              MOVE MLIACHD-MLCOYCDE    TO MLIAAFI-MLCOYCDE              <L04>
              MOVE MLIACHD-MLOLDIC     TO MLIAAFI-MLOLDIC               <L04>
              MOVE MLIACHD-MLOTHIC     TO MLIAAFI-MLOTHIC               <L04>
              MOVE MLIACHD-CLNTNAML    TO MLIAAFI-CLNTNAML              <L04>
              MOVE MLIACHD-DOB         TO MLIAAFI-DOB                   <L04>
              MOVE MLIACHD-RSKFLG      TO MLIAAFI-RSKFLG                <L04>
              MOVE MLIACHD-HPROPDTE    TO MLIAAFI-HPROPDTE              <L04>
              MOVE MLIACHD-MLHSPERD    TO MLIAAFI-MLHSPERD              <L04>
              MOVE MLIACHD-MLMEDCDE01  TO MLIAAFI-MLMEDCDE01            <L04>
              MOVE MLIACHD-MLMEDCDE02  TO MLIAAFI-MLMEDCDE02            <L04>
              MOVE MLIACHD-MLMEDCDE03  TO MLIAAFI-MLMEDCDE03            <L04>
              MOVE 'Y'                 TO MLIAAFI-INDC                  <L04>
              MOVE 'N'                 TO MLIAAFI-MIND                  <L04>
              MOVE TDAY                TO DTC1-FUNCTION                 <L04>
              CALL 'DATCON1' USING DTC1-DATCON1-REC                     <L04>
              MOVE DTC1-INT-DATE       TO WSAA-TODAY                    <L04>
              MOVE WSAA-TODAY          TO MLIAAFI-EFFDATE               <L04>
              MOVE MLIACHD-SECURITYNO  TO MLIAAFI-SECURITYNO            <L04>
              MOVE CHDRLNB-CHDRNUM     TO MLIAAFI-MLENTITY              <L04>
              MOVE '3'                 TO MLIAAFI-ACTN                  <L04>
              MOVE MLIAAFIREC          TO MLIAAFI-FORMAT                <L04>
              MOVE UPDAT               TO MLIAAFI-FUNCTION              <L04>
              CALL 'MLIAAFIIO'      USING MLIAAFI-PARAMS                <L04>
              IF MLIAAFI-STATUZ     NOT = O-K AND MRNF                  <L04>
                 MOVE MLIAAFI-STATUZ   TO SYSR-STATUZ                   <L04>
                 MOVE MLIAAFI-PARAMS   TO SYSR-PARAMS                   <L04>
                 PERFORM 600-FATAL-ERROR                                <L04>
              END-IF                                                    <L04>
           ELSE                                                         <L04>
              MOVE DELET               TO MLIACHD-FUNCTION              <L04>
              CALL 'MLIACHDIO'      USING MLIACHD-PARAMS                <L04>
              IF MLIACHD-STATUZ     NOT = O-K AND MRNF                  <L04>
                 MOVE MLIACHD-STATUZ   TO SYSR-STATUZ                   <L04>
                 MOVE MLIACHD-PARAMS   TO SYSR-PARAMS                   <L04>
                 PERFORM 600-FATAL-ERROR                                <L04>
              END-IF                                                    <L04>
           END-IF.                                                      <L04>
                                                                        <L04>
       M200-NEXTR.                                                      <L04>
                                                                        <L04>
           MOVE NEXTR                  TO MLIACHD-FUNCTION.             <L04>
                                                                        <L04>
       M200-EXIT.                                                       <L04>
           EXIT.                                                        <L04>
                                                                        <LA3425>
       M300-READ-LIFE SECTION.                                          <LA3425>
      ************************                                          <LA3425>
       M300-READ.                                                       <LA3425>
                                                                        <LA3425>
           MOVE SPACES                 TO LIFELNB-PARAMS.               <LA3425>
           MOVE WSSP-COMPANY           TO LIFELNB-CHDRCOY.              <LA3425>
           MOVE CHDRLNB-CHDRNUM        TO LIFELNB-CHDRNUM.              <LA3425>
           MOVE COVTCSN-LIFE           TO LIFELNB-LIFE.                 <LA3425>
           MOVE WSAA-JLIFE             TO LIFELNB-JLIFE.                <LA3425>
           MOVE READR                  TO LIFELNB-FUNCTION.             <LA3425>
                                                                        <LA3425>
           CALL 'LIFELNBIO'            USING LIFELNB-PARAMS.            <LA3425>
                                                                        <LA3425>
           IF LIFELNB-STATUZ           NOT = O-K                        <LA3425>
           AND LIFELNB-STATUZ          NOT = MRNF                       <LA3425>
               MOVE LIFELNB-PARAMS     TO SYSR-PARAMS                   <LA3425>
               MOVE LIFELNB-STATUZ     TO SYSR-STATUZ                   <LA3425>
               PERFORM 600-FATAL-ERROR                                  <LA3425>
           END-IF.                                                      <LA3425>
                                                                        <LA3425>
       M390-EXIT.                                                       <LA3425>
           EXIT.                                                        <LA3425>
      /
      *****************************************************************
      *     DECIDE WHICH TRANSACTION PROGRAM IS NEXT
      *****************************************************************
      *
       4000-WHERE-NEXT SECTION.
      *************************
      *
       4010-NEXT-PROGRAM.
      *

           ADD 1 TO WSSP-PROGRAM-PTR.
       4090-EXIT.
            EXIT.
      /                                                                 <FUPLET>
       6000-WRITE-LETTER SECTION.                                       <FUPLET>
      ***************************                                       <FUPLET>
      *                                                                 <FUPLET>
       6010-READ-T6634.                                                 <FUPLET>
      *                                                                 <FUPLET>
      *  Get the Letter type from T6634.                               <FUPLET>
      *                                                                <FUPLET>
           MOVE SPACES                     TO ITEM-DATA-AREA.           <FUPLET>
           MOVE 'IT'                       TO ITEM-ITEMPFX.             <FUPLET>
           MOVE CHDRLNB-CHDRCOY            TO ITEM-ITEMCOY.             <FUPLET>
      **** MOVE T6634                      TO ITEM-ITEMTABL.    <PCPPRT><FUPLET>
           MOVE TR384                      TO ITEM-ITEMTABL.            <PCPPRT>
      *                                                                <FUPLET>
      *  Build key to T6634 from contract type & transaction cod       <FUPLET>
      *                                                                <FUPLET>
           STRING                          CHDRLNB-CNTTYPE              <FUPLET>
                                           WSKY-BATC-BATCTRCDE          <FUPLET>
           DELIMITED BY                    SPACE                        <FUPLET>
           INTO                            ITEM-ITEMITEM.               <FUPLET>
      *                                                                <FUPLET>
           MOVE READR                      TO ITEM-FUNCTION.            <FUPLET>
      *                                                                <FUPLET>
           CALL 'ITEMIO' USING ITEM-PARAMS.                             <FUPLET>
      *                                                                <FUPLET>
      **** IF  ITEM-STATUZ             NOT = O-K                <FUPLET><PCPPRT>
           IF  ITEM-STATUZ             NOT = O-K  AND  MRNF             <PCPPRT>
               MOVE ITEM-PARAMS            TO SYSR-PARAMS               <FUPLET>
               PERFORM 600-FATAL-ERROR.                                 <FUPLET>

           IF ITEM-STATUZ              = MRNF                           <PCPPRT>
              STRING '***'                                              <PCPPRT>
                     WSKY-BATC-BATCTRCDE                                <PCPPRT>
                     DELIMITED BY SPACE                                 <PCPPRT>
                          INTO ITEM-ITEMITEM                            <PCPPRT>
                                                                        <PCPPRT>
              MOVE READR               TO ITEM-FUNCTION                 <PCPPRT>
              CALL 'ITEMIO'         USING ITEM-PARAMS                   <PCPPRT>
              IF  ITEM-STATUZ       NOT = O-K  AND  MRNF                <PCPPRT>
                  MOVE ITEM-STATUZ     TO SYSR-STATUZ                   <PCPPRT>
                  MOVE ITEM-PARAMS     TO SYSR-PARAMS                   <PCPPRT>
                  PERFORM 600-FATAL-ERROR                               <PCPPRT>
              END-IF                                                    <PCPPRT>
              IF  ITEM-STATUZ           = MRNF                          <PCPPRT>
                  MOVE SPACES          TO ITEM-GENAREA                  <PCPPRT>
              END-IF                                                    <PCPPRT>
           END-IF.                                                      <PCPPRT>

      *                                                                <FUPLET>
      **** MOVE ITEM-GENAREA               TO T6634-T6634-REC.  <PCPPRT><FUPLET>
           MOVE ITEM-GENAREA               TO TR384-TR384-REC.          <PCPPRT>
      *                                                                <FUPLET>
      **** IF  T6634-LETTER-TYPE           = SPACES             <PCPPRT><FUPLET>
           IF  TR384-LETTER-TYPE           = SPACES                     <PCPPRT>
               GO TO 6099-EXIT.                                         <FUPLET>
      *                                                                <FUPLET>
      *  Write LETC via LETRQST and update the seqno by 1              <FUPLET>
      *                                                                <FUPLET>
           MOVE SPACES                 TO LETRQST-STATUZ.               <FUPLET>
           MOVE CHDRLNB-CHDRCOY        TO LETRQST-REQUEST-COMPANY.      <FUPLET>
      **** MOVE T6634-LETTER-TYPE      TO LETRQST-LETTER-TYPE.  <PCPPRT><FUPLET>
           MOVE TR384-LETTER-TYPE      TO LETRQST-LETTER-TYPE.          <PCPPRT>
           MOVE CHDRLNB-COWNCOY        TO LETRQST-CLNTCOY.              <FUPLET>
           MOVE CHDRLNB-COWNNUM        TO LETRQST-CLNTNUM.              <FUPLET>
           MOVE WSAA-TODAY             TO LETRQST-LETTER-REQUEST-DATE.  <FUPLET>
           MOVE 'CH'                   TO LETRQST-RDOCPFX.              <FUPLET>
           MOVE CHDRLNB-CHDRCOY        TO LETRQST-RDOCCOY.              <FUPLET>
           MOVE CHDRLNB-CHDRCOY        TO LETRQST-CHDRCOY.              <LA3383>
           MOVE CHDRLNB-CHDRNUM        TO LETRQST-CHDRNUM.              <LA3383>
           MOVE CHDRLNB-CHDRNUM        TO LETRQST-RDOCNUM.              <FUPLET>
           MOVE CHDRLNB-TRANNO         TO LETRQST-TRANNO.               <FUPLET>
           MOVE WSSP-BRANCH            TO LETRQST-BRANCH.               <PCPPRT>
           MOVE CHDRLNB-DESPNUM        TO LETRQST-DESPNUM.              <V71L11>
           MOVE WSKY-BATC-BATCTRCDE    TO LETRQST-TRCDE.                <V71L11>
           MOVE WSKY-BATC-BATCTRCDE    TO LETRQST-OTHER-KEYS.           <FUPLET>
           STRING                          CHDRLNB-CHDRNUM              <FUPLET>
                                           WSKY-BATC-BATCTRCDE          <FUPLET>
                                           WSSP-LANGUAGE
           DELIMITED BY                    SPACE                        <FUPLET>
           INTO                            LETRQST-OTHER-KEYS.          <FUPLET>
                                                                        <FUPLET>
           MOVE 'ADD'                  TO LETRQST-FUNCTION.             <FUPLET>
      *                                                                <FUPLET>
      **** IF  T6634-LETTER-TYPE       NOT = SPACES             <PCPPRT><FUPLET>
           IF  TR384-LETTER-TYPE       NOT = SPACES                     <PCPPRT>
      *                                                                <FUPLET>
               CALL 'LETRQST' USING LETRQST-PARAMS                      <FUPLET>
      *                                                                <FUPLET>
               IF  LETRQST-STATUZ          NOT = O-K                    <FUPLET>
                   MOVE LETRQST-PARAMS     TO SYSR-PARAMS               <FUPLET>
                   MOVE LETRQST-STATUZ     TO SYSR-STATUZ               <FUPLET>
                   PERFORM 600-FATAL-ERROR                              <FUPLET>
               END-IF                                                   <FUPLET>
           END-IF.                                                      <FUPLET>
                                                                        <FUPLET>
       6099-EXIT.                                                       <FUPLET>
           EXIT.                                                        <FUPLET>
