      * Generation Parameters SCRVER(02)               Do Not Delete!   <S9503>
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P6236.
      *
      * Copyright 1986-2020, Computer Sciences Corporation.
      *
      *REMARKS.
      *
      * Initialise
      * ----------
      *
      *     Skip this section  if  returning  from  an optional selection
      *     (current stack position action flag = '*').
      *
      *     Clear the subfile ready for loading.
      *
      *     The details of the  contract  being  enquired  upon  will  be
      *     stored in the CHDRENQ  I/O  module.  Retrieve the details and
      *     set up the header portion of the screen.
      *
      *     Look up the following descriptions and names:
      *
      *          Contract Type, (CNTTYPE) - long description from T5688,
      *
      *          Contract Status,  (STATCODE)  -  short  description from
      *          T3623,
      *
      *          Premium Status,  (PSTATCODE)  -  short  description from
      *          T3588,
      *
      *          Servicing branch, - long description from T1692,
      *
      *          The owner's client (CLTS) details.
      *
      *          The joint owner's  client  (CLTS) details if they exist.
      *
      *
      *     The details of  the  transaction  selected  from the previous
      *     screen will be in  either  the  ACBLENQ  I/O  module  or  the
      *     SACSENQ I/O module. One  module  will  contain  a  record the
      *     other will not. By performing  a  RETRV  on  both  it will be
      *     possible to determine which data-set to read for the data for
      *     this function. If an ACBL  record  is found then ACMV will be
      *     used, if a SACS record is found then RTRN will be used.
      *
      *     Display the SACSCODE and its short description from T3616 and
      *     SACSTYP and its short  description  from  T3695 in the header
      *     portion of the screen.
      *
      *     The  details  that will  be  displayed  will  come  from  two
      *     data-sets:  ACMVSAC and  RTRNSAC.  The  same information will
      *     be extracted from both  data-sets  and they will both be read
      *     in the same sequence.
      *
      *     This program should  process all the relevant ACMVSAC records
      *     OR all the relevant RTRNSAC records.
      *
      *     If an ACBLENQ record  was  found set the Original Currency in
      *     the screen header  from ACBLENQ-ORIGCURR and load the subfile
      *     as follows:
      *
      *          Set the Accounting  Currency  in  the screen header from
      *          ACMVSAC-GENLCUR.
      *
      *          From  the  ACBLENQ   record  take  the  fields  RLDGCOY,
      *          SACSCODE,  RLDGACCT,   SACSTYP  and  ORIGCURR  and  read
      *          ACMVSAC with this key  until  end of file or any portion
      *          of the key, (apart from the ORIGCURR), changes.
      *
      *          Display the Effective Date (EFFDATE), Transaction number
      *          (TRANNO), G/L  code  (GLCODE), Original Amount (ORIGAMT)
      *          and Accounting Amount (ACCTAMT).
      *
      *     If a SACSENQ record  was  found  set the Original Currency in
      *     the screen header  from  SACSENQ-CNTCURR and load the subfile
      *     as follows:
      *
      *          Set the Accounting  Currency  in  the screen header from
      *          RTRNSAC-ACCTCCY.
      *
      *          From  the  SACSENQ   record  take  the  fields  CHDRCOY,
      *          SACSCODE, CHDRNUM, SACSTYP  and CNTCURR and read RTRNSAC
      *          with this key until end  of  file  or any portion of the
      *          key, (apart from the CNTCURR), changes.
      *
      *          Display the Effective Date (EFFDATE), Transaction number
      *          (TRANNO), G/L  code  (GLCODE), Original Amount (ORIGAMT)
      *          and Accounting Amount (ACCTAMT).
      *
      *
      *     Load all pages required  in  the  subfile and set the subfile
      *     more indicator to no.
      *
      * Validation
      * ----------
      *
      *     There is no validation required.
      *
      *
      * Updating
      * --------
      *
      *     There is no updating in this program.
      *
      *
      * Next Program
      * ------------
      *
      *     For "CF11" or "Enter"  continue  by adding one to the program
      *     pointer.
      *
      *
      *
      * Notes.
      * ------
      *
      *     Create a new view  of  ACMVPF  which  uses  only those fields
      *     required for this  program  and  keyed  on Company (RLDGCOY),
      *     SACSCODE, Contract  Number  (RLDGACCT),  SACSTYP and Original
      *     Currency (ORIGCURR).
      *
      *     Create a new view  of  RTRNPF  which  uses  only those fields
      *     required for this  program  and  keyed  on Company (RLDGCOY),
      *     SACSCODE, Contract  Number  (RLDGACCT),  SACSTYP and Original
      *     Currency (CNTCURR).
      *
      *
      *     Tables Used:
      *
      * T1688 - Branch Codes                      Key: Transaction Code
      * T1692 - Branch Codes                      Key: Branch Code
      * T3588 - Contract Premium Status           Key: PSTATCODE
      * T3616 - Sub-Account Code                  Key: SACSCODE
      * T3623 - Contract Risk Status              Key: STATCODE
      * T3695 - Sub-Account Type                  Key: SACSTYP
      * T5688 - Contract Structure                Key: CNTTYPE
      *
      *
      *****************************************************************
      *              AMENDMENT  HISTORY                               *
      *****************************************************************
      * DATE.....   BY..   AMENDMENT...............  NUMBER
      *
      * DD/MM/YY    X.X.   xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  NNN
      *
      * 11/04/91    N.F.   SDF1249.                                     <001>
      *                    Do not display Sub A/C accounting entry for  <001>
      *                    Requisitions unless they have been processed <001>
      *                    (I.E. Ignore RTRN's with BATCTRCDE of T206). <001>
      *
      * 22/07/91    I.W.   No longer use the SACS file for
      *                    displaying any details......             002
      *
      * 02/08/91    I.W.   If there are not any ACMV for the ACBL
      *                    then read the RTRN and display these
      *                    Details (An addition to 002).........    002
      *
      * 05/08/91    J.L     (SDF 1782)                              004
      *                     Initialise WSSP-LIFEKEY when scrn-statuz
      *                     = MASM.
      *
      * 25/10/91    J.L.   Add Entity, Crtable and Descrip fields   005
      *                    to screen. Only display Crtable and
      *                    Description if a component is selected.
      *
      * 23/03/92    J.L.   Change params on read of coverage from   006
      *                    CHDRNUM to RLDGACCT key.
      *
      * 25/08/93    S.T.   AQR 4668.                                007
      *                    Hard coding of 'NONE' removed when joint
      *                    life not found.
      *
      * 20/01/94    N.W.   AQR 4951                                 008
      *                    The ACCTCCY field is being removed from
      *                    the RTRN physical file, all RTRN logical
      *                    files and thus all programs that use RTRN
      *                    records. Thus remove all references to the
      *                    ACCTCCY field in this program that are
      *                    associated with RTRNs.
      *                    ACCTCCY has been REPLACED by GENLCUR in
      *                    RTRNSAC logical.
      *                    UPDATE SOURCE & OBJECT COMPUTER.
      *
      * 04/10/94    R.S.   AQR 5452.                                009
      *                    Changed the screen SFLSIZ from 14 to 13
      *                    and the SFLPAG from 13 to 12. Program
      *                    unchanged.
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
      * 30/01/96  01/01   D96NUM       Jon Simpson                          *
      * 05/02/96  01/01   D96NUM       Rob Yates                            *
      *                   DPAMT (DEFER-PERD-AMT)                            *
      *                   TMBEN (TOT-MTHLY-BENEFIT)                         *
      *                   STSMIN (STAT-SUMINS)                              *
      *                   VARSI  (VAR-SUM-INSURED)                          *
      *                   CRINST (CR-INSTAMT)                               *
      *                   SUMINS                                            *
      *                   EMV    (EST-MAT-VALUE)                            *
      *                   EMVINT (EST-MAT-INT)                              *
      *                   INSTPREM                                          *
      *                   SINGP                                             *
      *                   CRDEBT (COVERAGE-DEBT)                            *
      *                   ORIGAMT                                           *
      *                   ACCTAMT                                           *
      *                   SINSTAMT                                          *
      *                   ISAM                                              *
      *                                                                     *
      *                   The above field(s) have been increased in         *
      *                   size as part of the 1996 Numeric Field            *
      *                   Increase project. This is to enable users         *
      *                   to then convert (if they should wish to do        *
      *                   so) the fields to 'Monetary' and use the          *
      *                   SMART Variable Decimal Places technology          *
      *                   accordingly.                                      *
      *                                                                     *
      * 30/11/97    DUNC  SMART 9503 Conv for Client/Server.        <S9503>
      *                                                                     *
      * 08/10/99  01/01   V5L001       Josephine Chiam                      *
      *           RECOMPILED                                           ??   *
      *                                                                     *
      * 05/12/03  01/01   GBF007       Steve Hale                           *
      *           Modifications for Graphical Installation.            ??   *
      *           Hidden fields HACCCURR, HCURRCD, TRCODE added to          *
      *           screen.                                                   *
      *                                                                     *
      * 11/05/05  01/01   D05GB1       Steve Hale                           *
      *           Only MOVE 1 to the subfile RRN when not coming from a     *
      *           remote device so that a calling BO can get at the         *
      *           size of the subfile by using the RRN.                     *
      *                                                                     *
      * 20/08/07  01/01   V72L08       Xu Chen/ASIA/CSC (Beijing)           *
      *           Extend sub-account posting screen to display contract     *
      *           related Receipt,Payment and Direct Journal number.        *
      *                                                                     *
      * 06/03/08  01/01   ARCH01       Nath Mathmaluwe/FSG/CSC (Singa       *
      *           Re-Compile only                                      ??   *
      *                                                                     *
      * 15/05/20  01/01   CS020        Van Bao Tuyen - IT                   *
      *           Sort premium LP/S by date effect.                         *
      *                                                                     *
      * 16/10/20  01/01   PS070        Mai Yen Phi - IT                     *
      *           Commas displayed - change field HMHII -> LIAOA            *
      *                                           HMHLI -> LIAOP            *
      *                                                                     *
      **DD/MM/YY*************************************************************
      *****************************************************************
      *
      /
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
   ****SOURCE-COMPUTER. IBM-S38.                                        <008>
   ****OBJECT-COMPUTER. IBM-S38.                                        <008>
       SOURCE-COMPUTER. IBM-AS400.                                      <008>
       OBJECT-COMPUTER. IBM-AS400.                                      <008>
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       01  WSAA-PROG                   PIC X(05) VALUE 'P6236'.
       01  WSAA-VERSION                PIC X(02) VALUE '01'.
       01  WSAA-RLDGACCT.                                               <005>
         03 WSAA-CHDRNUM               PIC X(08).                       <005>
      *  03 WSAA-COMPONENT             PIC X(08).                  <005><006>
         03 WSAA-COMPONENT.                                             <006>
           05 WSAA-LIFE                PIC X(02).                       <006>
           05 WSAA-COVERAGE            PIC X(02).                       <006>
           05 WSAA-RIDER               PIC X(02).                       <006>
           05 WSAA-PLANSUFF            PIC X(02).                       <006>
      *
       01  WSAA-BATCKEY.
           COPY BATCKEY.
      *                                                                 <V72L08>
       01  WSAA-WSSPS.                                                  <V72L08>
           03  WSAA-FLAG               PIC X(01).                       <V72L08>
           03  WSAA-SUBMENU            PIC X(05).                       <V72L08>
           03  WSAA-NEXTPROG           PIC X(05).                       <V72L08>
           03  WSAA-SECTIONNO          PIC X(04).                       <V72L08>
           03  WSAA-PROGRAM-PTR        PIC S9(03) COMP-3.               <V72L08>
           03  WSAA-SEC-SWITCHING.                                      <V72L08>
               05  WSAA-SEC-ACTNS.                                      <V72L08>
                   07  WSAA-SEC-ACTN   PIC X(01) OCCURS 22.             <V72L08>
               05  WSAA-SEC-PROGS.                                      <V72L08>
                   07  WSAA-SEC-PROG   PIC X(05) OCCURS 22.             <V72L08>
           03  WSAA-GENVKEY            PIC X(22).                       <V72L08>
           03  WSAA-DOCTKEY            PIC X(12).                       <V72L08>
           03  WSAA-ALOCKEY            PIC X(16).                       <V72L08>
           03  WSAA-ACUMKEY            PIC X(41).                       <V72L08>
           03  WSAA-RGFORMAT           PIC X(17).                       <V72L08>
           03  WSAA-RGDETAIL           PIC X(19).                       <V72L08>
           03  WSAA-RGREPORT           PIC X(15).                       <V72L08>
           03  WSAA-BATCHKEY           PIC X(24).                       <V72L08>
      *                                                                 <V72L08>
       01  WSAA-REC-PROG               PIC X(07) VALUE 'P2067BO'.       <V72L08>
       01  WSAA-PAY-PROG               PIC X(07) VALUE 'P2201BO'.       <V72L08>
       01  WSAA-JRN-PROG               PIC X(07) VALUE 'P2629BO'.       <V72L08>
       01  WSAA-BOPROG                 PIC X(10).                       <V72L08>
       01  WSAA-COUNT                  PIC 9(02).                       <V72L08>
       01  WSAA-SELECT                 PIC 9(01).                       <V72L08>
       01  WSAA-FIRST-TIME             PIC X(01) VALUE 'Y'.             <V72L08>
       01  WSAA-FOUND-SELECTION        PIC X(01) VALUE 'N'.             <V72L08>
           88   FOUND-SELECTION        VALUE 'Y'.                       <V72L08>
      *                                                                 <V72L08>
       01  WSAA-JRN-KEY.                                                <V72L08>
           03  WSAA-JRN-JOURNAL        PIC  X(08).                      <V72L08>
           03  WSAA-JRN-DATE           PIC  S9(08).                     <V72L08>
           03  WSAA-JRN-ACTION         PIC  X(01) VALUE 'B'.            <V72L08>
       01  WSAA-REC-KEY.                                                <V72L08>
           03  WSAA-REC-BANKCODE       PIC  X(02).                      <V72L08>
           03  WSAA-REC-RECEIPT        PIC  X(08).                      <V72L08>
           03  WSAA-REC-CHEQUE         PIC  X(09) VALUE SPACES.         <V72L08>
           03  WSAA-REC-ACTION         PIC  X(01) VALUE 'C'.            <V72L08>
       01  WSAA-PAY-KEY.                                                <V72L08>
           03  WSAA-PAY-METHOD         PIC  X(01) VALUE SPACES.         <V72L08>
           03  WSAA-PAY-PAYEE          PIC  X(10) VALUE SPACES.         <V72L08>
           03  WSAA-PAY-PAYNME         PIC  X(10) VALUE SPACES.         <V72L08>
           03  WSAA-PAY-REQNNO         PIC  X(09).                      <V72L08>
           03  WSAA-PAY-BANKCODE       PIC  X(02) VALUE SPACES.         <V72L08>
           03  WSAA-PAY-ACTION         PIC  X(01) VALUE 'E'.            <V72L08>
                                                                        <V72L08>
       01  WSAA-GLKEY.                                                  <V72L08>
           05  FILLER                  PIC XX.                          <V72L08>
           05  WSAA-RLDGCOY            PIC X.                           <V72L08>
           05  WSAA-RLDGCURR           PIC XXX.                         <V72L08>
           05  WSAA-GLCODE             PIC X(14).                       <V72L08>
                                                                        <V72L08>
       01  WSAA-SBMACTION              PIC  X(01).                      <V72L08>

       01  ERRORS.
           03  E005                    PIC X(04) VALUE 'E005'.
           03  E040                    PIC X(04) VALUE 'E040'.
           03  G620                    PIC X(04) VALUE 'G620'.
      *
       01  TABLES.
           03  T1688                   PIC X(05) VALUE 'T1688'.
           03  T1692                   PIC X(05) VALUE 'T1692'.
           03  T3588                   PIC X(05) VALUE 'T3588'.
           03  T3616                   PIC X(05) VALUE 'T3616'.
           03  T3623                   PIC X(05) VALUE 'T3623'.
           03  T3695                   PIC X(05) VALUE 'T3695'.
           03  T5645                   PIC X(05) VALUE 'T5645'.
           03  T5687                   PIC X(05) VALUE 'T5687'.         <005>
           03  T5688                   PIC X(05) VALUE 'T5688'.
      *
       01  FORMATS.
           03  CHDRENQREC              PIC X(10) VALUE 'CHDRENQREC'.
           03  LIFEENQREC              PIC X(10) VALUE 'LIFEENQREC'.
           03  ACBLENQREC              PIC X(10) VALUE 'ACBLENQREC'.
      *****03  SACSENQREC              PIC X(10) VALUE 'SACSENQREC'.    <002>
           03  ACMVSACREC              PIC X(10) VALUE 'ACMVSACREC'.
           03  RTRNSACREC              PIC X(10) VALUE 'RTRNSACREC'.
           03  CLTSREC                 PIC X(10) VALUE 'CLTSREC'.
           03  DESCREC                 PIC X(10) VALUE 'DESCREC'.
           03  ITEMREC                 PIC X(10) VALUE 'ITEMREC'.
           03  COVRREC                 PIC X(10) VALUE 'COVRREC'.       <005>
           03  RDOCREC                 PIC X(10) VALUE 'RDOCREC'.       <V72L08>
           03  ACMVRCPREC              PIC X(10) VALUE 'ACMVRCPREC'.    <CS020>
           03  RTRNRCPREC              PIC X(10) VALUE 'RTRNRCPREC'.    <CS020>
      /
           COPY CHDRENQSKM.
      /
           COPY LIFEENQSKM.
      /
           COPY ACBLENQSKM.
      /
      *****COPY SACSENQSKM.                                             <002>
      /
           COPY ACMVSACSKM.
      /                                                                 <CS020>
           COPY RTRNRCPSKM.                                             <CS020>
      /                                                                 <CS020>
           COPY ACMVRCPSKM.                                             <CS020>
      /
           COPY RTRNSACSKM.
      /
           COPY CLTSSKM.
      /
           COPY DESCSKM.
      /
           COPY RDOCSKM.                                                <V72L08>
      /                                                                 <V72L08>
           COPY COVRSKM.                                                <005>
      /                                                                 <005>
           COPY VARCOM.
      *
           COPY SYSERRREC.
      *
           COPY OPSTATSREC.
      *
           COPY FSUPFXCPY.                                              <V72L08>
      *                                                                 <V72L08>
           COPY DATCON1REC.                                             <V72L08>
      *                                                                 <V72L08>
           COPY ACCINQREC.                                              <V72L08>
      *                                                                 <V72L08>
           COPY OPTSWCHREC.                                             <V72L08>
      *                                                                 <V72L08>
           COPY BOWSCPY.                                                <V72L08>
      *                                                                 <V72L08>
           COPY SESSIONO.                                               <V72L08>
      *                                                                 <V72L08>
           COPY LDRHDR.                                                 <V72L08>
      *                                                                 <V72L08>
           COPY LDRHDR                                                  <V72L08>
                      REPLACING LEADER-HEADER BY WSAA-LDRHDR.           <V72L08>
           COPY MSGDTA                                                  <V72L08>
                       REPLACING MESSAGE-DATA  BY WSAA-REQUEST.         <V72L08>
           COPY MSGDTA                                                  <V72L08>
                       REPLACING MESSAGE-DATA  BY REQUEST-DATA.         <V72L08>
           COPY MSGDTA                                                  <V72L08>
                       REPLACING MESSAGE-DATA  BY WSAA-RESPONSE.        <V72L08>
           COPY MSGDTA                                                  <V72L08>
                       REPLACING MESSAGE-DATA BY RESPONSE-DATA.         <V72L08>
      *                                                                 <V72L08>
           COPY WSSPLIFE                                                <V72L08>
                        REPLACING WSSP-USER-AREA BY L-WSSP-USER-AREA.   <V72L08>
      ***  COPY SCRNPARAMS.                                             <S9503>
      /
      ***  COPY S6236SCR.                                               <S9503>
      /
       LINKAGE SECTION.
      * Screen copybooks are now part of the linkage.                   <S9503>
      /                                                                 <S9503>
           COPY SCRNPARAMS.                                             <S9503>
      /                                                                 <S9503>
           COPY S6236SCR.                                               <S9503>

           COPY WSSPCOMN.

      *    COPY WSSPLIFE.                                               <V72L08>
      *                                                                 <V72L08>
           COPY WSSPLEDG.                                               <V72L08>
      /
      * Statement now includes screen copybooks.                        <S9503>
       PROCEDURE DIVISION USING WSSP-COMMON-AREA WSSP-USER-AREA         <S9503>
                                               SCRN-SCREEN-PARAMS       <S9503>
                                               S6236-DATA-AREA          <S9503>
                                               S6236-SUBFILE-AREA   .   <S9503>

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
      *************************                                         <003>
       1000-INITIALISE SECTION.
      *************************
       1010-INITIALISE.

           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'
                GO TO 1090-EXIT.

           MOVE WSSP-SBMACTION         TO WSAA-SBMACTION.               <V72L08>
           MOVE SPACES                 TO WSAA-RLDGACCT.
           MOVE WSAA-PROG              TO SYSR-SUBRNAME.
           MOVE WSSP-BATCHKEY          TO WSAA-BATCKEY.
           MOVE SPACES                 TO S6236-DATA-AREA.
           MOVE SPACES                 TO S6236-SUBFILE-AREA.
      **** MOVE ZEROES                 TO S6236-ACCTAMT                 <V72L08>
      ****                                S6236-ORIGAMT.                <V72L08>
      **** MOVE ZEROES                 TO S6236-HMHII           <PS070> <V72L08>
      ****                                S6236-HMHLI.          <PS070> <V72L08>
           MOVE ZEROES                 TO S6236-LIAOA                   <PS070>
                                          S6236-LIAOP.                  <PS070>
           MOVE SPACES                 TO S6236-SELECT.                 <V72L08>

           MOVE SCLR                   TO SCRN-FUNCTION.
           CALL 'S6236IO' USING SCRN-SCREEN-PARAMS
                                S6236-DATA-AREA
                                S6236-SUBFILE-AREA.
           IF SCRN-STATUZ NOT = O-K
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.
           MOVE 1                      TO SCRN-SUBFILE-RRN.

           PERFORM 1050-SCREEN-INIT-OPTSWCH.                            <V72L08>
      *
      * Read CHDRENQ (RETRV)  in  order to obtain the contract header
      * information.  If  the  number of policies in the plan is zero
      * or  one  then Plan-processing does not apply. If there is any
      * other  numeric  value,  this  value  indicates  the number of
      * policies in the Plan.
      *
           MOVE 'RETRV'                TO CHDRENQ-FUNCTION.
           CALL 'CHDRENQIO'         USING CHDRENQ-PARAMS.
      *
           IF CHDRENQ-STATUZ          NOT = O-K
               MOVE CHDRENQ-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
           MOVE CHDRENQ-CHDRNUM        TO S6236-CHDRNUM.
           MOVE CHDRENQ-CNTTYPE        TO S6236-CNTTYPE.
           MOVE CHDRENQ-CNTCURR        TO S6236-CNTCURR.
           MOVE CHDRENQ-REGISTER       TO S6236-REGISTER.
      *
      * Obtain the Life Assured and Joint Life Assured, if they exist.
      * The BEGN function is used to retrieve the first Life for the
      * contract in case life '01' has been deleted.
      *
           MOVE CHDRENQ-CHDRCOY        TO LIFEENQ-CHDRCOY.
           MOVE CHDRENQ-CHDRNUM        TO LIFEENQ-CHDRNUM.
           MOVE '01'                   TO LIFEENQ-LIFE.
           MOVE '00'                   TO LIFEENQ-JLIFE.
           MOVE BEGN                   TO LIFEENQ-FUNCTION.
           CALL 'LIFEENQIO'            USING LIFEENQ-PARAMS.
           IF LIFEENQ-STATUZ           NOT = O-K
                MOVE LIFEENQ-PARAMS    TO SYSR-PARAMS
                PERFORM 600-FATAL-ERROR.
      *
           MOVE LIFEENQ-LIFCNUM        TO S6236-LIFENUM
                                          CLTS-CLNTNUM.
           MOVE WSSP-FSUCO             TO CLTS-CLNTCOY.
           MOVE 'CN'                   TO CLTS-CLNTPFX.
           MOVE READR                  TO CLTS-FUNCTION.
           CALL 'CLTSIO'               USING CLTS-PARAMS.
           IF CLTS-STATUZ              NOT = O-K
                MOVE CLTS-PARAMS       TO SYSR-PARAMS
                PERFORM 600-FATAL-ERROR.
      *
           PERFORM PLAINNAME.
           MOVE WSSP-LONGCONFNAME      TO S6236-LIFENAME.
      *
      * Check for the existence of Joint Life details.
      *
           MOVE '01'                   TO LIFEENQ-JLIFE.
           MOVE READR                  TO LIFEENQ-FUNCTION.
           CALL 'LIFEENQIO'            USING LIFEENQ-PARAMS.
      ***  IF   LIFEENQ-STATUZ         NOT = O-K                        <007>
      ***       MOVE 'NONE'            TO S6236-JLIFE                   <007>
      ***                                 S6236-JLIFENAME               <007>
      ***  ELSE                                                         <007>
           IF   LIFEENQ-STATUZ             = O-K                        <007>
                MOVE LIFEENQ-LIFCNUM   TO S6236-JLIFE
                                          CLTS-CLNTNUM
                MOVE WSSP-FSUCO        TO CLTS-CLNTCOY
                MOVE 'CN'              TO CLTS-CLNTPFX
                MOVE READR             TO CLTS-FUNCTION
                CALL 'CLTSIO'          USING CLTS-PARAMS
                IF CLTS-STATUZ         NOT = O-K
                     MOVE CLTS-PARAMS  TO SYSR-PARAMS
                     PERFORM 600-FATAL-ERROR
                ELSE
                     PERFORM PLAINNAME
                     MOVE WSSP-LONGCONFNAME
                                       TO S6236-JLIFENAME.
      *
      *    Obtain the Contract Type description from T5688.
      *
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
                MOVE ALL '?'           TO S6236-CTYPEDES
           ELSE
                MOVE DESC-LONGDESC     TO S6236-CTYPEDES.
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
                MOVE ALL '?'           TO S6236-CHDRSTATUS
           ELSE
                MOVE DESC-SHORTDESC    TO S6236-CHDRSTATUS.
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
                MOVE ALL '?'           TO S6236-PREMSTATUS
           ELSE
                MOVE DESC-SHORTDESC    TO S6236-PREMSTATUS.
      *                                                                 <005>
      *    Read the stored ACBLENQ record for the contract.
      *    If details are found to have been stored, then ACMVSAC
      *    will be used for this enquiry, otherwise RTRNSAC will be
      *    used.
      *
           MOVE SPACES                 TO ACBLENQ-DATA-AREA.
           MOVE RETRV                  TO ACBLENQ-FUNCTION.
           CALL 'ACBLENQIO'         USING ACBLENQ-PARAMS.
           IF   ACBLENQ-STATUZ      NOT = O-K
            AND ACBLENQ-STATUZ      NOT = MRNF
                MOVE ACBLENQ-PARAMS    TO SYSR-PARAMS
                PERFORM 600-FATAL-ERROR.
      *                                                                 <005>
      * Get Coverage/Rider name from Coverage file.                     <005>
      *                                                                 <005>
           IF ACBLENQ-STATUZ        NOT = O-K                           <005>
              GO                       TO 1050-CONTINUE.                <005>
      *                                                                 <005>
           MOVE ACBLENQ-RLDGACCT       TO WSAA-RLDGACCT.                <005>
      *                                                                 <005>
      *****IF WSAA-COMPONENT           = SPACES                    <005><006>
           IF WSAA-COVERAGE            = SPACES                         <006>
              GO                       TO 1050-CONTINUE.                <005>
      *                                                                 <006>
           MOVE SPACE                  TO COVR-DATA-KEY.                <005>
           MOVE CHDRENQ-CHDRNUM        TO COVR-CHDRNUM.                 <005>
           MOVE CHDRENQ-CHDRCOY        TO COVR-CHDRCOY.                 <005>
           MOVE WSAA-LIFE              TO COVR-LIFE.                    <006>
           MOVE WSAA-COVERAGE          TO COVR-COVERAGE.                <006>
           MOVE WSAA-RIDER             TO COVR-RIDER.                   <006>
           MOVE WSAA-PLANSUFF          TO COVR-PLAN-SUFFIX.             <006>
      *    MOVE 0                      TO COVR-PLAN-SUFFIX.             <005>
           MOVE COVRREC                TO COVR-FORMAT.                  <005>
           MOVE BEGN                   TO COVR-FUNCTION.                <005>
      *                                                                 <005>
           CALL 'COVRIO' USING COVR-PARAMS.                             <005>
      *                                                                 <005>
           IF (COVR-STATUZ NOT = O-K) AND                               <005>
              (COVR-STATUZ NOT = ENDP)                                  <005>
               MOVE COVR-PARAMS        TO SYSR-PARAMS                   <005>
               MOVE COVR-STATUZ        TO SYSR-STATUZ                   <005>
               PERFORM 600-FATAL-ERROR.                                 <005>
      *                                                                 <005>
           MOVE COVR-CRTABLE           TO S6236-CRTABLE.                <005>
      *                                                                 <005>
      * Get Coverage/Rider Description from T5687.                      <005>
      *                                                                 <005>
           MOVE SPACES                 TO DESC-DATA-KEY.                <005>
           MOVE 'IT'                   TO DESC-DESCPFX.                 <005>
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.                 <005>
           MOVE T5687                  TO DESC-DESCTABL.                <005>
           MOVE COVR-CRTABLE           TO DESC-DESCITEM.                <005>
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.                <005>
           MOVE READR                  TO DESC-FUNCTION.                <005>
      *                                                                 <005>
           CALL 'DESCIO' USING DESC-PARAMS.                             <005>
           IF   DESC-STATUZ            NOT = O-K                        <005>
                                   AND NOT = MRNF                       <005>
                MOVE DESC-PARAMS       TO SYSR-PARAMS                   <005>
                PERFORM 600-FATAL-ERROR.                                <005>
      *                                                                 <005>
           IF   DESC-STATUZ            = MRNF                           <005>
                MOVE ALL '?'           TO S6236-DESCRIP                 <005>
           ELSE                                                         <005>
                MOVE DESC-LONGDESC     TO S6236-DESCRIP.                <005>
      *                                                                 <005>
       1050-CONTINUE.                                                   <005>
      *                                                                 <005>
           IF   ACBLENQ-STATUZ          = O-K
                IF ACBLENQ-SACSCODE = 'LP' AND                          <CS020>
                   ACBLENQ-SACSTYP  = 'S'                               <CS020>
                   PERFORM A1100-PROCESS-ACBL-LPS                       <CS020>
                ELSE                                                    <CS020>
                   PERFORM 1100-PROCESS-ACBL                            <CS020>
                END-IF                                                  <CS020>
           END-IF.                                                      <CS020>
      ****      PERFORM 1100-PROCESS-ACBL.                              <CS020>
      *****     PERFORM 1100-PROCESS-ACBL                               <003>
      *****ELSE                                                         <003>
      *****     PERFORM 1300-PROCESS-SACS.                              <003>

      *                                                                 <D05GB1>
      * Do not move 1 to the sunfile RRN if this is a call from a       <D05GB1>
      * remote device so that the RRN will contain the total number of  <D05GB1>
      * transaction records.                                            <D05GB1>
      *                                                                 <D05GB1>
      **** MOVE 1                      TO SCRN-SUBFILE-RRN.             <D05GB1>
                                                                        <D05GB1>
           IF  SCRN-DEVICE-IND      NOT = '*RMT'                        <D05GB1>
               MOVE 1                  TO SCRN-SUBFILE-RRN              <D05GB1>
           END-IF.                                                      <D05GB1>

       1090-EXIT.
            EXIT.
      /
           COPY CONFNAME.
      /
       1050-SCREEN-INIT-OPTSWCH SECTION.                                <V72L08>
      **********************************                                <V72L08>
       1050-OPTSWCH.                                                    <V72L08>
      *                                                                 <V72L08>
           MOVE 'INIT'                 TO OPTS-FUNCTION.                <V72L08>
           MOVE WSAA-PROG              TO OPTS-CALLING-PROG.            <V72L08>
           MOVE ZEROS                  TO OPTS-DTEEFF.                  <V72L08>
           MOVE WSSP-COMPANY           TO OPTS-COMPANY.                 <V72L08>
           MOVE WSSP-LANGUAGE          TO OPTS-LANGUAGE.                <V72L08>
           MOVE WSSP-TRANID            TO VRCM-TRANID.                  <V72L08>
           MOVE VRCM-USER              TO OPTS-USER.                    <V72L08>
           MOVE 'I'                    TO WSSP-FLAG.                    <V72L08>
                                                                        <V72L08>
           CALL 'OPTSWCH'              USING OPTSWCH-REC                <V72L08>
                                             WSSP-SEC-PROGS             <V72L08>
                                             WSSP-SEC-ACTNS             <V72L08>
                                             WSSP-PROGRAM-PTR           <V72L08>
                                             WSSP-FLAG.                 <V72L08>
           IF OPTS-STATUZ              NOT = O-K                        <V72L08>
              MOVE 'INIT'              TO SYSR-FUNCTION                 <V72L08>
              MOVE OPTS-STATUZ         TO SYSR-DBIO-STATUZ              <V72L08>
                                          SYSR-STATUZ                   <V72L08>
              MOVE 'OPTSWCH'           TO SYSR-IOMOD                    <V72L08>
              PERFORM 600-FATAL-ERROR                                   <V72L08>
           END-IF.                                                      <V72L08>
      *                                                                 <V72L08>
       1050-EXIT.                                                       <V72L08>
           EXIT.                                                        <V72L08>
      /                                                                 <V72L08>
      ***************************                                       <003>
       1100-PROCESS-ACBL SECTION.
      ***************************                                       <003>
      *                                                                 <003>
       1100-PARA.
      *                                                                 <003>
           MOVE ACBLENQ-SACSCODE       TO S6236-SACSCODE.
           MOVE ACBLENQ-SACSTYP        TO S6236-SACSTYP.
           MOVE ACBLENQ-RLDGACCT       TO S6236-ENTITY.                 <005>
      *
      *    Obtain the SACS CODE description from T3616.
      *
           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE 'IT'                   TO DESC-DESCPFX.
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.
           MOVE T3616                  TO DESC-DESCTABL.
           MOVE ACBLENQ-SACSCODE       TO DESC-DESCITEM.
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
                MOVE ALL '?'           TO S6236-SACSCODED
           ELSE
                MOVE DESC-SHORTDESC    TO S6236-SACSCODED.
      *
      *    Obtain the SACS TYPE description from T3695.
      *
           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE 'IT'                   TO DESC-DESCPFX.
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.
           MOVE T3695                  TO DESC-DESCTABL.
           MOVE ACBLENQ-SACSTYP        TO DESC-DESCITEM.
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.
           MOVE READR                  TO DESC-FUNCTION.
      *
           CALL 'DESCIO' USING DESC-PARAMS.
           IF   DESC-STATUZ            NOT = O-K
                                   AND NOT = MRNF
                MOVE DESC-PARAMS       TO SYSR-PARAMS
                PERFORM 600-FATAL-ERROR.

           IF   DESC-STATUZ            = MRNF
                MOVE ALL '?'           TO S6236-SACSTYPD
           ELSE
                MOVE DESC-SHORTDESC    TO S6236-SACSTYPD.

           MOVE ACBLENQ-RLDGCOY        TO ACMVSAC-RLDGCOY.
           MOVE ACBLENQ-SACSCODE       TO ACMVSAC-SACSCODE.
           MOVE ACBLENQ-RLDGACCT       TO ACMVSAC-RLDGACCT.
           MOVE ACBLENQ-SACSTYP        TO ACMVSAC-SACSTYP.
           MOVE ACBLENQ-ORIGCURR       TO ACMVSAC-ORIGCURR
                                          S6236-ORIGCCY.
           MOVE BEGN                   TO ACMVSAC-FUNCTION.
           CALL 'ACMVSACIO'         USING ACMVSAC-PARAMS.
           IF   ACMVSAC-STATUZ      NOT = O-K
            AND ACMVSAC-STATUZ      NOT = ENDP
                MOVE ACMVSAC-PARAMS    TO SYSR-PARAMS
                PERFORM 600-FATAL-ERROR.

           MOVE ACMVSAC-GENLCUR        TO S6236-ACCTCCY.

           PERFORM 1200-PROCESS-ACMVSAC
             UNTIL ACMVSAC-RLDGCOY  NOT = ACBLENQ-RLDGCOY  OR
                   ACMVSAC-SACSCODE NOT = ACBLENQ-SACSCODE OR
                   ACMVSAC-RLDGACCT NOT = ACBLENQ-RLDGACCT OR
                   ACMVSAC-SACSTYP  NOT = ACBLENQ-SACSTYP  OR
                   ACMVSAC-ORIGCURR NOT = ACBLENQ-ORIGCURR OR
                   ACMVSAC-STATUZ       = ENDP.
      *                                                                 <003>
       1150-RTRN-READ.                                                  <003>
      *                                                                 <003>
           MOVE ACBLENQ-RLDGCOY        TO RTRNSAC-RLDGCOY.              <003>
           MOVE ACBLENQ-SACSCODE       TO RTRNSAC-SACSCODE.             <003>
           MOVE ACBLENQ-RLDGACCT       TO RTRNSAC-RLDGACCT.             <003>
           MOVE ACBLENQ-SACSTYP        TO RTRNSAC-SACSTYP.              <003>
           MOVE ACBLENQ-ORIGCURR       TO RTRNSAC-ORIGCCY               <003>
                                          S6236-ORIGCCY.                <003>
           MOVE BEGN                   TO RTRNSAC-FUNCTION.             <003>
           CALL 'RTRNSACIO'         USING RTRNSAC-PARAMS.               <003>
           IF   RTRNSAC-STATUZ      NOT = O-K                           <003>
            AND RTRNSAC-STATUZ      NOT = ENDP                          <003>
                MOVE RTRNSAC-PARAMS    TO SYSR-PARAMS                   <003>
                PERFORM 600-FATAL-ERROR.                                <003>
                                                                        <003>
   ****    MOVE RTRNSAC-ACCTCCY        TO S6236-ACCTCCY.           <003><008>
           MOVE RTRNSAC-GENLCUR        TO S6236-ACCTCCY.                <008>
                                                                        <003>
           PERFORM 1400-PROCESS-RTRNSAC                                 <003>
             UNTIL RTRNSAC-RLDGCOY  NOT = ACBLENQ-RLDGCOY  OR           <003>
                   RTRNSAC-SACSCODE NOT = ACBLENQ-SACSCODE OR           <003>
                   RTRNSAC-RLDGACCT NOT = ACBLENQ-RLDGACCT OR           <003>
                   RTRNSAC-SACSTYP  NOT = ACBLENQ-SACSTYP  OR           <003>
                   RTRNSAC-ORIGCCY  NOT = ACBLENQ-ORIGCURR OR           <003>
                   RTRNSAC-STATUZ       = ENDP.                         <003>
                                                                        <003>
       1190-EXIT.
            EXIT.
      /
      *****************************                                     <003>
       1200-PROCESS-ACMVSAC SECTION.
      *****************************                                     <003>
      *                                                                 <003>
       1200-PARA.
      *                                                                 <003>
           MOVE ACMVSAC-EFFDATE        TO S6236-EFFDATE.
           MOVE ACMVSAC-TRANNO         TO S6236-TRANNO.
           MOVE ACMVSAC-GLCODE         TO S6236-GLCODE.
           IF   ACMVSAC-GLSIGN          = '-'
                COMPUTE ACMVSAC-ORIGAMT = ACMVSAC-ORIGAMT * -1
                COMPUTE ACMVSAC-ACCTAMT = ACMVSAC-ACCTAMT * -1.
      **** MOVE ACMVSAC-ORIGAMT        TO S6236-ORIGAMT.                <V72L08>
      **** MOVE ACMVSAC-ACCTAMT        TO S6236-ACCTAMT.                <V72L08>
      **** MOVE ACMVSAC-ORIGAMT        TO S6236-HMHII.          <PS070> <V72L08>
      **** MOVE ACMVSAC-ACCTAMT        TO S6236-HMHLI.          <PS070> <V72L08>
           MOVE ACMVSAC-ORIGAMT        TO S6236-LIAOA.                  <PS070>
           MOVE ACMVSAC-ACCTAMT        TO S6236-LIAOP.                  <PS070>
           MOVE ACMVSAC-BATCTRCDE      TO S6236-TRCODE                  <GBF007>
           MOVE ACMVSAC-ORIGCURR       TO S6236-HCURRCD.                <GBF007>
           MOVE ACMVSAC-GENLCUR        TO S6236-HACCCURR.               <GBF007>
           IF  ACMVSAC-RDOCPFX      NOT = SPACES                        <V72L08>
           AND ACMVSAC-RDOCPFX      NOT = PRFX-CHDR                     <V72L08>
               MOVE 'N'                TO S6236-SELECT-OUT(PR)          <V72L08>
               MOVE ACMVSAC-RDOCNUM    TO S6236-RDOCNUM                 <V72L08>
               MOVE ACMVSAC-RDOCPFX    TO S6236-STATZ                   <V72L08>
           ELSE                                                         <V72L08>
               MOVE 'Y'                TO S6236-SELECT-OUT(PR)          <V72L08>
               MOVE SPACES             TO S6236-RDOCNUM                 <V72L08>
                                          S6236-STATZ                   <V72L08>
           END-IF.                                                      <V72L08>
      *
      *    Write the subfile record.
      *
           MOVE SADD                   TO SCRN-FUNCTION
           CALL 'S6236IO'           USING SCRN-SCREEN-PARAMS
                                          S6236-DATA-AREA
                                          S6236-SUBFILE-AREA
           IF SCRN-STATUZ           NOT = O-K
                MOVE SCRN-STATUZ       TO SYSR-STATUZ
                PERFORM 600-FATAL-ERROR.
      *
      *    Read the next ACMVSAC record.
      *
           MOVE NEXTR                  TO ACMVSAC-FUNCTION.
           CALL 'ACMVSACIO'         USING ACMVSAC-PARAMS.
           IF   ACMVSAC-STATUZ      NOT = O-K
            AND ACMVSAC-STATUZ      NOT = ENDP
                MOVE ACMVSAC-PARAMS    TO SYSR-PARAMS
                PERFORM 600-FATAL-ERROR.

       1290-EXIT.
            EXIT.
      /
      *****                                                             <002>
      ***************************                                       <003>
      *1300-PROCESS-SACS SECTION.                                       <002>
      ***************************                                       <003>
      *1300-PARA.                                                       <002>
      *****                                                             <002>
      *****Read the stored SACSENQ record for the contract.             <002>
      *****If details are not found then a system error will have       <002>
      *****to be reported.                                              <002>
      *****                                                             <002>
      *****MOVE SPACES                 TO SACSENQ-DATA-AREA.            <002>
      *****MOVE RETRV                  TO SACSENQ-FUNCTION.             <002>
      *****CALL 'SACSENQIO'         USING
      *****IF   SACSENQ-STATUZ      NOT = O-K                           <002>
      *****     MOVE SACSENQ-PARAMS    TO SYSR-PARAMS                   <002>
      *****     PERFORM 600-FATAL-ERROR.                                <002>
      *****                                                             <002>
      *****MOVE SACSENQ-SACSCODE       TO S6236-SACSCODE.               <002>
      *****MOVE SACSENQ-SACSTYP        TO S6236-SACSTYP.                <002>
      *****                                                             <002>
      *****Obtain the SACS CODE description from T3616.                 <002>
      *****                                                             <002>
      *****MOVE SPACES                 TO DESC-DATA-KEY.                <002>
      *****MOVE 'IT'                   TO DESC-DESCPFX.                 <002>
      *****MOVE WSSP-COMPANY           TO DESC-DESCCOY.                 <002>
      *****MOVE T3616                  TO DESC-DESCTABL.                <002>
      *****MOVE SACSENQ-SACSCODE       TO DESC-DESCITEM.                <002>
      *****MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.                <002>
      *****MOVE READR                  TO DESC-FUNCTION.                <002>
      *****                                                             <002>
      *****CALL 'DESCIO' USING DESC-PARAMS.                             <002>
      *****IF   DESC-STATUZ            NOT = O-K                        <002>
      *****                        AND NOT = MRNF                       <002>
      *****     MOVE DESC-PARAMS       TO SYSR-PARAMS                   <002>
      *****     PERFORM 600-FATAL-ERROR.                                <002>
      *****                                                             <002>
      *****IF   DESC-STATUZ            = MRNF                           <002>
      *****     MOVE ALL '?'           TO S6236-SACSCODED               <002>
      *****ELSE                                                         <002>
      *****     MOVE DESC-SHORTDESC    TO S6236-SACSCODED.              <002>
      *****                                                             <002>
      *****Obtain the SACS TYPE description from T3695.                 <002>
      *****                                                             <002>
      *****MOVE SPACES                 TO DESC-DATA-KEY.                <002>
      *****MOVE 'IT'                   TO DESC-DESCPFX.                 <002>
      *****MOVE WSSP-COMPANY           TO DESC-DESCCOY.                 <002>
      *****MOVE T3695                  TO DESC-DESCTABL.                <002>
      *****MOVE SACSENQ-SACSTYP        TO DESC-DESCITEM.                <002>
      *****MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.                <002>
      *****MOVE READR                  TO DESC-FUNCTION.                <002>
      *****                                                             <002>
      *****CALL 'DESCIO' USING DESC-PARAMS.                             <002>
      *****IF   DESC-STATUZ            NOT = O-K                        <002>
      *****                        AND NOT = MRNF                       <002>
      *****     MOVE DESC-PARAMS       TO SYSR-PARAMS                   <002>
      *****     PERFORM 600-FATAL-ERROR.                                <002>
      *****                                                             <002>
      *****IF   DESC-STATUZ            = MRNF                           <002>
      *****     MOVE ALL '?'           TO S6236-SACSTYPD                <002>
      *****ELSE                                                         <002>
      *****     MOVE DESC-SHORTDESC    TO S6236-SACSTYPD.               <002>
      *****                                                             <002>
      *****MOVE SACSENQ-CHDRCOY        TO RTRNSAC-RLDGCOY.              <002>
      *****MOVE SACSENQ-SACSCODE       TO RTRNSAC-SACSCODE.             <002>
      *****MOVE SACSENQ-CHDRNUM        TO RTRNSAC-RLDGACCT.             <002>
      *****MOVE SACSENQ-SACSTYP        TO RTRNSAC-SACSTYP.              <002>
      *****MOVE SACSENQ-CNTCURR        TO RTRNSAC-ORIGCCY               <002>
      *****                               S6236-ORIGCCY.                <002>
      *****MOVE BEGN                   TO RTRNSAC-FUNCTION.             <002>
      *****CALL 'RTRNSACIO'         USING RTRNSAC-PARAMS.               <002>
      *****IF   RTRNSAC-STATUZ      NOT = O-K                           <002>
      ***** AND RTRNSAC-STATUZ      NOT = ENDP                          <002>
      *****     MOVE RTRNSAC-PARAMS    TO SYSR-PARAMS                   <002>
      *****     PERFORM 600-FATAL-ERROR.                                <002>
      *****                                                             <002>
      *****MOVE RTRNSAC-ACCTCCY        TO S6236-ACCTCCY.                <002>
      *****                                                             <002>
      *****PERFORM 1400-PROCESS-RTRNSAC                                 <002>
      *****  UNTIL RTRNSAC-RLDGCOY  NOT = SACSENQ-CHDRCOY  OR           <002>
      *****        RTRNSAC-SACSCODE NOT = SACSENQ-SACSCODE OR           <002>
      *****        RTRNSAC-RLDGACCT NOT = SACSENQ-CHDRNUM  OR           <002>
      *****        RTRNSAC-SACSTYP  NOT = SACSENQ-SACSTYP  OR           <002>
      *****        RTRNSAC-ORIGCCY  NOT = SACSENQ-CNTCURR  OR           <002>
      *****        RTRNSAC-STATUZ       = ENDP.                         <002>
      *****                                                             <002>
      *1390-EXIT.                                                       <002>
      ***** EXIT.                                                       <002>
      /****                                                             <002>
      ******************************                                    <003>
       1400-PROCESS-RTRNSAC SECTION.
      ******************************                                    <003>
      *                                                                 <003>
       1400-PARA.                                                       <003>
      *****                                                             <003>
      *****IF  RTRNSAC-BATCTRCDE       = 'T206'                         <001>
      *****    GO TO 1400-READNXT-RTRN.                                 <001>
      *****                                                             <001>
           PERFORM 6000-LOAD-BANKCODE.                                  <V72L08>
                                                                        <V72L08>
           MOVE RTRNSAC-EFFDATE        TO S6236-EFFDATE.
           MOVE RTRNSAC-TRANNO         TO S6236-TRANNO.
           MOVE RTRNSAC-GLCODE         TO S6236-GLCODE.
           IF   RTRNSAC-GLSIGN          = '-'
                COMPUTE RTRNSAC-ORIGAMT = RTRNSAC-ORIGAMT * -1
                COMPUTE RTRNSAC-ACCTAMT = RTRNSAC-ACCTAMT * -1.
      **** MOVE RTRNSAC-ORIGAMT        TO S6236-ORIGAMT.                <V72L08>
      **** MOVE RTRNSAC-ACCTAMT        TO S6236-ACCTAMT.                <V72L08>
      **** MOVE RTRNSAC-ORIGAMT        TO S6236-HMHII.          <PS070> <V72L08>
      **** MOVE RTRNSAC-ACCTAMT        TO S6236-HMHLI.          <PS070> <V72L08>
           MOVE RTRNSAC-ORIGAMT        TO S6236-LIAOA.                  <PS070>
           MOVE RTRNSAC-ACCTAMT        TO S6236-LIAOP.                  <PS070>
           MOVE RTRNSAC-BATCTRCDE      TO S6236-TRCODE                  <GBF007>
           MOVE RTRNSAC-ORIGCCY        TO S6236-HCURRCD.                <GBF007>
           MOVE RTRNSAC-GENLCUR        TO S6236-HACCCURR.               <GBF007>
           IF  RTRNSAC-RDOCPFX      NOT = SPACES                        <V72L08>
           AND RTRNSAC-RDOCPFX      NOT = PRFX-CHDR                     <V72L08>
               MOVE 'N'                TO S6236-SELECT-OUT(PR)          <V72L08>
               MOVE RTRNSAC-RDOCNUM    TO S6236-RDOCNUM                 <V72L08>
               MOVE RTRNSAC-RDOCPFX    TO S6236-STATZ                   <V72L08>
           ELSE                                                         <V72L08>
               MOVE 'Y'                TO S6236-SELECT-OUT(PR)          <V72L08>
               MOVE SPACES             TO S6236-RDOCNUM                 <V72L08>
                                          S6236-STATZ                   <V72L08>
           END-IF.                                                      <V72L08>
      *
      *    Write the subfile record.
      *
           MOVE SADD                   TO SCRN-FUNCTION
           CALL 'S6236IO'           USING SCRN-SCREEN-PARAMS
                                          S6236-DATA-AREA
                                          S6236-SUBFILE-AREA
           IF SCRN-STATUZ           NOT = O-K
                MOVE SCRN-STATUZ       TO SYSR-STATUZ
                PERFORM 600-FATAL-ERROR.
      *                                                                 <001>
       1400-READNXT-RTRN.                                               <001>
      *                                                                 <002>
      *    Read the next RTRNSAC record.                                <002>
      *                                                                 <002>
           MOVE NEXTR                  TO RTRNSAC-FUNCTION.
           CALL 'RTRNSACIO'         USING RTRNSAC-PARAMS.
           IF   RTRNSAC-STATUZ      NOT = O-K
            AND RTRNSAC-STATUZ      NOT = ENDP
                MOVE RTRNSAC-PARAMS    TO SYSR-PARAMS
                PERFORM 600-FATAL-ERROR.

       1400-EXIT.
            EXIT.
      /
      *********************************                                 <CS020>
       A1100-PROCESS-ACBL-LPS SECTION.                                  <CS020>
      *********************************                                 <CS020>
      *                                                                 <CS020>
       A1100-PARA.                                                      <CS020>
      *                                                                 <CS020>
           MOVE ACBLENQ-SACSCODE       TO S6236-SACSCODE.               <CS020>
           MOVE ACBLENQ-SACSTYP        TO S6236-SACSTYP.                <CS020>
           MOVE ACBLENQ-RLDGACCT       TO S6236-ENTITY.                 <CS020>
      *                                                                 <CS020>
      *    Obtain the SACS CODE description from T3616.                 <CS020>
      *                                                                 <CS020>
           MOVE SPACES                 TO DESC-DATA-KEY.                <CS020>
           MOVE 'IT'                   TO DESC-DESCPFX.                 <CS020>
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.                 <CS020>
           MOVE T3616                  TO DESC-DESCTABL.                <CS020>
           MOVE ACBLENQ-SACSCODE       TO DESC-DESCITEM.                <CS020>
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.                <CS020>
           MOVE READR                  TO DESC-FUNCTION.                <CS020>
      *                                                                 <CS020>
           CALL 'DESCIO' USING DESC-PARAMS.                             <CS020>
           IF   DESC-STATUZ            NOT = O-K                        <CS020>
                                   AND NOT = MRNF                       <CS020>
                MOVE DESC-PARAMS       TO SYSR-PARAMS                   <CS020>
                PERFORM 600-FATAL-ERROR.                                <CS020>
      *                                                                 <CS020>
           IF   DESC-STATUZ            = MRNF                           <CS020>
                MOVE ALL '?'           TO S6236-SACSCODED               <CS020>
           ELSE                                                         <CS020>
                MOVE DESC-SHORTDESC    TO S6236-SACSCODED.              <CS020>
      *                                                                 <CS020>
      *    Obtain the SACS TYPE description from T3695.                 <CS020>
      *                                                                 <CS020>
           MOVE SPACES                 TO DESC-DATA-KEY.                <CS020>
           MOVE 'IT'                   TO DESC-DESCPFX.                 <CS020>
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.                 <CS020>
           MOVE T3695                  TO DESC-DESCTABL.                <CS020>
           MOVE ACBLENQ-SACSTYP        TO DESC-DESCITEM.                <CS020>
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.                <CS020>
           MOVE READR                  TO DESC-FUNCTION.                <CS020>
      *                                                                 <CS020>
           CALL 'DESCIO' USING DESC-PARAMS.                             <CS020>
           IF   DESC-STATUZ            NOT = O-K                        <CS020>
                                   AND NOT = MRNF                       <CS020>
                MOVE DESC-PARAMS       TO SYSR-PARAMS                   <CS020>
                PERFORM 600-FATAL-ERROR.                                <CS020>
                                                                        <CS020>
           IF   DESC-STATUZ            = MRNF                           <CS020>
                MOVE ALL '?'           TO S6236-SACSTYPD                <CS020>
           ELSE                                                         <CS020>
                MOVE DESC-SHORTDESC    TO S6236-SACSTYPD.               <CS020>
                                                                        <CS020>
           MOVE ACBLENQ-RLDGCOY        TO ACMVRCP-RLDGCOY.              <CS020>
           MOVE ACBLENQ-SACSCODE       TO ACMVRCP-SACSCODE.             <CS020>
           MOVE ACBLENQ-RLDGACCT       TO ACMVRCP-RLDGACCT.             <CS020>
           MOVE ACBLENQ-SACSTYP        TO ACMVRCP-SACSTYP.              <CS020>
           MOVE ACBLENQ-ORIGCURR       TO ACMVRCP-ORIGCURR              <CS020>
                                          S6236-ORIGCCY.                <CS020>
           MOVE VRCM-MAX-DATE          TO ACMVRCP-EFFDATE.              <CS020>
           MOVE ACMVRCPREC             TO ACMVRCP-FORMAT.               <CS020>
           MOVE BEGN                   TO ACMVRCP-FUNCTION.             <CS020>
           CALL 'ACMVRCPIO'         USING ACMVRCP-PARAMS.               <CS020>
           IF   ACMVRCP-STATUZ      NOT = O-K                           <CS020>
            AND ACMVRCP-STATUZ      NOT = ENDP                          <CS020>
                MOVE ACMVRCP-PARAMS    TO SYSR-PARAMS                   <CS020>
                PERFORM 600-FATAL-ERROR.                                <CS020>
                                                                        <CS020>
           MOVE ACMVRCP-GENLCUR        TO S6236-ACCTCCY.                <CS020>
                                                                        <CS020>
           PERFORM A1200-PROCESS-ACMVRCP                                <CS020>
             UNTIL ACMVRCP-RLDGCOY  NOT = ACBLENQ-RLDGCOY  OR           <CS020>
                   ACMVRCP-SACSCODE NOT = ACBLENQ-SACSCODE OR           <CS020>
                   ACMVRCP-RLDGACCT NOT = ACBLENQ-RLDGACCT OR           <CS020>
                   ACMVRCP-SACSTYP  NOT = ACBLENQ-SACSTYP  OR           <CS020>
                   ACMVRCP-ORIGCURR NOT = ACBLENQ-ORIGCURR OR           <CS020>
                   ACMVRCP-STATUZ       = ENDP.                         <CS020>
      *                                                                 <CS020>
       A1150-RTRN-READ.                                                 <CS020>
      *                                                                 <CS020>
           MOVE ACBLENQ-RLDGCOY        TO RTRNRCP-RLDGCOY.              <CS020>
           MOVE ACBLENQ-SACSCODE       TO RTRNRCP-SACSCODE.             <CS020>
           MOVE ACBLENQ-RLDGACCT       TO RTRNRCP-RLDGACCT.             <CS020>
           MOVE ACBLENQ-SACSTYP        TO RTRNRCP-SACSTYP.              <CS020>
           MOVE ACBLENQ-ORIGCURR       TO RTRNRCP-ORIGCCY               <CS020>
                                          S6236-ORIGCCY.                <CS020>
           MOVE VRCM-MAX-DATE          TO RTRNRCP-EFFDATE.              <CS020>
           MOVE RTRNRCPREC             TO RTRNRCP-FORMAT.               <CS020>
           MOVE BEGN                   TO RTRNRCP-FUNCTION.             <CS020>
           CALL 'RTRNRCPIO'         USING RTRNRCP-PARAMS.               <CS020>
           IF   RTRNRCP-STATUZ      NOT = O-K                           <CS020>
            AND RTRNRCP-STATUZ      NOT = ENDP                          <CS020>
                MOVE RTRNRCP-PARAMS    TO SYSR-PARAMS                   <CS020>
                PERFORM 600-FATAL-ERROR.                                <CS020>
                                                                        <CS020>
           MOVE RTRNRCP-GENLCUR        TO S6236-ACCTCCY.                <CS020>
                                                                        <CS020>
           PERFORM A1400-PROCESS-RTRNRCP                                <CS020>
             UNTIL RTRNRCP-RLDGCOY  NOT = ACBLENQ-RLDGCOY  OR           <CS020>
                   RTRNRCP-SACSCODE NOT = ACBLENQ-SACSCODE OR           <CS020>
                   RTRNRCP-RLDGACCT NOT = ACBLENQ-RLDGACCT OR           <CS020>
                   RTRNRCP-SACSTYP  NOT = ACBLENQ-SACSTYP  OR           <CS020>
                   RTRNRCP-ORIGCCY  NOT = ACBLENQ-ORIGCURR OR           <CS020>
                   RTRNRCP-STATUZ       = ENDP.                         <CS020>
                                                                        <CS020>
       A1190-EXIT.                                                      <CS020>
            EXIT.                                                       <CS020>
      /                                                                 <CS020>
      *******************************                                   <CS020>
       A1200-PROCESS-ACMVRCP SECTION.                                   <CS020>
      *******************************                                   <CS020>
      *                                                                 <CS020>
       A1200-PARA.                                                      <CS020>
      *                                                                 <CS020>
           MOVE ACMVRCP-EFFDATE        TO S6236-EFFDATE.                <CS020>
           MOVE ACMVRCP-TRANNO         TO S6236-TRANNO.                 <CS020>
           MOVE ACMVRCP-GLCODE         TO S6236-GLCODE.                 <CS020>
           IF   ACMVRCP-GLSIGN          = '-'                           <CS020>
                COMPUTE ACMVRCP-ORIGAMT = ACMVRCP-ORIGAMT * -1          <CS020>
                COMPUTE ACMVRCP-ACCTAMT = ACMVRCP-ACCTAMT * -1.         <CS020>
      **** MOVE ACMVRCP-ORIGAMT        TO S6236-HMHII.          <PS070> <CS020>
      **** MOVE ACMVRCP-ACCTAMT        TO S6236-HMHLI.          <PS070> <CS020>
           MOVE ACMVRCP-ORIGAMT        TO S6236-LIAOA.                  <PS070>
           MOVE ACMVRCP-ACCTAMT        TO S6236-LIAOP.                  <PS070>
           MOVE ACMVRCP-BATCTRCDE      TO S6236-TRCODE                  <CS020>
           MOVE ACMVRCP-ORIGCURR       TO S6236-HCURRCD.                <CS020>
           MOVE ACMVRCP-GENLCUR        TO S6236-HACCCURR.               <CS020>
           IF  ACMVRCP-RDOCPFX      NOT = SPACES                        <CS020>
           AND ACMVRCP-RDOCPFX      NOT = PRFX-CHDR                     <CS020>
               MOVE 'N'                TO S6236-SELECT-OUT(PR)          <CS020>
               MOVE ACMVRCP-RDOCNUM    TO S6236-RDOCNUM                 <CS020>
               MOVE ACMVRCP-RDOCPFX    TO S6236-STATZ                   <CS020>
           ELSE                                                         <CS020>
               MOVE 'Y'                TO S6236-SELECT-OUT(PR)          <CS020>
               MOVE SPACES             TO S6236-RDOCNUM                 <CS020>
                                          S6236-STATZ                   <CS020>
           END-IF.                                                      <CS020>
      *                                                                 <CS020>
      *    Write the subfile record.                                    <CS020>
      *                                                                 <CS020>
           MOVE SADD                   TO SCRN-FUNCTION                 <CS020>
           CALL 'S6236IO'           USING SCRN-SCREEN-PARAMS            <CS020>
                                          S6236-DATA-AREA               <CS020>
                                          S6236-SUBFILE-AREA            <CS020>
           IF SCRN-STATUZ           NOT = O-K                           <CS020>
                MOVE SCRN-STATUZ       TO SYSR-STATUZ                   <CS020>
                PERFORM 600-FATAL-ERROR.                                <CS020>
      *                                                                 <CS020>
      *    Read the next ACMVSAC record.                                <CS020>
      *                                                                 <CS020>
           MOVE NEXTR                  TO ACMVRCP-FUNCTION.             <CS020>
           CALL 'ACMVRCPIO'         USING ACMVRCP-PARAMS.               <CS020>
           IF   ACMVRCP-STATUZ      NOT = O-K                           <CS020>
            AND ACMVRCP-STATUZ      NOT = ENDP                          <CS020>
                MOVE ACMVRCP-PARAMS    TO SYSR-PARAMS                   <CS020>
                PERFORM 600-FATAL-ERROR.                                <CS020>
                                                                        <CS020>
       A1290-EXIT.                                                      <CS020>
            EXIT.                                                       <CS020>
      /                                                                 <CS020>
      ******************************                                    <CS020>
       A1400-PROCESS-RTRNRCP SECTION.                                   <CS020>
      ******************************                                    <CS020>
      *                                                                 <CS020>
       A1400-PARA.                                                      <CS020>
                                                                        <CS020>
           PERFORM A6000-LOAD-BANKCODE.                                 <CS020>
                                                                        <CS020>
           MOVE RTRNRCP-EFFDATE        TO S6236-EFFDATE.                <CS020>
           MOVE RTRNRCP-TRANNO         TO S6236-TRANNO.                 <CS020>
           MOVE RTRNRCP-GLCODE         TO S6236-GLCODE.                 <CS020>
           IF   RTRNRCP-GLSIGN          = '-'                           <CS020>
                COMPUTE RTRNRCP-ORIGAMT = RTRNRCP-ORIGAMT * -1          <CS020>
                COMPUTE RTRNRCP-ACCTAMT = RTRNRCP-ACCTAMT * -1.         <CS020>
      **** MOVE RTRNRCP-ORIGAMT        TO S6236-HMHII.          <PS070> <CS020>
      **** MOVE RTRNRCP-ACCTAMT        TO S6236-HMHLI.          <PS070> <CS020>
           MOVE RTRNRCP-ORIGAMT        TO S6236-LIAOA.                  <PS070>
           MOVE RTRNRCP-ACCTAMT        TO S6236-LIAOP.                  <PS070>
           MOVE RTRNRCP-BATCTRCDE      TO S6236-TRCODE                  <CS020>
           MOVE RTRNRCP-ORIGCCY        TO S6236-HCURRCD.                <CS020>
           MOVE RTRNRCP-GENLCUR        TO S6236-HACCCURR.               <CS020>
           IF  RTRNRCP-RDOCPFX      NOT = SPACES                        <CS020>
           AND RTRNRCP-RDOCPFX      NOT = PRFX-CHDR                     <CS020>
               MOVE 'N'                TO S6236-SELECT-OUT(PR)          <CS020>
               MOVE RTRNRCP-RDOCNUM    TO S6236-RDOCNUM                 <CS020>
               MOVE RTRNRCP-RDOCPFX    TO S6236-STATZ                   <CS020>
           ELSE                                                         <CS020>
               MOVE 'Y'                TO S6236-SELECT-OUT(PR)          <CS020>
               MOVE SPACES             TO S6236-RDOCNUM                 <CS020>
                                          S6236-STATZ                   <CS020>
           END-IF.                                                      <CS020>
      *                                                                 <CS020>
      *    Write the subfile record.                                    <CS020>
      *                                                                 <CS020>
           MOVE SADD                   TO SCRN-FUNCTION                 <CS020>
           CALL 'S6236IO'           USING SCRN-SCREEN-PARAMS            <CS020>
                                          S6236-DATA-AREA               <CS020>
                                          S6236-SUBFILE-AREA            <CS020>
           IF SCRN-STATUZ           NOT = O-K                           <CS020>
                MOVE SCRN-STATUZ       TO SYSR-STATUZ                   <CS020>
                PERFORM 600-FATAL-ERROR.                                <CS020>
      *                                                                 <CS020>
       A1400-READNXT-RTRN.                                              <CS020>
      *                                                                 <CS020>
      *    Read the next RTRNSAC record.                                <CS020>
      *                                                                 <CS020>
           MOVE NEXTR                  TO RTRNRCP-FUNCTION.             <CS020>
           CALL 'RTRNRCPIO'         USING RTRNRCP-PARAMS.               <CS020>
           IF   RTRNRCP-STATUZ      NOT = O-K                           <CS020>
            AND RTRNRCP-STATUZ      NOT = ENDP                          <CS020>
                MOVE RTRNRCP-PARAMS    TO SYSR-PARAMS                   <CS020>
                PERFORM 600-FATAL-ERROR.                                <CS020>
                                                                        <CS020>
       A1400-EXIT.                                                      <CS020>
            EXIT.                                                       <CS020>
      /                                                                 <CS020>
       A6000-LOAD-BANKCODE SECTION.                                     <CS020>
      ******************************                                    <CS020>
       A6010-START.                                                     <CS020>
      *                                                                 <CS020>
           INITIALIZE                  RDOC-DATA-AREA.                  <CS020>
           MOVE PRFX-CASH              TO RDOC-RDOCPFX.                 <CS020>
           MOVE RTRNRCP-RLDGCOY        TO RDOC-RDOCCOY.                 <CS020>
           MOVE RTRNRCP-RDOCNUM        TO RDOC-RDOCNUM.                 <CS020>
           MOVE BEGN                   TO RDOC-FUNCTION.                <CS020>
           MOVE RDOCREC                TO RDOC-FORMAT.                  <CS020>
           CALL 'RDOCIO'               USING RDOC-PARAMS.               <CS020>
                                                                        <CS020>
           IF RDOC-STATUZ              NOT = O-K AND MRNF               <CS020>
              MOVE RDOC-PARAMS         TO SYSR-PARAMS                   <CS020>
              PERFORM 600-FATAL-ERROR                                   <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
           IF RDOC-STATUZ              = O-K             AND            <CS020>
              RDOC-RDOCPFX             = PRFX-CASH       AND            <CS020>
              RDOC-RDOCCOY             = RTRNRCP-RLDGCOY AND            <CS020>
              RDOC-RDOCNUM             = RTRNRCP-RDOCNUM                <CS020>
              MOVE RDOC-BANKCODE       TO S6236-BANKCODE                <CS020>
           END-IF.                                                      <CS020>
      *                                                                 <CS020>
       A6090-EXIT.                                                      <CS020>
           EXIT.                                                        <CS020>
      /                                                                 <CS020>
      *****************************************************************
      *     RETRIEVE SCREEN FIELDS AND EDIT
      *****************************************************************
      *
      **************************                                        <003>
       PRE-SCREEN-EDIT SECTION.                                         <S9503>
      ************************                                          <S9503>
      *                                                                 <S9503>
       PRE-START.                                                       <S9503>
      *                                                                 <S9503>
           IF WSSP-SEC-ACTN(WSSP-PROGRAM-PTR) = '*'                     <V72L08>
              MOVE 3000                TO WSSP-SECTIONNO                <V72L08>
              GO                       TO PRE-EXIT                      <V72L08>
           END-IF.                                                      <V72L08>
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
      *    CALL 'S6236IO' USING SCRN-SCREEN-PARAMS                      <S9503>
      *                         S6236-DATA-AREA                         <S9503>
      *                         S6236-SUBFILE-AREA.                     <S9503>
      *    IF SCRN-STATUZ              = MASM                   <V72L08><004>
      *       MOVE ZEROS               TO WSSP-LIFEKEY.         <V72L08><004>
           IF SCRN-STATUZ              = MASM                           <V72L08>
              MOVE ZEROS               TO WSSP-LIFEKEY.                 <V72L08>
      * Screen errors are now handled in the calling program.           <S9503>
      *    PERFORM 200-SCREEN-ERRORS.                                   <S9503>
           MOVE O-K                    TO WSSP-EDTERROR.

           IF SCRN-STATUZ              = KILL                           <V72L08>
              GO TO 2090-EXIT                                           <V72L08>
           END-IF.                                                      <V72L08>
                                                                        <V72L08>
      *                                                                 <V72L08>
       2040-CHECK-OPTSWCH.                                              <V72L08>
           MOVE SSTRT                  TO SCRN-FUNCTION.                <V72L08>
           PERFORM 9000-SUBFILE-IO.                                     <V72L08>
           MOVE 'N'                    TO WSAA-FOUND-SELECTION.         <V72L08>
           PERFORM 2400-VALIDATE-SUBFILE                                <V72L08>
                                       UNTIL SCRN-STATUZ = ENDP.        <V72L08>
           IF FOUND-SELECTION          AND                              <V72L08>
              S6236-ERROR-INDICATORS   = SPACES                         <V72L08>
              MOVE O-K                 TO WSSP-EDTERROR                 <V72L08>
           END-IF.                                                      <V72L08>
           MOVE 1                      TO SCRN-SUBFILE-RRN.             <V72L08>
       2090-EXIT.
            EXIT.
      /
       2400-VALIDATE-SUBFILE SECTION.                                   <V72L08>
      *******************************                                   <V72L08>
       2410-START.                                                      <V72L08>
      *                                                                 <V72L08>
           IF S6236-SELECT             = SPACES                         <V72L08>
              GO TO 2420-NEXT                                           <V72L08>
           END-IF.                                                      <V72L08>
                                                                        <V72L08>
           MOVE O-K                    TO OPTS-STATUZ.                  <V72L08>
           MOVE 'CHCK'                 TO OPTS-FUNCTION.                <V72L08>
           MOVE WSAA-PROG              TO OPTS-CALLING-PROG.            <V72L08>
           MOVE ZEROES                 TO OPTS-DTEEFF.                  <V72L08>
           MOVE WSSP-COMPANY           TO OPTS-COMPANY.                 <V72L08>
           MOVE WSSP-LANGUAGE          TO OPTS-LANGUAGE.                <V72L08>
           MOVE WSSP-TRANID            TO VRCM-TRANID.                  <V72L08>
           MOVE VRCM-USER              TO OPTS-USER.                    <V72L08>
           MOVE S6236-SELECT           TO OPTS-SEL-OPTNO.               <V72L08>
           MOVE 'L'                    TO OPTS-SEL-TYPE.                <V72L08>
           MOVE SPACES                 TO OPTS-SEL-CODE.                <V72L08>
                                                                        <V72L08>
           CALL 'OPTSWCH'              USING OPTSWCH-REC                <V72L08>
                                             WSSP-SEC-PROGS             <V72L08>
                                             WSSP-SEC-ACTNS             <V72L08>
                                             WSSP-PROGRAM-PTR           <V72L08>
                                             WSSP-FLAG.                 <V72L08>
           IF OPTS-STATUZ              NOT = O-K                        <V72L08>
              MOVE OPTS-STATUZ         TO S6236-SELECT-ERR              <V72L08>
           ELSE                                                         <V72L08>
              MOVE 'Y'                 TO WSAA-FOUND-SELECTION          <V72L08>
              GO TO 2420-NEXT                                           <V72L08>
           END-IF.                                                      <V72L08>
                                                                        <V72L08>
           MOVE SUPD                   TO SCRN-FUNCTION.                <V72L08>
           PERFORM 9000-SUBFILE-IO.                                     <V72L08>
      *                                                                 <V72L08>
       2420-NEXT.                                                       <V72L08>
           MOVE SRDN                   TO SCRN-FUNCTION.                <V72L08>
           PERFORM 9000-SUBFILE-IO.                                     <V72L08>
      *                                                                 <V72L08>
       2490-EXIT.                                                       <V72L08>
           EXIT.                                                        <V72L08>
      /                                                                 <V72L08>
      *****************************************************************
      *     UPDATE DATABASE IF REQUIRED AND LOG TRANSACTION
      *****************************************************************
      *
      **********************                                            <003>
       3000-UPDATE SECTION.
      **********************
       3010-UPDATE-DATABASE.
      *
           IF  WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'                   <V72L08>
              GO TO 3090-EXIT                                           <V72L08>
           END-IF.                                                      <V72L08>

       3090-EXIT.
            EXIT.
      /
      *****************************************************************
      *     DECIDE WHICH TRANSACTION PROGRAM IS NEXT
      *****************************************************************
      *
      *************************                                         <003>
       4000-WHERE-NEXT SECTION.
      *************************
      *
       4010-NEXT-PROGRAM.
      **** ADD 1                       TO WSSP-PROGRAM-PTR.             <V72L08>
                                                                        <V72L08>
           IF SCRN-STATUZ              = SUBM     OR                    <V72L08>
              SCRN-STATUZ              = KILL                           <V72L08>
              MOVE WSSP-SUBMENU        TO WSSP-NEXTPROG                 <V72L08>
              GO TO 4090-EXIT                                           <V72L08>
           END-IF.                                                      <V72L08>
                                                                        <V72L08>
           IF WSSP-SEC-ACTN(WSSP-PROGRAM-PTR) = '*'                     <V72L08>
              PERFORM 7200-RESTORE-WSSPLEDG                             <V72L08>
              GO TO 4020-CALL-STCK                                      <V72L08>
           END-IF.                                                      <V72L08>
      *
           MOVE O-K                    TO SCRN-STATUZ.                  <V72L08>
           MOVE 'N'                    TO WSAA-FOUND-SELECTION.         <V72L08>
           MOVE SSTRT                  TO SCRN-FUNCTION.                <V72L08>
           PERFORM 9000-SUBFILE-IO.                                     <V72L08>
           PERFORM UNTIL SCRN-STATUZ   = ENDP OR FOUND-SELECTION        <V72L08>
              IF S6236-SELECT          = '1'                            <V72L08>
                 MOVE S6236-SELECT     TO OPTS-SEL-OPTNO                <V72L08>
                 PERFORM 7000-CHECKING-DOCTYPE                          <V72L08>
                 MOVE SPACES           TO S6236-SELECT                  <V72L08>
                 MOVE SUPD             TO SCRN-FUNCTION                 <V72L08>
                 PERFORM 9000-SUBFILE-IO                                <V72L08>
              END-IF                                                    <V72L08>
              IF NOT FOUND-SELECTION                                    <V72L08>
                 MOVE SRDN             TO SCRN-FUNCTION                 <V72L08>
                 PERFORM 9000-SUBFILE-IO                                <V72L08>
              END-IF                                                    <V72L08>
           END-PERFORM.                                                 <V72L08>
                                                                        <V72L08>
           IF NOT FOUND-SELECTION                                       <V72L08>
              MOVE 1                   TO SCRN-SUBFILE-RRN              <V72L08>
              ADD 1                    TO WSSP-PROGRAM-PTR              <V72L08>
              MOVE WSAA-SBMACTION      TO WSSP-SBMACTION                <V72L08>
              GO TO 4090-EXIT                                           <V72L08>
           END-IF.                                                      <V72L08>
                                                                        <V72L08>
      *                                                                 <V72L08>
       4020-CALL-STCK.                                                  <V72L08>
           MOVE 'STCK'                 TO OPTS-FUNCTION.                <V72L08>
           CALL 'OPTSWCH'              USING OPTSWCH-REC                <V72L08>
                                             WSSP-SEC-PROGS             <V72L08>
                                             WSSP-SEC-ACTNS             <V72L08>
                                             WSSP-PROGRAM-PTR           <V72L08>
                                             WSSP-FLAG.                 <V72L08>
           IF OPTS-STATUZ              NOT = O-K AND ENDP               <V72L08>
              MOVE 'STCK'              TO SYSR-FUNCTION                 <V72L08>
              MOVE OPTS-STATUZ         TO SYSR-DBIO-STATUZ              <V72L08>
                                          SYSR-STATUZ                   <V72L08>
              MOVE 'OPTSWCH'           TO SYSR-IOMOD                    <V72L08>
              PERFORM 600-FATAL-ERROR                                   <V72L08>
           END-IF.                                                      <V72L08>
                                                                        <V72L08>
           IF OPTS-STATUZ              = ENDP                           <V72L08>
              MOVE SCRN-SCRNAME        TO WSSP-NEXTPROG                 <V72L08>
           ELSE                                                         <V72L08>
              ADD 1                    TO WSSP-PROGRAM-PTR              <V72L08>
           END-IF.                                                      <V72L08>
      *                                                                 <V72L08>
       4090-EXIT.
            EXIT.
      /                                                                 <V72L08>
       6000-LOAD-BANKCODE SECTION.                                      <V72L08>
      ****************************                                      <V72L08>
       6010-START.                                                      <V72L08>
      *                                                                 <V72L08>
           INITIALIZE                  RDOC-DATA-AREA.                  <V72L08>
           MOVE PRFX-CASH              TO RDOC-RDOCPFX.                 <V72L08>
           MOVE RTRNSAC-RLDGCOY        TO RDOC-RDOCCOY.                 <V72L08>
           MOVE RTRNSAC-RDOCNUM        TO RDOC-RDOCNUM.                 <V72L08>
           MOVE BEGN                   TO RDOC-FUNCTION.                <V72L08>
           MOVE RDOCREC                TO RDOC-FORMAT.                  <V72L08>
           CALL 'RDOCIO'               USING RDOC-PARAMS.               <V72L08>
                                                                        <V72L08>
           IF RDOC-STATUZ              NOT = O-K AND MRNF               <V72L08>
              MOVE RDOC-PARAMS         TO SYSR-PARAMS                   <V72L08>
              PERFORM 600-FATAL-ERROR                                   <V72L08>
           END-IF.                                                      <V72L08>
                                                                        <V72L08>
           IF RDOC-STATUZ              = O-K             AND            <V72L08>
              RDOC-RDOCPFX             = PRFX-CASH       AND            <V72L08>
              RDOC-RDOCCOY             = RTRNSAC-RLDGCOY AND            <V72L08>
              RDOC-RDOCNUM             = RTRNSAC-RDOCNUM                <V72L08>
              MOVE RDOC-BANKCODE       TO S6236-BANKCODE                <V72L08>
           END-IF.                                                      <V72L08>
      *                                                                 <V72L08>
       6090-EXIT.                                                       <V72L08>
           EXIT.                                                        <V72L08>
      /                                                                 <V72L08>
       7000-CHECKING-DOCTYPE SECTION.                                   <V72L08>
      *******************************                                   <V72L08>
       7010-START.                                                      <V72L08>
      *                                                                 <V72L08>
           MOVE SPACES                 TO OPTS-SEL-CODE.                <V72L08>
           IF OPTS-SEL-OPTNO            = 1                             <V72L08>
              INITIALIZE               ACCINQ-DATA                      <V72L08>
              IF S6236-STATZ            = PRFX-GJRN                     <V72L08>
                 MOVE S6236-STATZ      TO OPTS-SEL-CODE                 <V72L08>
                 MOVE S6236-RDOCNUM    TO WSAA-JRN-JOURNAL              <V72L08>
                 MOVE TDAY             TO DTC1-FUNCTION                 <V72L08>
                 CALL 'DATCON1' USING DTC1-DATCON1-REC                  <V72L08>
                 MOVE DTC1-INT-DATE    TO WSAA-JRN-DATE                 <V72L08>
                 MOVE WSAA-JRN-KEY     TO ACCI-IN-KEY                   <V72L08>
                 MOVE WSAA-JRN-PROG    TO WSAA-BOPROG                   <V72L08>
                 PERFORM 7100-CALL-BO-PROG                              <V72L08>
              ELSE                                                      <V72L08>
                 IF S6236-STATZ         = PRFX-CASH                     <V72L08>
                    MOVE S6236-STATZ    TO OPTS-SEL-CODE                <V72L08>
                    MOVE S6236-BANKCODE TO WSAA-REC-BANKCODE            <V72L08>
                    MOVE S6236-RDOCNUM  TO WSAA-REC-RECEIPT             <V72L08>
                    MOVE WSAA-REC-KEY   TO ACCI-IN-KEY                  <V72L08>
                    MOVE WSAA-REC-PROG  TO WSAA-BOPROG                  <V72L08>
                    PERFORM 7100-CALL-BO-PROG                           <V72L08>
                 END-IF                                                 <V72L08>
                 IF S6236-STATZ          = PRFX-REQN                    <V72L08>
                    MOVE S6236-STATZ    TO OPTS-SEL-CODE                <V72L08>
                    MOVE S6236-RDOCNUM  TO WSAA-PAY-REQNNO              <V72L08>
                    MOVE WSAA-PAY-KEY   TO ACCI-IN-KEY                  <V72L08>
                    MOVE WSAA-PAY-PROG  TO WSAA-BOPROG                  <V72L08>
                    PERFORM 7100-CALL-BO-PROG                           <V72L08>
                 END-IF                                                 <V72L08>
              END-IF                                                    <V72L08>
           END-IF.                                                      <V72L08>
           IF OPTS-SEL-CODE            = SPACES                         <V72L08>
              GO TO 7090-EXIT                                           <V72L08>
           END-IF.                                                      <V72L08>
           MOVE S6236-SELECT           TO WSAA-SELECT                   <V72L08>
           PERFORM VARYING WSAA-COUNT  FROM 1 BY 1                      <V72L08>
                   UNTIL WSAA-COUNT    > 20                             <V72L08>
              IF OPTS-TYPE(WSAA-COUNT) = 'L'                   AND      <V72L08>
                 OPTS-NO (WSAA-COUNT)  = WSAA-SELECT           AND      <V72L08>
                 OPTS-CODE(WSAA-COUNT) = OPTS-SEL-CODE                  <V72L08>
                 MOVE 'X'              TO OPTS-IND(WSAA-COUNT)          <V72L08>
                 MOVE 'Y'              TO WSAA-FOUND-SELECTION          <V72L08>
              END-IF                                                    <V72L08>
           END-PERFORM.                                                 <V72L08>
                                                                        <V72L08>
      *                                                                 <V72L08>
       7090-EXIT.                                                       <V72L08>
           EXIT.                                                        <V72L08>
      /                                                                 <V72L08>
       7100-CALL-BO-PROG SECTION.                                       <V72L08>
      ***************************                                       <V72L08>
       7110-START.                                                      <V72L08>
      *                                                                 <V72L08>
           MOVE WSSP-FLAG              TO WSAA-FLAG.                    <V72L08>
           MOVE WSSP-SEC-PROGS         TO WSAA-SEC-PROGS.               <V72L08>
           MOVE WSSP-SEC-ACTNS         TO WSAA-SEC-ACTNS.               <V72L08>
           MOVE WSSP-PROGRAM-PTR       TO WSAA-PROGRAM-PTR.             <V72L08>
           MOVE WSSP-SUBMENU           TO WSAA-SUBMENU.                 <V72L08>
           MOVE WSSP-NEXTPROG          TO WSAA-NEXTPROG.                <V72L08>
           MOVE WSSP-SECTIONNO         TO WSAA-SECTIONNO.               <V72L08>
      *                                                                 <V72L08>
           MOVE WSSP-GENVKEY           TO WSAA-GENVKEY.                 <V72L08>
           MOVE WSSP-DOCTKEY           TO WSAA-DOCTKEY.                 <V72L08>
           MOVE WSSP-ALOCKEY           TO WSAA-ALOCKEY.                 <V72L08>
           MOVE WSSP-ACUMKEY           TO WSAA-ACUMKEY.                 <V72L08>
           MOVE WSSP-RGFORMAT          TO WSAA-RGFORMAT.                <V72L08>
           MOVE WSSP-RGDETAIL          TO WSAA-RGDETAIL.                <V72L08>
           MOVE WSSP-RGREPORT          TO WSAA-RGREPORT.                <V72L08>
           MOVE WSSP-BATCHKEY          TO WSAA-BATCHKEY.                <V72L08>
      *                                                                 <V72L08>
           MOVE WSSP-COMPANY           TO ACCI-COMPANY.                 <V72L08>
           CALL WSAA-BOPROG            USING LEADER-HEADER              <V72L08>
                                             WSAA-RESPONSE              <V72L08>
                                             RESPONSE-DATA              <V72L08>
                                             ACCINQ-REC.                <V72L08>
           IF RESP-WARNING OF LEADER-HEADER                             <V72L08>
              MOVE ACCINQ-REC          TO SYSR-PARAMS                   <V72L08>
              PERFORM 600-FATAL-ERROR                                   <V72L08>
           END-IF.                                                      <V72L08>
      *                                                                 <V72L08>
           MOVE LEADER-HEADER          TO WSAA-LDRHDR.                  <V72L08>
           MOVE 'SESSION'              TO OBJID OF WSAA-LDRHDR.         <V72L08>
           MOVE 'RTRV '                TO VRBID OF WSAA-LDRHDR.         <V72L08>
           CALL 'SESSION'           USING WSAA-LDRHDR                   <V72L08>
                                          WSAA-REQUEST                  <V72L08>
                                          WSAA-RESPONSE.                <V72L08>
                                                                        <V72L08>
           MOVE WSAA-RESPONSE          TO SESSIONO-REC.                 <V72L08>
           MOVE SESSIONO-WSSP          TO SAVE-WSSP.                    <V72L08>
           MOVE SAVE-COMMON-AREA       TO WSSP-COMMON-AREA.             <V72L08>
           MOVE SAVE-USER-AREA         TO WSSP-USER-AREA.               <V72L08>
                                                                        <V72L08>
           MOVE ACCI-BATCHKEY          TO WSSP-BATCHKEY.                <V72L08>
      *                                                                 <V72L08>
           MOVE WSAA-FLAG              TO WSSP-FLAG.                    <V72L08>
           MOVE WSAA-SEC-PROGS         TO WSSP-SEC-PROGS.               <V72L08>
           MOVE WSAA-SEC-ACTNS         TO WSSP-SEC-ACTNS.               <V72L08>
           MOVE WSAA-PROGRAM-PTR       TO WSSP-PROGRAM-PTR.             <V72L08>
           MOVE WSAA-SUBMENU           TO WSSP-SUBMENU.                 <V72L08>
           MOVE WSAA-NEXTPROG          TO WSSP-NEXTPROG.                <V72L08>
           MOVE WSAA-SECTIONNO         TO WSSP-SECTIONNO.               <V72L08>
      *                                                                 <V72L08>
       7190-EXIT.                                                       <V72L08>
           EXIT.                                                        <V72L08>
      /                                                                 <V72L08>
       7200-RESTORE-WSSPLEDG SECTION.                                   <V72L08>
      *******************************                                   <V72L08>
       7210-START.                                                      <V72L08>
      *                                                                 <V72L08>
           MOVE WSAA-GLKEY             TO WSSP-GENLKEY.                 <V72L08>
           MOVE WSAA-GENVKEY           TO WSSP-GENVKEY.                 <V72L08>
           MOVE WSAA-DOCTKEY           TO WSSP-DOCTKEY.                 <V72L08>
           MOVE WSAA-ALOCKEY           TO WSSP-ALOCKEY.                 <V72L08>
           MOVE WSAA-ACUMKEY           TO WSSP-ACUMKEY.                 <V72L08>
           MOVE WSAA-RGFORMAT          TO WSSP-RGFORMAT.                <V72L08>
           MOVE WSAA-RGDETAIL          TO WSSP-RGDETAIL.                <V72L08>
           MOVE WSAA-RGREPORT          TO WSSP-RGREPORT.                <V72L08>
           MOVE WSAA-BATCHKEY          TO WSSP-BATCHKEY.                <V72L08>
      *                                                                 <V72L08>
       7290-EXIT.                                                       <V72L08>
           EXIT.                                                        <V72L08>
      /                                                                 <V72L08>
       9000-SUBFILE-IO SECTION.                                         <V72L08>
      *************************                                         <V72L08>
       9000-SUBFILE.                                                    <V72L08>
           CALL 'S6236IO'              USING SCRN-SCREEN-PARAMS         <V72L08>
                                             S6236-DATA-AREA            <V72L08>
                                             S6236-SUBFILE-AREA.        <V72L08>
           IF SCRN-STATUZ           NOT = O-K AND ENDP                  <V72L08>
              MOVE SCRN-STATUZ         TO SYSR-STATUZ                   <V72L08>
              PERFORM 600-FATAL-ERROR                                   <V72L08>
           END-IF.                                                      <V72L08>
                                                                        <V72L08>
       9000-EXIT.                                                       <V72L08>
           EXIT.                                                        <V72L08>
