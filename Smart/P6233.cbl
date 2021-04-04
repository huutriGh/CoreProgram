      * Generation Parameters SCRVER(02)               Do Not Delete!   <S9503>
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P6233.
      *REMARKS.
      *
      * Initialise
      * ----------
      *
      *     Skip  this  section  if  returning from an optional selection
      *     (current stack position action flag = '*').
      *
      *     Clear the subfile ready for loading.
      *
      *     The  details  of  the  contract  being  enquired upon will be
      *     stored  in  the  CHDRENQ I/O module. Retrieve the details and
      *     set up the header portion of the screen.
      *
      *     Look up the following descriptions and names:
      *
      *          Contract Type, (CNTTYPE) - long description from T5688,
      *
      *          Contract  Status,  (STATCODE)  -  short description from
      *          T3623,
      *
      *          Premium  Status,  (PSTATCODE)  -  short description from
      *          T3588,
      *
      *          Servicing branch, - long description from T1692,
      *
      *          The owner's client (CLTS) details.
      *
      *          The  joint  owner's client (CLTS) details if they exist.
      *
      *
      *     Load the subfile as follows:
      *
      *          Perform a BEGN on PTRNENQ with a key of Contract Company
      *          (CHDRCOY),  Contract Header (CHDRNUM) and Effective Date
      *          (PTRNEFF)  set  to  all  '9's.  Then  read  sequentially
      *          through  until end of file or Company or Contract Number
      *          changes.
      *
      *          Display   the   Effective   Date,   (PTRNEFF)   and  the
      *          Transaction Code, (BATCTRCODE).  The transaction code is
      *          also decoded for its long description against T1688.
      *
      *
      *     Load all  pages  required  in the subfile and set the subfile
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
      *     If  "CF11"  was  requested move spaces to the current program
      *     position add 1 to the program pointer and exit.
      *
      *     Release  any  PTRNENQ  record  that  may have been previously
      *     stored.
      *
      *     At this  point  the  program will be either searching for the
      *     FIRST  selected  record  in  order  to  pass  control  to the
      *     Transactions Postings program for the selected transaction or
      *     it  will  be returning from the Transactions Postings program
      *     after  displaying  some  details  and  searching for the NEXT
      *     selected record.
      *
      *     It will  be able to determine which of these two states it is
      *     in by examining the Stack Action Flag.
      *
      *     If not returning from a Transactions Postings display, (Stack
      *     Action Flag  is  blank),  perform  a  start on the subfile to
      *     position the file pointer at the beginning.
      *
      *     Each  time  it  returns  to  this  program after processing a
      *     previous selection its position in the subfile will have been
      *     retained and  it  will be able to continue from where it left
      *     off.
      *
      *     Processing from  here is the same for either state. After the
      *     Start  or  after  returning to the program after processing a
      *     previous selection read the next record from the subfile.  If
      *     this  is not selected (Select is blank), continue reading the
      *     next  subfile  record  until  one  is  found with a non-blank
      *     Select field  or end of file is reached. Do not use the 'Read
      *     Next Changed Subfile Record' function.
      *
      *     If nothing  was  selected  or there are no more selections to
      *     process,  continue by just moving spaces to the current stack
      *     action field and program position and exit.
      *
      *     If a  selection  has  been  found  read and store (READS) the
      *     associated   PTRNENQ   record  using  the  Contract  Company,
      *     Contract  Number and Effective date from the selected subfile
      *     record. Add 1 to the program pointer and exit.
      *
      *
      * Notes.
      * ------
      *
      *     Create a  new  view  of PTRNPF called PTRNENQ which uses only
      *     those fields required for this program. It should be keyed on
      *     CHDRCOY CHDRNUM and PTRNEFF.
      *
      *     Tables Used:
      *
      * T1688 - Transaction Codes                 Key: Transaction Code
      * T1692 - Branch Codes                      Key: Branch Code
      * T3588 - Contract Premium Status           Key: PSTATCODE
      * T3623 - Contract Risk Status              Key: STATCODE
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
      * 29/11/89    LI     If Transaction Code from PTRN = B216,
      *                    (ie. renewals), ignore the record and
      *                    do not display any transaction details.
      *                    Store the unique Transaction Number on
      *                    the subfile, as an hidden field.
      *                    For each PTRN selected, attempt to read
      *                    ACMV and/or RTRN records.  If neither are
      *                    found for the record, protect the Select
      *                    field associated with the subfile record.
      *                    When a record is selected, use the Transaction
      *                    Number stored to READS the PTRN record.      <001>
      *
      *  14.02.90   F.M.   Add field TRCODE (battrccde) to Key.     002 <002>
      *                    SDF 337                                  002 <002>
      *
      *  27.02.90   J.F.   Ammended the setting up of the PTRNENQ       <003>
      *                    key to reflect changes made to the           <003>
      *                    logical file.                            003 <003>
      *
      *  05.03.90   J.F.   Removed the Batch Transaction Code field     <004>
      *                    from the keys of the RTRNTRN and ACMVTRN     <004>
      *                    logical files.                           004 <004>
      *  13.03.90   T.S.   Reinstated to show the transaction code  005 <004>
      *                    'B216'.
      *  05/08/91   J.L    (SDF 1782)                               006
      *                    Initialise WSSP-LIFEKEY when scrn-statuz
      *                    = MASM.
      *
      *  99/99/99   ????   ??????????                               007
      *
      *  02/09/91   J.K.   (SDF - 2237)                             008
      *                    Do not show Valid Flag '2' PTRNs on
      *                    the subfile.
      *                    This is because a Full Contract Reversal
      *                    logically deletes PTRNs by setting the
      *                    Valid Flag to '2' - and we don't want to
      *                    see these PTRNs on an enquiry.
      *
      *  24/06/92   M.P.   (SDF - 3082)                             009
      *                    Correct the checking of ACMVs and RTRNs
      *
      * 24.08.93    H.P    AQR 4668.                                010
      *                    Hard coding of 'NONE' removed when joint
      *                    life not found.
      *
      * 13/10/93    D.W.   AQR 4490. Life Development L03-9311.     011
      *                    Show Validflag '2' PTRNs on the scroll
      *                     - screen has been amended to distinguish
      *                       between V/F '1' and '2' records.
      *
      * 11/10/94    B.L.   AQR 5586. Life Development 9503.         012
      *                    ACMV/RTRN records may not be in DASD, but
      *                    have been archived.
      *
      *                    Only when no ACMV's have been found
      *                    is the following performed:-
      *
      *                    Read T3715 to see if records have been
      *                    archived using a COLD solution or
      *                    whether a subset of ACMV's has been
      *                    created ie. ACMC record. (or both)
      *
      *                    Read the ARCM file which
      *                    holds the latest archived accounting
      *                    period. If the PTRN YYMM is >
      *                    ARCM YYMM then the select field is OK
      *                    to remain protected.
      *
      *                    If T3715 Enquiry Flag is not spaces
      *                    read ACMCTRN, this is the subset of
      *                    ACMV for enquiry only. If none are
      *                    found then protect the select field.
      *
      *                    If T3715 Cold Flag is not spaces
      *                    remove the protection of
      *                    the Select field. ie. ACMV's may exist
      *                    but they have been archived to Optical.
      *
      * 25/01/95    COS.   AQR : 5681                               013
      *                    Smart 9503 Client/server development.
      *                    Standardisation of code.
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
      * 01/02/96  01/01   D96NUM       Rachel Cartwright                    *
      * 02/02/96  01/01   D96NUM       Rob Yates                            *
      *                   ORIGAMT                                           *
      *                   SINSTAMT                                          *
      *                   ISAM                                              *
      *                   ACCTAMT                                           *
      *                   The above field(s) have been increased in         *
      *                   size as part of the 1996 Numeric Field            *
      *                   Increase project. This is to enable users         *
      *                   to then convert (if they should wish to do        *
      *                   so) the fields to 'Monetary' and use the          *
      *                   SMART Variable Decimal Places technology          *
      *                   accordingly.                                      *
      *                                                                     *
      * 17/07/96  01/01   CAS1.0       Kristin Mcleish                      *
      *                   Allow selection on reversed transactions
      *                   if accounting movements were created by
      *                   the reversal.
      *
      * 17/10/97  01/01   AY2K         Kee Jee                              *
      *           Increase the following fields by 2 char                   *
      *           - WSAA-ACYTR                                              *
      *           - WSBB-ACTYR                                              *
      *                                                                     *
      * 30/11/97    DUNC  SMART 9503 Conv for Client/Server.        <S9503>
      *                                                                     *
      * 08/10/99  01/01   V5L001       Josephine Chiam                      *
      *           RECOMPILED                                           ??   *
      *                                                                     *
      * 12/12/03  01/01   GBF          Steve Hale                           *
      *           Addition of new hidden fields, Reason code,               *
      *           Accounting movement flag and transaction date for         *
      *           Graphical front end usage.                                *
      *                                                                     *
      * 25/04/05  01/01   D05GB1       Steve Hale                           *
      *           Only MOVE 1 to the subfile RRN when not coming from a     *
      *           remote device so that a calling BO can get at the         *
      *           size of the subfile by using the RRN.                     *
      *                                                                     *
      * 25/01/07  01/01   V71L01       Wang Ge/FSG/CSC (Singapore)          *
      *           To display CRTUSER and DATESUB info as well.              *
      *                                                                     *
      * 15/08/07  01/01   V72L08       Xu Chen/ASIA/CSC (Beijing)           *
      *           Include selection 1 - Financial Details and 2- Fund       *
      *           Movements. When select 1, call GENSSW to display S6234,   *
      *           when select 2, call GENSSW to display new fund movement   *
      *           screen SR50U.                                             *
      *                                                                     *
      * 05/11/07  01/01   ARCH01       Vincent S Carillo/ASIA/CSC (Si       *
      *           New field added ==> Data Location ...                     *
      *                                                                     *
      * 04/02/08  01/01   ARCH01       Yoke Wah Tan/FSG/CSC (Singapor       *
      *           Recompile                                                 *
      *                                                                     *
      * 09/07/09  Retrofitted by Helen Cui                                  *
      *           01/01   LA4407       Jacco Landskroon                     *
      *           Remove redundant ACMC and ARCM processing.                *
      *                                                                     *
      * 23/10/13  01/01   GAPPH1       Thanh Do                             *
      *           Show Tax Invoice Ref Info.                                *
      *                                                                     *
      * 08/11/13  01/01   GAPPH2       Thanh Do                             *
      *           Always shows User Profile, not Create UserID.             *
      *                                                                     *
      * 24/07/15  01/01   PHE003       Phuong Le Dev                        *
      *           Show 'APL' in case of APL processing                      *
      *                                                                     *
      **DD/MM/YY*************************************************************
      **************************************************************
      /
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       01  WSAA-PROG                   PIC X(05) VALUE 'P6233'.
       01  WSAA-VERSION                PIC X(02) VALUE '01'.
       01  WSAA-SUBFCHG                PIC S9(03) COMP.                 <V72L08>
       01  WSAA-RECORD-FOUND           PIC X(01).                       <V72L08>
           88  RECORD-FOUND            VALUE 'Y'.                       <V72L08>
      *

           COPY GENSSWREC.

       01  WSAA-SEC-PROGS.
           03  WSAA-SEC-PROG           PIC X(05) OCCURS 8.

       01  WSAA-BATCKEY.
           COPY BATCKEY.

       01  WSAA-PERIOD.                                                 <012>
      **** 03  WSAA-ACTYR              PIC 9(02).               <AY2K>  <012>
           03  WSAA-ACTYR              PIC 9(04).                       <AY2K>
           03  WSAA-ACTMN              PIC 9(02).                       <012>
      *                                                                 <012>
       01  WSBB-PERIOD.                                                 <012>
      **** 03  WSBB-ACTYR              PIC 9(02).               <AY2K>  <012>
           03  WSBB-ACTYR              PIC 9(04).                       <AY2K>
           03  WSBB-ACTMN              PIC 9(02).                       <012>
      *                                                                 <012F
       01  WSAA-DATIME.                                                 <GBF>
           03  WSAA-YEAR               PIC X(04).                       <GBF>
           03  FILLER                  PIC X(01).                       <GBF>
           03  WSAA-MONTH              PIC X(02).                       <GBF>
           03  FILLER                  PIC X(01).                       <GBF>
           03  WSAA-DAY                PIC X(02).                       <GBF>
           03  FILLER                  PIC X(16).                       <GBF>
      *                                                                 <GBF>
       01  WSAA-DATE-FORMAT.                                            <GBF>
           03  WSAA-DF-YY              PIC X(04).                       <GBF>
           03  WSAA-DF-MM              PIC X(02).                       <GBF>
           03  WSAA-DF-DD              PIC X(02).                       <GBF>
       01  WSAA-TX-DATE REDEFINES WSAA-DATE-FORMAT                      <GBF>
                                       PIC 9(08).                       <GBF>
                                                                        <GBF>
       01  SUB1                        PIC S9(03) COMP-3.               <V72L08>
       01  SUB2                        PIC S9(03) COMP-3.               <V72L08>
      *                                                                 <GAPPH1>
       01  WSAA-TAX-INVOICE            PIC X(15).                       <GAPPH1>
       01  WSAA-CANC-CHR               PIC X(03) VALUE '***'.           <GAPPH1>
       01  WSAA-INVSEQ                 PIC X(07).                       <GAPPH1>
                                                                        <V72L08>
       01  ERRORS.
           03  E005                    PIC X(04) VALUE 'E005'.
           03  E040                    PIC X(04) VALUE 'E040'.
           03  G620                    PIC X(04) VALUE 'G620'.
           03  E494                    PIC X(04) VALUE 'E494'.          <V72L08>
           03  H093                    PIC X(04) VALUE 'H093'.          <V72L08>
      *
       01  TABLES.
           03  T1688                   PIC X(05) VALUE 'T1688'.
           03  T1692                   PIC X(05) VALUE 'T1692'.
           03  T3588                   PIC X(05) VALUE 'T3588'.
           03  T3623                   PIC X(05) VALUE 'T3623'.
           03  T5688                   PIC X(05) VALUE 'T5688'.
      **** 03  T3715                   PIC X(05) VALUE 'T3715'. <LA4407><012>
      *
       01  FORMATS.
           03  ACMVTRNREC              PIC X(10) VALUE 'ACMVTRNREC'.
           03  RTRNTRNREC              PIC X(10) VALUE 'RTRNTRNREC'.
           03  CHDRENQREC              PIC X(10) VALUE 'CHDRENQREC'.
      *    03  LIFEENQREC              PIC X(10) VALUE 'LIFEENQREC'.    <V71L01>
           03  LIFELNBREC              PIC X(10) VALUE 'LIFELNBREC'.    <V71L01>
           03  PTRNENQREC              PIC X(10) VALUE 'PTRNENQREC'.
           03  CLTSREC                 PIC X(10) VALUE 'CLTSREC'.
           03  DESCREC                 PIC X(10) VALUE 'DESCREC'.
           03  ITEMREC                 PIC X(10) VALUE 'ITEMREC'.       <012>
      **** 03  ARCMREC                 PIC X(10) VALUE 'ARCMREC'.       <LA4407>
      **** 03  ACMCTRNREC              PIC X(10) VALUE 'ACMCTRNREC'.    <LA4407>
           03  RESNENQREC              PIC X(10) VALUE 'RESNENQREC'.    <GBF>
           03  UTRSREC                 PIC X(10) VALUE 'UTRSREC'.       <V72L08>
           03  HITSREC                 PIC X(10) VALUE 'HITSREC'.       <V72L08>
           03  ZTAXENQREC              PIC X(10) VALUE 'ZTAXENQREC'.    <GAPPH1>
      /
           COPY CHDRENQSKM.
      /
      *    COPY LIFEENQSKM.                                             <V71L01>
           COPY LIFELNBSKM.                                             <V71L01>
      /
           COPY PTRNENQSKM.
      /
           COPY ACMVTRNSKM.
      /
      **** COPY ARCMSKM.                                        <LA4407><012>
      /
      **** COPY ACMCTRNSKM.                                     <LA4407><012>
      /
      **** COPY T3715REC.                                       <LA4407><012>
      /
           COPY RESNENQSKM.                                             <GBF>
                                                                        <GBF>
           COPY RTRNTRNSKM.
      /
           COPY CLTSSKM.
      /
           COPY DESCSKM.
      /
           COPY ITEMSKM.                                                <012>
      /
           COPY VARCOM.
      /
           COPY SYSERRREC.
      /
           COPY OPSTATSREC.
      /
      ***  COPY SCRNPARAMS.                                             <S9503>
      /
      ***  COPY S6233SCR.                                               <S9503>
      /
           COPY UTRSSKM.                                                <V72L08>
      /                                                                 <V72L08>
           COPY HITSSKM.                                                <V72L08>
           COPY ZTAXENQSKM.                                             <GAPPH1>
      /                                                                 <V72L08>
       LINKAGE SECTION.
      * Screen copybooks are now part of the linkage.                   <S9503>
      /                                                                 <S9503>
           COPY SCRNPARAMS.                                             <S9503>
      /                                                                 <S9503>
           COPY S6233SCR.                                               <S9503>
      /
           COPY WSSPCOMN.
      /
           COPY WSSPLIFE.
      /
      * Statement now includes screen copybooks.                        <S9503>
       PROCEDURE DIVISION USING WSSP-COMMON-AREA WSSP-USER-AREA         <S9503>
                                               SCRN-SCREEN-PARAMS       <S9503>
                                               S6233-DATA-AREA          <S9503>
                                               S6233-SUBFILE-AREA   .   <S9503>
      /
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

           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'
                GO TO 1090-EXIT.

           MOVE WSAA-PROG              TO SYSR-SUBRNAME.
           MOVE WSSP-BATCHKEY          TO WSAA-BATCKEY.
      **** MOVE SPACES                 TO S6233-DATA-AREA.              <GAPPH1>
      **** MOVE SPACES                 TO S6233-SUBFILE-AREA.           <GAPPH1>
                                                                        <GAPPH1>
           INITIALIZE                  S6233-DATA-AREA.                 <GAPPH1>
           INITIALIZE                  S6233-SUBFILE-AREA.              <GAPPH1>

           MOVE SCLR                   TO SCRN-FUNCTION.
           CALL 'S6233IO' USING SCRN-SCREEN-PARAMS
                                S6233-DATA-AREA
                                S6233-SUBFILE-AREA.
           IF SCRN-STATUZ NOT = O-K
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.
           MOVE 1                      TO SCRN-SUBFILE-RRN.

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
           MOVE CHDRENQ-CHDRNUM        TO S6233-CHDRNUM.
           MOVE CHDRENQ-CNTTYPE        TO S6233-CNTTYPE.
           MOVE CHDRENQ-CNTCURR        TO S6233-CNTCURR.
           MOVE CHDRENQ-REGISTER       TO S6233-REGISTER.
      *
      * Obtain the Life Assured and Joint Life Assured, if they exist.
      * The BEGN function is used to retrieve the first Life for the
      * contract in case life '01' has been deleted.
      *
      *    MOVE CHDRENQ-CHDRCOY        TO LIFEENQ-CHDRCOY.              <V71L01>
      *    MOVE CHDRENQ-CHDRNUM        TO LIFEENQ-CHDRNUM.              <V71L01>
      *    MOVE '01'                   TO LIFEENQ-LIFE.                 <V71L01>
      *    MOVE '00'                   TO LIFEENQ-JLIFE.                <V71L01>
      *    MOVE BEGN                   TO LIFEENQ-FUNCTION.             <V71L01>
      *    CALL 'LIFEENQIO'            USING LIFEENQ-PARAMS.            <V71L01>
      *    IF LIFEENQ-STATUZ           NOT = O-K                        <V71L01>
      *         MOVE LIFEENQ-PARAMS    TO SYSR-PARAMS                   <V71L01>
      *         PERFORM 600-FATAL-ERROR.                                <V71L01>
      *
      *    MOVE LIFEENQ-LIFCNUM        TO S6233-LIFENUM                 <V71L01>
           MOVE CHDRENQ-CHDRCOY        TO LIFELNB-CHDRCOY.              <V71L01>
           MOVE CHDRENQ-CHDRNUM        TO LIFELNB-CHDRNUM.              <V71L01>
           MOVE '01'                   TO LIFELNB-LIFE.                 <V71L01>
           MOVE '00'                   TO LIFELNB-JLIFE.                <V71L01>
           MOVE BEGN                   TO LIFELNB-FUNCTION.             <V71L01>
           CALL 'LIFELNBIO'            USING LIFELNB-PARAMS.            <V71L01>
           IF LIFELNB-STATUZ           NOT = O-K                        <V71L01>
                MOVE LIFELNB-PARAMS    TO SYSR-PARAMS                   <V71L01>
                PERFORM 600-FATAL-ERROR.                                <V71L01>
      *                                                                 <V71L01>
           MOVE LIFELNB-LIFCNUM        TO S6233-LIFENUM                 <V71L01>
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
           MOVE WSSP-LONGCONFNAME      TO S6233-LIFENAME.
      *
      * Check for the existence of Joint Life details.
      *
      *    MOVE '01'                   TO LIFEENQ-JLIFE.                <V71L01>
      *    MOVE READR                  TO LIFEENQ-FUNCTION.             <V71L01>
      *    CALL 'LIFEENQIO'            USING LIFEENQ-PARAMS.            <V71L01>
           MOVE '01'                   TO LIFELNB-JLIFE.                <V71L01>
           MOVE READR                  TO LIFELNB-FUNCTION.             <V71L01>
           CALL 'LIFELNBIO'            USING LIFELNB-PARAMS.            <V71L01>
      ***  IF   LIFEENQ-STATUZ         NOT = O-K                        <010>
      ***       MOVE 'NONE'            TO S6233-JLIFE                   <010>
      ***                                 S6233-JLIFENAME               <010>
      ***  ELSE                                                         <010>
      *    IF   LIFEENQ-STATUZ             = O-K                        <V71L01>
      *         MOVE LIFEENQ-LIFCNUM   TO S6233-JLIFE                   <V71L01>
      *                                   CLTS-CLNTNUM                  <V71L01>
      *         MOVE WSSP-FSUCO        TO CLTS-CLNTCOY                  <V71L01>
      *         MOVE 'CN'              TO CLTS-CLNTPFX                  <V71L01>
      *         MOVE READR             TO CLTS-FUNCTION                 <V71L01>
      *         CALL 'CLTSIO'          USING CLTS-PARAMS                <V71L01>
      *         IF CLTS-STATUZ         NOT = O-K                        <V71L01>
      *              MOVE CLTS-PARAMS  TO SYSR-PARAMS                   <V71L01>
      *              PERFORM 600-FATAL-ERROR                            <V71L01>
      *         ELSE                                                    <V71L01>
      *              PERFORM PLAINNAME                                  <V71L01>
      *              MOVE WSSP-LONGCONFNAME                             <V71L01>
      *                                TO S6233-JLIFENAME.              <V71L01>
           IF   LIFELNB-STATUZ             = O-K                        <V71L01>
                MOVE LIFELNB-LIFCNUM   TO S6233-JLIFE                   <V71L01>
                                          CLTS-CLNTNUM                  <V71L01>
                MOVE WSSP-FSUCO        TO CLTS-CLNTCOY                  <V71L01>
                MOVE 'CN'              TO CLTS-CLNTPFX                  <V71L01>
                MOVE READR             TO CLTS-FUNCTION                 <V71L01>
                CALL 'CLTSIO'          USING CLTS-PARAMS                <V71L01>
                IF CLTS-STATUZ         NOT = O-K                        <V71L01>
                     MOVE CLTS-PARAMS  TO SYSR-PARAMS                   <V71L01>
                     PERFORM 600-FATAL-ERROR                            <V71L01>
                ELSE                                                    <V71L01>
                     PERFORM PLAINNAME                                  <V71L01>
                     MOVE WSSP-LONGCONFNAME                             <V71L01>
                                       TO S6233-JLIFENAME               <V71L01>
                END-IF                                                  <V71L01>
           END-IF.                                                      <V71L01>
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
                MOVE ALL '?'           TO S6233-CTYPEDES
           ELSE
                MOVE DESC-LONGDESC     TO S6233-CTYPEDES.
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
                MOVE ALL '?'           TO S6233-CHDRSTATUS
           ELSE
                MOVE DESC-SHORTDESC    TO S6233-CHDRSTATUS.
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
                MOVE ALL '?'           TO S6233-PREMSTATUS
           ELSE
                MOVE DESC-SHORTDESC    TO S6233-PREMSTATUS.
      *
      *    Read the first PTRNENQ record for the contract.
      *
           MOVE SPACES                 TO PTRNENQ-DATA-AREA.
           MOVE WSSP-COMPANY           TO PTRNENQ-CHDRCOY.
           MOVE CHDRENQ-CHDRNUM        TO PTRNENQ-CHDRNUM
      *    MOVE VRCM-MAX-DATE          TO PTRNENQ-PTRNEFF.              <003>
      *    MOVE ZEROS                  TO PTRNENQ-TRANNO.               <003>
           MOVE ZEROS                  TO PTRNENQ-PTRNEFF.              <003>
           MOVE 99999                  TO PTRNENQ-TRANNO.               <003>
           MOVE BEGN                   TO PTRNENQ-FUNCTION.
           CALL 'PTRNENQIO'         USING PTRNENQ-PARAMS.
           IF   PTRNENQ-STATUZ      NOT = O-K
            AND PTRNENQ-STATUZ      NOT = ENDP
                MOVE PTRNENQ-PARAMS    TO SYSR-PARAMS
                PERFORM 600-FATAL-ERROR.

           PERFORM 1100-PROCESS-PTRN
             UNTIL PTRNENQ-CHDRCOY  NOT = WSSP-COMPANY
                OR PTRNENQ-CHDRNUM  NOT = CHDRENQ-CHDRNUM
                OR PTRNENQ-STATUZ       = ENDP.

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
      *
      *    Sections performed from the 1000 section above.
      *      (including subfile load section)
      *
           COPY CONFNAME.
      /
       1100-PROCESS-PTRN SECTION.
       1100-PARA.
      *                                                                 <001>
      *    Do not select the record if the Transaction Code = B216      <001>
      *                                                                 <001>
<005>**    IF PTRNENQ-BATCTRCDE        = "B216"                         <007>
<005>**       GO 1120-READ-NEXT.                                        <007>

      ****                                                         <008><011>
      **Do not show Valid Flag '2' PTRNs on the subfile            <008><011>
      ****                                                         <008><011>
      **** IF PTRNENQ-VALIDFLAG        = '2'                       <008><011>
      ****    GO TO 1120-READ-NEXT.                                <008><011>
      ****                                                              <011>
      *
      *    Set up the Transaction details from the PTRNENQ record.
      *
           MOVE SPACES                 TO S6233-SUBFILE-FIELDS.         <001>
           MOVE SPACES                 TO S6233-SELECT-OUT (PR).
           MOVE SPACES                 TO S6233-SELECT-OUT (ND).        <011>
           MOVE SPACES                 TO S6233-HFLAG                   <GBF>
                                          S6233-HREASON                 <GBF>
                                          S6233-HTXDATE                 <GBF>
           MOVE PTRNENQ-PTRNEFF        TO S6233-EFFDATE.
           MOVE PTRNENQ-BATCTRCDE      TO S6233-TRCODE.
           MOVE PTRNENQ-DATESUB        TO S6233-DATESUB.                <V71L01>
           IF PTRNENQ-CRTUSER      NOT = SPACE                          <V71L01>
              MOVE PTRNENQ-CRTUSER     TO S6233-CRTUSER                 <V71L01>
           ELSE                                                         <V71L01>
              MOVE PTRNENQ-USER-PROFILE                                 <V71L01>
                                       TO S6233-CRTUSER                 <V71L01>
           END-IF.                                                      <V71L01>
      * Show User Profile always:                                       <GAPPH2>
           MOVE PTRNENQ-USER-PROFILE   TO S6233-CRTUSER.                <GAPPH2>
           MOVE PTRNENQ-TRANNO         TO S6233-TRANNO.                 <001>
           MOVE SPACES                 TO S6233-FILLH.                  <011>
           MOVE SPACES                 TO S6233-FILLL.                  <011>
      *
      *    Obtain the Transaction Code description from T1688.
      *
           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE 'IT'                   TO DESC-DESCPFX.
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.
           MOVE T1688                  TO DESC-DESCTABL.
           MOVE PTRNENQ-BATCTRCDE      TO DESC-DESCITEM.
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
                MOVE ALL '?'           TO S6233-TRANDESC
           ELSE
                MOVE DESC-LONGDESC     TO S6233-TRANDESC.
      *                                                                 <GBF>
      *  Obtain the actual transaction date from the date time stamp    <GBF>
      *                                                                 <GBF>
           MOVE PTRNENQ-DATIME         TO WSAA-DATIME.                  <GBF>
           MOVE WSAA-YEAR              TO WSAA-DF-YY                    <GBF>
           MOVE WSAA-MONTH             TO WSAA-DF-MM                    <GBF>
           MOVE WSAA-DAY               TO WSAA-DF-DD                    <GBF>
           MOVE WSAA-TX-DATE           TO S6233-HTXDATE.                <GBF>
      *                                                                 <GBF>
      *  Check to see if a reason for the transaction exists            <GBF>
      *                                                                 <GBF>
           PERFORM 1300-REASON.                                         <GBF>
      *                                                                 <011>
      * If PTRN was a Validflag '2', then don't allow selection,        <011>
      *   set fields on either side of TRANNO, FILLH & FILLL to         <011>
      *   be '***'.                                                     <011>
      *                                                                 <011>
           IF PTRNENQ-VALIDFLAG        = '2'                            <011>
      ****     MOVE 'Y'                TO S6233-SELECT-OUT(PR)  <CAS1.0><011>
      ****     MOVE 'Y'                TO S6233-SELECT-OUT(ND)  <CAS1.0><011>
               MOVE '***'              TO S6233-FILLH                   <011>
               MOVE '***'              TO S6233-FILLL                   <011>
      ****     GO TO 1110-WRITE-SUBFILE                         <CAS1.0><011>
           END-IF.                                                      <011>
                                                                        <PHE003>
      *                                                                 <PHE003>
      *--  Show APL in case of APL processing                           <PHE003>
      *                                                                 <PHE003>
           IF  PTRNENQ-PRTFLG          = 'AO'                           <PHE003>
               MOVE 'APL'              TO S6233-FILLL                   <PHE003>
           END-IF.                                                      <PHE003>
      *
      *---------------------------------------------------------------* <001>
      *    Attempt to read the ACMVTRN data-set.                      * <001>
      *    If no records are found, set the protect for select to "ON"* <001>
      *---------------------------------------------------------------* <001>
      *                                                                 <001>
           MOVE SPACES                 TO ACMVTRN-DATA-AREA.            <001>
           MOVE PTRNENQ-CHDRCOY        TO ACMVTRN-RLDGCOY.              <001>
      *    MOVE PTRNENQ-BATCTRCDE      TO ACMVTRN-BATCTRCDE.            <004>
           MOVE PTRNENQ-CHDRNUM        TO ACMVTRN-RDOCNUM.              <001>
           MOVE PTRNENQ-TRANNO         TO ACMVTRN-TRANNO.               <001>
           MOVE BEGN                   TO ACMVTRN-FUNCTION.             <001>
           CALL 'ACMVTRNIO'         USING ACMVTRN-PARAMS.               <001>
           IF   ACMVTRN-STATUZ      NOT = O-K                           <001>
            AND ACMVTRN-STATUZ      NOT = ENDP                          <001>
                MOVE ACMVTRN-PARAMS    TO SYSR-PARAMS                   <001>
                PERFORM 600-FATAL-ERROR.                                <001>
                                                                        <001>
            IF ACMVTRN-RLDGCOY     NOT = PTRNENQ-CHDRCOY                <001>
      *     OR ACMVTRN-BATCTRCDE   NOT = PTRNENQ-BATCTRCDE              <004>
            OR ACMVTRN-RDOCNUM     NOT = PTRNENQ-CHDRNUM                <001>
            OR ACMVTRN-TRANNO      NOT = PTRNENQ-TRANNO                 <001>
            OR ACMVTRN-STATUZ          = ENDP                           <001>
      *                                                                 <009>
      *  continue to check for RTRNs                                    <009>
      ***      NEXT SENTENCE                                            <012>
      *******  MOVE "Y"               TO S6233-SELECT-OUT(PR)      <009><001>
      ****     PERFORM 1200-CHECK-ARCHIVES                      <LA4407><012>
      ****     IF S6233-HSELECT     = 'Y'                       <LA4407><012>
      ****        GO TO 1110-WRITE-SUBFILE                      <LA4407><012>
      ****     END-IF                                           <LA4407><012>
                NEXT SENTENCE                                           <LA4407>
            ELSE                                                        <001>
                MOVE 'Y'               TO S6233-HFLAG                   <GBF>
                MOVE ACMVTRN-DATA-LOCATION                              <ARCH01>
                                       TO S6233-DATALOC                 <ARCH01>
                GO 1110-WRITE-SUBFILE                                   <001>
            END-IF.                                                     <012>
      *                                                                 <001>
      *---------------------------------------------------------------* <001>
      *    Attempt to read the RTRNTRN data-set.                      * <001>
      *    If no records are found, set the protect for select to "ON"* <001>
      *---------------------------------------------------------------* <001>
      *                                                                 <001>
           MOVE SPACES                 TO RTRNTRN-DATA-AREA.            <001>
           MOVE PTRNENQ-CHDRCOY        TO RTRNTRN-RLDGCOY.              <001>
      *    MOVE PTRNENQ-BATCTRCDE      TO RTRNTRN-BATCTRCDE.            <004>
           MOVE PTRNENQ-CHDRNUM        TO RTRNTRN-RDOCNUM.              <001>
           MOVE PTRNENQ-TRANNO         TO RTRNTRN-TRANNO.               <001>
           MOVE BEGN                   TO RTRNTRN-FUNCTION.             <001>
           CALL 'RTRNTRNIO'         USING RTRNTRN-PARAMS.               <001>
           IF   RTRNTRN-STATUZ      NOT = O-K                           <001>
            AND RTRNTRN-STATUZ      NOT = ENDP                          <001>
                MOVE RTRNTRN-PARAMS    TO SYSR-PARAMS                   <001>
                PERFORM 600-FATAL-ERROR.                                <001>
                                                                        <001>
            IF RTRNTRN-RLDGCOY      NOT = PTRNENQ-CHDRCOY               <001>
      *     OR RTRNTRN-BATCTRCDE    NOT = PTRNENQ-BATCTRCDE             <004>
            OR RTRNTRN-RDOCNUM      NOT = PTRNENQ-CHDRNUM               <001>
            OR RTRNTRN-TRANNO       NOT = PTRNENQ-TRANNO                <001>
            OR RTRNTRN-STATUZ           = ENDP                          <001>
      ****     MOVE "Y"             TO S6233-SELECT-OUT(PR).       <GBF><001>
               MOVE 'Y'             TO S6233-SELECT-OUT(PR)             <GBF>
            ELSE                                                        <GBF>
                MOVE 'Y'               TO S6233-HFLAG                   <GBF>
            END-IF.                                                     <GBF>
            MOVE RTRNTRN-DATA-LOCATION                                  <ARCH01>
                                       TO S6233-DATALOC.                <ARCH01>
      *                                                                 <001>
       1110-WRITE-SUBFILE.                                              <001>
      *
      *    Write the subfile record.
      *
      * TaxInvoice Ref:                                                 <GAPPH1>
           PERFORM 1200-GET-TAX-INVOICE-INFO.                           <GAPPH1>
                                                                        <GAPPH1>
           MOVE SADD                   TO SCRN-FUNCTION
           CALL 'S6233IO'           USING SCRN-SCREEN-PARAMS
                                          S6233-DATA-AREA
                                          S6233-SUBFILE-AREA
           IF SCRN-STATUZ           NOT = O-K
                MOVE SCRN-STATUZ       TO SYSR-STATUZ
                PERFORM 600-FATAL-ERROR.
      *                                                                 <001>
       1120-READ-NEXT.                                                  <001>
      *
      *    Read the next PTRNENQ record.
      *
           MOVE NEXTR                  TO PTRNENQ-FUNCTION.
           CALL 'PTRNENQIO'         USING PTRNENQ-PARAMS.
           IF   PTRNENQ-STATUZ      NOT = O-K
            AND PTRNENQ-STATUZ      NOT = ENDP
                MOVE PTRNENQ-PARAMS    TO SYSR-PARAMS
                PERFORM 600-FATAL-ERROR.

       1100-EXIT.
           EXIT.
      /                                                                 <GAPPH1>
       1200-GET-TAX-INVOICE-INFO SECTION.                               <GAPPH1>
      ***********************************                               <GAPPH1>
       1201-START.                                                      <GAPPH1>
      *                                                                 <GAPPH1>
           MOVE SPACES                 TO WSAA-TAX-INVOICE.             <GAPPH1>
                                                                        <GAPPH1>
           INITIALIZE                  ZTAXENQ-PARAMS.                  <GAPPH1>
           MOVE PTRNENQ-CHDRNUM        TO ZTAXENQ-CHDRNUM.              <GAPPH1>
           MOVE PTRNENQ-TRANNO         TO ZTAXENQ-TRANNO.               <GAPPH1>
           MOVE PTRNENQ-BATCTRCDE      TO ZTAXENQ-TRANCD.               <GAPPH1>
           MOVE ZTAXENQREC             TO ZTAXENQ-FORMAT.               <GAPPH1>
           MOVE READR                  TO ZTAXENQ-FUNCTION.             <GAPPH1>
                                                                        <GAPPH1>
           CALL 'ZTAXENQIO'            USING ZTAXENQ-PARAMS.            <GAPPH1>
                                                                        <GAPPH1>
           IF ZTAXENQ-STATUZ           NOT = O-K                        <GAPPH1>
           AND                         NOT = MRNF                       <GAPPH1>
               MOVE ZTAXENQ-PARAMS     TO SYSR-PARAMS                   <GAPPH1>
               PERFORM 600-FATAL-ERROR                                  <GAPPH1>
           END-IF.                                                      <GAPPH1>
                                                                        <GAPPH1>
           IF ZTAXENQ-STATUZ           = O-K                            <GAPPH1>
               MOVE ZTAXENQ-INVSEQ     TO WSAA-INVSEQ                   <GAPPH1>
               STRING                                                   <GAPPH1>
                   ZTAXENQ-ZCODE ' ' WSAA-INVSEQ                        <GAPPH1>
                                       DELIMITED BY SIZE                <GAPPH1>
                                       INTO WSAA-TAX-INVOICE            <GAPPH1>
               END-STRING                                               <GAPPH1>
           ELSE                                                         <GAPPH1>
               MOVE SPACES             TO WSAA-TAX-INVOICE              <GAPPH1>
           END-IF.                                                      <GAPPH1>
                                                                        <GAPPH1>
           MOVE WSAA-TAX-INVOICE       TO S6233-INVCREF.                <GAPPH1>
                                                                        <GAPPH1>
           IF ZTAXENQ-STFLAG           NOT = 'A'                        <GAPPH1>
           AND ZTAXENQ-STFLAG          NOT = SPACES                     <GAPPH1>
               MOVE WSAA-CANC-CHR      TO S6233-LCAN                    <GAPPH1>
           ELSE                                                         <GAPPH1>
               MOVE SPACES             TO S6233-LCAN                    <GAPPH1>
           END-IF.                                                      <GAPPH1>
      *                                                                 <GAPPH1>
       1209-EXIT.                                                       <GAPPH1>
           EXIT.                                                        <GAPPH1>
                                                                        <GAPPH1>
      ****                                                              <LA4407>
      *1200-CHECK-ARCHIVES SECTION.                             <LA4407><012>
      *****************************                                     <012>
      *1200-PARA.                                               <LA4407><012>
      ****                                                      <LA4407><012>
      **** MOVE SPACES                  TO S6233-HSELECT.       <LA4407><012>
      ****                                                      <LA4407><012>
      **** MOVE SPACES                  TO ITEM-PARAMS.         <LA4407><012>
      **** MOVE 'IT'                    TO ITEM-ITEMPFX.        <LA4407><012>
      **** MOVE WSSP-COMPANY            TO ITEM-ITEMCOY.        <LA4407><012>
      **** MOVE T3715                   TO ITEM-ITEMTABL.       <LA4407><012>
      **** MOVE 'ACMV'                  TO ITEM-ITEMITEM.       <LA4407><012>
      **** MOVE READR                   TO ITEM-FUNCTION.       <LA4407><012>
      ****                                                      <LA4407><012>
      **** CALL 'ITEMIO' USING ITEM-PARAMS.                     <LA4407><012>
      ****                                                      <LA4407><012>
      **** IF ITEM-STATUZ          NOT = O-K                    <LA4407><012>
      ****    MOVE ITEM-PARAMS         TO SYSR-PARAMS           <LA4407><012>
      ****    PERFORM 600-FATAL-ERROR                           <LA4407><012>
      **** END-IF.                                              <LA4407><012>
      ****                                                      <LA4407><012>
      **** MOVE ITEM-GENAREA           TO T3715-T3715-REC.      <LA4407><012>
      ****                                                      <LA4407><012>
      **** MOVE SPACES                 TO ARCM-PARAMS.          <LA4407><012>
      **** MOVE 'ACMV'                 TO ARCM-FILE-NAME.       <LA4407><012>
      **** MOVE READR                  TO ARCM-FUNCTION.        <LA4407><012>
      **** MOVE ARCMREC                TO ARCM-FORMAT.          <LA4407><012>
      ****                                                      <LA4407><012>
      **** CALL 'ARCMIO'               USING ARCM-PARAMS.       <LA4407><012>
      ****                                                      <LA4407><012>
      **** IF ARCM-STATUZ              NOT = O-K                <LA4407><012>
      ****    AND ARCM-STATUZ          NOT = MRNF               <LA4407><012>
      ****     MOVE ARCM-PARAMS        TO SYSR-PARAMS           <LA4407><012>
      ****     MOVE ARCM-STATUZ        TO SYSR-STATUZ           <LA4407><012>
      ****     PERFORM 600-FATAL-ERROR                          <LA4407><012>
      **** END-IF.                                              <LA4407><012>
      **                                                        <LA4407><012>
      **If the PTRN being processed has an Accounting Period Greater    <LA4407>
      **than the last period Archived then there is no need to  <LA4407><012>
      **attempt to read the Optical Device.                     <LA4407><012>
      ****                                                      <LA4407><012>
      **** IF ARCM-STATUZ              = MRNF                   <LA4407><012>
      ****    MOVE ZEROES              TO WSBB-PERIOD           <LA4407><012>
      **** ELSE                                                 <LA4407><012>
      ****    MOVE ARCM-ACCTYR         TO WSBB-ACTYR            <LA4407><012>
      ****    MOVE ARCM-ACCTMNTH       TO WSBB-ACTMN            <LA4407><012>
      **** END-IF.                                              <LA4407><012>
      ****                                                      <LA4407><012>
      **** MOVE PTRNENQ-BATCACTYR      TO WSAA-ACTYR.           <LA4407><012>
      **** MOVE PTRNENQ-BATCACTMN      TO WSAA-ACTMN.           <LA4407><012>
      **** IF   WSAA-PERIOD             > WSBB-PERIOD           <LA4407><012>
      ****      GO TO 1290-EXIT                                 <LA4407><012>
      **** END-IF.                                              <LA4407><012>
      ****                                                      <LA4407><012>
      **** IF   T3715-ENQFLAG          NOT = SPACES             <LA4407><012>
      ****                                                      <LA4407><012>
      ****      MOVE SPACES            TO ACMCTRN-PARAMS        <LA4407><012>
      ****      MOVE PTRNENQ-CHDRCOY   TO ACMCTRN-RLDGCOY       <LA4407><012>
      ****      MOVE PTRNENQ-CHDRNUM   TO ACMCTRN-RDOCNUM       <LA4407><012>
      ****      MOVE PTRNENQ-TRANNO    TO ACMCTRN-TRANNO        <LA4407><012>
      ****      MOVE BEGN              TO ACMCTRN-FUNCTION      <LA4407><012>
      ****      CALL 'ACMCTRNIO'         USING ACMCTRN-PARAMS   <LA4407><012>
      ****      IF  ACMCTRN-STATUZ     NOT = O-K                <LA4407><012>
      ****      AND ACMCTRN-STATUZ     NOT = ENDP               <LA4407><012>
      ****         MOVE ACMCTRN-PARAMS TO SYSR-PARAMS           <LA4407><012>
      ****         PERFORM 600-FATAL-ERROR                      <LA4407><012>
      ****      END-IF                                          <LA4407><012>
      ****                                                      <LA4407><012>
      ****      IF ACMCTRN-RLDGCOY     NOT = PTRNENQ-CHDRCOY    <LA4407><012>
      ****      OR ACMCTRN-RDOCNUM     NOT = PTRNENQ-CHDRNUM    <LA4407><012>
      ****      OR ACMCTRN-TRANNO      NOT = PTRNENQ-TRANNO     <LA4407><012>
      ****      OR ACMCTRN-STATUZ      = ENDP                   <LA4407><012>
      ****          MOVE SPACES            TO S6233-HSELECT     <LA4407><012>
      ****      ELSE                                            <LA4407><012>
      ****          MOVE 'I'               TO S6233-HSELECT     <LA4407><012>
      ****          MOVE RTRNTRN-DATA-LOCATION                  <LA4407><ARCH01>
      ****                                 TO S6233-DATALOC     <LA4407><ARCH01>
      ****     END-IF                                           <LA4407><012>
      ****  END-IF.                                             <LA4407><012>
      ****                                                      <LA4407><012>
      **** IF   T3715-COLDFLAG         NOT = SPACES             <LA4407><012>
      ****      MOVE 'Y'               TO S6233-HSELECT         <LA4407><012>
      **** END-IF.                                              <LA4407><012>
      ****                                                      <LA4407><012>
      *1290-EXIT.                                               <LA4407><012>
      **** EXIT.                                                <LA4407><012>
      /                                                                 <GBF>
       1300-REASON SECTION.                                             <GBF>
      *********************                                             <GBF>
      *                                                                 <GBF>
       1300-START.                                                      <GBF>
      *                                                                 <GBF>
      *  Read the reason file to see if details exist for the           <GBF>
      *  transaction, if yes move the details to the Hidden screen      <GBF>
      *  field reason                                                   <GBF>
      *                                                                 <GBF>
           MOVE SPACES                 TO RESNENQ-DATA-AREA.            <GBF>
           MOVE WSSP-COMPANY           TO RESNENQ-CHDRCOY.              <GBF>
           MOVE CHDRENQ-CHDRNUM        TO RESNENQ-CHDRNUM               <GBF>
           MOVE PTRNENQ-BATCTRCDE      TO RESNENQ-TRANCDE.              <GBF>
           MOVE PTRNENQ-TRANNO         TO RESNENQ-TRANNO.               <GBF>
           MOVE READR                  TO RESNENQ-FUNCTION.             <GBF>
           MOVE RESNENQREC             TO RESNENQ-FORMAT.               <GBF>
      *                                                                 <GBF>
           CALL 'RESNENQIO'         USING RESNENQ-PARAMS.               <GBF>
      *                                                                 <GBF>
           IF  RESNENQ-STATUZ       NOT = O-K                           <GBF>
           AND RESNENQ-STATUZ       NOT = MRNF                          <GBF>
               MOVE RESNENQ-STATUZ     TO SYSR-STATUZ                   <GBF>
               MOVE RESNENQ-PARAMS     TO SYSR-PARAMS                   <GBF>
               PERFORM 600-FATAL-ERROR                                  <GBF>
           END-IF.                                                      <GBF>
      *                                                                 <GBF>
           IF  RESNENQ-STATUZ           = O-K                           <GBF>
               MOVE RESNENQ-RESNDESC   TO S6233-HREASON                 <GBF>
           ELSE                                                         <GBF>
               MOVE SPACES             TO S6233-HREASON                 <GBF>
           END-IF.                                                      <GBF>
      *                                                                 <GBF>
       1300-EXIT.                                                       <GBF>
           EXIT.                                                        <GBF>
      /                                                                 <012>
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
      * Skip this section if returning from an optional selection,      <S9503>
      * i.e. check the stack action flag equals '*'.                    <S9503>
      *                                                                 <S9503>
           IF   WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'                  <S9503>
                MOVE O-K               TO WSSP-EDTERROR                 <S9503>
              MOVE 3000                TO WSSP-SECTIONNO                <S9503>
           GO TO PRE-EXIT.                                              <S9503>
                                                                        <S9503>
       2010-SCREEN-IO.                                                  <S9503>
                                                                        <S9503>
           GO TO PRE-EXIT.                                              <S9503>
      *                                                                 <S9503>
       PRE-EXIT.                                                        <S9503>
           EXIT.                                                        <S9503>
      /                                                                 <S9503>
       2000-SCREEN-EDIT SECTION.
      **************************
      *2000-PARA.                                                       <013>
       2010-SCREEN-IO.                                                  <013>
      *    CALL 'S6233IO' USING SCRN-SCREEN-PARAMS                      <S9503>
      *                         S6233-DATA-AREA                         <S9503>
      *                         S6233-SUBFILE-AREA.                     <S9503>
           IF SCRN-STATUZ              = MASM                           <006>
              MOVE ZEROS               TO WSSP-LIFEKEY.                 <006>
      * Screen errors are now handled in the calling program.           <S9503>
      *    PERFORM 200-SCREEN-ERRORS.                                   <S9503>
           MOVE O-K                    TO WSSP-EDTERROR.

           IF   SCRN-STATUZ = 'KILL'
                GO TO 2090-EXIT.
      *
      *    No validation is necessary. Any non-blank character may be
      *    used to select a particular line.
      *
       2010-VALIDATE-SCREEN.                                            <V72L08>
                                                                        <V72L08>
           MOVE 1                      TO SCRN-SUBFILE-RRN.             <V72L08>
                                                                        <V72L08>
       2060-VALIDATE-SUBFILE.                                           <V72L08>
           MOVE SRNCH                  TO SCRN-FUNCTION.                <V72L08>
           CALL 'S6233IO'           USING SCRN-SCREEN-PARAMS            <V72L08>
                                          S6233-DATA-AREA               <V72L08>
                                          S6233-SUBFILE-AREA.           <V72L08>
           IF   SCRN-STATUZ         NOT = O-K                           <V72L08>
                                    AND ENDP                            <V72L08>
                MOVE SCRN-STATUZ       TO SYSR-STATUZ                   <V72L08>
                PERFORM 600-FATAL-ERROR.                                <V72L08>
                                                                        <V72L08>
           PERFORM 2600-VALIDATE-SUBFILE                                <V72L08>
             UNTIL SCRN-STATUZ = ENDP.                                  <V72L08>

       2090-EXIT.
            EXIT.
      /
       2600-VALIDATE-SUBFILE SECTION.                                   <V72L08>
      *******************************                                   <V72L08>
      *                                                                 <V72L08>
       2610-VALIDATION.                                                 <V72L08>
                                                                        <V72L08>
           IF S6233-SELECT              = SPACE                         <V72L08>
               GO TO 2670-UPDATE-ERROR-INDICATORS.                      <V72L08>
      *                                                                 <V72L08>
      *    These options are hard-coded on the screen display. If new   <V72L08>
      *    options are introduced then both the screen and this check   <V72L08>
      *    should be changed.                                           <V72L08>
      *                                                                 <V72L08>
           IF   S6233-SELECT            = '1' OR '2'                    <V72L08>
                NEXT SENTENCE                                           <V72L08>
           ELSE                                                         <V72L08>
                MOVE E005              TO S6233-SELECT-ERR              <V72L08>
                GO TO 2670-UPDATE-ERROR-INDICATORS.                     <V72L08>
                                                                        <V72L08>
           IF   S6233-SELECT        NOT = '2'                           <V72L08>
                GO TO 2670-UPDATE-ERROR-INDICATORS.                     <V72L08>
      *                                                                 <V72L08>
      *    If control reaches this point then option 2 has been chosen  <V72L08>
      *    and the program must check that a UTRS record exists.        <V72L08>
      *    First perform a BEGN  on the selected UTRS record. This will <V72L08>
      *    return the first UTRS record for the contract, life, policy  <V72L08>
      *    and component.                                               <V72L08>
      *                                                                 <V72L08>
           MOVE SPACES                 TO UTRS-DATA-AREA.               <V72L08>
           MOVE BEGN                   TO UTRS-FUNCTION.                <V72L08>
           MOVE WSSP-COMPANY           TO UTRS-CHDRCOY.                 <V72L08>
           MOVE CHDRENQ-CHDRNUM        TO UTRS-CHDRNUM.                 <V72L08>
           MOVE '01'                   TO UTRS-LIFE.                    <V72L08>
           MOVE '01'                   TO UTRS-COVERAGE.                <V72L08>
           MOVE '00'                   TO UTRS-RIDER.                   <V72L08>
           MOVE ZEROS                  TO UTRS-PLAN-SUFFIX.             <V72L08>
           CALL 'UTRSIO'            USING UTRS-PARAMS.                  <V72L08>
           IF   UTRS-STATUZ         NOT = O-K             OR            <V72L08>
                UTRS-CHDRCOY        NOT = WSSP-COMPANY    OR            <V72L08>
                UTRS-CHDRNUM        NOT = CHDRENQ-CHDRNUM OR            <V72L08>
                UTRS-LIFE           NOT = '01'            OR            <V72L08>
                UTRS-COVERAGE       NOT = '01'            OR            <V72L08>
                UTRS-RIDER          NOT = '00'                          <V72L08>
                NEXT SENTENCE                                           <V72L08>
           ELSE                                                         <V72L08>
                GO TO 2670-UPDATE-ERROR-INDICATORS                      <V72L08>
           END-IF.                                                      <V72L08>
                                                                        <V72L08>
           MOVE SPACES                 TO HITS-PARAMS.                  <V72L08>
           MOVE BEGN                   TO HITS-FUNCTION.                <V72L08>
           MOVE WSSP-COMPANY           TO HITS-CHDRCOY.                 <V72L08>
           MOVE CHDRENQ-CHDRNUM        TO HITS-CHDRNUM.                 <V72L08>
           MOVE '01'                   TO HITS-LIFE.                    <V72L08>
           MOVE '01'                   TO HITS-COVERAGE.                <V72L08>
           MOVE '00'                   TO HITS-RIDER.                   <V72L08>
           MOVE ZEROS                  TO HITS-PLAN-SUFFIX.             <V72L08>
                                                                        <V72L08>
           CALL 'HITSIO'               USING HITS-PARAMS.               <V72L08>
                                                                        <V72L08>
           IF HITS-STATUZ              NOT = O-K                        <V72L08>
           OR HITS-CHDRCOY             NOT = WSSP-COMPANY               <V72L08>
           OR HITS-CHDRNUM             NOT = CHDRENQ-CHDRNUM            <V72L08>
           OR HITS-LIFE                NOT = '01'                       <V72L08>
           OR HITS-COVERAGE            NOT = '01'                       <V72L08>
           OR HITS-RIDER               NOT = '00'                       <V72L08>
                MOVE E494              TO S6233-SELECT-ERR.             <V72L08>
                                                                        <V72L08>
                                                                        <V72L08>
       2670-UPDATE-ERROR-INDICATORS.                                    <V72L08>
                                                                        <V72L08>
           IF   S6233-ERROR-SUBFILE NOT = SPACES                        <V72L08>
                MOVE 'Y'               TO WSSP-EDTERROR.                <V72L08>
                                                                        <V72L08>
           MOVE SUPD                   TO SCRN-FUNCTION.                <V72L08>
           CALL 'S6233IO'           USING SCRN-SCREEN-PARAMS            <V72L08>
                                          S6233-DATA-AREA               <V72L08>
                                          S6233-SUBFILE-AREA.           <V72L08>
           IF   SCRN-STATUZ         NOT = O-K                           <V72L08>
                MOVE SCRN-STATUZ       TO SYSR-STATUZ                   <V72L08>
                PERFORM 600-FATAL-ERROR.                                <V72L08>
      *                                                                 <V72L08>
           IF   S6233-ERROR-SUBFILE NOT = SPACES                        <V72L08>
                MOVE 'Y'               TO WSSP-EDTERROR.                <V72L08>
      *                                                                 <V72L08>
       2680-READ-NEXT-MODIFIED-RECORD.                                  <V72L08>
           MOVE SRNCH                  TO SCRN-FUNCTION.                <V72L08>
           CALL 'S6233IO'           USING SCRN-SCREEN-PARAMS            <V72L08>
                                          S6233-DATA-AREA               <V72L08>
                                          S6233-SUBFILE-AREA.           <V72L08>
           IF   SCRN-STATUZ         NOT = O-K                           <V72L08>
                                    AND ENDP                            <V72L08>
                MOVE SCRN-STATUZ       TO SYSR-STATUZ                   <V72L08>
                PERFORM 600-FATAL-ERROR.                                <V72L08>
                                                                        <V72L08>
           ADD 1                       TO WSAA-SUBFCHG.                 <V72L08>
                                                                        <V72L08>
       2690-EXIT.                                                       <V72L08>
            EXIT.                                                       <V72L08>
      /                                                                 <V72L08>
      *****************************************************************
      *     UPDATE DATABASE IF REQUIRED AND LOG TRANSACTION
      *****************************************************************
      *
       3000-UPDATE SECTION.
      **********************
      *
       3010-UPDATE-DATABASE.

      *
      *  No database updates are required.
      *

       3090-EXIT.
            EXIT.
      /
      *****************************************************************
      *     DECIDE WHICH TRANSACTION PROGRAM IS NEXT
      *****************************************************************
      *
       4000-WHERE-NEXT SECTION.
      *************************
       4000-PARA.
           IF   SCRN-STATUZ             = 'KILL'
                MOVE SPACES            TO WSSP-SEC-PROG
                                         (WSSP-PROGRAM-PTR)
                                          WSSP-SEC-ACTN
                                         (WSSP-PROGRAM-PTR)
                GO TO 4090-EXIT.
      *
      *    Release any PTRNENQ record that may have been stored.
      *
           MOVE RLSE                   TO PTRNENQ-FUNCTION.
           CALL 'PTRNENQIO'         USING PTRNENQ-PARAMS.
           IF   PTRNENQ-STATUZ      NOT = O-K
                MOVE PTRNENQ-PARAMS    TO SYSR-PARAMS
                PERFORM 600-FATAL-ERROR.
      *
      *    If returning from a program further down the stack then
      *    bypass the start on the subfile.
      *
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'
                GO TO 4010-BYPASS-START.
      *
      *    Re-start the subfile.
      *
           MOVE O-K                    TO SCRN-STATUZ.
           MOVE SSTRT                  TO SCRN-FUNCTION.
           CALL 'S6233IO'           USING SCRN-SCREEN-PARAMS
                                          S6233-DATA-AREA
                                          S6233-SUBFILE-AREA.
           IF   SCRN-STATUZ         NOT = O-K
                                    AND ENDP
                MOVE SCRN-STATUZ       TO SYSR-STATUZ
                PERFORM 600-FATAL-ERROR.

       4010-BYPASS-START.

           IF S6233-SELECT = SPACES
           PERFORM 4100-READ-SUBFILE
             UNTIL S6233-SELECT     NOT = SPACES
                OR SCRN-STATUZ          = ENDP.

      *  Nothing pressed at all, end working
      *
           IF   SCRN-STATUZ             = ENDP
               AND WSSP-SEC-ACTN(WSSP-PROGRAM-PTR) = SPACES
                MOVE SPACES            TO WSSP-SEC-PROG
                                         (WSSP-PROGRAM-PTR)
                GO TO 4090-EXIT.


      *
      * All requests services,
      *
           IF  SCRN-STATUZ             = ENDP
            AND WSSP-SEC-ACTN(WSSP-PROGRAM-PTR) NOT = SPACES
                   MOVE ' '        TO WSSP-SEC-ACTN(WSSP-PROGRAM-PTR)
                   GO TO 4090-EXIT.
      *                                                                 <V72L08>
           IF   SCRN-STATUZ             = ENDP                          <V72L08>
                MOVE 0             TO WSAA-SUBFCHG                      <V72L08>
                GO TO 4085-NEXT-PROGRAM.                                <V72L08>
                                                                        <V72L08>
           COMPUTE SUB1                 =  WSSP-PROGRAM-PTR + 1.        <V72L08>
           MOVE 1                      TO SUB2.                         <V72L08>
           PERFORM 4200-SAVE-PROGRAM-STACK 8 TIMES.                     <V72L08>
           MOVE '*'                    TO WSSP-SEC-ACTN                 <V72L08>
                                         (WSSP-PROGRAM-PTR).            <V72L08>
      *
      * Blank out action to ensure it is not processed again
      *
      **** MOVE SPACE                  TO S6233-SELECT.                 <V72L08>

      *
      *    Read and store the selected PTRNENQ record.
      *
           MOVE SPACES                 TO PTRNENQ-DATA-AREA.
           MOVE WSSP-COMPANY           TO PTRNENQ-CHDRCOY.
           MOVE CHDRENQ-CHDRNUM        TO PTRNENQ-CHDRNUM
           MOVE S6233-EFFDATE          TO PTRNENQ-PTRNEFF.
           MOVE S6233-TRCODE           TO PTRNENQ-BATCTRCDE.            <002>
           MOVE S6233-TRANNO           TO PTRNENQ-TRANNO.               <001>
           MOVE READS                  TO PTRNENQ-FUNCTION.
           CALL 'PTRNENQIO'         USING PTRNENQ-PARAMS.
           IF   PTRNENQ-STATUZ      NOT = O-K
                MOVE PTRNENQ-PARAMS    TO SYSR-PARAMS
                PERFORM 600-FATAL-ERROR.
                                                                        <V72L08>
           MOVE SPACES                 TO WSSP-UNIT-TYPE.               <V72L08>
                                                                        <V72L08>
           IF S6233-SELECT            = '2'                             <V72L08>
                PERFORM 4800-UNIT-ENQUIRY                               <V72L08>
                MOVE WSSP-COMPANY           TO GENS-COMPANY             <V72L08>
                MOVE WSAA-PROG              TO GENS-PROG-IN             <V72L08>
                MOVE WSKY-BATC-BATCTRCDE    TO GENS-TRANSACT            <V72L08>
                MOVE 'A'                    TO GENS-FUNCTION            <V72L08>
                CALL 'GENSSW'            USING GENS-GENSSW-REC          <V72L08>
                IF   GENS-STATUZ         NOT = O-K                      <V72L08>
                                    AND NOT = MRNF                      <V72L08>
                    MOVE GENS-STATUZ       TO SYSR-STATUZ               <V72L08>
                    PERFORM 600-FATAL-ERROR                             <V72L08>
                END-IF                                                  <V72L08>
                                                                        <V72L08>
      * If an entry on T1675 was not found by Genswch, redisplay the    <V72L08>
      *   the screen with an error.                                     <V72L08>
                                                                        <V72L08>
               IF   GENS-STATUZ            = MRNF                       <V72L08>
                    GO TO 4090-EXIT                                     <V72L08>
               END-IF                                                   <V72L08>
      *                                                                 <V72L08>
               COMPUTE SUB1            =  WSSP-PROGRAM-PTR + 1          <V72L08>
               MOVE 1                 TO SUB2                           <V72L08>
               PERFORM 4300-LOAD-PROGRAM-STACK 8 TIMES                  <V72L08>
               GO TO 4080-NEXT-PROGRAM                                  <V72L08>
           END-IF.                                                      <V72L08>
                                                                        <V72L08>
           IF S6233-SELECT            = '1'                             <V72L08>
                MOVE WSSP-COMPANY           TO GENS-COMPANY             <V72L08>
                MOVE WSAA-PROG              TO GENS-PROG-IN             <V72L08>
                MOVE WSKY-BATC-BATCTRCDE    TO GENS-TRANSACT            <V72L08>
                MOVE 'B'                    TO GENS-FUNCTION            <V72L08>
                CALL 'GENSSW'            USING GENS-GENSSW-REC          <V72L08>
                IF   GENS-STATUZ         NOT = O-K                      <V72L08>
                                    AND NOT = MRNF                      <V72L08>
                    MOVE GENS-STATUZ       TO SYSR-STATUZ               <V72L08>
                    PERFORM 600-FATAL-ERROR                             <V72L08>
                END-IF                                                  <V72L08>
                                                                        <V72L08>
      * If an entry on T1675 was not found by Genswch, redisplay the    <V72L08>
      *   the screen with an error.                                     <V72L08>
      *                                                                 <V72L08>
               IF   GENS-STATUZ            = MRNF                       <V72L08>
                    GO TO 4090-EXIT                                     <V72L08>
               END-IF                                                   <V72L08>
      *                                                                 <V72L08>
               COMPUTE SUB1            =  WSSP-PROGRAM-PTR + 1          <V72L08>
               MOVE 1                 TO SUB2                           <V72L08>
               PERFORM 4300-LOAD-PROGRAM-STACK 8 TIMES                  <V72L08>
               GO TO 4080-NEXT-PROGRAM                                  <V72L08>
           END-IF.                                                      <V72L08>
                                                                        <V72L08>
      **** MOVE '*'                    TO WSSP-SEC-ACTN                 <V72L08>
      ****                               (WSSP-PROGRAM-PTR).            <V72L08>
      **** MOVE S6233-HSELECT          TO WSSP-UPDATE-FLAG.     <V72L08><012>
      **** ADD 1                       TO WSSP-PROGRAM-PTR.             <V72L08>

       4080-NEXT-PROGRAM.                                               <V72L08>
           MOVE SPACE                  TO S6233-SELECT.                 <V72L08>
                                                                        <V72L08>
           MOVE SUPD                   TO SCRN-FUNCTION.                <V72L08>
           CALL 'S6233IO'           USING SCRN-SCREEN-PARAMS            <V72L08>
                                          S6233-DATA-AREA               <V72L08>
                                          S6233-SUBFILE-AREA.           <V72L08>
           IF   SCRN-STATUZ         NOT = O-K                           <V72L08>
                MOVE SCRN-STATUZ       TO SYSR-STATUZ                   <V72L08>
                PERFORM 600-FATAL-ERROR.                                <V72L08>
                                                                        <V72L08>
      * If an entry on T1675 was not found by Genswch, redisplay the    <V72L08>
      *   the screen with an error.                                     <V72L08>
      *                                                                 <V72L08>
           IF   GENS-STATUZ            = MRNF                           <V72L08>
                MOVE ' '               TO WSSP-SEC-ACTN                 <V72L08>
                                               (WSSP-PROGRAM-PTR)       <V72L08>
                MOVE H093              TO SCRN-ERROR-CODE               <V72L08>
                MOVE SCRN-SCRNAME      TO WSSP-NEXTPROG                 <V72L08>
                GO TO 4090-EXIT.                                        <V72L08>
      *                                                                 <V72L08>
       4085-NEXT-PROGRAM.                                               <V72L08>
           MOVE WSAA-PROG              TO WSSP-NEXTPROG.                <V72L08>
           ADD 1                       TO WSSP-PROGRAM-PTR.             <V72L08>
                                                                        <V72L08>
      *
       4090-EXIT.
            EXIT.
      /
       4100-READ-SUBFILE SECTION.
      ***************************
       4100-READ.

           MOVE SRDN                   TO SCRN-FUNCTION.
           CALL 'S6233IO'           USING SCRN-SCREEN-PARAMS
                                          S6233-DATA-AREA
                                          S6233-SUBFILE-AREA.
           IF   SCRN-STATUZ         NOT = O-K
                                    AND ENDP
                MOVE SCRN-STATUZ       TO SYSR-STATUZ
                PERFORM 600-FATAL-ERROR.

       4190-EXIT.
            EXIT.
      /                                                                 <V72L08>
       4200-SAVE-PROGRAM-STACK SECTION.                                 <V72L08>
       4200-PARA.                                                       <V72L08>
                                                                        <V72L08>
           MOVE WSSP-SEC-PROG (SUB1)   TO WSAA-SEC-PROG (SUB2).         <V72L08>
           ADD  1                      TO SUB1 SUB2.                    <V72L08>
                                                                        <V72L08>
       4290-EXIT.                                                       <V72L08>
            EXIT.                                                       <V72L08>
                                                                        <V72L08>
       4300-LOAD-PROGRAM-STACK SECTION.                                 <V72L08>
       4300-PARA.                                                       <V72L08>
                                                                        <V72L08>
           MOVE GENS-PROG-OUT (SUB2)   TO WSSP-SEC-PROG (SUB1).         <V72L08>
           ADD  1                      TO SUB1 SUB2.                    <V72L08>
                                                                        <V72L08>
       4390-EXIT.                                                       <V72L08>
            EXIT.                                                       <V72L08>
      /                                                                 <V72L08>
       4800-UNIT-ENQUIRY SECTION.                                       <V72L08>
       4800-PARA.                                                       <V72L08>
      *                                                                 <V72L08>
      *    First perform a BEGN  on the selected UTRS record. This will <V72L08>
      *    return the first UTRS record for the contract, life, policy  <V72L08>
      *    and component.                                               <V72L08>
      *                                                                 <V72L08>
           MOVE RLSE                   TO UTRS-FUNCTION.                <V72L08>
           CALL 'UTRSIO'            USING UTRS-PARAMS.                  <V72L08>
           IF   UTRS-STATUZ         NOT = O-K                           <V72L08>
                MOVE UTRS-PARAMS       TO SYSR-PARAMS                   <V72L08>
                PERFORM 600-FATAL-ERROR                                 <V72L08>
           END-IF.                                                      <V72L08>
           MOVE SPACES                 TO UTRS-DATA-AREA.               <V72L08>
           MOVE BEGN                   TO UTRS-FUNCTION.                <V72L08>
           MOVE WSSP-COMPANY           TO UTRS-CHDRCOY.                 <V72L08>
           MOVE CHDRENQ-CHDRNUM        TO UTRS-CHDRNUM.                 <V72L08>
           MOVE '01'                   TO UTRS-LIFE.                    <V72L08>
           MOVE '01'                   TO UTRS-COVERAGE.                <V72L08>
           MOVE '00'                   TO UTRS-RIDER.                   <V72L08>
           MOVE ZEROS                  TO UTRS-PLAN-SUFFIX.             <V72L08>
           CALL 'UTRSIO'            USING UTRS-PARAMS.                  <V72L08>
           IF   UTRS-STATUZ         NOT = O-K             OR            <V72L08>
                UTRS-CHDRCOY        NOT = WSSP-COMPANY    OR            <V72L08>
                UTRS-CHDRNUM        NOT = CHDRENQ-CHDRNUM OR            <V72L08>
                UTRS-LIFE           NOT = '01'            OR            <V72L08>
                UTRS-COVERAGE       NOT = '01'            OR            <V72L08>
                UTRS-RIDER          NOT = '00'            OR            <V72L08>
                UTRS-PLAN-SUFFIX    NOT = ZEROS                         <V72L08>
                MOVE 'N'            TO WSAA-RECORD-FOUND                <V72L08>
                MOVE 'D'            TO WSSP-UNIT-TYPE                   <V72L08>
                GO TO 4850-CHECK-HITS                                   <V72L08>
            END-IF.                                                     <V72L08>
      *                                                                 <V72L08>
      *    If in Policy Level enquiry the Plan Suffix from the selected <V72L08>
      *    Policy is placed in the UTRS record before it is saved.      <V72L08>
      *                                                                 <V72L08>
           MOVE 'Y'                    TO WSAA-RECORD-FOUND.            <V72L08>
      *    IF   POLICY-LEVEL-ENQUIRY                                    <V72L08>
      *         MOVE S6233-PLAN-SUFFIX TO UTRS-PLAN-SUFFIX.             <V72L08>
           MOVE UTRSREC                TO UTRS-FORMAT.                  <V72L08>
           MOVE KEEPS                  TO UTRS-FUNCTION.                <V72L08>
           CALL 'UTRSIO'            USING UTRS-PARAMS.                  <V72L08>
           IF   UTRS-STATUZ         NOT = O-K                           <V72L08>
                MOVE UTRS-PARAMS       TO SYSR-PARAMS                   <V72L08>
                PERFORM 600-FATAL-ERROR.                                <V72L08>
                                                                        <V72L08>
       4850-CHECK-HITS.                                                 <V72L08>
           MOVE RLSE                   TO HITS-FUNCTION.                <V72L08>
           CALL 'HITSIO'            USING HITS-PARAMS.                  <V72L08>
           IF   HITS-STATUZ         NOT = O-K                           <V72L08>
                MOVE HITS-PARAMS       TO SYSR-PARAMS                   <V72L08>
                PERFORM 600-FATAL-ERROR                                 <V72L08>
           END-IF.                                                      <V72L08>
                                                                        <V72L08>
           MOVE SPACES                 TO HITS-PARAMS.                  <V72L08>
           MOVE BEGN                   TO HITS-FUNCTION.                <V72L08>
           MOVE WSSP-COMPANY           TO HITS-CHDRCOY.                 <V72L08>
           MOVE CHDRENQ-CHDRNUM        TO HITS-CHDRNUM.                 <V72L08>
           MOVE '01'                   TO HITS-LIFE.                    <V72L08>
           MOVE '01'                   TO HITS-COVERAGE.                <V72L08>
           MOVE '00'                   TO HITS-RIDER.                   <V72L08>
           MOVE ZEROS                  TO HITS-PLAN-SUFFIX.             <V72L08>
                                                                        <V72L08>
           CALL 'HITSIO'               USING HITS-PARAMS.               <V72L08>
                                                                        <V72L08>
           IF HITS-STATUZ              NOT = O-K                        <V72L08>
           OR HITS-CHDRCOY             NOT = WSSP-COMPANY               <V72L08>
           OR HITS-CHDRNUM             NOT = CHDRENQ-CHDRNUM            <V72L08>
           OR HITS-LIFE                NOT = '01'                       <V72L08>
           OR HITS-COVERAGE            NOT = '01'                       <V72L08>
           OR HITS-RIDER               NOT = '00'                       <V72L08>
      *    OR HITS-PLAN-SUFFIX         NOT = S6233-HSUFFIX              <V72L08>
              IF RECORD-FOUND                                           <V72L08>
                 GO TO 4800-EXIT                                        <V72L08>
              ELSE                                                      <V72L08>
                 MOVE HITS-PARAMS          TO SYSR-PARAMS               <V72L08>
                 PERFORM 600-FATAL-ERROR                                <V72L08>
           END-IF.                                                      <V72L08>
                                                                        <V72L08>
      *    IF POLICY-LEVEL-ENQUIRY                                      <V72L08>
      *        MOVE S6233-PLAN-SUFFIX  TO HITS-PLAN-SUFFIX              <V72L08>
      *    END-IF.                                                      <V72L08>
                                                                        <V72L08>
           MOVE KEEPS                  TO HITS-FUNCTION.                <V72L08>
           CALL 'HITSIO'               USING HITS-PARAMS.               <V72L08>
                                                                        <V72L08>
           IF HITS-STATUZ              NOT = O-K                        <V72L08>
               MOVE HITS-PARAMS        TO SYSR-PARAMS                   <V72L08>
               PERFORM 600-FATAL-ERROR                                  <V72L08>
           END-IF.                                                      <V72L08>
                                                                        <V72L08>
       4800-EXIT.                                                       <V72L08>
           EXIT.                                                        <V72L08>
