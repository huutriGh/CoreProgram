      * Generation Parameters SCRVER(02)               Do Not Delete!   <S9503>
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P6235.
      *
      * Copyright 1986-2020, Computer Sciences Corporation.
      *
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
      *     The  details  that  will  be  displayed  will  come  from two
      *     data-sets:  SACSENQ  and  ACBLENQ.  The same information will
      *     be  extracted  from both data-sets and they will both be read
      *     in  the  same sequence. However different fields are used for
      *     the keys on each data-set.
      *
      *     This  program should process all the relevant ACBLENQ records
      *     and then all the relevant SACSENQ records.
      *
      *     Load the subfile as follows:
      *
      *          T5645 contains a list of all the SACS Code and SACS Type
      *          combinations that are used to drive  the program through
      *          the  ACBLENQ and SACSENQ data-sets. Perform  a  BEGN  on
      *          T5645 using WSAA-PROG as the key. Note that there may be
      *          several  pages  on  T5645 each with up to  15  lines  of
      *          detail.  From each line take the SACS Code and SACS Type
      *          and use them to read through ACBLENQ. The  full  key is:
      *          RLDGCOY   (from  CHDRCOY),  RLDGACCT   (from   CHDRNUM),
      *          SACSCODE (from T5645) and SACSTYP (from T5645).
      *
      *          Display the SACS CODE with its  short  description  from
      *          T3616,  the SACS TYPE with its  short  description  from
      *          T3695,  the  Original Currency, (ORIGCURR)  and  Current
      *          Balance (SACSCURBAL).
      *
      *          Also store, on a hidden field in the  subfile  record, a
      *          flag  indicating that this record's  details  come  from
      *          ACBLENQ.
      *
      *          Process SACSENQ for the same combination  of  codes. The
      *          SACSENQ key is CHDRCOY, CHDRNUM,  SACSCODE  and SACSTYP.
      *          The  same  fields are displayed.  Original  Currency  is
      *          CNTCURR.
      *
      *          Also store, on a hidden field in the  subfile  record, a
      *          flag  indicating that this record's  details  come  from
      *          SACSENQ.
      *
      *     Load  all  pages  required in the subfile and set the subfile
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
      *     Release  both  the ACBLENQ and SACSENQ I/O  modules  in  case
      *     anything is held from a previous selection.
      *
      *     At  this  point  the program will be either searching for the
      *     FIRST  selected  record  in  order  to  pass  control  to the
      *     Transactions Postings program for the selected transaction or
      *     it  will  be returning from the Transactions Postings program
      *     after  displaying  some  details  and  searching for the NEXT
      *     selected record.
      *
      *     It  will be able to determine which of these two states it is
      *     in by examining the Stack Action Flag.
      *
      *     If not returning from a Transactions Postings display, (Stack
      *     Action  Flag  is  blank),  perform  a start on the subfile to
      *     position the file pointer at the beginning.
      *
      *     Each  time  it  returns  to  this  program after processing a
      *     previous seelction its position in the subfile will have been
      *     retained  and  it will be able to continue from where it left
      *     off.
      *
      *     Processing  from here is the same for either state. After the
      *     Start  or  after  returning to the program after processing a
      *     previous selection read the next record from the subfile.  If
      *     this  is not selected (Select is blank), continue reading the
      *     next  subfile  record  until  one  is  found with a non-blank
      *     Select  field or end of file is reached. Do not use the 'Read
      *     Next Changed Subfile Record' function.
      *
      *     If nothing was selected or there are no  more  selections  to
      *     process,  continue  by  just  moving spaces  to  the  current
      *     stackaction field and exit.
      *
      *     If a selection has been found the Sub-Account Balances are to
      *     be displayed.
      *
      *     If the selected subfile record contains data  from  a SACSENQ
      *     record then the next function will read the RTRN data-set for
      *     the Sub-Account Postings. Perform a READS  on the appropriate
      *     SACSENQ record, add 1 to the program pointer and exit.
      *
      *     If the selected subfile record contains data  from  a ACBLENQ
      *     record then the next function will read the ACMV data-set for
      *     the Sub-Account Postings. Perform a READS  on the appropriate
      *     ACBLENQ record, add 1 to the program pointer and exit.
      *
      *
      * Notes.
      * ------
      *
      *     Create  a  new  view of ACBLPF called ACBLENQ which uses only
      *     those  fields  required for this program. It will be keyed on
      *     RLDGCOY, RLDGACCT (first 8 characters), SACSCODE and SACSTYP.
      *
      *     Create  a  new  view of SACSPF called SACSENQ which uses only
      *     those fields required for this program. It will  be  keyed on
      *     CHDRCOY, CHDRNUM, SACSCODE and SACSTYP.
      *
      *     Tables Used:
      *
      * T1688 - Transaction Codes                 Key: Transaction Code
      * T1692 - Branch Codes                      Key: Branch Code
      * T3588 - Contract Premium Status           Key: PSTATCODE
      * T3616 - Sub-Account Codes                 Key: SACSCODE
      * T3623 - Contract Risk Status              Key: STATCODE
      * T3695 - Sub-Account Types                 Key: SACSTYP
      * T5645 - Financial Trans Accounting Rules  Key: WSAA-PROG
      * T5688 - Contract Structure                Key: CNTTYPE
      *
      *
      *****************************************************************
      *              AMENDMENT  HISTORY                               *
      *****************************************************************
      * DATE.....   BY..   AMENDMENT...............  NUMBER
      *
      * DD/MM/YY    X.X.   xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  NNN
      * 09/12/89    A.G.   Make sure the ITEM-GENAREA is reloaded   001
      * 03/05/90    B.C.   FOLLOWING IOM FIX INPUT AT BASE
      * 24/01/90    A.G.   Make sure ITEM-PARAMS  are intialised    002
      * 22/07/91    I.W.   No longer use the SACS file for
      *                    displaying any details......             003
      * 17/10/91    J.L.   Add Component field from screen.         004
      * 27/01/92    J.L    (SDF 1782)                               005
      *                    Initialise WSSP-LIFEKEY when scrn-statuz
      *                    = MASM.
      *
      * 19/01/93    MURP.  AQR 3750.
      *                    The BEGN/NEXTR read on the ITEM file     006
      *                    does not work presumably due to a
      *                    SMART IO change.
      *                    The BEGN found the 1st page of T5645
      *                    But the NEXTR retrieved item 0T1670||
      *                    Change code to individually read each page
      *                    of T5645 with the relevant seq No.
      *                    (NN Italy)
      *
      * 02/07/93    C.MCK. AQR 4601.                                007
      *                    More efficient access to ACBL to reduce
      *                    response time.  Changes relate to the
      *                    fact that the ACBLENQ logical file has
      *                    been altered so that records will be
      *                    accessed in a different order.
      *                    SOURCE COMPUTER IBM-S38 changed to IBM AS400
      *
      * 24.08.93    H.P    AQR 4668.                                008
      *                    Hard coding of 'NONE' removed when joint
      *                    life not found.
      *
      * 25/01/95    COS.   AQR : 5681                               009
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
      * 02/02/96  01/01   D96NUM       Rob Yates                            *
      * 27/03/96  01/01   D96NUM       Rachel Cartwright                    *
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
      * 16/10/20  01/01   PS070        Mai Yen Phi - IT                     *
      *           RECOMPILED                                           ??   *
      *                                                                     *
      **DD/MM/YY*************************************************************
      *
      *
      /
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
   ****SOURCE-COMPUTER. IBM-S38.                                        <007>
   ****OBJECT-COMPUTER. IBM-S38.                                        <007>
       SOURCE-COMPUTER.                               IBM-AS400.        <007>
       OBJECT-COMPUTER.                               IBM-AS400.        <007>
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       01  WSAA-PROG                   PIC X(05) VALUE 'P6235'.
       01  WSAA-VERSION                PIC X(02) VALUE '01'.
      *
       01  WSAA-ACBLENQ-FLAG           PIC X(01) VALUE 'A'.
       01  WSAA-SACSENQ-FLAG           PIC X(01) VALUE 'S'.
       01  WSAA-ITEMSEQ                PIC 99    VALUE 0.               <006>
       01  SUB1                        PIC S9(03) COMP-3.
       01  WSAA-RLDGACCT.                                               <004>
         03 WSAA-CHDRNUM               PIC X(08).                       <004>
         03 WSAA-COMPONENT             PIC X(08).                       <004>
      *
       01  WSAA-SEC-PROGS.
           03  WSAA-SEC-PROG           PIC X(05) OCCURS 8.

       01  WSAA-BATCKEY.
           COPY BATCKEY.

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
           03  T5688                   PIC X(05) VALUE 'T5688'.
      *
       01  FORMATS.
           03  CHDRENQREC              PIC X(10) VALUE 'CHDRENQREC'.
           03  LIFEENQREC              PIC X(10) VALUE 'LIFEENQREC'.
           03  ACBLENQREC              PIC X(10) VALUE 'ACBLENQREC'.
      *****03  SACSENQREC              PIC X(10) VALUE 'SACSENQREC'.    <003>
           03  CLTSREC                 PIC X(10) VALUE 'CLTSREC'.
           03  DESCREC                 PIC X(10) VALUE 'DESCREC'.
           03  ITEMREC                 PIC X(10) VALUE 'ITEMREC'.
      /
           COPY CHDRENQSKM.
      /
           COPY LIFEENQSKM.
      /
           COPY ACBLENQSKM.
      /
      *****COPY SACSENQSKM.                                             <003>
      /
           COPY CLTSSKM.
      /
           COPY DESCSKM.
      /
           COPY ITEMSKM.
      /
           COPY T5645REC.
      /
           COPY VARCOM.
      *
           COPY SYSERRREC.
      *
           COPY OPSTATSREC.
      *
      ***  COPY SCRNPARAMS.                                             <S9503>
      /
      ***  COPY S6235SCR.                                               <S9503>
      /
       LINKAGE SECTION.
      * Screen copybooks are now part of the linkage.                   <S9503>
      /                                                                 <S9503>
           COPY SCRNPARAMS.                                             <S9503>
      /                                                                 <S9503>
           COPY S6235SCR.                                               <S9503>

           COPY WSSPCOMN.

           COPY WSSPLIFE.
      /
      * Statement now includes screen copybooks.                        <S9503>
       PROCEDURE DIVISION USING WSSP-COMMON-AREA WSSP-USER-AREA         <S9503>
                                               SCRN-SCREEN-PARAMS       <S9503>
                                               S6235-DATA-AREA          <S9503>
                                               S6235-SUBFILE-AREA   .   <S9503>

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
           MOVE SPACES                 TO S6235-DATA-AREA.
           MOVE SPACES                 TO S6235-SUBFILE-AREA.
           MOVE ZEROES                 TO S6235-SACSCURBAL.
           MOVE SPACES                 TO WSAA-RLDGACCT.                <004>

           MOVE SCLR                   TO SCRN-FUNCTION.
           CALL 'S6235IO' USING SCRN-SCREEN-PARAMS
                                S6235-DATA-AREA
                                S6235-SUBFILE-AREA.
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
           MOVE CHDRENQ-CHDRNUM        TO S6235-CHDRNUM.
           MOVE CHDRENQ-CNTTYPE        TO S6235-CNTTYPE.
           MOVE CHDRENQ-CNTCURR        TO S6235-CNTCURR.
           MOVE CHDRENQ-REGISTER       TO S6235-REGISTER.
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
           MOVE LIFEENQ-LIFCNUM        TO S6235-LIFENUM
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
           MOVE WSSP-LONGCONFNAME      TO S6235-LIFENAME.
      *
      * Check for the existence of Joint Life details.
      *
           MOVE '01'                   TO LIFEENQ-JLIFE.
           MOVE READR                  TO LIFEENQ-FUNCTION.
           CALL 'LIFEENQIO'            USING LIFEENQ-PARAMS.
      ***  IF   LIFEENQ-STATUZ         NOT = O-K                        <008>
      ***       MOVE 'NONE'            TO S6235-JLIFE                   <008>
      ***                                 S6235-JLIFENAME               <008>
      ***  ELSE                                                         <008>
           IF   LIFEENQ-STATUZ             = O-K                        <008>
                MOVE LIFEENQ-LIFCNUM   TO S6235-JLIFE
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
                                       TO S6235-JLIFENAME.
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
                MOVE ALL '?'           TO S6235-CTYPEDES
           ELSE
                MOVE DESC-LONGDESC     TO S6235-CTYPEDES.
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
                MOVE ALL '?'           TO S6235-CHDRSTATUS
           ELSE
                MOVE DESC-SHORTDESC    TO S6235-CHDRSTATUS.
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
                MOVE ALL '?'           TO S6235-PREMSTATUS
           ELSE
                MOVE DESC-SHORTDESC    TO S6235-PREMSTATUS.
      *---------------------------------------------------------------*
      *    This is the processing for the ACBLENQ data-set. It will   *
      *    be repeated for the SACS data-set.                         *
      *---------------------------------------------------------------*
      *
      *    Read the first record from table T5645 for this program.
      *
           MOVE SPACES                 TO ITEM-PARAMS.                  <002>
      **** MOVE BEGN                   TO ITEM-FUNCTION.                <006>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <006>
           MOVE READR                  TO ITEM-FUNCTION.                <006>
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.
           MOVE T5645                  TO ITEM-ITEMTABL.
           MOVE WSAA-PROG              TO ITEM-ITEMITEM.
      *                                                                 <006>
      * 1ST page of Read of T5645.                                      <006>
      *                                                                 <006>
           MOVE SPACES                 TO ITEM-ITEMSEQ.                 <006>
           MOVE ZEROES                 TO WSAA-ITEMSEQ.                 <006>
      *                                                                 <006>
           CALL 'ITEMIO'            USING ITEM-PARAMS.
           IF   ITEM-STATUZ         NOT = O-K
           AND  ITEM-STATUZ         NOT = MRNF                          <006>
                MOVE ITEM-PARAMS       TO SYSR-PARAMS
                PERFORM 600-FATAL-ERROR.
      *                                                                 <006>
           IF  ITEM-STATUZ             = MRNF                           <006>
               GO TO 1090-EXIT.                                         <006>
                                                                        <006>
      **** IF   ITEM-ITEMPFX        NOT = 'IT'         OR               <006>
      ****      ITEM-ITEMCOY        NOT = WSSP-COMPANY OR               <006>
      ****      ITEM-ITEMTABL       NOT = T5645        OR               <006>
      ****      ITEM-ITEMITEM       NOT = WSAA-PROG                     <006>
      ****      MOVE ITEM-PARAMS       TO SYSR-PARAMS                   <006>
      ****      PERFORM 600-FATAL-ERROR.                                <006>

           MOVE ITEM-GENAREA           TO T5645-T5645-REC.
      *
      *    There may be several pages on T5645 for the current program.
      *    These are handled one at a time. Each page may have up to 15
      *    lines containing a pair of codes: SACS CODE and SACS TYPE.
      *    Each pair is then used in a key to read ACBLENQ. There may
      *    be several records on ACBLENQ sharing this pair of codes.
      *    Each one will be displayed as a line on the subfile.
      *
           PERFORM 1100-PROCESS-T5645-PAGE
             UNTIL ITEM-STATUZ             =  MRNF.                     <006>
      *                                                                 <006>
      ****   UNTIL ITEM-ITEMPFX        NOT = 'IT'         OR            <006>
      ****         ITEM-ITEMCOY        NOT = WSSP-COMPANY OR            <006>
      ****         ITEM-ITEMTABL       NOT = T5645        OR            <006>
      ****         ITEM-ITEMITEM       NOT = WSAA-PROG    OR            <006>
      ****         ITEM-STATUZ             = ENDP.                      <006>
           MOVE 1                      TO SCRN-SUBFILE-RRN.

                                                                        <001>
       1090-EXIT.
            EXIT.
      /
           COPY CONFNAME.
      /
       1100-PROCESS-T5645-PAGE SECTION.
       1100-PARA.

           MOVE 1                      TO SUB1.
      *                                                                 <007>
      **** PERFORM 1200-PROCESS-T5645-LINE 15 TIMES.                    <007>
      *
           PERFORM 1200-PROCESS-T5645-LINE                              <007>
                   UNTIL SUB1 > 15.                                     <007>
      *                                                                 <007>
      *    Read the next record from table T5645 for this program.
      *
      **** MOVE NEXTR                  TO ITEM-FUNCTION.                <006>
           MOVE READR                  TO ITEM-FUNCTION.                <006>
           ADD +1                      TO WSAA-ITEMSEQ.                 <006>
           MOVE WSAA-ITEMSEQ           TO ITEM-ITEMSEQ.                 <006>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <006>
      *                                                                 <006>
           CALL 'ITEMIO'            USING ITEM-PARAMS.
           IF   ITEM-STATUZ         NOT = O-K
           AND  ITEM-STATUZ         NOT = MRNF                          <006>
                MOVE ITEM-PARAMS       TO SYSR-PARAMS
                PERFORM 600-FATAL-ERROR.
      *
           IF  ITEM-STATUZ             = MRNF                           <006>
               GO TO 1090-EXIT.                                         <006>
      *                                                                 <006>
           MOVE ITEM-GENAREA           TO T5645-T5645-REC.              <001>

       1190-EXIT.
            EXIT.
      /
       1200-PROCESS-T5645-LINE SECTION.
       1200-PARA.
      *
      *    Take the SACS CODE and the SACS TYPE from the T5645 line and
      *    read all records on ACBLENQ that have matching codes. The
      *    key is Company, Contract No. and Trans. No. (descending).
      *
           MOVE CHDRENQ-CHDRCOY        TO ACBLENQ-RLDGCOY.
      **** MOVE CHDRENQ-CHDRNUM        TO ACBLENQ-RLDGACCT.             <007>
           MOVE CHDRENQ-CHDRNUM        TO WSAA-CHDRNUM.                 <004>
           MOVE SPACES                 TO WSAA-COMPONENT.               <007>
           MOVE WSAA-RLDGACCT          TO ACBLENQ-RLDGACCT.             <007>
           MOVE T5645-SACSCODE (SUB1)  TO ACBLENQ-SACSCODE.
           MOVE T5645-SACSTYPE (SUB1)  TO ACBLENQ-SACSTYP.
           MOVE SPACES                 TO ACBLENQ-ORIGCURR.
           MOVE BEGN                   TO ACBLENQ-FUNCTION.
           CALL 'ACBLENQIO'         USING ACBLENQ-PARAMS.
           IF   ACBLENQ-STATUZ      NOT = O-K
            AND ACBLENQ-STATUZ      NOT = ENDP
      *****     MOVE SACSENQ-PARAMS    TO SYSR-PARAMS                   <003>
                MOVE ACBLENQ-PARAMS    TO SYSR-PARAMS
                PERFORM 600-FATAL-ERROR.

           IF ACBLENQ-STATUZ            = O-K                           <007>
              MOVE ACBLENQ-RLDGACCT    TO WSAA-RLDGACCT                 <007>
           END-IF.                                                      <007>
                                                                        <007>
           PERFORM 1300-ACBLENQ-SACS-GROUP
             UNTIL ACBLENQ-RLDGCOY  NOT = CHDRENQ-CHDRCOY       OR
      ************ ACBLENQ-RLDGACCT NOT = CHDRENQ-CHDRNUM       OR      <004>
                      WSAA-CHDRNUM  NOT = CHDRENQ-CHDRNUM       OR      <004>
                   ACBLENQ-SACSCODE NOT = T5645-SACSCODE (SUB1) OR      <007>
                   ACBLENQ-SACSTYP  NOT = T5645-SACSTYPE (SUB1) OR      <007>
      ************ ACBLENQ-SACSCODE NOT = T5645-SACSCODE (SUB1) OR      <004>
      ************ ACBLENQ-SACSTYP  NOT = T5645-SACSTYPE (SUB1) OR      <004>
                   ACBLENQ-STATUZ       = ENDP.

      *****                                                             <003>
      *****Take the SACS CODE and the SACS TYPE from the T5645 line and <003>
      *****read all records on SACSENQ that have matching codes. The    <003>
      *****key is Company, Contract No. and Trans. No. (descending).    <003>
      *****                                                             <003>
      *****MOVE CHDRENQ-CHDRCOY        TO SACSENQ-CHDRCOY.              <003>
      *****MOVE CHDRENQ-CHDRNUM        TO SACSENQ-CHDRNUM.              <003>
      *****MOVE T5645-SACSCODE (SUB1)  TO SACSENQ-SACSCODE.             <003>
      *****MOVE T5645-SACSTYPE (SUB1)  TO SACSENQ-SACSTYP.              <003>
      *****MOVE BEGN                   TO SACSENQ-FUNCTION.             <003>
      *****MOVE SPACES                 TO SACSENQ-CNTCURR.              <003>
      *****CALL 'SACSENQIO'         USING SACSENQ-PARAMS.               <003>
      *****IF   SACSENQ-STATUZ      NOT = O-K                           <003>
      ***** AND SACSENQ-STATUZ      NOT = ENDP                          <003>
      *****     MOVE SACSENQ-PARAMS    TO SYSR-PARAMS                   <003>
      *****     PERFORM 600-FATAL-ERROR.                                <003>
      *****                                                             <003>
      *****PERFORM 1400-SACSENQ-SACS-GROUP                              <003>
      *****  UNTIL SACSENQ-CHDRCOY  NOT = CHDRENQ-CHDRCOY       OR      <003>
      *****        SACSENQ-CHDRNUM  NOT = CHDRENQ-CHDRNUM       OR      <003>
      *****        SACSENQ-SACSCODE NOT = T5645-SACSCODE (SUB1) OR      <003>
      *****        SACSENQ-SACSTYP  NOT = T5645-SACSTYPE (SUB1) OR      <003>
      *****        SACSENQ-STATUZ       = ENDP.                         <003>
       1270-NEXT.                                                       <004>

           ADD  1                      TO SUB1.

       1290-EXIT.
            EXIT.
      /
       1300-ACBLENQ-SACS-GROUP SECTION.
       1300-PARA.
      *                                                                 <004>
      **** MOVE ACBLENQ-RLDGACCT       TO WSAA-RLDGACCT.           <004><007>
      *                                                                 <004>
      **** IF WSAA-CHDRNUM             NOT = CHDRENQ-CHDRNUM  OR   <004><007>
      ****    T5645-SACSCODE(SUB1)     NOT = ACBLENQ-SACSCODE OR   <004><007>
      ****    T5645-SACSTYPE(SUB1)     NOT = ACBLENQ-SACSTYP       <004><007>
      ****    GO                       TO 1350-READ.               <004><007>
      *                                                                 <004>
           MOVE SPACES                 TO S6235-SUBFILE-FIELDS.
           MOVE WSAA-COMPONENT         TO S6235-COMPONENT.              <004>
           MOVE WSAA-ACBLENQ-FLAG      TO S6235-RECTYPE.
           MOVE ACBLENQ-SACSCODE       TO S6235-SACSCODE.
           MOVE ACBLENQ-SACSTYP        TO S6235-SACSTYP.
           MOVE ACBLENQ-ORIGCURR       TO S6235-CURR.
           MOVE ACBLENQ-SACSCURBAL     TO S6235-SACSCURBAL.
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
                MOVE ALL '?'           TO S6235-SACSCODED
           ELSE
                MOVE DESC-SHORTDESC    TO S6235-SACSCODED.
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
      *
           IF   DESC-STATUZ            = MRNF
                MOVE ALL '?'           TO S6235-SACSTYPD
           ELSE
                MOVE DESC-SHORTDESC    TO S6235-SACSTYPD.
      *
      *    Write the subfile record.
      *
           MOVE SADD                   TO SCRN-FUNCTION
           CALL 'S6235IO'           USING SCRN-SCREEN-PARAMS
                                          S6235-DATA-AREA
                                          S6235-SUBFILE-AREA
           IF SCRN-STATUZ           NOT = O-K
                MOVE SCRN-STATUZ       TO SYSR-STATUZ
                PERFORM 600-FATAL-ERROR.
      *                                                                 <004>
       1350-READ.                                                       <004>
      *
      *    Read the next ACBLENQ record.
      *
           MOVE NEXTR                  TO ACBLENQ-FUNCTION.
           CALL 'ACBLENQIO'         USING ACBLENQ-PARAMS.
           IF   ACBLENQ-STATUZ      NOT = O-K
            AND ACBLENQ-STATUZ      NOT = ENDP
                MOVE ACBLENQ-PARAMS    TO SYSR-PARAMS                   <003>
      *****     MOVE SACSENQ-PARAMS    TO SYSR-PARAMS                   <003>
                PERFORM 600-FATAL-ERROR.

           MOVE ACBLENQ-RLDGACCT       TO WSAA-RLDGACCT.                <007>
                                                                        <007>
       1390-EXIT.
            EXIT.
      /
      *1400-SACSENQ-SACS-GROUP SECTION.                                 <003>
      *1400-PARA.                                                       <003>
      *****                                                             <003>
      *****MOVE SPACES                 TO S6235-SUBFILE-FIELDS.         <003>
      *****MOVE WSAA-SACSENQ-FLAG      TO S6235-RECTYPE.                <003>
      *****MOVE SACSENQ-SACSCODE       TO S6235-SACSCODE.               <003>
      *****MOVE SACSENQ-SACSTYP        TO S6235-SACSTYP.                <003>
      *****MOVE SACSENQ-CNTCURR        TO S6235-CURR.                   <003>
      *****MOVE SACSENQ-SACSCURBAL     TO S6235-SACSCURBAL.             <003>
      *****                                                             <003>
      *****Obtain the SACS CODE description from T3616.                 <003>
      *****                                                             <003>
      *****MOVE SPACES                 TO DESC-DATA-KEY.                <003>
      *****MOVE 'IT'                   TO DESC-DESCPFX.                 <003>
      *****MOVE WSSP-COMPANY           TO DESC-DESCCOY.                 <003>
      *****MOVE T3616                  TO DESC-DESCTABL.                <003>
      *****MOVE SACSENQ-SACSCODE       TO DESC-DESCITEM.                <003>
      *****MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.                <003>
      *****MOVE READR                  TO DESC-FUNCTION.                <003>
      *****                                                             <003>
      *****CALL 'DESCIO' USING DESC-PARAMS.                             <003>
      *****IF   DESC-STATUZ            NOT = O-K                        <003>
      *****                        AND NOT = MRNF                       <003>
      *****     MOVE DESC-PARAMS       TO SYSR-PARAMS                   <003>
      *****     PERFORM 600-FATAL-ERROR.                                <003>
      *****                                                             <003>
      *****IF   DESC-STATUZ            = MRNF                           <003>
      *****     MOVE ALL '?'           TO S6235-SACSCODED               <003>
      *****ELSE                                                         <003>
      *****     MOVE DESC-SHORTDESC    TO S6235-SACSCODED.              <003>
      *****                                                             <003>
      *****Obtain the SACS TYPE description from T3695.                 <003>
      *****                                                             <003>
      *****MOVE SPACES                 TO DESC-DATA-KEY.                <003>
      *****MOVE 'IT'                   TO DESC-DESCPFX.                 <003>
      *****MOVE WSSP-COMPANY           TO DESC-DESCCOY.                 <003>
      *****MOVE T3695                  TO DESC-DESCTABL.                <003>
      *****MOVE SACSENQ-SACSTYP        TO DESC-DESCITEM.                <003>
      *****MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.                <003>
      *****MOVE READR                  TO DESC-FUNCTION.                <003>
      *****                                                             <003>
      *****CALL 'DESCIO' USING DESC-PARAMS.                             <003>
      *****IF   DESC-STATUZ            NOT = O-K                        <003>
      *****                        AND NOT = MRNF                       <003>
      *****     MOVE DESC-PARAMS       TO SYSR-PARAMS                   <003>
      *****     PERFORM 600-FATAL-ERROR.                                <003>
      *****                                                             <003>
      *****IF   DESC-STATUZ            = MRNF                           <003>
      *****     MOVE ALL '?'           TO S6235-SACSTYPD                <003>
      *****ELSE                                                         <003>
      *****     MOVE DESC-SHORTDESC    TO S6235-SACSTYPD.               <003>
      *****                                                             <003>
      *****Write the subfile record.                                    <003>
      *****                                                             <003>
      *****MOVE SADD                   TO SCRN-FUNCTION                 <003>
      *****CALL 'S6235IO'           USING SCRN-SCREEN-PARAMS            <003>
      *****                               S6235-DATA-AREA               <003>
      *****                               S6235-SUBFILE-AREA            <003>
      *****IF SCRN-STATUZ           NOT = O-K                           <003>
      *****     MOVE SCRN-STATUZ       TO SYSR-STATUZ                   <003>
      *****     PERFORM 600-FATAL-ERROR.                                <003>
      *****                                                             <003>
      *****Read the next SACSENQ record.                                <003>
      *****                                                             <003>
      *****MOVE NEXTR                  TO SACSENQ-FUNCTION.             <003>
      *****CALL 'SACSENQIO'         USING SACSENQ-PARAMS.               <003>
      *****IF   SACSENQ-STATUZ      NOT = O-K                           <003>
      ***** AND SACSENQ-STATUZ      NOT = ENDP                          <003>
      *****     MOVE SACSENQ-PARAMS    TO SYSR-PARAMS                   <003>
      *****     PERFORM 600-FATAL-ERROR.                                <003>
      *****                                                             <003>
      *1490-EXIT.                                                       <003>
      ***** EXIT.                                                       <003>
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
      * Skip this section if returning from an optional selection,      <S9503>
      * i.e. check the stack action flag equals '*'.                    <S9503>
      *                                                                 <S9503>
           MOVE 1                      TO SCRN-SUBFILE-RRN.             <S9503>
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
      *2000-PARA.                                                       <009>
       2010-SCREEN-IO.                                                  <009>
      *    CALL 'S6235IO' USING SCRN-SCREEN-PARAMS                      <S9503>
      *                         S6235-DATA-AREA                         <S9503>
      *                         S6235-SUBFILE-AREA.                     <S9503>
           IF SCRN-STATUZ              = MASM                           <005>
              MOVE ZEROS               TO WSSP-LIFEKEY.                 <005>
      * Screen errors are now handled in the calling program.           <S9503>
      *    PERFORM 200-SCREEN-ERRORS.                                   <S9503>
           MOVE O-K                    TO WSSP-EDTERROR.

       2010-VALIDATE-SCREEN.

           IF   SCRN-STATUZ = 'KILL'
                GO TO 2090-EXIT.
      *
      *    No validation is necessary. Any non-blank character may be
      *    used to select a particular line.
      *
       2090-EXIT.
            EXIT.
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
      *    Release any ACBLENQ record that may have been stored.
      *
           MOVE RLSE                   TO ACBLENQ-FUNCTION.             <004>
           CALL 'ACBLENQIO'         USING ACBLENQ-PARAMS.               <004>
           IF   ACBLENQ-STATUZ      NOT = O-K                           <004>
                MOVE ACBLENQ-PARAMS    TO SYSR-PARAMS                   <004>
                PERFORM 600-FATAL-ERROR.                                <004>
      *****                                                             <003>
      *****Release any SACSENQ record that may have been stored.        <003>
      *****                                                             <003>
      *****MOVE RLSE                   TO SACSENQ-FUNCTION.             <003>
      *****CALL 'SACSENQIO'         USING SACSENQ-PARAMS.               <003>
      *****IF   SACSENQ-STATUZ      NOT = O-K                           <003>
      *****     MOVE SACSENQ-PARAMS    TO SYSR-PARAMS                   <003>
      *****     PERFORM 600-FATAL-ERROR.                                <003>
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
           CALL 'S6235IO'           USING SCRN-SCREEN-PARAMS
                                          S6235-DATA-AREA
                                          S6235-SUBFILE-AREA.
           IF   SCRN-STATUZ         NOT = O-K
                                    AND ENDP
                MOVE SCRN-STATUZ       TO SYSR-STATUZ
                PERFORM 600-FATAL-ERROR.

       4010-BYPASS-START.

           IF S6235-SELECT = SPACES
           PERFORM 4100-READ-SUBFILE
             UNTIL S6235-SELECT     NOT = SPACES
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
      *
      * Blank out action to ensure it is not processed again
      *
           MOVE SPACE                  TO S6235-SELECT.

      *****IF   S6235-RECTYPE           = WSAA-SACSENQ-FLAG             <003>
      *****     GO TO 4020-STORE-SACS-RECORD.                           <003>
      *
      *    Read and store the selected ACBLENQ record.
      *
           MOVE SPACES                 TO ACBLENQ-DATA-AREA.
           MOVE WSSP-COMPANY           TO ACBLENQ-RLDGCOY.
      *****MOVE CHDRENQ-CHDRNUM        TO ACBLENQ-RLDGACCT.             <004>
           MOVE CHDRENQ-CHDRNUM        TO WSAA-CHDRNUM.                 <004>
           MOVE S6235-COMPONENT        TO WSAA-COMPONENT.               <004>
           MOVE WSAA-RLDGACCT          TO ACBLENQ-RLDGACCT.             <004>
           MOVE S6235-SACSCODE         TO ACBLENQ-SACSCODE.
           MOVE S6235-SACSTYP          TO ACBLENQ-SACSTYP.
           MOVE S6235-CURR             TO ACBLENQ-ORIGCURR.
           MOVE READS                  TO ACBLENQ-FUNCTION.
           CALL 'ACBLENQIO'         USING ACBLENQ-PARAMS.
           IF   ACBLENQ-STATUZ      NOT = O-K
                MOVE ACBLENQ-PARAMS    TO SYSR-PARAMS
                PERFORM 600-FATAL-ERROR.
           GO TO 4080-NEXT-PROGRAM.
      *****                                                             <003>
      *****Read and store the selected SACSENQ record.                  <003>
      *****                                                             <003>
      *4020-STORE-SACS-RECORD.                                          <003>
      *****                                                             <003>
      *****MOVE SPACES                 TO SACSENQ-DATA-AREA.            <003>
      *****MOVE WSSP-COMPANY           TO SACSENQ-CHDRCOY.              <003>
      *****MOVE CHDRENQ-CHDRNUM        TO SACSENQ-CHDRNUM               <003>
      *****MOVE S6235-SACSCODE         TO SACSENQ-SACSCODE.             <003>
      *****MOVE S6235-SACSTYP          TO SACSENQ-SACSTYP.              <003>
      *****MOVE S6235-CURR             TO SACSENQ-CNTCURR.              <003>
      *****MOVE READS                  TO SACSENQ-FUNCTION.             <003>
      *****CALL 'SACSENQIO'         USING SACSENQ-PARAMS.               <003>
      *****IF   SACSENQ-STATUZ      NOT = O-K                           <003>
      *****     MOVE SACSENQ-PARAMS    TO SYSR-PARAMS                   <003>
      *****     PERFORM 600-FATAL-ERROR.                                <003>
      *****                                                             <003>
       4080-NEXT-PROGRAM.

           MOVE '*'                    TO WSSP-SEC-ACTN
                                         (WSSP-PROGRAM-PTR).
           ADD 1                       TO WSSP-PROGRAM-PTR.

      *
       4090-EXIT.
            EXIT.
      /
       4100-READ-SUBFILE SECTION.
      ***************************
       4100-READ.

           MOVE SRDN                   TO SCRN-FUNCTION.
           CALL 'S6235IO'           USING SCRN-SCREEN-PARAMS
                                          S6235-DATA-AREA
                                          S6235-SUBFILE-AREA.
           IF   SCRN-STATUZ         NOT = O-K
                                    AND ENDP
                MOVE SCRN-STATUZ       TO SYSR-STATUZ
                PERFORM 600-FATAL-ERROR.

       4190-EXIT.
            EXIT.
