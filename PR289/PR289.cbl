      * Generation Parameters - SCRVER(02)            Do Not Delete|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PR289.
      **
      * Copyright 1986-2020, Computer Sciences Corporation.
      *
      *
      * RECEIPT HEADER
      * ~~~~~~~~~~~~~~
      * This program allows the entry or display of the Receipt Header,
      * depending on the action selected at the Submenu.
      *
      * Information on the payee, bank code, receipt type, amount, etc
      * are entered here. These information is passed through the
      * subsystem via the linkage as defined in WSSPDOCS (*CPY) to
      * Receipt Dissection program.
      *
      * This program only allows multiple mode of payments by cash and
      * Cheque. Other combination of payments mode are not allowed.
      *
      * For partial receipt cancellation , user can choose which
      * payment details on subfile is to be cancelled.
      *
      * This program doesn't do any updating to database , all the
      * updating is done in Receipt Dissection program P2610.
      *
      * There are two ways to switch to this program, either from
      * Submenu (P2067) or Receipt Dissection Screen (P2610 - F13).
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
      * 04/07/01  01/01   V6F108       Ch'ng Chin Tat                       *
      *           Initial Version                                           *
      *                                                                     *
      * 21/09/01  01/01   V6F108       R.Nagamuthu                          *
      *           Receipt via credit card, account no. field should be      *
      *           mandatory instead of branch description.                  *
      *           Cheque no. field is optional for Credit Card receipt.     *
      *                                                                     *
      * 01/10/01  01/01   R2-284       R.Nagamuthu                          *
      *           Allow combination of Cash, Cheque & Credit Card.          *
      *                                                                     *
      * 04/02/02  01/01   V62FIX       WORACHART                            *
      *           RETROFIT  31/01/02  01/01   CSC008       Leekimpau        *
      *           do a new receipt when the receipt no = spaces             *
      *                                                                     *
      * 19/12/02  01/01   V64F13       Lian Chui Tin                        *
      *           -To include new field Debtor Marry Indicator.             *
      *           -Read TR386 for field literal. Any futher addition        *
      *            of new field to this screen should adhere to this        *
      *            standard.                                                *
      *           -Call routine CHRFSUF which read TR377 for field          *
      *            validation rules. Any futher additon of new field        *
      *            to this screen should adhere to this standard.           *
      *           -Introduce an indicator in Working Storage to handle      *
      *            bad status returned by routine ALOCNO. Moving the        *
      *            ALNO-STATUZ to SR289-RECEIPT-ERR in 1000-Section         *
      *            is not appropriate as SR289-ERROR-INDICATORS is          *
      *            initialised to spaces at the beginning of 2000-          *
      *            Section.                                                 *
      *                                                                     *
      * 10/04/03  01/01   FA2978       Nagamuthu S.Ramaiah                  *
      *           Change due to T3688REC copybook field rename.             *
      *                                                                     *
      * 21/04/04  01/01   FA3287       Nancie lin - FSG Taiwan              *
      *           --> Initialized CSTP-CASHTYPE when PAYTYPE = space
      *           --> If ALNO-STATUZ not OK, Display Error Message
      *                                                                     *
      * 08/07/04  01/01   FA3350       Tan Boo Kean                         *
      *          (Retrofit from Jerneh Insurance)                           *
      *           Bombs when invalid date is entered.                       *
      *                                                                     *
      * 29/05/07  01/01   V72P02       Ariane Durr/FSG/CSC (Singapore       *
      *           MCH012 when enquiry. WSSP-SCRATE need to be set .    ??   *
      *                                                                     *
      * 09/06/09  01/01   V74F03       Xu Chen/ASIA/CSC (China)             *
      *           Anti Money Laundering Validation.                         *
      *          -Call 'AMLCHK' to check receipt amount against the limit   *
      *           store in TR24A.                                           *
      *          -A message box will be displayed if amount is equal or     *
      *           exceed the AML limit. Enter N to exit receipting. If      *
      *           user input Y to accept the amount, the case will be       *
      *           recorded in RPTF for report purpose.                      *
      *                                                                     *
      * 19/06/09  01/01   V74F03       Ali Hartono/FSG/CSC (Singapore       *
      *           Recompile only.                                           *
      *                                                                     *
      * 14/07/09  Retrofitted by Helen Cui                                  *
      *           01/01   FA4514       CSC - Niamh Fogarty                  *
      *           Set WSSP-TRANDATE in 3000 section so that it will be      *
      *           passed to the next screen. Also set this field when       *
      *           in enquiry mode.                                          *
      *                                                                     *
      * 22/08/10  01/01   V76F04       Nath Mathmaluwe/FSG/CSC (Singa       *
      * 16/11/10  01/01   V76F04       Josefa Francisco De Jesus/SGP/       *
      *           Introduce a new Function key for remarks.                 *
      *                                                                     *
      * 14/09/10  01/01   V76F06       Nancie Lin/FSG/CSC (Hong Kong)       *
      *           Call ZRDECPLC to do rounding processing                   *
      *                                                                     *
      * 16/09/10  01/01   V76F07       Nath Mathmaluwe/FSG/CSC (Singa       *
      * 08/10/10  01/01   V76F07       Josefa Francisco De Jesus/SGP/       *
      *           Default the receipt's first mode of payment's             *
      *           Payment Type and Cheque Type with the corresponding       *
      *           values setup in the TR377.                                *
      *           Default Payor as the the payor of the policy entered      *
      *           in the submenu.                                           *
      *                                                                     *
      * 24/11/10  01/01   V76F12       Josefa Francisco De Jesus/SGP/       *
      *           Status Code Receipt Enhancement.                          *
      *           ================================                          *
      *       (I) This enhancement has an option to turn off/on via         *
      *           table TR29S in FSU level.  If the switch is turned        *
      *           off, the Receipt should behave exactly the same way       *
      *           as in V7.5                                                *
      *                                                                     *
      *      (II) Introduce a receipt status that may track the many        *
      *           stages in its life cycle.  The receipt status is          *
      *           dictate by the setup in T3676, according to the           *
      *           transaction code.                                         *
      *                                                                     *
      *           For cancelation and reversal the staus is                 *
      *           dictated.  'CU' for cancelation and 'RR' for              *
      *           the reversal.                                             *
      *                                                                     *
      *      (IV) Introduce Post-Dated Receipt.                             *
      *           Post-Dated cheque will be incorporated with the           *
      *           current receipt module.  Upon receipt creation,           *
      *           system will identify/accept post-dated receipts based     *
      *           on the following criteria:                                *
      *             a) Payment Method is Cheque                             *
      *             b) Future dated Cheque i.e. Cheque Date > Current       *
      *                Date                                                 *
      *             c) Single payment instrument (RBNK)                     *
      *                                                                     *
      *           The system auto-determine if receipt is postdated and     *
      *           flag the receipt as POSTDATED = "Y".                      *
      *           Variable WSSP-CHEQ-PRESFLAG is used to hold the           *
      *           postdated flag.                                           *
      *                                                                     *
      *       (V) Modify Receipt.                                           *
      *           Below fields can only be modify: Cheque Number, Type,     *
      *           Date, Bank Description.                                   *
      *           If receipt is postdated and not yet printed, all          *
      *           field can be edited.                                      *
      *                                                                     *
      * 13/12/10  01/01   V76F13       Josefa Francisco De Jesus/SGP/       *
      *           Credit Card Set-up and Details.                           *
      *           ===============================                           *
      *           Introduce option switch for credit card details.
      *           - CHGFLAG field is credit card indicator if               *
      *             credit card mode has been change.  This will            *
      *             trigger to re-visit the credit card pop screen.         *
      *                                                                     *
      *           Support for 2 modes of Credit Card Payment.               *
      *           1) Online Authorization.                                  *
      *           2) Point of sale.                                         *
      *                                                                     *
      *           Modify program to writes the GL Account Key in RTRN       *
      *           to read T3688-Credit Card GL account for Credit card      *
      *           receipt payment.                                     ,    *
      *                                                                     *
      *           Credit card Payment cannot be combine with other          *
      *           payment mode and allow only 1 RBNK record.  As credit     *
      *           card receipt post in different GL Account, this           *
      *           validation is imposed.  And RTRN dissection in S2610      *
      *           cannot be be identify per RBNK record.                    *
      *                                                                     *
      * 12/01/11  01/01   V76F06       Wang Ge/FSG/CSC (Singapore)          *
      *           Rounding apply to:                                        *
      *           - SR289-DOCORIGAMT: input amount                          *
      *           - SR289-DOCACCTAMT: accounting amount                     *
      *           If the input amount does not match with the rounding      *
      *           amount, pop-up error message.                             *
      *                                                                     *
      * 08/10/13  01/01   RC002        Khang Nguyen - CSC Developer         *
      *           APPLY PRE-PRINTED RECEIPT                                 *
      *           use CURRFROM variable to transfer the Deposite Date       *
      *           to S2610 screen.                                          *
      *           Use WSSP-SERVUNIT to transfer the Pre-Printed Code        *
      *                                                                     *
      * 09/10/13  01/01   RC002        Thoai Anh - CSC Developer            *
      *           LEAVE THE RECEIPT DATE BLANK. USER MANUALLY ENTERS IT     *
      *                                                                     *
      * 24/10/13  01/01   RC002        Sang Nguyen - CSC Developer          *
      *           No check when re-printed number EQ 0000000                *
      *                                                                     *
      * 25/10/13  01/01   GAPPH1       Thanh Do                             *
      *           Set Default Payment Type for Each Bank Code.              *
      *                                                                     *
      * 12/03/14  01/01   PHE001       Thanh Do                             *
      *           Recompile.                                                *
      *                                                                     *
      * 13/03/14  01/01   PHLRMS       Phuong Le Dev                        *
      *           Add 2 fields DENTTYP , ENTYNO                             *
      *
      * 24/09/14  01/01   PHLRMS       Phuong Le Dev                        *
      *           Compare Assign Date instead of Printed Date               *
      *                                                                     *
      * 16/01/15  01/01   PHE003       Phuong Le Dev                        *
      *           Fix : - Skip error if not found in ZPPR                   *
      *                 - Only insert RCOL if Payment Type = '1'(Cash)      *
      *                                                                     *
      * 24/01/18  01/01   PS008        Tuyet Huynh IT - DEV                 *
      *           Update status of receipt when banked cash.                *
      *                                                                     *
      * 14/05/18  01/01   PHFX30       Phi Tran - IT DEV                    *
      *           Add Condition for Expiry Warning.                         *
      *                                                                     *
      * 15/06/18  01/01   CS009        Tuyet Huynh IT - DEV                 *
      *           Link from RMS information to Receipt.                     *
      *                                                                     *
      * 11/10/18  01/01   CS012        Thanh Do                             *
      *           Only allow Main Client when creating receipt.             *
      *                                                                     *
      * 27/02/19  01/01   CS016        Tuyet Huynh IT - DEV                 *
      *           Action M: only modify Payor and Application No.           *
      *           when creation receipt date = busn date.
      *                                                                     *
      * 04/12/19  01/01   PHFX57       Van Bao Tuyen - IT                   *
      *           Fix error link receipt number when creat receipt.         *
      *                                                                     *
      * 25/04/20  01/01   PS045        Van Bao Tuyen - IT                   *
      *           Remove auto display receipt number when receipt           *
      *           is created by bank.                                       *
      *                                                                     *
      * 14/10/20  01/01   PS070        Mai Yen Phi - IT                     *
      *           Recompile                                                 *
      *                                                                     *
      **DD/MM/YY*************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       01  WSAA-PROG                   PIC X(05) VALUE 'PR289'.
       01  WSAA-VERSION                PIC X(02) VALUE '01'.
      *
       01  WSAA-EXP-RCPT               PIC X(01).                       <PHLRMS>
       01  WSAA-AMNT                   PIC S9(15)V9(02).                <PHLRMS>
       01  WSAA-WARN                   PIC X(20)                        <PHLRMS>
           VALUE 'Confirm to continue:'.                                <PHLRMS>
       01  WSAA-FOUND                  PIC X(01).                       <PHLRMS>
       01  WSAA-VALID-RECEIPT          PIC X(01).                       <PHLRMS>
       01  WSAA-TABLE                  PIC X(05).                       <PHLRMS>
       01  WSAA-ITEM                   PIC X(08).                       <PHLRMS>
       01  WSAA-TODAY                  PIC 9(08).
       01  WSAA-LINE-CNT               PIC 9(02).
       01  WSAA-SCRN-STATUZ            PIC X(04).
       01  WSAA-DOCTYPE                PIC X(01).
       01  WSAA-CHEQNO                 PIC X(09).
       01  WSAA-ORIGCCY                PIC X(03).
       01  WSAA-SCRATE                 PIC S9(05)V9(07).
       01  WSAA-ACCTCCY                PIC X(03).
       01  WSAA-CASHNM                 PIC X(47).
       01  WSAA-TRANDATEX              PIC 9(08).
       01  WSAA-CLNT-FUNCTION          PIC X(04) VALUE 'CLNT'.
       01  WSAA-PAYEE-GIVN             PIC X(05) VALUE 'PYNMN'.
       01  WSAA-DOCORIGAMT             PIC S9(15)V9(2) VALUE ZEROES.    <PS008>
      *
       01  WSAA-TOT-DOCORIGAMT         PIC S9(15)V9(2).
       01  WSAA-TOT-DOCACCTAMT         PIC S9(15)V9(2).
      *
       01  WSAA-NEW-RECEIPT            PIC X(09).
      *
       01  WSAA-DOCTKEY.
           03  WSAA-RECEIPT-PFX        PIC X(02).
           03  WSAA-RECEIPT-COY        PIC X(01).
           03  WSAA-RECEIPT            PIC X(09).
      *                                                                 <V64F13>
       01  WSAA-TR386-KEY.                                              <V64F13>
           03  WSAA-TR386-LANG         PIC X(01).                       <V64F13>
           03  WSAA-TR386-PGM          PIC X(05).                       <V64F13>
           03  FILLER                  PIC X(02).                       <V64F13>
                                                                        <V64F13>
       01  WSAA-LITR-LEN-18            PIC S9(02) COMP-3 VALUE 18.      <V64F13>
      *
       01  WSAA-FOUND-SELECTION        PIC X(01) VALUE 'N'.
           88   FOUND-SELECTION        VALUE 'Y'.
      *                                                                 <V64F13>
       01  WSAA-ALNO-STATUS            PIC X(01) VALUE 'N'.             <V64F13>
           88   ALNO-EXHAUSTED         VALUE 'Y'.                       <V64F13>
      *
       01  WSAA-RTYP-CASH              PIC X(01) VALUE 'N'.
           88   RTYP-CASH              VALUE 'Y'.
       01  WSAA-RTYP-CHEQ              PIC X(01) VALUE 'N'.
           88   RTYP-CHEQ              VALUE 'Y'.
       01  WSAA-RTYP-BKCD              PIC X(01) VALUE 'N'.
           88   RTYP-BKCD              VALUE 'Y'.
       01  WSAA-RTYP-JNL               PIC X(01) VALUE 'N'.
           88   RTYP-JNL               VALUE 'Y'.
       01  WSAA-RTYP-DSH-CHQ           PIC X(01) VALUE 'N'.
           88   RTYP-DSH-CHQ           VALUE 'Y'.
       01  WSAA-RTYP-BANK-TAPE         PIC X(01) VALUE 'N'.
           88   RTYP-BANK-TAPE         VALUE 'Y'.
       01  WSAA-RTYP-CHEQ-CANCEL       PIC X(01) VALUE 'N'.
           88   RTYP-CHEQ-CANCEL       VALUE 'Y'.
       01  WSAA-RTYP-CREDIT-CARD       PIC X(01) VALUE 'N'.
           88   RTYP-CREDIT-CARD       VALUE 'Y'.
      *                                                                 <V76F12>
       01  WSAA-PRINTED                PIC X(01) VALUE ' '.             <V76F12>
           88 PRINTED                  VALUE 'Y'.                       <V76F12>
      *                                                                 <V76F12>
       01  WSAA-RCTSTATFLG             PIC X(01) VALUE ' '.             <V76F12>
           88 RCTSTATFLG               VALUE 'Y'.                       <V76F12>
      *                                                                 <V76F12>
       01  WSAA-POSTDTEFLG             PIC X(01) VALUE ' '.             <V76F12>
           88 NOT-POSTDATED            VALUE ' '.                       <V76F12>
           88 P-POSTDATED              VALUE 'P'.                       <V76F12>
           88 Y-POSTDATED              VALUE 'Y'.                       <V76F12>
      *                                                                 <V76F12>
       01  WSAA-VALID-STATUS           PIC X(01).                       <V76F12>
           88 VALID-STATUS             VALUE 'Y'.                       <V76F12>
      *                                                                 <V76F12>
       01  WSAA-VALID-STDESCSH         PIC X(01).                       <V76F12>
           88 VALID-STDESCSH           VALUE 'Y'.                       <V76F12>

       01  WSAA-SBMACTION              PIC X(01).
           88 SBM-CREATION             VALUE 'A' 'B'.
      *    88 SBM-MODIFY               VALUE 'K'.               <CS016> <V76F12>
           88 SBM-MODIFY               VALUE 'K' 'M'.                   <CS016>
           88 SBM-INQUIRY              VALUE 'C' 'H'.
           88 SBM-INQUIRY-BALANCE      VALUE 'D'.
           88 SBM-BULK                 VALUE 'E' 'F'.
           88 SBM-CANCELLATION         VALUE 'I'.
           88 SBM-PARTIAL-CANCEL       VALUE 'J'.
           88 VALID-SBM-ACTION         VALUE 'A' 'B' 'C' 'D'
                                             'K' 'L'                    <V76F12>
                                             'E' 'F' 'H' 'I' 'J'.
      *                                                                 <V74F03>
       01  WSAA-ASSIGNED-DATE          PIC S9(08).                      <PHLRMS>
       01  WSAA-PAYMENT-TYPE           PIC X(01).                       <V74F03>
       01  WSAA-ORIG-CURR              PIC X(03).                       <V74F03>
       01  WSAA-PAXMSG.                                                 <V74F03>
           03  WSAA-LANGUAGE           PIC X(01).                       <V74F03>
           03  WSAA-SERVICE-UNIT       PIC X(02) VALUE 'FA'.            <V74F03>
           03  WSAA-MSGID              PIC X(04).                       <V74F03>
       01  WSAA-VALUE.                                                  <V74F03>
           03  WSAA-VALUE1             PIC -ZZZZZZZZZZZZZ9.99.          <V74F03>
       01  WSAA-RMRKKEY.                                                <V76F04>
           03  WSAA-PR25C-RDOCPFX      PIC X(02).                       <V76F04>
           03  WSAA-PR25C-RDOCCOY      PIC X(01).                       <V76F04>
           03  WSAA-PR25C-RDOCNUM      PIC X(09).                       <V76F04>
      *                                                                 <V76F04>
       01  WSAA-REMARKS                PIC X(01).                       <V76F04>
       01  WSAA-REMARKS-ERR            PIC X(04).                       <V76F04>
       01  WSAA-REMARKS-OUT            PIC X(01).                       <V76F04>
      *                                                                 <V76F12>
        01 WSAA-TR29H-ITEM.                                             <V76F12>
           03  WSAA-TR29H-LANG         PIC X(01).                       <V76F12>
           03  WSAA-TR29H-CODE         PIC X(02).                       <V76F12>
           03  FILLER                  PIC X(05).                       <V76F12>
      *                                                                 <V76F12>
        01 WSAA-TR29I-ITEM.                                             <V76F12>
           03  WSAA-TR29I-LANG         PIC X(01).                       <V76F12>
           03  WSAA-TR29I-CODE         PIC X(02).                       <V76F12>
           03  FILLER                  PIC X(05).                       <V76F12>
      *                                                                 <V76F12>
       01  WSAA-FOUND-TEMP-RCPT        PIC X(01).                       <PHLRMS>
       01  IDX                         PIC 9(05) VALUE ZEROES.          <PHLRMS>
       01  IX                          PIC 9(05) VALUE ZEROES.          <V76F12>
       01  WSAA-SFL-RECORD             PIC 9(03) VALUE ZEROES.          <V76F12>
       01  WSAA-NUMBER                 PIC 9(05) VALUE ZEROES.          <V76F12>
       01  WSAA-SEQNBR                 PIC 9(03) VALUE ZEROES.          <V76F12>
       01  WSAA-MAX-RCTSTAT            PIC 9(05) VALUE 8.               <V76F12>
       01  WSAA-MAX-T3676-TRANSCD      PIC 9(03) VALUE 16.              <V76F12>
       01  WSAA-PRV-POSTFG             PIC X(01) VALUE SPACES.          <V76F12>
       01  WSAA-RBNK-POSTDTEFLG        PIC X(01) VALUE SPACES.          <V76F12>
       01  WSAA-RTRN-EXIST             PIC X(01) VALUE SPACES.          <V76F12>
       01  WSAA-RCPREPRTCD             PIC X(02) VALUE SPACES.          <PHLRMS>
       01  WSAA-RCPREPRNT              PIC X(08) VALUE SPACES.          <PHLRMS>
       01  WSAA-TMP-RCPREPRTCD         PIC X(02) VALUE SPACES.          <PHLRMS>
       01  WSAA-TMP-RCPREPRNT          PIC X(08) VALUE SPACES.          <PHLRMS>
       01  WSAA-TMP-AMOUNT             PIC S9(15)V9(02).                <PHLRMS>
      *                                                                 <V76F13>
       01  WSAA-C6                     PIC X(02) VALUE 'C6'.            <V76F13>
       01  WSAA-MAX-OPT                PIC 9(02) VALUE 20.              <V76F13>
       01  WSAA-COUNTER                PIC 9(01) VALUE ZEROS.           <RC002>
      *                                                                 <GAPPH1>
       01  WSAA-EXIST-ITEM             PIC X(01).                       <GAPPH1>
       01  WSAA-DUPLICATE-FLAG         PIC X(01).                       <RC002>
      *                                                                 <PHFX30>
       01  WSAA-EXPIRY-WARN            PIC X(01).                       <PHFX30>
      *
       01  ERRORS.
           03  E071                    PIC X(04) VALUE 'E071'.
           03  E133                    PIC X(04) VALUE 'E133'.
           03  E165                    PIC X(04) VALUE 'E165'.
           03  E190                    PIC X(04) VALUE 'E190'.
           03  E207                    PIC X(04) VALUE 'E207'.
           03  E305                    PIC X(04) VALUE 'E305'.
           03  E423                    PIC X(04) VALUE 'E423'.
           03  E456                    PIC X(04) VALUE 'E456'.
           03  E570                    PIC X(04) VALUE 'E570'.
           03  F022                    PIC X(04) VALUE 'F022'.
           03  F401                    PIC X(04) VALUE 'F401'.
           03  F530                    PIC X(04) VALUE 'F530'.
           03  F665                    PIC X(04) VALUE 'F665'.
           03  F857                    PIC X(04) VALUE 'F857'.
           03  F906                    PIC X(04) VALUE 'F906'.
           03  F982                    PIC X(04) VALUE 'F982'.
           03  H931                    PIC X(04) VALUE 'H931'.
           03  H933                    PIC X(04) VALUE 'H933'.
           03  RF29                    PIC X(04) VALUE 'RF29'.
           03  RF30                    PIC X(04) VALUE 'RF30'.
           03  RPAO                    PIC X(04) VALUE 'RPAO'.
           03  TL24                    PIC X(04) VALUE 'TL24'.
           03  H452                    PIC X(04) VALUE 'H452'.          <V64F13>
           03  RFHM                    PIC X(04) VALUE 'RFHM'.          <V76F12>
           03  E186                    PIC X(04) VALUE 'E186'.          <V76F12>
           03  E031                    PIC X(04) VALUE 'E031'.          <V76F12>
           03  E267                    PIC X(04) VALUE 'E267'.          <V76F12>
           03  W121                    PIC X(04) VALUE 'W121'.          <V76F13>
           03  H118                    PIC X(04) VALUE 'H118'.          <V76F13>
           03  RFI1                    PIC X(04) VALUE 'RFI1'.          <V76F12>
           03  RFIK                    PIC X(04) VALUE 'RFIK'.          <V76F06>
           03  F073                    PIC X(04) VALUE 'F073'.          <V76F12>
           03  RFIP                    PIC X(04) VALUE 'RFIP'.          <V76F12>
           03  RFIQ                    PIC X(04) VALUE 'RFIQ'.          <V76F12>
           03  R012                    PIC X(04) VALUE 'R012'.          <V76F13>
           03  RFKV                    PIC X(04) VALUE 'RFKV'.          <V76F12>
           03  EZ23                    PIC X(04) VALUE 'EZ23'.          <RC002>
           03  E048                    PIC X(04) VALUE 'E048'.          <RC002>
           03  E901                    PIC X(04) VALUE 'E901'.          <RC002>
           03  H366                    PIC X(04) VALUE 'H366'.          <RC002>
           03  EV26                    PIC X(04) VALUE 'EV26'.          <PHLRMS>
           03  EV31                    PIC X(04) VALUE 'EV31'.          <PHLRMS>
           03  EV17                    PIC X(04) VALUE 'EV17'.          <PHLRMS>
           03  F200                    PIC X(04) VALUE 'F200'.          <PHLRMS>
           03  G822                    PIC X(04) VALUE 'G822'.          <PHLRMS>
           03  EV53                    PIC X(04) VALUE 'EV53'.          <PHLRMS>
           03  EV58                    PIC X(04) VALUE 'EV58'.          <PHLRMS>
           03  EV63                    PIC X(04) VALUE 'EV63'.          <PHLRMS>
           03  P245                    PIC X(04) VALUE 'P245'.          <CS012>
      *
       01  TABLES.
           03  T3629                   PIC X(05) VALUE 'T3629'.
           03  T3674                   PIC X(05) VALUE 'T3674'.
           03  T3676                   PIC X(05) VALUE 'T3676'.         <V76F12>
           03  T3688                   PIC X(05) VALUE 'T3688'.
           03  TR386                   PIC X(05) VALUE 'TR386'.         <V64F13>
           03  TR29S                   PIC X(05) VALUE 'TR29S'.         <V76F12>
           03  TR29I                   PIC X(05) VALUE 'TR29I'.         <V76F12>
           03  TR29H                   PIC X(05) VALUE 'TR29H'.         <V76F12>
      *    03  TZ005                   PIC X(05) VALUE 'TZ005'. <PHLRMS><RC002>
           03  TV021                   PIC X(05) VALUE 'TV021'.         <PHLRMS>
           03  TV007                   PIC X(05) VALUE 'TV007'.         <GAPPH1>
           03  TV023                   PIC X(05) VALUE 'TV023'.         <PHLRMS>
           03  TV037                   PIC X(05) VALUE 'TV037'.         <PHLRMS>
      *
       01  FORMATS.
           03  ADOCREC                 PIC X(10) VALUE 'ADOCREC'.
           03  AGNTREC                 PIC X(10) VALUE 'AGNTREC'.
           03  BABRREC                 PIC X(10) VALUE 'BABRREC'.
           03  CLNTREC                 PIC X(10) VALUE 'CLNTREC'.
           03  ITEMREC                 PIC X(10) VALUE 'ITEMREC'.
           03  RBNKREC                 PIC X(10) VALUE 'RBNKREC'.
           03  RDOCREC                 PIC X(10) VALUE 'RDOCREC'.
           03  RTRNREC                 PIC X(10) VALUE 'RTRNREC'.
           03  RMRKREC                 PIC X(10) VALUE 'RMRKREC'.       <V76F04>
           03  RCPTREC                 PIC X(10) VALUE 'RCPTREC'.       <V76F12>
           03  RBNKSEQREC              PIC X(10) VALUE 'RBNKSEQREC'.    <V76F13>
           03  RBNKCHQREC              PIC X(10) VALUE 'RBNKCHQREC'.    <V76F12>
           03  DESCREC                 PIC X(10) VALUE 'DESCREC'.       <V76F12>
           03  ZPPRENQREC              PIC X(10) VALUE 'ZPPRENQREC'.    <RC002>
           03  ZPPRCHKREC              PIC X(10) VALUE 'ZPPRCHKREC'.    <RC002>
           03  TRRNINFREC              PIC X(10) VALUE 'TRRNINFREC'.    <PHLRMS>
           03  RCOLPNDREC              PIC X(10) VALUE 'RCOLPNDREC'.    <PHLRMS>
           03  RCOLINFREC              PIC X(10) VALUE 'RCOLINFREC'.    <PHLRMS>
           03  RCPYACTREC              PIC X(10) VALUE 'RCPYACTREC'.    <PHLRMS>
           03  TRRNCDEREC              PIC X(10) VALUE 'TRRNCDEREC'.    <PHLRMS>
           03  RCPYCHRREC              PIC X(10) VALUE 'RCPYCHRREC'.    <PS008>
           03  RCPYPOLREC              PIC X(10) VALUE 'RCPYPOLREC'.    <CS009>
           03  RCPYPCDREC              PIC X(10) VALUE 'RCPYPCDREC'.    <CS009>
           03  CHDRLIFREC              PIC X(10) VALUE 'CHDRLIFREC'.    <CS009>
      /
       01  WSAA-BATCKEY.
           COPY BATCKEY.
      /
       01  WSAA-CLNTKEY.
           COPY CLNTKEY.
      /
           COPY VARCOM.
           COPY CASHTYPCPY.
           COPY CSHRFCPY.
           COPY FSUPFXCPY.
           COPY SMTPFXCPY.
           COPY DBCSTRNCPY.                                             <V64F13>
      /
           COPY ALOCNOREC.
           COPY CASHEDREC.
           COPY CHKCURRREC.
           COPY DATCON1REC.
           COPY GETCLNTREC.
           COPY NAMADRSREC.
           COPY OPSTATSREC.
           COPY SYSERRREC.
           COPY CHKFSUFREC.                                             <V64F13>
           COPY AMLCHKREC.                                              <V74F03>
           COPY MSGBOXREC.                                              <V74F03>
           COPY OPTSWCHREC.                                             <V76F04>
           COPY ZRDECPLREC.                                             <V76F06>
           COPY RCTSTATCPY.                                             <V76F12>
           COPY CSHBCHCPY.                                              <V76F12>
      /
           COPY ADOCSKM.
           COPY AGNTSKM.
           COPY BABRSKM.
           COPY CLNTSKM.
           COPY ITEMSKM.
           COPY RBNKSKM.
           COPY RDOCSKM.
           COPY RMRKSKM.                                                <V76F04>
           COPY RCPTSKM.                                                <V76F12>
           COPY RBNKSEQSKM.                                             <V76F13>
           COPY RBNKCHQSKM.                                             <V76F12>
           COPY GETPAYRREC.                                             <V76F07>
           COPY DESCSKM.                                                <V76F12>
           COPY ZPPRENQSKM.                                             <RC002>
           COPY ZPPRCHKSKM.                                             <RC002>
           COPY TRRNINFSKM.                                             <PHLRMS>
           COPY TRRNCDESKM.                                             <PHLRMS>
           COPY RCOLPNDSKM.                                             <PHLRMS>
           COPY RCOLINFSKM.                                             <PHLRMS>
           COPY RCPYACTSKM.                                             <PHLRMS>
           COPY RCPYCHRSKM.                                             <PS008>
           COPY RCPYPOLSKM.                                             <CS009>
           COPY RCPYPCDSKM.                                             <CS009>
           COPY CHDRLIFSKM.                                             <CS009>
      /
           COPY T3629REC.
           COPY T3676REC.                                               <V76F12>
           COPY T3688REC.
           COPY TR386REC.                                               <V64F13>
           COPY TR29SREC.                                               <V76F12>
           COPY TV007REC.                                               <GAPPH1>
           COPY TV023REC.                                               <PHLRMS>
           COPY TV037REC.                                               <PHLRMS>
      /
       LINKAGE SECTION.

           COPY WSSPCOMN.

           COPY WSSPDOCS.

           COPY SCRNPARAMS.

           COPY SR289SCR.
      /
       PROCEDURE DIVISION USING WSSP-COMMON-AREA WSSP-USER-AREA
                                SCRN-SCREEN-PARAMS SR289-DATA-AREA
                                SR289-SUBFILE-AREA.

           COPY MAING.
      /
      *****************************************************************
      *      INITIALISE FIELDS FOR SHOWING ON SCREEN
      *****************************************************************
      *
       1000-INITIALISE SECTION.
      *************************
      *
       1010-INITIALISE.
      *                                                                 <V76F04>
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'                    <V76F04>
                GO TO 1090-EXIT                                         <V76F04>
           END-IF.                                                      <V76F04>
      * If switch from P2610 (F13) , skip this section.
           IF WSSP-LASTPROG(2:4)        = '2610'
              MOVE 1                   TO SCRN-SUBFILE-RRN
              GO TO 1090-EXIT
           END-IF.

           PERFORM 1100-INITIALIZATION.
                                                                        <V64F13>
           PERFORM C1000-TR386-LITERALS.                                <V64F13>
                                                                        <V64F13>
           PERFORM C1500-DEFAULT-FIELDS.                                <V64F13>
                                                                        <V64F13>
           PERFORM 1200-LOAD-HEADER.

           PERFORM 1300-LOAD-SUBFILE.

           PERFORM 1400-KEEPS-RBNK.
           PERFORM 1500-READ-TV037.                                     <PHLRMS>

      **** MOVE SR289-TRANDATEX        TO WSSP-TRANDATE.                <FA4514>
           MOVE WSAA-DOCTYPE           TO WSSP-DOCTYPE.

      * Show accumulated amount from existing RBNK records.
           MOVE WSAA-TOT-DOCORIGAMT    TO SR289-ORIGAMT
                                          WSSP-DOCORIGAMT.
           MOVE WSAA-TOT-DOCACCTAMT    TO SR289-ACCTAMT
                                          WSSP-DOCACCTAMT.
      *                                                                 <PS008>
      * Obtain the infos of Pre-Printed Documents                       <RC002>
                                                                        <RC002>
           INITIALIZE                     ZPPRENQ-PARAMS.               <RC002>
           MOVE PRFX-CASH              TO ZPPRENQ-PREFIX.               <RC002>
           MOVE WSSP-COMPANY           TO ZPPRENQ-COMPANY.              <RC002>
           MOVE WSSP-BANKCODE          TO ZPPRENQ-BANKCODE.             <RC002>
           MOVE SR289-RECEIPT          TO ZPPRENQ-RECEIPT.              <RC002>
     ****  MOVE  '1'                   TO ZPPRENQ-VALIDFLAG.            <RC002>
                                                                        <RC002>
           MOVE ZPPRENQREC             TO ZPPRENQ-FORMAT.               <RC002>
           MOVE BEGN                   TO ZPPRENQ-FUNCTION.             <RC002>
                                                                        <RC002>
       CALL-ZPPRENQ-IO.                                                 <RC002>
                                                                        <RC002>
           CALL 'ZPPRENQIO'         USING ZPPRENQ-PARAMS.               <RC002>
                                                                        <RC002>
           IF ZPPRENQ-STATUZ        NOT = O-K AND ENDP                  <RC002>
               MOVE ZPPRENQ-STATUZ     TO SYSR-STATUZ                   <RC002>
               MOVE ZPPRENQ-PARAMS     TO SYSR-PARAMS                   <RC002>
               PERFORM 600-FATAL-ERROR                                  <RC002>
           END-IF.                                                      <RC002>
                                                                        <RC002>
           IF ZPPRENQ-STATUZ            = O-K                           <RC002>
           AND ZPPRENQ-PREFIX           = PRFX-CASH                     <RC002>
           AND ZPPRENQ-COMPANY          = WSSP-COMPANY                  <RC002>
           AND ZPPRENQ-RECEIPT          = SR289-RECEIPT                 <RC002>
     ****  AND ZPPRENQ-BANKCODE         = WSSP-BANKCODE                 <RC002>
               MOVE ZPPRENQ-RCPREPRTCD TO SR289-RCPREPRTCD              <RC002>
               MOVE ZPPRENQ-RCPREPRNT  TO SR289-RCPREPRNT               <RC002>
               GO TO 1090-EXIT                                          <RC002>
           END-IF.                                                      <RC002>
                                                                        <RC002>
                                                                        <RC002>
       1090-EXIT.
            EXIT.
      /
       1100-INITIALIZATION SECTION.
      *****************************
       1100-BEGIN.

           INITIALIZE                     SR289-DATA-AREA.
           INITIALIZE                     SR289-SUBFILE-AREA.
                                                                        <PHLRMS>
           MOVE 'N'                    TO SR289-CONFIRM.                <PHLRMS>
           MOVE 'Y'                    TO SR289-CONFIRM-OUT  (PR).      <PHLRMS>
           MOVE 'Y'                    TO SR289-CONFIRM-OUT  (ND).      <PHLRMS>
           MOVE SPACES                 TO WSAA-TMP-RCPREPRTCD           <PHLRMS>
                                          WSAA-TMP-RCPREPRNT.           <PHLRMS>
           MOVE ZEROES                 TO WSAA-TMP-AMOUNT   .           <PHLRMS>
           MOVE 'N'                    TO WSAA-EXPIRY-WARN.             <PHFX30>
                                                                        <PHLRMS>
      *
           MOVE SCLR                   TO SCRN-FUNCTION.
           PERFORM 9000-SCREEN-IO.
           MOVE 1                      TO SCRN-SUBFILE-RRN.
      *
           MOVE VRCM-MAX-DATE          TO SR289-TRANDATEX.
           MOVE SPACES                 TO SR289-BANKCODE
                                          SR289-SHORTDESC
                                          SR289-RFCODE
                                          SR289-RFNUM
                                          SR289-POSTDTEFLG              <V76F12>
                                          SR289-CASHNM.
           MOVE ZEROES                 TO SR289-ACCTAMT
                                          SR289-ORIGAMT.
           MOVE ZEROES                TO WSAA-DOCORIGAMT.               <PS008>
      *
           MOVE SPACES                 TO WSAA-SCRN-STATUZ
                                          WSAA-DOCTYPE
                                          WSAA-CHEQNO
                                          WSAA-ORIGCCY
                                          WSAA-ACCTCCY
                                          WSAA-CASHNM
                                          WSAA-POSTDTEFLG               <V76F12>
                                          WSAA-PRINTED                  <V76F12>
                                          WSAA-PRV-POSTFG               <V76F12>
                                          WSAA-RBNK-POSTDTEFLG          <V76F12>
                                          WSAA-RTRN-EXIST               <V76F12>
                                          WSAA-NEW-RECEIPT.

           MOVE VRCM-MAX-DATE          TO WSAA-TODAY
                                          WSAA-TRANDATEX.
           MOVE ZEROES                 TO WSAA-LINE-CNT
                                          WSAA-SEQNBR                   <V76F12>
                                          WSAA-SCRATE
                                          WSAA-TOT-DOCORIGAMT
                                          WSAA-TOT-DOCACCTAMT.
           MOVE ZEROES                 TO WSSP-SUBFILE-END
                                          WSSP-SUBFILE-RRN.
      *
           MOVE WSSP-DOCTKEY           TO WSAA-DOCTKEY.
           MOVE WSSP-BATCHKEY          TO WSAA-BATCKEY.
           MOVE WSSP-SBMACTION         TO WSAA-SBMACTION.
                                                                        <PHLRMS>
           IF  WSAA-SBMACTION          NOT = 'A'                        <PHLRMS>
               MOVE 'Y'                TO SR289-RCPREPRTCD-OUT (PR)     <PHLRMS>
               MOVE 'Y'                TO SR289-RCPREPRNT-OUT  (PR)     <PHLRMS>
      **       MOVE 'Y'                TO SR289-DENTTYP-OUT    (PR)     <PHLRMS>
      **       MOVE 'Y'                TO SR289-ENTYNO-OUT     (PR)     <PHLRMS>
           ELSE                                                         <PHLRMS>
               MOVE 'N'                TO SR289-RCPREPRTCD-OUT (PR)     <PHLRMS>
               MOVE 'N'                TO SR289-RCPREPRNT-OUT  (PR)     <PHLRMS>
      **       MOVE 'N'                TO SR289-DENTTYP-OUT    (PR)     <PHLRMS>
      **       MOVE 'N'                TO SR289-ENTYNO-OUT     (PR)     <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PS008>
           IF  WSAA-SBMACTION          = 'B'                            <PS008>
               MOVE 'N'                TO SR289-RCPREPRTCD-OUT (PR)     <PS008>
               MOVE 'N'                TO SR289-RCPREPRNT-OUT  (PR)     <PS008>
           END-IF.                                                      <PS008>

      * This SR289-PROGNAM is a hidden field which is used
      * for Field Switching (Field RFNUM) in table T3568
           MOVE WSAA-PROG              TO SR289-PROGNAM.

      *
      * Get today's date and time.
      *
           PERFORM 1110-GET-TODAY.
      *
      * Read T3688 To get Prefix for Receipt Number
      *
           IF WSSP-BANKCODE        NOT = SPACES                         <FA3287>
           PERFORM 1120-READ-T3688.
      *
      * Retrieve the Ledger Currency.
      *
           PERFORM 1130-READ-T3629.
      *                                                                 <V76F12>
      * Retrieve Receipt parameters                                     <V76F12>
      *                                                                 <V76F12>
           PERFORM 1140-READ-TR29S.                                     <V76F12>
      *                                                                 <GAPPH1>
      * Read Default Payment Type for Bank Code:                        <GAPPH1>
      *                                                                 <GAPPH1>
           PERFORM 1150-READ-TV007.                                     <GAPPH1>
      *
       1100-EXIT.
            EXIT.
      /
       1110-GET-TODAY SECTION.
      ************************
       1110-BEGIN.
           MOVE TDAY                   TO DTC1-FUNCTION.
           CALL 'DATCON1' USING DTC1-DATCON1-REC.
           MOVE DTC1-INT-DATE          TO WSAA-TODAY.

       1110-EXIT.                                                       <PHLRMS>
            EXIT.
      /
       1120-READ-T3688 SECTION.
      *************************
       1120-BEGIN.

           INITIALIZE                     ITEM-DATA-KEY.
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.
           MOVE T3688                  TO ITEM-ITEMTABL.
           MOVE WSSP-BANKCODE          TO ITEM-ITEMITEM.
           PERFORM B1000-READR-ITEM.
           IF  ITEM-STATUZ          NOT = O-K
               MOVE ITEM-STATUZ        TO SYSR-STATUZ
               MOVE ITEM-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE ITEM-GENAREA           TO T3688-T3688-REC.
       1120-EXIT.
            EXIT.
      /
       1130-READ-T3629 SECTION.
      *************************
       1130-BEGIN.

           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.
           MOVE T3629                  TO ITEM-ITEMTABL.
           MOVE SPACES                 TO ITEM-ITEMITEM.
           MOVE BEGN                   TO ITEM-FUNCTION.
           CALL 'ITEMIO'            USING ITEM-PARAMS.

           IF ITEM-STATUZ           NOT = O-K AND MRNF
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE ITEM-GENAREA           TO T3629-T3629-REC.
       1130-EXIT.
            EXIT.
      /                                                                 <V76F12>
       1140-READ-TR29S SECTION.                                         <V76F12>
      *************************                                         <V76F12>
       1140-BEGIN.                                                      <V76F12>
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.                 <V76F12>
           MOVE WSSP-FSUCO             TO ITEM-ITEMCOY.                 <V76F12>
           MOVE TR29S                  TO ITEM-ITEMTABL.                <V76F12>
           MOVE WSSP-COMPANY           TO ITEM-ITEMITEM.                <V76F12>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <V76F12>
           MOVE READR                  TO ITEM-FUNCTION.                <V76F12>
           CALL 'ITEMIO'            USING ITEM-PARAMS.                  <V76F12>
           IF ITEM-STATUZ           NOT = O-K AND MRNF                  <V76F12>
              MOVE ITEM-STATUZ         TO SYSR-STATUZ                   <V76F12>
              MOVE ITEM-PARAMS         TO SYSR-PARAMS                   <V76F12>
              PERFORM 600-FATAL-ERROR                                   <V76F12>
           END-IF.                                                      <V76F12>
           IF ITEM-STATUZ               = O-K                           <V76F12>
              MOVE ITEM-GENAREA        TO TR29S-TR29S-REC               <V76F12>
           ELSE                                                         <V76F12>
              MOVE SPACES              TO TR29S-TR29S-REC               <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
           MOVE TR29S-APPFLAG          TO WSAA-RCTSTATFLG.              <V76F12>
       1140-EXIT.                                                       <V76F12>
           EXIT.                                                        <V76F12>
      /                                                                 <GAPPH1>
       1150-READ-TV007 SECTION.                                         <GAPPH1>
      *************************                                         <GAPPH1>
       1151-START.                                                      <GAPPH1>
      *                                                                 <GAPPH1>
           INITIALIZE                  ITEM-DATA-KEY.                   <GAPPH1>
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.                 <GAPPH1>
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.                 <GAPPH1>
           MOVE TV007                  TO ITEM-ITEMTABL.                <GAPPH1>
           MOVE WSSP-BANKCODE          TO ITEM-ITEMITEM.                <GAPPH1>
           PERFORM B1000-READR-ITEM.                                    <GAPPH1>
           IF  ITEM-STATUZ              = O-K                           <GAPPH1>
               MOVE ITEM-GENAREA       TO TV007-TV007-REC               <GAPPH1>
           ELSE                                                         <GAPPH1>
               MOVE SPACES             TO TV007-TV007-REC               <GAPPH1>
           END-IF.                                                      <GAPPH1>
      *                                                                 <GAPPH1>
       1159-EXIT.                                                       <GAPPH1>
           EXIT.                                                        <GAPPH1>
      /
       1160-TAKE-STATUS-POLICY SECTION.                                 <CS009>
      *********************************                                 <CS009>
       1161-START.                                                      <CS009>
      *                                                                 <CS009>
           INITIALIZE                  CHDRLIF-PARAMS.                  <CS009>
           MOVE 'CH'                   TO CHDRLIF-CHDRPFX.              <CS009>
           MOVE '2'                    TO CHDRLIF-CHDRCOY.              <CS009>
           MOVE WSSP-CHDRKEY(4:8)      TO CHDRLIF-CHDRNUM.              <CS009>
           MOVE CHDRLIFREC             TO CHDRLIF-FORMAT.               <CS009>
           MOVE READR                  TO CHDRLIF-FUNCTION.             <CS009>
                                                                        <CS009>
           CALL 'CHDRLIFIO'            USING CHDRLIF-PARAMS.            <CS009>
                                                                        <CS009>
           IF  CHDRLIF-STATUZ          NOT = O-K                        <CS009>
               MOVE CHDRLIF-PARAMS     TO SYSR-PARAMS                   <CS009>
               PERFORM 600-FATAL-ERROR                                  <CS009>
           END-IF.                                                      <CS009>
      *                                                                 <CS009>
       1169-EXIT.                                                       <CS009>
           EXIT.                                                        <CS009>
      /                                                                 <CS009>
       1200-LOAD-HEADER SECTION.
      **************************
       1200-BEGIN.
           EVALUATE WSAA-SBMACTION
             WHEN 'A'
              PERFORM 1220-GET-NEW-RECEIPT-NUMBER
              PERFORM 1230-LOAD-NEW-HEADER
              IF WSSP-CHDRKEY        NOT = SPACES                       <CS009>
                 PERFORM 1160-TAKE-STATUS-POLICY                        <CS009>
                 IF CHDRLIF-STATCODE = 'LA'                             <CS009>
                    PERFORM A2920-LOAD-RECEIPT                          <CS009>
                 ELSE                                                   <CS009>
                    PERFORM A2910-LOAD-RN-RECEIPT                       <CS009>
                 END-IF                                                 <CS009>
              END-IF                                                    <CS009>
             WHEN 'B'
              PERFORM 1220-GET-NEW-RECEIPT-NUMBER
              PERFORM 1230-LOAD-NEW-HEADER
      *       IF WSSP-CHDRKEY        NOT = SPACES               <PS045> <CS009>
      *          PERFORM A2900-LOAD-RN-RECEIPT                  <PS045> <CS009>
      *          PERFORM A2910-LOAD-RN-RECEIPT                          <CS009>
      *       END-IF                                            <PS045> <CS009>
             WHEN 'C'
              PERFORM 1210-READ-ADOC
              PERFORM C1800-RBNK-MARRYFLAG                              <V64F13>
             WHEN 'H'
              PERFORM 1210-READ-ADOC
              PERFORM C1800-RBNK-MARRYFLAG                              <V64F13>
             WHEN 'I'
              PERFORM 1220-GET-NEW-RECEIPT-NUMBER
              PERFORM 1210-READ-ADOC
              PERFORM A600-READ-ZPPRENQ                                 <RC002>
              PERFORM C1800-RBNK-MARRYFLAG                              <V64F13>
             WHEN 'J'
              PERFORM 1220-GET-NEW-RECEIPT-NUMBER
              PERFORM 1210-READ-ADOC
              PERFORM C1800-RBNK-MARRYFLAG                              <V64F13>
             WHEN 'K'                                                   <V76F12>
              PERFORM 1210-READ-ADOC                                    <V76F12>
              PERFORM C1800-RBNK-MARRYFLAG                              <CS016>
             WHEN 'M'                                                   <CS016>
              PERFORM 1210-READ-ADOC                                    <CS016>
              PERFORM C1800-RBNK-MARRYFLAG                              <CS016>
           END-EVALUATE.
                                                                        <V76F04>
           PERFORM A100-KEEP-RMRK.                                      <V76F04>
                                                                        <V76F04>
           PERFORM A200-INIT-OPTS.                                      <V76F04>
                                                                        <V76F04>
       1200-EXIT.
            EXIT.
      /
       A600-READ-ZPPRENQ SECTION.                                       <RC002>
      ***************************                                       <RC002>
       A610-START.                                                      <RC002>
           INITIALIZE                     ZPPRENQ-PARAMS.               <RC002>
           MOVE PRFX-CASH              TO ZPPRENQ-PREFIX.               <RC002>
           MOVE WSSP-COMPANY           TO ZPPRENQ-COMPANY.              <RC002>
      ***  MOVE WSSP-BANKCODE          TO ZPPRENQ-BANKCODE.             <RC002>
           MOVE ADOC-RTRN-RDOCNUM      TO ZPPRENQ-RECEIPT.              <RC002>
      ***  MOVE  '1'                   TO ZPPRENQ-VALIDFLAG.            <RC002>
                                                                        <RC002>
           MOVE ZPPRENQREC             TO ZPPRENQ-FORMAT.               <RC002>
           MOVE BEGN                   TO ZPPRENQ-FUNCTION.             <PHE003>
                                                                        <RC002>
           CALL 'ZPPRENQIO'         USING ZPPRENQ-PARAMS.               <RC002>
                                                                        <RC002>
           IF ZPPRENQ-STATUZ        NOT = O-K                           <RC002>
           AND                      NOT = ENDP                          <PHE003>
               MOVE ZPPRENQ-STATUZ     TO SYSR-STATUZ                   <RC002>
               MOVE ZPPRENQ-PARAMS     TO SYSR-PARAMS                   <RC002>
               PERFORM 600-FATAL-ERROR                                  <RC002>
           END-IF.                                                      <RC002>
                                                                        <RC002>
           IF ZPPRENQ-STATUZ            = O-K                           <RC002>
           AND ZPPRENQ-PREFIX           = PRFX-CASH                     <RC002>
           AND ZPPRENQ-COMPANY          = WSSP-COMPANY                  <RC002>
           AND ZPPRENQ-RECEIPT          = ADOC-RTRN-RDOCNUM             <RC002>
      ***  AND ZPPRENQ-BANKCODE         = WSSP-BANKCODE                 <RC002>
      ***  AND ZPPRENQ-VALIDFLAG        =  '1'                          <RC002>
               MOVE ZPPRENQ-RCPREPRTCD TO SR289-RCPREPRTCD              <RC002>
               MOVE ZPPRENQ-RCPREPRNT  TO SR289-RCPREPRNT               <RC002>
           END-IF.                                                      <RC002>
       A690-EXIT.                                                       <RC002>
            EXIT.                                                       <RC002>
      /                                                                 <RC002>
       1210-READ-ADOC SECTION.
      ***************************
       1210-BEGIN.
           MOVE SPACES                 TO WSAA-RTRN-EXIST.              <V76F12>
           PERFORM 1211-READ-ADOC.
           IF ADOC-STATUZ               = ENDP                          <V76F12>
              PERFORM 1211-READ-RCPT                                    <V76F12>
              MOVE RCPT-RDOCNUM        TO SR289-RECEIPT                 <V76F12>
              MOVE RCPT-EFFDATE        TO SR289-TRANDATEX               <V76F12>
              MOVE RCPT-BANKCODE       TO SR289-BANKCODE                <V76F12>
              MOVE WSSP-BANKDESC       TO SR289-SHORTDESC               <V76F12>
              MOVE RCPT-SACSCODE       TO SR289-RFCODE                  <V76F12>
              MOVE RCPT-RLDGACCT       TO SR289-RFNUM                   <V76F12>
              GO TO 1210-NAME-FORMAT                                    <V76F12>
           ELSE                                                         <V76F12>
              MOVE 'Y'                 TO WSAA-RTRN-EXIST               <V76F12>
           END-IF                                                       <V76F12>

           IF SBM-CANCELLATION    OR
              SBM-PARTIAL-CANCEL
              MOVE WSAA-NEW-RECEIPT    TO SR289-RECEIPT
           ELSE
              MOVE ADOC-RTRN-RDOCNUM   TO SR289-RECEIPT
           END-IF.

           MOVE ADOC-RTRN-EFFDATE      TO SR289-TRANDATEX.

           MOVE ADOC-RTRN-BANKCODE     TO SR289-BANKCODE.
           MOVE WSSP-BANKDESC          TO SR289-SHORTDESC.

           MOVE ADOC-RTRN-SACSCODE     TO SR289-RFCODE.
           MOVE ADOC-RTRN-RLDGACCT     TO SR289-RFNUM.

       1210-NAME-FORMAT.                                                <V76F12>
      *                                                                 <PHLRMS>
      *--  Show information of temp receipt                             <PHLRMS>
      *                                                                 <PHLRMS>
           PERFORM 3200-READ-RCOLINF.                                   <PHLRMS>
                                                                        <PHLRMS>
           IF  WSAA-FOUND-TEMP-RCPT    = 'Y'                            <PHLRMS>
               MOVE RCOLINF-RCTYPE     TO SR289-RCPREPRTCD              <PHLRMS>
               MOVE RCOLINF-RCSERNUM   TO SR289-RCPREPRNT               <PHLRMS>
               MOVE RCOLINF-DENTTYP    TO SR289-DENTTYP                 <PHLRMS>
               MOVE RCOLINF-ENTYNO     TO SR289-ENTYNO                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           PERFORM A2000-FORMAT-NAME.
           MOVE WSAA-CASHNM            TO SR289-CASHNM.

           IF SBM-CANCELLATION   OR
              SBM-PARTIAL-CANCEL
              MOVE 'Y'                 TO SR289-RFCODE-OUT    (PR)
                                          SR289-RFNUM-OUT     (PR)
                                          SR289-TRANDATEX-OUT (PR)
                                          SR289-MYFLG-OUT     (PR)      <V64F13>
           END-IF.
      *
           MOVE SR289-RFCODE           TO WSSP-PTCODE.
           MOVE SR289-RFNUM            TO WSSP-PAYESEL.
           MOVE SR289-CASHNM           TO WSSP-PAYENME.
       1210-EXIT.
            EXIT.
      /
       1211-READ-ADOC SECTION.
      ************************
       1211-BEGIN.
           INITIALIZE                     ADOC-DATA-KEY.
           MOVE PRFX-CASH              TO ADOC-RTRN-RDOCPFX.
           MOVE WSSP-COMPANY           TO ADOC-RTRN-RDOCCOY.
           MOVE WSAA-RECEIPT           TO ADOC-RTRN-RDOCNUM.
           MOVE ZEROES                 TO ADOC-RTRN-TRANNO.
           MOVE ZEROES                 TO ADOC-RTRN-JRNSEQ.
           PERFORM B1010-BEGN-ADOC.

           IF ADOC-STATUZ               = ENDP          OR
              ADOC-RTRN-RDOCPFX     NOT = PRFX-CASH     OR
              ADOC-RTRN-RDOCCOY     NOT = WSSP-COMPANY  OR
              ADOC-RTRN-RDOCNUM     NOT = WSAA-RECEIPT  OR
              ADOC-RTRN-TRANSEQ     NOT = ZEROES
              MOVE ENDP                TO ADOC-STATUZ                   <V76F12>
      *       MOVE ADOC-PARAMS         TO SYSR-PARAMS                   <V76F12>
      *       MOVE ADOC-STATUZ         TO SYSR-STATUZ                   <V76F12>
      *       PERFORM 600-FATAL-ERROR                                   <V76F12>
           END-IF.

       1211-EXIT.
            EXIT.
      /                                                                 <V76F12>
       1211-READ-RCPT SECTION.                                          <V76F12>
      ************************                                          <V76F12>
       1211-BEGIN-RCPT.                                                 <V76F12>
           INITIALIZE                     RCPT-DATA-KEY.                <V76F12>
           MOVE PRFX-CASH              TO RCPT-RDOCPFX.                 <V76F12>
           MOVE WSSP-COMPANY           TO RCPT-RDOCCOY.                 <V76F12>
           MOVE WSAA-RECEIPT           TO RCPT-RDOCNUM.                 <V76F12>
           MOVE ZEROES                 TO RCPT-SEQNBR.                  <V76F12>
           MOVE RCPTREC                TO RCPT-FORMAT.                  <V76F12>
           MOVE READR                  TO RCPT-FUNCTION.                <V76F12>
                                                                        <V76F12>
           CALL 'RCPTIO'            USING RCPT-PARAMS.                  <V76F12>
           IF  RCPT-STATUZ          NOT = O-K                           <V76F12>
               MOVE WSAA-DOCTKEY       TO SYSR-PARAMS                   <V76F12>
               MOVE RCPT-STATUZ        TO SYSR-STATUZ                   <V76F12>
               PERFORM 600-FATAL-ERROR                                  <V76F12>
           END-IF.                                                      <V76F12>
       1211-EXIT-RCPT.                                                  <V76F12>
            EXIT.                                                       <V76F12>
      /
       1220-GET-NEW-RECEIPT-NUMBER SECTION.
      *************************************
       1220-BEGIN.

           IF  WSSP-CHEQ-PRESFLAG       = 'Y'                           <V76F12>
           AND (WSAA-SBMACTION          = 'J' OR 'I')                   <V76F12>
               GO TO 1220-EXIT                                          <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
           PERFORM A1000-CALL-ALOCNO.

           IF ALNO-STATUZ               = O-K
              MOVE ALNO-ALOC-NO        TO WSAA-NEW-RECEIPT
           ELSE
              MOVE SPACES              TO SR289-RECEIPT
              MOVE ALNO-STATUZ         TO SR289-RECEIPT-ERR
              MOVE 'Y'                 TO WSAA-ALNO-STATUS              <V64F13>
              GO TO 1220-EXIT
           END-IF.

           PERFORM 1221-CHECK-EXISTENCE-RECEIPT.

       1220-EXIT.
            EXIT.
      /
       1221-CHECK-EXISTENCE-RECEIPT SECTION.
      **************************************
       1221-BEGIN.
      *
      * Check that New receipt number does not exist in RTRN
      *
           IF SBM-CREATION      OR
              SBM-CANCELLATION

              INITIALIZE                  RDOC-DATA-KEY
              MOVE PRFX-CASH           TO RDOC-RDOCPFX
              MOVE WSSP-COMPANY        TO RDOC-RDOCCOY
              MOVE WSAA-NEW-RECEIPT    TO RDOC-RDOCNUM
              MOVE '0000'              TO RDOC-TRANSEQ
              PERFORM B1020-READR-RDOC
              IF RDOC-STATUZ            = O-K
                 MOVE RDOC-PARAMS      TO SYSR-PARAMS
                 MOVE E071             TO SYSR-DBIO-STATUZ
                                          SYSR-STATUZ
                 PERFORM 600-FATAL-ERROR
              END-IF
           END-IF.
       1221-EXIT.
            EXIT.
      /
       1230-LOAD-NEW-HEADER SECTION.
      ******************************
       1230-BEGIN.
           MOVE WSAA-NEW-RECEIPT       TO SR289-RECEIPT.
      *    MOVE WSAA-TODAY             TO SR289-TRANDATEX
      *                                   WSAA-TRANDATEX.

           MOVE WSSP-BANKCODE          TO SR289-BANKCODE.
           MOVE WSSP-BANKDESC          TO SR289-SHORTDESC.

           MOVE CSRF-CN                TO SR289-RFCODE.
           MOVE SPACES                 TO SR289-RFNUM.
           MOVE SPACES                 TO SR289-CASHNM.
                                                                        <V76F07>
           PERFORM 1600-DEFAULT-PAYOR.                                  <V76F07>
           IF SR289-RFNUM              = SPACES                         <V76F07>
              GO TO 1230-EXIT.                                          <V76F07>
                                                                        <V76F07>
           EVALUATE SR289-RFCODE                                        <V76F07>
              WHEN CSRF-CN PERFORM 2300-VALIDATE-CLIENT                 <V76F07>
              WHEN CSRF-AG PERFORM 2400-VALIDATE-AGENT                  <V76F07>
           END-EVALUATE.                                                <V76F07>
                                                                        <V76F07>
           IF SR289-RFCODE-ERR         = SPACES AND                     <V76F07>
              SR289-RFNUM-ERR          = SPACES                         <V76F07>
              PERFORM A2000-FORMAT-NAME                                 <V76F07>
              MOVE WSAA-CASHNM         TO SR289-CASHNM                  <V76F07>
           END-IF.                                                      <V76F07>

       1230-EXIT.
            EXIT.
      /
       1300-LOAD-SUBFILE SECTION.
      ***************************
       1300-BEGIN.
           MOVE ZEROES                 TO WSAA-TOT-DOCORIGAMT
                                          WSAA-TOT-DOCACCTAMT.
           MOVE SPACES                 TO WSAA-VALID-STDESCSH.          <V76F12>

           INITIALIZE                     RBNK-DATA-AREA.
           MOVE O-K                    TO RBNK-STATUZ.
           MOVE PRFX-CASH              TO RBNK-RDOCPFX.
           MOVE WSSP-COMPANY           TO RBNK-RDOCCOY.
           MOVE WSAA-RECEIPT           TO RBNK-RDOCNUM.
           MOVE BEGN                   TO RBNK-FUNCTION.
           MOVE RBNKREC                TO RBNK-FORMAT.

      *    MOVE 1                      TO WSAA-LINE-CNT.                <V76F12>
           MOVE ZEROES                 TO WSAA-LINE-CNT                 <V76F12>
                                          WSAA-SEQNBR.                  <V76F12>

           PERFORM 1310-LOAD-SUBFILE
                      UNTIL RBNK-STATUZ   = ENDP
      *                 AND WSAA-LINE-CNT > SR289-SUBFILE-PAGE.         <V76F12>

           DIVIDE SCRN-SUBFILE-RRN     BY SR289-SUBFILE-PAGE            <V76F12>
                                       GIVING WSAA-NUMBER               <V76F12>
                                       REMAINDER WSAA-LINE-CNT.         <V76F12>
           PERFORM 1310-LOAD-SUBFILE                                    <V76F12>
                      UNTIL WSAA-LINE-CNT >= SR289-SUBFILE-PAGE.        <V76F12>
                                                                        <V76F12>
           MOVE 1                      TO SCRN-SUBFILE-RRN.

           IF SBM-CANCELLATION   OR
              SBM-PARTIAL-CANCEL OR
              SBM-MODIFY         OR                                     <V76F12>
              PRINTED            OR                                     <V76F12>
              SBM-INQUIRY
              MOVE 'N'                 TO SCRN-SUBFILE-MORE
           ELSE
              MOVE 'Y'                 TO SCRN-SUBFILE-MORE
           END-IF.
                                                                        <V76F12>
      *--If atleast one record in RBNK is Held(HU/HS) or Cancel(CU/CS), <V76F12>
      *--If Receipt Status is Held(HU/HS) or Cancel(CU/CS), set to      <V76F12>
      *--display the literals for SR289-STDESCSH-OUT(PR).               <V76F12>
           IF VALID-STDESCSH                                            <V76F12>
           OR SBM-CANCELLATION                                          <V76F12>
           OR SBM-PARTIAL-CANCEL                                        <V76F12>
              MOVE 'N'                 TO SR289-STDESCSH-OUT(ND)        <V76F12>
           ELSE                                                         <V76F12>
              MOVE 'Y'                 TO SR289-STDESCSH-OUT(ND)        <V76F12>
           END-IF.                                                      <V76F12>

       1300-EXIT.
            EXIT.
      /
       1310-LOAD-SUBFILE SECTION.
      ***************************
       1310-BEGIN.

           CALL 'RBNKIO'            USING RBNK-PARAMS.
           IF RBNK-STATUZ           NOT = O-K AND ENDP
              MOVE RBNK-STATUZ         TO SYSR-STATUZ
              MOVE RBNK-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

           IF RBNK-STATUZ               = ENDP
           OR RBNK-RDOCPFX          NOT = PRFX-CASH
           OR RBNK-RDOCCOY          NOT = WSSP-COMPANY
           OR RBNK-RDOCNUM          NOT = WSAA-RECEIPT
              MOVE ENDP                TO RBNK-STATUZ
           END-IF.

      *Set subfile screen fields
           ADD 1                       TO WSAA-SEQNBR                   <V76F12>
                                          WSAA-LINE-CNT.                <V76F12>
           IF RBNK-STATUZ              = ENDP
           OR WSAA-RECEIPT             = SPACES                         <CSC008>
              PERFORM 1311-LOAD-NEW-SUBFILE
           ELSE
              PERFORM 1312-LOAD-OLD-SUBFILE
           END-IF.

      *    ADD 1                       TO WSAA-LINE-CNT.                <V76F12>
      *                                                                 <V76F12>
      *    IF  RBNK-STATUZ              = O-K                           <V76F12>
      *    AND WSAA-LINE-CNT            > SR289-SUBFILE-PAGE            <V76F12>
      *       MOVE 1                   TO WSAA-LINE-CNT                 <V76F12>
      *    END-IF.                                                      <V76F12>

       1310-READ-NEXT.
           MOVE NEXTR                  TO RBNK-FUNCTION.
       1310-EXIT.
           EXIT.
      /
       1311-LOAD-NEW-SUBFILE SECTION.
      ******************************
       1311-BEGIN.
      * Do not load blank subfile lines if ....
      * - called from remote device i.e. if called from 3r.
      * - cancellation
      * - partial cancellation
      * - inquiry
           IF SCRN-DEVICE-IND          = '*RMT' OR
              SBM-CANCELLATION   OR
              SBM-PARTIAL-CANCEL OR
              (NOT Y-POSTDATED AND SBM-MODIFY) OR                       <V76F12>
              PRINTED OR                                                <V76F12>
              SBM-INQUIRY
              GO TO 1311-EXIT
           END-IF.

           MOVE 'Y'                    TO SR289-SELECT-OUT   (PR).
      *    MOVE SPACES                 TO SR289-PAYTYPE.                <V76F07>
           MOVE ZEROES                 TO SR289-DOCORIGAMT.
           IF WSAA-DOCORIGAMT          NOT = ZEROES                     <PS008>
              MOVE WSAA-DOCORIGAMT     TO SR289-DOCORIGAMT              <PS008>
              MOVE ZEROES              TO WSAA-DOCORIGAMT               <PS008>
           END-IF.                                                      <PS008>
           MOVE WSSP-ORIGCCY           TO SR289-ORIGCURR.
           MOVE 'Y'                    TO SR289-ORIGCURR-OUT (PR).
           MOVE ZEROES                 TO SR289-SCRATE.
           MOVE ZEROES                 TO SR289-DOCACCTAMT.
           MOVE T3629-LEDGCURR         TO SR289-ACCTCCY.
           MOVE SPACES                 TO SR289-CHEQNO.
      *    MOVE SPACES                 TO SR289-ZCHQTYP.                <V76F07>
           MOVE VRCM-MAX-DATE          TO SR289-TCHQDATE.
           MOVE SPACES                 TO SR289-BANKKEY.
           MOVE SPACES                 TO SR289-BANKDESC-01
                                          SR289-BANKDESC-02
                                          SR289-BANKDESC-03.
           MOVE SPACES                 TO SR289-CRCARDMOD               <V76F12>
                                          SR289-RCPTSTAT                <V76F12>
                                          SR289-INSREFNO                <V76F12>
                                          SR289-CNRSNCD                 <V76F12>
                                          SR289-CNRSNDESC.              <V76F12>
           MOVE VRCM-MAX-DATE          TO SR289-DEPDATE.                <V76F12>
           MOVE SR289-DEPDATE          TO WSSP-CURRFROM.                <RC002>
           MOVE WSSP-LANGUAGE          TO SR289-LANGUAGE.               <V76F12>
           MOVE WSAA-SEQNBR            TO SR289-SEQNBR.                 <V76F12>
           MOVE VRCM-MAX-DATE          TO SR289-AUTHDAT.                <V76F13>
           MOVE ZEROES                 TO SR289-CRCARDEXPM              <V76F13>
                                          SR289-CRCARDEXPY.             <V76F13>
           MOVE SPACES                 TO SR289-LXOPTIND                <V76F13>
                                          SR289-CCMID                   <V76F13>
                                          SR289-MCHNTID                 <V76F13>
                                          SR289-CCTID                   <V76F13>
                                          SR289-TRMNLID                 <V76F13>
                                          SR289-CRDTCARD                <V76F13>
                                          SR289-AUTHID                  <V76F13>
                                          SR289-CRCARDTYPE              <V76F13>
                                          SR289-INDIC                   <V76F13>
                                          SR289-POSTFLG                 <V76F12>
                                          SR289-CHGFLAG                 <V76F13>
                                          SR289-CRCNAME.                <V76F13>
                                                                        <V76F12>
      *    IF WSAA-LINE-CNT             = 1  AND                <V76F12><V76F07>
           IF WSAA-SEQNBR               = 1  AND                        <V76F12>
              SCRN-STATUZ               = O-K                           <V76F07>
      * Pay Type.                                                       <V76F07>
              MOVE SPACES              TO CHKF-FIELD-ID                 <V76F07>
              MOVE 'SR289-PAYTYPE       ' TO CHKF-FIELD-ID              <V76F07>
              CALL 'CHKFSUF'        USING CHKF-CHKF-REC                 <V76F07>
                                          SR289-PAYTYPE                 <V76F07>
                                          SR289-PAYTYPE-ERR             <V76F07>
                                          SR289-PAYTYPE-OUT(PR)         <V76F07>
                                                                        <V76F07>
      * Cheque Type.                                                    <V76F07>
              MOVE SPACES              TO CHKF-FIELD-ID                 <V76F07>
              MOVE 'SR289-ZCHQTYP       ' TO CHKF-FIELD-ID              <V76F07>
              CALL 'CHKFSUF'        USING CHKF-CHKF-REC                 <V76F07>
                                          SR289-ZCHQTYP                 <V76F07>
                                          SR289-ZCHQTYP-ERR             <V76F07>
                                          SR289-ZCHQTYP-OUT(PR)         <V76F07>
           ELSE                                                         <V76F07>
              MOVE SPACES              TO SR289-PAYTYPE                 <V76F07>
                                          SR289-ZCHQTYP                 <V76F07>
           END-IF.                                                      <V76F07>
                                                                        <V76F12>
           IF RCTSTATFLG                                                <V76F12>
              MOVE 'N'                 TO SR289-RCPTSTAT-OUT(ND)        <V76F12>
           ELSE                                                         <V76F12>
              MOVE 'Y'                 TO SR289-RCPTSTAT-OUT(ND)        <V76F12>
           END-IF.                                                      <V76F12>
           MOVE 'Y'                    TO SR289-CNRSNCD-OUT(ND)         <V76F12>
                                          SR289-CNRSNCD-OUT(PR)         <V76F12>
           IF WSAA-SBMACTION        NOT = 'B' AND 'C' AND 'K' AND 'H'   <RC002>
               MOVE 'Y'                TO SR289-INSREFNO-OUT(ND)        <RC002>
           ELSE                                                         <RC002>
               MOVE 'N'                TO SR289-INSREFNO-OUT(ND)        <RC002>
           END-IF.                                                      <RC002>
      *                                                                 <GAPPH1>
      * Set Default Payment Type:                                       <GAPPH1>
      *                                                                 <GAPPH1>
           IF  SBM-CREATION                                             <GAPPH1>
           AND WSAA-SEQNBR             = 1                              <GAPPH1>
           AND SR289-PAYTYPE           = SPACES                         <GAPPH1>
               MOVE TV007-PAYTYPE      TO SR289-PAYTYPE                 <GAPPH1>
           END-IF.                                                      <GAPPH1>

      *Write subfile
           MOVE SADD                   TO SCRN-FUNCTION.
           PERFORM 9000-SCREEN-IO.
       1311-EXIT.
            EXIT.
      /
       1312-LOAD-OLD-SUBFILE SECTION.
      ******************************
       1312-BEGIN.
           MOVE RBNK-PAYTYPE           TO SR289-PAYTYPE.
           MOVE RBNK-DOCORIGAMT        TO SR289-DOCORIGAMT.
           MOVE RBNK-ORIGCCY           TO SR289-ORIGCURR.
           MOVE RBNK-SCRATE            TO SR289-SCRATE.
           MOVE RBNK-DOCACCTAMT        TO SR289-DOCACCTAMT.
           MOVE RBNK-ACCTCCY           TO SR289-ACCTCCY.
           MOVE RBNK-CHQNUM            TO SR289-CHEQNO.
           MOVE RBNK-ZCHQTYP           TO SR289-ZCHQTYP.
           MOVE RBNK-TCHQDATE          TO SR289-TCHQDATE.
           MOVE RBNK-BANKKEY           TO SR289-BANKKEY.
           MOVE RBNK-BANKDESC01        TO SR289-BANKDESC-01.
           MOVE RBNK-BANKDESC02        TO SR289-BANKDESC-02.
           MOVE RBNK-BANKDESC03        TO SR289-BANKDESC-03.
                                                                        <V76F12>
           MOVE RBNK-DEPDATE           TO SR289-DEPDATE.                <V76F12>
           MOVE SR289-DEPDATE          TO WSSP-CURRFROM.                <RC002>
           MOVE RBNK-CRCARDMOD         TO SR289-CRCARDMOD               <V76F13>
                                          SR289-CHGFLAG.                <V76F13>
           MOVE RBNK-RCPTSTAT          TO SR289-RCPTSTAT.               <V76F12>
           MOVE RBNK-INSREFNO          TO SR289-INSREFNO.               <V76F12>
           MOVE RBNK-SEQNBR            TO SR289-SEQNBR.                 <V76F12>
                                                                        <V76F12>
           MOVE RBNK-CCMID             TO SR289-CCMID.                  <V76F13>
           MOVE RBNK-MCHNTID           TO SR289-MCHNTID.                <V76F13>
           MOVE RBNK-CCTID             TO SR289-CCTID.                  <V76F13>
           MOVE RBNK-TRMNLID           TO SR289-TRMNLID.                <V76F13>
           MOVE RBNK-CRDTCARD          TO SR289-CRDTCARD.               <V76F13>
           MOVE RBNK-AUTHID            TO SR289-AUTHID.                 <V76F13>
           MOVE RBNK-CRCARDTYPE        TO SR289-CRCARDTYPE.             <V76F13>
           MOVE RBNK-CRCNAME           TO SR289-CRCNAME.                <V76F13>
           MOVE RBNK-CRCARDEXPM        TO SR289-CRCARDEXPM.             <V76F13>
           MOVE RBNK-CRCARDEXPY        TO SR289-CRCARDEXPY.             <V76F13>
           MOVE RBNK-AUTHDAT           TO SR289-AUTHDAT.                <V76F13>
                                                                        <V76F13>
      *--Check if receipt is postdated only if Receipt Status           <V76F12>
      *--enhancement is applicable.                                     <V76F12>
           IF  RCTSTATFLG                                               <V76F12>
           AND RBNK-POSTDTEFLG      NOT = SPACES                        <V76F12>
           AND WSAA-POSTDTEFLG          = SPACES                        <V76F12>
              MOVE RBNK-POSTDTEFLG     TO WSSP-CHEQ-PRESFLAG            <V76F12>
                                          WSAA-POSTDTEFLG               <V76F12>
                                          SR289-POSTDTEFLG              <V76F12>
                                          WSAA-RBNK-POSTDTEFLG          <V76F12>
              IF Y-POSTDATED                                            <V76F12>
                 PERFORM 1211-READ-RCPT                                 <V76F12>
                 IF RCPT-PRTFLG     NOT = SPACES                        <V76F12>
                    MOVE 'Y'           TO WSAA-PRINTED                  <V76F12>
                 END-IF                                                 <V76F12>
              END-IF                                                    <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
      *--If Receipt Status is Held(HU/HS) or Cancel(CU/CS), set         <V76F12>
      *--indicator (VALID-STDESCSH) to display the literals for         <V76F12>
      *--SR289-STDESCSH-OUT(PR).  And also display the reason           <V76F12>
      *--code description.                                              <V76F12>
           IF SR289-RCPTSTAT            = RCTST-CS OR                   <V76F12>
                                          RCTST-CU                      <V76F12>
              MOVE RBNK-CNRSNCD        TO SR289-CNRSNCD                 <V76F12>
              PERFORM C3000-GET-CNRSNDESC                               <V76F12>
              MOVE 'Y'                 TO WSAA-VALID-STDESCSH           <V76F12>
           ELSE IF SR289-RCPTSTAT       = RCTST-HS OR                   <V76F12>
                                          RCTST-HU                      <V76F12>
              MOVE RBNK-HLRSNCD        TO SR289-CNRSNCD                 <V76F12>
              PERFORM C3000-GET-CNRSNDESC                               <V76F12>
              MOVE 'Y'                 TO WSAA-VALID-STDESCSH           <V76F12>
           ELSE                                                         <V76F12>
              MOVE SPACES              TO SR289-CNRSNCD                 <V76F12>
                                          SR289-CNRSNDESC               <V76F12>
           END-IF.                                                      <V76F12>
           MOVE WSSP-LANGUAGE          TO SR289-LANGUAGE.               <V76F12>

      * For receipt cancellation,
      * ..reverse the amount fields
      * ..protect fields
           MOVE 'Y'                    TO SR289-SELECT-OUT (PR).
           IF SBM-CANCELLATION       OR
              SBM-PARTIAL-CANCEL

              MOVE CSTP-CHEQ-CANCEL-VAL TO SR289-PAYTYPE
             MOVE SPACES               TO SR289-CNRSNCD                 <V76F12>
                                          SR289-CNRSNDESC               <V76F12>
             IF  NOT Y-POSTDATED                                        <V76F12>
              COMPUTE SR289-DOCORIGAMT   = SR289-DOCORIGAMT * -1
              COMPUTE SR289-DOCACCTAMT   = SR289-DOCACCTAMT * -1
             END-IF                                                     <V76F12>

              PERFORM 1313-PROTECT-FIELDS
           END-IF.
      *
           ADD SR289-DOCORIGAMT        TO WSAA-TOT-DOCORIGAMT.
           ADD SR289-DOCACCTAMT        TO WSAA-TOT-DOCACCTAMT.
      *
           IF WSAA-LINE-CNT             = 1
              MOVE SR289-PAYTYPE       TO WSAA-DOCTYPE
              MOVE SR289-CHEQNO        TO WSAA-CHEQNO
              MOVE SR289-ORIGCURR      TO WSAA-ORIGCCY
              MOVE SR289-SCRATE        TO WSAA-SCRATE
              MOVE SR289-ACCTCCY       TO WSAA-ACCTCCY
           END-IF.
           IF SBM-INQUIRY                                               <V76F04>
              PERFORM 1314-PROTECT-FIELDS                               <V76F04>
           END-IF.                                                      <V76F04>
           IF SBM-MODIFY                                                <V76F12>
              PERFORM 1315-PROTECT-FIELDS                               <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <CS016>
TUYET      IF WSAA-SBMACTION           = 'M'                            <CS016>
              MOVE 'Y'                 TO SR289-CHEQNO-OUT (PR)         <CS016>
                                          SR289-ZCHQTYP-OUT (PR)        <CS016>
                                          SR289-TCHQDATE-OUT (PR)       <CS016>
                                          SR289-BANKDESC01-OUT (PR)     <CS016>
                                          SR289-BANKDESC02-OUT (PR)     <CS016>
                                          SR289-BANKDESC03-OUT (PR)     <CS016>
                                          SR289-INSREFNO-OUT (PR)       <CS016>
           END-IF.                                                      <CS016>

           IF RCTSTATFLG                                                <V76F12>
              MOVE 'N'                 TO SR289-RCPTSTAT-OUT(ND)        <V76F12>
              IF SR289-CNRSNCD      NOT = SPACES                        <V76F12>
              OR SR289-RCPTSTAT         = RCTST-CS OR                   <V76F12>
                                          RCTST-CU OR                   <V76F12>
                                          RCTST-HS OR                   <V76F12>
                                          RCTST-HU                      <V76F12>
                 MOVE 'N'              TO SR289-CNRSNCD-OUT(ND)         <V76F12>
              ELSE                                                      <V76F12>
                 MOVE 'Y'              TO SR289-CNRSNCD-OUT(ND)         <V76F12>
                                          SR289-CNRSNCD-OUT(PR)         <V76F12>
              END-IF                                                    <V76F12>
              IF SBM-CANCELLATION                                       <V76F12>
              OR SBM-PARTIAL-CANCEL                                     <V76F12>
                 MOVE 'N'              TO SR289-CNRSNCD-OUT(ND)         <V76F12>
                                          SR289-CNRSNCD-OUT(PR)         <V76F12>
              END-IF                                                    <V76F12>
           ELSE                                                         <V76F12>
              MOVE 'Y'                 TO SR289-RCPTSTAT-OUT(ND)        <V76F12>
                                          SR289-CNRSNCD-OUT(ND)         <V76F12>
                                          SR289-CNRSNCD-OUT(PR)         <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
           PERFORM A300-SUBFILE-INIT-OPTSWCH                            <V76F13>
                                                                        <V76F13>
      *Write subfile
           MOVE SADD                   TO SCRN-FUNCTION.
           PERFORM 9000-SCREEN-IO.
       1312-EXIT.
            EXIT.
      /
       1313-PROTECT-FIELDS SECTION.
      *****************************
       1313-BEGIN.
      * For partial cancellation , if the RBNK is reversed before
      * , protect the "Select" field.
           IF SBM-PARTIAL-CANCEL
              IF RBNK-RCPTREV           = SPACES
                 MOVE 'N'              TO SR289-SELECT-OUT (PR)
              END-IF
           END-IF
      *
           MOVE 'Y'                    TO SR289-PAYTYPE-OUT    (PR)
                                          SR289-MYFLG-OUT      (PR)     <V64F13>
                                          SR289-DEPDATE-OUT    (PR)     <V76F12>
                                          SR289-DOCORIGAMT-OUT (PR)
                                          SR289-ORIGCURR-OUT   (PR)
                                          SR289-SCRATE-OUT     (PR)
                                          SR289-DOCACCTAMT-OUT (PR)
                                          SR289-ACCTCCY-OUT    (PR)
                                          SR289-CHEQNO-OUT     (PR)
                                          SR289-ZCHQTYP-OUT    (PR)
                                          SR289-TCHQDATE-OUT   (PR)
                                          SR289-BANKKEY-OUT    (PR)
                                          SR289-BANKDESC01-OUT (PR)
                                          SR289-BANKDESC02-OUT (PR)
                                          SR289-BANKDESC03-OUT (PR).
       1313-EXIT.
            EXIT.
      /                                                                 <V76F04>
       1314-PROTECT-FIELDS SECTION.                                     <V76F04>
      *****************************                                     <V76F04>
       1314-BEGIN.                                                      <V76F04>
      *                                                                 <V76F04>
           MOVE 'Y'                    TO SR289-PAYTYPE-OUT    (PR)     <V76F04>
                                          SR289-MYFLG-OUT      (PR)     <V76F04>
                                          SR289-DOCORIGAMT-OUT (PR)     <V76F04>
                                          SR289-ORIGCURR-OUT   (PR)     <V76F04>
                                          SR289-SCRATE-OUT     (PR)     <V76F04>
                                          SR289-DOCACCTAMT-OUT (PR)     <V76F04>
                                          SR289-ACCTCCY-OUT    (PR)     <V76F04>
                                          SR289-CHEQNO-OUT     (PR)     <V76F04>
                                          SR289-ZCHQTYP-OUT    (PR)     <V76F04>
                                          SR289-TCHQDATE-OUT   (PR)     <V76F04>
                                          SR289-BANKKEY-OUT    (PR)     <V76F04>
                                          SR289-BANKDESC01-OUT (PR)     <V76F04>
                                          SR289-BANKDESC02-OUT (PR)     <V76F04>
                                          SR289-BANKDESC03-OUT (PR)     <V76F04>
                                          SR289-CNRSNCD-OUT    (PR)     <V76F12>
                                          SR289-CRCARDMOD-OUT  (PR)     <V76F12>
                                          SR289-DEPDATE-OUT    (PR)     <V76F12>
                                          SR289-SELECT-OUT     (PR).    <V76F04>
       1314-EXIT.                                                       <V76F04>
            EXIT.                                                       <V76F04>
      /                                                                 <V76F12>
       1315-PROTECT-FIELDS SECTION.                                     <V76F12>
      *****************************                                     <V76F12>
       1315-BEGIN.                                                      <V76F12>
           IF Y-POSTDATED                                               <V76F12>
           AND NOT PRINTED                                              <V76F12>
               GO TO 1515-EXIT.                                         <V76F12>
                                                                        <V76F12>
           IF Y-POSTDATED                                               <V76F12>
              GO TO 1315-PROTECT.                                       <V76F12>
                                                                        <V76F12>
           MOVE SPACES                 TO WSAA-VALID-STATUS.            <V76F12>
                                                                        <V76F12>
           PERFORM VARYING IX FROM 1 BY 1                               <V76F12>
              UNTIL   IX > WSAA-MAX-RCTSTAT OR VALID-STATUS             <V76F12>
              IF SR289-RCPTSTAT         = TR29S-RCPTSTAT(IX)            <V76F12>
              AND TR29S-RCPTSTAT(IX) NOT = SPACES                       <V76F12>
                 MOVE 'Y'           TO WSAA-VALID-STATUS                <V76F12>
              END-IF                                                    <V76F12>
           END-PERFORM.                                                 <V76F12>
           IF NOT VALID-STATUS                                          <V76F12>
           OR SR289-RCPTSTAT            = RCTST-HS                      <V76F12>
           OR SR289-RCPTSTAT            = RCTST-HU                      <V76F12>
           OR SR289-RCPTSTAT            = RCTST-CS                      <V76F12>
           OR SR289-RCPTSTAT            = RCTST-CU                      <V76F12>
              PERFORM 1314-PROTECT-FIELDS                               <V76F12>
              GO TO 1515-EXIT                                           <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
       1315-PROTECT.                                                    <V76F12>
           MOVE 'Y'                    TO SR289-PAYTYPE-OUT    (PR)     <V76F12>
                                          SR289-MYFLG-OUT      (PR)     <V76F12>
                                          SR289-DOCORIGAMT-OUT (PR)     <V76F12>
                                          SR289-ORIGCURR-OUT   (PR)     <V76F12>
                                          SR289-SCRATE-OUT     (PR)     <V76F12>
                                          SR289-DOCACCTAMT-OUT (PR)     <V76F12>
                                          SR289-ACCTCCY-OUT    (PR)     <V76F12>
                                          SR289-BANKKEY-OUT    (PR)     <V76F12>
                                          SR289-CNRSNCD-OUT    (PR)     <V76F12>
                                          SR289-CRCARDMOD-OUT  (PR)     <V76F12>
                                          SR289-DEPDATE-OUT    (PR)     <V76F12>
                                          SR289-SELECT-OUT     (PR).    <V76F12>
           MOVE 'N'                    TO SR289-CHEQNO-OUT     (PR)     <V76F12>
                                          SR289-ZCHQTYP-OUT    (PR)     <V76F12>
                                          SR289-TCHQDATE-OUT            <V76F12>
                                          SR289-BANKDESC01-OUT (PR)     <V76F12>
                                          SR289-BANKDESC02-OUT (PR)     <V76F12>
                                          SR289-BANKDESC03-OUT (PR).    <V76F12>
                                                                        <V76F12>
       1515-EXIT.                                                       <V76F12>
           EXIT.                                                        <V76F12>
      /
       1400-KEEPS-RBNK SECTION.
      *************************
       1400-BEGIN.
      *   Old receipt number is passed thru WSSP-DOCTKEY.
      *   while new receipt number is passed thru RBNK-RDOCNUM
           INITIALIZE                     RBNK-DATA-AREA.
           MOVE PRFX-CASH              TO RBNK-RDOCPFX.
           MOVE WSSP-COMPANY           TO RBNK-RDOCCOY.
           MOVE SR289-RECEIPT          TO RBNK-RDOCNUM.
           MOVE RBNKREC                TO RBNK-FORMAT.
           MOVE KEEPS                  TO RBNK-FUNCTION.
           CALL 'RBNKIO'            USING RBNK-PARAMS.

           IF  RBNK-STATUZ            NOT = O-K
               MOVE RBNK-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.
       1400-EXIT.
            EXIT.
      /                                                                 <PHLRMS>
       1500-READ-TV037 SECTION.                                         <PHLRMS>
      *************************                                         <PHLRMS>
      *                                                                 <PHLRMS>
       1510-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                     TV037-TV037-REC.              <PHLRMS>
           MOVE TV037                  TO WSAA-TABLE.                   <PHLRMS>
           MOVE WSKY-BATC-BATCTRCDE    TO WSAA-ITEM.                    <PHLRMS>
           PERFORM A2300-READ-TABLE.                                    <PHLRMS>
           MOVE ITEM-GENAREA           TO TV037-TV037-REC.              <PHLRMS>
      *                                                                 <PHLRMS>
       1590-EXIT.                                                       <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /                                                                 <V76F04>
       A100-KEEP-RMRK SECTION.                                          <V76F04>
      *************************                                         <V76F04>
       A101-BEGIN.                                                      <V76F04>
      *                                                                 <V76F04>
           MOVE SPACES                 TO WSAA-RMRKKEY.                 <V76F04>
           MOVE WSSP-RDOCCOY           TO WSAA-PR25C-RDOCCOY.           <V76F04>
           MOVE WSSP-RDOCPFX           TO WSAA-PR25C-RDOCPFX.           <V76F04>
           MOVE SR289-RECEIPT          TO WSAA-PR25C-RDOCNUM.           <V76F04>
           MOVE WSAA-RMRKKEY           TO WSSP-CLONEKEY.                <V76F04>
      *                                                                 <V76F04>
       A199-EXIT.                                                       <V76F04>
            EXIT.                                                       <V76F04>
      /                                                                 <V76F04>
       A200-INIT-OPTS SECTION.                                          <V76F04>
      *************************                                         <V76F04>
       A201-INIT.                                                       <V76F04>
      *---Keeps for Existence Check program                             <V76F13>
           MOVE WSSP-RDOCPFX           TO RBNKSEQ-RDOCPFX.              <V76F13>
           MOVE WSSP-RDOCCOY           TO RBNKSEQ-RDOCCOY.              <V76F13>
           MOVE WSSP-RDOCNUM           TO RBNKSEQ-RDOCNUM.              <V76F13>
           MOVE ZEROES                 TO RBNKSEQ-SEQNBR.               <V76F13>
           MOVE RBNKSEQREC             TO RBNKSEQ-FORMAT.               <V76F13>
                                                                        <V76F13>
           MOVE KEEPS                  TO RBNKSEQ-FUNCTION.             <V76F13>
           CALL 'RBNKSEQIO'         USING RBNKSEQ-PARAMS.               <V76F13>
           IF  RBNKSEQ-STATUZ       NOT = O-K                           <V76F13>
               MOVE RBNKSEQ-PARAMS     TO SYSR-PARAMS                   <V76F13>
               MOVE RBNKSEQ-STATUZ     TO SYSR-STATUZ                   <V76F13>
               PERFORM 600-FATAL-ERROR                                  <V76F13>
           END-IF.                                                      <V76F13>
      *                                                                 <V76F04>
      *  Retrieve the Switching Options.                                <V76F04>
      *                                                                 <V76F04>
           MOVE 'INIT'                 TO OPTS-FUNCTION.                <V76F04>
           MOVE WSAA-PROG              TO OPTS-CALLING-PROG.            <V76F04>
           MOVE WSAA-TODAY             TO OPTS-DTEEFF.                  <V76F04>
           MOVE WSSP-COMPANY           TO OPTS-COMPANY.                 <V76F04>
           MOVE WSSP-LANGUAGE          TO OPTS-LANGUAGE.                <V76F04>
           MOVE VRCM-USER              TO OPTS-USER.                    <V76F04>
           CALL 'OPTSWCH'           USING OPTSWCH-REC                   <V76F04>
                                          WSSP-SEC-PROGS                <V76F04>
                                          WSSP-SEC-ACTNS                <V76F04>
                                          WSSP-PROGRAM-PTR              <V76F04>
                                          WSSP-FLAG.                    <V76F04>
           IF   OPTS-STATUZ         NOT = O-K                           <V76F04>
                MOVE 'INIT'            TO SYSR-FUNCTION                 <V76F04>
                MOVE OPTS-STATUZ       TO SYSR-DBIO-STATUZ              <V76F04>
                                          SYSR-STATUZ                   <V76F04>
                MOVE 'OPTSWCH'         TO SYSR-IOMOD                    <V76F04>
                PERFORM 600-FATAL-ERROR.                                <V76F04>
                                                                        <V76F13>
           MOVE RLSE                   TO RBNKSEQ-FUNCTION.             <V76F13>
           CALL 'RBNKSEQIO'         USING RBNKSEQ-PARAMS.               <V76F13>
           IF  RBNKSEQ-STATUZ       NOT = O-K                           <V76F13>
               MOVE RBNKSEQ-PARAMS     TO SYSR-PARAMS                   <V76F13>
               MOVE RBNKSEQ-STATUZ     TO SYSR-STATUZ                   <V76F13>
               PERFORM 600-FATAL-ERROR                                  <V76F13>
           END-IF.                                                      <V76F13>
       A299-EXIT.                                                       <V76F04>
            EXIT.                                                       <V76F04>
      /                                                                 <V76F13>
       A300-SUBFILE-INIT-OPTSWCH SECTION.                               <V76F13>
      ***********************************                               <V76F13>
       A301-INIT.                                                       <V76F13>
           IF SR289-PAYTYPE         NOT = CSTP-CREDIT-CARD-VAL          <V76F13>
              GO TO A398-INIT-OPT.                                      <V76F13>
                                                                        <V76F13>
           PERFORM A400-KEEPS-CCARD-DETAILS.                            <V76F13>
                                                                        <V76F13>
      *---Call Optswch Init Function                                    <V76F13>
           MOVE 'INIT'                 TO OPTS-FUNCTION.                <V76F13>
           MOVE WSAA-PROG              TO OPTS-CALLING-PROG.            <V76F13>
           MOVE ZEROS                  TO OPTS-DTEEFF.                  <V76F13>
           MOVE WSSP-COMPANY           TO OPTS-COMPANY.                 <V76F13>
           MOVE WSSP-LANGUAGE          TO OPTS-LANGUAGE.                <V76F13>
           MOVE WSSP-TRANID            TO VRCM-TRANID.                  <V76F13>
           MOVE VRCM-USER              TO OPTS-USER.                    <V76F13>
                                                                        <V76F13>
           CALL 'OPTSWCH'           USING OPTSWCH-REC                   <V76F13>
                                          WSSP-SEC-PROGS                <V76F13>
                                          WSSP-SEC-ACTNS                <V76F13>
                                          WSSP-PROGRAM-PTR              <V76F13>
                                          WSSP-FLAG.                    <V76F13>
           IF   OPTS-STATUZ         NOT = O-K                           <V76F13>
                MOVE 'INIT'            TO SYSR-FUNCTION                 <V76F13>
                MOVE OPTS-STATUZ       TO SYSR-DBIO-STATUZ              <V76F13>
                                          SYSR-STATUZ                   <V76F13>
                MOVE 'OPTSWCH'         TO SYSR-IOMOD                    <V76F13>
                PERFORM 600-FATAL-ERROR                                 <V76F13>
           END-IF.                                                      <V76F13>
                                                                        <V76F13>
           MOVE WSAA-C6                TO OPTS-SEL-CODE.                <V76F13>
           MOVE SPACES                 TO WSAA-FOUND-SELECTION.         <V76F13>
           PERFORM VARYING IX        FROM 1 BY 1                        <V76F13>
                                    UNTIL IX > WSAA-MAX-OPT             <V76F13>
                                       OR FOUND-SELECTION               <V76F13>
              IF  OPTS-TYPE(IX)         = 'L'                           <V76F13>
              AND OPTS-CODE(IX)         = OPTS-SEL-CODE                 <V76F13>
                 MOVE 'Y'              TO WSAA-FOUND-SELECTION          <V76F13>
                 IF SR289-LXOPTIND      = SPACES                        <V76F13>
                    MOVE OPTS-IND(IX)  TO SR289-LXOPTIND                <V76F13>
                 END-IF                                                 <V76F13>
              END-IF                                                    <V76F13>
           END-PERFORM.                                                 <V76F13>
                                                                        <V76F13>
           MOVE RLSE                   TO RBNKSEQ-FUNCTION.             <V76F13>
           CALL 'RBNKSEQIO'         USING RBNKSEQ-PARAMS.               <V76F13>
           IF  RBNKSEQ-STATUZ       NOT = O-K                           <V76F13>
               MOVE RBNKSEQ-PARAMS     TO SYSR-PARAMS                   <V76F13>
               MOVE RBNKSEQ-STATUZ     TO SYSR-STATUZ                   <V76F13>
               PERFORM 600-FATAL-ERROR                                  <V76F13>
           END-IF.                                                      <V76F13>
                                                                        <V76F13>
       A398-INIT-OPT.                                                   <V76F13>
           IF SBM-CREATION                                              <V76F13>
              IF SR289-CRDTCARD     NOT = SPACES                        <V76F13>
              AND SR289-PAYTYPE         = CSTP-CREDIT-CARD-VAL          <V76F13>
              AND SR289-LXOPTIND        = SPACES                        <V76F13>
                 MOVE '+'              TO SR289-LXOPTIND                <V76F13>
              END-IF                                                    <V76F13>
           END-IF.                                                      <V76F13>
           IF NOT SBM-CREATION                                          <V76F13>
              IF SR289-LXOPTIND         = SPACES                        <V76F13>
                 MOVE 'Y'              TO SR289-LXOPTIND-OUT(PR)        <V76F13>
              ELSE                                                      <V76F13>
                 MOVE 'N'              TO SR289-LXOPTIND-OUT(PR)        <V76F13>
              END-IF                                                    <V76F13>
           END-IF.                                                      <V76F13>
                                                                        <V76F13>
       A399-EXIT.                                                       <V76F13>
            EXIT.                                                       <V76F13>
      /                                                                 <V76F13>
       A400-KEEPS-CCARD-DETAILS SECTION.                                <V76F13>
      **********************************                                <V76F13>
       A400-KEEPS.                                                      <V76F13>
      *---Keeps for Existence Check program                             <V76F13>
           MOVE SR289-TRANDATEX        TO WSSP-TRANDATE.                <V76F13>
           MOVE SR289-CASHNM           TO WSSP-PAYENME.                 <V76F13>
           MOVE SR289-RFCODE           TO WSSP-PTCODE.                  <V76F13>
           MOVE SR289-RFNUM            TO WSSP-PAYESEL.                 <V76F13>
                                                                        <V76F13>
           MOVE WSSP-RDOCPFX           TO RBNKSEQ-RDOCPFX.              <V76F13>
           MOVE WSSP-RDOCCOY           TO RBNKSEQ-RDOCCOY.              <V76F13>
           MOVE SR289-RECEIPT          TO RBNKSEQ-RDOCNUM.              <V76F13>
           MOVE SR289-SEQNBR           TO RBNKSEQ-SEQNBR.               <V76F13>
           MOVE SR289-CRCARDMOD        TO RBNKSEQ-CRCARDMOD.            <V76F13>
           MOVE SR289-RCPTSTAT         TO RBNKSEQ-RCPTSTAT.             <V76F13>
           MOVE SR289-CCMID            TO RBNKSEQ-CCMID.                <V76F13>
           MOVE SR289-MCHNTID          TO RBNKSEQ-MCHNTID.              <V76F13>
           MOVE SR289-CCTID            TO RBNKSEQ-CCTID.                <V76F13>
           MOVE SR289-TRMNLID          TO RBNKSEQ-TRMNLID.              <V76F13>
           MOVE SR289-CRDTCARD         TO RBNKSEQ-CRDTCARD.             <V76F13>
           MOVE SR289-AUTHID           TO RBNKSEQ-AUTHID.               <V76F13>
           MOVE SR289-CRCARDTYPE       TO RBNKSEQ-CRCARDTYPE.           <V76F13>
           MOVE SR289-CRCNAME          TO RBNKSEQ-CRCNAME.              <V76F13>
           MOVE SR289-CRCARDEXPM       TO RBNKSEQ-CRCARDEXPM.           <V76F13>
           MOVE SR289-CRCARDEXPY       TO RBNKSEQ-CRCARDEXPY.           <V76F13>
           MOVE SR289-AUTHDAT          TO RBNKSEQ-AUTHDAT.              <V76F13>
           MOVE RBNKSEQREC             TO RBNKSEQ-FORMAT.               <V76F13>
                                                                        <V76F13>
           MOVE KEEPS                  TO RBNKSEQ-FUNCTION.             <V76F13>
           CALL 'RBNKSEQIO'         USING RBNKSEQ-PARAMS.               <V76F13>
           IF  RBNKSEQ-STATUZ       NOT = O-K                           <V76F13>
               MOVE RBNKSEQ-PARAMS     TO SYSR-PARAMS                   <V76F13>
               MOVE RBNKSEQ-STATUZ     TO SYSR-STATUZ                   <V76F13>
               PERFORM 600-FATAL-ERROR                                  <V76F13>
           END-IF.                                                      <V76F13>
       A400-EXIT.                                                       <V76F13>
           EXIT.                                                        <V76F13>
      /                                                                 <V76F07>
       1600-DEFAULT-PAYOR SECTION.                                      <V76F07>
      ****************************                                      <V76F07>
       1610-BEGIN.                                                      <V76F07>
           IF WSSP-CHDRKEY              = SPACES                        <V76F07>
              GO TO 1690-EXIT.                                          <V76F07>
                                                                        <V76F07>
           MOVE 'PAYR'                 TO PYOR-FUNCTION.                <V76F07>
           MOVE SR289-TRANDATEX        TO PYOR-EFFDATE.                 <V76F07>
           MOVE WSSP-CHDRKEY           TO PYOR-CHDRKEY.                 <V76F07>
                                                                        <V76F07>
           CALL 'GETPAYR'           USING PYOR-GETPYOR-REC.             <V76F07>
           IF PYOR-STATUZ           NOT = O-K AND MRNF                  <V76F07>
              MOVE PYOR-STATUZ         TO SYSR-STATUZ                   <V76F07>
              PERFORM 600-FATAL-ERROR                                   <V76F07>
           END-IF.                                                      <V76F07>
                                                                        <V76F07>
           IF PYOR-STATUZ           NOT = O-K                           <V76F07>
              GO TO 1690-EXIT                                           <V76F07>
           END-IF.                                                      <V76F07>
                                                                        <V76F07>
           MOVE PYOR-CLNTPFX           TO SR289-RFCODE.                 <V76F07>
           MOVE PYOR-COWNNUM           TO SR289-RFNUM.                  <V76F07>
                                                                        <V76F07>
       1690-EXIT.                                                       <V76F07>
           EXIT.                                                        <V76F07>
      /
       PRE-SCREEN-EDIT SECTION.
      *************************
      *
       PRE-START.
      *                                                         <V76F04><S9503>
      * If returning from an optional selection skip this section.      <S9503>
      *                                                         <V76F04><S9503>
           MOVE O-K                    TO WSSP-EDTERROR.                <S9503>
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'                    <V76F04>
              MOVE 3000                TO WSSP-SECTIONNO                <V76F04>
              GO TO PRE-EXIT                                            <V76F04>
           END-IF.                                                      <V76F04>
      *    IF SBM-INQUIRY                                               <V76F04>
      *       MOVE PROT                 TO SCRN-FUNCTION                <V76F04>
      *    END-IF.                                                      <V76F04>
           IF SBM-INQUIRY                                               <V76F04>
           OR SBM-MODIFY                                                <V76F12>
              MOVE 'Y'                  TO SR289-TRANDATEX-OUT (PR)     <V76F04>
                                           SR289-RFCODE-OUT    (PR)     <V76F04>
                                           SR289-RFNUM-OUT     (PR)     <V76F04>
           END-IF.                                                      <V76F04>
                                                                        <CS016>
           IF WSAA-SBMACTION           = 'M'                            <CS016>
              MOVE 'N'                 TO SR289-RFNUM-OUT (PR)          <CS016>
              MOVE 'N'                 TO SR289-TRANDATEX-OUT (PR)      <CS016>
           END-IF.                                                      <CS016>
                                                                        <CS016>
           IF NOT-POSTDATED                                             <V76F12>
              MOVE 'Y'                 TO SR289-POSTDTEFLG-OUT(ND)      <V76F12>
           ELSE                                                         <V76F12>
              MOVE 'N'                 TO SR289-POSTDTEFLG-OUT(ND)      <V76F12>
           END-IF.                                                      <V76F12>

           GO TO PRE-EXIT.
       PRE-EXIT.
           EXIT.
      /
      *****************************************************************
      *     RETRIEVE SCREEN FIELDS AND EDIT
      *****************************************************************
      *
       2000-SCREEN-EDIT SECTION.
      **************************
      *
       2010-SCREEN-IO.
      *
           MOVE O-K                    TO WSSP-EDTERROR.
           MOVE SCRN-STATUZ            TO WSAA-SCRN-STATUZ.
                                                                        <PHLRMS>
           IF  WSAA-SBMACTION          = 'A'                            <PHLRMS>
           AND (SR289-RCPREPRTCD       NOT = WSAA-TMP-RCPREPRTCD        <PHLRMS>
           OR  SR289-RCPREPRNT         NOT = WSAA-TMP-RCPREPRNT )       <PHLRMS>
               MOVE SR289-RCPREPRTCD   TO WSAA-TMP-RCPREPRTCD           <PHLRMS>
               MOVE SR289-RCPREPRNT    TO WSAA-TMP-RCPREPRNT            <PHLRMS>
               MOVE SPACES             TO SR289-DENTTYP                 <PHLRMS>
               MOVE SPACES             TO SR289-ENTYNO                  <PHLRMS>
               MOVE SPACES             TO SR289-SFLAG                   <PHLRMS>
               MOVE 'N'                TO SR289-CONFIRM                 <PHLRMS>
               MOVE 'Y'                TO SR289-CONFIRM-OUT  (PR)       <PHLRMS>
               MOVE 'Y'                TO SR289-CONFIRM-OUT  (ND)       <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
      *F5=CALC
           IF  WSAA-SCRN-STATUZ        = CALC
               MOVE 'Y'                TO WSSP-EDTERROR
           END-IF.
                                                                        <PHLRMS>
           IF  ALNO-STATUZ         NOT = O-K                            <FA3287>
               MOVE ALNO-STATUZ        TO SR289-RECEIPT-ERR             <FA3287>
           END-IF.                                                      <FA3287>
                                                                        <PS008>
                                                                        <FA3287>
      * Skip validation if ...
           IF SBM-INQUIRY        OR
              SBM-CANCELLATION
              IF  RCTSTATFLG                                            <V76F12>
              AND SBM-CANCELLATION                                      <V76F12>
                  PERFORM 2900-VALIDATE-REASON                          <V76F12>
              END-IF                                                    <V76F12>
              GO TO 2090-EXIT
           END-IF.

      * Roll Up
           IF  WSAA-SCRN-STATUZ        = ROLU
               PERFORM 2100-LOAD-SUBFILE
               MOVE 'Y'                TO WSSP-EDTERROR
               GO TO 2090-EXIT
           END-IF.
      *
       2010-VALIDATE-SCREEN.
      *
           IF ALNO-EXHAUSTED                                            <V64F13>
              MOVE ALNO-STATUZ         TO SR289-RECEIPT-ERR             <V64F13>
           END-IF.                                                      <V64F13>
                                                                        <V64F13>
           IF SR289-TRANDATEX          = VRCM-MAX-DATE
              MOVE F665                TO SR289-TRANDATEX-ERR
           ELSE
              IF SR289-TRANDATEX       = ZEROES
                 MOVE F401             TO SR289-TRANDATEX-ERR
              END-IF
           END-IF.
      *
           IF SR289-TRANDATEX          > WSAA-TODAY
              MOVE F530                TO SR289-TRANDATEX-ERR
           END-IF.
      *
      * Validate Received-from Code.
      *
           INITIALIZE                     ITEM-DATA-KEY.
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.
           MOVE T3674                  TO ITEM-ITEMTABL.
           MOVE SR289-RFCODE           TO ITEM-ITEMITEM.
           PERFORM B1000-READR-ITEM.

           IF ITEM-STATUZ               = MRNF
              MOVE E423                TO SR289-RFCODE-ERR
           END-IF.
      *
      * Validate Received-from client/agent Number - accordingly - and
      * retrieve the name with a Salut-Givname-Surname format.
      *
           IF SR289-RFNUM              = SPACES                         <V76F07>
           AND SR289-RFCODE            = CSRF-CN                        <V76F07>
              PERFORM 1600-DEFAULT-PAYOR                                <V76F07>
           END-IF.                                                      <V76F07>
                                                                        <V76F07>
           EVALUATE SR289-RFCODE
              WHEN CSRF-CN PERFORM 2300-VALIDATE-CLIENT
              WHEN CSRF-AG PERFORM 2400-VALIDATE-AGENT
           END-EVALUATE.

           IF SR289-RFCODE-ERR         = SPACES AND
              SR289-RFNUM-ERR          = SPACES
              PERFORM A2000-FORMAT-NAME
              MOVE WSAA-CASHNM         TO SR289-CASHNM
           END-IF.
                                                                        <V64F13>
      * Validation Pre-Printed Code is Entered correctly                <RC002>
      * If Action = 'A' (Cash not Banked), Pre-printed Document must    <RC002>
      * be entered. Pre-printed Code will be verified.                  <RC002>
           IF WSAA-SBMACTION            = 'A'                           <RC002>
              IF SR289-RCPREPRNT        = SPACES                        <RC002>
                 MOVE E186             TO SR289-RCPREPRNT-ERR           <RC002>
              END-IF                                                    <RC002>
              IF SR289-RCPREPRTCD       = SPACES                        <RC002>
                 MOVE E186             TO SR289-RCPREPRTCD-ERR          <RC002>
              END-IF                                                    <RC002>
           END-IF.                                                      <RC002>
                                                                        <RC002>
      * In case Modify, if the Receipt have already the pre-printed     <RC002>
      * docs, don't allow deleting it.                                  <RC002>
      * Error message: "Field cannot be blank"                          <RC002>
      *    IF SBM-MODIFY                                        <CS016> <RC002>
           IF WSAA-SBMACTION                = 'K'                       <CS016>
               IF (ZPPRENQ-RCPREPRTCD   NOT = SPACES AND                <RC002>
                   SR289-RCPREPRTCD         = SPACES  )                 <RC002>
               OR (ZPPRENQ-RCPREPRNT    NOT = SPACES AND                <RC002>
                   SR289-RCPREPRNT          = SPACES  )                 <RC002>
                   MOVE H366               TO SR289-RCPREPRTCD-ERR      <RC002>
                                              SR289-RCPREPRNT-ERR       <RC002>
               END-IF                                                   <RC002>
           END-IF.                                                      <RC002>
                                                                        <RC002>
                                                                        <RC002>
      * Check if users enter Pre-printed, both of 2 fls must be entered <RC002>
      * If not, show the message "Enter both fields"                    <RC002>
           IF SR289-RCPREPRTCD      NOT = SPACES                        <RC002>
           AND SR289-RCPREPRNT          = SPACES                        <RC002>
               MOVE E901               TO SR289-RCPREPRNT-ERR.          <RC002>
                                                                        <RC002>
           IF SR289-RCPREPRNT       NOT = SPACES                        <RC002>
           AND SR289-RCPREPRTCD         = SPACES                        <RC002>
               MOVE E901               TO SR289-RCPREPRTCD-ERR.         <RC002>
                                                                        <RC002>
      * Verify Pre-Printed Code entered correctly                       <RC002>
      *  Read TV021 Table with Pre-printed code as the item key         <PHLRMS>
           IF  WSAA-SBMACTION          = 'A'                            <PHLRMS>
              IF  SR289-RCPREPRTCD     NOT = SPACES                     <PHLRMS>
                  INITIALIZE              ITEM-PARAMS                   <PHLRMS>
                  MOVE SMTP-ITEM       TO ITEM-ITEMPFX                  <PHLRMS>
                  MOVE WSSP-COMPANY    TO ITEM-ITEMCOY                  <PHLRMS>
                  MOVE TV021           TO ITEM-ITEMTABL                 <PHLRMS>
                  MOVE SR289-RCPREPRTCD                                 <PHLRMS>
                                       TO ITEM-ITEMITEM                 <PHLRMS>
                  MOVE BEGN            TO ITEM-FUNCTION                 <PHLRMS>
                                                                        <PHLRMS>
                  CALL 'ITEMIO'        USING ITEM-PARAMS                <PHLRMS>
                                                                        <PHLRMS>
                  IF ITEM-STATUZ       NOT = O-K AND ENDP               <PHLRMS>
                     MOVE ITEM-PARAMS  TO SYSR-PARAMS                   <PHLRMS>
                     PERFORM 600-FATAL-ERROR                            <PHLRMS>
                  END-IF                                                <PHLRMS>
                                                                        <PHLRMS>
                  IF ITEM-STATUZ        = ENDP                          <PHLRMS>
                  OR ITEM-ITEMITEM     NOT = SR289-RCPREPRTCD           <PHLRMS>
                     MOVE EZ23         TO SR289-RCPREPRTCD-ERR          <PHLRMS>
                  END-IF                                                <PHLRMS>
              END-IF                                                    <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <RC002>
      *                                                                 <RC002>
      * If pre-printed number entered, it must be exactly 8 characters  <RC002>
      *                                                                 <RC002>
           IF  WSAA-SBMACTION          = 'A'                            <PHLRMS>
           OR  WSAA-SBMACTION          = 'B'                            <PS008>
               IF  SR289-RCPREPRNT     NOT = SPACES                     <PHLRMS>
                   MOVE ZEROS          TO WSAA-COUNTER                  <PHLRMS>
                   INSPECT SR289-RCPREPRNT                              <PHLRMS>
                   TALLYING WSAA-COUNTER  FOR ALL ' '                   <PHLRMS>
                   IF WSAA-COUNTER     NOT = ZEROS                      <PHLRMS>
                       MOVE E901       TO SR289-RCPREPRNT-ERR           <PHLRMS>
                   END-IF                                               <PHLRMS>
               END-IF                                                   <PHLRMS>
           END-IF.                                                      <RC002>
      *                                                                 <RC002>
      * Check Duplicated Pre-printed documents                          <RC002>
      *                                                                 <RC002>
           IF SR289-RCPREPRNT       NOT = SPACES                        <RC002>
           AND SR289-RCPREPRTCD     NOT = SPACES                        <RC002>
           AND SR289-RCPREPRNT      NOT = '00000000'                    <RC002>
      *        MOVE SPACE              TO ZPPRENQ-PARAMS                <RC002>
      *        MOVE O-K                TO ZPPRENQ-STATUZ                <RC002>
      *        MOVE PRFX-CASH          TO ZPPRENQ-PREFIX                <RC002>
      *        MOVE WSSP-COMPANY       TO ZPPRENQ-COMPANY               <RC002>
      *        MOVE WSSP-BANKCODE      TO ZPPRENQ-BANKCODE              <RC002>
      *        MOVE SR289-RCPREPRTCD   TO ZPPRENQ-RCPREPRTCD            <RC002>
      *        MOVE SR289-RCPREPRNT    TO ZPPRENQ-RCPREPRNT             <RC002>
      *        MOVE ZPPRENQREC         TO ZPPRENQ-FORMAT                <RC002>
      *        MOVE BEGN               TO ZPPRENQ-FUNCTION              <RC002>
                                                                        <RC002>
      *        CALL 'ZPPRENQIO'     USING ZPPRENQ-PARAMS                <RC002>
                                                                        <RC002>
      *        IF ZPPRENQ-STATUZ    NOT = O-K AND ENDP                  <RC002>
      *           MOVE ZPPRENQ-PARAMS      TO SYSR-PARAMS               <RC002>
      *           MOVE ZPPRENQ-STATUZ      TO SYSR-STATUZ               <RC002>
      *           PERFORM 600-FATAL-ERROR                               <RC002>
      *        END-IF                                                   <RC002>
                                                                        <RC002>
      *        IF ZPPRENQ-STATUZ        = O-K                           <RC002>
      *        AND ZPPRENQ-PREFIX       = PRFX-CASH                     <RC002>
      *        AND ZPPRENQ-COMPANY      = WSSP-COMPANY                  <RC002>
      *        AND ZPPRENQ-BANKCODE     = WSSP-BANKCODE                 <RC002>
      *        AND ZPPRENQ-RCPREPRTCD   = SR289-RCPREPRTCD              <RC002>
      *        AND ZPPRENQ-RCPREPRNT    = SR289-RCPREPRNT               <RC002>
      *   If Creation, just alert duplicate                             <RC002>
      *        IF SBM-CREATION                                          <RC002>
      *           MOVE E048       TO SR289-RCPREPRNT-ERR                <RC002>
      *        END-IF                                                   <RC002>
      *   If Modify, check if this number is already used               <RC002>
      *   for another receipt?                                          <RC002>
      *        IF SBM-MODIFY                                            <RC002>
      *        AND ZPPRENQ-RECEIPT NOT = SR289-RECEIPT                  <RC002>
      *            MOVE E048       TO SR289-RCPREPRNT-ERR               <RC002>
      *        END-IF                                                   <RC002>
                                                                        <RC002>
               INITIALIZE                 ZPPRCHK-PARAMS                <RC002>
                                          ZPPRCHK-DATA-AREA             <RC002>
               MOVE 'N'                 TO WSAA-DUPLICATE-FLAG          <RC002>
               MOVE O-K                TO ZPPRCHK-STATUZ                <RC002>
               MOVE PRFX-CASH          TO ZPPRCHK-PREFIX                <RC002>
               MOVE WSSP-COMPANY       TO ZPPRCHK-COMPANY               <RC002>
      ***      MOVE WSSP-BANKCODE      TO ZPPRCHK-BANKCODE              <RC002>
               MOVE SR289-RCPREPRTCD   TO ZPPRCHK-RCPREPRTCD            <RC002>
               MOVE SR289-RCPREPRNT    TO ZPPRCHK-RCPREPRNT             <RC002>
               MOVE ZPPRCHKREC         TO ZPPRCHK-FORMAT                <RC002>
               MOVE BEGN               TO ZPPRCHK-FUNCTION              <RC002>
                                                                        <RC002>
               PERFORM C4000-READ-ZPPRCHK UNTIL ZPPRCHK-STATUZ = ENDP   <RC002>
                                         OR WSAA-DUPLICATE-FLAG = 'Y'   <RC002>
                                                                        <RC002>
               IF WSAA-DUPLICATE-FLAG = 'Y'                             <RC002>
                  IF SBM-CREATION                                       <RC002>
                     MOVE E048       TO SR289-RCPREPRNT-ERR             <RC002>
                  END-IF                                                <RC002>
                                                                        <RC002>
                  IF SBM-MODIFY                                         <RC002>
                      MOVE E048       TO SR289-RCPREPRNT-ERR            <RC002>
                  END-IF                                                <RC002>
               END-IF                                                   <RC002>
           END-IF.                                                      <RC002>
                                                                        <PHLRMS>
           IF  WSAA-SBMACTION         = 'A'                             <PHLRMS>
           OR  WSAA-SBMACTION         = 'B'                             <PS008>
      ***      IF  SR289-RCPREPRNT    NOT NUMERIC                       <PHLRMS>
      ***          MOVE F200          TO SR289-RCPREPRNT-ERR            <PHLRMS>
      ***      ELSE                                                     <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Validate TR/RN receipt                                       <PHLRMS>
      *                                                                 <PHLRMS>
               IF  SR289-RCPREPRNT    IS NUMERIC                        <PHLRMS>
                   PERFORM 2200-VALIDATE-TRRN                           <PHLRMS>
               END-IF                                                   <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <V64F13>
        2020-VALIDATE-BY-TABLE.                                         <V64F13>
                                                                        <V64F13>
            PERFORM C2000-FIELD-CHECK.                                  <V64F13>
                                                                        <V64F13>
      *
       2050-CHECK-FOR-ERRORS.
      *
           IF SR289-ERROR-INDICATORS   NOT = SPACES
              MOVE 'Y'                 TO WSSP-EDTERROR
           END-IF.
      *
       2060-VALIDATE-SUBFILE.
      *
           MOVE 'N'                    TO WSAA-EXPIRY-WARN.             <CS009>
           MOVE ZEROES                 TO WSAA-TOT-DOCORIGAMT
                                          WSAA-TOT-DOCACCTAMT.
           MOVE ZEROES                 TO WSAA-SFL-RECORD.              <V76F12>
                                                                        <V76F12>
           MOVE 'N'                    TO WSAA-RTYP-CASH
                                          WSAA-RTYP-CHEQ
                                          WSAA-RTYP-BKCD
                                          WSAA-RTYP-JNL
                                          WSAA-RTYP-DSH-CHQ
                                          WSAA-RTYP-BANK-TAPE
                                          WSAA-RTYP-CHEQ-CANCEL
                                          WSAA-RTYP-CREDIT-CARD.
           MOVE 'N'                    TO WSAA-FOUND-SELECTION.
                                                                        <V74F03>
           MOVE SPACES                 TO WSAA-PAYMENT-TYPE             <V74F03>
                                          WSAA-ORIG-CURR.               <V74F03>
                                                                        <V76F12>
           MOVE SSTRT                  TO SCRN-FUNCTION.
           PERFORM 9000-SCREEN-IO.

           PERFORM 2600-VALIDATE-SUBFILE
                                       UNTIL SCRN-STATUZ = ENDP.
                                                                        <PHLRMS>
                                                                        <PHLRMS>
           IF  WSAA-SBMACTION          = 'A'                            <PHLRMS>
               IF  SR289-CONFIRM-ERR   NOT = SPACES                     <PHLRMS>
               OR  WSSP-EDTERROR       = 'Y'                            <PHLRMS>
                   MOVE 'N'            TO SR289-CONFIRM                 <PHLRMS>
               ELSE                                                     <PHLRMS>
                   MOVE 'Y'            TO SR289-CONFIRM                 <PHLRMS>
               END-IF                                                   <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  WSAA-SBMACTION          = 'A'                            <PHLRMS>
           OR                          = 'B'                            <CS009>
               IF  SR289-CONFIRM       = 'N'                            <PHLRMS>
               AND SR289-CONFIRM-ERR   NOT = SPACES                     <PHLRMS>
                   MOVE 'Y'            TO WSSP-EDTERROR                 <PHLRMS>
               END-IF                                                   <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           PERFORM 2700-VALIDATE-POSTDATED.                             <V76F12>
           PERFORM 2700A-CHECK-ONE-DISS                                 <V76F13>

           MOVE 1                      TO SCRN-SUBFILE-RRN.

           MOVE WSAA-TOT-DOCORIGAMT    TO SR289-ORIGAMT.
           MOVE WSAA-TOT-DOCACCTAMT    TO SR289-ACCTAMT.

      * For partial cancellation , at least one subfile must be seleted
           IF SBM-PARTIAL-CANCEL
              IF NOT FOUND-SELECTION
                 MOVE SREAD            TO SCRN-FUNCTION
                 PERFORM 9000-SCREEN-IO

                 MOVE E207             TO SR289-SELECT-ERR
                 MOVE 'Y'              TO WSSP-EDTERROR

                 MOVE SUPD             TO SCRN-FUNCTION
                 PERFORM 9000-SCREEN-IO
              END-IF
           END-IF.
                                                                        <V76F04>
           PERFORM 2800-CHECK-REMARKS.                                  <V76F04>
                                                                        <V74F03>
           IF  SBM-CREATION                                             <V74F03>
           AND WSSP-EDTERROR            = O-K                           <V74F03>
           AND SCRN-DEVICE-IND      NOT = '*RMT'                        <V74F03>
           AND WSAA-SCRN-STATUZ     NOT = 'DIRY'                        <V76F04>
               PERFORM 2500-LAUNDERING-CHECK                            <V74F03>
           END-IF.                                                      <V74F03>
                                                                        <V74F03>
                                                                        <V74F03>
       2090-EXIT.
            EXIT.
      /
       2100-LOAD-SUBFILE SECTION.
      ***************************
       2100-BEGIN.
           MOVE 1                      TO WSAA-LINE-CNT.

           PERFORM 1310-LOAD-SUBFILE
                   UNTIL RBNK-STATUZ   = ENDP
                     AND WSAA-LINE-CNT > SR289-SUBFILE-PAGE.
      *
       2100-EXIT.
            EXIT.
      /                                                                 <PHLRMS>
       2200-VALIDATE-TRRN SECTION.                                      <PHLRMS>
      ****************************                                      <PHLRMS>
      *                                                                 <PHLRMS>
       2210-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           IF  SR289-RCPREPRTCD        NOT = SPACES                     <PHLRMS>
           AND SR289-RCPREPRNT         NOT = SPACES                     <PHLRMS>
               PERFORM A2400-READ-RCPY                                  <PHLRMS>
               MOVE SR289-RCPREPRTCD   TO WSAA-RCPREPRTCD               <PS008>
               MOVE SR289-RCPREPRNT    TO WSAA-RCPREPRNT                <PS008>
               PERFORM A2100-FIND-LOCATION                              <PHLRMS>
                                                                        <PS008>
               IF ( WSAA-SBMACTION     = 'B'                            <CS009>
               OR                      = 'A' )                          <CS009>
               AND TRRNINF-RCSTAT      NOT = 'AS'                       <PS008>
               AND                     NOT = 'RA'                       <PS008>
                  MOVE EV17            TO SR289-RCPREPRTCD-ERR          <PS008>
                  MOVE EV17            TO SR289-RCPREPRNT-ERR           <PS008>
                  MOVE 'N'             TO WSAA-VALID-RECEIPT            <PS008>
               END-IF                                                   <PS008>
                                                                        <PS008>
               IF  WSAA-VALID-RECEIPT  = 'Y'                            <PHLRMS>
                   MOVE RCPYACT-DENTTYP                                 <PHLRMS>
                                       TO SR289-DENTTYP                 <PHLRMS>
                   MOVE RCPYACT-ENTYNUM                                 <PHLRMS>
                                       TO SR289-ENTYNO                  <PHLRMS>
                   PERFORM A2200-VALIDATE-ENTITY                        <PHLRMS>
                   IF  SR289-RCPREPRTCD-ERR  = SPACES                   <PHLRMS>
                   AND SR289-RCPREPRNT-ERR   = SPACES                   <PHLRMS>
                       PERFORM A2700-CHECK-EXPIRY                       <PHLRMS>
                   END-IF                                               <PHLRMS>
               END-IF                                                   <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       2290-EXIT.                                                       <PHLRMS>
            EXIT.                                                       <PHLRMS>
      /
       2300-VALIDATE-CLIENT SECTION.
      ******************************
       2300-BEGIN.
      *
           INITIALIZE                     CLNT-DATA-KEY.
           MOVE PRFX-CLNT              TO CLNT-CLNTPFX.
           MOVE WSSP-FSUCO             TO CLNT-CLNTCOY.
           MOVE SR289-RFNUM            TO CLNT-CLNTNUM.
           PERFORM B1030-READR-CLNT.

           IF CLNT-STATUZ              = MRNF
              MOVE E133                TO SR289-RFNUM-ERR
           END-IF.
      *                                                                 <CS012>
      * Check Master Client or not:                                     <CS012>
      *                                                                 <CS012>
           IF CLNT-STATUZ              = O-K                            <CS012>
           AND CLNT-CLTIND             NOT = 'C'                        <CS012>
               MOVE P245               TO SR289-RFNUM-ERR               <CS012>
           END-IF.                                                      <CS012>
      *
       2390-EXIT.
            EXIT.
      /
       2400-VALIDATE-AGENT SECTION.
      *****************************
       2410-BEGIN.
      *
           INITIALIZE                     AGNT-DATA-AREA.
           MOVE SR289-RFCODE           TO AGNT-AGNTPFX.
           MOVE WSSP-COMPANY           TO AGNT-AGNTCOY.
           MOVE SR289-RFNUM            TO AGNT-AGNTNUM.
           PERFORM B1040-READR-AGNT.

           IF AGNT-STATUZ              = MRNF
              MOVE E305                TO SR289-RFNUM-ERR
           END-IF.
      *
       2490-EXIT.
            EXIT.
      /
       2500-LAUNDERING-CHECK SECTION.                                   <V74F03>
      *******************************                                   <V74F03>
       2510-BEGIN.                                                      <V74F03>
                                                                        <V74F03>
           INITIALIZE                     AMLC-VALID-REC.               <V74F03>
           MOVE 'CHCK'                 TO AMLC-FUNCTION.                <V74F03>
           MOVE WSSP-LANGUAGE          TO AMLC-LANGUAGE.                <V74F03>
           MOVE O-K                    TO AMLC-STATUZ.                  <V74F03>
           MOVE WSSP-FSUCO             TO AMLC-FSUCO.                   <V74F03>
           MOVE WSSP-COMPANY           TO AMLC-COMPANY.                 <V74F03>
           MOVE WSSP-BRANCH            TO AMLC-BRANCH.                  <V74F03>
           MOVE WSAA-TODAY             TO AMLC-EFFDATE.                 <V74F03>
           MOVE SR289-RFCODE           TO AMLC-PREFIX.                  <V74F03>
           MOVE SR289-RFNUM            TO AMLC-RFNUM.                   <V74F03>
           MOVE WSKY-BATC-BATCTRCDE    TO AMLC-TRCDE.                   <V74F03>
           MOVE WSAA-ORIG-CURR         TO AMLC-CURRENCY.                <V74F03>
           MOVE WSAA-PAYMENT-TYPE      TO AMLC-TYPE.                    <V74F03>
           MOVE WSAA-TOT-DOCORIGAMT    TO AMLC-TRANAMT.                 <V74F03>
           MOVE SPACES                 TO AMLC-AMLIND.                  <V74F03>
           CALL 'AMLCHK'            USING AMLC-VALID-REC.               <V74F03>
           IF AMLC-STATUZ           NOT = O-K                           <V74F03>
               MOVE AMLC-VALID-REC     TO SYSR-PARAMS                   <V74F03>
               MOVE AMLC-STATUZ        TO SYSR-STATUZ                   <V74F03>
               PERFORM 600-FATAL-ERROR                                  <V74F03>
           END-IF.                                                      <V74F03>
                                                                        <V74F03>
           IF AMLC-AMLIND               = 'Y'                           <V74F03>
              MOVE SPACES              TO MBOX-CPFMSG MBOX-INSERT       <V74F03>
                                          MBOX-REPLY  MBOX-RESULT       <V74F03>
              MOVE 'X'                 TO MBOX-REPLY                    <V74F03>
              MOVE WSAA-TOT-DOCORIGAMT                                  <V74F03>
                                       TO WSAA-VALUE1                   <V74F03>
              MOVE WSAA-VALUE          TO MBOX-INSERT                   <V74F03>
              MOVE WSSP-LANGUAGE       TO WSAA-LANGUAGE                 <V74F03>
              MOVE '0003'              TO WSAA-MSGID                    <V74F03>
              MOVE WSAA-PAXMSG         TO MBOX-CPFMSG                   <V74F03>
              CALL 'MSGBOX'         USING MBOX-CPFMSG MBOX-INSERT       <V74F03>
                                          MBOX-REPLY  MBOX-RESULT       <V74F03>
              IF  MBOX-RESULT       NOT = O-K                           <V74F03>
              OR  MBOX-REPLY        NOT = 'Y'                           <V74F03>
                  MOVE 'Y'              TO WSSP-EDTERROR                <V74F03>
              END-IF                                                    <V74F03>
           END-IF.                                                      <V74F03>
                                                                        <V74F03>
       2590-EXIT.                                                       <V74F03>
           EXIT.                                                        <V74F03>
      /                                                                 <V74F03>
       2600-VALIDATE-SUBFILE SECTION.
      *******************************
      *
       2610-VALIDATION.
      * If Partial cancellation , at least one subfile must be selected
           IF SBM-PARTIAL-CANCEL
              IF SR289-SELECT      NOT =  SPACES
                 MOVE 'Y'              TO WSAA-FOUND-SELECTION
                 IF SR289-CNRSNCD       = SPACES                        <V76F12>
                 AND RCTSTATFLG                                         <V76F12>
                    MOVE E186          TO SR289-CNRSNCD-ERR             <V76F12>
                    MOVE 'Y'           TO WSSP-EDTERROR                 <V76F12>
                 END-IF                                                 <V76F12>
                 IF SR289-CNRSNCD   NOT = SPACES                        <V76F12>
                 AND RCTSTATFLG                                         <V76F12>
                    PERFORM 2910-READ-TR29H                             <V76F12>
                 END-IF                                                 <V76F12>
              END-IF
           END-IF.
                                                                        <V76F12>
      *Refresh Original Amount when input Pre-printed Number.           <PS008>
           IF WSAA-SCRN-STATUZ        = CALC                            <PS008>
           AND SR289-RCPREPRTCD       = 'RN'                            <CS009>
           AND SR289-RCPREPRTCD-ERR   = SPACES                          <PS008>
           AND SR289-RCPREPRNT-ERR    = SPACES                          <PS008>
           AND SCRN-SUBFILE-RRN        = 1                              <CS009>
      *    AND SR289-DOCORIGAMT       NOT = ZERO                        <CS009>
               PERFORM A2400-READ-RCPY                                  <PS008>
               IF  WSAA-AMNT          > ZERO                            <PS008>
               AND SR289-DOCORIGAMT   NOT = WSAA-AMNT                   <PS008>
                   MOVE WSAA-AMNT     TO SR289-DOCORIGAMT               <CS009>
               END-IF                                                   <PS008>
           END-IF.                                                      <PS008>
                                                                        <PS008>
      * If Partial cancellation , only compute and
      * accumulate the amounts , no other validations.
           IF SBM-PARTIAL-CANCEL
              GO TO 2645-ACCUMULATE-TOTALS
           END-IF.

           IF  SBM-CREATION                                             <V74F03>
           AND SCRN-SUBFILE-RRN        = 1                              <V74F03>
               MOVE SR289-PAYTYPE      TO WSAA-PAYMENT-TYPE             <V74F03>
               MOVE SR289-ORIGCURR     TO WSAA-ORIG-CURR                <V74F03>
               IF  WSAA-SBMACTION          = 'A'                        <PHLRMS>
                   PERFORM A2500-CHECK-TEMP-RCPT                        <PHLRMS>
                   IF  SR289-RCPREPRTCD    = 'RN'                       <PHLRMS>
      ***          AND SR289-RCPREPRNT     IS NUMERIC                   <PHLRMS>
                   AND SR289-RCPREPRTCD-ERR = SPACES                    <PHLRMS>
                   AND SR289-RCPREPRNT-ERR  = SPACES                    <PHLRMS>
                   AND SR289-DOCORIGAMT    NOT = ZERO                   <PHLRMS>
                       IF  SR289-CONFIRM   NOT = 'Y'                    <PHLRMS>
                           PERFORM A2400-READ-RCPY                      <PHLRMS>
                           IF  WSAA-AMNT       > ZERO                   <PHLRMS>
                               IF SR289-DOCORIGAMT NOT = WSAA-AMNT      <PHLRMS>
                                  MOVE G822    TO SR289-CONFIRM-ERR     <PHLRMS>
                                  MOVE WSAA-WARN                        <PHLRMS>
                                               TO SR289-SFLAG           <PHLRMS>
                                 MOVE 'N'     TO SR289-CONFIRM-OUT(PR)  <PHLRMS>
                                 MOVE 'N'     TO SR289-CONFIRM-OUT(ND)  <PHLRMS>
                              END-IF                                    <PHLRMS>
                          END-IF                                        <PHLRMS>
                      END-IF                                            <PHLRMS>
                  END-IF                                                <PHLRMS>
               END-IF                                                   <PHLRMS>
               IF  WSAA-SBMACTION          = 'B'                        <PS008>
               AND SR289-RCPREPRTCD        = 'RN'                       <PS008>
               AND SR289-RCPREPRTCD-ERR    = SPACES                     <PS008>
               AND SR289-RCPREPRNT-ERR     = SPACES                     <PS008>
               AND SR289-DOCORIGAMT        NOT = ZERO                   <PS008>
                   PERFORM A2400-READ-RCPY                              <PS008>
                   IF WSAA-AMNT            > ZERO                       <PS008>
                   AND SR289-DOCORIGAMT    NOT = WSAA-AMNT              <PS008>
                        MOVE G822          TO SR289-DOCORIGAMT-ERR      <PS008>
                   END-IF                                               <PS008>
               END-IF                                                   <PS008>
           END-IF.                                                      <V74F03>
                                                                        <V74F03>
      *-- Receipt Status                                                <V76F12>
      *-- Get Receipt status in T3676 base on the transaction code.     <V76F12>
           PERFORM 2671-VALIDATE-RCPTSTAT.                              <V76F12>
                                                                        <V76F13>
      * Initialize subfile check box                                    <V76F13>
           PERFORM A300-SUBFILE-INIT-OPTSWCH.                           <V76F13>
           IF SR289-LXOPTIND        NOT = SPACES                        <V76F13>
           AND SR289-PAYTYPE        NOT = CSTP-CREDIT-CARD-VAL          <V76F13>
               MOVE W121               TO SR289-LXOPTIND-ERR            <V76F13>
           END-IF.                                                      <V76F13>
                                                                        <V76F13>
           IF SR289-LXOPTIND        NOT = SPACES AND 'X' AND '+'        <V76F13>
           AND SR289-PAYTYPE            = CSTP-CREDIT-CARD-VAL          <V76F13>
               MOVE H118               TO SR289-LXOPTIND-ERR            <V76F13>
           END-IF.                                                      <V76F13>
      *--Always initialize credit card information if payment type not  <V76F13>
      *--equal to '9' during creation of receipt.  Just in case the     <V76F13>
      *--payment type was changed.                                      <V76F13>
           IF  SBM-CREATION                                             <V76F13>
           AND SR289-PAYTYPE        NOT = CSTP-CREDIT-CARD-VAL          <V76F13>
               MOVE SPACES                 TO SR289-CRCARDMOD           <V76F13>
                                              SR289-CCMID               <V76F13>
                                              SR289-CCTID               <V76F13>
                                              SR289-MCHNTID             <V76F13>
                                              SR289-TRMNLID             <V76F13>
                                              SR289-CRCARDTYPE          <V76F13>
                                              SR289-CRDTCARD            <V76F13>
                                              SR289-CRCNAME             <V76F13>
                                              SR289-AUTHID              <V76F13>
               MOVE ZEROES                 TO SR289-CRCARDEXPY          <V76F13>
                                              SR289-CRCARDEXPM          <V76F13>
           END-IF.                                                      <V76F13>
                                                                        <V76F12>
      * Validate Payment Type
           IF SR289-PAYTYPE       NOT = SPACES
              ADD 1                    TO WSAA-SFL-RECORD               <V76F12>
              PERFORM 2640-VALIDATE-PAYTYPE
              IF SR289-PAYTYPE-ERR NOT = SPACES
                 GO TO 2670-UPDATE-ERROR-INDICATORS
              END-IF
                                                                        <V76F07>
      * Pay Type.                                                       <V76F07>
              MOVE SPACES              TO CHKF-FIELD-ID                 <V76F07>
              MOVE 'SR289-PAYTYPE'     TO CHKF-FIELD-ID                 <V76F07>
              CALL 'CHKFSUF'       USING CHKF-CHKF-REC                  <V76F07>
                                          SR289-PAYTYPE                 <V76F07>
                                          SR289-PAYTYPE-ERR             <V76F07>
                                          SR289-PAYTYPE-OUT(PR)         <V76F07>
                                                                        <V76F07>
              IF SR289-PAYTYPE-ERR       NOT = SPACES                   <V76F07>
                 MOVE 'Y'              TO WSSP-EDTERROR                 <V76F07>
                 GO TO 2670-UPDATE-ERROR-INDICATORS                     <V76F07>
              END-IF                                                    <V76F07>
           ELSE                                                         <FA3287>
              MOVE SPACE               TO CSTP-CASHTYPE                 <FA3287>
           END-IF.                                                      <V76F12>
                                                                        <V76F07>
      * Cheque Type.                                                    <V76F07>
           MOVE SPACES                 TO CHKF-FIELD-ID.                <V76F07>
           MOVE 'SR289-ZCHQTYP       ' TO CHKF-FIELD-ID.                <V76F07>
           CALL 'CHKFSUF'          USING CHKF-CHKF-REC                  <V76F07>
                                          SR289-ZCHQTYP                 <V76F07>
                                          SR289-ZCHQTYP-ERR             <V76F07>
                                          SR289-ZCHQTYP-OUT(PR).        <V76F07>
                                                                        <V76F07>
           IF SR289-ZCHQTYP-ERR          NOT = SPACES                   <V76F07>
              MOVE 'Y'                 TO WSSP-EDTERROR                 <V76F07>
              GO TO 2670-UPDATE-ERROR-INDICATORS                        <V76F07>
           END-IF.                                                      <V76F07>

      * Check consistency between Payment Type ,credit card details
      * and bank details.
           IF SR289-PAYTYPE        NOT = SPACES
              PERFORM 2610-CHECK-DETAILS-NEEDED
           END-IF.
                                                                        <V76F12>
      * Check Deposit date.                                             <V76F12>
           PERFORM 2660-CHECK-DEPOSIT-DATE.                             <V76F12>
           IF SR289-DEPDATE-ERR     NOT = SPACES                        <V76F12>
              GO TO 2670-UPDATE-ERROR-INDICATORS                        <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
      *
      * Validate defaulted original Currency in the event of
      * inconsistency between T3688 and T3629.
      *
           MOVE SPACES                 TO ITEM-DATA-AREA.
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.
           MOVE T3629                  TO ITEM-ITEMTABL.
           MOVE SR289-ORIGCURR         TO ITEM-ITEMITEM.
           PERFORM B1000-READR-ITEM.

           IF ITEM-STATUZ              = MRNF
              MOVE F982                TO SR289-ORIGCURR-ERR
           END-IF.

           IF WSAA-TRANDATEX       NOT = SR289-TRANDATEX AND
              SR289-SCRATE             = ZEROES
              PERFORM 2650-GET-NEW-RATE
              GO TO 2640-COMPUTE-AMOUNTS
           END-IF.
      *
      * If the Rate has been blanked out, reset it to default Rate
      * (unless it can be calculated).
      *
           IF SR289-SCRATE             = ZEROES AND
             (SR289-DOCACCTAMT         = ZEROES OR
              SR289-DOCORIGAMT         = ZEROES)
              PERFORM 2650-GET-NEW-RATE
              GO TO 2640-COMPUTE-AMOUNTS
           END-IF.
      *
      * If the Rate has been modified, check that it is in the right
      * range.
      *
           IF SR289-SCRATE             = ZEROES
              GO TO 2640-COMPUTE-AMOUNTS
           END-IF.

           MOVE SPACES                 TO CKCU-CHKCURR-REC.
           MOVE 'RATECHEK'             TO CKCU-FUNCTION.
           MOVE SR289-SCRATE           TO CKCU-RATE.
           MOVE SR289-ORIGCURR         TO CKCU-CURRCODE.
           MOVE WSSP-COMPANY           TO CKCU-COMPANY.
           MOVE SR289-TRANDATEX        TO CKCU-DATE.

           CALL 'CHKCURR'           USING CKCU-CHKCURR-REC.

           IF CKCU-STATUZ              = BOMB
              MOVE CKCU-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.

           IF CKCU-STATUZ              NOT = O-K
              MOVE CKCU-STATUZ         TO SR289-SCRATE-ERR
              GO TO 2670-UPDATE-ERROR-INDICATORS
           END-IF.
      *                                                                 <V76F06>
      * Perform rounding check for the input amount as well , to ensure <V76F06>
      * it follow the curreny rounding rule that set up by company:     <V76F06>
      *                                                                 <V76F06>
           IF SR289-DOCORIGAMT     NOT = ZEROES                         <V76F06>
              MOVE SR289-DOCORIGAMT    TO ZRDP-AMOUNT-IN                <V76F06>
              MOVE SR289-ORIGCURR      TO ZRDP-CURRENCY                 <V76F06>
              PERFORM 8000-CALL-ROUNDING                                <V76F06>
              IF SR289-DOCORIGAMT  NOT = ZRDP-AMOUNT-OUT                <V76F06>
                 MOVE RFIK             TO SR289-DOCORIGAMT-ERR          <V76F06>
           END-IF.                                                      <V76F06>
      *
       2640-COMPUTE-AMOUNTS.
      *
      * Amount, Rate, GL Amount: compute the third one with the
      * other two.
      *
           IF SR289-SCRATE             NOT = ZEROES AND
              SR289-DOCORIGAMT         NOT = ZEROES
              COMPUTE SR289-DOCACCTAMT ROUNDED =
                      SR289-DOCORIGAMT * SR289-SCRATE
           END-IF.
      *
           IF SR289-DOCORIGAMT             = ZEROES AND
              SR289-SCRATE             NOT = ZEROES AND
              SR289-DOCACCTAMT         NOT = ZEROES
              COMPUTE SR289-DOCORIGAMT ROUNDED =
                      SR289-DOCACCTAMT / SR289-SCRATE
           END-IF.
      *
           IF SR289-SCRATE                 = ZEROES AND
              SR289-DOCACCTAMT         NOT = ZEROES AND
              SR289-DOCORIGAMT         NOT = ZEROES
              COMPUTE SR289-SCRATE ROUNDED =
                      SR289-DOCACCTAMT  / SR289-DOCORIGAMT
           END-IF.
      *
      * For a Payment Type of 'journal', both amounts must be equal
      * to zero (i.e all dissections balance to zero). Otherwise,
      * one of the two amount fields must be entered.
      *
           IF CSTP-JNL
              IF SR289-DOCORIGAMT   NOT = ZEROES OR
                 SR289-DOCACCTAMT   NOT = ZEROES
                 MOVE E165             TO SR289-PAYTYPE-ERR
                                          SR289-DOCORIGAMT-ERR
                                          SR289-DOCACCTAMT-ERR
              END-IF
           ELSE
              IF SR289-DOCORIGAMT       = ZEROES AND
                 SR289-DOCACCTAMT       = ZEROES AND
                 SCRN-SUBFILE-RRN       = 1
                 MOVE H933             TO SR289-DOCORIGAMT-ERR
                                          SR289-DOCACCTAMT-ERR
              END-IF
              IF SR289-DOCORIGAMT      = ZEROES AND
                 SR289-DOCACCTAMT      = ZEROES AND
                 SR289-PAYTYPE     NOT = SPACES
                 MOVE H933             TO SR289-DOCORIGAMT-ERR
                                          SR289-DOCACCTAMT-ERR
              END-IF
              IF SR289-DOCORIGAMT  NOT = ZEROES AND
                 SR289-DOCACCTAMT  NOT = ZEROES AND
                 SR289-PAYTYPE         = SPACES
                 MOVE E190             TO SR289-PAYTYPE-ERR
              END-IF
           END-IF.
      * If action B, check the reference number must be entered.        <RC002>
           IF WSAA-SBMACTION            = 'B'                           <RC002>
               IF SR289-PAYTYPE     NOT = SPACE                         <RC002>
               AND SR289-INSREFNO        = SPACES                       <RC002>
                   MOVE E186           TO SR289-INSREFNO-ERR            <RC002>
               END-IF                                                   <RC002>
           END-IF.                                                      <RC002>
                                                                        <RC002>
      * If bank reference number entered, check duplicate:              <RC002>
      *    If creation - B                                              <RC002>
      *    If modify   - K                                              <RC002>
           IF SR289-INSREFNO       NOT  = SPACES                        <RC002>
               MOVE SPACES             TO RBNK-PARAMS                   <RC002>
               MOVE PRFX-CASH          TO RBNK-RDOCPFX                  <RC002>
               MOVE WSSP-COMPANY       TO RBNK-RDOCCOY                  <RC002>
               MOVE SR289-INSREFNO     TO RBNK-INSREFNO                 <RC002>
               MOVE BEGN               TO RBNK-FUNCTION                 <RC002>
               MOVE RBNKREC            TO RBNK-FORMAT                   <RC002>
                                                                        <RC002>
               CALL 'RBNKIO'        USING RBNK-PARAMS                   <RC002>
                                                                        <RC002>
               IF RBNK-STATUZ       NOT = O-K AND ENDP                  <RC002>
                   MOVE RBNKCHQ-PARAMS TO SYSR-PARAMS                   <RC002>
                   PERFORM 600-FATAL-ERROR                              <RC002>
               END-IF                                                   <RC002>
                                                                        <RC002>
               IF RBNK-STATUZ           = O-K                           <RC002>
               AND RBNK-RDOCPFX         = PRFX-CASH                     <RC002>
               AND RBNK-RDOCCOY         = WSSP-COMPANY                  <RC002>
               AND RBNK-INSREFNO        = SR289-INSREFNO                <RC002>
               AND RBNK-RDOCNUM     NOT = SR289-RECEIPT                 <RC002>
                   MOVE E048           TO SR289-INSREFNO                <RC002>
               END-IF                                                   <RC002>
           END-IF.                                                      <RC002>
      *
       2645-ACCUMULATE-TOTALS.
      *
           IF SR289-ACCTCCY        NOT = SR289-ORIGCURR                 <V76F06>
              MOVE SR289-DOCACCTAMT    TO ZRDP-AMOUNT-IN                <V76F06>
              MOVE T3629-LEDGCURR      TO ZRDP-CURRENCY                 <V76F06>
              PERFORM 8000-CALL-ROUNDING                                <V76F06>
              MOVE ZRDP-AMOUNT-OUT     TO SR289-DOCACCTAMT.             <V76F06>
                                                                        <V76F06>
           IF SBM-PARTIAL-CANCEL
              IF SR289-SELECT       NOT = SPACES
                 ADD SR289-DOCORIGAMT  TO WSAA-TOT-DOCORIGAMT
                 ADD SR289-DOCACCTAMT  TO WSAA-TOT-DOCACCTAMT
              END-IF
           ELSE
              ADD SR289-DOCORIGAMT     TO WSAA-TOT-DOCORIGAMT
              ADD SR289-DOCACCTAMT     TO WSAA-TOT-DOCACCTAMT
           END-IF.
      *
           IF SCRN-SUBFILE-RRN          = 1
              MOVE SR289-PAYTYPE       TO WSAA-DOCTYPE
              MOVE SR289-CHEQNO        TO WSAA-CHEQNO
              MOVE SR289-ORIGCURR      TO WSAA-ORIGCCY
              MOVE SR289-SCRATE        TO WSAA-SCRATE
              MOVE SR289-ACCTCCY       TO WSAA-ACCTCCY
           END-IF.
      *
       2670-UPDATE-ERROR-INDICATORS.
      *
           IF SR289-ERROR-SUBFILE      NOT = SPACES
              MOVE 'Y'                 TO WSSP-EDTERROR
           END-IF.

           MOVE SUPD                   TO SCRN-FUNCTION.
           PERFORM 9000-SCREEN-IO.
      *
       2680-READ-NEXT-MODIFIED-RECORD.
      *
           MOVE SRDN                   TO SCRN-FUNCTION.
           PERFORM 9000-SCREEN-IO.
      *
       2690-EXIT.
            EXIT.
      /
       2610-CHECK-DETAILS-NEEDED SECTION.
      ***********************************
       2610-BEGIN.
           EVALUATE CSTP-CASHTYPE
              WHEN CSTP-CASH-VAL
                 PERFORM 2615-BANK-DETAILS-NOT-REQUIRED
              WHEN CSTP-BKCD-VAL
                 PERFORM 2615-BANK-DETAILS-NOT-REQUIRED
              WHEN CSTP-JNL-VAL
                 PERFORM 2615-BANK-DETAILS-NOT-REQUIRED
              WHEN CSTP-CHEQ-VAL
                 PERFORM 2620-BANK-DETAILS-REQUIRED
              WHEN CSTP-CREDIT-CARD-VAL
                 PERFORM 2625-CARD-DETAILS-REQUIRED
           END-EVALUATE.
       2610-EXIT.
            EXIT.
      /
       2615-BANK-DETAILS-NOT-REQUIRED SECTION.
      ****************************************
       2615-BEGIN.
      *
           IF SR289-BANKKEY            NOT = SPACES
              MOVE E570                TO SR289-BANKKEY-ERR
           END-IF.
      *
           IF SR289-CHEQNO             NOT = SPACES
              MOVE E570                TO SR289-CHEQNO-ERR
           END-IF.
      *
           IF SR289-ZCHQTYP            NOT = SPACES
              MOVE E570                TO SR289-ZCHQTYP-ERR
           END-IF.
      *
           IF SR289-TCHQDATE           NOT = VRCM-MAX-DATE
              MOVE E570                TO SR289-TCHQDATE-ERR
           END-IF.
      *
           IF SR289-BANKDESC-01        NOT = SPACES
              MOVE E570                TO SR289-BANKDESC01-ERR
           END-IF.
      *
           IF SR289-BANKDESC-02        NOT = SPACES
              MOVE E570                TO SR289-BANKDESC02-ERR
           END-IF.
      *
           IF SR289-BANKDESC-03        NOT = SPACES
              MOVE E570                TO SR289-BANKDESC03-ERR
           END-IF.
      *
       2615-EXIT.
            EXIT.
      /
       2620-BANK-DETAILS-REQUIRED SECTION.
      ************************************
       2620-BEGIN.
      *
           IF SR289-CHEQNO             = SPACES
              MOVE H931                TO SR289-CHEQNO-ERR
           END-IF.
      *
           IF SR289-ZCHQTYP            = SPACES
              MOVE RPAO                TO SR289-ZCHQTYP-ERR
           END-IF.
      *
           IF SR289-TCHQDATE           = VRCM-MAX-DATE
              MOVE TL24                TO SR289-TCHQDATE-ERR
           END-IF.
      *
           IF SR289-BANKKEY            = SPACES
              MOVE F906                TO SR289-BANKKEY-ERR
              GO TO 2620-EXIT
           END-IF.
      *
           MOVE SPACES                 TO BABR-DATA-KEY.
           MOVE SR289-BANKKEY          TO BABR-BANKKEY.
           PERFORM B1050-READR-BABR.

           IF BABR-STATUZ              = MRNF
              MOVE F906                TO SR289-BANKKEY-ERR
           END-IF.

           IF BABR-STATUZ              = O-K
           AND SR289-BANKDESC-01       = SPACES
           AND SR289-BANKDESC-02       = SPACES
              MOVE BABR-BANKDESC(1:30)  TO SR289-BANKDESC-01
              MOVE BABR-BANKDESC(31:30) TO SR289-BANKDESC-02
           END-IF.
                                                                        <V76F12>
      *--Check if Receipt Status flag is set, validate duplicate        <V76F12>
      *--check number.  Duplicate is by BANKKEY + CHEQNO                <V76F12>
           PERFORM 2621-CHK-DUPCHEQ.                                    <V76F12>
      *
       2620-EXIT.
            EXIT.
      /                                                                 <V76F12>
       2621-CHK-DUPCHEQ SECTION.                                        <V76F12>
      **************************                                        <V76F12>
       2621-BEGIN.                                                      <V76F12>
           INITIALIZE                  RBNKCHQ-DATA-KEY.                <V76F12>
           MOVE PRFX-CASH              TO RBNKCHQ-RDOCPFX.              <V76F12>
           MOVE WSSP-COMPANY           TO RBNKCHQ-RDOCCOY.              <V76F12>
           MOVE SR289-CHEQNO           TO RBNKCHQ-CHQNUM.               <V76F12>
           MOVE RBNKCHQREC             TO RBNKCHQ-FORMAT.               <V76F12>
           MOVE BEGN                   TO RBNKCHQ-FUNCTION.             <V76F12>
                                                                        <V76F12>
       2621-RBNKCHQ.                                                    <V76F12>
           CALL 'RBNKCHQIO'          USING RBNKCHQ-PARAMS.              <V76F12>
           IF RBNKCHQ-STATUZ         NOT = O-K                          <V76F12>
              IF RBNKCHQ-STATUZ      NOT = ENDP                         <V76F12>
                 MOVE RBNKCHQ-PARAMS    TO SYSR-PARAMS                  <V76F12>
                 PERFORM 600-FATAL-ERROR                                <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
       2621-CHECK-STATUZ.                                               <V76F12>
           IF RBNKCHQ-STATUZ            = ENDP                          <V76F12>
           OR RBNKCHQ-RDOCCOY       NOT = WSSP-COMPANY                  <V76F12>
           OR RBNKCHQ-CHQNUM        NOT = SR289-CHEQNO                  <V76F12>
              GO TO 2621-EXIT.                                          <V76F12>
                                                                        <V76F12>
           IF  RBNKCHQ-STATUZ           = O-K                           <V76F12>
           AND RBNKCHQ-RDOCCOY          = WSSP-COMPANY                  <V76F12>
           AND RBNKCHQ-CHQNUM           = SR289-CHEQNO                  <V76F12>
           AND RBNKCHQ-BANKKEY          = SR289-BANKKEY                 <V76F12>
           AND RBNKCHQ-RDOCNUM      NOT = SR289-RECEIPT                 <V76F12>
              MOVE E267                TO SR289-CHEQNO-ERR              <V76F12>
              GO TO 2621-EXIT.                                          <V76F12>
                                                                        <V76F12>
           MOVE NEXTR                  TO RBNKCHQ-FUNCTION.             <V76F12>
           GO TO 2621-RBNKCHQ.                                          <V76F12>
                                                                        <V76F12>
       2621-EXIT.                                                       <V76F12>
           EXIT.                                                        <V76F12>
      /
       2625-CARD-DETAILS-REQUIRED SECTION.
      ************************************
       2625-BEGIN.
      * Cheque number not required
      **** IF SR289-CHEQNO         NOT = SPACES                         <V6F108>
      ****    MOVE F857                TO SR289-CHEQNO-ERR              <V6F108>
      **** END-IF.                                                      <V6F108>
                                                                        <V76F13>
      *-- Credit card details is now handle in pop-up screen SR25N      <V76F13>
      *-- pop-up screen.  It is mandatory to visit the credit card      <V76F13>
      *-- details screen and checking of information for credit card    <V76F13>
      *-- in this screen is not applicable anymore.                     <V76F13>
           IF SR289-CRCARDMOD       NOT = SR289-CHGFLAG                 <V76F13>
              MOVE 'Y'                 TO SR289-INDIC                   <V76F13>
           ELSE                                                         <V76F13>
              MOVE SPACES              TO SR289-INDIC                   <V76F13>
           END-IF.                                                      <V76F13>
           IF SR289-LXOPTIND           = SPACES                         <V76F13>
           AND SBM-CREATION                                             <V76F13>
              MOVE 'Y'                 TO SR289-INDIC                   <V76F13>
              MOVE 'X'                 TO SR289-LXOPTIND                <V76F13>
           END-IF.                                                      <V76F13>
           IF SR289-CRCARDMOD          = SPACES                         <V76F13>
           AND SBM-CREATION                                             <V76F13>
              MOVE E186                TO SR289-CRCARDMOD-ERR           <V76F13>
              MOVE 'Y'                 TO SR289-INDIC                   <V76F13>
           END-IF.                                                      <V76F13>
                                                                        <V76F13>
      *--Bank details is not require anymore.                           <V76F13>
           PERFORM 2615-BANK-DETAILS-NOT-REQUIRED                       <V76F13>
                                                                        <V76F13>
      *--Skip the below condition.                                      <V76F13>
           GO TO 2625-EXIT                                              <V76F13>

      * Used for credit card type
           IF SR289-ZCHQTYP            = SPACES
              MOVE RPAO                TO SR289-ZCHQTYP-ERR
           END-IF.

      * Used for credit card expiry date
           IF SR289-TCHQDATE           = VRCM-MAX-DATE
              MOVE RF29                TO SR289-TCHQDATE-ERR
           END-IF.

      * Used for issuing bank
           IF SR289-BANKKEY            = SPACES
              MOVE F022                TO SR289-BANKKEY-ERR
              GO TO 2625-EXIT
           END-IF.
      *
           MOVE SPACES                 TO BABR-DATA-KEY.
           MOVE SR289-BANKKEY          TO BABR-BANKKEY.
           PERFORM B1050-READR-BABR.

           IF BABR-STATUZ              = MRNF
              MOVE F022                TO SR289-BANKKEY-ERR
           END-IF.

           IF BABR-STATUZ              = O-K
           AND SR289-BANKDESC-01       = SPACES
              MOVE BABR-BANKDESC(1:30)  TO SR289-BANKDESC-01
           END-IF.

      * Used for credit card number
      **** IF SR289-BANKDESC-02        = SPACES                         <V6F108>
      ****    MOVE RF30                TO SR289-BANKDESC02-ERR          <V6F108>
           IF SR289-BANKDESC-03        = SPACES                         <V6F108>
              MOVE RF30                TO SR289-BANKDESC03-ERR          <V6F108>
           END-IF.
      *
       2625-EXIT.
            EXIT.
      /
       2640-VALIDATE-PAYTYPE SECTION.
      *******************************
       2640-START.

           MOVE SR289-PAYTYPE         TO CSTP-CASHTYPE.
           PERFORM 2642-VALIDATE-COMBINATION.

       2640-EXIT.
            EXIT.
      /
       2642-VALIDATE-COMBINATION SECTION.
      ***********************************
       2642-BEGIN.
      **** MOVE 'N'                    TO WSAA-RTYP-BKCD                <R2-284>
      ****                                WSAA-RTYP-JNL                 <R2-284>
      ****                                WSAA-RTYP-DSH-CHQ             <R2-284>
      ****                                WSAA-RTYP-BANK-TAPE           <R2-284>
      ****                                WSAA-RTYP-CHEQ-CANCEL         <R2-284>
      ****                                WSAA-RTYP-CREDIT-CARD.        <R2-284>
           EVALUATE CSTP-CASHTYPE
             WHEN CSTP-CASH-VAL
                MOVE 'Y'               TO WSAA-RTYP-CASH
             WHEN CSTP-CHEQ-VAL
                MOVE 'Y'               TO WSAA-RTYP-CHEQ
             WHEN CSTP-BKCD-VAL
                MOVE 'Y'               TO WSAA-RTYP-BKCD
             WHEN CSTP-JNL-VAL
                MOVE 'Y'               TO WSAA-RTYP-JNL
             WHEN CSTP-DSH-CHQ-VAL
                MOVE 'Y'               TO WSAA-RTYP-DSH-CHQ
             WHEN CSTP-BANK-TAPE-VAL
                MOVE 'Y'               TO WSAA-RTYP-BANK-TAPE
             WHEN CSTP-CHEQ-CANCEL-VAL
                MOVE 'Y'               TO WSAA-RTYP-CHEQ-CANCEL
             WHEN CSTP-CREDIT-CARD-VAL
                MOVE 'Y'               TO WSAA-RTYP-CREDIT-CARD
           END-EVALUATE.

           IF RTYP-CASH OR RTYP-CHEQ
              IF RTYP-BKCD         OR
                 RTYP-JNL          OR
                 RTYP-DSH-CHQ      OR
                 RTYP-BANK-TAPE    OR
                 RTYP-CHEQ-CANCEL                                       <R2-284>
      ****       RTYP-CHEQ-CANCEL  OR                                   <R2-284>
      ****       RTYP-CREDIT-CARD                                       <R2-284>
                 MOVE E456             TO SR289-PAYTYPE-ERR
                 GO TO 2642-EXIT
              END-IF
           END-IF.

           IF RTYP-BKCD
              IF RTYP-CASH         OR
                 RTYP-CHEQ         OR
                 RTYP-JNL          OR
                 RTYP-DSH-CHQ      OR
                 RTYP-BANK-TAPE    OR
                 RTYP-CHEQ-CANCEL  OR
                 RTYP-CREDIT-CARD
                 MOVE E456             TO SR289-PAYTYPE-ERR
                 GO TO 2642-EXIT
              END-IF
           END-IF.

           IF RTYP-JNL
              IF RTYP-CASH         OR
                 RTYP-CHEQ         OR
                 RTYP-BKCD         OR
                 RTYP-DSH-CHQ      OR
                 RTYP-BANK-TAPE    OR
                 RTYP-CHEQ-CANCEL  OR
                 RTYP-CREDIT-CARD
                 MOVE E456             TO SR289-PAYTYPE-ERR
                 GO TO 2642-EXIT
              END-IF
           END-IF.

           IF RTYP-DSH-CHQ
              IF RTYP-CASH         OR
                 RTYP-CHEQ         OR
                 RTYP-BKCD         OR
                 RTYP-JNL          OR
                 RTYP-BANK-TAPE    OR
                 RTYP-CHEQ-CANCEL  OR
                 RTYP-CREDIT-CARD
                 MOVE E456             TO SR289-PAYTYPE-ERR
                 GO TO 2642-EXIT
              END-IF
           END-IF.

           IF RTYP-BANK-TAPE
              IF RTYP-CASH         OR
                 RTYP-CHEQ         OR
                 RTYP-BKCD         OR
                 RTYP-JNL          OR
                 RTYP-DSH-CHQ      OR
                 RTYP-CHEQ-CANCEL  OR
                 RTYP-CREDIT-CARD
                 MOVE E456             TO SR289-PAYTYPE-ERR
                 GO TO 2642-EXIT
              END-IF
           END-IF.

      *--Disallow Receipt Entry for payment type 8 - Cancellation.      <V76F12>
           IF RTYP-CHEQ-CANCEL
              IF RTYP-CASH         OR
                 RTYP-CHEQ         OR
                 RTYP-BKCD         OR
                 RTYP-JNL          OR
                 RTYP-DSH-CHQ      OR
                 RTYP-BANK-TAPE    OR
                 RTYP-CHEQ-CANCEL  OR                                   <V76F12>
                 RTYP-CREDIT-CARD
                 IF RTYP-CHEQ-CANCEL                                    <V76F12>
                    MOVE RFKV          TO SR289-PAYTYPE-ERR             <V76F12>
                 ELSE                                                   <V76F12>
                 MOVE E456             TO SR289-PAYTYPE-ERR
                 END-IF                                                 <V76F12>
                 GO TO 2642-EXIT
              END-IF
           END-IF.

      *--Credit card should only have 1 dissection and cannot be        <V76F13>
      *--combine with other payment type.                               <V76F13>
           IF RTYP-CREDIT-CARD
      ****    IF RTYP-CASH         OR                                   <R2-284>
      ****       RTYP-CHEQ         OR                                   <R2-284>
      ****       RTYP-BKCD         OR                                   <R2-284>
              IF RTYP-BKCD         OR                                   <R2-284>
                 RTYP-JNL          OR
                 RTYP-DSH-CHQ      OR
                 RTYP-BANK-TAPE    OR
                 RTYP-CASH         OR                                   <V76F13>
                 RTYP-CHEQ         OR                                   <V76F13>
                 RTYP-BKCD         OR                                   <V76F13>
                 RTYP-CHEQ-CANCEL
                 MOVE E456             TO SR289-PAYTYPE-ERR
                 GO TO 2642-EXIT
              END-IF
           END-IF.
       2642-EXIT.
            EXIT.
      /
       2650-GET-NEW-RATE SECTION.
      ***************************
       2650-BEGIN.
      *
           MOVE SPACES                 TO CKCU-CHKCURR-REC.
           MOVE 'RATE'                 TO CKCU-FUNCTION.
           MOVE SR289-SCRATE           TO CKCU-RATE.
           MOVE SR289-ORIGCURR         TO CKCU-CURRCODE.
           MOVE WSSP-COMPANY           TO CKCU-COMPANY.
           MOVE SR289-TRANDATEX        TO CKCU-DATE.
           CALL 'CHKCURR'           USING CKCU-CHKCURR-REC.
      *
           IF CKCU-STATUZ               = BOMB
              MOVE CKCU-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.
      *
           IF CKCU-STATUZ           NOT = O-K
              MOVE CKCU-STATUZ         TO SR289-SCRATE-ERR
           ELSE
              MOVE CKCU-RATE           TO SR289-SCRATE
                                          WSSP-SCRATE
           END-IF.
      *
       2650-EXIT.
            EXIT.
      /                                                                 <V76F12>
       2660-CHECK-DEPOSIT-DATE SECTION.                                 <V76F12>
      *********************************                                 <V76F12>
       2660-CHK-DEPDATE.                                                <V76F12>
           IF SR289-PAYTYPE             = SPACES                        <V76F12>
              GO TO 2660-EXIT.                                          <V76F12>
                                                                        <V76F12>
      *-- Validate only if Payment type is not spaces and receipt       <V76F12>
      *-- enhancement is applicate in TR29S setup                       <V76F12>
           IF NOT RCTSTATFLG                                            <V76F12>
              MOVE 'Y'                 TO SR289-DEPDATE-OUT(PR)         <V76F12>
              GO TO 2660-EXIT.                                          <V76F12>
                                                                        <V76F12>
           MOVE 'N'                    TO SR289-DEPDATE-OUT(PR).        <V76F12>
                                                                        <V76F12>
      *-- IF payment type is not cheque Deposit Date =                  <V76F12>
      *-- Business Date and cannot be changed                           <V76F12>
           IF SR289-PAYTYPE         NOT = CSTP-CHEQ-VAL                 <V76F12>
              IF SBM-CREATION                                           <V76F12>
              OR WSAA-RBNK-POSTDTEFLG   = 'Y'                           <V76F12>
                 MOVE WSAA-TODAY       TO SR289-DEPDATE                 <V76F12>
                 MOVE SR289-DEPDATE    TO WSSP-CURRFROM                 <RC002>
              END-IF                                                    <V76F12>
              MOVE 'Y'                 TO SR289-DEPDATE-OUT(PR)         <V76F12>
              GO TO 2660-EXIT                                           <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
      *-- IF payment type is cheque, Deposit Date = Business Date or    <V76F12>
      *-- Cheque Date which ever is higher.                             <V76F12>
           IF SR289-DEPDATE             = VRCM-MAX-DATE                 <V76F12>
           OR SR289-DEPDATE             = ZEROES                        <V76F12>
              IF  SR289-TCHQDATE    NOT = VRCM-MAX-DATE                 <V76F12>
              AND SR289-TCHQDATE        > WSAA-TODAY                    <V76F12>
                  MOVE SR289-TCHQDATE  TO SR289-DEPDATE                 <V76F12>
              ELSE                                                      <V76F12>
                  MOVE WSAA-TODAY      TO SR289-DEPDATE                 <V76F12>
              END-IF                                                    <V76F12>
              MOVE SR289-DEPDATE       TO WSSP-CURRFROM                 <RC002>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
      *-- IF payment type is cheque, Deposit Date = Business Date or    <V76F12>
      *-- Cheque Date which ever is higher.                             <V76F12>
           IF Y-POSTDATED                                               <V76F12>
              IF SR289-DEPDATE          < SR289-TCHQDATE                <V76F12>
                 MOVE RFIQ             TO SR289-DEPDATE-ERR             <V76F12>
              END-IF                                                    <V76F12>
           ELSE                                                         <V76F12>
              IF SR289-DEPDATE          < WSAA-TODAY                    <V76F12>
                 MOVE RFIP             TO SR289-DEPDATE-ERR             <V76F12>
              END-IF                                                    <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
       2660-EXIT.                                                       <V76F12>
           EXIT.                                                        <V76F12>
      /                                                                 <V76F12>
       2671-VALIDATE-RCPTSTAT SECTION.                                  <V76F12>
      ********************************                                  <V76F12>
       2671-BEGIN.                                                      <V76F12>
      *--Get Receipt Status from T3676.  For each payment type,         <V76F12>
      *--there is corresponding receipt status setup according to       <V76F12>
      *--transaction code.                                              <V76F12>
      *--The receipt Status setup in T3676 only applicable for receipt  <V76F12>
      *--creation.                                                      <V76F12>
      *--This enhancement is only applicable if the setup is ON in      <V76F12>
      *--TR29S in FSU level.                                            <V76F12>
           IF NOT RCTSTATFLG                                            <V76F12>
           OR WSAA-RTRN-EXIST           = 'Y'                           <V76F12>
              IF SR289-PAYTYPE      NOT = SPACES                        <V76F12>
                 MOVE 'Y'              TO SR289-POSTFLG                 <V76F12>
              END-IF                                                    <V76F12>
              GO TO 2671-EXIT                                           <V76F12>
           END-IF                                                       <V76F12>
                                                                        <V76F12>
           INITIALIZE                     ITEM-DATA-KEY.                <V76F12>
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.                 <V76F12>
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.                 <V76F12>
           MOVE T3676                  TO ITEM-ITEMTABL.                <V76F12>
           MOVE SR289-PAYTYPE          TO ITEM-ITEMITEM.                <V76F12>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <V76F12>
           MOVE READR                  TO ITEM-FUNCTION.                <V76F12>
                                                                        <V76F12>
           CALL 'ITEMIO'            USING ITEM-PARAMS.                  <V76F12>
           IF ITEM-STATUZ           NOT = O-K AND MRNF                  <V76F12>
              MOVE ITEM-STATUZ         TO SYSR-STATUZ                   <V76F12>
              MOVE ITEM-PARAMS         TO SYSR-PARAMS                   <V76F12>
              PERFORM 600-FATAL-ERROR                                   <V76F12>
           END-IF.                                                      <V76F12>
           IF ITEM-STATUZ               = O-K                           <V76F12>
              MOVE ITEM-GENAREA        TO T3676-T3676-REC               <V76F12>
           ELSE                                                         <V76F12>
              MOVE SPACES              TO T3676-T3676-REC               <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
           MOVE SPACES                 TO SR289-RCPTSTAT.               <V76F12>
           PERFORM VARYING IX FROM 1 BY 1                               <V76F12>
              UNTIL   IX > WSAA-MAX-T3676-TRANSCD                       <V76F12>
              OR SR289-RCPTSTAT     NOT = SPACES                        <V76F12>
                                                                        <V76F12>
              IF WSKY-BATC-BATCTRCDE    = T3676-TRANSCD(IX)             <V76F12>
                 MOVE T3676-RCPTST(IX) TO SR289-RCPTSTAT                <V76F12>
              END-IF                                                    <V76F12>
           END-PERFORM.                                                 <V76F12>
       2671-EXIT.                                                       <V76F12>
           EXIT.                                                        <V76F12>
      /                                                                 <V76F12>
       2700-VALIDATE-POSTDATED SECTION.                                 <V76F12>
      *********************************                                 <V76F12>
       2710-VALIDATE.                                                   <V76F12>
      *-- If Receipt Status flag is set to 'Y' in TR29S, postdated is   <V76F12>
      *-- feature is applicable.                                        <V76F12>
      *-- Receipt is consider Postdated if:                             <V76F12>
      *--   a) One header Dissection                                    <V76F12>
      *--   b) Payment Mode is check                                    <V76F12>
      *--   c) Check Date is greater than today                         <V76F12>
      *--                                                               <V76F12>
      *--  Note: For postdated check, saving of the receipt dissection  <V76F12>
      *--        is only in mirror file.                                <V76F12>
      *--        During modification, postdated check can be converted  <V76F12>
      *--        to non postdated receipt if not yet printed.           <V76F12>
                                                                        <V76F12>
           IF WSAA-SFL-RECORD           = ZEROES                        <V76F12>
           OR WSSP-EDTERROR         NOT = O-K                           <V76F12>
           OR NOT RCTSTATFLG                                            <V76F12>
              GO 2790-EXIT.                                             <V76F12>
                                                                        <V76F12>
           MOVE SSTRT                  TO SCRN-FUNCTION.                <V76F12>
           PERFORM 9000-SCREEN-IO.                                      <V76F12>
                                                                        <V76F12>
           MOVE SR289-PAYTYPE          TO CSTP-CASHTYPE.                <V76F12>
                                                                        <V76F12>
           IF  PRINTED                                                  <V76F12>
           AND Y-POSTDATED                                              <V76F12>
           AND SR289-TCHQDATE          <= WSAA-TODAY                    <V76F12>
               MOVE RFI1               TO SR289-TCHQDATE-ERR            <V76F12>
               MOVE 'Y'                TO WSSP-EDTERROR                 <V76F12>
               GO TO 2770-POSTDATED-UPDATED                             <V76F12>
           END-IF                                                       <V76F12>
                                                                        <V76F12>
           IF  WSAA-SFL-RECORD          = 1                             <V76F12>
           AND SR289-TCHQDATE           > WSAA-TODAY                    <V76F12>
           AND SR289-TCHQDATE       NOT = VRCM-MAX-DATE                 <V76F12>
           AND WSAA-RTRN-EXIST      NOT = 'Y'                           <V76F12>
           AND CSTP-CHEQ                                                <V76F12>
           AND NOT P-POSTDATED                                          <V76F12>
               MOVE 'Y'                TO WSAA-POSTDTEFLG               <V76F12>
               GO TO 2780-PRE-EXIT                                      <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
           IF NOT P-POSTDATED                                           <V76F12>
              MOVE SPACES              TO WSAA-POSTDTEFLG               <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
           PERFORM 2710-VALIDATE-CHECK UNTIL SCRN-STATUZ = ENDP.        <V76F12>
                                                                        <V76F12>
           GO TO 2780-PRE-EXIT.                                         <V76F12>
                                                                        <V76F12>
       2770-POSTDATED-UPDATED.                                          <V76F12>
           MOVE SUPD                   TO SCRN-FUNCTION.                <V76F12>
           PERFORM 9000-SCREEN-IO.                                      <V76F12>
                                                                        <V76F12>
       2780-PRE-EXIT.                                                   <V76F12>
           IF NOT-POSTDATED                                             <V76F12>
              MOVE 'Y'                 TO SR289-POSTDTEFLG-OUT(ND)      <V76F12>
           ELSE                                                         <V76F12>
              MOVE 'N'                 TO SR289-POSTDTEFLG-OUT(ND)      <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
           IF NOT-POSTDATED                                             <V76F12>
              MOVE SPACES              TO SR289-POSTDTEFLG              <V76F12>
                                          WSSP-CHEQ-PRESFLAG            <V76F12>
           END-IF.                                                      <V76F12>
           IF Y-POSTDATED                                               <V76F12>
              MOVE 'Y'                 TO SR289-POSTDTEFLG              <V76F12>
                                          WSSP-CHEQ-PRESFLAG            <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
           IF WSAA-PRV-POSTFG       NOT = WSAA-POSTDTEFLG               <V76F12>
              MOVE WSAA-POSTDTEFLG     TO WSAA-PRV-POSTFG               <V76F12>
              MOVE 'Y'                 TO WSSP-EDTERROR                 <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
       2790-EXIT.                                                       <V76F12>
           EXIT.                                                        <V76F12>
      /                                                                 <V76F12>
       2710-VALIDATE-CHECK SECTION.                                     <V76F12>
      *****************************                                     <V76F12>
       2711-BEGIN.                                                      <V76F12>
           MOVE SR289-PAYTYPE          TO CSTP-CASHTYPE.                <V76F12>
                                                                        <V76F12>
           IF NOT CSTP-CHEQ                                             <V76F12>
              GO 2718-READ-NEXT-SFL.                                    <V76F12>
                                                                        <V76F12>
           IF SR289-TCHQDATE            > WSAA-TODAY                    <V76F12>
           AND SR289-TCHQDATE       NOT = VRCM-MAX-DATE                 <V76F12>
              IF SBM-MODIFY                                             <V76F12>
                 MOVE F073             TO SR289-TCHQDATE-ERR            <V76F12>
              ELSE                                                      <V76F12>
                 MOVE RFHM             TO SR289-TCHQDATE-ERR            <V76F12>
              END-IF                                                    <V76F12>
              MOVE 'Y'                 TO WSSP-EDTERROR                 <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
           MOVE SUPD                   TO SCRN-FUNCTION.                <V76F12>
           PERFORM 9000-SCREEN-IO.                                      <V76F12>
                                                                        <V76F12>
       2718-READ-NEXT-SFL.                                              <V76F12>
           MOVE SRDN                   TO SCRN-FUNCTION.                <V76F12>
           PERFORM 9000-SCREEN-IO.                                      <V76F12>
                                                                        <V76F12>
       2719-EXIT.                                                       <V76F12>
           EXIT.                                                        <V76F12>
      /                                                                 <V76F13>
       2700A-CHECK-ONE-DISS SECTION.                                    <V76F13>
      ******************************                                    <V76F13>
       2700A-BEGIN.                                                     <V76F13>
           MOVE SSTRT                  TO SCRN-FUNCTION.                <V76F13>
           PERFORM 9000-SCREEN-IO.                                      <V76F13>
                                                                        <V76F13>
      *--If credit card payment, only 1 dissection is allowed to enter  <V76F13>
           IF  SR289-PAYTYPE            = CSTP-CREDIT-CARD-VAL          <V76F13>
           AND WSAA-SFL-RECORD          > 1                             <V76F13>
               MOVE R012               TO SCRN-ERROR-CODE               <V76F13>
               MOVE 'Y'                TO WSSP-EDTERROR                 <V76F13>
           END-IF.                                                      <V76F13>
                                                                        <V76F13>
       2700A-EXIT.                                                      <V76F13>
           EXIT.                                                        <V76F13>
      /                                                                 <V76F04>
       2800-CHECK-REMARKS SECTION.                                      <V76F04>
      ****************************                                      <V76F04>
       2810-BEGIN.                                                      <V76F04>
      *--Since we are validating for Function key, no fields is         <V76F04>
      *--available to process.                                          <V76F04>
      *--Setup dummy field in TR377 "REMARKS" in TR377 to validate      <V76F04>
      *--if remarks entry is mandatory.                                 <V76F04>
           IF WSAA-SCRN-STATUZ          = 'DIRY'                        <V76F04>
           OR WSSP-EDTERROR         NOT =  O-K                          <V76F04>
              GO TO 2890-EXIT.                                          <V76F04>
                                                                        <V76F04>
           MOVE 'CHECK'                TO CHKF-FUNCTION.                <V76F04>
           MOVE WSSP-COMPANY           TO CHKF-COMPANY.                 <V76F04>
           MOVE WSAA-PROG              TO CHKF-PROG.                    <V76F04>
                                                                        <V76F04>
           MOVE SPACES                 TO CHKF-FIELD-ID                 <V76F04>
                                          WSAA-REMARKS                  <V76F04>
                                          WSAA-REMARKS-ERR              <V76F04>
                                          WSAA-REMARKS-OUT.             <V76F04>
           MOVE 'SR289-REMARKS'        TO CHKF-FIELD-ID                 <V76F04>
           CALL 'CHKFSUF'           USING CHKF-CHKF-REC                 <V76F04>
                                          WSAA-REMARKS                  <V76F04>
                                          WSAA-REMARKS-ERR              <V76F04>
                                          WSAA-REMARKS-OUT.             <V76F04>
                                                                        <V76F04>
           IF WSAA-REMARKS-ERR      NOT = SPACES                        <V76F04>
              PERFORM 2810-VALIDATE-RMRK                                <V76F04>
           END-IF.                                                      <V76F04>
                                                                        <V76F04>
       2890-EXIT.                                                       <V76F04>
           EXIT.                                                        <V76F04>
      /                                                                 <V76F04>
       2810-VALIDATE-RMRK SECTION.                                      <V76F04>
      ****************************                                      <V76F04>
       2811-CHECK.                                                      <V76F04>
           MOVE SPACES                 TO RMRK-DATA-KEY.                <V76F04>
           MOVE WSSP-RDOCPFX           TO RMRK-RDOCPFX.                 <V76F04>
           MOVE WSSP-RDOCCOY           TO RMRK-RDOCCOY.                 <V76F04>
           MOVE SR289-RECEIPT          TO RMRK-RDOCNUM.                 <V76F04>
           MOVE 999                    TO RMRK-SEQNBR.                  <V76F04>
           MOVE RMRKREC                TO RMRK-FORMAT.                  <V76F04>
           MOVE BEGN                   TO RMRK-FUNCTION.                <V76F04>
                                                                        <V76F04>
           CALL 'RMRKIO'            USING RMRK-PARAMS.                  <V76F04>
           IF RMRK-STATUZ           NOT = O-K   AND ENDP                <V76F04>
              MOVE RMRK-PARAMS         TO SYSR-PARAMS                   <V76F04>
              PERFORM 600-FATAL-ERROR                                   <V76F04>
           END-IF.                                                      <V76F04>
                                                                        <V76F04>
           IF RMRK-STATUZ           NOT = O-K                           <V76F04>
           OR RMRK-RDOCPFX          NOT = WSSP-RDOCPFX                  <V76F04>
           OR RMRK-RDOCCOY          NOT = WSSP-RDOCCOY                  <V76F04>
           OR RMRK-RDOCNUM          NOT = SR289-RECEIPT                 <V76F04>
              MOVE WSAA-REMARKS-ERR    TO SCRN-ERROR-CODE               <V76F04>
              MOVE 'Y'                 TO WSSP-EDTERROR                 <V76F04>
           END-IF.                                                      <V76F04>
       2819-EXIT.                                                       <V76F04>
           EXIT.                                                        <V76F04>
      /                                                                 <V76F12>
       2900-VALIDATE-REASON SECTION.                                    <V76F12>
      ******************************                                    <V76F12>
       2900-BEGIN.                                                      <V76F12>
           MOVE SSTRT                  TO SCRN-FUNCTION.                <V76F12>
           PERFORM 9000-SCREEN-IO.                                      <V76F12>
           PERFORM UNTIL SCRN-STATUZ = ENDP                             <V76F12>
              IF SR289-CNRSNCD         = SPACES                         <V76F12>
                 MOVE E186             TO SR289-CNRSNCD-ERR             <V76F12>
                 MOVE 'Y'              TO WSSP-EDTERROR                 <V76F12>
              ELSE                                                      <V76F12>
                 PERFORM 2910-READ-TR29H                                <V76F12>
              END-IF                                                    <V76F12>
              MOVE SUPD                TO SCRN-FUNCTION                 <V76F12>
              PERFORM 9000-SCREEN-IO                                    <V76F12>
                                                                        <V76F12>
              MOVE SRDN                TO SCRN-FUNCTION                 <V76F12>
              PERFORM 9000-SCREEN-IO                                    <V76F12>
           END-PERFORM.                                                 <V76F12>
                                                                        <V76F12>
           MOVE 1                      TO SCRN-SUBFILE-RRN.             <V76F12>
       2900-EXIT.                                                       <V76F12>
           EXIT.                                                        <V76F12>
      /                                                                 <V76F12>
       2910-READ-TR29H SECTION.                                         <V76F12>
      *************************                                         <V76F12>
       2910-BEGIN.                                                      <V76F12>
           INITIALIZE                     ITEM-DATA-KEY.                <V76F12>
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.                 <V76F12>
           MOVE WSSP-FSUCO             TO ITEM-ITEMCOY.                 <V76F12>
           MOVE TR29H                  TO ITEM-ITEMTABL.                <V76F12>
           MOVE WSSP-LANGUAGE          TO WSAA-TR29H-LANG.              <V76F12>
           MOVE SR289-CNRSNCD          TO WSAA-TR29H-CODE.              <V76F12>
           MOVE WSAA-TR29H-ITEM        TO ITEM-ITEMITEM.                <V76F12>
                                                                        <V76F12>
           PERFORM B1000-READR-ITEM.                                    <V76F12>
           IF ITEM-STATUZ           NOT = O-K                           <V76F12>
              MOVE E031                TO SR289-CNRSNCD-ERR             <V76F12>
              MOVE 'Y'                 TO WSSP-EDTERROR                 <V76F12>
           END-IF.                                                      <V76F12>
       2910-EXIT.                                                       <V76F12>
           EXIT.                                                        <V76F12>
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
      *                                                                 <V76F04>
      * If returning from an optional selection skip this section.      <V76F04>
      *                                                                 <V76F04>
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'                    <V76F04>
           OR WSAA-SCRN-STATUZ                 = 'DIRY'                 <V76F04>
                GO TO 3090-EXIT                                         <V76F04>
           END-IF                                                       <V76F04>
           IF SBM-INQUIRY
              MOVE WSAA-SCRATE         TO WSSP-SCRATE                   <V72P02>
              MOVE SR289-TRANDATEX     TO WSSP-TRANDATE                 <FA4514>
              MOVE WSAA-CLNTKEY        TO WSSP-CLNTKEY                  <V76F07>
              GO TO 3090-EXIT
           END-IF.
                                                                        <PHLRMS>
           IF  WSAA-SBMACTION          = 'A'                            <PHLRMS>
           OR  ( WSAA-SBMACTION        = 'B'                            <PS008>
           AND SR289-RCPREPRTCD        NOT = SPACES                     <PS008>
           AND SR289-RCPREPRNT         IS NUMERIC )                     <PS008>
               PERFORM A2600-UPDATE-RCPY                                <PHLRMS>
           END-IF.                                                      <PHLRMS>

      * For first dissection header record in S2610
           MOVE WSAA-DOCTYPE           TO WSSP-DOCTYPE.
           MOVE SR289-RFCODE           TO WSSP-PTCODE.
           MOVE SR289-RFNUM            TO WSSP-PAYESEL.
           MOVE SR289-CASHNM           TO WSSP-PAYENME.
           MOVE WSAA-CHEQNO            TO WSSP-CHEQNUM.
           MOVE WSAA-TOT-DOCORIGAMT    TO WSSP-DOCORIGAMT.
           MOVE WSAA-ORIGCCY           TO WSSP-ORIGCCY.
           MOVE WSAA-SCRATE            TO WSSP-SCRATE.
           MOVE WSAA-TOT-DOCACCTAMT    TO WSSP-DOCACCTAMT.
           MOVE WSAA-ACCTCCY           TO WSSP-ACCTCCY.
           MOVE SR289-TRANDATEX        TO WSSP-TRANDATE.                <FA4514>

      * Other ...
           MOVE WSAA-CLNTKEY           TO WSSP-CLNTKEY.
           MOVE SCRN-SUBFILE-END       TO WSSP-SUBFILE-END.
           IF (WSAA-SBMACTION          = 'A' OR 'I' OR 'B' )            <PS008>
           AND (SR289-RCPREPRNT        NOT = SPACES)                    <PHE003>
           AND (SR289-RCPREPRTCD       NOT = SPACES)                    <PHE003>
      *                                                                 <PHLRMS>
      *--  Insert record of collection in RCOL file                     <PHLRMS>
      *                                                                 <PHLRMS>
               PERFORM 3100-INSERT-RCOLPND                              <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <V64F13>
        3020-KEEPS-RBNK.                                                <V64F13>
           INITIALIZE                     RBNK-DATA-AREA.               <V64F13>
           MOVE PRFX-CASH              TO RBNK-RDOCPFX.                 <V64F13>
           MOVE WSSP-COMPANY           TO RBNK-RDOCCOY.                 <V64F13>
           MOVE SR289-RECEIPT          TO RBNK-RDOCNUM.                 <V64F13>
           MOVE SR289-MARRYFLAG        TO RBNK-MARRYFLAG.               <V64F13>
           MOVE WSAA-RBNK-POSTDTEFLG   TO RBNK-POSTDTEFLG               <V76F12>
           MOVE RBNKREC                TO RBNK-FORMAT.                  <V64F13>
           MOVE KEEPS                  TO RBNK-FUNCTION.                <V64F13>
           CALL 'RBNKIO'            USING RBNK-PARAMS.                  <V64F13>
                                                                        <V64F13>
           IF  RBNK-STATUZ            NOT = O-K                         <V64F13>
               MOVE RBNK-PARAMS        TO SYSR-PARAMS                   <V64F13>
               PERFORM 600-FATAL-ERROR                                  <V64F13>
           END-IF.                                                      <V64F13>
      *
           IF AMLC-AMLIND               = 'Y'                           <V74F03>
              MOVE 'WRITR'             TO AMLC-FUNCTION                 <V74F03>
              CALL 'AMLCHK'         USING AMLC-VALID-REC                <V74F03>
              IF AMLC-STATUZ           NOT = O-K                        <V74F03>
                 MOVE AMLC-VALID-REC     TO SYSR-PARAMS                 <V74F03>
                 MOVE AMLC-STATUZ        TO SYSR-STATUZ                 <V74F03>
                 PERFORM 600-FATAL-ERROR                                <V74F03>
              END-IF                                                    <V74F03>
           END-IF.                                                      <V74F03>
      *                                                                 <V74F03>
       3090-EXIT.
            EXIT.
      /                                                                 <PHLRMS>
       3100-INSERT-RCOLPND SECTION.                                     <PHLRMS>
      *****************************                                     <PHLRMS>
      *                                                                 <PHLRMS>
       3110-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                     RCOLPND-PARAMS     .          <PHLRMS>
           MOVE WSAA-RECEIPT-PFX       TO RCOLPND-RDOCPFX    .          <PHLRMS>
           MOVE WSAA-RECEIPT-COY       TO RCOLPND-RDOCCOY    .          <PHLRMS>
           MOVE SR289-RECEIPT          TO RCOLPND-RDOCNUM    .          <PHLRMS>
                                                                        <PHLRMS>
           IF  SBM-CANCELLATION                                         <PHLRMS>
               MOVE WSSP-RDOCNUM       TO RCOLPND-RCPTREV               <PHLRMS>
           ELSE                                                         <PHLRMS>
               MOVE SPACES             TO RCOLPND-RCPTREV               <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           MOVE SR289-RCPREPRTCD       TO RCOLPND-RCTYPE     .          <PHLRMS>
           MOVE SR289-RCPREPRNT        TO RCOLPND-RCSERNUM   .          <PHLRMS>
           MOVE WSKY-BATC-BATCTRCDE    TO RCOLPND-TRANCDE    .          <PHLRMS>
           MOVE SR289-DENTTYP          TO RCOLPND-DENTTYP    .          <PHLRMS>
           MOVE SR289-ENTYNO           TO RCOLPND-ENTYNO     .          <PHLRMS>
           MOVE SR289-RFCODE           TO RCOLPND-RFCODE     .          <PHLRMS>
           MOVE SR289-RFNUM            TO RCOLPND-RFNUM      .          <PHLRMS>
           MOVE SR289-PAYTYPE          TO RCOLPND-PAYTYPE    .          <PHLRMS>
           MOVE SR289-DOCORIGAMT       TO RCOLPND-DOCORIGAMT .          <PHLRMS>
           MOVE SR289-TRANDATEX        TO RCOLPND-TRANDATEX  .          <PHLRMS>
           MOVE SR289-ORIGCURR         TO RCOLPND-ORIGCURR   .          <PHLRMS>
           MOVE SR289-SCRATE           TO RCOLPND-SCRATE     .          <PHLRMS>
           MOVE '3'                    TO RCOLPND-VALIDFLAG  .          <PHLRMS>
           MOVE RCOLPNDREC             TO RCOLPND-FORMAT     .          <PHLRMS>
           MOVE WRITR                  TO RCOLPND-FUNCTION   .          <PHLRMS>
                                                                        <PHLRMS>
           CALL 'RCOLPNDIO'            USING RCOLPND-PARAMS.            <PHLRMS>
                                                                        <PHLRMS>
           IF  RCOLPND-STATUZ          NOT = O-K                        <PHLRMS>
               MOVE RCOLPND-STATUZ     TO SYSR-STATUZ                   <PHLRMS>
               MOVE RCOLPND-PARAMS     TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       3190-EXIT.                                                       <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /
       3200-READ-RCOLINF SECTION.                                       <PHLRMS>
      ***************************                                       <PHLRMS>
      *                                                                 <PHLRMS>
       3210-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE 'N'                    TO WSAA-FOUND-TEMP-RCPT.         <PHLRMS>
           INITIALIZE                     RCOLINF-PARAMS     .          <PHLRMS>
           MOVE WSAA-RECEIPT-PFX       TO RCOLINF-RDOCPFX    .          <PHLRMS>
           MOVE WSAA-RECEIPT-COY       TO RCOLINF-RDOCCOY    .          <PHLRMS>
      ***  MOVE SR289-RECEIPT          TO RCOLINF-RDOCNUM    .          <PHLRMS>
           MOVE WSAA-RECEIPT           TO RCOLINF-RDOCNUM    .          <PHLRMS>
           MOVE RCOLINFREC             TO RCOLINF-FORMAT     .          <PHLRMS>
           MOVE READR                  TO RCOLINF-FUNCTION   .          <PHLRMS>
                                                                        <PHLRMS>
           CALL 'RCOLINFIO'            USING RCOLINF-PARAMS.            <PHLRMS>
                                                                        <PHLRMS>
           IF  RCOLINF-STATUZ          NOT = O-K                        <PHLRMS>
           AND                         NOT = MRNF                       <PHLRMS>
               MOVE RCOLINF-STATUZ     TO SYSR-STATUZ                   <PHLRMS>
               MOVE RCOLINF-PARAMS     TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  RCOLINF-STATUZ          = O-K                            <PHLRMS>
               MOVE 'Y'                TO WSAA-FOUND-TEMP-RCPT          <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       3290-EXIT.                                                       <PHLRMS>
           EXIT.                                                        <PHLRMS>
      *****************************************************************
      *     DECIDE WHICH TRANSACTION PROGRAM IS NEXT
      *****************************************************************
      *
       4000-WHERE-NEXT SECTION.
      *************************
      *
       4010-NEXT-PROGRAM.
      *                                                                 <RC002>
      * Tranfer the Pre-Printed Code & Number to others programs        <RC002>
      *  Use WSSP-SERVUNIT to transer Pre-Printed Code                  <RC002>
      *  Use Memory at position 102 (filler) to transer Pre-Printed Num <RC002>
      *                                                                 <RC002>
           MOVE SR289-RCPREPRTCD       TO WSSP-SERVUNIT.                <RC002>
           MOVE SR289-RCPREPRNT        TO WSSP-COMMON-AREA(102:8).      <RC002>
      *                                                                 <RC002>
      * Check for subfile selection.

      *    ADD 1                       TO WSSP-PROGRAM-PTR.             <V76F04>
           MOVE WSAA-PROG              TO WSSP-NEXTPROG.                <V76F04>
                                                                        <V76F04>
       4020-FKEY-SWITCHING.                                             <V76F13>
           IF WSAA-SCRN-STATUZ          = 'DIRY'                        <V76F04>
              MOVE WSAA-SCRN-STATUZ    TO OPTS-SEL-CODE                 <V76F04>
              MOVE 'F'                 TO OPTS-SEL-TYPE                 <V76F04>
              MOVE ZEROES              TO OPTS-SEL-OPTNO                <V76F04>
              MOVE SPACES              TO WSAA-SCRN-STATUZ              <V76F04>
              GO TO 4080-OPTSWCH                                        <V76F04>
           END-IF.                                                      <V76F04>
                                                                        <V76F13>
       4030-LINE-SWITCHING.                                             <V76F13>
           PERFORM 4100-SUBFILE-CHECK-BOXS.                             <V76F13>
           IF FOUND-SELECTION                                           <V76F13>
              GO TO 4080-OPTSWCH                                        <V76F13>
           END-IF.                                                      <V76F13>
                                                                        <V76F13>
           IF WSSP-LASTPROG(2:4)        = '2610'                        <V76F04>
              ADD  1                   TO WSSP-PROGRAM-PTR              <V76F04>
              GO TO 4090-EXIT                                           <V76F04>
           END-IF.                                                      <V76F04>
      *                                                                 <V76F04>
       4080-OPTSWCH.                                                    <V76F04>
      *                                                                 <V76F04>
           MOVE 'STCK'                 TO OPTS-FUNCTION.                <V76F04>
           CALL 'OPTSWCH'           USING OPTSWCH-REC                   <V76F04>
                                          WSSP-SEC-PROGS                <V76F04>
                                          WSSP-SEC-ACTNS                <V76F04>
                                          WSSP-PROGRAM-PTR              <V76F04>
                                          WSSP-FLAG.                    <V76F04>
           IF   OPTS-STATUZ         NOT = O-K AND ENDP                  <V76F04>
                MOVE 'STCK'            TO SYSR-FUNCTION                 <V76F04>
                MOVE OPTS-STATUZ       TO SYSR-DBIO-STATUZ              <V76F04>
                                          SYSR-STATUZ                   <V76F04>
                MOVE 'OPTSWCH'         TO SYSR-IOMOD                    <V76F04>
                PERFORM 600-FATAL-ERROR                                 <V76F04>
           END-IF.                                                      <V76F04>
      *                                                                 <V76F04>
           IF OPTS-STATUZ              = ENDP                           <V76F04>
           AND WSSP-SEC-ACTN(WSSP-PROGRAM-PTR) = ' '                    <V76F04>
               MOVE SCRN-SCRNAME       TO WSSP-NEXTPROG                 <V76F04>
               MOVE 1                  TO SCRN-SUBFILE-RRN              <V76F04>
                  PERFORM A100-KEEP-RMRK                                <V76F04>
                  PERFORM A200-INIT-OPTS                                <V76F04>
                  PERFORM 4500-CHECK-INDICS                             <V76F13>
           ELSE                                                         <V76F04>
               ADD 1                   TO WSSP-PROGRAM-PTR              <V76F04>
           END-IF.                                                      <V76F04>
                                                                        <V76F04>
      *
       4090-EXIT.
            EXIT.
      /                                                                 <V76F13>
       4100-SUBFILE-CHECK-BOXS SECTION.                                 <V76F13>
      *********************************                                 <V76F13>
       4100-BEGIN.                                                      <V76F13>
           MOVE SPACES                 TO WSAA-FOUND-SELECTION.         <V76F13>
           MOVE SSTRT                  TO SCRN-FUNCTION.                <V76F13>
           PERFORM 9000-SCREEN-IO.                                      <V76F13>
           PERFORM UNTIL SCRN-STATUZ NOT = O-K                          <V76F13>
                                      OR FOUND-SELECTION                <V76F13>
              MOVE SR289-PAYTYPE       TO CSTP-CASHTYPE                 <V76F13>
              IF  CSTP-CREDIT-CARD                                      <V76F13>
              AND (SR289-LXOPTIND       = 'X' OR                        <V76F13>
                   SR289-INDIC          = 'Y')                          <V76F13>
                 MOVE 'Y'              TO WSAA-FOUND-SELECTION          <V76F13>
              ELSE                                                      <V76F13>
                 MOVE SRDN             TO SCRN-FUNCTION                 <V76F13>
                 PERFORM 9000-SCREEN-IO                                 <V76F13>
              END-IF                                                    <V76F13>
           END-PERFORM.                                                 <V76F13>
                                                                        <V76F13>
           IF NOT FOUND-SELECTION                                       <V76F13>
              GO TO 4100-EXIT                                           <V76F13>
           END-IF.                                                      <V76F13>
                                                                        <V76F13>
           MOVE SPACES                 TO WSAA-FOUND-SELECTION.         <V76F13>
           MOVE 'L'                    TO OPTS-SEL-TYPE.                <V76F13>
           MOVE WSAA-C6                TO OPTS-SEL-CODE.                <V76F13>
           PERFORM VARYING IX        FROM 1 BY 1                        <V76F13>
                                    UNTIL IX > WSAA-MAX-OPT             <V76F13>
                                      OR FOUND-SELECTION                <V76F13>
              IF OPTS-TYPE(IX)          = 'L'                  AND      <V76F13>
                 OPTS-CODE(IX)          = OPTS-SEL-CODE                 <V76F13>
                 MOVE 'X'              TO OPTS-IND(IX)                  <V76F13>
                 MOVE 'Y'              TO WSAA-FOUND-SELECTION          <V76F13>
              END-IF                                                    <V76F13>
           END-PERFORM.                                                 <V76F13>
           PERFORM A400-KEEPS-CCARD-DETAILS.                            <V76F13>
                                                                        <V76F13>
           MOVE SPACES                 TO SR289-LXOPTIND.               <V76F13>
           MOVE SPACES                 TO SR289-INDIC.                  <V76F13>
           MOVE SUPD                   TO SCRN-FUNCTION.                <V76F13>
           PERFORM 9000-SCREEN-IO.                                      <V76F13>
                                                                        <V76F13>
       4100-EXIT.                                                       <V76F13>
           EXIT.                                                        <V76F13>
      /                                                                 <V76F13>
       4500-CHECK-INDICS SECTION.                                       <V76F13>
      ***************************                                       <V76F13>
       4500-CHECK.                                                      <V76F13>
      *--Rebuild Check Box.                                             <V76F13>
           MOVE SSTRT                  TO SCRN-FUNCTION.                <V76F13>
           PERFORM 9000-SCREEN-IO.                                      <V76F13>
                                                                        <V76F13>
           PERFORM UNTIL SCRN-STATUZ NOT = O-K                          <V76F13>
              PERFORM A300-SUBFILE-INIT-OPTSWCH                         <V76F13>
                                                                        <V76F13>
              MOVE SUPD                TO SCRN-FUNCTION                 <V76F13>
              PERFORM 9000-SCREEN-IO                                    <V76F13>
                                                                        <V76F13>
              MOVE SRDN                TO SCRN-FUNCTION                 <V76F13>
              PERFORM 9000-SCREEN-IO                                    <V76F13>
                                                                        <V76F13>
           END-PERFORM.                                                 <V76F13>
           MOVE 1                      TO SCRN-SUBFILE-RRN.             <V76F13>
       4500-EXIT.                                                       <V76F13>
           EXIT.                                                        <V76F13>
      /                                                                 <V76F06>
       8000-CALL-ROUNDING SECTION.                                      <V76F06>
      ****************************                                      <V76F06>
       8100-CALL.                                                       <V76F06>
      *                                                                 <V76F06>
           MOVE SPACES                 TO ZRDP-FUNCTION                 <V76F06>
           MOVE WSSP-COMPANY           TO ZRDP-COMPANY.                 <V76F06>
           MOVE O-K                    TO ZRDP-STATUZ.                  <V76F06>
           MOVE WSKY-BATC-BATCTRCDE    TO ZRDP-BATCTRCDE.               <V76F06>
                                                                        <V76F06>
           CALL 'ZRDECPLC'             USING ZRDP-ZRDECPL-REC.          <V76F06>
                                                                        <V76F06>
           IF  ZRDP-STATUZ             NOT = O-K                        <V76F06>
               MOVE ZRDP-STATUZ        TO SYSR-STATUZ                   <V76F06>
               MOVE ZRDP-ZRDECPL-REC   TO SYSR-PARAMS                   <V76F06>
               PERFORM 600-FATAL-ERROR.                                 <V76F06>
                                                                        <V76F06>
       8900-EXIT.                                                       <V76F06>
           EXIT.                                                        <V76F06>
      /
       9000-SCREEN-IO SECTION.
      ************************
       9000-SCREEN.

           CALL 'SR289IO' USING SCRN-SCREEN-PARAMS
                                SR289-DATA-AREA
                                SR289-SUBFILE-AREA.

           IF SCRN-STATUZ           NOT = O-K AND ENDP
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.

       9000-EXIT.
            EXIT.
      /
       A1000-CALL-ALOCNO SECTION.
      **************************
       A1000-BEGIN.
      *
      * Allocate next automatic Receipt Number.
      *
           MOVE 'NEXT '                TO ALNO-FUNCTION.
           MOVE PRFX-CASH              TO ALNO-PREFIX.
           IF T3688-BBALPFX            = SPACES
              MOVE WSSP-BANKCODE       TO ALNO-GENKEY
           ELSE
              MOVE T3688-BBALPFX       TO ALNO-GENKEY.
           MOVE WSSP-COMPANY           TO ALNO-COMPANY.

           CALL 'ALOCNO' USING ALNO-ALOCNO-REC.

           IF ALNO-STATUZ              = BOMB
              MOVE ALNO-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.
       A1000-EXIT.
            EXIT.
      /
       A2000-FORMAT-NAME SECTION.
      **************************
       A2000-BEGIN.
      *
           IF SR289-RFCODE             = CSRF-CN
              GO TO A2000-CALL-NAMADRS
           END-IF.
      *
      *  Get the agent Client Number.
      *
           MOVE SR289-RFCODE           TO GTCL-ACCT-PREFIX.
           MOVE WSSP-COMPANY           TO GTCL-ACCT-COMPANY.
           MOVE SR289-RFNUM            TO GTCL-ACCT-NUMBER.
           MOVE WSAA-CLNT-FUNCTION     TO GTCL-FUNCTION.

           CALL 'GETCLNT'           USING GTCL-GETCLNT-REC.

           IF GTCL-STATUZ              NOT = O-K
              MOVE GTCL-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.
      *
       A2000-CALL-NAMADRS.
      *
      * Format the Cash Receipt Name to Payee Name.
      *
           MOVE SPACES                 TO NMAD-NAMADRS-REC.
           IF SR289-RFCODE             = CSRF-CN
              MOVE SR289-RFCODE        TO NMAD-CLNT-PREFIX
                                          WSKY-CLNT-CLNTPFX
              MOVE WSSP-FSUCO          TO NMAD-CLNT-COMPANY
                                          WSKY-CLNT-CLNTCOY
              MOVE SR289-RFNUM         TO NMAD-CLNT-NUMBER
                                          WSKY-CLNT-CLNTNUM
           END-IF.
           IF SR289-RFCODE             = CSRF-AG
              MOVE GTCL-CLNT-PREFIX    TO NMAD-CLNT-PREFIX
                                          WSKY-CLNT-CLNTPFX
              MOVE GTCL-CLNT-COMPANY   TO NMAD-CLNT-COMPANY
                                          WSKY-CLNT-CLNTCOY
              MOVE GTCL-CLNT-NUMBER    TO NMAD-CLNT-NUMBER
                                          WSKY-CLNT-CLNTNUM
           END-IF.
           MOVE WSSP-LANGUAGE          TO NMAD-LANGUAGE.
           MOVE WSAA-PAYEE-GIVN        TO NMAD-FUNCTION.

           CALL 'NAMADRS'           USING NMAD-NAMADRS-REC.

           IF NMAD-STATUZ           NOT = O-K
              MOVE NMAD-NAMADRS-REC    TO SYSR-PARAMS
              MOVE NMAD-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.

           IF NMAD-STATUZ              = MRNF
              MOVE ALL '?'             TO WSAA-CASHNM
           ELSE
              MOVE NMAD-NAME           TO WSAA-CASHNM
           END-IF.
      *
       A2000-EXIT.
            EXIT.
      /
       B1000-READR-ITEM SECTION.
      **************************
       B1000-BEGIN.
           MOVE READR                  TO ITEM-FUNCTION.
           MOVE ITEMREC                TO ITEM-FORMAT.
           CALL 'ITEMIO'            USING ITEM-PARAMS.

           IF  ITEM-STATUZ          NOT = O-K AND MRNF
               MOVE ITEM-STATUZ        TO SYSR-STATUZ
               MOVE ITEM-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.
       B1000-EXIT.
            EXIT.
      /
       B1010-BEGN-ADOC SECTION.
      *************************
       B1010-BEGIN.
           MOVE BEGN                   TO ADOC-FUNCTION.
           MOVE RTRNREC                TO ADOC-FORMAT.
           CALL 'ADOCIO'            USING ADOC-PARAMS.

           IF ADOC-STATUZ           NOT = O-K AND ENDP
              MOVE ADOC-STATUZ         TO SYSR-STATUZ
              MOVE ADOC-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.
       B1010-EXIT.
            EXIT.
      /
       B1020-READR-RDOC SECTION.
      **************************
       B1020-BEGIN.
           MOVE READR                  TO RDOC-FUNCTION.
           MOVE RDOCREC                TO RDOC-FORMAT.
           CALL 'RDOCIO'            USING RDOC-PARAMS.
           IF RDOC-STATUZ           NOT = O-K AND MRNF
              MOVE RDOC-STATUZ         TO SYSR-STATUZ
              MOVE RDOC-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.
       B1020-EXIT.
            EXIT.
      /
       B1030-READR-CLNT SECTION.
      **************************
       B1030-BEGIN.
           MOVE READR                  TO CLNT-FUNCTION.
           MOVE CLNTREC                TO CLNT-FORMAT.
           CALL 'CLNTIO'            USING CLNT-PARAMS.

           IF CLNT-STATUZ           NOT = O-K AND MRNF
              MOVE CLNT-STATUZ         TO SYSR-STATUZ
              MOVE CLNT-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.
       B1030-EXIT.
            EXIT.
      /
       B1040-READR-AGNT SECTION.
      **************************
       B1040-BEGIN.
           MOVE READR                  TO AGNT-FUNCTION.
           MOVE AGNTREC                TO AGNT-FORMAT.
           CALL 'AGNTIO'            USING AGNT-PARAMS.

           IF AGNT-STATUZ          NOT = O-K AND MRNF
              MOVE AGNT-STATUZ         TO SYSR-STATUZ
              MOVE AGNT-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.
       B1040-EXIT.
            EXIT.
      /
       B1050-READR-BABR SECTION.
      **************************
       B1050-BEGIN.
           MOVE READR                  TO BABR-FUNCTION.
           MOVE BABRREC                TO BABR-FORMAT.
           CALL 'BABRIO'            USING BABR-PARAMS.

           IF BABR-STATUZ           NOT = O-K AND MRNF
              MOVE BABR-STATUZ         TO SYSR-STATUZ
              MOVE BABR-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

       B1050-EXIT.
            EXIT.
      /                                                                 <V64F13>
       C1000-TR386-LITERALS SECTION.                                    <V64F13>
      ******************************                                    <V64F13>
       C1010-PARA.                                                      <V64F13>
                                                                        <V64F13>
           MOVE SPACES                 TO SR289-LNGDSC.                 <V64F13>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <V64F13>
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.                 <V64F13>
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.                 <V64F13>
           MOVE TR386                  TO ITEM-ITEMTABL.                <V64F13>
           MOVE WSSP-LANGUAGE          TO WSAA-TR386-LANG.              <V64F13>
           MOVE WSAA-PROG              TO WSAA-TR386-PGM.               <V64F13>
           MOVE WSAA-TR386-KEY         TO ITEM-ITEMITEM.                <V64F13>
           MOVE SPACES                 TO ITEM-ITEMSEQ.                 <V64F13>
           MOVE READR                  TO ITEM-FUNCTION.                <V64F13>
           MOVE O-K                    TO ITEM-STATUZ.                  <V64F13>
                                                                        <V64F13>
           CALL 'ITEMIO' USING ITEM-PARAMS.                             <V64F13>
                                                                        <V64F13>
           IF ITEM-STATUZ           NOT = O-K                           <V64F13>
      ****    MOVE ITEM-STATUZ         TO SYSR-STATUZ                   <V64F13>
      ****    MOVE ITEM-PARAMS         TO SYSR-PARAMS                   <V64F13>
      ****    PERFORM 600-FATAL-ERROR                                   <V64F13>
              GO TO C1090-EXIT                                          <V64F13>
           END-IF.                                                      <V64F13>
                                                                        <V64F13>
           MOVE ITEM-GENAREA           TO TR386-TR386-REC.              <V64F13>
                                                                        <V64F13>
            MOVE TR386-PROGDESC-01     TO DBCS-INPUT-STRING             <V64F13>
            MOVE WSAA-LITR-LEN-18      TO DBCS-OUTPUT-LENGTH            <V64F13>
                                                                        <V64F13>
            MOVE SPACES                TO DBCS-STATUZ                   <V64F13>
            CALL 'DBCSTRNC'         USING DBCSTRNC-REC                  <V64F13>
                                                                        <V64F13>
            IF DBCS-STATUZ          NOT = O-K                           <V64F13>
               MOVE SPACES             TO DBCS-OUTPUT-STRING            <V64F13>
            END-IF.                                                     <V64F13>
            IF DBCS-OUTPUT-STRING      NOT = SPACES                     <V64F13>
               MOVE DBCS-OUTPUT-STRING TO SR289-LNGDSC                  <V64F13>
            END-IF.                                                     <V64F13>
                                                                        <V64F13>
           MOVE TR386-PROGDESC-02      TO DBCS-INPUT-STRING.            <V76F12>
           MOVE WSAA-LITR-LEN-18       TO DBCS-OUTPUT-LENGTH.           <V76F12>
                                                                        <V76F12>
           MOVE SPACES                 TO DBCS-STATUZ.                  <V76F12>
           CALL 'DBCSTRNC'         USING DBCSTRNC-REC                   <V76F12>
                                                                        <V76F12>
           IF DBCS-STATUZ          NOT = O-K                            <V76F12>
              MOVE SPACES             TO DBCS-OUTPUT-STRING             <V76F12>
           END-IF.                                                      <V76F12>
           IF DBCS-OUTPUT-STRING      NOT = SPACES                      <V76F12>
              MOVE DBCS-OUTPUT-STRING TO SR289-STDESCSH                 <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
       C1090-EXIT.                                                      <V64F13>
            EXIT.                                                       <V64F13>
      /                                                                 <V64F13>
       C1500-DEFAULT-FIELDS SECTION.                                    <V64F13>
      ******************************                                    <V64F13>
       C1510-PARA.                                                      <V64F13>
                                                                        <V64F13>
           MOVE 'CHKFL'                TO CHKF-FUNCTION.                <V64F13>
           MOVE WSSP-COMPANY           TO CHKF-COMPANY.                 <V64F13>
           MOVE WSAA-PROG              TO CHKF-PROG.                    <V64F13>
                                                                        <V64F13>
      * Debtor Marry Indicator.                                         <V64F13>
           MOVE SPACES                 TO CHKF-FIELD-ID.                <V64F13>
           MOVE 'SR289-MARRYFLAG     ' TO CHKF-FIELD-ID.                <V64F13>
           CALL 'CHKFSUF'          USING CHKF-CHKF-REC                  <V64F13>
                                          SR289-MARRYFLAG               <V64F13>
                                          SR289-MYFLG-ERR               <V64F13>
                                          SR289-MYFLG-OUT(PR).          <V64F13>
       C1590-EXIT.                                                      <V76F07>
            EXIT.                                                       <V76F07>
      /                                                                 <V76F07>
       C1800-RBNK-MARRYFLAG SECTION.                                    <V64F13>
      ******************************                                    <V64F13>
       C1810-PARA.                                                      <V64F13>
           INITIALIZE                     RBNK-DATA-AREA.               <V64F13>
           MOVE O-K                    TO RBNK-STATUZ.                  <V64F13>
           MOVE PRFX-CASH              TO RBNK-RDOCPFX.                 <V64F13>
           MOVE WSSP-COMPANY           TO RBNK-RDOCCOY.                 <V64F13>
           MOVE WSAA-RECEIPT           TO RBNK-RDOCNUM.                 <V64F13>
           MOVE READR                  TO RBNK-FUNCTION.                <V76F12>
           MOVE RBNKREC                TO RBNK-FORMAT.                  <V64F13>
                                                                        <V64F13>
           CALL 'RBNKIO'            USING RBNK-PARAMS.                  <V64F13>
           IF RBNK-STATUZ           NOT = O-K                           <V76F12>
              MOVE RBNK-STATUZ         TO SYSR-STATUZ                   <V64F13>
              MOVE RBNK-PARAMS         TO SYSR-PARAMS                   <V64F13>
              PERFORM 600-FATAL-ERROR                                   <V64F13>
           END-IF.                                                      <V64F13>
                                                                        <V64F13>
           MOVE RBNK-MARRYFLAG         TO SR289-MARRYFLAG.              <V64F13>
                                                                        <V64F13>
       C1890-EXIT.                                                      <V64F13>
            EXIT.                                                       <V64F13>
      /                                                                 <V64F13>
       C2000-FIELD-CHECK SECTION.                                       <V64F13>
      ***************************                                       <V64F13>
       C2010-PARA.                                                      <V64F13>
                                                                        <V64F13>
           MOVE 'CHECK'                TO CHKF-FUNCTION.                <V64F13>
           MOVE WSSP-COMPANY           TO CHKF-COMPANY.                 <V64F13>
           MOVE WSAA-PROG              TO CHKF-PROG.                    <V64F13>
                                                                        <V64F13>
      * Debtor Nary Indicator                                           <V64F13>
           MOVE SPACES                 TO CHKF-FIELD-ID.                <V64F13>
           MOVE 'SR289-MARRYFLAG     ' TO CHKF-FIELD-ID.                <V64F13>
           CALL 'CHKFSUF'          USING CHKF-CHKF-REC                  <V64F13>
                                          SR289-MARRYFLAG               <V64F13>
                                          SR289-MYFLG-ERR               <V64F13>
                                          SR289-MYFLG-OUT(PR).          <V64F13>
                                                                        <V64F13>
           IF SR289-MYFLG-ERR          NOT = SPACES                     <V64F13>
              MOVE 'Y'                 TO WSSP-EDTERROR                 <V64F13>
              GO TO C2090-EXIT                                          <V64F13>
           END-IF.                                                      <V64F13>
                                                                        <V64F13>
           IF SR289-MARRYFLAG          NOT = SPACE AND                  <V64F13>
              SR289-MARRYFLAG          NOT = 'G' AND                    <V64F13>
              SR289-MARRYFLAG          NOT = 'N'                        <V64F13>
              MOVE H452                TO SR289-MYFLG-ERR               <V64F13>
              MOVE 'Y'                 TO WSSP-EDTERROR                 <V64F13>
           END-IF.                                                      <V64F13>
                                                                        <V64F13>
       C2090-EXIT.                                                      <V64F13>
            EXIT.                                                       <V64F13>
      /                                                                 <V76F12>
       C3000-GET-CNRSNDESC SECTION.                                     <V76F12>
      *****************************                                     <V76F12>
       C3010-BEGIN.                                                     <V76F12>
           INITIALIZE                     DESC-DATA-AREA.               <V76F12>
           MOVE SMTP-ITEM              TO DESC-DESCPFX.                 <V76F12>
           MOVE WSSP-FSUCO             TO DESC-DESCCOY.                 <V76F12>
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.                <V76F12>
           MOVE SPACES                 TO DESC-ITEMSEQ.                 <V76F12>
           MOVE DESCREC                TO DESC-FORMAT.                  <V76F12>
                                                                        <V76F12>
           IF SR289-RCPTSTAT            = RCTST-CS OR                   <V76F12>
                                          RCTST-CU                      <V76F12>
              MOVE TR29H               TO DESC-DESCTABL                 <V76F12>
              MOVE WSSP-LANGUAGE       TO WSAA-TR29H-LANG               <V76F12>
              MOVE SR289-CNRSNCD       TO WSAA-TR29H-CODE               <V76F12>
              MOVE WSAA-TR29H-ITEM     TO DESC-DESCITEM                 <V76F12>
           ELSE                                                         <V76F12>
              MOVE TR29I               TO DESC-DESCTABL                 <V76F12>
              MOVE WSSP-LANGUAGE       TO WSAA-TR29I-LANG               <V76F12>
              MOVE SR289-CNRSNCD       TO WSAA-TR29I-CODE               <V76F12>
              MOVE WSAA-TR29I-ITEM     TO DESC-DESCITEM                 <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
           MOVE READR                  TO DESC-FUNCTION.                <V76F12>
           CALL 'DESCIO'            USING DESC-PARAMS.                  <V76F12>
                                                                        <V76F12>
           IF DESC-STATUZ           NOT = O-K                           <V76F12>
              MOVE SPACES              TO SR289-CNRSNDESC               <V76F12>
           ELSE                                                         <V76F12>
              MOVE DESC-LONGDESC       TO SR289-CNRSNDESC               <V76F12>
           END-IF.                                                      <V76F12>
       C3090-EXIT.                                                      <V76F12>
           EXIT.                                                        <V76F12>
      *                                                                 <RC002>
       C4000-READ-ZPPRCHK SECTION.                                      <RC002>
      ****************************                                      <RC002>
       C4100-READ.                                                      <RC002>
      *                                                                 <RC002>
           CALL 'ZPPRCHKIO' USING ZPPRCHK-PARAMS.                       <RC002>
                                                                        <RC002>
           IF (ZPPRCHK-STATUZ         NOT = O-K)  AND                   <RC002>
              (ZPPRCHK-STATUZ         NOT = ENDP)                       <RC002>
              MOVE ZPPRCHK-PARAMS        TO SYSR-PARAMS                 <RC002>
              PERFORM 600-FATAL-ERROR                                   <RC002>
           END-IF.                                                      <RC002>
                                                                        <RC002>
           IF ZPPRCHK-STATUZ              = ENDP                        <RC002>
              GO TO C4900-EXIT                                          <RC002>
           END-IF.                                                      <RC002>
                                                                        <RC002>
           IF ZPPRCHK-STATUZ        = O-K                               <RC002>
           AND ZPPRCHK-PREFIX       = PRFX-CASH                         <RC002>
           AND ZPPRCHK-COMPANY      = WSSP-COMPANY                      <RC002>
      ***  AND ZPPRCHK-BANKCODE     = WSSP-BANKCODE                     <RC002>
           AND ZPPRCHK-RCPREPRTCD   = SR289-RCPREPRTCD                  <RC002>
           AND ZPPRCHK-RCPREPRNT    = SR289-RCPREPRNT                   <RC002>
      *   If Creation, just alert duplicate                             <RC002>
               IF SBM-CREATION                                          <RC002>
                  MOVE 'Y'        TO WSAA-DUPLICATE-FLAG                <RC002>
               END-IF                                                   <RC002>
      *   If Modify, check if this number is already used               <RC002>
      *   for another receipt?                                          <RC002>
               IF SBM-MODIFY                                            <RC002>
               AND ZPPRCHK-RECEIPT NOT = SR289-RECEIPT                  <RC002>
                   MOVE 'Y'        TO WSAA-DUPLICATE-FLAG               <RC002>
               END-IF                                                   <RC002>
           ELSE                                                         <RC002>
               MOVE ENDP                  TO ZPPRCHK-STATUZ             <RC002>
           END-IF.                                                      <RC002>
                                                                        <RC002>
           MOVE NEXTR                    TO ZPPRCHK-FUNCTION.           <RC002>
                                                                        <RC002>
       C4900-EXIT.                                                      <RC002>
           EXIT.                                                        <RC002>
      /                                                                 <PHLRMS>
       A2100-FIND-LOCATION SECTION.                                     <PHLRMS>
      *******************************                                   <PHLRMS>
      *                                                                 <PHLRMS>
       A2110-START.                                                     <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE 'N'                    TO WSAA-EXP-RCPT.                <PHLRMS>
           MOVE 'Y'                    TO WSAA-VALID-RECEIPT.           <PHLRMS>
           INITIALIZE                     TRRNINF-PARAMS.               <PHLRMS>
      *    MOVE SR289-RCPREPRTCD       TO TRRNINF-RCTYPE.       <PS008> <PHLRMS>
      *    MOVE SR289-RCPREPRNT        TO TRRNINF-RCSERNUM.     <PS008> <PHLRMS>
           MOVE WSAA-RCPREPRTCD        TO TRRNINF-RCTYPE.               <PS008>
           MOVE WSAA-RCPREPRNT         TO TRRNINF-RCSERNUM.             <PS008>
           MOVE TRRNINFREC             TO TRRNINF-FORMAT.               <PHLRMS>
           MOVE READR                  TO TRRNINF-FUNCTION.             <PHLRMS>
                                                                        <PHLRMS>
           CALL 'TRRNINFIO'            USING TRRNINF-PARAMS.            <PHLRMS>
                                                                        <PHLRMS>
           IF TRRNINF-STATUZ           NOT = O-K                        <PHLRMS>
           AND                         NOT = MRNF                       <PHLRMS>
               MOVE TRRNINF-PARAMS     TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  TRRNINF-STATUZ          = MRNF                           <PHLRMS>
               MOVE EV31               TO SR289-RCPREPRTCD-ERR          <PHLRMS>
               MOVE EV31               TO SR289-RCPREPRNT-ERR           <PHLRMS>
               MOVE 'N'                TO WSAA-VALID-RECEIPT            <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
      *                                                                 <PHLRMS>
       A2190-EXIT.                                                      <PHLRMS>
            EXIT.                                                       <PHLRMS>
      /                                                                 <PHLRMS>
       A2200-VALIDATE-ENTITY SECTION.                                   <PHLRMS>
      *******************************                                   <PHLRMS>
      *                                                                 <PHLRMS>
       A2210-START.                                                     <PHLRMS>
      *                                                                 <PHLRMS>
      **   IF  (TRRNINF-TOLOC          NOT = SPACES)                    <PHLRMS>
      **       IF  SR289-DENTTYP       = SPACES                         <PHLRMS>
      **       AND SR289-ENTYNO        = SPACES                         <PHLRMS>
      **           MOVE TRRNINF-TOPFX  TO SR289-DENTTYP                 <PHLRMS>
      **           MOVE TRRNINF-TOLOC  TO SR289-ENTYNO                  <PHLRMS>
      **       ELSE                                                     <PHLRMS>
      **           IF  (SR289-DENTTYP  NOT = TRRNINF-TOPFX              <PHLRMS>
      **           OR   SR289-ENTYNO   NOT = TRRNINF-TOLOC)             <PHLRMS>
      **               MOVE EV26       TO SR289-RCPREPRTCD-ERR          <PHLRMS>
      **               MOVE EV26       TO SR289-RCPREPRNT-ERR           <PHLRMS>
      **           END-IF                                               <PHLRMS>
      **       END-IF                                                   <PHLRMS>
      **   END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
      **   IF  (TRRNINF-TOPER          NOT = SPACES)                    <PHLRMS>
      **       IF  SR289-DENTTYP       = SPACES                         <PHLRMS>
      **       AND SR289-ENTYNO        = SPACES                         <PHLRMS>
      **           MOVE TRRNINF-TOPFX  TO SR289-DENTTYP                 <PHLRMS>
      **           MOVE TRRNINF-TOPER  TO SR289-ENTYNO                  <PHLRMS>
      **       ELSE                                                     <PHLRMS>
      **           IF  (SR289-DENTTYP  NOT = TRRNINF-TOPFX              <PHLRMS>
      **           OR   SR289-ENTYNO   NOT = TRRNINF-TOPER)             <PHLRMS>
      **               MOVE EV26       TO SR289-RCPREPRTCD-ERR          <PHLRMS>
      **               MOVE EV26       TO SR289-RCPREPRNT-ERR           <PHLRMS>
      **           END-IF                                               <PHLRMS>
      **       END-IF                                                   <PHLRMS>
      **   END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           MOVE 'N'                    TO WSAA-FOUND.                   <PHLRMS>
                                                                        <PHLRMS>
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 20               <PHLRMS>
                                   OR WSAA-FOUND = 'Y'                  <PHLRMS>
                                   OR TV037-NCURSTAT(IDX)  = SPACES     <PHLRMS>
               IF  TV037-NCURSTAT(IDX) = TRRNINF-RCSTAT                 <PHLRMS>
                   MOVE 'Y'            TO WSAA-FOUND                    <PHLRMS>
               END-IF                                                   <PHLRMS>
           END-PERFORM.                                                 <PHLRMS>
                                                                        <PHLRMS>
           IF  WSAA-FOUND              = 'Y'                            <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Not allow to do                                              <PHLRMS>
      *                                                                 <PHLRMS>
               MOVE EV17               TO SR289-RCPREPRTCD-ERR          <PHLRMS>
               MOVE EV17               TO SR289-RCPREPRNT-ERR           <PHLRMS>
               GO TO A2290-EXIT                                         <PHLRMS>
           ELSE                                                         <PHLRMS>
               PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 20           <PHLRMS>
                                    OR WSAA-FOUND = 'Y'                 <PHLRMS>
                                    OR TV037-CURSTAT(IDX)  = SPACES     <PHLRMS>
                   IF  TV037-CURSTAT(IDX) = TRRNINF-RCSTAT              <PHLRMS>
                       MOVE 'Y'        TO WSAA-FOUND                    <PHLRMS>
                   END-IF                                               <PHLRMS>
               END-PERFORM                                              <PHLRMS>
      *                                                                 <PHLRMS>
      *--  If not parameter, allow all statuses                         <PHLRMS>
      *                                                                 <PHLRMS>
               IF  WSAA-FOUND          = 'N'                            <PHLRMS>
               AND IDX                 NOT = 1                          <PHLRMS>
                   MOVE EV17           TO SR289-RCPREPRTCD-ERR          <PHLRMS>
                   MOVE EV17           TO SR289-RCPREPRNT-ERR           <PHLRMS>
                   GO TO A2290-EXIT                                     <PHLRMS>
               END-IF                                                   <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Not allow to update receipt not printed yet                  <PHLRMS>
      *                                                                 <PHLRMS>
           IF  TRRNINF-PRINTDT         = VRCM-MAX-DATE                  <PHLRMS>
               MOVE EV58               TO SR289-RCPREPRTCD-ERR          <PHLRMS>
                                          SR289-RCPREPRNT-ERR           <PHLRMS>
               GO TO A2290-EXIT                                         <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Collect Date not > Assign Date                               <PHLRMS>
      *                                                                 <PHLRMS>
           PERFORM A2800-GET-ASSIGN-DATE.                               <PHLRMS>
                                                                        <PHLRMS>
           IF  SR289-TRANDATEX         < WSAA-ASSIGNED-DATE             <PHLRMS>
               MOVE EV63               TO SR289-TRANDATEX-ERR           <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       A2290-EXIT.                                                      <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /                                                                 <PHLRMS>
       A2300-READ-TABLE SECTION.                                        <PHLRMS>
      *************************                                         <PHLRMS>
      *                                                                 <PHLRMS>
       A2310-START.                                                     <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE SMTP-ITEM             TO ITEM-ITEMPFX.                  <PHLRMS>
           MOVE WSSP-COMPANY          TO ITEM-ITEMCOY.                  <PHLRMS>
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
       A2390-EXIT.                                                      <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /                                                                 <PHLRMS>
       A2400-READ-RCPY SECTION.                                         <PHLRMS>
      *************************                                         <PHLRMS>
      *                                                                 <PHLRMS>
       A2410-START.                                                     <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE ZERO                   TO WSAA-AMNT.                    <PHLRMS>
           INITIALIZE                     RCPYACT-PARAMS.               <PHLRMS>
           MOVE SR289-RCPREPRTCD       TO RCPYACT-RCTYPE.               <PHLRMS>
           MOVE SR289-RCPREPRNT        TO RCPYACT-RCSERNUM.             <PHLRMS>
           MOVE RCPYACTREC             TO RCPYACT-FORMAT.               <PHLRMS>
           MOVE READR                  TO RCPYACT-FUNCTION.             <PHLRMS>
                                                                        <PHLRMS>
           CALL 'RCPYACTIO'            USING RCPYACT-PARAMS.            <PHLRMS>
                                                                        <PHLRMS>
           IF  RCPYACT-STATUZ          NOT = O-K                        <PHLRMS>
           AND                         NOT = MRNF                       <PHLRMS>
               MOVE RCPYACT-STATUZ     TO SYSR-STATUZ                   <PHLRMS>
               MOVE RCPYACT-PARAMS     TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  RCPYACT-STATUZ          = O-K                            <PHLRMS>
               MOVE RCPYACT-AMNT       TO WSAA-AMNT                     <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       A2490-EXIT.                                                      <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /                                                                 <PHLRMS>
       A2500-CHECK-TEMP-RCPT SECTION.                                   <PHLRMS>
      *******************************                                   <PHLRMS>
      *                                                                 <PHLRMS>
       A2510-START.                                                     <PHLRMS>
      *                                                                 <PHLRMS>
           IF (SR289-RCPREPRTCD        NOT = WSAA-TMP-RCPREPRTCD        <PHLRMS>
           OR  SR289-RCPREPRNT         NOT = WSAA-TMP-RCPREPRNT         <PHLRMS>
           OR  SR289-DOCORIGAMT        NOT = WSAA-TMP-AMOUNT)           <PHLRMS>
           AND WSAA-TMP-RCPREPRTCD     NOT = SPACES                     <PHLRMS>
           AND WSAA-TMP-RCPREPRNT      NOT = SPACES                     <PHLRMS>
           AND WSAA-TMP-AMOUNT         NOT = ZERO                       <PHLRMS>
           AND WSAA-EXP-RCPT           = 'N'                            <PHLRMS>
               MOVE SR289-RCPREPRTCD   TO WSAA-TMP-RCPREPRTCD           <PHLRMS>
               MOVE SR289-RCPREPRNT    TO WSAA-TMP-RCPREPRNT            <PHLRMS>
               MOVE SR289-DOCORIGAMT   TO WSAA-TMP-AMOUNT               <PHLRMS>
               MOVE SPACES             TO SR289-SFLAG                   <PHLRMS>
               MOVE 'N'                TO SR289-CONFIRM                 <PHLRMS>
               MOVE 'Y'                TO SR289-CONFIRM-OUT  (PR)       <PHLRMS>
               MOVE 'Y'                TO SR289-CONFIRM-OUT  (ND)       <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
      **** IF  SR289-RCPREPRTCD        = 'RN'                           <PHLRMS>
      ****     IF  SR289-CONFIRM       = 'N'                            <PHLRMS>
      ****         PERFORM A2400-READ-RCPY                              <PHLRMS>
      ****         IF  WSAA-AMNT       > ZERO                           <PHLRMS>
      ****             MOVE 'N'        TO SR289-CONFIRM                 <PHLRMS>
      ****         ELSE                                                 <PHLRMS>
      ****             MOVE 'Y'        TO SR289-CONFIRM                 <PHLRMS>
      ****             MOVE SPACES     TO SR289-SFLAG                   <PHLRMS>
      ****         END-IF                                               <PHLRMS>
      ****     END-IF                                                   <PHLRMS>
      **** ELSE                                                         <PHLRMS>
      ****     MOVE 'Y'                TO SR289-CONFIRM                 <PHLRMS>
      ****     MOVE SPACES             TO SR289-SFLAG                   <PHLRMS>
      **** END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       A2590-EXIT.                                                      <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /                                                                 <PHLRMS>
       A2600-UPDATE-RCPY SECTION.                                       <PHLRMS>
      ***************************                                       <PHLRMS>
      *                                                                 <PHLRMS>
       A2610-START.                                                     <PHLRMS>
      *                                                                 <PHLRMS>
           PERFORM A2400-READ-RCPY.                                     <PHLRMS>
                                                                        <PHLRMS>
           IF  RCPYACT-STATUZ          = MRNF                           <PHLRMS>
               GO TO A2690-EXIT                                         <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           MOVE 'Y'                    TO RCPYACT-PAYFLAG.              <PHLRMS>
           MOVE SR289-TRANDATEX        TO RCPYACT-COLDTE .              <PHLRMS>
                                                                        <PHLRMS>
           MOVE WRITD                  TO RCPYACT-FUNCTION.             <PHLRMS>
                                                                        <PHLRMS>
           CALL 'RCPYACTIO'            USING RCPYACT-PARAMS.            <PHLRMS>
                                                                        <PHLRMS>
           IF  RCPYACT-STATUZ          NOT = O-K                        <PHLRMS>
               MOVE RCPYACT-STATUZ     TO SYSR-STATUZ                   <PHLRMS>
               MOVE RCPYACT-PARAMS     TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       A2690-EXIT.                                                      <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /                                                                 <PHLRMS>
       A2700-CHECK-EXPIRY SECTION.                                      <PHLRMS>
      ***************************                                       <PHLRMS>
      *                                                                 <PHLRMS>
       A2710-START.                                                     <PHLRMS>
      *                                                                 <PHLRMS>
           IF  WSAA-TODAY              > TRRNINF-EXPDTE                 <PHLRMS>
           AND WSAA-EXPIRY-WARN        NOT = 'Y'                        <PHFX30>
      *                                                                 <PHLRMS>
      *--  Require a confirmation                                       <PHLRMS>
      *                                                                 <PHLRMS>
               IF  SR289-CONFIRM       NOT = 'Y'                        <PHLRMS>
                   MOVE 'Y'            TO WSAA-EXP-RCPT                 <PHLRMS>
                   MOVE EV53           TO SR289-CONFIRM-ERR             <PHLRMS>
                   MOVE WSAA-WARN      TO SR289-SFLAG                   <PHLRMS>
                   MOVE 'N'            TO SR289-CONFIRM-OUT  (PR)       <PHLRMS>
                   MOVE 'N'            TO SR289-CONFIRM-OUT  (ND)       <PHLRMS>
                   MOVE 'Y'            TO WSAA-EXPIRY-WARN              <PHFX30>
               END-IF                                                   <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       A2790-EXIT.                                                      <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /                                                                 <PHLRMS>
       A2800-GET-ASSIGN-DATE SECTION.                                   <PHLRMS>
      *******************************                                   <PHLRMS>
      *                                                                 <PHLRMS>
       A2810-START.                                                     <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE VRCM-MAX-DATE          TO WSAA-ASSIGNED-DATE.           <PHLRMS>
                                                                        <PHLRMS>
           INITIALIZE                     TRRNCDE-PARAMS.               <PHLRMS>
           MOVE SR289-RCPREPRTCD       TO TRRNCDE-RCTYPE.               <PHLRMS>
           MOVE SR289-RCPREPRNT        TO TRRNCDE-RCSERNUM.             <PHLRMS>
           MOVE 'TA08'                 TO TRRNCDE-TRANCDE.              <PHLRMS>
           MOVE TRRNCDEREC             TO TRRNCDE-FORMAT.               <PHLRMS>
           MOVE READR                  TO TRRNCDE-FUNCTION.             <PHLRMS>
                                                                        <PHLRMS>
           CALL 'TRRNCDEIO'            USING TRRNCDE-PARAMS.            <PHLRMS>
                                                                        <PHLRMS>
           IF  TRRNCDE-STATUZ          NOT = O-K                        <PHLRMS>
           AND                         NOT = MRNF                       <PHLRMS>
               MOVE TRRNCDE-STATUZ     TO SYSR-STATUZ                   <PHLRMS>
               MOVE TRRNCDE-PARAMS     TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  TRRNCDE-STATUZ          = O-K                            <PHLRMS>
               MOVE TRRNCDE-TRANDATE   TO WSAA-ASSIGNED-DATE            <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       A2890-EXIT.                                                      <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /                                                                 <PS008>
      *A2900-LOAD-RN-RECEIPT SECTION.                           <PS045> <CS009>
      *******************************                                   <CS009>
      *A2901-START.                                             <PS045> <CS009>
      *                                                                 <CS009>
      *    INITIALIZE                  RCPYCHR-PARAMS.          <PS045> <CS009>
      *    MOVE WSSP-CHDRKEY(4:8)      TO RCPYCHR-CHDRNUM.      <PS045> <CS009>
      *    MOVE SPACES                 TO RCPYCHR-RCTYPE        <PS045> <CS009>
      *    MOVE ZEROES                 TO RCPYCHR-RCSERNUM      <PS045> <CS009>
      *    MOVE RCPYCHRREC             TO RCPYCHR-FORMAT.       <PS045> <CS009>
      *    MOVE BEGN                   TO RCPYCHR-FUNCTION.     <PS045> <CS009>
      *                                                         <PS045> <CS009>
      *A2905-LOOP.                                              <PS045> <CS009>
      *    CALL 'RCPYCHRIO'            USING RCPYCHR-PARAMS.    <PS045> <CS009>
      *                                                         <PS045> <CS009>
      *    IF  RCPYCHR-STATUZ          NOT = O-K                <PS045> <CS009>
      *    AND                         NOT = ENDP               <PS045> <CS009>
      *        MOVE RCPYACT-PARAMS     TO SYSR-PARAMS           <PS045> <CS009>
      *      PERFORM 600-FATAL-ERROR                            <PS045> <CS009>
      *    END-IF.                                              <PS045> <CS009>
      *                                                         <PS045> <CS009>
      *    IF RCPYCHR-STATUZ           = ENDP                   <PS045> <CS009>
      *    OR RCPYCHR-CHDRNUM          NOT = WSSP-CHDRKEY(4:8)  <PS045> <CS009>
      *       GO TO A2909-EXIT                                  <PS045> <CS009>
      *    END-IF.                                              <PS045> <CS009>
                                                                        <CS009>
      *    MOVE RCPYCHR-RCTYPE         TO WSAA-RCPREPRTCD.      <PS045> <CS009>
      *    MOVE RCPYCHR-RCSERNUM       TO WSAA-RCPREPRNT.       <PS045> <CS009>
      *    PERFORM A2100-FIND-LOCATION.                         <PS045> <CS009>
      *    IF TRRNINF-PRINTDT          = VRCM-MAX-DATE          <PS045> <CS009>
      *    OR TRRNINF-TRANDATE         < WSAA-TODAY                     <CS009>
      *    OR ( TRRNINF-RCSTAT         NOT = 'AS'               <PS045> <CS009>
      *    AND                         NOT = 'RA')              <PS045> <CS009>
      *       GO TO A2908-NEXT                                  <PS045> <CS009>
      *    END-IF.                                              <PS045> <CS009>
                                                                        <CS009>
      *    MOVE WSAA-RCPREPRTCD        TO SR289-RCPREPRTCD.     <PS045> <CS009>
      *    MOVE WSAA-RCPREPRNT         TO SR289-RCPREPRNT.      <PS045> <CS009>
      *    MOVE RCPYCHR-DENTTYP        TO SR289-DENTTYP.        <PS045> <CS009>
      *    MOVE RCPYCHR-ENTYNUM        TO SR289-ENTYNO.         <PS045> <CS009>
      *    MOVE RCPYCHR-AMNT           TO WSAA-DOCORIGAMT.      <PS045> <CS009>
      *    GO TO A2909-EXIT.                                    <PS045> <CS009>
      *                                                                 <CS009>
      *A2908-NEXT.                                              <PS045> <CS009>
      *     MOVE NEXTR                 TO RCPYCHR-FUNCTION.     <PS045> <CS009>
      *     GO TO A2905-LOOP.                                   <PS045> <CS009>
      *                                                         <PS045> <CS009>
      *A2909-EXIT.                                              <PS045> <CS009>
      *     EXIT.                                               <PS045> <CS009>
      /                                                                 <PS008>
       A2910-LOAD-RN-RECEIPT SECTION.                                   <CS009>
      *******************************                                   <CS009>
       A2911-START.                                                     <CS009>
      *                                                                 <CS009>
           INITIALIZE                  RCPYPOL-PARAMS.                  <CS009>
           MOVE WSSP-CHDRKEY(4:8)      TO RCPYPOL-CHDRNUM.              <CS009>
           MOVE ZEROES                 TO RCPYPOL-DATEDUE.              <CS009>
           MOVE RCPYPOLREC             TO RCPYPOL-FORMAT.               <CS009>
           MOVE BEGN                   TO RCPYPOL-FUNCTION.             <CS009>
                                                                        <CS009>
       A2915-LOOP.                                                      <CS009>
           CALL 'RCPYPOLIO'            USING RCPYPOL-PARAMS.            <CS009>
                                                                        <CS009>
           IF  RCPYPOL-STATUZ          NOT = O-K                        <CS009>
           AND                         NOT = ENDP                       <CS009>
               MOVE RCPYPOL-PARAMS     TO SYSR-PARAMS                   <CS009>
             PERFORM 600-FATAL-ERROR                                    <CS009>
           END-IF.                                                      <CS009>
                                                                        <CS009>
           IF RCPYPOL-STATUZ           = ENDP                           <CS009>
           OR RCPYPOL-CHDRNUM          NOT = WSSP-CHDRKEY(4:8)          <CS009>
              GO TO A2919-EXIT                                          <CS009>
           END-IF.                                                      <CS009>
                                                                        <CS009>
           MOVE RCPYPOL-RCTYPE         TO WSAA-RCPREPRTCD.              <CS009>
           MOVE RCPYPOL-RCSERNUM       TO WSAA-RCPREPRNT.               <CS009>
           PERFORM A2100-FIND-LOCATION.                                 <CS009>
           IF TRRNINF-PRINTDT          = VRCM-MAX-DATE                  <CS009>
           OR ( TRRNINF-RCSTAT         NOT = 'AS'                       <CS009>
           AND                         NOT = 'RA')                      <CS009>
           OR ( RCPYPOL-PURCODE        NOT = 'AP' AND 'PD' AND 'PP' )   <CS009>
              GO TO A2918-NEXT                                          <CS009>
           END-IF.                                                      <CS009>
                                                                        <CS009>
           MOVE WSAA-RCPREPRTCD        TO SR289-RCPREPRTCD              <CS009>
                                          WSAA-TMP-RCPREPRTCD.          <CS009>
           MOVE WSAA-RCPREPRNT         TO SR289-RCPREPRNT               <CS009>
                                          WSAA-TMP-RCPREPRNT.           <CS009>
           MOVE RCPYPOL-DENTTYP        TO SR289-DENTTYP.                <CS009>
           MOVE RCPYPOL-ENTYNUM        TO SR289-ENTYNO.                 <CS009>
           MOVE RCPYPOL-AMNT           TO WSAA-DOCORIGAMT.              <CS009>
           GO TO A2919-EXIT.                                            <CS009>
                                                                        <CS009>
       A2918-NEXT.                                                      <CS009>
            MOVE NEXTR                 TO RCPYPOL-FUNCTION.             <CS009>
            GO TO A2915-LOOP.                                           <CS009>
      *                                                                 <CS009>
       A2919-EXIT.                                                      <CS009>
           EXIT.                                                        <CS009>
      /                                                                 <CS009>
       A2920-LOAD-RECEIPT SECTION.                                      <CS009>
      ****************************                                      <CS009>
       A2921-START.                                                     <CS009>
      *                                                                 <CS009>
           INITIALIZE                  RCPYPCD-PARAMS.                  <CS009>
           MOVE WSSP-CHDRKEY(4:8)      TO RCPYPCD-CHDRNUM.              <CS009>
           MOVE 'RI'                   TO RCPYPCD-PURCODE.              <CS009>
           MOVE ZEROES                 TO RCPYPCD-EFFDATE.              <CS009>
           MOVE RCPYPCDREC             TO RCPYPCD-FORMAT.               <CS009>
           MOVE BEGN                   TO RCPYPCD-FUNCTION.             <CS009>
      *                                                                 <CS009>
       A2925-LOOP.                                                      <CS009>
      *                                                                 <CS009>
           CALL 'RCPYPCDIO'            USING RCPYPCD-PARAMS.            <CS009>
                                                                        <CS009>
           IF RCPYPCD-STATUZ           NOT = O-K                        <CS009>
           AND                         NOT = ENDP                       <CS009>
              MOVE RCPYPCD-PARAMS      TO SYSR-PARAMS                   <CS009>
              PERFORM 600-FATAL-ERROR                                   <CS009>
           END-IF.                                                      <CS009>
                                                                        <CS009>
           IF RCPYPCD-STATUZ           = ENDP                           <CS009>
           OR RCPYPCD-PURCODE          NOT = 'RI'                       <CS009>
           OR RCPYPCD-CHDRNUM          NOT = WSSP-CHDRKEY(4:8)          <PHFX57>
              MOVE ENDP                TO RCPYPCD-STATUZ                <CS009>
              GO TO A2929-EXIT                                          <CS009>
           END-IF.                                                      <CS009>
                                                                        <CS009>
           MOVE RCPYPCD-RCTYPE         TO WSAA-RCPREPRTCD.              <CS009>
           MOVE RCPYPCD-RCSERNUM       TO WSAA-RCPREPRNT.               <CS009>
           PERFORM A2100-FIND-LOCATION.                                 <CS009>
           IF TRRNINF-PRINTDT          = VRCM-MAX-DATE                  <CS009>
           OR ( TRRNINF-RCSTAT         NOT = 'AS'                       <CS009>
           AND                         NOT = 'RA')                      <CS009>
              GO TO A2928-NEXT                                          <CS009>
           END-IF.                                                      <CS009>
                                                                        <CS009>
           MOVE WSAA-RCPREPRTCD        TO SR289-RCPREPRTCD              <CS009>
                                          WSAA-TMP-RCPREPRTCD.          <CS009>
           MOVE WSAA-RCPREPRNT         TO SR289-RCPREPRNT               <CS009>
                                          WSAA-TMP-RCPREPRNT.           <CS009>
           MOVE RCPYPCD-DENTTYP        TO SR289-DENTTYP.                <CS009>
           MOVE RCPYPCD-ENTYNUM        TO SR289-ENTYNO.                 <CS009>
           MOVE RCPYPCD-AMNT           TO WSAA-DOCORIGAMT.              <CS009>
           GO TO A2929-EXIT.                                            <CS009>
      *                                                                 <CS009>
       A2928-NEXT.                                                      <CS009>
           MOVE NEXTR                  TO RCPYPCD-FUNCTION.             <CS009>
           GO TO A2925-LOOP.                                            <CS009>
      *                                                                 <CS009>
       A2929-EXIT.                                                      <CS009>
           EXIT.                                                        <CS009>
