       IDENTIFICATION DIVISION.
       PROGRAM-ID.     B5349.
      *
      *
      * Copyright 1986-2020, Computer Sciences Corporation.
      *
      *REMARKS.
      *
      *                   BILLING
      *                   -------
      * Overview
      * ________
      *
      *   This program is part of the new 'Multi-Threading Batch
      * Performance' suite. It runs directly after B5348, which 'splits'
      * the PAYRPF according to the number of billing programs to run.
      * All references to the PAYR are via PAYXPF - a temporary file
      * holding all the PAYR records for this program to process.
      *
      * B5349 will perform all the processing for Billing that is
      * applicable to LIFE/400. A new subroutine will be called from
      * within the batch job to perform the Billing processing that
      * is applicable to FSU.
      *
      * Billing processes only those PAYR records which have the
      * BILLCD <= effective date (adjusted by the lead days on T6654)
      *
      * The following control totals are maintained within the program:
      *
      *       3 - PAYR records read
      *       2 - PAYR records bill suppress'd
      *       3 - PTRN records produced
      *       4 - Total Amount Billed.
      *       5 - LINS records created.
      *       6 - Total amount on LINS records.
      *       7 - CHDR records that have invalid statii.
      *       8 - CHDR records that are 'locked'.
      *       9 - Media records (BEXT) created. } Passed back from
      *      10 - Total amount on BEXT records. }     BILLREQ1
      *
      * 1000-INITIALISE SECTION
      * _______________________
      *
      *  -  Frequently-referenced tables are stored in working storage
      *     arrays to minimize disk IO. These tables include
      *     T3629 (Bank codes), T3620 (Billing channels), T6654
      *     (Billing control) and T6687 (Tax relief methods).
      *
      *  -  Issue an override to read the correct PAYXPF for this run.
      *     The CRTTMPF process has already created a file specific to
      *     the run using the  PAYXPF fields and is identified  by
      *     concatenating the following:-
      *
      *     'PAYX'
      *      BPRD-SYSTEM-PARAM04
      *      BSSC-SCHEDULE-NUMBER
      *
      *      eg PAYX2B0001,  for the first run
      *         PAYX2B0002,  for the second etc.
      *
      *     The number of threads would have been created by the
      *     CRTTMPF process given the parameters of the process
      *     definition of the CRTTMPF.
      *     To get the correct member of the above physical for B5349
      *     to read from, concatenate :-
      *
      *     'THREAD'
      *     BSPR-PROCESS-OCC-NUM
      *
      *  -  Initialise the static values of the LIFACMV copybook.
      *
      * 2000-READ SECTION
      * _________________
      *
      * -  Read the PAYX records sequentially incrementing the control
      *    total.
      *
      * -  If end of file move ENDP to WSSP-EDTERROR.
      *
      * 2500-EDIT SECTION
      * _________________
      *
      * -  Move OK to WSSP-EDTERROR.
      *
      * -  Read and validate the CHDR risk status and premium status
      *    against those obtained from T5679. If the status is invalid
      *    add 1 to control total 6 and move SPACES to WSSP-EDTERROR.
      *
      * -  Obtain the number of lead days from table T6654 using the
      *    contract type as the key.
      *
      * -  Use DATCON2 to increment the bill date by the number of lead
      *    days.
      *
      * -  If the bill date is within the bill supprfrom and billsuppto
      *    by-pass the contract and increment the control total.
      *    Move SPACES to WSSP-EDTERROR to prevent 3000-update
      *    processing.
      *
      * - 'Soft lock' the contract, if it is to be processed.
      *    If the contract is already 'locked' increment control
      *    total number 8 and  move SPACES to WSSP-EDTERROR.
      *
      *  3000-UPDATE SECTION
      *  ___________________
      *
      *  - Read & hold the payer record using logical file PAYR.
      *
      *  - Read client roles (CLRF), role 'PY' to obtain the payer
      *    number.
      *
      *  - Read the ACBL file using the sub-account code & type from
      *    the T5645 entry to obtain the amount in contract suspense.
      *    If there is enough in suspense then BEXT records do not have
      *    to be produced later in program.
      *
      *  - Multiply the amount in the suspense account by the general
      *    ledger sign that was obtained from T3695.
      *
      *    For PAYR records to be billed
      *  - Calculate the premium
      *
      *  - Calculate any automatic increases
      *
      *  - Call DATCON4 to advance the billed to date by the billing
      *    freq. Update this field on the PAYR & CHDR. (DATCON4
      *    performs the same DATCON2, but takes into consideration the
      *    original day and month  and therefore avoiding the problems of
      *    that DATCON2 had with end months'.)
      *
      *  - Call DATCON4 to increment the billing renewal date by the
      *    billing frequency. Update this field on the PAYR & CHDR.
      *
      *  - Using DATCON2 subtract the lead days from the billing
      *    renewal date calculate the next billing extract date for the
      *    PAYR record.
      *
      *  - Set up fields for the LINS record, converting the contract
      *    total to the billing amount.  If the contract is not the
      *    same as the billing currency, call XCVRT.
      *
      *  - Find the tax-relief method for this payer to be written to
      *    the LINS.
      *
      *  - Write LINS record.
      *
      *  - If this is a direct debit payment obtain mandate details
      *    & client bank account details (CLBA).  If there has been a
      *    DD dishonour update the mandate status and the billdate
      *    on the BEXT record.
      *
      *  - Calculate the tax relief on the billed premium. Convert this
      *    net amount if the contract currency and billing currencies
      *    are different by calling XCVRT.  If there is enough money
      *    in suspense (calculated earlier) to cover instalment,  THERE
      *    IS NO NEED TO PRODUCE A BEXT RECORD. Call BILLREQ1 and
      *    update the relevant control total.
      *
      *  - Produce a PTRN record for this instalment.
      *
      *  - If the PAYR billing renewal date is still not greater
      *    than the bill date produce another LINS.
      *
      *  - Rewrite CHDR & PAYR
      *
      *  - Unlock the contract.
      *
      * 4000-CLOSE SECTION
      * __________________
      *
      *  - Close Files.
      *
      *  - Delete the Override function for the PAYXPF file
      *
      *   Error Processing:
      *
      *     Perform the 600-FATAL-ERROR section. The
      *     SYSR-SYSERR-TYPE flag does not need to be set in this
      *     program, because MAINB takes care of a system errors.
      *
      *          (BATD processing is handled in MAINB)
      *
      *****************************************************************
      *              AMENDMENT  HISTORY                               *
      *****************************************************************
      * DATE.....   BY..   AMENDMENT...............  NUMBER
      * DD/MM/YY    X.X.   xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  NNN
      * 03/02/94    F.Mc.  AQR 4999
      *                    Batch Performance enhancements 9405      000
      *                    New program to replace B5060.
      *
      * 16/05/94    CJS.   AQR 5219.                                001
      *                    Further Batch Performance Enhancements.
      *                    Don't enter the 3000 section if we
      *                    know the policy does not require billing.
      *
      * 07/12/94    P.E.   AQR 5608.                                002
      *                    Deleted  lines of resetting Billing
      *                    suppression fields in CHDR/PAYR because
      *                    these caused billing suppression beyond
      *                    the current bill date to never occur.
      *
      *****************************************************************
      *
      * ......... New Version of the Amendment History.
      *
      *****************************************************************
      *           AMENDMENT  HISTORY
      *****************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....
      *
      * 29/08/95  01/01   D604IB       Jacco Landskroon
      *           Initalise the INSTAMT's 07 to 10 that are used by
      *           POLISY in BILLREQ1.
      *
      * 27/10/95  01/01   A06275       Andrew Wall
      *           Automatic Increases - The Billed Amount Control Total
      *           (CT04) does not include the amount of any pending
      *           automatic increases but the LINS Control Total (CT06)
      *           does.  When trying to reconcile these two totals, if
      *           a pending automatic increase exists for any of the
      *           contracts, this reconciliation will fail.
      *           Move the accumulation of the Billed Amount Control
      *           total after the calculation of any pending automatic
      *           increase.
      *           When accumulating the LINS Control Total, do not add
      *           the pending increase amount as it is already
      *           incorporated in LINSRNL-INSTAMT06.
      *
      * 11/01/96  01/01   D9604        Peter Evans                          *
      *           Changes added to cater for flexible premium contracts.    *
      *
      * 24/01/96  01/01   A06388       Mary Mac Rae
      *           Initialise the MANDATE details if its not
      *           a direct debit since previous Mandate details for
      *           direct debit contracts were being written to cash
      *           contracts which were processed after.
      *
      * 29/04/96  01/01   D96NUM       Fiona Martin
      *                   LUSEAMT
      *                   MANDAMT
      *                   NEWSUM
      *                   NEWINST
      *                   ORIGINST
      *                   ORIGSUM
      *                   INSTAMT
      *                   SINSTAMT
      *                   INSTTOT
      *                   CBILLAMT
      *                   The above field(s) have been increased in
      *                   size as part of the 1996 Numeric Field
      *                   Increase project. This is to enable users
      *                   to then convert (if they should wish to do
      *                   so) the fields to 'Monetary' and use the
      *                   SMART Variable Decimal Places technology
      *                   accordingly.
      *                                                                     *
      * 12/07/96  01/01   CAS1.0       Kristin McLeish
      *                   1. Initialise INCRRGP-PARAMS before first         *
      *                      read.                                          *
      *                   2. If the PAYRLIF history record is not found     *
      *                      in the 3310- section, do not bomb but use      *
      *                      the PAYR values instead.                       *
      *                   3. Since the WSAA-INCREASE-DUE value is not
      *                      calculated til the 3500-AUTOMATIC-INCREASE
      *                      section, and since it is not initialised
      *                      until then, it should not be used to
      *                      calculated the outstanding amount on the
      *                      PAYR in the 3300-CALC-PAYR-PREM section.
      *                                                                     *
      * 29/07/96  01/01.  CAS1.0       Cat Chiu
      *                   Set up transaction date and time correctly.
      *                                                                     *
      * 06/08/96  01/01   CAS1.0       Kristin McLeish                      *
      *                   Improve handling of system errors.                *
      *                                                                     *
      * 28/08/97  01/01   LA2106       Fred Chow of Hong Kong               *
      *           Life Asia Version 2.1 retrofits.                     ??   *
      *                                                                     *
      * 11/09/97  01/01   D9703        Fred Chow of Hong Kong               *
      *           Life Asia Version 2.1 retrofit.                      ??   *
      *           Setup DATESUB in PTRN.                                    *
      *                                                                     *
      * 07/11/97  01/01   FPS30        Tan Yoke Wah                         *
      *           Initialize INSTAMTx from 11 to 15 (GST enhancement)       *
      *                                                                     *
      * 14/08/97  01/01   A06923       Margaret Gill                        *
      *                                                                     *
      *          Amend 7200-section to include check for ENDP when          *
      *          reading FPCO.                                              *
      *                                                                     *
      * 15/09/97  01/01   A06856       Carlos Rivas                         *
      *           WSAA-EFFECTIVE-DATE-PLUS-CNTLEAD has been replaced by     *
      *           BILLCD, BSSC-EFFECTIVE-DATE was included in the same      *
      *           condition. Perform until was included in 3000-update      *
      *           section. The reason is that 'From Date' has not been      *
      *           taken into account in the Billing Suppression.            *
      *                                                                     *
      * 25/12/98  01/01   V4L001       Balaji                               *
      *           Premium Deposit Enhancement                               *
      *                                                                     *
      * 29/01/99  01/01   LAV4AQ       CSC - Dominic Dunbar                 *
      *           Change of WSAA-T6654-SIZE from 180 to 500            ??   *
      *                                                                     *
      * 18/02/99  01/01   P002         CSC - Ali Hartono                    *
      *           Incorporate the writing of LETC record by calling         *
      *           letter request subroutine (HLETRQS).                      *
      *
      * 03/06/99  01/01   V4LAQR       Minh
      * AQR#30    Logical file DDSURNL has its keys modified and MANDS TAT
      *           was added in therefor program B5349 needs to move in Max
      *           value into this key otherwise the record is always        *
      *           returning ENDP.So the program has been modified by        *
      *           moving '99'.
      *
      * 17/03/00  01/01   LFA1034      Oi Leng                              *
      *           Amend to create HDIV record with EFFDATE = PAYR-PTDATE    *
      *           instead of the current PAYR-BILLCD.                       *
      *           (fix taken from Pru Taiwan).                              *
      *                                                                     *
      * 02/03/01  01/01   PCL186       Vincent Lui (AXA - HK)               *
      *           Re-compile                                                *
      *                                                                     *
      * 17/08/01  01/01   PCPPRT       Lai AT    - CSC Malaysia             *
      *                 1.Table T6634 is changed to TR384.                  *
      *                 2.Call 'LETRQST' instead of 'HLETRQS' i.e. to       *
      *                   revert back to UK version.                        *
      *                                                                     *
      * 19/03/03  01/01   FA2984       Nagamuthu S.Ramaiah                  *
      *           Pass FSU Coy to BILLREQ1
      *                                                                     *
      * 20/08/05  01/01   V65L19       Fred Chow - CSC Hong Kong            *
      *           - remove IO using BEGNH to avoid locking in multi-   ??   *
      *             thread.                                                 *
      *                                                                     *
      * 29/05/06  01/01   LA3998       Chen Xu - CSC Beijing                *
      *           Pass value to the RLPDLON-LANGUAGE field of linkage.      *
      *                                                                     *
      * 09/10/06  01/01   V71L01       Chen Xu - CSC Beijing                *
      *           Initialize PTRN-PARAMS.                                   *
      *                                                                     *
      * 22/05/09  01/01   LA5134       Ali Hartono/FSG/CSC (Singapore       *
      *        1. Backdated billing problem (due to Reversal and Wind-      *
      *           forward situation or suspend billing)                     *
      *           - Fix PAYR OUTSTAMT when cross rerating                   *
      *           - Fix Currency Conversion Date                            *
      *        2. To update PAYR OUTSTAMT if any Auto Increase.             *
      *                                                                     *
      * 29/06/09  01/01   V74L01       Jinkee Guerrero/ASIA/CSC (Mala       *
      *           Service Tax Enhancement. Modify program to calculate      *
      *           tax due on premium and contract fee.                      *
      *                                                                     *
      * 16/07/09  Retrofitted by Fred Lee                                   *
      *           01/01   FA5078       CSC - Aidan Galligan                 *
      *           Move PAYR-NEXTDATE to new BLRQ-NEXTDATE field.            *
      *                                                                     *
      * 24/07/09  01/01   V74L01       Ronald Otero/FSG/CSC (Singapor       *
      *           rectify codes after testing/review                        *
      *                                                                     *
      * 13/09/10  01/01   V76F06       Nancie Lin/FSG/CSC (Hong Kong)       *
      *           Call ZRDECPLC to do rounding processing                   *
      *                                                                     *
      * 03/11/10  01/01   V76F05       Gang Liu/ASIA/CSC (China)            *
      *           Recompiled due to the change in the parameter screen      *
      *           S6671. S6671 was re-created based on the Post  SMART      *
      *           9311 parameter prompt screen layout to allow the  use     *
      *           of the pre-load parameter feature.                        *
      *                                                                     *
      * 31/01/11  01/01   V76F06       Xu Chen/ASIA/CSC (China)             *
      *           Do rounding when monetary amount <> 0.                    *
      *                                                                     *
      * 27/04/11  01/01   V76F13       Xu Chen/ASIA/CSC (China)             *
      *           Amend the program to cater for credit card processing.    *
      *                                                                     *
      * 04/09/13  01/01   PS001        Sang Nguyen - CSC Developer          *
      *           CHECK VALIDATION WOP OR SUSPENS ACCOUNT                   *
      *                                                                     *
      * 23/12/13  01/01   GAPPH2       Thanh Do                             *
      *           Get Despatch Address of Owner if any.                     *
      *                                                                     *
      * 22/05/14  01/01   PS011        Phung Vi Hao                         *
      *           Update the anniversary date into the COVR if              *
      *           this coverage have anniversary method in T5687
      *                                                                     *
      * 17/06/14  01/01   GAPPH2       Tuan Le                              *
      *           Skip print billing letter for staff office on TV070.      *
      *                                                                     *
      * 25/09/14  01/01   PHE003       Thanh Do                             *
      *           Move Due Date to Other Key of LETCPF.                     *
      *                                                                     *
      * 09/10/15  01/01   UL001        Thanh Do                             *
      *           Move WSAA-OLD-BTDATE for Flexible Premium instead of      *
      *           LINSRNL-INSTFROM.
      *                                                                     *
      * 23/10/15  01/01   UL001        Tuan Le                              *
      *           Premium Collection for UL product when Contract           *
      *           has been policy year not at time flexible.                *
      *                                                                     *
      * 28/12/15  01/01   UL001        Ha Nguyen - IT DEV                   *
      *           Allocate Premium for Flexible Premium                     *
      *           in must pay period.
      *                                                                     *
      * 14/04/16  01/01   UL001        Phi Tran - IT DEV                    *
      *           Skip process if not UL Product.                           *
      *                                                                     *
TDO   * 17/04/16  01/01   UL001        Thanh Do                             *
      *           Check No. of Yr IF for Flexible Premium starting          *
      *
      * 27/07/17  01/01   PHFX22       Phi Tran - IT DEV                    *
      *           Correct BTDATE.                                           *
      *                                                                     *
      * 06/04/18  01/01   UL005        Vo Thi Hong Duyen - IT               *
      *           Skip basic premium for UL product .                       *
      *                                                                     *
      * 02/05/18  01/01   PHFX30       Phi Tran - IT DEV                    *
      *           Update Amounts in LINSPF when LINS Records existed.       *
      *                                                                     *
      * 17/01/19  01/01   PHFX39       Phi Tran - IT DEV                    *
      *           Enhance new Rule for Commission/Allocation Charge.        *
      *                                                                     *
      * 18/06/19  01/01   UL010        Ha Nguyen - IT DEV                   *
      *           FLEXIBLE PREMIUM.                                         *
      *                                                                     *
      * 09/03/20  01/01   PHFX80       Ha Nguyen - IT DEV                   *
      *           Fix production Issue.                                     *
      *                                                                     *
      * 21/07/20  01/01   CS020        Van Bao Tuyen - IT                   *
      *           Change rule extract letter billing notice 15 days.        *
      *                                                                     *
      **DD/MM/YY*************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.                                IBM-AS400.
       OBJECT-COMPUTER.                                IBM-AS400.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PAYXPF                   ASSIGN TO DATABASE-PAYXPF.

       DATA DIVISION.
       FILE SECTION.

       FD  PAYXPF                          LABEL RECORDS STANDARD
           DATA RECORDS                    ARE PAYXPF-REC.
       01  PAYXPF-REC.
           COPY DDS-ALL-FORMATS            OF PAYXPF.

       WORKING-STORAGE SECTION.

       01  WSAA-PROG                   PIC X(05) VALUE 'B5349'.
       01  WSAA-VERSION                PIC X(02) VALUE '01'.
       01  WSAA-SUB                    PIC 9(02).                       <D9604>
       01  WSAA-SUB2                   PIC 9(02).                       <UL001>
       01  WSAA-TEST                   PIC 9(02).                       <UL001>
       01  WSAA-NUM-PERIOD             PIC S9(06)V9(05) VALUE 0.        <UL001>

       01  WSAA-STATUZ                 PIC X(01).                       <PS001>
      *  These fields are required by MAINB processing and should not
      *  be deleted.

       01  WSAA-COMMIT-CNT             PIC S9(08) COMP-3.
       01  WSAA-CYCLE-CNT              PIC S9(08) COMP-3.
       01  WSAA-CNT                    PIC 9(02).
       01  WSSP-EDTERROR               PIC X(04).
       01  WSAA-T5679-SUB              PIC 9(02).
       01  WSAA-T5729-SUB              PIC S9(5)    COMP-3.             <D9604>
       01  WSAA-POLYEAR-FOUND          PIC X(02).                       <PS011>
       01  WSAA-POLYEAR-STDATE         PIC S9(08).                      <PS011>
       01  WSAA-POLYEAR-ENDDATE        PIC S9(08).                      <PS011>
       01  WSAA-COUNTER-PRE            PIC 9(02).                       <PS011>
       01  WSAA-DATE-3YEAR             PIC S9(08).                      <PS011>
       01  WSAA-TU477-FLAG             PIC X(01).

      *  PAYX parameters.

       01  WSAA-QCMDEXC-LENGTH         PIC S9(10)V9(05)
                                                       COMP-3
                                                       VALUE 100.
       01  WSAA-QCMDEXC                PIC X(100).

       01  WSAA-PAYX-FN.
           03  FILLER                  PIC X(04)   VALUE 'PAYX'.
           03  WSAA-PAYX-RUNID         PIC X(02).
           03  WSAA-PAYX-JOBNO         PIC 9(04).

       01  WSAA-THREAD-MEMBER.
           03  FILLER                  PIC X(06)   VALUE 'THREAD'.
           03  WSAA-THREAD-NUMBER      PIC 9(03).
           03  FILLER                  PIC X.

      * Arrays to store regularly-referenced data, and reduce
      * the amount of accesses to the Itempf.
      * Array sizes are also delared  so that an increase of table size
      * does not impact its processing.

      *01  WSAA-T6654-SIZE             PIC S9(3) COMP-3  VALUE 180.     <LAV4AQ>
       01  WSAA-T6654-SIZE             PIC S9(3) COMP-3  VALUE 500.     <LAV4AQ>
       01  WSAA-T6654-KEY2.
           05  WSAA-BILLCHNL2          PIC X.
           05  WSAA-CNTTYPE2           PIC X(3).

       01  WSAA-TV103-SIZE             PIC S9(2) COMP-3  VALUE 20.      <UL005>
       01  WSAA-TV103-ARRAY.                                            <UL005>
           03  WSAA-TV103-REC  OCCURS 20                                <UL005>
                               ASCENDING KEY IS WSAA-TV103-KEY          <UL005>
                               INDEXED BY WSAA-TV103-IX.                <UL005>
              05  WSAA-TV103-KEY.                                       <UL005>
                 07  WSAA-TV103-CNTTYPE    PIC X(3) VALUE HIGH-VALUES.  <UL005>
TVAN   01  WSAA-TZ028-SIZE             PIC S9(2) COMP-3  VALUE 20.      <CS020>
       01  WSAA-TZ028-ARRAY.                                            <CS020>
           03  WSAA-TZ028-REC  OCCURS 20                                <CS020>
                               ASCENDING KEY IS WSAA-TZ028-KEY          <CS020>
                               INDEXED BY WSAA-TZ028-IX.                <CS020>
              05  WSAA-TZ028-KEY.                                       <CS020>
                 07  WSAA-TZ028-CNTTYPE    PIC X(3) VALUE HIGH-VALUES.  <CS020>
              05  WSAA-TZ028-DATA.                                      <CS020>
                 07  WSAA-TZ028-NOFYEAR    PIC 9(2).                    <CS020>
TVAN   01  WSAA-TZ028-FOUND                PIC X(01).                   <CS020>
       01  WSAA-SINSTAMT01                 PIC S9(15)V9(02) COMP-3.     <UL005>
       01  WSAA-SINSTAMT06                 PIC S9(15)V9(02) COMP-3.     <UL005>
       01  WSAA-TV103-FOUND                PIC X(01).                   <UL005>
       01  WSAA-INSTPREM-TR                PIC S9(15)V9(02) COMP-3.     <PHFX80>
       01  WSAA-INSTPREM-COVR              PIC S9(15)V9(02) COMP-3.     <PHFX80>
                                                                        <UL005>
       01  WSAA-T6654-ARRAY.
      **** 03  WSAA-T6654-REC OCCURS 180                                <LAV4AQ>
           03  WSAA-T6654-REC OCCURS 500                                <LAV4AQ>
                              ASCENDING KEY IS WSAA-T6654-KEY
                              INDEXED BY WSAA-T6654-IX.
               05  WSAA-T6654-KEY.
                   07  WSAA-T6654-BILLCHNL PIC X    VALUE HIGH-VALUES.
                   07  WSAA-T6654-CNTTYPE  PIC X(3) VALUE HIGH-VALUES.
               05  WSAA-T6654-DATA.
                   07  WSAA-T6654-COLLSUB  PIC X(8).
                   07  WSAA-T6654-LEADDAY  PIC S9(3).

       01  WSAA-T6687-SIZE             PIC S9(2) COMP-3  VALUE 10.
       01  WSAA-T6687-ARRAY.
           03  WSAA-T6687-REC  OCCURS 10
                               ASCENDING KEY IS WSAA-T6687-KEY
                               INDEXED BY WSAA-T6687-IX.
              05  WSAA-T6687-KEY.
                 07  WSAA-T6687-TAXRELMTH  PIC X(8) VALUE HIGH-VALUES.
              05  WSAA-T6687-DATA.
                 07  WSAA-T6687-TAXRELSUBR PIC X(8).

       01  WSAA-T3620-SIZE             PIC S9(2) COMP-3  VALUE 10.
       01  WSAA-T3620-ARRAY.
           03  WSAA-T3620-REC  OCCURS 10
                               ASCENDING KEY IS WSAA-T3620-KEY
                               INDEXED BY WSAA-T3620-IX.
              05  WSAA-T3620-KEY.
                 07  WSAA-T3620-BILLCHNL   PIC X(2) VALUE HIGH-VALUES.
              05  WSAA-T3620-DATA.
                 07  WSAA-T3620-DDIND      PIC X.
                 07  WSAA-T3620-CRCIND     PIC X.                       <V76F13>

       01  WSAA-T3629-SIZE             PIC S9(2) COMP-3  VALUE 20.
       01  WSAA-T3629-ARRAY.
           03  WSAA-T3629-REC  OCCURS 20
                               ASCENDING KEY IS WSAA-T3629-KEY
                               INDEXED BY WSAA-T3629-IX.
              05  WSAA-T3629-KEY.
                 07  WSAA-T3629-BILLCURR   PIC X(3)  VALUE HIGH-VALUES.
              05  WSAA-T3629-DATA.
                 07  WSAA-T3629-BANKCODE   PIC X(2).
                                                                        <D9604>
      *  Storage for T5729 table items.                         <D9604>
                                                                        <D9604>
       01  WSAA-FRQCY                       PIC X(02).                  <D9604>
       01  WSAA-DURATIONS.                                              <D9604>
           03  WSAA-DURATION                PIC S9(04) OCCURS 4.        <D9604>
       01  WSAA-OVERDUE-MINS.                                           <D9604>
           03  WSAA-OVERDUE-MIN             PIC S9(03) OCCURS 4.        <D9604>
      *01  WSAA-OLD-OUTSTAMT                PIC S9(13)V99.      <D96NUM><D9604>
       01  WSAA-OLD-OUTSTAMT                PIC S9(15)V99.              <D96NUM>
       01  WSAA-OVERDUE-PER                 PIC S9(9)V99.               <D9604>
      *01  WSAA-COVR-INC                    PIC S9(13)V99.      <D96NUM><D9604>
      *01  WSAA-TOT-AMT                     PIC S9(13)V99.      <D96NUM><D9604>
       01  WSAA-COVR-INC                    PIC S9(15)V99.              <D96NUM>
       01  WSAA-TOT-AMT                     PIC S9(15)V99.              <D96NUM>
       01  WSAA-COVERAGE                    PIC X(64).                  <D9604>
       01  WSAA-FIRST-BILL                  PIC X(01) VALUE 'Y'.        <LA5134>
       01  WSAA-CASHDATE                    PIC S9(08) COMP-3.          <LA5134>
                                                                        <D9604>
       01  WSAA-T5729-SIZE                  PIC 9(2) VALUE 20.          <D9604>
                                                                        <D9604>
       01  WSAA-T5729-ARRAY.                                            <D9604>
           03  WSAA-T5729-REC  OCCURS 20                                <D9604>
                               INDEXED BY WSAA-T5729-IX.                <D9604>
              05  WSAA-T5729-CURRFROM       PIC S9(08) COMP-3.          <D9604>
              05  WSAA-T5729-CURRTO         PIC S9(08) COMP-3.          <D9604>
              05  WSAA-T5729-KEY.                                       <D9604>
                 07  WSAA-T5667-CNTTYPE     PIC X(3) VALUE SPACES.      <D9604>
              05  WSAA-T5729-DATA.                                      <UL001>
                 07  WSAA-T5729-FRQCYS.                                 <UL001>
                   10  WSAA-T5729-FRQCY      PIC X(2) OCCURS 06.        <UL001>
                 07  WSAA-T5729-DURATIONAS.                             <UL001>
                   10  WSAA-T5729-DURATIONA  PIC S9(04)                 <UL001>
                                             OCCURS 04.                 <UL001>
                 07  WSAA-T5729-DURATIONBS.                             <UL001>
                   10  WSAA-T5729-DURATIONB  PIC S9(04)                 <UL001>
                                             OCCURS 04.                 <UL001>
                 07  WSAA-T5729-DURATIONCS.                             <UL001>
                   10  WSAA-T5729-DURATIONC  PIC S9(04)                 <UL001>
                                             OCCURS 04.                 <UL001>
                 07  WSAA-T5729-DURATIONDS.                             <UL001>
                   10  WSAA-T5729-DURATIOND  PIC S9(04)                 <UL001>
                                             OCCURS 04.                 <UL001>
                 07  WSAA-T5729-DURATIONES.                             <UL001>
                   10  WSAA-T5729-DURATIONE  PIC S9(04)                 <UL001>
                                             OCCURS 04.                 <UL001>
                 07  WSAA-T5729-DURATIONFS.                             <UL001>
                   10  WSAA-T5729-DURATIONF  PIC S9(04)                 <UL001>
                                             OCCURS 04.                 <UL001>
                 07  WSAA-T5729-TARGET-MINAS.                           <UL001>
                   10  WSAA-T5729-TARGET-MINA   PIC S9(03)              <UL001>
                                             OCCURS 04.                 <UL001>
                 07  WSAA-T5729-TARGET-MINBS.                           <UL001>
                   10  WSAA-T5729-TARGET-MINB   PIC S9(03)              <UL001>
                                             OCCURS 04.                 <UL001>
                 07  WSAA-T5729-TARGET-MINCS.                           <UL001>
                   10  WSAA-T5729-TARGET-MINC   PIC S9(03)              <UL001>
                                             OCCURS 04.                 <UL001>
                 07  WSAA-T5729-TARGET-MINDS.                           <UL001>
                   10  WSAA-T5729-TARGET-MIND   PIC S9(03)              <UL001>
                                             OCCURS 04.                 <UL001>
                 07  WSAA-T5729-TARGET-MINES.                           <UL001>
                   10  WSAA-T5729-TARGET-MINE   PIC S9(03)              <UL001>
                                             OCCURS 04.                 <UL001>
                 07  WSAA-T5729-TARGET-MINFS.                           <UL001>
                   10  WSAA-T5729-TARGET-MINF   PIC S9(03)              <UL001>
                                             OCCURS 04.                 <UL001>
                 07  WSAA-T5729-TARGET-MAXAS.                           <UL001>
                   10  WSAA-T5729-TARGET-MAXA   PIC S9(05)              <UL001>
                                             OCCURS 04.                 <UL001>
                 07  WSAA-T5729-TARGET-MAXBS.                           <UL001>
                   10  WSAA-T5729-TARGET-MAXB   PIC S9(05)              <UL001>
                                             OCCURS 04.                 <UL001>
                 07  WSAA-T5729-TARGET-MAXCS.                           <UL001>
                   10  WSAA-T5729-TARGET-MAXC   PIC S9(05)              <UL001>
                                             OCCURS 04.                 <UL001>
                 07  WSAA-T5729-TARGET-MAXDS.                           <UL001>
                   10  WSAA-T5729-TARGET-MAXD   PIC S9(05)              <UL001>
                                             OCCURS 04.                 <UL001>
                 07  WSAA-T5729-TARGET-MAXES.                           <UL001>
                   10  WSAA-T5729-TARGET-MAXE   PIC S9(05)              <UL001>
                                             OCCURS 04.                 <UL001>
                 07  WSAA-T5729-TARGET-MAXFS.                           <UL001>
                   10  WSAA-T5729-TARGET-MAXF   PIC S9(05)              <UL001>
                                             OCCURS 04.                 <UL001>
                 07  WSAA-T5729-OVERDUE-MINAS.                          <UL001>
                   10  WSAA-T5729-OVERDUE-MINA  PIC S9(03)              <UL001>
                                             OCCURS 04.                 <UL001>
                 07  WSAA-T5729-OVERDUE-MINBS.                          <UL001>
                   10  WSAA-T5729-OVERDUE-MINB  PIC S9(03)              <UL001>
                                             OCCURS 04.                 <UL001>
                 07  WSAA-T5729-OVERDUE-MINCS.                          <UL001>
                   10  WSAA-T5729-OVERDUE-MINC  PIC S9(03)              <UL001>
                                             OCCURS 04.                 <UL001>
                 07  WSAA-T5729-OVERDUE-MINDS.                          <UL001>
                   10  WSAA-T5729-OVERDUE-MIND  PIC S9(03)              <UL001>
                                             OCCURS 04.                 <UL001>
                 07  WSAA-T5729-OVERDUE-MINES.                          <UL001>
                   10  WSAA-T5729-OVERDUE-MINE  PIC S9(03)              <UL001>
                                             OCCURS 04.                 <UL001>
                 07  WSAA-T5729-OVERDUE-MINFS.                          <UL001>
                   10  WSAA-T5729-OVERDUE-MINF  PIC S9(03)              <UL001>
                                             OCCURS 04.                 <UL001>
                                                                        <UL001>
        01  WSAA-FRQCY                       PIC X(02).                 <UL001>
                                                                        <UL001>
       01  WSAA-FREQ-FOUND                    PIC X     VALUE 'N'.      <UL001>
           88  FREQ-FOUND                               VALUE 'Y'.      <UL001>
                                                                        <UL001>
       01  WSAA-DURATION-FOUND                PIC X     VALUE 'N'.      <UL001>
           88  DURATION-FOUND                           VALUE 'Y'.      <UL001>
           88  DURATION-NOT-FOUND                       VALUE 'N'.      <UL001>
                                                                        <LA2106>
       01  WSAA-LAST-CAP-DATE              PIC S9(8) COMP-3.            <LA2106>
                                                                        <LA2106>
      *01  WSAA-ITEM-T6634.                                     <PCPPRT><P002>
       01  WSAA-ITEM-TR384.                                             <PCPPRT>
           03  WSAA-ITEM-CNTTYPE           PIC X(03).                   <P002>
           03  WSAA-ITEM-BATCTRCDE         PIC X(04).                   <P002>
           03  FILLER                      PIC X(01).                   <P002>
                                                                        <V74L01>
       01  WSAA-TR52E-KEY.                                              <V74L01>
           03  WSAA-TR52E-TXCODE       PIC X(01).                       <V74L01>
           03  WSAA-TR52E-CNTTYPE      PIC X(03).                       <V74L01>
           03  WSAA-TR52E-CRTABLE      PIC X(04).                       <V74L01>
                                                                        <V74L01>
       01  WSAA-RATE-ITEM.                                              <V74L01>
           03  WSAA-CNT-CURR           PIC X(03).                       <V74L01>
           03  WSAA-TXITEM             PIC X(04).                       <V74L01>
           03  FILLER                  PIC X.                           <V74L01>
                                                                        <GAPPH2>
       01  WSAA-ITEM-KEY.                                               <GAPPH2>
           03  WSAA-ITEMPFX                   PIC X(02).                <GAPPH2>
           03  WSAA-ITEMCOY                   PIC X(01).                <GAPPH2>
           03  WSAA-ITEMTABL                  PIC X(05).                <GAPPH2>
           03  WSAA-ITEMITEM                  PIC X(08).                <GAPPH2>
                                                                        <V74L01>
       01  WSAA-TAX                    PIC S9(15)V99 COMP-3.            <V74L01>
       01  WSAA-BILL-OUTST             PIC S9(15)V99 COMP-3 VALUE ZERO. <V74L01>
       01  WSAA-BILL-AMOUNT            PIC S9(15)V99 COMP-3 VALUE ZERO. <V74L01>
       01  WSAA-INCR-BINSTPREM         PIC S9(15)V99 COMP-3 VALUE ZERO. <V74L01>
       01  WSAA-INCR-INSTPREM          PIC S9(15)V99 COMP-3 VALUE ZERO. <V74L01>
       01  WSAA-TRANREF                PIC X(30).                       <V74L01>
       01  WSAA-TR517-IX               PIC S9(02).                      <V74L01>
       01  WSAA-TR517-ITEM             PIC X(08).                       <V74L01>
       01  WSAA-COVR-CALC-TAX          PIC X(01).                       <V74L01>
       01  WSAA-WOP-FOUND              PIC X(01) VALUE 'N'.             <V74L01>
           88 WOP-FOUND                          VALUE 'Y'.             <V74L01>
       01  WSAA-VALID-COVR             PIC X(01) VALUE 'N'.             <V74L01>
           88 VALID-COVR                         VALUE 'Y'.             <V74L01>
                                                                        <V74L01>
                                                                        <P002>
      * The formats BUPA BSSC BPRD BSPR and BMSG are required by MAINB
      *  processing and should not be deleted.

       01  FORMATS.
           03  BMSGREC                 PIC X(10) VALUE 'BMSGREC'.
           03  BPRDREC                 PIC X(10) VALUE 'BPRDREC'.
           03  BSPRREC                 PIC X(10) VALUE 'BSPRREC'.
           03  BSSCREC                 PIC X(10) VALUE 'BSSCREC'.
           03  BUPAREC                 PIC X(10) VALUE 'BUPAREC'.
           03  ACBLREC                 PIC X(07) VALUE 'ACBLREC'.
           03  CHDRLIFREC              PIC X(10) VALUE 'CHDRLIFREC'.
           03  CLBADDBREC              PIC X(10) VALUE 'CLBADDBREC'.
           03  DDSURNLREC              PIC X(10) VALUE 'DDSURNLREC'.
           03  INCRRGPREC              PIC X(10) VALUE 'INCRRGPREC'.
           03  ITEMREC                 PIC X(07) VALUE 'ITEMREC'.
           03  ITDMREC                 PIC X(07) VALUE 'ITDMREC'.       <D9604>
           03  LINSRNLREC              PIC X(10) VALUE 'LINSRNLREC'.
           03  PAYRLIFREC              PIC X(10) VALUE 'PAYRLIFREC'.
           03  PAYRREC                 PIC X(07) VALUE 'PAYRREC'.
           03  PTRNREC                 PIC X(07) VALUE 'PTRNREC'.
           03  SFTLOCKREC              PIC X(10) VALUE 'SFTLOCKREC'.
           03  FPRMREC                 PIC X(10) VALUE 'FPRMREC'.       <D9604>
           03  FPCOREC                 PIC X(10) VALUE 'FPCOREC'.       <D9604>
           03  COVRLNBREC              PIC X(10) VALUE 'COVRLNBREC'.    <D9604>
           03  COVRREC                 PIC X(10) VALUE 'COVRREC'.       <PS011>
           03  HDIVCSHREC              PIC X(10) VALUE 'HDIVCSHREC'.    <LA2106>
           03  HDIVREC                 PIC X(10) VALUE 'HDIVREC'.       <LA2106>
           03  HDISREC                 PIC X(10) VALUE 'HDISREC'.       <LA2106>
           03  HCSDREC                 PIC X(10) VALUE 'HCSDREC'.       <LA2106>
           03  TAXDREC                 PIC X(10) VALUE 'TAXDREC'.       <V74L01>
           03  INCRREC                 PIC X(10) VALUE 'INCRREC'.       <V74L01>
           03  SCLTENQREC              PIC X(10) VALUE 'SCLTENQREC'.    <GAPPH2>
           03  SCNTENQREC              PIC X(10) VALUE 'SCNTENQREC'.    <GAPPH2>
           03  LIFEENQREC              PIC X(10) VALUE 'LIFEENQREC'.    <GAPPH2>
           03  SCLTREC                 PIC X(10) VALUE 'SCLTREC'.       <GAPPH2>
           03  SCNTREC                 PIC X(10) VALUE 'SCNTREC'.       <GAPPH2>
           03  ACMVINFREC              PIC X(10) VALUE 'ACMVINFREC'.    <UL001>
           03  PTRNENQREC                 PIC X(10) VALUE 'PTRNREC'.
           03  LINSFPRREC              PIC X(10) VALUE 'LINSFPRREC'.    <UL001>
           03  LINSBLCREC              PIC X(10) VALUE 'LINSBLCREC'.    <PHFX30>
TUYEN      03  ZPPIENQREC              PIC X(10) VALUE 'ZPPIENQREC'.
           03  COVRENQREC              PIC X(10) VALUE 'COVRENQREC'.
TUYEN      03  ACMVENQREC              PIC X(10) VALUE 'ACMVENQREC'.

       01  ERRORS.
           03  H791                    PIC X(4)  VALUE 'H791'.
           03  IVRM                    PIC X(04) VALUE 'IVRM'.
           03  F035                    PIC X(4)  VALUE 'F035'.
           03  E308                    PIC X(4)  VALUE 'E308'.          <D9604>
           03  I086                    PIC X(4)  VALUE 'I086'.          <UL001>

       01  TABLES.
           03  T1692                   PIC X(06) VALUE 'T1692'.
           03  T1693                   PIC X(06) VALUE 'T1693'.
           03  T3629                   PIC X(06) VALUE 'T3629'.
           03  T3620                   PIC X(05) VALUE 'T3620'.
           03  T3695                   PIC X(05) VALUE 'T3695'.
           03  T5679                   PIC X(05) VALUE 'T5679'.
           03  T5645                   PIC X(05) VALUE 'T5645'.         <P002>
      **** 03  T6634                   PIC X(05) VALUE 'T6634'. <PCPPRT><P002>
           03  TR384                   PIC X(05) VALUE 'TR384'.         <PCPPRT>
           03  T6654                   PIC X(05) VALUE 'T6654'.
           03  T6687                   PIC X(05) VALUE 'T6687'.
           03  T5729                   PIC X(05) VALUE 'T5729'.         <D9604>
           03  TR517                   PIC X(05) VALUE 'TR517'.         <V74L01>
           03  TR52D                   PIC X(05) VALUE 'TR52D'.         <V74L01>
           03  TR52E                   PIC X(05) VALUE 'TR52E'.         <V74L01>
           03  T5687                   PIC X(05) VALUE 'T5687'.         <PS011>
           03  TV070                   PIC X(05) VALUE 'TV070'.         <GAPPH2>
           03  TV078                   PIC X(05) VALUE 'TV078'.
           03  TU477                   PIC X(05) VALUE 'TU477'.
           03  TV103                   PIC X(05) VALUE 'TV103'.         <UL005>
TVAN       03  TZ028                   PIC X(05) VALUE 'TZ028'.         <CS020>

       01  CONTROL-TOTALS.
           03  CT01                    PIC 9(02) VALUE 01.
           03  CT02                    PIC 9(02) VALUE 02.
           03  CT03                    PIC 9(02) VALUE 03.
           03  CT04                    PIC 9(02) VALUE 04.
           03  CT05                    PIC 9(02) VALUE 05.
           03  CT06                    PIC 9(02) VALUE 06.
           03  CT07                    PIC 9(02) VALUE 07.
           03  CT08                    PIC 9(02) VALUE 08.
           03  CT09                    PIC 9(02) VALUE 09.
           03  CT10                    PIC 9(02) VALUE 10.
           03  CT11                    PIC 9(02) VALUE 11.              <D9604>
           03  CT12                    PIC 9(02) VALUE 12.              <D9604>

       01  WSAA-OLD-BTDATE             PIC S9(08) COMP-3  VALUE 0.      <PHFX30>
       01  WSAA-EFFDATE-PLUS-CNTLEAD   PIC S9(08) COMP-3  VALUE 0.
       01  WSAA-SUSP-AVAIL             PIC S9(15)V9(02) COMP-3 VALUE 0.
       01  WSAA-OLD-BILLCD             PIC 9(08).
       01  WSAA-OLD-NEXTDATE           PIC S9(08) COMP-3  VALUE 0.      <FA5078>

   ****01  WSAA-INCREASE-DUE           PIC S9(11)V9(02) COMP-3.         <D96NUM>
       01  WSAA-INCREASE-DUE           PIC S9(15)V9(02) COMP-3.         <D96NUM>
       01  WSAA-INST-SUB               PIC S9(03) COMP-3 VALUE 0.

       01  WSAA-FORENUM.
           03 WSAA-CHDRNUM             PIC X(8).
           03 WSAA-PAYRSEQNO           PIC X(1).

       01  WSAA-T6654-ITEM.
           05  WSAA-BILLCHNL           PIC X(01).
           05  WSAA-CNTTYPE            PIC X(03).

       01  WSAA-VALID-CHDR             PIC X(01) VALUE 'N'.
          88  VALID-CONTRACT                     VALUE 'Y'.

       01  WSAA-FLEX-PREM              PIC X(1).                        <D9604>
           88 FLEXIBLE-PREMIUM-CONTRACT     VALUE 'Y'.                  <D9604>
           88 NOT-FLEXIBLE-PREMIUM-CONTRACT VALUE 'N'.                  <D9604>
                                                                        <D9604>
       01  WSAA-VALID-COVERAGE         PIC X     VALUE 'N'.             <D9604>
           88  VALID-COVERAGE                    VALUE 'Y'.             <D9604>
                                                                        <D9604>
                                                                        <UL005>
       01  WSAA-COVR-COUNT             PIC 9(03).                       <UL005>
       01  WSAA-CORRECT-PAYR-FOUND     PIC X(01).                       <CAS1.0>
                                                                        <CAS1.0>
       01  WSAA-GOT-PAYR-AT-BTDATE      PIC X(01).

       01  WSYS-SYSTEM-ERROR-PARAMS.
           03  WSYS-PAYRKEY.
              05  WSYS-CHDRNUM         PIC X(8).
              05  FILLER               PIC X.
              05  WSYS-BILLCD          PIC 9(8).
              05  FILLER               PIC X(3).
           03  WSYS-SYSPARAMS          PIC X(77).
                                                                        <LA2106>
       01  WSAA-PRINT-FLG              PIC X(01) VALUE SPACES.          <GAPPH2>
       01  WSAA-CLNTNUM                PIC X(08) VALUE SPACES.          <GAPPH2>
       01  WSAA-JRNSEQ                 PIC S9(03) COMP-3.               <LA2106>
       01  WSAA-ALL-DVD-TOT            PIC S9(15)V9(02) COMP-3.         <LA2106>
       01  WSAA-RUN-DVD-TOT            PIC S9(15)V9(02) COMP-3.         <LA2106>
       01  WSAA-TFR-AMT                PIC S9(15)V9(02) COMP-3.         <LA2106>
       01  WSAA-SHORTFALL              PIC S9(15)V9(02) COMP-3.         <LA2106>
       01  WSAA-PREM-SUSP              PIC S9(15)V9(02) COMP-3.         <LA2106>
       01  WSAA-DVD-SUSP               PIC S9(15)V9(02) COMP-3.         <LA2106>
       01  WSAA-IDX                    PIC S9(03) COMP-3.               <LA2106>
       01  WSAA-NO-OF-HDIS             PIC S9(03) COMP-3.               <LA2106>
       01  WSAA-HDIS-ARRAY.                                             <LA2106>
         03  WSAA-HDIS-REC             OCCURS 99.                       <LA2106>
             05  WSAA-HDIS-LIFE        PIC X(02).                       <LA2106>
             05  WSAA-HDIS-COVERAGE    PIC X(02).                       <LA2106>
             05  WSAA-HDIS-RIDER       PIC X(02).                       <LA2106>
             05  WSAA-HDIS-JLIFE       PIC X(02).                       <LA2106>
             05  WSAA-HDIS-PLNSFX      PIC 9(04).                       <LA2106>
             05  WSAA-ZDIVOPT          PIC X(04).                       <LA2106>
             05  WSAA-ZCSHDIVMTH       PIC X(04).                       <LA2106>
             05  WSAA-NEXT-CAP-DATE    PIC S9(08).                      <LA2106>
             05  WSAA-DVD-TOT          PIC S9(10)V9(07).                <LA2106>
             05  WSAA-DVD-SHARE        PIC S9(10)V9(07).                <LA2106>
                                                                        <UL001>
       01 WSAA-OVER-TARGET             PIC S9(15)V9(02) COMP-3.         <UL001>
       01 WSAA-PREM-NEED               PIC S9(15)V9(02) COMP-3.         <UL001>
       01 WSAA-PREM-TARGT              PIC S9(15)V9(02) COMP-3.         <UL001>
       01 WSAA-PRMTAR-APPL             PIC S9(15)V9(02) COMP-3.         <UL001>
                                                                        <UL001>
       01 WSAA-ENDYEAR-DATE            PIC S9(08).                      <UL001>
       01 WSAA-STRYEAR-DATE            PIC S9(08).                      <UL001>
                                                                        <UL001>
       01 WSAA-MUSTPAY                 PIC 9(01).                       <UL001>
       01 WSAA-FOUND                   PIC X(01).                       <UL001>
       01 WSAA-FOUND-REINS             PIC X(01) VALUE SPACES.          <UL001>
       01 WSAA-ALLOCATE-PRM            PIC X(01).                       <UL001>
                                                                        <UL001>
       01 WSAA-LE                      PIC X(02) VALUE 'LE'.            <UL001>
       01 WSAA-LP                      PIC X(02) VALUE 'LP'.            <UL001>
       01 WSAA-BILLFREQ                PIC 9(02).                       <UL001>
       01 WSAA-NUMBER                  PIC 9(02).                       <UL001>
       01 WSAA-IDX1                    PIC S9(03) COMP-3.               <UL001>
       01 WSAA-PTDATE                  PIC S9(08) VALUE  ZEROES.
       01 WSAA-DATE-YEAREND            PIC S9(08) VALUE  ZEROES.
       01 WSAA-POLYEAR                 PIC 9(02) VALUE ZEROES.
       01  WSAA-BATCTRCDE                  PIC X(04) VALUE 'TA85'.
                                                                        <UL001>
       01 WSAA-SKIP                    PIC X(01) VALUE 'N'.             <UL001>
TUYEN  01 WSAA-COUNT-NUM-BILL          PIC 9(02) VALUE 0.
       01 WSAA-PREM-PAID-AMT           PIC S9(15)V9(02) COMP-3 VALUE 0.
       01 WSAA-PLAN-PREM-AMT           PIC S9(15)V9(02) COMP-3 VALUE 0.
       01 WSAA-PREM-PL-TOPUP-AMT       PIC S9(15)V9(02) COMP-3 VALUE 0.
       01 WSAA-SACSCURBAL              PIC S9(15)V9(02) COMP-3 VALUE 0.
       01 WSAA-POLYR-IF                PIC S9(02) VALUE 0.
       01 WSAA-LAST-AS                 PIC 9(08) VALUE 0.
       01 WSAA-NEXT-AS                 PIC 9(08) VALUE 0.               <CS020>
       01 WSAA-BILLING-DATE            PIC S9(08) COMP-3  VALUE 0.      <CS020>
       01 WSAA-DUE-DATE-1ST            PIC 9(08) VALUE 0.
       01 WSAA-DUE-DATE-2ST            PIC 9(08) VALUE 0.
       01 WSAA-DUE-DATE-3ST            PIC 9(08) VALUE 0.
       01 WSAA-DUE-DATE-4ST            PIC 9(08) VALUE 0.
       01 WSAA-DUE-DATE-5ST            PIC 9(08) VALUE 0.
       01 WSAA-DUE-DATE-6ST            PIC 9(08) VALUE 0.
       01 WSAA-DUE-DATE-7ST            PIC 9(08) VALUE 0.
       01 WSAA-DUE-DATE-8ST            PIC 9(08) VALUE 0.
       01 WSAA-DUE-DATE-9ST            PIC 9(08) VALUE 0.
       01 WSAA-DUE-DATE-10ST           PIC 9(08) VALUE 0.
       01 WSAA-DUE-DATE-11ST           PIC 9(08) VALUE 0.
       01 WSAA-DUE-DATE-12ST           PIC 9(08) VALUE 0.
       01 WSAA-NUM-FREQ                PIC 9(02) VALUE 0.
       01 WSAA-RLDGACCT.
          03 WSAA-RLDGACCT-CHDRNUM     PIC X(08).
          03 WSAA-RLDGACCT-LIFE        PIC X(02).
          03 WSAA-RLDGACCT-COVERAGE    PIC X(02).
          03 WSAA-RLDGACCT-RIDER       PIC X(02).
          03 WSAA-RLDGACCT-PLSFX       PIC X(02).
tuyen *
      *  MAINB copybooks

           COPY BSSCSKM.
           COPY BSPRSKM.
           COPY BUPASKM.
           COPY BPRDSKM.
           COPY BATCUPREC.
           COPY BATCDORREC.
           COPY COMMONPAR.
           COPY VARCOM.
           COPY CONLOGREC.
           COPY SYSERRREC.

      *  Subroutine copybooks

           COPY CONTOTREC.

           COPY DATCON2REC.
           COPY DATCON3REC.                                             <D9604>
           COPY DATCON4REC.
           COPY BILLREQREC.
           COPY CONLINKREC.
           COPY PRASREC.
           COPY LIFACMVREC.                                             <LA2106>
           COPY RLPDLONREC.                                             <V4L001>
           COPY LETRQSTREC.                                             <P002>
           COPY TXCALCREC.                                              <V74L01>
           COPY ZRDECPLREC.                                             <V76F06>
           COPY PTRNENQSKM.
           COPY LINSFPRSKM.                                             <UL001>
           COPY LINSBLCSKM.                                             <PHFX30>

      *  Table layouts

           COPY T3620REC.
           COPY T3629REC.
           COPY T3695REC.
           COPY T5645REC.
           COPY T5679REC.
      **** COPY T6634REC.                                       <PCPPRT><P002>
           COPY TR384REC.                                               <PCPPRT>
           COPY T6654REC.
           COPY T6687REC.
           COPY T5729REC.                                               <D9604>
           COPY TR517REC.                                               <V74L01>
           COPY TR52DREC.                                               <V74L01>
           COPY TR52EREC.                                               <V74L01>
           COPY T5687REC.                                               <PS011>
           COPY TV070REC.                                               <GAPPH2>
TVAN       COPY TZ028REC.                                               <CS020>

      *  Used by B5349

           COPY P6671PAR.
           COPY SFTLOCKREC.
           COPY ACBLSKM.
           COPY CHDRLIFSKM.
           COPY CLBADDBSKM.
           COPY CLRFSKM.
           COPY CLNTSKM.
           COPY DDSURNLSKM.
           COPY INCRRGPSKM.
           COPY ITEMSKM.
           COPY ITDMSKM.                                                <D9604>
           COPY LINSRNLSKM.
           COPY MANDSKM.
           COPY PAYRSKM.
           COPY PAYRLIFSKM.
           COPY PTRNSKM.
           COPY FPRMSKM.                                                <D9604>
           COPY FPCOSKM.                                                <D9604>
           COPY COVRLNBSKM.                                             <D9604>
           COPY COVRSKM.                                                <PS011>
           COPY HDIVCSHSKM.                                             <LA2106>
           COPY HDIVSKM.                                                <LA2106>
           COPY HDISSKM.                                                <LA2106>
           COPY HCSDSKM.                                                <LA2106>
           COPY TAXDSKM.                                                <V74L01>
           COPY INCRSKM.                                                <V74L01>
           COPY SCLTSKM.                                                <GAPPH2>
           COPY SCNTSKM.                                                <GAPPH2>
           COPY SCLTENQSKM.                                             <GAPPH2>
           COPY SCNTENQSKM.                                             <GAPPH2>
           COPY LIFEENQSKM.                                             <GAPPH2>
           COPY LETCOKCPY.                                              <PHE003>
           COPY ACMVINFSKM.                                             <UL001>
           COPY TV078REC.
TUYEN      COPY ZPPIENQSKM.
           COPY COVRENQSKM.
           COPY ACMVENQSKM.
TUYEN      COPY ACBLENQSKM.


       LINKAGE SECTION.
      *****************

        01  LSAA-STATUZ                PIC X(04).
        01  LSAA-BSSCREC               PIC X(1024).
        01  LSAA-BSPRREC               PIC X(1024).
        01  LSAA-BPRDREC               PIC X(1024).
        01  LSAA-BUPAREC               PIC X(1024).

       PROCEDURE DIVISION        USING LSAA-STATUZ
                                       LSAA-BSSCREC
                                       LSAA-BSPRREC
                                       LSAA-BPRDREC
                                       LSAA-BUPAREC.


           COPY MAINB.

       0900-RESTART SECTION.
      **********************
       0910-RESTART.

      * Restarting of this program is handled by MAINB,
      * using a restart method of '3'.

       0990-EXIT.
           EXIT.

       1000-INITIALISE SECTION.
      *************************
       1010-INITIALISE.

           IF BPRD-RESTART-METHOD NOT  = '3'
               MOVE IVRM               TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR
           END-IF.

      *  Point to correct member of PAYXPF.

           ACCEPT VRCM-DATE            FROM DATE.                       <CAS1.0>
           MOVE BPRD-SYSTEM-PARAM04    TO WSAA-PAYX-RUNID.
           MOVE BSSC-SCHEDULE-NUMBER   TO WSAA-PAYX-JOBNO.
           MOVE BSPR-PROCESS-OCC-NUM   TO WSAA-THREAD-NUMBER.

           STRING
               'OVRDBF FILE(PAYXPF) TOFILE('
                                       DELIMITED BY SIZE
                BPRD-RUN-LIBRARY       DELIMITED BY SPACES
               '/' WSAA-PAYX-FN ') '
               'MBR(' WSAA-THREAD-MEMBER ')'
               ' SEQONLY(*YES 1000)'
                                       DELIMITED BY SIZE
                                       INTO WSAA-QCMDEXC
           END-STRING.

           CALL 'QCMDEXC' USING WSAA-QCMDEXC WSAA-QCMDEXC-LENGTH.

           OPEN INPUT PAYXPF.

      * Read T5679 for valid statii.

           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE BSPR-COMPANY           TO ITEM-ITEMCOY.
           MOVE T5679                  TO ITEM-ITEMTABL.
           MOVE BPRD-AUTH-CODE         TO ITEM-ITEMITEM.
           MOVE READR                  TO ITEM-FUNCTION.
           CALL 'ITEMIO'               USING ITEM-PARAMS.

           IF ITEM-STATUZ          NOT = O-K
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE ITEM-GENAREA           TO T5679-T5679-REC.


      * Read T5645 for Account Codes.

           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE BSPR-COMPANY           TO ITEM-ITEMCOY.
           MOVE T5645                  TO ITEM-ITEMTABL.
           MOVE WSAA-PROG              TO ITEM-ITEMITEM.
           MOVE READR                  TO ITEM-FUNCTION.
           CALL 'ITEMIO'               USING ITEM-PARAMS.

           IF ITEM-STATUZ          NOT = O-K
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE ITEM-GENAREA           TO T5645-T5645-REC.

      * Read T3695 to check the sign.

           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE BSPR-COMPANY           TO ITEM-ITEMCOY.
           MOVE T3695                  TO ITEM-ITEMTABL.
           MOVE T5645-SACSTYPE-01      TO ITEM-ITEMITEM.
           MOVE READR                  TO ITEM-FUNCTION.
           CALL 'ITEMIO'               USING ITEM-PARAMS.

           IF ITEM-STATUZ          NOT = O-K
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE ITEM-GENAREA           TO T3695-T3695-REC.

      *  Load Billing channels.

           MOVE O-K                    TO ITEM-STATUZ.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE BSPR-COMPANY           TO ITEM-ITEMCOY.
           MOVE T3620                  TO ITEM-ITEMTABL.
           MOVE SPACES                 TO ITEM-ITEMITEM.
           MOVE BEGN                   TO ITEM-FUNCTION.

           SET  WSAA-T3620-IX          TO 1.

           PERFORM 1500-LOAD-T3620
                             UNTIL ITEM-STATUZ = ENDP.

      *  Load Bank codes.

           MOVE O-K                    TO ITEM-STATUZ.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE BSPR-COMPANY           TO ITEM-ITEMCOY.
           MOVE T3629                  TO ITEM-ITEMTABL.
           MOVE SPACES                 TO ITEM-ITEMITEM.
           MOVE BEGN                   TO ITEM-FUNCTION.

           SET  WSAA-T3629-IX         TO 1.

           PERFORM 1600-LOAD-T3629
                             UNTIL ITEM-STATUZ = ENDP.

      *  Load the Billing routines

           MOVE O-K                    TO ITEM-STATUZ.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE BSPR-COMPANY           TO ITEM-ITEMCOY.
           MOVE T6654                  TO ITEM-ITEMTABL.
           MOVE SPACES                 TO ITEM-ITEMITEM.
           MOVE BEGN                   TO ITEM-FUNCTION.

           SET  WSAA-T6654-IX          TO 1.

           PERFORM 1700-LOAD-T6654
                             UNTIL ITEM-STATUZ = ENDP.

      *  Load tax relief methods.

           MOVE O-K                    TO ITEM-STATUZ.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE BSPR-COMPANY           TO ITEM-ITEMCOY.
           MOVE T6687                  TO ITEM-ITEMTABL.
           MOVE SPACES                 TO ITEM-ITEMITEM.
           MOVE BEGN                   TO ITEM-FUNCTION.

           SET  WSAA-T6687-IX         TO 1.

           PERFORM 1800-LOAD-T6687
                             UNTIL ITEM-STATUZ = ENDP.
                                                                        <UL005>
      *  Load Skip basic premium.                                       <UL005>
                                                                        <UL005>
           MOVE O-K                    TO ITEM-STATUZ.                  <UL005>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <UL005>
           MOVE BSPR-COMPANY           TO ITEM-ITEMCOY.                 <UL005>
           MOVE TV103                  TO ITEM-ITEMTABL.                <UL005>
           MOVE SPACES                 TO ITEM-ITEMITEM.                <UL005>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <UL005>
           MOVE BEGN                   TO ITEM-FUNCTION.                <UL005>
                                                                        <UL005>
           SET  WSAA-TV103-IX         TO 1.                             <UL005>
                                                                        <UL005>
           PERFORM A1200-LOAD-TV103                                     <UL005>
                      UNTIL ITEM-STATUZ = ENDP.                         <UL005>
TVAN                                                                    <CS020>
           MOVE O-K                    TO ITEM-STATUZ.                  <CS020>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <CS020>
           MOVE BSPR-COMPANY           TO ITEM-ITEMCOY.                 <CS020>
           MOVE TZ028                  TO ITEM-ITEMTABL.                <CS020>
           MOVE SPACES                 TO ITEM-ITEMITEM.                <CS020>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <CS020>
           MOVE BEGN                   TO ITEM-FUNCTION.                <CS020>
                                                                        <CS020>
           SET  WSAA-TZ028-IX         TO 1.                             <CS020>
                                                                        <CS020>
           PERFORM A1800-LOAD-TZ028                                     <CS020>
TVAN                  UNTIL ITEM-STATUZ = ENDP.                         <CS020>

       1090-EXIT.
           EXIT.
      *                                                                 <GAPPH2>
      /                                                                 <GAPPH2>
       1100-READ-ITEMPF SECTION.                                        <GAPPH2>
      **************************                                        <GAPPH2>
      *                                                                 <GAPPH2>
       1110-START.                                                      <GAPPH2>
      *                                                                 <GAPPH2>
           INITIALIZE                  ITEM-PARAMS.                     <GAPPH2>
           MOVE WSAA-ITEM-KEY          TO ITEM-DATA-KEY.                <GAPPH2>
           MOVE SPACES                 TO ITEM-ITEMSEQ.                 <GAPPH2>
           MOVE READR                  TO ITEM-FUNCTION.                <GAPPH2>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <GAPPH2>
                                                                        <GAPPH2>
           CALL 'ITEMIO'               USING ITEM-PARAMS.               <GAPPH2>
                                                                        <GAPPH2>
           IF ITEM-STATUZ              NOT = O-K AND MRNF               <GAPPH2>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <GAPPH2>
               PERFORM 600-FATAL-ERROR                                  <GAPPH2>
           END-IF.                                                      <GAPPH2>
      *                                                                 <GAPPH2>
       1190-EXIT.                                                       <GAPPH2>
           EXIT.                                                        <GAPPH2>
      *                                                                 <GAPPH2>

       1500-LOAD-T3620 SECTION.
      ************************
       1510-START.

           CALL 'ITEMIO'               USING ITEM-PARAMS.

           IF  ITEM-STATUZ             NOT = O-K
           AND ITEM-STATUZ             NOT = ENDP
               MOVE ITEM-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF ITEM-STATUZ              = ENDP
           OR ITEM-ITEMCOY         NOT = BSPR-COMPANY
           OR ITEM-ITEMTABL        NOT = T3620
              MOVE ENDP                TO ITEM-STATUZ
              GO TO 1599-EXIT
           END-IF.

           IF  WSAA-T3620-IX > WSAA-T3620-SIZE
               MOVE H791               TO SYSR-STATUZ
               MOVE T3620              TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE ITEM-GENAREA    TO T3620-T3620-REC.
           MOVE ITEM-ITEMITEM   TO WSAA-T3620-BILLCHNL (WSAA-T3620-IX).
           MOVE T3620-DDIND     TO WSAA-T3620-DDIND    (WSAA-T3620-IX).
           MOVE T3620-CRCIND    TO WSAA-T3620-CRCIND   (WSAA-T3620-IX). <V76F13>

           MOVE NEXTR           TO ITEM-FUNCTION.

           SET WSAA-T3620-IX UP BY 1.

       1599-EXIT.
           EXIT.

       1600-LOAD-T3629 SECTION.
      ************************
       1610-START.

           CALL 'ITEMIO'               USING ITEM-PARAMS.

           IF  ITEM-STATUZ         NOT = O-K
           AND ITEM-STATUZ         NOT = ENDP
               MOVE ITEM-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF ITEM-STATUZ              = ENDP
           OR ITEM-ITEMCOY         NOT = BSPR-COMPANY
           OR ITEM-ITEMTABL        NOT = T3629
              MOVE ENDP                TO ITEM-STATUZ
              GO TO 1699-EXIT
           END-IF.

           IF  WSAA-T3629-IX > WSAA-T3629-SIZE
               MOVE H791               TO SYSR-STATUZ
               MOVE T3629              TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE ITEM-GENAREA    TO T3629-T3629-REC.
           MOVE ITEM-ITEMITEM   TO WSAA-T3629-BILLCURR (WSAA-T3629-IX).
           MOVE T3629-BANKCODE  TO WSAA-T3629-BANKCODE (WSAA-T3629-IX).

           MOVE NEXTR            TO ITEM-FUNCTION.

           SET WSAA-T3629-IX UP BY 1.

       1699-EXIT.
           EXIT.

       1700-LOAD-T6654 SECTION.
      ************************
       1710-START.

           CALL 'ITEMIO'               USING ITEM-PARAMS.

           IF  ITEM-STATUZ         NOT = O-K
           AND ITEM-STATUZ         NOT = ENDP
               MOVE ITEM-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF ITEM-STATUZ              = ENDP
           OR ITEM-ITEMCOY         NOT = BSPR-COMPANY
           OR ITEM-ITEMTABL        NOT = T6654
               MOVE ENDP          TO ITEM-STATUZ
               GO TO 1799-EXIT
           END-IF.

           IF  WSAA-T6654-IX > WSAA-T6654-SIZE
               MOVE H791               TO SYSR-STATUZ
               MOVE T6654              TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE ITEM-GENAREA     TO T6654-T6654-REC.
           MOVE ITEM-ITEMITEM    TO WSAA-T6654-KEY (WSAA-T6654-IX).
           MOVE T6654-COLLECTSUB TO WSAA-T6654-COLLSUB (WSAA-T6654-IX).
           MOVE T6654-LEAD-DAYS  TO WSAA-T6654-LEADDAY (WSAA-T6654-IX).

           MOVE NEXTR             TO ITEM-FUNCTION.

           SET WSAA-T6654-IX UP BY 1.

       1799-EXIT.
           EXIT.


       1800-LOAD-T6687 SECTION.
      ************************
       1810-START.

           CALL 'ITEMIO' USING ITEM-PARAMS.

           IF  ITEM-STATUZ         NOT = O-K
           AND ITEM-STATUZ         NOT = ENDP
               MOVE ITEM-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF ITEM-STATUZ              = ENDP
           OR ITEM-ITEMCOY       NOT   = BSPR-COMPANY
           OR ITEM-ITEMTABL      NOT   = T6687
               MOVE ENDP               TO ITEM-STATUZ
               GO TO 1899-EXIT
           END-IF.

           IF WSAA-T6687-IX > WSAA-T6687-SIZE
               MOVE H791               TO SYSR-STATUZ
               MOVE T6687              TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE ITEM-GENAREA           TO T6687-T6687-REC.
           MOVE T6687-TAXRELSUB TO
                                 WSAA-T6687-TAXRELSUBR (WSAA-T6687-IX).
           MOVE ITEM-ITEMITEM TO WSAA-T6687-TAXRELMTH  (WSAA-T6687-IX).

           MOVE NEXTR                  TO ITEM-FUNCTION.

           SET WSAA-T6687-IX UP BY 1.

       1899-EXIT.
           EXIT.

       1900-READ-TABLE-T5729.                                           <D9604>
       1910-START.                                                      <D9604>
                                                                        <D9604>
      *  Load the premium variances.                            <D9604>
                                                                        <D9604>
           MOVE O-K                    TO ITDM-STATUZ.                  <D9604>
           MOVE 'IT'                   TO ITDM-ITEMPFX.                 <D9604>
           MOVE BSPR-COMPANY           TO ITDM-ITEMCOY.                 <D9604>
           MOVE T5729                  TO ITDM-ITEMTABL.                <D9604>
           MOVE SPACES                 TO ITDM-ITEMITEM.                <D9604>
           MOVE 0                      TO ITDM-ITMTO.                   <D9604>
           MOVE 0                      TO ITDM-ITMFRM.                  <D9604>
           MOVE BEGN                   TO ITDM-FUNCTION.                <D9604>
                                                                        <D9604>
           SET  WSAA-T5729-IX           TO 1.                           <D9604>
           MOVE 1                       TO WSAA-T5729-SUB.              <D9604>
                                                                        <D9604>
           PERFORM 1950-LOAD-T5729                                      <D9604>
                             UNTIL ITDM-STATUZ = ENDP.                  <D9604>
                                                                        <D9604>
                                                                        <D9604>
       1919-EXIT.                                                       <D9604>
           EXIT.                                                        <D9604>
                                                                        <D9604>
       1950-LOAD-T5729 SECTION.                                         <D9604>
       1951-START.                                                      <D9604>
                                                                        <D9604>
           CALL 'ITDMIO'  USING ITDM-PARAMS.                            <D9604>
                                                                        <D9604>
           IF  ITDM-STATUZ          NOT = O-K                           <D9604>
           AND ITDM-STATUZ          NOT = ENDP                          <D9604>
              MOVE ITDM-PARAMS      TO SYSR-PARAMS                      <D9604>
              PERFORM 600-FATAL-ERROR                                   <D9604>
           END-IF.                                                      <D9604>
                                                                        <D9604>
           IF ITDM-ITEMCOY          NOT = BPRD-COMPANY                  <D9604>
           OR ITDM-ITEMTABL         NOT = T5729                         <D9604>
           OR ITDM-STATUZ               = ENDP                          <D9604>
              MOVE ENDP             TO ITDM-STATUZ                      <D9604>
              GO TO 1959-EXIT                                           <D9604>
           END-IF.                                                      <D9604>
                                                                        <D9604>
           MOVE ITDM-GENAREA        TO T5729-T5729-REC                  <D9604>
                                                                        <D9604>
           IF  WSAA-T5729-IX > WSAA-T5729-SIZE                          <D9604>
               MOVE H791            TO SYSR-STATUZ                      <D9604>
               MOVE T5729           TO SYSR-PARAMS                      <D9604>
               PERFORM 600-FATAL-ERROR                                  <D9604>
           END-IF.                                                      <D9604>
                                                                        <D9604>
           MOVE ITDM-ITEMITEM  TO WSAA-T5729-KEY     (WSAA-T5729-IX).   <D9604>
           MOVE ITDM-ITMFRM    TO WSAA-T5729-CURRFROM(WSAA-T5729-IX).   <D9604>
           MOVE ITDM-ITMTO     TO WSAA-T5729-CURRTO(WSAA-T5729-IX).     <D9604>
                                                                        <UL001>
           PERFORM VARYING WSAA-T5729-SUB FROM 1 BY 1                   <UL001>
                                       UNTIL WSAA-T5729-SUB > 6         <UL001>
              MOVE T5729-FRQCYS  TO WSAA-T5729-FRQCYS(WSAA-T5729-IX)    <UL001>
           END-PERFORM                                                  <UL001>
                                                                        <UL001>
           MOVE 1                      TO WSAA-T5729-SUB                <UL001>
           PERFORM VARYING WSAA-T5729-SUB FROM 1 BY 1                   <UL001>
                                       UNTIL WSAA-T5729-SUB > 4         <UL001>
              MOVE T5729-DURATIONAS                                     <UL001>
                               TO WSAA-T5729-DURATIONAS(WSAA-T5729-IX)  <UL001>
              MOVE T5729-DURATIONBS                                     <UL001>
                               TO WSAA-T5729-DURATIONBS(WSAA-T5729-IX)  <UL001>
              MOVE T5729-DURATIONCS                                     <UL001>
                               TO WSAA-T5729-DURATIONCS(WSAA-T5729-IX)  <UL001>
              MOVE T5729-DURATIONDS                                     <UL001>
                               TO WSAA-T5729-DURATIONDS(WSAA-T5729-IX)  <UL001>
              MOVE T5729-DURATIONES                                     <UL001>
                               TO WSAA-T5729-DURATIONES(WSAA-T5729-IX)  <UL001>
              MOVE T5729-DURATIONFS                                     <UL001>
                               TO WSAA-T5729-DURATIONFS(WSAA-T5729-IX)  <UL001>
              MOVE T5729-OVERDUE-MINAS                                  <UL001>
                            TO WSAA-T5729-OVERDUE-MINAS(WSAA-T5729-IX)  <UL001>
              MOVE T5729-OVERDUE-MINBS                                  <UL001>
                            TO WSAA-T5729-OVERDUE-MINBS(WSAA-T5729-IX)  <UL001>
              MOVE T5729-OVERDUE-MINCS                                  <UL001>
                            TO WSAA-T5729-OVERDUE-MINCS(WSAA-T5729-IX)  <UL001>
              MOVE T5729-OVERDUE-MINDS                                  <UL001>
                            TO WSAA-T5729-OVERDUE-MINDS(WSAA-T5729-IX)  <UL001>
              MOVE T5729-OVERDUE-MINES                                  <UL001>
                            TO WSAA-T5729-OVERDUE-MINES(WSAA-T5729-IX)  <UL001>
              MOVE T5729-OVERDUE-MINFS                                  <UL001>
                            TO WSAA-T5729-OVERDUE-MINFS(WSAA-T5729-IX)  <UL001>
              MOVE T5729-TARGET-MAXAS                                   <UL001>
                            TO WSAA-T5729-TARGET-MAXAS(WSAA-T5729-IX)   <UL001>
              MOVE T5729-TARGET-MAXBS                                   <UL001>
                            TO WSAA-T5729-TARGET-MAXBS(WSAA-T5729-IX)   <UL001>
              MOVE T5729-TARGET-MAXCS                                   <UL001>
                            TO WSAA-T5729-TARGET-MAXCS(WSAA-T5729-IX)   <UL001>
              MOVE T5729-TARGET-MAXDS                                   <UL001>
                            TO WSAA-T5729-TARGET-MAXDS(WSAA-T5729-IX)   <UL001>
              MOVE T5729-TARGET-MAXES                                   <UL001>
                            TO WSAA-T5729-TARGET-MAXES(WSAA-T5729-IX)   <UL001>
              MOVE T5729-TARGET-MAXFS                                   <UL001>
                            TO WSAA-T5729-TARGET-MAXFS(WSAA-T5729-IX)   <UL001>
              MOVE T5729-TARGET-MINAS                                   <UL001>
                            TO WSAA-T5729-TARGET-MINAS(WSAA-T5729-IX)   <UL001>
              MOVE T5729-TARGET-MINBS                                   <UL001>
                            TO WSAA-T5729-TARGET-MINBS(WSAA-T5729-IX)   <UL001>
              MOVE T5729-TARGET-MINCS                                   <UL001>
                            TO WSAA-T5729-TARGET-MINCS(WSAA-T5729-IX)   <UL001>
              MOVE T5729-TARGET-MINDS                                   <UL001>
                            TO WSAA-T5729-TARGET-MINDS(WSAA-T5729-IX)   <UL001>
              MOVE T5729-TARGET-MINES                                   <UL001>
                            TO WSAA-T5729-TARGET-MINES(WSAA-T5729-IX)   <UL001>
              MOVE T5729-TARGET-MINFS                                   <UL001>
                            TO WSAA-T5729-TARGET-MINFS(WSAA-T5729-IX)   <UL001>
           END-PERFORM                                                  <UL001>
                                                                        <D9604>
           SET WSAA-T5729-IX  UP BY 1.                                  <D9604>
                                                                        <D9604>
           MOVE NEXTR                     TO ITDM-FUNCTION.             <D9604>
                                                                        <D9604>
                                                                        <D9604>
       1959-EXIT.                                                       <D9604>
           EXIT.                                                        <D9604>
                                                                        <D9604>
       2000-READ-FILE SECTION.
      ************************
       2010-READ-FILE.

           READ PAYXPF
           AT END
              MOVE ENDP                TO WSSP-EDTERROR
              GO TO 2090-EXIT.

           MOVE CT01                   TO CONT-TOTNO.
           MOVE 1                      TO CONT-TOTVAL.
           PERFORM 001-CALL-CONTOT.

      *  Set up the key for the SYSR- copybook, should a system error
      *  for this instalment occur.

           MOVE CHDRNUM                TO WSYS-CHDRNUM.
           MOVE BILLCD                 TO WSYS-BILLCD.

       2090-EXIT.
           EXIT.

       2500-EDIT SECTION.
      *******************
       2510-READ.

      * Read the contract header and validate the status against those
      * on T5679. If the status is invalid, add 1 to CT06, which is a
      * log of the Contract Header Records which have invalid statii.

           MOVE O-K                    TO WSSP-EDTERROR.

           PERFORM 2520-READ-CHDR.

           PERFORM A100-READ-TU477.
           IF  WSAA-TU477-FLAG      NOT = 'Y'
               MOVE SPACES              TO WSSP-EDTERROR
               GO TO 2590-EXIT
           END-IF.

      * Check Lapse Reins Code
           PERFORM C800-CHECK-REINS-CODE.
           IF WSAA-FOUND-REINS         = 'Y'
           OR CHDRLIF-STATCODE         NOT = 'IF'
               MOVE SPACES              TO WSSP-EDTERROR
               GO TO 2590-EXIT
           END-IF.

           PERFORM 2540-VALIDATE-CHDR.
           PERFORM 2545-CHECK-T5729.                                    <D9604>
           PERFORM 2550-CHECK-TV078.
           PERFORM 2620-CHECK-VALID-PTDATE.

           IF NOT VALID-CONTRACT

      *  Log CHDRs with invalid statii

              MOVE CT07                TO CONT-TOTNO
              MOVE 1                   TO CONT-TOTVAL
              PERFORM 001-CALL-CONTOT

              MOVE SPACES              TO WSSP-EDTERROR
              GO TO 2590-EXIT
           END-IF.

           PERFORM 2560-CALC-LEAD-DAYS.

      *    Once we have calculated the lead days for this contract      <001>
      *    type we can compare it to the BILLCD date to see if the      <001>
      *    contract requires billing.                                   <001>
                                                                        <001>
      ***  IF BILLCD > WSAA-EFFDATE-PLUS-CNTLEAD                        <001>
      ***      MOVE SPACES             TO WSSP-EDTERROR                 <001>
      ***      GO TO 2590-EXIT                                          <001>
      ***  END-IF.                                                      <001>
                                                                        <001>
      * If suppressed billing, check if the present bill date is within
      * the suppressed period.

           IF BILLSUPR                 = 'Y'
      ****     IF  BILLSPFROM     NOT > WSAA-EFFDATE-PLUS-CNTLEAD
      ****     AND BILLSPTO       NOT < WSAA-EFFDATE-PLUS-CNTLEAD
               IF  BILLCD              >= BILLSPFROM                    <A06856>
               AND BILLCD              <  BILLSPTO                      <A06856>
               AND BSSC-EFFECTIVE-DATE <  BILLSPTO                      <A06856>
                   MOVE CT02           TO CONT-TOTNO
                   MOVE 1              TO CONT-TOTVAL
                   PERFORM 001-CALL-CONTOT

                   MOVE SPACES         TO WSSP-EDTERROR
                   GO TO 2590-EXIT
               END-IF
           END-IF.

           PERFORM 2570-READ-TR52D.                                     <V74L01>
           PERFORM 2580-SOFTLOCK.

      *  Log CHDRs locked

           IF SFTL-STATUZ              = 'LOCK'

              MOVE CT08                TO CONT-TOTNO
              MOVE 1                   TO CONT-TOTVAL
              PERFORM 001-CALL-CONTOT

              MOVE SPACES              TO WSSP-EDTERROR
           END-IF.

       2590-EXIT.
           EXIT.
      *
       C800-CHECK-REINS-CODE SECTION.
      *********************************
       C801-CHECK.

           MOVE 'N'                    TO WSAA-FOUND-REINS.

           MOVE SPACES                 TO PTRNENQ-PARAMS.
           MOVE '2'                    TO PTRNENQ-CHDRCOY.
           MOVE CHDRNUM                TO PTRNENQ-CHDRNUM.
           MOVE 99999                  TO PTRNENQ-TRANNO.
           MOVE PTRNENQREC             TO PTRNENQ-FORMAT.
           MOVE BEGN                   TO PTRNENQ-FUNCTION.

           CALL 'PTRNENQIO'            USING PTRNENQ-PARAMS.

           IF PTRNENQ-STATUZ           NOT = O-K
           AND                         NOT = ENDP
              MOVE PTRNENQ-STATUZ      TO SYSR-STATUZ
              MOVE PTRNENQ-PARAMS      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

           IF PTRNENQ-STATUZ           = ENDP
           OR PTRNENQ-CHDRCOY          NOT = '2'
           OR PTRNENQ-CHDRNUM          NOT = CHDRNUM
              MOVE ENDP                TO PTRNENQ-STATUZ
              GO TO C809-EXIT
           END-IF.

           IF PTRNENQ-STATUZ           = O-K
           AND PTRNENQ-CHDRCOY         = '2'
           AND PTRNENQ-CHDRNUM         = CHDRNUM
           AND PTRNENQ-BATCTRCDE       = WSAA-BATCTRCDE
               MOVE 'Y'                TO WSAA-FOUND-REINS
           END-IF.

       C809-EXIT.
           EXIT.
      *
      *
       2520-READ-CHDR SECTION.
      ************************
       2520-START.

      * Read contract header record using CHDRLIF.

           MOVE CHDRCOY                TO CHDRLIF-CHDRCOY.
           MOVE CHDRNUM                TO CHDRLIF-CHDRNUM.
           MOVE READH                  TO CHDRLIF-FUNCTION.
           MOVE CHDRLIFREC             TO CHDRLIF-FORMAT.

           CALL 'CHDRLIFIO'            USING CHDRLIF-PARAMS.

           IF  CHDRLIF-STATUZ      NOT = O-K
               MOVE CHDRLIF-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.
      *                                                                 <UL001>
           MOVE CHDRCOY                TO PAYR-CHDRCOY.                 <UL001>
           MOVE CHDRNUM                TO PAYR-CHDRNUM.                 <UL001>
           MOVE PAYRSEQNO              TO PAYR-PAYRSEQNO.               <UL001>
           MOVE '1'                    TO PAYR-VALIDFLAG.               <UL001>
           MOVE READR                  TO PAYR-FUNCTION.                <UL001>
           MOVE PAYRREC                TO PAYR-FORMAT.                  <UL001>
                                                                        <UL001>
           CALL 'PAYRIO'               USING PAYR-PARAMS.               <UL001>
                                                                        <UL001>
           IF  PAYR-STATUZ             NOT = O-K                        <UL001>
               MOVE PAYR-PARAMS        TO SYSR-PARAMS                   <UL001>
               PERFORM 600-FATAL-ERROR                                  <UL001>
           END-IF.                                                      <UL001>
           MOVE PAYR-PTDATE            TO WSAA-PTDATE.

       2529-EXIT.
           EXIT.

       2545-CHECK-T5729 SECTION.                                        <D9604>
       2545-START.                                                      <D9604>
                                                                        <D9604>
           MOVE 'N' TO WSAA-FLEX-PREM.                                  <D9604>
                                                                        <D9604>
      *  Read T5729.                                            <D9604>
                                                                        <D9604>
           SET WSAA-T5729-IX              TO 1.                         <D9604>
                                                                        <D9604>
           SEARCH WSAA-T5729-REC                                        <D9604>
              WHEN WSAA-T5729-KEY (WSAA-T5729-IX) = CHDRLIF-CNTTYPE     <D9604>
              AND WSAA-T5729-CURRFROM(WSAA-T5729-IX)                    <D9604>
                  NOT > CHDRLIF-OCCDATE                                 <D9604>
              AND WSAA-T5729-CURRTO(WSAA-T5729-IX)                      <D9604>
                  NOT < CHDRLIF-OCCDATE                                 <D9604>
      ****    SET FLEXIBLE-PREMIUM-CONTRACT TO TRUE             <UL001> <D9604>
              CONTINUE                                                  <D9604>
           END-SEARCH.                                                  <D9604>
                                                                        <UL001>
      *  We must find the effective T5729 entry for the contract        <UL001>
      *  (T5729 entries will have been loaded in descending sequence    <UL001>
      *  into the array).                                               <UL001>
      *  Load the values from table T5729 which match the frequency     <UL001>
      *  on the PAYR record                                             <UL001>
                                                                        <UL001>
           MOVE 'N'                      TO WSAA-FREQ-FOUND.            <UL001>
           MOVE  1                       TO WSAA-T5729-SUB.             <UL001>
                                                                        <UL001>
           PERFORM VARYING WSAA-T5729-SUB                               <UL001>
                   FROM 1 BY 1                                          <UL001>
                   UNTIL WSAA-T5729-SUB > 6                             <UL001>
                   OR    FREQ-FOUND                                     <UL001>
              IF WSAA-T5729-FRQCY(WSAA-T5729-IX WSAA-T5729-SUB) =       <UL001>
                                                       PAYR-BILLFREQ    <UL001>
                 MOVE WSAA-T5729-SUB      TO WSAA-TEST                  <UL001>
                 MOVE 'Y'                           TO WSAA-FREQ-FOUND  <UL001>
              END-IF                                                    <UL001>
           END-PERFORM.                                                 <UL001>
                                                                        <UL001>
           IF WSAA-T5729-SUB > 6                                        <UL001>
              MOVE I086               TO SYSR-STATUZ                    <UL001>
              MOVE WSYS-SYSTEM-ERROR-PARAMS TO SYSR-PARAMS              <UL001>
              PERFORM 600-FATAL-ERROR                                   <UL001>
           END-IF.                                                      <UL001>
                                                                        <UL001>
      **** Frequency found ************                                 <UL001>
           EVALUATE WSAA-TEST                                           <UL001>
           WHEN 1                                                       <UL001>
            MOVE WSAA-T5729-DURATIONAS(WSAA-T5729-IX)                   <UL001>
                                               TO WSAA-DURATIONS        <UL001>
           WHEN 2                                                       <UL001>
            MOVE WSAA-T5729-DURATIONBS(WSAA-T5729-IX)                   <UL001>
                                               TO WSAA-DURATIONS        <UL001>
           WHEN 3                                                       <UL001>
            MOVE WSAA-T5729-DURATIONCS(WSAA-T5729-IX)                   <UL001>
                                               TO WSAA-DURATIONS        <UL001>
           WHEN 4                                                       <UL001>
            MOVE WSAA-T5729-DURATIONDS(WSAA-T5729-IX)                   <UL001>
                                               TO WSAA-DURATIONS        <UL001>
           WHEN 5                                                       <UL001>
            MOVE WSAA-T5729-DURATIONES(WSAA-T5729-IX)                   <UL001>
                                               TO WSAA-DURATIONS        <UL001>
           WHEN 6                                                       <UL001>
            MOVE WSAA-T5729-DURATIONFS(WSAA-T5729-IX)                   <UL001>
                                               TO WSAA-DURATIONS        <UL001>
           END-EVALUATE.                                                <UL001>
                                                                        <UL001>
      *  Determine the current variances from T5729 based on the        <UL001>
      *  difference between the Occdate and the effective date of this  <UL001>
      *  job.                                                           <UL001>
      *  Use the DATCON3 subroutine to calculate the freq factor        <UL001>
                                                                        <UL001>
           MOVE SPACES                 TO DTC3-DATCON3-REC              <UL001>
           MOVE CHDRLIF-OCCDATE        TO DTC3-INT-DATE-1               <UL001>
           MOVE PAYR-PTDATE            TO DTC3-INT-DATE-2               <UL001>
TDO   **** MOVE PAYR-BILLFREQ          TO DTC3-FREQUENCY                <UL001>
           MOVE '01'                   TO DTC3-FREQUENCY
           CALL 'DATCON3' USING DTC3-DATCON3-REC                        <UL001>
                                                                        <UL001>
           IF DTC3-STATUZ NOT = O-K                                     <UL001>
              MOVE DTC3-DATCON3-REC    TO SYSR-PARAMS                   <UL001>
              PERFORM 600-FATAL-ERROR                                   <UL001>
           END-IF.                                                      <UL001>
                                                                        <UL001>
TDO   **** MOVE DTC3-FREQ-FACTORX      TO WSAA-NUM-PERIOD.              <UL001>
           MOVE DTC3-FREQ-FACTOR       TO WSAA-NUM-PERIOD.
                                                                        <UL001>
           MOVE 0                      TO WSAA-SUB2                     <UL001>
           SET DURATION-NOT-FOUND      TO TRUE                          <UL001>
                                                                        <UL001>
           PERFORM VARYING WSAA-SUB FROM 1 BY 1                         <UL001>
                                UNTIL WSAA-SUB > 4                      <UL001>
                                OR WSAA-DURATION(WSAA-SUB) = SPACES     <UL001>
TDO   ****    IF WSAA-NUM-PERIOD        >= WSAA-DURATION(WSAA-SUB)      <UL001>
TDO           IF WSAA-NUM-PERIOD        >= WSAA-DURATION(WSAA-SUB)      <UL001>
              AND ZEROES            NOT =  WSAA-DURATION(WSAA-SUB)      <UL001>
              AND BSSC-EFFECTIVE-DATE   >= PAYR-PTDATE
                  MOVE WSAA-SUB      TO WSAA-SUB2                       <UL001>
                  SET FLEXIBLE-PREMIUM-CONTRACT TO TRUE                 <UL001>
              END-IF                                                    <UL001>
           END-PERFORM.                                                 <UL001>
                                                                        <UL001>
           IF WSAA-SUB2             NOT = ZEROES                        <UL001>
              SET DURATION-FOUND     TO TRUE                            <UL001>
           END-IF.                                                      <UL001>
                                                                        <D9604>
      *2549-EXIT.                                               <UL001> <D9604>
       2545-EXIT.                                                       <UL001>
           EXIT.                                                        <D9604>
      *
       2550-CHECK-TV078 SECTION.
      ***************************
       2551-CHK.

           MOVE O-K                    TO ITDM-STATUZ.
           MOVE 'IT'                   TO ITDM-ITEMPFX.
           MOVE '2'                    TO ITDM-ITEMCOY.
           MOVE TV078                  TO ITDM-ITEMTABL.
           MOVE CHDRLIF-CNTTYPE        TO ITDM-ITEMITEM
      ***  MOVE 0                      TO ITDM-ITMTO.
           MOVE 99999999               TO ITDM-ITMFRM.
           MOVE BEGN                   TO ITDM-FUNCTION.

           CALL 'ITDMIO'  USING ITDM-PARAMS.

           IF  ITDM-STATUZ          NOT = O-K
           AND ITDM-STATUZ          NOT = ENDP
              MOVE ITDM-PARAMS      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

           IF ITDM-ITEMCOY          NOT = '2'
           OR ITDM-ITEMTABL         NOT = TV078
           OR ITDM-STATUZ               = ENDP
           OR ITDM-ITEMITEM         NOT = CHDRLIF-CNTTYPE
              MOVE 'N'              TO WSAA-VALID-CHDR
              MOVE ENDP             TO ITDM-STATUZ
              GO TO 2559-EXIT
           END-IF.

           MOVE ITDM-GENAREA        TO TV078-TV078-REC.

           MOVE TV078-NOFYEAR       TO DTC2-FREQ-FACTOR.
           MOVE '01'                   TO DTC2-FREQUENCY.
           MOVE CHDRLIF-OCCDATE        TO DTC2-INT-DATE-1.

           CALL 'DATCON2'              USING DTC2-DATCON2-REC.

           IF DTC2-STATUZ           NOT = O-K
              MOVE DTC2-DATCON2-REC    TO SYSR-PARAMS
              MOVE DTC2-STATUZ         TO SYSR-STATUZ                   <CAS1.0>
              PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE DTC2-INT-DATE-2        TO WSAA-DATE-3YEAR.

           IF BSSC-EFFECTIVE-DATE      >= WSAA-DATE-3YEAR
               MOVE 'N'                TO WSAA-VALID-CHDR
           END-IF.

       2559-EXIT.
           EXIT.
      *
       2620-CHECK-VALID-PTDATE SECTION.
      **********************************
       2621-START.

           MOVE ZEROES                 TO WSAA-POLYEAR.
           MOVE CHDRLIF-OCCDATE        TO DTC3-INT-DATE-1.
           MOVE BSSC-EFFECTIVE-DATE    TO DTC3-INT-DATE-2.
           MOVE '01'                   TO DTC3-FREQUENCY.

           CALL 'DATCON3'           USING DTC3-DATCON3-REC.

           IF DTC3-STATUZ           NOT = O-K
              MOVE 01                  TO WSAA-POLYEAR
           ELSE
              COMPUTE WSAA-POLYEAR      = DTC3-FREQ-FACTOR + 1
           END-IF.

      *
           MOVE ZEROES                 TO WSAA-DATE-YEAREND.
           MOVE WSAA-POLYEAR           TO DTC2-FREQ-FACTOR.
           MOVE '01'                   TO DTC2-FREQUENCY.
           MOVE CHDRLIF-OCCDATE        TO DTC2-INT-DATE-1.

           CALL 'DATCON2'              USING DTC2-DATCON2-REC.

           IF DTC2-STATUZ           NOT = O-K
              MOVE DTC2-DATCON2-REC    TO SYSR-PARAMS
              MOVE DTC2-STATUZ         TO SYSR-STATUZ                   <CAS1.0>
              PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE DTC2-INT-DATE-2        TO WSAA-DATE-YEAREND.

           IF PAYR-PTDATE              >= WSAA-DATE-YEAREND
               MOVE 'N'                TO WSAA-VALID-CHDR
           END-IF.


       2629-EXIT.
           EXIT.
      *
                                                                        <D9604>
       2540-VALIDATE-CHDR SECTION.
      ***************************
       2540-START.

      * Validate Contract status against T5679.

           MOVE 'N'                    TO WSAA-VALID-CHDR.

           PERFORM VARYING WSAA-T5679-SUB
                   FROM 1 BY 1
                   UNTIL WSAA-T5679-SUB > 12
                      OR VALID-CONTRACT
              IF T5679-CN-RISK-STAT(WSAA-T5679-SUB) = CHDRLIF-STATCODE

                 PERFORM VARYING WSAA-T5679-SUB
                         FROM 1 BY 1
                         UNTIL WSAA-T5679-SUB > 12
                            OR VALID-CONTRACT

                    IF T5679-CN-PREM-STAT(WSAA-T5679-SUB)
                                       = CHDRLIF-PSTATCODE
                       MOVE 'Y'        TO WSAA-VALID-CHDR
                    END-IF
                 END-PERFORM

              END-IF
           END-PERFORM.

       2549-EXIT.
           EXIT.

       2560-CALC-LEAD-DAYS SECTION.
      *****************************
       2560-START.

      *  Search the array to find the subroutine for the contract
      *  required. If this is not found then the contract type (CNTTYPE)
      *  is replaced by '***'.

           MOVE BILLCHNL              TO WSAA-BILLCHNL2.
           MOVE CHDRLIF-CNTTYPE       TO WSAA-CNTTYPE2.

           SEARCH ALL WSAA-T6654-REC
              AT END
              MOVE '***'               TO WSAA-CNTTYPE2

              SEARCH ALL WSAA-T6654-REC
                   AT END
                   MOVE F035                   TO SYSR-STATUZ
                       STRING T6654
                               WSAA-T6654-KEY2
                                         DELIMITED BY SIZE
                                         INTO WSYS-SYSPARAMS
                   MOVE WSYS-SYSTEM-ERROR-PARAMS
                                       TO SYSR-PARAMS
                   PERFORM 600-FATAL-ERROR

              WHEN WSAA-T6654-KEY (WSAA-T6654-IX) = WSAA-T6654-KEY2
                 CONTINUE
              END-SEARCH

           WHEN WSAA-T6654-KEY (WSAA-T6654-IX) = WSAA-T6654-KEY2
              CONTINUE
           END-SEARCH.

      * Call 'DATCON2' to increment the effective date by the T6654
      * LEAD DAYS.

           MOVE WSAA-T6654-LEADDAY (WSAA-T6654-IX)
                                       TO DTC2-FREQ-FACTOR.
           MOVE 'DY'                   TO DTC2-FREQUENCY.
           MOVE BSSC-EFFECTIVE-DATE    TO DTC2-INT-DATE-1.

           CALL 'DATCON2'              USING DTC2-DATCON2-REC.

           IF DTC2-STATUZ           NOT = O-K
              MOVE DTC2-DATCON2-REC    TO SYSR-PARAMS
              MOVE DTC2-STATUZ         TO SYSR-STATUZ                   <CAS1.0>
              PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE DTC2-INT-DATE-2        TO WSAA-EFFDATE-PLUS-CNTLEAD.

       2569-EXIT.
            EXIT.

       2570-READ-TR52D SECTION.                                         <V74L01>
      *************************                                         <V74L01>
       2571-START.                                                      <V74L01>
           MOVE SPACES                 TO ITEM-DATA-KEY.                <V74L01>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <V74L01>
           MOVE BSPR-COMPANY           TO ITEM-ITEMCOY.                 <V74L01>
           MOVE TR52D                  TO ITEM-ITEMTABL.                <V74L01>
           MOVE CHDRLIF-REGISTER       TO ITEM-ITEMITEM.                <V74L01>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <V74L01>
           MOVE READR                  TO ITEM-FUNCTION.                <V74L01>
                                                                        <V74L01>
           CALL 'ITEMIO'               USING ITEM-PARAMS.               <V74L01>
                                                                        <V74L01>
           IF ITEM-STATUZ              NOT = O-K                        <V74L01>
           AND ITEM-STATUZ             NOT = MRNF                       <V74L01>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <V74L01>
               MOVE ITEM-STATUZ        TO SYSR-STATUZ                   <V74L01>
               PERFORM 600-FATAL-ERROR                                  <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF ITEM-STATUZ              = MRNF                           <V74L01>
              MOVE SPACES              TO ITEM-DATA-KEY                 <V74L01>
              MOVE 'IT'                TO ITEM-ITEMPFX                  <V74L01>
              MOVE BSPR-COMPANY        TO ITEM-ITEMCOY                  <V74L01>
              MOVE TR52D               TO ITEM-ITEMTABL                 <V74L01>
              MOVE '***'               TO ITEM-ITEMITEM                 <V74L01>
              MOVE READR               TO ITEM-FUNCTION                 <V74L01>
                                                                        <V74L01>
              CALL 'ITEMIO'           USING ITEM-PARAMS                 <V74L01>
                                                                        <V74L01>
              IF  ITEM-STATUZ         NOT = O-K                         <V74L01>
                  MOVE ITEM-PARAMS     TO SYSR-PARAMS                   <V74L01>
                  MOVE ITEM-STATUZ     TO SYSR-STATUZ                   <V74L01>
                  PERFORM 600-FATAL-ERROR                               <V74L01>
              END-IF                                                    <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           MOVE ITEM-GENAREA           TO TR52D-TR52D-REC.              <V74L01>
                                                                        <V74L01>
       2579-EXIT.                                                       <V74L01>
           EXIT.                                                        <V74L01>
      *                                                                 <V74L01>
      /                                                                 <V74L01>
       2580-SOFTLOCK SECTION.
      ***********************
       2580-START.

      * Soft lock the contract, if it is to be processed.

           MOVE 'LOCK'                 TO SFTL-FUNCTION.
           MOVE CHDRCOY                TO SFTL-COMPANY.
           MOVE 'CH'                   TO SFTL-ENTTYP.
           MOVE CHDRNUM                TO SFTL-ENTITY.
           MOVE BPRD-AUTH-CODE         TO SFTL-TRANSACTION.
           MOVE 999999                 TO SFTL-USER.

           CALL 'SFTLOCK'              USING SFTL-SFTLOCK-REC.

           IF  SFTL-STATUZ        NOT = O-K
           AND SFTL-STATUZ        NOT = 'LOCK'
               MOVE SFTL-SFTLOCK-REC         TO WSYS-SYSPARAMS
               MOVE WSYS-SYSTEM-ERROR-PARAMS TO SYSR-PARAMS
               MOVE SFTL-STATUZ              TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.

       2580-EXIT.
           EXIT.

       3000-UPDATE SECTION.
      *********************
       3010-UPDATE.

           PERFORM 3050-READ-PAYR.
           PERFORM 3100-READ-CLRF.
           PERFORM 3200-READ-ACBL.

           IF PAYR-SINSTAMT05          NOT = ZEROES                     <V74L01>
           AND TR52D-TXCODE            NOT = SPACES                     <V74L01>
              MOVE 'N'                 TO WSAA-WOP-FOUND                <V74L01>
              MOVE SPACES              TO WSAA-TR517-ITEM               <V74L01>
              MOVE SPACES              TO COVRLNB-PARAMS                <V74L01>
              MOVE CHDRLIF-CHDRCOY     TO COVRLNB-CHDRCOY               <V74L01>
              MOVE CHDRLIF-CHDRNUM     TO COVRLNB-CHDRNUM               <V74L01>
              MOVE ZEROES              TO COVRLNB-PLAN-SUFFIX           <V74L01>
              MOVE COVRLNBREC          TO COVRLNB-FORMAT                <V74L01>
              MOVE BEGN                TO COVRLNB-FUNCTION              <V74L01>
              PERFORM 3250-CHECK-WOP   UNTIL WOP-FOUND                  <V74L01>
                   OR COVRLNB-STATUZ   = ENDP                           <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           MOVE ZEROES                 TO WSAA-INSTPREM-TR.             <PHFX80>
           MOVE ZEROES                 TO WSAA-INSTPREM-COVR.           <PHFX80>
                                                                        <PHFX80>
           MOVE SPACES              TO COVR-PARAMS                      <PS011>
           MOVE CHDRLIF-CHDRCOY     TO COVR-CHDRCOY                     <PS011>
           MOVE CHDRLIF-CHDRNUM     TO COVR-CHDRNUM                     <PS011>
           MOVE ZEROES              TO COVR-PLAN-SUFFIX                 <PS011>
           MOVE COVRREC             TO COVR-FORMAT                      <PS011>
           MOVE BEGN                TO COVR-FUNCTION                    <PS011>
           PERFORM 3260-CHECK-ANN-METHOD UNTIL COVR-STATUZ = ENDP       <PS011>
                                                                        <PS011>
           MOVE 'Y'                    TO WSAA-FIRST-BILL.              <LA5134>
           IF VALID-CONTRACT                                            <UL001>
               PERFORM 4100-ALLOCATE-FLEXIBLE-PRMIUM                    <UL001>
           END-IF.                                                      <UL001>
           GO TO 3090-EXIT
      *
                                                                        <LA5134>
           PERFORM UNTIL PAYR-BILLCD > WSAA-EFFDATE-PLUS-CNTLEAD
                     OR (PAYR-BILLCD   >= BILLSPFROM                    <A06856>
                     AND PAYR-BILLCD   <  BILLSPTO                      <A06856>
                     AND BSSC-EFFECTIVE-DATE <  BILLSPTO)               <A06856>

               ADD 1                    TO CHDRLIF-TRANNO
               MOVE CHDRLIF-TRANNO      TO PAYR-TRANNO

               PERFORM A1000-GET-CURR-CONV-DATE                         <LA5134>
               PERFORM 3300-CALC-PAYR-PREM
               PERFORM 3400-ADVANCE-BTDATE
               PERFORM 3450-CALC-TAX                                    <V74L01>
               PERFORM 3500-AUTOMATIC-INCREASE
               PERFORM 3600-WRITE-LINS
               PERFORM 3700-PRODUCE-BEXT-RECORD
               PERFORM 3800-WRITE-PTRN

      *     Advance the old billed to date for subsequent billing

               MOVE PAYR-BTDATE        TO WSAA-OLD-BTDATE
                                                                        <V74L01>
      *     Add increase due and tax to outstanding amount              <V74L01>
               ADD WSAA-INCREASE-DUE                                    <V74L01>
                   WSAA-TAX            TO CHDRLIF-OUTSTAMT              <V74L01>
               ADD WSAA-INCREASE-DUE                                    <V74L01>
                   WSAA-TAX            TO PAYR-OUTSTAMT                 <V74L01>
           END-PERFORM.

            IF  PAYR-BILLCD         >= BILLSPFROM                       <A06856>
            AND PAYR-BILLCD         <  BILLSPTO                         <A06856>
            AND BSSC-EFFECTIVE-DATE <  BILLSPTO                         <A06856>
                                                                        <A06856>
               MOVE CT02           TO CONT-TOTNO                        <A06856>
               MOVE 1              TO CONT-TOTVAL                       <A06856>
               PERFORM 001-CALL-CONTOT                                  <A06856>
                                                                        <A06856>
            END-IF                                                      <A06856>
                                                                        <A06856>
           PERFORM 3900-REWRITE-CHDR.
           PERFORM 3950-REWRITE-PAYR.
                                                                        <P002>
      **** PERFORM A3000-WRITE-LETTER.                          <GAPPH2><P002>
                                                                        <GAPPH2>
           PERFORM A3100-CHECK-STAFF-PRINT.                             <GAPPH2>
                                                                        <GAPPH2>
           IF WSAA-PRINT-FLG            = 'Y'                           <GAPPH2>
              PERFORM A3000-WRITE-LETTER                                <GAPPH2>
           END-IF.                                                      <GAPPH2>
                                                                        <P002>
           PERFORM 4000-RELEASE-SOFTLOCK.                               <UL001>

       3090-EXIT.
           EXIT.
      *                                                                 <UL001>
       4100-ALLOCATE-FLEXIBLE-PRMIUM SECTION.                           <UL001>
      *****************************************                         <UL001>
       4110-START.                                                      <UL001>
                                                                        <UL001>
      * Get premium applied.                                            <UL001>
                                                                        <UL001>
           MOVE 'Y'                    TO WSAA-ALLOCATE-PRM.            <UL001>
                                                                        <UL001>
           MOVE ZEROES                 TO WSAA-OVER-TARGET,             <UL001>
                                          WSAA-PREM-NEED,               <UL001>
                                          WSAA-NUMBER.                  <UL001>
                                                                        <UL001>
           MOVE PAYR-BILLFREQ          TO WSAA-BILLFREQ.                <UL001>
      *                                                                 <UL005>
      *    Checking products are allow to skip basic premium.           <UL005>
      *                                                                 <UL005>
           MOVE 'N'                    TO WSAA-TV103-FOUND.             <UL005>
                                                                        <UL005>
           SEARCH ALL WSAA-TV103-REC                                    <UL005>
               AT END                                                   <UL005>
                    CONTINUE                                            <UL005>
               WHEN WSAA-TV103-KEY(WSAA-TV103-IX) = CHDRLIF-CNTTYPE     <UL005>
                    PERFORM A1300-READ-COVR                             <UL005>
                    MOVE 'Y'                     TO WSAA-TV103-FOUND    <UL005>
                    IF  WSAA-GOT-PAYR-AT-BTDATE   = 'Y'                 <UL005>
                        MOVE PAYRLIF-SINSTAMT01  TO WSAA-SINSTAMT01     <UL005>
                        MOVE PAYRLIF-SINSTAMT06  TO WSAA-SINSTAMT06     <UL005>
                        COMPUTE PAYRLIF-SINSTAMT01 =                    <UL005>
                            PAYRLIF-SINSTAMT01 - COVR-INSTPREM          <UL005>
                        COMPUTE PAYRLIF-SINSTAMT06 =                    <UL005>
                            PAYRLIF-SINSTAMT06 - COVR-INSTPREM          <UL005>
                    ELSE                                                <UL005>
                        MOVE PAYR-SINSTAMT01     TO WSAA-SINSTAMT01     <UL005>
                        MOVE PAYR-SINSTAMT06     TO WSAA-SINSTAMT06     <UL005>
                        COMPUTE PAYR-SINSTAMT01 =                       <UL005>
                            PAYR-SINSTAMT01 - COVR-INSTPREM             <UL005>
                        COMPUTE PAYR-SINSTAMT06 =                       <UL005>
                            PAYR-SINSTAMT06 - COVR-INSTPREM             <UL005>
                    END-IF                                              <UL005>
           END-SEARCH.                                                  <UL005>
                                                                        <CS020>
TVAN       MOVE 'N'                     TO WSAA-TZ028-FOUND             <CS020>
           SEARCH ALL WSAA-TZ028-REC                                    <CS020>
               AT END                                                   <CS020>
                    CONTINUE                                            <CS020>
               WHEN WSAA-TZ028-KEY(WSAA-TZ028-IX) = CHDRLIF-CNTTYPE     <CS020>
                  INITIALIZE                  DTC3-DATCON3-REC          <CS020>
                  MOVE O-K                    TO DTC3-STATUZ            <CS020>
                  MOVE CHDRLIF-OCCDATE        TO DTC3-INT-DATE-1        <CS020>
                  MOVE PAYR-PTDATE            TO DTC3-INT-DATE-2        <CS020>
                  MOVE '01'                   TO DTC3-FREQUENCY         <CS020>
                  CALL 'DATCON3'        USING DTC3-DATCON3-REC          <CS020>
                  IF DTC3-STATUZ              NOT = O-K                 <CS020>
                     MOVE DTC3-DATCON3-REC    TO SYSR-PARAMS            <CS020>
                     PERFORM 600-FATAL-ERROR                            <CS020>
                  END-IF                                                <CS020>
                  IF DTC3-FREQ-FACTOR  >=                               <CS020>
                                      WSAA-TZ028-NOFYEAR(WSAA-TZ028-IX) <CS020>
                     PERFORM A1300-READ-COVR                            <CS020>
                     MOVE 'Y'                     TO WSAA-TZ028-FOUND   <CS020>
                     IF WSAA-GOT-PAYR-AT-BTDATE   = 'Y'                 <CS020>
                        MOVE PAYRLIF-SINSTAMT01  TO WSAA-SINSTAMT01     <CS020>
                        MOVE PAYRLIF-SINSTAMT06  TO WSAA-SINSTAMT06     <CS020>
                        COMPUTE PAYRLIF-SINSTAMT01 =                    <CS020>
                           PAYRLIF-SINSTAMT01 - COVR-INSTPREM           <CS020>
                        COMPUTE PAYRLIF-SINSTAMT06 =                    <CS020>
                           PAYRLIF-SINSTAMT06 - COVR-INSTPREM           <CS020>
                     ELSE                                               <CS020>
                        MOVE PAYR-SINSTAMT01     TO WSAA-SINSTAMT01     <CS020>
                        MOVE PAYR-SINSTAMT06     TO WSAA-SINSTAMT06     <CS020>
                        COMPUTE PAYR-SINSTAMT01 =                       <CS020>
                            PAYR-SINSTAMT01 - COVR-INSTPREM             <CS020>
                        COMPUTE PAYR-SINSTAMT06 =                       <CS020>
                            PAYR-SINSTAMT06 - COVR-INSTPREM             <CS020>
                     END-IF                                             <CS020>
                  END-IF                                                <CS020>
TVAN       END-SEARCH.                                                  <CS020>
                                                                        <UL005>
                                                                        <UL001>
           COMPUTE WSAA-PREM-TARGT     = PAYR-SINSTAMT06                <UL001>
                                       * WSAA-BILLFREQ.                 <UL001>
                                                                        <UL001>
           PERFORM 4200-GET-PRM-APPLIED.                                <UL001>
           COMPUTE WSAA-NUMBER         = WSAA-BILLFREQ                  <UL001>
                                       - WSAA-NUM-PERIOD.               <UL001>
                                                                        <UL001>
      ***  COMPUTE WSAA-PREM-NEED      = WSAA-PREM-TARGT                <UL001>
      ***                              - WSAA-PRMTAR-APPL.              <UL001>
                                                                        <UL001>
           COMPUTE WSAA-PREM-NEED      = PAYR-SINSTAMT06                <UL001>
                                       * WSAA-NUMBER.                   <UL001>
                                                                        <UL001>
           IF  WSAA-SUSP-AVAIL          < WSAA-PREM-NEED                <UL001>
           OR PAYR-BILLFREQ            = '01'
           OR WSAA-PREM-NEED           = ZEROES
           OR WSAA-NUMBER              = 1                              <PHFX80>
               MOVE 'N'                TO WSAA-ALLOCATE-PRM             <UL001>
               GO TO 4190-EXIT                                          <UL001>
           ELSE                                                         <UL001>
               COMPUTE WSAA-OVER-TARGET                                 <UL001>
                                       = WSAA-SUSP-AVAIL                <UL001>
                                       - WSAA-PREM-NEED                 <UL001>
           END-IF.                                                      <UL001>
                                                                        <UL001>
           MOVE 1                      TO WSAA-IDX1.                    <UL001>
                                                                        <UL001>
           PERFORM VARYING WSAA-IDX1 FROM 1 BY 1
                           UNTIL WSAA-IDX1 > WSAA-NUMBER
      ***            OR (PAYR-BILLCD   >= BILLSPFROM                    <UL001>
      ***            AND PAYR-BILLCD   <  BILLSPTO                      <UL001>
      ***            AND BSSC-EFFECTIVE-DATE <  BILLSPTO)               <UL001>
                                                                        <UL001>
               ADD 1                    TO CHDRLIF-TRANNO               <UL001>
               MOVE CHDRLIF-TRANNO      TO PAYR-TRANNO                  <UL001>
                                                                        <UL001>
                                                                        <UL001>
      *     Advance the old billed to date for subsequent billing       <UL001>
                                                                        <UL001>
               MOVE PAYR-BTDATE        TO WSAA-OLD-BTDATE               <UL001>
               PERFORM X3600-WRITE-LINS                                 <UL001>
                                                                        <UL001>
      *     Add increase due and tax to outstanding amount      <UL001> <V74L01>
               ADD WSAA-INCREASE-DUE                                    <UL001>
                   WSAA-TAX            TO CHDRLIF-OUTSTAMT              <UL001>
               ADD WSAA-INCREASE-DUE                                    <UL001>
                   WSAA-TAX            TO PAYR-OUTSTAMT                 <UL001>
           END-PERFORM.                                                 <UL001>
                                                                        <UL001>
               PERFORM A1000-GET-CURR-CONV-DATE                         <UL001>
               PERFORM 3300-CALC-PAYR-PREM                              <UL001>
      ***      PERFORM 3400-ADVANCE-BTDATE                              <UL001>
               PERFORM 3450-CALC-TAX                                    <UL001>
               PERFORM 3500-AUTOMATIC-INCREASE                          <UL001>
      *****    PERFORM 3600-WRITE-LINS                                  <UL001>
               PERFORM 3700-PRODUCE-BEXT-RECORD                         <UL001>
               IF WSAA-NUMBER          > 1                              <UL010>
               OR (WSAA-NUMBER         = 1 AND                          <UL010>
                   WSAA-SKIP       NOT = 'Y')                           <UL010>
                   PERFORM 3800-WRITE-PTRN                              <UL010>
               END-IF.                                                  <UL010>
                                                                        <UL010>
      ***      PERFORM 3800-WRITE-PTRN                          <UL010> <PHFX39>
                                                                        <UL001>
            IF  PAYR-BILLCD         >= BILLSPFROM                       <UL001>
            AND PAYR-BILLCD         <  BILLSPTO                         <UL001>
            AND BSSC-EFFECTIVE-DATE <  BILLSPTO                         <UL001>
                                                                        <UL001>
               MOVE CT02           TO CONT-TOTNO                        <UL001>
               MOVE 1              TO CONT-TOTVAL                       <UL001>
               PERFORM 001-CALL-CONTOT                                  <UL001>
                                                                        <UL001>
            END-IF                                                      <UL001>
                                                                        <UL001>
           PERFORM 3900-REWRITE-CHDR.                                   <UL001>
           PERFORM 3950-REWRITE-PAYR.                                   <UL001>
                                                                        <UL001>
      **** PERFORM A3000-WRITE-LETTER.                                  <UL001>
                                                                        <UL001>
           PERFORM A3100-CHECK-STAFF-PRINT.                             <UL001>
                                                                        <UL001>
           IF WSAA-PRINT-FLG            = 'Y'                           <UL001>
              PERFORM A3000-WRITE-LETTER                                <UL001>
           END-IF.                                                      <UL001>
                                                                        <UL001>
           PERFORM 4000-RELEASE-SOFTLOCK.                               <UL001>
                                                                        <UL001>
       4190-EXIT.                                                       <UL001>
           EXIT.                                                        <UL001>
      *                                                                 <UL001>
       4200-GET-PRM-APPLIED SECTION.                                    <UL001>
      ********************************                                  <UL001>
       4210-GET.                                                        <UL001>
                                                                        <UL001>
           MOVE ZEROES                 TO WSAA-PRMTAR-APPL,             <UL001>
                                          WSAA-ENDYEAR-DATE,            <UL001>
                                          WSAA-STRYEAR-DATE.            <UL001>
                                                                        <UL001>
           PERFORM 4300-CALC-DATE-MUSTPAY.                              <UL001>
                                                                        <UL001>
      *  USE THE DATCON3 SUBROUTINE TO CALCULATE THE FREQ FACTOR        <UL001>
                                                                        <UL001>
           MOVE SPACES                 TO DTC3-DATCON3-REC              <UL001>
           MOVE WSAA-STRYEAR-DATE      TO DTC3-INT-DATE-1               <UL001>
           MOVE PAYR-PTDATE            TO DTC3-INT-DATE-2               <UL001>
           MOVE PAYR-BILLFREQ          TO DTC3-FREQUENCY                <UL001>
           CALL 'DATCON3' USING DTC3-DATCON3-REC                        <UL001>
                                                                        <UL001>
           IF DTC3-STATUZ NOT = O-K                                     <UL001>
              MOVE DTC3-DATCON3-REC    TO SYSR-PARAMS                   <UL001>
              PERFORM 600-FATAL-ERROR                                   <UL001>
           END-IF.                                                      <UL001>
                                                                        <UL001>
           MOVE DTC3-FREQ-FACTORX      TO WSAA-NUM-PERIOD.              <UL001>
                                                                        <UL001>
                                                                        <UL001>
      **   INITIALIZE                  ACMVINF-PARAMS.                  <UL001>
      **   MOVE '2'                    TO ACMVINF-RLDGCOY.              <UL001>
      **   MOVE WSAA-LE                TO ACMVINF-SACSCODE.             <UL001>
      **   MOVE WSAA-LP                TO ACMVINF-SACSTYP.              <UL001>
      **   MOVE CHDRNUM                TO ACMVINF-RLDGACCT.             <UL001>
      **   MOVE ZEROES                 TO ACMVINF-TRANNO.               <UL001>
      **   MOVE ACMVINFREC             TO ACMVINF-FORMAT.               <UL001>
      **   MOVE BEGN                   TO ACMVINF-FUNCTION.             <UL001>
      *                                                                 <UL001>
      *4220-LOOP.                                                       <UL001>
      *                                                                 <UL001>
      *    CALL 'ACMVINFIO'            USING ACMVINF-PARAMS.            <UL001>
      *                                                                 <UL001>
      *    IF ACMVINF-STATUZ           NOT = O-K                        <UL001>
      *    AND                         NOT = ENDP                       <UL001>
      *        MOVE ACMVINF-PARAMS     TO SYSR-PARAMS                   <UL001>
      *        PERFORM 600-FATAL-ERROR                                  <UL001>
      *    END-IF.                                                      <UL001>
      *                                                                 <UL001>
      *    IF ACMVINF-STATUZ           = ENDP                           <UL001>
      *    OR ACMVINF-RLDGCOY          NOT = '2'                        <UL001>
      *    OR ACMVINF-SACSCODE         NOT = WSAA-LE                    <UL001>
      *    OR ACMVINF-SACSTYP          NOT = WSAA-LP                    <UL001>
      *    OR ACMVINF-RLDGACCT(1:8)    NOT = WSAA-CHDRNUM               <UL001>
      *        MOVE ENDP               TO ACMVINF-STATUZ                <UL001>
      *        GO TO 4290-EXIT                                          <UL001>
      *    END-IF.                                                      <UL001>
      *                                                                 <UL001>
      *    IF ACMVINF-STATUZ           = O-K                            <UL001>
      *    AND ACMVINF-RLDGCOY         = '2'                            <UL001>
      *    AND ACMVINF-SACSCODE        = WSAA-LE                        <UL001>
      *    AND ACMVINF-SACSTYP         = WSAA-LP                        <UL001>
      *    AND ACMVINF-RLDGACCT(1:8)   = WSAA-CHDRNUM                   <UL001>
      *    AND ACMVINF-EFFDATE         >= WSAA-STRYEAR-DATE             <UL001>
      *    AND ACMVINF-EFFDATE         <= PAYR-PTDATE                   <UL001>
      *    AND ACMVINF-BATCTRCDE       NOT = 'B503'                     <UL001>
      *        ADD ACMVINF-ORIGAMT     TO WSAA-PRMTAR-APPL              <UL001>
      *    END-IF.                                                      <UL001>
      *                                                                 <UL001>
      *4280-NEXTR.                                                      <UL001>
      *    MOVE NEXTR                  TO ACMVINF-FUNCTION.             <UL001>
      *    GO TO 4220-LOOP.                                             <UL001>
      *                                                                 <UL001>
      *4290-EXIT.                                                       <UL001>
      *    EXIT.                                                        <UL001>
      *                                                                 <UL005>
       4290-EXIT.                                                       <UL005>
           EXIT.                                                        <UL005>
      *                                                                 <UL001>
       4300-CALC-DATE-MUSTPAY SECTION.                                  <UL001>
      *********************************                                 <UL001>
       4310-START.                                                      <UL001>
                                                                        <UL001>
           MOVE PAYR-PTDATE           TO WSAA-ENDYEAR-DATE.             <UL001>
           MOVE CHDRLIF-OCCDATE       TO WSAA-STRYEAR-DATE.             <UL001>
           MOVE 'N'                   TO WSAA-FOUND.                    <UL001>
                                                                        <UL001>
           MOVE 1                     TO WSAA-MUSTPAY.                  <UL001>
           PERFORM VARYING WSAA-MUSTPAY  FROM 1 BY 1                    <UL001>
                    UNTIL  WSAA-MUSTPAY > 3                             <UL001>
                    OR     WSAA-FOUND   = 'Y'                           <UL001>
                                                                        <UL001>
              INITIALIZE                DTC2-DATCON2-REC                <UL001>
      ***     MOVE WSAA-MUSTPAY      TO DTC2-FREQ-FACTOR                <UL001>
              MOVE 01                TO DTC2-FREQ-FACTOR                <UL001>
              MOVE '01'              TO DTC2-FREQUENCY                  <UL001>
              MOVE ZERO              TO DTC2-INT-DATE-2                 <UL001>
              MOVE WSAA-STRYEAR-DATE    TO DTC2-INT-DATE-1              <UL001>
                                                                        <UL001>
              CALL 'DATCON2'      USING DTC2-DATCON2-REC                <UL001>
                                                                        <UL001>
              IF DTC2-STATUZ        NOT = O-K                           <UL001>
                 MOVE DTC2-DATCON2-REC TO SYSR-PARAMS                   <UL001>
                 MOVE DTC2-STATUZ      TO SYSR-STATUZ                   <UL001>
                 PERFORM 600-FATAL-ERROR                                <UL001>
              END-IF                                                    <UL001>
      ***     IF DTC2-INT-DATE-2 >=  PAYR-PTDATE                        <UL001>
              IF DTC2-INT-DATE-2 >   PAYR-PTDATE                        <UL001>
                 MOVE DTC2-INT-DATE-2   TO WSAA-ENDYEAR-DATE            <UL001>
                 MOVE 'Y'               TO WSAA-FOUND                   <UL001>
              ELSE                                                      <UL001>
                 MOVE DTC2-INT-DATE-2   TO WSAA-STRYEAR-DATE            <UL001>
              END-IF                                                    <UL001>
           END-PERFORM.                                                 <UL001>
                                                                        <UL001>
       4390-EXIT.                                                       <UL001>
           EXIT.                                                        <UL001>
      *                                                                 <UL001>

       3050-READ-PAYR SECTION.
      ************************
       3050-START.

      *  Read and hold the PAYR record.

           MOVE CHDRCOY                TO PAYR-CHDRCOY.
           MOVE CHDRNUM                TO PAYR-CHDRNUM.
           MOVE PAYRSEQNO              TO PAYR-PAYRSEQNO.
           MOVE '1'                    TO PAYR-VALIDFLAG.
           MOVE READH                  TO PAYR-FUNCTION.
           MOVE PAYRREC                TO PAYR-FORMAT.

           CALL 'PAYRIO'               USING PAYR-PARAMS.

           IF  PAYR-STATUZ             NOT = O-K
               MOVE PAYR-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE PAYR-BTDATE            TO WSAA-OLD-BTDATE.
           MOVE PAYR-BILLCD            TO WSAA-OLD-BILLCD.
           MOVE PAYR-NEXTDATE          TO WSAA-OLD-NEXTDATE.            <FA5078>
                                                                        <D9604>
      * Save original amount as this is not to be updated for   <D9604>
      * flexible premium contracts                              <D9604>
           MOVE PAYR-OUTSTAMT          TO WSAA-OLD-OUTSTAMT.            <D9604>

       3060-EXIT.
           EXIT.

       3100-READ-CLRF SECTION.
      ************************
       3110-START.

      * Read the client role file using the contract header
      * to get the client who is paying.

           MOVE 'CH'                   TO CLRF-FOREPFX.
           MOVE CHDRCOY                TO CLRF-FORECOY.
           MOVE CHDRNUM                TO WSAA-CHDRNUM.
           MOVE PAYRSEQNO              TO WSAA-PAYRSEQNO.
           MOVE WSAA-FORENUM           TO CLRF-FORENUM.
           MOVE 'PY'                   TO CLRF-CLRRROLE.
           MOVE READR                  TO CLRF-FUNCTION.

           CALL 'CLRFIO'               USING CLRF-PARAMS.

           IF CLRF-STATUZ          NOT = O-K
              MOVE CLRF-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

       3199-EXIT.
           EXIT.

       3200-READ-ACBL SECTION.
      ************************
       3210-START.

           MOVE 0                      TO WSAA-SUSP-AVAIL.

           MOVE CHDRCOY                TO ACBL-RLDGCOY.
           MOVE CHDRNUM                TO ACBL-RLDGACCT.
           MOVE PAYR-BILLCURR          TO ACBL-ORIGCURR.
           MOVE T5645-SACSCODE-01      TO ACBL-SACSCODE.
           MOVE T5645-SACSTYPE-01      TO ACBL-SACSTYP.
           MOVE READR                  TO ACBL-FUNCTION.
           MOVE ACBLREC                TO ACBL-FORMAT.

           CALL 'ACBLIO'               USING ACBL-PARAMS.
           IF  ACBL-STATUZ        NOT = O-K
           AND ACBL-STATUZ        NOT = MRNF
               MOVE ACBL-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

      * Multiply the balance by the sign from T3695.

           IF ACBL-STATUZ                  = MRNF
               MOVE 0                   TO WSAA-SUSP-AVAIL
           ELSE
              IF T3695-SIGN               = '-'
                 COMPUTE WSAA-SUSP-AVAIL = (0 - ACBL-SACSCURBAL)
              ELSE
                 MOVE ACBL-SACSCURBAL TO WSAA-SUSP-AVAIL
              END-IF
           END-IF.
      *                                                                 <V4L001>
      * To include Advance Premium Deposit into premium suspense        <V4L001>
      * account WSAA-SUSP-AVAIL. So that it will show the total suspense<V4L001>
      * available.                                                      <V4L001>
      *                                                                 <V4L001>
       3220-INCLUDE-APA.                                                <V4L001>
           INITIALIZE                      RLPDLON-REC.                 <V4L001>
           MOVE INFO                       TO RLPDLON-FUNCTION.         <V4L001>
           MOVE CHDRCOY                    TO RLPDLON-CHDRCOY.          <V4L001>
           MOVE CHDRNUM                    TO RLPDLON-CHDRNUM.          <V4L001>
           MOVE ZEROES                     TO RLPDLON-PRMDEPST.         <V4L001>
           MOVE BSSC-LANGUAGE              TO RLPDLON-LANGUAGE.         <LA3998>
                                                                        <V4L001>
           CALL 'RLPDLON'                  USING RLPDLON-REC.           <V4L001>
                                                                        <V4L001>
           IF RLPDLON-STATUZ               NOT = O-K                    <V4L001>
              MOVE RLPDLON-STATUZ          TO SYSR-STATUZ               <V4L001>
              MOVE RLPDLON-REC             TO SYSR-PARAMS               <V4L001>
              PERFORM 600-FATAL-ERROR                                   <V4L001>
           END-IF.                                                      <V4L001>
                                                                        <V4L001>
           ADD RLPDLON-PRMDEPST            TO WSAA-SUSP-AVAIL.          <V4L001>
                                                                        <V4L001>
      *                                                         <LA2106><LA2106>
      * Store LP S amount just read to WSAA-PREM-SUSP           <LA2106><LA2106>
                                                                        <LA2106>
           MOVE WSAA-SUSP-AVAIL        TO WSAA-PREM-SUSP.               <LA2106>
                                                                        <V76F06>
      **** MOVE WSAA-PREM-SUSP         TO ZRDP-AMOUNT-IN.               <V76F06>
      **** MOVE PAYR-BILLCURR          TO ZRDP-CURRENCY.                <V76F06>
      **** PERFORM 8000-CALL-ROUNDING.                                  <V76F06>
      **** MOVE ZRDP-AMOUNT-OUT        TO WSAA-PREM-SUSP.               <V76F06>
                                                                        <V76F06>
           IF WSAA-PREM-SUSP           NOT = 0                          <V76F06>
              MOVE WSAA-PREM-SUSP      TO ZRDP-AMOUNT-IN                <V76F06>
              MOVE PAYR-BILLCURR       TO ZRDP-CURRENCY                 <V76F06>
              PERFORM 8000-CALL-ROUNDING                                <V76F06>
              MOVE ZRDP-AMOUNT-OUT     TO WSAA-PREM-SUSP                <V76F06>
           END-IF.                                                      <V76F06>
      *                                                         <LA2106><LA2106>
      * Read ACBL for LC DS                                     <LA2106><LA2106>
                                                                        <LA2106>
           MOVE CHDRLIF-CNTCURR        TO ACBL-ORIGCURR.                <LA2106>
           MOVE T5645-SACSCODE-03      TO ACBL-SACSCODE.                <LA2106>
           MOVE T5645-SACSTYPE-03      TO ACBL-SACSTYP.                 <LA2106>
           MOVE READR                  TO ACBL-FUNCTION.                <LA2106>
           MOVE ACBLREC                TO ACBL-FORMAT.                  <LA2106>
                                                                        <LA2106>
           CALL 'ACBLIO'               USING ACBL-PARAMS.               <LA2106>
           IF  ACBL-STATUZ        NOT = O-K                             <LA2106>
           AND ACBL-STATUZ        NOT = MRNF                            <LA2106>
               MOVE ACBL-PARAMS        TO SYSR-PARAMS                   <LA2106>
               PERFORM 600-FATAL-ERROR                                  <LA2106>
           END-IF.                                                      <LA2106>
                                                                        <LA2106>
      * Multiply the balance by the sign from T3695.            <LA2106><LA2106>
                                                                        <LA2106>
           IF ACBL-STATUZ              = MRNF                           <LA2106>
               MOVE 0                  TO WSAA-DVD-SUSP                 <LA2106>
               GO 3039-EXIT                                             <LA2106>
           END-IF.                                                      <LA2106>
                                                                        <LA2106>
           IF  ACBL-SACSCURBAL         = 0                              <LA2106>
               MOVE 0                  TO WSAA-DVD-SUSP                 <LA2106>
               GO 3039-EXIT                                             <LA2106>
           END-IF.                                                      <LA2106>
                                                                        <LA2106>
      * Read T3695 to check the sign.                           <LA2106><LA2106>
                                                                        <LA2106>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <LA2106>
           MOVE BSPR-COMPANY           TO ITEM-ITEMCOY.                 <LA2106>
           MOVE T3695                  TO ITEM-ITEMTABL.                <LA2106>
           MOVE T5645-SACSTYPE-03      TO ITEM-ITEMITEM.                <LA2106>
           MOVE READR                  TO ITEM-FUNCTION.                <LA2106>
           CALL 'ITEMIO'               USING ITEM-PARAMS.               <LA2106>
                                                                        <LA2106>
           IF ITEM-STATUZ          NOT = O-K                            <LA2106>
              MOVE ITEM-PARAMS         TO SYSR-PARAMS                   <LA2106>
              PERFORM 600-FATAL-ERROR                                   <LA2106>
           END-IF.                                                      <LA2106>
                                                                        <LA2106>
           MOVE ITEM-GENAREA           TO T3695-T3695-REC.              <LA2106>
                                                                        <LA2106>
           IF T3695-SIGN               = '-'                            <LA2106>
               COMPUTE ACBL-SACSCURBAL = ACBL-SACSCURBAL * -1           <LA2106>
           END-IF.                                                      <LA2106>
      *                                                         <LA2106><LA2106>
      * Convert if billing ccy and contract ccy differs.        <LA2106><LA2106>
                                                                        <LA2106>
           IF  CHDRLIF-BILLCURR    NOT = CHDRLIF-CNTCURR                <LA2106>
               MOVE SPACES             TO CLNK-STATUZ                   <LA2106>
               MOVE CHDRLIF-CNTCURR    TO CLNK-CURR-IN                  <LA2106>
               MOVE ACBL-SACSCURBAL    TO CLNK-AMOUNT-IN                <LA2106>
               MOVE VRCM-MAX-DATE      TO CLNK-CASHDATE                 <LA2106>
               MOVE CHDRLIF-BILLCURR   TO CLNK-CURR-OUT                 <LA2106>
               MOVE CHDRLIF-CHDRCOY    TO CLNK-COMPANY                  <LA2106>
               MOVE ZEROES             TO CLNK-AMOUNT-OUT               <LA2106>
               MOVE 'REAL'             TO CLNK-FUNCTION                 <LA2106>
               CALL 'XCVRT'  USING  CLNK-CLNK002-REC                    <LA2106>
               IF CLNK-STATUZ              NOT = '****'                 <LA2106>
                   MOVE CLNK-CLNK002-REC   TO SYSR-PARAMS               <LA2106>
                   MOVE CLNK-STATUZ        TO SYSR-STATUZ               <LA2106>
                   PERFORM 600-FATAL-ERROR                              <LA2106>
               END-IF                                                   <LA2106>
               MOVE CLNK-AMOUNT-OUT    TO WSAA-DVD-SUSP                 <LA2106>
                                                                        <V76F06>
               IF  WSAA-DVD-SUSP           NOT = 0                      <V76F06>
               MOVE WSAA-DVD-SUSP          TO ZRDP-AMOUNT-IN            <V76F06>
               MOVE CHDRLIF-BILLCURR       TO ZRDP-CURRENCY             <V76F06>
               PERFORM 8000-CALL-ROUNDING                               <V76F06>
               MOVE ZRDP-AMOUNT-OUT        TO WSAA-DVD-SUSP             <V76F06>
               END-IF                                                   <V76F06>
                                                                        <V76F06>
           ELSE                                                         <LA2106>
               MOVE ACBL-SACSCURBAL    TO WSAA-DVD-SUSP                 <LA2106>
           END-IF.                                                      <LA2106>
      *                                                         <LA2106><LA2106>
      * Consolidate total suspense                              <LA2106><LA2106>
                                                                        <LA2106>
           ADD WSAA-DVD-SUSP           TO WSAA-SUSP-AVAIL.              <LA2106>
                                                                        <LA2106>

       3039-EXIT.
           EXIT.

       3250-CHECK-WOP SECTION.                                          <V74L01>
      ************************                                          <V74L01>
       3251-START.                                                      <V74L01>
                                                                        <V74L01>
           CALL 'COVRLNBIO'            USING COVRLNB-PARAMS.            <V74L01>
                                                                        <V74L01>
           IF COVRLNB-STATUZ           NOT = O-K                        <V74L01>
           AND COVRLNB-STATUZ          NOT = ENDP                       <V74L01>
              MOVE COVRLNB-PARAMS      TO SYSR-PARAMS                   <V74L01>
              MOVE COVRLNB-STATUZ      TO SYSR-STATUZ                   <V74L01>
              PERFORM  600-FATAL-ERROR                                  <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF COVRLNB-CHDRCOY          NOT = CHDRLIF-CHDRCOY            <V74L01>
           OR COVRLNB-CHDRNUM          NOT = CHDRLIF-CHDRNUM            <V74L01>
           OR COVRLNB-STATUZ               = ENDP                       <V74L01>
              MOVE ENDP                TO COVRLNB-STATUZ                <V74L01>
              GO TO 3259-EXIT                                           <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
      * Check to see if coverage is found in TR517                      <V74L01>
                                                                        <V74L01>
           MOVE SPACES                 TO ITDM-PARAMS.                  <V74L01>
           MOVE 'IT'                   TO ITDM-ITEMPFX.                 <V74L01>
           MOVE COVRLNB-CHDRCOY        TO ITDM-ITEMCOY.                 <V74L01>
           MOVE TR517                  TO ITDM-ITEMTABL.                <V74L01>
           MOVE COVRLNB-CRTABLE        TO ITDM-ITEMITEM.                <V74L01>
           MOVE COVRLNB-CRRCD          TO ITDM-ITMFRM.                  <V74L01>
           MOVE ITEMREC                TO ITDM-FORMAT.                  <V74L01>
           MOVE BEGN                   TO ITDM-FUNCTION.                <V74L01>
                                                                        <V74L01>
           CALL 'ITDMIO'               USING ITDM-PARAMS.               <V74L01>
                                                                        <V74L01>
           IF  ITDM-STATUZ             NOT =  O-K AND                   <V74L01>
               ITDM-STATUZ             NOT =  ENDP                      <V74L01>
               MOVE ITDM-PARAMS        TO SYSR-PARAMS                   <V74L01>
               MOVE ITDM-STATUZ        TO SYSR-STATUZ                   <V74L01>
               PERFORM 600-FATAL-ERROR                                  <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF ITDM-ITEMCOY             NOT =  COVRLNB-CHDRCOY OR        <V74L01>
              ITDM-ITEMITEM            NOT =  COVRLNB-CRTABLE OR        <V74L01>
              ITDM-ITEMTABL            NOT =  TR517           OR        <V74L01>
              ITDM-STATUZ              =  ENDP                          <V74L01>
              MOVE SPACES              TO TR517-TR517-REC               <V74L01>
           ELSE                                                         <V74L01>
              MOVE ITDM-GENAREA        TO TR517-TR517-REC               <V74L01>
              MOVE 'Y'                 TO WSAA-WOP-FOUND                <V74L01>
              MOVE ITDM-ITEMITEM       TO WSAA-TR517-ITEM               <V74L01>
              GO TO 3259-EXIT                                           <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           MOVE NEXTR                  TO COVRLNB-FUNCTION.             <V74L01>
                                                                        <V74L01>
       3259-EXIT.                                                       <V74L01>
           EXIT.                                                        <V74L01>
                                                                        <V74L01>
       3260-CHECK-ANN-METHOD SECTION.                                   <PS011>
      ********************************                                  <PS011>
       3261-START.                                                      <PS011>
                                                                        <PS011>
           CALL 'COVRIO'              USING COVR-PARAMS.                <PS011>
                                                                        <PS011>
           IF COVR-STATUZ             NOT = O-K                         <PS011>
           AND COVR-STATUZ            NOT = ENDP                        <PS011>
              MOVE COVR-PARAMS        TO SYSR-PARAMS                    <PS011>
              MOVE COVR-STATUZ        TO SYSR-STATUZ                    <PS011>
              PERFORM  600-FATAL-ERROR                                  <PS011>
           END-IF.                                                      <PS011>
                                                                        <PS011>
           IF COVR-CHDRCOY             NOT = CHDRLIF-CHDRCOY            <PS011>
           OR COVR-CHDRNUM             NOT = CHDRLIF-CHDRNUM            <PS011>
           OR COVR-STATUZ                  = ENDP                       <PS011>
              MOVE ENDP                TO COVR-STATUZ                   <PS011>
              GO TO 3269-EXIT                                           <PS011>
           END-IF.                                                      <PS011>
                                                                        <PHFX80>
           IF COVR-STATUZ                  = O-K                        <PHFX80>
           AND COVR-CHDRCOY                = CHDRLIF-CHDRCOY            <PHFX80>
           AND COVR-CHDRNUM                = CHDRLIF-CHDRNUM            <PHFX80>
           AND COVR-VALIDFLAG              = '1'                        <PHFX80>
              IF (COVR-STATCODE            = 'TR'                       <PHFX80>
              AND COVR-PSTATCODE           = 'TR')                      <PHFX80>
                   ADD COVR-INSTPREM        TO WSAA-INSTPREM-TR         <PHFX80>
              ELSE                                                      <PHFX80>
                  IF (COVR-STATCODE        = 'IF'                       <PHFX80>
                  AND COVR-PSTATCODE       = 'PP')                      <PHFX80>
                  OR (COVR-STATCODE        = 'IF'                       <PHFX80>
                  AND COVR-PSTATCODE       = 'FP'                       <PHFX80>
                  AND WSAA-OLD-BTDATE       < COVR-CURRFROM)            <PHFX80>
                      ADD COVR-INSTPREM    TO WSAA-INSTPREM-COVR        <PHFX80>
                  END-IF                                                <PHFX80>
              END-IF                                                    <PHFX80>
           END-IF.                                                      <PHFX80>
                                                                        <PHFX80>
      * Check to see if anniversary method is found in T5687            <PS011>
                                                                        <PS011>
           MOVE SPACES                 TO ITDM-PARAMS.                  <PS011>
           MOVE 'IT'                   TO ITDM-ITEMPFX.                 <PS011>
           MOVE COVR-CHDRCOY           TO ITDM-ITEMCOY.                 <PS011>
           MOVE T5687                  TO ITDM-ITEMTABL.                <PS011>
           MOVE COVR-CRTABLE           TO ITDM-ITEMITEM.                <PS011>
           MOVE COVR-CRRCD             TO ITDM-ITMFRM.                  <PS011>
           MOVE ITEMREC                TO ITDM-FORMAT.                  <PS011>
           MOVE BEGN                   TO ITDM-FUNCTION.                <PS011>
                                                                        <PS011>
           CALL 'ITDMIO'               USING ITDM-PARAMS.               <PS011>
           IF  ITDM-STATUZ             NOT =  O-K AND                   <PS011>
               ITDM-STATUZ             NOT =  ENDP                      <PS011>
               MOVE ITDM-PARAMS        TO SYSR-PARAMS                   <PS011>
               MOVE ITDM-STATUZ        TO SYSR-STATUZ                   <PS011>
               PERFORM 600-FATAL-ERROR                                  <PS011>
           END-IF.                                                      <PS011>
                                                                        <PS011>
           IF ITDM-ITEMCOY             NOT =  COVR-CHDRCOY OR           <PS011>
              ITDM-ITEMITEM            NOT =  COVR-CRTABLE OR           <PS011>
              ITDM-ITEMTABL            NOT =  T5687        OR           <PS011>
              ITDM-STATUZ              =  ENDP                          <PS011>
              MOVE SPACES              TO T5687-T5687-REC               <PS011>
           ELSE                                                         <PS011>
              MOVE ITDM-GENAREA        TO T5687-T5687-REC               <PS011>
              IF  T5687-ANNIVERSARY-METHOD > SPACES                     <PS011>
              AND COVR-ANNIV-PROC-DATE <= 0                             <PS011>
              AND COVR-VALIDFLAG = '1'                                  <PS011>
                  PERFORM 3270-CALC-ANNIV-DATE                          <PS011>
                  MOVE WSAA-POLYEAR-ENDDATE  TO COVR-ANNIV-PROC-DATE    <PS011>
                  MOVE UPDAT                 TO COVR-FUNCTION           <PS011>
                  CALL 'COVRIO'              USING COVR-PARAMS          <PS011>
                  IF  COVR-STATUZ            NOT = O-K                  <PS011>
                      MOVE COVR-PARAMS       TO SYSR-PARAMS             <PS011>
                      MOVE COVR-STATUZ       TO SYSR-STATUZ             <PS011>
                      PERFORM  600-FATAL-ERROR                          <PS011>
                  END-IF                                                <PS011>
              END-IF                                                    <PS011>
           END-IF.                                                      <PS011>
                                                                        <PS011>
           MOVE NEXTR                  TO COVR-FUNCTION.                <PS011>
                                                                        <PS011>
       3269-EXIT.                                                       <PS011>
           EXIT.                                                        <PS011>
                                                                        <PS011>
       3270-CALC-ANNIV-DATE SECTION.                                    <PS011>
      *******************************                                   <PS011>
       3271-START.                                                      <PS011>
           MOVE PAYR-PTDATE           TO WSAA-POLYEAR-ENDDATE.          <PS011>
           MOVE CHDRLIF-OCCDATE       TO WSAA-POLYEAR-STDATE.           <PS011>
           MOVE 'N'                   TO WSAA-POLYEAR-FOUND.            <PS011>
           PERFORM UNTIL WSAA-POLYEAR-FOUND = 'Y'                       <PS011>
              INITIALIZE                DTC2-DATCON2-REC                <PS011>
              MOVE  01               TO DTC2-FREQ-FACTOR                <PS011>
              MOVE '01'              TO DTC2-FREQUENCY                  <PS011>
              MOVE ZERO              TO DTC2-INT-DATE-2                 <PS011>
              MOVE WSAA-POLYEAR-STDATE  TO DTC2-INT-DATE-1              <PS011>
                                                                        <PS011>
              CALL 'DATCON2'      USING DTC2-DATCON2-REC                <PS011>
              IF DTC2-STATUZ        NOT = O-K                           <PS011>
                 MOVE DTC2-DATCON2-REC TO SYSR-PARAMS                   <PS011>
                 MOVE DTC2-STATUZ      TO SYSR-STATUZ                   <PS011>
                 PERFORM 600-FATAL-ERROR                                <PS011>
              END-IF                                                    <PS011>
              IF DTC2-INT-DATE-2 >=  PAYR-PTDATE                        <PS011>
                 MOVE DTC2-INT-DATE-2   TO WSAA-POLYEAR-ENDDATE         <PS011>
                 MOVE 'Y'               TO WSAA-POLYEAR-FOUND           <PS011>
              ELSE                                                      <PS011>
                 MOVE DTC2-INT-DATE-2   TO WSAA-POLYEAR-STDATE          <PS011>
              END-IF                                                    <PS011>
           END-PERFORM.                                                 <PS011>
                                                                        <PS011>
       3279-EXIT.                                                       <PS011>
           EXIT.                                                        <PS011>
                                                                        <PS011>
       3300-CALC-PAYR-PREM SECTION.                                     <PS011>
      *****************************                                     <PS011>
       3310-START.                                                      <PS011>
                                                                        <PS011>
      * Check if the PAYR record has the correct premium for the        <PS011>
      * instalment date, if not obtain it from history records          <PS011>
                                                                        <PS011>
           MOVE PAYR-BILLCD            TO WSAA-OLD-BILLCD.              <PS011>
           MOVE PAYR-NEXTDATE          TO WSAA-OLD-NEXTDATE.            <FA5078>
           MOVE 'N'                    TO WSAA-GOT-PAYR-AT-BTDATE.

           IF PAYR-BTDATE < PAYR-EFFDATE

              MOVE CHDRNUM             TO PAYRLIF-CHDRNUM
              MOVE CHDRCOY             TO PAYRLIF-CHDRCOY
              MOVE ZERO                TO PAYRLIF-PAYRSEQNO
              MOVE PAYRLIFREC          TO PAYRLIF-FORMAT
              MOVE BEGN                TO PAYRLIF-FUNCTION

              MOVE 'Y'                 TO WSAA-CORRECT-PAYR-FOUND       <CAS1.0>
              PERFORM 3310-FIND-CORRECT-PAYR
                      UNTIL WSAA-GOT-PAYR-AT-BTDATE = 'Y'
                         OR WSAA-CORRECT-PAYR-FOUND = 'N'               <CAS1.0>
           END-IF.

           IF WSAA-GOT-PAYR-AT-BTDATE            = 'Y'
      ****     COMPUTE CONT-TOTVAL     = PAYRLIF-SINSTAMT06 +           <A06275>
      ****                               WSAA-INCREASE-DUE              <A06275>
    ****       COMPUTE PAYR-OUTSTAMT   = PAYRLIF-OUTSTAMT +             <LA5134>
    ****                                 PAYRLIF-SINSTAMT06     <LA5134><CAS1.0>
      ****                               PAYRLIF-SINSTAMT06 +           <CAS1.0>
      ****                               WSAA-INCREASE-DUE              <CAS1.0>
               IF WSAA-FIRST-BILL       = 'Y'                           <LA5134>
                 COMPUTE PAYR-OUTSTAMT  = PAYRLIF-OUTSTAMT +            <LA5134>
                                          PAYRLIF-SINSTAMT06            <LA5134>
                 MOVE 'N'              TO WSAA-FIRST-BILL               <LA5134>
               ELSE                                                     <LA5134>
                 COMPUTE PAYR-OUTSTAMT  = PAYR-OUTSTAMT  +              <LA5134>
                                          PAYRLIF-SINSTAMT06            <LA5134>
               END-IF                                                   <LA5134>
               MOVE PAYR-OUTSTAMT      TO CHDRLIF-OUTSTAMT
           ELSE
      ****     COMPUTE CONT-TOTVAL     = PAYR-SINSTAMT06 +              <A06275>
      ****                               WSAA-INCREASE-DUE              <A06275>
               COMPUTE PAYR-OUTSTAMT   = PAYR-OUTSTAMT +
                                         PAYR-SINSTAMT06                <CAS1.0>
      ****                               PAYR-SINSTAMT06 +              <CAS1.0>
      ****                               WSAA-INCREASE-DUE              <CAS1.0>
               MOVE PAYR-OUTSTAMT      TO CHDRLIF-OUTSTAMT
           END-IF.

      *  Log total amnount billed

      ****                                                              <A06275>
      **** Wait until the pending automatic increase amount has been    <A06275>
      **** calculated before logging the total amount billed.           <A06275>
      ****                                                              <A06275>
      **** MOVE CT04                   TO CONT-TOTNO.                   <A06275>
      **** PERFORM 001-CALL-CONTOT.                                     <A06275>


       3399-EXIT.
           EXIT.

       3310-FIND-CORRECT-PAYR SECTION.
      ********************************
       3311-START.

      *  Read PAYRLIF to get the history record for the instalment
      *  amount. There could be more than one history record, so
      *  check the PAYR-EFFDATE to see if it is compatible with the
      *  billing date.

           CALL 'PAYRLIFIO'            USING PAYRLIF-PARAMS.

           IF  PAYRLIF-STATUZ         NOT = O-K                         <CAS1.0>
           AND PAYRLIF-STATUZ         NOT = ENDP                        <CAS1.0>
              MOVE PAYRLIF-PARAMS     TO SYSR-PARAMS                    <CAS1.0>
              PERFORM 600-FATAL-ERROR                                   <CAS1.0>
           END-IF.                                                      <CAS1.0>
                                                                        <CAS1.0>
           IF PAYRLIF-STATUZ          NOT = O-K OR
              PAYRLIF-CHDRNUM         NOT = CHDRNUM OR
              PAYRLIF-CHDRCOY         NOT = CHDRCOY
              MOVE 'N'                TO WSAA-CORRECT-PAYR-FOUND        <CAS1.0>
              GO TO 3319-EXIT                                           <CAS1.0>
      ****    MOVE PAYRLIF-PARAMS     TO SYSR-PARAMS                    <CAS1.0>
      ****    PERFORM 600-FATAL-ERROR                                   <CAS1.0>
           END-IF.

           IF  PAYR-BTDATE          NOT < PAYRLIF-EFFDATE
               MOVE 'Y'                TO WSAA-GOT-PAYR-AT-BTDATE
           END-IF.

           MOVE NEXTR                  TO PAYRLIF-FUNCTION.

       3319-EXIT.
           EXIT.

       3400-ADVANCE-BTDATE SECTION.
      *****************************
       3410-START.

      * Advance the 'Billed to date' by one Frequency.
      * The current saved BTDATE is used, so that the INSTFROM and
      * BILLdates can be updated.

           MOVE 1                      TO DTC4-FREQ-FACTOR
                                          DTC2-FREQ-FACTOR.
           MOVE PAYR-BILLFREQ          TO DTC4-FREQUENCY
                                          DTC2-FREQUENCY.
           MOVE PAYR-BTDATE            TO DTC4-INT-DATE-1
                                          DTC2-INT-DATE-1.
           MOVE PAYR-DUEDD             TO DTC4-BILLDAY.
           MOVE PAYR-DUEMM             TO DTC4-BILLMONTH.

           CALL 'DATCON4'              USING DTC4-DATCON4-REC.

           IF DTC4-STATUZ              NOT = O-K
               MOVE DTC4-DATCON4-REC   TO SYSR-PARAMS
               MOVE DTC4-STATUZ        TO SYSR-STATUZ                   <CAS1.0>
               PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE DTC4-INT-DATE-2        TO PAYR-BTDATE
                                          CHDRLIF-BTDATE.
           MOVE PAYR-BILLCD            TO WSAA-OLD-BILLCD.
           MOVE PAYR-NEXTDATE          TO WSAA-OLD-NEXTDATE.            <FA5078>

      * Advance the BILLING COMMENCEMENT DATE by one frequency

           MOVE 1                      TO DTC4-FREQ-FACTOR.
           MOVE PAYR-BILLFREQ          TO DTC4-FREQUENCY.
           MOVE PAYR-BILLCD            TO DTC4-INT-DATE-1.
           MOVE PAYR-BILLDAY           TO DTC4-BILLDAY.
           MOVE PAYR-BILLMONTH         TO DTC4-BILLMONTH.

           CALL 'DATCON4'              USING DTC4-DATCON4-REC.

           IF DTC4-STATUZ          NOT = O-K
              MOVE DTC4-DATCON4-REC    TO SYSR-PARAMS
              MOVE DTC4-STATUZ         TO SYSR-STATUZ                   <CAS1.0>
              PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE DTC4-INT-DATE-2        TO PAYR-BILLCD
                                          CHDRLIF-BILLCD.

      * Subtract the lead days from the billed to date to calculate
      * the next billing extract date for the PAYR record.

           COMPUTE DTC2-FREQ-FACTOR =
                         ( ZERO - WSAA-T6654-LEADDAY (WSAA-T6654-IX)).
           MOVE 'DY'                   TO DTC2-FREQUENCY.
           MOVE PAYR-BILLCD            TO DTC2-INT-DATE-1.

           CALL 'DATCON2'              USING DTC2-DATCON2-REC.

           IF DTC2-STATUZ           NOT = O-K
              MOVE DTC2-DATCON2-REC    TO SYSR-PARAMS
              MOVE DTC2-STATUZ         TO SYSR-STATUZ                   <CAS1.0>
              PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE DTC2-INT-DATE-2        TO PAYR-NEXTDATE.

       3499-EXIT.
           EXIT.

       3450-CALC-TAX SECTION.                                           <V74L01>
      ***********************                                           <V74L01>
       3451-START.                                                      <V74L01>
                                                                        <V74L01>
           MOVE ZERO                   TO WSAA-TAX.                     <V74L01>
                                                                        <V74L01>
           IF FLEXIBLE-PREMIUM-CONTRACT                                 <V74L01>
           OR TR52D-TXCODE             = SPACES                         <V74L01>
              GO TO 3459-EXIT                                           <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
      * Read all active coverages for the contract                      <V74L01>
                                                                        <V74L01>
           MOVE SPACES                 TO COVRLNB-PARAMS.               <V74L01>
           MOVE CHDRLIF-CHDRNUM        TO COVRLNB-CHDRNUM.              <V74L01>
           MOVE CHDRLIF-CHDRCOY        TO COVRLNB-CHDRCOY.              <V74L01>
           MOVE ZEROES                 TO COVRLNB-PLAN-SUFFIX.          <V74L01>
           MOVE COVRLNBREC             TO COVRLNB-FORMAT.               <V74L01>
           MOVE BEGN                   TO COVRLNB-FUNCTION.             <V74L01>
                                                                        <V74L01>
           PERFORM 345A-PROCESS-COVR-TAX UNTIL                          <V74L01>
               COVRLNB-STATUZ = ENDP.                                   <V74L01>
                                                                        <V74L01>
           IF PAYR-SINSTAMT02          >  0                             <V74L01>
              PERFORM 345B-PROCESS-CTFEE-TAX                            <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
       3459-EXIT.                                                       <V74L01>
           EXIT.                                                        <V74L01>
                                                                        <V74L01>
       345A-PROCESS-COVR-TAX SECTION.                                   <V74L01>
      *******************************                                   <V74L01>
       345A-START.                                                      <V74L01>
      * Calculate tax on premiums.                                      <V74L01>
                                                                        <V74L01>
           MOVE 'N'                    TO WSAA-VALID-COVR               <V74L01>
                                          WSAA-COVR-CALC-TAX.           <V74L01>
                                                                        <V74L01>
           CALL 'COVRLNBIO'            USING COVRLNB-PARAMS.            <V74L01>
           IF COVRLNB-STATUZ           NOT = O-K                        <V74L01>
           AND COVRLNB-STATUZ          NOT = ENDP                       <V74L01>
              MOVE COVRLNB-PARAMS      TO SYSR-PARAMS                   <V74L01>
              MOVE COVRLNB-STATUZ      TO SYSR-STATUZ                   <V74L01>
              PERFORM  600-FATAL-ERROR                                  <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF COVRLNB-CHDRCOY          NOT = CHDRLIF-CHDRCOY            <V74L01>
           OR COVRLNB-CHDRNUM          NOT = CHDRLIF-CHDRNUM            <V74L01>
           OR COVRLNB-STATUZ               = ENDP                       <V74L01>
              MOVE ENDP                TO COVRLNB-STATUZ                <V74L01>
              GO TO 345A-EXIT                                           <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
      * Check to see CURRFROM <= PAYR-BTDATE & CURRTO > PAYR-BTDATE     <V74L01>
                                                                        <V74L01>
           IF (COVRLNB-CURRFROM        < PAYR-BTDATE                    <V74L01>
           OR  COVRLNB-CURRFROM        = PAYR-BTDATE)                   <V74L01>
           AND COVRLNB-CURRTO          > PAYR-BTDATE                    <V74L01>
              NEXT SENTENCE                                             <V74L01>
           ELSE                                                         <V74L01>
              MOVE NEXTR               TO COVRLNB-FUNCTION              <V74L01>
              GO TO 345A-EXIT                                           <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
      * If installment premium = zero, exit                             <V74L01>
                                                                        <V74L01>
           IF COVRLNB-INSTPREM         = ZERO                           <V74L01>
              MOVE NEXTR               TO COVRLNB-FUNCTION              <V74L01>
              GO TO 345A-EXIT                                           <V74L01>
           END-IF                                                       <V74L01>
                                                                        <V74L01>
      * Check to see if coverage is of a valid status                   <V74L01>
                                                                        <V74L01>
           IF COVRLNB-VALIDFLAG        NOT = '1'                        <V74L01>
              MOVE NEXTR               TO COVRLNB-FUNCTION              <V74L01>
              GO TO 345A-EXIT                                           <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           PERFORM VARYING WSAA-T5679-SUB                               <V74L01>
                   FROM 1 BY 1                                          <V74L01>
                   UNTIL WSAA-T5679-SUB      > 12                       <V74L01>
                                                                        <V74L01>
              IF T5679-COV-RISK-STAT (WSAA-T5679-SUB)= COVRLNB-STATCODE <V74L01>
                 PERFORM VARYING WSAA-T5679-SUB                         <V74L01>
                         FROM 1 BY 1                                    <V74L01>
                         UNTIL WSAA-T5679-SUB   > 12                    <V74L01>
                    IF T5679-COV-PREM-STAT (WSAA-T5679-SUB)             <V74L01>
                                                    = COVRLNB-PSTATCODE <V74L01>
                       MOVE  13  TO WSAA-T5679-SUB                      <V74L01>
                       MOVE 'Y'  TO WSAA-VALID-COVR                     <V74L01>
                    END-IF                                              <V74L01>
                 END-PERFORM                                            <V74L01>
              END-IF                                                    <V74L01>
           END-PERFORM.                                                 <V74L01>
                                                                        <V74L01>
      *  If the coverage is not of a valid status read the next         <V74L01>
      *  record for the contract:                                       <V74L01>
                                                                        <V74L01>
           IF NOT VALID-COVR                                            <V74L01>
              MOVE NEXTR            TO COVRLNB-FUNCTION                 <V74L01>
              GO TO 345A-EXIT                                           <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
      *  Read table TR52E.                                              <V74L01>
           MOVE SPACES                 TO WSAA-TR52E-KEY.               <V74L01>
           MOVE TR52D-TXCODE           TO WSAA-TR52E-TXCODE.            <V74L01>
           MOVE CHDRLIF-CNTTYPE        TO WSAA-TR52E-CNTTYPE.           <V74L01>
           MOVE COVRLNB-CRTABLE        TO WSAA-TR52E-CRTABLE.           <V74L01>
           PERFORM A1100-READ-TR52E.                                    <V74L01>
                                                                        <V74L01>
           IF TR52E-TR52E-REC = SPACES                                  <V74L01>
              MOVE SPACES              TO WSAA-TR52E-KEY                <V74L01>
              MOVE TR52D-TXCODE        TO WSAA-TR52E-TXCODE             <V74L01>
              MOVE CHDRLIF-CNTTYPE     TO WSAA-TR52E-CNTTYPE            <V74L01>
              MOVE '****'              TO WSAA-TR52E-CRTABLE            <V74L01>
              PERFORM A1100-READ-TR52E                                  <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF TR52E-TR52E-REC = SPACES                                  <V74L01>
              MOVE SPACES              TO WSAA-TR52E-KEY                <V74L01>
              MOVE TR52D-TXCODE        TO WSAA-TR52E-TXCODE             <V74L01>
              MOVE '***'               TO WSAA-TR52E-CNTTYPE            <V74L01>
              MOVE '****'              TO WSAA-TR52E-CRTABLE            <V74L01>
              PERFORM A1100-READ-TR52E                                  <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
      *  If TR52E tax indicator not = 'Y', do not calculate tax         <V74L01>
                                                                        <V74L01>
           IF TR52E-TAXIND-01          NOT = 'Y'                        <V74L01>
              MOVE NEXTR               TO COVRLNB-FUNCTION              <V74L01>
              GO TO 345A-EXIT                                           <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
      *  If PAYR-SINSTAMT05 not = zero, check to see if component       <V74L01>
      * exists in TR517                                                 <V74L01>
           IF PAYR-SINSTAMT05          NOT = ZERO                       <V74L01>
           AND COVRLNB-CRTABLE         = WSAA-TR517-ITEM                <V74L01>
           AND TR517-ZRWVFLG-01        = 'Y'                            <V74L01>
      **      Waive the premium of the wop component.                   <V74L01>
              MOVE NEXTR               TO COVRLNB-FUNCTION              <V74L01>
              GO TO 345A-EXIT                                           <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           MOVE ZERO                   TO WSAA-TR517-IX.                <V74L01>
           IF PAYR-SINSTAMT05          NOT = ZERO                       <V74L01>
              PERFORM VARYING WSAA-TR517-IX FROM 1 BY 1 UNTIL           <V74L01>
                       WSAA-TR517-IX   > 50                             <V74L01>
               IF TR517-CTABLE (WSAA-TR517-IX) = SPACES                 <V74L01>
                  MOVE 51              TO WSAA-TR517-IX                 <V74L01>
               ELSE                                                     <V74L01>
                IF TR517-CTABLE (WSAA-TR517-IX)                         <V74L01>
                                       =  COVRLNB-CRTABLE               <V74L01>
                   MOVE 'Y'            TO WSAA-COVR-CALC-TAX            <V74L01>
                   MOVE 51             TO WSAA-TR517-IX                 <V74L01>
                END-IF                                                  <V74L01>
               END-IF                                                   <V74L01>
              END-PERFORM                                               <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
      *  If component found in TR517, do not calculate tax              <V74L01>
                                                                        <V74L01>
           IF WSAA-COVR-CALC-TAX       =  'Y'                           <V74L01>
              MOVE NEXTR               TO COVRLNB-FUNCTION              <V74L01>
              GO TO 345A-EXIT                                           <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
      *  Check if component found in INCR where INCD-CRRCD < PAYR-BTDATE<V74L01>
      *  and validflag = '1'                                            <V74L01>
                                                                        <V74L01>
           MOVE SPACES                 TO INCR-DATA-AREA.               <V74L01>
           MOVE COVRLNB-CHDRCOY        TO INCR-CHDRCOY.                 <V74L01>
           MOVE COVRLNB-CHDRNUM        TO INCR-CHDRNUM.                 <V74L01>
           MOVE COVRLNB-LIFE           TO INCR-LIFE.                    <V74L01>
           MOVE COVRLNB-COVERAGE       TO INCR-COVERAGE.                <V74L01>
           MOVE COVRLNB-RIDER          TO INCR-RIDER.                   <V74L01>
           MOVE COVRLNB-PLAN-SUFFIX    TO INCR-PLAN-SUFFIX.             <V74L01>
           MOVE INCRREC                TO INCR-FORMAT.                  <V74L01>
           MOVE READR                  TO INCR-FUNCTION.                <V74L01>
                                                                        <V74L01>
           CALL 'INCRIO'            USING INCR-PARAMS.                  <V74L01>
                                                                        <V74L01>
           IF  INCR-STATUZ             NOT = O-K                        <V74L01>
           AND INCR-STATUZ             NOT = MRNF                       <V74L01>
               MOVE INCR-PARAMS        TO SYSR-PARAMS                   <V74L01>
               MOVE INCR-STATUZ        TO SYSR-STATUZ                   <V74L01>
               PERFORM 600-FATAL-ERROR                                  <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF  INCR-CHDRCOY            = COVRLNB-CHDRCOY                <V74L01>
           AND INCR-CHDRNUM            = COVRLNB-CHDRNUM                <V74L01>
           AND INCR-LIFE               = COVRLNB-LIFE                   <V74L01>
           AND INCR-COVERAGE           = COVRLNB-COVERAGE               <V74L01>
           AND INCR-RIDER              = COVRLNB-RIDER                  <V74L01>
           AND INCR-PLAN-SUFFIX        = COVRLNB-PLAN-SUFFIX            <V74L01>
           AND INCR-STATUZ             NOT = ENDP                       <V74L01>
           AND INCR-VALIDFLAG          = '1'                            <V74L01>
           AND INCR-CRRCD              < PAYR-BTDATE                    <V74L01>
               COMPUTE WSAA-INCR-BINSTPREM ROUNDED                      <V74L01>
                                       = INCR-ZBNEWINST                 <V74L01>
                                       - INCR-ZBLASTINST                <V74L01>
               COMPUTE WSAA-INCR-INSTPREM ROUNDED                       <V74L01>
                                       = INCR-NEWINST                   <V74L01>
                                       - INCR-LAST-INST                 <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
      *  Set values to tax linkage and call tax subroutine              <V74L01>
                                                                        <V74L01>
           INITIALIZE                     TXCL-LINK-REC.                <V74L01>
           MOVE 'CALC'                 TO TXCL-FUNCTION.                <V74L01>
           MOVE O-K                    TO TXCL-STATUZ.                  <V74L01>
           MOVE 'PREM'                 TO TXCL-TRANS-TYPE.              <V74L01>
           MOVE COVRLNB-CHDRCOY        TO TXCL-CHDRCOY.                 <V74L01>
           MOVE COVRLNB-CHDRNUM        TO TXCL-CHDRNUM.                 <V74L01>
           MOVE WSAA-TR52E-KEY         TO TXCL-TAXRULE.                 <V74L01>
           MOVE SPACES                 TO WSAA-RATE-ITEM.               <V74L01>
           MOVE CHDRLIF-CNTCURR        TO TXCL-CCY                      <V74L01>
                                          WSAA-CNT-CURR.                <V74L01>
           MOVE TR52E-TXITEM           TO WSAA-TXITEM.                  <V74L01>
           MOVE WSAA-RATE-ITEM         TO TXCL-RATE-ITEM.               <V74L01>
           MOVE TR52D-TXCODE           TO TXCL-TXCODE.                  <V74L01>
           MOVE ZERO                   TO TXCL-AMOUNT-IN.               <V74L01>
           IF TR52E-ZBASTYP            =  'Y'                           <V74L01>
              COMPUTE TXCL-AMOUNT-IN ROUNDED                            <V74L01>
                                       = COVRLNB-ZBINSTPREM             <V74L01>
                                       + WSAA-INCR-BINSTPREM            <V74L01>
           ELSE                                                         <V74L01>
              COMPUTE TXCL-AMOUNT-IN ROUNDED                            <V74L01>
                                       = COVRLNB-INSTPREM               <V74L01>
                                       + WSAA-INCR-INSTPREM             <V74L01>
           END-IF.                                                      <V74L01>
           MOVE WSAA-OLD-BTDATE        TO TXCL-EFFDATE.                 <V74L01>
           MOVE SPACES                 TO TXCL-TAX-TYPE(1)              <V74L01>
                                          TXCL-TAX-TYPE(2)              <V74L01>
                                          TXCL-TAX-ABSORB(1)            <V74L01>
                                          TXCL-TAX-ABSORB(2).           <V74L01>
           MOVE ZEROES                 TO TXCL-TAX-AMT(1)               <V74L01>
                                          TXCL-TAX-AMT(2).              <V74L01>
                                                                        <V74L01>
           CALL TR52D-TXSUBR        USING TXCL-LINK-REC.                <V74L01>
                                                                        <V74L01>
           IF TXCL-STATUZ              NOT = O-K                        <V74L01>
              MOVE TXCL-LINK-REC       TO SYSR-PARAMS                   <V74L01>
              MOVE TXCL-STATUZ         TO SYSR-STATUZ                   <V74L01>
              PERFORM 600-FATAL-ERROR                                   <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF TXCL-TAX-AMT(1)          > ZERO                           <V74L01>
           OR TXCL-TAX-AMT(2)          > ZERO                           <V74L01>
              IF TXCL-TAX-ABSORB(1)    NOT = 'Y'                        <V74L01>
                 ADD TXCL-TAX-AMT(1)   TO WSAA-TAX                      <V74L01>
              END-IF                                                    <V74L01>
              IF TXCL-TAX-ABSORB(2)    NOT = 'Y'                        <V74L01>
                 ADD TXCL-TAX-AMT(2)   TO WSAA-TAX                      <V74L01>
              END-IF                                                    <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
      *  Create TAXD record                                             <V74L01>
                                                                        <V74L01>
           INITIALIZE                     TAXD-DATA-AREA.               <V74L01>
           MOVE COVRLNB-CHDRCOY        TO TAXD-CHDRCOY.                 <V74L01>
           MOVE COVRLNB-CHDRNUM        TO TAXD-CHDRNUM.                 <V74L01>
           MOVE 'PREM'                 TO TAXD-TRANTYPE.                <V74L01>
           MOVE COVRLNB-LIFE           TO TAXD-LIFE.                    <V74L01>
           MOVE COVRLNB-COVERAGE       TO TAXD-COVERAGE.                <V74L01>
           MOVE COVRLNB-RIDER          TO TAXD-RIDER.                   <V74L01>
           MOVE ZERO                   TO TAXD-PLANSFX.                 <V74L01>
           MOVE WSAA-OLD-BTDATE        TO TAXD-EFFDATE                  <V74L01>
                                          TAXD-INSTFROM.                <V74L01>
           MOVE PAYR-BTDATE            TO TAXD-INSTTO.                  <V74L01>
           MOVE WSAA-OLD-BILLCD        TO TAXD-BILLCD.                  <V74L01>
           MOVE SPACES                   TO WSAA-TRANREF.               <V74L01>
           STRING TXCL-TAXRULE, TXCL-RATE-ITEM                          <V74L01>
                  DELIMITED BY SIZE    INTO WSAA-TRANREF.               <V74L01>
           MOVE WSAA-TRANREF           TO TAXD-TRANREF.                 <V74L01>
           MOVE CHDRLIF-TRANNO         TO TAXD-TRANNO.                  <V74L01>
           MOVE TXCL-AMOUNT-IN         TO TAXD-BASEAMT.                 <V74L01>
           MOVE TXCL-TAX-AMT(1)        TO TAXD-TAXAMT01.                <V74L01>
           MOVE TXCL-TAX-AMT(2)        TO TAXD-TAXAMT02.                <V74L01>
           MOVE ZERO                   TO TAXD-TAXAMT03.                <V74L01>
           MOVE TXCL-TAX-ABSORB(1)     TO TAXD-TXABSIND01.              <V74L01>
           MOVE TXCL-TAX-ABSORB(2)     TO TAXD-TXABSIND02.              <V74L01>
           MOVE SPACES                 TO TAXD-TXABSIND03.              <V74L01>
           MOVE TXCL-TAX-TYPE(1)       TO TAXD-TXTYPE01.                <V74L01>
           MOVE TXCL-TAX-TYPE(2)       TO TAXD-TXTYPE02.                <V74L01>
           MOVE SPACES                 TO TAXD-TXTYPE03.                <V74L01>
           MOVE SPACES                 TO TAXD-POSTFLG.                 <V74L01>
                                                                        <V74L01>
           MOVE TAXDREC                TO TAXD-FORMAT.                  <V74L01>
           MOVE WRITR                  TO TAXD-FUNCTION.                <V74L01>
           CALL 'TAXDIO'               USING TAXD-PARAMS.               <V74L01>
                                                                        <V74L01>
           IF TAXD-STATUZ              NOT = O-K                        <V74L01>
              MOVE TAXD-PARAMS         TO SYSR-PARAMS                   <V74L01>
              MOVE TAXD-STATUZ         TO SYSR-STATUZ                   <V74L01>
              PERFORM 600-FATAL-ERROR                                   <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
      *  Read next COVRLNB                                              <V74L01>
                                                                        <V74L01>
           MOVE NEXTR                  TO COVRLNB-FUNCTION.             <V74L01>
                                                                        <V74L01>
       345A-EXIT.                                                       <V74L01>
           EXIT.                                                        <V74L01>
                                                                        <V74L01>
       345B-PROCESS-CTFEE-TAX SECTION.                                  <V74L01>
      ********************************                                  <V74L01>
       345B-START.                                                      <V74L01>
      * Calculate tax on contract fee.                                  <V74L01>
                                                                        <V74L01>
           IF PAYR-SINSTAMT05          NOT = ZERO                       <V74L01>
           AND TR517-ZRWVFLG-03        = 'Y'                            <V74L01>
              GO TO 345B-EXIT                                           <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
      *  Read table TR52E.                                              <V74L01>
           MOVE SPACES                 TO WSAA-TR52E-KEY.               <V74L01>
           MOVE TR52D-TXCODE           TO WSAA-TR52E-TXCODE.            <V74L01>
           MOVE CHDRLIF-CNTTYPE        TO WSAA-TR52E-CNTTYPE.           <V74L01>
           MOVE COVRLNB-CRTABLE        TO WSAA-TR52E-CRTABLE.           <V74L01>
           PERFORM A1100-READ-TR52E.                                    <V74L01>
                                                                        <V74L01>
           IF TR52E-TR52E-REC = SPACES                                  <V74L01>
              MOVE SPACES              TO WSAA-TR52E-KEY                <V74L01>
              MOVE TR52D-TXCODE        TO WSAA-TR52E-TXCODE             <V74L01>
              MOVE CHDRLIF-CNTTYPE     TO WSAA-TR52E-CNTTYPE            <V74L01>
              MOVE '****'              TO WSAA-TR52E-CRTABLE            <V74L01>
              PERFORM A1100-READ-TR52E                                  <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF TR52E-TR52E-REC = SPACES                                  <V74L01>
              MOVE SPACES              TO WSAA-TR52E-KEY                <V74L01>
              MOVE TR52D-TXCODE        TO WSAA-TR52E-TXCODE             <V74L01>
              MOVE '***'               TO WSAA-TR52E-CNTTYPE            <V74L01>
              MOVE '****'              TO WSAA-TR52E-CRTABLE            <V74L01>
              PERFORM A1100-READ-TR52E                                  <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
      *  If TR52E tax indicator2 not 'Y', do not calculate tax          <V74L01>
                                                                        <V74L01>
           IF TR52E-TAXIND-02          NOT = 'Y'                        <V74L01>
              MOVE NEXTR               TO COVRLNB-FUNCTION              <V74L01>
              GO TO 345B-EXIT                                           <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
      *  Set values to tax linkage and call tax subroutine              <V74L01>
                                                                        <V74L01>
           INITIALIZE                     TXCL-LINK-REC.                <V74L01>
           MOVE 'CALC'                 TO TXCL-FUNCTION.                <V74L01>
           MOVE O-K                    TO TXCL-STATUZ.                  <V74L01>
           MOVE 'CNTF'                 TO TXCL-TRANS-TYPE.              <V74L01>
           MOVE CHDRLIF-CHDRCOY        TO TXCL-CHDRCOY.                 <V74L01>
           MOVE CHDRLIF-CHDRNUM        TO TXCL-CHDRNUM.                 <V74L01>
           MOVE WSAA-TR52E-KEY         TO TXCL-TAXRULE.                 <V74L01>
           MOVE SPACES                 TO WSAA-RATE-ITEM.               <V74L01>
           MOVE CHDRLIF-CNTCURR        TO TXCL-CCY                      <V74L01>
                                          WSAA-CNT-CURR.                <V74L01>
           MOVE TR52E-TXITEM           TO WSAA-TXITEM.                  <V74L01>
           MOVE WSAA-RATE-ITEM         TO TXCL-RATE-ITEM.               <V74L01>
           MOVE WSAA-OLD-BTDATE        TO TXCL-EFFDATE.                 <V74L01>
           IF WSAA-GOT-PAYR-AT-BTDATE  =  'Y'                           <V74L01>
              MOVE PAYRLIF-SINSTAMT02  TO TXCL-AMOUNT-IN                <V74L01>
           ELSE                                                         <V74L01>
              MOVE PAYR-SINSTAMT02     TO TXCL-AMOUNT-IN                <V74L01>
           END-IF.                                                      <V74L01>
           MOVE SPACES                 TO TXCL-TAX-TYPE(1)              <V74L01>
                                          TXCL-TAX-TYPE(2)              <V74L01>
                                          TXCL-TAX-ABSORB(1)            <V74L01>
                                          TXCL-TAX-ABSORB(2).           <V74L01>
           MOVE ZEROES                 TO TXCL-TAX-AMT(1)               <V74L01>
                                          TXCL-TAX-AMT(2).              <V74L01>
                                                                        <V74L01>
           CALL TR52D-TXSUBR        USING TXCL-LINK-REC.                <V74L01>
                                                                        <V74L01>
           IF TXCL-STATUZ              NOT = O-K                        <V74L01>
              MOVE TXCL-LINK-REC       TO SYSR-PARAMS                   <V74L01>
              MOVE TXCL-STATUZ         TO SYSR-STATUZ                   <V74L01>
              PERFORM 600-FATAL-ERROR                                   <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF TXCL-TAX-AMT(1)          > ZERO                           <V74L01>
           OR TXCL-TAX-AMT(2)          > ZERO                           <V74L01>
              IF TXCL-TAX-ABSORB(1)    NOT = 'Y'                        <V74L01>
                 ADD TXCL-TAX-AMT(1)   TO WSAA-TAX                      <V74L01>
              END-IF                                                    <V74L01>
              IF TXCL-TAX-ABSORB(2)    NOT = 'Y'                        <V74L01>
                 ADD TXCL-TAX-AMT(2)   TO WSAA-TAX                      <V74L01>
              END-IF                                                    <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
      *  Create TAXD record                                             <V74L01>
                                                                        <V74L01>
           INITIALIZE                     TAXD-DATA-AREA.               <V74L01>
           MOVE CHDRLIF-CHDRCOY        TO TAXD-CHDRCOY.                 <V74L01>
           MOVE CHDRLIF-CHDRNUM        TO TAXD-CHDRNUM.                 <V74L01>
           MOVE 'CNTF'                 TO TAXD-TRANTYPE.                <V74L01>
           MOVE SPACES                 TO TAXD-LIFE                     <V74L01>
                                          TAXD-COVERAGE                 <V74L01>
                                          TAXD-RIDER.                   <V74L01>
           MOVE ZERO                   TO TAXD-PLANSFX.                 <V74L01>
           MOVE WSAA-OLD-BTDATE        TO TAXD-EFFDATE                  <V74L01>
                                          TAXD-INSTFROM.                <V74L01>
           MOVE PAYR-BTDATE            TO TAXD-INSTTO.                  <V74L01>
           MOVE WSAA-OLD-BILLCD        TO TAXD-BILLCD.                  <V74L01>
           MOVE SPACES                   TO WSAA-TRANREF.               <V74L01>
           STRING TXCL-TAXRULE, TXCL-RATE-ITEM                          <V74L01>
                  DELIMITED BY SIZE    INTO WSAA-TRANREF.               <V74L01>
           MOVE WSAA-TRANREF           TO TAXD-TRANREF.                 <V74L01>
           MOVE CHDRLIF-TRANNO         TO TAXD-TRANNO.                  <V74L01>
           MOVE TXCL-AMOUNT-IN         TO TAXD-BASEAMT.                 <V74L01>
           MOVE TXCL-TAX-AMT(1)        TO TAXD-TAXAMT01.                <V74L01>
           MOVE TXCL-TAX-AMT(2)        TO TAXD-TAXAMT02.                <V74L01>
           MOVE ZERO                   TO TAXD-TAXAMT03.                <V74L01>
           MOVE TXCL-TAX-ABSORB(1)     TO TAXD-TXABSIND01.              <V74L01>
           MOVE TXCL-TAX-ABSORB(2)     TO TAXD-TXABSIND02.              <V74L01>
           MOVE SPACES                 TO TAXD-TXABSIND03.              <V74L01>
           MOVE TXCL-TAX-TYPE(1)       TO TAXD-TXTYPE01.                <V74L01>
           MOVE TXCL-TAX-TYPE(2)       TO TAXD-TXTYPE02.                <V74L01>
           MOVE SPACES                 TO TAXD-TXTYPE03.                <V74L01>
           MOVE SPACES                 TO TAXD-POSTFLG.                 <V74L01>
                                                                        <V74L01>
           MOVE TAXDREC                TO TAXD-FORMAT.                  <V74L01>
           MOVE WRITR                  TO TAXD-FUNCTION.                <V74L01>
           CALL 'TAXDIO'               USING TAXD-PARAMS.               <V74L01>
                                                                        <V74L01>
           IF TAXD-STATUZ              NOT = O-K                        <V74L01>
              MOVE TAXD-PARAMS         TO SYSR-PARAMS                   <V74L01>
              MOVE TAXD-STATUZ         TO SYSR-STATUZ                   <V74L01>
              PERFORM 600-FATAL-ERROR                                   <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
       345B-EXIT.                                                       <V74L01>
           EXIT.                                                        <V74L01>
                                                                        <V74L01>
       3500-AUTOMATIC-INCREASE SECTION.
      ********************************
       3510-START.

      * Look up any automatic increases.

           MOVE ZEROES                 TO WSAA-INCREASE-DUE.

           MOVE SPACES                 TO INCRRGP-PARAMS.               <CAS1.0>
           MOVE O-K                    TO INCRRGP-STATUZ.
           MOVE CHDRCOY                TO INCRRGP-CHDRCOY.
           MOVE CHDRNUM                TO INCRRGP-CHDRNUM.
           MOVE ZEROES                 TO INCRRGP-PLAN-SUFFIX.
           MOVE INCRRGPREC             TO INCRRGP-FORMAT.
           MOVE BEGN                   TO INCRRGP-FUNCTION.

           PERFORM 3510-READ-INCRS UNTIL INCRRGP-STATUZ = ENDP.
                                                                        <LA5134>
      *                                                                 <LA5134>
      * Update Increase amount into PAYR OUTSTAMT                       <LA5134>
      *                                                                 <LA5134>
           COMPUTE PAYR-OUTSTAMT        = PAYR-OUTSTAMT                 <LA5134>
                                        + WSAA-INCREASE-DUE.            <LA5134>
           MOVE PAYR-OUTSTAMT          TO CHDRLIF-OUTSTAMT.             <LA5134>

      ****                                                              <A06275>
      **** Log total amount billed.                                     <A06275>
      ****                                                              <A06275>
                                                                        <A06275>
           IF WSAA-GOT-PAYR-AT-BTDATE   = 'Y'                           <A06275>
               COMPUTE CONT-TOTVAL      = PAYRLIF-SINSTAMT06            <A06275>
                                        + WSAA-INCREASE-DUE             <A06275>
           ELSE                                                         <A06275>
               COMPUTE CONT-TOTVAL      = PAYR-SINSTAMT06               <A06275>
                                        + WSAA-INCREASE-DUE             <A06275>
           END-IF.                                                      <A06275>
                                                                        <V74L01>
           ADD WSAA-TAX                TO CONT-TOTVAL.                  <V74L01>
                                                                        <A06275>
           MOVE CT04                   TO CONT-TOTNO.                   <A06275>
           PERFORM 001-CALL-CONTOT.                                     <A06275>
                                                                        <A06275>
       3599-EXIT.
           EXIT.

       3510-READ-INCRS SECTION.
      *************************
       3511-INCRS.

           CALL 'INCRRGPIO'         USING INCRRGP-PARAMS.

           IF  INCRRGP-STATUZ       NOT = O-K
           AND INCRRGP-STATUZ       NOT = ENDP
               MOVE INCRRGP-PARAMS  TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF  INCRRGP-CHDRCOY      NOT = CHDRCOY
           OR  INCRRGP-CHDRNUM      NOT = CHDRNUM
           OR  INCRRGP-STATUZ           = ENDP
               MOVE ENDP            TO INCRRGP-STATUZ
               GO TO 3519-EXIT
           END-IF.

           IF  INCRRGP-CRRCD             < PAYR-BTDATE
               COMPUTE WSAA-INCREASE-DUE = WSAA-INCREASE-DUE +
                                           INCRRGP-NEWINST -
                                           INCRRGP-LAST-INST
           END-IF.

           MOVE NEXTR               TO INCRRGP-FUNCTION.

       3519-EXIT.
           EXIT.

       3600-WRITE-LINS SECTION.
      ************************
       3610-START.

           MOVE SPACES                 TO LINSRNL-INSTJCTL
                                          LINSRNL-DUEFLG.

           IF WSAA-GOT-PAYR-AT-BTDATE            = 'Y'
               MOVE PAYRLIF-SINSTAMT01 TO  LINSRNL-INSTAMT01
               ADD  WSAA-INCREASE-DUE  TO  LINSRNL-INSTAMT01
               MOVE PAYRLIF-SINSTAMT02 TO  LINSRNL-INSTAMT02
               MOVE PAYRLIF-SINSTAMT03 TO  LINSRNL-INSTAMT03
               MOVE PAYRLIF-SINSTAMT04 TO  LINSRNL-INSTAMT04
               MOVE PAYRLIF-SINSTAMT05 TO  LINSRNL-INSTAMT05
               MOVE PAYRLIF-SINSTAMT06 TO  LINSRNL-INSTAMT06
               ADD  WSAA-INCREASE-DUE  TO  LINSRNL-INSTAMT06
           ELSE
               MOVE PAYR-SINSTAMT01    TO  LINSRNL-INSTAMT01
               ADD  WSAA-INCREASE-DUE  TO  LINSRNL-INSTAMT01
               MOVE PAYR-SINSTAMT02    TO  LINSRNL-INSTAMT02
               MOVE PAYR-SINSTAMT03    TO  LINSRNL-INSTAMT03
               MOVE PAYR-SINSTAMT04    TO  LINSRNL-INSTAMT04
               MOVE PAYR-SINSTAMT05    TO  LINSRNL-INSTAMT05
               MOVE PAYR-SINSTAMT06    TO  LINSRNL-INSTAMT06
               ADD  WSAA-INCREASE-DUE  TO  LINSRNL-INSTAMT06
           END-IF.

           MOVE LINSRNL-INSTAMT06      TO WSAA-BILL-AMOUNT.             <V74L01>
           ADD WSAA-TAX                TO LINSRNL-INSTAMT06.            <V74L01>
                                                                        <V74L01>
      * Convert contract amount(total) to billing amount if contract
      * currency is not the same as billing currency.

           IF PAYR-CNTCURR             = PAYR-BILLCURR
           OR LINSRNL-INSTAMT06        = ZERO
               MOVE LINSRNL-INSTAMT06  TO LINSRNL-CBILLAMT
           ELSE
               MOVE LINSRNL-INSTAMT06  TO CLNK-AMOUNT-IN

               PERFORM 5000-CALL-XCVRT

               MOVE CLNK-AMOUNT-OUT    TO LINSRNL-CBILLAMT
           END-IF.
                                                                        <D9604>
      * If this is a flexible premium contract then we do not   <D9604>
      * want to write a LINS record. We have come this far      <D9604>
      * however as we do need to know linsrnl-cbillamt value    <D9604>
      * to update the FPRM file. Also we maintain CT11 which    <D9604>
      * logs the number of flexible premium billings:           <D9604>
                                                                        <D9604>
           IF FLEXIBLE-PREMIUM-CONTRACT                                 <D9604>
              PERFORM 7000-WRITE-FPCO                                   <D9604>
              MOVE CT11                   TO CONT-TOTNO                 <D9604>
              MOVE 1                      TO CONT-TOTVAL                <D9604>
              PERFORM 001-CALL-CONTOT                                   <D9604>
              GO TO 3699-EXIT                                           <D9604>
           END-IF.                                                      <D9604>

           MOVE CHDRNUM                TO LINSRNL-CHDRNUM.
           MOVE CHDRCOY                TO LINSRNL-CHDRCOY.
           MOVE BILLCHNL               TO LINSRNL-BILLCHNL.
           MOVE PAYRSEQNO              TO LINSRNL-PAYRSEQNO.

           MOVE BATD-BRANCH            TO LINSRNL-BRANCH.
           MOVE BPRD-AUTH-CODE         TO LINSRNL-TRANSCODE.

           MOVE PAYR-BILLFREQ          TO LINSRNL-INSTFREQ.
           MOVE PAYR-CNTCURR           TO LINSRNL-CNTCURR.
           MOVE PAYR-BILLCURR          TO LINSRNL-BILLCURR.
           MOVE PAYR-BTDATE            TO LINSRNL-INSTTO.
           MOVE PAYR-MANDREF           TO LINSRNL-MANDREF.

           MOVE WSAA-OLD-BTDATE        TO LINSRNL-INSTFROM.
           MOVE WSAA-OLD-BILLCD        TO LINSRNL-BILLCD.
           MOVE CHDRLIF-ACCTMETH       TO LINSRNL-ACCTMETH.

           MOVE 'O'                    TO LINSRNL-PAYFLAG.
           MOVE '1'                    TO LINSRNL-VALIDFLAG.

      *  Update the tax relief method on the LINS record
      *  if tax relief is applied.

      **** MOVE LINSRNL-INSTAMT06      TO PRAS-GROSSPREM.               <V74L01>
                                                                        <V74L01>
      *  Exclude WSAA-TAX in calculating Tax Relief                     <V74L01>
                                                                        <V74L01>
           IF PAYR-CNTCURR             =  PAYR-BILLCURR                 <V74L01>
              MOVE WSAA-BILL-AMOUNT    TO PRAS-GROSSPREM                <V74L01>
           ELSE                                                         <V74L01>
              MOVE WSAA-BILL-AMOUNT    TO CLNK-AMOUNT-IN                <V74L01>
              PERFORM 5000-CALL-XCVRT                                   <V74L01>
              MOVE CLNK-AMOUNT-OUT     TO PRAS-GROSSPREM                <V74L01>
           END-IF.                                                      <V74L01>
      *                                                                 <UL001>
                                                                        <V74L01>
           PERFORM 6000-CALCULATE-TAX-RELIEF.

           MOVE WRITR                  TO LINSRNL-FUNCTION.
           MOVE LINSRNLREC             TO LINSRNL-FORMAT.

           CALL 'LINSRNLIO' USING LINSRNL-PARAMS.

           IF  LINSRNL-STATUZ NOT = O-K
               MOVE LINSRNL-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

      *  Log number of LINS written

           MOVE CT05                   TO CONT-TOTNO.
           MOVE 1                      TO CONT-TOTVAL.
           PERFORM 001-CALL-CONTOT

      *  Log total of this instalement

      ****                                                              <A06275>
      **** Do not add the increase amount into the Control Total since  <A06275>
      **** LINSRNL-INSTAMT06 already incorporates the increase.         <A06275>
      ****                                                              <A06275>
      **** COMPUTE CONT-TOTVAL =                                        <A06275>
      ****                 WSAA-INCREASE-DUE + LINSRNL-INSTAMT06.       <A06275>
                                                                        <A06275>
           MOVE LINSRNL-INSTAMT06      TO CONT-TOTVAL.                  <A06275>
           MOVE CT06                   TO CONT-TOTNO.
           PERFORM 001-CALL-CONTOT.

       3699-EXIT.
           EXIT.
      *                                                                 <UL001>
       X3600-WRITE-LINS SECTION.                                        <UL001>
      ****************************                                      <UL001>
       X3610-START.                                                     <UL001>

           MOVE SPACES                 TO LINSRNL-INSTJCTL              <UL001>
                                          LINSRNL-DUEFLG.               <UL001>
           IF WSAA-PTDATE              = ZEROES
               MOVE PAYR-PTDATE        TO WSAA-PTDATE
           END-IF.
                                                                        <UL001>
      ****    INITIALIZE                DTC2-DATCON2-REC        <PHFX22><UL001>
      ****    MOVE  01               TO DTC2-FREQ-FACTOR        <PHFX22><UL001>
      ****    MOVE PAYR-BILLFREQ     TO DTC2-FREQUENCY          <PHFX22><UL001>
      ****    MOVE ZERO              TO DTC2-INT-DATE-2         <PHFX22><UL001>
      ****    MOVE WSAA-PTDATE       TO DTC2-INT-DATE-1         <PHFX22><UL001>
      ****                                                      <PHFX22><UL001>
      ****    CALL 'DATCON2'      USING DTC2-DATCON2-REC        <PHFX22><UL001>
      ****    IF DTC2-STATUZ        NOT = O-K                   <PHFX22><UL001>
      ****       MOVE DTC2-DATCON2-REC TO SYSR-PARAMS           <PHFX22><UL001>
      ****       MOVE DTC2-STATUZ      TO SYSR-STATUZ           <PHFX22><UL001>
      ****       PERFORM 600-FATAL-ERROR                        <PHFX22><UL001>
      ****    END-IF                                            <PHFX22><UL001>
      ****    MOVE DTC2-INT-DATE-2     TO PAYR-BTDATE           <PHFX22><UL001>
      ****                                CHDRLIF-BTDATE.               <PHFX22>
           INITIALIZE                     DTC4-DATCON4-REC.             <PHFX22>
           MOVE 1                      TO DTC4-FREQ-FACTOR.             <PHFX22>
           MOVE PAYR-BILLFREQ          TO DTC4-FREQUENCY.               <PHFX22>
           MOVE WSAA-PTDATE            TO DTC4-INT-DATE-1.              <PHFX22>
           MOVE PAYR-DUEDD             TO DTC4-BILLDAY.                 <PHFX22>
           MOVE PAYR-DUEMM             TO DTC4-BILLMONTH.               <PHFX22>
                                                                        <PHFX22>
           CALL 'DATCON4'              USING DTC4-DATCON4-REC.          <PHFX22>
                                                                        <PHFX22>
           IF DTC4-STATUZ              NOT = O-K                        <PHFX22>
               MOVE DTC4-DATCON4-REC   TO SYSR-PARAMS                   <PHFX22>
               MOVE DTC4-STATUZ        TO SYSR-STATUZ                   <PHFX22>
               PERFORM 600-FATAL-ERROR                                  <PHFX22>
           END-IF.                                                      <PHFX22>
                                                                        <PHFX22>
           MOVE DTC4-INT-DATE-2        TO PAYR-BTDATE                   <PHFX22>
                                          CHDRLIF-BTDATE.               <PHFX22>
                                                                        <UL001>
           IF WSAA-GOT-PAYR-AT-BTDATE            = 'Y'                  <UL001>
               MOVE PAYRLIF-SINSTAMT01 TO  LINSRNL-INSTAMT01            <UL001>
               ADD  WSAA-INCREASE-DUE  TO  LINSRNL-INSTAMT01            <UL001>
               MOVE PAYRLIF-SINSTAMT02 TO  LINSRNL-INSTAMT02            <UL001>
               MOVE PAYRLIF-SINSTAMT03 TO  LINSRNL-INSTAMT03            <UL001>
               MOVE PAYRLIF-SINSTAMT04 TO  LINSRNL-INSTAMT04            <UL001>
               MOVE PAYRLIF-SINSTAMT05 TO  LINSRNL-INSTAMT05            <UL001>
               MOVE PAYRLIF-SINSTAMT06 TO  LINSRNL-INSTAMT06            <UL001>
               ADD  WSAA-INCREASE-DUE  TO  LINSRNL-INSTAMT06            <UL001>
               IF WSAA-INSTPREM-TR     > 0                              <PHFX80>
               AND NOT(PAYR-SINSTAMT06     = PAYRLIF-SINSTAMT06)        <PHFX80>
               AND NOT(WSAA-INSTPREM-COVR  = PAYRLIF-SINSTAMT06)        <PHFX80>
                   MOVE WSAA-INSTPREM-COVR TO LINSRNL-INSTAMT06         <PHFX80>
                   MOVE LINSRNL-INSTAMT06  TO LINSRNL-INSTAMT01         <PHFX80>
      **           COMPUTE LINSRNL-INSTAMT06                            <PHFX80>
      **                                   = LINSRNL-INSTAMT06          <PHFX80>
      **                                   - WSAA-INSTPREM-TR           <PHFX80>
               END-IF                                                   <PHFX80>
           ELSE                                                         <UL001>
               MOVE PAYR-SINSTAMT01    TO  LINSRNL-INSTAMT01            <UL001>
               ADD  WSAA-INCREASE-DUE  TO  LINSRNL-INSTAMT01            <UL001>
               MOVE PAYR-SINSTAMT02    TO  LINSRNL-INSTAMT02            <UL001>
               MOVE PAYR-SINSTAMT03    TO  LINSRNL-INSTAMT03            <UL001>
               MOVE PAYR-SINSTAMT04    TO  LINSRNL-INSTAMT04            <UL001>
               MOVE PAYR-SINSTAMT05    TO  LINSRNL-INSTAMT05            <UL001>
               MOVE PAYR-SINSTAMT06    TO  LINSRNL-INSTAMT06            <UL001>
               ADD  WSAA-INCREASE-DUE  TO  LINSRNL-INSTAMT06            <UL001>
           END-IF.                                                      <UL001>
                                                                        <UL001>
           IF  WSAA-TV103-FOUND         = 'Y'                           <UL005>
TVAN       OR  WSAA-TZ028-FOUND         = 'Y'                           <CS020>
               IF  WSAA-GOT-PAYR-AT-BTDATE = 'Y'                        <UL005>
                   MOVE WSAA-SINSTAMT01  TO PAYRLIF-SINSTAMT01          <UL005>
                   MOVE WSAA-SINSTAMT06  TO PAYRLIF-SINSTAMT06          <UL005>
               ELSE                                                     <UL005>
                   MOVE WSAA-SINSTAMT01  TO PAYR-SINSTAMT01             <UL005>
                   MOVE WSAA-SINSTAMT06  TO PAYR-SINSTAMT06             <UL005>
               END-IF                                                   <UL005>
           END-IF.                                                      <UL005>
                                                                        <UL005>
           MOVE LINSRNL-INSTAMT06      TO WSAA-BILL-AMOUNT.             <UL001>
           ADD WSAA-TAX                TO LINSRNL-INSTAMT06.            <UL001>
                                                                        <UL001>
      * Convert contract amount(total) to billing amount if contract    <UL001>
      * currency is not the same as billing currency.                   <UL001>
                                                                        <UL001>
           IF PAYR-CNTCURR             = PAYR-BILLCURR                  <UL001>
           OR LINSRNL-INSTAMT06        = ZERO                           <UL001>
               MOVE LINSRNL-INSTAMT06  TO LINSRNL-CBILLAMT              <UL001>
           ELSE                                                         <UL001>
               MOVE LINSRNL-INSTAMT06  TO CLNK-AMOUNT-IN                <UL001>
                                                                        <UL001>
               PERFORM 5000-CALL-XCVRT                                  <UL001>
                                                                        <UL001>
               MOVE CLNK-AMOUNT-OUT    TO LINSRNL-CBILLAMT              <UL001>
           END-IF.                                                      <UL001>
      * If this is a flexible premium contract then we do not   <D9604> <UL001>
      * want to write a LINS record. We have come this far      <D9604> <UL001>
      * however as we do need to know linsrnl-cbillamt value    <D9604> <UL001>
      * to update the FPRM file. Also we maintain CT11 which    <D9604> <UL001>
      * logs the number of flexible premium billings:           <D9604> <UL001>
      *    IF FLEXIBLE-PREMIUM-CONTRACT                         <UL001> <D9604>
      *       PERFORM 7000-WRITE-FPCO                           <UL001> <D9604>
      *       MOVE CT11                   TO CONT-TOTNO         <UL001> <D9604>
      *       MOVE 1                      TO CONT-TOTVAL        <UL001> <D9604>
      *       PERFORM 001-CALL-CONTOT                           <UL001> <D9604>
      *       GO TO 3699-EXIT                                   <UL001> <D9604>
      *    END-IF.                                              <UL001> <D9604>
                                                                        <UL001>
           MOVE CHDRNUM                TO LINSRNL-CHDRNUM.              <UL001>
           MOVE CHDRCOY                TO LINSRNL-CHDRCOY.              <UL001>
           MOVE BILLCHNL               TO LINSRNL-BILLCHNL.             <UL001>
           MOVE PAYRSEQNO              TO LINSRNL-PAYRSEQNO.            <UL001>
                                                                        <UL001>
           MOVE BATD-BRANCH            TO LINSRNL-BRANCH.               <UL001>
           MOVE BPRD-AUTH-CODE         TO LINSRNL-TRANSCODE.            <UL001>
                                                                        <UL001>
           MOVE PAYR-BILLFREQ          TO LINSRNL-INSTFREQ.             <UL001>
           MOVE PAYR-CNTCURR           TO LINSRNL-CNTCURR.              <UL001>
           MOVE PAYR-BILLCURR          TO LINSRNL-BILLCURR.             <UL001>
           MOVE PAYR-BTDATE            TO LINSRNL-INSTTO.               <UL001>
           MOVE PAYR-MANDREF           TO LINSRNL-MANDREF.              <UL001>

      **   MOVE WSAA-OLD-BTDATE        TO LINSRNL-INSTFROM.
      **   MOVE WSAA-OLD-BILLCD        TO LINSRNL-BILLCD.


           MOVE WSAA-PTDATE            TO LINSRNL-INSTFROM.             <UL001>
      ***  MOVE PAYR-BTDATE            TO LINSRNL-BILLCD,               <UL001>
      ***                                 CHDRLIF-BILLCD.
           MOVE WSAA-PTDATE            TO LINSRNL-BILLCD,               <UL001>
                                          CHDRLIF-BILLCD.
           MOVE CHDRLIF-ACCTMETH       TO LINSRNL-ACCTMETH.             <UL001>
                                                                        <UL001>
           MOVE 'O'                    TO LINSRNL-PAYFLAG.              <UL001>
           MOVE '1'                    TO LINSRNL-VALIDFLAG.            <UL001>
                                                                        <UL001>
           MOVE PAYR-BTDATE            TO PAYR-BILLCD
                                          CHDRLIF-BILLCD.
      *
      *  Update the tax relief method on the LINS record                <UL001>
      *  if tax relief is applied.                                      <UL001>
                                                                        <UL001>
      **** MOVE LINSRNL-INSTAMT06      TO PRAS-GROSSPREM.       <UL001> <V74L01>
      *  Exclude WSAA-TAX in calculating Tax Relief             <UL001> <V74L01>
                                                                        <UL001>
           IF PAYR-CNTCURR             =  PAYR-BILLCURR                 <UL001>
              MOVE WSAA-BILL-AMOUNT    TO PRAS-GROSSPREM                <UL001>
           ELSE                                                         <UL001>
              MOVE WSAA-BILL-AMOUNT    TO CLNK-AMOUNT-IN                <UL001>
              PERFORM 5000-CALL-XCVRT                                   <UL001>
              MOVE CLNK-AMOUNT-OUT     TO PRAS-GROSSPREM                <UL001>
           END-IF.                                                      <UL001>
                                                                        <UL001>
           PERFORM 6000-CALCULATE-TAX-RELIEF.                           <UL001>
                                                                        <UL001>
      * Check duppicate data in LINSPF.                                 <UL001>
                                                                        <UL001>
           PERFORM Y3600-CHECK-DUPP-DATA.                               <UL001>
                                                                        <UL001>
           IF WSAA-SKIP             = 'Y'                               <UL001>
      ****     MOVE DTC2-INT-DATE-2    TO PAYR-PTDATE           <PHFX22><UL001>
      ****     MOVE DTC2-INT-DATE-2    TO WSAA-PTDATE           <PHFX22><UL001>
HA    **       MOVE DTC4-INT-DATE-2    TO PAYR-PTDATE           <UL010> <PHFX22>
               MOVE DTC4-INT-DATE-2    TO WSAA-PTDATE                   <PHFX22>
               GO TO X3690-EXIT                                         <UL001>
           END-IF.                                                      <UL001>
      *                                                                 <UL001>
                                                                        <UL001>
           MOVE WRITR                  TO LINSRNL-FUNCTION.             <UL001>
           MOVE LINSRNLREC             TO LINSRNL-FORMAT.               <UL001>
                                                                        <UL001>
           CALL 'LINSRNLIO' USING LINSRNL-PARAMS.                       <UL001>
                                                                        <UL001>
           IF  LINSRNL-STATUZ NOT = O-K                                 <UL001>
               MOVE LINSRNL-PARAMS     TO SYSR-PARAMS                   <UL001>
               PERFORM 600-FATAL-ERROR                                  <UL001>
           END-IF.                                                      <UL001>
                                                                        <UL001>
      **** MOVE DTC2-INT-DATE-2        TO PAYR-PTDATE.          <PHFX22><UL001>
      **** MOVE DTC2-INT-DATE-2        TO WSAA-PTDATE.          <PHFX22><UL001>
      **   MOVE DTC4-INT-DATE-2        TO PAYR-PTDATE.          <UL001> <PHFX22>
           MOVE DTC4-INT-DATE-2        TO WSAA-PTDATE.                  <PHFX22>
      *  Log number of LINS written                                     <UL001>
                                                                        <UL001>
           MOVE CT05                   TO CONT-TOTNO.                   <UL001>
           MOVE 1                      TO CONT-TOTVAL.                  <UL001>
           PERFORM 001-CALL-CONTOT                                      <UL001>
                                                                        <UL001>
      *  Log total of this instalement                                  <UL001>
                                                                        <UL001>
      ****                                                      <UL001> <UL001>
      **** Do not add the increase amount into the Control Total since  <UL001>
      **** LINSRNL-INSTAMT06 already incorporates the increase. <UL001> <UL001>
      ****                                                      <UL001> <UL001>
      **** COMPUTE CONT-TOTVAL =                                <UL001> <UL001>
      ****                 WSAA-INCREASE-DUE + LINSRNL-INSTAMT06.       <UL001>
                                                                        <UL001>
           MOVE LINSRNL-INSTAMT06      TO CONT-TOTVAL.                  <UL001>
           MOVE CT06                   TO CONT-TOTNO.                   <UL001>
           PERFORM 001-CALL-CONTOT.                                     <UL001>
                                                                        <UL001>
                                                                        <UL001>
       X3690-EXIT.                                                      <UL001>
           EXIT.                                                        <UL001>
      *                                                                 <UL001>
      *                                                                 <UL001>
       Y3600-CHECK-DUPP-DATA SECTION.                                   <UL001>
      ********************************                                  <UL001>
       Y3610-CHECK.                                                     <UL001>
                                                                        <UL001>
      **** MOVE SPACES                 TO LINSFPR-PARAMS.       <PHFX30><UL001>
      **** MOVE CHDRCOY                TO LINSFPR-CHDRCOY.      <PHFX30><UL001>
      **** MOVE CHDRNUM                TO LINSFPR-CHDRNUM.      <PHFX30><UL001>
      **** MOVE WSAA-PTDATE            TO LINSFPR-BILLCD.       <PHFX30><UL001>
      **** MOVE READR                  TO LINSFPR-FUNCTION.     <PHFX30><UL001>
      **** MOVE LINSFPRREC             TO LINSFPR-FORMAT.       <PHFX30><UL001>
      ****                                                      <PHFX30><UL001>
      **** CALL 'LINSFPRIO' USING LINSFPR-PARAMS.               <PHFX30><UL001>
                                                                        <UL001>
      **** IF  LINSFPR-STATUZ          NOT = O-K                <PHFX30><UL001>
      **** AND                         NOT = MRNF               <PHFX30><UL001>
      ****     MOVE LINSFPR-PARAMS     TO SYSR-PARAMS           <PHFX30><UL001>
      ****     PERFORM 600-FATAL-ERROR                          <PHFX30><UL001>
      **** END-IF.                                              <PHFX30><UL001>
                                                                        <UL001>
      **** IF LINSFPR-STATUZ           = O-K                    <PHFX30><UL001>
      ****     MOVE 'Y'                TO WSAA-SKIP             <PHFX30><UL001>
      **** ELSE                                                 <PHFX30><UL001>
      ****     MOVE 'N'                TO WSAA-SKIP             <PHFX30><UL001>
      **** END-IF.                                              <PHFX30><UL001>
           MOVE SPACES                 TO LINSBLC-PARAMS.               <PHFX30>
           MOVE CHDRCOY                TO LINSBLC-CHDRCOY.              <PHFX30>
           MOVE CHDRNUM                TO LINSBLC-CHDRNUM.              <PHFX30>
           MOVE WSAA-PTDATE            TO LINSBLC-BILLCD.               <PHFX30>
           MOVE READR                  TO LINSBLC-FUNCTION.             <PHFX30>
           MOVE LINSBLCREC             TO LINSBLC-FORMAT.               <PHFX30>
                                                                        <PHFX30>
           CALL 'LINSBLCIO' USING LINSBLC-PARAMS.                       <PHFX30>
                                                                        <PHFX30>
           IF  LINSBLC-STATUZ          NOT = O-K                        <PHFX30>
           AND                         NOT = MRNF                       <PHFX30>
               MOVE LINSBLC-PARAMS     TO SYSR-PARAMS                   <PHFX30>
               PERFORM 600-FATAL-ERROR                                  <PHFX30>
           END-IF.                                                      <PHFX30>
                                                                        <PHFX30>
           IF LINSBLC-STATUZ           = O-K                            <PHFX30>
               MOVE 'Y'                TO WSAA-SKIP                     <PHFX30>
               MOVE LINSRNL-INSTAMT01  TO LINSBLC-INSTAMT01             <PHFX30>
               MOVE LINSRNL-INSTAMT02  TO LINSBLC-INSTAMT02             <PHFX30>
               MOVE LINSRNL-INSTAMT03  TO LINSBLC-INSTAMT03             <PHFX30>
               MOVE LINSRNL-INSTAMT04  TO LINSBLC-INSTAMT04             <PHFX30>
               MOVE LINSRNL-INSTAMT05  TO LINSBLC-INSTAMT05             <PHFX30>
               MOVE LINSRNL-INSTAMT06  TO LINSBLC-INSTAMT06             <PHFX30>
               MOVE LINSRNL-CBILLAMT   TO LINSBLC-CBILLAMT              <PHFX30>
               MOVE WRITD              TO LINSBLC-FUNCTION              <PHFX30>
               CALL 'LINSBLCIO'     USING LINSBLC-PARAMS                <PHFX30>
                                                                        <PHFX30>
               IF  LINSBLC-STATUZ   NOT = O-K                           <PHFX30>
                   MOVE LINSBLC-PARAMS TO SYSR-PARAMS                   <PHFX30>
                   PERFORM 600-FATAL-ERROR                              <PHFX30>
               END-IF                                                   <PHFX30>
           ELSE                                                         <PHFX30>
               MOVE 'N'                TO WSAA-SKIP                     <PHFX30>
           END-IF.                                                      <PHFX30>
                                                                        <UL001>
       Y3690-EXIT.                                                      <UL001>
           EXIT.                                                        <UL001>
      *                                                                 <UL001>
                                                                        <UL001>

       3700-PRODUCE-BEXT-RECORD SECTION.
      **********************************
       3710-START.

           MOVE ZERO                   TO WSAA-BILL-OUTST.              <V74L01>
                                                                        <V74L01>
      * Read T3620 from the array to determine whether DD details reqd

           SEARCH ALL WSAA-T3620-REC
              AT END
              MOVE MRNF                TO SYSR-STATUZ
                   STRING T3620
                       PAYR-BILLCHNL
                                       DELIMITED BY SIZE
                                       INTO WSYS-SYSPARAMS
                       MOVE WSYS-SYSTEM-ERROR-PARAMS TO SYSR-PARAMS

              PERFORM 600-FATAL-ERROR

           WHEN WSAA-T3620-KEY (WSAA-T3620-IX) = PAYR-BILLCHNL
              CONTINUE
           END-SEARCH.

      * Convert contract amount to billing amount if contract currency
      * is not the same as billing currency.

           IF  PAYR-CNTCURR            = PAYR-BILLCURR
               COMPUTE PRAS-GROSSPREM  =
               PAYR-OUTSTAMT + WSAA-INCREASE-DUE
               PERFORM 6000-CALCULATE-TAX-RELIEF
               COMPUTE CLNK-AMOUNT-OUT = PAYR-OUTSTAMT -
                                         PRAS-TAXRELAMT +
                                         WSAA-INCREASE-DUE
               COMPUTE WSAA-BILL-OUTST ROUNDED                          <V74L01>
                                       = CLNK-AMOUNT-OUT +              <V74L01>
                                         WSAA-TAX                       <V74L01>
           ELSE
               IF PAYR-OUTSTAMT    NOT = ZERO

                 COMPUTE CLNK-AMOUNT-IN    =
                                    (PAYR-OUTSTAMT + WSAA-INCREASE-DUE)

                 PERFORM 5000-CALL-XCVRT

                 MOVE CLNK-AMOUNT-OUT      TO PRAS-GROSSPREM
                 PERFORM 6000-CALCULATE-TAX-RELIEF
                 SUBTRACT PRAS-TAXRELAMT   FROM CLNK-AMOUNT-OUT
                 MOVE CLNK-AMOUNT-OUT      TO WSAA-BILL-OUTST           <V74L01>
                 MOVE WSAA-TAX             TO CLNK-AMOUNT-IN            <V74L01>
                 PERFORM 5000-CALL-XCVRT                                <V74L01>
                 ADD CLNK-AMOUNT-OUT       TO WSAA-BILL-OUTST           <V74L01>
               END-IF
           END-IF.

      * Do not produce bext if there is enough money in the suspense
      * account to cover the instalment amount.
      * Additional checking for dividend suspense               <LA2106><LA2106>

      **** IF WSAA-SUSP-AVAIL          >=  CLNK-AMOUNT-OUT              <V74L01>
           IF WSAA-SUSP-AVAIL          >=  WSAA-BILL-OUTST              <V74L01>
              IF  WSAA-DVD-SUSP        = 0                              <LA2106>
                  GO TO                3799-EXIT                        <LA2106>
              END-IF                                                    <LA2106>
      *                                                         <LA2106><LA2106>
      * Transfer LP DS to LP S                                  <LA2106><LA2106>
                                                                        <LA2106>
              PERFORM H100-TFR-DVD-SUSP                                 <LA2106>
              GO TO                    3799-EXIT
           END-IF.

           IF WSAA-T3620-DDIND (WSAA-T3620-IX) NOT = SPACES
           OR WSAA-T3620-CRCIND(WSAA-T3620-IX) NOT = SPACES             <V76F13>
              PERFORM 3720-GET-PAYR-BANK-ACCOUNT
           ELSE                                                         <A06388>
              MOVE SPACES           TO MAND-BANKKEY                     <A06388>
              MOVE SPACES           TO MAND-BANKACCKEY                  <A06388>
              MOVE SPACES           TO MAND-MANDSTAT                    <A06388>
              MOVE SPACES           TO CLBADDB-FACTHOUS                 <A06388>
           END-IF.

           PERFORM 3730-READ-DISHONOURS.
           PERFORM 3740-CALL-BILLREQ1.

       3799-EXIT.
           EXIT.

       3720-GET-PAYR-BANK-ACCOUNT SECTION.
      ************************************
       3721-START.

      *   Check that a Mandate does not exist

           MOVE CLRF-CLNTCOY        TO MAND-PAYRCOY.
           MOVE CLRF-CLNTNUM        TO MAND-PAYRNUM.
           MOVE PAYR-MANDREF        TO MAND-MANDREF.
           MOVE READR               TO MAND-FUNCTION.

           CALL 'MANDIO'            USING MAND-PARAMS.

           IF MAND-STATUZ       NOT = O-K
              MOVE MAND-PARAMS      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

      *   Move the Bank Details for Direct Debit for later
      *   use on the BEXT file

           MOVE 'CN'                TO CLBADDB-CLNTPFX.
           MOVE CLRF-CLNTCOY        TO CLBADDB-CLNTCOY.
           MOVE CLRF-CLNTNUM        TO CLBADDB-CLNTNUM.
           MOVE MAND-BANKKEY        TO CLBADDB-BANKKEY.
           MOVE MAND-BANKACCKEY     TO CLBADDB-BANKACCKEY.
           MOVE CLBADDBREC          TO CLBADDB-FORMAT.
           MOVE READR               TO CLBADDB-FUNCTION.

           CALL 'CLBADDBIO'         USING CLBADDB-PARAMS.

           IF CLBADDB-STATUZ        NOT = O-K
              MOVE CLBADDB-PARAMS   TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

       3729-EXIT.
           EXIT.

       3730-READ-DISHONOURS SECTION.
      *****************************
       3731-START.

      *  Check if a previously dishonoured payment exists by reading
      *  DDSURNL with a BEGN. If there is a key break move the
      *  original MANDSTAT to BEXT-MANDSTAT, move the later of the
      *  BSSC-EFFECTIVE-DATE or BILLCD to BILLDATE, else move DDSURNL-
      *  MANDSTAT to BEXT-MANDSTAT and increment the BSSC-EFFECTIVE-
      *  DATE by DDSURNL-LAPDAY to give BEXT-BILLDATE.

           MOVE CLRF-CLNTCOY           TO DDSURNL-PAYRCOY.
           MOVE CLRF-CLNTNUM           TO DDSURNL-PAYRNUM.
           MOVE PAYR-MANDREF           TO DDSURNL-MANDREF.
           MOVE WSAA-OLD-BILLCD        TO DDSURNL-BILLCD.
           MOVE '99'                   TO DDSURNL-MANDSTAT.             <V4LAQR>
           MOVE DDSURNLREC             TO DDSURNL-FORMAT.
           MOVE BEGN                   TO DDSURNL-FUNCTION.

           CALL 'DDSURNLIO'            USING DDSURNL-PARAMS.

           IF  (DDSURNL-STATUZ     NOT = O-K)
           AND (DDSURNL-STATUZ     NOT = ENDP)
              MOVE DDSURNL-PARAMS      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

           IF (DDSURNL-STATUZ           = ENDP)
           OR (DDSURNL-PAYRCOY      NOT = CLRF-CLNTCOY)
           OR (DDSURNL-PAYRNUM      NOT = CLRF-CLNTNUM)
           OR (DDSURNL-MANDREF      NOT = PAYR-MANDREF)
           OR (DDSURNL-BILLCD       NOT = WSAA-OLD-BILLCD)
              MOVE MAND-MANDSTAT       TO BLRQ-MANDSTAT

              IF BSSC-EFFECTIVE-DATE           >  WSAA-OLD-BILLCD
                  MOVE BSSC-EFFECTIVE-DATE     TO BLRQ-BILLDATE
              ELSE
                  MOVE WSAA-OLD-BILLCD     TO BLRQ-BILLDATE
              END-IF

           ELSE

              MOVE DDSURNL-MANDSTAT    TO BLRQ-MANDSTAT

              MOVE DDSURNL-LAPDAY      TO DTC2-FREQ-FACTOR
              MOVE 'DY'                TO DTC2-FREQUENCY
              MOVE BSSC-EFFECTIVE-DATE        TO DTC2-INT-DATE-1

              CALL 'DATCON2'           USING DTC2-DATCON2-REC

              IF DTC2-STATUZ       NOT = O-K
                 MOVE DTC2-DATCON2-REC TO SYSR-PARAMS
                 MOVE DTC2-STATUZ      TO SYSR-STATUZ
                 PERFORM 600-FATAL-ERROR
              END-IF

              MOVE DTC2-INT-DATE-2  TO BLRQ-BILLDATE

           END-IF.

       3739-EXIT.
           EXIT.

       3740-CALL-BILLREQ1 SECTION.
      ****************************
       3741-START.

      * Search the T3629 array to obtain the required bank code.

           SEARCH ALL WSAA-T3629-REC
              AT END
              MOVE MRNF                TO SYSR-STATUZ
                   STRING T3629
                       PAYR-BILLCURR
                                       DELIMITED BY SIZE
                                       INTO WSYS-SYSPARAMS
              MOVE WSYS-SYSTEM-ERROR-PARAMS TO SYSR-PARAMS

              PERFORM 600-FATAL-ERROR

           WHEN WSAA-T3629-KEY (WSAA-T3629-IX) = PAYR-BILLCURR
              CONTINUE
           END-SEARCH.

           MOVE WSAA-T3629-BANKCODE (WSAA-T3629-IX)
                                       TO BLRQ-BANKCODE.
           MOVE 0                      TO BLRQ-USER
                                          BLRQ-CONTOT-01
                                          BLRQ-CONTOT-02.
           MOVE SPACES                 TO BLRQ-INSTJCTL
                                          BLRQ-PAYFLAG
                                          BLRQ-BILFLAG
                                          BLRQ-OUTFLAG.
           MOVE 'N'                    TO BLRQ-SUPFLAG.

           MOVE BSPR-COMPANY           TO BLRQ-COMPANY.
           MOVE BATD-BRANCH            TO BLRQ-BRANCH.
           MOVE BSSC-LANGUAGE          TO BLRQ-LANGUAGE.
           MOVE BSSC-EFFECTIVE-DATE    TO BLRQ-EFFDATE.
           MOVE BATD-ACTYEAR           TO BLRQ-ACCTYEAR.
           MOVE BATD-ACTMONTH          TO BLRQ-ACCTMONTH.
           MOVE BATD-TRCDE             TO BLRQ-TRANCODE.
           MOVE BATD-BATCH             TO BLRQ-BATCH.
           MOVE BSPR-FSUCO             TO BLRQ-FSUCO.                   <FA2984>
           MOVE 'BATCH'                TO BLRQ-MODE-IND.

           MOVE SPACES                 TO BLRQ-TERMID.
           MOVE ZEROS                  TO BLRQ-TIME.
           MOVE ZEROS                  TO BLRQ-DATE.

           MOVE CHDRLIF-TRANNO         TO BLRQ-TRANNO.
           MOVE CHDRLIF-CHDRPFX        TO BLRQ-CHDRPFX.
           MOVE CHDRLIF-CHDRCOY        TO BLRQ-CHDRCOY.
           MOVE CHDRLIF-CHDRNUM        TO BLRQ-CHDRNUM.
           MOVE CHDRLIF-SERVUNIT       TO BLRQ-SERVUNIT.
           MOVE CHDRLIF-CNTTYPE        TO BLRQ-CNTTYPE.
           MOVE CHDRLIF-OCCDATE        TO BLRQ-OCCDATE.
           MOVE CHDRLIF-CCDATE         TO BLRQ-CCDATE.
           MOVE CHDRLIF-COLLCHNL       TO BLRQ-INSTCCHNL.
           MOVE CHDRLIF-COWNPFX        TO BLRQ-COWNPFX.
           MOVE CHDRLIF-COWNCOY        TO BLRQ-COWNCOY.
           MOVE CHDRLIF-COWNNUM        TO BLRQ-COWNNUM.
           MOVE CHDRLIF-CNTBRANCH      TO BLRQ-CNTBRANCH.
           MOVE CHDRLIF-AGNTPFX        TO BLRQ-AGNTPFX.
           MOVE CHDRLIF-AGNTCOY        TO BLRQ-AGNTCOY.
           MOVE CHDRLIF-AGNTNUM        TO BLRQ-AGNTNUM.

           MOVE PAYR-CNTCURR           TO BLRQ-CNTCURR.
           MOVE PAYR-BILLCURR          TO BLRQ-BILLCURR.
           MOVE PAYR-PTDATE            TO BLRQ-PTDATE.
           MOVE PAYR-BTDATE            TO BLRQ-INSTTO.
           MOVE PAYR-BILLCHNL          TO BLRQ-INSTBCHNL
                                          BLRQ-BILLCHNL.
           MOVE PAYR-BILLFREQ          TO BLRQ-INSTFREQ.
           MOVE PAYR-GRUPCOY           TO BLRQ-GRPSCOY.
           MOVE PAYR-GRUPNUM           TO BLRQ-GRPSNUM.
           MOVE PAYR-MEMBSEL           TO BLRQ-MEMBSEL.
           MOVE PAYR-MANDREF           TO BLRQ-MANDREF.
           MOVE PAYR-NEXTDATE          TO BLRQ-NEXTDATE.                <FA5078>

           MOVE CLRF-CLNTPFX           TO BLRQ-PAYRPFX.
           MOVE CLRF-CLNTCOY           TO BLRQ-PAYRCOY.
           MOVE CLRF-CLNTNUM           TO BLRQ-PAYRNUM.

           MOVE CLBADDB-FACTHOUS       TO BLRQ-FACTHOUS.
           MOVE MAND-BANKKEY           TO BLRQ-BANKKEY.
           MOVE MAND-BANKACCKEY        TO BLRQ-BANKACCKEY.

           MOVE WSAA-OLD-BTDATE        TO BLRQ-INSTFROM
                                          BLRQ-BTDATE
                                          BLRQ-DUEDATE.
           MOVE WSAA-OLD-BILLCD        TO BLRQ-BILLCD.
           MOVE WSAA-OLD-NEXTDATE      TO BLRQ-NEXTDATE.                <FA5078>

           MOVE T5645-SACSCODE-01      TO BLRQ-SACSCODE-01.
           MOVE T5645-SACSTYPE-01      TO BLRQ-SACSTYPE-01.
           MOVE T5645-GLMAP-01         TO BLRQ-GLMAP-01.
           MOVE T5645-SIGN-01          TO BLRQ-GLSIGN-01.
           MOVE T5645-SACSCODE-02      TO BLRQ-SACSCODE-02.
           MOVE T5645-SACSTYPE-02      TO BLRQ-SACSTYPE-02.
           MOVE T5645-GLMAP-02         TO BLRQ-GLMAP-02.
           MOVE T5645-SIGN-02          TO BLRQ-GLSIGN-02.

      * If BILLREQ1 needs to know the PAYER NAME then it will be looked
      * up using the PAYRPFX

           MOVE SPACES                 TO BLRQ-PAYERNAME.

           IF PAYR-CNTCURR         NOT = PAYR-BILLCURR
              PERFORM 3742-CURR-NOT-EQUAL-BILLCURR
           ELSE
              IF WSAA-GOT-PAYR-AT-BTDATE           = 'Y'
                 PERFORM 3744-GOT-PAYR-AT-BTDATE
              ELSE
                  PERFORM 3746-PAYR
              END-IF
           END-IF.


           CALL 'BILLREQ1'             USING BLRQ-BILLREQ-REC.

           IF BLRQ-STATUZ NOT = O-K
              MOVE BLRQ-BILLREQ-REC    TO SYSR-PARAMS
              MOVE BLRQ-STATUZ         TO SYSR-STATUZ                   <CAS1.0>
              PERFORM 600-FATAL-ERROR
           END-IF.

      *  Log number of BEXT recs created.

           MOVE CT09                   TO CONT-TOTNO.
           MOVE BLRQ-CONTOT-01         TO CONT-TOTVAL.
           PERFORM 001-CALL-CONTOT.

      *  Log total amount for this BEXT rec.

           MOVE CT10                   TO CONT-TOTNO.
           MOVE BLRQ-CONTOT-02         TO CONT-TOTVAL.
           PERFORM 001-CALL-CONTOT.

       3749-EXIT.
           EXIT.

       3742-CURR-NOT-EQUAL-BILLCURR SECTION.
      **************************************
       3742-START.

           IF WSAA-GOT-PAYR-AT-BTDATE        = 'Y'
              PERFORM 3736-CONVERT-OLD-INSTAMTS
              VARYING WSAA-INST-SUB FROM 1 BY 1
                UNTIL   WSAA-INST-SUB > 6
           ELSE
              PERFORM 3737-CONVERT-INSTAMTS
                VARYING WSAA-INST-SUB FROM 1 BY 1
              UNTIL   WSAA-INST-SUB > 6
           END-IF.

           MOVE BLRQ-INSTAMT06         TO  PRAS-GROSSPREM.
      **** ADD  WSAA-INCREASE-DUE      TO  PRAS-GROSSPREM.              <V74L01>
           MOVE WSAA-TAX               TO CLNK-AMOUNT-IN.               <V74L01>
           PERFORM 5000-CALL-XCVRT.                                     <V74L01>
           SUBTRACT CLNK-AMOUNT-OUT    FROM PRAS-GROSSPREM.             <V74L01>

           PERFORM 6000-CALCULATE-TAX-RELIEF.

           COMPUTE BLRQ-INSTAMT06  =   BLRQ-INSTAMT06 -
                                       PRAS-TAXRELAMT.                  <V74L01>
      ****                             PRAS-TAXRELAMT +                 <V74L01>
      ****                             WSAA-INCREASE-DUE.               <V74L01>
           MOVE ZEROS                  TO  BLRQ-INSTAMT07.              <D604IB>
           MOVE ZEROS                  TO  BLRQ-INSTAMT08.              <D604IB>
           MOVE ZEROS                  TO  BLRQ-INSTAMT09.              <D604IB>
           MOVE ZEROS                  TO  BLRQ-INSTAMT10.              <D604IB>
           MOVE ZEROS                  TO  BLRQ-INSTAMT11.              <FPS30>
           MOVE ZEROS                  TO  BLRQ-INSTAMT12.              <FPS30>
           MOVE ZEROS                  TO  BLRQ-INSTAMT13.              <FPS30>
           MOVE ZEROS                  TO  BLRQ-INSTAMT14.              <FPS30>
           MOVE ZEROS                  TO  BLRQ-INSTAMT15.              <FPS30>

       3742-EXIT.
           EXIT.

       3744-GOT-PAYR-AT-BTDATE SECTION.
      ********************************
       3744-START.

           MOVE PAYRLIF-SINSTAMT01     TO  BLRQ-INSTAMT01.
           ADD  WSAA-INCREASE-DUE      TO  BLRQ-INSTAMT01.
           MOVE PAYRLIF-SINSTAMT02     TO  BLRQ-INSTAMT02.
           MOVE PAYRLIF-SINSTAMT03     TO  BLRQ-INSTAMT03.
           MOVE PAYRLIF-SINSTAMT04     TO  BLRQ-INSTAMT04.
           MOVE PAYRLIF-SINSTAMT05     TO  BLRQ-INSTAMT05.
           COMPUTE PRAS-GROSSPREM =
                          (PAYRLIF-SINSTAMT06 + WSAA-INCREASE-DUE).
           PERFORM 6000-CALCULATE-TAX-RELIEF.
           COMPUTE BLRQ-INSTAMT06 =    PAYRLIF-SINSTAMT06 -
                                       PRAS-TAXRELAMT +
                                       WSAA-INCREASE-DUE.
           ADD WSAA-TAX                TO  BLRQ-INSTAMT06.              <V74L01>
           MOVE ZEROS                  TO  BLRQ-INSTAMT07.              <D604IB>
           MOVE ZEROS                  TO  BLRQ-INSTAMT08.              <D604IB>
           MOVE ZEROS                  TO  BLRQ-INSTAMT09.              <D604IB>
           MOVE ZEROS                  TO  BLRQ-INSTAMT10.              <D604IB>
           MOVE ZEROS                  TO  BLRQ-INSTAMT11.              <FPS30>
           MOVE ZEROS                  TO  BLRQ-INSTAMT12.              <FPS30>
           MOVE ZEROS                  TO  BLRQ-INSTAMT13.              <FPS30>
           MOVE ZEROS                  TO  BLRQ-INSTAMT14.              <FPS30>
           MOVE ZEROS                  TO  BLRQ-INSTAMT15.              <FPS30>
                                                                        <D604IB>
       3744-EXIT.
           EXIT.

       3746-PAYR SECTION.
      *******************
       3746-START.

           MOVE PAYR-SINSTAMT01        TO BLRQ-INSTAMT01.
           ADD  WSAA-INCREASE-DUE      TO BLRQ-INSTAMT01.
           MOVE PAYR-SINSTAMT02        TO BLRQ-INSTAMT02.
           MOVE PAYR-SINSTAMT03        TO BLRQ-INSTAMT03.
           MOVE PAYR-SINSTAMT04        TO BLRQ-INSTAMT04.
           MOVE PAYR-SINSTAMT05        TO BLRQ-INSTAMT05.
           COMPUTE PRAS-GROSSPREM =
           (PAYR-SINSTAMT06 + WSAA-INCREASE-DUE).
           PERFORM 6000-CALCULATE-TAX-RELIEF.
           COMPUTE BLRQ-INSTAMT06 =    PAYR-SINSTAMT06 -
                                       PRAS-TAXRELAMT +
                                       WSAA-INCREASE-DUE.
           ADD WSAA-TAX                TO  BLRQ-INSTAMT06.              <V74L01>
           MOVE ZEROS                  TO  BLRQ-INSTAMT07.              <D604IB>
           MOVE ZEROS                  TO  BLRQ-INSTAMT08.              <D604IB>
           MOVE ZEROS                  TO  BLRQ-INSTAMT09.              <D604IB>
           MOVE ZEROS                  TO  BLRQ-INSTAMT10.              <D604IB>
           MOVE ZEROS                  TO  BLRQ-INSTAMT11.              <FPS30>
           MOVE ZEROS                  TO  BLRQ-INSTAMT12.              <FPS30>
           MOVE ZEROS                  TO  BLRQ-INSTAMT13.              <FPS30>
           MOVE ZEROS                  TO  BLRQ-INSTAMT14.              <FPS30>
           MOVE ZEROS                  TO  BLRQ-INSTAMT15.              <FPS30>

       3746-EXIT.
           EXIT.

       3736-CONVERT-OLD-INSTAMTS SECTION.
      ***********************************
       3736-START.

           IF PAYRLIF-SINSTAMT (WSAA-INST-SUB) = ZERO
              MOVE ZERO                TO BLRQ-INSTAMT (WSAA-INST-SUB)
           END-IF.

           MOVE PAYRLIF-SINSTAMT(WSAA-INST-SUB)
                                       TO CLNK-AMOUNT-IN.

      * The increase is only added the first time.

           IF WSAA-INST-SUB        = 1
              ADD WSAA-INCREASE-DUE    TO CLNK-AMOUNT-IN
           END-IF.

           IF WSAA-INST-SUB        = 6                                  <V74L01>
              ADD WSAA-TAX                                              <V74L01>
                  WSAA-INCREASE-DUE    TO CLNK-AMOUNT-IN                <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           PERFORM 5000-CALL-XCVRT.

           MOVE CLNK-AMOUNT-OUT        TO BLRQ-INSTAMT(WSAA-INST-SUB).

       3736-EXIT.
            EXIT.

       3737-CONVERT-INSTAMTS SECTION.
      *******************************
       3737-START.

           IF   PAYR-SINSTAMT (WSAA-INST-SUB) = ZERO
              MOVE ZERO                TO BLRQ-INSTAMT (WSAA-INST-SUB)
           END-IF.

           MOVE PAYR-SINSTAMT(WSAA-INST-SUB)
                                       TO CLNK-AMOUNT-IN.

      * The increase is only added the first time.

           IF WSAA-INST-SUB            = 1
              ADD WSAA-INCREASE-DUE    TO CLNK-AMOUNT-IN
           END-IF.

           IF WSAA-INST-SUB        = 6                                  <V74L01>
              ADD WSAA-TAX                                              <V74L01>
                  WSAA-INCREASE-DUE    TO CLNK-AMOUNT-IN                <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           PERFORM 5000-CALL-XCVRT.

           MOVE CLNK-AMOUNT-OUT        TO BLRQ-INSTAMT(WSAA-INST-SUB).

       3737-EXIT.
           EXIT.

       3800-WRITE-PTRN SECTION.
      *************************
       3810-WRITE-PTRN.

           INITIALIZE                  PTRN-PARAMS.                     <V71L01>
           MOVE VRCM-TERMID            TO PTRN-TERMID.

      **** MOVE BSSC-EFFECTIVE-DATE    TO PTRN-TRANSACTION-DATE.        <CAS1.0>
           MOVE VRCM-DATE              TO PTRN-TRANSACTION-DATE.        <CAS1.0>
           MOVE VRCM-TIME              TO PTRN-TRANSACTION-TIME.
           MOVE ZEROS                  TO PTRN-USER.
           MOVE BATD-BATCHKEY          TO PTRN-DATA-KEY.
           MOVE CHDRLIF-TRANNO         TO PTRN-TRANNO.
           IF FLEXIBLE-PREMIUM-CONTRACT                                 <D9604>
               MOVE WSAA-OLD-BTDATE    TO PTRN-PTRNEFF                  <D9604>
           ELSE                                                         <D9604>
               MOVE LINSRNL-INSTFROM   TO PTRN-PTRNEFF.                 <D9604>
           MOVE CHDRLIF-CHDRCOY        TO PTRN-CHDRCOY.
           MOVE CHDRLIF-CHDRNUM        TO PTRN-CHDRNUM.
           MOVE BSSC-EFFECTIVE-DATE    TO PTRN-DATESUB.                 <D9703>
           MOVE PTRNREC                TO PTRN-FORMAT.
           MOVE WRITR                  TO PTRN-FUNCTION.

           CALL 'PTRNIO'               USING PTRN-PARAMS.

           IF PTRN-STATUZ              NOT = O-K
              MOVE PTRN-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

      * Log the policy transaction records produced

           MOVE CT03           TO CONT-TOTNO.
           MOVE 1              TO CONT-TOTVAL.
           PERFORM 001-CALL-CONTOT.


       3820-EXIT.
           EXIT.

       3900-REWRITE-CHDR SECTION.
      ***************************
       3910-START.

      * Update the CHDR bill supression details

      **** MOVE 0                      TO CHDRLIF-BILLSPFROM            <002>
      ****                                CHDRLIF-BILLSPTO.             <002>
      **** MOVE 'N'                    TO CHDRLIF-BILLSUPR.             <002>
      *                                                         <D9604>
      * Restore outstanding amount if this is a flexible premium<D9604>
           IF FLEXIBLE-PREMIUM-CONTRACT                                 <D9604>
              MOVE WSAA-OLD-OUTSTAMT   TO CHDRLIF-OUTSTAMT              <D9604>
           END-IF.                                                      <D9604>

      * Rewrite contract header

           MOVE REWRT                  TO CHDRLIF-FUNCTION.
           MOVE CHDRLIFREC             TO CHDRLIF-FORMAT.

           CALL 'CHDRLIFIO'            USING CHDRLIF-PARAMS.
           IF CHDRLIF-STATUZ       NOT = O-K
               MOVE CHDRLIF-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

       3919-EXIT.
           EXIT.

       3950-REWRITE-PAYR SECTION.
      ***************************
       3950-START.
      * Update the PAYR bill supression details

      *    MOVE 0                      TO PAYR-BILLSPFROM               <002>
      *                                   PAYR-BILLSPTO.                <002>
      *    MOVE 'N'                    TO PAYR-BILLSUPR.                <002>
      *                                                         <D9604>
      * Restore original outstanding amount                     <D9604>
           IF FLEXIBLE-PREMIUM-CONTRACT                                 <D9604>
               MOVE WSAA-OLD-OUTSTAMT   TO PAYR-OUTSTAMT.               <D9604>

      * Rewrite the PAYR record

           MOVE REWRT                  TO PAYR-FUNCTION.

           CALL 'PAYRIO'               USING PAYR-PARAMS.
           IF  PAYR-STATUZ         NOT = O-K
              MOVE PAYR-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

       3999-EXIT.
           EXIT.

       A3000-WRITE-LETTER SECTION.                                      <P002>
      ****************************                                      <P002>
       A3010-START.                                                     <P002>
      *                                                                 <UL005>
      * If skip basic premium product and do not have rider,            <UL005>
      * then do not generate Billing notice Letter.                     <UL005>
           IF  WSAA-TV103-FOUND        = 'Y'                            <UL005>
TVAN       OR  WSAA-TZ028-FOUND        = 'Y'                            <CS020>
               PERFORM A1400-COUNT-COMPONENTS                           <UL005>
               IF  WSAA-COVR-COUNT    <= 1                              <UL005>
                   GO TO A3090-EXIT                                     <UL005>
               END-IF                                                   <UL005>
           END-IF.                                                      <UL005>
tuyen *
      * If Total premium has paid in current policy year +
      * Total top-up in current policy year + Suspense >=
      * Premium plan in frequency * times of billing in current year
      * do not generate Billing Notice Letter.
      *
           PERFORM A3200-VALID-CONDITION.
           IF WSAA-PREM-PAID-AMT   >= WSAA-PLAN-PREM-AMT
              GO TO A3090-EXIT
tuyen      END-IF.
      *                                                                 <P002>
      * Read Table T6634 for get letter-type.                           <P002>
      *                                                                 <P002>
           MOVE BATD-TRCDE             TO WSAA-ITEM-BATCTRCDE.          <P002>
           MOVE CHDRLIF-CNTTYPE        TO WSAA-ITEM-CNTTYPE.            <P002>
                                                                        <P002>
      *A3020-READ-T6634.                                        <PCPPRT><P002>
       A3020-READ-TR384.                                                <PCPPRT>
                                                                        <P002>
           MOVE SPACES                 TO ITEM-DATA-KEY.                <P002>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <P002>
           MOVE BSPR-COMPANY           TO ITEM-ITEMCOY.                 <P002>
      **** MOVE T6634                  TO ITEM-ITEMTABL.        <PCPPRT><P002>
      **** MOVE WSAA-ITEM-T6634        TO ITEM-ITEMITEM.        <PCPPRT><P002>
           MOVE TR384                  TO ITEM-ITEMTABL.                <PCPPRT>
           MOVE WSAA-ITEM-TR384        TO ITEM-ITEMITEM.                <PCPPRT>
           MOVE READR                  TO ITEM-FUNCTION.                <P002>
                                                                        <P002>
           CALL 'ITEMIO'            USING ITEM-PARAMS.                  <P002>
           IF ITEM-STATUZ              NOT =  O-K AND MRNF              <P002>
              MOVE ITEM-STATUZ         TO SYSR-STATUZ                   <P002>
              MOVE ITEM-PARAMS         TO SYSR-PARAMS                   <P002>
              PERFORM 600-FATAL-ERROR.                                  <P002>
      *                                                                 <P002>
      * If record not found then read again using generic key.          <P002>
      *                                                                 <P002>
           IF ITEM-STATUZ              = O-K                            <P002>
              NEXT SENTENCE                                             <P002>
           ELSE                                                         <P002>
           IF ITEM-STATUZ              = MRNF                           <P002>
           AND WSAA-ITEM-CNTTYPE       NOT = '***'                      <P002>
               MOVE '***'              TO WSAA-ITEM-CNTTYPE             <P002>
      ****     GO TO A3020-READ-T6634                           <PCPPRT><P002>
               GO TO A3020-READ-TR384                                   <PCPPRT>
           ELSE                                                         <P002>
              MOVE ITEM-STATUZ         TO SYSR-STATUZ                   <P002>
              MOVE ITEM-PARAMS         TO SYSR-PARAMS                   <P002>
              PERFORM 600-FATAL-ERROR.                                  <P002>
                                                                        <P002>
      **** MOVE ITEM-GENAREA           TO T6634-T6634-REC.              <P002>
           MOVE ITEM-GENAREA           TO TR384-TR384-REC.              <P002>
                                                                        <P002>
      **** IF T6634-LETTER-TYPE        = SPACES                 <PCPPRT><P002>
           IF TR384-LETTER-TYPE        = SPACES                         <PCPPRT>
              GO TO A3090-EXIT                                          <P002>
           END-IF.                                                      <P002>
                                                                        <P002>
           MOVE SPACES                 TO WSAA-STATUZ.                  <PS001>
                                                                        <PS001>
           CALL 'ZREGPVAL'          USING WSAA-STATUZ                   <PS001>
                                          CHDRLIF-CHDRCOY               <PS001>
                                          CHDRLIF-CHDRNUM.              <PS001>
                                                                        <PS001>
           IF WSAA-STATUZ               = 'Y'                           <PS001>
              GO TO A3090-EXIT                                          <PS001>
           END-IF.                                                      <PS001>
                                                                        <PS001>
      *****   Get set-up parameter for call 'HLETRQS'                   <P002>
                                                                        <P002>
           MOVE SPACE                  TO LETRQST-STATUZ.               <P002>
           MOVE CHDRLIF-CHDRCOY        TO LETRQST-REQUEST-COMPANY.      <P002>
      **** MOVE T6634-LETTER-TYPE      TO LETRQST-LETTER-TYPE.  <PCPPRT><P002>
           MOVE TR384-LETTER-TYPE      TO LETRQST-LETTER-TYPE.          <PCPPRT>
           MOVE BSSC-EFFECTIVE-DATE    TO LETRQST-LETTER-REQUEST-DATE.  <P002>
           MOVE CHDRLIF-CHDRPFX        TO LETRQST-RDOCPFX.              <P002>
           MOVE CHDRLIF-CHDRCOY        TO LETRQST-RDOCCOY.              <P002>
           MOVE CHDRLIF-CHDRNUM        TO LETRQST-RDOCNUM.              <P002>
           MOVE BSSC-LANGUAGE          TO LETRQST-OTHER-KEYS.           <P002>
           IF FLEXIBLE-PREMIUM-CONTRACT                                 <UL001>
               MOVE WSAA-OLD-BTDATE    TO LSAV-LD-DATE                  <UL001>
           ELSE                                                         <UL001>
           MOVE LINSRNL-INSTFROM       TO LSAV-LD-DATE.                 <PHE003>
           MOVE LSAV-SAVE-OTHER-KEYS   TO LETRQST-OTHER-KEYS.           <PHE003>
                                                                        <P002>
           MOVE CHDRLIF-COWNCOY        TO LETRQST-CLNTCOY.              <P002>
           MOVE CHDRLIF-COWNNUM        TO LETRQST-CLNTNUM.              <P002>
           MOVE CHDRLIF-DESPNUM        TO LETRQST-DESPNUM.              <GAPPH2>
           MOVE CHDRLIF-CHDRCOY        TO LETRQST-CHDRCOY.              <P002>
           MOVE CHDRLIF-CHDRNUM        TO LETRQST-CHDRNUM.              <P002>
           MOVE CHDRLIF-TRANNO         TO LETRQST-TRANNO.               <P002>
           MOVE CHDRLIF-CNTBRANCH      TO LETRQST-BRANCH.               <PCPPRT>
           MOVE 'ADD'                  TO LETRQST-FUNCTION.             <P002>
                                                                        <P002>
      **** CALL 'HLETRQS' USING LETRQST-PARAMS.                 <PCPPRT><P002>
           CALL 'LETRQST' USING LETRQST-PARAMS.                         <PCPPRT>
                                                                        <P002>
           IF  LETRQST-STATUZ          NOT = O-K                        <P002>
               MOVE LETRQST-PARAMS     TO SYSR-PARAMS                   <P002>
               MOVE LETRQST-STATUZ     TO SYSR-STATUZ                   <P002>
               PERFORM 600-FATAL-ERROR                                  <P002>
           END-IF.                                                      <P002>
                                                                        <P002>
       A3090-EXIT.                                                      <P002>
           EXIT.                                                        <P002>
                                                                        <GAPPH2>
       A3100-CHECK-STAFF-PRINT SECTION.                                 <GAPPH2>
      *******************************                                   <GAPPH2>
       A3110-START.                                                     <GAPPH2>
      *                                                                 <GAPPH2>
           MOVE 'Y'                    TO WSAA-PRINT-FLG.               <GAPPH2>
           MOVE SPACES                 TO SCNTENQ-PARAMS.               <GAPPH2>
           MOVE CHDRLIF-CHDRPFX        TO SCNTENQ-CHDRPFX.              <GAPPH2>
           MOVE CHDRLIF-CHDRCOY        TO SCNTENQ-CHDRCOY.              <GAPPH2>
           MOVE CHDRLIF-CHDRNUM        TO SCNTENQ-CHDRNUM.              <GAPPH2>
           MOVE SCNTENQREC             TO SCNTENQ-FORMAT.               <GAPPH2>
           MOVE READR                  TO SCNTENQ-FUNCTION.             <GAPPH2>
                                                                        <GAPPH2>
           CALL 'SCNTENQIO'         USING SCNTENQ-PARAMS.               <GAPPH2>
                                                                        <GAPPH2>
           IF SCNTENQ-STATUZ        NOT = O-K AND MRNF                  <GAPPH2>
               MOVE SCNTENQ-PARAMS     TO SYSR-PARAMS                   <GAPPH2>
               PERFORM 600-FATAL-ERROR                                  <GAPPH2>
           END-IF.                                                      <GAPPH2>
                                                                        <GAPPH2>
           IF SCNTENQ-STATUZ            = MRNF                          <GAPPH2>
              GO TO A3190-EXIT                                          <GAPPH2>
           END-IF.                                                      <GAPPH2>
                                                                        <GAPPH2>
           IF  SCNTENQ-FROMOFF           = SPACES                       <GAPPH2>
               GO TO A3190-EXIT                                         <GAPPH2>
           END-IF.                                                      <GAPPH2>
                                                                        <GAPPH2>
           MOVE 'IT'                      TO WSAA-ITEMPFX.              <GAPPH2>
           MOVE BSPR-COMPANY              TO WSAA-ITEMCOY.              <GAPPH2>
           MOVE TV070                     TO WSAA-ITEMTABL.             <GAPPH2>
           MOVE SCNTENQ-FROMOFF           TO WSAA-ITEMITEM.             <GAPPH2>
           PERFORM 1100-READ-ITEMPF.                                    <GAPPH2>
           MOVE ITEM-GENAREA              TO TV070-TV070-REC.           <GAPPH2>
                                                                        <GAPPH2>
           IF  ITEM-STATUZ                 = MRNF                       <GAPPH2>
               GO TO A3190-EXIT                                         <GAPPH2>
           END-IF.                                                      <GAPPH2>
                                                                        <GAPPH2>
           IF  ITEM-STATUZ                 = O-K                        <GAPPH2>
           AND TV070-PRNTFLG-01            = 'Y'                        <GAPPH2>
           AND TV070-PRNTFLG-02            = 'Y'                        <GAPPH2>
               GO TO A3190-EXIT                                         <GAPPH2>
           END-IF.                                                      <GAPPH2>
                                                                        <GAPPH2>
           IF  TV070-PRNTFLG-01          = 'N'                          <GAPPH2>
               MOVE CHDRLIF-COWNNUM    TO WSAA-CLNTNUM                  <GAPPH2>
               PERFORM A3120-CHECK-SCLT                                 <GAPPH2>
               IF SCLTENQ-PRTFLG         = 'N'                          <GAPPH2>
                  MOVE 'N'             TO WSAA-PRINT-FLG                <GAPPH2>
                  GO TO A3190-EXIT                                      <GAPPH2>
               END-IF                                                   <GAPPH2>
           END-IF.                                                      <GAPPH2>
                                                                        <GAPPH2>
           IF  TV070-PRNTFLG-02          = 'N'                          <GAPPH2>
               MOVE SPACES             TO LIFEENQ-PARAMS                <GAPPH2>
               MOVE CHDRLIF-COWNPFX    TO LIFEENQ-CHDRCOY               <GAPPH2>
               MOVE CHDRLIF-COWNCOY    TO LIFEENQ-CHDRNUM               <GAPPH2>
               MOVE '01'               TO LIFEENQ-LIFE                  <GAPPH2>
               MOVE '00'               TO LIFEENQ-JLIFE                 <GAPPH2>
               MOVE LIFEENQREC         TO LIFEENQ-FORMAT                <GAPPH2>
               MOVE BEGN               TO LIFEENQ-FUNCTION              <GAPPH2>
               PERFORM A3130-READ-LIFE UNTIL LIFEENQ-STATUZ = ENDP      <GAPPH2>
           END-IF.                                                      <GAPPH2>
                                                                        <GAPPH2>
           GO TO A3190-EXIT.                                            <GAPPH2>
      *                                                                 <GAPPH2>
       A3120-CHECK-SCLT.                                                <GAPPH2>
      *                                                                 <GAPPH2>
           MOVE SPACES                 TO SCLTENQ-PARAMS.               <GAPPH2>
           MOVE CHDRLIF-COWNPFX        TO SCLTENQ-CLNTPFX.              <GAPPH2>
           MOVE CHDRLIF-COWNCOY        TO SCLTENQ-CLNTCOY.              <GAPPH2>
           MOVE WSAA-CLNTNUM           TO SCLTENQ-CLNTNUM.              <GAPPH2>
           MOVE SCLTENQREC             TO SCLTENQ-FORMAT.               <GAPPH2>
           MOVE READR                  TO SCLTENQ-FUNCTION.             <GAPPH2>
                                                                        <GAPPH2>
           CALL 'SCLTENQIO'         USING SCLTENQ-PARAMS.               <GAPPH2>
                                                                        <GAPPH2>
           IF SCLTENQ-STATUZ        NOT = O-K AND MRNF                  <GAPPH2>
               MOVE SCLTENQ-PARAMS     TO SYSR-PARAMS                   <GAPPH2>
               PERFORM 600-FATAL-ERROR                                  <GAPPH2>
           END-IF.                                                      <GAPPH2>
      *                                                                 <GAPPH2>
       A3130-READ-LIFE.                                                 <GAPPH2>
      *                                                                 <GAPPH2>
           CALL 'LIFEENQIO'         USING LIFEENQ-PARAMS.               <GAPPH2>
                                                                        <GAPPH2>
           IF LIFEENQ-STATUZ        NOT = O-K AND ENDP                  <GAPPH2>
              MOVE LIFEENQ-PARAMS      TO SYSR-PARAMS                   <GAPPH2>
              PERFORM 600-FATAL-ERROR                                   <GAPPH2>
           END-IF.                                                      <GAPPH2>
                                                                        <GAPPH2>
           IF LIFEENQ-STATUZ        NOT = O-K                           <GAPPH2>
           OR LIFEENQ-CHDRCOY       NOT = CHDRLIF-CHDRCOY               <GAPPH2>
           OR LIFEENQ-CHDRNUM       NOT = CHDRLIF-CHDRNUM               <GAPPH2>
              MOVE ENDP                TO LIFEENQ-STATUZ                <GAPPH2>
           ELSE                                                         <GAPPH2>
              MOVE LIFEENQ-LIFCNUM     TO WSAA-CLNTNUM                  <GAPPH2>
              PERFORM A3120-CHECK-SCLT                                  <GAPPH2>
                                                                        <GAPPH2>
              IF SCLTENQ-PRTFLG         = 'N'                           <GAPPH2>
                 MOVE 'N'              TO WSAA-PRINT-FLG                <GAPPH2>
                 MOVE ENDP             TO LIFEENQ-STATUZ                <GAPPH2>
              END-IF                                                    <GAPPH2>
           END-IF.                                                      <GAPPH2>
                                                                        <GAPPH2>
           MOVE NEXTR                  TO LIFEENQ-FUNCTION.             <GAPPH2>
                                                                        <GAPPH2>
      *                                                                 <GAPPH2>
       A3190-EXIT.                                                      <GAPPH2>
           EXIT.                                                        <GAPPH2>
                                                                        <P002>
       4000-RELEASE-SOFTLOCK SECTION.
      *******************************
       4010-START.

      * Release the soft lock on the contract.

           MOVE BATD-COMPANY           TO SFTL-COMPANY.
           MOVE CHDRNUM                TO SFTL-ENTITY.
           MOVE 'CH'                   TO SFTL-ENTTYP.
           MOVE 999999                 TO SFTL-USER.
           MOVE BPRD-AUTH-CODE         TO SFTL-TRANSACTION.
           MOVE SPACES                 TO SFTL-STATUZ.
           MOVE 'UNLK'                 TO SFTL-FUNCTION.

           CALL 'SFTLOCK'              USING SFTL-SFTLOCK-REC.
           IF SFTL-STATUZ          NOT = O-K
              MOVE SFTL-SFTLOCK-REC    TO SYSR-PARAMS
              MOVE SFTL-STATUZ         TO SYSR-STATUZ                   <CAS1.0>
              PERFORM 600-FATAL-ERROR
           END-IF.

       4999-EXIT.
           EXIT.

       3500-COMMIT SECTION.
      **********************
       3510-COMMIT.

      * Place any additional commitment processing in here.

       3590-EXIT.
           EXIT.

       3600-ROLLBACK SECTION.
      **********************
       3610-ROLLBACK.

      * Place any additional rollback processing in here.

       3690-EXIT.
           EXIT.

       4000-CLOSE SECTION.
      ********************
       4010-CLOSE-FILES.

      *   Close all files, and delete the override function

           CLOSE PAYXPF.

           MOVE 'DLTOVR FILE(PAYXPF)'
                                       TO WSAA-QCMDEXC.
           CALL 'QCMDEXC' USING WSAA-QCMDEXC WSAA-QCMDEXC-LENGTH.

           MOVE O-K                    TO LSAA-STATUZ.

       4090-EXIT.
           EXIT.


       5000-CALL-XCVRT SECTION.
      *************************
       5000-START.

           MOVE PAYR-CNTCURR           TO CLNK-CURR-IN.
           MOVE PAYR-BILLCURR          TO CLNK-CURR-OUT.

           MOVE 0                      TO CLNK-RATE-USED
                                          CLNK-AMOUNT-OUT.
     ****  MOVE BSSC-EFFECTIVE-DATE    TO CLNK-CASHDATE.                <LA5134>
           MOVE WSAA-CASHDATE          TO CLNK-CASHDATE.                <LA5134>
           MOVE 'SURR'                 TO CLNK-FUNCTION.
           MOVE BATD-COMPANY           TO CLNK-COMPANY.

           CALL 'XCVRT' USING CLNK-CLNK002-REC.

           IF CLNK-STATUZ           NOT = O-K
              MOVE CLNK-CLNK002-REC    TO SYSR-PARAMS
              MOVE CLNK-STATUZ         TO SYSR-STATUZ                   <CAS1.0>
              PERFORM 600-FATAL-ERROR
           END-IF.

      **** MOVE CLNK-AMOUNT-OUT        TO ZRDP-AMOUNT-IN.               <V76F06>
      **** MOVE PAYR-BILLCURR          TO ZRDP-CURRENCY.                <V76F06>
      **** PERFORM 8000-CALL-ROUNDING.                                  <V76F06>
      **** MOVE ZRDP-AMOUNT-OUT        TO CLNK-AMOUNT-OUT.              <V76F06>
                                                                        <V76F06>
           IF CLNK-AMOUNT-OUT          NOT = 0                          <V76F06>
              MOVE CLNK-AMOUNT-OUT     TO ZRDP-AMOUNT-IN                <V76F06>
              MOVE PAYR-BILLCURR       TO ZRDP-CURRENCY                 <V76F06>
              PERFORM 8000-CALL-ROUNDING                                <V76F06>
              MOVE ZRDP-AMOUNT-OUT     TO CLNK-AMOUNT-OUT               <V76F06>
           END-IF.                                                      <V76F06>
                                                                        <V76F06>
       5990-EXIT.
           EXIT.

       6000-CALCULATE-TAX-RELIEF  SECTION.
      ************************************
       6010-START.

           MOVE SPACES                 TO LINSRNL-TAXRELMTH.
           MOVE ZERO                   TO PRAS-TAXRELAMT.

           IF PAYR-TAXRELMTH           = SPACES
              GO TO                    6999-EXIT
           END-IF.

      * Look up the subroutine on T6687 Array in working storage.

           SEARCH ALL WSAA-T6687-REC
           AT END
               MOVE MRNF               TO SYSR-STATUZ
               STRING T6687
                      PAYR-TAXRELMTH
                                       DELIMITED BY SIZE
                                       INTO WSYS-SYSPARAMS
               MOVE WSYS-SYSTEM-ERROR-PARAMS TO SYSR-PARAMS

               PERFORM 600-FATAL-ERROR

           WHEN WSAA-T6687-KEY (WSAA-T6687-IX) = PAYR-TAXRELMTH
              CONTINUE
           END-SEARCH.

           IF WSAA-T6687-TAXRELSUBR (WSAA-T6687-IX)  = SPACES
              GO TO                    6999-EXIT
           END-IF.

      * If the tax relief method is not spaces calculate the tax
      * relief amount and deduct it from the premium......

           MOVE CLRF-CLNTNUM           TO PRAS-CLNTNUM.
           MOVE CLRF-CLNTCOY           TO PRAS-CLNTCOY.
           MOVE PAYR-INCOME-SEQ-NO     TO PRAS-INCOME-SEQ-NO.
           MOVE CHDRLIF-CNTTYPE        TO PRAS-CNTTYPE.
           MOVE PAYR-TAXRELMTH         TO PRAS-TAXRELMTH.
           MOVE PAYR-BILLCD            TO PRAS-EFFDATE.
           MOVE CHDRCOY                TO PRAS-COMPANY.
           MOVE O-K                    TO PRAS-STATUZ.

           CALL WSAA-T6687-TAXRELSUBR (WSAA-T6687-IX)
                                       USING PRAS-PRASCALC-REC.

           IF PRAS-STATUZ                    NOT = O-K
              MOVE PRAS-STATUZ               TO SYSR-STATUZ
              MOVE WSAA-T6687-TAXRELSUBR (WSAA-T6687-IX)
                                             TO SYSR-SUBRNAME
              MOVE PRAS-PRASCALC-REC         TO WSYS-SYSPARAMS
              MOVE WSYS-SYSTEM-ERROR-PARAMS  TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

      **** MOVE PRAS-TAXRELAMT         TO ZRDP-AMOUNT-IN.               <V76F06>
      **** MOVE PAYR-BILLCURR          TO ZRDP-CURRENCY.                <V76F06>
      **** PERFORM 8000-CALL-ROUNDING.                                  <V76F06>
      **** MOVE ZRDP-AMOUNT-OUT        TO PRAS-TAXRELAMT.               <V76F06>
                                                                        <V76F06>
           IF PRAS-TAXRELAMT           NOT = 0                          <V76F06>
              MOVE PRAS-TAXRELAMT      TO ZRDP-AMOUNT-IN                <V76F06>
              MOVE PAYR-BILLCURR       TO ZRDP-CURRENCY                 <V76F06>
              PERFORM 8000-CALL-ROUNDING                                <V76F06>
              MOVE ZRDP-AMOUNT-OUT     TO PRAS-TAXRELAMT                <V76F06>
           END-IF.                                                      <V76F06>
                                                                        <V76F06>
           IF PRAS-TAXRELAMT NOT = 0
               MOVE PAYR-TAXRELMTH     TO LINSRNL-TAXRELMTH
           END-IF.

       6999-EXIT.
           EXIT.

       7000-WRITE-FPCO SECTION.                                         <D9604>
       7010-START.                                                      <D9604>
                                                                        <D9604>
      * Read all active coverages for the contract              <D9604>
                                                                        <D9604>
           MOVE ZEROES                 TO WSAA-TOT-AMT.                 <D9604>
           MOVE ZEROES                 TO WSAA-OVERDUE-PER.             <D9604>
           MOVE SPACES                 TO COVRLNB-PARAMS.               <D9604>
           MOVE SPACES                 TO WSAA-COVERAGE.                <D9604>
           MOVE CHDRLIF-CHDRNUM        TO COVRLNB-CHDRNUM.              <D9604>
           MOVE CHDRLIF-CHDRCOY        TO COVRLNB-CHDRCOY.              <D9604>
           MOVE ZEROES                 TO COVRLNB-PLAN-SUFFIX.          <D9604>
           MOVE COVRLNBREC             TO COVRLNB-FORMAT.               <D9604>
           MOVE BEGN                   TO COVRLNB-FUNCTION.             <D9604>
                                                                        <D9604>
           PERFORM 7100-READ-COVRLNB UNTIL                              <D9604>
               COVRLNB-STATUZ = ENDP.                                   <D9604>
                                                                        <D9604>
      * Read the FPRM record:                                   <D9604>
                                                                        <D9604>
           MOVE CHDRLIF-CHDRNUM        TO FPRM-CHDRNUM.                 <D9604>
           MOVE CHDRLIF-CHDRCOY        TO FPRM-CHDRCOY.                 <D9604>
           IF WSAA-GOT-PAYR-AT-BTDATE            = 'Y'                  <D9604>
               MOVE PAYRLIF-PAYRSEQNO  TO FPRM-PAYRSEQNO                <D9604>
           ELSE                                                         <D9604>
               MOVE PAYR-PAYRSEQNO     TO FPRM-PAYRSEQNO.               <D9604>
           MOVE FPRMREC                TO FPRM-FORMAT.                  <D9604>
           MOVE READH                  TO FPRM-FUNCTION.                <D9604>
                                                                        <D9604>
           CALL 'FPRMIO'               USING FPRM-PARAMS.               <D9604>
           IF FPRM-STATUZ              NOT = O-K                        <D9604>
              MOVE FPRM-PARAMS         TO SYSR-PARAMS                   <D9604>
              MOVE FPRM-STATUZ         TO SYSR-STATUZ                   <D9604>
              PERFORM  600-FATAL-ERROR                                  <D9604>
           END-IF.                                                      <D9604>
                                                                        <D9604>
      * Update total billed:                                    <D9604>
                                                                        <D9604>
           COMPUTE FPRM-TOTAL-BILLED = FPRM-TOTAL-BILLED                <D9604>
                                     + WSAA-TOT-AMT                     <D9604>
           END-COMPUTE.                                                 <D9604>
                                                                        <D9604>
      * Log total billed:                                       <D9604>
                                                                        <D9604>
           MOVE CT12                   TO CONT-TOTNO.                   <D9604>
           MOVE WSAA-TOT-AMT           TO CONT-TOTVAL.                  <D9604>
           PERFORM 001-CALL-CONTOT.                                     <D9604>
                                                                        <D9604>
           COMPUTE FPRM-MIN-PRM-REQD ROUNDED =                          <D9604>
                 ( WSAA-OVERDUE-PER                                     <D9604>
                 / 100                                                  <D9604>
                 * WSAA-TOT-AMT)                                        <D9604>
                 + FPRM-MIN-PRM-REQD                                    <D9604>
           END-COMPUTE.                                                 <D9604>
                                                                        <D9604>
      **** MOVE FPRM-MIN-PRM-REQD      TO ZRDP-AMOUNT-IN.               <V76F06>
      **** MOVE PAYR-BILLCURR          TO ZRDP-CURRENCY.                <V76F06>
      **** PERFORM 8000-CALL-ROUNDING.                                  <V76F06>
      **** MOVE ZRDP-AMOUNT-OUT        TO FPRM-MIN-PRM-REQD.            <V76F06>
                                                                        <V76F06>
           IF FPRM-MIN-PRM-REQD        NOT = 0                          <V76F06>
              MOVE FPRM-MIN-PRM-REQD   TO ZRDP-AMOUNT-IN                <V76F06>
              MOVE PAYR-BILLCURR       TO ZRDP-CURRENCY                 <V76F06>
              PERFORM 8000-CALL-ROUNDING                                <V76F06>
              MOVE ZRDP-AMOUNT-OUT     TO FPRM-MIN-PRM-REQD             <V76F06>
           END-IF.                                                      <V76F06>
                                                                        <V76F06>
      * Write the record back to FPRM file.                     <D9604>
                                                                        <D9604>
           MOVE REWRT                  TO FPRM-FUNCTION.                <D9604>
                                                                        <D9604>
           CALL 'FPRMIO'               USING FPRM-PARAMS.               <D9604>
           IF FPRM-STATUZ              NOT = O-K                        <D9604>
              MOVE FPRM-PARAMS         TO SYSR-PARAMS                   <D9604>
              MOVE FPRM-STATUZ         TO SYSR-STATUZ                   <D9604>
              PERFORM  600-FATAL-ERROR                                  <D9604>
           END-IF.                                                      <D9604>
                                                                        <D9604>
       7090-EXIT.                                                       <D9604>
           EXIT.                                                        <D9604>
                                                                        <D9604>
       7100-READ-COVRLNB SECTION.                                       <D9604>
       7110-COVRLNB.                                                    <D9604>
                                                                        <D9604>
           CALL 'COVRLNBIO'               USING COVRLNB-PARAMS.         <D9604>
           IF COVRLNB-STATUZ              NOT = O-K                     <D9604>
           AND COVRLNB-STATUZ             NOT = ENDP                    <D9604>
              MOVE COVRLNB-PARAMS   TO SYSR-PARAMS                      <D9604>
              MOVE COVRLNB-STATUZ   TO SYSR-STATUZ                      <D9604>
              PERFORM  600-FATAL-ERROR                                  <D9604>
           END-IF.                                                      <D9604>
                                                                        <D9604>
           IF COVRLNB-CHDRCOY          NOT = CHDRLIF-CHDRCOY            <D9604>
           OR COVRLNB-CHDRNUM          NOT = CHDRLIF-CHDRNUM            <D9604>
           OR COVRLNB-STATUZ               = ENDP                       <D9604>
              MOVE ENDP                TO COVRLNB-STATUZ                <D9604>
              GO TO 7190-EXIT                                           <D9604>
           END-IF.                                                      <D9604>
                                                                        <D9604>
      * Check to see if coverage is of a valid status           <D9604>
                                                                        <D9604>
           MOVE 'N'                    TO WSAA-VALID-COVERAGE.          <D9604>
           IF COVRLNB-VALIDFLAG        NOT = '1'                        <D9604>
              MOVE NEXTR            TO COVRLNB-FUNCTION                 <D9604>
              GO TO 7190-EXIT                                           <D9604>
           END-IF.                                                      <D9604>
                                                                        <D9604>
           PERFORM VARYING WSAA-T5679-SUB                               <D9604>
                   FROM 1 BY 1                                          <D9604>
                   UNTIL WSAA-T5679-SUB      > 12                       <D9604>
                                                                        <D9604>
              IF T5679-COV-RISK-STAT (WSAA-T5679-SUB)= COVRLNB-STATCODE <D9604>
                 PERFORM VARYING WSAA-T5679-SUB                         <D9604>
                         FROM 1 BY 1                                    <D9604>
                         UNTIL WSAA-T5679-SUB   > 12                    <D9604>
                    IF T5679-COV-PREM-STAT (WSAA-T5679-SUB)             <D9604>
                                                    = COVRLNB-PSTATCODE <D9604>
                       MOVE  13  TO WSAA-T5679-SUB                      <D9604>
                       MOVE 'Y'  TO WSAA-VALID-COVERAGE                 <D9604>
                    END-IF                                              <D9604>
                 END-PERFORM                                            <D9604>
              END-IF                                                    <D9604>
           END-PERFORM.                                                 <D9604>
                                                                        <D9604>
      *  If the coverage is not of a valid status read the next <D9604>
      *  record for the contract:                               <D9604>
                                                                        <D9604>
           IF NOT VALID-COVERAGE                                        <D9604>
              MOVE NEXTR            TO COVRLNB-FUNCTION                 <D9604>
              GO TO 7190-EXIT                                           <D9604>
           END-IF.                                                      <D9604>
                                                                        <D9604>
           IF COVRLNB-INSTPREM         = 0                              <D9604>
              MOVE NEXTR            TO COVRLNB-FUNCTION                 <D9604>
              GO TO 7190-EXIT                                           <D9604>
           END-IF                                                       <D9604>
                                                                        <D9604>
           MOVE COVRLNB-DATA-KEY       TO WSAA-COVERAGE.                <D9604>
                                                                        <D9604>
           MOVE SPACES                 TO INCRRGP-PARAMS.               <D9604>
           MOVE COVRLNB-CHDRCOY        TO INCRRGP-CHDRCOY.              <D9604>
           MOVE COVRLNB-CHDRNUM        TO INCRRGP-CHDRNUM.              <D9604>
           MOVE COVRLNB-COVERAGE       TO INCRRGP-COVERAGE.             <D9604>
           MOVE COVRLNB-RIDER          TO INCRRGP-RIDER.                <D9604>
           MOVE COVRLNB-PLAN-SUFFIX    TO INCRRGP-PLAN-SUFFIX.          <D9604>
           MOVE INCRRGPREC             TO INCRRGP-FORMAT.               <D9604>
           MOVE BEGN                   TO INCRRGP-FUNCTION.             <D9604>
                                                                        <D9604>
           CALL 'INCRRGPIO'         USING INCRRGP-PARAMS.               <D9604>
                                                                        <D9604>
           IF  INCRRGP-STATUZ       NOT = O-K                           <D9604>
           AND INCRRGP-STATUZ       NOT = ENDP                          <D9604>
               MOVE INCRRGP-PARAMS  TO SYSR-PARAMS                      <D9604>
               PERFORM 600-FATAL-ERROR                                  <D9604>
           END-IF.                                                      <D9604>
                                                                        <D9604>
           IF  INCRRGP-CHDRCOY          = COVRLNB-CHDRCOY               <D9604>
           AND INCRRGP-CHDRNUM          = COVRLNB-CHDRNUM               <D9604>
           AND INCRRGP-COVERAGE         = COVRLNB-COVERAGE              <D9604>
           AND INCRRGP-RIDER            = COVRLNB-RIDER                 <D9604>
           AND INCRRGP-PLAN-SUFFIX      = COVRLNB-PLAN-SUFFIX           <D9604>
           AND INCRRGP-STATUZ      NOT  = ENDP                          <D9604>
           AND INCRRGP-CRRCD             < PAYR-BTDATE                  <D9604>
               COMPUTE WSAA-COVR-INC     = INCRRGP-NEWINST -            <D9604>
                                           INCRRGP-LAST-INST            <D9604>
           ELSE                                                         <D9604>
               MOVE ZERO                 TO WSAA-COVR-INC               <D9604>
           END-IF.                                                      <D9604>
                                                                        <D9604>
      * Select FPCO record which is active and has not reached  <D9604>
      * Target Premium                                          <D9604>
                                                                        <D9604>
           MOVE SPACES              TO FPCO-PARAMS.                     <D9604>
           MOVE COVRLNB-CHDRCOY     TO FPCO-CHDRCOY.                    <D9604>
           MOVE COVRLNB-CHDRNUM     TO FPCO-CHDRNUM.                    <D9604>
           MOVE COVRLNB-LIFE        TO FPCO-LIFE.                       <D9604>
           MOVE COVRLNB-COVERAGE    TO FPCO-COVERAGE.                   <D9604>
           MOVE COVRLNB-RIDER       TO FPCO-RIDER.                      <D9604>
           MOVE COVRLNB-PLAN-SUFFIX TO FPCO-PLAN-SUFFIX.                <D9604>
           MOVE ZEROES              TO FPCO-TARGFROM.                   <D9604>
      **** MOVE BEGNH               TO FPCO-FUNCTION.           <V65L19><D9604>
           MOVE BEGN                TO FPCO-FUNCTION.                   <V65L19>
                                                                        <D9604>
           PERFORM 7200-READ-FPCO UNTIL                                 <D9604>
               FPCO-STATUZ = ENDP.                                      <D9604>
                                                                        <D9604>
           MOVE NEXTR                  TO COVRLNB-FUNCTION.             <D9604>
                                                                        <D9604>
       7190-EXIT.                                                       <D9604>
           EXIT.                                                        <D9604>
                                                                        <D9604>
       7200-READ-FPCO SECTION.                                          <D9604>
       7210-FPCO.                                                       <D9604>
                                                                        <D9604>
           CALL 'FPCOIO'               USING FPCO-PARAMS.               <D9604>
           IF FPCO-STATUZ              NOT = O-K                        <D9604>
           AND FPCO-STATUZ             NOT = ENDP                       <A06923>
              MOVE FPCO-PARAMS   TO SYSR-PARAMS                         <D9604>
              MOVE FPCO-STATUZ   TO SYSR-STATUZ                         <D9604>
              PERFORM  600-FATAL-ERROR                                  <D9604>
           END-IF.                                                      <D9604>
                                                                        <D9604>
           IF FPCO-CHDRCOY          NOT = COVRLNB-CHDRCOY               <D9604>
           OR FPCO-CHDRNUM          NOT = COVRLNB-CHDRNUM               <D9604>
           OR FPCO-LIFE             NOT = COVRLNB-LIFE                  <D9604>
           OR FPCO-COVERAGE         NOT = COVRLNB-COVERAGE              <D9604>
           OR FPCO-RIDER            NOT = COVRLNB-RIDER                 <D9604>
           OR FPCO-PLAN-SUFFIX      NOT = COVRLNB-PLAN-SUFFIX           <D9604>
           OR FPCO-STATUZ           = ENDP                              <A06923>
      ****    MOVE RLSE              TO FPCO-FUNCTION           <V65L19><D9604>
      ****    CALL 'FPCOIO'               USING FPCO-PARAMS     <V65L19><D9604>
      ****    IF FPCO-STATUZ              NOT = O-K             <V65L19><D9604>
      ****       MOVE FPCO-PARAMS   TO SYSR-PARAMS              <V65L19><D9604>
      ****       MOVE FPCO-STATUZ   TO SYSR-STATUZ              <V65L19><D9604>
      ****       PERFORM  600-FATAL-ERROR                       <V65L19><D9604>
      ****    END-IF                                            <V65L19><D9604>
              MOVE ENDP  TO FPCO-STATUZ                                 <D9604>
              GO TO 7290-EXIT                                           <D9604>
           END-IF.                                                      <D9604>
                                                                        <D9604>
           IF FPCO-BILLED-IN-PERIOD > FPCO-TARGET-PREMIUM OR            <D9604>
              FPCO-BILLED-IN-PERIOD = FPCO-TARGET-PREMIUM               <D9604>
      ****    MOVE RLSE              TO FPCO-FUNCTION           <V65L19><D9604>
      ****    CALL 'FPCOIO'          USING FPCO-PARAMS          <V65L19><D9604>
      ****    IF FPCO-STATUZ         NOT = O-K                  <V65L19><D9604>
      ****       MOVE FPCO-PARAMS   TO SYSR-PARAMS              <V65L19><D9604>
      ****       MOVE FPCO-STATUZ   TO SYSR-STATUZ              <V65L19><D9604>
      ****       PERFORM  600-FATAL-ERROR                       <V65L19><D9604>
      ****    END-IF                                            <V65L19><D9604>
              MOVE NEXTR            TO FPCO-FUNCTION                    <D9604>
              GO TO 7290-EXIT                                           <D9604>
           END-IF.                                                      <D9604>
                                                                        <D9604>
      * Update FPCO record with total of installment due and    <D9604>
      * overdue minimum percentage. This should include any     <D9604>
      * increases due                                           <D9604>
                                                                        <D9604>
           COMPUTE FPCO-BILLED-IN-PERIOD  = FPCO-BILLED-IN-PERIOD       <D9604>
                                            + COVRLNB-INSTPREM          <D9604>
                                            + WSAA-COVR-INC             <D9604>
           END-COMPUTE.                                                 <D9604>
           COMPUTE WSAA-TOT-AMT      = WSAA-TOT-AMT                     <D9604>
                                     + COVRLNB-INSTPREM                 <D9604>
                                     + WSAA-COVR-INC                    <D9604>
           END-COMPUTE.                                                 <D9604>
                                                                        <D9604>
           MOVE FPCO-MIN-OVERDUE-PER TO WSAA-OVERDUE-PER.               <D9604>
                                                                        <D9604>
           COMPUTE FPCO-OVERDUE-MIN  ROUNDED =                          <D9604>
                 ( WSAA-OVERDUE-PER                                     <D9604>
                 / 100                                                  <D9604>
                 * (COVRLNB-INSTPREM + WSAA-COVR-INC))                  <D9604>
                 + FPCO-OVERDUE-MIN                                     <D9604>
           END-COMPUTE.                                                 <D9604>
                                                                        <V76F06>
      **** MOVE FPCO-OVERDUE-MIN       TO ZRDP-AMOUNT-IN.               <V76F06>
      **** MOVE PAYR-BILLCURR          TO ZRDP-CURRENCY.                <V76F06>
      **** PERFORM 8000-CALL-ROUNDING.                                  <V76F06>
      **** MOVE ZRDP-AMOUNT-OUT        TO FPCO-OVERDUE-MIN.             <V76F06>
                                                                        <V76F06>
           IF FPCO-OVERDUE-MIN         NOT = 0                          <V76F06>
              MOVE FPCO-OVERDUE-MIN    TO ZRDP-AMOUNT-IN                <V76F06>
              MOVE PAYR-BILLCURR       TO ZRDP-CURRENCY                 <V76F06>
              PERFORM 8000-CALL-ROUNDING                                <V76F06>
              MOVE ZRDP-AMOUNT-OUT     TO FPCO-OVERDUE-MIN              <V76F06>
           END-IF.                                                      <V76F06>
                                                                        <D9604>
      * Write the record back to FPCO file.                     <D9604>
                                                                        <D9604>
      **** MOVE REWRT                  TO FPCO-FUNCTION.        <V65L19><D9604>
           MOVE WRITD                  TO FPCO-FUNCTION.                <V65L19>
                                                                        <D9604>
           CALL 'FPCOIO'               USING FPCO-PARAMS.               <D9604>
           IF FPCO-STATUZ              NOT = O-K                        <D9604>
              MOVE FPCO-PARAMS         TO SYSR-PARAMS                   <D9604>
              MOVE FPCO-STATUZ         TO SYSR-STATUZ                   <D9604>
              PERFORM  600-FATAL-ERROR                                  <D9604>
           END-IF.                                                      <D9604>
                                                                        <D9604>
           MOVE ENDP TO FPCO-STATUZ.                                    <D9604>
                                                                        <D9604>
       7290-EXIT.                                                       <D9604>
           EXIT.                                                        <D9604>
                                                                        <LA2106>
                                                                        <V76F06>
       8000-CALL-ROUNDING SECTION.                                      <V76F06>
      ****************************                                      <V76F06>
       8100-CALL.                                                       <V76F06>
      *                                                                 <V76F06>
           MOVE SPACES                 TO ZRDP-FUNCTION                 <V76F06>
           MOVE BSPR-COMPANY           TO ZRDP-COMPANY.                 <V76F06>
           MOVE O-K                    TO ZRDP-STATUZ.                  <V76F06>
           MOVE BATD-TRCDE             TO ZRDP-BATCTRCDE.               <V76F06>
                                                                        <V76F06>
           CALL 'ZRDECPLC'             USING ZRDP-ZRDECPL-REC.          <V76F06>
                                                                        <V76F06>
           IF  ZRDP-STATUZ             NOT = O-K                        <V76F06>
               MOVE ZRDP-STATUZ        TO SYSR-STATUZ                   <V76F06>
               MOVE ZRDP-ZRDECPL-REC   TO SYSR-PARAMS                   <V76F06>
               PERFORM 600-FATAL-ERROR                                  <V76F06>
           END-IF.                                                      <V76F06>
                                                                        <V76F06>
       8900-EXIT.                                                       <V76F06>
           EXIT.                                                        <V76F06>
      *                                                         <LA2106><LA2106>
       H100-TFR-DVD-SUSP SECTION.                                       <LA2106>
      ***************************                               <LA2106><LA2106>
       H100-PARA.                                                       <LA2106>
      *                                                         <LA2106><LA2106>
      * Determine the shortfall from the premium suspense for billing   <LA2106>
                                                                        <LA2106>
           COMPUTE WSAA-SHORTFALL = CLNK-AMOUNT-OUT - WSAA-PREM-SUSP.   <LA2106>
      *                                                         <LA2106><LA2106>
      * Shortfall < or = zero, no billing generated             <LA2106><LA2106>
                                                                        <LA2106>
           IF  WSAA-SHORTFALL     NOT > 0                               <LA2106>
               GO H190-EXIT                                             <LA2106>
           ELSE                                                         <LA2106>
      *                                                         <LA2106><LA2106>
      * Shortfall > dividend suspense, transfer from dividend suspense  <LA2106>
      * Note that with tolerance limit, even the shortfall > dvd-susp,  <LA2106>
      * there is a chance no billing will be necessary.         <LA2106><LA2106>
                                                                        <LA2106>
               IF  WSAA-SHORTFALL     > WSAA-DVD-SUSP                   <LA2106>
                   MOVE WSAA-DVD-SUSP TO WSAA-TFR-AMT                   <LA2106>
               ELSE                                                     <LA2106>
      *                                                         <LA2106><LA2106>
      * Dividend suspense > shortfall, transfer enough for dvd-susp     <LA2106>
      * no billing required.                                    <LA2106><LA2106>
                                                                        <LA2106>
                   MOVE WSAA-SHORTFALL TO WSAA-TFR-AMT                  <LA2106>
               END-IF                                                   <LA2106>
           END-IF.                                                      <LA2106>
      *                                                         <LA2106><LA2106>
      * Set up common fields in LIFA once only                  <LA2106><LA2106>
                                                                        <LA2106>
           MOVE 0                      TO WSAA-JRNSEQ.                  <LA2106>
           MOVE BATD-COMPANY           TO LIFA-BATCCOY.                 <LA2106>
           MOVE BATD-BRANCH            TO LIFA-BATCBRN.                 <LA2106>
           MOVE BATD-ACTYEAR           TO LIFA-BATCACTYR.               <LA2106>
           MOVE BATD-ACTMONTH          TO LIFA-BATCACTMN.               <LA2106>
           MOVE BATD-TRCDE             TO LIFA-BATCTRCDE.               <LA2106>
           MOVE BATD-BATCH             TO LIFA-BATCBATCH.               <LA2106>
           MOVE BATD-COMPANY           TO LIFA-RLDGCOY,                 <LA2106>
                                          LIFA-GENLCOY.                 <LA2106>
           MOVE SPACES                 TO LIFA-POSTYEAR                 <LA2106>
                                          LIFA-POSTMONTH.               <LA2106>
                                                                        <LA2106>
           MOVE CHDRLIF-CHDRNUM        TO LIFA-RDOCNUM                  <LA2106>
                                          LIFA-RLDGACCT                 <LA2106>
                                          LIFA-TRANREF.                 <LA2106>
           MOVE CHDRLIF-TRANNO         TO LIFA-TRANNO.                  <LA2106>
           MOVE PAYR-BILLCD            TO LIFA-EFFDATE.                 <LA2106>
           MOVE VRCM-MAX-DATE          TO LIFA-FRCDATE.                 <LA2106>
           MOVE ZERO                   TO LIFA-CRATE                    <LA2106>
                                          LIFA-ACCTAMT                  <LA2106>
                                          LIFA-CONTOT                   <LA2106>
                                          LIFA-RCAMT.                   <LA2106>
           MOVE SPACES                 TO LIFA-TERMID.                  <LA2106>
           MOVE BSSC-EFFECTIVE-DATE    TO LIFA-TRANSACTION-DATE.        <LA2106>
           MOVE VRCM-TIME              TO LIFA-TRANSACTION-TIME.        <LA2106>
           MOVE ZERO                   TO LIFA-USER.                    <LA2106>
                                                                        <LA2106>
           MOVE 'Dividend Transfer'    TO LIFA-TRANDESC.                <LA2106>
           MOVE CHDRLIF-CNTTYPE        TO LIFA-SUBSTITUTE-CODE(1).      <LA2106>
                                                                        <LA2106>
      *  Compare contract currency to billing and post accordingly.     <LA2106>
                                                                        <LA2106>
           IF CHDRLIF-CNTCURR      NOT = CHDRLIF-BILLCURR               <LA2106>
               MOVE SPACES             TO CLNK-STATUZ                   <LA2106>
               MOVE CHDRLIF-BILLCURR   TO CLNK-CURR-IN                  <LA2106>
               MOVE WSAA-TFR-AMT       TO CLNK-AMOUNT-IN                <LA2106>
               MOVE VRCM-MAX-DATE      TO CLNK-CASHDATE                 <LA2106>
               MOVE CHDRLIF-CNTCURR    TO CLNK-CURR-OUT                 <LA2106>
               MOVE CHDRLIF-CHDRCOY    TO CLNK-COMPANY                  <LA2106>
               MOVE ZEROES             TO CLNK-AMOUNT-OUT               <LA2106>
               MOVE 'REAL'             TO CLNK-FUNCTION                 <LA2106>
               CALL 'XCVRT'  USING  CLNK-CLNK002-REC                    <LA2106>
               IF CLNK-STATUZ              NOT = '****'                 <LA2106>
                   MOVE CLNK-CLNK002-REC   TO SYSR-PARAMS               <LA2106>
                   MOVE CLNK-STATUZ        TO SYSR-STATUZ               <LA2106>
                   PERFORM 600-FATAL-ERROR                              <LA2106>
               END-IF                                                   <LA2106>
                                                                        <V76F06>
               IF  CLNK-AMOUNT-OUT         NOT = 0                      <V76F06>
               MOVE CLNK-AMOUNT-OUT        TO ZRDP-AMOUNT-IN            <V76F06>
               MOVE CHDRLIF-CNTCURR        TO ZRDP-CURRENCY             <V76F06>
               PERFORM 8000-CALL-ROUNDING                               <V76F06>
               MOVE ZRDP-AMOUNT-OUT        TO CLNK-AMOUNT-OUT           <V76F06>
               END-IF                                                   <V76F06>
      *                                                         <LA2106><LA2106>
      * Transfer out Dividend Suspense                          <LA2106><LA2106>
                                                                        <LA2106>
               MOVE CLNK-AMOUNT-OUT    TO LIFA-ORIGAMT                  <LA2106>
               MOVE CHDRLIF-CNTCURR    TO LIFA-ORIGCURR                 <LA2106>
               MOVE T5645-SACSCODE-03  TO LIFA-SACSCODE                 <LA2106>
               MOVE T5645-SACSTYPE-03  TO LIFA-SACSTYP                  <LA2106>
               MOVE T5645-GLMAP-03     TO LIFA-GLCODE                   <LA2106>
               MOVE T5645-SIGN-03      TO LIFA-GLSIGN                   <LA2106>
               MOVE T5645-CNTTOT-03    TO LIFA-CONTOT                   <LA2106>
               PERFORM H200-POST-ACMV-RECORD                            <LA2106>
      *                                                         <LA2106><LA2106>
      * Transfer Dividend into Currency Exchange Account        <LA2106><LA2106>
                                                                        <LA2106>
               MOVE T5645-SACSCODE-04  TO LIFA-SACSCODE                 <LA2106>
               MOVE T5645-SACSTYPE-04  TO LIFA-SACSTYP                  <LA2106>
               MOVE T5645-GLMAP-04     TO LIFA-GLCODE                   <LA2106>
               MOVE T5645-SIGN-04      TO LIFA-GLSIGN                   <LA2106>
               MOVE T5645-CNTTOT-04    TO LIFA-CONTOT                   <LA2106>
               PERFORM H200-POST-ACMV-RECORD                            <LA2106>
      *                                                         <LA2106><LA2106>
      * Transfer out Dividend Suspense from Currency Exchange Account   <LA2106>
                                                                        <LA2106>
               MOVE WSAA-TFR-AMT       TO LIFA-ORIGAMT                  <LA2106>
               MOVE CHDRLIF-BILLCURR   TO LIFA-ORIGCURR                 <LA2106>
               MOVE T5645-SACSCODE-05  TO LIFA-SACSCODE                 <LA2106>
               MOVE T5645-SACSTYPE-05  TO LIFA-SACSTYP                  <LA2106>
               MOVE T5645-GLMAP-05     TO LIFA-GLCODE                   <LA2106>
               MOVE T5645-SIGN-05      TO LIFA-GLSIGN                   <LA2106>
               MOVE T5645-CNTTOT-05    TO LIFA-CONTOT                   <LA2106>
               PERFORM H200-POST-ACMV-RECORD                            <LA2106>
      *                                                         <LA2106><LA2106>
      * Transfer Dividend Suspense into Premium Suspense        <LA2106><LA2106>
                                                                        <LA2106>
               MOVE T5645-SACSCODE-01  TO LIFA-SACSCODE                 <LA2106>
               MOVE T5645-SACSTYPE-01  TO LIFA-SACSTYP                  <LA2106>
               MOVE T5645-GLMAP-01     TO LIFA-GLCODE                   <LA2106>
               MOVE T5645-SIGN-01      TO LIFA-GLSIGN                   <LA2106>
               MOVE T5645-CNTTOT-01    TO LIFA-CONTOT                   <LA2106>
               PERFORM H200-POST-ACMV-RECORD                            <LA2106>
      *                                                         <LA2106><LA2106>
      * Set WSAA-TFR-AMT to converted amount in contract currency       <LA2106>
                                                                        <LA2106>
               MOVE CLNK-AMOUNT-OUT    TO WSAA-TFR-AMT                  <LA2106>
                                                                        <LA2106>
           ELSE                                                         <LA2106>
                                                                        <LA2106>
      *                                                         <LA2106><LA2106>
      * Transfer out Dividend Suspense                          <LA2106><LA2106>
                                                                        <LA2106>
               MOVE WSAA-TFR-AMT       TO LIFA-ORIGAMT                  <LA2106>
               MOVE CHDRLIF-CNTCURR    TO LIFA-ORIGCURR                 <LA2106>
               MOVE T5645-SACSCODE-03  TO LIFA-SACSCODE                 <LA2106>
               MOVE T5645-SACSTYPE-03  TO LIFA-SACSTYP                  <LA2106>
               MOVE T5645-GLMAP-03     TO LIFA-GLCODE                   <LA2106>
               MOVE T5645-SIGN-03      TO LIFA-GLSIGN                   <LA2106>
               MOVE T5645-CNTTOT-03    TO LIFA-CONTOT                   <LA2106>
               PERFORM H200-POST-ACMV-RECORD                            <LA2106>
      *                                                         <LA2106><LA2106>
      * Transfer Dividend Suspense into Premium Suspense        <LA2106><LA2106>
                                                                        <LA2106>
               MOVE T5645-SACSCODE-01  TO LIFA-SACSCODE                 <LA2106>
               MOVE T5645-SACSTYPE-01  TO LIFA-SACSTYP                  <LA2106>
               MOVE T5645-GLMAP-01     TO LIFA-GLCODE                   <LA2106>
               MOVE T5645-SIGN-01      TO LIFA-GLSIGN                   <LA2106>
               MOVE T5645-CNTTOT-01    TO LIFA-CONTOT                   <LA2106>
               PERFORM H200-POST-ACMV-RECORD                            <LA2106>
                                                                        <LA2106>
           END-IF.                                                      <LA2106>
                                                                        <LA2106>
       H120-WRITE-HDIV.                                                 <LA2106>
      *                                                         <LA2106><LA2106>
      * Now write HDIV to denote the withdrawal of dividend at coverage <LA2106>
      * level.  So spread the amount across all the coverages according <LA2106>
      * to their share.                                         <LA2106><LA2106>
                                                                        <LA2106>
           INITIALIZE WSAA-HDIS-ARRAY.                                  <LA2106>
           MOVE SPACES                 TO HDIS-PARAMS.                  <LA2106>
           MOVE CHDRLIF-CHDRCOY        TO HDIS-CHDRCOY.                 <LA2106>
           MOVE CHDRLIF-CHDRNUM        TO HDIS-CHDRNUM.                 <LA2106>
           MOVE ZEROES                 TO HDIS-PLAN-SUFFIX.             <LA2106>
           MOVE HDISREC                TO HDIS-FORMAT.                  <LA2106>
                                                                        <LA2106>
           MOVE BEGN                   TO HDIS-FUNCTION.                <LA2106>
           CALL 'HDISIO'  USING  HDIS-PARAMS.                           <LA2106>
           IF  HDIS-STATUZ             NOT = O-K                        <LA2106>
           AND                         NOT = ENDP                       <LA2106>
               MOVE HDIS-PARAMS        TO SYSR-PARAMS                   <LA2106>
               MOVE HDIS-STATUZ        TO SYSR-STATUZ                   <LA2106>
               PERFORM 600-FATAL-ERROR                                  <LA2106>
           END-IF.                                                      <LA2106>
                                                                        <LA2106>
           IF HDIS-CHDRCOY         NOT = CHDRLIF-CHDRCOY                <LA2106>
           OR HDIS-CHDRNUM         NOT = CHDRLIF-CHDRNUM                <LA2106>
              MOVE ENDP                TO HDIS-STATUZ                   <LA2106>
           END-IF.                                                      <LA2106>
                                                                        <LA2106>
           MOVE ZEROES                 TO WSAA-IDX                      <LA2106>
                                          WSAA-ALL-DVD-TOT.             <LA2106>
           PERFORM H300-PROCESS-HDIS                                    <LA2106>
               UNTIL HDIS-STATUZ       = ENDP.                          <LA2106>
      *                                                         <LA2106><LA2106>
      * Set total no. of HDIS read                              <LA2106><LA2106>
                                                                        <LA2106>
           MOVE WSAA-IDX               TO WSAA-NO-OF-HDIS.              <LA2106>
      *                                                         <LA2106><LA2106>
      * Then calculate share of each coverage on the dividend   <LA2106><LA2106>
                                                                        <LA2106>
           MOVE ZEROES                 TO WSAA-IDX.                     <LA2106>
           PERFORM                                                      <LA2106>
               UNTIL WSAA-IDX      NOT < WSAA-NO-OF-HDIS                <LA2106>
                                                                        <LA2106>
               ADD 1                   TO WSAA-IDX                      <LA2106>
               IF  WSAA-IDX            = WSAA-NO-OF-HDIS                <LA2106>
                   COMPUTE WSAA-DVD-SHARE(WSAA-IDX) =                   <LA2106>
                                       WSAA-TFR-AMT - WSAA-RUN-DVD-TOT  <LA2106>
               ELSE                                                     <LA2106>
                   COMPUTE WSAA-DVD-SHARE(WSAA-IDX) = WSAA-TFR-AMT *    <LA2106>
                       (WSAA-DVD-TOT(WSAA-IDX) / WSAA-ALL-DVD-TOT)      <LA2106>
                   ADD WSAA-DVD-SHARE(WSAA-IDX) TO WSAA-RUN-DVD-TOT     <LA2106>
               END-IF                                                   <LA2106>
                                                                        <V76F06>
               IF WSAA-DVD-SHARE(WSAA-IDX) NOT = 0                      <V76F06>
               MOVE WSAA-DVD-SHARE(WSAA-IDX) TO ZRDP-AMOUNT-IN          <V76F06>
               MOVE CHDRLIF-CNTCURR        TO ZRDP-CURRENCY             <V76F06>
               PERFORM 8000-CALL-ROUNDING                               <V76F06>
               MOVE ZRDP-AMOUNT-OUT        TO WSAA-DVD-SHARE(WSAA-IDX)  <V76F06>
               END-IF                                                   <V76F06>
                                                                        <LA2106>
           END-PERFORM.                                                 <LA2106>
      *                                                         <LA2106><LA2106>
      * Write withdrawn amount for each coverage                <LA2106><LA2106>
                                                                        <LA2106>
           MOVE ZEROES                 TO WSAA-IDX.                     <LA2106>
           PERFORM                                                      <LA2106>
               UNTIL WSAA-IDX          NOT < WSAA-NO-OF-HDIS            <LA2106>
                                                                        <LA2106>
               ADD 1                   TO WSAA-IDX                      <LA2106>
               INITIALIZE HDIV-DATA-AREA                                <LA2106>
               MOVE CHDRLIF-CHDRCOY    TO HDIV-CHDRCOY                  <LA2106>
               MOVE CHDRLIF-CHDRNUM    TO HDIV-CHDRNUM                  <LA2106>
               MOVE WSAA-HDIS-LIFE(WSAA-IDX)                            <LA2106>
                                       TO HDIV-LIFE                     <LA2106>
               MOVE WSAA-HDIS-JLIFE(WSAA-IDX)                           <LA2106>
                                       TO HDIV-JLIFE                    <LA2106>
               MOVE WSAA-HDIS-COVERAGE(WSAA-IDX)                        <LA2106>
                                       TO HDIV-COVERAGE                 <LA2106>
               MOVE WSAA-HDIS-RIDER(WSAA-IDX)                           <LA2106>
                                       TO HDIV-RIDER                    <LA2106>
               MOVE WSAA-HDIS-PLNSFX(WSAA-IDX)                          <LA2106>
                                       TO HDIV-PLAN-SUFFIX              <LA2106>
               MOVE CHDRLIF-TRANNO     TO HDIV-TRANNO                   <LA2106>
      *******  MOVE PAYR-BILLCD        TO HDIV-EFFDATE         <LFA1034><LA2106>
      ********                            HDIV-DIVD-ALLOC-DATE <LFA1034><LA2106>
               MOVE PAYR-PTDATE        TO HDIV-EFFDATE                  <LFA1034
                                          HDIV-DIVD-ALLOC-DATE          <LFA1034
               MOVE WSAA-NEXT-CAP-DATE(WSAA-IDX)                        <LA2106>
                                       TO HDIV-DIVD-INT-CAP-DATE        <LA2106>
               MOVE CHDRLIF-CNTCURR    TO HDIV-CNTCURR                  <LA2106>
               COMPUTE HDIV-DIVD-AMOUNT = WSAA-DVD-SHARE(WSAA-IDX)      <LA2106>
                                          * -1                          <LA2106>
               MOVE ZEROES             TO HDIV-DIVD-RATE                <LA2106>
               MOVE VRCM-MAX-DATE      TO HDIV-DIVD-RT-EFFDT            <LA2106>
               MOVE BATD-COMPANY       TO HDIV-BATCCOY                  <LA2106>
               MOVE BATD-BRANCH        TO HDIV-BATCBRN                  <LA2106>
               MOVE BATD-ACTYEAR       TO HDIV-BATCACTYR                <LA2106>
               MOVE BATD-ACTMONTH      TO HDIV-BATCACTMN                <LA2106>
               MOVE BATD-TRCDE         TO HDIV-BATCTRCDE                <LA2106>
               MOVE BATD-BATCH         TO HDIV-BATCBATCH                <LA2106>
               MOVE 'C'                TO HDIV-DIVD-TYPE                <LA2106>
               MOVE WSAA-ZDIVOPT(WSAA-IDX) TO HDIV-ZDIVOPT              <LA2106>
               MOVE WSAA-ZCSHDIVMTH(WSAA-IDX) TO HDIV-ZCSHDIVMTH        <LA2106>
               MOVE ZEROES             TO HDIV-DIVD-OPTPROC-TRANNO      <LA2106>
                                          HDIV-DIVD-CAP-TRANNO          <LA2106>
                                          HDIV-DIVD-STMT-NO             <LA2106>
                                          HDIV-PU-ADD-NBR               <LA2106>
               MOVE HDIVREC            TO HDIV-FORMAT                   <LA2106>
                                                                        <LA2106>
               MOVE WRITR              TO HDIV-FUNCTION                 <LA2106>
               CALL 'HDIVIO'  USING HDIV-PARAMS                         <LA2106>
               IF  HDIV-STATUZ     NOT = O-K                            <LA2106>
                   MOVE HDIV-PARAMS    TO SYSR-PARAMS                   <LA2106>
                   MOVE HDIV-STATUZ    TO SYSR-STATUZ                   <LA2106>
                   PERFORM 600-FATAL-ERROR                              <LA2106>
               END-IF                                                   <LA2106>
                                                                        <LA2106>
           END-PERFORM.                                                 <LA2106>
                                                                        <LA2106>
       H190-EXIT.                                                       <LA2106>
           EXIT.                                                        <LA2106>
                                                                        <LA2106>
      *                                                         <LA2106><LA2106>
       H200-POST-ACMV-RECORD SECTION.                                   <LA2106>
      *******************************                           <LA2106><LA2106>
       H210-POST.                                                       <LA2106>
                                                                        <LA2106>
           IF  LIFA-ORIGAMT            = ZERO                           <LA2106>
               GO TO H290-EXIT                                          <LA2106>
           END-IF.                                                      <LA2106>
                                                                        <LA2106>
           MOVE 'PSTW'                 TO LIFA-FUNCTION.                <LA2106>
                                                                        <LA2106>
           ADD 1                       TO WSAA-JRNSEQ.                  <LA2106>
           MOVE WSAA-JRNSEQ            TO LIFA-JRNSEQ.                  <LA2106>
                                                                        <LA2106>
           CALL 'LIFACMV'              USING LIFA-LIFACMV-REC.          <LA2106>
                                                                        <LA2106>
           IF LIFA-STATUZ              NOT = O-K                        <LA2106>
              MOVE LIFA-LIFACMV-REC    TO SYSR-PARAMS                   <LA2106>
              MOVE LIFA-STATUZ         TO SYSR-STATUZ                   <LA2106>
              PERFORM 600-FATAL-ERROR                                   <LA2106>
           END-IF.                                                      <LA2106>
                                                                        <LA2106>
       H290-EXIT.                                                       <LA2106>
           EXIT.                                                        <LA2106>
      *                                                         <LA2106><LA2106>
       H300-PROCESS-HDIS SECTION.                                       <LA2106>
      ***************************                               <LA2106><LA2106>
       H300-HDIS.                                                       <LA2106>
      *                                                         <LA2106><LA2106>
      * Skip validflag not '1'                                  <LA2106><LA2106>
                                                                        <LA2106>
           If  HDIS-VALIDFLAG      NOT = '1'                            <LA2106>
               GO H320-NEXT-HDIS                                        <LA2106>
           END-IF.                                                      <LA2106>
                                                                        <LA2106>
           ADD 1                       TO WSAA-IDX.                     <LA2106>
           MOVE HDIS-LIFE              TO WSAA-HDIS-LIFE(WSAA-IDX).     <LA2106>
           MOVE HDIS-COVERAGE          TO WSAA-HDIS-COVERAGE(WSAA-IDX). <LA2106>
           MOVE HDIS-RIDER             TO WSAA-HDIS-RIDER(WSAA-IDX).    <LA2106>
           MOVE HDIS-PLAN-SUFFIX       TO WSAA-HDIS-PLNSFX(WSAA-IDX).   <LA2106>
           MOVE HDIS-JLIFE             TO WSAA-HDIS-JLIFE(WSAA-IDX).    <LA2106>
           MOVE HDIS-NEXT-CAP-DATE     TO WSAA-NEXT-CAP-DATE(WSAA-IDX). <LA2106>
                                                                        <LA2106>
           MOVE HDIS-DATA-KEY          TO HCSD-DATA-KEY.                <LA2106>
           MOVE HCSDREC                TO HCSD-FORMAT.                  <LA2106>
           MOVE READR                  TO HCSD-FUNCTION.                <LA2106>
           CALL 'HCSDIO'  USING  HCSD-PARAMS.                           <LA2106>
           IF  HCSD-STATUZ         NOT = O-K                            <LA2106>
               MOVE HCSD-PARAMS        TO SYSR-PARAMS                   <LA2106>
               MOVE HCSD-STATUZ        TO SYSR-STATUZ                   <LA2106>
               PERFORM 600-FATAL-ERROR                                  <LA2106>
           END-IF.                                                      <LA2106>
                                                                        <LA2106>
           MOVE HCSD-ZDIVOPT           TO WSAA-ZDIVOPT(WSAA-IDX).       <LA2106>
           MOVE HCSD-ZCSHDIVMTH        TO WSAA-ZCSHDIVMTH(WSAA-IDX).    <LA2106>
                                                                        <LA2106>
           ADD  HDIS-BAL-SINCE-LAST-CAP TO WSAA-DVD-TOT(WSAA-IDX)       <LA2106>
                                           WSAA-ALL-DVD-TOT.            <LA2106>
                                                                        <LA2106>
      *    Read through HDIVCSH from last interest date of HDIS,<LA2106><LA2106>
      *    calculate interest on the withdrawn dividend, accumulate     <LA2106>
      *    to the total dividend of the coverage                <LA2106><LA2106>
                                                                        <LA2106>
           MOVE HDIS-CHDRCOY           TO HDIVCSH-CHDRCOY.              <LA2106>
           MOVE HDIS-CHDRNUM           TO HDIVCSH-CHDRNUM.              <LA2106>
           MOVE HDIS-LIFE              TO HDIVCSH-LIFE.                 <LA2106>
           MOVE HDIS-COVERAGE          TO HDIVCSH-COVERAGE.             <LA2106>
           MOVE HDIS-RIDER             TO HDIVCSH-RIDER.                <LA2106>
           MOVE HDIS-PLAN-SUFFIX       TO HDIVCSH-PLAN-SUFFIX.          <LA2106>
           ADD 1, HDIS-LAST-CAP-DATE   GIVING WSAA-LAST-CAP-DATE.       <LA2106>
           MOVE WSAA-LAST-CAP-DATE     TO HDIVCSH-DIVD-INT-CAP-DATE.    <LA2106>
           MOVE BEGN                   TO HDIVCSH-FUNCTION.             <LA2106>
           CALL 'HDIVCSHIO'  USING  HDIVCSH-PARAMS.                     <LA2106>
                                                                        <LA2106>
           IF  HDIVCSH-STATUZ          NOT = O-K                        <LA2106>
           AND                         NOT = ENDP                       <LA2106>
               MOVE HDIVCSH-PARAMS     TO SYSR-PARAMS                   <LA2106>
               MOVE HDIVCSH-STATUZ     TO SYSR-STATUZ                   <LA2106>
           END-IF.                                                      <LA2106>
                                                                        <LA2106>
           IF  HDIVCSH-CHDRCOY         NOT = HDIS-CHDRCOY               <LA2106>
           OR  HDIVCSH-CHDRNUM         NOT = HDIS-CHDRNUM               <LA2106>
           OR  HDIVCSH-LIFE            NOT = HDIS-LIFE                  <LA2106>
           OR  HDIVCSH-COVERAGE        NOT = HDIS-COVERAGE              <LA2106>
           OR  HDIVCSH-RIDER           NOT = HDIS-RIDER                 <LA2106>
           OR  HDIVCSH-PLAN-SUFFIX     NOT = HDIS-PLAN-SUFFIX           <LA2106>
               MOVE ENDP               TO HDIVCSH-STATUZ                <LA2106>
           END-IF.                                                      <LA2106>
                                                                        <LA2106>
           PERFORM                                                      <LA2106>
               UNTIL HDIVCSH-STATUZ = ENDP                              <LA2106>
                                                                        <LA2106>
               ADD HDIVCSH-DIVD-AMOUNT TO WSAA-DVD-TOT(WSAA-IDX)        <LA2106>
                                                                        <LA2106>
               MOVE NEXTR              TO HDIVCSH-FUNCTION              <LA2106>
               CALL 'HDIVCSHIO'  USING  HDIVCSH-PARAMS                  <LA2106>
                                                                        <LA2106>
               IF  HDIVCSH-STATUZ      NOT = O-K                        <LA2106>
               AND                     NOT = ENDP                       <LA2106>
                   MOVE HDIVCSH-PARAMS TO SYSR-PARAMS                   <LA2106>
                   MOVE HDIVCSH-STATUZ TO SYSR-STATUZ                   <LA2106>
               END-IF                                                   <LA2106>
                                                                        <LA2106>
               IF  HDIVCSH-CHDRCOY         NOT = HDIS-CHDRCOY           <LA2106>
               OR  HDIVCSH-CHDRNUM         NOT = HDIS-CHDRNUM           <LA2106>
               OR  HDIVCSH-LIFE            NOT = HDIS-LIFE              <LA2106>
               OR  HDIVCSH-COVERAGE        NOT = HDIS-COVERAGE          <LA2106>
               OR  HDIVCSH-RIDER           NOT = HDIS-RIDER             <LA2106>
               OR  HDIVCSH-PLAN-SUFFIX     NOT = HDIS-PLAN-SUFFIX       <LA2106>
                   MOVE ENDP               TO HDIVCSH-STATUZ            <LA2106>
               END-IF                                                   <LA2106>
                                                                        <LA2106>
           END-PERFORM.                                                 <LA2106>
                                                                        <LA2106>
       H320-NEXT-HDIS.                                                  <LA2106>
                                                                        <LA2106>
           MOVE NEXTR                  TO HDIS-FUNCTION.                <LA2106>
           CALL 'HDISIO'     USING  HDIS-PARAMS.                        <LA2106>
                                                                        <LA2106>
           IF  HDIS-STATUZ         NOT = O-K                            <LA2106>
           AND                     NOT = ENDP                           <LA2106>
               MOVE HDIS-PARAMS        TO SYSR-PARAMS                   <LA2106>
               MOVE HDIS-STATUZ        TO SYSR-STATUZ                   <LA2106>
           END-IF.                                                      <LA2106>
                                                                        <LA2106>
           IF  HDIS-CHDRCOY        NOT = CHDRLIF-CHDRCOY                <LA2106>
           OR  HDIS-CHDRNUM        NOT = CHDRLIF-CHDRNUM                <LA2106>
               MOVE ENDP               TO HDIS-STATUZ                   <LA2106>
           END-IF.                                                      <LA2106>
                                                                        <LA2106>
       H390-EXIT.                                                       <LA2106>
           EXIT.                                                        <LA2106>
                                                                        <LA5134>
       A1000-GET-CURR-CONV-DATE SECTION.                                <LA5134>
      **********************************                                <LA5134>
       A1010-START.                                                     <LA5134>
                                                                        <LA5134>
      * If Contract and Billing currency is not the same :              <LA5134>
      * Subtract the lead days from the current BILLCD to get a date    <LA5134>
      * for currency conversion. This date will be used when calling    <LA5134>
      * XCVRT subroutine. For normal case this date = BSSC-EFF-DATE.    <LA5134>
                                                                        <LA5134>
           MOVE VRCM-MAX-DATE          TO WSAA-CASHDATE.                <LA5134>
                                                                        <LA5134>
           IF PAYR-CNTCURR              = PAYR-BILLCURR                 <LA5134>
              GO TO A1090-EXIT                                          <LA5134>
           END-IF.                                                      <LA5134>
                                                                        <LA5134>
           COMPUTE DTC2-FREQ-FACTOR     =                               <LA5134>
                         ( ZERO - WSAA-T6654-LEADDAY (WSAA-T6654-IX)).  <LA5134>
           MOVE 'DY'                   TO DTC2-FREQUENCY.               <LA5134>
           MOVE PAYR-BILLCD            TO DTC2-INT-DATE-1.              <LA5134>
           CALL 'DATCON2'           USING DTC2-DATCON2-REC.             <LA5134>
                                                                        <LA5134>
           IF DTC2-STATUZ           NOT = O-K                           <LA5134>
              MOVE DTC2-DATCON2-REC    TO SYSR-PARAMS                   <LA5134>
              MOVE DTC2-STATUZ         TO SYSR-STATUZ                   <LA5134>
              PERFORM 600-FATAL-ERROR                                   <LA5134>
           END-IF.                                                      <LA5134>
                                                                        <LA5134>
           MOVE DTC2-INT-DATE-2        TO WSAA-CASHDATE.                <LA5134>
                                                                        <LA5134>
           IF WSAA-CASHDATE             < CHDRLIF-OCCDATE               <LA5134>
              MOVE CHDRLIF-OCCDATE     TO WSAA-CASHDATE                 <LA5134>
           END-IF.                                                      <LA5134>
                                                                        <LA5134>
       A1090-EXIT.                                                      <LA5134>
           EXIT.                                                        <LA5134>
                                                                        <V74L01>
       A1100-READ-TR52E SECTION.                                        <V74L01>
      **************************                                        <V74L01>
       A1110-START.                                                     <V74L01>
           MOVE SPACES                 TO ITDM-DATA-AREA                <V74L01>
                                          TR52E-TR52E-REC.              <V74L01>
           MOVE BSPR-COMPANY           TO ITDM-ITEMCOY.                 <V74L01>
           MOVE TR52E                  TO ITDM-ITEMTABL.                <V74L01>
           MOVE WSAA-TR52E-KEY         TO ITDM-ITEMITEM.                <V74L01>
           MOVE WSAA-OLD-BTDATE        TO ITDM-ITMFRM.                  <V74L01>
           MOVE BEGN                   TO ITDM-FUNCTION.                <V74L01>
                                                                        <V74L01>
           CALL 'ITDMIO'               USING ITDM-PARAMS.               <V74L01>
                                                                        <V74L01>
           IF (ITDM-STATUZ             NOT = O-K) AND                   <V74L01>
              (ITDM-STATUZ             NOT = ENDP)                      <V74L01>
              MOVE ITDM-PARAMS         TO SYSR-PARAMS                   <V74L01>
              MOVE ITDM-STATUZ         TO SYSR-STATUZ                   <V74L01>
              PERFORM 600-FATAL-ERROR                                   <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF ((ITDM-ITEMCOY          NOT = BSPR-COMPANY)    OR         <V74L01>
               (ITDM-ITEMTABL         NOT = TR52E)           OR         <V74L01>
               (ITDM-ITEMITEM         NOT  = WSAA-TR52E-KEY) OR         <V74L01>
               (ITDM-STATUZ           = ENDP))               AND        <V74L01>
               (WSAA-TR52E-KEY(2:7)   = '*******')                      <V74L01>
              MOVE WSAA-TR52E-KEY     TO SYSR-PARAMS                    <V74L01>
              MOVE ITDM-STATUZ        TO SYSR-STATUZ                    <V74L01>
              PERFORM 600-FATAL-ERROR                                   <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF ((ITDM-ITEMCOY           = BSPR-COMPANY) AND              <V74L01>
               (ITDM-ITEMTABL          = TR52E)        AND              <V74L01>
               (ITDM-ITEMITEM          = WSAA-TR52E-KEY) AND            <V74L01>
               (ITDM-STATUZ            NOT = ENDP))                     <V74L01>
               MOVE ITDM-GENAREA       TO TR52E-TR52E-REC               <V74L01>
            END-IF.                                                     <V74L01>
                                                                        <V74L01>
       A1190-EXIT.                                                      <V74L01>
           EXIT.                                                        <V74L01>
      /                                                                 <UL005>
       A1200-LOAD-TV103 SECTION.                                        <UL005>
      **************************                                        <UL005>
       A1210-START.                                                     <UL005>
                                                                        <UL005>
           CALL 'ITEMIO'            USING ITEM-PARAMS.                  <UL005>
                                                                        <UL005>
           IF  ITEM-STATUZ         NOT = O-K                            <UL005>
           AND ITEM-STATUZ         NOT = ENDP                           <UL005>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <UL005>
               PERFORM 600-FATAL-ERROR                                  <UL005>
           END-IF.                                                      <UL005>
                                                                        <UL005>
           IF ITEM-STATUZ              = ENDP                           <UL005>
           OR ITEM-ITEMCOY       NOT   = BSPR-COMPANY                   <UL005>
           OR ITEM-ITEMTABL      NOT   = TV103                          <UL005>
               MOVE ENDP               TO ITEM-STATUZ                   <UL005>
               GO TO A1290-EXIT                                         <UL005>
           END-IF.                                                      <UL005>
                                                                        <UL005>
           IF WSAA-TV103-IX > WSAA-TV103-SIZE                           <UL005>
               MOVE H791               TO SYSR-STATUZ                   <UL005>
               MOVE TV103              TO SYSR-PARAMS                   <UL005>
               PERFORM 600-FATAL-ERROR                                  <UL005>
           END-IF.                                                      <UL005>
                                                                        <UL005>
           MOVE ITEM-ITEMITEM          TO                               <UL005>
                                 WSAA-TV103-CNTTYPE (WSAA-TV103-IX).    <UL005>
                                                                        <UL005>
           MOVE NEXTR                  TO ITEM-FUNCTION.                <UL005>
                                                                        <UL005>
           SET WSAA-TV103-IX UP BY 1.                                   <UL005>
      *                                                                 <UL005>
       A1290-EXIT.                                                      <UL005>
           EXIT.                                                        <UL005>
      /                                                                 <UL005>
       A1300-READ-COVR SECTION.                                         <UL005>
      *************************                                         <UL005>
      *                                                                 <UL005>
       A1310-START.                                                     <UL005>
      *                                                                 <UL005>
      *    Read COVR to get basic premium.                              <UL005>
      *                                                                 <UL005>
           INITIALIZE                     COVR-PARAMS.                  <UL005>
           MOVE CHDRLIF-CHDRCOY        TO COVR-CHDRCOY.                 <UL005>
           MOVE CHDRLIF-CHDRNUM        TO COVR-CHDRNUM.                 <UL005>
           MOVE '01'                   TO COVR-LIFE.                    <UL005>
           MOVE '01'                   TO COVR-COVERAGE.                <UL005>
           MOVE '00'                   TO COVR-RIDER.                   <UL005>
           MOVE ZEROS                  TO COVR-PLAN-SUFFIX.             <UL005>
           MOVE READR                  TO COVR-FUNCTION.                <UL005>
           MOVE COVRREC                TO COVR-FORMAT.                  <UL005>
           MOVE O-K                    TO COVR-STATUZ.                  <UL005>
      *                                                                 <UL005>
           CALL 'COVRIO'            USING COVR-PARAMS.                  <UL005>
      *                                                                 <UL005>
           IF  COVR-STATUZ          NOT = O-K                           <UL005>
               MOVE COVR-PARAMS        TO SYSR-PARAMS                   <UL005>
               MOVE COVR-STATUZ        TO SYSR-STATUZ                   <UL005>
               PERFORM 600-FATAL-ERROR                                  <UL005>
           END-IF.                                                      <UL005>
      *                                                                 <UL005>
       A1390-EXIT.                                                      <UL005>
           EXIT.                                                        <UL005>
      /                                                                 <UL005>
       A1400-COUNT-COMPONENTS SECTION.                                  <UL005>
      ********************************                                  <UL005>
      *                                                                 <UL005>
       A1410-START.                                                     <UL005>
      *                                                                 <UL005>
      *  Read COVR and count component.                                 <UL005>
      *                                                                 <UL005>
           MOVE ZEROS                  TO WSAA-COVR-COUNT.              <UL005>
           INITIALIZE                     COVR-PARAMS.                  <UL005>
           MOVE CHDRLIF-CHDRCOY        TO COVR-CHDRCOY.                 <UL005>
           MOVE CHDRLIF-CHDRNUM        TO COVR-CHDRNUM.                 <UL005>
           MOVE ZEROES                 TO COVR-PLAN-SUFFIX.             <UL005>
           MOVE COVRREC                TO COVR-FORMAT.                  <UL005>
           MOVE BEGN                   TO COVR-FUNCTION.                <UL005>
                                                                        <UL005>
           PERFORM A1500-LOOP-COVR                                      <UL005>
                UNTIL COVR-STATUZ       = ENDP.                         <UL005>
      *                                                                 <UL005>
       A1490-EXIT.                                                      <UL005>
           EXIT.                                                        <UL005>
      /                                                                 <UL005>
       A1500-LOOP-COVR SECTION.                                         <UL005>
      *************************                                         <UL005>
      *                                                                 <UL005>
       A1510-START.                                                     <UL005>
      *                                                                 <UL005>
      *  Loop COVR to count no of components.                           <UL005>
      *                                                                 <UL005>
           CALL 'COVRIO'            USING COVR-PARAMS.                  <UL005>
                                                                        <UL005>
           IF  COVR-STATUZ          NOT = O-K                           <UL005>
           AND COVR-STATUZ          NOT = ENDP                          <UL005>
              MOVE COVR-PARAMS        TO SYSR-PARAMS                    <UL005>
              MOVE COVR-STATUZ        TO SYSR-STATUZ                    <UL005>
              PERFORM 600-FATAL-ERROR                                   <UL005>
           END-IF.                                                      <UL005>
                                                                        <UL005>
           IF COVR-CHDRCOY          NOT = CHDRLIF-CHDRCOY               <UL005>
           OR COVR-CHDRNUM          NOT = CHDRLIF-CHDRNUM               <UL005>
           OR COVR-STATUZ               = ENDP                          <UL005>
              MOVE ENDP                TO COVR-STATUZ                   <UL005>
              GO TO A1590-EXIT                                          <UL005>
           END-IF.                                                      <UL005>
      *                                                                 <UL005>
      * Check to see if coverage is of a valid status                   <UL005>
      *                                                         <UL005> <UL005>
           MOVE 'N'                    TO WSAA-VALID-COVERAGE.          <UL005>
           IF  COVR-VALIDFLAG       NOT = '1'                           <UL005>
               GO TO A1580-NEXT                                         <UL005>
           END-IF.                                                      <UL005>
                                                                        <UL005>
           PERFORM VARYING WSAA-T5679-SUB                               <UL005>
                   FROM 1 BY 1                                          <UL005>
                   UNTIL WSAA-T5679-SUB      > 12                       <UL005>
                                                                        <UL005>
              IF T5679-COV-RISK-STAT (WSAA-T5679-SUB)= COVR-STATCODE    <UL005>
                 PERFORM VARYING WSAA-T5679-SUB                         <UL005>
                         FROM 1 BY 1                                    <UL005>
                         UNTIL WSAA-T5679-SUB   > 12                    <UL005>
                    IF T5679-COV-PREM-STAT (WSAA-T5679-SUB)             <UL005>
                                        = COVR-PSTATCODE                <UL005>
                       MOVE  13        TO WSAA-T5679-SUB                <UL005>
                       MOVE 'Y'        TO WSAA-VALID-COVERAGE           <UL005>
                       ADD 1           TO WSAA-COVR-COUNT               <UL005>
                    END-IF                                              <UL005>
                 END-PERFORM                                            <UL005>
              END-IF                                                    <UL005>
           END-PERFORM.                                                 <UL005>
      *                                                                 <UL005>
       A1580-NEXT.                                                      <UL005>
      *                                                                 <UL005>
           MOVE NEXTR                  TO COVR-FUNCTION.                <UL005>
      *                                                                 <UL005>
       A1590-EXIT.                                                      <UL005>
           EXIT.                                                        <UL005>
      /
       A100-READ-TU477 SECTION.                                         <UL001>
      *************************                                         <UL001>
       A110-START.                                                      <UL001>
      *                                                                 <UL001>
           MOVE 'N'                    TO WSAA-TU477-FLAG.              <UL001>
           MOVE SPACES                 TO ITEM-DATA-KEY.                <UL001>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <UL001>
           MOVE CHDRLIF-CHDRCOY        TO ITEM-ITEMCOY.                 <UL001>
           MOVE CHDRLIF-CNTTYPE        TO ITEM-ITEMITEM.                <UL001>
           MOVE TU477                  TO ITEM-ITEMTABL.                <UL001>
           MOVE READR                  TO ITEM-FUNCTION.                <UL001>
      *                                                                 <UL001>
           CALL 'ITEMIO' USING ITEM-PARAMS.                             <UL001>
      *                                                                 <UL001>
           IF ITEM-STATUZ           NOT = O-K AND MRNF                  <UL001>
              MOVE ITEM-STATUZ         TO SYSR-STATUZ                   <UL001>
              MOVE ITEM-PARAMS         TO SYSR-PARAMS                   <UL001>
              PERFORM 600-FATAL-ERROR                                   <UL001>
           END-IF.                                                      <UL001>
                                                                        <UL001>
           IF ITEM-STATUZ               = O-K                           <UL001>
              MOVE 'Y'                 TO WSAA-TU477-FLAG               <UL001>
           END-IF.                                                      <UL001>
      *                                                                 <UL001>
       A190-EXIT.                                                       <UL001>
            EXIT.                                                       <UL001>
      /
TVAN   A1800-LOAD-TZ028 SECTION.                                        <CS020>
      ***************************                                       <CS020>
       A1810-START.                                                     <CS020>
                                                                        <CS020>
           CALL 'ITEMIO'            USING ITEM-PARAMS.                  <CS020>
                                                                        <CS020>
           IF  ITEM-STATUZ         NOT = O-K                            <CS020>
           AND ITEM-STATUZ         NOT = ENDP                           <CS020>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <CS020>
               PERFORM 600-FATAL-ERROR                                  <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
           IF ITEM-STATUZ              = ENDP                           <CS020>
           OR ITEM-ITEMCOY       NOT   = BSPR-COMPANY                   <CS020>
           OR ITEM-ITEMTABL      NOT   = TZ028                          <CS020>
               MOVE ENDP               TO ITEM-STATUZ                   <CS020>
               GO TO A1890-EXIT                                         <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
           MOVE ITEM-GENAREA        TO TZ028-TZ028-REC.                 <CS020>
                                                                        <CS020>
           IF WSAA-TZ028-IX > WSAA-TZ028-SIZE                           <CS020>
               MOVE H791               TO SYSR-STATUZ                   <CS020>
               MOVE TZ028              TO SYSR-PARAMS                   <CS020>
               PERFORM 600-FATAL-ERROR                                  <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
           MOVE ITEM-ITEMITEM          TO                               <CS020>
                                 WSAA-TZ028-CNTTYPE (WSAA-TZ028-IX).    <CS020>
           MOVE TZ028-NOFYEAR          TO                               <CS020>
                                 WSAA-TZ028-NOFYEAR (WSAA-TZ028-IX).    <CS020>
                                                                        <CS020>
           MOVE NEXTR                  TO ITEM-FUNCTION.                <CS020>
                                                                        <CS020>
           SET WSAA-TZ028-IX UP BY 1.                                   <CS020>
                                                                        <CS020>
       A1890-EXIT.                                                      <CS020>
             EXIT.                                                      <CS020>
TVAN  /                                                                 <CS020>
                                                                        <CS020>
TUYEN  A3200-VALID-CONDITION SECTION.
      ********************************
       A3210-START.
      *                                                                 <CS020>
           MOVE WSAA-EFFDATE-PLUS-CNTLEAD                               <CS020>
                                       TO WSAA-BILLING-DATE.            <CS020>
           PERFORM A3300-GET-LAST-AS.
           PERFORM A3400-READ-COVR.
           PERFORM A3500-READ-ACBL.
           PERFORM A3800-READ-ZPPIENQ.
           COMPUTE WSAA-PLAN-PREM-AMT = ( WSAA-PLAN-PREM-AMT
                                      + WSAA-PREM-PL-TOPUP-AMT )
                                      * WSAA-COUNT-NUM-BILL.
       A3290-EXIT.
             EXIT.                                                      <CS020>
      /
       A3300-GET-LAST-AS SECTION.
      ****************************
       A3310-START.
      *
           INITIALIZE                     DTC3-DATCON3-REC.
           MOVE CHDRLIF-OCCDATE        TO DTC3-INT-DATE-1.
           MOVE BSSC-EFFECTIVE-DATE    TO DTC3-INT-DATE-2.
           MOVE '01'                   TO DTC3-FREQUENCY.
           CALL 'DATCON3' USING        DTC3-DATCON3-REC.
           IF DTC3-STATUZ              NOT = O-K
               MOVE DTC3-STATUZ        TO SYSR-STATUZ
               MOVE DTC3-DATCON3-REC   TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.
           MOVE DTC3-FREQ-FACTOR  TO WSAA-POLYR-IF.

           INITIALIZE                DTC2-DATCON2-REC.
           MOVE WSAA-POLYR-IF     TO DTC2-FREQ-FACTOR.
           MOVE '01'              TO DTC2-FREQUENCY.
           MOVE CHDRLIF-OCCDATE   TO DTC2-INT-DATE-1.
           CALL 'DATCON2' USING        DTC2-DATCON2-REC.
           IF DTC2-STATUZ              = BOMB
               MOVE DTC2-STATUZ        TO SYSR-STATUZ
               MOVE DTC2-DATCON2-REC   TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.
           MOVE DTC2-INT-DATE-2   TO WSAA-LAST-AS.
                                                                        <CS020>
           INITIALIZE                     DTC2-DATCON2-REC.             <CS020>
           MOVE '01'                   TO DTC2-FREQUENCY.               <CS020>
           MOVE WSAA-LAST-AS           TO DTC2-INT-DATE-1.              <CS020>
           MOVE  1                     TO DTC2-FREQ-FACTOR.             <CS020>
           PERFORM A200-CALL-DATCON2.                                   <CS020>
           MOVE DTC2-INT-DATE-2        TO WSAA-NEXT-AS.                 <CS020>

           INITIALIZE                     DTC2-DATCON2-REC.
           MOVE CHDRLIF-BILLFREQ       TO DTC2-FREQUENCY.
           MOVE WSAA-LAST-AS           TO DTC2-INT-DATE-1.
           MOVE  1                     TO DTC2-FREQ-FACTOR.
           PERFORM A200-CALL-DATCON2.
           MOVE DTC2-INT-DATE-2        TO WSAA-DUE-DATE-1ST.

           IF WSAA-BILLING-DATE         >= WSAA-NEXT-AS                 <CS020>
              MOVE WSAA-NEXT-AS        TO WSAA-LAST-AS                  <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
       A3390-EXIT.
            EXIT.
      /
       A3400-READ-COVR SECTION.                                         <CS020>
      **************************                                        <CS020>
       A3410-START.                                                     <CS020>
           MOVE ZEROES                 TO WSAA-PREM-PAID-AMT            <CS020>
                                          WSAA-PLAN-PREM-AMT.
           INITIALIZE                     COVRENQ-PARAMS.               <CS020>
           MOVE CHDRLIF-CHDRCOY        TO COVRENQ-CHDRCOY.              <CS020>
           MOVE CHDRLIF-CHDRNUM        TO COVRENQ-CHDRNUM.              <CS020>
           MOVE SPACES                 TO COVRENQ-PLAN-SUFFIX.          <CS020>
           MOVE COVRENQREC             TO COVRENQ-FORMAT.               <CS020>
           MOVE BEGN                   TO COVRENQ-FUNCTION.             <CS020>
                                                                        <CS020>
       A3420-CALL.                                                      <CS020>
           CALL 'COVRENQIO'           USING COVRENQ-PARAMS.             <CS020>
                                                                        <CS020>
           IF COVRENQ-STATUZ           NOT = O-K                        <CS020>
           AND COVRENQ-STATUZ          NOT = ENDP                       <CS020>
               MOVE COVRENQ-STATUZ        TO SYSR-STATUZ                <CS020>
               MOVE COVRENQ-PARAMS        TO SYSR-PARAMS                <CS020>
               PERFORM 600-FATAL-ERROR                                  <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
           IF COVRENQ-CHDRCOY          NOT = CHDRLIF-CHDRCOY            <CS020>
           OR COVRENQ-CHDRNUM          NOT = CHDRLIF-CHDRNUM            <CS020>
           OR COVRENQ-STATUZ               = ENDP                       <CS020>
              MOVE ENDP                TO COVRENQ-STATUZ                <CS020>
              GO TO A3490-EXIT                                          <CS020>
           END-IF.                                                      <CS020>

           PERFORM A3600-VALID-RIDER.
                                                                        <CS020>
           MOVE SPACES                 TO WSAA-RLDGACCT.                <CS020>
           MOVE COVRENQ-CHDRNUM        TO WSAA-RLDGACCT-CHDRNUM.        <CS020>
           MOVE COVRENQ-LIFE           TO WSAA-RLDGACCT-LIFE.           <CS020>
           MOVE COVRENQ-COVERAGE       TO WSAA-RLDGACCT-COVERAGE.       <CS020>
           MOVE COVRENQ-RIDER          TO WSAA-RLDGACCT-RIDER.          <CS020>
           MOVE ZEROES                 TO WSAA-RLDGACCT-PLSFX.          <CS020>
                                                                        <CS020>
           PERFORM A3700-SUM-ACMV.                                      <CS020>
                                                                        <CS020>
       A3480-NEXTR.                                                     <CS020>
           MOVE NEXTR               TO COVRENQ-FUNCTION.                <CS020>
           GO TO A3420-CALL.                                            <CS020>
                                                                        <CS020>
       A3490-EXIT.                                                      <CS020>
            EXIT.                                                       <CS020>
      /
       A3600-VALID-RIDER SECTION.
      ****************************
       A3610-START.
      *
           PERFORM VARYING WSAA-T5679-SUB
                   FROM 1 BY 1
                   UNTIL WSAA-T5679-SUB      > 12
              IF T5679-COV-RISK-STAT (WSAA-T5679-SUB)= COVRENQ-STATCODE
                 PERFORM VARYING WSAA-T5679-SUB
                         FROM 1 BY 1
                         UNTIL WSAA-T5679-SUB   > 12
                    IF T5679-COV-PREM-STAT (WSAA-T5679-SUB)
                                                    = COVRENQ-PSTATCODE
                       MOVE  13  TO WSAA-T5679-SUB
TVAN                   IF WSAA-TZ028-FOUND  = 'Y' AND                   <CS020>
                          COVRENQ-LIFE      = '01' AND                  <CS020>
                          COVRENQ-COVERAGE  = '01' AND                  <CS020>
                          COVRENQ-RIDER     = '00'                      <CS020>
                          CONTINUE                                      <CS020>
                       ELSE                                             <CS020>
                          ADD COVRENQ-INSTPREM  TO WSAA-PLAN-PREM-AMT   <CS020>
TVAN                   END-IF                                           <CS020>
                    END-IF
                 END-PERFORM
              END-IF
           END-PERFORM.
      *
       A3690-EXIT.
             EXIT.
      /                                                                 <CS020>
       A3700-SUM-ACMV SECTION.                                          <CS020>
      ************************                                          <CS020>
       A3710-START.                                                     <CS020>
      *                                                                 <CS020>
      *    Lopp ACMVENQ to sum total premium apply in 1 year contract.  <CS020>
      *                                                                 <CS020>
           INITIALIZE                     ACMVENQ-PARAMS.               <CS020>
           MOVE CHDRLIF-CHDRCOY        TO ACMVENQ-RLDGCOY.              <CS020>
           MOVE WSAA-RLDGACCT          TO ACMVENQ-RLDGACCT.             <CS020>
           MOVE WSAA-LAST-AS           TO ACMVENQ-EFFDATE.              <CS020>
           MOVE BEGN                   TO ACMVENQ-FUNCTION.             <CS020>
                                                                        <CS020>
       A3720-CALL.                                                      <CS020>
           CALL 'ACMVENQIO'         USING ACMVENQ-PARAMS.               <CS020>
                                                                        <CS020>
           IF  ACMVENQ-STATUZ       NOT = O-K                           <CS020>
           AND ACMVENQ-STATUZ       NOT = ENDP                          <CS020>
               MOVE ACMVENQ-PARAMS     TO SYSR-PARAMS                   <CS020>
               MOVE ACMVENQ-STATUZ     TO SYSR-STATUZ                   <CS020>
               PERFORM 600-FATAL-ERROR                                  <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
           IF  ACMVENQ-RLDGCOY      NOT = CHDRLIF-CHDRCOY               <CS020>
           OR  ACMVENQ-RLDGACCT     NOT = WSAA-RLDGACCT                 <CS020>
           OR  ACMVENQ-STATUZ           = ENDP                          <CS020>
           OR  ACMVENQ-EFFDATE          > BSSC-EFFECTIVE-DATE           <CS020>
               MOVE ENDP               TO ACMVENQ-STATUZ                <CS020>
               GO TO A3790-EXIT                                         <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
           IF ACMVENQ-SACSCODE          = 'LE' AND                      <CS020>
            ( ACMVENQ-SACSTYP           = 'LP' OR 'SP' )                <CS020>
              ADD ACMVENQ-ACCTAMT      TO WSAA-PREM-PAID-AMT            <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
       A3780-NEXTR.                                                     <CS020>
           MOVE NEXTR               TO ACMVENQ-FUNCTION.                <CS020>
           GO TO A3720-CALL.                                            <CS020>
                                                                        <CS020>
       A3790-EXIT.                                                      <CS020>
            EXIT.                                                       <CS020>
      /                                                                 <CS020>
       A3500-READ-ACBL SECTION.                                         <CS020>
      **************************                                        <CS020>
       A3510-START.                                                     <CS020>
      *
           MOVE ZEROES                 TO WSAA-SACSCURBAL.              <CS020>
           INITIALIZE                     ACBLENQ-PARAMS.               <CS020>
           MOVE CHDRLIF-CHDRCOY        TO ACBLENQ-RLDGCOY.              <CS020>
           MOVE SPACES                 TO WSAA-RLDGACCT.                <CS020>
           MOVE CHDRLIF-CHDRNUM        TO WSAA-RLDGACCT-CHDRNUM.        <CS020>
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
               GO TO A3590-EXIT                                         <CS020>
           END-IF.                                                      <CS020>
                                                                        <CS020>
           COMPUTE WSAA-SACSCURBAL      = ACBLENQ-SACSCURBAL * -1.      <CS020>
           ADD WSAA-SACSCURBAL         TO WSAA-PREM-PAID-AMT.
                                                                        <CS020>
       A3590-EXIT.                                                      <CS020>
            EXIT.                                                       <CS020>
      /                                                                 <CS020>
       A3800-READ-ZPPIENQ SECTION.
      *****************************
       A3810-START.
      *
           INITIALIZE                     ZPPIENQ-PARAMS.
           MOVE CHDRLIF-CHDRNUM        TO ZPPIENQ-CHDRNUM.
           MOVE READR                  TO ZPPIENQ-FUNCTION.
           MOVE ZPPIENQREC             TO ZPPIENQ-FORMAT.

           CALL 'ZPPIENQIO'         USING ZPPIENQ-PARAMS.

           IF ZPPIENQ-STATUZ        NOT = O-K AND MRNF
              MOVE ZPPIENQ-STATUZ      TO SYSR-STATUZ
              MOVE ZPPIENQ-PARAMS      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

           IF ZPPIENQ-STATUZ            =  O-K                          <CS020>
              EVALUATE TRUE                                             <CS020>
              WHEN WSAA-POLYR-IF           = 0                          <CS020>
                 MOVE ZPPIENQ-PREMESTA    TO WSAA-PREM-PL-TOPUP-AMT     <CS020>
              WHEN WSAA-POLYR-IF           = 1                          <CS020>
                 MOVE ZPPIENQ-PREMESTB    TO WSAA-PREM-PL-TOPUP-AMT     <CS020>
              WHEN WSAA-POLYR-IF           = 2                          <CS020>
                 MOVE ZPPIENQ-PREMESTC    TO WSAA-PREM-PL-TOPUP-AMT     <CS020>
              WHEN OTHER                                                <CS020>
                 MOVE ZPPIENQ-PREMESTD    TO WSAA-PREM-PL-TOPUP-AMT     <CS020>
              END-EVALUATE                                              <CS020>
           ELSE                                                         <CS020>
              MOVE ZEROES                 TO WSAA-PREM-PL-TOPUP-AMT     <CS020>
           END-IF                                                       <CS020>
      *
           EVALUATE TRUE
           WHEN CHDRLIF-BILLFREQ        = '01'
              MOVE 1                   TO WSAA-NUM-FREQ
              MOVE 1                   TO WSAA-COUNT-NUM-BILL           <CS020>
           WHEN CHDRLIF-BILLFREQ        = '02'
              MOVE 2                   TO WSAA-NUM-FREQ
              IF WSAA-BILLING-DATE      = WSAA-DUE-DATE-1ST             <CS020>
                 MOVE 2                TO WSAA-COUNT-NUM-BILL
              ELSE
                 IF WSAA-BILLING-DATE   = WSAA-NEXT-AS                  <CS020>
                    MOVE 1             TO WSAA-COUNT-NUM-BILL           <CS020>
                 ELSE                                                   <CS020>
                    MOVE PAYR-PTDATE      TO WSAA-BILLING-DATE          <CS020>
                    IF WSAA-BILLING-DATE   = WSAA-DUE-DATE-1ST          <CS020>
                       MOVE 2             TO WSAA-COUNT-NUM-BILL        <CS020>
                    ELSE                                                <CS020>
                       MOVE 1          TO WSAA-COUNT-NUM-BILL           <CS020>
                    END-IF                                              <CS020>
                 END-IF                                                 <CS020>
              END-IF
           WHEN CHDRLIF-BILLFREQ        = '04'
              MOVE 4                   TO WSAA-NUM-FREQ
              IF WSAA-BILLING-DATE          = WSAA-DUE-DATE-1ST         <CS020>
                 MOVE 2                TO WSAA-COUNT-NUM-BILL
              ELSE
                 INITIALIZE                     DTC2-DATCON2-REC
                 MOVE CHDRLIF-BILLFREQ       TO DTC2-FREQUENCY
                 MOVE WSAA-DUE-DATE-1ST      TO DTC2-INT-DATE-1
                 MOVE  1                     TO DTC2-FREQ-FACTOR
                 PERFORM A200-CALL-DATCON2
                 MOVE DTC2-INT-DATE-2        TO WSAA-DUE-DATE-2ST
                 IF WSAA-BILLING-DATE          = WSAA-DUE-DATE-2ST      <CS020>
                    MOVE 3                TO WSAA-COUNT-NUM-BILL
                 ELSE
                    INITIALIZE                 DTC2-DATCON2-REC
                    MOVE CHDRLIF-BILLFREQ   TO DTC2-FREQUENCY
                    MOVE WSAA-DUE-DATE-2ST  TO DTC2-INT-DATE-1
                    MOVE  1                 TO DTC2-FREQ-FACTOR
                    PERFORM A200-CALL-DATCON2
                    MOVE DTC2-INT-DATE-2    TO WSAA-DUE-DATE-3ST
                    IF WSAA-BILLING-DATE     = WSAA-DUE-DATE-3ST        <CS020>
                       MOVE 4               TO WSAA-COUNT-NUM-BILL
                    ELSE
                       IF WSAA-BILLING-DATE  = WSAA-NEXT-AS             <CS020>
                          MOVE 1            TO WSAA-COUNT-NUM-BILL      <CS020>
                       ELSE                                             <CS020>
                          MOVE PAYR-PTDATE      TO WSAA-BILLING-DATE    <CS020>
                          EVALUATE TRUE                                 <CS020>
                          WHEN WSAA-BILLING-DATE  = WSAA-DUE-DATE-1ST   <CS020>
                             MOVE 2    TO WSAA-COUNT-NUM-BILL           <CS020>
                          WHEN WSAA-BILLING-DATE  = WSAA-DUE-DATE-2ST   <CS020>
                             MOVE 3    TO WSAA-COUNT-NUM-BILL           <CS020>
                          WHEN WSAA-BILLING-DATE  = WSAA-DUE-DATE-3ST   <CS020>
                             MOVE 4    TO WSAA-COUNT-NUM-BILL           <CS020>
                          WHEN OTHER                                    <CS020>
                             MOVE 1    TO WSAA-COUNT-NUM-BILL           <CS020>
                          END-EVALUATE                                  <CS020>
                       END-IF                                           <CS020>
                    END-IF
                 END-IF
              END-IF
           WHEN OTHER
              MOVE 12                  TO WSAA-NUM-FREQ
              PERFORM A3900-CALC-DUE-DATE
              EVALUATE TRUE                                             <CS020>
              WHEN WSAA-BILLING-DATE      = WSAA-DUE-DATE-1ST           <CS020>
                 MOVE 2                TO WSAA-COUNT-NUM-BILL
              WHEN WSAA-BILLING-DATE      = WSAA-DUE-DATE-2ST           <CS020>
                 MOVE 3                TO WSAA-COUNT-NUM-BILL
              WHEN WSAA-BILLING-DATE      = WSAA-DUE-DATE-3ST           <CS020>
                 MOVE 4                TO WSAA-COUNT-NUM-BILL
              WHEN WSAA-BILLING-DATE      = WSAA-DUE-DATE-4ST           <CS020>
                 MOVE 5                TO WSAA-COUNT-NUM-BILL
              WHEN WSAA-BILLING-DATE      = WSAA-DUE-DATE-5ST           <CS020>
                 MOVE 6                TO WSAA-COUNT-NUM-BILL
              WHEN WSAA-BILLING-DATE      = WSAA-DUE-DATE-6ST           <CS020>
                 MOVE 7                TO WSAA-COUNT-NUM-BILL
              WHEN WSAA-BILLING-DATE      = WSAA-DUE-DATE-7ST           <CS020>
                 MOVE 8                TO WSAA-COUNT-NUM-BILL
              WHEN WSAA-BILLING-DATE      = WSAA-DUE-DATE-8ST           <CS020>
                 MOVE 9                TO WSAA-COUNT-NUM-BILL
              WHEN WSAA-BILLING-DATE      = WSAA-DUE-DATE-9ST           <CS020>
                 MOVE 10               TO WSAA-COUNT-NUM-BILL
              WHEN WSAA-BILLING-DATE      = WSAA-DUE-DATE-10ST          <CS020>
                 MOVE 11               TO WSAA-COUNT-NUM-BILL
              WHEN WSAA-BILLING-DATE      = WSAA-DUE-DATE-11ST          <CS020>
                 MOVE 12               TO WSAA-COUNT-NUM-BILL
              WHEN WSAA-BILLING-DATE      = WSAA-DUE-DATE-12ST          <CS020>
                 MOVE 1                TO WSAA-COUNT-NUM-BILL           <CS020>
              WHEN OTHER                                                <CS020>
                 MOVE PAYR-PTDATE      TO WSAA-BILLING-DATE             <CS020>
                 EVALUATE TRUE                                          <CS020>
                 WHEN WSAA-BILLING-DATE      = WSAA-DUE-DATE-1ST        <CS020>
                    MOVE 2                TO WSAA-COUNT-NUM-BILL        <CS020>
                 WHEN WSAA-BILLING-DATE      = WSAA-DUE-DATE-2ST        <CS020>
                    MOVE 3                TO WSAA-COUNT-NUM-BILL        <CS020>
                 WHEN WSAA-BILLING-DATE      = WSAA-DUE-DATE-3ST        <CS020>
                    MOVE 4                TO WSAA-COUNT-NUM-BILL        <CS020>
                 WHEN WSAA-BILLING-DATE      = WSAA-DUE-DATE-4ST        <CS020>
                    MOVE 5                TO WSAA-COUNT-NUM-BILL        <CS020>
                 WHEN WSAA-BILLING-DATE      = WSAA-DUE-DATE-5ST        <CS020>
                    MOVE 6                TO WSAA-COUNT-NUM-BILL        <CS020>
                 WHEN WSAA-BILLING-DATE      = WSAA-DUE-DATE-6ST        <CS020>
                    MOVE 7                TO WSAA-COUNT-NUM-BILL        <CS020>
                 WHEN WSAA-BILLING-DATE      = WSAA-DUE-DATE-7ST        <CS020>
                    MOVE 8                TO WSAA-COUNT-NUM-BILL        <CS020>
                 WHEN WSAA-BILLING-DATE      = WSAA-DUE-DATE-8ST        <CS020>
                    MOVE 9                TO WSAA-COUNT-NUM-BILL        <CS020>
                 WHEN WSAA-BILLING-DATE      = WSAA-DUE-DATE-9ST        <CS020>
                    MOVE 10               TO WSAA-COUNT-NUM-BILL        <CS020>
                 WHEN WSAA-BILLING-DATE      = WSAA-DUE-DATE-10ST       <CS020>
                    MOVE 11               TO WSAA-COUNT-NUM-BILL        <CS020>
                 WHEN WSAA-BILLING-DATE      = WSAA-DUE-DATE-11ST       <CS020>
                    MOVE 12               TO WSAA-COUNT-NUM-BILL        <CS020>
                 WHEN WSAA-BILLING-DATE      = WSAA-DUE-DATE-12ST       <CS020>
                    MOVE 1                TO WSAA-COUNT-NUM-BILL        <CS020>
                 WHEN OTHER                                             <CS020>
                    CONTINUE                                            <CS020>
                 END-EVALUATE                                           <CS020>
              END-EVALUATE                                              <CS020>
           END-EVALUATE.
      *
           COMPUTE WSAA-PREM-PL-TOPUP-AMT = WSAA-PREM-PL-TOPUP-AMT
                                          / WSAA-NUM-FREQ.
       A3890-EXIT.
             EXIT.
      /
       A3900-CALC-DUE-DATE SECTION.
      ******************************
       A3910-START.
      *
           INITIALIZE                     DTC2-DATCON2-REC.
           MOVE CHDRLIF-BILLFREQ       TO DTC2-FREQUENCY.
           MOVE WSAA-DUE-DATE-1ST      TO DTC2-INT-DATE-1.
           MOVE  1                     TO DTC2-FREQ-FACTOR.
           PERFORM A200-CALL-DATCON2.
           MOVE DTC2-INT-DATE-2        TO WSAA-DUE-DATE-2ST.
      *
           INITIALIZE                     DTC2-DATCON2-REC.
           MOVE CHDRLIF-BILLFREQ       TO DTC2-FREQUENCY.
           MOVE WSAA-DUE-DATE-2ST      TO DTC2-INT-DATE-1.
           MOVE  1                     TO DTC2-FREQ-FACTOR.
           PERFORM A200-CALL-DATCON2.
           MOVE DTC2-INT-DATE-2        TO WSAA-DUE-DATE-3ST.
      *
           INITIALIZE                     DTC2-DATCON2-REC.
           MOVE CHDRLIF-BILLFREQ       TO DTC2-FREQUENCY.
           MOVE WSAA-DUE-DATE-3ST      TO DTC2-INT-DATE-1.
           MOVE  1                     TO DTC2-FREQ-FACTOR.
           PERFORM A200-CALL-DATCON2.
           MOVE DTC2-INT-DATE-2        TO WSAA-DUE-DATE-4ST.
      *
           INITIALIZE                     DTC2-DATCON2-REC.
           MOVE CHDRLIF-BILLFREQ       TO DTC2-FREQUENCY.
           MOVE WSAA-DUE-DATE-4ST      TO DTC2-INT-DATE-1.
           MOVE  1                     TO DTC2-FREQ-FACTOR.
           PERFORM A200-CALL-DATCON2.
           MOVE DTC2-INT-DATE-2        TO WSAA-DUE-DATE-5ST.
      *
           INITIALIZE                     DTC2-DATCON2-REC.
           MOVE CHDRLIF-BILLFREQ       TO DTC2-FREQUENCY.
           MOVE WSAA-DUE-DATE-5ST      TO DTC2-INT-DATE-1.
           MOVE  1                     TO DTC2-FREQ-FACTOR.
           PERFORM A200-CALL-DATCON2.
           MOVE DTC2-INT-DATE-2        TO WSAA-DUE-DATE-6ST.
      *
           INITIALIZE                     DTC2-DATCON2-REC.
           MOVE CHDRLIF-BILLFREQ       TO DTC2-FREQUENCY.
           MOVE WSAA-DUE-DATE-6ST      TO DTC2-INT-DATE-1.
           MOVE  1                     TO DTC2-FREQ-FACTOR.
           PERFORM A200-CALL-DATCON2.
           MOVE DTC2-INT-DATE-2        TO WSAA-DUE-DATE-7ST.
      *
           INITIALIZE                     DTC2-DATCON2-REC.
           MOVE CHDRLIF-BILLFREQ       TO DTC2-FREQUENCY.
           MOVE WSAA-DUE-DATE-7ST      TO DTC2-INT-DATE-1.
           MOVE  1                     TO DTC2-FREQ-FACTOR.
           PERFORM A200-CALL-DATCON2.
           MOVE DTC2-INT-DATE-2        TO WSAA-DUE-DATE-8ST.
      *
           INITIALIZE                     DTC2-DATCON2-REC.
           MOVE CHDRLIF-BILLFREQ       TO DTC2-FREQUENCY.
           MOVE WSAA-DUE-DATE-8ST      TO DTC2-INT-DATE-1.
           MOVE  1                     TO DTC2-FREQ-FACTOR.
           PERFORM A200-CALL-DATCON2.
           MOVE DTC2-INT-DATE-2        TO WSAA-DUE-DATE-9ST.
      *
           INITIALIZE                     DTC2-DATCON2-REC.
           MOVE CHDRLIF-BILLFREQ       TO DTC2-FREQUENCY.
           MOVE WSAA-DUE-DATE-9ST      TO DTC2-INT-DATE-1.
           MOVE  1                     TO DTC2-FREQ-FACTOR.
           PERFORM A200-CALL-DATCON2.
           MOVE DTC2-INT-DATE-2        TO WSAA-DUE-DATE-10ST.
      *
           INITIALIZE                     DTC2-DATCON2-REC.
           MOVE CHDRLIF-BILLFREQ       TO DTC2-FREQUENCY.
           MOVE WSAA-DUE-DATE-10ST      TO DTC2-INT-DATE-1.
           MOVE  1                     TO DTC2-FREQ-FACTOR.
           PERFORM A200-CALL-DATCON2.
           MOVE DTC2-INT-DATE-2        TO WSAA-DUE-DATE-11ST.
      *
           INITIALIZE                     DTC2-DATCON2-REC.
           MOVE CHDRLIF-BILLFREQ       TO DTC2-FREQUENCY.
           MOVE WSAA-DUE-DATE-11ST      TO DTC2-INT-DATE-1.
           MOVE  1                     TO DTC2-FREQ-FACTOR.
           PERFORM A200-CALL-DATCON2.
           MOVE DTC2-INT-DATE-2        TO WSAA-DUE-DATE-12ST.
      *
       A3990-EXIT.
             EXIT.
      /
       A200-CALL-DATCON2 SECTION.
      ****************************
       A210-START.
      *
           CALL 'DATCON2'          USING DTC2-DATCON2-REC.
           IF DTC2-STATUZ              NOT = O-K
              MOVE DTC2-DATCON2-REC    TO SYSR-PARAMS
              MOVE DTC2-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.
      *
       A290-EXIT.
            EXIT.
TUYEN /
