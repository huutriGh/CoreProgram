      * Generation Parameters SCRVER(02)               Do Not Delete!   <S9503>
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P6378.
      *
      * Copyright 1986-2021, Computer Sciences Corporation.
      *
      *REMARKS.
      *
      *              PRE-ISSUE VALIDATION
      *
      * Initialise
      * ----------
      *
      * PREPARATION
      *
      *   On the first  time  into  the  program,  read  the  financial
      *   accounting  rules   for   the  transaction  (T5645,  item  is
      *   transaction number passed in WSSP). Also read the sub-account
      *   type record for the  FIRST  posting (posting no. 01 on T5645)
      *   to get the sign (T3695).
      *
      * AMOUNTS ETC ON SCREEN
      *
      *   Retrieve the contract header (CHDRLNB) for the contract being
      *   worked on. Using these details, look up:
      *
      *        - the contract type description from T5688,
      *
      *        - the contract owner  (CLTS)  and  format  the  name for
      *             confirmation.
      *
      *   Calculate the amounts to be shown on the screen as follows:
      *
      *        Contract Suspense  -  read  the  sub-account balance for
      *             posting 01 (as  defined  on  T5645) for the current
      *             contract. Apply  the  sign for the sub-account type
      *             (i.e. if the  sign  is -ve, negate the balance read
      *             when displaying it on the screen).
      *
      *        Contract Fee  -  read  the  contract  definition details
      *             (T5688) for the  contract type held on the contract
      *             header. If the  contract  fee  method is not blank,
      *             look up the  subroutine  used  to calculate it from
      *             T5674 and call  it  with the required linkage area.
      *             This  subroutine   will  return  the  contract  fee
      *             amount. If there  is  no  contract fee method, this
      *             amount is zero. If not zero, adjust the fee by  the
      *             the number of frequencies required prior to  issue.
      *             To   do  this,  call  DATCON3  with   the  contract
      *             commencement  date,  billing commencement date  and
      *             billing  frequency  from the  contract header. This
      *             will  return a factor.  Multiply the fee amount  by
      *             this factor to give the fee to be displayed on  the
      *             screen.
      *
      *        Premium  -  read through the coverage/rider transactions
      *             (COVTLNB).  For  each  record  read, accumulate all
      *             single  premiums  payable  and all regular premiums
      *             payable.  A  premium  is a single premium amount if
      *             the  billing  frequency  for  the whole contract is
      *             '00',  or the coverage/rider definition (T5687) has
      *             a single premium indicator of 'Y'.  Otherwise it is
      *             regular  premium.  Once  all the premiums have been
      *             accumulated,  adjust  the  regular premiums (if not
      *             zero)  by  the number of frequencies required prior
      *             to  issue.  To  do  this,  call  DATCON3  with  the
      *             contract  commencement  date,  billing commencement
      *             date   and  billing  frequency  from  the  contract
      *             header.  This  will  return a factor.  Multiply the
      *             regular  premium  amount  by this factor and add it
      *             to    the   single   premium  amount  to  give  the
      *             "instalment"    premium  to  be  displayed  on  the
      *             screen.
      *
      *   Cross check the contract details  for consistency errors. For
      *   each error detected, call the error messages routine (ERRMSG)
      *   and load the message returned,  along  with  the  appropriate
      *   "key" details in the subfile. Load all pages of the subfile.
      *
      * CONTRACT LEVEL VALIDATION
      *
      *   Check  that  the  premium amount plus the contract fee amount
      *   displayed (the "required premium") are less  than or equal to
      *   the contract suspense amount paid  (NB  check  screen amounts
      *   not sub-account amounts).  If there  is insufficient contract
      *   suspense, check that  it  is  within  the  tolerance for this
      *   transaction.
      *
      *        - look up the tolerance applicable  (T5667,  item  key =
      *             transaction code/contract currency).
      *
      *        - apply  the  tolerance  percentage   for  the  contract
      *             billing frequency to the  required  premium  amount
      *             calculated.  If  this is less  than  the  tolerance
      *             amount limit, use this. Otherwise use the tolerance
      *             amount.
      *
      *        - check that the difference between the required premium
      *             and the suspense amount is less  than  or  equal to
      *             the tolerance amount.
      *
      *   Read  through  all  the  follow-ups set  up  for the contract
      *   (FLUP).  For  each  record  read,  check to see that none are
      *   still outstanding (status matching on T5661). - NOTE there is
      *   code to do this in P5004, 1200- SECTION.
      *
      *   The contract definition table (T5688 read above) defines the
      *   minimum  number of lives which must be on the contract.  Read
      *   through  the  lives  attached  to  the contract (LIFELNB) and
      *   count them.  Only count life records  which have a joint life
      *   number of '00' (the others are joint lives and do not count).
      *   Cross check the number read with the minimum required.
      *
      * LIFE LEVEL VALIDATION
      *
      *   For each life and joint life  read,  also cross validate that
      *   critical details on the contract  header  and the life record
      *   match.  If  they  do  not,   the   life/joint  life  must  be
      *   "re-visited".  These are:
      *
      *        1) Age next birthday at risk commencement. Take the date
      *             of  birth  from  the  life   record  and  the  risk
      *             commencement date from  the  contract  header. Feed
      *             these with a frequency of  1  to DATCON3. This will
      *             return a factor. Round this up to the nearest whole
      *             number.  This must be the  same  as  the  age  next
      *             birthday as held on the life record.
      *
      * COMPONENT LEVEL VALIDATION
      *
      *   Read  each  coverage/rider   transaction   record  (COVTLNB).
      *   Each  coverage/rider  must be checked  to  see  if  it  needs
      *   re-visiting. If it does for any  of the reasons listed below,
      *   skip the other checks and only output one message.
      *
      *   For the first coverage on a life,  look up the life and joint
      *   life (if it exists) to which it applies.
      *
      *        1) Cross check that the following  on the life record(s)
      *             are the same as on the component transaction:
      *             - age next birthday
      *             - sex
      *
      *        2) Cross check that the following on the contract header
      *             and coverage transaction are the same:
      *             - payment method
      *             - payment frequency
      *             - risk commencement date
      *
      *   If  there are any errors (and  hence  records  added  to  the
      *   subfile making SCRN-SUBFILE-RRN not = zero), this contract is
      *   not yet ready for issue.
      *
      * Validation
      * ----------
      *
      *   No actual screen validation is required.
      *
      * Updating
      * --------
      *
      *   If  there  were  no validation errors displayed on the screen
      *   (1000  section), set the available for issue  flag to 'Y' and
      *   KEEPS the contract header.
      *
      * Next
      * ----
      *
      *   Add one to the program pointer and exit.
      *
      *
      ******************Enhancements for Life Asia 1.0****************
      *
      * Enhancement 1:
      *
      * This module has been enhanced to cater for waiver of premium
      * (WOP) processing. The waiver of premium sum assured amount is
      * based on the accumulation of all the premiums of the
      * components on the proposal and the policy fee as specified on
      * table TR517. The validation of the sum assured on the WOP
      * component is as follows :
      *
      * - Read each component for the contract using COVTTRM. Access
      *   table TR517 with the component. If an item does not exist
      *   then continue reading the components until all components
      *   have been read for the contract.
      *
      * - If the item is found on TR517, indicating that we are now
      *   processing a WOP component, then calculate the WOP sum
      *   assured :
      *
      *     - Read the covrage records using COVTLNB logical.
      *     - Accumulate the WOP sum assured if :
      *
      *       (i) COVTLNB-LIFE and COVTTRM-LIFE are the same
      *           and TR517-ZRWVGLG-02 is 'N'. This flag is set to
      *           'N' when a waiver is only applicable to one of the
      *           live's components. If this is the case then a check
      *           is required on the life. If the flag is set to 'Y',
      *           then the check is not required.
      *
      *      (ii) The coverage is present on TR517 indicating that the
      *           premium may be waived on this component.
      *
      *     - Add the policy fee to the accumulated sum assured if
      *       specified on TR517.
      *
      * - If the accumulated sum assured is not the same as the
      *   sum assured on the WOP component, then issue the appropriate
      *   validation error messsge.
      *
      * ==============
      * Enhancement 2:
      *
      * This enhancement allows situation when issuing contracts,
      * suspense available in different currencies is checked to settle
      * the premium payable.  The sequence for checking suspense runs
      * from contract currency, billing currency, then other currency.
      *
      * Underwriting decision date must be captured for successful
      * issue.
      *
      ******************Enhancements for Life Asia 2.0****************
      *
      * Enhancement 1:
      *
      * If a non-traditional contract has been issued and HITR
      * records processed through UNITDEAL, it is possible to process
      * an AFI and then re-issue the contract without running
      * UNITDEAL. Thus we need a test here to prevent re-issuing
      * contracts with unprocessed HITR records.
      *
      * Enhancement 2:
      *
      * Call subroutine HCRTFUP to check/create automatic follow ups.
      *
      * Move the outstanding follow-up check to after the life level
      * check, since follow-ups may get created there.
      *
      *****************************************************************
      *              AMENDMENT  HISTORY                               *
      *****************************************************************
      * DATE.....   BY..   AMENDMENT...............  NUMBER
      *
      * DD/MM/YY    X.X.   xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  NNN
      * 08/12/88    A.C.   Author.
      * 8.03.89     MR. X  ADD FREQUENCY CALCS FOR REGUALR PREMIUM
      * 31.05.89    A.L.   Single and Regular Premiums now held in  001
      *                    separate fields on COVT/COVR so:-
      *                       a) don't need to ckeck T5687 for
      *                          SINGLE-PREM-IND, just move the
      *                          corresponding fields.
      * 06.06.89    J.M.   Replace PAYEENAME with PLAINNAME perform 002
      *
      * 20.12.89    RAH.   Include checks for relevant details if   N05
      *                    MOP = 'B'(Bank direct Debit) or 'G'(Group).
      *
      * 13.03.90    A.L.   Code to check for tolerance allowed on   003
      *                    T5667 etc. appears to have been forgotten.
      *
      * 05.04.90    T.S.   Check the table T3625 to determine       004
      *                    whether the bank details/group details
      *                    is required or not.
      *
      * 09.04.90    J.F.   Use the FSU company to key the Client        <005>
      *                    file, not the logon company.             005 <005>
      *
      * 01.05.90    B.C.   Check for T5674 Fee Calculation equal    006 <006>
      *                    SPACES before attempting to call
      *                    the held subroutine (NM FIX).
      *
      * 01.05.90    B.C.   ZEROISE Fee accumulator fields & Linkage 007 <007>
      *
      * 19/07/90    B.G.                                            008 <008>
      *
      *  P6378 - Direct Debit Amendment Specification.
      *  --------------------------------------------
      *  Subject: Billing Commencement Date
      *  --------------------------
      *  Overview.
      *  The new direct debit processing will  automatically  set  up
      *  a  Billing Commencement Date while writing a record onto the
      *  Contract Header File and Mandate File. However it  is  still
      *  possible  for  the  user,  at  a  later  stage, to alter the
      *  Billing  Commencement  Date  on  the  Contract  Header  file
      *  without updating the Mandate File accordingly.
      *  This  amendment  is  aimed  at  performing  some  validation
      *  checks against the two files.
      *  Processing.
      *  -----------
      *  All of this validation only applies to  contracts  that  are
      *  paid  by Direct debit. Contracts that are not paid by Direct
      *  Debit will not have a  Billing  Channel  opf  'B'  and  this
      *  validation  is  not  applicable  to  them. For each Contract
      *  Header record if the Billing Channel is 'B' form  a  key  of
      *  Payor   Client   Company   'PAYRCOY',  Payor  Client  Number
      *  'PAYRNUM' and Mandate Reference Number 'MANDREF',  and  read
      *  the  appropriate  record  from  Mandate  File.  If it is not
      *  found then report an error otherwise perform  the  following
      *  validations:
      *  1)  'BILLCD'  must  be  equal  to OR greater than 'EFFDATE'.
      *  Otherwise display an appropriate error message.
      *  2) If mandate amount 'MANDAMT' is not equal  to  zero,  then
      *  it  must  have  a  same  value  as  'SINSTAMT06'.  Otherwise
      *  display an appropriate error message.
      *  3)  Ensure that the billing currency code 'BILLCURR' on  the
      *  Contract  Header  File (CHDRPF) is same as 'CURRCODE' on the
      *  Client/Bank Account Record file.
      *  Retrieve the client's Bank key 'BANKKEY'  and  Bank  Account
      *  Number 'BANKACCKEY' from the Mandate File and pick up the
      *  associated record from the Client/Bank Account File.
      *  If  'BILLCURR'  is  not  equal to 'CURRCODE' then display an
      *  appropriate error message.
      *  4) For non Direct Debit Contracts ensure  that  the  Mandate
      *  Reference Number field on the Contract header is blank.
      *  5) If the GO NO-GO FLAG on the mandate status code
      *  table (T3678) is 'N' display an appropriate error message.
      *
      * 01/09/90    JOS    Remove hardcoded references to checking  009
      *                    of billing-channel for Direct Debits and
      *                    use the Direct Debit Indicator (DDIND)
      *                    from T3625.
      *
      * 03/12/90    JOS    Check if the Client Bank A/C CURRFROM  010
      *                    and CURRTO dates are within the range
      *                    of the Mandate Effective date, else
      *                    report an error.
      *
      * 25/01/91    J.K.   (SDF-1323)                             011
      *                    A problem arises when a contract is
      *                    changed from Prem Paying to a Single
      *                    Premium Frequency.The COVT still reflects
      *                    old details until the COVT details can be
      *                    revisited.If the user attempts Pre-Issue
      *                    Validation before the COVT's are revisited
      *                    an error of IVRQ is returned from DATCON3.
      *                    This amendment will prevent IVRQ if
      *                    screens have not been revisited.
      *                    (WASA Fix)
      *
      * 28/01/91   C.G     SDF 2095                                 012
      *                    If the risk commencment is 28, 29, 30 of
      *                    January, 30 March, 30 May, 30 of August
      *                    or the 30 of October, the initial instalment
      *                    required is incorrectly calculated as
      *                    1 Month + 1 Day * premium instead of 1
      *                    Month(frequency 12). This is a datcon3
      *                    problem.
      * 05/02/91   JOS     Amend program to read T3678tables with  013
      *                    WSSP-FSUCO instead of the signon company
      *
      * 02/07/91   B.G     CHANGES FOR UK PERSONAL PENSIONS        014
      *
      *   (1). Add a 9 dimension table to working storage to hold
      *        the individual payer details/totals.
      *
      *   (2). Load the WS table by reading all the payer and payer
      *        role records for the contract.
      *
      *   (3). Change the premium accumulation across all components
      *        to add the amount into the relevant entry in the
      *        WS table depending on the payer number in the COVT.
      *        Add any contract fee payable to the payer of the first
      *        coverage.
      *
      *   (4).  Change the premium pro-rata to be calculated on a
      *         payer by payer basis.
      *
      *   (5). Add the calling of the tax relief subroutine for
      *        each payer by obtaining the tax relief method from
      *        T5688 and looking up the subroutine required on T6687.
      *
      *   (6). All the billing information will now be validated
      *        on a payer by payer basis using the information on
      *        the payer file.
      *
      *   (7). Remove the billing frequency, method of payment
      *        and billing currency from the screen.
      *
      * 23/07/91    I.W.   No longer use the SACS file but
      *                    will now use the ACBL.........           015
      *
      * 14/11/91    B.L    Replace the client specific error        016
      *                    numbers with base range numbers.
      *                    U016 replaced with G816.
      *                    U023 replaced with F941.
      *                    U024 replaced with H015.
      *                    U025 replaced with H017.
      *                    U026 replaced with H030.
      *                    U027 not used so removed.
      *
      * 20/11/91    J.L.   Change parameter on ACBL read from       017
      *                    acbl-params to acbl-data-area and
      *                    add 'it' to itdm-itempfx. Also move
      *                    MGFEELREC to sysr-params on error.
      *
      * 24/02/92    M.R.   SDF2280                                  018
      *                    CANNOT HANDLE FOR. LANGUAGES FOR LONG
      *                    DESC. ON T5688.
      *
      *
      * 28/02/92    G.D.   SDF3157                                  019
      *                    Correct contract fee processing for the
      *                    the use of single premium.
      *                    Single premium fee will be obtained by
      *                    calling the apporiate subroutine with
      *                    with freq = '00'.
      *                    No regular fee is applicable when BILLCD
      *                    = rcd date.
      *
      *                    AQR 1801.
      * 25/06/92    J.L.   Include the Polinc field in the check    020
      *                    for revisiting Covrs.
      *                    AQR 883 - check for change of Currency.
      *
      * 08/07/92    E.H.   SDF3102                                  021
      *                    Pro rata fee amount due (again). Error
      *                    introduced during fix <014> due to
      *                    the strange habbits of the COBOL MULTIPLY
      *                    statement when GIVING is not specified.
      *
      * 07/12/92   M.F     SDF 2095                                 022
      *                    Comment out the change made by C.G. on
      *                    28/01/91 as DATCON3 has been amended to
      *                    cater for this problem.
      *
      * 08/01/93    S.W.   AQR 1073                                 023
      *                    Pre-issue validation should check that
      *                    the Risk/Premium cessation dates are
      *                    no later than the corresponding dates
      *                    on the coverage.
      *
      * 11/01/93    S.W.   AQR 3754                                 024
      *                    If a unit linked contract has been
      *                    issued and UTRN records processed
      *                    through UNITDEAL, it is possible to
      *                    process an AFI and then re-issue the
      *                    contract without running UNITDEAL.
      *                    Thus we need a test in here to prevent
      *                    issuing contracts with unprocessed
      *                    UTRN records.
      *
      * 27/07/93    L.H.   AQR 4490. Development L03 - 11/93.       025
      *                    Changed to read T3620 for 'Bank Details
      *                    Required' and 'Group Details Required', as
      *                    these have been removed from T3625 for the
      *                    11/93 Development.
      *
      * 12/08/93    CJH    AQR 4490. Development L03 - 11/93.       026
      *                    Group number is now stored on PAYR-GRUPNUM
      *                    instead of PAYR-GRUPKEY.
      *
      * 17/03/94    D.W.   AQR 5071.                                027
      *                    When more than 1 life, this program
      *                    LOOPS in the 1000 section where
      *                    the 1101-CHECK to 1103-READ-RIDER
      *                    paragraph labels are. The error is the
      *                    IF statement checking when the coverage
      *                    has changed - It should check for when
      *                    the Coverage or Life has changed.
      *
      * 05/10/94    R.S.   AQR 4734                                 028
      *                    When an error is detected by this program
      *                    for a fast track contract, program should
      *                    return to the fast track screen and not
      *                    continue to issue the contract. So have
      *                    changed Section 4000 to return to previous
      *                    program if there is an error.
      *                    Also change CLTS-PARAMS to move to
      *                    SYSR-PARAMS instead of CLTS-PARAMS.
      *
      * 16/11/94    M.A.   AQR 5514.                                029
      *                    New Age calculation routine added. Replaces
      *                    DATCON3.
      *
      *
      *********************************************************************
      *
      * ......... New Version of the Amendment History.
      *
      *****************************************************************
      *           AMENDMENT  HISTORY
      *****************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....
      *
      * 16/10/95  01/01   A05691       Maria Murphy
      *           Changes to allow Single Premium Component cases on a
      *           Contract which also has a Regular Premium Component.
      *           When checking for changes across the contract, it
      *           is now possible for an individual coverage to have a
      *           different Billing Frequency to that on the PAYR
      *           record. When checking if the frequencies are
      *           different, include a check to ensure that this is
      *           is not a Single Premium Component. It is impossible
      *           to change the frequency of a Single Premium Component
      *           regardless of changes which may occur on the PAYR
      *           record.
      *           To this end the read of T5687 has been re-introduced
      *           such that the Single Premium Indicator can now be
      *           checked.
      *
      * 24/01/96  01/31   A05743       Andrew Wall
      *
      *           Check that none of the following are dead: the owner,
      *           the payer and all assured lives.
      *           Validate the servicing agent and any agents receiving
      *           commission for the contract to ensure that they are
      *           all currently active.
      *
      * 03/05/96  01/01   R96REA       Fiona Martin
      *           New Reassurance Retrofit.
      *           Call reassurance cession calculation
      *           subroutine, CSNCALC and if Status of 'FACL'
      *           is returned, output a message to the screen
      *           saying that a revisit to the Facultative
      *           Reassurer is necessary.
      *
      * 13/05/96  01/01   D96NUM       Fiona Martin                         *
      *                   CNTFEE
      *                   CNTSUSP
      *                   MANDAMT
      *                   MGFL-MGFEE (in copybook MGFEELREC)
      *                   SUMINS
      *                   INSTPREM
      *                   INSTPRM
      *                   SINGP
      *                   ORIGAMT
      *                   ACCTAMT
      *                   + Numerous working storage fields....
      *                   SINSTAMT
      *                   INSTTOT
      *
      *                   The above field(s) have been increased in
      *                   size as part of the 1996 Numeric Field
      *                   Increase project. This is to enable users
      *                   to then convert (if they should wish to do
      *                   so) the fields to 'Monetary' and use the
      *                   SMART Variable Decimal Places technology
      *                   accordingly.
      *                                                                     *
      *********************************************************************
      *
      * ......... New Version of the Amendment History (Life Asia 1.0)
      *
      *****************************************************************
      *           AMENDMENT  HISTORY
      *****************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....
      *
      * 19/07/96  01/01   CAS1.0       Kristin McLeish
      *           The reassurance cession calculation routine, CSNCALC,
      *           must be called once for each life on the policy to
      *           ensure that all components attached to multiple lives
      *           are considered for reassurance.
      *           Also, when displaying the error to the screen, it
      *           should only display the life details, since CSNCALC
      *           is called at life level, not at component level, and
      *           therefore the error is not applicable to individual
      *           components. This will imply to the user that all the
      *           components attached to the life assured should be
      *           re-checked. While this may be somewhat misleading,
      *           (in cases where only one of the components has been
      *           facultatively reassured), it is necessary since
      *           CSNCALC is only called once for each life.
      *           Note: the reason CSNCALC is not called at component
      *           level is to ensure that components attached to a life
      *           assured are reassured in the correct order (based on
      *           the reassurance priorities set up on T5448).
      *
      * 24/07/96  01/01   CAS1.0       Kristin McLeish
      *           Reset the available for issue flag to 'N' if any
      *           errors were found (this is because P5074 will always
      *           force a return to pre-issue validation if reassurance
      *           records exist for the policy even if the available
      *           for issue flag is 'Y'; therefore this program must
      *           set it back to 'N' if errors are present; see P5074
      *           for related changes.)
      *
      * 26/07/96  01/01   CAS1.0       Sunil Patel
      *           Enhancment for Waiver of Premium Function. This
      *           enhancment is to enable the system to automatically
      *           calculate the waiver of premium sum assured and the
      *           policy fee (if it is to be waived). This sum is
      *           then checked against the WOP coverag/rider. If they
      *           are inconsistent, then the pre-validation will fail.
      *
      * 08/08/96  01/01   CAS1.0       Cat Chiu
      *           Check suspense in other currencies other than
      *           contract currency. The checking is first contract
      *           currency, then billing currency, then other currency.
      *           Underwriting decision date on HPAD must exist for
      *           issue.
      *
      * 22/08/96  01/01   CAS1.0       Cat Chiu
      *           Ensure contact fee is initialised to zero if not
      *           calculated.
      *
      * 29/01/97  01/01   INTBR        Dominic Dunbar                       *
      *           If a non-traditional contract has been issued and         *
      *           HITR records processed through UNITDEAL, it is            *
      *           possible to process an AFI and then re-issue the          *
      *           contract without running UNITDEAL. Thus we need a         *
      *           test in here to prevent re- issuing contracts with        *
      *           unprocessed HITR records.                                 *
      *                                                                     *
      * 12/02/97  01/01   FUPLET       Dominic Dunbar                       *
      *           Call subroutine HCRTFUP to check/create automatic         *
      *           follow-ups                                                *
      *                                                                     *
      *           Move the outstanding follow-up check to after the         *
      *           life level check, since follow-ups may get created        *
      *           there.                                                    *
      *                                                                     *
      * 16/08/97  01/01   LA2103       Fred Chow of Hong Kong               *
      *           Change error number.                                 ??   *
      *           - HL13 --> HL42                                           *
      *           - ZCRTFUP --> HCRTFUP                                     *
      *                                                                     *
      * 01/09/97  01/01   LA2110       Tony Tang - CSC Hong Kong            *
      *           Re-compiled.                                              *
      *                                                                     *
      * 29/11/97    DUNC  SMART 9503 Conv for Client/Server.        <S9503>
      *                                                                     *
      * 14/10/98  A001    WOR.    S01                                       *
      *           Do not allow to issue contract if agent is on black list. *
      *                                                                     *
      * 24/12/98  01/01   V4L001       Balaji                               *
      *           Premium Deposit Enhancement                               *
      *                                                                     *
      * 25/12/98  01/01   V4L009       CSC - Daniel Wong                    *
      *           Minimum Modal Premium Validation                          *
      *                                                                     *
      * 15/01/99  01/01   N003         CSC - Ali Hartono                    *
      *           Check file TTRC. Send error message if temporary          *
      *           receipt no. and date is not entered.                      *
      *           Check these fields are mandatory/not in TH506.            8
      *                                                                     *
      * 03/03/99  01/01   TN01         CSC - Worachart                      *
      *           Amend reading of TR517 to access details on continuation  *
      *           screens.                                                  *
      *                                                                     *
      * 02/04/99  01/01   A002         CSC - Ali Hartono                    *
      *           Check Agent's Licence No. expiry date. the agent          *
      *           should not be allowed to be an agent associated with      *
      *           a policy if the date of pre-issue validation is after     *
      *           the expiry date of agent's licence.                       *
      *                                                                     *
      * 08/10/99  01/01   V5L001       Josephine Chiam                      *
      *           RECOMPILED                                           ??   *
      *                                                                     *
      * 17/11/99  01/01   V42003       CSC - James Low                      *
      *           Check if Accelerated Crisis Waiver flag is 'Y' in    ion  *
      *           TR517. If yes, compute sum insured of WOP should be       *
      *           sum insured of (base coverage minus rider). Otherwise     *
      *           proceed with normal method.                               *
      *                                                                     *
      * 19/11/99  01/01   V42006       CSC - Yong Kee Jee                   *
      *           To allow follow ups by contract type / Risk Class         *
      *           depending on table TR694                                  *
      *                                                                     *
      * 26/11/99  01/01   V42013       James Low                            *
      *           Premium Tolerance Enhancement                             *
      *                                                                     *
      * 27/12/99  01/01   V42L014      CSC - Yong Kee Jee                   *
      *         - To reset waiver Sum Assured at start of every Component   *
      *         - MGFEE is only added to the 1st Component once, it should  *
      *           not be added to any subsequent Component                  *
      *                                                                     *
      * 17/01/00  01/01   LFA1058      CSC - Ronald Macarulay               *
      *           Normal Waiver of Premium creating problem                 *
      *           (taken from Pru Philippines)                              *
      *                                                                     *
      * 28/02/00  01/01   LFA1064      CSC - Ronald Macarulay               *
      *           Agent Terminated should not be allowed to be issued       *
      *           (taken from Pru Philippines)                              *
      *                                                                     *
      * 14/06/00  01/01   MLS001       Boboy                                *
      *           Add language as part of the key of table T2240.      to   *
      *                                                                     *
      * 23/10/00  01/01   LA1168       CSC - Venkatesh Senthamarai          *
      *           Waiver of premium - Benefit amount not computed           *
      *           for second life. Program does not read next record.
      *                                                                     *
      * 23/04/99  01/01   A07303       MGILL                                *
      * ------->  Retrofitted by Chua Ming Jye on 13/07/2001.               *
      *           If proposal is amended then ensure that if a beneficiary  *
      *           exists the beneficiary is revisited to update the BNFY    *
      *           file correctly with any amended details eg. EFFDATE would *
      *           need to be updated if the RCD had been amended for the    *
      *           proposal.                                                 *
      *                                                                     *
      * 19/01/04  01/01   V65F14       Saw Geok Tin
      *           CLBL key structure now includes CLNTNUM to allow
      *           multiple usage of same bank account.
      *                                                                     *
      * 08/07/04  01/01   SMU002       Wincent Li                           *
      *           Modify REMARKS layout for SCRIBE documentation       ??   *
      *                                                                     *
      * 02/11/04  01/01   LA3392       Fred Chow - CSC HK                   *
      *           - Joint Life record is missing to be processed for   ??   *
      *             validation.                                             *
      *                                                                     *
      * 29/05/06  01/01   LA3998       Chen Xu - CSC Beijing                *
      *           Pass value to the RLPDLON-LANGUAGE field of linkage.      *
      *                                                                     *
      * 19/01/07  01/01   V71L12       Wang Ge/FSG/CSC (Singapore)          *
      *           Retrofit UDW001.                                          *
      *           The pre-issue validation prcess will perform the full     *
      *           underwriting assessment for each life assured by          *
      *           Checking the following details:                           *
      *           - Answers provided to the underwriting questions          *
      *           - The BMI value                                           *
      *           - The component underwriting rules (if applicable)        *
      *           But above assessment will be skipped if life assured      *
      *           is already U/W confirmed.                                 *
      *           Note: HCRTFUP is also not performed if life assured       *
      *                 is U/W confirmed.                                   *
      * 05/11/03  01/01   UDW001       Kathryn Mooney                       *
      *           Add section to process any required underwriting.         *
      *                                                                     *
      * 18/07/07  01/01   LA4216       Xu Chen/ASIA/CSC (Beijing)           *
      *           Fix on error caused by UNDL.                              *
      *           WSAA-QUESTSET value is set within the 1640-READ-TR675     *
      *           section.  When WSAA-QUESTSET is set by a contract,        *
      *           WSAA-QUESTSET will not get initialize if the subsequence  *
      *           contract does not have an entry in TR675.  Therefore,     *
      *           1650-UNDERWRITING section is perforned.                   *
      *                                                                     *
      * 03/09/07  01/01   V72L07       Xu Chen/ASIA/CSC (Beijing)           *
      *           Call VLPDRULE for product cross check validation,and      *
      *           add hidden field ERRCDE for Error Code.                   *
      *                                                                     *
      * 27/02/08  01/01   V72L11       Xu Chen/ASIA/CSC (Beijing)           *
      *           Add language as part of the key of Table T5661.           *
      *                                                                     *
      * 27/11/08  01/01   V73F02       Xu Chen/ASIA/CSC (China)             *
      *           Re-compile.                                               *
      *                                                                     *
      * 01/06/09  01/01   V74L01       Jinkee Guerrero/ASIA/CSC (Malaysia)  *
      *           Modify program to display new field total amount due      *
      *           inclusive of Service tax and Education Cess. Taxes will   *
      *           be calculated via routine from TR52D                      *
      *                                                                     *
      * 25/06/09  Retrofitted by Henry Wong                                 *
      *           01/01   A06596       Kathryn Mooney                       *
      *           Added validation to display error if RCD is greater       *
      *           than the date of death of the life assured(s),            *
      *           owner, and payer.                                         *
      *                                                                     *
      * 26/06/09  Retrofitted by Helen Cui                                  *
      *           01/01   A07109       Kathryn Mooney                       *
      *           Pre-Issue is crashing when the user presses F4.           *
      *                                                                     *
      * 24/07/09  Retrofitted by Helen Cui                                  *
      *           01/01   LA5070       Neil Davey                           *
      *           Need to check all BNFY records when checking Eff Date     *
      *           against the Occ Date of the Policy                        *
      *                                                                     *
      * 25/08/09  01/01   A06596       Fred Lee (CSC Hong Kong)             *
      *           Death Client check should also for Assignee, trustee      *
      *           joint owner, beneficiary and despatch address             *
      *                                                                     *
      *                                                                     *
      * 02/10/09  01/01   V74L01       Jinkee Guerrero/ASIA/CSC (Mala       *
      *           Add checking for contract fee indicator.                  *
      *                                                                     *
      * 21/01/10  01/01   LA4754       Xu Chen/ASIA/CSC (China)             *
      *           Extend age length from 2 to 3 digits.                     *
      *                                                                     *
      * 10/09/10  01/01   V76F06       Nancie Lin/FSG/CSC (Hong Kong)       *
      *           Call ZRDECPLC to do rounding processing                   *
      *           MOVE WSKY-BATC-BATCTRCDE    TO CSNC-BATCTRCDE.            *
      *                                                                     *
      * 10/02/11  01/01   V76F06       Gang Liu/ASIA/CSC (China)            *
      *           Do rounding for :                                         *
      *           S6378-INST-PREM                                           *
      *           S6378-TAXAMT-01                                           *
      *           S6378-PUFEE                                               *
      *           S6378-TAXAMT-02                                           *
      *                                                                     *
      * 22/03/11  01/01   V76L01       Xu Chen/ASIA/CSC (China)             *
      *           Enhance to check if there is at lease one beneficiary     *
      *           type defined in the Contract Mandatory Beneficiary Table  *
      *           (TR52Z).                                                  *
      *                                                                     *
      * 21/04/11  01/01   V76F13       Xu Chen/ASIA/CSC (China)             *
      *           Check if the mandate type is Credit Card or Bank.         *
      *                                                                     *
      * 14/10/13  01/01   DUMMY        Sang Nguyen - CSC Developer          *
      *           TEST                                                      *
      *                                                                     *
      * 14/10/13  01/01   NB005        Sang Nguyen - CSC Developer          *
      *           CALL HCRTFUP SUBROUTINE                                   *
      *                                                                     *
      * 16/05/14  01/01   CLM09        Phung Vi Hao                         *
      *           RECOMPILE ONLY                                            *
      *                                                                     *
      * 04/11/14  01/01   PHE003       Phuong Le Dev                        *
      *           ADD NEW FIELD IN TR517                                    *
      *                                                                     *
      * 18/04/17  01/01   NB010        Tuyet Huynh IT - DEV                 *
      *           Check Owner and LA are agent to show warning              *
      *                                                                     *
      * 19/09/19  01/01   NB022        Phi Tran - IT DEV                    *
      *           Don't Held UNDL when enquiry Policy.                      *
      *                                                                     *
      * 12/10/20  01/01   NB030        Mai Yen Phi - IT                     *
      *           Recompile.                                                *
      *                                                                     *
      * 13/11/20  01/01   NB031        Mai Yen Phi - IT                     *
      *           Add field AGNTNUM for HCRTFUPREC                          *
      *                                                                     *
      * 16/03/21  01/01   NB043        Van Bao Tuyen - IT                   *
      *           Add new validation for not selling riders for life        *
      *           assured has occupation class 5 in pre-issue screen.       *
      *                                                                     *
      * 16/03/21  01/01   NB034        Van Bao Tuyen - IT                   *
      *           New rule UW BMI code.                                     *
      *                                                                     *
      **DD/MM/YY*************************************************************
      /
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       01  WSAA-PROG                   PIC X(05) VALUE 'P6378'.
       01  WSAA-VERSION                PIC X(02) VALUE '01'.
      *
       01  WSAA-T5661-KEY.                                              <V72L11>
           03  WSAA-T5661-LANG         PIC X(01).                       <V72L11>
           03  WSAA-T5661-FUPCODE      PIC X(03).                       <V72L11>
      *                                                                 <V72L11>
       01  ERRORS.
           03  E304                    PIC X(04) VALUE 'E304'.
           03  E351                    PIC X(04) VALUE 'E351'.
           03  E355                    PIC X(04) VALUE 'E355'.
           03  I005                    PIC X(04) VALUE 'I005'.          <N05>
           03  E571                    PIC X(04) VALUE 'E571'.          <N05>
           03  E714                    PIC X(04) VALUE 'E714'.          <N05>
           03  E964                    PIC X(04) VALUE 'E964'.          <V71L12>
           03  F151                    PIC X(04) VALUE 'F151'.
           03  F290                    PIC X(04) VALUE 'F290'.
           03  F826                    PIC X(04) VALUE 'F826'.          <N05>
           03  G041                    PIC X(04) VALUE 'G041'.
           03  G066                    PIC X(04) VALUE 'G066'.
           03  G228                    PIC X(04) VALUE 'G228'.
           03  G816                    PIC X(04) VALUE 'G816'.          <016>
           03  G161                    PIC X(04) VALUE 'G161'.          <023>
           03  G772                    PIC X(04) VALUE 'G772'.          <V42013>
           03  H015                    PIC X(04) VALUE 'H015'.          <016>
           03  H017                    PIC X(04) VALUE 'H017'.          <016>
           03  H030                    PIC X(04) VALUE 'H030'.          <016>
           03  H360                    PIC X(04) VALUE 'H360'.
           03  H361                    PIC X(04) VALUE 'H361'.
           03  H362                    PIC X(04) VALUE 'H362'.
           03  I008                    PIC X(04) VALUE 'I008'.          <008>
           03  I009                    PIC X(04) VALUE 'I009'.          <008>
           03  I010                    PIC X(04) VALUE 'I010'.          <008>
           03  I004                    PIC X(04) VALUE 'I004'.          <008>
           03  I007                    PIC X(04) VALUE 'I007'.          <008>
           03  I014                    PIC X(04) VALUE 'I014'.          <008>
           03  I020                    PIC X(04) VALUE 'I020'.
           03  I030                    PIC X(04) VALUE 'I030'.          <024>
           03  F921                    PIC X(04) VALUE 'F921'.          <025>
           03  E507                    PIC X(04) VALUE 'E507'.          <A05743>
           03  E508                    PIC X(04) VALUE 'E508'.          <A05743>
           03  E510                    PIC X(04) VALUE 'E510'.          <A05743>
      **** 03  F782                    PIC X(04) VALUE 'F782'.  <A06596><A05743>
           03  W343                    PIC X(04) VALUE 'W343'.          <A06596>
           03  R064                    PIC X(04) VALUE 'R064'.          <R96REA>
           03  E977                    PIC X(04) VALUE 'E977'.          <A07303>
           03  RL06                    PIC X(04) VALUE 'RL06'.          <CAS1.0>
           03  HL05                    PIC X(04) VALUE 'HL05'.          <CAS1.0>
           03  HL08                    PIC X(04) VALUE 'HL08'.          <INTBR>
           03  HL42                    PIC X(04) VALUE 'HL42'.          <LA2103>
           03  TL01                    PIC X(04) VALUE 'TL01'.          <S01>
      **** 03  F941                    PIC X(04) VALUE 'F941'.     <016><025>
      ***  03  E005                    PIC X(04) VALUE 'E005'.          <016>
      ***  03  E040                    PIC X(04) VALUE 'E040'.          <016>
      ***  03  E665                    PIC X(04) VALUE 'E665'.          <016>
      ***  03  G692                    PIC X(04) VALUE 'G692'.          <016>
      ***  03  U016                    PIC X(04) VALUE 'U016'.          <016>
      ***  03  U023                    PIC X(04) VALUE 'U023'.          <016>
      ***  03  U024                    PIC X(04) VALUE 'U024'.          <016>
      ***  03  U025                    PIC X(04) VALUE 'U025'.          <016>
      ***  03  U026                    PIC X(04) VALUE 'U026'.          <016>
      ***  03  U027                    PIC X(04) VALUE 'U027'.          <016>
      ***  03  V025                    PIC X(04) VALUE 'V025'.          <016>
           03  RL11                    PIC X(04) VALUE 'RL11'.          <V4L009>
           03  RL12                    PIC X(04) VALUE 'RL12'.          <V4L009>
           03  TL15                    PIC X(04) VALUE 'TL15'.          <N003>
           03  TL16                    PIC X(04) VALUE 'TL16'.          <N003>
           03  TL43                    PIC X(04) VALUE 'TL43'.          <A002>
           03  F294                    PIC X(04) VALUE 'F294'.          <V42003>
           03  E591                    PIC X(04) VALUE 'E591'.          <V71L12>
           03  RPID                    PIC X(04) VALUE 'RPID'.          <V71L12>
           03  RFK2                    PIC X(04) VALUE 'RFK2'.          <V76L01>
           03  EV95                    PIC X(04) VALUE 'EV95'.          <NB010>
           03  ZER1                    PIC X(04) VALUE 'ZER1'.          <NB043>
                                                                        <PHE003>
       01  WSAA-AMT-BEFORE-DISC        PIC S9(15)V9(02) COMP-3.         <PHE003>
       01  WSAA-DISC-AMOUNT            PIC S9(09)V9(02) COMP-3.         <PHE003>
      *
      *01  WSAA-SINGP                  PIC S9(13)V99 COMP-3.            <014>
      *01  WSAA-REGPREM                PIC S9(13)V99 COMP-3.            <014>
      *01  WSAA-INSTPRM                PIC S9(07)V9(2) COMP-3.          <014>
      *01  WSAA-CNTFEE                 PIC S9(05)V9(2).                 <D96NUM>
       01  WSAA-CNTFEE                 PIC S9(15)V9(2).                 <D96NUM>
       01  WSAA-FACTOR                 PIC S9(6)V9(5) COMP-3.
       01  WSAA-OUTSTANFLU             PIC X.
       01  WSAA-OUTFLUP-FOUND          PIC X(01).                       <FUPLET>
       01  WSAA-DOC-REQD               PIC X(01).                       <FUPLET>
       01  WSAA-X                      PIC 9(3) COMP-3.
       01  WSAA-ERROR-FLAG             PIC X.
      *01  WSAA-ANB                    PIC S9(02) VALUE ZERO.           <LA4754>
       01  WSAA-ANB                    PIC S9(03) VALUE ZERO.           <LA4754>
       01  WSAA-TODAY                  PIC S9(08).                      <A002>
      *
       01  WSBB-ANB-INT                PIC 9(11) VALUE 0.
      *
       01  SUB                         PIC 99 VALUE ZERO.               <V71L12>
       01  WSAA-TOLERANCE-APP          PIC S9(02)V99.                   <003>
       01  WSAA-TOLERANCE-APP2         PIC S9(02)V99.                   <V42013>
       01  WSAA-SUB                    PIC 9(03)    COMP-3.             <003>
       01  WSBB-SUB                    PIC 99.                          <014>
      *01  WSAA-DIFF                   PIC 9(11)V99 COMP-3.     <D96NUM><003>
      *01  WSAA-CALC-TOLERANCE         PIC S9(11)V99 COMP-3.    <D96NUM><003>
      *01  WSAA-AMOUNT-LIMIT           PIC S9(11)V99 COMP-3.    <D96NUM><003>
      *01  WSAA-TOLERANCE              PIC S9(11)V99 COMP-3.    <D96NUM><003>
       01  WSAA-DIFF                   PIC 9(15)V99 COMP-3.             <D96NUM>
       01  WSAA-CALC-TOLERANCE         PIC S9(15)V99 COMP-3.            <D96NUM>
       01  WSAA-CALC-TOLERANCE2        PIC S9(15)V99 COMP-3.            <V42013>
       01  WSAA-AMOUNT-LIMIT           PIC S9(15)V99 COMP-3.            <D96NUM>
       01  WSAA-AMOUNT-LIMIT2          PIC S9(15)V99 COMP-3.            <V42013>
       01  WSAA-TOLERANCE              PIC S9(15)V99 COMP-3.            <D96NUM>
       01  WSAA-TOLERANCE2             PIC S9(15)V99 COMP-3.            <V42013>
      *01  WSAA-CNT-SUSPENSE           PIC S9(11)V99 COMP-3.    <D96NUM><014>
      *01  WSAA-PAYR-SUSPENSE          PIC S9(11)V99 COMP-3.    <D96NUM><014>
      *01  WSAA-AMNT-DUE               PIC S9(11)V99 COMP-3.    <D96NUM><014>
       01  WSAA-CNT-SUSPENSE           PIC S9(15)V99 COMP-3.            <D96NUM>
       01  WSAA-PAYR-SUSPENSE          PIC S9(15)V99 COMP-3.            <D96NUM>
       01  WSAA-AMNT-DUE               PIC S9(15)V99 COMP-3.            <D96NUM>
      *01  WSAA-TOTAL-PREMIUM          PIC S9(11)V99 COMP-3.    <D96NUM><014>
       01  WSAA-TOTAL-PREMIUM          PIC S9(15)V99 COMP-3.            <D96NUM>
      *01  WSAA-TOTAL-SUSPENSE         PIC S9(11)V99 COMP-3.    <D96NUM><014>
       01  WSAA-TOTAL-SUSPENSE         PIC S9(15)V99 COMP-3.            <D96NUM>
       01  WSAA-LAST-PAYER             PIC 9.                           <014>
       01  WSAA-INSUFFICIENT-SUSPENSE  PIC X VALUE 'N'.                 <014>
      *01  WSAA-TOTAMNT                PIC S9(11)V99 COMP-3.    <D96NUM><003>
       01  WSAA-TOTAMNT                PIC S9(15)V99 COMP-3.            <CAS1.0>
       01  WSAA-PAYRNUM                PIC X(08).                       <008>
       01  WSAA-SING-PRM-IND           PIC X(01) VALUE 'N'.             <019>
      *01  WSAA-SINGP-FEE              PIC S9(11)V9(02)   .     <D96NUM><019>
       01  WSAA-SINGP-FEE              PIC S9(15)V9(02)   .             <D96NUM>
       01  WSAA-SUSP-IND               PIC X.                           <CAS1.0>
       01  WSAA-T5667-KEY.                                              <003>
           03  WSAA-T5667-TRANCD       PIC X(4).                        <003>
           03  WSAA-T5667-CURR         PIC X(4).                        <003>
       01  WSAA-AGT-TERMINATED-FLAG    PIC X.                           <V42013>
      *                                                                 <V76L01>
       01 WSAA-BNYTYPES.                                                <V76L01>
            05  WSAA-BNYTYPE                  PIC X(00002)              <V76L01>
                                               OCCURS 10 .              <V76L01>
       01  WSAA-MANDATORYS.                                             <V76L01>
           05 WSAA-MANDATORY           PIC X                            <V76L01>
                                       OCCURS 30.                       <V76L01>
                                                                        <V76L01>
       01  WSAA-FOUND                  PIC X(01) VALUE SPACES.          <V76L01>
           88  FOUND                             VALUE 'Y'.             <V76L01>
           88  NOT-FOUND                         VALUE 'N'.             <V76L01>
      *                                                                 <V76L01>
       01  WSAA-ITC                    PIC 9(03)    COMP-3.             <V76L01>
       01  WSAA-IUC                    PIC 9(03)    COMP-3.             <V76L01>
       01  WSAA-IVC                    PIC 9(03)    COMP-3.             <V76L01>
       01  WSAA-IWC                    PIC 9(03)    COMP-3.             <V76L01>
       01  WSAA-IZC                    PIC 9(03)    COMP-3.             <V76L01>
      *                                                                 <V72L07>
       01  WSAA-IXC                    PIC 9(03)    COMP-3.             <V72L07>
       01  WSAA-IYC                    PIC 9(03)    COMP-3.             <V72L07>
       01  WSAA-PROD-ERROR.                                             <V72L07>
           03  WSAA-PRODTYP             PIC X(04).                      <V72L07>
           03  FILLER                   PIC X(01) VALUE ' '.            <V72L07>
           03  WSAA-PROD-ERR            PIC X(19).                      <V72L07>
                                                                        <V72L07>
<004> *01  WSAA-T3625-KEY.                                         <004><025>
<004> **** 03  WSAA-T3625-LP           PIC X(02).                  <004><025>
<004> **** 03  WSAA-T3625-BILLCHNL     PIC X(01).                  <004><025>
<004> **** 03  WSAA-T3625-BILLFREQ     PIC X(02).                  <004><025>
      *                                                                 <014>
       01 WSAA-PAYRKEY.                                                 <014>
          03 WSAA-CHDRNUM              PIC X(8).                        <014>
          03 WSAA-PAYRSEQNO            PIC X.                           <014>
      *                                                                 <014>
       01  WSAA-BILLING-INFORMATION.                                    <014>
           03 WSAA-BILLING-DETAILS OCCURS 9 TIMES.                      <014>
              05 WSAA-INCOME-SEQ-NO   PIC 99.                           <014>
              05 WSAA-BILLFREQ        PIC X(02).                        <014>
              05 WSAA-BILLCHNL        PIC X(02).                        <014>
              05 WSAA-BILLCD          PIC S9(08) COMP-3.                <014>
              05 WSAA-BTDATE          PIC S9(08) COMP-3.                <014>
              05 WSAA-BILLCURR        PIC X(03).                        <014>
              05 WSAA-CLNTCOY         PIC X(01).                        <014>
              05 WSAA-CLNTNUM         PIC X(08).                        <014>
              05 WSAA-MANDREF         PIC X(05).                        <014>
              05 WSAA-GRUPKEY         PIC X(12).                        <014>
      ****    05 WSAA-REGPREM         PIC S9(11)V9(2) COMP-3.   <D96NUM><014>
      ****    05 WSAA-SINGP           PIC S9(11)V9(2) COMP-3.   <D96NUM><014>
              05 WSAA-REGPREM         PIC S9(15)V9(2) COMP-3.           <D96NUM>
              05 WSAA-SINGP           PIC S9(15)V9(2) COMP-3.           <D96NUM>
              05 WSAA-SP-TAX          PIC S9(15)V9(2) COMP-3.           <V74L01>
              05 WSAA-RP-TAX          PIC S9(15)V9(2) COMP-3.           <V74L01>
      ****    05 WSAA-INSTPRM         PIC S9(11)V9(2) COMP-3.   <D96NUM><014>
              05 WSAA-INSTPRM         PIC S9(15)V9(2) COMP-3.           <D96NUM>
              05 WSAA-PREM-TAX        PIC S9(15)V9(2) COMP-3.           <V74L01>
      ****    05 WSAA-TAXRELAMT       PIC S9(11)V9(2) COMP-3.   <D96NUM><014>
              05 WSAA-TAXRELAMT       PIC S9(15)V9(2) COMP-3.           <D96NUM>
              05 WSAA-INREVNUM        PIC X(08).                        <014>
  *****                                                            <022>
  *****01  WSAA-PREM-DATE1             PIC 9(08).                  <022><012>
  *****01  FILLER REDEFINES WSAA-PREM-DATE1.                       <022><012>
  *****    03  WSAA-PREM-YEAR1         PIC 9(04).                  <022><012>
  *****    03  WSAA-PREM-MTH1          PIC 9(02).                  <022><012>
  *****    03  WSAA-PREM-DAY1          PIC 9(02).                  <022><012>
  *****                                                            <022><012>
  *****01  WSAA-PREM-DATE2             PIC 9(08).                  <022><012>
  *****01  FILLER REDEFINES WSAA-PREM-DATE2.                       <022><012>
  *****    03  WSAA-PREM-YEAR2         PIC 9(04).                  <022><012>
  *****    03  WSAA-PREM-MTH2          PIC 9(02).                  <022><012>
  *****    03  WSAA-PREM-DAY2          PIC 9(02).                  <022><012>

       01  WSAA-RISK-CESS-DATE          PIC S9(08) VALUE 0.             <023>
       01  WSAA-PREM-CESS-DATE          PIC S9(08) VALUE 0.             <023>
       01  WSAA-COVT-COVERAGE           PIC X(2) VALUE ' '.             <023>
       01  WSAA-COVT-LIFE               PIC X(2) VALUE SPACES.          <027>
       01  SUB1                         PIC S9(03) COMP-3.              <CAS1.0>
       01  WSAA-WAIVE-IT                PIC X(01).                      <CAS1.0>
       01  WSAA-WAIVE-SUMINS            PIC S9(11)V9(02) COMP-3.        <TN01>
       01  WSAA-WAIVE-CONT             PIC X(01).                       <TN01>
       01  WSAA-ZRWVFLGS.                                               <TN01>
    ***    03  WSAA-ZRWVFLG            PIC X(00001) OCCURS 03.  <V42003><TN01>
    ***    03  WSAA-ZRWVFLG            PIC X(00001) OCCURS 04.  <PHE003><V42003>
           03  WSAA-ZRWVFLG            PIC X(00001) OCCURS 05.          <PHE003>
       01  WSAA-TR517-REC              PIC X(250).                      <TN01>
                                                                        <V74L01>
       01  WSAA-TAX                     PIC S9(15)V9(02) COMP-3.        <V74L01>
       01  WSAA-TOTAL-TAX               PIC S9(15)V9(02) COMP-3.        <V74L01>
       01  WSAA-CNTFEE-TAX              PIC S9(15)V9(02) COMP-3.        <V74L01>
       01  WSAA-SINGPFEE-TAX            PIC S9(15)V9(02) COMP-3.        <V74L01>
                                                                        <A05743>
       01  WSAA-AGENT-ERROR.                                            <A05743>
           03  WSAA-AGNTNUM             PIC X(08).                      <A05743>
           03  FILLER                   PIC X(01) VALUE SPACES.         <A05743>
           03  WSAA-ERRMESG             PIC X(15).                      <A05743>
                                                                        <A05743>
       01  WSAA-LIFE                    PIC X(02).                      <CAS1.0>

       01  WSAA-TH611-ITEM.                                             <V4L009>
           03  WSAA-TH611-CNTTYPE       PIC X(03).                      <V4L009>
           03  WSAA-TH611-CURRCODE      PIC X(04).                      <V4L009>

       01  WSAA-MODAL-PREMIUM           PIC S9(13)V99 COMP-3.           <V4L009>
      *                                                                 <V4L009>
       01  WSAA-AGENT-SUSPEND           PIC X(01) VALUE 'N'.            <S01>
       01  WSAA-BLACK-LIST              PIC X(01).                      <S01>
           88  NO-ISSUE                 VALUE 'I' 'X'.                  <S01>
      *    88  NO-COMMISSION            VALUE 'C' 'X'.                  <S01>
                                                                        <V42003>
       01  WSAA-MAIN-LIFE-DETS.                                         <V42003>
           03 WSAA-ANB-AT-CCD          PIC S9(03) COMP-3.               <V42003>
           03 WSAA-CLTDOB              PIC S9(08) COMP-3.               <V42003>
           03 WSAA-SEX                 PIC X(01).                       <V42003>
                                                                        <V42003>
       01  WSBB-JOINT-LIFE-DETS.                                        <V42003>
           03 WSBB-ANB-AT-CCD          PIC S9(03) COMP-3.               <V42003>
           03 WSBB-CLTDOB              PIC S9(08) COMP-3.               <V42003>
           03 WSBB-SEX                 PIC X(01).                       <V42003>
                                                                        <V42003>
       01  WSAA-RISK-CESS-TERM         PIC 999.                         <V42003>
       01  WSAA-MAIN-LIFE              PIC X(2) VALUE SPACES.           <V42003>
                                                                        <V42003>
       01  WSAA-PREM-STATUZ            PIC X.                           <V42003>
           88 PREM-REQD                VALUE 'Y'.                       <V42003>
           88 PREM-NOT-REQD            VALUE 'N'.                       <V42003>
           88 USER-PREM-ENTERED        VALUE 'U'.                       <V42003>
      *                                                                 <V42003>
       01  WSAA-MAIN-CRTABLE           PIC X(04).                       <V42003>
       01  WSAA-MAIN-COVERAGE          PIC X(02).                       <V42003>
       01  WSAA-MAIN-CESSDATE          PIC 9(08).                       <V42003>
       01  WSAA-MAIN-PCESSDTE          PIC 9(08).                       <V42003>
       01  WSAA-MAIN-MORTCLASS         PIC X(01).                       <V42003>
                                                                        <V42003>
       01  WSZZ-RATED-LIFE-DETS.                                        <V42003>
           03 WSZZ-ANB-AT-CCD          PIC S9(03) COMP-3.               <V42003>
           03 WSZZ-CLTDOB              PIC S9(08) COMP-3.               <V42003>
           03 WSZZ-SEX                 PIC X(01).                       <V42003>
       01  WSAA-CLNTNUM-IO             PIC X(08).                       <A06596>
       01  WSAA-EXTRA-MSGPFX           PIC X(02).                       <A06596>
      *                                                                 <V71L12>
       01  WSAA-REVISIT-QUESTIONNAIRE  PIC X(01).                       <V71L12>
           88  WSAA-REVISIT-Q          VALUE 'Y'.                       <V71L12>
       01  WSAA-QUESTSET               PIC X(08).                       <V71L12>
      *                                                                 <V71L12>
       01  WSAA-UNDWFLAG               PIC X(01)  VALUE 'N'.            <V71L12>
           88  UNDWFLAG-YES                       VALUE 'Y'.            <V71L12>
           88  UNDWFLAG-NO                        VALUE 'N'.            <V71L12>
       01  WSAA-OCCUP.                                                  <NB043>
           03  WSAA-OCCUP-MOR          PIC X(01).                       <NB043>
               88  WSAA-OCCUP5                    VALUE '5'.            <NB043>
           03  WSAA-OTHERS             PIC X(03).                       <NB043>
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
                                                                        <NB010>
       01 IDX                          PIC 9(02).                       <NB010>
       01 WSAA-SERVAGNT                PIC X(08).                       <NB010>
       01 WSAA-COWNNUM                 PIC X(08).                       <NB010>
       01 WSAA-WARNING                 PIC X(01).                       <NB010>
       01 WSAA-K                       PIC X(01).                       <NB010>
       01 WSAA-CLTNUM                  PIC X(08).                       <NB010>
       01 WSAA-LIFCNUMS.                                                <NB010>
           03  WSAA-LIFCNUM            PIC X(08) OCCURS 10 .            <NB010>

       01  WSAA-BATCKEY.
           COPY BATCKEY.
      *
       01  TABLES.
           03  T5645                   PIC X(05) VALUE 'T5645'.
           03  T3695                   PIC X(05) VALUE 'T3695'.
           03  T5688                   PIC X(05) VALUE 'T5688'.
           03  T5674                   PIC X(05) VALUE 'T5674'.
           03  T5687                   PIC X(05) VALUE 'T5687'.
           03  T5661                   PIC X(05) VALUE 'T5661'.
           03  T5667                   PIC X(05) VALUE 'T5667'.         <003>
           03  T3678                   PIC X(05) VALUE 'T3678'.         <008>
           03  T6640                   PIC X(05) VALUE 'T6640'.         <014>
           03  T6687                   PIC X(05) VALUE 'T6687'.         <014>
           03  T3620                   PIC X(05) VALUE 'T3620'.         <025>
           03  TR517                   PIC X(05) VALUE 'TR517'.         <CAS1.0>
           03  TH506                   PIC X(05) VALUE 'TH506'.         <CAS1.0>
<004> **** 03  T3625                   PIC X(05) VALUE 'T3625'.    <004><025>
           03  TH611                   PIC X(05) VALUE 'TH611'.         <V4L009>
           03  T5675                   PIC X(05) VALUE 'T5675'.         <V42003>
           03  TR675                   PIC X(05) VALUE 'TR675'.         <V71L12>
           03  T6771                   PIC X(05) VALUE 'T6771'.         <V71L12>
           03  TR52D                   PIC X(05) VALUE 'TR52D'.         <V74L01>
           03  TR52E                   PIC X(05) VALUE 'TR52E'.         <V74L01>
           03  TR52Z                   PIC X(05) VALUE 'TR52Z'.         <V76L01>
           03  TV071                   PIC X(05) VALUE 'TV071'.         <PHE003>

      *
       01  FORMATS.
           03  CHDRLNBREC              PIC X(10) VALUE 'CHDRLNBREC'.
           03  CLTSREC                 PIC X(10) VALUE 'CLTSREC'.
           03  LIFELNBREC              PIC X(10) VALUE 'LIFELNBREC'.
           03  COVTLNBREC              PIC X(10) VALUE 'COVTLNBREC'.
           03  ITEMREC                 PIC X(10) VALUE 'ITEMREC'.
           03  DESCREC                 PIC X(10) VALUE 'DESCREC'.
           03  FLUPLNBREC              PIC X(10) VALUE 'FLUPLNBREC'.
      *****03  SACSLNBREC              PIC X(10) VALUE 'SACSLNBREC'.    <015>
      **** 03  ACBLREC                 PIC X(10) VALUE 'ACBLREC'.  <015><CAS1.0>
           03  ACBLENQREC              PIC X(10) VALUE 'ACBLENQREC'.    <CAS1.0>
           03  CLBLREC                 PIC X(10) VALUE 'CLBLREC'.       <N05>
           03  GRPSREC                 PIC X(10) VALUE 'GRPSREC'.       <N05>
           03  MANDLNBREC              PIC X(10) VALUE 'MANDLNBREC'.    <008>
           03  HPADREC                 PIC X(10) VALUE 'HPADREC'.       <CAS1.0>
           03  PAYRREC                 PIC X(10) VALUE 'PAYRREC'.       <014>
           03  CLRFREC                 PIC X(10) VALUE 'CLRFREC'.       <014>
           03  RTRNSACREC              PIC X(10) VALUE 'RTRNSACREC'.    <014>
           03  COVTTRMREC              PIC X(10) VALUE 'COVTTRMREC'.    <023>
           03  UTRNRNLREC              PIC X(10) VALUE 'UTRNRNLREC'.    <024>
           03  AGLFREC                 PIC X(10) VALUE 'AGLFREC'.       <A05743>
           03  AGNTREC                 PIC X(10) VALUE 'AGNTREC'.       <A05743>
           03  PCDDLNBREC              PIC X(10) VALUE 'PCDDLNBREC'.    <A05743>
           03  HITRRNLREC              PIC X(10) VALUE 'HITRRNLREC'.    <INTBR>
           03  COVTRASREC              PIC X(10) VALUE 'COVTRASREC'.    <INTBR>
           03  TTRCREC                 PIC X(10) VALUE 'TTRCREC'.       <N003>
           03  BNFYLNBREC              PIC X(10) VALUE 'BNFYLNBREC'.    <A07303>
           03  UNDCREC                 PIC X(07) VALUE 'UNDCREC'.       <V71L12>
           03  UNDLREC                 PIC X(07) VALUE 'UNDLREC'.       <V71L12>
           03  UNDQREC                 PIC X(07) VALUE 'UNDQREC'.       <V71L12>
           03  ASGNLNBREC              PIC X(10) VALUE 'ASGNLNBREC'.    <A06596>
           03  CTRSREC                 PIC X(10) VALUE 'CTRSREC'.       <A06596>
           03  ZDISREC                 PIC X(10) VALUE 'ZDISREC'.       <PHE003>
           03  CLRRFCCREC              PIC X(10) VALUE 'CLRRFCCREC'.    <NB010>
           03  COVTRBNREC              PIC X(10) VALUE 'COVTRBNREC'.    <NB043>
           03  LIFERNLREC              PIC X(10) VALUE 'LIFERNLREC'.    <NB043>

           COPY VARCOM.
      *
           COPY SYSERRREC.
      *
           COPY OPSTATSREC.
      *
      ***  COPY SCRNPARAMS.                                             <S9503>
      /
           COPY VLPDRULREC.                                             <V72L07>
      /                                                                 <V72L07>
           COPY SMTPFXCPY.                                              <V76L01>
      /                                                                 <V76L01>
           COPY T5645REC.
      /
           COPY T3695REC.
      /
           COPY T5688REC.
      /
           COPY T5674REC.
      /
           COPY T5687REC.
      /
           COPY T5661REC.
      /
           COPY T5667REC.                                               <003>
      /
<004> **** COPY T3625REC.                                          <004><025>
      /
           COPY T3678REC.                                               <008>
      /                                                                 <014>
           COPY T6640REC.                                               <014>
      /                                                                 <014>
           COPY T6687REC.                                               <014>
      /                                                                 <025>
           COPY T3620REC.                                               <025>
      /
           COPY TR517REC.                                               <CAS1.0>
      /                                                                 <CAS1.0>
           COPY TH506REC.                                               <CAS1.0>
      /                                                                 <CAS1.0>
           COPY TH611REC.                                               <V4L009>
      /                                                                 <CAS1.0>
           COPY TR675REC.                                               <V71L12>
      /                                                                 <V71L12>
           COPY T6771REC.                                               <V71L12>
      /                                                                 <V71L12>
           COPY TR52DREC.                                               <V74L01>
      /                                                                 <V74L01>
           COPY TR52EREC.                                               <V74L01>
      /                                                                 <V74L01>
           COPY TR52ZREC.                                               <V76L01>
      /                                                                 <PHE003>
           COPY TV071REC.                                               <PHE003>
           COPY COVTRBNSKM.                                             <NB043>
           COPY LIFERNLSKM.                                             <NB043>
      /                                                                 <V76L01>
           COPY CHDRLNBSKM.
      /
           COPY MANDLNBSKM.
      /
           COPY CLTSSKM.
      /
           COPY LIFELNBSKM.
      /
           COPY COVTLNBSKM.
      /
           COPY COVTTRMSKM.                                             <023>
      /
           COPY UTRNRNLSKM.                                             <024>
      /
           COPY ITEMSKM.
      /
           COPY ITDMSKM.
      /
           COPY DESCSKM.
           COPY ZDISSKM.                                                <PHE003>
      /
           COPY DATCON1REC.
      /
           COPY DATCON3REC.
      /
           COPY BNFYLNBSKM.                                             <A07303>
      /                                                                 <029>
           COPY AGECALCREC.                                             <029>
      /
           COPY ERRMESGREC.
      /
           COPY FLUPLNBSKM.
      /
      *****COPY SACSLNBSKM.                                             <015>
      **** COPY ACBLSKM.                                           <015><CAS1.0>
           COPY ACBLENQSKM.                                             <CAS1.0>
      /                                                                 <CAS1.0>
           COPY CONLINKREC.                                             <CAS1.0>
      /                                                                 <CAS1.0>
           COPY HPADSKM.                                                <CAS1.0>
      /
           COPY MGFEELREC.
      /
           COPY CLBLSKM.                                                <N05>
      /
           COPY GRPSSKM.                                                <N05>
      /                                                                 <014>
           COPY PAYRSKM.                                                <014>
      /                                                                 <014>
           COPY CLRFSKM.                                                <014>
      /                                                                 <014>
           COPY PRASREC.                                                <014>
      /                                                                 <014>
           COPY RTRNSACSKM.                                             <014>
      /                                                                 <A05743>
           COPY AGLFSKM.                                                <A05743>
      /                                                                 <A05743>
           COPY AGNTSKM.                                                <A05743>
      /                                                                 <A05743>
           COPY PCDDLNBSKM.                                             <A05743>
      /
           COPY CSNCALCREC.                                             <R96REA>
      /                                                                 <R96REA>
           COPY HITRRNLSKM.                                             <INTBR>
      /                                                                 <INTBR>
           COPY HCRTFUPREC.                                             <LA2103>
      /                                                                 <FUPLET>
      ***  COPY S6378SCR.                                               <S9503>
           COPY RLPDLONREC.                                             <V4L001>
      /
           COPY PREMIUMREC.                                             <V42003>
      /
           COPY TTRCSKM.                                                <N003>
           COPY CHKRLREC.                                               <V42006>
      /
           COPY T5675REC.                                               <V42003>
      /
           COPY FREQCPY.                                                <V42003>
      /                                                                 <V71L12>
           COPY UNDCSKM.                                                <V71L12>
      /                                                                 <V71L12>
           COPY UNDLSKM.                                                <V71L12>
      /                                                                 <V71L12>
           COPY UNDQSKM.                                                <V71L12>
      /                                                                 <V71L12>
           COPY UNDWSUBREC.                                             <V71L12>
      /
           COPY TXCALCREC.                                              <V74L01>
      /                                                                 <V74L01>
           COPY ASGNLNBSKM.                                             <A06596>
           COPY CTRSSKM.                                                <A06596>
           COPY ZRDECPLREC.                                             <V76F06>
           COPY CLRRFCCSKM.                                             <NB010>
      /                                                                 <A06596>
       LINKAGE SECTION.
      * Screen copybooks are now part of the linkage.                   <S9503>
      /                                                                 <S9503>
           COPY SCRNPARAMS.                                             <S9503>
      /                                                                 <S9503>
           COPY S6378SCR.                                               <S9503>

           COPY WSSPCOMN.

      *    COPY WSSPSMART.
       01  WSSP-FILLER                 PIC X(768).
      /
      * Statement now includes screen copybooks.                        <S9503>
       PROCEDURE DIVISION USING WSSP-COMMON-AREA WSSP-FILLER            <S9503>
                                               SCRN-SCREEN-PARAMS       <S9503>
                                               S6378-DATA-AREA          <S9503>
                                               S6378-SUBFILE-AREA   .   <S9503>

      *                                                                 <S9503>
      * MAINF has been replaced by MAING as the screen                  <S9503>
      * or driver now calls the program.                                <S9503>
      *                                                                 <S9503>
           COPY MAING.                                                  <S9503>
      /
           COPY CONFNAME.
      /
      *****************************************************************
      *      INITIALISE FIELDS FOR SHOWING ON SCREEN
      *****************************************************************
      *
       1000-INITIALISE SECTION.
      *************************
      *
       1010-INITIALISE.

           MOVE WSSP-TRANID            TO VRCM-TRANID.                  <FUPLET>
           MOVE SPACES                 TO S6378-DATA-AREA.
           MOVE SPACES                 TO S6378-SUBFILE-AREA.
           MOVE 'N'                    TO WSAA-ERROR-FLAG.
           MOVE 'N'                    TO WSAA-SING-PRM-IND             <019>
           MOVE ZERO                   TO WSAA-SINGP-FEE                <019>
      **** MOVE 0                      TO WSAA-SINGP.                   <014>
      **** MOVE 0                      TO WSAA-INSTPRM.                 <014>
      **** MOVE 0                      TO WSAA-REGPREM.                 <014>
           MOVE 0                      TO WSAA-FACTOR.                  <014>
           MOVE 0                      TO WSAA-TOTAL-PREMIUM.           <014>
           MOVE 0                      TO WSAA-TOTAL-SUSPENSE.          <014>
           MOVE 'N'                    TO WSAA-INSUFFICIENT-SUSPENSE.   <014>
           MOVE 0                      TO S6378-PAYRSEQNO.              <014>
           MOVE 0                      TO WSAA-MODAL-PREMIUM.           <V4L009>
                                                                        <NB010>
           MOVE 0                      TO IDX.                          <NB010>
           MOVE SPACES                 TO WSAA-SERVAGNT                 <NB010>
                                          WSAA-COWNNUM                  <NB010>
                                          WSAA-WARNING                  <NB010>
                                          WSAA-K                        <NB010>
                                          WSAA-CLTNUM                   <NB010>
                                          WSAA-LIFCNUMS.                <NB010>
                                                                        <V74L01>
           MOVE 0                      TO WSAA-TAX                      <V74L01>
                                          WSAA-TOTAL-TAX                <V74L01>
                                          WSAA-CNTFEE-TAX               <V74L01>
                                          WSAA-SINGPFEE-TAX.            <V74L01>
                                                                        <V76L01>
           MOVE 0                      TO WSAA-ITC                      <V76L01>
                                          WSAA-IUC                      <V76L01>
                                          WSAA-IVC                      <V76L01>
                                          WSAA-IWC                      <V76L01>
                                          WSAA-IXC                      <V76L01>
                                          WSAA-IYC.                     <V76L01>
                                                                        <V74L01>
           MOVE 0                      TO S6378-TAXAMT-01.              <V74L01>
           MOVE 0                      TO S6378-TAXAMT-02.              <V74L01>
           MOVE 'Y'                    TO S6378-TAXAMT01-OUT (ND)       <V74L01>
                                          S6378-TAXAMT02-OUT (ND)       <V74L01>
                                          S6378-TAXAMT01-OUT (PR)       <V74L01>
                                          S6378-TAXAMT02-OUT (PR).      <V74L01>
                                                                        <V74L01>
           MOVE 1 TO WSBB-SUB.                                          <014>
                                                                        <014>
           PERFORM UNTIL WSBB-SUB > 9                                   <014>
              MOVE ZEROS               TO WSAA-INCOME-SEQ-NO(WSBB-SUB), <014>
                                          WSAA-BILLCD(WSBB-SUB),        <014>
                                          WSAA-BTDATE(WSBB-SUB),        <014>
                                          WSAA-REGPREM(WSBB-SUB),       <014>
                                          WSAA-SINGP(WSBB-SUB),         <014>
                                          WSAA-SP-TAX(WSBB-SUB),        <V74L01>
                                          WSAA-RP-TAX(WSBB-SUB),        <V74L01>
                                          WSAA-INSTPRM(WSBB-SUB),       <014>
                                          WSAA-PREM-TAX(WSBB-SUB),      <V74L01>
                                          WSAA-TAXRELAMT(WSBB-SUB)      <014>
              MOVE SPACES              TO WSAA-BILLFREQ(WSBB-SUB),      <014>
                                          WSAA-BILLCHNL(WSBB-SUB),      <014>
                                          WSAA-BILLCURR(WSBB-SUB),      <014>
                                          WSAA-CLNTNUM(WSBB-SUB),       <014>
                                          WSAA-CLNTCOY(WSBB-SUB),       <014>
                                          WSAA-INREVNUM(WSBB-SUB),      <014>
                                          WSAA-MANDREF(WSBB-SUB),       <014>
                                          WSAA-GRUPKEY(WSBB-SUB)        <014>
              ADD 1                    TO WSBB-SUB                      <014>
           END-PERFORM.                                                 <014>

           MOVE SCLR                   TO SCRN-FUNCTION.
           CALL 'S6378IO' USING SCRN-SCREEN-PARAMS
                                S6378-DATA-AREA
                                S6378-SUBFILE-AREA.
           IF SCRN-STATUZ NOT = O-K
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.
           MOVE 1                      TO SCRN-SUBFILE-RRN.

      **** MOVE ZERO                                                    <CAS1.0>
      ****      TO S6378-CNTFEE                   .                     <CAS1.0>
      **** MOVE ZERO                                                    <CAS1.0>
      ****      TO S6378-CNTSUSP                  .                     <CAS1.0>
      **** MOVE ZERO                                                    <CAS1.0>
      ****      TO S6378-INST-PREM                .                     <CAS1.0>
           MOVE ZERO                   TO S6378-EXRAT                   <CAS1.0>
                                          S6378-INST-PREM               <CAS1.0>
                                          S6378-PREM-CURR               <CAS1.0>
                                          S6378-CNTFEE                  <CAS1.0>
                                          S6378-PUFEE                   <CAS1.0>
                                          S6378-PREMSUSP                <CAS1.0>
                                          WSAA-TOTAMNT.                 <CAS1.0>

           MOVE O-K                    TO WSSP-EDTERROR.                <V72L07>

      *
      *    Set screen fields
      * Copy of P5009 program..  till 2000 section.

       1020-READ-ACC-RULE-TABLE.
      *****
      *    Read the Program  table T5645 for the Financial Accounting
      *    Rules for the transaction.
      *****
           MOVE WSSP-BATCHKEY          TO WSAA-BATCKEY.
           MOVE SPACES                 TO ITEM-DATA-KEY.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.
           MOVE T5645                  TO ITEM-ITEMTABL.
           MOVE WSAA-PROG              TO ITEM-ITEMITEM.
           MOVE READR                  TO ITEM-FUNCTION.

           CALL 'ITEMIO' USING ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = O-K
                                   AND NOT = MRNF
               MOVE ITEM-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
           IF ITEM-STATUZ              = MRNF
      ***     MOVE U016                TO SCRN-ERROR-CODE               <016>
              MOVE G816                TO SCRN-ERROR-CODE               <016>
              MOVE 'Y'                 TO WSSP-EDTERROR
              GO TO 1190-EXIT.
      *
           MOVE ITEM-GENAREA           TO T5645-T5645-REC.
      *
       1020-GET-SIGN.
      *****
      *    Read the table T3695 for the field sign of the 01 posting.
      *****
           MOVE SPACES                 TO ITEM-DATA-KEY.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.
           MOVE T3695                  TO ITEM-ITEMTABL.
           MOVE T5645-SACSTYPE-01      TO ITEM-ITEMITEM.
           MOVE 'READR'                TO ITEM-FUNCTION.


           CALL 'ITEMIO' USING ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = O-K
                                   AND NOT = MRNF
               MOVE ITEM-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
           IF ITEM-STATUZ              = MRNF
              MOVE G228                TO SCRN-ERROR-CODE
              MOVE 'Y'                 TO WSSP-EDTERROR
              GO TO 1190-EXIT.

           MOVE ITEM-GENAREA TO T3695-T3695-REC.
      *
       1020-GET-DATE.
      *****
      *    Get the current date from DATCON1.
      *****
           MOVE TDAY                   TO DTC1-FUNCTION.
           CALL 'DATCON1' USING DTC1-DATCON1-REC.
      *
       1030-RETREIVE-HEADER.
      *****
      *    Read CHDRLNB (retrv) in order to obtain the contract header.
      *****
           MOVE RETRV                  TO CHDRLNB-FUNCTION.
           MOVE CHDRLNBREC             TO CHDRLNB-FORMAT.

           CALL 'CHDRLNBIO' USING CHDRLNB-PARAMS

           IF CHDRLNB-STATUZ           NOT = O-K
              MOVE CHDRLNB-PARAMS      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.
      *                                                                 <CAS1.0>
      * Read HPAD file for Proposal Details                             <CAS1.0>
      *                                                                 <CAS1.0>
           MOVE SPACES                 TO HPAD-DATA-AREA.               <CAS1.0>
           MOVE CHDRLNB-CHDRCOY        TO HPAD-CHDRCOY.                 <CAS1.0>
           MOVE CHDRLNB-CHDRNUM        TO HPAD-CHDRNUM.                 <CAS1.0>
           MOVE HPADREC                TO HPAD-FORMAT.                  <CAS1.0>
           MOVE READR                  TO HPAD-FUNCTION.                <CAS1.0>
      *                                                                 <CAS1.0>
           CALL 'HPADIO'            USING HPAD-PARAMS.                  <CAS1.0>
                                                                        <CAS1.0>
           IF HPAD-STATUZ           NOT = O-K                           <CAS1.0>
              MOVE HPAD-PARAMS         TO SYSR-PARAMS                   <CAS1.0>
              MOVE HPAD-STATUZ         TO SYSR-STATUZ                   <CAS1.0>
              PERFORM 600-FATAL-ERROR                                   <CAS1.0>
           END-IF.                                                      <CAS1.0>
      *                                                                 <CAS1.0>
      * Read table TH506 to see if underwriting decision date is        <CAS1.0>
      * mandatory and therefore check HPAD-HUWDCDATE is set             <CAS1.0>
      *                                                                 <CAS1.0>
           MOVE SPACES                 TO ITEM-DATA-KEY.                <CAS1.0>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <CAS1.0>
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.                 <CAS1.0>
           MOVE TH506                  TO ITEM-ITEMTABL.                <CAS1.0>
           MOVE CHDRLNB-CNTTYPE        TO ITEM-ITEMITEM.                <CAS1.0>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <CAS1.0>
           MOVE READR                  TO ITEM-FUNCTION.                <CAS1.0>
           CALL 'ITEMIO' USING ITEM-PARAMS.                             <CAS1.0>
           IF ITEM-STATUZ              NOT = O-K                        <CAS1.0>
                                   AND NOT = MRNF                       <CAS1.0>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <CAS1.0>
               PERFORM 600-FATAL-ERROR.                                 <CAS1.0>
      *                                                                 <CAS1.0>
           IF ITEM-STATUZ              = MRNF                           <CAS1.0>
              IF  ITEM-ITEMITEM        NOT = '***'                      <CAS1.0>
                MOVE '***'             TO ITEM-ITEMITEM                 <CAS1.0>
                MOVE ITEMREC           TO ITEM-FORMAT                   <CAS1.0>
                MOVE READR             TO ITEM-FUNCTION                 <CAS1.0>
                CALL 'ITEMIO' USING ITEM-PARAMS                         <CAS1.0>
                IF ITEM-STATUZ         NOT = O-K                        <CAS1.0>
                                       AND NOT = MRNF                   <CAS1.0>
                   MOVE ITEM-PARAMS   TO SYSR-PARAMS                    <CAS1.0>
                   PERFORM 600-FATAL-ERROR.                             <CAS1.0>
      *                                                                 <CAS1.0>
           IF ITEM-STATUZ              = O-K                            <CAS1.0>
              MOVE ITEM-GENAREA        TO TH506-TH506-REC               <CAS1.0>
           END-IF.                                                      <CAS1.0>
                                                                        <CAS1.0>
           IF HPAD-HUWDCDTE             = VRCM-MAX-DATE                 <CAS1.0>
           AND TH506-MANDATORY-02       = 'Y'                           <CAS1.0>
              MOVE SPACES              TO ERMS-ERRMESG-REC              <CAS1.0>
              MOVE HL05                TO ERMS-EROR                     <CAS1.0>
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES                               <CAS1.0>
           END-IF.                                                      <CAS1.0>
      *                                                                 <N003>
      * Check Temporary receipt number and date.                        <N003>
      *                                                                 <N003>
           IF TH506-CFLG               NOT = 'Y'                        <N003>
              GO TO 1035-LOAD-PAYER-DETAILS                             <N003>
           END-IF.                                                      <N003>
                                                                        <N003>
           MOVE SPACES                 TO TTRC-PARAMS.                  <N003>
           MOVE CHDRLNB-CHDRCOY        TO TTRC-CHDRCOY.                 <N003>
           MOVE CHDRLNB-CHDRNUM        TO TTRC-CHDRNUM.                 <N003>
           MOVE VRCM-MAX-DATE          TO TTRC-EFFDATE.                 <N003>
           MOVE BEGN                   TO TTRC-FUNCTION.                <N003>
           MOVE TTRCREC                TO TTRC-FORMAT.                  <N003>
                                                                        <N003>
           CALL 'TTRCIO'           USING  TTRC-PARAMS.                  <N003>
                                                                        <N003>
           IF TTRC-STATUZ              NOT = O-K  AND  ENDP             <N003>
              MOVE TTRC-STATUZ         TO SYSR-STATUZ                   <N003>
              MOVE TTRC-PARAMS         TO SYSR-PARAMS                   <N003>
              PERFORM 600-FATAL-ERROR                                   <N003>
           END-IF.                                                      <N003>
                                                                        <N003>
           IF TTRC-STATUZ              = ENDP                           <N003>
           OR TTRC-CHDRCOY             NOT = CHDRLNB-CHDRCOY            <N003>
           OR TTRC-CHDRNUM             NOT = CHDRLNB-CHDRNUM            <N003>
               MOVE SPACES             TO ERMS-ERRMESG-REC              <N003>
               MOVE TL15               TO ERMS-EROR                     <N003>
               MOVE SPACES             TO WSAA-EXTRA-MSGPFX             <A06596>
               PERFORM 1800-ERROR-MESSAGES                              <N003>
           ELSE                                                         <N003>
              IF TTRC-TTMPRCNO         = SPACES                         <N003>
                 MOVE SPACES           TO ERMS-ERRMESG-REC              <N003>
                 MOVE TL15             TO ERMS-EROR                     <N003>
                 MOVE SPACES           TO WSAA-EXTRA-MSGPFX             <A06596>
                 PERFORM 1800-ERROR-MESSAGES                            <N003>
              ELSE                                                      <N003>
              IF TTRC-TTMPRCDTE        = VRCM-MAX-DATE                  <N003>
                 MOVE SPACES           TO ERMS-ERRMESG-REC              <N003>
                 MOVE TL16             TO ERMS-EROR                     <N003>
                 MOVE SPACES           TO WSAA-EXTRA-MSGPFX             <A06596>
                 PERFORM 1800-ERROR-MESSAGES                            <N003>
              END-IF                                                    <N003>
           END-IF.                                                      <N003>
                                                                        <N003>
       1035-LOAD-PAYER-DETAILS.                                         <014>
      *****                                                             <014>
      * Load the WS table by reading all the payer                      <014>
      * and payer role recored for this contract.                       <014>
      *****                                                             <014>
                                                                        <014>
           MOVE SPACES                 TO PAYR-DATA-AREA.               <014>
           MOVE CHDRLNB-CHDRCOY        TO PAYR-CHDRCOY.                 <014>
           MOVE CHDRLNB-CHDRNUM        TO PAYR-CHDRNUM.                 <014>
           MOVE 1                      TO PAYR-VALIDFLAG.               <014>
           MOVE 1                      TO PAYR-PAYRSEQNO.               <014>
           MOVE PAYRREC                TO PAYR-FORMAT.                  <014>
           MOVE BEGN                   TO PAYR-FUNCTION.                <014>
                                                                        <014>
           CALL 'PAYRIO' USING PAYR-PARAMS.                             <014>
                                                                        <014>
           IF PAYR-STATUZ NOT = O-K                                     <014>
              MOVE PAYR-STATUZ         TO SYSR-STATUZ                   <014>
              MOVE PAYR-PARAMS         TO SYSR-PARAMS                   <014>
              PERFORM 600-FATAL-ERROR.                                  <014>
                                                                        <014>
           PERFORM 1B00-LOAD-PAYER-DETAILS UNTIL                        <014>
               PAYR-CHDRCOY NOT = CHDRLNB-CHDRCOY OR                    <014>
               PAYR-CHDRNUM NOT = CHDRLNB-CHDRNUM OR                    <014>
               PAYR-STATUZ = ENDP.
                                                                        <014>
      *****                                                             <014>
      ****Read T3625 to access the payment plan  details for the contrac<014>
      ****                                                              <014>
<004> **** MOVE SPACES                 TO ITEM-DATA-KEY.                <014>
<004> **** MOVE 'IT'                   TO ITEM-ITEMPFX.                 <014>
<004> **** MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.                 <014>
<004> **** MOVE T3625                  TO ITEM-ITEMTABL.                <014>
<004> **** MOVE 'LP'                   TO WSAA-T3625-LP.                <014>
<004> **** MOVE CHDRLNB-BILLCHNL       TO WSAA-T3625-BILLCHNL.          <014>
<004> **** MOVE CHDRLNB-BILLFREQ       TO WSAA-T3625-BILLFREQ.          <014>
<004> **** MOVE WSAA-T3625-KEY         TO ITEM-ITEMITEM.                <014>
<004> **** MOVE 'READR'                TO ITEM-FUNCTION.                <014>
<004> ****                                                              <014>
<004> **** CALL 'ITEMIO' USING ITEM-PARAMS.                             <014>
<004> ****  F ITEM-STATUZ              NOT = O-K                        <014>
<004> ****                         AND NOT = MRNF                       <014>
<004> ****     MOVE ITEM-STATUZ        TO SYSR-STATUZ                   <014>
<004> ****     PERFORM 600-FATAL-ERROR.                                 <014>
<004> ****                                                              <014>
<004> **** IF ITEM-STATUZ              = MRNF                           <014>
<004> ****    MOVE SPACES              TO ERMS-ERRMESG-REC              <014>
<004> ****    MOVE U023                TO ERMS-EROR                     <014>
<004> ****    PERFORM 1800-ERROR-MESSAGES.                              <014>
<004> ****                                                              <014>
<004> **** MOVE ITEM-GENAREA           TO T3625-T3625-REC.              <014>
      ****                                                              <014>
      *1035-READ-MANDATE-AND-CLBL.                                 <014><008>
      ****                                                         <014><008>
      ****   If method of payment is direct debit read the         <014><008>
      ****   relative mandate record & client bank A/C details     <014><008>
      ****                                                         <014><008>
      ****   IF CHDRLNB-BILLCHNL = 'B'                             <009><008>
      ****  IF T3625-DDIND      = 'Y'                              <014><009>
      ****     MOVE SPACES                 TO MANDLNB-PARAMS       <014><008>
      ****     IF CHDRLNB-PAYRNUM = SPACES                         <014><008>
      ****       MOVE CHDRLNB-COWNCOY      TO MANDLNB-PAYRCOY      <014><008>
      ****       MOVE CHDRLNB-COWNNUM      TO MANDLNB-PAYRNUM      <014><008>
      ****     ELSE                                                <014><008>
      ****        MOVE CHDRLNB-PAYRCOY      TO MANDLNB-PAYRCOY     <014><008>
      ****        MOVE CHDRLNB-PAYRNUM     TO MANDLNB-PAYRNUM      <014><008>
      ****     END-IF                                              <014><008>
      ****     MOVE CHDRLNB-MANDREF        TO MANDLNB-MANDREF      <014><008>
      ****     MOVE READR                  TO MANDLNB-FUNCTION     <014><008>
      ****     MOVE MANDLNBREC             TO MANDLNB-FORMAT       <014><008>
      ****                                                         <014><008>
      ****     CALL 'MANDLNBIO' USING MANDLNB-PARAMS               <014><008>
      ****                                                         <014><008>
      ****     IF MANDLNB-STATUZ          NOT = O-K                <014><008>
      ****        AND MANDLNB-STATUZ      NOT = MRNF               <014><008>
      ****       MOVE MANDLNB-PARAMS      TO SYSR-PARAMS           <014><008>
      ****       PERFORM 600-FATAL-ERROR                           <014><008>
      ****     END-IF                                              <014><008>
      ****                                                         <014><008>
      ****    IF MANDLNB-STATUZ = MRNF                             <014><008>
      ****        MOVE SPACES              TO ERMS-ERRMESG-REC     <014><008>
      ****        MOVE I007                TO ERMS-EROR            <014><008>
      ****        PERFORM 1800-ERROR-MESSAGES                      <014><008>
      ****    END-IF                                               <014><008>
      ****                                                         <014><008>
      **Read CLBL Bank Details.                                    <014><008>
      ****                                                         <014><008>
<008> ****    MOVE CHDRLNB-BANKKEY        TO CLBL-BANKKEY          <014><008>
<008> ****    MOVE CHDRLNB-BANKACCKEY     TO CLBL-BANKACCKEY       <014><008>
      ****    IF MANDLNB-STATUZ = O-K                              <014><008>
      ****       MOVE MANDLNB-BANKKEY        TO CLBL-BANKKEY       <014><008>
      ****       MOVE MANDLNB-BANKACCKEY     TO CLBL-BANKACCKEY    <014><008>
      ****       MOVE READR                  TO CLBL-FUNCTION      <014><008>
      ****                                                         <014><008>
      ****       CALL 'CLBLIO' USING         CLBL-PARAMS           <014><008>
      ****       IF CLBL-STATUZ              NOT = O-K AND         <014><008>
      ****                                   NOT = MRNF            <014><008>
      ****         MOVE CLBL-PARAMS         TO SYSR-PARAMS         <014><008>
      ****         MOVE CLBL-STATUZ         TO SYSR-STATUZ         <014><008>
      ****         PERFORM 600-FATAL-ERROR                         <014><008>
      ****       END-IF                                            <014><008>
      ****                                                         <014><008>
      **If bank A/C not on file display message                    <014><008>
      ****                                                         <014><008>
      ****       IF CLBL-STATUZ              = MRNF                <014><008>
      ****          MOVE SPACES              TO ERMS-ERRMESG-REC   <014><008>
      ****          MOVE F826                TO ERMS-EROR          <014><008>
      ****          PERFORM 1800-ERROR-MESSAGES                    <014><008>
      ****       END-IF                                            <014><008>
      **Read table T3678                                           <014><008>
      ****       MOVE SPACES              TO ITEM-DATA-KEY         <014><008>
      ****       MOVE 'IT'                TO ITEM-ITEMPFX          <014><008>
**********       MOVE WSSP-COMPANY        TO ITEM-ITEMCOY          <014><008>
      ****       MOVE WSSP-FSUCO          TO ITEM-ITEMCOY          <014><013>
      ****       MOVE T3678               TO ITEM-ITEMTABL         <014><008>
      ****       MOVE MANDLNB-MANDSTAT    TO ITEM-ITEMITEM         <014><008>
      ****       MOVE 'READR'             TO ITEM-FUNCTION         <014><008>
      ****                                                         <014><008>
      ****       CALL 'ITEMIO' USING ITEM-PARAMS                   <014><008>
      ****       IF ITEM-STATUZ              NOT = O-K             <014><008>
      ****          MOVE ITEM-PARAMS        TO SYSR-PARAMS         <014><008>
      ****          PERFORM 600-FATAL-ERROR                        <014><008>
      ****       END-IF                                            <014><008>
      ****       MOVE ITEM-GENAREA TO T3678-T3678-REC              <014><008>
      ****                                                         <014><008>
      ****    END-IF                                               <014><008>
      ****    GO TO 1108-CONT.                                     <014><008>
      ****  END-IF.                                                <014><008>
      *
       1040-READ-CONTRACT-LONGDESC.
      *****
      *    Read Contract header Long description  from table T5688 for
      *    the contract type held on CHDRLND.
      *****
           MOVE SPACES                 TO DESC-PARAMS.
           MOVE 'IT'                   TO DESC-DESCPFX.
           MOVE CHDRLNB-CHDRCOY        TO DESC-DESCCOY.
           MOVE 'T5688'                TO DESC-DESCTABL.
           MOVE CHDRLNB-CNTTYPE        TO DESC-DESCITEM.
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.
           MOVE READR                  TO DESC-FUNCTION.
      *
           CALL 'DESCIO' USING DESC-PARAMS.
           IF DESC-STATUZ              NOT = O-K
           AND DESC-STATUZ             NOT = MRNF                       <018>
               MOVE DESC-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.

           IF DESC-STATUZ              = MRNF                           <018>
              MOVE ALL '?'             TO S6378-CTYPEDES                <018>
           ELSE                                                         <018>
              MOVE DESC-LONGDESC       TO S6378-CTYPEDES.               <018>
                                                                        <018>
           MOVE CHDRLNB-CHDRNUM        TO S6378-CHDRNUM.
           MOVE CHDRLNB-CNTTYPE        TO S6378-CNTTYPE.
      *****MOVE DESC-LONGDESC          TO S6378-CTYPEDES.               <018>
           MOVE CHDRLNB-COWNNUM        TO S6378-COWNNUM.

      *
       1050-RETREIVE-CLIENT-DETAILS.
      *****
      *    Read the Client details for the associated Life.
      *****
           MOVE SPACES                 TO CLTS-PARAMS.
           MOVE 'CN'                   TO CLTS-CLNTPFX.
      *    MOVE WSSP-COMPANY           TO CLTS-CLNTCOY.                 <005>
           MOVE WSSP-FSUCO             TO CLTS-CLNTCOY.                 <005>
           MOVE CHDRLNB-COWNNUM        TO CLTS-CLNTNUM.
           MOVE READR                  TO CLTS-FUNCTION.

           CALL 'CLTSIO'               USING CLTS-PARAMS.

           IF CLTS-STATUZ              NOT = O-K
           AND CLTS-STATUZ             NOT = MRNF
              MOVE CLTS-PARAMS         TO SYSR-PARAMS                   <028>
      ****    MOVE CLTS-PARAMS         TO CLTS-PARAMS                   <028>
              PERFORM 600-FATAL-ERROR.
      *
       1060-CLIENT-NAME-FORMAT.
      *****
      *    Format the Client name extracted. Special Format.
      *    SEE Copy Confname in the procedure division.
      *****
           IF CLTS-STATUZ             = MRNF
              OR CLTS-VALIDFLAG         NOT = 1
               MOVE E304                TO S6378-OWNERNAME-ERR
               MOVE SPACES              TO S6378-OWNERNAME
           ELSE
      *        PERFORM PAYEENAME                                        002
               PERFORM PLAINNAME                                        002
               MOVE WSSP-LONGCONFNAME   TO S6378-OWNERNAME.
      *
      *1070-CALC-SUSPENSE.                                              <CAS1.0>
      *****                                                             <CAS1.0>
      *    Code redundant now Suspense is checked for Contract          <CAS1.0>
      *    Currency; Billing Currency or any currency                   <CAS1.0>
      *    Read  the sub account balance for  the contract and apply
      *    the sign for the sub account type.
      *****
      *****                                                             <015>
      *****MOVE SPACES                 TO ACBL-PARAMS.                  <017>
      **** MOVE SPACES                 TO ACBL-DATA-AREA.          <017><CAS1.0>
      **** MOVE ZEROS                  TO ACBL-RLDGACCT.           <015><CAS1.0>
      **** MOVE WSSP-COMPANY           TO ACBL-RLDGCOY.            <015><CAS1.0>
      **** MOVE CHDRLNB-CHDRNUM        TO ACBL-RLDGACCT.           <015><CAS1.0>
      **** MOVE CHDRLNB-CNTCURR        TO ACBL-ORIGCURR.           <015><CAS1.0>
      **** MOVE T5645-SACSCODE-01      TO ACBL-SACSCODE.           <015><CAS1.0>
      **** MOVE T5645-SACSTYPE-01      TO ACBL-SACSTYP.            <015><CAS1.0>
      **** MOVE READR                  TO ACBL-FUNCTION.           <015><CAS1.0>
      ****                                                         <015><CAS1.0>
      **** CALL 'ACBLIO'               USING ACBL-PARAMS.          <015><CAS1.0>
      ****                                                         <015><CAS1.0>
      **** IF  ACBL-STATUZ         NOT = O-K                       <015><CAS1.0>
      **** AND                     NOT = MRNF                      <015><CAS1.0>
      ****    MOVE ACBL-PARAMS         TO SYSR-PARAMS              <015><CAS1.0>
      ****    PERFORM 600-FATAL-ERROR.                             <015><CAS1.0>
                                                                        <015>
      *****MOVE SPACES                 TO SACSLNB-PARAMS.               <015>
      *****MOVE WSSP-COMPANY           TO SACSLNB-CHDRCOY.              <015>
      *****MOVE CHDRLNB-CHDRNUM        TO SACSLNB-CHDRNUM.              <015>
      *****MOVE CHDRLNB-CNTCURR        TO SACSLNB-CNTCURR.              <015>
      *****MOVE T5645-SACSCODE-01      TO SACSLNB-SACSCODE.             <015>
      *****MOVE T5645-SACSTYPE-01      TO SACSLNB-SACSTYP.              <015>
      *****MOVE READR                  TO SACSLNB-FUNCTION.             <015>
      *****                                                             <015>
      *****CALL 'SACSLNBIO'            USING SACSLNB-PARAMS.            <015>
      *****                                                             <015>
      *****IF SACSLNB-STATUZ           NOT = O-K                        <015>
      *****                        AND NOT = MRNF                       <015>
      *****   MOVE SACSLNB-PARAMS      TO SYSR-PARAMS                   <015>
      *****   PERFORM 600-FATAL-ERROR.                                  <015>
      *****                                                             <015>


      *****IF SACSLNB-STATUZ               = MRNF                       <015>
      **** IF ACBL-STATUZ                  = MRNF                  <015><CAS1.0>
      *****   MOVE ZERO          TO S6378-CNTSUSP
      ****    MOVE ZERO          TO WSAA-CNT-SUSPENSE              <014><CAS1.0>
      ****    MOVE ZERO          TO WSAA-TOTAL-SUSPENSE            <014><CAS1.0>
      **** ELSE                                                         <CAS1.0>
      ****    IF T3695-SIGN               = '-'                         <014>
      ****       MULTIPLY SACSLNB-SACSCURBAL BY -1                      <014>
      ****                             GIVING S6378-CNTSUSP             <014>
      ****    ELSE                                                      <014>
      ****       MOVE SACSLNB-SACSCURBAL  TO S6378-CNTSUSP.             <014>
      ****    IF T3695-SIGN               = '-'                    <014><CAS1.0>
      *****      MULTIPLY SACSLNB-SACSCURBAL BY -1                 <014><015>
      ****       MULTIPLY ACBL-SACSCURBAL BY -1                    <015><CAS1.0>
      ****                             GIVING WSAA-CNT-SUSPENSE    <014><CAS1.0>
      ****       MOVE WSAA-CNT-SUSPENSE   TO WSAA-TOTAL-SUSPENSE   <014><CAS1.0>
      ****    ELSE                                                 <014><CAS1.0>
      *****      MOVE SACSLNB-SACSCURBAL  TO WSAA-CNT-SUSPENSE.    <014><015>
      *****      MOVE ACBL-SACSCURBAL     TO WSAA-CNT-SUSPENSE.         <015>
      ****       MOVE ACBL-SACSCURBAL     TO WSAA-CNT-SUSPENSE     <015><CAS1.0>
                                                                        <014>
      ****       MOVE WSAA-CNT-SUSPENSE   TO WSAA-TOTAL-SUSPENSE.  <014><CAS1.0>
      *                                                                 <CAS1.0>
      * Check if a Suspense payment has been made in the Contract       <CAS1.0>
      * Currency, Billing Currency or any currency.                     <CAS1.0>
      *                                                                 <CAS1.0>
           MOVE ZERO                   TO WSAA-CNT-SUSPENSE.            <CAS1.0>
           MOVE 'N'                    TO WSAA-SUSP-IND.                <CAS1.0>
                                                                        <CAS1.0>
           MOVE SPACES                 TO ACBLENQ-DATA-AREA.            <CAS1.0>
           MOVE ZEROS                  TO ACBLENQ-RLDGACCT.             <CAS1.0>
           MOVE WSSP-COMPANY           TO ACBLENQ-RLDGCOY.              <CAS1.0>
           MOVE CHDRLNB-CHDRNUM        TO ACBLENQ-RLDGACCT.             <CAS1.0>
           MOVE T5645-SACSCODE-01      TO ACBLENQ-SACSCODE.             <CAS1.0>
           MOVE T5645-SACSTYPE-01      TO ACBLENQ-SACSTYP.              <CAS1.0>
                                                                        <CAS1.0>
           PERFORM 1C100-CHECK-SUSPENSE   VARYING WSAA-SUB              <CAS1.0>
                                FROM 1 BY 1 UNTIL WSAA-SUB > 3  OR      <CAS1.0>
                                          WSAA-SUSP-IND = 'Y'.          <CAS1.0>
      *                                                                 <V4L001>
      * Look up Advance premium deposit for available amount            <V4L001>
      *                                                                 <V4L001>
           INITIALIZE                      RLPDLON-REC.                 <V4L001>
           MOVE INFO                       TO RLPDLON-FUNCTION.         <V4L001>
           MOVE CHDRLNB-CHDRCOY            TO RLPDLON-CHDRCOY.          <V4L001>
           MOVE CHDRLNB-CHDRNUM            TO RLPDLON-CHDRNUM.          <V4L001>
           MOVE ZEROES                     TO RLPDLON-PRMDEPST.         <V4L001>
           MOVE WSSP-LANGUAGE              TO RLPDLON-LANGUAGE.         <LA3998>
                                                                        <V4L001>
           CALL 'RLPDLON'                  USING RLPDLON-REC.           <V4L001>
                                                                        <V4L001>
           IF RLPDLON-STATUZ               NOT = O-K                    <V4L001>
              MOVE RLPDLON-STATUZ          TO SYSR-STATUZ               <V4L001>
              MOVE RLPDLON-REC             TO SYSR-PARAMS               <V4L001>
              PERFORM 600-FATAL-ERROR                                   <V4L001>
           END-IF.                                                      <V4L001>
           IF RLPDLON-PRMDEPST             > ZEROES                     <V4L001>
              MOVE 'Y'                     TO WSAA-SUSP-IND.            <V4L001>
      *                                                                 <V4L001>
      *
       1080-CALC-CONTRACT-FEE.
      *****                                                             <V74L01>
      *    Read table TR52D using CHDRLNB-REGISTER as key               <V74L01>
      *****                                                             <V74L01>
           MOVE SPACES                 TO ITEM-DATA-KEY.                <V74L01>
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.                 <V74L01>
           MOVE TR52D                  TO ITEM-ITEMTABL.                <V74L01>
           MOVE CHDRLNB-REGISTER       TO ITEM-ITEMITEM.                <V74L01>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <V74L01>
           MOVE READR                  TO ITEM-FUNCTION.                <V74L01>
           CALL 'ITEMIO' USING ITEM-PARAMS.                             <V74L01>
                                                                        <V74L01>
                                                                        <V74L01>
           IF ITEM-STATUZ              NOT = O-K                        <V74L01>
                                       AND NOT = MRNF                   <V74L01>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <V74L01>
               MOVE ITEM-STATUZ        TO SYSR-STATUZ                   <V74L01>
               PERFORM 600-FATAL-ERROR                                  <V74L01>
           END-IF.                                                      <V74L01>
      *                                                                 <V74L01>
           IF ITEM-STATUZ              = MRNF                           <V74L01>
              MOVE SPACES              TO ITEM-DATA-KEY                 <V74L01>
              MOVE WSSP-COMPANY        TO ITEM-ITEMCOY                  <V74L01>
              MOVE TR52D               TO ITEM-ITEMTABL                 <V74L01>
              MOVE '***'               TO ITEM-ITEMITEM                 <V74L01>
              MOVE 'IT'                TO ITEM-ITEMPFX                  <V74L01>
              MOVE READR               TO ITEM-FUNCTION                 <V74L01>
              CALL 'ITEMIO' USING ITEM-PARAMS                           <V74L01>
                                                                        <V74L01>
              IF ITEM-STATUZ           NOT = O-K                        <V74L01>
                 MOVE ITEM-PARAMS      TO SYSR-PARAMS                   <V74L01>
                 MOVE ITEM-STATUZ      TO SYSR-STATUZ                   <V74L01>
                 PERFORM 600-FATAL-ERROR                                <V74L01>
              END-IF                                                    <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           MOVE ITEM-GENAREA           TO TR52D-TR52D-REC.              <V74L01>
                                                                        <V74L01>
      *                                                                 <V74L01>
      *****
      *    Read the contract definition details T5688 for the contract
      *    type held on the contract header.
      *****
           MOVE SPACES                 TO ITDM-DATA-KEY.
           MOVE WSSP-COMPANY           TO ITDM-ITEMCOY.
           MOVE T5688                  TO ITDM-ITEMTABL.
           MOVE CHDRLNB-CNTTYPE        TO ITDM-ITEMITEM.
           MOVE CHDRLNB-OCCDATE        TO ITDM-ITMFRM.
           MOVE 'IT'                   TO ITDM-ITEMPFX.                 <017>
           MOVE BEGN                   TO ITDM-FUNCTION.
           CALL 'ITDMIO' USING ITDM-PARAMS.


           IF ITDM-STATUZ              NOT = O-K
                                   AND NOT = ENDP
               MOVE ITDM-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
           IF ITDM-ITEMCOY NOT = WSSP-COMPANY
            OR ITDM-ITEMTABL NOT = T5688
            OR ITDM-ITEMITEM NOT = CHDRLNB-CNTTYPE
            OR ITDM-STATUZ = ENDP
              MOVE F290                TO SCRN-ERROR-CODE
           ELSE
              MOVE ITDM-GENAREA        TO T5688-T5688-REC.
      *


      *****
      *    Calculate the contract fee by using the subroutine found in
      *    table T5674. See 1200 Section.
      *****
           IF T5688-FEEMETH            NOT = SPACES
              PERFORM 1200-CALC-FEE ELSE                                <V74L01>
           MOVE ZERO                   TO MGFL-MGFEE                    <CAS1.0>
           MOVE ZERO                   TO S6378-CNTFEE.
      *
       1090-CALC-PREMIUM.
      *****
      *    Read through the Coverage/Rider transactions COVTLNB.
      *****
           MOVE CHDRLNB-CNTCURR        TO S6378-CNTCURR.
           MOVE WSAA-BILLCURR (1)      TO S6378-BILLCURR.               <CAS1.0>
      **** MOVE CHDRLNB-BILLCURR       TO S6378-BILLCURR.               <014>
      **** MOVE CHDRLNB-BILLFREQ       TO S6378-BILLFREQ.               <014>
      **** MOVE CHDRLNB-BILLCHNL       TO S6378-MOP.                    <014>

           MOVE SPACES                 TO COVTLNB-PARAMS.
           MOVE CHDRLNB-CHDRCOY        TO COVTLNB-CHDRCOY
           MOVE CHDRLNB-CHDRNUM        TO COVTLNB-CHDRNUM
           MOVE '00'                   TO COVTLNB-LIFE
           MOVE ZERO                   TO COVTLNB-SEQNBR.
           MOVE BEGN                   TO COVTLNB-FUNCTION.
           PERFORM 1300-CALC-PREMIUM.
           PERFORM 1400-CHECK-MIN-MAX-LIMIT.                            <V4L009>
      *                                                                 <NB043>
      * Check the Coverage/Rider of LA have mortality ='5' then         <NB043>
      * not allow sell if it not HI.                                    <NB043>
      *                                                                 <NB043>
           PERFORM B1300-CHECK-MORTALITY-LA.                            <NB043>
      *
       1100-ADJUST-REGULAR-PREM.
                                                                        <014>
      * Change the premium pro-rata to calculated on a payer            <014>
      * by payer basis.                                                 <014>

      **** If The Calculated Regular Premium is equal to zeros then     <014>
      **** move zeros to the screen fields.                             <014>
      ****                                                              <014>
      **** IF WSAA-REGPREM             = ZERO                           <014>
      ****    MOVE 0                   TO WSAA-INSTPRM                  <014>
      ****    GO TO 1105-TOTAL-PREMIUMS.                                <014>
      ****                                                              <014>
      ****                                                              <014>
      **** Get the frequency Factor from DATCON3 for Regular premiums.  <014>
      ****                                                              <014>
      **** MOVE CHDRLNB-OCCDATE        TO DTC3-INT-DATE-1.              <014>
      **** MOVE CHDRLNB-BILLCD         TO DTC3-INT-DATE-2.         <014><008>
      **** MOVE CHDRLNB-BTDATE         TO DTC3-INT-DATE-2.         <014><008>
      **** MOVE CHDRLNB-BILLFREQ       TO DTC3-FREQUENCY.               <014>
      ****                                                              <014>
      **** CALL 'DATCON3'              USING DTC3-DATCON3-REC.          <014>
      ****                                                              <014>
      **** IF DTC3-STATUZ              NOT = O-K                        <014>
      ****    MOVE DTC3-STATUZ         TO SYSR-STATUZ                   <014>
      ****    PERFORM 600-FATAL-ERROR.                                  <014>
      ****                                                              <014>
      ****                                                              <014>
      **** Use the DATCON3 Frequency Factor to calculate the Instate-   <014>
      **** ment Premium.                                                <014>
      ****                                                              <014>
      **** MOVE DTC3-FREQ-FACTOR       TO WSAA-FACTOR.                  <014>
      ****                                                         <014><012>
      **** We have to be sure that the date's involved are not     <014><012>
      **** problem dates(for details see amendment note)           <014><012>
      ****                                                         <014><012>
      **** MOVE CHDRLNB-OCCDATE         TO WSAA-PREM-DATE1.        <014><012>
      **** MOVE CHDRLNB-BILLCD          TO WSAA-PREM-DATE2.        <014><012>
      ****                                                         <014><012>
      **** PERFORM 1900-CHECK-FREQ-DATE.                           <014><012>
      ****                                                         <014><012>
      **** MULTIPLY WSAA-REGPREM       BY WSAA-FACTOR                   <014>
      ****                             GIVING WSAA-INSTPRM.             <014>
      ****                                                              <014>
      *1105-TOTAL-PREMIUMS.                                             <014>
      **** ADD WSAA-SINGP              TO WSAA-INSTPRM.                 <014>
      **** MOVE WSAA-INSTPRM           TO S6378-INST-PREM.              <014>
      **** MOVE S6378-CNTFEE           TO WSAA-CNTFEE.                  <014>
      **** IF WSAA-INSTPRM             = 0                              <014>
      ****    MOVE 0                   TO S6378-CNTFEE                  <014>
      **** ELSE                                                         <014>
      ****    MULTIPLY S6378-CNTFEE    BY WSAA-FACTOR                   <014>
      ****                             GIVING S6378-CNTFEE.             <014>
      ****                                                              <014>
      ****                                                              <014>
      **Check that the premium amount + the contract fee amount         <014>
      **is less than the contract suspense amount paid.                 <014>
      **** ADD S6378-CNTFEE             TO WSAA-INSTPRM.                <014>
      **** IF WSAA-INSTPRM        >       S6378-CNTSUSP                 <014>
      ****                                                         <014><003>
      **** IF WSAA-INSTPRM        NOT >   S6378-CNTSUSP            <014><003>
      ****    GO TO 1106-CONT.                                     <014><003>

      *                                                                 <003>
      *  Look up tolerance applicable                                   <003>
      *  (code removed as Tolerance details are obtained in Section     <CAS1.0>
      *  1C200-SUSPENCE-AMOUNT for appropriate suspense currency).      <CAS1.0>
      *                                                                 <003>
      **** MOVE SPACES                 TO ITEM-DATA-KEY.           <003><CAS1.0>
      **** MOVE 'IT'                   TO ITEM-ITEMPFX.            <003><CAS1.0>
      **** MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.            <003><CAS1.0>
      **** MOVE T5667                  TO ITEM-ITEMTABL.           <003><CAS1.0>
      **** MOVE WSKY-BATC-BATCTRCDE    TO WSAA-T5667-TRANCD.       <003><CAS1.0>
      **** MOVE CHDRLNB-CNTCURR        TO WSAA-T5667-CURR.         <003><CAS1.0>
      **** MOVE WSAA-T5667-KEY         TO ITEM-ITEMITEM.           <003><CAS1.0>
      **** MOVE 'READR'                TO ITEM-FUNCTION.           <003><CAS1.0>
                                                                        <003>
      **** CALL 'ITEMIO' USING ITEM-PARAMS.                        <003><CAS1.0>
      **** IF ITEM-STATUZ              NOT = O-K                   <003><CAS1.0>
      ****                         AND NOT = MRNF                  <003><CAS1.0>
      ****     MOVE ITEM-PARAMS        TO SYSR-PARAMS              <003><CAS1.0>
      ****     PERFORM 600-FATAL-ERROR.                            <003><CAS1.0>
      *                                                                 <003>
      **** IF ITEM-STATUZ              = MRNF                      <014><003>
      ****    MOVE              ZERO TO WSAA-TOLERANCE-APP,        <014><003>
      ****                              WSAA-AMOUNT-LIMIT.         <014><003>
      ****    GO TO 1105B-CALC-AMOUNT-AVAILABLE.                   <014><003>
      ****                                                         <014><003>
      **** MOVE ITEM-GENAREA TO T5667-T5667-REC.                   <014><003>

      **** IF ITEM-STATUZ              = MRNF                      <014><CAS1.0>
      ****    MOVE              ZERO TO WSAA-TOLERANCE-APP,        <014><CAS1.0>
      ****                              WSAA-AMOUNT-LIMIT          <014><CAS1.0>
      **** ELSE                                                    <014><CAS1.0>
      ****    MOVE ITEM-GENAREA TO T5667-T5667-REC.                <014><CAS1.0>

      **** PERFORM 1A00-SEARCH-FOR-TOLERANCE VARYING WSAA-SUB      <014><003>
      ****                                   FROM 1 BY 1           <014><003>
      ****                                   UNTIL WSAA-SUB > 11  OR014><003>
      ****                                         CHDRLNB-BILLFREQ<=14><003>
      ****                                         T5667-FREQ(WSAA-SUB).<003>
      ****                                                         <014><003>
      **** IF WSAA-SUB > 11                                        <014><003>
      ****    MOVE              ZERO TO WSAA-TOLERANCE-APP,        <014><003>
      ****                              WSAA-AMOUNT-LIMIT          <014><003>
      ****    GO TO 1105B-CALC-AMOUNT-AVAILABLE.                   <014><003>
      ****                                                         <014><003>
      *1105A-CALC-TOLERANCE.                                       <014><003>
      ****                                                         <014><003>
      **** COMPUTE WSAA-CALC-TOLERANCE =                           <014><003>
      ****  (T5667-PRMTOL(WSAA-SUB) * S6378-INST-PREM) / 100.      <014><003>
      ****                                                         <014><003>
      **** Check % amount is less than Limit on T5667, if so use this14><003>
      **** else use Limit.                                         <014><003>
      ****                                                         <014><003>
      **** IF WSAA-CALC-TOLERANCE      < T5667-MAX-AMOUNT(WSAA-SUB)<014><003>
      ****    MOVE WSAA-CALC-TOLERANCE TO WSAA-TOLERANCE           <014><003>
      **** ELSE                                                    <014><003>
      ****    MOVE T5667-MAX-AMOUNT(WSAA-SUB) TO WSAA-TOLERANCE.   <014><003>
      ****                                                         <014><003>
      **** COMPUTE WSAA-DIFF        = WSAA-INSTPRM - S6378-CNTSUSP.<014><003>
      **** IF WSAA-DIFF                < WSAA-TOLERANCE            <014><003>
      ****    MOVE WSAA-DIFF           TO WSAA-TOLERANCE.          <014><003>
      ****                                                         <014><003>
      *1105B-CALC-AMOUNT-AVAILABLE.                                <014><003>
      ****                                                         <014><003>
      ****alculate Total amount AVAILABLE                          <014><003>
      ****                                                         <014><003>
      **** COMPUTE WSAA-TOTAMNT = S6378-CNTSUSP                    <014><003>
      ****        + WSAA-TOLERANCE.                                <014><003>
      ****                                                         <014><003>
      ***Check if the amount required is greater than the amount   <014><003>
      **** available - if this is the case the contract cannot be  <014><003>
      **** issued.                                                 <014><003>
      ****                                                         <014><003>
      **** IF WSAA-INSTPRM              > WSAA-TOTAMNT             <014><003>
      ****    MOVE SPACES               TO ERMS-ERRMESG-REC             <014>
      ****    MOVE U026                 TO ERMS-EROR                    <014>
      ****    MOVE SPACES               TO S6378-LIFE                   <014>
      ****                                 S6378-JLIFE                  <014>
      ****                                 S6378-COVERAGE               <014>
      ****                                 S6378-RIDER                  <014>
      ****    PERFORM 1800-ERROR-MESSAGES.                              <014>
      ****                                                              <014>
                                                                        <014>
                                                                        <014>
      * Read the RTRN file to see if a cash receipt has been
      * created for this contract.

           MOVE WSSP-COMPANY           TO RTRNSAC-RLDGCOY.              <014>
           MOVE CHDRLNB-CHDRNUM        TO RTRNSAC-RLDGACCT.             <014>
      **** MOVE CHDRLNB-CNTCURR        TO RTRNSAC-ORIGCCY.         <014><CAS1.0>
           IF WSAA-SUSP-IND             = 'Y'                           <CAS1.0>
              MOVE ACBLENQ-ORIGCURR    TO RTRNSAC-ORIGCCY               <CAS1.0>
           ELSE                                                         <CAS1.0>
              MOVE SPACE               TO RTRNSAC-ORIGCCY               <CAS1.0>
           END-IF.                                                      <CAS1.0>
           MOVE T5645-SACSCODE-01      TO RTRNSAC-SACSCODE.             <014>
           MOVE T5645-SACSTYPE-01      TO RTRNSAC-SACSTYP.              <014>
           MOVE READR                  TO RTRNSAC-FUNCTION.             <014>
                                                                        <014>
           CALL 'RTRNSACIO'            USING RTRNSAC-PARAMS.            <014>
                                                                        <014>
           IF  RTRNSAC-STATUZ      NOT = O-K                            <014>
           AND                     NOT = MRNF                           <014>
              MOVE RTRNSAC-PARAMS      TO SYSR-PARAMS                   <014>
              PERFORM 600-FATAL-ERROR.                                  <014>
                                                                        <014>
           IF RTRNSAC-STATUZ           = MRNF                           <014>
              MOVE VRCM-MAX-DATE       TO RTRNSAC-EFFDATE.              <014>
                                                                        <014>
      * Look up the tax relief method on T5688.                         <014>
                                                                        <014>
           MOVE SPACES                 TO ITDM-DATA-KEY.                <014>
           MOVE T5688                  TO ITDM-ITEMTABL.                <014>
           MOVE CHDRLNB-CNTTYPE        TO ITDM-ITEMITEM.                <014>
           MOVE CHDRLNB-CHDRCOY        TO ITDM-ITEMCOY.                 <014>
           MOVE CHDRLNB-OCCDATE        TO ITDM-ITMFRM.                  <014>
           MOVE 'IT'                   TO ITDM-ITEMPFX.                 <017>
           MOVE 'BEGN'                 TO ITDM-FUNCTION.                <014>
                                                                        <014>
           CALL 'ITDMIO' USING ITDM-PARAMS.                             <014>
                                                                        <014>
           IF ITDM-STATUZ              NOT = O-K                        <014>
                                   AND NOT = ENDP                       <014>
               MOVE ITDM-STATUZ        TO SYSR-STATUZ                   <014>
               PERFORM 600-FATAL-ERROR.                                 <014>
                                                                        <014>
                                                                        <014>
           MOVE ITDM-GENAREA TO T5688-T5688-REC.                        <014>
                                                                        <014>
                                                                        <014>
      * Look up the subroutine on T6687.                                <014>
                                                                        <014>
           MOVE SPACES                 TO ITEM-DATA-KEY.                <014>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <014>
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.                 <014>
           MOVE T6687                  TO ITEM-ITEMTABL.                <014>
           MOVE T5688-TAXRELMTH        TO ITEM-ITEMITEM.                <014>
                                                                        <014>
           CALL 'ITEMIO' USING ITEM-PARAMS.                             <014>
                                                                        <014>
           IF ITEM-STATUZ  NOT = O-K  AND                               <014>
                           NOT = MRNF                                   <014>
               MOVE ITEM-STATUZ        TO SYSR-STATUZ                   <014>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <014>
               PERFORM 600-FATAL-ERROR.                                 <014>
                                                                        <014>
           IF ITEM-STATUZ = O-K                                         <014>
               MOVE ITEM-GENAREA       TO T6687-T6687-REC               <014>
           ELSE                                                         <014>
               MOVE SPACES             TO T6687-T6687-REC.              <014>
                                                                        <014>
                                                                        <014>
           MOVE TDAY                   TO DTC1-FUNCTION.                <014>
           CALL 'DATCON1' USING DTC1-DATCON1-REC.                       <014>
                                                                        <014>
      * Adjust the regular premiums (if not zero) by the number         <014>
      * of frequencies required prior to issue.                         <014>
      * If the contract has a tax relief method deduct the tax          <014>
      * relief amount from the amount due.                              <014>
      * Also check if there is enough money in suspense to issue        <014>
      * this contract.                                                  <014>
                                                                        <014>
           MOVE 1                      TO WSBB-SUB.                     <014>
           PERFORM 1C00-ADJUST-PREMIUM UNTIL WSBB-SUB > 9 OR            <014>
                   WSAA-CLNTNUM(WSBB-SUB) = SPACES.                     <014>
                                                                        <014>
      **** MOVE WSAA-TOTAL-PREMIUM     TO S6378-INST-PREM.         <014><CAS1.0>
      **** MOVE WSAA-TOTAL-SUSPENSE    TO S6378-CNTSUSP.           <014><CAS1.0>
????       MOVE WSAA-TOTAL-PREMIUM     TO S6378-INST-PREM.              <CAS1.0>
           MOVE RLPDLON-PRMDEPST       TO S6378-PRMDEPST.               <V4L001>
      *                                                                 <CAS1.0>
      * If Suspense payment found, retrieve appropriate Tolerance       <CAS1.0>
      * details & check Suspense Amount is within Tolerance range.      <CAS1.0>
      * Otherwise check if Suspense is required, (initial Premium       <CAS1.0>
      * > zero), before displaying error.                               <CAS1.0>
      *                                                                 <CAS1.0>
           IF WSAA-SUSP-IND             = 'Y'                           <CAS1.0>
              MOVE SPACES              TO ITEM-DATA-KEY                 <CAS1.0>
              MOVE 'IT'                TO ITEM-ITEMPFX                  <CAS1.0>
              MOVE WSSP-COMPANY        TO ITEM-ITEMCOY                  <CAS1.0>
              MOVE T5667               TO ITEM-ITEMTABL                 <CAS1.0>
              MOVE WSKY-BATC-BATCTRCDE TO WSAA-T5667-TRANCD             <CAS1.0>
              MOVE ACBLENQ-ORIGCURR    TO WSAA-T5667-CURR               <CAS1.0>
              MOVE WSAA-T5667-KEY      TO ITEM-ITEMITEM                 <CAS1.0>
              MOVE READR               TO ITEM-FUNCTION                 <CAS1.0>
                                                                        <CAS1.0>
              CALL 'ITEMIO'         USING ITEM-PARAMS                   <CAS1.0>
                                                                        <CAS1.0>
              IF (ITEM-STATUZ       NOT = O-K )    AND                  <CAS1.0>
                 (ITEM-STATUZ       NOT = MRNF)                         <CAS1.0>
                    MOVE ITEM-PARAMS        TO SYSR-PARAMS              <CAS1.0>
                    MOVE ITEM-STATUZ        TO SYSR-STATUZ              <CAS1.0>
                    PERFORM 600-FATAL-ERROR                             <CAS1.0>
              END-IF                                                    <CAS1.0>
                                                                        <CAS1.0>
              IF ITEM-STATUZ            = MRNF                          <CAS1.0>
                 MOVE SPACE            TO T5667-T5667-REC               <CAS1.0>
              ELSE                                                      <CAS1.0>
                 MOVE ITEM-GENAREA     TO T5667-T5667-REC               <CAS1.0>
              END-IF                                                    <CAS1.0>
                                                                        <CAS1.0>
              PERFORM 1C300-CHECK-AGENT-TERMINATE                       <V42013>
              PERFORM 1C200-SUSPENSE-AMOUNT                             <CAS1.0>
           ELSE                                                         <CAS1.0>
              IF WSAA-TOTAL-PREMIUM     > ZERO                          <CAS1.0>
                 MOVE 'Y'              TO WSAA-INSUFFICIENT-SUSPENSE    <CAS1.0>
              END-IF                                                    <CAS1.0>
           END-IF.                                                      <CAS1.0>
                                                                        <CAS1.0>
                                                                        <014>
           IF WSAA-INSUFFICIENT-SUSPENSE = 'Y'                          <014>
              MOVE SPACES               TO ERMS-ERRMESG-REC             <014>
      ***     MOVE U026                 TO ERMS-EROR                    <016>
              MOVE H030                 TO ERMS-EROR                    <016>
              MOVE SPACES               TO S6378-LIFE                   <014>
                                           S6378-JLIFE                  <014>
                                           S6378-COVERAGE               <014>
                                           S6378-RIDER                  <014>
              MOVE 0                    TO S6378-PAYRSEQNO              <014>
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES.                              <014>
                                                                        <014>
      ****                                                              <A05743>
      ****  Check that the owner is alive.                              <A05743>
      ****                                                              <A05743>
                                                                        <A05743>
           IF CLTS-CLTDOD           NOT = VRCM-MAX-DATE                 <A05743>
           AND CHDRLNB-OCCDATE          > CLTS-CLTDOD                   <A06596>
               MOVE SPACES             TO ERMS-ERRMESG-REC              <A05743>
      ****     MOVE F782               TO ERMS-EROR             <A06596><A05743>
               MOVE W343               TO ERMS-EROR                     <A06596>
               MOVE SPACES             TO S6378-LIFE                    <A05743>
                                          S6378-JLIFE                   <A05743>
                                          S6378-COVERAGE                <A05743>
                                          S6378-RIDER                   <A05743>
               MOVE ZEROES             TO S6378-PAYRSEQNO               <A05743>
               MOVE 'OW'               TO WSAA-EXTRA-MSGPFX             <A06596>
               PERFORM 1800-ERROR-MESSAGES                              <A05743>
           END-IF.                                                      <A05743>
           PERFORM A2000-CHECK-DEATH-DATE.                              <A06596>
                                                                        <003>
      *1106-CONT.                                                       <003>
<004> *****MOVE SPACES                 TO ITEM-DATA-KEY.           <014><004>
<004> *****MOVE 'IT'                   TO ITEM-ITEMPFX.            <014><004>
<004> *****MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.            <014><004>
<004> *****MOVE T3625                  TO ITEM-ITEMTABL.           <014><004>
<004> *****MOVE 'LP'                   TO WSAA-T3625-LP.           <014><004>
<004> *****MOVE CHDRLNB-BILLCHNL       TO WSAA-T3625-BILLCHNL.     <014><004>
<004> *****MOVE CHDRLNB-BILLFREQ       TO WSAA-T3625-BILLFREQ.     <014><004>
<004> *****MOVE WSAA-T3625-KEY         TO ITEM-ITEMITEM.           <014><004>
<004> *****MOVE 'READR'                TO ITEM-FUNCTION.           <014><004>
<004> ****                                                         <014><004>
<004> *****CALL 'ITEMIO' USING ITEM-PARAMS.                        <014><004>
<004> *****IF ITEM-STATUZ              NOT = O-K                   <014><004>
<004> **********                   AND NOT = MRNF                  <014><004>
<004> *********MOVE ITEM-STATUZ        TO SYSR-STATUZ              <014><004>
<004> *********PERFORM 600-FATAL-ERROR.                            <014><004>
<004> ****                                                         <014><004>
<004> *****IF ITEM-STATUZ              = MRNF                      <014><004>
<004> ********MOVE SPACES              TO ERMS-ERRMESG-REC         <014><004>
<004> ********MOVE U023                TO ERMS-EROR                <014><004>
<004> ********PERFORM 1800-ERROR-MESSAGES.                         <014><004>
<004> ****                                                         <014><004>
<004> *****MOVE ITEM-GENAREA           TO T3625-T3625-REC.         <014><004>
<004> ****                                                         <014><N05>
<004> **For Direct Debit, check details exist on CHDR              <014><N05>
<004> ****                                                         <014><N05>
<004> **** IF T3625-DDIND              = 'Y'                       <014><008>
<004>***** IF CHDRLNB-BILLCHNL         = 'B'                       <014><008>
<004>*****    IF CHDRLNB-BANKKEY       = SPACES OR                 <014><008>
<004>*****       CHDRLNB-BANKACCKEY    = SPACES                    <014><008>
<004> ****    IF CHDRLNB-MANDREF       = SPACES                    <014><008>
<004> ****       MOVE SPACES           TO ERMS-ERRMESG-REC         <014><008>
<004> ****       MOVE F826             TO ERMS-EROR                <014><008>
<004> ****       PERFORM 1800-ERROR-MESSAGES.                      <014><008>
<004> ****       GO TO 1108-CONT.                                  <014><008>
<004> ****                                                         <014><008>
<004> ****                                                         <014><N05>
<004> **** IF T3625-DDIND            NOT  = 'Y'                    <014><004>
<004>***** IF CHDRLNB-BILLCHNL       NOT = 'B'                     <014><004>
<004> ****    GO TO 1107-CHECK-GROUP.                              <014><N05>
<004> **For non Direct Debit, check details don't exist on CHDR    <014><N05>
<004> ****                                                         <014><N05>
<004> **** IF T3625-DDIND         NOT  = 'Y'                       <014><004>
<004>***** IF CHDRLNB-BILLCHNL    NOT  = 'B'                       <014><004>
<004>*****    IF CLDRLNB-BANKKEY       NOT = SPACES OR             <014><008>
<004>*****       CHDRLNB-BANKACCKEY    NOT = SPACES                <014><008>
<004> ****    IF CHDRLNB-MANDREF       NOT = SPACES                <014><008>
<004> ****       MOVE SPACES           TO ERMS-ERRMESG-REC         <014><N05>
<004> ****       MOVE I005             TO ERMS-EROR                <014><N05>
<004> ****       PERFORM 1800-ERROR-MESSAGES                       <014><N05>
<004> ****       GO TO 1108-CONT                                   <014><N05>
<004> ****    ELSE                                                 <014><009>
<004> ****       GO TO 1108-CONT.                                  <014><009>
<004> **1106A-VALIDATE-MANDATE.                                    <014><008>
<004> ****                                                         <014><008>
<004> **Validate the mandate record against the contract header    <014><008>
<004> **and the client bank account record.                        <014><008>
<004> ****                                                         <014><008>
<004> **** IF CLBL-STATUZ = O-K AND MANDLNB-STATUZ = O-K           <014><008>
<004> ****      IF CHDRLNB-BILLCD < MANDLNB-EFFDATE                <014><008>
<004> ****         MOVE SPACES              TO ERMS-ERRMESG-REC    <014><008>
<004> ****         MOVE I008                TO ERMS-EROR           <014><008>
<004> ****         PERFORM 1800-ERROR-MESSAGES                     <014><008>
<004> ****         END-IF                                          <014><008>
<004> ****                                                         <014><008>
<004> ****      IF  MANDLNB-MAND-AMT NOT = 0                       <014><008>
<004> ****       IF MANDLNB-MAND-AMT NOT = WSAA-REGPREM + WSAA-CNTFEE14><008>
<004> ****          MOVE SPACES              TO ERMS-ERRMESG-REC   <014><008>
<004> ****          MOVE I009                TO ERMS-EROR          <014><008>
<004> ****          PERFORM 1800-ERROR-MESSAGES                    <014><008>
<004> ****      END-IF                                             <014><008>
<004> ****     END-IF                                              <014><008>
<004> ****     IF CHDRLNB-PAYRNUM = SPACES                         <014><008>
<004> ****       MOVE CHDRLNB-COWNNUM        TO WSAA-PAYRNUM       <014><008>
<004> ****     ELSE                                                <014><008>
<004> ****        MOVE CHDRLNB-PAYRNUM       TO WSAA-PAYRNUM       <014><008>
<004> ****     END-IF                                              <014><008>
<004> ****     IF WSAA-PAYRNUM NOT = CLBL-CLNTNUM                  <014><008>
<004> ****       MOVE SPACES                 TO ERMS-ERRMESG-REC   <014><008>
<004> ****       MOVE I004                   TO ERMS-EROR          <014><008>
<004> ****       PERFORM 1800-ERROR-MESSAGES                       <014><008>
<004> ****     END-IF                                              <014><008>
<004> ****                                                         <014><008>
<004> ****     IF CHDRLNB-BILLCURR NOT = CLBL-CURRCODE             <014><008>
<004> ****        MOVE SPACES                TO ERMS-ERRMESG-REC   <014><008>
<004> ****        MOVE I010                  TO ERMS-EROR          <014><008>
<004> ****        PERFORM 1800-ERROR-MESSAGES                      <014><008>
<004> ****     END-IF                                              <014><008>
<004> ****                                                         <014><008>
<004> ****     IF T3678-GONOGOFLG = 'N'                            <014><008>
<004> ****        MOVE SPACES                TO ERMS-ERRMESG-REC   <014><008>
<004> ****        MOVE I014                  TO ERMS-EROR          <014><008>
<004> ****        PERFORM 1800-ERROR-MESSAGES                      <014><008>
<004> ****     END-IF                                              <014><008>
<004> ****                                                         <014><010>
<004> *If*the Current From date of the bank account is greater than<the><010>
<004> *effective date of the mandate (i.e. the bank account is not yet4><010>
<004> *effective) then display an error message.                   <014><010>
<004> ****                                                         <014><010>
<004> ****                                                         <014><010>
<004> ****     IF CLBL-CURRFROM            > MANDLNB-EFFDATE       <014><010>
<004> ****        MOVE SPACES              TO ERMS-ERRMESG-REC     <014><010>
<004> ****        MOVE I020                TO ERMS-EROR            <014><010>
<004> ****        PERFORM 1800-ERROR-MESSAGES                      <014><010>
<004> ****     END-IF                                              <014><010>
<004> ****                                                         <014><010>
<004> *If*the Current To date of the bank account is less than the <014><010>
<004> *effective date of the mandate (i.e. the bank account is no  <014><010>
<004> *longer in force) then display an error message.             <014><010>
<004> ****                                                         <014><010>
<004> ****     IF CLBL-CURRTO          NOT = ZEROES                <014><010>
<004> ****        IF CLBL-CURRTO           < MANDLNB-EFFDATE       <014><010>
<004> ****           MOVE SPACES           TO ERMS-ERRMESG-REC     <014><010>
<004> ****           MOVE I020             TO ERMS-EROR            <014><010>
<004> ****           PERFORM 1800-ERROR-MESSAGES                   <014><010>
<004> ****        END-IF                                           <014><010>
<004> ****     END-IF                                              <014><010>
<004> **** END-IF.                                                 <014><N05>
<004> ****                                                         <014><004>
<004> *1107-CHECK-GROUP.                                           <014><N05>
<004> ****                                                         <014><N05>
<004> **For Group Billing, check details exist on CHDR             <014><N05>
<004> ****                                                         <014><N05>
<004> **** IF T3625-GRPIND             = 'Y' AND                   <014><004>
<004>***** IF CHDRLNB-BILLCHNL         = 'G' AND                   <014><004>
<004> ****    CHDRLNB-GRUPKEY          = SPACES                    <014><N05>
<004> ****    MOVE SPACES              TO ERMS-ERRMESG-REC         <014><N05>
<004> ****    MOVE E714                TO ERMS-EROR                <014><N05>
<004> ****    PERFORM 1800-ERROR-MESSAGES                          <014><N05>
<004> ****    GO TO 1108-CONT.                                     <014><N05>
<004> ****                                                         <014><N05>
<004> **For non Group Billing, check details don't exist on CHDR   <014><N05>
<004> ****                                                         <014><N05>
<004> **** IF T3625-GRPIND        NOT  = 'Y' AND                   <014><004>
<004>***** IF CHDRLNB-BILLCHNL         NOT = 'G' AND               <014><004>
<004> ****    CHDRLNB-GRUPKEY          NOT = SPACES                <014><N05>
<004> ****    MOVE SPACES              TO ERMS-ERRMESG-REC         <014><N05>
<004> ****    MOVE E571                TO ERMS-EROR                <014><N05>
<004> ****    PERFORM 1800-ERROR-MESSAGES                          <014><N05>
<004> ****    GO TO 1108-CONT.                                     <014><N05>
<004> ****                                                         <014><N05>
<004> **** IF T3625-GRPIND        NOT  = 'Y'                       <014><004>
<004>***** IF CHDRLNB-BILLCHNL         NOT = 'G'                   <014><004>
<004> ****    GO TO 1108-CONT.                                     <014><N05>
<004> ****                                                         <014><N05>
<004> **Read Group Details.                                        <014><N05>
<004> ****                                                         <014><N05>
<004> **** MOVE CHDRLNB-CHDRCOY        TO GRPS-GRUPCOY.            <014><N05>
<004> **** MOVE CHDRLNB-GRUPKEY        TO GRPS-GRUPNUM.            <014><N05>
<004> ****                                                         <014><N05>
<004> **** MOVE READR                  TO GRPS-FUNCTION.           <014><N05>
<004> **** CALL 'GRPSIO' USING         GRPS-PARAMS.                <014><N05>
<004> **** IF GRPS-STATUZ              NOT = O-K AND               <014><N05>
<004> ****                             NOT = MRNF                  <014><N05>
<004> ****     MOVE GRPS-PARAMS        TO SYSR-PARAMS              <014><N05>
<004> ****     MOVE GRPS-STATUZ        TO SYSR-STATUZ              <014><N05>
<004> ****     PERFORM 600-FATAL-ERROR.                            <014><N05>
<004> ****                                                         <014><N05>
<004> **For Group Billing, check details exist.                    <014><N05>
<004> ****                                                         <014><N05>
<004> **** IF GRPS-STATUZ              = MRNF                      <014><N05>
<004> ****                                                         <014>
<004> ****    MOVE SPACES              TO ERMS-ERRMESG-REC         <014><N05>
<004> ****    MOVE E714                TO ERMS-EROR                <014><N05>
<004> ****    PERFORM 1800-ERROR-MESSAGES.                         <014><N05>
<004> ****                                                         <014><004>

      * All the billing details will now be validated on a payer        <014>
      * by payer basis using the information on the payer records.      <014>
                                                                        <014>
              MOVE 1                   TO WSBB-SUB.                     <014>
              PERFORM 1D00-PAYER-VALIDATION UNTIL                       <014>
                            WSBB-SUB > 9 OR                             <014>
                            WSAA-CLNTNUM(WSBB-SUB) = SPACES.            <014>

           MOVE 'N'                    TO WSAA-AGENT-SUSPEND.           <S01>
           MOVE SPACES                 TO COVTTRM-PARAMS.               <CAS1.0>
           MOVE CHDRLNB-CHDRCOY        TO COVTTRM-CHDRCOY.              <CAS1.0>
           MOVE CHDRLNB-CHDRNUM        TO COVTTRM-CHDRNUM.              <CAS1.0>
           MOVE SPACES                 TO COVTTRM-LIFE                  <CAS1.0>
                                          COVTTRM-COVERAGE              <CAS1.0>
                                          COVTTRM-RIDER.                <CAS1.0>
           MOVE ZEROES                 TO COVTTRM-SEQNBR.               <CAS1.0>
           MOVE BEGN                   TO COVTTRM-FUNCTION.             <CAS1.0>
      *                                                                 <CAS1.0>
       1101-CALL-COVTTRMIO.                                             <CAS1.0>
           CALL 'COVTTRMIO'            USING COVTTRM-PARAMS.            <CAS1.0>
                                                                        <CAS1.0>
           IF COVTTRM-STATUZ           NOT = O-K  AND                   <CAS1.0>
                                       NOT = ENDP                       <CAS1.0>
               MOVE COVTTRM-PARAMS     TO SYSR-PARAMS                   <CAS1.0>
               PERFORM 600-FATAL-ERROR                                  <CAS1.0>
           END-IF.                                                      <CAS1.0>
           IF COVTTRM-CHDRCOY          NOT = CHDRLNB-CHDRCOY OR         <CAS1.0>
              COVTTRM-CHDRNUM          NOT = CHDRLNB-CHDRNUM OR         <CAS1.0>
              COVTTRM-STATUZ           = ENDP                           <CAS1.0>
               GO TO 1101-CHECK                                         <CAS1.0>
           END-IF.                                                      <CAS1.0>
           PERFORM 5000-READ-TR517.                                     <CAS1.0>
           IF ITDM-STATUZ              = O-K                            <CAS1.0>
              PERFORM 5200-CALC-WAIVE-SUMINS                            <CAS1.0>
              IF TR517-ZRWVFLG-03 = 'Y'                                 <CAS1.0>
              AND COVTTRM-COVERAGE     = '01'                           V42L014
                 ADD MGFL-MGFEE        TO WSAA-WAIVE-SUMINS             <CAS1.0>
              END-IF                                                    <CAS1.0>
              IF WSAA-WAIVE-SUMINS     NOT = COVTTRM-SUMINS             <CAS1.0>
                 MOVE SPACES           TO ERMS-ERRMESG-REC              <CAS1.0>
                 MOVE RL06             TO ERMS-EROR                     <CAS1.0>
                 MOVE COVTTRM-LIFE     TO S6378-LIFE                    <CAS1.0>
                 MOVE COVTTRM-JLIFE    TO S6378-JLIFE                   <CAS1.0>
                 MOVE COVTTRM-COVERAGE TO S6378-COVERAGE                <CAS1.0>
                 MOVE COVTTRM-RIDER    TO S6378-RIDER                   <CAS1.0>
                 MOVE 0                TO S6378-PAYRSEQNO               <CAS1.0>
                 MOVE SPACES           TO WSAA-EXTRA-MSGPFX             <A06596>
                 PERFORM 1800-ERROR-MESSAGES                            <CAS1.0>
              END-IF                                                    <CAS1.0>
           END-IF.                                                      <CAS1.0>
      ****                                                              <A05743>
      **** Check that all of the agents linked to the contract are      <A05743>
      **** active.                                                      <A05743>
      ****                                                              <A05743>
                                                                        <A05743>
           PERFORM 1F00-CHECK-AGENTS.                                   <A05743>
                                                                        <A05743>
           MOVE NEXTR                   TO COVTTRM-FUNCTION.            <CAS1.0>
           GO TO 1101-CALL-COVTTRMIO.                                   <CAS1.0>
       1101-CHECK.                                                      <023>
                                                                        <023>
      * Initialise work fields.                                         <023>
                                                                        <023>
           MOVE ZEROES                 TO WSAA-RISK-CESS-DATE,          <023>
                                          WSAA-PREM-CESS-DATE.          <023>
           MOVE SPACES                 TO WSAA-COVT-COVERAGE.           <023>
           MOVE SPACES                 TO WSAA-COVT-LIFE.               <027>
                                                                        <023>
      * Read COVT and check for risk-cess-date/premium-cess-date.       <023>
      * As a policy can have more than 1 Coverage, read through         <023>
      * all COVT records in order to valid all risk/premium cessation   <023>
      * dates.                                                          <023>
                                                                        <023>
           MOVE CHDRLNB-CHDRCOY        TO COVTTRM-CHDRCOY.              <023>
           MOVE CHDRLNB-CHDRNUM        TO COVTTRM-CHDRNUM.              <023>
           MOVE '01'                   TO COVTTRM-LIFE.                 <023>
           MOVE '01'                   TO COVTTRM-COVERAGE.             <023>
           MOVE '00'                   TO COVTTRM-RIDER.                <023>
           MOVE ZEROES                 TO COVTTRM-SEQNBR.               <023>
                                                                        <023>
       1102-READ-COVT.                                                  <023>
                                                                        <023>
           MOVE BEGN                   TO COVTTRM-FUNCTION.             <023>
           CALL 'COVTTRMIO'            USING COVTTRM-PARAMS.            <023>
           IF COVTTRM-STATUZ           NOT = O-K AND                    <023>
                                       NOT = ENDP                       <023>
              MOVE COVTTRM-PARAMS      TO SYSR-PARAMS                   <023>
              PERFORM 600-FATAL-ERROR                                   <023>
           END-IF.                                                      <023>
                                                                        <023>
      * If no Coverage record found or key breaks, go to next step.     <023>
                                                                        <023>
           IF COVTTRM-STATUZ           = ENDP OR                        <023>
              COVTTRM-CHDRCOY          NOT = CHDRLNB-CHDRCOY OR         <023>
              COVTTRM-CHDRNUM          NOT = CHDRLNB-CHDRNUM            <023>
              MOVE ENDP                TO COVTTRM-STATUZ                <023>
              GO TO 1108-CONT                                           <023>
           END-IF.                                                      <023>
                                                                        <023>
      * Else, save Coverage details.                                    <023>
                                                                        <023>
           MOVE COVTTRM-RISK-CESS-DATE TO WSAA-RISK-CESS-DATE.          <023>
           MOVE COVTTRM-PREM-CESS-DATE TO WSAA-PREM-CESS-DATE.          <023>
           MOVE COVTTRM-COVERAGE       TO WSAA-COVT-COVERAGE.           <023>
           MOVE COVTTRM-LIFE           TO WSAA-COVT-LIFE.               <027>
                                                                        <023>
       1103-READ-RIDER.                                                 <023>
                                                                        <023>
      * Start reading Rider records.                                    <023>
                                                                        <023>
           MOVE NEXTR                  TO COVTTRM-FUNCTION.             <023>
           CALL 'COVTTRMIO'            USING COVTTRM-PARAMS.            <023>
           IF COVTTRM-STATUZ           NOT = O-K AND                    <023>
                                       NOT = ENDP                       <023>
              MOVE COVTTRM-PARAMS      TO SYSR-PARAMS                   <023>
              PERFORM 600-FATAL-ERROR                                   <023>
           END-IF.                                                      <023>
                                                                        <023>
      * If no Rider found or key breaks, go to next step.               <023>
                                                                        <023>
           IF COVTTRM-STATUZ           = ENDP OR                        <023>
              COVTTRM-CHDRCOY          NOT = CHDRLNB-CHDRCOY OR         <023>
              COVTTRM-CHDRNUM          NOT = CHDRLNB-CHDRNUM            <023>
              MOVE ENDP                TO COVTTRM-STATUZ                <023>
              GO TO 1108-CONT                                           <023>
           END-IF.                                                      <023>
      ****                                                         <023><027>
      **If new Coverage read, move Coverage as key.                <023><027>
      ****                                                         <023><027>
      **** IF COVTTRM-COVERAGE         NOT = WSAA-COVT-COVERAGE    <023><027>
      ****    MOVE CHDRLNB-CHDRCOY     TO COVTTRM-CHDRCOY          <023><027>
      ****    MOVE CHDRLNB-CHDRNUM     TO COVTTRM-CHDRNUM          <023><027>
      ****    MOVE '01'                TO COVTTRM-LIFE             <023><027>
      ****    MOVE COVTTRM-COVERAGE    TO WSAA-COVT-COVERAGE       <023><027>
      ****    MOVE '00'                TO COVTTRM-RIDER            <023><027>
      ****    MOVE ZEROES              TO COVTTRM-SEQNBR           <023><027>
      ****    GO TO 1102-READ-COVT                                 <023><027>
      **** END-IF.                                                 <023><027>
                                                                        <027>
      * Check if the Coverage or Life has changed                       <027>
                                                                        <027>
           IF COVTTRM-LIFE             NOT = WSAA-COVT-LIFE             <027>
              OR COVTTRM-COVERAGE      NOT = WSAA-COVT-COVERAGE         <027>
                                                                        <027>
               MOVE CHDRLNB-CHDRCOY    TO COVTTRM-CHDRCOY               <027>
               MOVE CHDRLNB-CHDRNUM    TO COVTTRM-CHDRNUM               <027>
               MOVE COVTTRM-LIFE       TO WSAA-COVT-LIFE                <027>
               MOVE COVTTRM-COVERAGE   TO WSAA-COVT-COVERAGE            <027>
               MOVE '00'               TO COVTTRM-RIDER                 <027>
               MOVE ZEROS              TO COVTTRM-SEQNBR                <027>
               GO TO 1102-READ-COVT                                     <027>
           END-IF.                                                      <027>
                                                                        <023>
      * Else, compare Coverage/Rider cessation dates.                   <023>
                                                                        <023>
      *
      * Check table T6640, new indicator field has been created on
      * this table to allow a rider risk cession date > main covr's
      *
           PERFORM 5100-READ-T6640.                                     <CAS1.0>

      **** IF COVTTRM-RISK-CESS-DATE   > WSAA-RISK-CESS-DATE OR         <023>
      ****    COVTTRM-PREM-CESS-DATE   > WSAA-PREM-CESS-DATE            <023>
           IF  ( COVTTRM-RISK-CESS-DATE   > WSAA-RISK-CESS-DATE OR      <023>
                 COVTTRM-PREM-CESS-DATE   > WSAA-PREM-CESS-DATE )       <023>
           AND ( T6640-ZRMANDIND       NOT = 'Y' )
              MOVE COVTTRM-COVERAGE    TO S6378-COVERAGE                <023>
              MOVE COVTTRM-LIFE        TO S6378-LIFE                    <023>
              MOVE COVTTRM-RIDER       TO S6378-RIDER                   <023>
              MOVE 0                   TO S6378-PAYRSEQNO               <023>
              MOVE SPACES              TO ERMS-ERRMESG-REC              <023>
              MOVE G161                TO ERMS-EROR                     <023>
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES                               <023>
           END-IF.                                                      <023>
                                                                        <023>
      * Read next Rider.                                                <023>
                                                                        <023>
           GO TO 1103-READ-RIDER.                                       <023>

      *
       1108-CONT.                                                       <N05>

      * Ensure that there are no unprocessed UTRN records.              <024>
                                                                        <024>
           MOVE SPACE                  TO UTRNRNL-PARAMS.               <024>
           MOVE CHDRLNB-CHDRNUM        TO UTRNRNL-CHDRNUM.              <024>
           MOVE CHDRLNB-CHDRCOY        TO UTRNRNL-CHDRCOY.              <024>
           MOVE READR                  TO UTRNRNL-FUNCTION.             <024>
                                                                        <024>
           CALL 'UTRNRNLIO' USING UTRNRNL-PARAMS.                       <024>
                                                                        <024>
           IF UTRNRNL-STATUZ           NOT = O-K  AND                   <024>
              UTRNRNL-STATUZ           NOT = MRNF                       <024>
              MOVE UTRNRNL-PARAMS      TO SYSR-PARAMS                   <024>
              PERFORM 600-FATAL-ERROR                                   <024>
           END-IF.                                                      <024>
                                                                        <024>
      * If unprocessed UTRN record exists, display error message.       <024>
                                                                        <024>
           IF UTRNRNL-STATUZ           NOT = MRNF                       <024>
              MOVE SPACES              TO ERMS-ERRMESG-REC              <024>
              MOVE I030                TO ERMS-EROR                     <024>
              MOVE SPACES              TO S6378-LIFE                    <024>
                                          S6378-JLIFE                   <024>
                                          S6378-COVERAGE                <024>
                                          S6378-RIDER                   <024>
              MOVE 0                   TO S6378-PAYRSEQNO               <024>
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES                               <024>
           END-IF.                                                      <024>

      * Ensure that there are no unprocessed HITR records.              <INTBR>
                                                                        <INTBR>
           MOVE SPACE                  TO HITRRNL-PARAMS.               <INTBR>
           MOVE CHDRLNB-CHDRNUM        TO HITRRNL-CHDRNUM.              <INTBR>
           MOVE CHDRLNB-CHDRCOY        TO HITRRNL-CHDRCOY.              <INTBR>
           MOVE HITRRNLREC             TO HITRRNL-FORMAT.               <INTBR>
           MOVE READR                  TO HITRRNL-FUNCTION.             <INTBR>
                                                                        <INTBR>
           CALL 'HITRRNLIO' USING HITRRNL-PARAMS.                       <INTBR>
                                                                        <INTBR>
           IF HITRRNL-STATUZ           NOT = O-K  AND                   <INTBR>
              HITRRNL-STATUZ           NOT = MRNF                       <INTBR>
              MOVE HITRRNL-PARAMS      TO SYSR-PARAMS                   <INTBR>
              PERFORM 600-FATAL-ERROR                                   <INTBR>
           END-IF.                                                      <INTBR>
                                                                        <INTBR>
      * If unprocessed HITR record exists, display error message        <INTBR>
                                                                        <INTBR>
           IF HITRRNL-STATUZ           NOT = MRNF                       <INTBR>
              MOVE SPACES              TO ERMS-ERRMESG-REC              <INTBR>
              MOVE HL08                TO ERMS-EROR                     <INTBR>
              MOVE SPACES              TO S6378-LIFE                    <INTBR>
                                          S6378-JLIFE                   <INTBR>
                                          S6378-COVERAGE                <INTBR>
                                          S6378-RIDER                   <INTBR>
              MOVE 0                   TO S6378-PAYRSEQNO               <INTBR>
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES                               <INTBR>
           END-IF.                                                      <INTBR>
      *
           MOVE SPACES                 TO WSAA-MANDATORYS               <V76L01>
                                          WSAA-BNYTYPES.                <V76L01>
           MOVE ZEROES                 TO WSAA-IWC.                     <V76L01>
      *                                                                 <V76L01>
           MOVE SPACES                 TO BNFYLNB-DATA-KEY.             <LA5070>
           MOVE O-K                    TO BNFYLNB-STATUZ.               <LA5070>
           MOVE WSSP-COMPANY           TO BNFYLNB-CHDRCOY.              <LA5070>
           MOVE CHDRLNB-CHDRNUM        TO BNFYLNB-CHDRNUM.              <LA5070>
                                                                        <LA5070>
           MOVE BEGN                   TO BNFYLNB-FUNCTION.             <LA5070>
           PERFORM 1400-READ-FOR-BNFY  UNTIL BNFYLNB-STATUZ = ENDP.     <LA5070>
                                                                        <V76L01>
      *                                                                 <V76L01>
      *-- for every mandatory beneficiary defined (TR52Z)               <V76L01>
      *       check if there is a matching beneficiary                  <V76L01>
      *                                                                 <V76L01>
           PERFORM 1H00-MANDATORY-BNFY.                                 <V76L01>
                                                                        <V76L01>
           INSPECT WSAA-MANDATORYS TALLYING WSAA-ITC                    <V76L01>
               FOR ALL 'N'.                                             <V76L01>
                                                                        <V76L01>
           IF WSAA-ITC    > 0                                           <V76L01>
              MOVE SPACES              TO ERMS-ERRMESG-REC              <V76L01>
              MOVE RFK2                TO ERMS-EROR                     <V76L01>
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <V76L01>
              PERFORM 1800-ERROR-MESSAGES                               <V76L01>
           END-IF.                                                      <V76L01>
                                                                        <V76L01>
      **** PERFORM 1400-READ-FOR-BNFY.                          <LA5070><A07303>
      *                                                                 <V71L12>
      * First of all determine whether underwriting is required for the <V71L12>
      * product type by reading TR675                                   <V71L12>
      *
       1110-LIFE-VALIDATION.
           MOVE SPACES                 TO LIFELNB-PARAMS.
           MOVE CHDRLNB-CHDRCOY        TO LIFELNB-CHDRCOY.
           MOVE CHDRLNB-CHDRNUM        TO LIFELNB-CHDRNUM.
           MOVE BEGN                   TO LIFELNB-FUNCTION.
           MOVE SPACES                 TO WSAA-LIFE.                    <CAS1.0>
           MOVE 'Y'                    TO CFUP-FIRST-LIFE.              <FUPLET>
           PERFORM 1600-LIFE-LEVEL     UNTIL LIFELNB-STATUZ = ENDP.

           MOVE SPACES                 TO FLUPLNB-DATA-KEY.             <FUPLET>
           MOVE CHDRLNB-CHDRCOY        TO FLUPLNB-CHDRCOY.              <FUPLET>
           MOVE CHDRLNB-CHDRNUM        TO FLUPLNB-CHDRNUM.              <FUPLET>
           MOVE ZERO                   TO FLUPLNB-FUPNO.                <FUPLET>
           MOVE BEGN                   TO FLUPLNB-FUNCTION.             <FUPLET>
                                                                        <FUPLET>
           CALL 'FLUPLNBIO'  USING FLUPLNB-PARAMS.                      <FUPLET>
           IF  FLUPLNB-STATUZ          NOT = O-K                        <FUPLET>
           AND FLUPLNB-STATUZ          NOT = ENDP                       <FUPLET>
               MOVE FLUPLNB-PARAMS     TO SYSR-PARAMS                   <FUPLET>
               PERFORM 600-FATAL-ERROR                                  <FUPLET>
           END-IF.                                                      <FUPLET>
                                                                        <FUPLET>
           IF FLUPLNB-CHDRCOY          NOT = CHDRLNB-CHDRCOY            <FUPLET>
           OR FLUPLNB-CHDRNUM          NOT = CHDRLNB-CHDRNUM            <FUPLET>
           OR FLUPLNB-STATUZ           = ENDP                           <FUPLET>
               MOVE 'N'                TO WSAA-OUTSTANFLU               <FUPLET>
               GO TO 1120-COMP-VALIDATION                               <FUPLET>
           END-IF.                                                      <FUPLET>
      *                                                                 <FUPLET>
      *  But are any of them outstanding?                               <FUPLET>
                                                                        <FUPLET>
           MOVE SPACE                  TO WSAA-OUTFLUP-FOUND            <FUPLET>
                                          WSAA-DOC-REQD                 <FUPLET>
           PERFORM 1500-CHECK-FOLLOW-UPS                                <FUPLET>
              UNTIL FLUPLNB-STATUZ      = ENDP.                         <FUPLET>
                                                                        <FUPLET>
           IF WSAA-OUTFLUP-FOUND       = 'Y'                            <FUPLET>
              MOVE SPACES              TO ERMS-ERRMESG-REC              <FUPLET>
              MOVE H017                TO ERMS-EROR                     <FUPLET>
              MOVE SPACES              TO S6378-LIFE                    <FUPLET>
                                          S6378-JLIFE                   <FUPLET>
                                          S6378-COVERAGE                <FUPLET>
                                          S6378-RIDER                   <FUPLET>
              MOVE 0                   TO S6378-PAYRSEQNO               <FUPLET>
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES                               <FUPLET>
           END-IF.                                                      <FUPLET>
                                                                        <FUPLET>
           IF WSAA-DOC-REQD             = 'Y'          AND              <FUPLET>
              HPAD-ZDOCTOR              = SPACE                         <FUPLET>
              MOVE SPACES              TO ERMS-ERRMESG-REC              <FUPLET>
              MOVE HL42                TO ERMS-EROR                     <LA2103>
              MOVE SPACES              TO S6378-LIFE                    <FUPLET>
                                          S6378-JLIFE                   <FUPLET>
                                          S6378-COVERAGE                <FUPLET>
                                          S6378-RIDER                   <FUPLET>
              MOVE ZERO                TO S6378-PAYRSEQNO               <FUPLET>
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES                               <FUPLET>
           END-IF.                                                      <FUPLET>
                                                                        <FUPLET>
       1120-COMP-VALIDATION.                                            <FUPLET>
                                                                        <FUPLET>
      * Check the component level validation.
           MOVE SPACES                 TO COVTLNB-PARAMS.
           MOVE CHDRLNB-CHDRCOY        TO COVTLNB-CHDRCOY.
           MOVE CHDRLNB-CHDRNUM        TO COVTLNB-CHDRNUM.
           MOVE ZERO                   TO COVTLNB-SEQNBR.
           MOVE BEGN                   TO COVTLNB-FUNCTION.
           CALL 'COVTLNBIO'            USING COVTLNB-PARAMS.
           IF  COVTLNB-STATUZ          NOT = ENDP AND NOT = O-K
               MOVE COVTLNB-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.

           IF  COVTLNB-CHDRCOY         NOT = CHDRLNB-CHDRCOY
           OR  COVTLNB-CHDRNUM         NOT = CHDRLNB-CHDRNUM
               MOVE ENDP               TO COVTLNB-STATUZ.

           IF COVTLNB-STATUZ           = ENDP
              MOVE SPACES              TO  ERMS-ERRMESG-REC
              MOVE G066                TO ERMS-EROR
              MOVE SPACES              TO S6378-LIFE
                                           S6378-JLIFE
                                           S6378-COVERAGE
                                           S6378-RIDER
              MOVE 0                    TO S6378-PAYRSEQNO              <014>
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES.

           PERFORM 1700-COMPONENT-LEVEL UNTIL COVTLNB-STATUZ = ENDP.
      *                                                                 <V72L07>
           PERFORM A1700-CROSS-CHECK-PRODUCT.                           <V72L07>
           PERFORM A4000-CHECK-AGENTSER.                                <NB010>
                                                                        <CAS1.0>
       1190-EXIT.                                                       <CAS1.0>
           EXIT.                                                        <CAS1.0>
                                                                        <CAS1.0>
       1150-CALL-CSNCALC SECTION.                                       <CAS1.0>
      ***************************                                       <CAS1.0>
       1151-CALL.                                                       <CAS1.0>
                                                                        <R96REA>
           MOVE SPACES                  TO CSNC-CSNCALC-REC.            <R96REA>
           MOVE 'COVT'                  TO CSNC-FUNCTION.               <R96REA>
           MOVE CHDRLNB-CHDRCOY         TO CSNC-CHDRCOY.                <R96REA>
           MOVE CHDRLNB-CHDRNUM         TO CSNC-CHDRNUM.                <R96REA>
      **** MOVE '01'                    TO CSNC-LIFE.           <CAS1.0><R96REA>
           MOVE LIFELNB-LIFE            TO CSNC-LIFE.                   <CAS1.0>
           MOVE CHDRLNB-CNTTYPE         TO CSNC-CNTTYPE.                <R96REA>
           MOVE CHDRLNB-CNTCURR         TO CSNC-CURRENCY.               <R96REA>
           MOVE WSSP-FSUCO              TO CSNC-FSUCO.                  <R96REA>
           MOVE WSSP-LANGUAGE           TO CSNC-LANGUAGE.               <R96REA>
           MOVE ZEROES                  TO CSNC-INCR-AMT                <R96REA>
                                           CSNC-PLAN-SUFFIX.            <R96REA>
           MOVE CHDRLNB-OCCDATE         TO CSNC-EFFDATE.                <R96REA>
           MOVE CHDRLNB-TRANNO          TO CSNC-TRANNO.                 <R96REA>
           MOVE WSKY-BATC-BATCTRCDE     TO CSNC-BATCTRCDE.              <V76F06>
                                                                        <R96REA>
           CALL 'CSNCALC'               USING CSNC-CSNCALC-REC.         <R96REA>
                                                                        <R96REA>
           IF  CSNC-STATUZ              NOT = O-K                       <R96REA>
           AND CSNC-STATUZ              NOT = 'FACL'                    <R96REA>
               MOVE CSNC-STATUZ         TO SYSR-STATUZ                  <R96REA>
               MOVE CSNC-CSNCALC-REC    TO SYSR-PARAMS                  <R96REA>
               PERFORM 600-FATAL-ERROR                                  <R96REA>
           END-IF.                                                      <R96REA>
                                                                        <R96REA>
           IF CSNC-STATUZ              = 'FACL'                         <R96REA>
              MOVE SPACES              TO ERMS-ERRMESG-REC              <R96REA>
              MOVE R064                TO ERMS-EROR                     <R96REA>
              MOVE CSNC-LIFE           TO S6378-LIFE                    <R96REA>
              MOVE SPACES              TO S6378-JLIFE                   <R96REA>
              MOVE SPACES              TO S6378-COVERAGE                <CAS1.0>
              MOVE SPACES              TO S6378-RIDER                   <CAS1.0>
      ****    MOVE CSNC-COVERAGE       TO S6378-COVERAGE        <CAS1.0><R96REA>
      ****    MOVE CSNC-RIDER          TO S6378-RIDER           <CAS1.0><R96REA>
              MOVE 0                   TO S6378-PAYRSEQNO               <R96REA>
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES                               <R96REA>
           END-IF.                                                      <R96REA>
                                                                        <R96REA>
      *1190-EXIT.                                                       <CAS1.0>
       1159-EXIT.                                                       <CAS1.0>
           EXIT.
      /
      *****************************************************************
       1200-CALC-FEE SECTION.
      *
       1210-READ-SUBROUTINE-TABLE.
      *****
      *    Reference T5674 to obtain the subroutine required to work
      *    out the Fee amount by the correct method.
      *****
           MOVE SPACES                 TO ITEM-PARAMS.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE T5674                  TO ITEM-ITEMTABL
           MOVE T5688-FEEMETH          TO ITEM-ITEMITEM
           MOVE 'READR'                TO ITEM-FUNCTION
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.
      *
           CALL 'ITEMIO' USING ITEM-PARAMS.
      *
           IF ITEM-STATUZ              NOT = O-K
                                   AND NOT = MRNF
               MOVE ITEM-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
           IF ITEM-STATUZ              = MRNF
               MOVE ITEM-PARAMS        TO SYSR-PARAMS
               MOVE F151               TO SCRN-ERROR-CODE
               MOVE 'Y' TO WSSP-EDTERROR
               GO TO 1290-EXIT.

           MOVE ITEM-GENAREA           TO T5674-T5674-REC.
      *
      * Check subroutine NOT = SPACES before attempting call.           <006>
      *                                                                 <006>
           IF T5674-COMMSUBR           = SPACES                         <006>
              GO TO 1290-EXIT.                                          <006>
      *
           MOVE SPACES                 TO MGFL-MGFEEL-REC.              <007>
           MOVE ZEROES                 TO MGFL-EFFDATE,                 <007>
                                          MGFL-MGFEE.                   <007>
                                                                        <007>
           MOVE CHDRLNB-CNTTYPE        TO MGFL-CNTTYPE.
      **** MOVE CHDRLNB-BILLFREQ       TO MGFL-BILLFREQ.                <014>
           MOVE WSAA-BILLFREQ(1)       TO MGFL-BILLFREQ.                <014>
           MOVE CHDRLNB-OCCDATE        TO MGFL-EFFDATE.
           MOVE CHDRLNB-CNTCURR        TO MGFL-CNTCURR.
           MOVE WSSP-COMPANY           TO MGFL-COMPANY.
                                                                        <019>
                                                                        <019>
           CALL T5674-COMMSUBR         USING MGFL-MGFEEL-REC.

           IF MGFL-STATUZ              NOT = O-K
                                   AND NOT = ENDP
              MOVE MGFL-MGFEEL-REC     TO SYSR-PARAMS                   <017>
              PERFORM 600-FATAL-ERROR.
                                                                        <V76F06>
           MOVE MGFL-MGFEE             TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 8000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO MGFL-MGFEE.                   <V76F06>
                                                                        <V76F06>
           IF CHDRLNB-BILLCD   = CHDRLNB-OCCDATE                        <019>
              MOVE ZERO                TO S6378-CNTFEE                  <019>
           ELSE                                                         <019>
           MOVE MGFL-MGFEE             TO S6378-CNTFEE.
                                                                        <V74L01>
           IF S6378-CNTFEE             > ZERO                           <V74L01>
              MOVE S6378-CNTFEE        TO WSAA-CNTFEE                   <V74L01>
              PERFORM 7100-CHECK-CALC-CONT-TAX                          <V74L01>
              MOVE WSAA-TAX            TO WSAA-CNTFEE-TAX               <V74L01>
           END-IF.                                                      <V74L01>
      *
       1290-EXIT.
           EXIT.
      /
      *****************************************************************
       1300-CALC-PREMIUM SECTION.
      *
       1310-READSEQ-COVTLNB.
      *****
      *    Read each record and accumulate all single and regular
      *    premiums payable.
      *****
           CALL 'COVTLNBIO' USING COVTLNB-PARAMS.

           IF COVTLNB-STATUZ           NOT = O-K
                                   AND NOT = ENDP
              MOVE COVTLNB-PARAMS      TO COVTLNB-PARAMS
              PERFORM 600-FATAL-ERROR.

           IF COVTLNB-STATUZ           = ENDP
              GO TO 1390-EXIT.
      *
           IF CHDRLNB-CHDRCOY NOT = COVTLNB-CHDRCOY OR
              CHDRLNB-CHDRNUM NOT = COVTLNB-CHDRNUM
              MOVE ENDP TO COVTLNB-STATUZ
              GO TO 1390-EXIT.
      *
      *1320-COVER-RIDER-DEFINITION.                                     <001>
      *****                                                             <001>
      *****Check the Coverage/Rider Definition T5687 for a single       <001>
      *****Premium indicator of 'Y'.                                    <001>
      *****                                                             <001>
      *****                                                             <001>
      *****MOVE SPACES                 TO ITEM-PARAMS.                  <001>
      *****MOVE T5687                  TO ITEM-ITEMTABL.                <001>
      *****MOVE COVTLNB-CRTABLE        TO ITEM-ITEMITEM.                <001>
      *****MOVE 'READR'                TO ITEM-FUNCTION.                <001>
      *****MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.                 <001>
      *****MOVE 'IT'                   TO ITEM-ITEMPFX.                 <001>
      *****CALL 'ITEMIO' USING ITEM-PARAMS.                             <001>
      *****                                                             <001>
      *****IF ITEM-STATUZ              NOT = O-K                        <001>
      *****                        AND NOT = MRNF                       <001>
      *****    MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <001>
      *****    PERFORM 600-FATAL-ERROR.                                 <001>
      *****                                                             <001>
      *****IF ITEM-STATUZ              = MRNF                           <001>
      *****    MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <001>
      *****    MOVE E351               TO SCRN-ERROR-CODE               <001>
      *****    MOVE 'Y' TO WSSP-EDTERROR                                <001>
      *****    GO TO 1390-EXIT.                                         <001>
      *****                                                             <001>
      *****MOVE ITEM-GENAREA TO T5687-T5687-REC.                        <001>
      *****                                                             <001>
      *****                                                             <001>
      *****                                                             <001>
      *1330-SINGLE-OR-REGULAR.                                          <001>
      *****
      *    Single or Regular Premium?
      *    Single and Regular Premiums are now held in separate fields  <001>
      *    on COVT, so just move corresponding fields. If the COVT Inst <001>
      *    amount is zero, then don't bother to apply the frequency     <001>
      *    factor etc. Both fields may be non-zero!                     <001>
      *****
      **** IF CHDRLNB-BILLFREQ         = '00'                           <001>
      **** OR T5687-SINGLE-PREM-IND    = 'Y'                            <001>

      * Add the premium into the relevant entry in the WS table         <014>
      * depending  on the payer number on the COVT.                     <014>
                                                                        <014>
           MOVE COVTLNB-PAYRSEQNO      TO WSBB-SUB.                     <014>

      **** ADD COVTLNB-SINGP           TO WSAA-SINGP.                   <014>
           ADD COVTLNB-SINGP           TO WSAA-SINGP(WSBB-SUB).         <014>
                                                                        <019>
           IF COVTLNB-SINGP NOT = 0                                     <019>
              MOVE 'Y'                 TO WSAA-SING-PRM-IND.            <019>
                                                                        <V4L009>
           IF WSAA-BILLFREQ(WSBB-SUB)  = '00'                           <V4L009>
              ADD COVTLNB-SINGP     TO WSAA-MODAL-PREMIUM               <V4L009>
           END-IF.                                                      <V4L009>
                                                                        <V4L009>
           IF COVTLNB-INSTPREM         = 0                              <001>
              GO TO 1380-CONTINUE.

      *****
      * Get the frequency Factor from DATCON3 for Regular premiums.
      *****
      *    MOVE CHDRLNB-BILLCD         TO DTC3-INT-DATE-2.
      *    MOVE CHDRLNB-OCCDATE        TO DTC3-INT-DATE-1.
      *    MOVE CHDRLNB-BILLFREQ       TO DTC3-FREQUENCY.
      *
      *    CALL 'DATCON3' USING DTC3-DATCON3-REC.
      *
      *    IF DTC3-STATUZ              NOT = O-K
      *       GO TO 1190-EXIT.
      *
      *****
      *    Use the DATCON3 Frequency Factor to calculate the Instate-
      *    ment Premium.
      *****
      **** MULTIPLY COVTLNB-SINGP      BY DTC3-FREQ-FACTOR              <001>
      *    MULTIPLY COVTLNB-INSTPREM   BY DTC3-FREQ-FACTOR              <001>
      *                                GIVING WSAA-INSTPRM.
      *
      *    ADD WSAA-INSTPRM         TO WSAA-REGPREM.

      *                                                                 <011>
      * If the Freq has been changed to SP and screens have not been    <011>
      * revisited COVT will still show it has an INSTPREM when in fact  <011>
      * this is not true. A IVFQ status will be returned from a later   <011>
      * call to DATCON3 without the following code                      <011>
      *                                                                 <011>

      **** IF CHDRLNB-BILLFREQ         = '00'                      <014><011>
      ****    GO TO 1380-CONTINUE.                                 <014><011>
      ****                                                              <014>
      **** ADD COVTLNB-INSTPREM     TO WSAA-REGPREM.                    <014>

           IF WSAA-BILLFREQ(WSBB-SUB)  = '00'                           <014>
              GO TO 1380-CONTINUE.                                      <014>
                                                                        <014>
           ADD COVTLNB-INSTPREM     TO WSAA-REGPREM(WSBB-SUB).          <014>
           ADD COVTLNB-INSTPREM     TO WSAA-MODAL-PREMIUM.              <V4L009>

       1380-CONTINUE.
           PERFORM 7000-CHECK-CALC-COMP-TAX.                            <V74L01>
           MOVE NEXTR                  TO COVTLNB-FUNCTION.
           GO TO 1310-READSEQ-COVTLNB.
      *
       1390-EXIT.
           EXIT.
      /
       1400-CHECK-MIN-MAX-LIMIT SECTION.                                <V4L009>
      *********************************                                 <V4L009>
      *                                                                 <V4L009>
       1400-PARA.                                                       <V4L009>
      *****                                                             <V4L009>
      *    Check premium against the limits set in TH611 according to   <V4L009>
      *    the range limits for billing frequency.                      <V4L009>
      *****                                                             <V4L009>
           MOVE CHDRLNB-CNTTYPE        TO WSAA-TH611-CNTTYPE.           <V4L009>
           PERFORM 850-READ-TABL-TH611.                                 <V4L009>
           IF WSAA-TH611-ITEM          NOT = ITDM-ITEMITEM              <V4L009>
            OR CHDRLNB-CHDRCOY         NOT = ITDM-ITEMCOY               <V4L009>
            OR ITDM-ITEMTABL           NOT = TH611                      <V4L009>
            OR ITDM-STATUZ             = 'ENDP'                         <V4L009>
              MOVE '***'               TO WSAA-TH611-CNTTYPE            <V4L009>
              PERFORM 850-READ-TABL-TH611                               <V4L009>
           ELSE GO TO 1410-FIND-MAX-MIN-LIMIT                           <V4L009>
           END-IF.                                                      <V4L009>
              IF WSAA-TH611-ITEM          NOT = ITDM-ITEMITEM           <V4L009>
               OR CHDRLNB-CHDRCOY         NOT = ITDM-ITEMCOY            <V4L009>
               OR ITDM-ITEMTABL           NOT = TH611                   <V4L009>
               OR ITDM-STATUZ             = 'ENDP'                      <V4L009>
                 MOVE SPACES              TO ERMS-ERRMESG-REC           <V4L009>
                 MOVE RL12                TO ERMS-EROR                  <V4L009>
                 MOVE SPACES              TO WSAA-EXTRA-MSGPFX          <A06596>
                 PERFORM 1800-ERROR-MESSAGES                            <V4L009>
                 GO TO 1420-EXIT
              END-IF.                                                   <V4L009>
      *
       1410-FIND-MAX-MIN-LIMIT.                                         <V4L009>
      *
           MOVE ITDM-GENAREA              TO TH611-TH611-REC.           <V4L009>
      *****                                                             <V4L009>
      *    Process to find MAX/MIN limits for frequency required.       <V4L009>
      *****                                                             <V4L009>
           PERFORM 950-FIND-BILLFREQ VARYING WSAA-SUB FROM 1 BY         <V4L009>
           1 UNTIL WSAA-SUB          >   8                              <V4L009>
           OR CHDRLNB-BILLFREQ       =   TH611-FREQUENCY(WSAA-SUB).     <V4L009>
      *****                                                             <V4L009>
      *    If a range is not found or premium is out with the range     <V4L009>
      *    then error.                                                  <V4L009>
      *****                                                             <V4L009>
           IF WSAA-SUB                 > 8  OR                          <V4L009>
              ((WSAA-MODAL-PREMIUM  < TH611-CMIN(WSAA-SUB)) OR          <V4L009>
               (WSAA-MODAL-PREMIUM  > TH611-CMAX(WSAA-SUB)))            <V4L009>
              MOVE SPACES              TO ERMS-ERRMESG-REC              <V4L009>
              MOVE RL11                TO ERMS-EROR                     <V4L009>
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES                               <V4L009>
           END-IF.                                                      <V4L009>
      *
       1420-EXIT.                                                       <V4L009>
            EXIT.                                                       <V4L009>
      *                                                                 <V4L009>
       850-READ-TABL-TH611 SECTION.                                     <V4L009>
      ****************************                                      <V4L009>
      *****                                                             <V4L009>
      *    Read table TH611                                             <V4L009>
      *****                                                             <V4L009>
       850-READ.                                                        <V4L009>
           MOVE SPACES                 TO ITDM-PARAMS.                  <V4L009>
           MOVE CHDRLNB-CHDRCOY        TO ITDM-ITEMCOY.                 <V4L009>
           MOVE TH611                  TO ITDM-ITEMTABL.                <V4L009>
           MOVE CHDRLNB-CNTCURR        TO WSAA-TH611-CURRCODE.          <V4L009>
           MOVE WSAA-TH611-ITEM        TO ITDM-ITEMITEM.                <V4L009>
           MOVE CHDRLNB-OCCDATE        TO ITDM-ITMFRM.                  <V4L009>
           MOVE  BEGN                  TO ITDM-FUNCTION.                <V4L009>

           CALL 'ITDMIO'               USING ITDM-PARAMS.               <V4L009>

           IF ITDM-STATUZ              NOT = '****' AND                 <V4L009>
              ITDM-STATUZ              NOT = 'ENDP'                     <V4L009>
              MOVE ITDM-PARAMS         TO SYSR-PARAMS                   <V4L009>
              PERFORM 600-FATAL-ERROR                                   <V4L009>
           END-IF.                                                      <V4L009>
                                                                        <V4L009>
       850-EXIT.                                                        <V4L009>
           EXIT.                                                        <V4L009>
      *                                                                 <V4L009>
      ***************************                                       <V4L009>
       950-FIND-BILLFREQ  SECTION.                                      <V4L009>
      ***************************                                       <V4L009>
      *                                                                 <V4L009>
       950-READ.                                                        <V4L009>
      *****                                                             <V4L009>
      *    Dummy paragraph to find billing frequency.                   <V4L009>
      *****                                                             <V4L009>
       950-EXIT.                                                        <V4L009>
           EXIT.                                                        <V4L009>
      *                                                                 <V4L009>
      /                                                                 <A07303>
       1400-READ-FOR-BNFY SECTION.                                      <A07303>
      ****************************                                      <A07303>
      *                                                                 <A07303>
       1400-PARA.                                                       <A07303>
      *                                                                 <A07303>
      **** MOVE SPACES                 TO BNFYLNB-DATA-KEY.     <LA5070><A07303>
                                                                        <A07303>
      **** MOVE WSSP-COMPANY           TO BNFYLNB-CHDRCOY.      <LA5070><A07303>
      **** MOVE CHDRLNB-CHDRNUM        TO BNFYLNB-CHDRNUM.      <LA5070><A07303>
                                                                        <A07303>
      **** MOVE BEGN                   TO BNFYLNB-FUNCTION.     <LA5070><A07303>
           ADD 1                       TO WSAA-IWC                      <V76L01>
                                                                        <A07303>
           CALL 'BNFYLNBIO'         USING BNFYLNB-PARAMS.               <A07303>
                                                                        <A07303>
           IF (BNFYLNB-STATUZ          NOT = O-K) AND                   <A07303>
              (BNFYLNB-STATUZ          NOT = ENDP)                      <A07303>
               MOVE BNFYLNB-PARAMS     TO SYSR-PARAMS                   <A07303>
               PERFORM 600-FATAL-ERROR                                  <A07303>
           END-IF.                                                      <A07303>
                                                                        <A07303>
           IF (BNFYLNB-CHDRCOY      NOT = CHDRLNB-CHDRCOY)              <A07303>
           OR (BNFYLNB-CHDRNUM      NOT = CHDRLNB-CHDRNUM)              <A07303>
           OR (BNFYLNB-STATUZ           = ENDP)                         <A07303>
              MOVE ENDP                TO BNFYLNB-STATUZ                <LA5070>
              GO TO 1400-EXIT                                           <A07303>
           END-IF.                                                      <A07303>
                                                                        <A07303>
           IF BNFYLNB-EFFDATE       NOT = CHDRLNB-OCCDATE               <A07303>
              MOVE SPACES              TO ERMS-ERRMESG-REC              <A07303>
              MOVE E977                TO ERMS-EROR                     <A07303>
              MOVE SPACES              TO S6378-LIFE                    <A07303>
                                          S6378-JLIFE                   <A07303>
                                          S6378-COVERAGE                <A07303>
                                          S6378-RIDER                   <A07303>
              MOVE 0                   TO S6378-PAYRSEQNO               <A07303>
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES                               <A07303>
              MOVE ENDP                TO BNFYLNB-STATUZ                <LA5070>
              GO TO 1400-EXIT                                           <LA5070>
           END-IF.                                                      <A07303>
      ****  Check that the Beneficaries is not dead.                    <A06596>
           MOVE BNFYLNB-BNYCLT         TO WSAA-CLNTNUM-IO.              <A06596>
           PERFORM A3000-CALL-CLTSIO.                                   <A06596>
           IF CLTS-STATUZ               = O-K                           <A06596>
              IF CLTS-CLTDOD        NOT = VRCM-MAX-DATE                 <A06596>
              AND CHDRLNB-OCCDATE       > CLTS-CLTDOD                   <A06596>
                  MOVE SPACES          TO ERMS-ERRMESG-REC              <A06596>
                  MOVE W343            TO ERMS-EROR                     <A06596>
                  MOVE SPACES          TO S6378-COVERAGE                <A06596>
                                          S6378-RIDER                   <A06596>
                  MOVE ZEROES          TO S6378-PAYRSEQNO               <A06596>
                  MOVE 'BN'            TO WSAA-EXTRA-MSGPFX             <A06596>
                  PERFORM 1800-ERROR-MESSAGES                           <A06596>
              END-IF                                                    <A06596>
           END-IF.                                                      <A06596>
                                                                        <LA5070>
           MOVE NEXTR                  TO BNFYLNB-FUNCTION.             <LA5070>
           MOVE BNFYLNB-BNYTYPE        TO WSAA-BNYTYPE (WSAA-IWC).      <V76L01>
      *                                                                 <A07303>
       1400-EXIT.                                                       <A07303>
           EXIT.                                                        <A07303>
      /
      *
       1500-CHECK-FOLLOW-UPS      SECTION.
      ************************************
      *
       1510-READ-STATUS-TABLE.
           MOVE SPACES                 TO ITEM-DATA-KEY.
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE T5661                  TO ITEM-ITEMTABL.
      *    MOVE FLUPLNB-FUPCODE        TO ITEM-ITEMITEM.                <V72L11>
           MOVE WSSP-LANGUAGE          TO WSAA-T5661-LANG.              <V72L11>
           MOVE FLUPLNB-FUPCODE        TO WSAA-T5661-FUPCODE.           <V72L11>
           MOVE WSAA-T5661-KEY         TO ITEM-ITEMITEM.                <V72L11>
           MOVE 'READR'                TO ITEM-FUNCTION.
           CALL 'ITEMIO'               USING ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = O-K
                                   AND NOT = MRNF
               MOVE ITEM-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
           IF ITEM-STATUZ              = MRNF
               MOVE ITEM-PARAMS        TO SYSR-PARAMS
               MOVE E351               TO SCRN-ERROR-CODE
               MOVE 'Y' TO WSSP-EDTERROR
               GO TO 1590-EXIT.
      *
           MOVE ITEM-GENAREA TO T5661-T5661-REC.
      *
      *  Assume the current status will not be found,
      *     therefore, the follow-up is outstanding.
      *
           MOVE 'Y'                    TO WSAA-OUTSTANFLU.
           PERFORM 1520-OUTSTANDING-CHECK
                   VARYING WSAA-X FROM 1 BY 1
                   UNTIL WSAA-X > 10
                      OR WSAA-OUTSTANFLU = 'N'.
      *                                                                 <FUPLET>
      *  Set indicator if outstanding Follow Up found. This will be     <FUPLET>
      *  lost on next read as we no longer abort on first outstanding   <FUPLET>
      *  Follow Up.                                                     <FUPLET>
      *                                                                 <FUPLET>
           IF WSAA-OUTSTANFLU             = 'Y'                         <FUPLET>
               MOVE WSAA-OUTSTANFLU      TO WSAA-OUTFLUP-FOUND          <FUPLET>
           END-IF.                                                      <FUPLET>
      *                                                                 <FUPLET>
      *  Check if the Follow Up also requires Doctor details.           <FUPLET>
      *                                                                 <FUPLET>
           IF T5661-ZDOCIND               = 'Y'                         <FUPLET>
               MOVE 'Y'                  TO WSAA-DOC-REQD               <FUPLET>
           END-IF.                                                      <FUPLET>
                                                                        <FUPLET>
           GO TO 1530-READ-NEXT-FOLLOW-UP.
      *
      *  If any one of the codes matches, it is not outstanding.
      *
       1520-OUTSTANDING-CHECK.
           IF FLUPLNB-FUPSTAT = T5661-FUPOSS (WSAA-X)
              MOVE 'N'                 TO WSAA-OUTSTANFLU.

      *
      *  If a match was not found, this is all we need to know.
      *
       1530-READ-NEXT-FOLLOW-UP.
      **** IF WSAA-OUTSTANFLU = 'Y'                                     <FUPLET>
           IF WSAA-OUTFLUP-FOUND          = 'Y'        AND              <FUPLET>
              WSAA-DOC-REQD               = 'Y'                         <FUPLET>
              MOVE ENDP                  TO FLUPLNB-STATUZ              <FUPLET>
              GO TO 1590-EXIT.                                          <FUPLET>
      *
      *  So far, an outstanding follow-up has not been found,
      *    so read the next one.
      *
           MOVE NEXTR                  TO FLUPLNB-FUNCTION.
      *
           CALL 'FLUPLNBIO'  USING FLUPLNB-PARAMS.
           IF (FLUPLNB-STATUZ          NOT = O-K)  AND
              (FLUPLNB-STATUZ          NOT = ENDP)
               MOVE FLUPLNB-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
           IF (FLUPLNB-CHDRCOY         NOT = CHDRLNB-CHDRCOY)  OR
              (FLUPLNB-CHDRNUM         NOT = CHDRLNB-CHDRNUM)
               MOVE ENDP               TO FLUPLNB-STATUZ.
      *
       1590-EXIT.
           EXIT.
      *
      /
      *****************************************************************
      *     LIFE LEVEL VALIDATION
      *****************************************************************
      *
       1600-LIFE-LEVEL SECTION.
      *************************
      *
       1610-LIFE-LEVEL.

           CALL 'LIFELNBIO'            USING LIFELNB-PARAMS.

           IF  LIFELNB-STATUZ          NOT = ENDP AND NOT = O-K
               MOVE LIFELNB-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.

           IF (LIFELNB-CHDRCOY         NOT = WSSP-COMPANY
           OR  LIFELNB-CHDRNUM         NOT = CHDRLNB-CHDRNUM
           OR  LIFELNB-STATUZ          = ENDP)
           AND LIFELNB-FUNCTION        = BEGN
              MOVE SPACES              TO ERMS-ERRMESG-REC
              MOVE E355                TO ERMS-EROR
              MOVE SPACES              TO S6378-LIFE
                                          S6378-JLIFE
                                          S6378-COVERAGE
                                          S6378-RIDER
              MOVE 0                   TO S6378-PAYRSEQNO               <014>
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES.

           IF  LIFELNB-CHDRCOY         NOT = WSSP-COMPANY
           OR  LIFELNB-CHDRNUM         NOT = CHDRLNB-CHDRNUM
           OR  LIFELNB-STATUZ          = ENDP
               MOVE ENDP               TO LIFELNB-STATUZ
               GO TO 1690-EXIT.                                         <LA3392>

      * Check age next birthday.
      *                                                                 <029>
      *    MOVE LIFELNB-CLTDOB         TO DTC3-INT-DATE-1.              <029>
      *    MOVE CHDRLNB-OCCDATE        TO DTC3-INT-DATE-2.              <029>
      *    MOVE '01'                   TO DTC3-FREQUENCY.               <029>
      *    CALL 'DATCON3'              USING DTC3-DATCON3-REC.          <029>
      *    IF DTC3-STATUZ              NOT = O-K                        <029>
      *       MOVE SPACES              TO ERMS-ERRMESG-REC              <029>
      *       MOVE DTC3-STATUZ         TO ERMS-EROR                     <029>
      *       MOVE LIFELNB-LIFE        TO S6378-LIFE                    <029>
      *       MOVE SPACES              TO S6378-JLIFE                   <029>
      *                                   S6378-COVERAGE                <029>
      *                                   S6378-RIDER                   <029>
      *       MOVE 0                   TO S6378-PAYRSEQNO          <014><029>
      *       PERFORM 1800-ERROR-MESSAGES                               <029>
      *       MOVE NEXTR               TO LIFELNB-FUNCTION              <029>
      *       GO TO 1690-EXIT.                                          <029>

      *     ADD 0.999, DTC3-FREQ-FACTOR GIVING WSBB-ANB-INT.            <029>
      *                                                                 <029>
      *                                                                 <A06596>
      * Display error if RCD is greater than the date of                <A06596>
      * death of the life assured(s).                                   <A06596>
      *                                                                 <A06596>
           PERFORM 1900-CALC-AGE.                                       <029>
           IF AGEC-STATUZ              NOT = O-K                        <029>
              MOVE SPACES              TO ERMS-ERRMESG-REC              <029>
              MOVE AGEC-STATUZ         TO ERMS-EROR                     <029>
              MOVE LIFELNB-LIFE        TO S6378-LIFE                    <029>
              MOVE SPACES              TO S6378-JLIFE                   <029>
                                          S6378-COVERAGE                <029>
                                          S6378-RIDER                   <029>
              MOVE 0                   TO S6378-PAYRSEQNO               <029>
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES                               <029>
              MOVE NEXTR               TO LIFELNB-FUNCTION              <029>
              GO TO 1690-EXIT.                                          <029>
                                                                        <029>
           MOVE WSAA-ANB TO WSBB-ANB-INT.                               <029>
      *                                                                 <029>
            IF WSBB-ANB-INT            NOT = LIFELNB-ANB-AT-CCD
                MOVE LIFELNB-LIFE      TO S6378-LIFE
                MOVE LIFELNB-JLIFE     TO S6378-JLIFE
                MOVE SPACES            TO S6378-COVERAGE
                                          S6378-RIDER
                MOVE 0                 TO S6378-PAYRSEQNO               <014>
                MOVE SPACES            TO  ERMS-ERRMESG-REC
      ***       MOVE U024              TO ERMS-EROR                     <016>
                MOVE H015              TO ERMS-EROR                     <016>
                MOVE SPACES            TO WSAA-EXTRA-MSGPFX             <A06596>
                PERFORM 1800-ERROR-MESSAGES.

      * Check whether a cover exists for this life.
           MOVE SPACES                 TO COVTLNB-PARAMS.
           MOVE CHDRLNB-CHDRCOY        TO COVTLNB-CHDRCOY.
           MOVE CHDRLNB-CHDRNUM        TO COVTLNB-CHDRNUM.
           MOVE LIFELNB-LIFE           TO COVTLNB-LIFE.
           MOVE LIFELNB-JLIFE          TO COVTLNB-JLIFE.
           MOVE ZERO                   TO COVTLNB-SEQNBR.
           MOVE BEGN                   TO COVTLNB-FUNCTION.
           CALL 'COVTLNBIO'            USING COVTLNB-PARAMS.
           IF  COVTLNB-STATUZ          NOT = ENDP
                                         AND NOT = O-K
               MOVE COVTLNB-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.

           IF COVTLNB-CHDRCOY          NOT = CHDRLNB-CHDRCOY
           OR COVTLNB-CHDRNUM          NOT = CHDRLNB-CHDRNUM
           OR COVTLNB-LIFE             NOT = LIFELNB-LIFE
           OR COVTLNB-STATUZ           = ENDP
              MOVE LIFELNB-LIFE        TO S6378-LIFE
              MOVE SPACES              TO ERMS-ERRMESG-REC
              MOVE G066                TO ERMS-EROR
              MOVE SPACES              TO S6378-JLIFE,
                                          S6378-COVERAGE,
                                          S6378-RIDER
              MOVE 0                   TO S6378-PAYRSEQNO               <014>
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES.

      * Only call CSNCALC once per life (in joint life cases, there     <CAS1.0>
      * will be more than 1 LIFE record for the same life.)             <CAS1.0>
                                                                        <CAS1.0>
           IF LIFELNB-LIFE             NOT = WSAA-LIFE                  <CAS1.0>
               PERFORM 1150-CALL-CSNCALC                                <CAS1.0>
               MOVE LIFELNB-LIFE       TO WSAA-LIFE                     <CAS1.0<
           END-IF.                                                      <CAS1.0>
      *                                                                 <V71L12>
           MOVE SPACES                 TO WSAA-QUESTSET.                <LA4216>
           PERFORM 1640-READ-TR675.                                     <V71L12>
      *                                                                 <V71L12>
           IF WSAA-QUESTSET         NOT = SPACE                         <V71L12>
              PERFORM 1650-UNDERWRITING                                 <V71L12>
           END-IF.                                                      <V71L12>
                                                                        <V71L12>
           IF WSAA-REVISIT-Q                                            <V71L12>
              MOVE SPACES              TO ERMS-ERRMESG-REC              <V71L12>
              MOVE E591                TO ERMS-EROR                     <V71L12>
              MOVE LIFELNB-LIFE        TO S6378-LIFE                    <V71L12>
              MOVE LIFELNB-JLIFE       TO S6378-JLIFE                   <V71L12>
              MOVE SPACES              TO S6378-COVERAGE                <V71L12>
                                          S6378-RIDER                   <V71L12>
              MOVE ZEROES              TO S6378-PAYRSEQNO               <V71L12>
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES                               <V71L12>
           END-IF.                                                      <V71L12>
                                                                        <V71L12>
           INITIALIZE                     UNDC-PARAMS.                  <V71L12>
           MOVE LIFELNB-CHDRCOY        TO UNDC-CHDRCOY.                 <V71L12>
           MOVE LIFELNB-CHDRNUM        TO UNDC-CHDRNUM.                 <V71L12>
           MOVE LIFELNB-LIFE           TO UNDC-LIFE.                    <V71L12>
           MOVE '00'                   TO UNDC-COVERAGE.                <V71L12>
           MOVE '00'                   TO UNDC-RIDER.                   <V71L12>
           MOVE UNDCREC                TO UNDC-FORMAT.                  <V71L12>
           MOVE BEGN                   TO UNDC-FUNCTION.                <V71L12>
                                                                        <V71L12>
           PERFORM 1660-CHECK-UNDC  UNTIL UNDC-STATUZ = ENDP.           <V71L12>
                                                                        <V71L12>
                                                                        <CAS1.0>
           IF  WSSP-FLAG               NOT = 'I'                        <FUPLET>
      ***  AND UNDL-UNDWFLAG           NOT = 'Y'                        <V71L12>
           AND WSKY-BATC-BATCTRCDE     NOT = 'T642'
               PERFORM A100-CHDR-OR-RISK                                <V42006>
      ****     PERFORM 1G00-CALL-HCRTFUP                        <LA2103><V42006>
           END-IF.                                                      <FUPLET>
                                                                        <FUPLET>
      **** Since 1150-CALL-CSNCALC will distort the pointer of LIFLNB   <LA3392>
      **** Use READR to correct the pointer.                            <LA3392>
      ****                                                              <LA3392>
           MOVE READR                  TO LIFELNB-FUNCTION.             <LA3392>
                                                                        <LA3392>
           CALL 'LIFELNBIO'            USING LIFELNB-PARAMS.            <LA3392>
                                                                        <LA3392>
           IF  LIFELNB-STATUZ          NOT = MRNF AND NOT = O-K         <LA3392>
               MOVE LIFELNB-PARAMS     TO SYSR-PARAMS                   <LA3392>
               MOVE LIFELNB-STATUZ     TO SYSR-STATUZ                   <LA3392>
               PERFORM 600-FATAL-ERROR                                  <LA3392>
           END-IF.                                                      <LA3392>
      ***                                                               <LA3392>
           MOVE NEXTR                  TO LIFELNB-FUNCTION.
           ADD 1                       TO IDX.                          <NB010>
           MOVE LIFELNB-LIFCNUM        TO WSAA-LIFCNUM(IDX).            <NB010>

       1690-EXIT.
            EXIT.
      *                                                                 <V71L12>
      * At this stage no UNDQ records exist for the life so we need to  <V71L12>
      * check if this is OK or not. I.e. check if any of the questions  <V71L12>
      * on T6771 for this product are mandatory. If yes then do not     <V71L12>
      * perform any underwriting but display a message telling the user <V71L12>
      * to revisit the questionnaire. All mandatory questions must be   <V71L12>
      * answered before any underwriting can be determined.             <V71L12>
      *                                                                 <V71L12>
      *                                                                 <V71L12>
      * This section calls the underwriting subroutine.                 <V71L12>
      *                                                                 <V71L12>
      /                                                                 <V71L12>
       1640-READ-TR675 SECTION.                                         <V71L12>
      *************************                                         <V71L12>
       1641-START.                                                      <V71L12>
                                                                        <V71L12>
           MOVE SPACE                  TO WSAA-REVISIT-QUESTIONNAIRE.   <V71L12>
                                                                        <V71L12>
           MOVE SPACES                 TO ITDM-PARAMS.                  <V71L12>
           MOVE 'IT'                   TO ITDM-ITEMPFX.                 <V71L12>
           MOVE WSSP-COMPANY           TO ITDM-ITEMCOY.                 <V71L12>
           MOVE TR675                  TO ITDM-ITEMTABL.                <V71L12>
           MOVE CHDRLNB-CNTTYPE        TO ITDM-ITEMITEM.                <V71L12>
           MOVE CHDRLNB-OCCDATE        TO ITDM-ITMFRM.                  <V71L12>
           MOVE BEGN                   TO ITDM-FUNCTION.                <V71L12>
                                                                        <V71L12>
           CALL 'ITDMIO'            USING ITDM-PARAMS.                  <V71L12>
                                                                        <V71L12>
           IF ITDM-STATUZ           NOT = O-K                           <V71L12>
                                AND NOT = ENDP                          <V71L12>
              MOVE ITDM-STATUZ         TO SYSR-STATUZ                   <V71L12>
              MOVE ITDM-PARAMS         TO SYSR-PARAMS                   <V71L12>
              PERFORM 600-FATAL-ERROR                                   <V71L12>
           END-IF.                                                      <V71L12>
                                                                        <V71L12>
           IF  ITDM-ITEMPFX         NOT = 'IT'                          <V71L12>
           OR  ITDM-ITEMCOY         NOT = WSSP-COMPANY                  <V71L12>
           OR  ITDM-ITEMTABL        NOT = TR675                         <V71L12>
           OR  ITDM-ITEMITEM        NOT = CHDRLNB-CNTTYPE               <V71L12>
               MOVE ENDP               TO ITDM-STATUZ                   <V71L12>
           END-IF.                                                      <V71L12>
                                                                        <V71L12>
           IF ITDM-STATUZ               = ENDP                          <V71L12>
              GO TO 1649-EXIT                                           <V71L12>
           END-IF.                                                      <V71L12>
                                                                        <V71L12>
           MOVE ITDM-GENAREA           TO TR675-TR675-REC.              <V71L12>
           MOVE SPACE                  TO WSAA-QUESTSET.                <V71L12>
      *                                                                 <V71L12>
      *  Get the Question Set from TR675                                <V71L12>
      *                                                                 <V71L12>
           IF LIFELNB-CLTSEX           = 'M'                            <V71L12>
              IF LIFELNB-ANB-AT-CCD    <= TR675-AGE-01                  <V71L12>
                 MOVE TR675-QUESTSET-01                                 <V71L12>
                                       TO WSAA-QUESTSET                 <V71L12>
                 GO TO 1642-READ-T6771                                  <V71L12>
              END-IF                                                    <V71L12>
              IF LIFELNB-ANB-AT-CCD    >  TR675-AGE-01 AND              <V71L12>
                                       <= TR675-AGE-02                  <V71L12>
                 MOVE TR675-QUESTSET-03                                 <V71L12>
                                       TO WSAA-QUESTSET                 <V71L12>
                 GO TO 1642-READ-T6771                                  <V71L12>
              END-IF                                                    <V71L12>
              IF LIFELNB-ANB-AT-CCD    >  TR675-AGE-02 AND              <V71L12>
                                       <= TR675-AGE-03                  <V71L12>
                 MOVE TR675-QUESTSET-05                                 <V71L12>
                                       TO WSAA-QUESTSET                 <V71L12>
                 GO TO 1642-READ-T6771                                  <V71L12>
              END-IF                                                    <V71L12>
              IF LIFELNB-ANB-AT-CCD    >  TR675-AGE-03 AND              <V71L12>
                                       <= TR675-AGE-04                  <V71L12>
                 MOVE TR675-QUESTSET-07                                 <V71L12>
                                       TO WSAA-QUESTSET                 <V71L12>
                 GO TO 1642-READ-T6771                                  <V71L12>
              END-IF                                                    <V71L12>
              IF LIFELNB-ANB-AT-CCD    >  TR675-AGE-04 AND              <V71L12>
                                       <= TR675-AGE-05                  <V71L12>
                 MOVE TR675-QUESTSET-09                                 <V71L12>
                                       TO WSAA-QUESTSET                 <V71L12>
                 GO TO 1642-READ-T6771                                  <V71L12>
              END-IF                                                    <V71L12>
              IF LIFELNB-ANB-AT-CCD    >  TR675-AGE-05 AND              <V71L12>
                                       <= TR675-AGE-06                  <V71L12>
                 MOVE TR675-QUESTSET-11                                 <V71L12>
                                       TO WSAA-QUESTSET                 <V71L12>
                 GO TO 1642-READ-T6771                                  <V71L12>
              END-IF                                                    <V71L12>
           ELSE                                                         <V71L12>
              IF LIFELNB-ANB-AT-CCD    <= TR675-AGE-01                  <V71L12>
                 MOVE TR675-QUESTSET-02                                 <V71L12>
                                       TO WSAA-QUESTSET                 <V71L12>
                 GO TO 1642-READ-T6771                                  <V71L12>
              END-IF                                                    <V71L12>
              IF LIFELNB-ANB-AT-CCD    >  TR675-AGE-01 AND              <V71L12>
                                       <= TR675-AGE-02                  <V71L12>
                 MOVE TR675-QUESTSET-04                                 <V71L12>
                                       TO WSAA-QUESTSET                 <V71L12>
                 GO TO 1642-READ-T6771                                  <V71L12>
              END-IF                                                    <V71L12>
              IF LIFELNB-ANB-AT-CCD    >  TR675-AGE-02 AND              <V71L12>
                                       <= TR675-AGE-03                  <V71L12>
                 MOVE TR675-QUESTSET-06                                 <V71L12>
                                       TO WSAA-QUESTSET                 <V71L12>
                 GO TO 1642-READ-T6771                                  <V71L12>
              END-IF                                                    <V71L12>
              IF LIFELNB-ANB-AT-CCD    >  TR675-AGE-03 AND              <V71L12>
                                       <= TR675-AGE-04                  <V71L12>
                 MOVE TR675-QUESTSET-08                                 <V71L12>
                                       TO WSAA-QUESTSET                 <V71L12>
                 GO TO 1642-READ-T6771                                  <V71L12>
              END-IF                                                    <V71L12>
              IF LIFELNB-ANB-AT-CCD    >  TR675-AGE-04 AND              <V71L12>
                                       <= TR675-AGE-05                  <V71L12>
                 MOVE TR675-QUESTSET-10                                 <V71L12>
                                       TO WSAA-QUESTSET                 <V71L12>
                 GO TO 1642-READ-T6771                                  <V71L12>
              END-IF                                                    <V71L12>
              IF LIFELNB-ANB-AT-CCD    >  TR675-AGE-05 AND              <V71L12>
                                       <= TR675-AGE-06                  <V71L12>
                 MOVE TR675-QUESTSET-12                                 <V71L12>
                                       TO WSAA-QUESTSET                 <V71L12>
                 GO TO 1642-READ-T6771                                  <V71L12>
              END-IF                                                    <V71L12>
           END-IF.                                                      <V71L12>
      *                                                                 <V71L12>
       1642-READ-T6771.                                                 <V71L12>
      *                                                                 <V71L12>
      * If Underwriting is required on the product and the BMI Basis    <V71L12>
      * exists on TR675, read T6769 (Underwriting based on BMI) to get  <V71L12>
      * the BMI Factor for the given BMI Basis.                         <V71L12>
      *                                                                 <V71L12>
           MOVE SPACES                 TO ITDM-DATA-KEY.                <V71L12>
           MOVE WSSP-COMPANY           TO ITDM-ITEMCOY.                 <V71L12>
           MOVE T6771                  TO ITDM-ITEMTABL.                <V71L12>
           MOVE WSAA-QUESTSET          TO ITDM-ITEMITEM.                <V71L12>
           MOVE CHDRLNB-OCCDATE        TO ITDM-ITMFRM.                  <V71L12>
           MOVE BEGN                   TO ITDM-FUNCTION.                <V71L12>
      *                                                                 <V71L12>
           CALL 'ITDMIO'            USING ITDM-PARAMS.                  <V71L12>
                                                                        <V71L12>
           IF  ITDM-STATUZ         NOT = O-K                            <V71L12>
           AND ITDM-STATUZ         NOT = ENDP                           <V71L12>
               MOVE ITDM-PARAMS        TO SYSR-PARAMS                   <V71L12>
               MOVE ITDM-STATUZ        TO SYSR-STATUZ                   <V71L12>
               PERFORM 600-FATAL-ERROR                                  <V71L12>
           END-IF.                                                      <V71L12>
      *                                                                 <V71L12>
           IF ITDM-ITEMCOY         NOT = WSSP-COMPANY                   <V71L12>
           OR ITDM-ITEMTABL        NOT = T6771                          <V71L12>
           OR ITDM-ITEMITEM        NOT = WSAA-QUESTSET                  <V71L12>
           OR ITDM-STATUZ              = ENDP                           <V71L12>
              MOVE SPACES              TO ERMS-ERRMESG-REC              <V71L12>
              MOVE RPID                TO ERMS-EROR                     <V71L12>
              MOVE ZEROES              TO S6378-PAYRSEQNO               <V71L12>
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES                               <V71L12>
           ELSE                                                         <V71L12>
               MOVE ITDM-GENAREA       TO T6771-T6771-REC               <V71L12>
           END-IF.                                                      <V71L12>
                                                                        <V71L12>
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 40               <V71L12>
              IF T6771-QUESTST(SUB)     = 'M'                           <V71L12>
                 SET WSAA-REVISIT-Q    TO TRUE                          <V71L12>
              END-IF                                                    <V71L12>
           END-PERFORM.                                                 <V71L12>
                                                                        <V71L12>
       1649-EXIT.                                                       <V71L12>
           EXIT.                                                        <V71L12>
      /                                                                 <V71L12>
       1650-UNDERWRITING SECTION.                                       <V71L12>
      ***************************                                       <V71L12>
       1651-START.                                                      <V71L12>
                                                                        <V71L12>
      *                                                                 <V71L12>
      * Ensure that all mandatory lifestyle questions have been         <V71L12>
      * answered before determining any underwriting rules.             <V71L12>
      *                                                                 <V71L12>
           MOVE SPACES                 TO UNDQ-PARAMS.                  <V71L12>
           MOVE LIFELNB-CHDRCOY        TO UNDQ-CHDRCOY.                 <V71L12>
           MOVE LIFELNB-CHDRNUM        TO UNDQ-CHDRNUM.                 <V71L12>
           MOVE LIFELNB-LIFE           TO UNDQ-LIFE.                    <V71L12>
           MOVE LIFELNB-JLIFE          TO UNDQ-JLIFE.                   <V71L12>
           MOVE UNDQREC                TO UNDQ-FORMAT.                  <V71L12>
           MOVE BEGN                   TO UNDQ-FUNCTION.                <V71L12>
                                                                        <V71L12>
           CALL 'UNDQIO'            USING UNDQ-PARAMS.                  <V71L12>
                                                                        <V71L12>
           IF  UNDQ-STATUZ          NOT = O-K                           <V71L12>
           AND UNDQ-STATUZ          NOT = ENDP                          <V71L12>
               MOVE UNDQ-STATUZ        TO SYSR-STATUZ                   <V71L12>
               MOVE UNDQ-PARAMS        TO SYSR-PARAMS                   <V71L12>
               PERFORM 600-FATAL-ERROR                                  <V71L12>
           END-IF.                                                      <V71L12>
                                                                        <V71L12>
           IF  UNDQ-CHDRCOY         NOT = LIFELNB-CHDRCOY               <V71L12>
           OR  UNDQ-CHDRNUM         NOT = LIFELNB-CHDRNUM               <V71L12>
           OR  UNDQ-LIFE            NOT = LIFELNB-LIFE                  <V71L12>
           OR  UNDQ-JLIFE           NOT = LIFELNB-JLIFE                 <V71L12>
               MOVE ENDP               TO UNDQ-STATUZ                   <V71L12>
           END-IF.                                                      <V71L12>
      *                                                                 <V71L12>
      * If no lifestyle records exist for this life then check that     <V71L12>
      * there are no mandatory questions for this product type (T6771). <V71L12>
      *                                                                 <V71L12>
           IF  UNDQ-STATUZ              = ENDP                          <V71L12>
               IF WSAA-REVISIT-Q                                        <V71L12>
                  GO TO 1659-EXIT                                       <V71L12>
               END-IF                                                   <V71L12>
           ELSE                                                         <V71L12>
               MOVE SPACE              TO WSAA-REVISIT-QUESTIONNAIRE    <V71L12>
           END-IF.                                                      <V71L12>
                                                                        <V71L12>
           MOVE SPACES                 TO UNDL-PARAMS.                  <V71L12>
           MOVE LIFELNB-CHDRCOY        TO UNDL-CHDRCOY.                 <V71L12>
           MOVE LIFELNB-CHDRNUM        TO UNDL-CHDRNUM.                 <V71L12>
           MOVE LIFELNB-LIFE           TO UNDL-LIFE.                    <V71L12>
           MOVE LIFELNB-JLIFE          TO UNDL-JLIFE.                   <V71L12>
           MOVE UNDLREC                TO UNDL-FORMAT.                  <V71L12>
           MOVE READH                  TO UNDL-FUNCTION.                <V71L12>
           IF WSSP-FLAG                 = 'I'                           <NB022>
              MOVE READR               TO UNDL-FUNCTION                 <NB022>
           END-IF.                                                      <NB022>
                                                                        <V71L12>
           CALL 'UNDLIO'            USING UNDL-PARAMS.                  <V71L12>
                                                                        <V71L12>
           IF  UNDL-STATUZ          NOT = O-K                           <V71L12>
              MOVE UNDL-STATUZ         TO SYSR-STATUZ                   <V71L12>
              MOVE UNDL-PARAMS         TO SYSR-PARAMS                   <V71L12>
              PERFORM 600-FATAL-ERROR                                   <V71L12>
           END-IF.                                                      <V71L12>
                                                                        <V71L12>
           INITIALIZE                     UNDW-UNDWSUB-REC.             <V71L12>
           MOVE CHDRLNB-CNTBRANCH      TO UNDW-BRANCH.                  <V71L12>
           MOVE WSSP-LANGUAGE          TO UNDW-LANGUAGE.                <V71L12>
           MOVE CHDRLNB-CNTCURR        TO UNDW-CURRENCY.                <V71L12>
           MOVE LIFELNB-CHDRCOY        TO UNDW-CHDRCOY.                 <V71L12>
           MOVE LIFELNB-CHDRNUM        TO UNDW-CHDRNUM.                 <V71L12>
           MOVE LIFELNB-LIFE           TO UNDW-LIFE.                    <V71L12>
           MOVE LIFELNB-JLIFE          TO UNDW-JLIFE.                   <V71L12>
           MOVE LIFELNB-CURRFROM       TO UNDW-EFFDATE.                 <V71L12>
           MOVE LIFELNB-TRANNO         TO UNDW-TRANNO.                  <V71L12>
           MOVE LIFELNB-LIFCNUM        TO UNDW-CLNTNUM.                 <V71L12>
           MOVE WSKY-BATC-BATCTRCDE    TO UNDW-BATCTRCDE.               <V71L12>
                                                                        <V71L12>
           MOVE UNDL-BMIRULE           TO UNDW-BMIRULE.                 <V71L12>
           MOVE UNDL-CLNTNUM01         TO UNDW-DOCTOR.                  <V71L12>
           MOVE WSSP-FSUCO             TO UNDW-CLNTCOY.                 <V71L12>
           MOVE VRCM-USER              TO UNDW-USER.                    <V71L12>
      *                                                                 <V71L12>
      * If in Enquiry mode (WSSP-FLAG = 'I' do not call the             <V71L12>
      * underwriting subroutine, bypass it.                             <V71L12>
      *                                                                 <V71L12>
      *                                                                 <V71L12>
      * If the underwriting completed flag on UNDLPF = 'Y' then do not  <V71L12>
      * call the underwriting subroutine.                               <V71L12>
      *                                                                 <V71L12>
            IF  UNDL-UNDWFLAG      NOT = 'Y'                            <V71L12>
            AND T6771-UNDWSUBR     NOT = SPACES                         <V71L12>
      *                                                                 <V71L12>
                CALL T6771-UNDWSUBR USING UNDW-UNDWSUB-REC              <V71L12>
      *                                                                 <V71L12>
                IF WSSP-FLAG                 = 'I'                      <NB022>
                   MOVE SPACES         TO ERMS-ERRMESG-REC              <NB022>
                   MOVE E964           TO ERMS-EROR                     <NB022>
                   MOVE UNDW-LIFE      TO S6378-LIFE                    <NB022>
                   MOVE UNDW-JLIFE     TO S6378-JLIFE                   <NB022>
                   MOVE SPACES         TO S6378-COVERAGE                <NB022>
                                          S6378-RIDER                   <NB022>
                   MOVE ZEROES         TO S6378-PAYRSEQNO               <NB022>
                   MOVE SPACES         TO WSAA-EXTRA-MSGPFX             <NB022>
                   PERFORM 1800-ERROR-MESSAGES                          <NB022>
                   IF UNDW-ERROR-CODE NOT = SPACES                      <NB022>
                      MOVE SPACES      TO ERMS-ERRMESG-REC              <NB022>
                      MOVE UNDW-ERROR-CODE TO ERMS-EROR                 <NB022>
                      MOVE UNDW-LIFE   TO S6378-LIFE                    <NB022>
                      MOVE UNDW-JLIFE  TO S6378-JLIFE                   <NB022>
                      MOVE SPACES      TO S6378-COVERAGE                <NB022>
                                          S6378-RIDER                   <NB022>
                      MOVE ZEROES      TO S6378-PAYRSEQNO               <NB022>
                      MOVE SPACES      TO WSAA-EXTRA-MSGPFX             <NB022>
                      PERFORM 1800-ERROR-MESSAGES                       <NB022>
                   END-IF                                               <NB022>
                   GO TO 1659-EXIT                                      <NB022>
                END-IF                                                  <NB022>
                                                                        <NB022>
                MOVE UNDW-OVRRULE      TO UNDL-OVRRULE                  <V71L12>
                MOVE REWRT             TO UNDL-FUNCTION                 <V71L12>
      *                                                                 <V71L12>
                CALL 'UNDLIO'       USING UNDL-PARAMS                   <V71L12>
      *                                                                 <V71L12>
                IF  UNDL-STATUZ    NOT = O-K                            <V71L12>
                    MOVE UNDL-STATUZ   TO SYSR-STATUZ                   <V71L12>
                    MOVE UNDL-PARAMS   TO SYSR-PARAMS                   <V71L12>
                    PERFORM 600-FATAL-ERROR                             <V71L12>
                END-IF                                                  <V71L12>
      *                                                                 <V71L12>
      *                                                                 <NB022>
      *         MOVE SPACES            TO ERMS-ERRMESG-REC      <NB034> <V71L12>
      *         MOVE E964              TO ERMS-EROR             <NB034> <V71L12>
      *         MOVE UNDW-LIFE         TO S6378-LIFE            <NB034> <V71L12>
      *         MOVE UNDW-JLIFE        TO S6378-JLIFE           <NB034> <V71L12>
      *         MOVE SPACES            TO S6378-COVERAGE        <NB034> <V71L12>
      *                                   S6378-RIDER           <NB034> <V71L12>
      *         MOVE ZEROES            TO S6378-PAYRSEQNO       <NB034> <V71L12>
      *         MOVE SPACES            TO WSAA-EXTRA-MSGPFX     <NB034> <A06596>
      *         PERFORM 1800-ERROR-MESSAGES                     <NB034> <V71L12>
      *                                                                 <V71L12>
      *         IF UNDW-ERROR-CODE NOT = SPACES                 <NB034> <V71L12>
      *            MOVE SPACES         TO ERMS-ERRMESG-REC      <NB034> <V71L12>
      *            MOVE UNDW-ERROR-CODE TO ERMS-EROR            <NB034> <V71L12>
      *            MOVE UNDW-LIFE      TO S6378-LIFE            <NB034> <V71L12>
      *            MOVE UNDW-JLIFE     TO S6378-JLIFE           <NB034> <V71L12>
      *            MOVE SPACES         TO S6378-COVERAGE        <NB034> <V71L12>
      *                                   S6378-RIDER           <NB034> <V71L12>
      *            MOVE ZEROES         TO S6378-PAYRSEQNO       <NB034> <V71L12>
      *            MOVE SPACES         TO WSAA-EXTRA-MSGPFX     <NB034> <A06596>
      *            PERFORM 1800-ERROR-MESSAGES                  <NB034> <V71L12>
      *         END-IF                                          <NB034> <V71L12>
           END-IF.                                                      <V71L12>
                                                                        <V71L12>
       1659-EXIT.                                                       <V71L12>
           EXIT.                                                        <V71L12>
      /                                                                 <V71L12>
       1660-CHECK-UNDC SECTION.                                         <V71L12>
      *************************                                         <V71L12>
       1660-START.                                                      <V71L12>
                                                                        <V71L12>
           CALL 'UNDCIO'          USING UNDC-PARAMS.                    <V71L12>
                                                                        <V71L12>
           IF  UNDC-STATUZ          NOT = O-K                           <V71L12>
           AND UNDC-STATUZ          NOT = ENDP                          <V71L12>
               MOVE UNDC-STATUZ        TO SYSR-STATUZ                   <V71L12>
               MOVE UNDC-PARAMS        TO SYSR-PARAMS                   <V71L12>
               PERFORM 600-FATAL-ERROR                                  <V71L12>
           END-IF.                                                      <V71L12>
                                                                        <V71L12>
           IF  UNDC-CHDRCOY         NOT = LIFELNB-CHDRCOY               <V71L12>
           OR  UNDC-CHDRNUM         NOT = LIFELNB-CHDRNUM               <V71L12>
           OR  UNDC-LIFE            NOT = LIFELNB-LIFE                  <V71L12>
               MOVE ENDP               TO UNDC-STATUZ                   <V71L12>
           END-IF.                                                      <V71L12>
                                                                        <V71L12>
           IF  UNDC-STATUZ              = O-K                           <V71L12>
           AND UNDC-SPECIND         NOT = SPACES                        <V71L12>
               MOVE SPACES             TO ERMS-ERRMESG-REC              <V71L12>
               MOVE H362               TO ERMS-EROR                     <V71L12>
               MOVE UNDC-LIFE          TO S6378-LIFE                    <V71L12>
               MOVE UNDC-JLIFE         TO S6378-JLIFE                   <V71L12>
               MOVE UNDC-COVERAGE      TO S6378-COVERAGE                <V71L12>
               MOVE UNDC-RIDER         TO S6378-RIDER                   <V71L12>
               MOVE ZEROES             TO S6378-PAYRSEQNO               <V71L12>
               MOVE SPACES             TO WSAA-EXTRA-MSGPFX             <A06596>
               PERFORM 1800-ERROR-MESSAGES                              <V71L12>
           END-IF.                                                      <V71L12>
                                                                        <V71L12>
           MOVE NEXTR                  TO UNDC-FUNCTION.                <V71L12>
                                                                        <V71L12>
       1669-EXIT.                                                       <V71L12>
           EXIT.                                                        <V71L12>
      /
      *****************************************************************
      *     COMPONENT LEVEL VALIDATION
      *****************************************************************
      *
       1700-COMPONENT-LEVEL SECTION.
      ******************************
      *
       1710-COMPONENT-LEVEL.

      * Set up coverage and rider numbers for error messages
           MOVE COVTLNB-COVERAGE       TO S6378-COVERAGE.
           IF COVTLNB-RIDER = '00' OR '  '
              MOVE SPACES              TO S6378-RIDER
           ELSE
              MOVE COVTLNB-RIDER       TO S6378-RIDER.
           MOVE COVTLNB-LIFE           TO S6378-LIFE.
           MOVE SPACES                 TO S6378-JLIFE.
           MOVE 0                      TO S6378-PAYRSEQNO.              <014>

      **** Check the Coverage/Rider Definition T5687 for a single       <A05691>
      **** Premium indicator of 'Y'.                                    <A05691>
                                                                        <A05691>
           MOVE SPACES                 TO ITDM-PARAMS.                  <A05691>
           MOVE T5687                  TO ITDM-ITEMTABL.                <A05691>
           MOVE COVTLNB-CRTABLE        TO ITDM-ITEMITEM.                <A05691>
           MOVE CHDRLNB-OCCDATE        TO ITDM-ITMFRM.                  <A05691>
           MOVE BEGN                   TO ITDM-FUNCTION.                <A05691>
           MOVE WSSP-COMPANY           TO ITDM-ITEMCOY.                 <A05691>
           MOVE 'IT'                   TO ITDM-ITEMPFX.                 <A05691>
                                                                        <A05691>
           CALL 'ITDMIO' USING ITDM-PARAMS.                             <A05691>
                                                                        <A05691>
           IF  ITDM-STATUZ        NOT  =  O-K  AND                      <A05691>
               ITDM-STATUZ        NOT  =  ENDP                          <A05691>
               MOVE ITDM-PARAMS        TO SYSR-PARAMS                   <A05691>
               PERFORM 600-FATAL-ERROR                                  <A05691>
           END-IF.                                                      <A05691>
                                                                        <A05691>
           IF  ITDM-ITEMCOY       NOT  =  WSSP-COMPANY                  <A05691>
           OR  ITDM-ITEMTABL      NOT  =  T5687                         <A05691>
           OR  ITDM-ITEMITEM      NOT  =  COVTLNB-CRTABLE               <A05691>
           OR  ITDM-VALIDFLAG     NOT  =  '1'                           <A05691>
           OR  ITDM-STATUZ             =  ENDP                          <A05691>
               MOVE ITDM-PARAMS        TO SYSR-PARAMS                   <A05691>
               MOVE E351               TO SCRN-ERROR-CODE               <A05691>
               MOVE 'Y'                TO WSSP-EDTERROR                 <A05691>
               GO TO 1790-EXIT                                          <A05691>
           END-IF.                                                      <A05691>
                                                                        <A05691>
           MOVE ITDM-GENAREA           TO T5687-T5687-REC.              <A05691>
      *                                                                 <A05691>
      * Check pay method, payment frequency, and risk commencement
      * date.

      **** IF COVTLNB-BILLCHNL         NOT = CHDRLNB-BILLCHNL           <014>
      **** OR COVTLNB-BILLFREQ         NOT = CHDRLNB-BILLFREQ           <014>
           MOVE COVTLNB-PAYRSEQNO    TO WSBB-SUB.                       <014>
           IF COVTLNB-BILLCHNL       NOT = WSAA-BILLCHNL (WSBB-SUB)     <014>
      **** OR COVTLNB-BILLFREQ    NOT = WSAA-BILLFREQ (WSBB-SUB)<A05691><014>
           OR (T5687-SINGLE-PREM-IND   NOT = 'Y'                        <A05691>
           AND COVTLNB-BILLFREQ        NOT = WSAA-BILLFREQ (WSBB-SUB))  <A05691>
           OR COVTLNB-EFFDATE          NOT = CHDRLNB-OCCDATE
           OR COVTLNB-POLINC           NOT = CHDRLNB-POLINC             <020>
           OR COVTLNB-CNTCURR          NOT = CHDRLNB-CNTCURR            <020>
              MOVE SPACES               TO  ERMS-ERRMESG-REC
              MOVE 0                   TO S6378-PAYRSEQNO               <015>
              MOVE G041                TO ERMS-EROR
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES.


      * Check that life (joint life) exists for this component.
           MOVE SPACES                 TO LIFELNB-PARAMS.
           MOVE WSSP-COMPANY           TO LIFELNB-CHDRCOY.
           MOVE CHDRLNB-CHDRNUM        TO LIFELNB-CHDRNUM.
           MOVE COVTLNB-LIFE           TO LIFELNB-LIFE.
           MOVE '00'                   TO LIFELNB-JLIFE.
           MOVE READR                  TO LIFELNB-FUNCTION.
           CALL 'LIFELNBIO'            USING LIFELNB-PARAMS.
           IF  LIFELNB-STATUZ          NOT = O-K
               MOVE LIFELNB-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.

      * Check sex for life.
           IF COVTLNB-SEX01            NOT = LIFELNB-CLTSEX
              MOVE SPACES              TO ERMS-ERRMESG-REC
              MOVE 0                   TO S6378-PAYRSEQNO               <014>
              MOVE H362                TO ERMS-EROR
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES.

      * Check age next birthday.
           IF COVTLNB-ANB-AT-CCD01  NOT = LIFELNB-ANB-AT-CCD
           MOVE SPACES                 TO ERMS-ERRMESG-REC
              MOVE 0                   TO S6378-PAYRSEQNO               <014>
              MOVE H361                TO ERMS-EROR
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES.

      *    Read the Client details for the associated Life.
      *****
           MOVE SPACES                 TO CLTS-PARAMS.
           MOVE 'CN'                   TO CLTS-CLNTPFX.
      *    MOVE WSSP-COMPANY           TO CLTS-CLNTCOY.                 <005>
           MOVE WSSP-FSUCO             TO CLTS-CLNTCOY.                 <005>
           MOVE LIFELNB-LIFCNUM        TO CLTS-CLNTNUM.
           MOVE READR                  TO CLTS-FUNCTION.

           CALL 'CLTSIO'               USING CLTS-PARAMS.

           IF CLTS-STATUZ              NOT = O-K
           AND CLTS-STATUZ             NOT = MRNF
              MOVE CLTS-PARAMS         TO SYSR-PARAMS                   <028>
      ****    MOVE CLTS-PARAMS         TO CLTS-PARAMS                   <028>
              PERFORM 600-FATAL-ERROR.

           IF CLTS-CLTDOB NOT = LIFELNB-CLTDOB
              MOVE SPACES              TO ERMS-ERRMESG-REC
              MOVE H360                TO ERMS-EROR
              MOVE 0                   TO S6378-PAYRSEQNO               <014>
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES.
                                                                        <A05743>
      ****                                                              <A05743>
      ****  Check that the life assured is not dead.                    <A05743>
      ****                                                              <A05743>
                                                                        <A05743>
           IF CLTS-CLTDOD           NOT = VRCM-MAX-DATE                 <A05743>
           AND CHDRLNB-OCCDATE         >  CLTS-CLTDOD                   <A06596>
               MOVE SPACES             TO ERMS-ERRMESG-REC              <A05743>
      ****     MOVE F782               TO ERMS-EROR             <A06596><A05743>
               MOVE W343               TO ERMS-EROR                     <A06596>
               MOVE SPACES             TO S6378-COVERAGE                <A05743>
                                          S6378-RIDER                   <A05743>
               MOVE ZEROES             TO S6378-PAYRSEQNO               <A05743>
               MOVE 'LF'               TO WSAA-EXTRA-MSGPFX             <A06596>
               PERFORM 1800-ERROR-MESSAGES                              <A05743>
           END-IF.                                                      <A05743>

      * CHECK THAT THE DATE OF BIRTH MATCHES UP WITH THE
      * AGE NEXT BIRTHDAY.
      *    MOVE LIFELNB-CLTDOB         TO DTC3-INT-DATE-1.              <029>
      *    MOVE CHDRLNB-OCCDATE        TO DTC3-INT-DATE-2.              <029>
      *    MOVE '01'                   TO DTC3-FREQUENCY.               <029>

      *    CALL 'DATCON3' USING DTC3-DATCON3-REC.                       <029>

      *    IF DTC3-STATUZ              = O-K                            <029>
      * Round up the age.
      *       ADD .99999 DTC3-FREQ-FACTOR GIVING WSAA-ANB.              <029>
      *                                                                 <029>
           PERFORM 1900-CALC-AGE.                                       <029>
      *                                                                 <029>
              IF WSAA-ANB NOT = LIFELNB-ANB-AT-CCD
              MOVE SPACES              TO ERMS-ERRMESG-REC
              MOVE 0                   TO S6378-PAYRSEQNO               <014>
              MOVE H360                TO ERMS-EROR
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES.


      * Read joint life (to see if it is there).
           MOVE '01'                   TO LIFELNB-JLIFE.
           MOVE READR                  TO LIFELNB-FUNCTION.
           CALL 'LIFELNBIO'            USING LIFELNB-PARAMS.
           IF  LIFELNB-STATUZ          NOT = O-K
                                   AND NOT = MRNF
               MOVE LIFELNB-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.

           IF  LIFELNB-STATUZ          = MRNF
               GO TO 1780-READ-NEXT.

           MOVE '01'                   TO S6378-JLIFE.

      * Check sex for life.
           IF COVTLNB-SEX02            NOT = LIFELNB-CLTSEX
           MOVE SPACES                 TO ERMS-ERRMESG-REC
              MOVE 0                   TO S6378-PAYRSEQNO               <014>
              MOVE H362                TO ERMS-EROR
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES.

      * Check age next birthday.
           IF COVTLNB-ANB-AT-CCD02  NOT = LIFELNB-ANB-AT-CCD
           MOVE SPACES                 TO ERMS-ERRMESG-REC
              MOVE 0                   TO S6378-PAYRSEQNO               <014>
              MOVE H361                TO ERMS-EROR
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES.
      *    Read the Client details for the associated Life.
      *****
           MOVE SPACES                 TO CLTS-PARAMS.
           MOVE 'CN'                   TO CLTS-CLNTPFX.
      *    MOVE WSSP-COMPANY           TO CLTS-CLNTCOY.                 <005>
           MOVE WSSP-FSUCO             TO CLTS-CLNTCOY.                 <005>
           MOVE LIFELNB-LIFCNUM        TO CLTS-CLNTNUM.
           MOVE READR                  TO CLTS-FUNCTION.

           CALL 'CLTSIO'               USING CLTS-PARAMS.

           IF CLTS-STATUZ              NOT = O-K
           AND CLTS-STATUZ             NOT = MRNF
              MOVE CLTS-PARAMS         TO SYSR-PARAMS                   <028>
      ****    MOVE CLTS-PARAMS         TO CLTS-PARAMS                   <028>
              PERFORM 600-FATAL-ERROR.

           IF CLTS-CLTDOB NOT = LIFELNB-CLTDOB
              MOVE SPACES              TO ERMS-ERRMESG-REC
              MOVE 0                   TO S6378-PAYRSEQNO               <014>
              MOVE H360                TO ERMS-EROR
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES.

      ****                                                              <A05743>
      ****  Check that the JOINT life assured is not dead.              <A05743>
      ****                                                              <A05743>
                                                                        <A05743>
           IF CLTS-CLTDOD           NOT = VRCM-MAX-DATE                 <A05743>
           AND CHDRLNB-OCCDATE         > CLTS-CLTDOD                    <A06596>
               MOVE SPACES             TO ERMS-ERRMESG-REC              <A05743>
      *****    MOVE F782               TO ERMS-EROR             <A06596><A05743>
               MOVE W343               TO ERMS-EROR                     <A06596>
               MOVE SPACES             TO S6378-COVERAGE                <A05743>
                                          S6378-RIDER                   <A05743>
               MOVE ZEROES             TO S6378-PAYRSEQNO               <A05743>
               MOVE 'JL'               TO WSAA-EXTRA-MSGPFX             <A06596>
               PERFORM 1800-ERROR-MESSAGES                              <A05743>
           END-IF.                                                      <A05743>

      * CHECK THAT THE DATE OF BIRTH MATCHES UP WITH THE
      * AGE NEXT BIRTHDAY.
      *    MOVE LIFELNB-CLTDOB         TO DTC3-INT-DATE-1.              <029>
      *    MOVE CHDRLNB-OCCDATE        TO DTC3-INT-DATE-2.              <029>
      *    MOVE '01'                   TO DTC3-FREQUENCY.               <029>

      *    CALL 'DATCON3' USING DTC3-DATCON3-REC.                       <029>

      *    IF DTC3-STATUZ              = O-K                            <029>
      * Round up the age.
      *       ADD .99999 DTC3-FREQ-FACTOR GIVING WSAA-ANB.              <029>
      *                                                                 <029>
           PERFORM 1900-CALC-AGE.                                       <029>
      *                                                                 <029>
              IF WSAA-ANB NOT = LIFELNB-ANB-AT-CCD
              MOVE SPACES              TO ERMS-ERRMESG-REC
              MOVE 0                   TO S6378-PAYRSEQNO               <014>
              MOVE H360                TO ERMS-EROR
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES.

       1780-READ-NEXT.
           MOVE NEXTR                  TO COVTLNB-FUNCTION.
           CALL 'COVTLNBIO'            USING COVTLNB-PARAMS.
           IF COVTLNB-STATUZ           NOT = ENDP
                                        AND  O-K
              MOVE COVTLNB-PARAMS      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.

           IF  COVTLNB-CHDRCOY         NOT = CHDRLNB-CHDRCOY
           OR  COVTLNB-CHDRNUM         NOT = CHDRLNB-CHDRNUM
               MOVE ENDP               TO COVTLNB-STATUZ.

       1790-EXIT.
            EXIT.
      *
      /
      *****************************************************************
      *     CALL ERROR MESSAGE SUBROUTINE
      *****************************************************************
      *
       1800-ERROR-MESSAGES SECTION.
      *****************************
      *
       1810-ERROR-MESSAGES.

      * Go get the error message.
           MOVE SCRN-LANGUAGE          TO ERMS-LANGUAGE.
           MOVE WSAA-PROG              TO ERMS-EROR-PROG.
           MOVE SCRN-COMPANY           TO ERMS-COMPANY.
           MOVE SPACES                 TO ERMS-FUNCTION.
           CALL 'ERRMESG'              USING ERMS-ERRMESG-REC.

      * Move all details to the screen.
           IF                                                           <A06596>
              WSAA-EXTRA-MSGPFX      NOT = SPACES                       <A06596>
           THEN                                                         <A06596>
              STRING WSAA-EXTRA-MSGPFX ' -  '                           <A06596>
                     ERMS-ERRMESG(01)                                   <A06596>
                     DELIMITED BY '  '   INTO S6378-ERORDSC             <A06596>
              END-STRING                                                <A06596>
           ELSE                                                         <A06596>
           MOVE ERMS-ERRMESG(01)       TO S6378-ERORDSC.
           MOVE ERMS-EROR              TO S6378-ERRCDE.                 <V72L07>
      *
      * Add the record to the subfile.
           MOVE SADD                   TO SCRN-FUNCTION.
           CALL 'S6378IO'              USING SCRN-SCREEN-PARAMS
                                             S6378-DATA-AREA
                                             S6378-SUBFILE-AREA.
           IF SCRN-STATUZ              NOT = O-K
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.
           MOVE 'Y'                    TO WSAA-ERROR-FLAG.

       1890-EXIT.
            EXIT.
      /                                                                 <029>
       1850-AGENT-ERROR-MESSAGE SECTION.                                <A05743>
      **********************************                                <A05743>
      *                                                                 <A05743>
       1860-ERROR.                                                      <A05743>
                                                                        <A05743>
      ****                                                              <A05743>
      **** Call ERRMESG to retrieve the error message for display.      <A05743>
      ****                                                              <A05743>
                                                                        <A05743>
           MOVE SCRN-LANGUAGE          TO ERMS-LANGUAGE.                <A05743>
           MOVE WSAA-PROG              TO ERMS-EROR-PROG.               <A05743>
           MOVE SCRN-COMPANY           TO ERMS-COMPANY.                 <A05743>
           MOVE SPACES                 TO ERMS-FUNCTION.                <A05743>
                                                                        <A05743>
           CALL 'ERRMESG'              USING ERMS-ERRMESG-REC.          <A05743>
                                                                        <A05743>
      ****                                                              <A05743>
      **** Clear the subfile line then string the Agent number plus the <A05743>
      **** error message into the error display.                        <A05743>
      ****                                                              <A05743>
                                                                        <A05743>
           MOVE SPACES                 TO S6378-LIFE                    <A05743>
                                          S6378-JLIFE                   <A05743>
                                          S6378-COVERAGE                <A05743>
                                          S6378-RIDER                   <A05743>
           MOVE ZEROES                 TO S6378-PAYRSEQNO.              <A05743>
                                                                        <A05743>
           MOVE AGNT-AGNTNUM           TO WSAA-AGNTNUM.                 <A05743>
           MOVE ERMS-ERRMESG(01)       TO WSAA-ERRMESG.                 <A05743>
                                                                        <A05743>
           MOVE WSAA-AGENT-ERROR       TO S6378-ERORDSC.                <A05743>
           MOVE ERMS-EROR              TO S6378-ERRCDE.                 <V72L07>
                                                                        <A05743>
      ****                                                              <A05743>
      **** Add the record to the subfile.                               <A05743>
      ****                                                              <A05743>
                                                                        <A05743>
           MOVE SADD                   TO SCRN-FUNCTION.                <A05743>
                                                                        <A05743>
           CALL 'S6378IO'              USING SCRN-SCREEN-PARAMS         <A05743>
                                             S6378-DATA-AREA            <A05743>
                                             S6378-SUBFILE-AREA.        <A05743>
                                                                        <A05743>
           IF SCRN-STATUZ           NOT = O-K                           <A05743>
               MOVE SCRN-STATUZ        TO SYSR-STATUZ                   <A05743>
               PERFORM 600-FATAL-ERROR                                  <A05743>
           END-IF.                                                      <A05743>
                                                                        <A05743>
           MOVE 'Y'                    TO WSAA-ERROR-FLAG.              <A05743>
                                                                        <A05743>
       1899-EXIT.                                                       <A05743>
            EXIT.                                                       <A05743>
      /                                                                 <A05743>
       1900-CALC-AGE         SECTION.                                   <029>
      *******************************                                   <029>
       1910-INIT.                                                       <029>
      *                                                                 <029>
           INITIALIZE                  AGEC-AGECALC-REC.                <029>
           MOVE 'CALCP'                TO AGEC-FUNCTION.                <029>
           MOVE WSSP-LANGUAGE          TO AGEC-LANGUAGE.                <MLS001>
           MOVE CHDRLNB-CNTTYPE        TO AGEC-CNTTYPE.                 <029>
           MOVE LIFELNB-CLTDOB         TO AGEC-INT-DATE-1.              <029>
           MOVE CHDRLNB-OCCDATE        TO AGEC-INT-DATE-2.              <029>
           MOVE WSSP-FSUCO             TO AGEC-COMPANY.                 <029>
      *                                                                 <029>
           CALL 'AGECALC' USING AGEC-AGECALC-REC.                       <029>
      *                                                                 <029>
           IF AGEC-STATUZ          NOT = O-K                            <029>
               GO TO 1990-EXIT.                                         <029>
      *                                                                 <029>
           MOVE AGEC-AGERATING         TO WSAA-ANB.                     <029>
      *                                                                 <029>
       1990-EXIT.                                                       <029>
            EXIT.                                                       <029>
      /                                                                 <029>
  *****                                                            <022><012>
  *****1900-CHECK-FREQ-DATE  SECTION.                              <022><012>
  ***********************************                              <022><012>
  *****                                                            <022><012>
  *****                                                            <022><012>
  *****    we are only interested in monthly or half yearly        <022><012>
  *****    payment frequencies                                     <022><012>
  *****                                                            <022><012>
  *****    IF CHDRLNB-BILLFREQ  = '12'                             <022><012>
  *****        GO TO 1930-CHECK-MONTHLY-DATES.                     <022><012>
  *****                                                            <022><012>
  *****    IF CHDRLNB-BILLFREQ NOT = '02'                          <022><012>
  *****        GO TO 1990-EXIT.                                    <022><012>
  *****                                                            <022><012>
  *****1920-CHECK-HALF-ANNUAL-DATES.                               <022><012>
  *****                                                            <022><012>
  *****    IF ((WSAA-PREM-MTH1    = 02) AND                        <022><012>
  *****        (WSAA-PREM-DAY1    > 27) AND                        <022><012>
  *****        (WSAA-PREM-DAY1    < 30)) AND                       <022><012>
  *****       ((WSAA-PREM-DAY2    = 31) AND                        <022><012>
  *****        (WSAA-PREM-MTH2    = 08))                           <022><012>
  *****        MOVE 1             TO WSAA-FACTOR.                  <022><012>
  *****                                                            <022><012>
  *****    IF ((WSAA-PREM-MTH1    = 03) AND                        <022><012>
  *****        (WSAA-PREM-DAY1    = 30)) AND                       <022><012>
  *****       ((WSAA-PREM-DAY2    = 30) AND                        <022><012>
  *****        (WSAA-PREM-MTH2    = 08))                           <022><012>
  *****        MOVE 1             TO WSAA-FACTOR.                  <022><012>
  *****                                                            <022><012>
  *****    IF ((WSAA-PREM-MTH1    = 04) AND                        <022><012>
  *****        (WSAA-PREM-DAY1    = 30)) AND                       <022><012>
  *****       ((WSAA-PREM-DAY2    = 31) AND                        <022><012>
  *****        (WSAA-PREM-MTH2    = 10))                           <022><012>
  *****        MOVE 1             TO WSAA-FACTOR.                  <022><012>
  *****                                                            <022><012>
  *****    IF ((WSAA-PREM-MTH1    = 05) AND                        <022><012>
  *****        (WSAA-PREM-DAY1    = 30)) AND                       <022><012>
  *****       ((WSAA-PREM-DAY2    = 30) AND                        <022><012>
  *****        (WSAA-PREM-MTH2    = 11))                           <022><012>
  *****        MOVE 1             TO WSAA-FACTOR.                  <022><012>
  *****                                                            <022><012>
  *****    IF ((WSAA-PREM-MTH1    = 06) AND                        <022><012>
  *****        (WSAA-PREM-DAY1    = 30)) AND                       <022><012>
  *****       ((WSAA-PREM-DAY2    = 31) AND                        <022><012>
  *****        (WSAA-PREM-MTH2    = 12))                           <022><012>
  *****        MOVE 1             TO WSAA-FACTOR.                  <022><012>
  *****                                                            <022><012>
  *****    IF ((WSAA-PREM-MTH1    = 08) AND                        <022><012>
  *****        (WSAA-PREM-DAY1    = 30)) AND                       <022><012>
  *****       ((WSAA-PREM-DAY2    = 28) AND                        <022><012>
  *****        (WSAA-PREM-MTH2    = 02))                           <022><012>
  *****        MOVE 1             TO WSAA-FACTOR.                  <022><012>
  *****                                                            <022><012>
  *****    IF ((WSAA-PREM-MTH1    = 09) AND                        <022><012>
  *****        (WSAA-PREM-DAY1    = 30)) AND                       <022><012>
  *****       ((WSAA-PREM-DAY2    = 31) AND                        <022><012>
  *****        (WSAA-PREM-MTH2    = 03))                           <022><012>
  *****        MOVE 1             TO WSAA-FACTOR.                  <022><012>
  *****                                                            <022><012>
  *****    IF ((WSAA-PREM-MTH1    = 10) AND                        <022><012>
  *****        (WSAA-PREM-DAY1    = 30)) AND                       <022><012>
  *****       ((WSAA-PREM-DAY2    = 30) AND                        <022><012>
  *****        (WSAA-PREM-MTH2    = 04))                           <022><012>
  *****        MOVE 1             TO WSAA-FACTOR.                  <022><012>
  *****                                                            <022><012>
  *****    IF ((WSAA-PREM-MTH1    = 11) AND                        <022><012>
  *****        (WSAA-PREM-DAY1    = 30)) AND                       <022><012>
  *****       ((WSAA-PREM-DAY2    = 31) AND                        <022><012>
  *****        (WSAA-PREM-MTH2    = 05))                           <022><012>
  *****        MOVE 1             TO WSAA-FACTOR.                  <022><012>
  *****                                                            <022><012>
  *****    IF ((WSAA-PREM-MTH1    = 12) AND                        <022><012>
  *****        (WSAA-PREM-DAY1    = 30)) AND                       <022><012>
  *****       ((WSAA-PREM-DAY2    = 30) AND                        <022><012>
  *****        (WSAA-PREM-MTH2    = 06))                           <022><012>
  *****        MOVE 1             TO WSAA-FACTOR.                  <022><012>
  *****                                                            <022><012>
  *****    GO TO 1990-EXIT.                                        <022><012>
  *****                                                            <022><012>
  *****                                                            <022><012>
  *****                                                            <022><012>
  *****                                                            <022><012>
  *****1930-CHECK-MONTHLY-DATES.                                   <022><012>
  *****                                                            <022><012>
  *****                                                            <022><012>
  *****    IF ((WSAA-PREM-MTH1    = 01) AND                        <022><012>
  *****        (WSAA-PREM-DAY1    > 27)  AND                       <022><012>
  *****        (WSAA-PREM-DAY1    < 31)) AND                       <022><012>
  *****       ((WSAA-PREM-DAY2    = 28) AND                        <022><012>
  *****        (WSAA-PREM-MTH2    = 02))                           <022><012>
  *****        MOVE 1             TO WSAA-FACTOR.                  <022><012>
  *****                                                            <022><012>
  *****    IF ((WSAA-PREM-MTH1    = 03) AND                        <022><012>
  *****        (WSAA-PREM-DAY1    = 30)) AND                       <022><012>
  *****       ((WSAA-PREM-DAY2    = 30) AND                        <022><012>
  *****        (WSAA-PREM-MTH2    = 04))                           <022><012>
  *****        MOVE 1             TO WSAA-FACTOR.                  <022><012>
  *****                                                            <022><012>
  *****    IF ((WSAA-PREM-MTH1    = 05) AND                        <022><012>
  *****        (WSAA-PREM-DAY1    = 30)) AND                       <022><012>
  *****       ((WSAA-PREM-DAY2    = 30) AND                        <022><012>
  *****        (WSAA-PREM-MTH2    = 06))                           <022><012>
  *****        MOVE 1             TO WSAA-FACTOR.                  <022><012>
  *****                                                            <022><012>
  *****    IF ((WSAA-PREM-MTH1    = 08) AND                        <022><012>
  *****        (WSAA-PREM-DAY1    = 30)) AND                       <022><012>
  *****       ((WSAA-PREM-DAY2    = 30) AND                        <022><012>
  *****        (WSAA-PREM-MTH2    = 09))                           <022><012>
  *****        MOVE 1             TO WSAA-FACTOR.                  <022><012>
  *****                                                            <022><012>
  *****    IF ((WSAA-PREM-MTH1    = 10) AND                        <022><012>
  *****        (WSAA-PREM-DAY1    = 30)) AND                       <022><012>
  *****       ((WSAA-PREM-DAY2    = 30) AND                        <022><012>
  *****        (WSAA-PREM-MTH2    = 11))                           <022><012>
  *****        MOVE 1             TO WSAA-FACTOR.                  <022><012>
  *****                                                            <022><012>
  *****1990-EXIT.                                                  <022><012>
      /
       1A00-SEARCH-FOR-TOLERANCE       SECTION.                         <003>
      ******************************************                        <003>
       1A10-GO.                                                         <003>
      ****  IF CHDRLNB-BILLFREQ        NOT = T5667-FREQ (WSAA-SUB)      <014>
            IF WSAA-BILLFREQ(WSBB-SUB) NOT = T5667-FREQ (WSAA-SUB)      <014>
                 GO TO 1A20-EXIT.                                       <003>
            MOVE T5667-PRMTOL (WSAA-SUB) TO WSAA-TOLERANCE-APP.         <003>
            MOVE T5667-MAX-AMOUNT (WSAA-SUB) TO WSAA-AMOUNT-LIMIT.      <003>
                                                                        <003>
       1A20-EXIT.                                                       <003>
            EXIT.                                                       <003>

       1B00-LOAD-PAYER-DETAILS SECTION.                                 <014>
      *********************************                                 <014>
       1B10-LOAD-PAYER.                                                 <014>
                                                                        <014>
      *                                                                 <014>
      * Read the client role file to get the payer number.              <014>
      *                                                                 <014>
                                                                        <014>
           MOVE CHDRLNB-CHDRPFX    TO CLRF-FOREPFX.                     <014>
           MOVE PAYR-CHDRCOY       TO CLRF-FORECOY.                     <014>
           MOVE PAYR-CHDRNUM       TO WSAA-CHDRNUM.                     <014>
           MOVE PAYR-PAYRSEQNO     TO WSAA-PAYRSEQNO.                   <014>
           MOVE WSAA-PAYRKEY       TO CLRF-FORENUM.                     <014>
           MOVE 'PY'               TO CLRF-CLRRROLE.                    <014>
           MOVE READR              TO CLRF-FUNCTION.                    <014>
                                                                        <014>
           CALL 'CLRFIO' USING CLRF-PARAMS.                             <014>
                                                                        <014>
           IF CLRF-STATUZ NOT = O-K                                     <014>
              MOVE CLRF-STATUZ     TO SYSR-STATUZ                       <014>
              MOVE CLRF-PARAMS     TO SYSR-PARAMS                       <014>
              PERFORM 600-FATAL-ERROR.                                  <014>
                                                                        <014>
      * Load the working storage table using the payer sequence         <014>
      * number as the subscript.                                        <014>
                                                                        <014>
           MOVE PAYR-PAYRSEQNO     TO WSBB-SUB.                         <014>
           MOVE PAYR-INCOME-SEQ-NO TO WSAA-INCOME-SEQ-NO(WSBB-SUB).     <014>
           MOVE PAYR-BILLFREQ      TO WSAA-BILLFREQ(WSBB-SUB).          <014>
           MOVE PAYR-BILLCHNL      TO WSAA-BILLCHNL(WSBB-SUB).          <014>
           MOVE PAYR-BILLCD        TO WSAA-BILLCD(WSBB-SUB).            <014>
           MOVE PAYR-BTDATE        TO WSAA-BTDATE(WSBB-SUB).            <014>
           MOVE PAYR-BILLCURR      TO WSAA-BILLCURR(WSBB-SUB).          <014>
           MOVE PAYR-MANDREF       TO WSAA-MANDREF(WSBB-SUB).           <014>
      **** MOVE PAYR-GRUPKEY       TO WSAA-GRUPKEY(WSBB-SUB).      <014><026>
           MOVE PAYR-GRUPNUM       TO WSAA-GRUPKEY(WSBB-SUB).           <026>
           MOVE CLRF-CLNTNUM       TO WSAA-CLNTNUM(WSBB-SUB).           <014>
           MOVE CLRF-CLNTCOY       TO WSAA-CLNTCOY(WSBB-SUB).           <014>
                                                                        <014>
           MOVE PAYR-PAYRSEQNO     TO WSAA-LAST-PAYER                   <014>
                                                                        <014>
           MOVE NEXTR              TO PAYR-FUNCTION.                    <014>
           CALL 'PAYRIO'           USING PAYR-PARAMS.                   <014>
                                                                        <014>
           IF PAYR-STATUZ NOT = O-K AND                                 <014>
                          NOT = ENDP                                    <014>
              MOVE PAYR-STATUZ     TO SYSR-STATUZ                       <014>
              MOVE PAYR-PARAMS     TO SYSR-PARAMS                       <014>
              PERFORM 600-FATAL-ERROR.                                  <014>
                                                                        <014>
      *                                                                 <014>
       1B90-EXIT.                                                       <014>
            EXIT.                                                       <014>
      /                                                                 <014>
                                                                        <014>
       1C00-ADJUST-PREMIUM SECTION.                                     <014>
      ******************************                                    <014>
       1C10-ADJUST-PREMIUM.                                             <014>
      *    Get the frequency Factor from DATCON3.                       <014>
                                                                        <014>
           IF WSAA-REGPREM(WSBB-SUB)       NOT = ZERO                   <014>
               MOVE CHDRLNB-OCCDATE        TO DTC3-INT-DATE-1           <014>
               MOVE WSAA-BTDATE(WSBB-SUB)  TO DTC3-INT-DATE-2           <014>
               MOVE WSAA-BILLFREQ(WSBB-SUB) TO DTC3-FREQUENCY           <014>
                                                                        <014>
               CALL 'DATCON3' USING DTC3-DATCON3-REC                    <014>
                                                                        <014>
               IF DTC3-STATUZ              NOT = O-K                    <014>
                  MOVE DTC3-STATUZ         TO SYSR-STATUZ               <014>
                  PERFORM 600-FATAL-ERROR                               <014>
               END-IF                                                   <014>
                                                                        <014>
      *    If the risk commencment is 28, 29, 30, of january,           <014>
      *    30 march, 30 may, 30 august or the 30 of october             <014>
      *    the initial instalment required is incorrectly               <014>
      *    calculated as 1 month + 1 day * premium instead of           <014>
      *    1 month(frequency 12)                                        <014>
                                                                        <014>
  *****    MOVE CHDRLNB-OCCDATE            TO WSAA-PREM-DATE1      <022><014>
  *****    MOVE CHDRLNB-BILLCD             TO WSAA-PREM-DATE2      <022><014>
                                                                        <014>
  *****    PERFORM 1900-CHECK-FREQ-DATE                            <022><014>
                                                                        <014>
      * Calculate the instalment premium.                               <014>
                                                                        <014>
               MOVE DTC3-FREQ-FACTOR       TO WSAA-FACTOR               <014>
               MULTIPLY WSAA-REGPREM(WSBB-SUB) BY WSAA-FACTOR           <014>
                                   GIVING WSAA-INSTPRM(WSBB-SUB)        <014>
           END-IF.                                                      <014>
                                                                        <014>
      * Add in the single premium.                                      <014>
                                                                        <014>
           ADD WSAA-SINGP(WSBB-SUB)        TO WSAA-INSTPRM(WSBB-SUB).   <014>
                                                                        <014>
                                                                        <014>
      * If the tax relief method is not spaces calculate the tax        <014>
      * relief amount and deduct it from the premium.                   <014>
                                                                        <014>
           IF T5688-TAXRELMTH NOT = SPACES                              <014>
               MOVE WSAA-CLNTNUM(WSBB-SUB) TO PRAS-CLNTNUM              <014>
               MOVE WSAA-CLNTCOY(WSBB-SUB) TO PRAS-CLNTCOY              <014>
               MOVE WSAA-INCOME-SEQ-NO(WSBB-SUB) TO PRAS-INCOME-SEQ-NO  <014>
               MOVE CHDRLNB-CNTTYPE        TO PRAS-CNTTYPE              <014>
               MOVE T5688-TAXRELMTH        TO PRAS-TAXRELMTH            <014>
                                                                        <014>
      * Use the due date unless a receipt exists with a date later      <014>
      * then the due date.                                              <014>
                                                                        <014>
               IF RTRNSAC-EFFDATE = VRCM-MAX-DATE                       <014>
                   MOVE CHDRLNB-OCCDATE       TO PRAS-EFFDATE           <014>
               ELSE                                                     <014>
                  IF CHDRLNB-OCCDATE       > RTRNSAC-EFFDATE            <014>
                     MOVE CHDRLNB-OCCDATE       TO PRAS-EFFDATE         <014>
                  ELSE                                                  <014>
                     MOVE RTRNSAC-EFFDATE       TO PRAS-EFFDATE         <014>
                  END-IF                                                <014>
               END-IF                                                   <014>
                                                                        <014>
               MOVE CHDRLNB-CHDRCOY        TO PRAS-COMPANY              <014>
               MOVE WSAA-INSTPRM(WSBB-SUB) TO PRAS-GROSSPREM            <014>
               MOVE O-K                    TO PRAS-STATUZ               <014>
                                                                        <014>
               CALL T6687-TAXRELSUB  USING PRAS-PRASCALC-REC            <014>
                                                                        <014>
               IF PRAS-STATUZ  NOT = O-K                                <014>
                    MOVE PRAS-STATUZ       TO SYSR-STATUZ               <014>
                    MOVE T6687-TAXRELSUB   TO SYSR-SUBRNAME             <014>
                    PERFORM 600-FATAL-ERROR                             <014>
               END-IF                                                   <014>
                                                                        <014>
               MOVE PRAS-INREVNUM          TO WSAA-INREVNUM(WSBB-SUB)   <014>
               SUBTRACT PRAS-TAXRELAMT     FROM WSAA-INSTPRM(WSBB-SUB)  <014>
           END-IF.                                                      <014>
                                                                        <014>
           ADD WSAA-INSTPRM(WSBB-SUB)      TO WSAA-TOTAL-PREMIUM.       <014>
                                                                        <014>
      * Add the contract fee to the instalment premium for              <014>
      * payer No. 1.                                                    <014>
                                                                        <019>
      * IF SINGLE PREMIUM IS APPLIED CHECK IF ANY FEE IS NEEDED         <019>
                                                                        <019>
           IF WSAA-SING-PRM-IND = 'Y'                                   <019>
           AND T5688-FEEMETH            NOT = SPACES                    <019>
              PERFORM 1E00-SING-PREM-FEE.                               <019>
                                                                        <019>
           COMPUTE WSAA-PREM-TAX(WSBB-SUB) ROUNDED                      <V74L01>
                                           =  (WSAA-RP-TAX(WSBB-SUB)    <V74L01>
                                           * WSAA-FACTOR)               <V74L01>
                                           + WSAA-SP-TAX(WSBB-SUB).     <V74L01>
                                                                        <V74L01>
           COMPUTE WSAA-TOTAL-TAX ROUNDED  =  WSAA-TOTAL-TAX +          <V74L01>
                                              WSAA-PREM-TAX(WSBB-SUB).  <V74L01>
                                                                        <014>
           IF WSBB-SUB = 1                                              <014>
              IF WSAA-INSTPRM(WSBB-SUB)    = ZERO                       <014>
                 MOVE ZERO                 TO S6378-CNTFEE              <014>
              ELSE                                                      <014>
                 MULTIPLY S6378-CNTFEE     BY WSAA-FACTOR               <014>
                                           GIVING S6378-CNTFEE          <021>
              END-IF                                                    <014>
                                                                        <014>
              ADD WSAA-SINGP-FEE TO S6378-CNTFEE                        <019>
              ADD S6378-CNTFEE             TO WSAA-INSTPRM(WSBB-SUB)    <014>
                                                                        <V74L01>
              IF WSAA-SINGP-FEE            NOT = ZERO                   <V74L01>
                 MOVE WSAA-SINGP-FEE       TO WSAA-CNTFEE               <V74L01>
                 PERFORM 7100-CHECK-CALC-CONT-TAX                       <V74L01>
                 MOVE WSAA-TAX             TO WSAA-SINGPFEE-TAX         <V74L01>
              END-IF                                                    <V74L01>
                                                                        <V74L01>
              COMPUTE WSAA-CNTFEE-TAX ROUNDED                           <V74L01>
                                           = WSAA-CNTFEE-TAX *          <V74L01>
                                             WSAA-FACTOR                <V74L01>
                                                                        <V74L01>
              COMPUTE WSAA-TOTAL-TAX ROUNDED                            <V74L01>
                                           = WSAA-TOTAL-TAX +           <V74L01>
                           WSAA-SINGPFEE-TAX + WSAA-CNTFEE-TAX          <V74L01>
           END-IF.                                                      <014>
                                                                        <V76F06>
           MOVE WSAA-TOTAL-TAX         TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 8000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO WSAA-TOTAL-TAX.               <V76F06>
                                                                        <V74L01>
           MOVE WSAA-TOTAL-TAX             TO S6378-TAXAMT-01.          <V74L01>
                                                                        <014>
       1C50-CHECK-SUSPENSE.                                             <014>
                                                                        <014>
      *****                                                             <014>
      *  Code removed as Contract Suspense now caters for Suspense      <CAS1.0>
      *  Amount in currency other than Contract Currency.               <CAS1.0>
      *  Check if there is enough money in suspense to issue            <014>
      *  this contract.                                                 <014>
      *****                                                             <014>
                                                                        <014>
                                                                        <014>
      *************  Not to be included until pahse B  ************     <014>
                                                                        <014>
      * Get the payer suspense.                                         <014>
                                                                        <014>
      **   MOVE SPACES                 TO SACSLNB-PARAMS.               <014>
      **   MOVE WSSP-COMPANY           TO SACSLNB-CHDRCOY.              <014>
      **   MOVE CHDRLNB-CHDRNUM        TO WSAA-CHDRNUM.                 <014>
      **   MOVE WSBB-SUB               TO WSAA-PAYRSEQNO.               <014>
      **   MOVE WSAA-PAYRKEY           TO SACSLNB-CHDRNUM.              <014>
      **   MOVE CHDRLNB-CNTCURR        TO SACSLNB-CNTCURR.              <014>
      **   MOVE T5645-SACSCODE-01      TO SACSLNB-SACSCODE.             <014>
      **   MOVE T5645-SACSTYPE-01      TO SACSLNB-SACSTYP.              <014>
      **   MOVE READR                  TO SACSLNB-FUNCTION.             <014>
      **                                                                <014>
      **   CALL 'SACSLNBIO'            USING SACSLNB-PARAMS.            <014>
      **                                                                <014>
      **   IF SACSLNB-STATUZ           NOT = O-K                        <014>
      **                           AND NOT = MRNF                       <014>
      **      MOVE SACSLNB-PARAMS      TO SYSR-PARAMS                   <014>
      **      PERFORM 600-FATAL-ERROR.                                  <014>
      **                                                                <014>
      **   IF SACSLNB-STATUZ           = MRNF                           <014>
      **      MOVE ZERO                TO WSAA-PAYR-SUSPENSE            <014>
      **   ELSE                                                         <014>
      **      IF T3695-SIGN            = '-'                            <014>
      **         MULTIPLY SACSLNB-SACSCURBAL BY -1                      <014>
      **                               GIVING WSAA-PAYR-SUSPENSE        <014>
      **      ELSE                                                      <014>
      **         MOVE SACSLNB-SACSCURBAL TO WSAA-PAYR-SUSPENSE.         <014>
      **                                                                <014>
      **      ADD WSAA-PAYR-SUSPENSE   TO WSAA-TOTAL-SUSPENSE.          <014>
              MOVE ZERO                TO WSAA-PAYR-SUSPENSE.           <014>
                                                                        <014>
      *************************************************************     <014>
                                                                        <014>
      * Look up tolerance applicable                                    <014>
                                                                        <014>
      **** MOVE ZERO                TO WSAA-TOLERANCE-APP,         <014><CAS1.0>
      ****                             WSAA-AMOUNT-LIMIT.          <014><CAS1.0>
                                                                        <014>
      **** MOVE 1                      TO WSAA-SUB.                <014><CAS1.0>
      **** PERFORM UNTIL WSAA-SUB > 11                             <014><CAS1.0>
      ****    IF WSAA-BILLFREQ(WSBB-SUB) = T5667-FREQ(WSAA-SUB)    <014><CAS1.0>
      ****       MOVE T5667-PRMTOL(WSAA-SUB) TO WSAA-TOLERANCE-APP <014><CAS1.0>
      ****       MOVE T5667-MAX-AMOUNT(WSAA-SUB) TO WSAA-AMOUNT-LIMIT<014><CAS1.
      ****       MOVE 11                     TO WSAA-SUB           <014><CAS1.0>
      ****    END-IF                                               <014><CAS1.0>
      ****    ADD 1 TO WSAA-SUB                                    <014><CAS1.0>
      **** END-PERFORM.                                            <014><CAS1.0>
                                                                        <014>
                                                                        <014>
      * Calculate the  tolerance applicable.                            <014>
                                                                        <014>
      **** COMPUTE WSAA-CALC-TOLERANCE =                           <014><CAS1.0>
      ****  (WSAA-TOLERANCE-APP * WSAA-INSTPRM(WSBB-SUB) ) / 100.  <014><CAS1.0>
                                                                        <014>
      *    Check % amount is less than Limit on T5667, if so use this   <014>
      *    else use Limit.                                              <014>
                                                                        <014>
      **** IF WSAA-CALC-TOLERANCE < WSAA-AMOUNT-LIMIT              <014><CAS1.0>
      ****    MOVE WSAA-CALC-TOLERANCE TO WSAA-TOLERANCE           <014><CAS1.0>
      **** ELSE                                                    <014><CAS1.0>
      ****    MOVE WSAA-AMOUNT-LIMIT   TO WSAA-TOLERANCE.          <014><CAS1.0>
                                                                        <014>
                                                                        <014>
      * If there is not enough money in the payer suspense account      <014>
      * plus tolerance to cover the premium due check to see if         <014>
      * there is enough money in contract suspense to cover the         <014>
      * remainder (or all if payr suspense was zero) of the             <014>
      * premium due.                                                    <014>
      * If the remainder of the premium amount due is greater then      <014>
      * the contract suspense plus the tolerance display an error       <014>
      * message.                                                        <014>
      * If there is enough money in  contract suspense reduce the       <014>
      * contract suspense amount by the remainder of the premium due.   <014>
      *                                                                 <014>
      *                                                                 <014>
                                                                        <014>
      **** MOVE WSAA-INSTPRM(WSBB-SUB) TO WSAA-AMNT-DUE.           <014><CAS1.0>
      **** IF WSAA-AMNT-DUE > WSAA-PAYR-SUSPENSE + WSAA-TOLERANCE  <014><CAS1.0>
      ****    SUBTRACT WSAA-PAYR-SUSPENSE  FROM WSAA-AMNT-DUE      <014><CAS1.0>
      ****    IF WSAA-AMNT-DUE >  WSAA-CNT-SUSPENSE + WSAA-TOLERANCE<014><CAS1.0
      ****       MOVE 'Y'              TO WSAA-INSUFFICIENT-SUSPENSE<014><CAS1.0
      ****    ELSE                                                 <014><CAS1.0>
      ****       SUBTRACT WSAA-AMNT-DUE FROM WSAA-CNT-SUSPENSE     <014><CAS1.0>
      **** END-IF.                                                 <014><CAS1.0>
                                                                        <014>
           ADD 1 TO WSBB-SUB.                                           <014>
                                                                        <014>
      *                                                                 <014>
       1C90-EXIT.                                                       <014>
            EXIT.                                                       <014>
      /                                                                 <CAS1.0>
      ******************************                                    <CAS1.0>
       1C100-CHECK-SUSPENSE SECTION.                                    <CAS1.0>
      ******************************                                    <CAS1.0>
       1C110-LOCATE-SUSPENSE.                                           <CAS1.0>
      *                                                                 <CAS1.0>
      *    Read the Suspense file to see if any money has been          <CAS1.0>
      *    received for this contract. The search order for Suspense    <CAS1.0>
      *    details is Contract Currency; Billing Currency; Any          <CAS1.0>
      *    Currency. If Suspense is found set appropriate values.       <CAS1.0>
      *    But only if the Suspense amount is not zero.                 <CAS1.0>
      *                                                                 <CAS1.0>
           IF WSAA-SUB                  = 1                             <CAS1.0>
              MOVE CHDRLNB-CNTCURR     TO ACBLENQ-ORIGCURR              <CAS1.0>
              MOVE READR               TO ACBLENQ-FUNCTION              <CAS1.0>
           ELSE                                                         <CAS1.0>
           IF WSAA-SUB                  = 2                             <CAS1.0>
              MOVE CHDRLNB-BILLCURR    TO ACBLENQ-ORIGCURR              <CAS1.0>
              MOVE READR               TO ACBLENQ-FUNCTION              <CAS1.0>
           ELSE                                                         <CAS1.0>
              MOVE SPACE               TO ACBLENQ-ORIGCURR              <CAS1.0>
              MOVE BEGN                TO ACBLENQ-FUNCTION              <CAS1.0>
           END-IF.                                                      <CAS1.0>
                                                                        <CAS1.0>
           CALL 'ACBLENQIO'         USING ACBLENQ-PARAMS.               <CAS1.0>
                                                                        <CAS1.0>
           IF (ACBLENQ-STATUZ       NOT = O-K )   AND                   <CAS1.0>
              (ACBLENQ-STATUZ       NOT = MRNF)   AND                   <CAS1.0>
              (ACBLENQ-STATUZ       NOT = ENDP)                         <CAS1.0>
                 MOVE ACBLENQ-PARAMS   TO SYSR-PARAMS                   <CAS1.0>
                 MOVE ACBLENQ-STATUZ   TO SYSR-STATUZ                   <CAS1.0>
                 PERFORM 600-FATAL-ERROR                                <CAS1.0>
           END-IF.                                                      <CAS1.0>
                                                                        <CAS1.0>
           IF (ACBLENQ-STATUZ           = O-K              )  AND       <CAS1.0>
              (ACBLENQ-RLDGCOY          = WSSP-COMPANY     )  AND       <CAS1.0>
              (ACBLENQ-RLDGACCT         = CHDRLNB-CHDRNUM  )  AND       <CAS1.0>
              (ACBLENQ-SACSCODE         = T5645-SACSCODE-01)  AND       <CAS1.0>
              (ACBLENQ-SACSTYP          = T5645-SACSTYPE-01)  AND       <CAS1.0>
              (ACBLENQ-SACSCURBAL   NOT = ZERO             )            <CAS1.0>
                 MOVE 'Y'              TO WSAA-SUSP-IND                 <CAS1.0>
                                                                        <CAS1.0>
                 IF T3695-SIGN          = '-'                           <CAS1.0>
                    MULTIPLY ACBLENQ-SACSCURBAL BY -1                   <CAS1.0>
                                         GIVING WSAA-TOTAL-SUSPENSE     <CAS1.0>
                 ELSE                                                   <CAS1.0>
                    MOVE ACBLENQ-SACSCURBAL  TO WSAA-TOTAL-SUSPENSE     <CAS1.0>
                 END-IF                                                 <CAS1.0>
                                                                        <CAS1.0>
           END-IF.                                                      <CAS1.0>
                                                                        <CAS1.0>
       1C199-EXIT.                                                      <CAS1.0>
           EXIT.                                                        <CAS1.0>
      /                                                                 <CAS1.0>
      *******************************                                   <CAS1.0>
       1C200-SUSPENSE-AMOUNT SECTION.                                   <CAS1.0>
      *******************************                                   <CAS1.0>
       1C210-VALID-AMOUNT.                                              <CAS1.0>
      *                                                                 <CAS1.0>
      *    If the Suspense amount is in a currency other than the       <CAS1.0>
      *    Contract currency, the Premium & Contract Fee must first     <CAS1.0>
      *    be converted before validating against the Suspense Amount.  <CAS1.0>
      *                                                                 <CAS1.0>
           MOVE ACBLENQ-ORIGCURR       TO S6378-BILLCURR.               <CAS1.0>
           MOVE WSAA-TOTAL-SUSPENSE    TO S6378-PREMSUSP.               <CAS1.0>
                                                                        <CAS1.0>
           IF CHDRLNB-CNTCURR           = ACBLENQ-ORIGCURR              <CAS1.0>
              MOVE 1                   TO S6378-EXRAT                   <CAS1.0>
              MOVE WSAA-TOTAL-PREMIUM  TO S6378-PREM-CURR               <CAS1.0>
              MOVE S6378-CNTFEE        TO S6378-PUFEE                   <CAS1.0>
              ADD WSAA-TOTAL-PREMIUM S6378-CNTFEE                       <CAS1.0>
                  WSAA-TOTAL-TAX                                        <V74L01>
                                   GIVING WSAA-TOTAMNT                  <CAS1.0>
              MOVE WSAA-TOTAL-TAX      TO S6378-TAXAMT-02               <V74L01>
           ELSE                                                         <CAS1.0>
              PERFORM 1C220-CONVERT-SUSPENSE                            <CAS1.0>
           END-IF.                                                      <CAS1.0>
                                                                        <CAS1.0>
           MOVE ZERO                   TO WSAA-TOLERANCE-APP            <CAS1.0>
                                          WSAA-AMOUNT-LIMIT.            <CAS1.0>
                                                                        <CAS1.0>
           PERFORM VARYING WSAA-SUB  FROM 1 BY 1                        <CAS1.0>
                                     UNTIL WSAA-SUB > 11                <CAS1.0>
              IF WSAA-BILLFREQ(1)             = T5667-FREQ(WSAA-SUB)    <CAS1.0>
                 MOVE T5667-PRMTOL(WSAA-SUB) TO WSAA-TOLERANCE-APP      <CAS1.0>
                 MOVE T5667-PRMTOLN(WSAA-SUB) TO WSAA-TOLERANCE-APP2    <V42013>
                 MOVE T5667-MAX-AMOUNT(WSAA-SUB)                        <CAS1.0>
                                             TO WSAA-AMOUNT-LIMIT       <CAS1.0>
                 MOVE T5667-MAXAMT (WSAA-SUB)                           <V42013>
                                             TO WSAA-AMOUNT-LIMIT2      <V42013>
                 MOVE 11                     TO WSAA-SUB                <CAS1.0>
              END-IF                                                    <CAS1.0>
           END-PERFORM.                                                 <CAS1.0>
      *                                                                 <CAS1.0>
      * Calculate the tolerance applicable.                             <CAS1.0>
      *                                                                 <CAS1.0>
           COMPUTE WSAA-CALC-TOLERANCE =                                <CAS1.0>
                  (WSAA-TOLERANCE-APP * WSAA-TOTAMNT) / 100.            <CAS1.0>
      *                                                                 <CAS1.0>
      *    Check % amount is less than Limit on T5667, if so use this   <CAS1.0>
      *    else use Limit.                                              <CAS1.0>
      *                                                                 <CAS1.0>
           IF WSAA-CALC-TOLERANCE < WSAA-AMOUNT-LIMIT                   <CAS1.0>
              MOVE WSAA-CALC-TOLERANCE TO WSAA-TOLERANCE                <CAS1.0>
           ELSE                                                         <CAS1.0>
              MOVE WSAA-AMOUNT-LIMIT   TO WSAA-TOLERANCE                <CAS1.0>
           END-IF.                                                      <CAS1.0>

           MOVE 0        TO WSAA-TOLERANCE2.                            <V42013>
           IF WSAA-AMOUNT-LIMIT2   > 0                                  <V42013>
              IF WSAA-AGT-TERMINATED-FLAG = 'N' OR                      <V42013>
                (WSAA-AGT-TERMINATED-FLAG = 'Y' AND  T5667-SFIND = '2') <V42013>
                 COMPUTE WSAA-CALC-TOLERANCE2 =                         <V42013>
                         (WSAA-TOLERANCE-APP2 * WSAA-TOTAMNT) / 100     <V42013>
                 IF WSAA-CALC-TOLERANCE2  < WSAA-AMOUNT-LIMIT2          <V42013>
                    MOVE WSAA-CALC-TOLERANCE2  TO WSAA-TOLERANCE2       <V42013>
                 ELSE                                                   <V42013>
                    MOVE WSAA-AMOUNT-LIMIT2    TO WSAA-TOLERANCE2       <V42013>
                 END-IF                                                 <V42013>
              END-IF                                                    <V42013>
           END-IF                                                       <V42013>

      *                                                                 <CAS1.0>
      * If the premium amount due is greater than the Contract          <CAS1.0>
      * Suspense plus the tolerance display an error message.           <CAS1.0>
      *                                                                 <CAS1.0>
      *    IF WSAA-TOTAMNT>(WSAA-TOTAL-SUSPENSE+WSAA-TOLERANCE) <V4L001><CAS1.0>
           IF WSAA-TOTAMNT > WSAA-TOTAL-SUSPENSE + WSAA-TOLERANCE       <V4L001>
                             + RLPDLON-PRMDEPST                         <V4L001>
              IF WSAA-TOTAMNT > WSAA-TOTAL-SUSPENSE + WSAA-TOLERANCE2   <V42013>
                                + RLPDLON-PRMDEPST                      <V42013>
                 MOVE 'Y'                 TO WSAA-INSUFFICIENT-SUSPENSE <CAS1.0>
              END-IF                                                    <V42013>
           END-IF.                                                      <CAS1.0>
                                                                        <CAS1.0>
       1C299-EXIT.                                                      <CAS1.0>
           EXIT.                                                        <CAS1.0>
      /                                                                 <CAS1.0>
      ********************************                                  <CAS1.0>
       1C220-CONVERT-SUSPENSE SECTION.                                  <CAS1.0>
      ********************************                                  <CAS1.0>
      *                                                                 <CAS1.0>
      * Convert first the Total Premium & then the Contract fee         <CAS1.0>
      * into the suspense currency.                                     <CAS1.0>
      *                                                                 <CAS1.0>
       1C220-CONVERT.                                                   <CAS1.0>
      *                                                                 <CAS1.0>
           MOVE SPACES                 TO CLNK-CLNK002-REC.             <CAS1.0>
           MOVE ZEROES                 TO CLNK-AMOUNT-OUT               <CAS1.0>
                                          CLNK-RATE-USED.               <CAS1.0>
           MOVE HPAD-HPRRCVDT          TO CLNK-CASHDATE.                <CAS1.0>
           MOVE CHDRLNB-CNTCURR        TO CLNK-CURR-IN.                 <CAS1.0>
           MOVE ACBLENQ-ORIGCURR       TO CLNK-CURR-OUT.                <CAS1.0>
           MOVE WSAA-TOTAL-PREMIUM     TO CLNK-AMOUNT-IN.               <CAS1.0>
           MOVE CHDRLNB-CHDRCOY        TO CLNK-COMPANY.                 <CAS1.0>
           MOVE 'SURR'                 TO CLNK-FUNCTION.                <CAS1.0>
                                                                        <CAS1.0>
           CALL 'XCVRT'             USING CLNK-CLNK002-REC.             <CAS1.0>
                                                                        <CAS1.0>
           IF CLNK-STATUZ           NOT = O-K                           <CAS1.0>
              MOVE CLNK-CLNK002-REC    TO SYSR-PARAMS                   <CAS1.0>
              MOVE CLNK-STATUZ         TO SYSR-STATUZ                   <CAS1.0>
              PERFORM 600-FATAL-ERROR                                   <CAS1.0>
           END-IF.                                                      <CAS1.0>
                                                                        <CAS1.0>
           MOVE CLNK-AMOUNT-OUT        TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE ACBLENQ-ORIGCURR       TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 8000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO CLNK-AMOUNT-OUT.              <V76F06>
                                                                        <V76F06>
           ADD  CLNK-AMOUNT-OUT        TO WSAA-TOTAMNT.                 <CAS1.0>
           MOVE CLNK-AMOUNT-OUT        TO S6378-PREM-CURR.              <CAS1.0>
           MOVE CLNK-RATE-USED         TO S6378-EXRAT.                  <CAS1.0>
      *                                                                 <CAS1.0>
      * Convert the Contract fee.                                       <CAS1.0>
      *                                                                 <CAS1.0>
           IF S6378-CNTFEE          NOT = ZERO                          <CAS1.0>
              MOVE ZEROES              TO CLNK-AMOUNT-OUT               <CAS1.0>
                                          CLNK-RATE-USED                <CAS1.0>
              MOVE S6378-CNTFEE        TO CLNK-AMOUNT-IN                <CAS1.0>
                                                                        <CAS1.0>
              CALL 'XCVRT'          USING CLNK-CLNK002-REC              <CAS1.0>
                                                                        <CAS1.0>
              IF CLNK-STATUZ        NOT = O-K                           <CAS1.0>
                 MOVE CLNK-CLNK002-REC TO SYSR-PARAMS                   <CAS1.0>
                 MOVE CLNK-STATUZ      TO SYSR-STATUZ                   <CAS1.0>
                 PERFORM 600-FATAL-ERROR                                <CAS1.0>
              END-IF                                                    <CAS1.0>
                                                                        <V76F06>
              MOVE CLNK-AMOUNT-OUT        TO ZRDP-AMOUNT-IN             <V76F06>
              MOVE ACBLENQ-ORIGCURR       TO ZRDP-CURRENCY              <V76F06>
              PERFORM 8000-CALL-ROUNDING                                <V76F06>
              MOVE ZRDP-AMOUNT-OUT        TO CLNK-AMOUNT-OUT            <V76F06>
      *                                                                 <CAS1.0>
              ADD  CLNK-AMOUNT-OUT        TO WSAA-TOTAMNT               <CAS1.0>
              MOVE CLNK-AMOUNT-OUT        TO S6378-PUFEE                <CAS1.0>
           END-IF.                                                      <CAS1.0>
      *                                                                 <CAS1.0>
      * Convert the Total tax.                                          <V74L01>
      *                                                                 <V74L01>
           IF WSAA-TOTAL-TAX           > ZERO                           <V74L01>
              MOVE ZEROES              TO CLNK-AMOUNT-OUT               <V74L01>
                                          CLNK-RATE-USED                <V74L01>
              MOVE WSAA-TOTAL-TAX      TO CLNK-AMOUNT-IN                <V74L01>
                                                                        <V74L01>
              CALL 'XCVRT'          USING CLNK-CLNK002-REC              <V74L01>
                                                                        <V74L01>
              IF CLNK-STATUZ        NOT = O-K                           <V74L01>
                 MOVE CLNK-CLNK002-REC TO SYSR-PARAMS                   <V74L01>
                 MOVE CLNK-STATUZ      TO SYSR-STATUZ                   <V74L01>
                 PERFORM 600-FATAL-ERROR                                <V74L01>
              END-IF                                                    <V74L01>
                                                                        <V76F06>
              MOVE CLNK-AMOUNT-OUT        TO ZRDP-AMOUNT-IN             <V76F06>
              MOVE ACBLENQ-ORIGCURR       TO ZRDP-CURRENCY              <V76F06>
              PERFORM 8000-CALL-ROUNDING                                <V76F06>
              MOVE ZRDP-AMOUNT-OUT        TO CLNK-AMOUNT-OUT            <V76F06>
      *                                                                 <V74L01>
              ADD  CLNK-AMOUNT-OUT        TO WSAA-TOTAMNT               <V74L01>
              MOVE CLNK-AMOUNT-OUT        TO S6378-TAXAMT-02            <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
       1C229-EXIT.                                                      <CAS1.0>
           EXIT.                                                        <CAS1.0>
                                                                        <010>
       1C300-CHECK-AGENT-TERMINATE SECTION.                             <V42013>
      *************************************                             <V42013>
       1C310-AGENT-TERMINATE.                                           <V42013>
      *                                                                 <V42013>
      * Retrieve Today's date.                                          <V42013>
      *                                                                 <V42013>
           MOVE TDAY                   TO DTC1-FUNCTION.                <V42013>
           CALL 'DATCON1' USING DTC1-DATCON1-REC.                       <V42013>

           IF DTC1-STATUZ           NOT = O-K                           <V42013>
              MOVE DTC1-DATCON1-REC    TO SYSR-PARAMS                   <V42013>
              PERFORM 600-FATAL-ERROR.                                  <V42013>

           MOVE SPACES                 TO AGLF-DATA-KEY.                <V42013>
           MOVE CHDRLNB-AGNTCOY        TO AGLF-AGNTCOY.                 <V42013>
           MOVE CHDRLNB-AGNTNUM        TO AGLF-AGNTNUM.                 <V42013>
                                                                        <V42013>
           MOVE READR                  TO AGLF-FUNCTION.                <V42013>
           MOVE AGLFREC                TO AGLF-FORMAT.                  <V42013>
                                                                        <V42013>
           CALL 'AGLFIO'               USING AGLF-PARAMS.               <V42013>
                                                                        <V42013>
           IF AGLF-STATUZ           NOT = O-K                           <V42013>
              AND                   NOT = MRNF                          <V42013>
               MOVE AGLF-PARAMS        TO SYSR-PARAMS                   <V42013>
               MOVE AGLF-STATUZ        TO SYSR-STATUZ                   <V42013>
               PERFORM 600-FATAL-ERROR                                  <V42013>
           END-IF.                                                      <V42013>
                                                                        <V42013>
           IF AGLF-STATUZ               = MRNF                          <V42013>
               MOVE SPACES             TO ERMS-ERRMESG-REC              <V42013>
               MOVE G772               TO ERMS-EROR                     <V42013>
               PERFORM 1850-AGENT-ERROR-MESSAGE                         <A05743>
           END-IF.                                                      <V42013>
                                                                        <V42013>
           MOVE 'N'                    TO WSAA-AGT-TERMINATED-FLAG.     <V42013>
                                                                        <V42013>
    ****** IF AGLF-DTETRM           < DTC1-INT-DATE  OR        <LFA1064><V42013>
           IF AGLF-DTETRM          <= DTC1-INT-DATE  OR                 <LFA1064
              AGLF-DTEEXP           < DTC1-INT-DATE  OR                 <V42013>
              AGLF-DTEAPP           > DTC1-INT-DATE                     <V42013>
              MOVE 'Y'                TO WSAA-AGT-TERMINATED-FLAG       <V42013>
           END-IF.                                                      <V42013>
                                                                        <V42013>
       1C390-EXIT.                                                      <V42013>
            EXIT.                                                       <V42013>

      /                                                                 <014>
      ***************************************************************** <014>
      *     PAYER LEVEL VALIDATION                                      <014>
      ***************************************************************** <014>
      *                                                                 <014>
       1D00-PAYER-VALIDATION     SECTION.                               <014>
      ******************************************                        <014>
       1D05-CHECK-DATE-OF-DEATH.                                        <A05743>
                                                                        <A05743>
      ****                                                              <A05743>
      ****  If the payer is different from the owner, ensure that the   <A05743>
      ****  payer is alive by checking for a date of death on the       <A05743>
      ****  client file.                                                <A05743>
      ****                                                              <A05743>
                                                                        <A05743>
           IF CHDRLNB-COWNNUM       NOT = WSAA-CLNTNUM(WSBB-SUB)        <A05743>
               MOVE SPACES             TO CLTS-PARAMS                   <A05743>
               MOVE 'CN'               TO CLTS-CLNTPFX                  <A05743>
               MOVE WSSP-FSUCO         TO CLTS-CLNTCOY                  <A05743>
               MOVE WSAA-CLNTNUM(WSBB-SUB)                              <A05743>
                                       TO CLTS-CLNTNUM                  <A05743>
               MOVE READR              TO CLTS-FUNCTION                 <A05743>
                                                                        <A05743>
               CALL 'CLTSIO'           USING CLTS-PARAMS                <A05743>
                                                                        <A05743>
               IF CLTS-STATUZ       NOT = O-K                           <A05743>
                  AND               NOT = MRNF                          <A05743>
                   MOVE CLTS-PARAMS    TO SYSR-PARAMS                   <A05743>
                   MOVE CLTS-STATUZ    TO SYSR-STATUZ                   <A05743>
                   PERFORM 600-FATAL-ERROR                              <A05743>
               END-IF                                                   <A05743>
                                                                        <A05743>
               IF CLTS-CLTDOD       NOT = VRCM-MAX-DATE                 <A05743>
                  AND CLTS-STATUZ       = O-K                           <A05743>
               AND CHDRLNB-OCCDATE     > CLTS-CLTDOD                    <A06596>
                   MOVE SPACES         TO ERMS-ERRMESG-REC              <A05743>
      *****        MOVE F782           TO ERMS-EROR             <A06596><A05743>
                   MOVE W343           TO ERMS-EROR                     <A06596>
                   MOVE WSBB-SUB       TO S6378-PAYRSEQNO               <A05743>
                   MOVE 'PY'           TO WSAA-EXTRA-MSGPFX             <A06596>
                   PERFORM 1800-ERROR-MESSAGES                          <A05743>
               END-IF                                                   <A05743>
           END-IF.                                                      <A05743>
                                                                        <A05743>
                                                                        <014>
       1D10-READ-T3625.                                                 <014>
      ****                                                         <014><025>
      ****Read T3625 to access the payment plan  details for the co<014><025>
      ****                                                         <014><025>
      **** MOVE SPACES                 TO ITEM-DATA-KEY.           <014><025>
      **** MOVE 'IT'                   TO ITEM-ITEMPFX.            <014><025>
      **** MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.            <014><025>
      **** MOVE T3625                  TO ITEM-ITEMTABL.           <014><025>
      **** MOVE 'LP'                   TO WSAA-T3625-LP.           <014><025>
      **** MOVE WSAA-BILLCHNL(WSBB-SUB)TO WSAA-T3625-BILLCHNL.     <014><025>
      **** MOVE WSAA-BILLFREQ(WSBB-SUB)TO WSAA-T3625-BILLFREQ.     <014><025>
      **** MOVE WSAA-T3625-KEY         TO ITEM-ITEMITEM.           <014><025>
      **** MOVE 'READR'                TO ITEM-FUNCTION.           <014><025>
      ****                                                         <014><025>
      **** CALL 'ITEMIO' USING ITEM-PARAMS.                        <014><025>
      **** IF ITEM-STATUZ              NOT = O-K                   <014><025>
      ****                         AND NOT = MRNF                  <014><025>
      ****     MOVE ITEM-STATUZ        TO SYSR-STATUZ              <014><025>
      ****     PERFORM 600-FATAL-ERROR.                            <014><025>
      ****                                                         <014><025>
      **** IF ITEM-STATUZ              = MRNF                      <014><025>
      ****    MOVE SPACES              TO ERMS-ERRMESG-REC         <014><025>
      ****    MOVE U023                TO ERMS-EROR                <016><025>
      ****    MOVE F941                TO ERMS-EROR                <016><025>
      ****    MOVE WSBB-SUB            TO S6378-PAYRSEQNO          <014><025>
      ****    PERFORM 1800-ERROR-MESSAGES                          <014><025>
      ****    GO TO 1D85-INCREMENT-SUB.                            <014><025>
      ****                                                         <014><025>
      **** MOVE ITEM-GENAREA           TO T3625-T3625-REC.         <014><025>
      *                                                            <014><025>
      *   Read T3620 to access the payment plan  details for the contract025>
      *                                                                 <025>
           MOVE SPACES                 TO ITEM-DATA-KEY.                <025>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <025>
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.                 <025>
           MOVE T3620                  TO ITEM-ITEMTABL.                <025>
           MOVE WSAA-BILLCHNL(WSBB-SUB)TO ITEM-ITEMITEM.                <025>
           MOVE 'READR'                TO ITEM-FUNCTION.                <025>
      *                                                                 <025>
           CALL 'ITEMIO' USING ITEM-PARAMS.                             <025>
           IF ITEM-STATUZ              NOT = O-K                        <025>
                                   AND NOT = MRNF                       <025>
               MOVE ITEM-STATUZ        TO SYSR-STATUZ                   <025>
               PERFORM 600-FATAL-ERROR                                  <025>
           END-IF.                                                      <025>
      *                                                                 <025>
           IF ITEM-STATUZ              = MRNF                           <025>
              MOVE SPACES              TO ERMS-ERRMESG-REC              <025>
              MOVE F921                TO ERMS-EROR                     <025>
              MOVE WSBB-SUB            TO S6378-PAYRSEQNO               <025>
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES                               <025>
              GO TO 1D85-INCREMENT-SUB                                  <025>
           END-IF.                                                      <025>
      *                                                                 <025>
           MOVE ITEM-GENAREA           TO T3620-T3620-REC.              <025>
      *                                                                 <025>
       1D30-VALIDATE-MANDATE.                                           <014>
                                                                        <014>
      * For non Direct Debit, check details don't exist on CHDR         <014>
                                                                        <014>
      **** IF T3625-DDIND          NOT = 'Y'                       <014><025>
           IF T3620-DDIND              = SPACES                         <025>
           AND T3620-CRCIND            = SPACES                         <V76F13>
              IF WSAA-MANDREF(WSBB-SUB) NOT = SPACES                    <014>
                 MOVE SPACES           TO ERMS-ERRMESG-REC              <014>
                 MOVE I005             TO ERMS-EROR                     <014>
                 MOVE WSBB-SUB         TO S6378-PAYRSEQNO               <014>
                 MOVE SPACES           TO WSAA-EXTRA-MSGPFX             <A06596>
                 PERFORM 1800-ERROR-MESSAGES.                           <014>
                                                                        <014>
      * If method of payment is direct debit read the                   <014>
      *  relative mandate record & client bank A/C details              <014>
                                                                        <014>
      ****  IF T3625-DDIND  NOT = 'Y'                              <014><025>
            IF T3620-DDIND      = SPACES                                <025>
            AND T3620-CRCIND    = SPACES                                <V76F13>
                GO TO 1D50-CHECK-GROUP.                                 <014>
                                                                        <014>
            MOVE SPACES                 TO MANDLNB-PARAMS.              <014>
            MOVE WSAA-CLNTCOY(WSBB-SUB) TO MANDLNB-PAYRCOY.             <014>
            MOVE WSAA-CLNTNUM(WSBB-SUB) TO MANDLNB-PAYRNUM.             <014>
            MOVE WSAA-MANDREF(WSBB-SUB) TO MANDLNB-MANDREF.             <014>
            MOVE READR                  TO MANDLNB-FUNCTION.            <014>
            MOVE MANDLNBREC             TO MANDLNB-FORMAT.              <014>
                                                                        <014>
            CALL 'MANDLNBIO' USING MANDLNB-PARAMS.                      <014>
                                                                        <014>
            IF MANDLNB-STATUZ          NOT = O-K                        <014>
               AND MANDLNB-STATUZ      NOT = MRNF                       <014>
               MOVE MANDLNB-PARAMS      TO SYSR-PARAMS                  <014>
               PERFORM 600-FATAL-ERROR.                                 <014>
                                                                        <014>
            IF MANDLNB-STATUZ = MRNF                                    <014>
                MOVE SPACES              TO ERMS-ERRMESG-REC            <014>
                MOVE I007                TO ERMS-EROR                   <014>
                MOVE WSBB-SUB            TO S6378-PAYRSEQNO             <014>
                MOVE SPACES            TO WSAA-EXTRA-MSGPFX             <A06596>
                PERFORM 1800-ERROR-MESSAGES                             <014>
                GO TO 1D50-CHECK-GROUP.                                 <014>
      *                                                                 <V76F13>
      * Check the correct mandate type is present.                      <V76F13>
      *                                                                 <V76F13>
            IF  MANDLNB-CRCIND       NOT = SPACES                       <V76F13>
            AND T3620-DDIND          NOT = SPACES                       <V76F13>
                MOVE I007                TO ERMS-EROR                   <V76F13>
                MOVE WSBB-SUB            TO S6378-PAYRSEQNO             <V76F13>
                PERFORM 1800-ERROR-MESSAGES                             <V76F13>
                GO TO 1D50-CHECK-GROUP                                  <V76F13>
            END-IF.                                                     <V76F13>
                                                                        <V76F13>
            IF  MANDLNB-CRCIND           = SPACES                       <V76F13>
            AND T3620-CRCIND         NOT = SPACES                       <V76F13>
                MOVE I007                TO ERMS-EROR                   <V76F13>
                MOVE WSBB-SUB            TO S6378-PAYRSEQNO             <V76F13>
                PERFORM 1800-ERROR-MESSAGES                             <V76F13>
                GO TO 1D50-CHECK-GROUP                                  <V76F13>
            END-IF.                                                     <V76F13>
                                                                        <V76F13>
                                                                        <014>
      * Read CLBL Bank Details.                                         <014>
                                                                        <014>
            MOVE MANDLNB-BANKKEY        TO CLBL-BANKKEY.                <014>
            MOVE MANDLNB-BANKACCKEY     TO CLBL-BANKACCKEY.             <014>
            MOVE MANDLNB-PAYRCOY        TO CLBL-CLNTCOY.                <V65F14>
            MOVE MANDLNB-PAYRNUM        TO CLBL-CLNTNUM.                <V65F14>
            MOVE READR                  TO CLBL-FUNCTION.               <014>
                                                                        <014>
            CALL 'CLBLIO' USING         CLBL-PARAMS.                    <014>
                                                                        <014>
            IF CLBL-STATUZ              NOT = O-K AND                   <014>
                                        NOT = MRNF                      <014>
               MOVE CLBL-PARAMS         TO SYSR-PARAMS                  <014>
               MOVE CLBL-STATUZ         TO SYSR-STATUZ                  <014>
               PERFORM 600-FATAL-ERROR.                                 <014>
                                                                        <014>
      * If bank A/C not on file display message                         <014>
                                                                        <014>
            IF CLBL-STATUZ              = MRNF                          <014>
               MOVE SPACES              TO ERMS-ERRMESG-REC             <014>
               MOVE F826                TO ERMS-EROR                    <014>
               MOVE WSBB-SUB            TO S6378-PAYRSEQNO              <014>
               MOVE SPACES              TO WSAA-EXTRA-MSGPFX            <A06596>
               PERFORM 1800-ERROR-MESSAGES                              <014>
               GO TO 1D50-CHECK-GROUP.                                  <014>
                                                                        <014>
      * Read table T3678                                                <014>
                                                                        <014>
            MOVE SPACES              TO ITEM-DATA-KEY.                  <014>
            MOVE 'IT'                TO ITEM-ITEMPFX.                   <014>
            MOVE WSSP-FSUCO          TO ITEM-ITEMCOY.                   <014>
            MOVE T3678               TO ITEM-ITEMTABL.                  <014>
            MOVE MANDLNB-MANDSTAT    TO ITEM-ITEMITEM.                  <014>
            MOVE 'READR'             TO ITEM-FUNCTION.                  <014>
                                                                        <014>
            CALL 'ITEMIO' USING ITEM-PARAMS.                            <014>
                                                                        <014>
            IF ITEM-STATUZ              NOT = O-K                       <014>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <014>
               PERFORM 600-FATAL-ERROR.                                 <014>
            MOVE ITEM-GENAREA TO T3678-T3678-REC.                       <014>
                                                                        <014>
      * The mandate and bank A/C details exist so validate the          <014>
      * mandate record against the contract header and client           <014>
      * bank account record.                                            <014>
                                                                        <014>
            IF WSAA-BILLCD(WSBB-SUB) < MANDLNB-EFFDATE                  <014>
               MOVE SPACES             TO ERMS-ERRMESG-REC              <014>
               MOVE I008               TO ERMS-EROR                     <014>
               MOVE WSBB-SUB           TO S6378-PAYRSEQNO               <014>
               MOVE SPACES             TO WSAA-EXTRA-MSGPFX             <A06596>
               PERFORM 1800-ERROR-MESSAGES.                             <014>
      *                                                                 <014>
            IF MANDLNB-MAND-AMT NOT = 0                                 <014>
               IF MANDLNB-MAND-AMT NOT = WSAA-INSTPRM(WSBB-SUB)         <014>
                  MOVE SPACES          TO ERMS-ERRMESG-REC              <014>
                  MOVE I009            TO ERMS-EROR                     <014>
                  MOVE WSBB-SUB        TO S6378-PAYRSEQNO               <014>
                  MOVE SPACES          TO WSAA-EXTRA-MSGPFX             <A06596>
                  PERFORM 1800-ERROR-MESSAGES.                          <014>
      *                                                                 <014>
            IF WSAA-CLNTNUM(WSBB-SUB) NOT = CLBL-CLNTNUM                <014>
               MOVE SPACES             TO ERMS-ERRMESG-REC              <014>
               MOVE I004               TO ERMS-EROR                     <014>
               MOVE WSBB-SUB           TO S6378-PAYRSEQNO               <014>
               MOVE SPACES             TO WSAA-EXTRA-MSGPFX             <A06596>
               PERFORM 1800-ERROR-MESSAGES.                             <014>
      *                                                                 <014>
            IF WSAA-BILLCURR(WSBB-SUB) NOT = CLBL-CURRCODE              <014>
            AND MANDLNB-CRCIND         NOT = 'C'                        <V76F13>
               MOVE SPACES             TO ERMS-ERRMESG-REC              <014>
               MOVE I010               TO ERMS-EROR                     <014>
               MOVE WSBB-SUB           TO S6378-PAYRSEQNO               <014>
               MOVE SPACES             TO WSAA-EXTRA-MSGPFX             <A06596>
               PERFORM 1800-ERROR-MESSAGES.                             <014>
      *                                                                 <014>
            IF T3678-GONOGOFLG = 'N'                                    <014>
               MOVE SPACES             TO ERMS-ERRMESG-REC              <014>
               MOVE I014               TO ERMS-EROR                     <014>
               MOVE WSBB-SUB           TO S6378-PAYRSEQNO               <014>
               MOVE SPACES             TO WSAA-EXTRA-MSGPFX             <A06596>
               PERFORM 1800-ERROR-MESSAGES.                             <014>
                                                                        <014>
      *If the Current From date of the bank account is greater than the <014>
      *effective date of the mandate (i.e. the bank account is not yet  <014>
      *effective) then display an error message.                        <014>
      *                                                                 <014>
                                                                        <014>
            IF CLBL-CURRFROM > MANDLNB-EFFDATE                          <014>
               MOVE SPACES             TO ERMS-ERRMESG-REC              <014>
               MOVE I020               TO ERMS-EROR                     <014>
               MOVE WSBB-SUB           TO S6378-PAYRSEQNO               <014>
               MOVE SPACES             TO WSAA-EXTRA-MSGPFX             <A06596>
               PERFORM 1800-ERROR-MESSAGES.                             <014>
                                                                        <014>
      *If the Current To date of the bank account is less than the      <014>
      *effective date of the mandate (i.e. the bank account is no       <014>
      *longer in force) then display an error message.                  <014>
                                                                        <014>
            IF CLBL-CURRTO          NOT = ZEROES                        <014>
               IF CLBL-CURRTO          < MANDLNB-EFFDATE                <014>
                  MOVE SPACES          TO ERMS-ERRMESG-REC              <014>
                  MOVE I020            TO ERMS-EROR                     <014>
                  MOVE WSBB-SUB        TO S6378-PAYRSEQNO               <014>
                  MOVE SPACES          TO WSAA-EXTRA-MSGPFX             <A06596>
                  PERFORM 1800-ERROR-MESSAGES.                          <014>
                                                                        <014>
                                                                        <014>
        1D50-CHECK-GROUP.                                               <014>
                                                                        <014>
      * For non Group Billing, check details don't exist on CHDR        <014>
                                                                        <014>
      **** IF T3625-GRPIND        NOT  = 'Y' AND                   <014><025>
           IF T3620-GRPIND             = SPACES AND                     <025>
              WSAA-GRUPKEY(WSBB-SUB)   NOT = SPACES                     <014>
              MOVE SPACES              TO ERMS-ERRMESG-REC              <014>
              MOVE E571                TO ERMS-EROR                     <014>
              MOVE WSBB-SUB            TO S6378-PAYRSEQNO               <014>
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES                               <014>
              GO TO 1D85-INCREMENT-SUB.                                 <014>
                                                                        <014>
                                                                        <014>
      **** IF T3625-GRPIND  NOT = 'Y'                              <014><025>
           IF T3620-GRPIND      = SPACES                                <025>
              GO TO 1D85-INCREMENT-SUB.                                 <014>
                                                                        <014>
      * For Group Billing, check details exist on CHDR                  <014>
                                                                        <014>
           IF WSAA-GRUPKEY(WSBB-SUB)   = SPACES                         <014>
              MOVE SPACES              TO ERMS-ERRMESG-REC              <014>
              MOVE E714                TO ERMS-EROR                     <014>
              MOVE WSBB-SUB            TO S6378-PAYRSEQNO               <014>
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES                               <014>
              GO TO 1D85-INCREMENT-SUB.                                 <014>
                                                                        <014>
      *                                                                 <014>
      * Read Group Details.                                             <014>
      *                                                                 <014>
           MOVE CHDRLNB-CHDRCOY        TO GRPS-GRUPCOY.                 <014>
           MOVE WSAA-GRUPKEY(WSBB-SUB) TO GRPS-GRUPNUM.                 <014>
                                                                        <014>
           MOVE READR                  TO GRPS-FUNCTION.                <014>
           CALL 'GRPSIO' USING         GRPS-PARAMS.                     <014>
           IF GRPS-STATUZ              NOT = O-K AND                    <014>
                                       NOT = MRNF                       <014>
               MOVE GRPS-PARAMS        TO SYSR-PARAMS                   <014>
               MOVE GRPS-STATUZ        TO SYSR-STATUZ                   <014>
               PERFORM 600-FATAL-ERROR.                                 <014>
      *                                                                 <014>
      * For Group Billing, check details exist.                         <014>
      *                                                                 <014>
           IF GRPS-STATUZ              = MRNF                           <014>
              MOVE SPACES              TO ERMS-ERRMESG-REC              <014>
              MOVE E714                TO ERMS-EROR                     <014>
              MOVE WSBB-SUB            TO S6378-PAYRSEQNO               <014>
              MOVE SPACES              TO WSAA-EXTRA-MSGPFX             <A06596>
              PERFORM 1800-ERROR-MESSAGES.                              <014>

       1D85-INCREMENT-SUB.                                              <014>
           ADD 1                       TO WSBB-SUB.                     <014>
                                                                        <014>
       1D90-EXIT.                                                       <014>
            EXIT.                                                       <014>
       1E00-SING-PREM-FEE SECTION.                                      <019>
           IF T5674-COMMSUBR           = SPACES                         <019>
              GO TO 1E90-EXIT.                                          <019>
      *                                                                 <019>
           MOVE SPACES                 TO MGFL-MGFEEL-REC.              <019>
           MOVE ZEROES                 TO MGFL-EFFDATE,                 <019>
                                          MGFL-MGFEE.                   <019>
                                                                        <019>
           MOVE CHDRLNB-CNTTYPE        TO MGFL-CNTTYPE.                 <019>
           MOVE '00'                   TO MGFL-BILLFREQ.                <019>
           MOVE CHDRLNB-OCCDATE        TO MGFL-EFFDATE.                 <019>
           MOVE CHDRLNB-CNTCURR        TO MGFL-CNTCURR.                 <019>
           MOVE WSSP-COMPANY           TO MGFL-COMPANY.                 <019>
                                                                        <019>
                                                                        <019>
           CALL T5674-COMMSUBR         USING MGFL-MGFEEL-REC.           <019>
                                                                        <019>
           IF MGFL-STATUZ              NOT = O-K                        <019>
                                   AND NOT = ENDP                       <019>
              MOVE MGFL-MGFEEL-REC     TO SYSR-PARAMS                   <019>
              PERFORM 600-FATAL-ERROR.                                  <019>
                                                                        <019>
           MOVE MGFL-MGFEE             TO WSAA-SINGP-FEE.               <019>
      *
       1E90-EXIT.                                                       <019>
            EXIT.                                                       <019>
      *
      *    Sections performed from the 1000 section above.
      *      (including subfile load section)
      *

       1F00-CHECK-AGENTS SECTION.                                       <A05743>
      ***************************                                       <A05743>
       1F10-CHECK.                                                      <A05743>
                                                                        <A05743>
           MOVE TDAY                   TO DTC1-FUNCTION.                <A002>
           CALL 'DATCON1' USING DTC1-DATCON1-REC.                       <A002>
           MOVE DTC1-INT-DATE          TO WSAA-TODAY.                   <A002>
                                                                        <A002>
      ****                                                              <A05743>
      **** Check that the servicing agent is alive by reading the       <A05743>
      **** client record and checking for a date of death.              <A05743>
      ****                                                              <A05743>
                                                                        <A05743>
           MOVE 'AG'                   TO AGNT-AGNTPFX.                 <A05743>
           MOVE WSSP-COMPANY           TO AGNT-AGNTCOY.                 <A05743>
           MOVE CHDRLNB-AGNTNUM        TO AGNT-AGNTNUM.                 <A05743>
                                                                        <A05743>
           MOVE READR                  TO AGNT-FUNCTION.                <A05743>
           MOVE AGNTREC                TO AGNT-FORMAT.                  <A05743>
                                                                        <A05743>
           CALL 'AGNTIO'               USING AGNT-PARAMS.               <A05743>
                                                                        <A05743>
           IF AGNT-STATUZ           NOT = O-K                           <A05743>
               MOVE AGNT-STATUZ        TO SYSR-STATUZ                   <A05743>
               MOVE AGNT-PARAMS        TO SYSR-PARAMS                   <A05743>
               PERFORM 600-FATAL-ERROR                                  <A05743>
           END-IF.                                                      <A05743>
                                                                        <A05743>
           MOVE AGNT-CLNTPFX           TO CLTS-CLNTPFX.                 <A05743>
           MOVE AGNT-CLNTCOY           TO CLTS-CLNTCOY.                 <A05743>
           MOVE AGNT-CLNTNUM           TO CLTS-CLNTNUM.                 <A05743>
                                                                        <A05743>
           MOVE READR                  TO CLTS-FUNCTION.                <A05743>
           MOVE CLTSREC                TO CLTS-FORMAT.                  <A05743>
                                                                        <A05743>
           CALL 'CLTSIO'               USING CLTS-PARAMS.               <A05743>
                                                                        <A05743>
           IF CLTS-STATUZ           NOT = O-K                           <A05743>
               MOVE CLTS-STATUZ        TO SYSR-STATUZ                   <A05743>
               MOVE CLTS-PARAMS        TO SYSR-PARAMS                   <A05743>
               PERFORM 600-FATAL-ERROR                                  <A05743>
           END-IF.                                                      <A05743>
                                                                        <A05743>
           IF CLTS-CLTDOD           NOT = VRCM-MAX-DATE                 <A05743>
               MOVE SPACES             TO ERMS-ERRMESG-REC              <A05743>
               MOVE E510               TO ERMS-EROR                     <A05743>
               PERFORM 1850-AGENT-ERROR-MESSAGE                         <A05743>
           END-IF.                                                      <A05743>
                                                                        <A05743>
      ****                                                              <A05743>
      **** Read the Agent Life record to obtain the appointed, expired  <A05743>
      **** and terminated dates for the agent.  If no record is found,  <A05743>
      **** display an error.  Check the agent is current by comparing   <A05743>
      **** the above dates against the effective date.                  <A05743>
      ****                                                              <A05743>
      **** Check whether the agent is on black list.                    <S01>
                                                                        <A05743>

           MOVE SPACES                 TO AGLF-DATA-KEY.                <A05743>
           MOVE CHDRLNB-AGNTCOY        TO AGLF-AGNTCOY.                 <A05743>
           MOVE CHDRLNB-AGNTNUM        TO AGLF-AGNTNUM.                 <A05743>
                                                                        <A05743>
           MOVE AGLFREC                TO AGLF-FORMAT.                  <A05743>
           MOVE READR                  TO AGLF-FUNCTION.                <A05743>
                                                                        <A05743>
           CALL 'AGLFIO'               USING AGLF-PARAMS.               <A05743>
                                                                        <A05743>
           IF AGLF-STATUZ           NOT = O-K                           <A05743>
              AND                   NOT = MRNF                          <A05743>
               MOVE AGLF-STATUZ        TO SYSR-STATUZ                   <A05743>
               MOVE AGLF-PARAMS        TO SYSR-PARAMS                   <A05743>
               PERFORM 600-FATAL-ERROR                                  <A05743>
           END-IF.                                                      <A05743>
                                                                        <A05743>
           IF AGLF-STATUZ               = MRNF                          <A05743>
               MOVE SPACES             TO ERMS-ERRMESG-REC              <A05743>
               MOVE E508               TO ERMS-EROR                     <A05743>
               PERFORM 1850-AGENT-ERROR-MESSAGE                         <A05743>
           END-IF.                                                      <A05743>
                                                                        <A05743>
     ***** IF  AGLF-DTETRM             <  CHDRLNB-OCCDATE  OR  <LFA1064><A05743>
           IF  AGLF-DTETRM             <= CHDRLNB-OCCDATE  OR           <LFA1064
               AGLF-DTEEXP             <  CHDRLNB-OCCDATE  OR           <A05743>
               AGLF-DTEAPP             >  CHDRLNB-OCCDATE               <A05743>
               MOVE SPACES             TO ERMS-ERRMESG-REC              <A05743>
               MOVE E507               TO ERMS-EROR                     <A05743>
               PERFORM 1850-AGENT-ERROR-MESSAGE                         <A05743>
           END-IF.                                                      <A05743>
                                                                        <A05743>
           MOVE AGLF-TAGSUSIND         TO WSAA-BLACK-LIST.              <S01>
           IF  NO-ISSUE                                                 <S01>
           AND WSAA-AGENT-SUSPEND      = 'N'                            <S01>
              MOVE 'Y'                 TO WSAA-AGENT-SUSPEND            <S01>
              MOVE SPACES              TO ERMS-ERRMESG-REC              <S01>
              MOVE TL01                TO ERMS-EROR                     <S01>
              PERFORM 1850-AGENT-ERROR-MESSAGE                          <S01>
           END-IF.                                                      <S01>
                                                                        <S01>
           IF  AGLF-TLICEXPDT          NOT = VRCM-MAX-DATE              <A002>
           AND AGLF-TLICEXPDT          < WSAA-TODAY                     <A002>
               MOVE SPACES             TO ERMS-ERRMESG-REC              <A002>
               MOVE TL43               TO ERMS-EROR                     <A002>
               MOVE SPACES             TO WSAA-EXTRA-MSGPFX             <A06596>
               PERFORM 1800-ERROR-MESSAGES                              <A002>
           END-IF.                                                      <A002>
                                                                        <A002>
      ****                                                              <A05743>
      **** Check if there are any other agents receiving a split of     <A05743>
      **** the commission.  For each one found, check that the agent    <A05743>
      **** concerned is both alive and active.                          <A05743>
      ****                                                              <A05743>
                                                                        <A05743>
           MOVE CHDRLNB-CHDRCOY        TO PCDDLNB-CHDRCOY.              <A05743>
           MOVE CHDRLNB-CHDRNUM        TO PCDDLNB-CHDRNUM.              <A05743>
           MOVE SPACES                 TO PCDDLNB-AGNTNUM.              <A05743>
                                                                        <A05743>
           MOVE PCDDLNBREC             TO PCDDLNB-FORMAT.               <A05743>
           MOVE BEGN                   TO PCDDLNB-FUNCTION.             <A05743>
                                                                        <A05743>
           CALL 'PCDDLNBIO'            USING PCDDLNB-PARAMS.            <A05743>
                                                                        <A05743>
           IF PCDDLNB-STATUZ        NOT = O-K                           <A05743>
              AND                   NOT = ENDP                          <A05743>
               MOVE PCDDLNB-STATUZ     TO SYSR-STATUZ                   <A05743>
               MOVE PCDDLNB-PARAMS     TO SYSR-PARAMS                   <A05743>
               PERFORM 600-FATAL-ERROR                                  <A05743>
           END-IF.                                                      <A05743>
                                                                        <A05743>
           IF PCDDLNB-CHDRCOY       NOT = CHDRLNB-CHDRCOY               <A05743>
              OR PCDDLNB-CHDRNUM    NOT = CHDRLNB-CHDRNUM               <A05743>
               MOVE ENDP               TO PCDDLNB-STATUZ                <A05743>
           END-IF.                                                      <A05743>
                                                                        <A05743>
           PERFORM 1F50-CHECK-SPLIT-AGENTS                              <A05743>
              UNTIL PCDDLNB-STATUZ      = ENDP.                         <A05743>
                                                                        <A05743>
                                                                        <A05743>
       1F49-EXIT.                                                       <A05743>
            EXIT.                                                       <A05743>
                                                                        <A05743>
       1F50-CHECK-SPLIT-AGENTS SECTION.                                 <A05743>
      *********************************                                 <A05743>
       1F60-CHECK.                                                      <A05743>
                                                                        <A05743>
      * Check that the split commission agent is alive by reading       <A05743>
      * the client record and checking for a date of death.             <A05743>
                                                                        <A05743>
           IF  PCDDLNB-CHDRCOY         =  WSSP-COMPANY  AND             <A05743>
               PCDDLNB-AGNTNUM         =  CHDRLNB-AGNTNUM               <A05743>
               GO TO 1F70-READ-NEXT                                     <A05743>
           END-IF.                                                      <A05743>
                                                                        <A05743>
           MOVE 'AG'                   TO AGNT-AGNTPFX.                 <A05743>
           MOVE WSSP-COMPANY           TO AGNT-AGNTCOY.                 <A05743>
           MOVE PCDDLNB-AGNTNUM        TO AGNT-AGNTNUM.                 <A05743>
                                                                        <A05743>
           MOVE READR                  TO AGNT-FUNCTION.                <A05743>
           MOVE AGNTREC                TO AGNT-FORMAT.                  <A05743>
                                                                        <A05743>
           CALL 'AGNTIO'               USING AGNT-PARAMS.               <A05743>
                                                                        <A05743>
           IF AGNT-STATUZ           NOT = O-K                           <A05743>
               MOVE AGNT-STATUZ        TO SYSR-STATUZ                   <A05743>
               MOVE AGNT-PARAMS        TO SYSR-PARAMS                   <A05743>
               PERFORM 600-FATAL-ERROR                                  <A05743>
           END-IF.                                                      <A05743>
                                                                        <A05743>
           MOVE AGNT-CLNTPFX           TO CLTS-CLNTPFX.                 <A05743>
           MOVE AGNT-CLNTCOY           TO CLTS-CLNTCOY.                 <A05743>
           MOVE AGNT-CLNTNUM           TO CLTS-CLNTNUM.                 <A05743>
                                                                        <A05743>
           MOVE READR                  TO CLTS-FUNCTION.                <A05743>
           MOVE CLTSREC                TO CLTS-FORMAT.                  <A05743>
                                                                        <A05743>
           CALL 'CLTSIO'               USING CLTS-PARAMS.               <A05743>
                                                                        <A05743>
           IF CLTS-STATUZ           NOT = O-K                           <A05743>
               MOVE CLTS-STATUZ        TO SYSR-STATUZ                   <A05743>
               MOVE CLTS-PARAMS        TO SYSR-PARAMS                   <A05743>
               PERFORM 600-FATAL-ERROR                                  <A05743>
           END-IF.                                                      <A05743>
                                                                        <A05743>
           IF CLTS-CLTDOD           NOT = VRCM-MAX-DATE                 <A05743>
               MOVE SPACES             TO ERMS-ERRMESG-REC              <A05743>
               MOVE E510               TO ERMS-EROR                     <A05743>
               PERFORM 1850-AGENT-ERROR-MESSAGE                         <A05743>
           END-IF.                                                      <A05743>
                                                                        <A05743>
      ****                                                              <A05743>
      **** Read the Agent Life record to obtain the appointed, expired  <A05743>
      **** and terminated dates for the agent.  If no record is found,  <A05743>
      **** display an error.  Check the agent is current by comparing   <A05743>
      **** the above dates against the effective date.                  <A05743>
      ****                                                              <A05743>
                                                                        <A05743>
           MOVE SPACES                 TO AGLF-DATA-KEY.                <A05743>
           MOVE PCDDLNB-CHDRCOY        TO AGLF-AGNTCOY.                 <A05743>
           MOVE PCDDLNB-AGNTNUM        TO AGLF-AGNTNUM.                 <A05743>
                                                                        <A05743>
           MOVE AGLFREC                TO AGLF-FORMAT.                  <A05743>
           MOVE READR                  TO AGLF-FUNCTION.                <A05743>
                                                                        <A05743>
           CALL 'AGLFIO'               USING AGLF-PARAMS.               <A05743>
                                                                        <A05743>
           IF AGLF-STATUZ           NOT = O-K                           <A05743>
              AND                   NOT = MRNF                          <A05743>
               MOVE AGLF-STATUZ        TO SYSR-STATUZ                   <A05743>
               MOVE AGLF-PARAMS        TO SYSR-PARAMS                   <A05743>
               PERFORM 600-FATAL-ERROR                                  <A05743>
           END-IF.                                                      <A05743>
                                                                        <A05743>
           IF AGLF-STATUZ              =  MRNF                          <A05743>
               MOVE SPACES             TO ERMS-ERRMESG-REC              <A05743>
               MOVE E508               TO ERMS-EROR                     <A05743>
               PERFORM 1850-AGENT-ERROR-MESSAGE                         <A05743>
           END-IF.                                                      <A05743>
                                                                        <A05743>
      **** IF  AGLF-DTETRM             <  CHDRLNB-OCCDATE OR   <LFA1064><A05743>
           IF  AGLF-DTETRM             <= CHDRLNB-OCCDATE OR            <LFA1064
               AGLF-DTEEXP             <  CHDRLNB-OCCDATE OR            <A05743>
               AGLF-DTEAPP             >  CHDRLNB-OCCDATE               <A05743>
               MOVE SPACES             TO ERMS-ERRMESG-REC              <A05743>
               MOVE E507               TO ERMS-EROR                     <A05743>
               PERFORM 1850-AGENT-ERROR-MESSAGE                         <A05743>
           END-IF.                                                      <A05743>
                                                                        <A05743>
       1F70-READ-NEXT.                                                  <A05743>
                                                                        <A05743>
      * Look for the next agent receiving a split of the commission.    <A05743>
                                                                        <A05743>
           MOVE NEXTR                  TO PCDDLNB-FUNCTION.             <A05743>
                                                                        <A05743>
           CALL 'PCDDLNBIO'            USING PCDDLNB-PARAMS.            <A05743>
                                                                        <A05743>
           IF PCDDLNB-STATUZ        NOT = O-K                           <A05743>
              AND                   NOT = ENDP                          <A05743>
               MOVE PCDDLNB-STATUZ     TO SYSR-STATUZ                   <A05743>
               MOVE PCDDLNB-PARAMS     TO SYSR-PARAMS                   <A05743>
               PERFORM 600-FATAL-ERROR                                  <A05743>
           END-IF.                                                      <A05743>
                                                                        <A05743>
           IF PCDDLNB-CHDRCOY       NOT = CHDRLNB-CHDRCOY               <A05743>
              OR PCDDLNB-CHDRNUM    NOT = CHDRLNB-CHDRNUM               <A05743>
               MOVE ENDP               TO PCDDLNB-STATUZ                <A05743>
           END-IF.                                                      <A05743>
                                                                        <A05743>
       1F90-EXIT.                                                       <A05743>
            EXIT.                                                       <A05743>
                                                                        <A05743>
       1G00-CALL-HCRTFUP SECTION.                                       <LA2103>
      ***************************                                       <FUPLET>
       1G10-FOLLOW-UPS.                                                 <FUPLET>

           IF CHKRL-LRKCLS(WSAA-SUB)   = SPACE                          <V42006>
              GO TO 1G90-EXIT.                                          <V42006>
      *                                                                 <FUPLET>
      * Check for automatic follow-ups by calling HCRTFUP               <LA2103>
      *                                                                 <FUPLET>
           MOVE O-K                    TO CFUP-STATUZ.                  <FUPLET>
           MOVE LIFELNB-CHDRCOY        TO CFUP-CHDRCOY.                 <FUPLET>
           MOVE LIFELNB-CHDRNUM        TO CFUP-CHDRNUM.                 <FUPLET>
           MOVE CHDRLNB-CNTTYPE        TO CFUP-CNTTYPE.                 <FUPLET>
           MOVE CHKRL-LRKCLS(WSAA-SUB) TO CFUP-LRKCLS.                  <V42006>
           MOVE LIFELNB-LIFE           TO CFUP-LIFE.                    <FUPLET>
           MOVE LIFELNB-JLIFE          TO CFUP-JLIFE.                   <FUPLET>
           MOVE LIFELNB-LIFCNUM        TO CFUP-LIFCNUM.                 <FUPLET>
           MOVE LIFELNB-ANB-AT-CCD     TO CFUP-ANBCCD.                  <FUPLET>
           MOVE CHDRLNB-TRANNO         TO CFUP-TRANNO.                  <FUPLET>
           MOVE CHDRLNB-AGNTNUM        TO CFUP-AGNTNUM.                 <NB031>
           MOVE WSKY-BATC-BATCTRCDE    TO CFUP-BATCTRCDE.               <FUPLET>
           MOVE WSSP-LANGUAGE          TO CFUP-LANGUAGE.                <FUPLET>
           MOVE WSSP-FSUCO             TO CFUP-FSUCO.                   <FUPLET>
           MOVE VRCM-USER              TO CFUP-USER.                    <FUPLET>
           MOVE VRCM-TIME              TO CFUP-TRANSACTION-TIME.        <FUPLET>
           MOVE VRCM-DATE              TO CFUP-TRANSACTION-DATE.        <FUPLET>
      *                                                                 <FUPLET>
           CALL 'HCRTFUP'              USING CFUP-CRTFUP-REC.           <LA2103>
      *                                                                 <FUPLET>
           IF  CFUP-STATUZ             NOT = O-K                        <FUPLET>
               MOVE CFUP-CRTFUP-REC    TO SYSR-PARAMS                   <FUPLET>
               MOVE CFUP-STATUZ        TO SYSR-STATUZ                   <FUPLET>
               PERFORM 600-FATAL-ERROR                                  <FUPLET>
           END-IF.                                                      <FUPLET>
                                                                        <FUPLET>
       1G90-EXIT.                                                       <FUPLET>
           EXIT.                                                        <FUPLET>
      /
       1H00-MANDATORY-BNFY SECTION.                                     <V76L01>
      *****************************                                     <V76L01>
       1H10-MANDA.                                                      <V76L01>
                                                                        <V76L01>
      *                                                                 <V76L01>
      *  Check for Mandatory Beneficiary's relation using TR52Z.        <V76L01>
      *                                                                 <V76L01>
           MOVE ZEROS                  TO WSAA-IZC.                     <V76L01>
           MOVE SPACES                 TO WSAA-FOUND.                   <V76L01>
                                                                        <V76L01>
           INITIALIZE                     ITEM-PARAMS.                  <V76L01>
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.                 <V76L01>
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.                 <V76L01>
           MOVE TR52Z                  TO ITEM-ITEMTABL.                <V76L01>
           MOVE CHDRLNB-CNTTYPE        TO ITEM-ITEMITEM.                <V76L01>
           MOVE READR                  TO ITEM-FUNCTION.                <V76L01>
      *                                                                 <V76L01>
           CALL 'ITEMIO'            USING ITEM-PARAMS.                  <V76L01>
      *                                                                 <V76L01>
           IF ITEM-STATUZ           NOT = O-K                           <V76L01>
           AND ITEM-STATUZ          NOT = MRNF                          <V76L01>
              MOVE ITEM-PARAMS         TO SYSR-PARAMS                   <V76L01>
              MOVE ITEM-STATUZ         TO SYSR-STATUZ                   <V76L01>
              PERFORM 600-FATAL-ERROR                                   <V76L01>
           END-IF.                                                      <V76L01>
      *                                                                 <V76L01>
           IF ITEM-STATUZ               = MRNF                          <V76L01>
              INITIALIZE                  ITEM-PARAMS                   <V76L01>
              MOVE SMTP-ITEM           TO ITEM-ITEMPFX                  <V76L01>
              MOVE WSSP-COMPANY        TO ITEM-ITEMCOY                  <V76L01>
              MOVE TR52Z               TO ITEM-ITEMTABL                 <V76L01>
              MOVE '***'               TO ITEM-ITEMITEM                 <V76L01>
              MOVE READR               TO ITEM-FUNCTION                 <V76L01>
      *                                                                 <V76L01>
              CALL 'ITEMIO'         USING ITEM-PARAMS                   <V76L01>
      *                                                                 <V76L01>
              IF ITEM-STATUZ        NOT = O-K                           <V76L01>
              AND ITEM-STATUZ       NOT = MRNF                          <V76L01>
                 MOVE ITEM-PARAMS      TO SYSR-PARAMS                   <V76L01>
                 MOVE ITEM-STATUZ      TO SYSR-STATUZ                   <V76L01>
                 PERFORM 600-FATAL-ERROR                                <V76L01>
              END-IF                                                    <V76L01>
      *                                                                 <V76L01>
           END-IF.                                                      <V76L01>
      *                                                                 <V76L01>
           MOVE ITEM-GENAREA           TO TR52Z-TR52Z-REC.              <V76L01>
                                                                        <V76L01>
           PERFORM VARYING WSAA-IUC FROM 1 BY 1 UNTIL WSAA-IUC > 10     <V76L01>
               IF  TR52Z-BNYTYPE(WSAA-IUC) NOT = SPACES                 <V76L01>
               AND TR52Z-MANOPT(WSAA-IUC)     = 'Y'                     <V76L01>
                   ADD 1               TO WSAA-IZC                      <V76L01>
                   PERFORM VARYING WSAA-IVC FROM 1 BY 1                 <V76L01>
                     UNTIL WSAA-IVC  > WSAA-IWC  OR                     <V76L01>
                           WSAA-MANDATORY(WSAA-IZC) = 'Y'               <V76L01>
                       MOVE 'N'        TO WSAA-MANDATORY(WSAA-IZC)      <V76L01>
                       IF TR52Z-BNYTYPE(WSAA-IUC)                       <V76L01>
                                         = WSAA-BNYTYPE(WSAA-IVC)       <V76L01>
                          MOVE 'Y'     TO WSAA-MANDATORY(WSAA-IZC)      <V76L01>
                       END-IF                                           <V76L01>
                   END-PERFORM                                          <V76L01>
             END-IF                                                     <V76L01>
           END-PERFORM.                                                 <V76L01>
                                                                        <V76L01>
           IF WSAA-IUC                  > 10                            <V76L01>
              MOVE 1                   TO WSAA-IUC                      <V76L01>
              PERFORM 1I00-CONTINUE-CHECK UNTIL NOT-FOUND               <V76L01>
           END-IF.                                                      <V76L01>
      *                                                                 <V76L01>
       1H90-EXIT.                                                       <V76L01>
           EXIT.                                                        <V76L01>
      /                                                                 <V76L01>
       1I00-CONTINUE-CHECK SECTION.                                     <V76L01>
      *****************************                                     <V76L01>
       1I10-CONT.                                                       <V76L01>
                                                                        <V76L01>
           IF  TR52Z-GITEM              = SPACES                        <V76L01>
               MOVE 'N'                TO WSAA-FOUND                    <V76L01>
               GO TO 1I90-EXIT                                          <V76L01>
           END-IF.                                                      <V76L01>
                                                                        <V76L01>
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.                 <V76L01>
           MOVE TR52Z                  TO ITEM-ITEMTABL.                <V76L01>
           MOVE TR52Z-GITEM            TO ITEM-ITEMITEM                 <V76L01>
           MOVE READR                  TO ITEM-FUNCTION.                <V76L01>
           CALL 'ITEMIO'            USING ITEM-PARAMS.                  <V76L01>
           IF ITEM-STATUZ           NOT = O-K AND MRNF                  <V76L01>
              MOVE ITEM-PARAMS         TO SYSR-PARAMS                   <V76L01>
              MOVE ITEM-STATUZ         TO SYSR-STATUZ                   <V76L01>
              PERFORM 600-FATAL-ERROR                                   <V76L01>
           END-IF.                                                      <V76L01>
                                                                        <V76L01>
           MOVE ITEM-GENAREA           TO TR52Z-TR52Z-REC.              <V76L01>
                                                                        <V76L01>
           PERFORM VARYING WSAA-IUC FROM 1 BY 1 UNTIL WSAA-IUC > 10     <V76L01>
               IF  TR52Z-BNYTYPE(WSAA-IUC) NOT = SPACES                 <V76L01>
               AND TR52Z-MANOPT(WSAA-IUC)     = 'Y'                     <V76L01>
                   ADD 1               TO WSAA-IZC                      <V76L01>
                   PERFORM VARYING WSAA-IVC FROM 1 BY 1                 <V76L01>
                     UNTIL WSAA-IVC  > WSAA-IWC  OR                     <V76L01>
                           WSAA-MANDATORY(WSAA-IZC) = 'Y'               <V76L01>
                       MOVE 'N'        TO WSAA-MANDATORY(WSAA-IZC)      <V76L01>
                       IF TR52Z-BNYTYPE(WSAA-IUC)                       <V76L01>
                                         = WSAA-BNYTYPE(WSAA-IVC)       <V76L01>
                          MOVE 'Y'     TO WSAA-MANDATORY(WSAA-IZC)      <V76L01>
                       END-IF                                           <V76L01>
                   END-PERFORM                                          <V76L01>
             END-IF                                                     <V76L01>
           END-PERFORM.                                                 <V76L01>
                                                                        <V76L01>
                                                                        <V76L01>
       1I90-EXIT.                                                       <V76L01>
           EXIT.                                                        <V76L01>
      /                                                                 <V76L01>
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
           IF S6378-TAXAMT-01          >  ZERO                          <V74L01>
           OR S6378-TAXAMT-02          >  ZERO                          <V74L01>
              MOVE 'N'                 TO S6378-TAXAMT01-OUT (ND)       <V74L01>
                                          S6378-TAXAMT01-OUT (PR)       <V74L01>
                                          S6378-TAXAMT02-OUT (ND)       <V74L01>
                                          S6378-TAXAMT02-OUT (PR)       <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <S9503>
           MOVE PROT                   TO SCRN-FUNCTION.                <A07109>
                                                                        <A07109>
           MOVE 1                      TO SCRN-SUBFILE-RRN.             <S9503>
           GO TO PRE-EXIT.                                              <S9503>
      *                                                                 <S9503>
       PRE-EXIT.                                                        <S9503>
           EXIT.                                                        <S9503>
      /                                                                 <S9503>
       2000-SCREEN-EDIT SECTION.
      **************************
      *
       2010-SCREEN-IO.
      *    CALL 'S6378IO' USING SCRN-SCREEN-PARAMS                      <S9503>
      *                         S6378-DATA-AREA                         <S9503>
      *                         S6378-SUBFILE-AREA.                     <S9503>
      * Screen errors are now handled in the calling program.           <S9503>
      *    PERFORM 200-SCREEN-ERRORS.                                   <S9503>
           MOVE O-K                    TO WSSP-EDTERROR.

       2010-VALIDATE-SCREEN.

           IF WSAA-WARNING = 'Y'                                        <NB010>
              PERFORM A5000-CHECK-WARNING                               <NB010>
           END-IF.                                                      <NB010>



      *
      *    Validate fields
      *

       2050-CHECK-FOR-ERRORS.
           IF S6378-ERROR-INDICATORS NOT = SPACES
              MOVE 'Y'                 TO WSSP-EDTERROR.

       2060-VALIDATE-SUBFILE.
           MOVE SRNCH                  TO SCRN-FUNCTION.
           CALL 'S6378IO' USING SCRN-SCREEN-PARAMS
                                S6378-DATA-AREA
                                S6378-SUBFILE-AREA.
           IF SCRN-STATUZ NOT = O-K
                            AND ENDP
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.

           PERFORM 2600-VALIDATE-SUBFILE
                                       UNTIL SCRN-STATUZ = ENDP.

       2090-EXIT.
            EXIT.
      /
      *****************************************************************

      *
      *    Sections performed from the 2000 section above.
      *          (Screen validation)
      *

      /
       2600-VALIDATE-SUBFILE SECTION.
      *******************************
      *
       2610-VALIDATION.




      *
      *    Validate subfile fields
      *




       2670-UPDATE-ERROR-INDICATORS.
           MOVE SUPD                   TO SCRN-FUNCTION.
           CALL 'S6378IO' USING SCRN-SCREEN-PARAMS
                                S6378-DATA-AREA
                                S6378-SUBFILE-AREA.
           IF SCRN-STATUZ NOT = O-K
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.
      *
           IF S6378-ERROR-SUBFILE NOT = SPACES
              MOVE 'Y'                 TO WSSP-EDTERROR.
      *
       2680-READ-NEXT-MODIFIED-RECORD.
           MOVE SRNCH                  TO SCRN-FUNCTION.
           CALL 'S6378IO' USING SCRN-SCREEN-PARAMS
                                S6378-DATA-AREA
                                S6378-SUBFILE-AREA.
           IF SCRN-STATUZ NOT = O-K
                            AND ENDP
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.

       2690-EXIT.
            EXIT.
      /
      *****************************************************************

      *
      *    Sections performed from the 2600 section above.
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
      *  Update database files as required
      *

      * If there are no validation errors in the screen then the
      * available issue flag is set to 'y'.
      * If validation errors exist, then the available for issue        <CAS1.0>
      * flag is set to 'N'.                                             <CAS1.0>
           IF WSAA-ERROR-FLAG          = 'Y'
              MOVE 'N'                 TO CHDRLNB-AVLISU                <CAS1.0>
           ELSE                                                         <CAS1.0>
              MOVE 'Y'                 TO CHDRLNB-AVLISU                <CAS1.0>
           END-IF.                                                      <CAS1.0>
      ****    GO TO 3090-EXIT.                                          <CAS1.0>

      **** MOVE 'Y'                    TO CHDRLNB-AVLISU.               <CAS1.0>
           MOVE KEEPS                  TO CHDRLNB-FUNCTION.
           CALL 'CHDRLNBIO'            USING CHDRLNB-PARAMS.
           IF CHDRLNB-STATUZ           NOT = O-K
              MOVE CHDRLNB-PARAMS      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.

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
      **                                                                <028>
      * When the Fast track selection is being used, and an error has   <028>
      * been found by Pre-issue validation the system must return to    <028>
      * the previous program.                                           <028>
      **                                                                <028>
           IF ERMS-EROR       NOT =  SPACES                             <028>
                SUBTRACT 1    FROM   WSSP-PROGRAM-PTR                   <028>
                MOVE   '*'     TO    WSSP-SEC-ACTN(WSSP-PROGRAM-PTR)    <028>
                MOVE   ' '     TO    ERMS-EROR                          <028>
           ELSE                                                         <028>
                ADD      1     TO    WSSP-PROGRAM-PTR                   <028>
           END-IF.                                                      <028>
      ****                                                              <028>
      **** ADD 1                       TO WSSP-PROGRAM-PTR.             <028>
      *
       4090-EXIT.
            EXIT.
      *
       5000-READ-TR517 SECTION.                                         <CAS1.0>
      *************************                                         <CAS1.0>
   ****5100-PARA.                                                       <CAS1.0>
       5010-PARA.                                                       <CAS1.0>
                                                                        <CAS1.0>
           MOVE SPACES                 TO ITDM-PARAMS.                  <CAS1.0>
           MOVE WSSP-COMPANY           TO ITDM-ITEMCOY.                 <CAS1.0>
           MOVE TR517                  TO ITDM-ITEMTABL.                <CAS1.0>
           MOVE COVTTRM-CRTABLE        TO ITDM-ITEMITEM.                <CAS1.0>
           MOVE CHDRLNB-OCCDATE        TO ITDM-ITMFRM.                  <CAS1.0>
           MOVE BEGN                   TO ITDM-FUNCTION.                <CAS1.0>
                                                                        <CAS1.0>
           CALL 'ITDMIO'               USING ITDM-PARAMS.               <CAS1.0>
                                                                        <CAS1.0>
           IF ITDM-STATUZ              NOT = O-K  AND                   <CAS1.0>
                                       NOT = ENDP                       <CAS1.0>
               MOVE ITDM-PARAMS        TO SYSR-PARAMS                   <CAS1.0>
               PERFORM 600-FATAL-ERROR                                  <CAS1.0>
           END-IF.                                                      <CAS1.0>
                                                                        <CAS1.0>
           IF ITDM-ITEMCOY             NOT = WSSP-COMPANY    OR         <CAS1.0>
              ITDM-ITEMTABL            NOT = TR517           OR         <CAS1.0>
              ITDM-ITEMITEM            NOT = COVTTRM-CRTABLE OR         <CAS1.0>
              ITDM-STATUZ              = ENDP                           <CAS1.0>
               MOVE ENDP               TO ITDM-STATUZ                   <CAS1.0>
   ****        GO TO 5190-EXIT                                          <CAS1.0>
               GO TO 5090-EXIT                                          <CAS1.0>
           END-IF.                                                      <CAS1.0>
                                                                        <CAS1.0>
           MOVE ITDM-GENAREA           TO TR517-TR517-REC.              <CAS1.0>
      *                                                                 <CAS1.0>
   ****5190-EXIT.                                                       <CAS1.0>
       5090-EXIT.                                                       <CAS1.0>
           EXIT.                                                        <CAS1.0>
      /
       5100-READ-T6640 SECTION.                                         <CAS1.0>
      *************************                                         <CAS1.0>
       5110-START.

           MOVE SPACES                 TO ITDM-PARAMS.                  <CAS1.0>
           MOVE 'IT'                   TO ITDM-ITEMPFX.
           MOVE WSSP-COMPANY           TO ITDM-ITEMCOY.
           MOVE T6640                  TO ITDM-ITEMTABL.
           MOVE COVTTRM-CRTABLE        TO ITDM-ITEMITEM.
           MOVE CHDRLNB-OCCDATE        TO ITDM-ITMFRM.                  <CAS1.0>
           MOVE BEGN                   TO ITDM-FUNCTION.

           CALL 'ITDMIO'               USING ITDM-PARAMS.
           IF ITDM-STATUZ              NOT = O-K AND ENDP
               MOVE ITDM-PARAMS        TO SYSR-PARAMS                   <CAS1.0>
               PERFORM 600-FATAL-ERROR.                                 <CAS1.0>

           IF ITDM-ITEMCOY             NOT = WSSP-COMPANY
           OR ITDM-ITEMTABL            NOT = T6640
           OR ITDM-ITEMITEM            NOT = COVTTRM-CRTABLE
           OR ITDM-STATUZ                  = ENDP
              MOVE SPACE               TO T6640-ZRMANDIND
           ELSE
              MOVE ITDM-GENAREA        TO T6640-T6640-REC.

       5190-EXIT.
           EXIT.
      /                                                                 <CAS1.0>
       5200-CALC-WAIVE-SUMINS SECTION.                                  <CAS1.0>
      ********************************                                  <CAS1.0>
       5210-PARA.                                                       <CAS1.0>
                                                                        <CAS1.0>
           MOVE TR517-TR517-REC        TO WSAA-TR517-REC.               <TN01>
           MOVE ZEROES                 TO WSAA-WAIVE-SUMINS.            <CAS1.0>
      **** MOVE SPACES                 TO COVTLNB-PARAMS.       <CAS1.0>V42L014
           INITIALIZE COVTLNBREC-KEY-DATA.                              V42L014
           MOVE CHDRLNB-CHDRCOY        TO COVTLNB-CHDRCOY.              <CAS1.0>
           MOVE CHDRLNB-CHDRNUM        TO COVTLNB-CHDRNUM.              <CAS1.0>
           MOVE COVTTRM-LIFE           TO COVTLNB-LIFE                  V42L014
           MOVE COVTTRM-COVERAGE       TO COVTLNB-COVERAGE              V42L014
      **** MOVE SPACES                 TO COVTLNB-LIFE          <CAS1.0>V42L014
      ****                                COVTLNB-COVERAGE      <CAS1.0>V42L014
      ****                                COVTLNB-RIDER.        <CAS1.0>V42L014
      **** MOVE ZEROES                 TO COVTLNB-SEQNBR.       <CAS1.0>V42L014
           IF   TR517-ZRWVFLG-04   NOT  = 'Y'                           <LFA1058
                MOVE SPACES            TO COVTLNB-LIFE                  <LFA1058
                                          COVTLNB-COVERAGE              <LFA1058
                                          COVTLNB-RIDER                 <LFA1058
                MOVE ZEROES            TO COVTLNB-SEQNBR                <LFA1058
           END-IF                                                       <LFA1058
           MOVE BEGN                   TO COVTLNB-FUNCTION.             <CAS1.0>
      *                                                                 <CAS1.0>
       5210-CALL-COVTLNBIO.                                             <CAS1.0>
           CALL 'COVTLNBIO'            USING COVTLNB-PARAMS.            <CAS1.0>
                                                                        <CAS1.0>
           MOVE WSAA-TR517-REC         TO TR517-TR517-REC.              <TN01>
           IF COVTLNB-STATUZ           NOT = O-K  AND                   <CAS1.0>
                                       NOT = ENDP                       <CAS1.0>
               MOVE COVTLNB-PARAMS     TO SYSR-PARAMS                   <CAS1.0>
               PERFORM 600-FATAL-ERROR                                  <CAS1.0>
           END-IF.                                                      <CAS1.0>
           IF COVTLNB-CHDRCOY          NOT = CHDRLNB-CHDRCOY OR         <CAS1.0>
              COVTLNB-CHDRNUM          NOT = CHDRLNB-CHDRNUM OR         <CAS1.0>
              COVTLNB-STATUZ           = ENDP                           <CAS1.0>
               GO TO 5290-EXIT                                          <CAS1.0>
           END-IF.                                                      <CAS1.0>
           IF COVTLNB-LIFE             NOT = COVTTRM-LIFE AND           <CAS1.0>
              TR517-ZRWVFLG-02         = 'N'                            <CAS1.0>
      *        GO TO 5290-EXIT                                  <LA1168><CAS1.0>
               GO TO 5289-NEXTR                                         <LA1168>
           END-IF.                                                      <CAS1.0>
                                                                        <PHE003>
           IF TR517-ZRWVFLG-05         = 'Y'                            <PHE003>
           AND COVTLNB-LIFE            = COVTTRM-LIFE                   <PHE003>
               GO TO 5289-NEXTR                                         <PHE003>
           END-IF.                                                      <PHE003>

     ***** IF COVTLNB-COVERAGE         NOT = COVTTRM-COVERAGE   <LFA1058>V42L014
     *****    MOVE ENDP                TO COVTLNB-STATUZ        <LFA1058>V42L014
     *****    GO TO 5290-EXIT                                   <LFA1058>V42L014
     ***** END-IF.                                                      V42L014
                                                                        <LFA1058
           IF TR517-ZRWVFLG-04         = 'Y'                            <LFA1058
              IF COVTLNB-COVERAGE         NOT = COVTTRM-COVERAGE        <LFA1058
                 MOVE ENDP                TO COVTLNB-STATUZ             <LFA1058
                 GO TO 5290-EXIT                                        <LFA1058
              END-IF                                                    <LFA1058
           END-IF.                                                      <LFA1058
                                                                        <LFA1058
           MOVE 'N'                    TO WSAA-WAIVE-IT.                <CAS1.0>
      *                                                                 <TN01>
       5220-CHECK.                                                      <TN01>
           PERFORM VARYING SUB1        FROM 1 BY 1                      <PHE003>
                   UNTIL   SUB1        > 50   OR                        <CAS1.0>
                   WSAA-WAIVE-IT       = 'Y'                            <CAS1.0>
              IF COVTLNB-CRTABLE       = TR517-CTABLE (SUB1)            <CAS1.0>
                  MOVE 'Y'             TO WSAA-WAIVE-IT                 <CAS1.0>
              END-IF                                                    <CAS1.0>
           END-PERFORM.                                                 <CAS1.0>
           IF  WSAA-WAIVE-IT            NOT = 'Y'                       <TN01>
           AND TR517-CONTITEM           NOT = SPACES                    <TN01>
               PERFORM 500C-READ-TR517                                  <TN01>
               IF WSAA-WAIVE-CONT       = 'Y'                           <TN01>
                  GO TO 5220-CHECK                                      <TN01>
               END-IF                                                   <TN01>
           END-IF.                                                      <TN01>
      *    IF WSAA-WAIVE-IT            = 'Y'                    <V42003><CAS1.0>
      *        ADD COVTLNB-INSTPREM    TO WSAA-WAIVE-SUMINS     <V42003><CAS1.0>
      *    END-IF.                                              <V42003><CAS1.0>

           IF  TR517-ZRWVFLG-04         = 'Y'  AND WSAA-WAIVE-IT = 'Y'  <V42003>
               IF COVTLNB-RIDER         = '00'                          <V42003>
      ****        ADD COVTLNB-SUMINS          TO WSAA-WAIVE-SUMINS      V42L014
                  MOVE COVTLNB-SUMINS         TO WSAA-WAIVE-SUMINS      V42L014
                  MOVE COVTLNB-CRTABLE        TO WSAA-MAIN-CRTABLE      <V42003>
                  MOVE COVTLNB-COVERAGE       TO WSAA-MAIN-COVERAGE     <V42003>
                  MOVE COVTLNB-RISK-CESS-DATE TO WSAA-MAIN-CESSDATE     <V42003>
                  MOVE COVTLNB-PREM-CESS-DATE TO WSAA-MAIN-PCESSDTE     <V42003>
                  MOVE COVTLNB-MORTCLS        TO WSAA-MAIN-MORTCLASS    <V42003>
                  MOVE COVTLNB-LIFE           TO WSAA-MAIN-LIFE         <V42003>
               ELSE                                                     <V42003>
                  COMPUTE WSAA-WAIVE-SUMINS = WSAA-WAIVE-SUMINS -       <V42003>
                                              COVTLNB-SUMINS            <V42003>
                  PERFORM A300-CALC-BENEFIT-AMOUNT                      <V42003>
               END-IF                                                   <V42003>
           ELSE
               IF WSAA-WAIVE-IT        = 'Y'                            <PHE003>
                  PERFORM 5300-READ-TV071                               <PHE003>
                                                                        <PHE003>
                  MOVE ZEROES          TO WSAA-DISC-AMOUNT              <PHE003>
                  IF  TV071-ACTN       = 'B'                            <PHE003>
                      PERFORM 5400-GET-DISC-AMOUNT                      <PHE003>
                  END-IF                                                <PHE003>
                                                                        <PHE003>
                  MOVE ZEROES          TO WSAA-AMT-BEFORE-DISC          <PHE003>
                  COMPUTE WSAA-AMT-BEFORE-DISC = COVTLNB-INSTPREM       <PHE003>
                                               + WSAA-DISC-AMOUNT       <PHE003>
                  ADD WSAA-AMT-BEFORE-DISC                              <PHE003>
                                       TO WSAA-WAIVE-SUMINS             <PHE003>
               END-IF                                                   <V42003>
           END-IF.                                                      <V42003>
      *                                                                 <LA1168>
       5289-NEXTR.                                                      <LA1168>
      *                                                                 <LA1168>
           MOVE NEXTR                  TO COVTLNB-FUNCTION.             <CAS1.0>
           GO TO 5210-CALL-COVTLNBIO.                                   <CAS1.0>
      *                                                                 <CAS1.0>
       5290-EXIT.                                                       <CAS1.0>
           EXIT.                                                        <CAS1.0>
      /                                                                 <PHE003>
       5300-READ-TV071 SECTION.                                         <PHE003>
      *************************                                         <PHE003>
      *                                                                 <PHE003>
       5310-START.                                                      <PHE003>
      *                                                                 <PHE003>
           MOVE SPACES                 TO TV071-TV071-REC.              <PHE003>
                                                                        <PHE003>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <PHE003>
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.                 <PHE003>
           MOVE TV071                  TO ITEM-ITEMTABL.                <PHE003>
           MOVE COVTTRM-CRTABLE        TO ITEM-ITEMITEM.                <PHE003>
           MOVE SPACES                 TO ITEM-ITEMSEQ .                <PHE003>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <PHE003>
           MOVE READR                  TO ITEM-FUNCTION.                <PHE003>
                                                                        <PHE003>
           CALL 'ITEMIO'               USING ITEM-PARAMS.               <PHE003>
                                                                        <PHE003>
           IF ITEM-STATUZ              NOT = O-K                        <PHE003>
           AND                         NOT = MRNF                       <PHE003>
              MOVE ITEM-PARAMS         TO SYSR-PARAMS                   <PHE003>
              PERFORM 600-FATAL-ERROR                                   <PHE003>
           END-IF.                                                      <PHE003>
                                                                        <PHE003>
           IF ITEM-STATUZ              = O-K                            <PHE003>
              MOVE ITEM-GENAREA        TO TV071-TV071-REC               <PHE003>
           END-IF.                                                      <PHE003>
      *                                                                 <PHE003>
       5390-EXIT.                                                       <PHE003>
           EXIT.                                                        <PHE003>
      /                                                                 <PHE003>
       5400-GET-DISC-AMOUNT SECTION.                                    <PHE003>
      ******************************                                    <PHE003>
      *                                                                 <PHE003>
       5410-START.                                                      <PHE003>
      *                                                         <PHE003><TN01>
           INITIALIZE                     ZDIS-PARAMS.                  <PHE003>
           MOVE COVTLNB-CHDRCOY        TO ZDIS-CHDRCOY .                <PHE003>
           MOVE COVTLNB-CHDRNUM        TO ZDIS-CHDRNUM .                <PHE003>
           MOVE COVTLNB-LIFE           TO ZDIS-LIFE    .                <PHE003>
           MOVE COVTLNB-COVERAGE       TO ZDIS-COVERAGE.                <PHE003>
           MOVE COVTLNB-RIDER          TO ZDIS-RIDER   .                <PHE003>
           MOVE ZDISREC                TO ZDIS-FORMAT  .                <PHE003>
           MOVE READR                  TO ZDIS-FUNCTION.                <PHE003>
                                                                        <PHE003>
           CALL 'ZDISIO'               USING ZDIS-PARAMS.               <PHE003>
                                                                        <PHE003>
           IF  ZDIS-STATUZ             NOT = O-K                        <PHE003>
           AND                         NOT = MRNF                       <PHE003>
               MOVE ZDIS-STATUZ        TO SYSR-STATUZ                   <PHE003>
               MOVE ZDIS-PARAMS        TO SYSR-PARAMS                   <PHE003>
               PERFORM 600-FATAL-ERROR                                  <PHE003>
           END-IF.                                                      <PHE003>
                                                                        <PHE003>
           IF  ZDIS-STATUZ             = O-K                            <PHE003>
               MOVE ZDIS-DISCAMT       TO WSAA-DISC-AMOUNT              <PHE003>
           END-IF.                                                      <PHE003>
      *                                                                 <PHE003>
       5490-EXIT.                                                       <PHE003>
           EXIT.                                                        <PHE003>
      /                                                                 <PHE003>
       500C-READ-TR517 SECTION.                                         <TN01>
      *************************                                         <TN01>
       510C-PARA.                                                       <TN01>
                                                                        <TN01>
           MOVE 'N'                    TO WSAA-WAIVE-CONT.              <TN01>
           MOVE TR517-ZRWVFLGS         TO WSAA-ZRWVFLGS.                <TN01>
           MOVE SPACES                 TO ITDM-PARAMS.                  <TN01>
           MOVE WSSP-COMPANY           TO ITDM-ITEMCOY.                 <TN01>
           MOVE TR517                  TO ITDM-ITEMTABL.                <TN01>
           MOVE TR517-CONTITEM         TO ITDM-ITEMITEM.                <TN01>
           MOVE CHDRLNB-OCCDATE        TO ITDM-ITMFRM.                  <TN01>
           MOVE BEGN                   TO ITDM-FUNCTION.                <TN01>
                                                                        <TN01>
           CALL 'ITDMIO'               USING ITDM-PARAMS.               <TN01>
                                                                        <TN01>
           IF ITDM-ITEMCOY             NOT = WSSP-COMPANY               <TN01>
           OR ITDM-ITEMTABL            NOT = TR517                      <TN01>
           OR ITDM-ITEMITEM            NOT = TR517-CONTITEM             <TN01>
           OR ITDM-STATUZ              NOT = O-K                        <TN01>
               GO TO 519C-EXIT                                          <TN01>
           END-IF.                                                      <TN01>
                                                                        <TN01>
           MOVE 'Y'                    TO WSAA-WAIVE-CONT.              <TN01>
           MOVE ITDM-GENAREA           TO TR517-TR517-REC.              <TN01>
           MOVE WSAA-ZRWVFLGS          TO TR517-ZRWVFLGS.               <TN01>
      *                                                                 <TN01>
       519C-EXIT.                                                       <TN01>
           EXIT.                                                        <TN01>
      *                                                                 <TN01>
       7000-CHECK-CALC-COMP-TAX SECTION.                                <V74L01>
      **********************************                                <V74L01>
       7010-START.                                                      <V74L01>
                                                                        <V74L01>
           IF TR52D-TXCODE             =  SPACE                         <V74L01>
              GO TO 7090-EXIT                                           <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF COVTLNB-SINGP            = ZERO                           <V74L01>
           AND COVTLNB-INSTPREM        = ZERO                           <V74L01>
              GO TO 7090-EXIT                                           <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
      * Read table TR52E                                                <V74L01>
           MOVE SPACES                 TO WSAA-TR52E-KEY.               <V74L01>
           MOVE TR52D-TXCODE           TO WSAA-TR52E-TXCODE.            <V74L01>
           MOVE CHDRLNB-CNTTYPE        TO WSAA-TR52E-CNTTYPE.           <V74L01>
           MOVE COVTLNB-CRTABLE        TO WSAA-TR52E-CRTABLE.           <V74L01>
           PERFORM 7200-READ-TR52E.                                     <V74L01>
                                                                        <V74L01>
           IF TR52E-TR52E-REC = SPACES                                  <V74L01>
              MOVE SPACES              TO WSAA-TR52E-KEY                <V74L01>
              MOVE TR52D-TXCODE        TO WSAA-TR52E-TXCODE             <V74L01>
              MOVE CHDRLNB-CNTTYPE     TO WSAA-TR52E-CNTTYPE            <V74L01>
              MOVE '****'              TO WSAA-TR52E-CRTABLE            <V74L01>
              PERFORM 7200-READ-TR52E                                   <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF TR52E-TR52E-REC = SPACES                                  <V74L01>
              MOVE SPACES              TO WSAA-TR52E-KEY                <V74L01>
              MOVE TR52D-TXCODE        TO WSAA-TR52E-TXCODE             <V74L01>
              MOVE '***'               TO WSAA-TR52E-CNTTYPE            <V74L01>
              MOVE '****'              TO WSAA-TR52E-CRTABLE            <V74L01>
              PERFORM 7200-READ-TR52E                                   <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
      * Call TR52D tax subroutine                                       <V74L01>
           IF TR52E-TAXIND-01          NOT = 'Y'                        <V74L01>
              GO TO 7090-EXIT                                           <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           INITIALIZE                     TXCL-LINK-REC.                <V74L01>
           MOVE 'CALC'                 TO TXCL-FUNCTION.                <V74L01>
           MOVE O-K                    TO TXCL-STATUZ.                  <V74L01>
           MOVE CHDRLNB-CHDRCOY        TO TXCL-CHDRCOY.                 <V74L01>
           MOVE CHDRLNB-CHDRNUM        TO TXCL-CHDRNUM.                 <V74L01>
           MOVE COVTLNB-LIFE           TO TXCL-LIFE.                    <V74L01>
           MOVE COVTLNB-COVERAGE       TO TXCL-COVERAGE.                <V74L01>
           MOVE COVTLNB-RIDER          TO TXCL-RIDER.                   <V74L01>
           MOVE ZERO                   TO TXCL-PLAN-SUFFIX.             <V74L01>
           MOVE COVTLNB-CRTABLE        TO TXCL-CRTABLE.                 <V74L01>
           MOVE CHDRLNB-CNTTYPE        TO TXCL-CNTTYPE.                 <V74L01>
           MOVE CHDRLNB-REGISTER       TO TXCL-REGISTER.                <V74L01>
           MOVE WSAA-TR52E-KEY         TO TXCL-TAXRULE.                 <V74L01>
           MOVE SPACES                 TO WSAA-RATE-ITEM.               <V74L01>
           MOVE CHDRLNB-CNTCURR        TO TXCL-CCY.                     <V74L01>
           MOVE CHDRLNB-CNTCURR        TO WSAA-CNT-CURR.                <V74L01>
           MOVE TR52E-TXITEM           TO WSAA-TXITEM.                  <V74L01>
           MOVE WSAA-RATE-ITEM         TO TXCL-RATE-ITEM.               <V74L01>
           MOVE CHDRLNB-OCCDATE        TO TXCL-EFFDATE.                 <V74L01>
           MOVE 'PREM'                 TO TXCL-TRANS-TYPE.              <V74L01>
           MOVE SPACES                 TO TXCL-TAX-TYPE(1)              <V74L01>
                                          TXCL-TAX-TYPE(2).             <V74L01>
           MOVE ZEROES                 TO TXCL-TAX-AMT(1)               <V74L01>
                                          TXCL-TAX-AMT(2).              <V74L01>
           MOVE SPACES                 TO TXCL-TAX-ABSORB(1)            <V74L01>
                                          TXCL-TAX-ABSORB(2).           <V74L01>
                                                                        <V74L01>
           IF COVTLNB-INSTPREM         NOT = ZERO                       <V74L01>
              IF TR52E-ZBASTYP         =  'Y'                           <V74L01>
                 MOVE COVTLNB-ZBINSTPREM                                <V74L01>
                                       TO TXCL-AMOUNT-IN                <V74L01>
              ELSE                                                      <V74L01>
                 MOVE COVTLNB-INSTPREM TO TXCL-AMOUNT-IN                <V74L01>
              END-IF                                                    <V74L01>
                                                                        <V74L01>
              CALL TR52D-TXSUBR        USING TXCL-LINK-REC              <V74L01>
              IF TXCL-STATUZ           NOT = O-K                        <V74L01>
                 MOVE TXCL-LINK-REC    TO SYSR-PARAMS                   <V74L01>
                 MOVE TXCL-STATUZ      TO SYSR-STATUZ                   <V74L01>
                 PERFORM 600-FATAL-ERROR                                <V74L01>
              END-IF                                                    <V74L01>
                                                                        <V74L01>
              IF TXCL-TAX-AMT(1)       > ZERO                           <V74L01>
              OR TXCL-TAX-AMT(2)       > ZERO                           <V74L01>
                 IF TXCL-TAX-ABSORB(1) NOT = 'Y'                        <V74L01>
                    ADD TXCL-TAX-AMT(1)                                 <V74L01>
                                       TO WSAA-RP-TAX(WSBB-SUB)         <V74L01>
                 END-IF                                                 <V74L01>
                 IF TXCL-TAX-ABSORB(2) NOT = 'Y'                        <V74L01>
                    ADD TXCL-TAX-AMT(2)                                 <V74L01>
                                       TO WSAA-RP-TAX(WSBB-SUB)         <V74L01>
                 END-IF                                                 <V74L01>
              END-IF                                                    <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF COVTLNB-SINGP            NOT = ZERO                       <V74L01>
              MOVE COVTLNB-SINGP       TO TXCL-AMOUNT-IN                <V74L01>
                                                                        <V74L01>
              CALL TR52D-TXSUBR        USING TXCL-LINK-REC              <V74L01>
              IF TXCL-STATUZ           NOT = O-K                        <V74L01>
                 MOVE TXCL-LINK-REC    TO SYSR-PARAMS                   <V74L01>
                 MOVE TXCL-STATUZ      TO SYSR-STATUZ                   <V74L01>
                 PERFORM 600-FATAL-ERROR                                <V74L01>
              END-IF                                                    <V74L01>
                                                                        <V74L01>
              IF TXCL-TAX-AMT(1)       > ZERO                           <V74L01>
              OR TXCL-TAX-AMT(2)       > ZERO                           <V74L01>
                 IF TXCL-TAX-ABSORB(1) NOT = 'Y'                        <V74L01>
                    ADD TXCL-TAX-AMT(1)                                 <V74L01>
                                       TO WSAA-SP-TAX(WSBB-SUB)         <V74L01>
                 END-IF                                                 <V74L01>
                 IF TXCL-TAX-ABSORB(2) NOT = 'Y'                        <V74L01>
                    ADD TXCL-TAX-AMT(2)                                 <V74L01>
                                       TO WSAA-SP-TAX(WSBB-SUB)         <V74L01>
                 END-IF                                                 <V74L01>
              END-IF                                                    <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
       7090-EXIT.                                                       <V74L01>
           EXIT.                                                        <V74L01>
      *                                                                 <V74L01>
       7100-CHECK-CALC-CONT-TAX SECTION.                                <V74L01>
      **********************************                                <V74L01>
       7110-START.                                                      <V74L01>
           MOVE 0                      TO WSAA-TAX                      <V74L01>
                                                                        <V74L01>
           IF MGFL-MGFEE               =  ZERO                          <V74L01>
              GO TO 7190-EXIT                                           <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF TR52D-TXCODE             =  SPACE                         <V74L01>
              GO TO 7190-EXIT                                           <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
      * Read table TR52E                                                <V74L01>
           MOVE SPACES                 TO WSAA-TR52E-KEY.               <V74L01>
           MOVE TR52D-TXCODE           TO WSAA-TR52E-TXCODE.            <V74L01>
           MOVE CHDRLNB-CNTTYPE        TO WSAA-TR52E-CNTTYPE.           <V74L01>
           MOVE '****'                 TO WSAA-TR52E-CRTABLE.           <V74L01>
           PERFORM 7200-READ-TR52E.                                     <V74L01>
                                                                        <V74L01>
           IF TR52E-TR52E-REC = SPACES                                  <V74L01>
              MOVE SPACES              TO WSAA-TR52E-KEY                <V74L01>
              MOVE TR52D-TXCODE        TO WSAA-TR52E-TXCODE             <V74L01>
              MOVE '***'               TO WSAA-TR52E-CNTTYPE            <V74L01>
              MOVE '****'              TO WSAA-TR52E-CRTABLE            <V74L01>
              PERFORM 7200-READ-TR52E                                   <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF TR52E-TAXIND-02          NOT = 'Y'                        <V74L01>
              GO TO 7190-EXIT                                           <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
      * Call TR52D tax subroutine                                       <V74L01>
           INITIALIZE                     TXCL-LINK-REC.                <V74L01>
           MOVE 'CALC'                 TO TXCL-FUNCTION.                <V74L01>
           MOVE O-K                    TO TXCL-STATUZ.                  <V74L01>
           MOVE CHDRLNB-CHDRCOY        TO TXCL-CHDRCOY.                 <V74L01>
           MOVE CHDRLNB-CHDRNUM        TO TXCL-CHDRNUM.                 <V74L01>
           MOVE SPACES                 TO TXCL-LIFE                     <V74L01>
                                          TXCL-COVERAGE                 <V74L01>
                                          TXCL-RIDER                    <V74L01>
                                          TXCL-CRTABLE.                 <V74L01>
           MOVE ZERO                   TO TXCL-PLAN-SUFFIX.             <V74L01>
           MOVE CHDRLNB-CNTTYPE        TO TXCL-CNTTYPE.                 <V74L01>
           MOVE CHDRLNB-REGISTER       TO TXCL-REGISTER.                <V74L01>
           MOVE WSAA-TR52E-KEY         TO TXCL-TAXRULE.                 <V74L01>
           MOVE SPACES                 TO WSAA-RATE-ITEM.               <V74L01>
           MOVE CHDRLNB-CNTCURR        TO TXCL-CCY.                     <V74L01>
           MOVE CHDRLNB-CNTCURR        TO WSAA-CNT-CURR.                <V74L01>
           MOVE TR52E-TXITEM           TO WSAA-TXITEM.                  <V74L01>
           MOVE WSAA-RATE-ITEM         TO TXCL-RATE-ITEM.               <V74L01>
           MOVE SPACES                 TO TXCL-TAX-TYPE(1)              <V74L01>
                                          TXCL-TAX-TYPE(2).             <V74L01>
           MOVE ZEROES                 TO TXCL-TAX-AMT(1)               <V74L01>
                                          TXCL-TAX-AMT(2).              <V74L01>
           MOVE SPACES                 TO TXCL-TAX-ABSORB(1)            <V74L01>
                                          TXCL-TAX-ABSORB(2).           <V74L01>
           MOVE WSAA-CNTFEE            TO TXCL-AMOUNT-IN.               <V74L01>
           MOVE CHDRLNB-OCCDATE        TO TXCL-EFFDATE.                 <V74L01>
           MOVE 'CNTF'                 TO TXCL-TRANS-TYPE.              <V74L01>
                                                                        <V74L01>
           CALL TR52D-TXSUBR           USING TXCL-LINK-REC.             <V74L01>
                                                                        <V74L01>
           IF TXCL-STATUZ              NOT = O-K                        <V74L01>
              MOVE TXCL-LINK-REC       TO SYSR-PARAMS                   <V74L01>
              MOVE TXCL-STATUZ         TO SYSR-STATUZ                   <V74L01>
              PERFORM 600-FATAL-ERROR                                   <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF TXCL-TAX-AMT(1)          >  ZERO                          <V74L01>
           OR TXCL-TAX-AMT(2)          >  ZERO                          <V74L01>
              IF TXCL-TAX-ABSORB(1)    NOT = 'Y'                        <V74L01>
                 ADD TXCL-TAX-AMT(1)   TO WSAA-TAX                      <V74L01>
              END-IF                                                    <V74L01>
              IF TXCL-TAX-ABSORB(2)    NOT = 'Y'                        <V74L01>
                 ADD TXCL-TAX-AMT(2)   TO WSAA-TAX                      <V74L01>
              END-IF                                                    <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
       7190-EXIT.                                                       <V74L01>
           EXIT.                                                        <V74L01>
                                                                        <V74L01>
       7200-READ-TR52E SECTION.                                         <V74L01>
      *************************                                         <V74L01>
       7210-START.                                                      <V74L01>
                                                                        <V74L01>
           MOVE SPACES                 TO ITDM-DATA-AREA                <V74L01>
                                          TR52E-TR52E-REC.              <V74L01>
           MOVE WSSP-COMPANY           TO ITDM-ITEMCOY.                 <V74L01>
           MOVE TR52E                  TO ITDM-ITEMTABL.                <V74L01>
           MOVE WSAA-TR52E-KEY         TO ITDM-ITEMITEM.                <V74L01>
           MOVE CHDRLNB-OCCDATE        TO ITDM-ITMFRM.                  <V74L01>
           MOVE BEGN                   TO ITDM-FUNCTION.                <V74L01>
                                                                        <V74L01>
           CALL 'ITDMIO'               USING ITDM-PARAMS.               <V74L01>
                                                                        <V74L01>
           IF (ITDM-STATUZ             NOT = O-K) AND                   <V74L01>
              (ITDM-STATUZ             NOT = ENDP)                      <V74L01>
               MOVE ITDM-PARAMS        TO SYSR-PARAMS                   <V74L01>
               MOVE ITDM-STATUZ        TO SYSR-STATUZ                   <V74L01>
               PERFORM 600-FATAL-ERROR                                  <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF ((ITDM-ITEMCOY           NOT = WSSP-COMPANY) OR           <V74L01>
               (ITDM-ITEMTABL          NOT = TR52E)          OR         <V74L01>
               (ITDM-ITEMITEM          NOT  = WSAA-TR52E-KEY) OR        <V74L01>
               (ITDM-STATUZ            = ENDP))        AND              <V74L01>
               (WSAA-TR52E-KEY(2:7)    = '*******')                     <V74L01>
               MOVE WSAA-TR52E-KEY     TO SYSR-PARAMS                   <V74L01>
               MOVE ITDM-STATUZ        TO SYSR-STATUZ                   <V74L01>
               PERFORM 600-FATAL-ERROR                                  <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF ((ITDM-ITEMCOY           = WSSP-COMPANY) AND              <V74L01>
               (ITDM-ITEMTABL          = TR52E)        AND              <V74L01>
               (ITDM-ITEMITEM          = WSAA-TR52E-KEY) AND            <V74L01>
               (ITDM-STATUZ            NOT = ENDP))                     <V74L01>
               MOVE ITDM-GENAREA       TO TR52E-TR52E-REC               <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
       7290-EXIT.                                                       <V74L01>
           EXIT.                                                        <V74L01>
                                                                        <V74L01>
      /                                                                 <V76F06>
       8000-CALL-ROUNDING SECTION.                                      <V76F06>
      ****************************                                      <V76F06>
       8100-CALL.                                                       <V76F06>
      *                                                                 <V76F06>
           MOVE SPACES                 TO ZRDP-FUNCTION                 <V76F06>
           MOVE WSSP-COMPANY           TO ZRDP-COMPANY.                 <V76F06>
           MOVE O-K                    TO ZRDP-STATUZ.                  <V76F06>
      *    MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
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
      /                                                                 <V76F06>
       A100-CHDR-OR-RISK SECTION.                                       <V42006>
       A100-CTRL.                                                       <V42006>
                                                                        <V42006>
           MOVE CHDRLNB-CHDRCOY        TO CHKRL-COMPANY.                <V42006>
           MOVE CHDRLNB-CNTTYPE        TO CHKRL-CNTTYPE.                <V42006>
           MOVE 'C/R?'                 TO CHKRL-FUNCTION.               <V42006>
           CALL 'CHKRLMT'              USING CHKRL-PARM-REC.            <V42006>
           IF CHKRL-STATUZ             NOT = 'R'                        <V42006>
              MOVE CHDRLNB-CNTTYPE     TO CHKRL-LRKCLSS                 <V42006>
              MOVE 1                   TO WSAA-SUB                      <V42006>
              PERFORM 1G00-CALL-HCRTFUP                                 <V42006>
              GO TO A100-EXIT                                           <V42006>
           END-IF.                                                      <V42006>
                                                                        <V42006>
           INITIALIZE COVTTRMREC-KEY-DATA.                              <V42006>
           MOVE CHDRLNB-CHDRCOY        TO COVTTRM-CHDRCOY.              <V42006>
           MOVE CHDRLNB-CHDRNUM        TO COVTTRM-CHDRNUM.              <V42006>
           MOVE COVTTRMREC             TO COVTTRM-FORMAT.               <V42006>
           MOVE BEGN                   TO COVTTRM-FUNCTION.             <V42006>
           MOVE O-K                    TO COVTTRM-STATUZ.               <V42006>
           PERFORM A200-NEXTR-COVTTRMIO                                 <V42006>
             UNTIL COVTTRM-STATUZ NOT = O-K.                            <V42006>
                                                                        <V42006>
       A100-EXIT.                                                       <V42006>
           EXIT.                                                        <V42006>
                                                                        <V42006>
       A200-NEXTR-COVTTRMIO SECTION.                                    <V42006>
       A200-CTRL.                                                       <V42006>
                                                                        <V42006>
           CALL 'COVTTRMIO'            USING COVTTRM-PARAMS.            <V42006>
           IF NOT (COVTTRM-STATUZ      = O-K                            <V42006>
           AND CHDRLNB-CHDRCOY         = COVTTRM-CHDRCOY                <V42006>
           AND CHDRLNB-CHDRNUM         = COVTTRM-CHDRNUM)               <V42006>
               MOVE ENDP               TO COVTTRM-STATUZ                <V42006>
               GO TO A200-EXIT.                                         <V42006>
           MOVE NEXTR                  TO COVTTRM-FUNCTION.             <V42006>
                                                                        <V42006>
           MOVE COVTTRM-CRTABLE        TO CHKRL-CRTABLE.                <V42006>
           MOVE 'READ'                 TO CHKRL-FUNCTION.               <V42006>
           CALL 'CHKRLMT'              USING CHKRL-PARM-REC.            <V42006>
           IF CHKRL-STATUZ             NOT = O-K                        <V42006>
              MOVE CHKRL-STATUZ        TO SYSR-STATUZ                   <V42006>
              MOVE CHKRL-PARAMS        TO SYSR-PARAMS                   <V42006>
              PERFORM 600-FATAL-ERROR                                   <V42006>
           END-IF.                                                      <V42006>
                                                                        <V42006>
           PERFORM 1G00-CALL-HCRTFUP                                    <V42006>
             VARYING WSAA-SUB FROM 1 BY 1                               <V42006>
             UNTIL WSAA-SUB > 5.                                        <V42006>
                                                                        <V42006>
       A200-EXIT.                                                       <V42006>
           EXIT.                                                        <V42006>
                                                                        <V42006>
       A300-CALC-BENEFIT-AMOUNT SECTION.                                <V42003>
       A300-CTRL.                                                       <V42003>
   ****                                                                 <V42003>
   **** To read T5687 2 times here, one for the main coverage the other <V42003>
   **** read is to det it back to the current rider. Same with T5675.   <V42003>
   ****                                                                 <V42003>
   ****  This is the first read using the main component                <V42003>
   ****                                                                 <V42003>
           MOVE WSSP-COMPANY           TO ITDM-ITEMCOY.                 <V42003>
           MOVE T5687                  TO ITDM-ITEMTABL.                <V42003>
           MOVE WSAA-MAIN-CRTABLE      TO ITDM-ITEMITEM.                <V42003>
           MOVE CHDRLNB-OCCDATE        TO ITDM-ITMFRM.                  <V42003>
           MOVE BEGN                   TO ITDM-FUNCTION.                <V42003>
           CALL 'ITDMIO' USING         ITDM-PARAMS.                     <V42003>
           IF WSSP-COMPANY       NOT = ITDM-ITEMCOY                     <V42003>
            OR T5687             NOT = ITDM-ITEMTABL                    <V42003>
            OR WSAA-MAIN-CRTABLE NOT = ITDM-ITEMITEM                    <V42003>
             MOVE ENDP                 TO ITDM-STATUZ.                  <V42003>
                                                                        <V42003>
           IF ITDM-STATUZ              NOT = O-K                        <V42003>
                                   AND NOT = ENDP                       <V42003>
              MOVE ITDM-PARAMS         TO SYSR-PARAMS                   <V42003>
              PERFORM 600-FATAL-ERROR.                                  <V42003>
                                                                        <V42003>
           IF ITDM-STATUZ              = ENDP                           <V42003>
              MOVE SPACES              TO T5687-T5687-REC               <V42003>
              MOVE F294                TO SCRN-ERROR-CODE               <V42003>
           ELSE                                                         <V42003>
              MOVE ITDM-GENAREA        TO T5687-T5687-REC.              <V42003>
   ****                                                                 <V42003>
   ****  Read table T5675                                               <V42003>
   ****                                                                 <V42003>
           MOVE READR                  TO ITEM-FUNCTION.                <V42003>
           MOVE T5687-PREMMETH         TO ITEM-ITEMITEM.                <V42003>
           MOVE T5675                  TO ITEM-ITEMTABL.                <V42003>
           CALL 'ITEMIO' USING         ITEM-PARAMS.                     <V42003>
           IF ITEM-STATUZ              NOT = O-K AND                    <V42003>
                                       NOT = MRNF                       <V42003>
              MOVE ITEM-PARAMS         TO SYSR-PARAMS                   <V42003>
              PERFORM 600-FATAL-ERROR.                                  <V42003>
                                                                        <V42003>
           IF ITEM-STATUZ              = MRNF                           <V42003>
              SET PREM-REQD            TO TRUE                          <V42003>
              GO TO A300-EXIT.                                          <V42003>
                                                                        <V42003>
           MOVE ITEM-GENAREA           TO T5675-T5675-REC.              <V42003>
                                                                        <V42003>
   ****                                                                 <V42003>
   ****  Read Main Life file                                            <V42003>
   ****                                                                 <V42003>
           MOVE SPACES                 TO LIFELNB-PARAMS.               <V42003>
           MOVE WSSP-COMPANY           TO LIFELNB-CHDRCOY.              <V42003>
           MOVE CHDRLNB-CHDRNUM        TO LIFELNB-CHDRNUM.              <V42003>
           MOVE WSAA-MAIN-LIFE         TO LIFELNB-LIFE.                 <V42003>
           MOVE '00'                   TO LIFELNB-JLIFE.                <V42003>
           MOVE READR                  TO LIFELNB-FUNCTION.             <V42003>
           CALL 'LIFELNBIO'            USING LIFELNB-PARAMS.            <V42003>
           IF  LIFELNB-STATUZ          NOT = O-K                        <V42003>
               MOVE LIFELNB-PARAMS     TO SYSR-PARAMS                   <V42003>
               PERFORM 600-FATAL-ERROR.                                 <V42003>
                                                                        <V42003>
           MOVE LIFELNB-ANB-AT-CCD     TO  WSAA-ANB-AT-CCD.             <V42003>
           MOVE LIFELNB-CLTDOB         TO  WSAA-CLTDOB.                 <V42003>
           MOVE LIFELNB-CLTSEX         TO  WSAA-SEX.                    <V42003>
           MOVE ZEROES                 TO  WSBB-ANB-AT-CCD.             <V42003>
           MOVE ZEROES                 TO  WSBB-CLTDOB.                 <V42003>
           MOVE SPACES                 TO  WSBB-SEX.                    <V42003>
   ****                                                                 <V42003>
   ****  Read Join Life file                                            <V42003>
   ****                                                                 <V42003>
           MOVE '01'                   TO LIFELNB-JLIFE.                <V42003>
           MOVE READR                  TO LIFELNB-FUNCTION.             <V42003>
           CALL 'LIFELNBIO'            USING LIFELNB-PARAMS.            <V42003>
           IF  LIFELNB-STATUZ          NOT = O-K                        <V42003>
                                   AND NOT = MRNF                       <V42003>
               MOVE LIFELNB-PARAMS     TO SYSR-PARAMS                   <V42003>
               PERFORM 600-FATAL-ERROR.                                 <V42003>
                                                                        <V42003>
           IF  LIFELNB-STATUZ          = O-K                            <V42003>
               MOVE LIFELNB-ANB-AT-CCD     TO  WSAA-ANB-AT-CCD          <V42003>
               MOVE LIFELNB-CLTDOB         TO  WSAA-CLTDOB              <V42003>
               MOVE LIFELNB-CLTSEX         TO  WSAA-SEX                 <V42003>
           END-IF.                                                      <V42003>
                                                                        <V42003>
           PERFORM A400-CALL-PREMIUM-CALC.                              <V42003>
                                                                        <V42003>
                                                                        <V42003>
       A300-EXIT.                                                       <V42003>
           EXIT.                                                        <V42003>
                                                                        <V42003>
       A400-CALL-PREMIUM-CALC   SECTION.                                <V42003>
       A400-CTRL.                                                       <V42003>
                                                                        <V42003>
           MOVE 'CALC'                 TO CPRM-FUNCTION.                <V42003>
           MOVE WSAA-MAIN-CRTABLE      TO CPRM-CRTABLE.                 <V42003>
           MOVE CHDRLNB-CHDRCOY        TO CPRM-CHDR-CHDRCOY.            <V42003>
           MOVE CHDRLNB-CHDRNUM        TO CPRM-CHDR-CHDRNUM.            <V42003>
           MOVE WSAA-MAIN-LIFE         TO CPRM-LIFE-LIFE.               <V42003>
           MOVE '00'                   TO CPRM-LIFE-JLIFE.              <V42003>
           MOVE WSAA-MAIN-COVERAGE     TO CPRM-COVR-COVERAGE.           <V42003>
           MOVE '00'                   TO CPRM-COVR-RIDER.              <V42003>
           MOVE CHDRLNB-OCCDATE        TO CPRM-EFFECTDT.                <V42003>
           MOVE WSAA-MAIN-PCESSDTE     TO CPRM-TERMDATE.                <V42003>
           MOVE CHDRLNB-CNTCURR        TO CPRM-CURRCODE.                <V42003>
           MOVE WSAA-SEX               TO CPRM-LSEX.                    <V42003>
           MOVE WSAA-ANB-AT-CCD        TO CPRM-LAGE.                    <V42003>
           MOVE WSBB-SEX               TO CPRM-JLSEX.                   <V42003>
           MOVE WSBB-ANB-AT-CCD        TO CPRM-JLAGE.                   <V42003>
      *                                                                 <V42003>
           MOVE CPRM-EFFECTDT          TO DTC3-INT-DATE-1.              <V42003>
           MOVE CPRM-TERMDATE          TO DTC3-INT-DATE-2.              <V42003>
           MOVE FREQ-YRLY              TO DTC3-FREQUENCY.               <V42003>
           CALL 'DATCON3' USING        DTC3-DATCON3-REC.                <V42003>
           IF DTC3-STATUZ              NOT = O-K                        <V42003>
               MOVE DTC3-STATUZ        TO SYSR-STATUZ                   <V42003>
               PERFORM 600-FATAL-ERROR.                                 <V42003>
           ADD 0.99999  TO DTC3-FREQ-FACTOR.                            <V42003>
           MOVE DTC3-FREQ-FACTOR       TO CPRM-DURATION.                <V42003>
      *  (wsaa-sumin already adjusted for plan processing)              <V42003>
                                                                        <V42003>
           MOVE CHDRLNB-CNTTYPE        TO CPRM-CNTTYPE.                 <V42003>
                                                                        <V42003>
           MOVE CPRM-EFFECTDT          TO DTC3-INT-DATE-1.              <V42003>
           MOVE WSAA-MAIN-CESSDATE     TO DTC3-INT-DATE-2.              <V42003>
           MOVE FREQ-YRLY              TO DTC3-FREQUENCY.               <V42003>
           CALL 'DATCON3' USING DTC3-DATCON3-REC.                       <V42003>
           IF DTC3-STATUZ NOT = O-K                                     <V42003>
              MOVE DTC3-STATUZ         TO SYSR-STATUZ                   <V42003>
              PERFORM 600-FATAL-ERROR.                                  <V42003>
                                                                        <V42003>
           ADD 0.99999                 TO DTC3-FREQ-FACTOR.             <V42003>
           MOVE DTC3-FREQ-FACTOR       TO WSAA-RISK-CESS-TERM.          <V42003>
           MOVE WSAA-RISK-CESS-TERM    TO CPRM-RISK-CESS-TERM.          <V42003>
                                                                        <V42003>
           MOVE ZEROES                 TO CPRM-BEN-CESS-TERM.           <V42003>
                                                                        <V42003>
           MOVE WSAA-WAIVE-SUMINS      TO CPRM-SUMIN.                   <V42003>
           MOVE WSAA-MAIN-MORTCLASS    TO CPRM-MORTCLS.                 <V42003>
           MOVE PAYR-BILLFREQ          TO CPRM-BILLFREQ.                <V42003>
           MOVE PAYR-BILLCHNL          TO CPRM-MOP                      <V42003>
           MOVE CHDRLNB-OCCDATE        TO CPRM-RATINGDATE.              <V42003>
           MOVE CHDRLNB-OCCDATE        TO CPRM-RE-RATE-DATE.            <V42003>
           MOVE ZEROES                 TO CPRM-CALC-PREM.               <V42003>
           MOVE ZEROES                 TO CPRM-CALC-BAS-PREM.           <V42003>
           MOVE ZEROES                 TO CPRM-CALC-LOA-PREM.           <V42003>
      *                                                                 <V42003>
            MOVE SPACES             TO  CPRM-ADVANCE,                   <V42003>
                                        CPRM-ARREARS,                   <V42003>
                                        CPRM-FREQANN,                   <V42003>
                                        CPRM-WITHPROP,                  <V42003>
                                        CPRM-WITHOPROP,                 <V42003>
                                        CPRM-PPIND,                     <V42003>
                                        CPRM-NOMLIFE                    <V42003>
            MOVE ZEROS              TO  CPRM-GUARPERD,                  <V42003>
                                        CPRM-INTANNY,                   <V42003>
                                        CPRM-CAPCONT,                   <V42003>
                                        CPRM-DTHPERCN,                  <V42003>
                                        CPRM-DTHPERCO                   <V42003>
                                                                        <V42003>
           MOVE WSSP-LANGUAGE          TO CPRM-LANGUAGE.                <MLS001>
           CALL T5675-PREMSUBR USING   CPRM-PREMIUM-REC.                <V42003>
                                                                        <V42003>
           IF CPRM-STATUZ               = BOMB                          <V42003>
               MOVE CPRM-PREMIUM-REC   TO SYSR-PARAMS                   <V42003>
               MOVE CPRM-STATUZ        TO SYSR-STATUZ                   <V42003>
               PERFORM 600-FATAL-ERROR.                                 <V42003>
                                                                        <V42003>
           IF CPRM-STATUZ NOT = O-K                                     <V42003>
              GO TO A300-EXIT.                                          <V42003>
                                                                        <V42003>
           MOVE   CPRM-CALC-PREM       TO WSAA-WAIVE-SUMINS.            <V42003>
                                                                        <V42003>
       A400-EXIT.                                                       <V42003>
           EXIT.                                                        <V42003>
      /                                                                 <V72L07>
       A1700-CROSS-CHECK-PRODUCT SECTION.                               <V72L07>
      ***********************************                               <V72L07>
       A1710-BEGIN.                                                     <V72L07>
           INITIALIZE VLPD-VALID-REC.                                   <V72L07>
           MOVE 'PROP'                 TO VLPD-FUNCTION.                <V72L07>
           MOVE WSKY-BATC-BATCTRCDE    TO VLPD-BATCTRCDE.               <V72L07>
           MOVE CHDRLNB-CHDRCOY        TO VLPD-CHDRCOY.                 <V72L07>
           MOVE CHDRLNB-CHDRNUM        TO VLPD-CHDRNUM.                 <V72L07>
           MOVE CHDRLNB-OCCDATE        TO VLPD-EFFDATE.                 <V72L07>
           MOVE CHDRLNB-CNTTYPE        TO VLPD-CNTTYPE.                 <V72L07>
           MOVE CHDRLNB-SRCEBUS        TO VLPD-SRCEBUS.                 <V72L07>
           MOVE WSSP-LANGUAGE          TO VLPD-LANGUAGE.                <V72L07>
           MOVE CHDRLNB-COWNNUM        TO VLPD-COWNNUM.                 <V72L07>
           MOVE CHDRLNB-BILLFREQ       TO VLPD-BILLFREQ.                <V72L07>
           CALL 'VLPDRULE'             USING VLPD-VALID-REC.            <V72L07>
           IF VLPD-STATUZ              NOT = O-K                        <V72L07>
               MOVE VLPD-VALID-REC     TO SYSR-PARAMS                   <V72L07>
               MOVE VLPD-STATUZ        TO SYSR-STATUZ                   <V72L07>
               PERFORM 600-FATAL-ERROR                                  <V72L07>
           END-IF.                                                      <V72L07>
                                                                        <V72L07>
           PERFORM                     VARYING WSAA-IXC FROM 1 BY 1     <V72L07>
                                       UNTIL WSAA-IXC > 50              <V72L07>
              IF VLPD-ERR-LIFE(WSAA-IXC) = SPACES                       <V72L07>
              AND VLPD-ERR-CNTTYPE(WSAA-IXC)                            <V72L07>
                                       = SPACES                         <V72L07>
                 MOVE 50               TO WSAA-IXC                      <V72L07>
              ELSE                                                      <V72L07>
                 PERFORM               VARYING WSAA-IYC FROM 1 BY 1     <V72L07>
                                       UNTIL WSAA-IYC > 20              <V72L07>
                   IF VLPD-ERR-CODE(WSAA-IXC WSAA-IYC)                  <V72L07>
                                       = SPACES                         <V72L07>
                      MOVE 20          TO WSAA-IYC                      <V72L07>
                   ELSE                                                 <V72L07>
                      MOVE SPACES      TO ERMS-ERRMESG-REC              <V72L07>
                      MOVE VLPD-ERR-CODE(WSAA-IXC WSAA-IYC)             <V72L07>
                                       TO ERMS-EROR                     <V72L07>
                      MOVE 0           TO S6378-PAYRSEQNO               <V72L07>
                      PERFORM A1800-PRODUCT-ERROR-MESSAGE               <V72L07>
                   END-IF                                               <V72L07>
                 END-PERFORM                                            <V72L07>
              END-IF                                                    <V72L07>
           END-PERFORM.                                                 <V72L07>
       A1790-EXIT.                                                      <V72L07>
           EXIT.                                                        <V72L07>
      /                                                                 <V72L07>
       A1800-PRODUCT-ERROR-MESSAGE SECTION.                             <V72L07>
      *************************************                             <V72L07>
      *                                                                 <V72L07>
       A1810-ERROR.                                                     <V72L07>
      ****                                                              <V72L07>
      **** Call ERRMESG to retrieve the error message for display.      <V72L07>
      ****                                                              <V72L07>
                                                                        <V72L07>
           MOVE SCRN-LANGUAGE          TO ERMS-LANGUAGE.                <V72L07>
           MOVE WSAA-PROG              TO ERMS-EROR-PROG.               <V72L07>
           MOVE SCRN-COMPANY           TO ERMS-COMPANY.                 <V72L07>
           MOVE SPACES                 TO ERMS-FUNCTION.                <V72L07>
                                                                        <V72L07>
           CALL 'ERRMESG'              USING ERMS-ERRMESG-REC.          <V72L07>
                                                                        <V72L07>
      ****                                                              <V72L07>
      **** Write error to subfile                                       <V72L07>
      ****                                                              <V72L07>
                                                                        <V72L07>
           MOVE VLPD-ERR-LIFE(WSAA-IXC)                                 <V72L07>
                                       TO S6378-LIFE.                   <V72L07>
           MOVE VLPD-ERR-JLIFE(WSAA-IXC)                                <V72L07>
                                       TO S6378-JLIFE.                  <V72L07>
           MOVE VLPD-ERR-COVERAGE(WSAA-IXC)                             <V72L07>
                                       TO S6378-COVERAGE.               <V72L07>
           MOVE VLPD-ERR-RIDER(WSAA-IXC)                                <V72L07>
                                       TO S6378-RIDER.                  <V72L07>
           MOVE ZEROES                 TO S6378-PAYRSEQNO.              <V72L07>
                                                                        <V72L07>
           IF VLPD-ERR-DET(WSAA-IXC WSAA-IYC)                           <V72L07>
                                       = SPACES                         <V72L07>
              MOVE ERMS-ERRMESG(01)    TO S6378-ERORDSC                 <V72L07>
           ELSE                                                         <V72L07>
              MOVE VLPD-ERR-DET(WSAA-IXC WSAA-IYC)                      <V72L07>
                                       TO WSAA-PRODTYP                  <V72L07>
              MOVE ERMS-ERRMESG(01)    TO WSAA-PROD-ERR                 <V72L07>
              MOVE WSAA-PROD-ERROR     TO S6378-ERORDSC                 <V72L07>
           END-IF.                                                      <V72L07>
           MOVE ERMS-EROR              TO S6378-ERRCDE.                 <V72L07>
      *                                                                 <V72L07>
      **** Add the record to the subfile.                               <V72L07>
           MOVE SADD                   TO SCRN-FUNCTION.                <V72L07>
                                                                        <V72L07>
           CALL 'S6378IO'              USING SCRN-SCREEN-PARAMS         <V72L07>
                                             S6378-DATA-AREA            <V72L07>
                                             S6378-SUBFILE-AREA.        <V72L07>
           IF SCRN-STATUZ              NOT = O-K                        <V72L07>
               MOVE SCRN-STATUZ        TO SYSR-STATUZ                   <V72L07>
               PERFORM 600-FATAL-ERROR                                  <V72L07>
           END-IF.                                                      <V72L07>
                                                                        <V72L07>
           MOVE 'Y'                    TO WSAA-ERROR-FLAG.              <V72L07>
       A1899-EXIT.                                                      <V72L07>
            EXIT.                                                       <V72L07>
      /                                                                 <A06596>
       A2000-CHECK-DEATH-DATE SECTION.                                  <A06596>
      ********************************                                  <A06596>
       A2010-PARA.                                                      <A06596>
      ****  Check that the Joint Owner is not dead.                     <A06596>
           IF                                                           <A06596>
              CHDRLNB-JOWNNUM       NOT = SPACES                        <A06596>
           THEN                                                         <A06596>
              MOVE CHDRLNB-JOWNNUM     TO WSAA-CLNTNUM-IO               <A06596>
              PERFORM A3000-CALL-CLTSIO                                 <A06596>
              IF CLTS-STATUZ            = O-K                           <A06596>
              IF CLTS-CLTDOD        NOT = VRCM-MAX-DATE                 <A06596>
              AND CHDRLNB-OCCDATE      > CLTS-CLTDOD                    <A06596>
                  MOVE SPACES          TO ERMS-ERRMESG-REC              <A06596>
                  MOVE W343            TO ERMS-EROR                     <A06596>
                  MOVE SPACES          TO S6378-COVERAGE                <A06596>
                                          S6378-RIDER                   <A06596>
                  MOVE ZEROES          TO S6378-PAYRSEQNO               <A06596>
                  MOVE 'JO'            TO WSAA-EXTRA-MSGPFX             <A06596>
                  PERFORM 1800-ERROR-MESSAGES                           <A06596>
              END-IF                                                    <A06596>
              END-IF                                                    <A06596>
           END-IF.                                                      <A06596>
                                                                        <A06596>
      ****  Check that the Despatch address is not dead.                <A06596>
           IF                                                           <A06596>
              CHDRLNB-DESPNUM       NOT = SPACES                        <A06596>
           THEN                                                         <A06596>
              MOVE CHDRLNB-DESPNUM     TO WSAA-CLNTNUM-IO               <A06596>
              PERFORM A3000-CALL-CLTSIO                                 <A06596>
              IF CLTS-STATUZ            = O-K                           <A06596>
              IF CLTS-CLTDOD        NOT = VRCM-MAX-DATE                 <A06596>
              AND CHDRLNB-OCCDATE      > CLTS-CLTDOD                    <A06596>
                  MOVE SPACES          TO ERMS-ERRMESG-REC              <A06596>
                  MOVE W343            TO ERMS-EROR                     <A06596>
                  MOVE SPACES          TO S6378-COVERAGE                <A06596>
                                          S6378-RIDER                   <A06596>
                  MOVE ZEROES          TO S6378-PAYRSEQNO               <A06596>
                  MOVE 'DA'            TO WSAA-EXTRA-MSGPFX             <A06596>
                  PERFORM 1800-ERROR-MESSAGES                           <A06596>
              END-IF                                                    <A06596>
              END-IF                                                    <A06596>
           END-IF.                                                      <A06596>
                                                                        <A06596>
      ****  Check that the Assignee is not dead.                        <A06596>
       A2030-START-ASSIGNEE.                                            <A06596>
           MOVE SPACES                 TO ASGNLNB-PARAMS.               <A06596>
           MOVE CHDRLNB-CHDRCOY        TO ASGNLNB-CHDRCOY.              <A06596>
           MOVE CHDRLNB-CHDRNUM        TO ASGNLNB-CHDRNUM.              <A06596>
           MOVE ZERO                   TO ASGNLNB-SEQNO.                <A06596>
           MOVE BEGN                   TO ASGNLNB-FUNCTION.             <A06596>
           MOVE O-K                    TO ASGNLNB-STATUZ.               <V42006>
           PERFORM A2100-CHECK-ASSIGNEE UNTIL ASGNLNB-STATUZ = ENDP.    <A06596>
                                                                        <A06596>
      ****  Check that the Trustee is not dead.                         <A06596>
       A2060-START-TRUSTEE.                                             <A06596>
           MOVE SPACES                 TO CTRS-PARAMS.                  <A06596>
           MOVE CHDRLNB-CHDRCOY        TO CTRS-CHDRCOY.                 <A06596>
           MOVE CHDRLNB-CHDRNUM        TO CTRS-CHDRNUM.                 <A06596>
           MOVE ZERO                   TO CTRS-SEQNO.                   <A06596>
           MOVE BEGN                   TO CTRS-FUNCTION.                <A06596>
           MOVE O-K                    TO CTRS-STATUZ.                  <V42006>
           PERFORM A2200-CHECK-TRUSTEE UNTIL CTRS-STATUZ = ENDP.        <A06596>
      *                                                                 <A06596>
       A2069-EXIT.                                                      <A06596>
           EXIT.                                                        <A06596>
      *                                                                 <A06596>
      ****  Check that the Assignee is not dead.                        <A06596>
       A2100-CHECK-ASSIGNEE SECTION.                                    <A06596>
       A2101-START.                                                     <A06596>
           CALL 'ASGNLNBIO'         USING ASGNLNB-PARAMS.               <A06596>
           MOVE NEXTR                  TO ASGNLNB-FUNCTION.             <A06596>
           IF ASGNLNB-CHDRCOY       NOT = CHDRLNB-CHDRCOY               <A06596>
           OR ASGNLNB-CHDRNUM       NOT = CHDRLNB-CHDRNUM               <A06596>
           OR ASGNLNB-STATUZ            = ENDP                          <A06596>
              MOVE ENDP                TO ASGNLNB-STATUZ                <A06596>
           ELSE                                                         <A06596>
              MOVE ASGNLNB-ASGNNUM     TO WSAA-CLNTNUM-IO               <A06596>
              PERFORM A3000-CALL-CLTSIO                                 <A06596>
              IF CLTS-STATUZ            = O-K                           <A06596>
              IF CLTS-CLTDOD        NOT = VRCM-MAX-DATE                 <A06596>
              AND CHDRLNB-OCCDATE      > CLTS-CLTDOD                    <A06596>
                  MOVE SPACES          TO ERMS-ERRMESG-REC              <A06596>
                  MOVE W343            TO ERMS-EROR                     <A06596>
                  MOVE SPACES          TO S6378-COVERAGE                <A06596>
                                          S6378-RIDER                   <A06596>
                  MOVE ZEROES          TO S6378-PAYRSEQNO               <A06596>
                  MOVE 'NE'            TO WSAA-EXTRA-MSGPFX             <A06596>
                  PERFORM 1800-ERROR-MESSAGES                           <A06596>
              END-IF                                                    <A06596>
              END-IF                                                    <A06596>
           END-IF.                                                      <A06596>
      *                                                                 <A06596>
       A2109-EXIT.                                                      <A06596>
           EXIT.                                                        <A06596>
      *                                                                 <A06596>
      ****  Check that the Trustee is not dead.                         <A06596>
       A2200-CHECK-TRUSTEE  SECTION.                                    <A06596>
       A2201-START.                                                     <A06596>
           CALL 'CTRSIO'         USING CTRS-PARAMS.                     <A06596>
           MOVE NEXTR                  TO CTRS-FUNCTION.                <A06596>
           IF CTRS-CHDRCOY       NOT = CHDRLNB-CHDRCOY                  <A06596>
           OR CTRS-CHDRNUM       NOT = CHDRLNB-CHDRNUM                  <A06596>
           OR CTRS-STATUZ            = ENDP                             <A06596>
              MOVE ENDP                TO CTRS-STATUZ                   <A06596>
           ELSE                                                         <A06596>
              MOVE CTRS-CLNTNUM        TO WSAA-CLNTNUM-IO               <A06596>
              PERFORM A3000-CALL-CLTSIO                                 <A06596>
              IF CLTS-STATUZ            = O-K                           <A06596>
              IF CLTS-CLTDOD        NOT = VRCM-MAX-DATE                 <A06596>
              AND CHDRLNB-OCCDATE      > CLTS-CLTDOD                    <A06596>
                  MOVE SPACES          TO ERMS-ERRMESG-REC              <A06596>
                  MOVE W343            TO ERMS-EROR                     <A06596>
                  MOVE SPACES          TO S6378-COVERAGE                <A06596>
                                          S6378-RIDER                   <A06596>
                  MOVE ZEROES          TO S6378-PAYRSEQNO               <A06596>
                  MOVE 'TR'            TO WSAA-EXTRA-MSGPFX             <A06596>
                  PERFORM 1800-ERROR-MESSAGES                           <A06596>
              END-IF                                                    <A06596>
              END-IF                                                    <A06596>
           END-IF.                                                      <A06596>
      *                                                                 <A06596>
       A2209-EXIT.                                                      <A06596>
           EXIT.                                                        <A06596>
      *                                                                 <A06596>
       A3000-CALL-CLTSIO SECTION.                                       <A06596>
      ******************************                                    <A06596>
       A3010-PARA.                                                      <A06596>
           MOVE SPACES                 TO CLTS-PARAMS.                  <A06596>
           MOVE 'CN'                   TO CLTS-CLNTPFX.                 <A06596>
           MOVE WSSP-FSUCO             TO CLTS-CLNTCOY.                 <A06596>
           MOVE WSAA-CLNTNUM-IO        TO CLTS-CLNTNUM.                 <A06596>
           MOVE READR                  TO CLTS-FUNCTION.                <A06596>
                                                                        <A06596>
           CALL 'CLTSIO'               USING CLTS-PARAMS.               <A06596>
                                                                        <A06596>
           IF CLTS-STATUZ              NOT = O-K                        <A06596>
           AND CLTS-STATUZ             NOT = MRNF                       <A06596>
              MOVE CLTS-PARAMS         TO SYSR-PARAMS                   <A06596>
              PERFORM 600-FATAL-ERROR                                   <A06596>
           END-IF.                                                      <A06596>
      *                                                                 <A06596>
       A3090-EXIT.                                                      <A06596>
           EXIT.                                                        <A06596>
      /                                                                 <NB010>
       A4000-CHECK-AGENTSER SECTION.                                    <NB010>
      ******************************                                    <NB010>
       A4010-START.                                                     <NB010>
      *                                                                 <NB010>
           MOVE AGNT-CLNTNUM           TO WSAA-SERVAGNT.                <NB010>
           MOVE CHDRLNB-COWNNUM        TO WSAA-COWNNUM.                 <NB010>
           MOVE 'Y'                    TO WSAA-WARNING.                 <NB010>
                                                                        <NB010>
           IF WSAA-SERVAGNT            = WSAA-COWNNUM                   <NB010>
              MOVE 'N'                 TO WSAA-WARNING                  <NB010>
           ELSE                                                         <NB010>
              MOVE 0 TO IDX                                             <NB010>
              PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 10 OR         <NB010>
                                        WSAA-LIFCNUM(IDX) = SPACES      <NB010>
                IF WSAA-SERVAGNT       = WSAA-LIFCNUM(IDX)              <NB010>
                     MOVE 'N'            TO WSAA-WARNING                <NB010>
                     MOVE 11             TO IDX                         <NB010>
                  END-IF                                                <NB010>
                END-PERFORM                                             <NB010>
           END-IF.                                                      <NB010>
      *                                                                 <NB010>
       A4090-EXIT.                                                      <NB010>
           EXIT.                                                        <NB010>
      /                                                                 <NB010>
       A5000-CHECK-WARNING SECTION.                                     <NB010>
      *****************************                                     <NB010>
       A5010-START.                                                     <NB010>
      *                                                                 <NB010>
           MOVE WSAA-COWNNUM        TO WSAA-CLTNUM.                     <NB010>
           PERFORM A6000-CHECK-OWNER-AGENT.                             <NB010>
                                                                        <NB010>
           IF CLRRFCC-STATUZ           = O-K                            <NB010>
           AND WSAA-K                  NOT = 'Y'                        <NB010>
               MOVE EV95               TO S6378-CHDRNUM-ERR             <NB010>
               MOVE 'Y'                TO WSAA-K                        <NB010>
               GO TO A5090-EXIT                                         <NB010>
           ELSE                                                         <NB010>
               MOVE 0                  TO IDX                           <NB010>
               PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 10 OR        <NB010>
                                         WSAA-LIFCNUM(IDX) = SPACES     <NB010>
                  MOVE WSAA-LIFCNUM(IDX)  TO WSAA-CLTNUM                <NB010>
                  PERFORM A6000-CHECK-OWNER-AGENT                       <NB010>
                                                                        <NB010>
                  IF CLRRFCC-STATUZ    = O-K                            <NB010>
                  AND WSAA-K           NOT = 'Y'                        <NB010>
                      MOVE EV95        TO S6378-CHDRNUM-ERR             <NB010>
                      MOVE 'Y'         TO WSAA-K                        <NB010>
                      MOVE 11          TO IDX                           <NB010>
                  END-IF                                                <NB010>
               END-PERFORM                                              <NB010>
           END-IF.                                                      <NB010>
      *                                                                 <NB010>
       A5090-EXIT.                                                      <NB010>
           EXIT.                                                        <NB010>
      /                                                                 <NB010>
       A6000-CHECK-OWNER-AGENT SECTION.                                 <NB010>
      *******************************                                   <NB010>
       A6010-START.                                                     <NB010>
      *                                                                 <NB010>
           INITIALIZE                  CLRRFCC-PARAMS.                  <NB010>
           MOVE '2'                    TO CLRRFCC-FORECOY.              <NB010>
           MOVE WSAA-CLTNUM            TO CLRRFCC-CLNTNUM.              <NB010>
           MOVE 'AG'                   TO CLRRFCC-CLRRROLE.             <NB010>
           MOVE CLRRFCCREC             TO CLRRFCC-FORMAT.               <NB010>
           MOVE READR                  TO CLRRFCC-FUNCTION.             <NB010>
                                                                        <NB010>
           CALL 'CLRRFCCIO'            USING CLRRFCC-PARAMS.            <NB010>
                                                                        <NB010>
           IF CLRRFCC-STATUZ           NOT = O-K                        <NB010>
           AND                         NOT = MRNF                       <NB010>
               MOVE CLRRFCC-PARAMS     TO SYSR-PARAMS                   <NB010>
               PERFORM 600-FATAL-ERROR                                  <NB010>
           END-IF.                                                      <NB010>
      *                                                                 <NB010>
       A6090-EXIT.                                                      <NB010>
           EXIT.                                                        <NB010>
      /                                                                 <NB043>
       B1300-CHECK-MORTALITY-LA SECTION.                                <NB043>
      ***********************************                               <NB043>
       B1310-START.                                                     <NB043>
      *                                                                 <NB043>
           MOVE SPACES                 TO LIFERNL-PARAMS.               <NB043>
           MOVE CHDRLNB-CHDRCOY        TO LIFERNL-CHDRCOY.              <NB043>
           MOVE CHDRLNB-CHDRNUM        TO LIFERNL-CHDRNUM.              <NB043>
           MOVE '01'                   TO LIFERNL-LIFE.                 <NB043>
           MOVE '00'                   TO LIFERNL-JLIFE.                <NB043>
           MOVE LIFERNLREC             TO LIFERNL-FORMAT.               <NB043>
           MOVE BEGN                   TO LIFERNL-FUNCTION.             <NB043>
                                                                        <NB043>
       B1320-CALL.                                                      <NB043>
           CALL 'LIFERNLIO' USING LIFERNL-PARAMS.                       <NB043>
                                                                        <NB043>
           IF  LIFERNL-STATUZ NOT = O-K                                 <NB043>
           AND LIFERNL-STATUZ NOT = ENDP                                <NB043>
              MOVE LIFERNL-PARAMS     TO SYSR-PARAMS                    <NB043>
              MOVE LIFERNL-STATUZ     TO SYSR-STATUZ                    <NB043>
              PERFORM 600-FATAL-ERROR                                   <NB043>
           END-IF.                                                      <NB043>
                                                                        <NB043>
           IF LIFERNL-CHDRCOY      NOT = CHDRLNB-CHDRCOY                <NB043>
           OR LIFERNL-CHDRNUM      NOT = CHDRLNB-CHDRNUM                <NB043>
           OR LIFERNL-STATUZ           = ENDP                           <NB043>
              GO TO B1390-EXIT                                          <NB043>
           END-IF.                                                      <NB043>
                                                                        <NB043>
           MOVE LIFERNL-OCCUP         TO WSAA-OCCUP.                    <NB043>
           IF WSAA-OCCUP5                                               <NB043>
              PERFORM B1400-CHECK-RIDER                                 <NB043>
           END-IF.                                                      <NB043>
                                                                        <NB043>
       B1380-NEXTR.                                                     <NB043>
           MOVE NEXTR                  TO LIFERNL-FUNCTION.             <NB043>
           GO TO B1320-CALL.                                            <NB043>
      *                                                                 <NB043>
       B1390-EXIT.                                                      <NB043>
             EXIT.                                                      <NB043>
      /                                                                 <NB043>
       B1400-CHECK-RIDER SECTION.                                       <NB043>
      ****************************                                      <NB043>
       B1410-START.                                                     <NB043>
      *                                                                 <NB043>
           MOVE SPACES                 TO COVTRBN-PARAMS.               <NB043>
           MOVE CHDRLNB-CHDRCOY        TO COVTRBN-CHDRCOY.              <NB043>
           MOVE CHDRLNB-CHDRNUM        TO COVTRBN-CHDRNUM.              <NB043>
           MOVE LIFERNL-LIFE           TO COVTRBN-LIFE.                 <NB043>
           MOVE ZERO                   TO COVTRBN-SEQNBR.               <NB043>
           MOVE BEGN                   TO COVTRBN-FUNCTION.             <NB043>
                                                                        <NB043>
       B1420-CALL.                                                      <NB043>
           CALL 'COVTRBNIO'        USING  COVTRBN-PARAMS.               <NB043>
                                                                        <NB043>
           IF COVTRBN-STATUZ           NOT = O-K AND                    <NB043>
                                       NOT = ENDP                       <NB043>
              MOVE COVTRBN-PARAMS      TO SYSR-PARAMS                   <NB043>
              PERFORM 600-FATAL-ERROR.                                  <NB043>
                                                                        <NB043>
           IF CHDRLNB-CHDRCOY       NOT = COVTRBN-CHDRCOY               <NB043>
           OR CHDRLNB-CHDRNUM       NOT = COVTRBN-CHDRNUM               <NB043>
           OR LIFERNL-LIFE          NOT = COVTRBN-LIFE                  <NB043>
           OR COVTRBN-STATUZ            = ENDP                          <NB043>
              GO TO B1490-EXIT.                                         <NB043>
                                                                        <NB043>
           IF COVTRBN-LIFE          = '01' AND                          <NB043>
              COVTRBN-COVERAGE      = '01'                              <NB043>
              GO TO B1480-NEXTR                                         <NB043>
           ELSE                                                         <NB043>
              MOVE COVTRBN-LIFE     TO S6378-LIFE                       <NB043>
              MOVE COVTRBN-JLIFE    TO S6378-JLIFE                      <NB043>
              MOVE COVTRBN-COVERAGE TO S6378-COVERAGE                   <NB043>
              MOVE COVTRBN-RIDER    TO S6378-RIDER                      <NB043>
              MOVE 0                TO S6378-PAYRSEQNO                  <NB043>
              MOVE SPACES           TO ERMS-ERRMESG-REC                 <NB043>
              MOVE ZER1             TO ERMS-EROR                        <NB043>
              MOVE SPACES           TO WSAA-EXTRA-MSGPFX                <NB043>
              PERFORM 1800-ERROR-MESSAGES                               <NB043>
           END-IF.                                                      <NB043>
      *                                                                 <NB043>
       B1480-NEXTR.                                                     <NB043>
           MOVE NEXTR                  TO COVTRBN-FUNCTION.             <NB043>
           GO TO B1420-CALL.                                            <NB043>
      *                                                                 <NB043>
       B1490-EXIT.                                                      <NB043>
             EXIT.                                                      <NB043>
      /                                                                 <NB043>
