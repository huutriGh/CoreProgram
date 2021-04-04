       IDENTIFICATION DIVISION.
       PROGRAM-ID.  P5074AT.
       AUTHOR.
      *
      *
      * Copyright 1986-2020, Computer Sciences Corporation.
      *
      *REMARKS.
      *                AT MODULE FOR LIFE ISSUE
      *
      * This AT  module  converts  the  proposal  into  an  in  force
      * contract.  The contract number is passed in the AT parameters
      * area  as  the  "primary  key". The total amount of cash to be
      * applied to  the  contract from contract suspense is passed as
      * the "transaction area".
      *
      *PREPARATION
      *
      * Read the contract header.
      *
      * Call DATCON1 and store today's date (TDAY).
      *
      * Read the Transaction Status Codes table entry (T5679) for the
      * current transaction code. The record status codes and premium
      * status codes are required for all record updates.
      *
      *CONTRACT HEADER LEVEL
      *
      * The  roles for all  the  contract  level  client  fields  are
      * checked  and  updated  if required.  Role records will not
      * exist, so for each role on the client header write a role
      * record.
      * Check the following roles:
      *
      *           Dispatch Address, if  blank  on  CHDR,  set up role
      *                using Owner,
      *           Payer, if blank on CHDR, set up role using Owner,
      *           Assignee,  if not blank write a role record for
      *                      assignee.
      *
      * Write the beneficiary roles as follows:
      *
      *      Read the beneficiary file (BNFYLNB) for contract company
      *           and contract number, writing a role (CLRR) for each
      *           record read.
      *
      * Check  the  commission details (PCDDLNB) for the contract. If
      * there  are no records set up, write one with all details from
      * the contract header except:
      *           - agent number from servicing agent on the contract
      *                header,
      *           - percentage commission split = 100%,
      *           - percentage bonus split = 100%.
      *
      *LIVES ASSURED
      *
      * Read all lives (LIFELNB)  for  the contract. Update each life
      * as follows:
      *           Valid flag to '1'
      *           Transaction number from contract header
      *           Status code to life status from T5679 read above
      *
      *COMPONENTS - CALCULATE SUMMARISED POLICIES
      *
      * If plan processing is  required  (no-policies  in  plan > 1),
      * read all component transaction  records  for  the coverage to
      * work out the number of components to be maintained in summary
      * format. For each  coverage/rider,  one  or  more  transaction
      * records will have been  written. Each transaction will have a
      * "no-policies-applicable"    field.    Store    the   SMALLEST
      * no-policies-applicable out of  the FIRST transaction for each
      * coverage/rider (i.e.  ignore  all  transaction records, other
      * than the first one.)
      *
      * This smallest number is the  number  of policies to be stored
      * in the single coverage/rider  records  written  with  a  plan
      * suffix  of zero. (Non-plan  policies  will  have  their  only
      * policy coverage/rider records written  with  a plan suffix of
      * zero.)  All  other  policies   within   the  plan  will  have
      * individual coverage/rider  records written with non-zero plan
      * suffixes. If this number is  1,  there  will  be  no  records
      * written in summary format. If  this is the same as the number
      * of  policies in the plan,  only  a  summary  record  will  be
      * written.
      *
      * Before  they  are  forgotten, store this "smallest number" on
      * the  contract  header  as  the no-summarised-policies and the
      * next-available-suffix  and  re-write the record.  (The record
      * will be actually  updated  again  later,  but it is needed in
      * this state by called subroutines).
      *
      *COMPONENTS - CREATE COVERAGE/RIDER INFORMATION
      *
      * Coverage/rider  records are  written  and  processed  in  one
      * standard way. This way  applies  to both a summary/individual
      * policy components and  to  variation  policy  components. The
      * generation of component information  is driven by reading the
      * coverage/rider  transaction  (COVTLNB)  records  sequentially
      * (again).  Once each record is finished with, delete it.
      *
      * The following cases are possible when creating coverage/rider
      * information (COVRLNB):
      *
      *      A) Plan processing is  not  applicable  or  only summary
      *           records  are  required.  Write  one  coverage/rider
      *           record for each  transaction  record  read  with  a
      *           suffix of zero.
      *
      *      B) Plan processing is applicable, but no summary records
      *           are to be  written.  For  each  transaction record,
      *           divide the amounts  (e.g.  sum insured, premium and
      *           lump sum) by no. of policies applicable. Write  one
      *           coverage/rider record  for  each policy. Start with
      *           suffix number 1 and add one until a transaction for
      *           a  different   component   is   read  (coverage-no,
      *           rider-no different).
      *
      *      C) Plan  processing  is   applicable  and  some  of  the
      *           policies  are  to  be  summarised.  For  the  first
      *           transaction for a  component  (a set of transaction
      *           records with the  same  coverage  and  rider number
      *           apply  to the one  component),  if  the  number  of
      *           policies which apply  to the transaction is greater
      *           than  the   "SMALLEST"   number  calculated  above,
      *           generate  a  summary  record  for  this  "SMALLEST"
      *           no. of policies. Apportion the premium, sum assured
      *           and lump sum by dividing them  by  no. of  policies
      *           applicable to  transaction  and  multiplying by the
      *           number to be summarised.  Write  this record with a
      *           plan suffix of zero. Then write one record for each
      *           policy remaining  unaccounted  for  on the existing
      *           transaction record, and  for  each  policy  on  any
      *           other transaction  records  (for  this  component).
      *           Calculate  the  first   suffix  as  the  number  of
      *           policies summarised plus  one.  Then  add  one each
      *           time as in method  (B).  If  the number of policies
      *           applicable  is  the  same   as  the  number  to  be
      *           summarised, write a summary coverage/rider for this
      *           transaction and individual policy ones as above for
      *           the subsequent transactions.
      *
      * Before the coverage records are written calculate the BENEFIT
      * BILLING.
      *      1) If the  benefit  billing  method (on T5687) is blank,
      *           accumulate  total  premium  amount  for  accounting
      *           later.   Accumulate   single  premium  and  regular
      *           premium  separately.  Single  and  Regular  premium
      *           ammounts are held separately on COVT/COVR. Both may
      *           be present on one  COVT/COVR if the  component edit
      *           rules allow. If it is a regular premium, it must be
      *           adjusted  for  the  period  which is actually being
      *           paid for. This is done by calling DATCON3 with  the
      *           contract  commencement  date, billing  commencement
      *           date and frequency. Multiply  the  COVRLNB  premium
      *           amount by the factor returned to  give  the  actual
      *           premium amount to be accounted for. Accumulate both
      *           the adjusted and non-adjusted amounts.
      *
      *      2) If the  benefit  billing method is not blank, look-up
      *           the benefit billing subroutine (T5534) and call it.
      *           Linkage as follows:
      *           - Company
      *           - Contract number
      *           - Life number
      *           - Coverage number
      *           - Rider number
      *           - Plan suffix
      *           - Contract type
      *           - Contract currency
      *           - Batch key
      *           - Client type
      *           - Client currency
      *           - Status
      *
      *      (The copybook UBBLALLPAR contains the above fields.)
      *
      * Access the general  coverage  rider details (T5687) effective
      * at the risk commencement  date  for  the coverage/rider code.
      * Initialise coverage/rider records as follows:
      *           All fields from COVTLNB (NB amounts)
      *           Current from and current to from contract header
      *           Transaction number from contract header
      *           Valid flag to '1'
      *           Status code to coverage/rider status from T5679
      *           Premium status  code  to coverage/rider status from
      *                T5679 (regular premium if billing frequency on
      *                contract header is not '00' and single premium
      *                indicator  on  T5687  is  not  'Y',  otherwise
      *                single premium)
      *           Coverage/rider  commencement  date from "effective"
      *                date
      *           Statistical and reporting codes from T5687
      *           Update the optional extras records (LEXT), and set
      *           the benefit billing date,rerate and CPI dates.
      *           All other numerics and dates to zero
      *
      * The COVR-CPI-DATE is calculated as follows:-
      *
      * The INCTPF option field, updated during  proposal. This will
      * be  driven  from  Risk  Commencement  Date using a Frequency
      * Factor stored on  T6658.  T6658 is keyed  on the Anniversary
      * Method from T5687.
      *
      * A  check  on  'Minimum Term to Cessation'  and 'Maximum Age'
      * must  be  made on T6658. If the contract does  not pass  the
      * test for Minimum Term or Maximum Age then set  COVR-CPI-DATE
      * to HI-DATE.
      *
      * If a LEXT record exists AND T6654 'Add to Existing Component'
      * is equal to spaces then set COVR-CPI-DATE to HI-DATE.
      *
      * Delete INCTPF record.
      *
      * If this is a flexible premium contract & the installment premium
      * not = 0 then write a FPCO record with information such as
      * target premium, minimum overdue, maximum overdue.
      *
      * After each COVRLNB record is written:
      * If the benefit billing method is not blank, look-up
      *          the benefit billing subroutine (T5534) and call it.
      *          Linkage as follows :
      *          - Company
      *          - Contract number
      *          - Life number
      *          - Coverage number
      *          - Rider number
      *          - Plan suffix
      *          - Contract type
      *          - Contract currency
      *          - Batch key
      *          - Client type
      *          - Client currency
      *          - Status
      *
      * AFTER EACH COVRLNB record is written:
      *
      *      3) If the stamp duty method is not blank, call the stamp
      *           duty   calculation   subroutine   (held  on  T5676)
      *           obtained  for  the method on T5687.  Accumulate the
      *           amount for accounting later. Linkage as follows:
      *           - Company
      *           - Contract number
      *           - Life number
      *           - Coverage number
      *           - Rider number
      *           - Policy suffix
      *           - Stamp duty amount due
      *           - Effective date (contract commencement date)
      *           - contract currency code
      *           - Status
      *
      *      (The copybook STDTALLREC contains the above fields.)
      *
      *      4) Look  up   the   coverage/rider   generic   component
      *           processing   programs    (T5671   -   accessed   by
      *           transaction number concatenated with coverage/rider
      *           code). Call  each non-blank subroutine, passing the
      *           standard linkage:
      *           - Company
      *           - Contract number
      *           - Life number
      *           - Coverage number
      *           - Rider number
      *           - Policy suffix
      *           - Premium amount paid/applicable (as calculated for
      *                accumulation  in  (1)  above,  single  premium
      *                amount/adjusted regular amount)
      *           - Batch key
      *           - Status
      *
      *      (The copybook ISUALLREC contains the above fields.)
      *
      *      For each component, it will call ACTVRES to activate the
      *      reassurance cessions (RACD) for the component and then
      *      update the reassurance history. ACTVRES will call REXPUPD
      *      to update the amount retained by the company, reassured
      *      by treaty and reassured facultatively.
      *
      *      5) COMMISSION  CALCULATION  -  The  following  steps are
      *           required  for  each  commission  agent  (read  from
      *           PCDDLNB):
      *           Work through 5a) to 5f) using the Regular Premium
      *           amount, if non-zero. T5687 now holds Commission methods
      *           for Regular, Single and Top Ups, so use those applicable
      *           for Regular first.
      *           Next trundle through 5a) to 5c) if the Single Premium
      *           amount is non-zero using the appropriate method from
      *           T5687 for Single premiums.
      *
      *      5a) If  the  initial commission calculation method (from
      *           T5687  looked-up  above)  is not blank, look up the
      *           subroutine  required  (T5647).  For each commission
      *           split  record  set  up  for the contract (PCDDLNB),
      *           call the commission calculation subroutine passing:
      *           - Company
      *           - Contract number
      *           - Life number
      *           - Coverage number
      *           - Rider number
      *           - Plan suffix
      *           - Agent number (from PCDDLNB)
      *           - Joint life number
      *           - Coverage/rider code (CRTABLE)
      *           - Commission method code
      *           - Proportion  of  annualised premium (calculated as
      *                COVRLNB  premium  amount * payment frequency *
      *                split%)
      *           - Premium amount paid/applicable (as calculated for
      *                accumulation in (1) above)
      *           - Total initial commission = 0
      *           - Paid initial commission = 0
      *           - Earned initial commission = 0
      *           - Payment amount = 0
      *           - Earned amount = 0
      *           - Effective date = coverage/rider effective date
      *           - Status
      *
      *      (The copybook COMLINKREC contains the above fields.)
      *
      *      5b) If  the initial commission payment method from T5687
      *           above is not blank, look up the subroutine required
      *           from  T5644.  For  each  commission agent (as above
      *           from PCDDLNB), if the commission payment method was
      *           not blank,  call  it with the standard linkage area
      *           as passed back from the last subroutine. Otherwise,
      *           if the  method was blank, look up the agent details
      *           (AGLFLNB) and use the initial payment method stored
      *           there in the  same  way as if it was entered on the
      *           table.
      *
      *      5c) Accumulate  the  commission  amounts  for accounting
      *           later.  Keep  one set of totals per agent (there is
      *           a  maximum  of  10  commission  agents allowed on a
      *           contract), per subsidiary account type:
      *           - Initial commission due
      *           - Initial commission earned
      *           - Initial commission paid in advance
      *
      *      5d) If  the  servicing  commission  payment  method from
      *           T5687  above  is  not  blank  (otherwise  check the
      *           agent details as above for the method), look up the
      *           subroutine required from T5644 and call it with the
      *           standard  linkage  area  re-initialised  as in (5a)
      *           above,  and  accumulate  the  amounts  returned for
      *           servicing    commission   accounting   later   (per
      *           agent/sub-a/c):
      *           - Servicing commission due
      *           - Servicing commission earned
      *
      *      5e) For renewal commission payment method, use the method
      *          defined in the agent details (AGNT). If this method is
      *          set to spaces, then use the method defined on table
      *          T5687.  Look up the subroutine required from T5644
      *          and call it with the standard linkage area
      *          re-initialised as in (5A) above and accumulated  for
      *          the amounts returned for renewal commission accounting
      *          later. ( Per agent/sub-a/c):
      *           - Renewal commission due
      *           - Renewal commission earned
      *
      *      5f) For  each  commission agent (as above from PCDDLNB),
      *           write an AGCM record as follows:
      *           - Current from, current  to and transaction no from
      *                contract header
      *           - Valid flag to '1'
      *           - "Key" details from coverage/rider record
      *           - Annualised premium as calculated above
      *           - Initial  commission amounts (total, paid, earned)
      *                as returned from steps (5a/b) above
      *           - Effective date as above
      *
      *
      *      6) Keep  track  of  the  oldest  premium  cessation date
      *           written.  These dates will define the contract risk
      *           cessation and premium cessation dates.
      *
      * REASSURANCE
      *
      * For each Coverage/Rider written, if there are any associated
      *   Reassurance records, ( found by using the RACTLNB logical ),
      *   then Read-lock, update Tranno field, set validflag from
      *   '3' to '1' (ie. In Force) and Rewrite each one of them.
      *
      *CONTRACT HEADER
      *
      * Look-up the contract  definition  details  (T5688)  using the
      * contract type code, effective  at  risk commencement date. If
      * the  contract fee method is not blank, look-up the subroutine
      * required from T5674 and call it with the following linkage:
      *           - Company
      *           - Contract fee
      *           - Status
      *
      *      (The copybook MGFEELREC contains the above fields.)
      *
      * Update the contract header itself as follows:
      *
      *           Valid flag to '1'
      *           Contract  commencement  date  to  original contract
      *                commencement date
      *           Contract status to status on T5679
      *           Contract status date to today
      *           Contract   status   transaction   no   to  contract
      *                transaction number
      *           Premium status code  to  contract status from T5679
      *                (regular  premium   if  billing  frequency  on
      *                contract header  is  not  00, otherwise single
      *                premium)
      *           Premium status date to today
      *           Premium   status   transaction   no   to   contract
      *                transaction number
      *           Transaction    number   last   used   to   contract
      *                transaction number
      *           If the  total  of  the single premiums and adjusted
      *                regular  premiums  (ie  there was some sort of
      *                payment made):
      *           - current   instalment   from   date   to  original
      *                commencement date
      *           - current    instalment    to   date   to   billing
      *                commencement date
      *           - current total instalment amounts 01-06;  01=total
      *                single  premium plus adjusted regular premiums
      *                accumulated  above,  02=contract  fee  amount;
      *                03=tolerance  amount  calculated as 01+02 less
      *                the  amount passed in the AT transaction area;
      *                04, 05=0;  06=total(01 to 05)
      *           If the billing frequency is 00:
      *           - paid to date, billed to date to premium cessation
      *                date worked out above
      *           If the billing frequency is not 00:
      *           - paid   to  date,  billed  to  date  from  billing
      *                commencement date
      *           - instalment  billing  commencement date to billing
      *                commencement date
      *           - instalment  billing  to date to premium cessation
      *                date worked out above
      *           - billing  day  to day part of billing commencement
      *                date
      *           - billing   month   to   month   part   of  billing
      *                commencement date
      *           - instalment   amounts   01-06;   01=total  regular
      *                premiums  accumulated  above,  02=contract fee
      *                amount; 03, 04, 05=0; 06=total(01 to 05)
      *           - The stdg. inst to date (SINSTTO) is no longer a
      *                Life requirement so set to Max-date.
      *
      *CONTRACT ACCOUNTING
      *
      * The total premium and  stamp  duty  amounts  will  have  been
      * accumulated.  The  contract  fee  amount  will also have been
      * worked out.
      *
      * Read the transaction accounting rules from T5645. The meaning
      * of each rule is as follows:
      *           01 - Contract suspense
      *           02 - Single Premium
      *           03 - Regular Premium
      *           04 - Contract fee
      *           05 - Tolerance write off
      *           06 - Stamp duty
      *           07 - Stamp duty payable
      *           08 - Initial commission due
      *           09 - Initial commission earned
      *           10 - Initial commission paid in advance
      *           11 - Servicing commission due
      *           12 - Servicing commission earned
      *           13 - Renewal commission due
      *           14 - Renewal commission earned
      *********** 15 - Reinsurance ??????????????
      *********** 16 - Reinsurance ??????????????
      *           15 - Override commission due
      * Seq 01... 01 - Override commission earned
      *
      * Read  the  description  of  this table entry as well. This is
      * used   as   a  narrative  description  on  all  the  postings
      * generated.
      *
      * If  the amount passed in the AT transaction area is not zero,
      * call LIFRTRN ("cash" posting routine) to post to the contract
      * suspense  account.  The posting required is defined in "line"
      * 01 on the  T5645  entry.  Set up and pass the linkage area as
      * follows:
      *           Function - PSTW
      *           Batch key - from AT linkage
      *           Document number - contract number
      *           Sequence  number  -  add 1 to the previous sequence
      *                number each time (originally 0)
      *           Sub-account  code  and  type,  GL  map, GL sign and
      *                control  total  number - from applicable T5645
      *                entry
      *           Company codes (sub-ledger and GL) - batch company
      *           Subsidiary account - contract number
      *           Original currency code - contract currency
      *           Original  currency  amount  -  as  passed in the AT
      *                transaction area
      *           Accounting currency code - blank (it will be looked
      *                up by the subroutine)
      *           Accounting  currency  amount  -  zero  (it  will be
      *                calculated by the subroutine)
      *           Exchange  rate - zero (it will be calculated by the
      *                subroutine)
      *           Transaction reference - contract transaction number
      *           Transaction description  -  from  transaction  code
      *                description
      *           Posting  month  and  year  -  blank  (they  will be
      *                defaulted)
      *           Effective date - contract commencement date
      *           Reconciliation amount - zero
      *           Reconciled date - Max-Date
      *           Transaction ID - from AT linkage
      *           Substitution code 1 - contract type
      *
      * Call LIFACMV  ("non-cash"  posting routine) six times to post
      * to the premiums  (02  and 03), fees (04), tolerance write off
      * (05), stamp duty (06) and stamp duty payable (07).  (Skip the
      * call if the  appropriate amount is zero.) Set up and pass the
      * linkage area as follows:
      *           Function - PSTW
      *           Batch key - from AT linkage
      *           Document number - contract number
      *           Sequence  number  -  add 1 to the previous sequence
      *                number each time
      *           Sub-account  code  and  type,  GL  map, GL sign and
      *                control  total  number - from applicable T5645
      *                entry
      *           Company codes (sub-ledger and GL) - batch company
      *           Subsidiary account - contract number
      *           Original currency code - contract currency
      *           Original  currency amount - 02=total single premium
      *                accumulated   from   each   component   above,
      *                03=total  adjusted regular premium accumulated
      *                from  each  component  above,  04=contract fee
      *                returned    from    the    subroutine   above,
      *                05=02+03+04-01,  06  and  07=total  stamp duty
      *                accumulated from each component above
      *           Accounting currency code - blank (it will be looked
      *                up by the subroutine)
      *           Accounting  currency  amount  -  zero  (it  will be
      *                calculated by the subroutine)
      *           Exchange  rate - zero (it will be calculated by the
      *                subroutine)
      *           Transaction reference - contract transaction number
      *           Transaction description  -  from  transaction  code
      *                description
      *           Posting  month  and  year  -  blank  (they  will be
      *                defaulted)
      *           Effective date - contract commencement date
      *           Reconciliation amount - zero
      *           Reconciled date - Max-Date
      *           Transaction ID - from AT linkage
      *           Substitution code 1 - contract type
      *
      *COMMISSION ACCOUNTING
      *
      * Call  LIFACMV  ("non-cash" posting routine) up to seven times
      * for  each  commission  agent.  These  amounts  will have been
      * accumulated  during the commission calculations done earlier.
      * These  amounts  are  (just  in case you haven't worked it out
      * yet):
      *           08 - Initial commission due
      *           09 - Initial commission earned
      *           10 - Initial commission paid in advance
      *           11 - Servicing commission due
      *           12 - Servicing commission earned
      *           13 - Renewal commission due
      *           14 - Renewal commission earned
      *           15 - Override commission due
      * Seq 01... 01 - Override commission earned
      *
      * Skip the call  if  the appropriate amount is zero. Set up and
      * pass the linkage area as follows:
      *           Function - PSTW
      *           Batch key - from AT linkage
      *           Document number - contract number
      *           Sequence  number  -  add 1 to the previous sequence
      *                number each time
      *           Sub-account  code  and  type,  GL  map, GL sign and
      *                control  total  number - from applicable T5645
      *                entry
      *           Company codes (sub-ledger and GL) - batch company
      *           Subsidiary account - commission agent number
      *           Original currency code - contract currency
      *           Original   currency   amount  -  applicable  amount
      *                accumulated for the "line" being posted
      *           Accounting currency code - blank (it will be looked
      *                up by the subroutine)
      *           Accounting  currency  amount  -  zero  (it  will be
      *                calculated by the subroutine)
      *           Exchange  rate - zero (it will be calculated by the
      *                subroutine)
      *           Transaction reference - contract transaction number
      *           Transaction description  -  from  transaction  code
      *                description
      *           Posting  month  and  year  -  blank  (they  will be
      *                defaulted)
      *           Effective date - contract commencement date
      *           Reconciliation amount - zero
      *           Reconciled date - Max-Date
      *           Transaction ID - from AT linkage
      *           Substitution code 1 - contract type
      *
      *GENERAL HOUSE-KEEPING
      *
      * 1) Write transaction PTRN record:
      *           - contract key from contract header,
      *           - transaction number from contract header,
      *           - transaction effective date to today,
      *           - batch key information from AT linkage.
      *
      * 2)  update the batch header by calling BATCUP with a function
      * of WRITS and the following parameters:
      *           - transaction count = 1,
      *           - all other amounts zero,
      *           - batch key from WSSP.
      *
      * 3)  Call  policy  schedule  printing routine? (table driven?)
      * This  will  be  added  when  the  plan  schedule  routine  is
      * specified (shortly).
      *
      * 4) Release contract soft lock.
      *
      * A000-STATISTICS SECTION.
      * ~~~~~~~~~~~~~~~~~~~~~~~
      * AGENT/GOVERNMENT STATISTICS TRANSACTION SUBROUTINE.
      *
      * LIFSTTR
      * ~~~~~~~
      * This subroutine has two basic functions;
      * a) To reverse Statistical movement records.
      * b) To create  Statistical movement records.
      *
      * The STTR file will hold all the relevant details of
      * selected transactions which affect a contract and have
      * to have a statistical record written for it.
      *
      * Two new coverage logicals have been created for the
      * Statistical subsystem. These are COVRSTS and COVRSTA.
      * COVRSTS allows only validflag 1 records, and COVRSTA
      * allows only validflag 2 records.
      * Another logical AGCMSTS has been set up for the agent's
      * commission records. This allows only validflag 1 records
      * to be read.
      * The other logical AGCMSTA set up for the old agent's
      * commission records, allows only validflag 2 records,
      * dormant flag 'Y' to be read.
      *
      * This subroutine will be included in various ATs
      * which affect contracts/coverages. It is designed to
      * track a policy through its life and report on any
      * changes to it. The statistical records produced form
      * the basis for the Agent Statistical subsystem.
      *
      * The three tables used for Statistics are;
      *     T6627, T6628, T6629.
      * T6628 has as its item name the transaction code and
      * the contract status of a contract, e.g. T642IF. This
      * table is read first to see if any statistical (STTR)
      * records are required to be written. If not the program
      * will finish without any processing needed. If the table
      * has valid statistical categories, then these are used
      * to access T6629. An item name for T6629 could be 'IF'.
      * On T6629, there are two question fields, which state
      * if agent and/or government details need accumulating.
      *
      * The four fields under each question are used to access
      * T6627. These fields refer to Age, Sum assured, Risk term
      * and Premium. T6627 has items which are used to check the
      * value of these fields, eg. the Age of the policy holder
      * is 35. On T6627, the Age item has several values on
      * To and From fields. These could be 0 - 20, 21 - 40 etc.
      * The Age will be found to be within a certain band, and
      * the band label for that range, eg AB, will be moved to
      * the STTR record. The same principal applies for the
      * other T6629 fields.
      *
      * T6628 splits the statistical categories into four
      * areas, PREVIOUS, CURRENT, INCREASE and DECREASE.
      * All previous categories refer to COVRSTA records, and
      * current categories refer to COVRSTS records. AGSMSTS
      * records can refer to both. On the coverage logicals,
      * the INCREASE and DECREASE are for the premium, and
      * on the AGCMSTS, these refer to the commission.
      *
      * STTR records are written for each valid T6628 category.
      * So for a current record, there could be several current
      * categories, therefore the corresponding number of STTRs
      * will be written. Also, if the premium has increased or
      * decreased, a number of STTRs will be written. This process
      * will be repeated for all the AGCMSTS records attached to
      * that current record.
      *
      * When a reversal of STTRs is required, the linkage field
      * LIFS-TRANNOR has the transaction number, which the STTRs
      * have to be reversed back to. The LIFS-TRANNO has the
      * current tranno for a particular coverage. All STTR records
      * are reversed out up to and including the TRANNOR number.
      *
      ******************Enhancements for Life Asia 1.0****************
      *
      * Enhancement 1
      *
      * This enhancement to update the issue dates maintained on
      * Contract Additional Detail file HPAD.
      *
      * =============
      * Enhancement 2
      *
      * Referring to enhancement in P6378 where on issuing contracts,
      * suspense available in different currencies is checked to settle
      * the premium payable.  The enhancement here is to display
      * suspense and tolerance limits in collection currency rather
      * than always in contract currency.
      *
      ******************Enhancements for Life Asia 2.0****************
      *
      *  Update the new basic and loaded premium fields when writing        *
      *  the new COVR record.                                               *
      *
      *  Before calling the commission calculation routines, check          *
      *  the basic commission indicator on T5687. If it is set,             *
      *  use the basic premium, as opposed to the gross premium,            *
      *  to calculate commission.                                           *
      *                                                                     *
      *****************************************************
      *              AMENDMENT  HISTORY                   *
      ****************************************************************
      * DATE.....   BY..   AMENDMENT..........................  NUMBER
      *
      * DD/MM/YY    X.X.   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX    NNN
      *
      * 27/01/93    D.W.   VERSION 2. Amendments 001-066 Archived.  000
      *                    ---------------------------------------
      *                    This program is the post "November 1992
      *                    Release" program. Since it is a new
      *                    version, all previous amendments tags
      *                    have been removed and all redundant code
      *                    deleted.
      *
      * 24/11/92    J.B.   AQR 3103.                                067
      *                    Hard coded half-yearly freq factor
      *                    calcs in the 1600 section are now
      *                    omitted as DATCON3 now handles these.
      *
      * 07/12/92   M.F     SDF 2095                                 068
      *                    Comment out the change made by C.G. on
      *                    28/01/91 as DATCON3 has been amended to
      *                    cater for this problem.
      *
      * 10/12/92    M.P.   AQR 3249.                                069
      *
      *                    In the case of overriding agents, their
      *                    ceding agents were not being written to
      *                    the AGCM.
      *
      *                    Removed incorrect initialisation of
      *                    AGCM-CEDAGENT before writing out AGCM
      *                    for overriding agent (233E-WRITE-AGCM).
      *                    Use the Agent number from the contract
      *                    header as the Cede Agent.
      *                    The use of WSAA-AGCM-AGNTNUM was redund-
      *                    ant and so was removed.
      *
      * 27/01/93    D.W. - AQR 3763. Life Development L02-05/93.    070
      *                    When Issuing a Contract, we want to
      *                    process Reassurance RACT records as
      *                    well. ie. we want to convert all
      *                    validflag '3' records to In Force
      *                    validflag '1' RACT records. Use new
      *                    RACTLNB logical file to get RACT
      *                    records which require processing.
      *                    Remove all references to section
      *                    2700-... it contains no cobol source.
      *                    Initialise the ISUA-NEW-TRANNO field
      *                    to zeros. It is a new field in the
      *                    linkage and will only be used by
      *                    Component Modify Reversals.
      *             B.L. - Development changes for Automatic
      *                    Increases are recorded under the same
      *                    Tag no. If an INCT record is found for
      *                    a contract, calculate CPI-DATE as
      *                    appropriate.
      *                  - Set ISUA-EFFDATE to CRRCD.
      *                    The linkage now includes EFFDATE as it
      *                    is needed during Component Change.
      *
      * 16/06/93    D.W. - AQR 4535.                                071
      *                    Initialise the ISUA-FUNCTION field to
      *                    SPACES. This field is new to the
      *                    ISUALLREC and is not directly used by
      *                    this program, it just needs to be
      *                    initialised.
      *
      * 02/04/93    I.K. - AQR 4316.                                072
      *                    Update payor tranno inline with contract
      *                    header tranno.
      *
      * 10/06/93    J.E. - AQR 4570.                                073
      *                    Comment referencing programmer's name
      *                    physically removed. Reference to section
      *                    2830-... also removed as it is not used.
      *                    Redundant paragraph names also removed
      *                    to prevent informational messages at
      *                    compile time.
      *
      * 02/09/93   F.O'R.  AQR 4753.                                074
      *                    CHDR & PAYR premium status codes were
      *                    being incorrectly set to the single premium
      *                    staus codes from T5679 when an individual
      *                    component had the single payment flag on
      *                    on T5687 set to 'Y'.
      *                    This would only happen if the last coverage
      *                    processed in the 2300 section, had the flag
      *                    set to 'Y'.
      *
      * 15/09/93   C.MCK.  AQR 4763.                                075
      *                    Incorrect amounts being written for override
      *                    commission earned for non component level
      *                    accounting contracts.
      *
      * 06/04/94   V.H.    AQR 4933 Life Development L04 - 05/94.   076
      *                    Initialisation of new fields added to
      *                    the ISUALLREC: ISUA-OLDCOVR and
      *                    ISUA-OLDRIDER.
      *
      * 23/08/94    S.P    AQR 5383                                 077
      *                    Replace ',' with '.'.
      *                    The class in the AGCM file for
      *                    override agents was being picked up
      *                    from a working storage field which
      *                    contains the class of the agent
      *                    servicing the contract. The class
      *                    should be picked up from the AGLF
      *                    file after the record has been
      *                    been retrieved for each override
      *                    agent.
      *
      * 23/11/94    W.O    AQR 5517                                 078
      *                    Agent number increased to 8 characters.
      *
      * 27/01/95    PRH    AQR 5696                                 079
      *                    The unlock of the softlock on the contract
      *                    performed after all other processing
      *                    (specifically Stats.) is complete.
      *
      * 27/01/95    P.E.   AQR 5274                                 080
      *                    Amend the setting of the re-rate date
      *                    and the re-rate rate date to take into
      *                    account the minimum guarantee period.
      *                    If re-rate date =< min guarantee period
      *                    then use the OCCDATE as the re-rate rate
      *                    date, otherwise use the re-rate date.
      *                    Previously, the Minimum Guarantee Period was
      *                    being ignored.
      *
      * 14/12/94    M.A    AQR 5514.                                081
      *                    Letters routine added.
      *
      * 02/03/95    M.A    AQR 5514.                                082
      *                    COVT Benefit Term details upgrade.
      *
      * 10/04/95    M.A    AQR 5514.                                083
      *                    Bonus Application field added.
      *
      * 12/04/95    A.M.   AQR 5811.                                084
      *                    When a contract is created with split
      *                    commission and the commission is split
      *                    to two decimal places the calculation
      *                    is not done correctly.  The problem was
      *                    solved by creating WSAA-CLNK-ANNPREM of
      *                    PIC 9(13)V9999 and substituting it to
      *                    CLNK-ANNPREM in commission calculations.
      *
      * 10/05/95    CCD.   AQR 5958                                 085
      *                    Redundant line of code moving 'LINJO' to
      *                    CLNK-FUNCTION removed.
      *
      * 10/05/95    CCD.   AQR 5894                                 086
      *                    Problem when commission was going to more
      *                    than one overriding level e.g. A reports to
      *                    B who reports to C etc. If B was due 10%
      *                    overriding commission and then C was due
      *                    10% of that figure (= 1% of the original),
      *                    C would receive 10% of the initial
      *                    commission figure instead.
      *                    Running totals have been created which are
      *                    now the basis of any overriding commission
      *                    calculation beyond the first level (i.e. C
      *                    onwards). WSAA-OVRTIMES shows whether it
      *                    is the first level of O/R commission or
      *                    beyond that.
      *
      *****************************************************************
      *
      *  ........ New Version of the Amendment History
      *
      *****************************************************************
      * DATE....  VSN/MOD  WORK UNIT    BY....
      *
      * 06/11/95  01/01   A05691       Maria Murphy
      *           ISUALLREC Linkage area now has the field Language.
      *           Set this field in this program such that it may be
      *           available for future use in the Issue Subroutines if
      *           required.
      *
      * 03/01/96  01/01   A05940       Andrew Wall
      *           When writing an AGCM record for a single premium
      *           within the 233D (COMMISSION-AGENT) section, set the
      *           paid-to date on the AGCM to zero.  This will ensure
      *           that all single premium AGCMs (including single
      *           premiums on regular premium components) can be
      *           distinguished from regular premium AGCMs when
      *           performing reversals and premium decreases.
      *           Do the same for overrider commission.
      *
      *           Ensure that the field which stores the level of
      *           overriding commission is initialised before each
      *           perform of the overriding commission section.
      *
      * 04/01/96  01/01   D9604        Peter Evans
      *           Changes added to cater for Flexible Premiums.
      *           Contract types which exist on table T5729 are
      *           identified and an FPRM record written per payer.
      *           An FPCO record is also written.
      *           Also for FP contracts we do not want to pay any
      *           servicing (ie overtarget) commission at issue.
      *           Simply set up the method on the agcm file.
      *           This processing is in addition to the existing
      *           functionality and does not affect 'non-flexible
      *           premium' contracts.
      *
      * 07/05/96  01/01   R96REA       Fiona Martin                         *
      *           Call ACTVRES to activate reassurance                      *
      *           records.                                                  *
      *           All reference to old RACT reassurance                     *
      *           records has been removed.                                 *
      *                                                                     *
      * 14/05/96  01/01   D96NUM       Fiona Martin                         *
      *                   RAAMOUNT (RA-AMOUNT)
      *                   DPAMT (DEFER-PERD-AMT)
      *                   TMBEN (TOT-MTHLY-BENEFIT)
      *                   STSMIN (STAT-SUMINS)
      *                   VARSI  (VAR-SUM-INSURED)
      *                   CRINST (CR-INSTAMT)
      *                   SUMINS
      *                   EMV    (EST-MAT-VALUE)
      *                   EMVINT (EST-MAT-INT)
      *                   INSTPREM
      *                   SINGP
      *                   CRDEBT (COVERAGE-DEBT)
      *                   INITCOM
      *                   COMERN
      *                   COMPAY
      *                   RNLCEARN
      *                   RNLCDUE
      *                   SCMEARN
      *                   SCMDUE
      *                   ANNPREM
      *                   WSAA-CALC-TOLERANCE ..... No longer used.
      *                   ORIGAMT
      *                   ACCTAMT
      *                   + Numerous working storage fields.....
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
      *
      * 09/07/96    CAS1.0        Dominic Dunbar.
      *                   Initialise UBBL-SINGP.
      *
      *                   Change Overide Processing to call the new
      *                   subroutine.
      *
      * 22/07/96  01/01   CAS1.0       Rob Cooper
      *                   Remove reference to T5694 - not used.
      *
      * 01/08/96  01/01   CAS1.0       Kristin McLeish
      *                   Wrong premium status from T5679 being used
      *                   for single premium riders (was always using
      *                   single premium status for main coverage).
      *
      * 08/08/96  01/01   CAS1.0       Cat Chiu
      *                   Update the HPAD file when issuing the
      *                   contract.
      *
      * 12/08/96  01/01   CAS1.0       Cat Chiu
      *                   When determining if a tolerance allowance
      *                   is required, use the Suspense payment
      *                   currency as this may differ from the
      *                   Contract currency.
      *                   Where the Suspense and Contract currencies
      *                   differ, update the GL to include currency
      *                   conversion details.
      *
      * 05/02/97  01/01   SPLPRM       Dominic Dunbar                       *
      *           Update the new basic and loaded premium fields            *
      *           when writing the new COVR record.                         *
      *           Before calling the commission calculation routines,       *
      *           check basic commission indicator on T5687. If it          *
      *           is set, use basic premium, not gross premium, to          *
      *           calculate commission.                                     *
      *                                                                     *
      * 25/04/97  01/01   POLSCD       Dominic Dunbar                       *
      *           Set up LETOKEYS with contract no, language & eff date     *
      *                                                                     *
      * 19/08/97  01/01   LA2110       Tony Tang - CSC Hong Kong            *
      *           Force a re-rate date to the Waiver of Premium with        *
      *           the earliest re-rate date or the earliest CPI date        *
      *           of the components it applies to.                          *
      *                                                                     *
      * 22/08/97  01/01   LA2113       Tony Tang - CSC Hong Kong            *
      *           Set PTRN-PTRNEFF to today's date.                         *
      *                                                                     *
      * 12/09/97  01/01   D9703        Tony Tang - CSC Hong Kong            *
      *           Set up new date fields added for the                      *
      *           Windforward after Reversal development.                   *
      *           Set the ISUA-RUN-DATE to COVR-CRRCD date.
      *                                                                     *
      * 20/10/97  01/01   LIF2.1       Chenli Su                            *
      *           Initialize LETRQST-RDOCPFX, -RDOCCOY & -RDOCNUM with      *
      *           CHDRLNB details before 'ADD' letter record (LETC) via     *
      *           'LETRQST' sub routine.                                    *
      *                                                                     *
      * 29/08/97  01/01   A06557       Margaret Gill                        *
      *                                                                     *
      *           Flexible Premium contracts - if the first billing         *
      *           date day/month is not equal to that of the anniversary    *
      *           day/month then the anniversary processing day/            *
      *           month is set to that of the billing day/month.            *
      *                                                                     *
      * 17/10/97  01/01   A06785       Margaret Gill                        *
      *                                                                     *
      *           Pass CRRCD to ACTVRES.                                    *
      *                                                                     *
      * 01/10/98  01/01   PCPRT        LEW (RETROFIT PC PRINTING/400)       *
      * 18/04/97  01/01   PCPRT        Christopher Scott                    *
      *           Changed initialisation of LETRQST fields.                 *
      *                                                                     *
      * 10/09/98  01/01   A06503       Graham Judd                          *
      *           When calculating Rerate-From-Date ensure that             *
      *           if there is a Premium-Guarantee-Period and                *
      *           Contract-Commencement-Date + Guarantee-Period             *
      *           is greater than the Rerate-Date then the                  *
      *           Rerate-From-Date is set to CCD + GP rather than           *
      *           just CCD.                                                 *
      *           This is calculated in 2324-RERATE-DATES.                  *
      *                                                                     *
      * 17/12/98  01/01   V4L011       Daniel Wong                          *
      *           Life Insurance Association Substd and Hospitalisn details *
      *                                                                     *
      * 23/12/98  01/01   N003         CSC - Sukalya                        *
      *           Changed call new subroutine HLETRQS                  ??   *
      *            instead of LETRQST                                  ??   *
      *                                                                     *
      * 23/12/98  01/01   V4L001       Balaji                               *
      *           Premium Deposit Retrofit                             ??   *
      *                                                                     *
      * 04/01/99  01/01   V4L014       Balaji . B                           *
      *           Override Commission Enhancement                           *
      *                                                                     *
      * 05/01/99  01/01   N003         CSC - Ali Hartono                    *
      *           Set Letter Other Key to language.                         *
      *                                                                     *
      * 03/03/99  01/01   TN01         CSC - Worachart                      *
      *           Amend reading of TR517 to access details on continuation  *
      *           screens.                                                  *
      *                                                                     *
      * 19/05/99  01/01   N003         CSC: Ali Hartono                     *
      *           Read T6634 again using generic key.                       *
      *                                                                     *
      * 12/11/99  01/01   V42005       Yong Kee Jee                         *
      *           Maintenance of a new enquiry file, UNDRPF                 *
      *                                                                     *
      * 18/11/99  01/01   V42004       Oi Leng                              *
      *           Commission rule at component level to follow main         *
      *           coverage.                                                 *
      *                                                                     *
      * 30/11/99  01/01   V42013       Oi Leng                              *
      *           Include checking for 2nd premium shortfall tolerance      *
      *           limit set in table T5667.                                 *
      *                                                                     *
      * 16/03/00  01/01   LFA1062      CSC - Ronald Macarulay               *
      *           To reflect the correct client for Life 1 and Life 2       *
      *           (taken from Pru Philippines)                              *
      *                                                                     *
      * 11/07/00  01/01   MLS001       Germaine                             *
      *           Move language to CLNK-LANGUAGE.                           *
      *                                                                     *
      * 23/10/00  01/01   V5L007       Chua Ming Jye                        *
      *           Init CLNK-PTDATE.                                         *
      *                                                                     *
      * 29/05/01  01/01   V6L000       HO CHIH YING                         *
      *           Splitting single premium commission from initial          *
      *           commission during posting                                 *
      *                                                                     *
      * 29/02/00  01/01   A07459       Andy Jerrom                          *
      * ------->  Retrofit td by Chua Ming Jye on 13/07/2001.               *
      *           Fix to stop posting's to ACMV when Override Commission    *
      *           earned is zero.                                           *
      *                                                                     *
      *                                                                     *
      * 17/08/01  01/01   PCPPRT       Lai AT    - CSC Malaysia             *
      *                   Table T6634 is changed to TR384.                  *
      *                                                                     *
      * 30/10/01  01/01   PCPPRT  DAVIDE MARASCHI      CSC SINGAPORE        *
      *                   Revert back to UK version by calling LETRQST      *
      *                   instead of HLETRQS.                               *
      *                                                                     *
      * 24/06/04  01/01   SMU002       Simon Wong - CSC HK                  *
      *                   Update program remarks for scribe manual.         *
      *                                                                     *
      * 16/07/04  01/01   LA3428       Nancie lin - FSG Taiwan              *
      *           If single premium and non-reducing term product,     ??   *
      *           setup the Rerate Date as the Risk Cessation Date.??????   *
      *           This is to avoid the re-rate transaction for the     ??   *
      *           single premium products.                             ??   *
      *                                                                     *
      * 14/10/04  01/01   DRY001       Diary Installation user              *
      *           Install DIARY/400 into LIFE/ASIA.                         *
      *                                                                     *
      * 26/11/04  01/01   LA3380       Fred Chow - CSC HK                   *
      *           Enhance error handling for RLLIADB.                  ??   *
      *                                                                     *
      * 13/04/05  01/01   DRYAPL       Joan Chiow - CSC Singapore           *
      *           Add a call to the diary processing subroutine.            *
      *                                                                     *
      * 27/09/05  01/01   V65L16       Wang Ge - CSC Singapore              *
      *           Set up value to the new linkage field UBBL-OCCDATE.       *
      *                                                                     *
      * 07/11/05  01/01   LA3875       Wang Ge - CSC Singapore              *
      *           Set up correct Re-rate date for WOP component.            *
      *                                                                     *
      * 15/02/06  01/01   DRYAP2       Wang Ge - CSC Singapore              *
      *           Set up DRYP-APLSUPTO & DRYP-STMDTE as well.               *
      *                                                                     *
      * 17/04/06  01/01   LA3948       Wang Ge - CSC Singapore              *
      *           WOP rerate date is incorrect when policy got more than    *
      *           one Life Insured.                                         *
      *                                                                     *
      * 07/06/06  01/01   LA3998       Wang Ge - CSC Singapore              *
      *           Not allow hard coding of LANGUAGE field.                  *
      *                                                                     *
      * 30/06/06  01/01   V70L01       Henry Wong Wai Hung                  *
      *           Write the premium & commission history records for        *
      *           Bonus Workbench Extraction.                               *
      *                                                                     *
      * 03/08/06  01/01   V70L01       Chen Xu - CSC Beijing                *
      *           Before write the premium & commission history records for *
      *           Bonus Workbench Extraction, check TH605 first,if TH605-   *
      *           BONUS-IND set to 'N',no need to update.                   *
      *                                                                     *
      * 10/10/06  01/01   V71L05       Sow Keng - CSC Singapore             *
      *           Policy Owner Change for Juvenile Policy.                  *
      *                                                                     *
      * 17/01/07  01/01   V71L12       Wang Ge/FSG/CSC (Singapore)          *
      *           Retrofit UDW001.                                          *
      * 26/11/03  01/01   UDW001       Lynne Dornan                         *
      *           Underwriting added.                                       *
      *           Update the Underwriting files to validflag '1'.           *
      *                  UNDL - Underwriting Life records                   *
      *                  UNDC - Underwriting Component records              *
      *                  UNDQ - Underwriting Question records               *
      *                                                                     *
      * 17/08/07  01/01   LA4238       Gang Liu/ASIA/CSC (Beijing)          *
      *           If Age sufficent date = 99999999, set 'Y' to the          *
      *           process flag (PROCFLG)                                    *
      *                                                                     *
      * 28/05/08  01/01   V73L01       Wang Ge/FSG/CSC (Singapore)          *
      *           To cater overriding commision calculation and posting     *
      *           immediately, when "Override based on Agent OR Details"    *
      *           is 'Y' on TH605.                                          *
      *                                                                     *
      * 14/08/08  01/01   LA4351       Wang Ge/FSG/CSC (Singapore)          *
      *           Change to use DATCON4, not DATCON2 to calculate the       *
      *           next Benefit Billing Date, to avoid the problem of        *
      *           that DATCON2 had with end months.                         *
      *                                                                     *
      * 19/09/08  01/01   V73L03       Xu Chen/ASIA/CSC (Beijing)           *
      *           Redefine WSAA-MIN-TRM-TO-CESS from S9(02) to S9(03).      *
      *                                                                     *
      * 01/11/08  01/01   V73L03       Gang Liu/ASIA/CSC (China)            *
      *           Change due to T6658REC changed.                           *
      *                                                                     *
      * 04/02/09  01/01   V73L05       Pin Wen Chin/FSG/CSC (Malaysia       *
      *           Include field ACVR-LANGUAGE.                              *
      *                                                                     *
      * 29/05/09  01/01   LA5139       Kah Choy Ng/ASIA/CSC (Malaysia       *
      *        1. Create 2 new variables to keep the PAYR-BILLDAY and       *
      *           PAYR-BILLMONTH field as below;                            *
      *           PAYR-BILLDAY   ==> WSAA-BILLDAY                           *
      *           PAYR-BILLMONTH ==> WSAA-BILLMONTH                         *
      *        2. To use the WSAA-BILLING-INFORMATION variable instead of   *
      *           using PAYR variable directly in section Bonus Workbenchh  *
      *                                                                     *
      * 03/06/09  01/01   V74L01       Jinkee Guerrero/ASIA/CSC (Malaysia)  *
      *           Modify program to include component and contract level    *
      *           accounting entries for Service tax and Education Cess Amts*
      *                                                                     *
      * 01/07/09  Retrofitted by Helen Cui                                  *
      *           01/01   A06484       Steve Hale                           *
      *           It is possible in how the tables can be set and the       *
      *           dates the user enters that the Premium Cessation date     *
      *           can be set to occur before the system defined Rerate      *
      *           date.  In this situation the Premium Cessation date       *
      *           should take precedence and therefore the rerate date      *
      *           should be set to the Prem Cess Date.                      *
      *                                                                     *
      * 01/07/09  Retrofitted by Helen Cui                                  *
      *           01/01   A06541       Warren Ornstein                      *
      *           Use contract num in RLDGACCT for tax relief posting.      *
      *                                                                     *
      * 02/07/09  01/01   V74L01       Edelino R Juarez/FSG/CSC (Sing       *
      *           Service Tax Enhancements.                                 *
      *             -Calculate tax on Premium amount and contract fee       *
      *                                                                     *
      * 21/07/09  01/01   V74L01       Ronald Otero/FSG/CSC (Singapor       *
      *           Rectify codes after review / testing
      *                                                                     *
      * 24/07/09  01/01   V74L03       Xu Chen/ASIA/CSC (China)             *
      *          -Update the policy dispatch details fields into contract   *
      *           additional details file(HPAD).                            *
      *          -Update the effective date on AGPY record based on         *
      *           whether or not commission to be released only upon        *
      *           receipt of policy acknowledgement slip defined in TH605.  *
      *                                                                     *
      * 13/08/09  01/01   V74F03       Ali Hartono/FSG/CSC (Singapore       *
      *           To skip creating client role for beneficiary and          *
      *           despatch address as they have been created by P5010       *
      *           P6225 respectively.                                       *
      *                                                                     *
      * 19/08/09 Retrofitted by Fred Lee (CSC Hong Kong)                    *
      *           01/01   LA4596       Saw Hoong Ong/FSG/CSC (Malaysia)     *
      *          -Update the value of Self Sufficient Date first before     *
      *           updating the Process Flag for HPAD                        *
      *                                                                     *
      * 26/11/09  01/01   V75F01       Saw Hoong Ong/FSG/CSC (Malaysia)     *
      *           Secured Data Access                                       *
      *           Call BLDENRL in order to create ENRL                      *
      *           Entity Relationship records.                              *
      *                                                                     *
      * 25/01/10  01/01   LA4754       Wang Ge/FSG/CSC (Singapore)          *
      *           Change maximum age from 99 to 999 for T6658.              *
      *                                                                     *
      * 09/09/10  01/01   V76F06       Nancie Lin/FSG/CSC (Hong Kong)       *
      *           Call ZRDECPLC to do rounding processing                   *
      *                                                                     *
      * 15/03/11  01/01   V76F06       Gang Liu/ASIA/CSC (China)            *
      *           Do rounding process after currency conversion             *
      *                                                                     *
      * 26/09/13  01/01   DUMMY        Khang Nguyen - CSC Developer         *
      *           RE-COMPILE ONLY                                           *
      *                                                                     *
      * 14/10/13  01/01   DUMMY        Israez Prieto - CSC Developer        *
      *           TEST                                                      *
      *                                                                     *
      * 19/12/13  01/01   PHL108       Thoai Anh - CSC Developer            *
      *           STAFF DISCOUNT                                            *
      *                                                                     *
      * 04/03/14  01/01   PHE001       Thanh Do                             *
      *           Check Staff Flag before booking Discount if any.          *
      *                                                                     *
      * 19/03/14  01/01   GAPPH2       Tuan Le                              *
      *           Check Staff Flag on Policy Owner.                         *
      *                                                                     *
      * 28/04/14  01/01   CR020B       Phung Vi Hao                         *
      *           Modify the PGM to not re-update the rereate date          *
      *           during update the ZDIS file
      *                                                                     *
      * 01/07/14  01/01   EN001        Tuan Le                              *
      *           Add DESPNUM to LETC for printing.                         *
      *                                                                     *
      * 04/11/14  01/01   PHE003       Phuong Le Dev                        *
      *           ADD NEW FIELD IN TR517                                    *
      *
      * 02/12/14  01/01   PHE003       Phuong Le Dev                        *
      *           USE ORIGINAL DATE                                         *
      *                                                                     *
      * 03/02/15  01/01   PHE003       Thanh Do                             *
      *           First Despatch Date is the Original Issued Date.          *
      *                                                                     *
      * 14/10/15  01/01   UL001        Tuan Le                              *
      *           EXCESS PREMIUM ALLOCATION                                 *
      *                                                                     *
      * 13/11/15  01/01   UL001        Thanh Do                             *
      *           Pass Contract Currency to parameter.                      *
      *                                                                     *
      * 07/12/15  01/01   UL001        Phi Tran - IT DEV                    *
      *           Recompile Only                                            *
      *                                                                     *
      * 14/07/17  01/01   UL003        Ha Nguyen - IT DEV                   *
      *           2P ALLOCATION.                                            *
      *                                                                     *
      * 10/08/17  01/01   UL003        Phi Tran - IT DEV                    *
      *           Call LETRQST at end of process.                           *
      *                                                                     *
      * 23/10/17  01/01   DA005        Thanh Do                             *
      *           Add Servicing Agent Code to Premium Recs in TRANREF.      *
      *                                                                     *
      * 18/12/17  01/01   DA006        Thanh Do                             *
      *           Save Billing Frequency when Policy issued.                *
      *                                                                     *
      * 16/01/19  01/01   PHFX39       Phi Tran - IT DEV                    *
      *           Enhance new rule for commission/Allocation Charge.        *
      *                                                                     *
      * 16/07/19  01/01   PS043        Vo Thi Hong Duyen - IT               *
      *           Do not clear ACK date.
      *                                                                     *
      * 07/09/20  01/01   DA024        Thanh Do                             *
      *           Get EA agent as Serv.Agent if any for DMS                 *
      *                                                                     *
      * 25/11/20  01/01   UL006        Phi Tran - IT DEV                    *
      *           Save Contract Information when issued Policy.             *
      *                                                                     *
      **DD/MM/YY*************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.                                AS-400.
       OBJECT-COMPUTER.                                AS-400.
      *
      *
       DATA DIVISION.
      /
       WORKING-STORAGE SECTION.
      *
       01  WSAA-PROG                   PIC X(08) VALUE 'P5074AT'.
       01  WSAA-VERSION                PIC X(02) VALUE '02'.            <070>
       01  WSAA-BATCKEY.
           COPY BATCKEY.
       01  WSAA-PRIMARY-KEY.
           03  WSAA-PRIMARY-CHDRNUM    PIC 9(8).
           03  FILLER                  PIC X(28).
       01  WSAA-TRANSACTION-REC.
      **** 03  WSAA-TOTAMNT            PIC S9(11)V99  COMP-3.           <D96NUM>
           03  WSAA-TOTAMNT            PIC S9(15)V99  COMP-3.           <D96NUM>
           03  WSAA-TOTAMNT1           PIC S9(15)V99  COMP-3.           <V4L001>
           03  WSAA-FSU-COY            PIC X.
           03  WSAA-TRANSACTION-DATE   PIC S9(06) COMP-3.
           03  WSAA-TRANSACTION-TIME   PIC S9(06) COMP-3.
           03  WSAA-USER               PIC S9(06) COMP-3.
           03  WSAA-TERMID             PIC X(04).
      **** 03  FILLER                  PIC X(176) VALUE SPACES.         <D96NUM>
           03  FILLER                  PIC X(174) VALUE SPACES.         <D96NUM>
       01  WSAA-NUMAPP                 PIC S9(04) COMP-3.
       01  WSAA-COVERAGE               PIC X(02).
       01  WSAA-RIDER                  PIC X(02).
       01  WSAA-SING-PRM-IND           PIC X(01) VALUE 'N'.
       01  WSAA-SUSP-IND               PIC X.                           <CAS1.0>
      *01  WSAA-MGFEE                  PIC S9(11)V9(02) COMP-3.         <D96NUM>
      *01  WSAA-MGFEE-REG-AMT          PIC S9(11)V9(02) COMP-3.         <D96NUM>
      *01  WSAA-SINGP-FEE              PIC S9(11)V9(02) COMP-3.         <D96NUM>
       01  WSAA-MGFEE                  PIC S9(15)V9(02) COMP-3.         <D96NUM>
       01  WSAA-MGFEE-REG-AMT          PIC S9(15)V9(02) COMP-3.         <D96NUM>
       01  WSAA-SINGP-FEE              PIC S9(15)V9(02) COMP-3.         <D96NUM>
       01  WSAA-PLAN-SUFFIX            PIC S9(04) COMP-3.
       01  WSAA-NO-OF-RECS             PIC 9(03) COMP-3.
       01  WSAA-AGENT-SUB              PIC 9(03) COMP-3.
      *01  WSAA-STAMP-DUTY-ACC         PIC S9(11)V9(02) COMP-3.         <D96NUM>
      *01  WSAA-TOLERANCE              PIC S9(11)V9(02) COMP-3.         <D96NUM>
       01  WSAA-STAMP-DUTY-ACC         PIC S9(15)V9(02) COMP-3.         <D96NUM>
      *01  WSAA-TOLERANCE              PIC S9(15)V9(02) COMP-3. <V42013><D96NUM>
       01  WSAA-SUB                    PIC 9(03) COMP-3.
       01  WSAA-SUB1                   PIC 9(03) COMP-3.                <V42013>
       01  WSAA-OLD-CESS-DATE          PIC S9(08) COMP-3.
       01  WSAA-LEXT-DATE              PIC S9(08) COMP-3.
       01  WSAA-RE-RATE-DATE           PIC S9(08) COMP-3.
       01  WSAA-EFFDATE                PIC S9(08) .                     <UL001>
       01  WSAA-NO-SUMMARY-REC         PIC X(01) VALUE SPACE.
      *01  WSAA-AGCM-AGNTNUM           PIC X(05).                       <069>
      *01  WSAA-AGCM-CEDAGENT          PIC X(05).                  <069><078>
       01  WSAA-AGCM-CEDAGENT          PIC X(08).                       <078>
       01  WSAA-SAVE-AGNTNUM           PIC X(08).                       <V4L014>
TDO    01  WSAA-SERVAG                 PIC X(08).                       <DA005>
       01  WSAA-JRNSEQ                 PIC S9(03) COMP-3.
       01  WSAA-TRANREF                PIC X(30).                       <V74L01>
       01  WSAA-AMOUNT-IN              PIC S9(15)V9(02) COMP-3.         <V74L01>
       01  WSAA-AGENTS.
           03 WSAA-AGENTS-COMMISSIONS  OCCURS 10 TIMES.
      ****    05 WSAA-AGNTNUM          PIC X(05).                       <078>
              05 WSAA-AGNTNUM          PIC X(08).                       <078>
              05 WSAA-SPLIT-BCOMM      PIC S9(03)V9(02) COMP-3.
      ****    05 WSAA-COMM-DUE         PIC S9(11)V9(02) COMP-3.         <D96NUM>
      ****    05 WSAA-COMM-EARN        PIC S9(11)V9(02) COMP-3.         <D96NUM>
      ****    05 WSAA-COMM-PAID        PIC S9(11)V9(02) COMP-3.         <D96NUM>
      ****    05 WSAA-SERV-DUE         PIC S9(11)V9(02) COMP-3.         <D96NUM>
      ****    05 WSAA-SERV-EARN        PIC S9(11)V9(02) COMP-3.         <D96NUM>
      ****    05 WSAA-RENL-DUE         PIC S9(11)V9(02) COMP-3.         <D96NUM>
      ****    05 WSAA-RENL-EARN        PIC S9(11)V9(02) COMP-3.         <D96NUM>
              05 WSAA-COMM-DUE         PIC S9(15)V9(02) COMP-3.         <D96NUM>
              05 WSAA-COMM-EARN        PIC S9(15)V9(02) COMP-3.         <D96NUM>
              05 WSAA-COMM-PAID        PIC S9(15)V9(02) COMP-3.         <D96NUM>
              05 WSAA-SERV-DUE         PIC S9(15)V9(02) COMP-3.         <D96NUM>
              05 WSAA-SERV-EARN        PIC S9(15)V9(02) COMP-3.         <D96NUM>
              05 WSAA-RENL-DUE         PIC S9(15)V9(02) COMP-3.         <D96NUM>
              05 WSAA-RENL-EARN        PIC S9(15)V9(02) COMP-3.         <D96NUM>
              05 WSAA-TCOM-DUE         PIC S9(15)V9(02) COMP-3.         <V6L000>
              05 WSAA-ANNPREM          PIC S9(15)V9(02) COMP-3.         <V73L01>
              05 WSAA-SINGP            PIC S9(15)V9(02) COMP-3.         <V73L01>
      *
       01  WSAA-LETOKEYS.                                               <P002>
           03  WSAA-OKEY-LANGUAGE      PIC X(01).                       <P002>
           03  WSAA-OKEY-TRANNO        PIC 9(05).                       <P002>
           03  FILLER                  PIC X(24).                       <P002>

       01  WSAA-OVERRIDE-AGENTS.
           03 WSAA-OVERRIDE-AGENT        OCCURS 10 TIMES.
              05  WSAA-OVERRIDE-AGENTS-COMM     OCCURS 10 TIMES.
      ****      07 WSAA-OVERRIDE-AGNTNUM PIC X(05).                     <078>
                07 WSAA-OVERRIDE-AGNTNUM PIC X(08).                     <078>
      ****      07 WSAA-OVERRIDE-COMM    PIC S9(11)V9(02) COMP-3.       <D96NUM>
                07 WSAA-OVERRIDE-COMM    PIC S9(15)V9(02) COMP-3.       <D96NUM>
      ****      07 WSAA-CEDAGENT         PIC X(05).                     <078>
                07 WSAA-CEDAGENT         PIC X(08).                     <078>
      ****      07 WSAA-OVRD-COMM-PAID   PIC S9(11)V9(02) COMP-3.       <D96NUM>
                07 WSAA-OVRD-COMM-PAID   PIC S9(15)V9(02) COMP-3.       <D96NUM>
      *
       01  WSAA-COMPKEY.
           03 WSAA-COMPKEY-TRANNO      PIC X(04).
           03 WSAA-COMPKEY-CRTABLE     PIC X(04).
      *                                                                 <UL001>
       01  WSAA-EXC-PRM-KEY.                                            <UL001>
           03 WSAA-EXCPKEY-ITEM        PIC X(04)  VALUE 'ZVUL'.         <UL001>
           03 WSAA-EXCPKEY-CRTABLE     PIC X(04).                       <UL001>
   ****01  WSAA-T5694                      PIC S999V99 VALUE ZEROS.     <CAS1.0>
       01  WSAA-TERM                       PIC 99.
       01  WSAA-INDEX                      PIC 99.
       01  WSAA-C                          PIC 999.
       01  WSAA-L                          PIC 99.
       01  WSAA-T6647-KEY.
           05  WSAA-T6647-TRCDE            PIC X(04).
           05  WSAA-T6647-CNTTYPE          PIC X(03).

       01  WSAA-COVERAGE-PREMIUMS.
           03  WSAA-LIFE-LEVEL             OCCURS 10.
               05  WSAA-COVERAGE-LEVEL     OCCURS 99.
      ****         10  WSAA-COVT-INSTPREM  PIC S9(11)V9(02).            <D96NUM>
      ****         10  WSAA-COVT-SINGP     PIC S9(11)V9(02).            <D96NUM>
                   10  WSAA-COVT-INSTPREM  PIC S9(15)V9(02).            <D96NUM>
                   10  WSAA-COVT-SINGP     PIC S9(15)V9(02).            <D96NUM>

       01  WSAA-COVERAGE-NUM               PIC 9(02) VALUE 0.
       01  WSAA-LIFE-NUM                   PIC 9(02) VALUE 0.
      *
      *01  WSAA-CLNK-ANNPREM               PIC 9(13)V9999 COMP-3<D96NUM><084>
       01  WSAA-CLNK-ANNPREM               PIC 9(14)V9999 COMP-3.       <D96NUM>
       01  WSAA-PREMIUM-FLAG               PIC X     VALUE SPACES.
       01  WSAA-STORED-OVCPC               PIC S9(03)V9(02) COMP-3.
       01  WSAA-STORED-AGENT-CLASS         PIC X(4).
       01  WSAA-T5645.
           03  WSAA-STORED-T5645           OCCURS 15.
               05  WSAA-T5645-CNTTOT       PIC S9(02).
               05  WSAA-T5645-GLMAP        PIC X(00014).
               05  WSAA-T5645-SACSCODE     PIC X(00002).
               05  WSAA-T5645-SACSTYPE     PIC X(00002).
               05  WSAA-T5645-SIGN         PIC X(00001).
      *
   ****                                                                 <068>
   ****01  WSAA-PREM-DATE1                 PIC 9(08).                   <068>
   ****01  FILLER REDEFINES WSAA-PREM-DATE1.                            <068>
   ****    03  WSAA-PREM-YEAR1             PIC 9(04).                   <068>
   ****    03  WSAA-PREM-MTH1              PIC 9(02).                   <068>
   ****    03  WSAA-PREM-DAY1              PIC 9(02).                   <068>
   ****                                                                 <068>
   ****01  WSAA-PREM-DATE2                 PIC 9(08).                   <068>
   ****01  FILLER REDEFINES WSAA-PREM-DATE2.                            <068>
   ****    03  WSAA-PREM-YEAR2             PIC 9(04).                   <068>
   ****    03  WSAA-PREM-MTH2              PIC 9(02).                   <068>
   ****    03  WSAA-PREM-DAY2              PIC 9(02).                   <068>

      *01  WSAA-SHORTFALL            PIC S9(11)V9(2) COMP-3.            <D96NUM>
      *01  WSAA-CNT-SUSPENSE         PIC S9(11)V9(2) COMP-3.            <D96NUM>
      *01  WSAA-PAYR-SUSPENSE        PIC S9(11)V9(2) COMP-3.            <D96NUM>
      *01  WSAA-TOT-SINGP            PIC S9(11)V9(2) COMP-3.            <D96NUM>
      *01  WSAA-TOT-REGP-ADJ         PIC S9(11)V9(2) COMP-3.            <D96NUM>
      *01  WSAA-TOT-TOLERANCE        PIC S9(11)V9(2) COMP-3.            <D96NUM>
       01  WSAA-SHORTFALL            PIC S9(15)V9(2) COMP-3.            <D96NUM>
       01  WSAA-CNT-SUSPENSE         PIC S9(15)V9(2) COMP-3.            <D96NUM>
       01  WSAA-INSTPREM-TOT         PIC S9(11)V9(2) COMP-3.            <CAS1.0>
       01  WSAA-INSTPREM-TOT-XGE     PIC S9(11)V9(2) COMP-3.            <CAS1.0>
       01  WSAA-PAYR-SUSPENSE        PIC S9(15)V9(2) COMP-3.            <D96NUM>
       01  WSAA-TOT-SINGP            PIC S9(15)V9(2) COMP-3.            <D96NUM>
       01  WSAA-TOT-REGP-ADJ         PIC S9(15)V9(2) COMP-3.            <D96NUM>
       01  WSAA-TOT-TOLERANCE        PIC S9(15)V9(2) COMP-3.            <D96NUM>
       01  WSAA-FEE-FREQ             PIC S9(06)V9(5).
      *01  WSAA-AMNT-DUE             PIC S9(11)V9(2) COMP-3.            <D96NUM>
       01  WSAA-AMNT-DUE             PIC S9(15)V9(2) COMP-3.            <D96NUM>
      *01  WSAA-TOLERANCE-APP        PIC S9(02)V99.                     <D96NUM>
       01  WSAA-TOLERANCE-APP        PIC S9(02)V99.                     <V42013>
       01  WSAA-TOLERANCE-APP2       PIC S9(02)V99.                     <V42013>
      *01  WSAA-CALC-TOLERANCE       PIC S9(11)V99 COMP-3.              <D96NUM>
       01  WSAA-CALC-TOLERANCE       PIC S9(15)V99 COMP-3.              <D96NUM>
       01  WSAA-TOLERANCE            PIC S9(15)V99 COMP-3.              <V42013>
       01  WSAA-TOLERANCE2           PIC S9(15)V99 COMP-3.              <V42013>
      *01  WSAA-AMOUNT-LIMIT         PIC S9(04)V99.                     <V42013>
       01  WSAA-AMOUNT-LIMIT         PIC S9(15)V99.                     <V42013>
       01  WSAA-AMOUNT-LIMIT2        PIC S9(15)V99.                     <V42013>
       01  WSAA-BILLFREQ-9           PIC 99.
      *01  WSAA-FEE-ADJ              PIC S9(11)V99.                     <D96NUM>
       01  WSAA-FEE-ADJ              PIC S9(15)V99.                     <D96NUM>
       01  WSAA-MAIN-COVERAGE        PIC X(04).                         <V42004>
       01  WSAA-BASIC-COMM-METH      PIC X(04).                         <V42004>
       01  WSAA-BASCPY               PIC X(04).                         <V42004>
       01  WSAA-SRVCPY               PIC X(04).                         <V42004>
       01  WSAA-RNWCPY               PIC X(04).                         <V42004>
       01  WSAA-BASSCMTH             PIC X(04).                         <V42004>
       01  WSAA-BASSCPY              PIC X(04).                         <V42004>

       01  WSAA-TR695-KEY.                                              <V42004>
           03  WSAA-TR695-COVERAGE   PIC X(04).                         <V42004>
           03  WSAA-TR695-RIDER      PIC X(04).                         <V42004>

       01  WSAA-T5667-KEY.
           03  WSAA-T5667-TRANCD       PIC X(4).
           03  WSAA-T5667-CURR         PIC X(4).

       01  WSAA-T7508-KEY.                                              <DRY001>
           03  WSAA-T7508-BATCTRCDE    PIC X(04).                       <DRY001>
           03  WSAA-T7508-CNTTYPE      PIC X(04).                       <DRY001>
                                                                        <DRY001>
       01  WSAA-TR52E-KEY.                                              <V74L01>
           03  WSAA-TR52E-TXCODE       PIC X(01).                       <V74L01>
           03  WSAA-TR52E-CNTTYPE      PIC X(03).                       <V74L01>
           03  WSAA-TR52E-CRTABLE      PIC X(04).                       <V74L01>
                                                                        <V74L01>
       01  WSAA-RATE-ITEM.                                              <V74L01>
           03  WSAA-CNT-CURR           PIC X(03).                       <V74L01>
           03  WSAA-TXITEM             PIC X(04).                       <V74L01>
           03  FILLER                  PIC X.                           <V74L01>
                                                                        <V74L01>
       01   WSAA-PAYRKEY.
            03 WSAA-CHDRNUM           PIC X(8).
            03 WSAA-PAYRSEQNO         PIC X.

      *
       01  WSAA-BILLING-INFORMATION.
           03 WSAA-BILLING-DETAILS OCCURS 9 TIMES.
              05 WSAA-INCOME-SEQ-NO   PIC 99.
              05 WSAA-BILLFREQ        PIC X(02).                        <D9604>
              05 WSAA-BILLCHNL        PIC X(02).
              05 WSAA-BILLCD          PIC S9(08) COMP-3.
              05 WSAA-BTDATE          PIC S9(08) COMP-3.
              05 WSAA-BILLCURR        PIC X(03).
              05 WSAA-CLNTCOY         PIC X(01).
              05 WSAA-CLNTNUM         PIC X(08).
      ****    05 WSAA-REG-PREM-ACC    PIC S9(11)V9(2) COMP-3.           <D96NUM>
      ****    05 WSAA-SING-PREM-ACC   PIC S9(11)V9(2) COMP-3.           <D96NUM>
      ****    05 WSAA-REG-PREM-ADJ    PIC S9(11)V9(2) COMP-3.           <D96NUM>
      ****    05 WSAA-INSTPREM        PIC S9(11)V9(2) COMP-3.           <D96NUM>
              05 WSAA-REG-PREM-ACC    PIC S9(15)V9(2) COMP-3.           <D96NUM>
              05 WSAA-SING-PREM-ACC   PIC S9(15)V9(2) COMP-3.           <D96NUM>
              05 WSAA-REG-PREM-ADJ    PIC S9(15)V9(2) COMP-3.           <D96NUM>
              05 WSAA-INSTPREM        PIC S9(15)V9(2) COMP-3.           <D96NUM>
              05 WSAA-SP-TAX          PIC S9(15)V9(2) COMP-3.           <V74L01>
              05 WSAA-RP-TAX          PIC S9(15)V9(2) COMP-3.           <V74L01>
              05 WSAA-FE-TAX          PIC S9(15)V9(2) COMP-3.           <V74L01>
              05 WSAA-FREQ-FACTOR     PIC S9(06)V9(05).
      ****    05 WSAA-TAXRELAMT       PIC S9(11)V9(2) COMP-3.           <D96NUM>
              05 WSAA-TAXRELAMT       PIC S9(15)V9(2) COMP-3.           <D96NUM>
              05 WSAA-INREVNUM        PIC X(08).
      ****    05 WSAA-TOLR-USED       PIC S9(11)V9(2) COMP-3.           <D96NUM>
              05 WSAA-TOLR-USED       PIC S9(15)V9(2) COMP-3.           <D96NUM>
              05 WSAA-BILLDAY         PIC X(02).                        <LA5139>
              05 WSAA-BILLMONTH       PIC X(02).                        <LA5139>
      *
      *01  WSAA-COMPONENT-TOTALS.                                       <D96NUM>
      **** 03  WSAA-COMP-SING-PREM     PIC S9(11)V9(2) COMP-3.          <D96NUM>
      **** 03  WSAA-COMP-REG-PREM      PIC S9(11)V9(2) COMP-3.          <D96NUM>
      **** 03  WSAA-COMP-ERN-ICOMM     PIC S9(11)V9(2) COMP-3.          <D96NUM>
      **** 03  WSAA-COMP-ADV-ICOMM     PIC S9(11)V9(2) COMP-3.          <D96NUM>
      **** 03  WSAA-COMP-ERN-SCOMM     PIC S9(11)V9(2) COMP-3.          <D96NUM>
      **** 03  WSAA-COMP-ERN-RCOMM     PIC S9(11)V9(2) COMP-3.          <D96NUM>
      **** 03  WSAA-COMP-ERN-OCOMM     PIC S9(11)V9(2) COMP-3.          <D96NUM>
      **** 03  WSAA-COMP-ADV-OCOMM     PIC S9(11)V9(2) COMP-3.          <D96NUM>
      **** 03  WSAA-COMP-TAX-RELIEF    PIC S9(11)V9(2) COMP-3.          <D96NUM>
       01  WSAA-COMPONENT-TOTALS.                                       <D96NUM>
           03  WSAA-COMP-SING-PREM     PIC S9(15)V9(2) COMP-3.          <D96NUM>
           03  WSAA-COMP-REG-PREM      PIC S9(15)V9(2) COMP-3.          <D96NUM>
           03  WSAA-COMP-ERN-ICOMM     PIC S9(15)V9(2) COMP-3.          <D96NUM>
           03  WSAA-COMP-ADV-ICOMM     PIC S9(15)V9(2) COMP-3.          <D96NUM>
           03  WSAA-COMP-ERN-SCOMM     PIC S9(15)V9(2) COMP-3.          <D96NUM>
           03  WSAA-COMP-ERN-RCOMM     PIC S9(15)V9(2) COMP-3.          <D96NUM>
           03  WSAA-COMP-ERN-OCOMM     PIC S9(15)V9(2) COMP-3.          <D96NUM>
           03  WSAA-COMP-ADV-OCOMM     PIC S9(15)V9(2) COMP-3.          <D96NUM>
           03  WSAA-COMP-TAX-RELIEF    PIC S9(15)V9(2) COMP-3.          <D96NUM>

       01  WSAA-RLDGACCT.
           03  WSAA-RLDG-CHDRNUM       PIC X(08).
           03  WSAA-RLDG-LIFE          PIC X(02).
           03  WSAA-RLDG-COVERAGE      PIC X(02).
           03  WSAA-RLDG-RIDER         PIC X(02).
           03  WSAA-RLDG-PLAN-SUFFIX   PIC X(02).
      *
       01  WSAA-PLAN-SUFF.
           03 WSAA-PLAN                PIC 9(04).
           03 WSAA-PLAN-R REDEFINES WSAA-PLAN.
              05 WSAA-FILLER           PIC 9(02).
              05 WSAA-PLANSUFF         PIC 9(02).

       01  WSAA-ACCT-LEVEL             PIC X(1).
           88  COMP-LEVEL-ACC          VALUE 'Y'.
      *
       01  WSAA-FLEX-PREM              PIC X(1).                        <D9604>
           88 FLEXIBLE-PREMIUM-CONTRACT     VALUE 'Y'.                  <D9604>
           88 NOT-FLEXIBLE-PREMIUM-CONTRACT VALUE 'N'.                  <D9604>
                                                                        <D9604>
       01  WSAA-FLEX-PREM-FQ           PIC 99.                          <D9604>
                                                                        <D9604>
        01  WSAA-COUNT                 PIC 99.
      *
      * 01  WSAA-MIN-TRM-TO-CESS       PIC S9(02).              <V73L03><070>
        01  WSAA-MIN-TRM-TO-CESS       PIC S9(03).                      <V73L03>
        01  WSAA-SPEC-TERMS-EXIST      PIC X(01).                       <070>
        01  WSBB-SUB                   PIC 99.
      *                                                                 <081>
       01  WSAA-EARLIEST-RERATE-DATE   PIC 9(08) COMP-3.                <LA2110>
       01  WSAA-CPI-VALID              PIC X(01).                       <LA2110>
           88  CPI-VALID               VALUE     'Y'.                   <LA2110>
       01  WSAA-WOP-MATCH              PIC X(01).                       <LA2110>
           88  WOP-MATCH               VALUE     'Y'.                   <LA2110>
           88  WOP-NOT-MATCH           VALUE     'N'.                   <LA2110>
       01  WSAA-CRTABLE-MATCH          PIC X(01) VALUE 'N'.             <LA2110>
           88  CRTABLE-MATCH                     VALUE 'Y'.             <LA2110>
       01  WSAA-CNT                    PIC 9(02).                       <LA2110>
      *01  WSAA-LETOKEYS.                                       <PCPPRT><081>
      **** 03  WSAA-LT-CHDRNUM         PIC 9(8).                <PCPPRT><081>
      **** 03  WSAA-LT-LANGUAGE        PIC X(1).                <PCPPRT><POLSCD>
      **** 03  WSAA-SV-TOTAL           PIC Z9(11).99.           <D96NUM><081>
      **** 03  WSAA-SV-TOTAL           PIC Z9(15).99.           <D96NUM><POLSCD>
      **** 03  FILLER                  PIC X(08).               <PCPPRT><081>
      **** 03  FILLER                  PIC X(21).               <PCPPRT><POLSCD>
      *                                                                 <086>
      * 01 WSAA-COMTOT-KEPT            PIC S9(11)V99 COMP-3.    <D96NUM><086>
      * 01 WSAA-COMPAY-KEPT            PIC S9(11)V99 COMP-3.    <D96NUM><086>
      * 01 WSAA-COMERN-KEPT            PIC S9(11)V99 COMP-3.    <D96NUM><086>
        01 WSAA-COMTOT-KEPT            PIC S9(15)V99 COMP-3.            <D96NUM>
        01 WSAA-COMPAY-KEPT            PIC S9(15)V99 COMP-3.            <D96NUM>
        01 WSAA-COMERN-KEPT            PIC S9(15)V99 COMP-3.            <D96NUM>
        01 WSAA-OVRTIMES               PIC 99.                          <086>
       01 WSAA-MIN-OVERDUE             PIC 9(3) COMP-3.                 <D9604>
       01  WSAA-T5729-SUB              PIC S9(5) COMP-3.                <D9604>
        01 WSAA-COMPAY-KEPT-2          PIC S9(11)V99 COMP-3.            <CAS1.0>
       01  WSAA-PRMDEPST             PIC S9(15)V9(2) COMP-3.            <V4L001>
      *                                                                 <086>
       01  WSAA-LIFE                   PIC X(02) VALUE SPACES.          <R96REA>
       01  WSAA-L1-CLNTNUM             PIC X(08) VALUE SPACES.          <R96REA>
       01  WSAA-L2-CLNTNUM             PIC X(08) VALUE SPACES.          <R96REA>
       01  WSAA-WAIVE-CONT             PIC X(01).                       <TN01>
       01  WSAA-ZRWVFLGS.                                               <TN01>
      *    03  WSAA-ZRWVFLG            PIC X(00001) OCCURS 03.  <PHE003><TN01>
           03  WSAA-ZRWVFLG            PIC X(00001) OCCURS 05.          <PHE003>
       01  WSAA-TR517-REC              PIC X(250).                      <TN01>
       01  WSAA-DOB-PLUS-TR627         PIC 9(08).                       <V71L05>
       01  WSAA-ZSUFCDTE               PIC 9(08).                       <V71L05>
       01  WSAA-LIFCNUM                PIC X(08).                       <V71L05>
                                                                        <A06557>
       01  WSAA-BILLING-DATE           PIC 9(08).                       <A06557>
       01  FILLER REDEFINES WSAA-BILLING-DATE.                          <A06557>
           03  WSAA-BILL-DATE-YR       PIC 9(04).                       <A06557>
           03  WSAA-BILL-DATE-MNTH     PIC 9(02).                       <A06557>
           03  WSAA-BILL-DATE-DAY      PIC 9(02).                       <A06557>
                                                                        <A06557>
       01  WSAA-ANNIV-EXTRACT-DATE     PIC 9(08).                       <A06557>
       01  FILLER REDEFINES WSAA-ANNIV-EXTRACT-DATE.                    <A06557>
           03  FILLER                  PIC 9(04).                       <A06557>
           03  WSAA-ANNIV-EXTRACT-MNTH PIC 9(02).                       <A06557>
           03  WSAA-ANNIV-EXTRACT-DAY  PIC 9(02).                       <A06557>
                                                                        <DRYAPL>
       01  WSAA-EARLIEST-RCESDTE       PIC S9(08) COMP-3.               <DRYAPL>
       01  WSAA-EARLIEST-CRRCD         PIC S9(08) COMP-3.               <DRYAPL>
                                                                        <A06557>
       01  WSAA-REG-PREM-FIRST         PIC S9(15)V9(2) COMP-3.          <V70L01>
       01  WSAA-REG-PREM-RENEWAL       PIC S9(15)V9(2) COMP-3.          <V70L01>
       01  WSAA-ZCTN-ANNPREM           PIC S9(15)V9(2) COMP-3.          <V70L01>
       01  WSAA-ZCTN-INSTPREM          PIC S9(15)V9(2) COMP-3.          <V70L01>
       01  WSAA-REG-INTM-DATE          PIC S9(08) COMP-3.               <V70L01>
       01  WSAA-BILLFQ-9               PIC 99.                          <V70L01>
       01  WSAA-EXCESS-INST-NUM        PIC 9(05).                       <V70L01>
       01  WSAA-PREM-TAX-01            PIC S9(15)V99.                   <V74L01>
       01  WSAA-PREM-TAX-02            PIC S9(15)V99.                   <V74L01>
       01  WSAA-TAX-BASE-AMT           PIC S9(15)V99.                   <V74L01>
                                                                        <V70L01>
       01  WSAA-OCCDATE                PIC 9(08).                       <LA4351>
       01  FILLER REDEFINES WSAA-OCCDATE.                               <LA4351>
           03  WSAA-OCC-YY             PIC 9(04).                       <LA4351>
           03  WSAA-OCC-MM             PIC 9(02).                       <LA4351>
           03  WSAA-OCC-DD             PIC 9(02).                       <LA4351>
      *                                                                 <LA4351>
       01  WSAA-FOUND                  PIC X(01).                       <V74L03>
           88  FOUND                   VALUE 'Y'.                       <V74L03>
           88  NOT-FOUND               VALUE 'N'.                       <V74L03>
       01  IX                          PIC 9(02).                       <V74L03>
PHL108 01  WSAA-ZDISPF-FOUND           PIC X(01).                       <PHL108>
           88  ZDISPF-FOUND            VALUE 'Y'.                       <PHL108>
           88  ZDISPF-NOT-FOUND        VALUE 'N'.                       <PHL108>
       01  WSAA-STORED-STAFF-DIS       PIC S9(15)V99.                   <PHL108>
       01  WSAA-T5645-SEQ-02.                                           <PHL108>
           03  WSAA-STORED-T5645-02        OCCURS 15.                   <PHL108>
               05  WSAA-T5645-CNTTOT-02    PIC S9(02).                  <PHL108>
               05  WSAA-T5645-GLMAP-02     PIC X(00014).                <PHL108>
               05  WSAA-T5645-SACSCODE-02  PIC X(00002).                <PHL108>
               05  WSAA-T5645-SACSTYPE-02  PIC X(00002).                <PHL108>
               05  WSAA-T5645-SIGN-02      PIC X(00001).                <PHL108>
       01  WSAA-STAFF-FLAG             PIC X(01).                       <PHE001>
       01  WSAA-ITEMCHK.                                                <PHE001>
           03  WSAA-CNTTYPE            PIC X(03).                       <PHE001>
           03  WSAA-PRODCODE           PIC X(04).                       <PHE001>
           03  WSAA-STAFFIND           PIC X(01).                       <PHE001>
                                                                        <UL001>
       01  WSAA-T5671-REC.                                              <UL001>
             03  WSAA-T5671-EDTITMS                  .                  <UL001>
               05  WSAA-T5671-EDTITM                   PIC X(00005)     <UL001>
                                                  OCCURS 04 .           <UL001>
             03  FILLER REDEFINES WSAA-T5671-EDTITMS                  . <UL001>
               05  WSAA-T5671-EDTITM-01                PIC X(00005)    .<UL001>
               05  WSAA-T5671-EDTITM-02                PIC X(00005)    .<UL001>
               05  WSAA-T5671-EDTITM-03                PIC X(00005)    .<UL001>
               05  WSAA-T5671-EDTITM-04                PIC X(00005)    .<UL001>
             03  WSAA-T5671-PGMS                     .                  <UL001>
               05  WSAA-T5671-PGM                      PIC X(00005)     <UL001>
                                                  OCCURS 04 .           <UL001>
             03  FILLER REDEFINES WSAA-T5671-PGMS                     . <UL001>
               05  WSAA-T5671-PGM-01                   PIC X(00005)    .<UL001>
               05  WSAA-T5671-PGM-02                   PIC X(00005)    .<UL001>
               05  WSAA-T5671-PGM-03                   PIC X(00005)    .<UL001>
               05  WSAA-T5671-PGM-04                   PIC X(00005)    .<UL001>
             03  WSAA-T5671-SUBPROGS                 .                  <UL001>
               05  WSAA-T5671-SUBPROG                  PIC X(00010)     <UL001>
                                                  OCCURS 04 .           <UL001>
             03  FILLER REDEFINES WSAA-T5671-SUBPROGS                 . <UL001>
               05  WSAA-T5671-SUBPROG-01               PIC X(00010)    .<UL001>
               05  WSAA-T5671-SUBPROG-02               PIC X(00010)    .<UL001>
               05  WSAA-T5671-SUBPROG-03               PIC X(00010)    .<UL001>
               05  WSAA-T5671-SUBPROG-04               PIC X(00010)    .<UL001>
             03  WSAA-T5671-TREVSUBS                 .                  <UL001>
               05  WSAA-T5671-TREVSUB                  PIC X(00010)     <UL001>
                                                  OCCURS 04 .           <UL001>
             03  FILLER REDEFINES WSAA-T5671-TREVSUBS                 . <UL001>
               05  WSAA-T5671-TREVSUB-01               PIC X(00010)    .<UL001>
               05  WSAA-T5671-TREVSUB-02               PIC X(00010)    .<UL001>
               05  WSAA-T5671-TREVSUB-03               PIC X(00010)    .<UL001>
               05  WSAA-T5671-TREVSUB-04               PIC X(00010)    .<UL001>
             03  FILLER                         PIC X(380).             <UL001>
      *                                                                 <UL006>
       01  WSAA-BAS-SUMINS             PIC S9(15)V9(02) COMP-3.         <UL006>
      *                                                                 <V74L03>
       01  ERRORS.
           03  F294                    PIC X(04) VALUE 'F294'.
           03  E308                    PIC X(04) VALUE 'E308'.
           03  I035                    PIC X(04) VALUE 'I035'.
           03  H036                    PIC X(04) VALUE 'H036'.          <070>
           03  G437                    PIC X(04) VALUE 'G437'.          <081>
           03  H791                    PIC X(04) VALUE 'H791'.          <D9604>
           03  TL17                    PIC X(04) VALUE 'TL17'.          <N003>
      *
       01  TABLES.
           03  T5679                   PIC X(05) VALUE 'T5679'.
           03  T5687                   PIC X(05) VALUE 'T5687'.
           03  T5534                   PIC X(05) VALUE 'T5534'.
           03  T5676                   PIC X(05) VALUE 'T5676'.
           03  T5671                   PIC X(05) VALUE 'T5671'.
           03  T5647                   PIC X(05) VALUE 'T5647'.
           03  T5644                   PIC X(05) VALUE 'T5644'.
           03  T5688                   PIC X(05) VALUE 'T5688'.
           03  T5674                   PIC X(05) VALUE 'T5674'.
           03  T5645                   PIC X(05) VALUE 'T5645'.
      **** 03  T5694                   PIC X(05) VALUE 'T5694'.         <CAS1.0>
           03  T6647                   PIC X(05) VALUE 'T6647'.
           03  T6659                   PIC X(05) VALUE 'T6659'.
           03  T6687                   PIC X(05) VALUE 'T6687'.
           03  T5667                   PIC X(05) VALUE 'T5667'.
           03  T3695                   PIC X(05) VALUE 'T3695'.
           03  T6658                   PIC X(05) VALUE 'T6658'.         <070>
      **** 03  T6634                   PIC X(05) VALUE 'T6634'. <PCPPRT><081>
           03  TR384                   PIC X(05) VALUE 'TR384'.         <PCPPRT>
           03  T5729                   PIC X(05) VALUE 'T5729'.         <D9604>
           03  TR517                   PIC X(05) VALUE 'TR517'.         <LA2110>
           03  TR695                   PIC X(05) VALUE 'TR695'.         <V42004>
           03  T7508                   PIC X(05) VALUE 'T7508'.         <DRY001>
           03  TH605                   PIC X(05) VALUE 'TH605'.         <V70L01>
           03  TR627                   PIC X(05) VALUE 'TR627'.         <V71L05>
           03  TR52D                   PIC X(05) VALUE 'TR52D'.         <V74L01>
           03  TR52E                   PIC X(05) VALUE 'TR52E'.         <V74L01>
           03  TR52Q                   PIC X(05) VALUE 'TR52Q'.         <V74L03>
           03  TZ018                   PIC X(05) VALUE 'TZ018'.         <PHE001>
      *
       01  FORMATS.
           03  CHDRLNBREC              PIC X(10) VALUE 'CHDRLNBREC'.
    ****   03  BNFYLNB                 PIC X(10) VALUE 'BNFYLNB'.       <V74F03>
           03  CLRRREC                 PIC X(10) VALUE 'CLRRREC'.
           03  PCDDLNBREC              PIC X(10) VALUE 'PCDDLNBREC'.
           03  LIFELNBREC              PIC X(10) VALUE 'LIFELNBREC'.
           03  LIFRTRNREC              PIC X(10) VALUE 'LIFRTRNREC'.
           03  LIFACMVREC              PIC X(10) VALUE 'LIFACMVREC'.
           03  COVTLNBREC              PIC X(10) VALUE 'COVTLNBREC'.
           03  COVRLNBREC              PIC X(10) VALUE 'COVRLNBREC'.
           03  COVRTRBREC              PIC X(10) VALUE 'COVRTRBREC'.    <LA2110>
           03  AGLFLNBREC              PIC X(10) VALUE 'AGLFLNBREC'.
           03  ZRAPREC                 PIC X(10) VALUE 'ZRAPREC'.       <CAS1.0>
           03  PTRNREC                 PIC X(10) VALUE 'PTRNREC'.
           03  AGCMREC                 PIC X(10) VALUE 'AGCMREC'.
           03  LEXTREC                 PIC X(10) VALUE 'LEXTREC'.
           03  PAYRREC                 PIC X(10) VALUE 'PAYRREC'.
           03  CLRFREC                 PIC X(10) VALUE 'CLRFREC'.
           03  RTRNSACREC              PIC X(10) VALUE 'RTRNSACREC'.
      **** 03  ACBLREC                 PIC X(10) VALUE 'ACBLREC'.       <CAS1.0>
           03  ACBLENQREC              PIC X(10) VALUE 'ACBLENQREC'.    <CAS1.0>
      **** 03  RACTLNBREC              PIC X(10) VALUE 'RACTLNBR<R96REA><070>
           03  AGPYAGTREC              PIC X(10) VALUE 'AGPYAGTREC'.    <V74L03>
           03  AGPYDOCREC              PIC X(10) VALUE 'AGPYDOCREC'.    <V74L03>
           03  INCTREC                 PIC X(10) VALUE 'INCTREC'.       <070>
           03  FPRMREC                 PIC X(10) VALUE 'FPRMREC'.       <D9604>
           03  FPCOREC                 PIC X(10) VALUE 'FPCOREC'.       <D9604>
           03  ITEMREC                 PIC X(10) VALUE 'ITEMREC'.       <R96REA>
           03  HPADREC                 PIC X(10) VALUE 'HPADREC'.       <CAS1.0>
      **** 03  LIFEREC                 PIC X(10) VALUE 'LIFEREC'.       <V42005>
           03  ZPTNREC                 PIC X(10) VALUE 'ZPTNREC'.       <V70L01>
           03  ZCTNREC                 PIC X(10) VALUE 'ZCTNREC'.       <V70L01>
           03  CLTSREC                 PIC X(10) VALUE 'CLTSREC'.       <V71L05>
           03  UNDLREC                 PIC X(10) VALUE 'UNDLREC'.       <V71L12>
           03  UNDCREC                 PIC X(10) VALUE 'UNDCREC'.       <V71L12>
           03  UNDQREC                 PIC X(10) VALUE 'UNDQREC'.       <V71L12>
           03  TAXDREC                 PIC X(10) VALUE 'TAXDREC'.       <V74L01>
           03  ZDISREC                 PIC X(10) VALUE 'ZDISREC'.       <PHL108>
           03  CLEXREC                 PIC X(10) VALUE 'CLEXREC'.       <PHE001>
           03  ZPFRREC                 PIC X(10) VALUE 'ZPFRREC'.       <DA006>
           03  ZPOSREC                 PIC X(10) VALUE 'ZPOSREC'.       <UL006>
                                                                        <V42005>
      **** COPY LIFESKM.                                                <V42005>
                                                                        <V42005>
           COPY CRTUNDWREC.                                             <V42005>
      *
           COPY VARCOM.
      *
           COPY SMTPFXCPY.                                              <V74L03>
      * Records.
           COPY CHDRLNBSKM.
    ****   COPY BNFYLNBSKM.                                             <V74F03>
           COPY PCDDLNBSKM.
           COPY LIFELNBSKM.
           COPY LIFRTRNREC.
           COPY LIFACMVREC.
           COPY COVTLNBSKM.
           COPY COVRLNBSKM.
           COPY COVRTRBSKM.                                             <LA2110>
           COPY AGLFLNBSKM.
           COPY ZRAPSKM.                                                <CAS1.0>
           COPY AGCMSKM.
           COPY PTRNSKM.
           COPY ITEMSKM.
           COPY ITDMSKM.
           COPY DESCSKM.
           COPY CLRRSKM.
           COPY LEXTSKM.
           COPY PAYRSKM.
           COPY CLRFSKM.
           COPY PRASREC.
           COPY ZPTNSKM.                                                <V70L01>
           COPY ZCTNSKM.                                                <V70L01>
           COPY RTRNSACSKM.
      **** COPY ACBLSKM.                                                <CAS1.0>
      /                                                                 <CAS1.0>
           COPY ACBLENQSKM.                                             <CAS1.0>
      /                                                                 <CAS1.0>
           COPY CONLINKREC.                                             <CAS1.0>
      **** COPY RACTLNBSKM.                                     <R96REA><070>
           COPY INCTSKM.                                                <070>
           COPY FPRMSKM.                                                <D9604>
           COPY FPCOSKM.                                                <D9604>
           COPY ACTVRESREC.                                             <R96REA>
           COPY HPADSKM.                                                <CAS1.0>
           COPY CLTSSKM.                                                <V71L05>
           COPY UNDLSKM.                                                <V71L12>
           COPY UNDCSKM.                                                <V71L12>
           COPY UNDQSKM.                                                <V71L12>
           COPY TAXDSKM.                                                <V74L01>
           COPY AGPYAGTSKM.                                             <V74L03>
           COPY AGPYDOCSKM.                                             <V74L03>
           COPY ZDISSKM.                                                <PHL108>
           COPY CLEXSKM.                                                <PHE001>
           COPY ZPFRSKM.                                                <DA006>
           copy ZPOSSKM.                                                <UL006>
      *
      * System records.
           COPY SYSERRREC.
      *
      * Tables.
           COPY T5679REC.
           COPY T5687REC.
           COPY T5534REC.
           COPY T5676REC.
           COPY T5671REC.
           COPY T5647REC.
           COPY T5644REC.
           COPY T5688REC.
           COPY TR517REC.                                               <LA2110>
           COPY T5674REC.
           COPY T5645REC.
      **** COPY T5694REC.                                               <CAS1.0>
           COPY T6647REC.
           COPY T6659REC.
           COPY T6687REC.
           COPY T5667REC.
           COPY T3695REC.
           COPY T6658REC.                                               <070>
      **** COPY T6634REC.                                       <PCPPRT><081>
           COPY TR384REC.                                               <PCPPRT>
           COPY T5729REC.                                               <D9604>
           COPY TR695REC.                                               <V42004>
           COPY T7508REC.                                               <DRY001>
           COPY TH605REC.                                               <V70L01>
           COPY TR627REC.                                               <V71L05>
           COPY TR52DREC.                                               <V74L01>
           COPY TR52EREC.                                               <V74L01>
           COPY TR52QREC.                                               <V74L03>
      *
      * Subroutines.
           COPY DATCON1REC.
           COPY DATCON2REC.
           COPY DATCON3REC.
           COPY DATCON4REC.                                             <V70L01>
           COPY DRYPRCREC.                                              <DRY001>
           COPY DRYPRCLNK.                                              <DRY001>
           COPY SFTLOCKREC.
           COPY UBBLALLPAR.
           COPY STDTALLREC.
           COPY ISUALLREC.
           COPY RNLALLREC.                                              <UL001>
           COPY COMLINKREC.
           COPY BATCUPREC.
           COPY MGFEELREC.
           COPY CLTRELNREC.
           COPY LETRQSTREC.                                             <N003>
      /
           COPY LIFSTTRREC.
      **** COPY RLLIAREC.                                       <LA3380><V4L011>
           COPY RLLIADBREC.                                             <LA3380>
           COPY RLPDLONREC.                                             <V4L001>
           COPY ZORLNKREC.                                              <V73L01>
           COPY TXCALCREC.                                              <V74L01>
           COPY BLDENRLREC.                                             <V75F01>
           COPY ZRDECPLREC.                                             <V76F06>
           COPY ZCHKRAGREC.                                             <DA024>
      /
       LINKAGE SECTION.
      *
           COPY ATMODREC.
      *
       PROCEDURE DIVISION USING  ATMD-ATMOD-REC.
      *
       0000-MAINLINE SECTION.
      **********************
       0010-MAINLINE.
      *
           PERFORM 1000-INITIALISE.
           PERFORM 2000-PROCESS.
      *
           PERFORM A000-STATISTICS.
           PERFORM A500-ADD-UNDERWRITTING.                              <V42005>
           PERFORM 8000-DRY-PROCESSING.                                 <DRY001>
           PERFORM 3100-EXCESS-PREMIUM-ALLOCATION.                      <UL001>
           PERFORM A1000-WRITE-ZPOS.                                    <UL006>
           PERFORM 5000-WRITE-LETTER.                                   <UL003>
           PERFORM 2840-RLSE-SOFTLOCK.                                  <079>
      *
       0090-EXIT.
           EXIT PROGRAM.
      *
      ***************************************************************
      * Bomb out of this AT module using the fatal error section    *
      * copied from MAINF.                                          *
      ***************************************************************
      *
       XXXX-FATAL-ERROR SECTION.
      **************************
       XXXX-FATAL-ERRORS.
      *
           IF SYSR-STATUZ              = BOMB
               GO TO XXX1-ERROR-PROG.
      *
           MOVE SYSR-STATUZ            TO SYSR-SYSERR-STATUZ.
      *
           IF SYSR-SYSERR-TYPE         NOT = '2'
               MOVE '1'                TO SYSR-SYSERR-TYPE.
      *
           CALL 'SYSERR' USING SYSR-SYSERR-REC.
      *
       XXX1-ERROR-PROG.

      * AT
           MOVE BOMB                   TO ATMD-STATUZ.
      *
       XXXX-EXIT.
            EXIT PROGRAM.
      /
      *
       1000-INITIALISE SECTION.
      *************************
       1010-INITIALISE.
      *
           MOVE 'N'                    TO WSAA-SING-PRM-IND.
           MOVE ZERO                   TO WSAA-MGFEE.
           MOVE ZERO                   TO WSAA-MGFEE-REG-AMT.
           MOVE ZERO                   TO WSAA-SINGP-FEE.
           MOVE ZERO                   TO WSAA-COMTOT-KEPT.             <086>
           MOVE ZERO                   TO WSAA-COMPAY-KEPT.             <086>
           MOVE ZERO                   TO WSAA-COMERN-KEPT.             <086>
           MOVE ZERO                   TO WSAA-OVRTIMES.                <086>
           MOVE ZERO                   TO WSAA-COMPAY-KEPT-2.           <CAS1.0>
           MOVE ATMD-LANGUAGE          TO CLNK-LANGUAGE.                <MLS001>
           MOVE VRCM-MAX-DATE          TO WSAA-DOB-PLUS-TR627.          <V71L05>
           MOVE VRCM-MAX-DATE          TO WSAA-ZSUFCDTE.                <V71L05>
           MOVE SPACES                 TO WSAA-LIFCNUM.                 <V71L05>
           MOVE ZERO                   TO WSAA-AMOUNT-IN.               <V74L01>
           MOVE ZERO                   TO WSAA-COMP-SING-PREM.          <V74L01>
           MOVE ZERO                   TO WSAA-COMP-REG-PREM.           <V74L01>
           MOVE ZEROES                 TO WSAA-BAS-SUMINS.              <UL006>
      *                                                                 <086>
           PERFORM 1100-READ-CONTRACT-HEADER.
           PERFORM 1150-READ-TABLE-T5729.                               <D9604>
           PERFORM 1180-READ-TABLE-TH605.                               <V70L01>
                                                                        <V74L01>
      * Read table TR52D using CHDRLNB-REGISTER as key                  <V74L01>
           MOVE SPACES                 TO ITEM-DATA-KEY.                <V74L01>
           MOVE ATMD-COMPANY           TO ITEM-ITEMCOY.                 <V74L01>
           MOVE TR52D                  TO ITEM-ITEMTABL.                <V74L01>
           MOVE CHDRLNB-REGISTER       TO ITEM-ITEMITEM.                <V74L01>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <V74L01>
           MOVE READR                  TO ITEM-FUNCTION.                <V74L01>
           CALL 'ITEMIO' USING ITEM-PARAMS.                             <V74L01>
                                                                        <V74L01>
           IF ITEM-STATUZ              NOT = O-K                        <V74L01>
           AND ITEM-STATUZ             NOT = MRNF                       <V74L01>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <V74L01>
               MOVE ITEM-STATUZ        TO SYSR-STATUZ                   <V74L01>
               PERFORM XXXX-FATAL-ERROR                                 <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF ITEM-STATUZ              = MRNF                           <V74L01>
               MOVE SPACES              TO ITEM-DATA-KEY                <V74L01>
               MOVE ATMD-COMPANY        TO ITEM-ITEMCOY                 <V74L01>
               MOVE TR52D               TO ITEM-ITEMTABL                <V74L01>
               MOVE '***'               TO ITEM-ITEMITEM                <V74L01>
               MOVE 'IT'                TO ITEM-ITEMPFX                 <V74L01>
               MOVE READR               TO ITEM-FUNCTION                <V74L01>
               CALL 'ITEMIO' USING ITEM-PARAMS                          <V74L01>
                                                                        <V74L01>
               IF ITEM-STATUZ           NOT = O-K                       <V74L01>
                  MOVE ITEM-PARAMS      TO SYSR-PARAMS                  <V74L01>
                  MOVE ITEM-STATUZ      TO SYSR-STATUZ                  <V74L01>
                  PERFORM XXXX-FATAL-ERROR                              <V74L01>
               END-IF                                                   <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           MOVE ITEM-GENAREA           TO TR52D-TR52D-REC.              <V74L01>
                                                                        <V70L01>
           PERFORM 1200-CALL-DATCON1.
           PERFORM 1300-READ-STATUS-CODES.
           PERFORM 1400-CALC-SUSPENSE.
           PERFORM 1500-CLEAR-FIELDS.
      *                                                                 <069>
      *                                                                 <V4L001>
           INITIALIZE                  RLPDLON-REC.                     <V4L001>
           MOVE INFO                   TO RLPDLON-FUNCTION.             <V4L001>
           MOVE ZEROES                 TO RLPDLON-PRMDEPST.             <V4L001>
           MOVE ZEROES                 TO WSAA-PRMDEPST.                <V4L001>
           PERFORM R200-UPDATE-APA.                                     <V4L001>
           MOVE RLPDLON-PRMDEPST       TO WSAA-PRMDEPST.                <V4L001>
           IF RLPDLON-PRMDEPST         > ZEROES                         <V4L001>
              MOVE 'Y'                 TO WSAA-SUSP-IND                 <V4L001>
           END-IF.                                                      <V4L001>
      *    Store the contract agent number, to be used later, if        <069>
      *    the agent cedes commision to other agents.                   <069>
      *                                                                 <069>
           MOVE CHDRLNB-AGNTNUM        TO WSAA-AGCM-CEDAGENT.           <069>
                                                                        <069>
           MOVE SPACES                 TO PAYR-DATA-AREA.
           MOVE CHDRLNB-CHDRCOY        TO PAYR-CHDRCOY.
           MOVE CHDRLNB-CHDRNUM        TO PAYR-CHDRNUM.
           MOVE 1                      TO PAYR-VALIDFLAG.
           MOVE 1                      TO PAYR-PAYRSEQNO.
           MOVE PAYRREC                TO PAYR-FORMAT.
           MOVE BEGN                   TO PAYR-FUNCTION.

           CALL 'PAYRIO' USING PAYR-PARAMS.

           IF PAYR-STATUZ NOT = O-K
              MOVE PAYR-STATUZ         TO SYSR-STATUZ
              MOVE PAYR-PARAMS         TO SYSR-PARAMS
              PERFORM XXXX-FATAL-ERROR.

           PERFORM 1700-LOAD-PAYER-DETAILS UNTIL
               PAYR-CHDRCOY NOT = CHDRLNB-CHDRCOY OR
               PAYR-CHDRNUM NOT = CHDRLNB-CHDRNUM OR
               PAYR-STATUZ = ENDP.

      *
       1090-EXIT.
           EXIT.
      /
      *
      ***************************************************************
      * Read the contract head.                                     *
      ***************************************************************
      *
       1100-READ-CONTRACT-HEADER SECTION.
      ***********************************
       1110-READ-CONTRACT-HEADER.
      *
           MOVE ATMD-PRIMARY-KEY       TO WSAA-PRIMARY-KEY.

           MOVE SPACES                 TO CHDRLNB-DATA-AREA.
           MOVE ATMD-COMPANY           TO CHDRLNB-CHDRCOY.
           MOVE WSAA-PRIMARY-CHDRNUM   TO CHDRLNB-CHDRNUM.
           MOVE READS                  TO CHDRLNB-FUNCTION.

           CALL 'CHDRLNBIO'            USING CHDRLNB-PARAMS.
           IF CHDRLNB-STATUZ           NOT = O-K
              MOVE CHDRLNB-PARAMS      TO SYSR-PARAMS
              MOVE CHDRLNB-STATUZ      TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
                                                                        <DA005>
TDO   * Save Servicing Agent Code:                                      <DA005>
           MOVE CHDRLNB-AGNTNUM        TO WSAA-SERVAG.                  <DA005>
                                                                        <DA024>
           MOVE CHDRLNB-CHDRCOY        TO ZRAG-CHDRCOY.                 <DA024>
           MOVE CHDRLNB-CHDRNUM        TO ZRAG-CHDRNUM.                 <DA024>
           CALL 'ZCHKRAG'              USING ZRAG-ZCHKRAG-REC.          <DA024>
           IF  ZRAG-STATUZ             = O-K                            <DA024>
           AND ZRAG-AGNTNUM        NOT = SPACES                         <DA024>
               MOVE ZRAG-AGNTNUM       TO WSAA-SERVAG                   <DA024>
           END-IF.                                                      <DA024>
                                                                        <DA024>
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
              PERFORM XXXX-FATAL-ERROR                                  <CAS1.0>
           END-IF.                                                      <CAS1.0>
      *
       1190-EXIT.
           EXIT.
      /
      *
       1150-READ-TABLE-T5729 SECTION.                                   <D9604>
       1151-START.                                                      <D9604>
                                                                        <D9604>
           SET NOT-FLEXIBLE-PREMIUM-CONTRACT                            <D9604>
                                       TO TRUE.                         <D9604>
                                                                        <D9604>
      *  Read T5729.                                            <D9604>
                                                                        <D9604>
           MOVE O-K                    TO ITDM-STATUZ.                  <D9604>
           MOVE CHDRLNB-CHDRCOY        TO ITDM-ITEMCOY.                 <D9604>
           MOVE T5729                  TO ITDM-ITEMTABL.                <D9604>
           MOVE CHDRLNB-CNTTYPE        TO ITDM-ITEMITEM.                <D9604>
           MOVE 0                      TO ITDM-ITMTO.                   <D9604>
           MOVE CHDRLNB-OCCDATE        TO ITDM-ITMFRM.                  <D9604>
           MOVE BEGN                   TO ITDM-FUNCTION.                <D9604>
                                                                        <D9604>
           CALL 'ITDMIO' USING         ITDM-PARAMS.                     <D9604>
                                                                        <D9604>
           IF ITDM-STATUZ              NOT = O-K                        <D9604>
           AND ITDM-STATUZ             NOT = ENDP                       <D9604>
              STRING                   CHDRLNB-CHDRCOY                  <D9604>
                                       T5729                            <D9604>
                                       CHDRLNB-CNTTYPE                  <D9604>
                DELIMITED BY SPACE                                      <D9604>
                INTO SYSR-PARAMS                                        <D9604>
              END-STRING                                                <D9604>
              MOVE ITDM-STATUZ         TO SYSR-STATUZ                   <D9604>
              PERFORM XXXX-FATAL-ERROR                                  <D9604>
           END-IF.                                                      <D9604>
                                                                        <D9604>
           IF ITDM-STATUZ                  = ENDP                       <D9604>
           OR ITDM-ITEMCOY             NOT = CHDRLNB-CHDRCOY            <D9604>
           OR ITDM-ITEMTABL            NOT = T5729                      <D9604>
           OR ITDM-ITEMITEM            NOT = CHDRLNB-CNTTYPE            <D9604>
           OR ITDM-ITMFRM                  > CHDRLNB-OCCDATE            <D9604>
           OR ITDM-ITMTO                   < CHDRLNB-OCCDATE            <D9604>
              GO TO 1159-EXIT                                           <D9604>
           END-IF.                                                      <D9604>
                                                                        <D9604>
           MOVE ITDM-GENAREA        TO T5729-T5729-REC.                 <D9604>
           SET FLEXIBLE-PREMIUM-CONTRACT TO TRUE.                       <D9604>
                                                                        <D9604>
       1159-EXIT.                                                       <D9604>
           EXIT.                                                        <D9604>
                                                                        <D9604>
       1180-READ-TABLE-TH605 SECTION.                                   <V70L01>
       1181-START.                                                      <V70L01>
                                                                        <V70L01>
      *  Read TH605 to see if bonus workbench extraction used.          <V70L01>
           MOVE SPACES                 TO ITEM-DATA-KEY.                <V70L01>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <V70L01>
           MOVE ATMD-COMPANY           TO ITEM-ITEMCOY.                 <V70L01>
           MOVE TH605                  TO ITEM-ITEMTABL.                <V70L01>
           MOVE ATMD-COMPANY           TO ITEM-ITEMITEM.                <V70L01>
           MOVE READR                  TO ITEM-FUNCTION.                <V70L01>
                                                                        <V70L01>
           CALL 'ITEMIO'               USING ITEM-PARAMS.               <V70L01>
                                                                        <V70L01>
      *    IF ITEM-STATUZ           NOT = O-K AND MRNF          <V70L01><V73L01>
           IF ITEM-STATUZ           NOT = O-K                           <V73L01>
              MOVE ITEM-STATUZ         TO SYSR-STATUZ                   <V70L01>
              MOVE ITEM-PARAMS         TO SYSR-PARAMS                   <V70L01>
              PERFORM XXXX-FATAL-ERROR                                  <V70L01>
           END-IF.                                                      <V70L01>
      *                                                                 <V70L01>
           MOVE ITEM-GENAREA           TO TH605-TH605-REC.              <V70L01>
                                                                        <V70L01>
       1181-EXIT.                                                       <V70L01>
           EXIT.                                                        <V70L01>
      ***************************************************************
      * Call DATCON1 subroutine.                                    *
      ***************************************************************
      *
       1200-CALL-DATCON1 SECTION.
      ***************************
       1210-CALL-DATCON1.
      *
           MOVE TDAY                   TO DTC1-FUNCTION.
           CALL 'DATCON1' USING DTC1-DATCON1-REC.
      *
           IF DTC1-STATUZ              NOT = O-K
              MOVE DTC1-DATCON1-REC    TO SYSR-PARAMS
              MOVE DTC1-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.

       1290-EXIT.
           EXIT.
      /
      *
      ***************************************************************
      * Read the status code table.                                 *
      ***************************************************************
      *
       1300-READ-STATUS-CODES SECTION.
      ********************************
       1310-READ-STATUS-CODES.
      *
           MOVE ATMD-BATCH-KEY         TO WSAA-BATCKEY.
           MOVE SPACES                 TO ITEM-DATA-KEY.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE ATMD-COMPANY           TO ITEM-ITEMCOY.
           MOVE T5679                  TO ITEM-ITEMTABL.
           MOVE WSKY-BATC-BATCTRCDE    TO ITEM-ITEMITEM.
           MOVE 'READR'                TO ITEM-FUNCTION.
      *
           CALL 'ITEMIO' USING ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = O-K
               MOVE ITEM-PARAMS        TO SYSR-PARAMS
               MOVE ITEM-STATUZ        TO SYSR-STATUZ
               PERFORM XXXX-FATAL-ERROR.

           MOVE ITEM-GENAREA TO T5679-T5679-REC.
      *
       1390-EXIT.
           EXIT.
      /
      *
      ***************************************************************
      * Clear all the accumulators.                                 *
      ***************************************************************
      *
       1400-CALC-SUSPENSE SECTION.
      ****************************
       1410-CALC-SUSPENSE.

      *
      *  Read financial accounting rules
      *
           MOVE SPACES                 TO ITEM-DATA-KEY.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE ATMD-COMPANY           TO ITEM-ITEMCOY.
           MOVE T5645                  TO ITEM-ITEMTABL.
           MOVE 'P5074'                TO ITEM-ITEMITEM.
           MOVE  READR                 TO ITEM-FUNCTION.
      *
           CALL 'ITEMIO' USING ITEM-PARAMS.

           IF  ITEM-STATUZ         NOT = O-K
               MOVE ITEM-PARAMS        TO SYSR-PARAMS
               PERFORM XXXX-FATAL-ERROR.
           MOVE ITEM-GENAREA TO T5645-T5645-REC.

      *****
      *    Read the table T3695 for the field sign of the 01 posting.
      *****
           MOVE SPACES                 TO ITEM-DATA-KEY.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE ATMD-COMPANY           TO ITEM-ITEMCOY.
           MOVE T3695                  TO ITEM-ITEMTABL.
           MOVE T5645-SACSTYPE-01      TO ITEM-ITEMITEM.
           MOVE 'READR'                TO ITEM-FUNCTION.

           CALL 'ITEMIO' USING ITEM-PARAMS.

           IF  ITEM-STATUZ         NOT = O-K
               MOVE ITEM-PARAMS        TO SYSR-PARAMS
               PERFORM XXXX-FATAL-ERROR.
           MOVE ITEM-GENAREA TO T3695-T3695-REC.

      *    Read  the sub account balance for  the contract and apply
      *    the sign for the sub account type.
      *    Sub Account search enhanced to look for Suspense payment     <CAS1.0>
      *    in Contract Currency, Billing Currency or any currency.      <CAS1.0>

      **** MOVE SPACES                 TO ACBL-DATA-AREA.               <CAS1.0>
      **** MOVE ZEROS                  TO ACBL-RLDGACCT.                <CAS1.0>
      **** MOVE ATMD-COMPANY           TO ACBL-RLDGCOY.                 <CAS1.0>
      **** MOVE CHDRLNB-CHDRNUM        TO ACBL-RLDGACCT.                <CAS1.0>
      **** MOVE CHDRLNB-CNTCURR        TO ACBL-ORIGCURR.                <CAS1.0>
      **** MOVE T5645-SACSCODE-01      TO ACBL-SACSCODE.                <CAS1.0>
      **** MOVE T5645-SACSTYPE-01      TO ACBL-SACSTYP.                 <CAS1.0>
      **** MOVE READR                  TO ACBL-FUNCTION.                <CAS1.0>

      **** CALL 'ACBLIO'               USING ACBL-PARAMS.               <CAS1.0>

      **** IF  (ACBL-STATUZ         NOT = O-K)                          <CAS1.0>
      **** AND (ACBL-STATUZ         NOT = MRNF)                         <CAS1.0>
      ****    MOVE ACBL-PARAMS      TO SYSR-PARAMS                      <CAS1.0>
      ****    PERFORM XXXX-FATAL-ERROR.                                 <CAS1.0>

      **** IF ACBL-STATUZ               = MRNF                          <CAS1.0>
      ****    MOVE ZERO                 TO WSAA-CNT-SUSPENSE            <CAS1.0>
      **** ELSE                                                         <CAS1.0>
      ****    IF T3695-SIGN             = '-'                           <CAS1.0>
      ****       MULTIPLY ACBL-SACSCURBAL BY -1                         <CAS1.0>
      ****                              GIVING WSAA-CNT-SUSPENSE        <CAS1.0>
      ****    ELSE                                                      <CAS1.0>
      ****        MOVE ACBL-SACSCURBAL  TO WSAA-CNT-SUSPENSE.           <CAS1.0>

           MOVE ZERO                   TO WSAA-CNT-SUSPENSE.            <CAS1.0>
           MOVE 'N'                    TO WSAA-SUSP-IND.                <CAS1.0>
                                                                        <CAS1.0>
           MOVE SPACES                 TO ACBLENQ-DATA-AREA.            <CAS1.0>
           MOVE ZEROS                  TO ACBLENQ-RLDGACCT.             <CAS1.0>
           MOVE ATMD-COMPANY           TO ACBLENQ-RLDGCOY.              <CAS1.0>
           MOVE CHDRLNB-CHDRNUM        TO ACBLENQ-RLDGACCT.             <CAS1.0>
           MOVE T5645-SACSCODE-01      TO ACBLENQ-SACSCODE.             <CAS1.0>
           MOVE T5645-SACSTYPE-01      TO ACBLENQ-SACSTYP.              <CAS1.0>
                                                                        <CAS1.0>
           PERFORM 1400A-CHECK-SUSPENSE   VARYING WSAA-SUB              <CAS1.0>
                                FROM 1 BY 1 UNTIL WSAA-SUB > 3  OR      <CAS1.0>
                                          WSAA-SUSP-IND = 'Y'.          <CAS1.0>
                                                                        <CAS1.0>
       1499-EXIT.                                                       <CAS1.0>
           EXIT.                                                        <CAS1.0>
      /                                                                 <CAS1.0>
      ******************************                                    <CAS1.0>
       1400A-CHECK-SUSPENSE SECTION.                                    <CAS1.0>
      ******************************                                    <CAS1.0>
       1410A-LOCATE-SUSPENSE.                                           <CAS1.0>
      *                                                                 <CAS1.0>
      *    Read the Suspense file to see if any money has been          <CAS1.0>
      *    received for this contract. The search order for Suspense    <CAS1.0>
      *    details is Contract Currency; Billing Currency; Any          <CAS1.0>
      *    Currency. If Suspense is found set appropriate values.       <CAS1.0>
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
                 PERFORM XXXX-FATAL-ERROR                               <CAS1.0>
           END-IF.                                                      <CAS1.0>
                                                                        <CAS1.0>
           IF (ACBLENQ-STATUZ           = O-K              )  AND       <CAS1.0>
              (ACBLENQ-RLDGCOY          = ATMD-COMPANY     )  AND       <CAS1.0>
              (ACBLENQ-RLDGACCT         = CHDRLNB-CHDRNUM  )  AND       <CAS1.0>
              (ACBLENQ-SACSCODE         = T5645-SACSCODE-01)  AND       <CAS1.0>
              (ACBLENQ-SACSTYP          = T5645-SACSTYPE-01)  AND       <CAS1.0>
              (ACBLENQ-SACSCURBAL   NOT = ZERO             )            <CAS1.0>
                 MOVE 'Y'              TO WSAA-SUSP-IND                 <CAS1.0>
                                                                        <CAS1.0>
                 IF T3695-SIGN          = '-'                           <CAS1.0>
                    MULTIPLY ACBLENQ-SACSCURBAL BY -1                   <CAS1.0>
                                         GIVING WSAA-CNT-SUSPENSE       <CAS1.0>
                 ELSE                                                   <CAS1.0>
                    MOVE ACBLENQ-SACSCURBAL  TO WSAA-CNT-SUSPENSE       <CAS1.0>
                 END-IF                                                 <CAS1.0>
                                                                        <CAS1.0>
           END-IF.                                                      <CAS1.0>
                                                                        <CAS1.0>
       1499A-EXIT.                                                      <CAS1.0>
           EXIT.                                                        <CAS1.0>

      /
      ***************************************************************
      * Clear all the accumulators.                                 *
      ***************************************************************
      *
       1500-CLEAR-FIELDS SECTION.
      ***************************
       1510-CLEAR-FIELDS.
      *

           MOVE 0                      TO WSAA-STAMP-DUTY-ACC,
                                          WSAA-TOT-SINGP,
                                          WSAA-TOT-REGP-ADJ,
                                          WSAA-TOT-TOLERANCE.

           MOVE 1                      TO WSBB-SUB.
           MOVE 0                      TO PRAS-TAXRELAMT.
           MOVE ZERO                   TO WSAA-INSTPREM-TOT             <CAS1.0>
                                          WSAA-INSTPREM-TOT-XGE.        <CAS1.0>

           PERFORM UNTIL WSBB-SUB > 9
              MOVE ZEROS               TO WSAA-INCOME-SEQ-NO(WSBB-SUB),
                                          WSAA-BILLCD(WSBB-SUB),
                                          WSAA-BTDATE(WSBB-SUB),
                                          WSAA-REG-PREM-ACC(WSBB-SUB),
                                          WSAA-REG-PREM-ADJ(WSBB-SUB),
                                          WSAA-SING-PREM-ACC(WSBB-SUB),
                                          WSAA-INSTPREM(WSBB-SUB),
                                          WSAA-FREQ-FACTOR(WSBB-SUB),
                                          WSAA-RP-TAX(WSBB-SUB),        <V74L01>
                                          WSAA-SP-TAX(WSBB-SUB),        <V74L01>
                                          WSAA-FE-TAX(WSBB-SUB),        <V74L01>
                                          WSAA-TAXRELAMT(WSBB-SUB)

              MOVE SPACES              TO WSAA-BILLFREQ(WSBB-SUB),
                                          WSAA-BILLCHNL(WSBB-SUB),
                                          WSAA-BILLCURR(WSBB-SUB),
                                          WSAA-CLNTNUM(WSBB-SUB),
                                          WSAA-CLNTCOY(WSBB-SUB),
                                          WSAA-INREVNUM(WSBB-SUB)
              ADD 1                    TO WSBB-SUB
           END-PERFORM.

           MOVE SPACE                  TO WSAA-NO-SUMMARY-REC.
           PERFORM 1520-CLEAR-COMM-TABLE
                                       VARYING WSAA-SUB
                                       FROM 1 BY 1
                                       UNTIL WSAA-SUB > 10.
           MOVE 0                      TO WSAA-SUB.
           MOVE ATMD-TRANS-AREA        TO WSAA-TRANSACTION-REC.
           GO TO 1590-EXIT.
      *
       1520-CLEAR-COMM-TABLE.
           MOVE 0                      TO WSAA-SPLIT-BCOMM(WSAA-SUB),
                                          WSAA-COMM-DUE(WSAA-SUB),
                                          WSAA-COMM-EARN(WSAA-SUB),
                                          WSAA-COMM-PAID(WSAA-SUB),
                                          WSAA-SERV-DUE(WSAA-SUB),
                                          WSAA-SERV-EARN(WSAA-SUB),
                                          WSAA-RENL-DUE(WSAA-SUB),
                                          WSAA-ANNPREM(WSAA-SUB),       <V73L01>
                                          WSAA-SINGP(WSAA-SUB),         <V73L01>
                                          WSAA-RENL-EARN(WSAA-SUB).
           MOVE 0                      TO WSAA-TCOM-DUE(WSAA-SUB).      <V6L000>
           PERFORM 1550-CLEAR-OVERRIDE  VARYING WSAA-AGENT-SUB
                                        FROM 1 BY 1
                                        UNTIL WSAA-AGENT-SUB > 10.
      *
       1550-CLEAR-OVERRIDE.
           MOVE SPACES TO WSAA-OVERRIDE-AGNTNUM
                                           (WSAA-SUB WSAA-AGENT-SUB).
           MOVE ZEROS TO WSAA-OVERRIDE-COMM(WSAA-SUB WSAA-AGENT-SUB).
           MOVE SPACES TO WSAA-CEDAGENT
                                           (WSAA-SUB WSAA-AGENT-SUB).
           MOVE ZEROS TO WSAA-OVRD-COMM-PAID(WSAA-SUB WSAA-AGENT-SUB).

       1590-EXIT.
           EXIT.
      /

   ****1600-CHECK-FREQ-DATES SECTION.                                   <068>
   **********************************                                   <068>
   ****                                                                 <068>
   ****    Code for checking half-yearly dates to allocate         <067><068>
   ****    frequency factors is no longer needed, as DATCON3       <067><068>
   ****    now correctly calculates these.                         <067><068>
   ****                                                            <067><068>
   ****                                                                 <068>
   ****    we are only interested in monthly or half yearly             <068>
   ****    payment frequencies                                          <068>
   ****                                                                 <068>
      **** IF CHDRLNB-BILLFREQ  = '12'                                  <067>
      *****IF WSAA-BILLFREQ(WSBB-SUB)  = '12'                           <067>
      *****    GO TO 1630-CHECK-MONTHLY-DATES.                          <067>
      *****                                                             <067>
      **** IF CHDRLNB-BILLFREQ NOT = '02'                               <067>
      *****IF WSAA-BILLFREQ(WSBB-SUB) NOT = '02'                        <067>
   ****    IF WSAA-BILLFREQ(WSBB-SUB) NOT = '12'                   <067><068>
   ****        GO TO 1690-EXIT.                                         <068>

      *
      *****IF ((WSAA-PREM-MTH1    = 02) AND                             <067>
      *****    (WSAA-PREM-DAY1    > 27) AND                             <067>
      *****    (WSAA-PREM-DAY1    < 30)) AND                            <067>
      *****   ((WSAA-PREM-DAY2    = 31) AND                             <067>
      *****    (WSAA-PREM-MTH2    = 08))                                <067>
      *****    MOVE 1             TO WSAA-FREQ-FACTOR(WSBB-SUB).        <067>
      *****                                                             <067>
      *****                                                             <067>
      *****IF ((WSAA-PREM-MTH1    = 03) AND                             <067>
      *****    (WSAA-PREM-DAY1    = 30)) AND                            <067>
      *****   ((WSAA-PREM-DAY2    = 30) AND                             <067>
      *****    (WSAA-PREM-MTH2    = 09))                                <067>
      *****    MOVE 1             TO WSAA-FREQ-FACTOR(WSBB-SUB).        <067>
      *****                                                             <067>
      *****IF ((WSAA-PREM-MTH1    = 04) AND                             <067>
      *****    (WSAA-PREM-DAY1    = 30)) AND                            <067>
      *****   ((WSAA-PREM-DAY2    = 31) AND                             <067>
      *****    (WSAA-PREM-MTH2    = 10))                                <067>
      *****    MOVE 1             TO WSAA-FREQ-FACTOR(WSBB-SUB).        <067>
      *****                                                             <067>
      *****IF ((WSAA-PREM-MTH1    = 05) AND                             <067>
      *****    (WSAA-PREM-DAY1    = 30)) AND                            <067>
      *****   ((WSAA-PREM-DAY2    = 30) AND                             <067>
      *****    (WSAA-PREM-MTH2    = 11))                                <067>
      *****    MOVE 1             TO WSAA-FREQ-FACTOR(WSBB-SUB).        <067>
      *****                                                             <067>
      *****IF ((WSAA-PREM-MTH1    = 06) AND                             <067>
      *****    (WSAA-PREM-DAY1    = 30)) AND                            <067>
      *****   ((WSAA-PREM-DAY2    = 31) AND                             <067>
      *****    (WSAA-PREM-MTH2    = 12))                                <067>
      *****    MOVE 1             TO WSAA-FREQ-FACTOR(WSBB-SUB).        <067>
      *****                                                             <067>
      *****IF ((WSAA-PREM-MTH1    = 08) AND                             <067>
      *****    (WSAA-PREM-DAY1    = 30)) AND                            <067>
      *****   ((WSAA-PREM-DAY2    = 28) AND                             <067>
      *****    (WSAA-PREM-MTH2    = 02))                                <067>
      *****    MOVE 1             TO WSAA-FREQ-FACTOR(WSBB-SUB).        <067>
      *****                                                             <067>
      *****IF ((WSAA-PREM-MTH1    = 09) AND                             <067>
      *****    (WSAA-PREM-DAY1    = 30)) AND                            <067>
      *****   ((WSAA-PREM-DAY2    = 31) AND                             <067>
      *****    (WSAA-PREM-MTH2    = 03))                                <067>
      *****    MOVE 1             TO WSAA-FREQ-FACTOR(WSBB-SUB).        <067>
      *****                                                             <067>
      *****IF ((WSAA-PREM-MTH1    = 10) AND                             <067>
      *****    (WSAA-PREM-DAY1    = 30)) AND                            <067>
      *****   ((WSAA-PREM-DAY2    = 30) AND                             <067>
      *****    (WSAA-PREM-MTH2    = 04))                                <067>
      *****    MOVE 1             TO WSAA-FREQ-FACTOR(WSBB-SUB).        <067>
      *****                                                             <067>
      *****IF ((WSAA-PREM-MTH1    = 11) AND                             <067>
      *****    (WSAA-PREM-DAY1    = 30)) AND                            <067>
      *****   ((WSAA-PREM-DAY2    = 31) AND                             <067>
      *****    (WSAA-PREM-MTH2    = 05))                                <067>
      *****    MOVE 1             TO WSAA-FREQ-FACTOR(WSBB-SUB).        <067>
      *****                                                             <067>
      *****IF ((WSAA-PREM-MTH1    = 12) AND                             <067>
      *****    (WSAA-PREM-DAY1    = 30)) AND                            <067>
      *****   ((WSAA-PREM-DAY2    = 30) AND                             <067>
      *****    (WSAA-PREM-MTH2    = 06))                                <067>
      *****    MOVE 1             TO WSAA-FREQ-FACTOR(WSBB-SUB).        <067>
      *****                                                             <067>
      *****GO TO 1690-EXIT.                                             <067>
      *
      *1630-CHECK-MONTHLY-DATES.                                        <073>
      *

   ****    IF ((WSAA-PREM-MTH1    = 01) AND                             <068>
   ****        (WSAA-PREM-DAY1    > 27)  AND                            <068>
   ****        (WSAA-PREM-DAY1    < 31)) AND                            <068>
   ****       ((WSAA-PREM-DAY2    = 28) AND                             <068>
   ****        (WSAA-PREM-MTH2    = 02))                                <068>
   ****        MOVE 1             TO WSAA-FREQ-FACTOR(WSBB-SUB).        <068>
   ****                                                                 <068>
   ****    IF ((WSAA-PREM-MTH1    = 03) AND                             <068>
   ****        (WSAA-PREM-DAY1    = 30)) AND                            <068>
   ****       ((WSAA-PREM-DAY2    = 30) AND                             <068>
   ****        (WSAA-PREM-MTH2    = 04))                                <068>
   ****        MOVE 1             TO WSAA-FREQ-FACTOR(WSBB-SUB).        <068>
   ****                                                                 <068>
   ****    IF ((WSAA-PREM-MTH1    = 05) AND                             <068>
   ****        (WSAA-PREM-DAY1    = 30)) AND                            <068>
   ****       ((WSAA-PREM-DAY2    = 30) AND                             <068>
   ****        (WSAA-PREM-MTH2    = 06))                                <068>
   ****        MOVE 1             TO WSAA-FREQ-FACTOR(WSBB-SUB).        <068>
   ****                                                                 <068>
   ****    IF ((WSAA-PREM-MTH1    = 08) AND                             <068>
   ****        (WSAA-PREM-DAY1    = 30)) AND                            <068>
   ****       ((WSAA-PREM-DAY2    = 30) AND                             <068>
   ****        (WSAA-PREM-MTH2    = 09))                                <068>
   ****        MOVE 1             TO WSAA-FREQ-FACTOR(WSBB-SUB).        <068>
   ****                                                                 <068>
   ****    IF ((WSAA-PREM-MTH1    = 10) AND                             <068>
   ****        (WSAA-PREM-DAY1    = 30)) AND                            <068>
   ****       ((WSAA-PREM-DAY2    = 30) AND                             <068>
   ****        (WSAA-PREM-MTH2    = 11))                                <068>
   ****        MOVE 1             TO WSAA-FREQ-FACTOR(WSBB-SUB).        <068>
   ****                                                                 <068>
   ****1690-EXIT.                                                       <068>
   ****     EXIT.                                                       <068>
   ****                                                                 <068>
      *
       1700-LOAD-PAYER-DETAILS SECTION.
      **********************************
       1710-LOAD-PAYER-DETAILS.

      *
      * Read the client role file to get the payer number.
      *

           MOVE CHDRLNB-CHDRPFX    TO CLRF-FOREPFX.
           MOVE PAYR-CHDRCOY       TO CLRF-FORECOY.
           MOVE PAYR-CHDRNUM       TO WSAA-CHDRNUM.
           MOVE PAYR-PAYRSEQNO     TO WSAA-PAYRSEQNO.
           MOVE WSAA-PAYRKEY       TO CLRF-FORENUM.
           MOVE 'PY'               TO CLRF-CLRRROLE.
           MOVE READR              TO CLRF-FUNCTION.

           CALL 'CLRFIO' USING CLRF-PARAMS.

           IF CLRF-STATUZ NOT = O-K
              MOVE CLRF-STATUZ     TO SYSR-STATUZ
              MOVE CLRF-PARAMS     TO SYSR-PARAMS
              PERFORM XXXX-FATAL-ERROR.

      * Load the working storage table using the payer sequence
      * number as the subscript.

           MOVE PAYR-PAYRSEQNO     TO WSBB-SUB.
           MOVE PAYR-INCOME-SEQ-NO TO WSAA-INCOME-SEQ-NO(WSBB-SUB).
           MOVE PAYR-BILLFREQ      TO WSAA-BILLFREQ(WSBB-SUB).
           MOVE PAYR-BILLCHNL      TO WSAA-BILLCHNL(WSBB-SUB).
           MOVE PAYR-BILLCD        TO WSAA-BILLCD(WSBB-SUB).
           MOVE PAYR-BTDATE        TO WSAA-BTDATE(WSBB-SUB).
           MOVE PAYR-BILLCURR      TO WSAA-BILLCURR(WSBB-SUB).
           MOVE CLRF-CLNTNUM       TO WSAA-CLNTNUM(WSBB-SUB).
           MOVE CLRF-CLNTCOY       TO WSAA-CLNTCOY(WSBB-SUB).
           MOVE PAYR-BILLDAY       TO WSAA-BILLDAY(WSBB-SUB).           <LA5139>
           MOVE PAYR-BILLMONTH     TO WSAA-BILLMONTH(WSBB-SUB).         <LA5139>

      * Work out how many instalments are to be paid at issue.

           IF PAYR-BILLFREQ         = '00'
              MOVE 1                TO WSAA-FREQ-FACTOR(WSBB-SUB).

           IF PAYR-BILLFREQ            NOT = '00'
           AND PAYR-BTDATE             NOT = CHDRLNB-OCCDATE            <067>
              MOVE CHDRLNB-OCCDATE        TO DTC3-INT-DATE-1
              MOVE PAYR-BTDATE            TO DTC3-INT-DATE-2
              MOVE PAYR-BILLFREQ          TO DTC3-FREQUENCY

              CALL 'DATCON3' USING DTC3-DATCON3-REC

              IF DTC3-STATUZ           NOT = O-K
                 MOVE DTC3-DATCON3-REC TO SYSR-PARAMS
                 MOVE DTC3-STATUZ      TO SYSR-STATUZ
                 PERFORM XXXX-FATAL-ERROR
              END-IF
              MOVE DTC3-FREQ-FACTOR    TO WSAA-FREQ-FACTOR(WSBB-SUB)

      * Store the first payer's frequency factor to adjust
      * the contract fee later.

              IF PAYR-PAYRSEQNO = 1
                 MOVE DTC3-FREQ-FACTOR  TO WSAA-FEE-FREQ
              END-IF

      * if we are dealing with unusual dates, set up a predetermined
      *  factor(for details of dates refer to sdf 2095)

   ****       MOVE CHDRLNB-OCCDATE     TO WSAA-PREM-DATE1               <068>
   ****       MOVE PAYR-BILLCD         TO WSAA-PREM-DATE2               <068>
   ****       PERFORM 1600-CHECK-FREQ-DATES                             <068>
           END-IF.

      *  Read the next payr record.

           MOVE NEXTR              TO PAYR-FUNCTION.
           CALL 'PAYRIO'           USING PAYR-PARAMS.

           IF PAYR-STATUZ NOT = O-K AND
                          NOT = ENDP
              MOVE PAYR-STATUZ     TO SYSR-STATUZ
              MOVE PAYR-PARAMS     TO SYSR-PARAMS
              PERFORM XXXX-FATAL-ERROR.
       1790-EXIT.
           EXIT.
      /
      *
      ***************************************************************
      * Do everything.                                              *
      ***************************************************************
      *
       2000-PROCESS SECTION.
      **********************
       2010-PROCESS.
      *
      * read t5688

           MOVE SPACES                 TO ITDM-DATA-AREA.
           MOVE CHDRLNB-CHDRCOY        TO ITDM-ITEMCOY.
           MOVE T5688                  TO ITDM-ITEMTABL.
           MOVE CHDRLNB-CNTTYPE        TO ITDM-ITEMITEM.
           MOVE CHDRLNB-OCCDATE        TO ITDM-ITMFRM.
           MOVE BEGN                   TO ITDM-FUNCTION.
           CALL 'ITDMIO' USING         ITDM-PARAMS.
           IF ITDM-STATUZ              NOT = O-K AND NOT = ENDP
              MOVE ITDM-PARAMS         TO SYSR-PARAMS
              MOVE ITDM-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
           IF ITDM-ITEMCOY             NOT = CHDRLNB-CHDRCOY
           OR ITDM-ITEMTABL            NOT = T5688
           OR ITDM-ITEMITEM            NOT = CHDRLNB-CNTTYPE
           OR ITDM-STATUZ              = ENDP
              MOVE CHDRLNB-CNTTYPE     TO SYSR-PARAMS
              MOVE E308                TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR
           ELSE
              MOVE ITDM-GENAREA        TO T5688-T5688-REC.

           MOVE T5688-COMLVLACC        TO WSAA-ACCT-LEVEL.

       2020-ACCOUNTING-RULES.
      *    Read first sequence of T5645 first to determine override
      *    commission details and tax relief details.
      *
           MOVE SPACES                 TO ITEM-DATA-AREA.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE CHDRLNB-CHDRCOY        TO ITEM-ITEMCOY.
           MOVE T5645                  TO ITEM-ITEMTABL.
           MOVE 'P5074'                TO ITEM-ITEMITEM.
           MOVE '01'                   TO ITEM-ITEMSEQ.
           MOVE READR                  TO ITEM-FUNCTION.
           CALL 'ITEMIO' USING         ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = O-K
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              MOVE ITEM-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
           MOVE ITEM-GENAREA           TO T5645-T5645-REC.
      *
      * Set up array containing 2nd page of T5645 entries
      *
           MOVE ZERO                   TO WSAA-COUNT.

           PERFORM UNTIL WSAA-COUNT = 15

               ADD 1            TO WSAA-COUNT

               MOVE T5645-CNTTOT ( WSAA-COUNT )
                                TO WSAA-T5645-CNTTOT ( WSAA-COUNT )
               MOVE T5645-SACSCODE ( WSAA-COUNT )
                                TO WSAA-T5645-SACSCODE ( WSAA-COUNT )
               MOVE T5645-SACSTYPE ( WSAA-COUNT )
                                TO WSAA-T5645-SACSTYPE ( WSAA-COUNT )
               MOVE T5645-GLMAP ( WSAA-COUNT )
                                TO WSAA-T5645-GLMAP ( WSAA-COUNT )
               MOVE T5645-SIGN  ( WSAA-COUNT )
                                TO WSAA-T5645-SIGN ( WSAA-COUNT )

           END-PERFORM.

PHL108*    READ THIRD SEQUENCE OF T5645                                 <PHL108>
      *                                                                 <PHL108>
           MOVE SPACES                 TO ITEM-DATA-AREA.               <PHL108>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <PHL108>
           MOVE CHDRLNB-CHDRCOY        TO ITEM-ITEMCOY.                 <PHL108>
           MOVE T5645                  TO ITEM-ITEMTABL.                <PHL108>
           MOVE 'P5074'                TO ITEM-ITEMITEM.                <PHL108>
           MOVE '02'                   TO ITEM-ITEMSEQ.                 <PHL108>
           MOVE READR                  TO ITEM-FUNCTION.                <PHL108>
           CALL 'ITEMIO' USING         ITEM-PARAMS.                     <PHL108>
           IF ITEM-STATUZ              NOT = O-K                        <PHL108>
              MOVE ITEM-PARAMS         TO SYSR-PARAMS                   <PHL108>
              MOVE ITEM-STATUZ         TO SYSR-STATUZ                   <PHL108>
              PERFORM XXXX-FATAL-ERROR.                                 <PHL108>
           MOVE ITEM-GENAREA           TO T5645-T5645-REC.              <PHL108>
      *                                                                 <PHL108>
      * SET UP ARRAY CONTAINING 3RD PAGE OF T5645 ENTRIES               <PHL108>
      *                                                                 <PHL108>
           MOVE ZERO                   TO WSAA-COUNT.                   <PHL108>
                                                                        <PHL108>
           PERFORM UNTIL WSAA-COUNT = 15                                <PHL108>
                                                                        <PHL108>
               ADD 1            TO WSAA-COUNT                           <PHL108>
                                                                        <PHL108>
               MOVE T5645-CNTTOT ( WSAA-COUNT )                         <PHL108>
                          TO WSAA-T5645-CNTTOT-02 ( WSAA-COUNT )        <PHL108>
               MOVE T5645-SACSCODE ( WSAA-COUNT )                       <PHL108>
                          TO WSAA-T5645-SACSCODE-02 ( WSAA-COUNT )      <PHL108>
               MOVE T5645-SACSTYPE ( WSAA-COUNT )                       <PHL108>
                          TO WSAA-T5645-SACSTYPE-02 ( WSAA-COUNT )      <PHL108>
               MOVE T5645-GLMAP ( WSAA-COUNT )                          <PHL108>
                          TO WSAA-T5645-GLMAP-02 ( WSAA-COUNT )         <PHL108>
               MOVE T5645-SIGN  ( WSAA-COUNT )                          <PHL108>
                                TO WSAA-T5645-SIGN-02 ( WSAA-COUNT )    <PHL108>
                                                                        <PHL108>
           END-PERFORM.                                                 <PHL108>
                                                                        <PHL108>
                                                                        <PHL108>
                                                                        <PHL108>
      *    Read other sequence of T5645.
      *
           MOVE SPACES                 TO ITEM-DATA-AREA.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE CHDRLNB-CHDRCOY        TO ITEM-ITEMCOY.
           MOVE T5645                  TO ITEM-ITEMTABL.
           MOVE 'P5074'                TO ITEM-ITEMITEM.
           MOVE SPACES                 TO ITEM-ITEMSEQ.
           MOVE READR                  TO ITEM-FUNCTION.
           CALL 'ITEMIO' USING         ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = O-K
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              MOVE ITEM-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
           MOVE ITEM-GENAREA           TO T5645-T5645-REC.

      *    Read T5645 descriptions.
      *
           MOVE 'IT'                   TO DESC-DESCPFX.
           MOVE CHDRLNB-CHDRCOY        TO DESC-DESCCOY.
           MOVE T5645                  TO DESC-DESCTABL.
           MOVE ATMD-LANGUAGE          TO DESC-LANGUAGE.
           MOVE 'P5074'                TO DESC-DESCITEM
           MOVE READR                  TO DESC-FUNCTION.
           CALL 'DESCIO' USING DESC-PARAMS.
           IF DESC-STATUZ              NOT = O-K
           AND DESC-STATUZ             NOT = MRNF
               MOVE DESC-PARAMS        TO SYSR-PARAMS
               MOVE DESC-STATUZ        TO SYSR-STATUZ
               PERFORM XXXX-FATAL-ERROR.
      *
      *   If a description is not found in the signon language then
      *   English is used as a default.
      *
           IF DESC-STATUZ              =  MRNF
      ****    MOVE 'E'                 TO DESC-LANGUAGE                 <LA3998>
              MOVE ATMD-LANGUAGE       TO DESC-LANGUAGE                 <LA3998>
              MOVE READR               TO DESC-FUNCTION

              CALL 'DESCIO' USING DESC-PARAMS

              IF DESC-STATUZ           NOT = O-K
                  MOVE DESC-PARAMS     TO SYSR-PARAMS
                  MOVE DESC-STATUZ     TO SYSR-STATUZ
                  PERFORM XXXX-FATAL-ERROR
              END-IF

           END-IF.
      *
           PERFORM 2100-CONTRACT-HEADER-LEVEL.
           PERFORM 2200-LIVES-ASSURED.
           PERFORM 2300-COMPONENTS.
           PERFORM 2X00-UNDERWRITING-UNDL.                              <V71L12>
           PERFORM 2Y00-UNDERWRITING-UNDC.                              <V71L12>
           PERFORM 2Z00-UNDERWRITING-UNDQ.                              <V71L12>

           MOVE CHDRLNB-CHDRCOY        TO INCT-CHDRCOY.                 <070>
           MOVE CHDRLNB-CHDRNUM        TO INCT-CHDRNUM.                 <070>
           MOVE READH                  TO INCT-FUNCTION.                <070>
           CALL 'INCTIO'            USING INCT-PARAMS.                  <070>
           IF  INCT-STATUZ           NOT = O-K                          <070>
           AND INCT-STATUZ           NOT = MRNF                         <070>
              MOVE INCT-PARAMS      TO SYSR-PARAMS                      <070>
              MOVE INCT-STATUZ      TO SYSR-STATUZ                      <070>
              PERFORM XXXX-FATAL-ERROR.                                 <070>
                                                                        <070>
           IF  INCT-STATUZ               = O-K                          <070>
               MOVE DELET             TO INCT-FUNCTION                  <070>
               CALL 'INCTIO'       USING INCT-PARAMS                    <070>
               IF  INCT-STATUZ           NOT = O-K                      <070>
                   MOVE INCT-PARAMS      TO SYSR-PARAMS                 <070>
                   MOVE INCT-STATUZ      TO SYSR-STATUZ                 <070>
                   PERFORM XXXX-FATAL-ERROR.                            <070>
                                                                        <070>
           MOVE SPACES                 TO PAYR-DATA-AREA.
           MOVE CHDRLNB-CHDRCOY        TO PAYR-CHDRCOY.
           MOVE CHDRLNB-CHDRNUM        TO PAYR-CHDRNUM.
           MOVE 1                      TO PAYR-VALIDFLAG.
           MOVE 1                      TO PAYR-PAYRSEQNO.
           MOVE PAYRREC                TO PAYR-FORMAT.
           MOVE BEGN                   TO PAYR-FUNCTION.

           CALL 'PAYRIO' USING PAYR-PARAMS.

           IF PAYR-STATUZ NOT = O-K
              MOVE PAYR-STATUZ         TO SYSR-STATUZ
              MOVE PAYR-PARAMS         TO SYSR-PARAMS
              PERFORM XXXX-FATAL-ERROR.

           PERFORM 2350-UPDATE-PAYER UNTIL
               PAYR-CHDRCOY NOT = CHDRLNB-CHDRCOY OR
               PAYR-CHDRNUM NOT = CHDRLNB-CHDRNUM OR
               PAYR-STATUZ = ENDP.

           PERFORM 2360-GET-TOLERANCE.                                  <V42013>

           PERFORM 6000-PROC-CHILD.                                     <V71L05>
           PERFORM 2400-CONTRACT-HEADER-REV.
           PERFORM 2500-CONTRACT-ACCOUNTING.
           MOVE 1                      TO WSBB-SUB.
           PERFORM 2550-PAYER-ACCOUNTING UNTIL WSBB-SUB > 9 OR
                            WSAA-CLNTNUM(WSBB-SUB) = SPACES.

           MOVE 1                      TO WSAA-AGENT-SUB.
           PERFORM 2600-COMMISSION-ACCOUNTING.
           PERFORM 2A00-COMMISSION-HOLD.                                <V74L03>
      **** PERFORM 2700-REINSURANCE-ACCOUNTING.                         <070>
           PERFORM 2800-HOUSEKEEPING.
      **** PERFORM 3100-EXCESS-PREMIUM-ALLOCATION.                      <UL001>
      **** PERFORM 5000-WRITE-LETTER.                           <UL003> <081>
      *
       2090-EXIT.
           EXIT.
      /
      *
      ***************************************************************
      * Contract header level processing.                           *
      ***************************************************************
      *
       2100-CONTRACT-HEADER-LEVEL SECTION.
      ************************************
       2101-CONTRACT-HEADER-LEVEL.
      *
           PERFORM 2110-CONTRACT-HEADER-ROLES.
      **** PERFORM 2120-BENEFICIARY-ROLES.                              <V74F03>
           PERFORM 2130-COMMISSION-DETAILS.
      *
       2109-EXIT.
           EXIT.
      /
      *
      ***************************************************************
      * Update the role records.                                    *
      ***************************************************************
      *
       2110-CONTRACT-HEADER-ROLES SECTION.
      ************************************
       2111-CONTRACT-HEADER-ROLES.
      *

      * DISPATCH
      * If the despatch number is blank then use owner.
           IF CHDRLNB-DESPNUM          = SPACES
              MOVE CHDRLNB-COWNNUM     TO CLRN-CLNTNUM
           ELSE
      ****    MOVE CHDRLNB-DESPNUM     TO CLRN-CLNTNUM.                 <V74F03>
              GO TO 2111-SKIP-DESPATCH                                  <V74F03>
           END-IF.                                                      <V74F03>

           MOVE 'DA'                   TO CLRN-CLRRROLE.
           PERFORM 2113-WRITE-CLIENT-ROLE.

       2111-SKIP-DESPATCH.                                              <V74F03>
                                                                        <V74F03>
      * ASSIGNEE
      * If assignee is blank then delete any existing role record.
           IF CHDRLNB-ASGNNUM           NOT = SPACES
                MOVE CHDRLNB-ASGNNUM    TO CLRN-CLNTNUM
                MOVE 'NE'               TO CLRN-CLRRROLE
                PERFORM 2113-WRITE-CLIENT-ROLE.


           GO TO 2119-EXIT.

      * Write role record.
       2113-WRITE-CLIENT-ROLE.
           MOVE 'CN'                   TO CLRN-CLNTPFX.
           MOVE WSAA-FSU-COY           TO CLRN-CLNTCOY.
           MOVE 'CH'                   TO CLRN-FOREPFX.
           MOVE ATMD-COMPANY           TO CLRN-FORECOY.
           MOVE CHDRLNB-CHDRNUM        TO CLRN-FORENUM.
           MOVE 'ADD  '                TO CLRN-FUNCTION.
           CALL 'CLTRELN'              USING CLRN-CLTRELN-REC.
           IF CLRN-STATUZ              NOT = O-K
              MOVE CLRN-CLTRELN-REC    TO SYSR-PARAMS
              MOVE CLRN-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.

           INITIALIZE                     BLDENRL-BLDENRLREC.           <V75F01>
           MOVE CHDRLNB-CHDRPFX        TO BLDENRL-PREFIX.               <V75F01>
           MOVE CHDRLNB-CHDRCOY        TO BLDENRL-COMPANY.              <V75F01>
           MOVE CHDRLNB-CHDRNUM        TO BLDENRL-UENTITY.              <V75F01>
      *                                                                 <V75F01>
           CALL 'BLDENRL'              USING BLDENRL-BLDENRLREC.        <V75F01>
      *                                                                 <V75F01>
           IF  BLDENRL-STATUZ          NOT = O-K                        <V75F01>
               MOVE BLDENRL-BLDENRLREC TO SYSR-PARAMS                   <V75F01>
               MOVE BLDENRL-STATUZ     TO SYSR-STATUZ                   <V75F01>
               PERFORM XXXX-FATAL-ERROR                                 <V75F01>
           END-IF.                                                      <V75F01>
      *
       2119-EXIT.
           EXIT.
      *
      /
      *
      ***************************************************************
      * Update the benificiary role records.                        *
      ***************************************************************
      *
   ****2120-BENEFICIARY-ROLES SECTION.                                  <V74F03>
      ********************************
   ****2121-BENEFICIARY-ROLES.                                          <V74F03>
      *
   ****    MOVE SPACES                 TO BNFYLNB-DATA-AREA.            <V74F03>
   ****    MOVE ATMD-COMPANY           TO BNFYLNB-CHDRCOY.              <V74F03>
   ****    MOVE CHDRLNB-CHDRNUM        TO BNFYLNB-CHDRNUM.              <V74F03>
   ****    MOVE BEGN                   TO BNFYLNB-FUNCTION.             <V74F03>
   ****    CALL 'BNFYLNBIO'            USING BNFYLNB-PARAMS.            <V74F03>
   ****    IF BNFYLNB-STATUZ           NOT = O-K                        <V74F03>
   ****                                AND NOT = ENDP                   <V74F03>
   ****       MOVE BNFYLNB-PARAMS      TO SYSR-PARAMS                   <V74F03>
   ****       MOVE BNFYLNB-STATUZ      TO SYSR-STATUZ                   <V74F03>
   ****       PERFORM XXXX-FATAL-ERROR.                                 <V74F03>
   ****    IF BNFYLNB-STATUZ           = ENDP                           <V74F03>
   ****    OR BNFYLNB-CHDRCOY          NOT = ATMD-COMPANY               <V74F03>
   ****    OR BNFYLNB-CHDRNUM          NOT = CHDRLNB-CHDRNUM            <V74F03>
   ****       GO TO 2129-EXIT.                                          <V74F03>
   ****                                                                 <V74F03>
   ****    MOVE 'CN'                   TO CLRN-CLNTPFX.                 <V74F03>
   ****    MOVE WSAA-FSU-COY           TO CLRN-CLNTCOY.                 <V74F03>
   ****    MOVE 'CH'                   TO CLRN-FOREPFX.                 <V74F03>
   ****    MOVE ATMD-COMPANY           TO CLRN-FORECOY.                 <V74F03>
   ****    MOVE CHDRLNB-CHDRNUM        TO CLRN-FORENUM.                 <V74F03>
   ****                                                                 <V74F03>
   ****2122-WRITE-BENEFICIARY-ROLE.                                     <V74F03>
   ****    MOVE BNFYLNB-BNYCLT         TO CLRN-CLNTNUM.                 <V74F03>
   ****    MOVE 'BN'                   TO CLRN-CLRRROLE.                <V74F03>
   ****    MOVE 'ADD  '                TO CLRN-FUNCTION.                <V74F03>
   ****    CALL 'CLTRELN'              USING CLRN-CLTRELN-REC.          <V74F03>
   ****    IF CLRN-STATUZ              NOT = O-K                        <V74F03>
   ****       MOVE CLRN-CLTRELN-REC    TO SYSR-PARAMS                   <V74F03>
   ****       MOVE CLRN-STATUZ         TO SYSR-STATUZ                   <V74F03>
   ****       PERFORM XXXX-FATAL-ERROR.                                 <V74F03>
   ****                                                                 <V74F03>
   ****    MOVE NEXTR                  TO BNFYLNB-FUNCTION.             <V74F03>
   ****    CALL 'BNFYLNBIO'            USING BNFYLNB-PARAMS.            <V74F03>
   ****    IF BNFYLNB-STATUZ           NOT = O-K                        <V74F03>
   ****                                AND NOT = ENDP                   <V74F03>
   ****       MOVE BNFYLNB-PARAMS      TO SYSR-PARAMS                   <V74F03>
   ****       MOVE BNFYLNB-STATUZ      TO SYSR-STATUZ                   <V74F03>
   ****       PERFORM XXXX-FATAL-ERROR.                                 <V74F03>
   ****                                                                 <V74F03>
   ****    IF BNFYLNB-CHDRCOY          NOT = ATMD-COMPANY               <V74F03>
   ****    OR BNFYLNB-CHDRNUM          NOT = CHDRLNB-CHDRNUM            <V74F03>
   ****       MOVE ENDP                TO BNFYLNB-STATUZ.               <V74F03>
   ****                                                                 <V74F03>
   ****2123-PERFORM-WRITE-READN.                                        <V74F03>
   ****    PERFORM 2122-WRITE-BENEFICIARY-ROLE                          <V74F03>
   ****                                UNTIL BNFYLNB-STATUZ = ENDP.     <V74F03>
      *
   ****2129-EXIT.                                                       <V74F03>
   ****    EXIT.                                                        <V74F03>
      *
      /
      *
      ***************************************************************
      * Write a commission details record if none exists.           *
      ***************************************************************
      *
       2130-COMMISSION-DETAILS SECTION.
      *********************************
       2131-COMMISSION-DETAILS.
      *
           MOVE ATMD-COMPANY           TO PCDDLNB-CHDRCOY.
           MOVE CHDRLNB-CHDRNUM        TO PCDDLNB-CHDRNUM.
           MOVE SPACES                 TO PCDDLNB-AGNTNUM.
           MOVE BEGN                   TO PCDDLNB-FUNCTION.
           CALL 'PCDDLNBIO'            USING PCDDLNB-PARAMS.
           IF PCDDLNB-STATUZ           NOT = O-K
                                       AND NOT = ENDP
              MOVE PCDDLNB-PARAMS      TO SYSR-PARAMS
              MOVE PCDDLNB-STATUZ      TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.

           IF  PCDDLNB-CHDRCOY         = ATMD-COMPANY
           AND PCDDLNB-CHDRNUM         = CHDRLNB-CHDRNUM
           AND PCDDLNB-STATUZ          NOT = ENDP
              GO TO 2139-EXIT.

           MOVE ATMD-COMPANY           TO PCDDLNB-CHDRCOY.
           MOVE CHDRLNB-CHDRNUM        TO PCDDLNB-CHDRNUM.
           MOVE CHDRLNB-AGNTNUM        TO PCDDLNB-AGNTNUM.
           MOVE CHDRLNB-TRANNO         TO PCDDLNB-TRANNO.
           MOVE CHDRLNB-OCCDATE        TO PCDDLNB-CURRFROM.
           MOVE CHDRLNB-CURRTO         TO PCDDLNB-CURRTO.
           MOVE WSAA-USER              TO PCDDLNB-USER.
           MOVE WSAA-TERMID            TO PCDDLNB-TERMID.
           MOVE WSAA-TRANSACTION-TIME  TO PCDDLNB-TRANSACTION-TIME.
           MOVE WSAA-TRANSACTION-DATE  TO PCDDLNB-TRANSACTION-DATE.
           MOVE 100                    TO PCDDLNB-SPLIT-BCOMM.
           MOVE 100                    TO PCDDLNB-SPLIT-BPTS.
           MOVE '1'                    TO PCDDLNB-VALIDFLAG.
           MOVE PCDDLNBREC             TO PCDDLNB-FORMAT.
           MOVE WRITR                  TO PCDDLNB-FUNCTION.
           CALL 'PCDDLNBIO'            USING PCDDLNB-PARAMS.
           IF PCDDLNB-STATUZ           NOT = O-K
              MOVE PCDDLNB-PARAMS      TO SYSR-PARAMS
              MOVE PCDDLNB-STATUZ      TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
      *
       2139-EXIT.
           EXIT.
      *
      /
      *
      ***************************************************************
      * Update all the life records.                                *
      ***************************************************************
      *
       2200-LIVES-ASSURED SECTION.
      ****************************
       2201-LIVES-ASSURED.
      *
      * Read first life.
           MOVE SPACES                 TO LIFELNB-DATA-AREA.
           MOVE ATMD-COMPANY           TO LIFELNB-CHDRCOY.
           MOVE CHDRLNB-CHDRNUM        TO LIFELNB-CHDRNUM.
           MOVE BEGN                   TO LIFELNB-FUNCTION.
           CALL 'LIFELNBIO'            USING LIFELNB-PARAMS.
           IF LIFELNB-STATUZ           NOT = O-K AND
                                       NOT = ENDP
              MOVE LIFELNB-PARAMS      TO SYSR-PARAMS
              MOVE LIFELNB-STATUZ      TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.

           MOVE LIFELNB-LIFCNUM        TO WSAA-LIFCNUM.                 <V71L05>
                                                                        <V71L05>
           PERFORM 2210-UPDATE-LIFE    UNTIL LIFELNB-STATUZ = ENDP
                             OR LIFELNB-CHDRCOY NOT = ATMD-COMPANY
                             OR LIFELNB-CHDRNUM NOT = CHDRLNB-CHDRNUM.
      *
       2209-EXIT.
           EXIT.
      *
      /
      *
      ***************************************************************
      * Actually update the life record.                            *
      ***************************************************************
      *
       2210-UPDATE-LIFE SECTION.
      **************************
       2211-UPDATE-LIFE.
      *
           MOVE '1'                    TO LIFELNB-VALIDFLAG.
           MOVE CHDRLNB-TRANNO         TO LIFELNB-TRANNO.
           MOVE T5679-SET-LIFE-STAT    TO LIFELNB-STATCODE.
           IF LIFELNB-JLIFE            NOT = SPACES
                                       AND NOT = '00'
              MOVE T5679-SET-JLIFE-STAT
                                       TO LIFELNB-STATCODE.
           MOVE WSAA-USER              TO LIFELNB-USER.
           MOVE WSAA-TRANSACTION-DATE  TO LIFELNB-TRANSACTION-DATE.
           MOVE WSAA-TRANSACTION-TIME  TO LIFELNB-TRANSACTION-TIME.
           MOVE WSAA-TERMID            TO LIFELNB-TERMID.

           MOVE UPDAT                  TO LIFELNB-FUNCTION.
           CALL 'LIFELNBIO'            USING LIFELNB-PARAMS.
           IF LIFELNB-STATUZ           NOT = O-K
              MOVE LIFELNB-PARAMS      TO SYSR-PARAMS
              MOVE LIFELNB-STATUZ      TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.

           MOVE NEXTR                  TO LIFELNB-FUNCTION.
           CALL 'LIFELNBIO'            USING LIFELNB-PARAMS.
           IF LIFELNB-STATUZ           NOT = O-K
                                       AND NOT = ENDP
              MOVE LIFELNB-PARAMS      TO SYSR-PARAMS
              MOVE LIFELNB-STATUZ      TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.

      *
       2219-EXIT.
           EXIT.
      *
      /
      *
      ***************************************************************
      * Create cover and rider information.                         *
      ***************************************************************
      *
       2300-COMPONENTS SECTION.
      *************************
       2301-COMPONENTS.
      *
      *    PTDATE moved in early as it is used in commission processing
           IF CHDRLNB-BILLFREQ            = '00'
              MOVE WSAA-OLD-CESS-DATE     TO CHDRLNB-PTDATE
           ELSE
              MOVE CHDRLNB-BTDATE         TO CHDRLNB-PTDATE.
      *
      *

      * Find the smallest number applicable.
           MOVE 1                      TO WSAA-NUMAPP.
           PERFORM 2310-CALC-SUMMARISED-POLICIES.
           MOVE WSAA-NUMAPP            TO CHDRLNB-POLSUM
                                          CHDRLNB-NXTSFX.
           IF WSAA-NUMAPP              = 1
           MOVE ZERO                   TO CHDRLNB-POLSUM.
           MOVE WSAA-TERMID            TO VRCM-COMP-TERMID.
           MOVE WSAA-USER              TO VRCM-USER.
           MOVE WSAA-TRANSACTION-DATE  TO VRCM-DATE.
           MOVE WSAA-TRANSACTION-TIME  TO VRCM-TIME.
           MOVE VRCM-TRANID-N          TO VRCM-COMP-TRANID-N.
           MOVE VRCM-COMP-TRANID       TO CHDRLNB-TRANID.

           MOVE KEEPS                  TO CHDRLNB-FUNCTION.
           MOVE CHDRLNBREC             TO CHDRLNB-FORMAT.
           CALL 'CHDRLNBIO'            USING CHDRLNB-PARAMS.
           IF CHDRLNB-STATUZ           NOT = O-K
              MOVE CHDRLNB-PARAMS      TO SYSR-PARAMS
              MOVE CHDRLNB-STATUZ      TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.

           MOVE WRITS                  TO CHDRLNB-FUNCTION.
           MOVE CHDRLNBREC             TO CHDRLNB-FORMAT.
           CALL 'CHDRLNBIO'            USING CHDRLNB-PARAMS.
           IF CHDRLNB-STATUZ           NOT = O-K
              MOVE CHDRLNB-PARAMS      TO SYSR-PARAMS
              MOVE CHDRLNB-STATUZ      TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.

      * Create the cover and rider records.
       2302-NO-PLAN-PROCESSING.
           PERFORM 2320-CREATE-RIDER-COVER-INFO.
      *                                                                 <LA2110>
      * To detect for WOP and update the re-rate and CPI dates          <LA2110>
      * to it if applicable.                                            <LA2110>
      *                                                                 <LA2110>
           PERFORM A100-WOP-DATES.                                      <LA2110>
      *
       2309-EXIT.
           EXIT.
      *
      /
      *
      ***************************************************************
      * Calculate the sumarised policies.                           *
      ***************************************************************
      *
       2310-CALC-SUMMARISED-POLICIES SECTION.
      ***************************************
       2311-CALC-SUMMARISED-POLICIES.
      *
      * Initialise coverage premium table.
      *
           MOVE 1                      TO WSAA-C
                                          WSAA-L.
           PERFORM 3000-INITIALISE-COVR-PREM
               UNTIL (WSAA-L > 10).
      *
      * Start the COVT file.
           MOVE ZERO                   TO WSAA-NUMAPP.
           MOVE SPACES                 TO COVTLNB-PARAMS.
           MOVE CHDRLNB-CHDRCOY        TO COVTLNB-CHDRCOY.
           MOVE CHDRLNB-CHDRNUM        TO COVTLNB-CHDRNUM.
           MOVE 0                      TO COVTLNB-SEQNBR.
           MOVE BEGN                   TO COVTLNB-FUNCTION.
           CALL 'COVTLNBIO'            USING COVTLNB-PARAMS.
           IF COVTLNB-STATUZ           NOT = O-K
                                       AND NOT = ENDP
              MOVE COVTLNB-PARAMS      TO SYSR-PARAMS
              MOVE COVTLNB-STATUZ      TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.

           IF COVTLNB-CHDRCOY          NOT = CHDRLNB-CHDRCOY
           OR COVTLNB-CHDRNUM          NOT = CHDRLNB-CHDRNUM
           OR COVTLNB-STATUZ           = ENDP
              GO TO 2319-EXIT.

           MOVE COVTLNB-COVERAGE       TO WSAA-COVERAGE.
           MOVE COVTLNB-RIDER          TO WSAA-RIDER.
           MOVE COVTLNB-NUMAPP         TO WSAA-NUMAPP.

           IF COVTLNB-RIDER = '00' OR '  '
               MOVE COVTLNB-COVERAGE   TO WSAA-COVERAGE-NUM
               MOVE COVTLNB-LIFE       TO WSAA-LIFE-NUM
               ADD COVTLNB-INSTPREM    TO
                   WSAA-COVT-INSTPREM(WSAA-LIFE-NUM WSAA-COVERAGE-NUM)
               ADD COVTLNB-SINGP       TO
                   WSAA-COVT-SINGP(WSAA-LIFE-NUM WSAA-COVERAGE-NUM).

           MOVE NEXTR                  TO COVTLNB-FUNCTION.
           PERFORM 231A-READ-COVER-RIDER
                                       UNTIL COVTLNB-STATUZ = ENDP.

      *
       2319-EXIT.
           EXIT.
      *
      /
      *
      ***************************************************************
      * Read the COVT file, when the cover changes check the number *
      * applicable against the least store number applicable,       *
      * storing the least.                                          *
      ***************************************************************
      *
       231A-READ-COVER-RIDER SECTION.
      ***************************************
       231B-READ-COVER-RIDER.
      *
           CALL 'COVTLNBIO'            USING COVTLNB-PARAMS.
           IF COVTLNB-STATUZ           NOT = O-K
                                       AND NOT = ENDP
              MOVE COVTLNB-PARAMS      TO SYSR-PARAMS
              MOVE COVTLNB-STATUZ      TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.

           IF COVTLNB-CHDRCOY          NOT = CHDRLNB-CHDRCOY
           OR COVTLNB-CHDRNUM          NOT = CHDRLNB-CHDRNUM
           OR COVTLNB-STATUZ           = ENDP
              MOVE ENDP                TO COVTLNB-STATUZ
              GO TO 231Z-EXIT.

           IF COVTLNB-COVERAGE         NOT = WSAA-COVERAGE
           OR COVTLNB-RIDER            NOT = WSAA-RIDER
              MOVE COVTLNB-COVERAGE    TO WSAA-COVERAGE
              MOVE COVTLNB-RIDER       TO WSAA-RIDER
              IF COVTLNB-NUMAPP        < WSAA-NUMAPP
                 MOVE COVTLNB-NUMAPP   TO WSAA-NUMAPP.

           IF COVTLNB-RIDER = '00' OR '  '
               MOVE COVTLNB-COVERAGE   TO WSAA-COVERAGE-NUM
               MOVE COVTLNB-LIFE       TO WSAA-LIFE-NUM
               ADD COVTLNB-INSTPREM    TO
                   WSAA-COVT-INSTPREM(WSAA-LIFE-NUM WSAA-COVERAGE-NUM)
               ADD COVTLNB-SINGP       TO
                   WSAA-COVT-SINGP(WSAA-LIFE-NUM WSAA-COVERAGE-NUM).
      *
       231Z-EXIT.
           EXIT.
      *
      /
      *
      ***************************************************************
      * Actually create the COVR records depending on criteria.     *
      ***************************************************************
      *
       2320-CREATE-RIDER-COVER-INFO SECTION.
      *************************************
       2321-CREATE-RIDER-COVER-INFO.
      *
      * Begin the COVT file again.
           MOVE SPACES                 TO COVTLNB-PARAMS.
           MOVE CHDRLNB-CHDRCOY        TO COVTLNB-CHDRCOY.
           MOVE CHDRLNB-CHDRNUM        TO COVTLNB-CHDRNUM.
           MOVE 0                      TO COVTLNB-SEQNBR.


           MOVE BEGN                   TO COVTLNB-FUNCTION.

           CALL 'COVTLNBIO'            USING COVTLNB-PARAMS.
           IF COVTLNB-STATUZ           NOT = O-K
                                       AND NOT = ENDP
              MOVE COVTLNB-PARAMS      TO SYSR-PARAMS
              MOVE COVTLNB-STATUZ      TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.

           IF COVTLNB-CHDRCOY          NOT = CHDRLNB-CHDRCOY
           OR COVTLNB-CHDRNUM          NOT = CHDRLNB-CHDRNUM
           OR COVTLNB-STATUZ           = ENDP
              MOVE ENDP                TO COVTLNB-STATUZ
              GO TO 2329-EXIT.
      *

           IF COVTLNB-SINGP NOT = 0
              MOVE 'Y'                 TO WSAA-SING-PRM-IND.

           MOVE COVTLNB-PREM-CESS-DATE TO WSAA-OLD-CESS-DATE.

      *
      * Clear the COVR record for subsequent writing..
           MOVE SPACES                 TO COVRLNB-NON-KEY.
           MOVE 0                      TO COVRLNB-SUMINS,
                                          COVRLNB-VAR-SUM-INSURED,
                                          COVRLNB-DEFER-PERD-AMT,
                                          COVRLNB-TOT-MTHLY-BENEFIT,
                                          COVRLNB-SINGP,
                                          COVRLNB-INSTPREM,
                                          COVRLNB-ZBINSTPREM,           <SPLPRM>
                                          COVRLNB-ZLINSTPREM,           <SPLPRM>
                                          COVRLNB-STAT-SUMINS,
                                          COVRLNB-ANB-AT-CCD,
                                          COVRLNB-PAYRSEQNO,
                                          COVRLNB-CRRCD.

      * No plan processing is applicable for every coverage or rider
      * so just write a COVR record for each COVT record.
           IF WSAA-NUMAPP              = CHDRLNB-POLINC
           OR CHDRLNB-POLINC           NOT > 1
              PERFORM 232A-A-PLAN-NOT-APPLICABLE
                                       UNTIL COVTLNB-STATUZ = ENDP
              GO TO 2329-EXIT.

      * No summary records are to be written. Split out all covers
      * and rider records starting with a suffix number of 1.
           IF  WSAA-NUMAPP             = 1
           AND CHDRLNB-POLINC          > 1
               MOVE COVTLNB-COVERAGE   TO WSAA-COVERAGE
               MOVE COVTLNB-RIDER      TO WSAA-RIDER
               MOVE 'Y'                TO WSAA-NO-SUMMARY-REC
               MOVE 1                  TO WSAA-PLAN-SUFFIX
               PERFORM 232B-B-PLAN-APPLICABLE
                                       UNTIL COVTLNB-STATUZ = ENDP
              GO TO 2329-EXIT.

      * Otherwise plan processing is applicable.
           PERFORM 232C-C-PLAN-AND-SUMMARIES
                                       UNTIL COVTLNB-STATUZ = ENDP.
      *
       2329-EXIT.
           EXIT.
      *
      /
      *
       232A-A-PLAN-NOT-APPLICABLE SECTION.
      *************************************
       2321-A-PLAN-NOT-APPLICABLE.
      *

           MOVE 0                      TO WSAA-PLAN-SUFFIX.
           MOVE COVTLNB-SUMINS         TO COVRLNB-SUMINS.
           MOVE COVTLNB-SINGP          TO COVRLNB-SINGP.
           MOVE COVTLNB-INSTPREM       TO COVRLNB-INSTPREM.
           MOVE COVTLNB-ZBINSTPREM     TO COVRLNB-ZBINSTPREM.           <SPLPRM>
           MOVE COVTLNB-ZLINSTPREM     TO COVRLNB-ZLINSTPREM.           <SPLPRM>
           PERFORM 232D-WRITE-COVER-RIDERS.
           PERFORM M800-UPDATE-MLIA.                                    <V4L011>
           PERFORM 232E-DELET-AND-READ-COVT.
      *
       232A-EXIT.
           EXIT.
      *
      /
      *
       232B-B-PLAN-APPLICABLE SECTION.
      ********************************
       2321-B-PLAN-APPLICABLE.
      *
      * Divide sum insured and premium by the number applicable.
           DIVIDE COVTLNB-SUMINS       BY COVTLNB-NUMAPP
                                       GIVING COVRLNB-SUMINS
                                              ROUNDED.
                                                                        <V76F06>
           MOVE COVRLNB-SUMINS         TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO COVRLNB-SUMINS.               <V76F06>
      *
      * Initialise SINGP and INSTPREM first, since they may contain
      * values from the last policy.(Essential for non-identical
      * policies)
      *
           MOVE 0                      TO COVRLNB-SINGP
                                          COVRLNB-ZBINSTPREM            <SPLPRM>
                                          COVRLNB-ZLINSTPREM            <SPLPRM>
                                          COVRLNB-INSTPREM.
      *
           IF COVTLNB-SINGP            NOT = 0
               DIVIDE COVTLNB-SINGP    BY COVTLNB-NUMAPP
                                       GIVING COVRLNB-SINGP
                                              ROUNDED.

           MOVE COVRLNB-SINGP          TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO COVRLNB-SINGP.                <V76F06>
                                                                        <V76F06>
           IF COVTLNB-INSTPREM         NOT = 0
               DIVIDE COVTLNB-INSTPREM BY COVTLNB-NUMAPP
                                       GIVING COVRLNB-INSTPREM
                                              ROUNDED.

           MOVE COVRLNB-INSTPREM       TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO COVRLNB-INSTPREM.             <V76F06>
                                                                        <V76F06>
           IF COVTLNB-ZBINSTPREM       NOT = 0                          <SPLPRM>
               DIVIDE COVTLNB-ZBINSTPREM BY COVTLNB-NUMAPP              <SPLPRM>
                                       GIVING COVRLNB-ZBINSTPREM        <SPLPRM>
                                              ROUNDED.                  <SPLPRM>
                                                                        <SPLPRM>
           MOVE COVRLNB-ZBINSTPREM     TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO COVRLNB-ZBINSTPREM.           <V76F06>
                                                                        <V76F06>
           IF COVTLNB-ZLINSTPREM       NOT = 0                          <SPLPRM>
               DIVIDE COVTLNB-ZLINSTPREM BY COVTLNB-NUMAPP              <SPLPRM>
                                       GIVING COVRLNB-ZLINSTPREM        <SPLPRM>
                                              ROUNDED.                  <SPLPRM>
                                                                        <SPLPRM>
           MOVE COVRLNB-ZLINSTPREM     TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO COVRLNB-ZLINSTPREM.           <V76F06>
                                                                        <V76F06>
      * Start with a plan suffix of 1 (and adding 1 each time a
      * record is written.) and write a cover/rider record for
      * each policy.
           PERFORM 232D-WRITE-COVER-RIDERS
                                       COVTLNB-NUMAPP TIMES.
           PERFORM 232E-DELET-AND-READ-COVT.

      * Reset the plan suffix number when a different component
      * is read.
           IF COVTLNB-COVERAGE         NOT = WSAA-COVERAGE
           OR COVTLNB-RIDER            NOT = WSAA-RIDER
              MOVE COVTLNB-COVERAGE    TO WSAA-COVERAGE
              MOVE COVTLNB-RIDER       TO WSAA-RIDER
              MOVE 'Y'                 TO WSAA-NO-SUMMARY-REC
              MOVE 1                   TO WSAA-PLAN-SUFFIX.

      *
       232B-EXIT.
           EXIT.
      *
      /
      *
       232C-C-PLAN-AND-SUMMARIES SECTION.
      ***********************************
       2321-C-PLAN-AND-SUMMARIES.
      *
      * Write a summary record.
           MOVE 0                      TO WSAA-PLAN-SUFFIX.
           COMPUTE COVRLNB-SUMINS ROUNDED
                                    = (COVTLNB-SUMINS / COVTLNB-NUMAPP)
                                                    * WSAA-NUMAPP.
           MOVE COVRLNB-SUMINS         TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO COVRLNB-SUMINS.               <V76F06>
      *
      * Initialise SINGP and INSTPREM first, since they may contain
      * values from the last policy.(Essential for non-identical
      * policies)
      *
           MOVE 0                      TO COVRLNB-SINGP
                                          COVRLNB-ZBINSTPREM            <SPLPRM>
                                          COVRLNB-ZLINSTPREM            <SPLPRM>
                                          COVRLNB-INSTPREM.

           IF COVTLNB-SINGP            NOT = 0
               COMPUTE COVRLNB-SINGP ROUNDED
                                  = (COVTLNB-SINGP  / COVTLNB-NUMAPP)
                                                    * WSAA-NUMAPP.
           MOVE COVRLNB-SINGP          TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO COVRLNB-SINGP.                <V76F06>
                                                                        <V76F06>
           IF COVTLNB-INSTPREM         NOT = 0
               COMPUTE COVRLNB-INSTPREM ROUNDED
                                  = (COVTLNB-INSTPREM / COVTLNB-NUMAPP)
                                                    * WSAA-NUMAPP.
           MOVE COVRLNB-INSTPREM       TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO COVRLNB-INSTPREM.             <V76F06>
                                                                        <SPLPRM>
           IF COVTLNB-ZBINSTPREM       NOT = 0                          <SPLPRM>
               COMPUTE COVRLNB-ZBINSTPREM ROUNDED                       <SPLPRM>
                                = (COVTLNB-ZBINSTPREM / COVTLNB-NUMAPP) <SPLPRM>
                                                    * WSAA-NUMAPP.      <SPLPRM>
           MOVE COVRLNB-ZBINSTPREM     TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO COVRLNB-ZBINSTPREM.           <V76F06>
                                                                        <SPLPRM>
           IF COVTLNB-ZLINSTPREM       NOT = 0                          <SPLPRM>
               COMPUTE COVRLNB-ZLINSTPREM ROUNDED                       <SPLPRM>
                                = (COVTLNB-ZLINSTPREM / COVTLNB-NUMAPP) <SPLPRM>
                                                    * WSAA-NUMAPP.      <SPLPRM>
           MOVE COVRLNB-ZLINSTPREM     TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO COVRLNB-ZLINSTPREM.           <V76F06>
                                                                        <V76F06>
      * Write COVR records with a number applicable fraction of
      * the sum insured and premium and write the COVR records
      * until the number applicable is reached.
           PERFORM 232D-WRITE-COVER-RIDERS.

           SUBTRACT WSAA-NUMAPP        FROM COVTLNB-NUMAPP
                                       GIVING WSAA-NO-OF-RECS.
           ADD WSAA-NUMAPP, 1          GIVING WSAA-PLAN-SUFFIX.

           IF WSAA-NO-OF-RECS          NOT = 0
              PERFORM 2322-WRITE-COVRS WSAA-NO-OF-RECS TIMES.

           MOVE COVTLNB-COVERAGE       TO WSAA-COVERAGE.
           MOVE COVTLNB-RIDER          TO WSAA-RIDER.

           PERFORM 232E-DELET-AND-READ-COVT.

      * Read the next record.
           PERFORM 2323-NEXT-COVER-RIDER
                        UNTIL COVTLNB-STATUZ = ENDP
                           OR COVTLNB-COVERAGE NOT = WSAA-COVERAGE
                           OR COVTLNB-RIDER    NOT = WSAA-RIDER.

      * By pass paragraphs below - they are called from above.
           GO TO 232C-EXIT.
      *
       2322-WRITE-COVRS.
           COMPUTE COVRLNB-SUMINS ROUNDED
                                  = (COVTLNB-SUMINS / COVTLNB-NUMAPP).
      *
      * Initialise SINGP and INSTPREM first, since they may contain
      * values from the last policy.(Essential for non-identical
      * policies)
      *
           MOVE 0                      TO COVRLNB-SINGP
                                          COVRLNB-ZBINSTPREM            <SPLPRM>
                                          COVRLNB-ZLINSTPREM            <SPLPRM>
                                          COVRLNB-INSTPREM.

           IF COVTLNB-SINGP            NOT = 0
               COMPUTE COVRLNB-SINGP ROUNDED
                                  = (COVTLNB-SINGP  / COVTLNB-NUMAPP).
           IF COVTLNB-INSTPREM         NOT = 0
               COMPUTE COVRLNB-INSTPREM ROUNDED
                                  = (COVTLNB-INSTPREM / COVTLNB-NUMAPP).
           IF COVTLNB-ZBINSTPREM       NOT = 0                          <SPLPRM>
               COMPUTE COVRLNB-ZBINSTPREM ROUNDED                       <SPLPRM>
                                = (COVTLNB-ZBINSTPREM / COVTLNB-NUMAPP).<SPLPRM>
           IF COVTLNB-ZLINSTPREM       NOT = 0                          <SPLPRM>
               COMPUTE COVRLNB-ZLINSTPREM ROUNDED                       <SPLPRM>
                                = (COVTLNB-ZLINSTPREM / COVTLNB-NUMAPP).<SPLPRM>
           PERFORM 232D-WRITE-COVER-RIDERS.

       2323-NEXT-COVER-RIDER.
           PERFORM 2322-WRITE-COVRS    COVTLNB-NUMAPP TIMES.
           PERFORM 232E-DELET-AND-READ-COVT.
      *
       232C-EXIT.
           EXIT.
      *
      /
      *
       232D-WRITE-COVER-RIDERS SECTION.
      *********************************
       2321-WRITE-COVER-RIDERS.
      * Access the table to initialise the COVR fields.
           MOVE SPACES                 TO ITDM-DATA-AREA.
           MOVE ATMD-COMPANY           TO ITDM-ITEMCOY.
           MOVE T5687                  TO ITDM-ITEMTABL.
           MOVE COVTLNB-CRTABLE        TO ITDM-ITEMITEM.
           MOVE COVTLNB-EFFDATE        TO ITDM-ITMFRM.
           MOVE BEGN                   TO ITDM-FUNCTION.
           CALL 'ITDMIO' USING         ITDM-PARAMS.
           IF ITDM-STATUZ              NOT = O-K AND NOT = ENDP
              MOVE ITDM-PARAMS         TO SYSR-PARAMS
              MOVE ITDM-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
           IF ITDM-ITEMCOY             NOT = ATMD-COMPANY
           OR ITDM-ITEMTABL            NOT = T5687
           OR ITDM-ITEMITEM            NOT = COVTLNB-CRTABLE
           OR ITDM-STATUZ              = ENDP
              MOVE COVTLNB-CRTABLE     TO SYSR-PARAMS
              MOVE F294                TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR
           ELSE
      ***     MOVE ITDM-GENAREA        TO T5687-T5687-REC.              <V42004>
              MOVE ITDM-GENAREA        TO T5687-T5687-REC               <V42004>
              MOVE T5687-BASIC-COMM-METH                                <V42004>
                                       TO WSAA-BASIC-COMM-METH          <V42004>
              MOVE T5687-BASCPY        TO WSAA-BASCPY                   <V42004>
              MOVE T5687-SRVCPY        TO WSAA-SRVCPY                   <V42004>
              MOVE T5687-RNWCPY        TO WSAA-RNWCPY                   <V42004>
              MOVE T5687-BASSCMTH      TO WSAA-BASSCMTH                 <V42004>
              MOVE T5687-BASSCPY       TO WSAA-BASSCPY.                 <V42004>

      *
           MOVE WSAA-PLAN-SUFFIX       TO COVRLNB-PLAN-SUFFIX.
           MOVE CHDRLNB-OCCDATE        TO COVRLNB-CURRFROM.
           MOVE CHDRLNB-CURRTO         TO COVRLNB-CURRTO.
           MOVE CHDRLNB-TRANNO         TO COVRLNB-TRANNO.
           MOVE COVTLNB-EFFDATE        TO COVRLNB-CRRCD.
           MOVE '1'                    TO COVRLNB-VALIDFLAG.

           IF  COVTLNB-RIDER           = '00' OR SPACES
               MOVE COVTLNB-CRTABLE    TO WSAA-MAIN-COVERAGE            <V42004>
               MOVE T5679-SET-COV-RISK-STAT TO COVRLNB-STATCODE
           ELSE
              MOVE WSAA-MAIN-COVERAGE  TO WSAA-TR695-COVERAGE           <V42004>
              MOVE COVTLNB-CRTABLE     TO WSAA-TR695-RIDER              <V42004>
                                                                        <V42004>
              MOVE SPACES              TO ITDM-DATA-AREA                <V42004>
              MOVE ATMD-COMPANY        TO ITDM-ITEMCOY                  <V42004>
              MOVE TR695               TO ITDM-ITEMTABL                 <V42004>
              MOVE WSAA-TR695-KEY      TO ITDM-ITEMITEM                 <V42004>
              MOVE COVTLNB-EFFDATE     TO ITDM-ITMFRM                   <V42004>
              MOVE BEGN                TO ITDM-FUNCTION                 <V42004>
              CALL 'ITDMIO' USING         ITDM-PARAMS                   <V42004>
              IF ITDM-STATUZ           NOT = O-K AND NOT = ENDP         <V42004>
                 MOVE ITDM-PARAMS      TO SYSR-PARAMS                   <V42004>
                 MOVE ITDM-STATUZ      TO SYSR-STATUZ                   <V42004>
                 PERFORM XXXX-FATAL-ERROR                               <V42004>
              END-IF                                                    <V42004>
                                                                        <V42004>
              IF ITDM-ITEMCOY          NOT = ATMD-COMPANY               <V42004>
              OR ITDM-ITEMTABL         NOT = TR695                      <V42004>
              OR ITDM-ITEMITEM         NOT = WSAA-TR695-KEY             <V42004>
              OR ITDM-STATUZ           = ENDP                           <V42004>
                 MOVE ENDP             TO ITDM-STATUZ                   <V42004>
              END-IF                                                    <V42004>

              IF ITDM-STATUZ            = ENDP                          <V42004>
                 MOVE '****'           TO WSAA-TR695-RIDER              <V42004>
                 MOVE SPACES           TO ITDM-DATA-AREA                <V42004>
                 MOVE ATMD-COMPANY     TO ITDM-ITEMCOY                  <V42004>
                 MOVE TR695            TO ITDM-ITEMTABL                 <V42004>
                 MOVE WSAA-TR695-KEY   TO ITDM-ITEMITEM                 <V42004>
                 MOVE COVTLNB-EFFDATE  TO ITDM-ITMFRM                   <V42004>
                 MOVE BEGN             TO ITDM-FUNCTION                 <V42004>
                 CALL 'ITDMIO' USING      ITDM-PARAMS                   <V42004>

                 IF ITDM-STATUZ        NOT = O-K AND NOT = ENDP         <V42004>
                    MOVE ITDM-PARAMS   TO SYSR-PARAMS                   <V42004>
                    MOVE ITDM-STATUZ   TO SYSR-STATUZ                   <V42004>
                    PERFORM XXXX-FATAL-ERROR                            <V42004>
                 END-IF                                                 <V42004>

                 IF ITDM-ITEMCOY       NOT = ATMD-COMPANY               <V42004>
                 OR ITDM-ITEMTABL      NOT = TR695                      <V42004>
                 OR ITDM-ITEMITEM      NOT = WSAA-TR695-KEY             <V42004>
                 OR ITDM-STATUZ        = ENDP                           <V42004>
                    CONTINUE                                            <V42004>
                 ELSE
                    MOVE ITDM-GENAREA  TO TR695-TR695-REC               <V42004>
                    MOVE TR695-BASIC-COMM-METH                          <V42004>
                                       TO WSAA-BASIC-COMM-METH          <V42004>
                    MOVE TR695-BASCPY  TO WSAA-BASCPY                   <V42004>
                    MOVE TR695-SRVCPY  TO WSAA-SRVCPY                   <V42004>
                    MOVE TR695-RNWCPY  TO WSAA-RNWCPY                   <V42004>
                    MOVE TR695-BASSCMTH TO WSAA-BASSCMTH                <V42004>
                    MOVE TR695-BASSCPY TO WSAA-BASSCPY                  <V42004>
                 END-IF                                                 <V42004>
              ELSE                                                      <V42004>
                 MOVE ITDM-GENAREA     TO TR695-TR695-REC               <V42004>
                 MOVE TR695-BASIC-COMM-METH                             <V42004>
                                       TO WSAA-BASIC-COMM-METH          <V42004>
                 MOVE TR695-BASCPY     TO WSAA-BASCPY                   <V42004>
                 MOVE TR695-SRVCPY     TO WSAA-SRVCPY                   <V42004>
                 MOVE TR695-RNWCPY     TO WSAA-RNWCPY                   <V42004>
                 MOVE TR695-BASSCMTH   TO WSAA-BASSCMTH                 <V42004>
                 MOVE TR695-BASSCPY    TO WSAA-BASSCPY                  <V42004>
              END-IF                                                    <V42004>

      ***      MOVE T5679-SET-RID-RISK-STAT TO COVRLNB-STATCODE.        <V42004>
              MOVE T5679-SET-RID-RISK-STAT TO COVRLNB-STATCODE          <V42004>
           END-IF.

           IF  CHDRLNB-BILLFREQ        NOT = '00'
           AND T5687-SINGLE-PREM-IND   NOT = 'Y'
               IF COVTLNB-RIDER        = SPACES OR '00'
                  MOVE T5679-SET-COV-PREM-STAT
                                       TO COVRLNB-PSTATCODE
               ELSE
                  MOVE T5679-SET-RID-PREM-STAT
                                       TO COVRLNB-PSTATCODE
               END-IF                                                   <CAS1.0>
           ELSE
               IF COVTLNB-RIDER        = SPACES OR '00'
                  MOVE T5679-SET-SNGP-COV-STAT
                                       TO COVRLNB-PSTATCODE
               ELSE
                  MOVE T5679-SET-SNGP-RID-STAT                          <CAS1.0>
                                       TO COVRLNB-PSTATCODE             <CAS1.0>
               END-IF                                                   <CAS1.0>
           END-IF.                                                      <CAS1.0>
      ****        MOVE T5679-SET-SNGP-COV-STAT                          <CAS1.0>
      ****                             TO COVRLNB-PSTATCODE.            <CAS1.0>
           MOVE T5687-REPTCDS          TO COVRLNB-REPTCDS.
           MOVE T5687-STAT-FUND        TO COVRLNB-STAT-FUND.
           MOVE T5687-STAT-SECT        TO COVRLNB-STAT-SECT.
           MOVE T5687-STAT-SUB-SECT    TO COVRLNB-STAT-SUBSECT.
      * Move relevent COVT fields and clear non-used numerics
      * and dates.
           MOVE COVTLNB-CHDRCOY        TO COVRLNB-CHDRCOY.
           MOVE COVTLNB-CHDRNUM        TO COVRLNB-CHDRNUM.
           MOVE COVTLNB-LIFE           TO COVRLNB-LIFE.
           MOVE COVTLNB-COVERAGE       TO COVRLNB-COVERAGE.
           MOVE COVTLNB-RIDER          TO COVRLNB-RIDER.
           MOVE 0                      TO COVRLNB-CR-INSTAMT01,
                                          COVRLNB-CR-INSTAMT02,
                                          COVRLNB-CR-INSTAMT03,
                                          COVRLNB-CR-INSTAMT04,
                                          COVRLNB-CR-INSTAMT05,
                                          COVRLNB-EST-MAT-VALUE01,
                                          COVRLNB-EST-MAT-VALUE02,
                                          COVRLNB-EST-MAT-DATE01,
                                          COVRLNB-EST-MAT-DATE02,
                                          COVRLNB-EST-MAT-INT01,
                                          COVRLNB-EST-MAT-INT02.
           MOVE CHDRLNB-CNTCURR        TO COVRLNB-PREM-CURRENCY.
           MOVE COVTLNB-JLIFE          TO COVRLNB-JLIFE.
           IF COVTLNB-JLIFE            = SPACES OR '00'
              MOVE COVTLNB-ANB-AT-CCD01 TO COVRLNB-ANB-AT-CCD
              MOVE COVTLNB-SEX01        TO COVRLNB-SEX
           ELSE
              MOVE COVTLNB-ANB-AT-CCD02 TO COVRLNB-ANB-AT-CCD
              MOVE COVTLNB-SEX02        TO COVRLNB-SEX.
           MOVE COVTLNB-CRTABLE        TO COVRLNB-CRTABLE.
           MOVE COVTLNB-RISK-CESS-DATE TO COVRLNB-RISK-CESS-DATE.
           MOVE COVTLNB-PREM-CESS-DATE TO COVRLNB-PREM-CESS-DATE.
           MOVE COVTLNB-BEN-CESS-DATE  TO COVRLNB-BEN-CESS-DATE.        <082>
           MOVE 0                      TO COVRLNB-NEXT-ACT-DATE.
           MOVE COVTLNB-RISK-CESS-AGE  TO COVRLNB-RISK-CESS-AGE.
           MOVE COVTLNB-PREM-CESS-AGE  TO COVRLNB-PREM-CESS-AGE.
           MOVE COVTLNB-BEN-CESS-AGE   TO COVRLNB-BEN-CESS-AGE.         <082>
      *****MOVE 0                      TO COVRLNB-BEN-CESS-AGE.         <082>
           MOVE COVTLNB-RISK-CESS-TERM TO COVRLNB-RISK-CESS-TERM.
           MOVE COVTLNB-PREM-CESS-TERM TO COVRLNB-PREM-CESS-TERM.
           MOVE COVTLNB-BEN-CESS-TERM  TO COVRLNB-BEN-CESS-TERM.        <082>
      *****MOVE 0                      TO COVRLNB-BEN-CESS-TERM.        <082>
           MOVE 0                      TO COVRLNB-VAR-SUM-INSURED.
           MOVE COVTLNB-MORTCLS        TO COVRLNB-MORTCLS.
           MOVE COVTLNB-LIENCD         TO COVRLNB-LIENCD.
           MOVE COVTLNB-BAPPMETH       TO COVRLNB-BAPPMETH.             <083>
           MOVE 0                      TO COVRLNB-DEFER-PERD-AMT,
                                          COVRLNB-TOT-MTHLY-BENEFIT,
                                          COVRLNB-COVERAGE-DEBT,
                                          COVRLNB-STAT-SUMINS,
                                          COVRLNB-RTRNYRS,
                                          COVRLNB-PREM-CESS-AGE-MTH,
                                          COVRLNB-PREM-CESS-AGE-DAY,
                                          COVRLNB-PREM-CESS-TERM-MTH,
                                          COVRLNB-PREM-CESS-TERM-DAY,
                                          COVRLNB-RISK-CESS-AGE-MTH,
                                          COVRLNB-RISK-CESS-AGE-DAY,
                                          COVRLNB-RISK-CESS-TERM-MTH,
                                          COVRLNB-RISK-CESS-TERM-DAY.
           MOVE WSAA-TRANSACTION-DATE  TO COVRLNB-TRANSACTION-DATE.
           MOVE WSAA-TRANSACTION-TIME  TO COVRLNB-TRANSACTION-TIME.
           MOVE WSAA-USER              TO COVRLNB-USER.
           MOVE WSAA-TERMID            TO COVRLNB-TERMID.
           MOVE COVTLNB-RESERVE-UNITS-IND
                                       TO COVRLNB-RESERVE-UNITS-IND.
           MOVE COVTLNB-RESERVE-UNITS-DATE
                                      TO COVRLNB-RESERVE-UNITS-DATE.
           MOVE COVTLNB-PAYRSEQNO     TO COVRLNB-PAYRSEQNO.

      * Before the coverage record is written compute the rerate,
      * rereate from and benefit billing dates.
           PERFORM 232D-COMPUTE-DATES.

      * Compute the Benefit Billing.
           PERFORM 233A-BENEFIT-BILLING.

      * If this is a flexible premium contract then we need to w<D9604>
      * an FPCO record at coverage level.                       <D9604>
                                                                        <D9604>
           IF  FLEXIBLE-PREMIUM-CONTRACT                                <D9604>
           AND COVRLNB-INSTPREM NOT = 0                                 <D9604>
              PERFORM 7000-WRITE-FPCO                                   <D9604>
           END-IF.                                                      <D9604>
                                                                        <D9604>
      * Write the covr record.
           MOVE COVRLNBREC             TO COVRLNB-FORMAT.
           MOVE WRITR                  TO COVRLNB-FUNCTION.
           CALL 'COVRLNBIO'            USING COVRLNB-PARAMS.
           IF COVRLNB-STATUZ           NOT = O-K
              MOVE COVRLNB-PARAMS      TO SYSR-PARAMS
              MOVE COVRLNB-STATUZ      TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
      *                                                                 <UL006>
      * Keep Basic Component SA.                                        <UL006>
      *                                                                 <UL006>
           IF  COVRLNB-LIFE             = '01'                          <UL006>
           AND COVRLNB-COVERAGE         = '01'                          <UL006>
           AND COVRLNB-RIDER            = '00'                          <UL006>
               MOVE COVRLNB-SUMINS     TO WSAA-BAS-SUMINS               <UL006>
           END-IF.                                                      <UL006>
TDO   *                                                                 <DA006>
      * Keep Billing Freq. at Issue Transaction for DMS using.          <DA006>
      *                                                                 <DA006>
           PERFORM X100-WRITE-ZPFR-FILE.                                <DA006>

      * After each COVR record is written do benefit billing, stamp
      * duty, extra component processing and commission calculations.

      * Call the benefit billing routine (NOW).
           IF T5534-SUBPROG            NOT = SPACES
               CALL T5534-SUBPROG      USING UBBL-UBBLALL-REC
               MOVE SPACES             TO T5534-T5534-REC
               IF UBBL-STATUZ              NOT = O-K
                  MOVE UBBL-UBBLALL-REC    TO SYSR-PARAMS
                  MOVE UBBL-STATUZ         TO SYSR-STATUZ
                  PERFORM XXXX-FATAL-ERROR.
      *
           PERFORM 2330-SUBROUTINE.

           ADD 1                       TO WSAA-PLAN-SUFFIX.
      *
       232D-EXIT.
           EXIT.
      *
      /
      *
       232D-COMPUTE-DATES SECTION.
      ****************************
       2321-OPTIONAL-EXTRAS.
      *
           MOVE SPACES                 TO WSAA-SPEC-TERMS-EXIST.        <070>
           MOVE SPACES                 TO LEXT-PARAMS.
           MOVE CHDRLNB-CHDRCOY        TO LEXT-CHDRCOY.
           MOVE CHDRLNB-CHDRNUM        TO LEXT-CHDRNUM.
           MOVE COVRLNB-LIFE           TO LEXT-LIFE.
           MOVE COVRLNB-COVERAGE       TO LEXT-COVERAGE.
           MOVE COVRLNB-RIDER          TO LEXT-RIDER.
           MOVE 0                      TO LEXT-SEQNBR.
           MOVE BEGN                   TO LEXT-FUNCTION.
           CALL 'LEXTIO'               USING LEXT-PARAMS.
           IF LEXT-STATUZ              NOT = O-K AND NOT = ENDP
              MOVE LEXT-PARAMS         TO SYSR-PARAMS
              MOVE LEXT-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.

           IF  LEXT-CHDRCOY         NOT = CHDRLNB-CHDRCOY               <070>
           OR  LEXT-CHDRNUM         NOT = CHDRLNB-CHDRNUM               <070>
           OR  LEXT-LIFE            NOT = COVRLNB-LIFE                  <070>
           OR  LEXT-COVERAGE        NOT = COVRLNB-COVERAGE              <070>
           OR  LEXT-RIDER           NOT = COVRLNB-RIDER                 <070>
               MOVE ENDP               TO LEXT-STATUZ.                  <070>
                                                                        <070>
           IF LEXT-STATUZ               = ENDP                          <070>
              MOVE 'N'                 TO WSAA-SPEC-TERMS-EXIST.        <070>

           MOVE VRCM-MAX-DATE          TO WSAA-LEXT-DATE.
           PERFORM 2322-OPTIONAL-EXTRA-PROCESSING
                                       THRU 2323-READ-NEXT-LEXT
                   UNTIL  LEXT-STATUZ         = ENDP
               OR  LEXT-CHDRCOY        NOT = CHDRLNB-CHDRCOY
               OR  LEXT-CHDRNUM        NOT = CHDRLNB-CHDRNUM
               OR  LEXT-COVERAGE       NOT = COVRLNB-COVERAGE
               OR  LEXT-RIDER          NOT = COVRLNB-RIDER.

           GO TO 2323-SET-BEN-BILL-DATE.

       2322-OPTIONAL-EXTRA-PROCESSING.

           IF LEXT-EXT-CESS-TERM        = 0
              GO TO 2323-READ-NEXT-LEXT.

           MOVE '01'                    TO DTC2-FREQUENCY.
           MOVE CHDRLNB-OCCDATE         TO DTC2-INT-DATE-1.
           MOVE LEXT-EXT-CESS-TERM      TO DTC2-FREQ-FACTOR.
           CALL 'DATCON2'               USING DTC2-DATCON2-REC.
           IF DTC2-STATUZ               NOT = O-K
              MOVE DTC2-DATCON2-REC     TO SYSR-PARAMS
              MOVE DTC2-STATUZ          TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.

           IF DTC2-INT-DATE-2           = LEXT-EXT-CESS-DATE
              GO TO 2323-READ-NEXT-LEXT.

           MOVE DTC2-INT-DATE-2         TO LEXT-EXT-CESS-DATE.
           MOVE WSAA-TERMID             TO LEXT-TERMID.
           MOVE WSAA-TRANSACTION-DATE   TO LEXT-TRANSACTION-DATE.
           MOVE WSAA-TRANSACTION-TIME   TO LEXT-TRANSACTION-TIME.
           MOVE WSAA-USER               TO LEXT-USER.
           MOVE LEXTREC                 TO LEXT-FORMAT.
           MOVE UPDAT                   TO LEXT-FUNCTION.
           CALL 'LEXTIO'                USING LEXT-PARAMS.
           IF LEXT-STATUZ               NOT = O-K
              MOVE LEXT-PARAMS          TO SYSR-PARAMS
              MOVE LEXT-STATUZ          TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.

       2323-READ-NEXT-LEXT.

           IF LEXT-EXT-CESS-DATE        < WSAA-LEXT-DATE
              MOVE LEXT-EXT-CESS-DATE   TO WSAA-LEXT-DATE.

           MOVE NEXTR                   TO LEXT-FUNCTION.
           CALL 'LEXTIO'                USING LEXT-PARAMS.
           IF LEXT-STATUZ               NOT = O-K AND NOT = ENDP
              MOVE LEXT-PARAMS          TO SYSR-PARAMS
              MOVE LEXT-STATUZ          TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.

       2323-SET-BEN-BILL-DATE.
           IF T5687-BBMETH              = SPACES
      *****   MOVE VRCM-MAX-DATE        TO COVRLNB-BEN-CESS-DATE        <082>
              MOVE 0                    TO COVRLNB-BEN-BILL-DATE
           ELSE
      *****   MOVE COVTLNB-PREM-CESS-DATE TO COVRLNB-BEN-CESS-DATE      <082>
              MOVE CHDRLNB-OCCDATE      TO WSAA-OCCDATE                 <LA4351>
              MOVE CHDRLNB-OCCDATE      TO COVRLNB-BEN-BILL-DATE.
      *                                                                 <080>
       2324-RERATE-DATES.                                               <080>
      *                                                                 <080>
      ****  Calculate the re-rate date. Add the re-rate frequency from  <080>
      ****  T5687 to the Contract Commencement date. If there is no     <080>
      ****  frequency specified then the re-rate date is the premium    <080>
      ****  cessation date for this component.                          <080>
      *                                                                 <080>
           IF  T5687-RTRNWFREQ     NOT  = 0                             <080>
               MOVE CHDRLNB-OCCDATE      TO DTC2-INT-DATE-1             <080>
               MOVE '01'                 TO DTC2-FREQUENCY              <080>
               MOVE T5687-RTRNWFREQ      TO DTC2-FREQ-FACTOR            <080>
      *                                                                 <080>
               CALL 'DATCON2'           USING DTC2-DATCON2-REC          <080>
      *                                                                 <080>
               IF  DTC2-STATUZ           NOT = O-K                      <080>
                   MOVE DTC2-DATCON2-REC TO SYSR-PARAMS                 <080>
                   MOVE DTC2-STATUZ      TO SYSR-STATUZ                 <080>
                   PERFORM XXXX-FATAL-ERROR                             <080>
               ELSE                                                     <080>
                   MOVE DTC2-INT-DATE-2  TO COVRLNB-RERATE-DATE         <080>
               END-IF                                                   <080>
           ELSE                                                         <080>
               IF CHDRLNB-BILLFREQ     = '00' AND                       <LA3428>
                  T5687-ZSREDTRM   NOT = 'Y'                            <LA3428>
                   MOVE COVRLNB-RISK-CESS-DATE TO COVRLNB-RERATE-DATE   <LA3428>
               ELSE                                                     <LA3428>
               MOVE COVRLNB-PREM-CESS-DATE    TO COVRLNB-RERATE-DATE    <080>
               END-IF                                                   <LA3428>
           END-IF.                                                      <080>
      *                                                                 <080>
      ****  If a Special Term (LEXT) record expires before the re-rate  <080>
      ****  date then set the re-rate date to that instead.             <080>
      *                                                                 <080>
           IF  WSAA-LEXT-DATE              < COVRLNB-RERATE-DATE        <080>
               MOVE WSAA-LEXT-DATE         TO COVRLNB-RERATE-DATE       <080>
           END-IF.                                                      <080>
      *                                                                 <A06484>
      * If the rerate date is greater than the premium cessation date   <A06484>
      * then set the re-rate date to the premium cessation date.        <A06484>
      *                                                                 <A06484>
           IF  COVRLNB-PREM-CESS-DATE   < COVRLNB-RERATE-DATE           <A06484>
               MOVE COVRLNB-PREM-CESS-DATE                              <A06484>
                                       TO COVRLNB-RERATE-DATE           <A06484>
           END-IF.                                                      <A06484>
      *                                                                 <080>
      ****  Set the date to use for the rates when re-rating.           <080>
      ****  If a minimum guarantee period is specified on T5687 then    <080>
      ****  use this to calculate the date that this runs to by         <080>
      ****  adding it to the Contract Commencement date.                <080>
      ****  If there is no minimum guarantee period then the rates      <080>
      ****  from the re-rate date will be used.                         <080>
      *                                                                 <080>
           IF  T5687-PREM-GUAR-PERIOD      = 0                          <080>
               MOVE COVRLNB-RERATE-DATE    TO COVRLNB-RERATE-FROM-DATE  <080>
           ELSE                                                         <080>
               MOVE CHDRLNB-OCCDATE        TO DTC2-INT-DATE-1           <080>
               MOVE '01'                   TO DTC2-FREQUENCY            <080>
               MOVE T5687-PREM-GUAR-PERIOD TO DTC2-FREQ-FACTOR          <080>
      *                                                                 <080>
               CALL 'DATCON2'              USING DTC2-DATCON2-REC       <080>
      *                                                                 <080>
               IF  DTC2-STATUZ         NOT = O-K                        <080>
                   MOVE DTC2-DATCON2-REC   TO SYSR-PARAMS               <080>
                   MOVE DTC2-STATUZ        TO SYSR-STATUZ               <080>
                   PERFORM XXXX-FATAL-ERROR                             <080>
               END-IF                                                   <080>
      *                                                                 <080>
      ****  Check whether the re-rate date is within the minimum        <080>
      ****  guarantee period. If it is then use the rates from the      <080>
      ****  Contract Commencement date. If not use the rates from the   <080>
      ****  re-rate date.                                               <080>
      *                                                                 <080>
               IF  DTC2-INT-DATE-2         < COVRLNB-RERATE-DATE        <080>
                   MOVE COVRLNB-RERATE-DATE TO COVRLNB-RERATE-FROM-DATE <080>
               ELSE                                                     <080>

      * Use the calculated Rerate-From-Date rather than                 <A06503>
      * Contract Commencement Date.                                     <A06503>

      ****         MOVE CHDRLNB-OCCDATE TO COVRLNB-RERATE-FROM-DATE<080><A06503>
                   MOVE DTC2-INT-DATE-2    TO COVRLNB-RERATE-FROM-DATE  <A06503>
               END-IF                                                   <080>
           END-IF.                                                      <080>
      *                                                                 <080>
      *2324-RERATE-DATES.                                               <080>
      *    MOVE CHDRLNB-OCCDATE        TO COVRLNB-RERATE-FROM-DATE.     <080>
      *    MOVE WSAA-LEXT-DATE         TO WSAA-RE-RATE-DATE.            <080>
      *    IF COVRLNB-PREM-CESS-DATE   <  WSAA-RE-RATE-DATE             <080>
      *       MOVE COVRLNB-PREM-CESS-DATE TO WSAA-RE-RATE-DATE.         <080>
      *                                                                 <080>
      *    IF T5687-RTRNWFREQ     NOT  = 0                              <080>
      *       MOVE CHDRLNB-OCCDATE     TO DTC2-INT-DATE-1               <080>
      *       MOVE '01'                TO DTC2-FREQUENCY                <080>
      *       MOVE T5687-RTRNWFREQ     TO DTC2-FREQ-FACTOR              <080>
      *       CALL 'DATCON2'           USING DTC2-DATCON2-REC           <080>
      *       IF DTC2-STATUZ           NOT = O-K                        <080>
      *          MOVE DTC2-DATCON2-REC TO SYSR-PARAMS                   <080>
      *          MOVE DTC2-STATUZ      TO SYSR-STATUZ                   <080>
      *          PERFORM XXXX-FATAL-ERROR                               <080>
      *       ELSE                                                      <080>
      *          IF DTC2-INT-DATE-2    < WSAA-RE-RATE-DATE              <080>
      *             MOVE DTC2-INT-DATE-2 TO WSAA-RE-RATE-DATE,          <080>
      *                                  COVRLNB-RERATE-FROM-DATE.      <080>
      *                                                                 <080>
      *    MOVE WSAA-RE-RATE-DATE      TO COVRLNB-RERATE-DATE.          <080>
      *                                                                 <080>
       2325-CPI-DATES.                                                  <070>
      *                                                                 <070>
           MOVE 99999999               TO COVRLNB-CPI-DATE.             <070>
      *                                                                 <070>
           MOVE CHDRLNB-CHDRCOY        TO INCT-CHDRCOY.                 <070>
           MOVE CHDRLNB-CHDRNUM        TO INCT-CHDRNUM.                 <070>
           MOVE READR                  TO INCT-FUNCTION.                <070>
           CALL 'INCTIO'            USING INCT-PARAMS.                  <070>
           IF  INCT-STATUZ           NOT = O-K                          <070>
           AND INCT-STATUZ           NOT = MRNF                         <070>
              MOVE INCT-PARAMS      TO SYSR-PARAMS                      <070>
              MOVE INCT-STATUZ      TO SYSR-STATUZ                      <070>
              PERFORM XXXX-FATAL-ERROR.                                 <070>
                                                                        <070>
           IF INCT-STATUZ               = MRNF                          <070>
           OR T5687-ANNIVERSARY-METHOD  = SPACES                        <070>
              GO TO 232D-OXIT.                                          <070>
                                                                        <070>
           MOVE SPACES                 TO ITDM-DATA-AREA.               <070>
           MOVE 'IT'                   TO ITDM-ITEMPFX.                 <070>
           MOVE CHDRLNB-CHDRCOY        TO ITDM-ITEMCOY.                 <070>
           MOVE COVRLNB-CRRCD          TO ITDM-ITMFRM.                  <070>
           MOVE T6658                  TO ITDM-ITEMTABL.                <070>
           MOVE T5687-ANNIVERSARY-METHOD                                <070>
                                       TO ITDM-ITEMITEM.                <070>
           MOVE BEGN                   TO ITDM-FUNCTION.                <070>
                                                                        <070>
           CALL 'ITDMIO' USING         ITDM-PARAMS.                     <070>
           IF ITDM-STATUZ              NOT = O-K AND NOT = ENDP         <070>
               MOVE ITDM-PARAMS        TO SYSR-PARAMS                   <070>
               PERFORM XXXX-FATAL-ERROR.                                <070>
                                                                        <070>
           IF ITDM-ITEMCOY             NOT = CHDRLNB-CHDRCOY            <070>
           OR ITDM-ITEMTABL            NOT = T6658                      <070>
           OR ITDM-ITEMITEM            NOT = T5687-ANNIVERSARY-METHOD   <070>
           OR ITDM-STATUZ              = ENDP                           <070>
               MOVE CHDRLNB-CHDRCOY    TO ITDM-ITEMCOY                  <070>
               MOVE T6658              TO ITDM-ITEMTABL                 <070>
               MOVE COVRLNB-CRRCD      TO ITDM-ITMFRM                   <070>
               MOVE T5687-ANNIVERSARY-METHOD                            <070>
                                       TO ITDM-ITEMITEM                 <070>
               MOVE ITDM-PARAMS        TO SYSR-PARAMS                   <070>
               MOVE H036               TO SYSR-STATUZ                   <070>
               PERFORM XXXX-FATAL-ERROR.                                <070>
                                                                        <070>
           MOVE ITDM-GENAREA           TO T6658-T6658-REC.              <070>
      *                                                                 <070>
           IF  WSAA-SPEC-TERMS-EXIST     = SPACES                       <070>
           AND T6658-ADDEXIST            = SPACES                       <070>
              GO TO 232D-OXIT.                                          <070>
                                                                        <070>
           IF T6658-PREMSUBR            = SPACES                        <070>
           OR T6658-BILLFREQ            = '00'                          <070>
              GO TO 232D-OXIT.                                          <070>
                                                                        <070>
           MOVE SPACES                 TO DTC2-DATCON2-REC.             <070>
           MOVE COVRLNB-CRRCD          TO DTC2-INT-DATE-1.              <070>
           MOVE '01'                   TO DTC2-FREQUENCY.               <070>
           MOVE T6658-BILLFREQ         TO DTC2-FREQ-FACTOR.             <070>
      *                                                                 <070>
       2327-CALC-DATE.                                                  <070>
           CALL 'DATCON2'              USING DTC2-DATCON2-REC.          <070>
                                                                        <070>
           IF DTC2-STATUZ              NOT = O-K                        <070>
               MOVE DTC2-DATCON2-REC   TO SYSR-PARAMS                   <070>
               MOVE DTC2-STATUZ        TO SYSR-STATUZ                   <070>
               PERFORM XXXX-FATAL-ERROR.                                <070>
                                                                        <070>
           MOVE DTC2-INT-DATE-2        TO COVRLNB-CPI-DATE.             <070>
                                                                        <070>
           MOVE COVRLNB-CRRCD          TO DTC3-INT-DATE-1.              <070>
           MOVE COVRLNB-PREM-CESS-DATE TO DTC3-INT-DATE-2.              <070>
           MOVE '01'                   TO DTC3-FREQUENCY.               <070>
           CALL 'DATCON3' USING        DTC3-DATCON3-REC.                <070>
           IF DTC3-STATUZ              NOT = O-K                        <070>
               MOVE DTC3-DATCON3-REC   TO SYSR-PARAMS                   <070>
               MOVE DTC3-STATUZ        TO SYSR-STATUZ                   <070>
               PERFORM XXXX-FATAL-ERROR.                                <070>
           ADD 0.99999  TO DTC3-FREQ-FACTOR.                            <070>
           MOVE DTC3-FREQ-FACTOR       TO WSAA-MIN-TRM-TO-CESS.         <070>
                                                                        <070>
      *    IF  T6658-MAX-AGE        NOT > 0                     <V73L03><070>
      *        MOVE 99                 TO T6658-MAX-AGE.        <V73L03><070>
      *                                                         <V73L03><070>
      *    IF  T6658-MINCTRM            > WSAA-MIN-TRM-TO-CESS  <V73L03><070>
      *    OR  T6658-MAX-AGE            < COVRLNB-ANB-AT-CCD    <V73L03><070>
      *        MOVE 99999999               TO COVRLNB-CPI-DATE. <V73L03><070>
                                                                        <070>
           IF  T6658-AGEMAX         NOT > 0                             <V73L03>
      *        MOVE 99                 TO T6658-AGEMAX.         <V73L03><LA4754>
               MOVE 999                TO T6658-AGEMAX.                 <LA4754>
                                                                        <V73L03>
           IF  T6658-MINCTRM            > WSAA-MIN-TRM-TO-CESS          <V73L03>
           OR  T6658-AGEMAX             < COVRLNB-ANB-AT-CCD            <V73L03>
               MOVE 99999999               TO COVRLNB-CPI-DATE.         <V73L03>
       232D-OXIT.
           EXIT.
      *
      /
      *
       232E-DELET-AND-READ-COVT SECTION.
      **********************************
       2321-DELET-AND-READ-COVT.
      *

           MOVE READH                  TO COVTLNB-FUNCTION.
           CALL 'COVTLNBIO'            USING COVTLNB-PARAMS.
           IF COVTLNB-STATUZ           NOT = O-K
              MOVE COVTLNB-PARAMS      TO SYSR-PARAMS
              MOVE COVTLNB-STATUZ      TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.

           MOVE DELET                  TO COVTLNB-FUNCTION.
           CALL 'COVTLNBIO'            USING COVTLNB-PARAMS.
           IF COVTLNB-STATUZ           NOT = O-K
              MOVE COVTLNB-PARAMS      TO SYSR-PARAMS
              MOVE COVTLNB-STATUZ      TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.

           MOVE NEXTR                  TO COVTLNB-FUNCTION.
           CALL 'COVTLNBIO'            USING COVTLNB-PARAMS.
           IF COVTLNB-STATUZ           NOT = O-K
                                       AND NOT = ENDP
              MOVE COVTLNB-PARAMS      TO SYSR-PARAMS
              MOVE COVTLNB-STATUZ      TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.

           IF COVTLNB-CHDRCOY          NOT = CHDRLNB-CHDRCOY
           OR COVTLNB-CHDRNUM          NOT = CHDRLNB-CHDRNUM
           OR COVTLNB-STATUZ           = ENDP
              MOVE ENDP                TO COVTLNB-STATUZ
              GO TO 232E-EXIT.

      * Keep the oldest cessation date.
           IF COVTLNB-PREM-CESS-DATE   < WSAA-OLD-CESS-DATE
              MOVE COVTLNB-PREM-CESS-DATE
                                       TO WSAA-OLD-CESS-DATE.
      *
       232E-EXIT.
           EXIT.
      *
      /
      *
      ***************************************************************
      * Performed after each COVR is written.
      ***************************************************************
       2330-SUBROUTINE SECTION.
      *************************
       2331-SUBROUTINE.

           MOVE 'IT'                   TO DESC-DESCPFX.
           MOVE CHDRLNB-CHDRCOY        TO DESC-DESCCOY.
           MOVE T5645                  TO DESC-DESCTABL.
           MOVE ATMD-LANGUAGE          TO DESC-LANGUAGE.
           MOVE 'P5074'                TO DESC-DESCITEM
           MOVE READR                  TO DESC-FUNCTION.
           CALL 'DESCIO' USING DESC-PARAMS.
           IF DESC-STATUZ              NOT = O-K
           AND DESC-STATUZ             NOT = MRNF
               MOVE DESC-PARAMS        TO SYSR-PARAMS
               MOVE DESC-STATUZ        TO SYSR-STATUZ
               PERFORM XXXX-FATAL-ERROR.

      *
      *   If a description is not found in the signon language then
      *   English is used as a default.
      *
           IF DESC-STATUZ              =  MRNF
              MOVE 'E'                 TO DESC-LANGUAGE
              MOVE READR               TO DESC-FUNCTION

              CALL 'DESCIO' USING DESC-PARAMS

              IF DESC-STATUZ           NOT = O-K
                  MOVE DESC-PARAMS     TO SYSR-PARAMS
                  MOVE DESC-STATUZ     TO SYSR-STATUZ
                  PERFORM XXXX-FATAL-ERROR
              END-IF

           END-IF.

      *                                                         <V4L001><V4L001>
           MOVE COVRLNB-CRTABLE         TO LIFA-SUBSTITUTE-CODE(06).    <V4L001>
                                                                        <V4L001>
           PERFORM 233B-STAMP-DUTY.
           PERFORM 233C-COMPONENT-PROCESSING.
                                                                        <V70L01>
           IF TH605-BONUS-IND          NOT = 'Y'                        <V70L01>
              GO TO 2333-SKIP-BONUS-WORKBENCH                           <V70L01>
           END-IF.                                                      <V70L01>
                                                                        <V70L01>
      *******************                                               <V70L01>
      * Bonus Workbench *                                               <V70L01>
      *******************                                               <V70L01>
                                                                        <V70L01>
      * Single Premium                                                  <V70L01>
           IF COVRLNB-SINGP            NOT = 0                          <V70L01>
              MOVE SPACES              TO ZPTN-PARAMS                   <V70L01>
              MOVE COVRLNB-CHDRCOY     TO ZPTN-CHDRCOY                  <V70L01>
              MOVE COVRLNB-CHDRNUM     TO ZPTN-CHDRNUM                  <V70L01>
              MOVE COVRLNB-LIFE        TO ZPTN-LIFE                     <V70L01>
              MOVE COVRLNB-COVERAGE    TO ZPTN-COVERAGE                 <V70L01>
              MOVE COVRLNB-RIDER       TO ZPTN-RIDER                    <V70L01>
              MOVE COVRLNB-TRANNO      TO ZPTN-TRANNO                   <V70L01>
              MOVE COVRLNB-SINGP       TO ZPTN-ORIGAMT                  <V70L01>
              MOVE WSKY-BATC-BATCTRCDE TO ZPTN-TRANS-CODE               <V70L01>
              MOVE COVRLNB-CRRCD       TO ZPTN-EFFDATE                  <V70L01>
                                          ZPTN-BILLCD                   <V70L01>
                                          ZPTN-INSTFROM                 <V70L01>
                                          ZPTN-INSTTO                   <V70L01>
              MOVE DTC1-INT-DATE       TO ZPTN-TRANDATE                 <V70L01>
              MOVE 'S'                 TO ZPTN-ZPRFLG                   <V70L01>
              MOVE ZPTNREC             TO ZPTN-FORMAT                   <V70L01>
              MOVE WRITR               TO ZPTN-FUNCTION                 <V70L01>
                                                                        <V70L01>
              CALL 'ZPTNIO'            USING ZPTN-PARAMS                <V70L01>
                                                                        <V70L01>
              IF ZPTN-STATUZ           NOT = O-K                        <V70L01>
                 MOVE ZPTN-STATUZ      TO SYSR-STATUZ                   <V70L01>
                 MOVE ZPTN-PARAMS      TO SYSR-PARAMS                   <V70L01>
                 PERFORM XXXX-FATAL-ERROR                               <V70L01>
              END-IF                                                    <V70L01>
           END-IF.                                                      <V70L01>
                                                                        <V70L01>
      * Regular Premium                                                 <V70L01>
                                                                        <V70L01>
      ***  MOVE PAYR-BILLFREQ            TO WSAA-BILLFQ-9.      <LA5139><V70L01>
           MOVE WSAA-BILLFREQ(WSBB-SUB)  TO WSAA-BILLFQ-9.              <LA5139>
                                                                        <V70L01>
           IF WSAA-FREQ-FACTOR(WSBB-SUB)  > WSAA-BILLFQ-9               <V70L01>
              SUBTRACT WSAA-BILLFQ-9   FROM WSAA-FREQ-FACTOR(WSBB-SUB)  <V70L01>
                                       GIVING WSAA-EXCESS-INST-NUM      <V70L01>
                                                                        <V70L01>
              MULTIPLY COVRLNB-INSTPREM BY WSAA-BILLFQ-9                <V70L01>
                                       GIVING WSAA-REG-PREM-FIRST       <V70L01>
                                                                        <V70L01>
              MULTIPLY COVRLNB-INSTPREM BY WSAA-EXCESS-INST-NUM         <V70L01>
                                       GIVING WSAA-REG-PREM-RENEWAL     <V70L01>
                                                                        <V70L01>
              INITIALIZE               DTC4-DATCON4-REC                 <V70L01>
              MOVE 1                   TO DTC4-FREQ-FACTOR              <V70L01>
              MOVE '01'                TO DTC4-FREQUENCY                <V70L01>
              MOVE CHDRLNB-OCCDATE     TO DTC4-INT-DATE-1               <V70L01>
      ***     MOVE PAYR-BILLDAY        TO DTC4-BILLDAY          <LA5139><V70L01>
      ***     MOVE PAYR-BILLMONTH      TO DTC4-BILLMONTH        <LA5139><V70L01>
              MOVE WSAA-BILLDAY(WSBB-SUB)                               <LA5139>
                                       TO DTC4-BILLDAY                  <LA5139>
              MOVE WSAA-BILLMONTH(WSBB-SUB)                             <LA5139>
                                       TO DTC4-BILLMONTH                <LA5139>
                                                                        <V70L01>
              CALL 'DATCON4'           USING DTC4-DATCON4-REC           <V70L01>
                                                                        <V70L01>
              IF DTC4-STATUZ           NOT = O-K                        <V70L01>
                 MOVE DTC4-STATUZ      TO SYSR-STATUZ                   <V70L01>
                 MOVE DTC4-DATCON4-REC TO SYSR-PARAMS                   <V70L01>
                 PERFORM XXXX-FATAL-ERROR                               <V70L01>
              END-IF                                                    <V70L01>
                                                                        <V70L01>
              MOVE DTC4-INT-DATE-2     TO WSAA-REG-INTM-DATE            <V70L01>
                                                                        <V70L01>
           ELSE                                                         <V70L01>
              MULTIPLY COVRLNB-INSTPREM   BY WSAA-FREQ-FACTOR(WSBB-SUB) <V70L01>
                                          GIVING WSAA-REG-PREM-FIRST    <V70L01>
              MOVE ZERO                TO WSAA-REG-PREM-RENEWAL         <V70L01>
      ***     MOVE PAYR-BTDATE         TO WSAA-REG-INTM-DATE    <LA5139><V70L01>
              MOVE WSAA-BTDATE(WSBB-SUB)                                <LA5139>
                                       TO WSAA-REG-INTM-DATE            <LA5139>
           END-IF.                                                      <V70L01>
                                                                        <V70L01>
      * Regular Premium - First Year                                    <V70L01>
                                                                        <V70L01>
           IF WSAA-REG-PREM-FIRST      NOT = 0                          <V70L01>
              MOVE SPACES              TO ZPTN-PARAMS                   <V70L01>
              MOVE COVRLNB-CHDRCOY     TO ZPTN-CHDRCOY                  <V70L01>
              MOVE COVRLNB-CHDRNUM     TO ZPTN-CHDRNUM                  <V70L01>
              MOVE COVRLNB-LIFE        TO ZPTN-LIFE                     <V70L01>
              MOVE COVRLNB-COVERAGE    TO ZPTN-COVERAGE                 <V70L01>
              MOVE COVRLNB-RIDER       TO ZPTN-RIDER                    <V70L01>
              MOVE COVRLNB-TRANNO      TO ZPTN-TRANNO                   <V70L01>
              MOVE WSAA-REG-PREM-FIRST TO ZPTN-ORIGAMT                  <V70L01>
              MOVE WSKY-BATC-BATCTRCDE TO ZPTN-TRANS-CODE               <V70L01>
              MOVE CHDRLNB-OCCDATE     TO ZPTN-EFFDATE                  <V70L01>
                                          ZPTN-BILLCD                   <V70L01>
                                          ZPTN-INSTFROM                 <V70L01>
              MOVE WSAA-REG-INTM-DATE  TO ZPTN-INSTTO                   <V70L01>
              MOVE DTC1-INT-DATE       TO ZPTN-TRANDATE                 <V70L01>
              MOVE 'I'                 TO ZPTN-ZPRFLG                   <V70L01>
              MOVE ZPTNREC             TO ZPTN-FORMAT                   <V70L01>
              MOVE WRITR               TO ZPTN-FUNCTION                 <V70L01>
                                                                        <V70L01>
              CALL 'ZPTNIO'            USING ZPTN-PARAMS                <V70L01>
                                                                        <V70L01>
              IF ZPTN-STATUZ           NOT = O-K                        <V70L01>
                 MOVE ZPTN-STATUZ      TO SYSR-STATUZ                   <V70L01>
                 MOVE ZPTN-PARAMS      TO SYSR-PARAMS                   <V70L01>
                 PERFORM XXXX-FATAL-ERROR                               <V70L01>
              END-IF                                                    <V70L01>
           END-IF.                                                      <V70L01>
                                                                        <V70L01>
      * Regular Premium - Renewal Year                                  <V70L01>
                                                                        <V70L01>
           IF WSAA-REG-PREM-RENEWAL    NOT = 0                          <V70L01>
              MOVE SPACES              TO ZPTN-PARAMS                   <V70L01>
              MOVE COVRLNB-CHDRCOY     TO ZPTN-CHDRCOY                  <V70L01>
              MOVE COVRLNB-CHDRNUM     TO ZPTN-CHDRNUM                  <V70L01>
              MOVE COVRLNB-LIFE        TO ZPTN-LIFE                     <V70L01>
              MOVE COVRLNB-COVERAGE    TO ZPTN-COVERAGE                 <V70L01>
              MOVE COVRLNB-RIDER       TO ZPTN-RIDER                    <V70L01>
              MOVE COVRLNB-TRANNO      TO ZPTN-TRANNO                   <V70L01>
              MOVE WSAA-REG-PREM-RENEWAL TO ZPTN-ORIGAMT                <V70L01>
              MOVE WSKY-BATC-BATCTRCDE TO ZPTN-TRANS-CODE               <V70L01>
              MOVE CHDRLNB-OCCDATE     TO ZPTN-BILLCD                   <V70L01>
              MOVE WSAA-REG-INTM-DATE  TO ZPTN-EFFDATE                  <V70L01>
                                          ZPTN-INSTFROM                 <V70L01>
      ***     MOVE PAYR-BTDATE         TO ZPTN-INSTTO           <LA5139><V70L01>
              MOVE WSAA-BTDATE(WSBB-SUB)                                <LA5139>
                                       TO ZPTN-INSTTO                   <LA5139>
              MOVE DTC1-INT-DATE       TO ZPTN-TRANDATE                 <V70L01>
              MOVE 'R'                 TO ZPTN-ZPRFLG                   <V70L01>
              MOVE ZPTNREC             TO ZPTN-FORMAT                   <V70L01>
              MOVE WRITR               TO ZPTN-FUNCTION                 <V70L01>
                                                                        <V70L01>
              CALL 'ZPTNIO'            USING ZPTN-PARAMS                <V70L01>
                                                                        <V70L01>
              IF ZPTN-STATUZ           NOT = O-K                        <V70L01>
                 MOVE ZPTN-STATUZ      TO SYSR-STATUZ                   <V70L01>
                 MOVE ZPTN-PARAMS      TO SYSR-PARAMS                   <V70L01>
                 PERFORM XXXX-FATAL-ERROR                               <V70L01>
              END-IF                                                    <V70L01>
           END-IF.                                                      <V70L01>
                                                                        <V70L01>
       2333-SKIP-BONUS-WORKBENCH.                                       <V70L01>
                                                                        <V70L01>
      ****                                                      <R96REA><070>
      ***Introducing Reassurance RACT record processing         <R96REA><070>
      ****                                                      <R96REA><070>
                                                                        <R96REA>
      *  Activate Reassurance Cessions (RACD records) if require        <R96REA>
                                                                        <R96REA>
           PERFORM 4000-CHECK-FOR-REASSURANCE.                          <070>
                                                                        <070>

      * We need the component premiums to do the tax posting            <V74L01>
      * move the skiping of the component posting further down this     <V74L01>
      * section.                                                        <V74L01>
      *                                                                 <V74L01>
      **** IF NOT COMP-LEVEL-ACC                                        <V74L01>
      ****     GO TO 2339-BEGIN-PCCD.                                   <V74L01>
      *                                                                 <V74L01>
      *
           MOVE 'PSTW'                     TO LIFA-FUNCTION.
           MOVE ATMD-BATCH-KEY             TO LIFA-BATCKEY.
           MOVE WSAA-TRANSACTION-TIME      TO LIFA-TRANSACTION-TIME.
           MOVE WSAA-TERMID                TO LIFA-TERMID.
           MOVE WSAA-USER                  TO LIFA-USER.
           MOVE CHDRLNB-CHDRNUM            TO LIFA-RDOCNUM,
           MOVE CHDRLNB-CHDRCOY            TO LIFA-RLDGCOY.
           MOVE CHDRLNB-CNTCURR            TO LIFA-ORIGCURR.
           MOVE CHDRLNB-TRANNO             TO LIFA-TRANNO.
           MOVE DESC-LONGDESC              TO LIFA-TRANDESC.
           MOVE 0                          TO LIFA-CRATE,
                                              LIFA-ACCTAMT,
                                              LIFA-RCAMT.
           MOVE SPACES                     TO LIFA-GENLCUR.
           MOVE CHDRLNB-CHDRCOY            TO LIFA-GENLCOY.
           MOVE SPACES                     TO LIFA-POSTYEAR,
                                              LIFA-POSTMONTH.
           MOVE CHDRLNB-OCCDATE            TO LIFA-EFFDATE.
           MOVE VRCM-MAX-DATE              TO LIFA-FRCDATE.
           MOVE CHDRLNB-CNTTYPE            TO LIFA-SUBSTITUTE-CODE(01).
           MOVE +0                         TO WSAA-JRNSEQ.
           MOVE COVRLNB-CHDRNUM            TO WSAA-RLDG-CHDRNUM.
           MOVE COVRLNB-LIFE               TO WSAA-RLDG-LIFE.
           MOVE COVRLNB-COVERAGE           TO WSAA-RLDG-COVERAGE.
           MOVE COVRLNB-RIDER              TO WSAA-RLDG-RIDER.
           MOVE COVRLNB-PLAN-SUFFIX        TO WSAA-PLAN.
           MOVE WSAA-PLANSUFF              TO WSAA-RLDG-PLAN-SUFFIX.
           MOVE WSAA-RLDGACCT              TO LIFA-RLDGACCT.
      *
      * If component level account applicable,
      * do the premium postings for this component.
      *
      * Store premiums.
      *
           MOVE COVRLNB-SINGP              TO WSAA-COMP-SING-PREM.
           MOVE COVRLNB-INSTPREM           TO WSAA-COMP-REG-PREM.
      *
      * Adjust Regular Premium (WSBB-SUB will still be set as the
      * payor for this component).
      *
           MULTIPLY          WSAA-COMP-REG-PREM
           BY                WSAA-FREQ-FACTOR(WSBB-SUB)
           GIVING            WSAA-COMP-REG-PREM.

              MOVE COVRLNB-CRTABLE         TO LIFA-SUBSTITUTE-CODE(06).
                                                                        <V74L01>
      *                                                                 <V74L01>
      * skip the posting of accounts at this level if we have a         <V74L01>
      * contract level posting.                                         <V74L01>
      * redirect to post the premium tax before checking PCDD           <V74L01>
      *                                                                 <V74L01>
           IF NOT COMP-LEVEL-ACC                                        <V74L01>
               GO TO 233D-PREM-TAX                                      <V74L01>
           END-IF.                                                      <V74L01>

       233D-POST-SINGLE-PREMIUM.
           IF WSAA-COMP-SING-PREM NOT = 0
              MOVE WSAA-T5645-SACSCODE(04)      TO LIFA-SACSCODE
              MOVE WSAA-T5645-SACSTYPE(04)      TO LIFA-SACSTYP
              MOVE WSAA-T5645-GLMAP(04)         TO LIFA-GLCODE
              MOVE WSAA-T5645-SIGN(04)          TO LIFA-GLSIGN
              MOVE WSAA-T5645-CNTTOT(04)        TO LIFA-CONTOT
              MOVE COVRLNB-CHDRNUM         TO LIFA-TRANREF
              MOVE WSAA-SERVAG             TO LIFA-TRANREF(20:8)        <DA005>
              MOVE WSAA-COMP-SING-PREM     TO LIFA-ORIGAMT
              ADD  +1                      TO WSAA-JRNSEQ
              MOVE WSAA-JRNSEQ             TO LIFA-JRNSEQ
              CALL 'LIFACMV'               USING LIFA-LIFACMV-REC
              PERFORM 2950-LIFACMV-CHECK.

       233D-POST-REGULAR-PREMIUM.
           IF WSAA-COMP-REG-PREM NOT = 0
              MOVE WSAA-T5645-SACSCODE(05)      TO LIFA-SACSCODE
              MOVE WSAA-T5645-SACSTYPE(05)      TO LIFA-SACSTYP
              MOVE WSAA-T5645-GLMAP(05)         TO LIFA-GLCODE
              MOVE WSAA-T5645-SIGN(05)          TO LIFA-GLSIGN
              MOVE WSAA-T5645-CNTTOT(05)        TO LIFA-CONTOT
              MOVE COVRLNB-CHDRNUM         TO LIFA-TRANREF
              MOVE WSAA-SERVAG             TO LIFA-TRANREF(20:8)        <DA005>
              MOVE WSAA-COMP-REG-PREM      TO LIFA-ORIGAMT
              ADD  +1                      TO WSAA-JRNSEQ
              MOVE WSAA-JRNSEQ             TO LIFA-JRNSEQ
              CALL 'LIFACMV'               USING LIFA-LIFACMV-REC
              PERFORM 2950-LIFACMV-CHECK.

       233D-PREM-TAX.                                                   <V74L01>
           IF COVRLNB-SINGP             NOT = ZERO                      <V74L01>
           OR COVRLNB-INSTPREM          NOT = ZERO                      <V74L01>
              IF TR52D-TXCODE           NOT = SPACE                     <V74L01>
                 PERFORM 233G-PREM-TAX                                  <V74L01>
              END-IF                                                    <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
       2339-BEGIN-PCCD.
      * Begin PCDDLNB file.
           MOVE SPACES                 TO PCDDLNB-PARAMS.
           MOVE CHDRLNB-CHDRCOY        TO PCDDLNB-CHDRCOY.
           MOVE CHDRLNB-CHDRNUM        TO PCDDLNB-CHDRNUM.
           MOVE BEGN                   TO PCDDLNB-FUNCTION.
           MOVE 1                      TO WSAA-AGENT-SUB.
           PERFORM 233D-COMMISSION-AGENT
                UNTIL PCDDLNB-CHDRCOY  NOT = CHDRLNB-CHDRCOY
                   OR PCDDLNB-CHDRNUM  NOT = CHDRLNB-CHDRNUM
                   OR PCDDLNB-STATUZ   = ENDP.
      *
       2339-EXIT.
           EXIT.
      *
      /
      *
       233A-BENEFIT-BILLING SECTION.
      ******************************
       2331-BENEFIT-BILLING.

      * No need to check the frequency of the contract as we now have
      * a separate field for Single and Regular Prems. The correct one
      * will have been filled in at New Business!


      *    Accumulate the Premiums.

           MOVE COVRLNB-PAYRSEQNO  TO WSBB-SUB.
           ADD COVRLNB-SINGP       TO WSAA-SING-PREM-ACC(WSBB-SUB).
           ADD COVRLNB-INSTPREM    TO WSAA-REG-PREM-ACC(WSBB-SUB).

      * If the benefit billing method is blank there will be no
      * benefit billing subroutine to call so we've finished!

           IF T5687-BBMETH             = SPACES
              GO TO 233A-EXIT.


       2334-CALL-SUBROUTINE.
      * Fetch relevent billing subroutine.
           MOVE SPACES                 TO ITEM-DATA-AREA.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE ATMD-COMPANY           TO ITEM-ITEMCOY.
           MOVE T5534                  TO ITEM-ITEMTABL.
           MOVE T5687-BBMETH           TO ITEM-ITEMITEM.
           MOVE READR                  TO ITEM-FUNCTION.
           CALL 'ITEMIO' USING         ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = O-K
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              MOVE ITEM-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
           MOVE ITEM-GENAREA            TO T5534-T5534-REC.


           IF T5534-SUBPROG            = SPACES
              GO TO 233A-EXIT.

           MOVE SPACES                 TO UBBL-UBBLALL-REC.
           MOVE CHDRLNB-CHDRCOY        TO UBBL-CHDR-CHDRCOY.
           MOVE CHDRLNB-CHDRNUM        TO UBBL-CHDR-CHDRNUM.
           MOVE COVRLNB-LIFE           TO UBBL-LIFE-LIFE.
           MOVE COVRLNB-JLIFE          TO UBBL-LIFE-JLIFE.
           MOVE COVRLNB-COVERAGE       TO UBBL-COVR-COVERAGE.
           MOVE COVRLNB-RIDER          TO UBBL-COVR-RIDER.
           MOVE COVRLNB-PLAN-SUFFIX    TO UBBL-PLAN-SUFFIX.
           MOVE T5534-UNIT-FREQ        TO UBBL-BILLFREQ.
           MOVE CHDRLNB-CNTCURR        TO UBBL-CNTCURR.
           MOVE CHDRLNB-CNTTYPE        TO UBBL-CNTTYPE.
           MOVE CHDRLNB-TRANNO         TO UBBL-TRANNO.
           MOVE COVRLNB-CRRCD          TO UBBL-EFFDATE.
           MOVE T5534-PREMMETH         TO UBBL-PREM-METH.
           MOVE T5534-JL-PREM-METH     TO UBBL-JLIFE-PREM-METH.
           MOVE COVRLNB-SUMINS         TO UBBL-SUMINS.
           MOVE COVRLNB-PREM-CESS-DATE TO UBBL-PREM-CESS-DATE.
           MOVE COVRLNB-CRTABLE        TO UBBL-CRTABLE.
           MOVE COVTLNB-BILLCHNL       TO UBBL-BILLCHNL.
           MOVE COVTLNB-MORTCLS        TO UBBL-MORTCLS.
           MOVE T5534-SV-METHOD        TO UBBL-SV-METHOD.
           MOVE ATMD-LANGUAGE          TO UBBL-LANGUAGE.
           MOVE WSAA-USER              TO UBBL-USER.
           MOVE WSKY-BATC-BATCCOY      TO UBBL-BATCCOY.
           MOVE WSKY-BATC-BATCBRN      TO UBBL-BATCBRN.
           MOVE WSKY-BATC-BATCACTYR    TO UBBL-BATCACTYR.
           MOVE WSKY-BATC-BATCACTMN    TO UBBL-BATCACTMN.
           MOVE WSKY-BATC-BATCTRCDE    TO UBBL-BATCTRCDE.
           MOVE WSKY-BATC-BATCBATCH    TO UBBL-BATCH.
           MOVE CHDRLNB-TRANNO         TO UBBL-TRANNO
           MOVE T5534-ADFEEMTH         TO UBBL-ADFEEMTH.
           MOVE 'ISSUE'                TO UBBL-FUNCTION.
           MOVE CHDRLNB-POLSUM         TO UBBL-POLSUM.
           MOVE CHDRLNB-PTDATE         TO UBBL-PTDATE.
           MOVE CHDRLNB-POLINC         TO UBBL-POLINC.
           MOVE ZEROES                 TO UBBL-SINGP.                   <CAS1.0>
           MOVE CHDRLNB-OCCDATE        TO UBBL-OCCDATE.                 <V65L16>
           MOVE CHDRLNB-REGISTER       TO UBBL-CHDR-REGISTER.           <V74L01>

      * Get the next benefit billing date.
      *    MOVE SPACE                  TO DTC2-DATCON2-REC.             <LA4351>
      *    MOVE T5534-UNIT-FREQ        TO DTC2-FREQUENCY.               <LA4351>
      *    MOVE 1                      TO DTC2-FREQ-FACTOR.             <LA4351>
      *    MOVE COVRLNB-CRRCD          TO DTC2-INT-DATE-1.              <LA4351>
      *    MOVE 0                      TO DTC2-INT-DATE-2.              <LA4351>
      *    CALL 'DATCON2'              USING DTC2-DATCON2-REC.          <LA4351>
      *    IF DTC2-STATUZ              NOT = O-K                        <LA4351>
      *       MOVE DTC2-DATCON2-REC    TO SYSR-PARAMS                   <LA4351>
      *       MOVE DTC2-STATUZ         TO SYSR-STATUZ                   <LA4351>
      *       PERFORM XXXX-FATAL-ERROR.                                 <LA4351>
      *    MOVE DTC2-INT-DATE-2        TO COVRLNB-BEN-BILL-DATE.        <LA4351>
      *
           INITIALIZE                  DTC4-DATCON4-REC.                <LA4351>
           MOVE T5534-UNIT-FREQ        TO DTC4-FREQUENCY.               <LA4351>
           MOVE 1                      TO DTC4-FREQ-FACTOR.             <LA4351>
           MOVE COVRLNB-CRRCD          TO DTC4-INT-DATE-1.              <LA4351>
           MOVE 0                      TO DTC4-INT-DATE-2.              <LA4351>
           MOVE WSAA-OCC-DD            TO DTC4-BILLDAY-NUM.             <LA4351>
           MOVE WSAA-OCC-MM            TO DTC4-BILLMONTH-NUM.           <LA4351>
      *                                                                 <LA4351>
           CALL 'DATCON4'           USING DTC4-DATCON4-REC.             <LA4351>
      *                                                                 <LA4351>
           IF  DTC4-STATUZ         NOT = O-K                            <LA4351>
               MOVE DTC4-DATCON4-REC   TO SYSR-PARAMS                   <LA4351>
               MOVE DTC4-STATUZ        TO SYSR-STATUZ                   <LA4351>
               PERFORM XXXX-FATAL-ERROR                                 <LA4351>
           END-IF.                                                      <LA4351>
      *                                                                 <LA4351>
           MOVE DTC4-INT-DATE-2        TO COVRLNB-BEN-BILL-DATE.        <LA4351>
      *                                                                 <LA4351>
       233A-EXIT.
           EXIT.
      *
      /
      *
       233B-STAMP-DUTY SECTION.
      *************************
       2331-STAMP-DUTY.
      *
      * If no stamp duty then exit otherwise call the stamp duty
      * calculation subroutine.
           IF T5687-STAMP-DUTY-METH    = SPACES
              GO TO 233B-EXIT.

      * Fetch relevent stamp duty subroutine.
           MOVE SPACES                 TO ITEM-DATA-AREA.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE ATMD-COMPANY           TO ITEM-ITEMCOY.
           MOVE T5676                  TO ITEM-ITEMTABL.
           MOVE T5687-STAMP-DUTY-METH  TO ITEM-ITEMITEM.
           MOVE READR                  TO ITEM-FUNCTION.
           CALL 'ITEMIO' USING         ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = O-K
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              MOVE ITEM-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
           MOVE ITEM-GENAREA            TO T5676-T5676-REC.

           IF T5676-SUBPROG            = SPACES
              GO TO 233B-EXIT.

           MOVE CHDRLNB-CHDRCOY        TO STDT-COMPANY.
           MOVE CHDRLNB-CHDRNUM        TO STDT-CHDRNUM.
           MOVE COVRLNB-LIFE           TO STDT-LIFE.
           MOVE COVRLNB-COVERAGE       TO STDT-COVERAGE.
           MOVE COVRLNB-RIDER          TO STDT-RIDER.
           MOVE COVRLNB-PLAN-SUFFIX    TO STDT-PLNSFX.
           MOVE CHDRLNB-CNTCURR        TO STDT-CNTCURR.
           MOVE COVRLNB-CRRCD          TO STDT-EFFDATE.
           MOVE 0                      TO STDT-STAMP-DUTY.
           CALL T5676-SUBPROG          USING STDT-STDT001-REC.
      *
           IF STDT-STATUZ              NOT = O-K
              MOVE STDT-STDT001-REC    TO SYSR-PARAMS
              MOVE STDT-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.

           MOVE STDT-STAMP-DUTY        TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           ADD  ZRDP-AMOUNT-OUT        TO WSAA-STAMP-DUTY-ACC.          <V76F06>
                                                                        <V76F06>
      **** ADD STDT-STAMP-DUTY         TO WSAA-STAMP-DUTY-ACC.          <V76F06>
      *
       233B-EXIT.
           EXIT.
      *
      /
      *
      ***************************************************************
      * Lookup the generic component processing programs (T5671)
      * and call each non blank subroutine.
      ***************************************************************
      *
       233C-COMPONENT-PROCESSING SECTION.
      ***********************************
      *2331-COMPONENT-PROCESSING.                                       <073>
      *
           MOVE WSKY-BATC-BATCTRCDE    TO WSAA-COMPKEY-TRANNO.
           MOVE COVRLNB-CRTABLE        TO WSAA-COMPKEY-CRTABLE.
                                                                        <UL001>
           IF  COVRLNB-LIFE             = '01'                          <UL001>
           AND COVRLNB-COVERAGE         = '01'                          <UL001>
           AND COVRLNB-RIDER            = '00'                          <UL001>
               MOVE COVRLNB-CRTABLE    TO WSAA-EXCPKEY-CRTABLE          <UL001>
               MOVE COVRLNB-CRRCD      TO WSAA-EFFDATE                  <UL001>
           END-IF.                                                      <UL001>
                                                                        <UL001>
      * Fetch coverage/rider generic component processing progs.
           MOVE SPACES                 TO ITEM-DATA-AREA.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE ATMD-COMPANY           TO ITEM-ITEMCOY.
           MOVE T5671                  TO ITEM-ITEMTABL.
           MOVE WSAA-COMPKEY           TO ITEM-ITEMITEM.
           MOVE READR                  TO ITEM-FUNCTION.
           CALL 'ITEMIO' USING         ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = O-K AND NOT = MRNF
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              MOVE ITEM-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.

           IF ITEM-STATUZ              = MRNF
              GO TO 233C-EXIT.

           MOVE ITEM-GENAREA            TO T5671-T5671-REC.

           MOVE CHDRLNB-CHDRCOY        TO ISUA-COMPANY.
           MOVE CHDRLNB-CHDRNUM        TO ISUA-CHDRNUM.
           MOVE COVRLNB-LIFE           TO ISUA-LIFE.
           MOVE COVRLNB-LIFE           TO WSAA-LIFE-NUM.
           MOVE COVRLNB-COVERAGE       TO ISUA-COVERAGE.
           MOVE COVRLNB-COVERAGE       TO WSAA-COVERAGE-NUM.
           MOVE COVRLNB-RIDER          TO ISUA-RIDER.
           MOVE COVRLNB-PLAN-SUFFIX    TO ISUA-PLAN-SUFFIX.
           MOVE COVRLNB-PAYRSEQNO      TO WSBB-SUB.
           MOVE WSAA-FREQ-FACTOR(WSBB-SUB) TO ISUA-FREQ-FACTOR.
           MOVE ATMD-BATCH-KEY         TO ISUA-BATCHKEY.
           MOVE WSAA-TRANSACTION-DATE  TO ISUA-TRANSACTION-DATE.
           MOVE WSAA-TRANSACTION-TIME  TO ISUA-TRANSACTION-TIME.
           MOVE WSAA-USER              TO ISUA-USER.
           MOVE WSAA-TERMID            TO ISUA-TERMID.
           MOVE WSAA-NO-SUMMARY-REC    TO ISUA-CONVERT-UNLT.
           MOVE WSAA-COVT-INSTPREM(WSAA-LIFE-NUM WSAA-COVERAGE-NUM)
                                       TO ISUA-COVR-INSTPREM.
           MOVE WSAA-COVT-SINGP(WSAA-LIFE-NUM WSAA-COVERAGE-NUM)
                                       TO ISUA-COVR-SINGP.
           MOVE COVRLNB-CRRCD          TO ISUA-EFFDATE.                 <070>
      *                                                                 <070>
      *    This new field, ISUA-NEW-TRANNO, is NOT used for issue.      <070>
      *     just initialise to ZEROS. It is only used by the            <070>
      *     subroutine GRVULCHG when trying to Reverse INCI records     <070>
      *     affected by a Component Modify.                             <070>
      *                                                                 <070>
           MOVE ZEROS                  TO ISUA-NEW-TRANNO.              <070>
           MOVE SPACES                 TO ISUA-FUNCTION.                <071>
      *
           MOVE SPACES                 TO ISUA-OLDCOVR.                 <076>
           MOVE SPACES                 TO ISUA-OLDRIDER.                <076>
      *                                                                 <A05691>
      ****  Pass the Language field to the Issue Subroutine.            <A05691>
      *                                                                 <A05691>
           MOVE ATMD-LANGUAGE          TO ISUA-LANGUAGE.                <A05691>
      *                                                                 <D9703>
           MOVE COVRLNB-CRRCD          TO ISUA-RUN-DATE.                <D9703>
      *                                                                 <076>
      *
           PERFORM 2332-SUB-CALL       VARYING WSAA-SUB FROM 1 BY 1
                                       UNTIL WSAA-SUB > 3.
      *
      *
      * Do the subroutine calls.
       2332-SUB-CALL.
           MOVE KEEPS                  TO CHDRLNB-FUNCTION.
           MOVE CHDRLNBREC             TO CHDRLNB-FORMAT.
           CALL 'CHDRLNBIO'            USING CHDRLNB-PARAMS.
           IF CHDRLNB-STATUZ           NOT = O-K
              MOVE CHDRLNB-PARAMS      TO SYSR-PARAMS
              MOVE CHDRLNB-STATUZ      TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
           MOVE O-K                    TO ISUA-STATUZ.
           IF T5671-SUBPROG(WSAA-SUB)  NOT = SPACES
              CALL T5671-SUBPROG(WSAA-SUB)
                                       USING ISUA-ISUALL-REC
              IF ISUA-STATUZ NOT = O-K
                  MOVE ISUA-STATUZ        TO SYSR-STATUZ
                  MOVE ISUA-ISUALL-REC    TO SYSR-PARAMS
                  PERFORM XXXX-FATAL-ERROR.

           IF ISUA-STATUZ NOT = O-K
               MOVE ISUA-STATUZ        TO SYSR-STATUZ
               MOVE ISUA-ISUALL-REC    TO SYSR-PARAMS
               PERFORM XXXX-FATAL-ERROR.

           MOVE SPACE                  TO WSAA-NO-SUMMARY-REC.
           MOVE RETRV                  TO CHDRLNB-FUNCTION.
           CALL 'CHDRLNBIO'            USING CHDRLNB-PARAMS.
           IF CHDRLNB-STATUZ           NOT = O-K
              MOVE CHDRLNB-PARAMS      TO SYSR-PARAMS
              MOVE CHDRLNB-STATUZ      TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
      *
       233C-EXIT.
           EXIT.
      *
      /
      *
       233D-COMMISSION-AGENT SECTION.
      *******************************
       2331-COMMISSION-AGENT.
      *
      * Zeroise values for this coverage/agent combination.
      *
           MOVE ZERO                   TO WSAA-COMP-SING-PREM
                                          WSAA-COMP-REG-PREM
                                          WSAA-COMP-ERN-ICOMM
                                          WSAA-COMP-ADV-ICOMM
                                          WSAA-COMP-ERN-SCOMM
                                          WSAA-COMP-ERN-RCOMM
                                          WSAA-COMP-ERN-OCOMM
                                          WSAA-COMP-ADV-OCOMM
 ******                                   WSAA-COMP-TAX-RELIEF.         <086>
                                          WSAA-COMP-TAX-RELIEF          <086>
                                          WSAA-COMTOT-KEPT              <086>
                                          WSAA-COMPAY-KEPT              <086>
                                          WSAA-COMERN-KEPT              <086>
                                          WSAA-OVRTIMES.                <086>
      *
           MOVE 'PSTW'                     TO LIFA-FUNCTION.
           MOVE ATMD-BATCH-KEY             TO LIFA-BATCKEY.
           MOVE WSAA-TRANSACTION-TIME      TO LIFA-TRANSACTION-TIME.
           MOVE WSAA-TRANSACTION-DATE      TO LIFA-TRANSACTION-DATE.
           MOVE WSAA-TERMID                TO LIFA-TERMID.
           MOVE WSAA-USER                  TO LIFA-USER.
           MOVE CHDRLNB-CHDRNUM            TO LIFA-RDOCNUM,
           MOVE CHDRLNB-CHDRCOY            TO LIFA-RLDGCOY.
           MOVE CHDRLNB-CNTCURR            TO LIFA-ORIGCURR.
           MOVE CHDRLNB-TRANNO             TO LIFA-TRANNO.
           MOVE DESC-LONGDESC              TO LIFA-TRANDESC.
           MOVE 0                          TO LIFA-CRATE,
                                              LIFA-ACCTAMT,
                                              LIFA-RCAMT.
           MOVE SPACES                     TO LIFA-GENLCUR.
           MOVE CHDRLNB-CHDRCOY            TO LIFA-GENLCOY.
           MOVE SPACES                     TO LIFA-POSTYEAR,
                                              LIFA-POSTMONTH.
           MOVE CHDRLNB-OCCDATE            TO LIFA-EFFDATE.
           MOVE VRCM-MAX-DATE              TO LIFA-FRCDATE.
           MOVE CHDRLNB-CNTTYPE            TO LIFA-SUBSTITUTE-CODE(01).
           MOVE +0                         TO WSAA-JRNSEQ.
           MOVE COVRLNB-CHDRNUM            TO WSAA-RLDG-CHDRNUM.
           MOVE COVRLNB-LIFE               TO WSAA-RLDG-LIFE.
           MOVE COVRLNB-COVERAGE           TO WSAA-RLDG-COVERAGE.
           MOVE COVRLNB-RIDER              TO WSAA-RLDG-RIDER.
           MOVE COVRLNB-PLAN-SUFFIX        TO WSAA-PLAN.
           MOVE WSAA-PLANSUFF              TO WSAA-RLDG-PLAN-SUFFIX.
           MOVE WSAA-RLDGACCT              TO LIFA-RLDGACCT.
      *
           CALL 'PCDDLNBIO'            USING PCDDLNB-PARAMS.
      *
           IF PCDDLNB-STATUZ           NOT = O-K
                                       AND NOT = ENDP
              MOVE PCDDLNB-PARAMS      TO SYSR-PARAMS
              MOVE PCDDLNB-STATUZ      TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
           IF PCDDLNB-CHDRCOY          NOT = CHDRLNB-CHDRCOY
           OR PCDDLNB-CHDRNUM          NOT = CHDRLNB-CHDRNUM
           OR PCDDLNB-STATUZ           = ENDP
              GO TO 233D-EXIT.

       2331-READ-AGENT-FILE.
      *
      * Read agent file.
           MOVE PCDDLNB-CHDRCOY     TO AGLFLNB-AGNTCOY.
           MOVE PCDDLNB-AGNTNUM     TO AGLFLNB-AGNTNUM.
           MOVE READR               TO AGLFLNB-FUNCTION.
           CALL 'AGLFLNBIO'         USING AGLFLNB-PARAMS.
      *
           IF AGLFLNB-STATUZ        NOT = O-K
                                    AND NOT = MRNF
              MOVE AGLFLNB-PARAMS   TO SYSR-PARAMS
              MOVE AGLFLNB-STATUZ   TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR
               ELSE
               IF AGLFLNB-STATUZ        = MRNF
                  MOVE SPACES           TO AGLFLNB-DATA-AREA
                  MOVE 0                TO AGLFLNB-OVCPC.
      *
           MOVE AGLFLNB-AGENT-CLASS    TO WSAA-STORED-AGENT-CLASS.

       2332-INITIALISE-LINKAGE.
           MOVE PCDDLNB-CHDRCOY        TO CLNK-CHDRCOY.
           MOVE PCDDLNB-CHDRNUM        TO CLNK-CHDRNUM.
           MOVE COVRLNB-LIFE           TO CLNK-LIFE.
           MOVE COVRLNB-COVERAGE       TO CLNK-COVERAGE.
           MOVE CHDRLNB-OCCDATE        TO CLNK-EFFDATE.
           MOVE COVRLNB-CURRTO         TO CLNK-CURRTO.                  <D9604>
           MOVE COVRLNB-RIDER          TO CLNK-RIDER.
           MOVE COVRLNB-PLAN-SUFFIX    TO CLNK-PLAN-SUFFIX.
           MOVE PCDDLNB-AGNTNUM        TO CLNK-AGENT.
           MOVE COVRLNB-JLIFE          TO CLNK-JLIFE.
           MOVE COVRLNB-CRTABLE        TO CLNK-CRTABLE.
           MOVE WSAA-STORED-AGENT-CLASS TO CLNK-AGENT-CLASS.
           MOVE 0                      TO CLNK-ICOMMTOT,
                                          CLNK-ICOMMPD,
                                          CLNK-ICOMMERND,
                                          CLNK-PAYAMNT,
                                          CLNK-ERNDAMT,
                                          CLNK-INSTPREM,
                                          CLNK-PTDATE,                  <V5L007>
                                          CLNK-ANNPREM,                 <D9604>
                                          CLNK-TARGET-PREM.             <D9604>
      *                                                                 <084>
           MOVE 0                      TO WSAA-CLNK-ANNPREM.            <084>
      *                                                                 <084>

           MOVE WSAA-BILLFREQ(WSBB-SUB) TO CLNK-BILLFREQ.
           MOVE 1                       TO CLNK-SEQNO.                  <D9604>

      *
           MOVE PCDDLNB-AGNTNUM       TO WSAA-AGNTNUM(WSAA-AGENT-SUB).
           MOVE PCDDLNB-SPLIT-BCOMM
                                 TO WSAA-SPLIT-BCOMM(WSAA-AGENT-SUB).


       233D-REGULAR-PREMIUM.

      * Need to process both Regular and Single Premium commission
      * using different methods from T5687. They may, in some cases,
      * both appear on the same coverage!!!
      * We will deal with the Regular bit first.

           IF COVRLNB-INSTPREM              = 0
               GO TO 233D-SINGLE-PREMIUM.


           MOVE COVRLNB-PAYRSEQNO  TO WSBB-SUB.
           MOVE WSAA-BILLFREQ(WSBB-SUB) TO WSAA-BILLFREQ-9
*******    COMPUTE CLNK-ANNPREM ROUNDED                                 <084>
           IF T5687-ZRRCOMBAS      NOT = SPACES                         <SPLPRM>
               COMPUTE WSAA-CLNK-ANNPREM ROUNDED                        <SPLPRM>
                   = COVRLNB-ZBINSTPREM                                 <SPLPRM>
                   * WSAA-BILLFREQ-9                                    <SPLPRM>
                   * (PCDDLNB-SPLIT-BCOMM / 100)                        <SPLPRM>
                                                                        <V70L01>
              IF TH605-BONUS-IND   NOT = 'Y'                            <V70L01>
               COMPUTE WSAA-ZCTN-ANNPREM ROUNDED                        <V70L01>
                   = COVRLNB-ZBINSTPREM                                 <V70L01>
                   * WSAA-BILLFREQ-9                                    <V70L01>
              END-IF                                                    <V70L01>
                                                                        <V70L01>
           ELSE                                                         <SPLPRM>
                                                                        <V70L01>
           IF TH605-BONUS-IND          NOT = 'Y'                        <V70L01>
           COMPUTE WSAA-ZCTN-ANNPREM ROUNDED                            <V70L01>
                   = COVRLNB-INSTPREM                                   <V70L01>
                   * WSAA-BILLFREQ-9                                    <V70L01>
           END-IF                                                       <V70L01>
                                                                        <V70L01>
           COMPUTE WSAA-CLNK-ANNPREM ROUNDED                            <084>
                   = COVRLNB-INSTPREM *                                 <SPLPRM>
                     WSAA-BILLFREQ-9                                    <SPLPRM>
                   * (PCDDLNB-SPLIT-BCOMM / 100).
      *                                                                 <084>
      * The following Compute statement is used to round the contents   <084>
      * of WSAA-CLNK-ANNPREM and move them into CLNK-ANNPREM.           <084>
      *                                                                 <084>
           COMPUTE CLNK-ANNPREM ROUNDED                                 <084>
                   = WSAA-CLNK-ANNPREM * 1.                             <084>
      *                                                                 <084>
           IF T5687-ZRRCOMBAS      NOT = SPACES                         <SPLPRM>
           COMPUTE CLNK-INSTPREM ROUNDED                                <SPLPRM>
                                   = COVRLNB-ZBINSTPREM                 <SPLPRM>
                                   * WSAA-FREQ-FACTOR(WSBB-SUB)         <SPLPRM>
                                   * (PCDDLNB-SPLIT-BCOMM / 100)        <SPLPRM>
                                                                        <V70L01>
           IF TH605-BONUS-IND      NOT = 'Y'                            <V70L01>
           COMPUTE  WSAA-ZCTN-INSTPREM ROUNDED                          <V70L01>
                                   = COVRLNB-ZBINSTPREM                 <V70L01>
                                   * WSAA-FREQ-FACTOR(WSBB-SUB)         <V70L01>
           END-IF                                                       <V70L01>
                                                                        <V70L01>
           ELSE                                                         <SPLPRM>
                                                                        <V70L01>
           IF TH605-BONUS-IND      NOT = 'Y'                            <V70L01>
           COMPUTE  WSAA-ZCTN-INSTPREM ROUNDED                          <V70L01>
                                   = COVRLNB-INSTPREM                   <V70L01>
                                   * WSAA-FREQ-FACTOR(WSBB-SUB)         <V70L01>
           END-IF                                                       <V70L01>
                                                                        <V70L01>
           COMPUTE CLNK-INSTPREM ROUNDED
                                   = COVRLNB-INSTPREM *
                                     WSAA-FREQ-FACTOR(WSBB-SUB)
                                  * (PCDDLNB-SPLIT-BCOMM / 100).

      ***  IF T5687-BASIC-COMM-METH    = SPACES                         <V42004>
      ***     GO TO 233D-5B.                                            <V42004>

           IF WSAA-BASIC-COMM-METH     = SPACES                         <V42004>
              GO TO 233D-5B.                                            <V42004>

       233D-5A.
           MOVE SPACES                 TO ITEM-DATA-AREA.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE CHDRLNB-CHDRCOY        TO ITEM-ITEMCOY.
           MOVE T5647                  TO ITEM-ITEMTABL.
      ***  MOVE T5687-BASIC-COMM-METH  TO ITEM-ITEMITEM,                <V42004>
      ***                                 AGCM-BASIC-COMM-METH.         <V42004>
           MOVE WSAA-BASIC-COMM-METH   TO ITEM-ITEMITEM,                <V42004>
                                          AGCM-BASIC-COMM-METH.         <V42004>

           MOVE READR                  TO ITEM-FUNCTION.
           CALL 'ITEMIO' USING         ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = O-K
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              MOVE ITEM-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
           MOVE ITEM-GENAREA           TO T5647-T5647-REC.

      ***  MOVE T5687-BASIC-COMM-METH  TO CLNK-METHOD.                  <V42004>
           MOVE WSAA-BASIC-COMM-METH   TO CLNK-METHOD.                  <V42004>

           IF T5647-COMMSUBR           NOT = SPACES
              CALL T5647-COMMSUBR      USING CLNK-CLNKALL-REC
      ****    IF CLNK-STATUZ         NOT  = O-K                         <CAS1.0>
              IF CLNK-STATUZ            OF   CLNK-CLNKALL-REC           <CAS1.0>
                                       NOT = O-K                        <CAS1.0>
                 MOVE CLNK-CLNKALL-REC    TO SYSR-PARAMS
      ****       MOVE CLNK-STATUZ         TO SYSR-STATUZ                <CAS1.0>
                 MOVE CLNK-STATUZ         OF CLNK-CLNKALL-REC           <CAS1.0>
                                          TO SYSR-STATUZ                <CAS1.0>
                 PERFORM XXXX-FATAL-ERROR.


       233D-5B.
      ***  IF  T5687-BASCPY            = SPACES                         <V42004>
      ***  AND AGLFLNB-BCMTAB          = SPACES                         <V42004>
      ***      GO TO 233D-5C.                                           <V42004>
                                                                        <V42004>
           IF  WSAA-BASCPY             = SPACES                         <V42004>
           AND AGLFLNB-BCMTAB          = SPACES                         <V42004>
               GO TO 233D-5C.                                           <V42004>

      *                                                         <D9604>
      * Dont use AGLF if Flexible Premium contract *            <D9604>
      ***  IF T5687-BASCPY             = SPACES                 <V42004><D9604>
      ***     AND FLEXIBLE-PREMIUM-CONTRACT                     <V42004><D9604>
      ***     GO TO 233D-5C.                                    <V42004><D9604>

           IF WSAA-BASCPY              = SPACES                         <V42004>
              AND FLEXIBLE-PREMIUM-CONTRACT                             <V42004>
              GO TO 233D-5C.                                            <V42004>

      *                                                         <D9604>
      * Set up target and period for Flexible Prem              <D9604>
      * Contracts                                               <D9604>
      *                                                         <D9604>
           COMPUTE CLNK-TARGET-PREM  ROUNDED                            <D9604>
                 = COVRLNB-INSTPREM *                                   <D9604>
                   WSAA-BILLFREQ-9.                                     <D9604>
           MOVE CHDRLNB-OCCDATE TO CLNK-CURRTO.                         <D9604>
      **** IF FLEXIBLE-PREMIUM-CONTRACT                         <PHFX39><D9604>
      ****    MOVE 0                   TO CLNK-INSTPREM         <PHFX39><D9604>
      **** END-IF.                                              <PHFX39><D9604>
      *                                                         <D9604>
           MOVE SPACES                 TO ITEM-DATA-AREA.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE CHDRLNB-CHDRCOY        TO ITEM-ITEMCOY.
           MOVE T5644                  TO ITEM-ITEMTABL.
      ***  IF T5687-BASCPY             = SPACES                         <V42004>
           IF WSAA-BASCPY              = SPACES                         <V42004>
              MOVE AGLFLNB-BCMTAB      TO ITEM-ITEMITEM
                                          CLNK-METHOD
                                          AGCM-BASCPY
           ELSE
      ***     MOVE T5687-BASCPY     TO ITEM-ITEMITEM                    <V42004>
      ***                              AGCM-BASCPY                      <V42004>
      ***                              CLNK-METHOD                      <V42004>
              MOVE WSAA-BASCPY      TO ITEM-ITEMITEM                    <V42004>
                                       AGCM-BASCPY                      <V42004>
                                       CLNK-METHOD                      <V42004>
           END-IF.                                                      <V42004>

           MOVE READR                  TO ITEM-FUNCTION.
           CALL 'ITEMIO' USING         ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = O-K
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              MOVE ITEM-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
           MOVE ITEM-GENAREA           TO T5644-T5644-REC.

           IF T5644-COMPYSUBR           NOT = SPACES
              CALL T5644-COMPYSUBR      USING CLNK-CLNKALL-REC
      ****    IF CLNK-STATUZ          NOT = O-K                         <CAS1.0>
              IF CLNK-STATUZ            OF   CLNK-CLNKALL-REC           <CAS1.0>
                                       NOT = O-K                        <CAS1.0>
                 MOVE CLNK-CLNKALL-REC    TO SYSR-PARAMS
      ****       MOVE CLNK-STATUZ         TO SYSR-STATUZ                <CAS1.0>
                 MOVE CLNK-STATUZ         OF CLNK-CLNKALL-REC           <CAS1.0>
                                          TO SYSR-STATUZ                <CAS1.0>
                 PERFORM XXXX-FATAL-ERROR.

           MOVE CLNK-PAYAMNT           TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO CLNK-PAYAMNT.                 <V76F06>
                                                                        <V76F06>
           MOVE CLNK-ICOMMTOT          TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO CLNK-ICOMMTOT.                <V76F06>
                                                                        <V76F06>
           MOVE CLNK-ERNDAMT           TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO CLNK-ERNDAMT.                 <V76F06>
      * Accumulate the commission amounts.
       233D-5C.
           ADD CLNK-PAYAMNT      TO WSAA-COMM-DUE(WSAA-AGENT-SUB).
           MOVE CLNK-ANNPREM     TO WSAA-ANNPREM(WSAA-AGENT-SUB).       <V73L01>
           COMPUTE WSAA-COMM-PAID(WSAA-AGENT-SUB) ROUNDED =
                             WSAA-COMM-PAID(WSAA-AGENT-SUB) +
                              (CLNK-PAYAMNT - CLNK-ERNDAMT).
           ADD CLNK-ERNDAMT      TO WSAA-COMM-EARN(WSAA-AGENT-SUB).
                                                                        <V70L01>
                                                                        <V70L01>
           IF TH605-BONUS-IND          NOT = 'Y'                        <V70L01>
              GO TO 233D-5C-SKIP                                        <V70L01>
           END-IF.                                                      <V70L01>
                                                                        <V70L01>
      *******************                                               <V70L01>
      * Bonus Workbench *                                               <V70L01>
      *******************                                               <V70L01>
                                                                        <V70L01>
           IF CLNK-PAYAMNT             NOT = ZERO                       <V70L01>
              MOVE SPACES              TO ZCTN-PARAMS                   <V70L01>
              MOVE PCDDLNB-CHDRCOY     TO ZCTN-AGNTCOY                  <V70L01>
              MOVE PCDDLNB-AGNTNUM     TO ZCTN-AGNTNUM                  <V70L01>
              MOVE CLNK-PAYAMNT        TO ZCTN-COMM-AMT                 <V70L01>
              IF CLNK-INSTPREM         > CLNK-ANNPREM                   <V70L01>
                 MOVE WSAA-ZCTN-ANNPREM TO ZCTN-PREMIUM                 <V70L01>
              ELSE                                                      <V70L01>
                 MOVE WSAA-ZCTN-INSTPREM TO ZCTN-PREMIUM                <V70L01>
              END-IF                                                    <V70L01>
              MOVE PCDDLNB-SPLIT-BCOMM TO ZCTN-SPLIT-BCOMM              <V70L01>
              MOVE 'I'                 TO ZCTN-ZPRFLG                   <V70L01>
              MOVE CLNK-CHDRCOY        TO ZCTN-CHDRCOY                  <V70L01>
              MOVE CLNK-CHDRNUM        TO ZCTN-CHDRNUM                  <V70L01>
              MOVE CLNK-LIFE           TO ZCTN-LIFE                     <V70L01>
              MOVE CLNK-COVERAGE       TO ZCTN-COVERAGE                 <V70L01>
              MOVE CLNK-RIDER          TO ZCTN-RIDER                    <V70L01>
              MOVE COVRLNB-TRANNO      TO ZCTN-TRANNO                   <V70L01>
              MOVE WSKY-BATC-BATCTRCDE TO ZCTN-TRANS-CODE               <V70L01>
              MOVE CLNK-EFFDATE        TO ZCTN-EFFDATE                  <V70L01>
              MOVE DTC1-INT-DATE       TO ZCTN-TRANDATE                 <V70L01>
              MOVE ZCTNREC             TO ZCTN-FORMAT                   <V70L01>
              MOVE WRITR               TO ZCTN-FUNCTION                 <V70L01>
                                                                        <V70L01>
              CALL 'ZCTNIO'            USING ZCTN-PARAMS                <V70L01>
                                                                        <V70L01>
              IF ZCTN-STATUZ           NOT = O-K                        <V70L01>
                 MOVE ZCTN-STATUZ      TO SYSR-STATUZ                   <V70L01>
                 MOVE ZCTN-PARAMS      TO SYSR-PARAMS                   <V70L01>
                 PERFORM XXXX-FATAL-ERROR                               <V70L01>
              END-IF                                                    <V70L01>
           END-IF.                                                      <V70L01>
                                                                        <V70L01>
       233D-5C-SKIP.                                                    <V70L01>

      * Set values on AGCM.
           MOVE CLNK-ICOMMTOT          TO AGCM-INITCOM.
           MOVE CLNK-PAYAMNT           TO AGCM-COMPAY.
           MOVE CLNK-ERNDAMT           TO AGCM-COMERN.
      *
      * Accumulate component level initial commission values for
      * later posting.
      *
           IF COMP-LEVEL-ACC
               ADD CLNK-ERNDAMT            TO WSAA-COMP-ERN-ICOMM
               COMPUTE WSAA-COMP-ADV-ICOMM ROUNDED =
                               WSAA-COMP-ADV-ICOMM +
                               (CLNK-PAYAMNT - CLNK-ERNDAMT).


      * Servicing commission.
       233D-5D.
           MOVE 0                      TO AGCM-SCMEARN,
                                          AGCM-SCMDUE.
           MOVE SPACES                 TO ITEM-DATA-AREA.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE CHDRLNB-CHDRCOY        TO ITEM-ITEMCOY.
           MOVE T5644                  TO ITEM-ITEMTABL.
      ***  IF T5687-SRVCPY             = SPACES                         <V42004>
           IF WSAA-SRVCPY              = SPACES                         <V42004>
              MOVE AGLFLNB-SCMTAB      TO ITEM-ITEMITEM
                                          AGCM-SRVCPY
           ELSE
      ***     MOVE T5687-SRVCPY        TO ITEM-ITEMITEM                 <V42004>
              MOVE WSAA-SRVCPY         TO ITEM-ITEMITEM                 <V42004>
                                          AGCM-SRVCPY.

           IF ITEM-ITEMITEM            = SPACES
              GO TO 233D-5E.
                                                                        <D9604>
      *  No servicing commission for flexible premiums at issue <D9604>
                                                                        <D9604>
           IF FLEXIBLE-PREMIUM-CONTRACT                                 <D9604>
              GO TO 233D-5E                                             <D9604>
           END-IF.                                                      <D9604>
                                                                        <D9604>

           MOVE READR                  TO ITEM-FUNCTION.
           CALL 'ITEMIO' USING         ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = O-K  AND NOT = MRNF
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              MOVE ITEM-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
      *
           IF  ITEM-STATUZ =  MRNF GO TO 233D-5E.
      *
           MOVE ITEM-GENAREA           TO T5644-T5644-REC.

           PERFORM 2332-INITIALISE-LINKAGE.


           MOVE COVRLNB-PAYRSEQNO      TO WSBB-SUB.
           MOVE WSAA-BILLFREQ(WSBB-SUB) TO WSAA-BILLFREQ-9.
*******    COMPUTE CLNK-ANNPREM ROUNDED                                 <084>
           IF T5687-ZRRCOMBAS      NOT = SPACES                         <SPLPRM>
               COMPUTE WSAA-CLNK-ANNPREM ROUNDED                        <SPLPRM>
                   = COVRLNB-ZBINSTPREM                                 <SPLPRM>
                   * WSAA-BILLFREQ-9                                    <SPLPRM>
                   * (PCDDLNB-SPLIT-BCOMM / 100)                        <SPLPRM>
           ELSE                                                         <SPLPRM>
           COMPUTE WSAA-CLNK-ANNPREM ROUNDED                            <084>
                = COVRLNB-INSTPREM
                * WSAA-BILLFREQ-9
                * (PCDDLNB-SPLIT-BCOMM / 100).
      *                                                                 <084>
      * The following Compute statement is used to round the contents   <084>
      * of WSAA-CLNK-ANNPREM and move them into CLNK-ANNPREM.           <084>
      *                                                                 <084>
           COMPUTE CLNK-ANNPREM ROUNDED                                 <084>
                   = WSAA-CLNK-ANNPREM * 1.                             <084>
      *                                                                 <084>
           IF T5687-ZRRCOMBAS          NOT = SPACES                     <SPLPRM>
           COMPUTE CLNK-INSTPREM ROUNDED                                <SPLPRM>
                       = COVRLNB-ZBINSTPREM                             <SPLPRM>
                       * WSAA-FREQ-FACTOR(WSBB-SUB)                     <SPLPRM>
                       * (PCDDLNB-SPLIT-BCOMM / 100)                    <SPLPRM>
           ELSE                                                         <SPLPRM>
           COMPUTE CLNK-INSTPREM ROUNDED
                       = COVRLNB-INSTPREM
                       * WSAA-FREQ-FACTOR(WSBB-SUB)
                       * (PCDDLNB-SPLIT-BCOMM / 100).

           IF AGLFLNB-SCMTAB  NOT      = SPACES
              MOVE AGLFLNB-SCMTAB      TO CLNK-METHOD
           ELSE
      ***     MOVE T5687-SRVCPY        TO CLNK-METHOD.                  <V42004>
              MOVE WSAA-SRVCPY         TO CLNK-METHOD.                  <V42004>

           IF T5644-COMPYSUBR           NOT = SPACES
              CALL T5644-COMPYSUBR      USING CLNK-CLNKALL-REC
      ****    IF CLNK-STATUZ          NOT = O-K                         <CAS1.0>
              IF CLNK-STATUZ            OF   CLNK-CLNKALL-REC           <CAS1.0>
                                       NOT = O-K                        <CAS1.0>
                 MOVE CLNK-CLNKALL-REC    TO SYSR-PARAMS
      ****       MOVE CLNK-STATUZ         TO SYSR-STATUZ                <CAS1.0>
                 MOVE CLNK-STATUZ         OF CLNK-CLNKALL-REC           <CAS1.0>
                                          TO SYSR-STATUZ                <CAS1.0>
                 PERFORM XXXX-FATAL-ERROR.

           MOVE CLNK-PAYAMNT           TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO CLNK-PAYAMNT.                 <V76F06>
                                                                        <V76F06>
           MOVE CLNK-ICOMMTOT          TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO CLNK-ICOMMTOT.                <V76F06>
                                                                        <V76F06>
           MOVE CLNK-ERNDAMT           TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO CLNK-ERNDAMT.                 <V76F06>
           ADD CLNK-PAYAMNT      TO WSAA-SERV-DUE(WSAA-AGENT-SUB).
           ADD CLNK-ERNDAMT      TO WSAA-SERV-EARN(WSAA-AGENT-SUB).
      *
      * Accumulate component level servicing commission values for
      * later posting.
      *
           IF COMP-LEVEL-ACC
               ADD CLNK-ERNDAMT        TO WSAA-COMP-ERN-SCOMM.

      * Set up AGCM values.
           MOVE CLNK-PAYAMNT           TO AGCM-SCMDUE.
           MOVE CLNK-ERNDAMT           TO AGCM-SCMEARN.

      * Renewal commission.
       233D-5E.
           PERFORM 2332-INITIALISE-LINKAGE.

           MOVE COVRLNB-PAYRSEQNO      TO WSBB-SUB.
           MOVE WSAA-BILLFREQ(WSBB-SUB) TO WSAA-BILLFREQ-9.
*******    COMPUTE CLNK-ANNPREM ROUNDED                                 <084>
           IF T5687-ZRRCOMBAS      NOT = SPACES                         <SPLPRM>
               COMPUTE WSAA-CLNK-ANNPREM ROUNDED                        <SPLPRM>
                   = COVRLNB-ZBINSTPREM                                 <SPLPRM>
                   * WSAA-BILLFREQ-9                                    <SPLPRM>
                   * (PCDDLNB-SPLIT-BCOMM / 100)                        <SPLPRM>
                                                                        <V70L01>
               IF TH605-BONUS-IND      NOT = 'Y'                        <V70L01>
               COMPUTE WSAA-ZCTN-ANNPREM ROUNDED                        <V70L01>
                   = COVRLNB-ZBINSTPREM                                 <V70L01>
                   * WSAA-BILLFREQ-9                                    <V70L01>
               END-IF                                                   <V70L01>
                                                                        <V70L01>
           ELSE                                                         <SPLPRM>
           IF TH605-BONUS-IND      NOT = 'Y'                            <V70L01>
           COMPUTE WSAA-ZCTN-ANNPREM ROUNDED                            <V70L01>
                   = COVRLNB-INSTPREM                                   <V70L01>
                   * WSAA-BILLFREQ-9                                    <V70L01>
           END-IF                                                       <V70L01>
                                                                        <V70L01>
           COMPUTE WSAA-CLNK-ANNPREM ROUNDED                            <084>
                  = COVRLNB-INSTPREM
                  *  WSAA-BILLFREQ-9
                  * (PCDDLNB-SPLIT-BCOMM / 100).
      *                                                                 <084>
      * The following Compute statement is used to round the contents   <084>
      * of WSAA-CLNK-ANNPREM and move them into CLNK-ANNPREM.           <084>
      *                                                                 <084>
           COMPUTE CLNK-ANNPREM ROUNDED                                 <084>
                   = WSAA-CLNK-ANNPREM * 1.                             <084>
      *                                                                 <084>
           IF T5687-ZRRCOMBAS          NOT = SPACES                     <SPLPRM>
           COMPUTE CLNK-INSTPREM ROUNDED                                <SPLPRM>
                       = COVRLNB-ZBINSTPREM                             <SPLPRM>
                       *  WSAA-FREQ-FACTOR(WSBB-SUB)                    <SPLPRM>
                       * (PCDDLNB-SPLIT-BCOMM / 100)                    <SPLPRM>
                                                                        <V70L01>
           IF TH605-BONUS-IND          NOT = 'Y'                        <V70L01>
           COMPUTE WSAA-ZCTN-INSTPREM ROUNDED                           <V70L01>
                       = COVRLNB-ZBINSTPREM                             <V70L01>
                       * WSAA-FREQ-FACTOR(WSBB-SUB)                     <V70L01>
           END-IF                                                       <V70L01>
                                                                        <V70L01>
           ELSE                                                         <SPLPRM>
                                                                        <V70L01>
           IF TH605-BONUS-IND          NOT = 'Y'                        <V70L01>
           COMPUTE WSAA-ZCTN-INSTPREM ROUNDED                           <V70L01>
                       = COVRLNB-INSTPREM                               <V70L01>
                       * WSAA-FREQ-FACTOR(WSBB-SUB)                     <V70L01>
           END-IF                                                       <V70L01>
                                                                        <V70L01>
           COMPUTE CLNK-INSTPREM ROUNDED
                       = COVRLNB-INSTPREM *
                          WSAA-FREQ-FACTOR(WSBB-SUB)
                       * (PCDDLNB-SPLIT-BCOMM / 100).
           MOVE 0                      TO AGCM-RNLCDUE,
                                          AGCM-RNLCEARN.

      * Set up target and period for Flexible Premium           <D9604>
      * contracts                                               <D9604>
                                                                        <D9604>
           IF COVRLNB-INSTPREM NOT = ZERO                               <D9604>
              COMPUTE CLNK-TARGET-PREM  ROUNDED                         <D9604>
                   = COVRLNB-INSTPREM *                                 <D9604>
                     WSAA-BILLFREQ-9                                    <D9604>
           END-IF.                                                      <D9604>
           MOVE CHDRLNB-OCCDATE TO CLNK-CURRTO.                         <D9604>
                                                                        <D9604>
           MOVE SPACES                 TO ITEM-DATA-AREA.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE CHDRLNB-CHDRCOY        TO ITEM-ITEMCOY.
           MOVE T5644                  TO ITEM-ITEMTABL.

      *                                                         <D9604>
      * Dont use AGLF if Flexible Premium contract *            <D9604>
      *                                                         <D9604>
      ***  IF T5687-RNWCPY             = SPACES                 <V42004><D9604>
           IF WSAA-RNWCPY              = SPACES                         <V42004>
              AND FLEXIBLE-PREMIUM-CONTRACT                             <D9604>
              GO TO 233D-5F.                                            <D9604>
      *                                                         <D9604>
      * USE T5687 ENTRY IF PRESENT                              <D9604> <D9604>
      *    IF AGLFLNB-RCMTAB  NOT      = SPACES                 <D9604>
      ***  IF T5687-RNWCPY             = SPACES                 <V42004><D9604>
           IF WSAA-RNWCPY              = SPACES                         <V42004>
              MOVE AGLFLNB-RCMTAB      TO ITEM-ITEMITEM
                                          CLNK-METHOD
                                          AGCM-RNWCPY
           ELSE
      ***     MOVE T5687-RNWCPY        TO ITEM-ITEMITEM                 <V42004>
              MOVE WSAA-RNWCPY         TO ITEM-ITEMITEM                 <V42004>
                                          AGCM-RNWCPY
                                          CLNK-METHOD.


           IF ITEM-ITEMITEM            = SPACES
              GO TO 233D-5F.

           MOVE READR                  TO ITEM-FUNCTION.
           CALL 'ITEMIO' USING         ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = O-K  AND  NOT = MRNF
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              MOVE ITEM-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
      *
           IF  ITEM-STATUZ              = MRNF
               GO TO 233D-5F.
      *
           MOVE ITEM-GENAREA           TO T5644-T5644-REC.
      *
           IF T5644-COMPYSUBR           NOT = SPACES
              CALL T5644-COMPYSUBR      USING CLNK-CLNKALL-REC
      ****    IF CLNK-STATUZ          NOT = O-K                         <CAS1.0>
              IF CLNK-STATUZ            OF   CLNK-CLNKALL-REC           <CAS1.0>
                                       NOT = O-K                        <CAS1.0>
                 MOVE CLNK-CLNKALL-REC    TO SYSR-PARAMS
      ****       MOVE CLNK-STATUZ         TO SYSR-STATUZ                <CAS1.0>
                 MOVE CLNK-STATUZ         OF CLNK-CLNKALL-REC           <CAS1.0>
                                          TO SYSR-STATUZ                <CAS1.0>
                 PERFORM XXXX-FATAL-ERROR
              ELSE
                 NEXT SENTENCE
           ELSE
              GO TO 233D-5F.
           MOVE CLNK-PAYAMNT           TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO CLNK-PAYAMNT.                 <V76F06>
                                                                        <V76F06>
           MOVE CLNK-ERNDAMT           TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO CLNK-ERNDAMT.                 <V76F06>
                                                                        <V76F06>
           ADD CLNK-PAYAMNT      TO WSAA-RENL-DUE(WSAA-AGENT-SUB).
           ADD CLNK-ERNDAMT      TO WSAA-RENL-EARN(WSAA-AGENT-SUB).
                                                                        <V70L01>
                                                                        <V70L01>
           IF TH605-BONUS-IND          NOT = 'Y'                        <V70L01>
              GO TO 233D-5E-SKIP                                        <V70L01>
           END-IF.                                                      <V70L01>
                                                                        <V70L01>
      *******************                                               <V70L01>
      * Bonus Workbench *                                               <V70L01>
      *******************                                               <V70L01>
                                                                        <V70L01>
           IF CLNK-PAYAMNT             NOT = ZERO                       <V70L01>
              MOVE SPACES              TO ZCTN-PARAMS                   <V70L01>
              MOVE PCDDLNB-CHDRCOY     TO ZCTN-AGNTCOY                  <V70L01>
              MOVE PCDDLNB-AGNTNUM     TO ZCTN-AGNTNUM                  <V70L01>
              MOVE CLNK-PAYAMNT        TO ZCTN-COMM-AMT                 <V70L01>
              COMPUTE ZCTN-PREMIUM ROUNDED = WSAA-ZCTN-INSTPREM         <V70L01>
                                           - WSAA-ZCTN-ANNPREM          <V70L01>
              MOVE PCDDLNB-SPLIT-BCOMM TO ZCTN-SPLIT-BCOMM              <V70L01>
              MOVE 'R'                 TO ZCTN-ZPRFLG                   <V70L01>
              MOVE CLNK-CHDRCOY        TO ZCTN-CHDRCOY                  <V70L01>
              MOVE CLNK-CHDRNUM        TO ZCTN-CHDRNUM                  <V70L01>
              MOVE CLNK-LIFE           TO ZCTN-LIFE                     <V70L01>
              MOVE CLNK-COVERAGE       TO ZCTN-COVERAGE                 <V70L01>
              MOVE CLNK-RIDER          TO ZCTN-RIDER                    <V70L01>
              MOVE COVRLNB-TRANNO      TO ZCTN-TRANNO                   <V70L01>
              MOVE WSKY-BATC-BATCTRCDE TO ZCTN-TRANS-CODE               <V70L01>
              MOVE WSAA-REG-INTM-DATE  TO ZCTN-EFFDATE                  <V70L01>
              MOVE DTC1-INT-DATE       TO ZCTN-TRANDATE                 <V70L01>
              MOVE ZCTNREC             TO ZCTN-FORMAT                   <V70L01>
              MOVE WRITR               TO ZCTN-FUNCTION                 <V70L01>
                                                                        <V70L01>
              CALL 'ZCTNIO'            USING ZCTN-PARAMS                <V70L01>
                                                                        <V70L01>
              IF ZCTN-STATUZ           NOT = O-K                        <V70L01>
                 MOVE ZCTN-STATUZ      TO SYSR-STATUZ                   <V70L01>
                 MOVE ZCTN-PARAMS      TO SYSR-PARAMS                   <V70L01>
                 PERFORM XXXX-FATAL-ERROR                               <V70L01>
              END-IF                                                    <V70L01>
           END-IF.                                                      <V70L01>
                                                                        <V70L01>
       233D-5E-SKIP.                                                    <V70L01>
                                                                        <V70L01>
      * Set up AGCM values.
           MOVE CLNK-PAYAMNT           TO AGCM-RNLCDUE.
           MOVE CLNK-ERNDAMT           TO AGCM-RNLCEARN.
      *
      * Accumulate component level renewal commission values for
      * later posting.
      *
           IF COMP-LEVEL-ACC
               ADD CLNK-ERNDAMT        TO WSAA-COMP-ERN-RCOMM.

      * Write AGCM record.
       233D-5F.
*******    MOVE CLNK-ANNPREM           TO AGCM-ANNPREM.                 <084>
      *                                                                 <084>
      * The following 2 Compute statement are used to round the         <084>
      * contents of WSAA-CLNK-ANNPREM and move them into CLNK-ANNPREM   <084>
      * and AGCM-ANNPREM.                                               <084>
      *                                                                 <084>
           COMPUTE CLNK-ANNPREM ROUNDED                                 <084>
                   = WSAA-CLNK-ANNPREM * 1.                             <084>
      *                                                                 <084>
           COMPUTE AGCM-ANNPREM ROUNDED                                 <084>
                   = WSAA-CLNK-ANNPREM * 1.                             <084>
      *

      * If all commission values are zero do not write an AGCM.
           IF  AGCM-ANNPREM            = 0
           AND AGCM-INITCOM            = 0
           AND AGCM-COMPAY             = 0
           AND AGCM-COMERN             = 0
           AND AGCM-SCMDUE             = 0
           AND AGCM-SCMEARN            = 0
           AND AGCM-RNLCDUE            = 0
           AND AGCM-RNLCEARN           = 0
              GO TO 233D-SINGLE-PREMIUM.

           MOVE WSAA-TERMID            TO AGCM-TERMID.
           MOVE WSAA-TRANSACTION-DATE  TO AGCM-TRANSACTION-DATE.
           MOVE WSAA-TRANSACTION-TIME  TO AGCM-TRANSACTION-TIME.
           MOVE WSAA-USER              TO AGCM-USER.
           MOVE COVRLNB-CHDRCOY        TO AGCM-CHDRCOY.
           MOVE COVRLNB-CHDRNUM        TO AGCM-CHDRNUM.
           MOVE AGLFLNB-AGNTNUM        TO AGCM-AGNTNUM,
      *****                               WSAA-AGCM-AGNTNUM.            <069>
                                          WSAA-SAVE-AGNTNUM.            <V4L014>
           MOVE AGLFLNB-AGENT-CLASS    TO AGCM-AGENT-CLASS.
           MOVE COVRLNB-LIFE           TO AGCM-LIFE.
           MOVE COVRLNB-COVERAGE       TO AGCM-COVERAGE.
           MOVE COVRLNB-RIDER          TO AGCM-RIDER.
           MOVE COVRLNB-PLAN-SUFFIX    TO AGCM-PLAN-SUFFIX.
           MOVE CHDRLNB-TRANNO         TO AGCM-TRANNO.
           MOVE CHDRLNB-OCCDATE        TO AGCM-EFDATE.
           MOVE COVRLNB-CURRFROM       TO AGCM-CURRFROM.
           MOVE COVRLNB-CURRTO         TO AGCM-CURRTO.
           MOVE 'B'                    TO AGCM-OVRDCAT.
           MOVE SPACES                 TO AGCM-CEDAGENT.
           MOVE '1'                    TO AGCM-VALIDFLAG.

           MOVE CHDRLNB-PTDATE         TO AGCM-PTDATE.
           MOVE 1                      TO AGCM-SEQNO.
           MOVE AGCMREC                TO AGCM-FORMAT.
           MOVE WRITR                  TO AGCM-FUNCTION.
           CALL 'AGCMIO'               USING AGCM-PARAMS.
           IF AGCM-STATUZ              NOT = O-K
              MOVE AGCM-PARAMS         TO SYSR-PARAMS
              MOVE AGCM-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.

           MOVE 'R'                    TO WSAA-PREMIUM-FLAG.
           MOVE ZEROS                  TO WSAA-SUB.
           MOVE AGCM-COMPAY            TO WSAA-COMPAY-KEPT-2.
                                                                        <A05940>
           MOVE ZEROES                 TO WSAA-OVRTIMES.                <A05940>
           MOVE BEGN                   TO ZRAP-FUNCTION.                <CAS1.0>
           MOVE O-K                    TO ZRAP-STATUZ                   <CAS1.0>
                                                                        <A05940>
           PERFORM 233E-OVERRIDER-COMMISSIONS                           <CAS1.0>
                                       UNTIL AGLFLNB-REPORTAG = SPACES  <CAS1.0>
                                       OR    AGLFLNB-OVCPC = ZEROS.     <CAS1.0>

      **** PERFORM 233E-OVERRIDER-COMMISSIONS                           <CAS1.0>
      ****                             UNTIL ZRAP-STATUZ  = ENDP.       <CAS1.0>
                                                                        <CAS1.0>
           IF COVRLNB-SINGP            = 0
      ***  OR T5687-BASSCMTH           = SPACES                         <V42004>
           OR WSAA-BASSCMTH            = SPACES                         <V42004>
              GO 2332-READ-NEXT.
      *
      *    Get original agent details back again
      *
           PERFORM 2331-READ-AGENT-FILE.
      *
        233D-SINGLE-PREMIUM.
      *
      * Now for the single premium bit (if any)!

              IF COVRLNB-SINGP         = 0
                  GO TO 2332-READ-NEXT.

           PERFORM 2332-INITIALISE-LINKAGE.

           MOVE '00'                   TO CLNK-BILLFREQ.
*******    COMPUTE CLNK-ANNPREM ROUNDED                                 <084>
           IF T5687-ZRRCOMBAS          NOT = SPACES                     <SPLPRM>
           AND COVRLNB-INSTPREM        = 0                              <SPLPRM>
               COMPUTE WSAA-CLNK-ANNPREM ROUNDED                        <SPLPRM>
                                       = COVRLNB-ZBINSTPREM             <SPLPRM>
                                       * (PCDDLNB-SPLIT-BCOMM / 100)    <SPLPRM>
           ELSE                                                         <SPLPRM>
           COMPUTE WSAA-CLNK-ANNPREM ROUNDED                            <084>
                                = COVRLNB-SINGP
                                  * (PCDDLNB-SPLIT-BCOMM / 100).
*******    MOVE CLNK-ANNPREM           TO CLNK-INSTPREM.                <084>
      *                                                                 <084>
      * The following Compute statements are used to round the contents <084>
      * of WSAA-CLNK-ANNPREM and move them into CLNK-ANNPREM and        <084>
      * CLNK-INSTPREM.                                                  <084>
      *                                                                 <084>
           COMPUTE CLNK-ANNPREM ROUNDED                                 <084>
                   = WSAA-CLNK-ANNPREM * 1.                             <084>
      *                                                                 <084>
           COMPUTE CLNK-INSTPREM ROUNDED                                <084>
                   = WSAA-CLNK-ANNPREM * 1.                             <084>
      *                                                                 <084>


      ***  IF T5687-BASSCMTH           = SPACES                         <V42004>
           IF WSAA-BASSCMTH            = SPACES                         <V42004>
              GO TO 2332-READ-NEXT.

       233D-6A.
           MOVE SPACES                 TO ITEM-DATA-AREA.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE CHDRLNB-CHDRCOY        TO ITEM-ITEMCOY.
           MOVE T5647                  TO ITEM-ITEMTABL.
      ***  MOVE T5687-BASSCMTH         TO ITEM-ITEMITEM,                <V42004>
           MOVE WSAA-BASSCMTH          TO ITEM-ITEMITEM,                <V42004>
                                          AGCM-BASIC-COMM-METH.
           MOVE READR                  TO ITEM-FUNCTION.
           CALL 'ITEMIO' USING         ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = O-K
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              MOVE ITEM-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
           MOVE ITEM-GENAREA           TO T5647-T5647-REC.

      ***  MOVE T5687-BASSCMTH         TO CLNK-METHOD.                  <V42004>
           MOVE WSAA-BASSCMTH          TO CLNK-METHOD.                  <V42004>

           IF T5647-COMMSUBR           NOT = SPACES
              CALL T5647-COMMSUBR      USING CLNK-CLNKALL-REC
      ****    IF CLNK-STATUZ          NOT = O-K                         <CAS1.0>
              IF CLNK-STATUZ            OF   CLNK-CLNKALL-REC           <CAS1.0>
                                       NOT = O-K                        <CAS1.0>
                 MOVE CLNK-CLNKALL-REC    TO SYSR-PARAMS
      ****       MOVE CLNK-STATUZ         TO SYSR-STATUZ                <CAS1.0>
                 MOVE CLNK-STATUZ         OF CLNK-CLNKALL-REC           <CAS1.0>
                                          TO SYSR-STATUZ                <CAS1.0>
                 PERFORM XXXX-FATAL-ERROR.

           MOVE CLNK-ERNDAMT           TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO CLNK-ERNDAMT.                 <V76F06>
                                                                        <V76F06>
           MOVE CLNK-PAYAMNT           TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO CLNK-PAYAMNT.                 <V76F06>
                                                                        <V76F06>
       233D-6B.
      ***  IF  T5687-BASSCPY           = SPACES                         <V42004>
           IF  WSAA-BASSCPY            = SPACES                         <V42004>
           AND AGLFLNB-BCMTAB          = SPACES
           OR  COVRLNB-SINGP           = 0
               GO TO 233D-6C.

      *                                                                 <D9604>
      * Dont use AGLF if Flexible Premium contract *                    <D9604>
      ***  IF T5687-BASSCPY            = SPACES                 <V42004><D9604>
           IF WSAA-BASSCPY             = SPACES                         <V42004>
              AND FLEXIBLE-PREMIUM-CONTRACT                             <D9604>
              MOVE SPACES              TO AGCM-BASCPY                   <D9604>
              GO TO 233D-6C.                                            <D9604>
                                                                        <D9604>
           MOVE SPACES                 TO ITEM-DATA-AREA.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE CHDRLNB-CHDRCOY        TO ITEM-ITEMCOY.
           MOVE T5644                  TO ITEM-ITEMTABL.
      ***  IF T5687-BASSCPY            = SPACES                         <V42004>
           IF WSAA-BASSCPY             = SPACES                         <V42004>
              MOVE AGLFLNB-BCMTAB      TO ITEM-ITEMITEM
                                          CLNK-METHOD
                                          AGCM-BASCPY
           ELSE
      ***     MOVE T5687-BASSCPY       TO ITEM-ITEMITEM                 <V42004>
              MOVE WSAA-BASSCPY        TO ITEM-ITEMITEM                 <V42004>
                                          AGCM-BASCPY
                                          CLNK-METHOD.
           MOVE READR                  TO ITEM-FUNCTION.
           CALL 'ITEMIO' USING         ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = O-K
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              MOVE ITEM-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
           MOVE ITEM-GENAREA           TO T5644-T5644-REC.

           IF T5644-COMPYSUBR           NOT = SPACES
              CALL T5644-COMPYSUBR      USING CLNK-CLNKALL-REC
      ****    IF CLNK-STATUZ          NOT = O-K                         <CAS1.0>
              IF CLNK-STATUZ            OF   CLNK-CLNKALL-REC           <CAS1.0>
                                       NOT = O-K                        <CAS1.0>
                 MOVE CLNK-CLNKALL-REC    TO SYSR-PARAMS
      ****       MOVE CLNK-STATUZ         TO SYSR-STATUZ                <CAS1.0>
                 MOVE CLNK-STATUZ         OF CLNK-CLNKALL-REC           <CAS1.0>
                                          TO SYSR-STATUZ                <CAS1.0>
                 PERFORM XXXX-FATAL-ERROR.

           MOVE CLNK-ERNDAMT           TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO CLNK-ERNDAMT.                 <V76F06>
                                                                        <V76F06>
           MOVE CLNK-PAYAMNT           TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO CLNK-PAYAMNT.                 <V76F06>
      * Accumulate the commission amounts.
       233D-6C.
      **** to split the accumulation of single premium commsion.
      *    ADD CLNK-PAYAMNT      TO WSAA-COMM-DUE(WSAA-AGENT-SUB).      <V6L000>
           ADD CLNK-PAYAMNT      TO WSAA-TCOM-DUE(WSAA-AGENT-SUB).      <V6L000>
           MOVE CLNK-ANNPREM     TO WSAA-SINGP(WSAA-AGENT-SUB).         <V73L01>
           COMPUTE WSAA-COMM-PAID(WSAA-AGENT-SUB) ROUNDED =
                             WSAA-COMM-PAID(WSAA-AGENT-SUB) +
                              (CLNK-PAYAMNT - CLNK-ERNDAMT).
           ADD CLNK-ERNDAMT      TO WSAA-COMM-EARN(WSAA-AGENT-SUB).
                                                                        <V70L01>
           IF TH605-BONUS-IND          NOT = 'Y'                        <V70L01>
              GO TO 233D-6C-SKIP                                        <V70L01>
           END-IF.                                                      <V70L01>
                                                                        <V70L01>
      *******************                                               <V70L01>
      * Bonus Workbench *                                               <V70L01>
      *******************                                               <V70L01>
                                                                        <V70L01>
           IF CLNK-PAYAMNT             NOT = ZERO                       <V70L01>
              MOVE SPACES              TO ZCTN-PARAMS                   <V70L01>
              MOVE PCDDLNB-CHDRCOY     TO ZCTN-AGNTCOY                  <V70L01>
              MOVE PCDDLNB-AGNTNUM     TO ZCTN-AGNTNUM                  <V70L01>
              MOVE CLNK-PAYAMNT        TO ZCTN-COMM-AMT                 <V70L01>
              MOVE COVRLNB-SINGP       TO ZCTN-PREMIUM                  <V70L01>
              MOVE PCDDLNB-SPLIT-BCOMM TO ZCTN-SPLIT-BCOMM              <V70L01>
              MOVE 'S'                 TO ZCTN-ZPRFLG                   <V70L01>
              MOVE CLNK-CHDRCOY        TO ZCTN-CHDRCOY                  <V70L01>
              MOVE CLNK-CHDRNUM        TO ZCTN-CHDRNUM                  <V70L01>
              MOVE CLNK-LIFE           TO ZCTN-LIFE                     <V70L01>
              MOVE CLNK-COVERAGE       TO ZCTN-COVERAGE                 <V70L01>
              MOVE CLNK-RIDER          TO ZCTN-RIDER                    <V70L01>
              MOVE COVRLNB-TRANNO      TO ZCTN-TRANNO                   <V70L01>
              MOVE WSKY-BATC-BATCTRCDE TO ZCTN-TRANS-CODE               <V70L01>
              MOVE WSAA-REG-INTM-DATE  TO ZCTN-EFFDATE                  <V70L01>
              MOVE DTC1-INT-DATE       TO ZCTN-TRANDATE                 <V70L01>
              MOVE ZCTNREC             TO ZCTN-FORMAT                   <V70L01>
              MOVE WRITR               TO ZCTN-FUNCTION                 <V70L01>
                                                                        <V70L01>
              CALL 'ZCTNIO'            USING ZCTN-PARAMS                <V70L01>
                                                                        <V70L01>
              IF ZCTN-STATUZ           NOT = O-K                        <V70L01>
                 MOVE ZCTN-STATUZ      TO SYSR-STATUZ                   <V70L01>
                 MOVE ZCTN-PARAMS      TO SYSR-PARAMS                   <V70L01>
                 PERFORM XXXX-FATAL-ERROR                               <V70L01>
              END-IF                                                    <V70L01>
           END-IF.                                                      <V70L01>
                                                                        <V70L01>
       233D-6C-SKIP.                                                    <V70L01>
                                                                        <V70L01>
      *
      * Accumulate component level initial commission values for
      * later posting.
      *
           IF COMP-LEVEL-ACC
               ADD CLNK-ERNDAMT        TO WSAA-COMP-ERN-ICOMM
               COMPUTE WSAA-COMP-ADV-ICOMM ROUNDED =
                               WSAA-COMP-ADV-ICOMM +
                               (CLNK-PAYAMNT - CLNK-ERNDAMT).

      * Set values on AGCM.
           MOVE CLNK-ICOMMTOT          TO AGCM-INITCOM.
           MOVE CLNK-PAYAMNT           TO AGCM-COMPAY.
           MOVE CLNK-ERNDAMT           TO AGCM-COMERN.
           MOVE 0                      TO AGCM-SCMEARN,
                                          AGCM-SCMDUE.
           MOVE SPACES                 TO AGCM-SRVCPY.
           MOVE 0                      TO AGCM-RNLCDUE,
                                          AGCM-RNLCEARN.
           MOVE SPACES                 TO AGCM-RNWCPY.

      * Write AGCM record.

       233D-6D.
*******    MOVE CLNK-ANNPREM           TO AGCM-ANNPREM.                 <084>
      *                                                                 <084>
      * The following Compute statements are used to round the contents <084>
      * of WSAA-CLNK-ANNPREM and move them into CLNK-ANNPREM and        <084>
      * AGCM-ANNPREM.                                                   <084>
      *                                                                 <084>
           COMPUTE CLNK-ANNPREM ROUNDED                                 <084>
                   = WSAA-CLNK-ANNPREM * 1.                             <084>
      *                                                                 <084>
           COMPUTE AGCM-ANNPREM ROUNDED                                 <084>
                   = WSAA-CLNK-ANNPREM * 1.                             <084>
      *                                                                 <084>

      *
      * If all commission values are zero do not write an AGCM.
           IF  AGCM-ANNPREM            = 0
           AND AGCM-INITCOM            = 0
           AND AGCM-COMPAY             = 0
           AND AGCM-COMERN             = 0
           AND AGCM-SCMDUE             = 0
           AND AGCM-SCMEARN            = 0
           AND AGCM-RNLCDUE            = 0
           AND AGCM-RNLCEARN           = 0
              GO TO 2332-READ-NEXT.

           MOVE WSAA-TERMID            TO AGCM-TERMID.
           MOVE WSAA-TRANSACTION-DATE  TO AGCM-TRANSACTION-DATE.
           MOVE WSAA-TRANSACTION-TIME  TO AGCM-TRANSACTION-TIME.
           MOVE WSAA-USER              TO AGCM-USER.
           MOVE COVRLNB-CHDRCOY        TO AGCM-CHDRCOY.
           MOVE COVRLNB-CHDRNUM        TO AGCM-CHDRNUM.
           MOVE AGLFLNB-AGNTNUM        TO AGCM-AGNTNUM,
      *****                               WSAA-AGCM-AGNTNUM.            <069>
                                          WSAA-SAVE-AGNTNUM.            <V4L014>
           MOVE WSAA-STORED-AGENT-CLASS TO AGCM-AGENT-CLASS.
           MOVE COVRLNB-LIFE           TO AGCM-LIFE.
           MOVE COVRLNB-COVERAGE       TO AGCM-COVERAGE.
           MOVE COVRLNB-RIDER          TO AGCM-RIDER.
           MOVE COVRLNB-PLAN-SUFFIX    TO AGCM-PLAN-SUFFIX.
           MOVE CHDRLNB-TRANNO         TO AGCM-TRANNO.
           MOVE CHDRLNB-OCCDATE        TO AGCM-EFDATE.
           MOVE COVRLNB-CURRFROM       TO AGCM-CURRFROM.
           MOVE COVRLNB-CURRTO         TO AGCM-CURRTO.
           MOVE SPACES                 TO AGCM-CEDAGENT.
           MOVE '1'                    TO AGCM-VALIDFLAG.
           MOVE 'B'                    TO AGCM-OVRDCAT.
           MOVE SPACES                 TO AGCM-CEDAGENT.
      **** MOVE CHDRLNB-PTDATE         TO AGCM-PTDATE.                  <A05940>
           MOVE ZEROES                 TO AGCM-PTDATE.                  <A05940>
           MOVE 1                      TO AGCM-SEQNO.
           MOVE AGCMREC                TO AGCM-FORMAT.
           MOVE WRITR                  TO AGCM-FUNCTION.
           CALL 'AGCMIO'               USING AGCM-PARAMS.
           IF AGCM-STATUZ              NOT = O-K
              MOVE AGCM-PARAMS         TO SYSR-PARAMS
              MOVE AGCM-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
      *
           MOVE 'S'                    TO WSAA-PREMIUM-FLAG.
           MOVE AGCM-COMPAY            TO WSAA-COMPAY-KEPT-2.
           MOVE ZEROS                  TO WSAA-SUB.
           MOVE BEGN                   TO ZRAP-FUNCTION.                <CAS1.0>
                                                                        <A05940>
           MOVE ZEROES                 TO WSAA-OVRTIMES.                <A05940>
                                                                        <A05940>
           PERFORM 233E-OVERRIDER-COMMISSIONS                           <CAS1.0>
                                       UNTIL AGLFLNB-REPORTAG = SPACES  <CAS1.0>
                                       OR    AGLFLNB-OVCPC = ZEROS.     <CAS1.0>
                                                                        <CAS1.0>
      **** PERFORM 233E-OVERRIDER-COMMISSIONS                           <CAS1.0>
      ****                             UNTIL ZRAP-STATUZ  = ENDP.       <CAS1.0>
                                                                        <CAS1.0>
       2332-READ-NEXT.
      *
              PERFORM 233F-COMPONENT-POSTINGS.
      *
           MOVE NEXTR                  TO PCDDLNB-FUNCTION.
           ADD 1                       TO WSAA-AGENT-SUB.
           IF WSAA-AGENT-SUB           > 10
              MOVE I035                TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
      *
       233D-EXIT.
           EXIT.
      *
      /
      *
       233E-OVERRIDER-COMMISSIONS SECTION.
      ************************************
       2331-OVERRIDER-COMMISSIONS.
      *
           MOVE AGLFLNB-OVCPC          TO WSAA-STORED-OVCPC.
      *
      * Call Basic and initial commission subroutines.

           MOVE PCDDLNB-CHDRCOY        TO CLNK-CHDRCOY.
           MOVE PCDDLNB-CHDRNUM        TO CLNK-CHDRNUM.
           MOVE COVRLNB-LIFE           TO CLNK-LIFE.
           MOVE COVRLNB-COVERAGE       TO CLNK-COVERAGE.
           MOVE CHDRLNB-OCCDATE        TO CLNK-EFFDATE.
           MOVE COVRLNB-RIDER          TO CLNK-RIDER.
           MOVE COVRLNB-PLAN-SUFFIX    TO CLNK-PLAN-SUFFIX.
           MOVE AGLFLNB-AGNTNUM        TO CLNK-AGENT.
      **** MOVE 'LINJO'                TO CLNK-FUNCTION.                <085>
           MOVE COVRLNB-JLIFE          TO CLNK-JLIFE.
           MOVE COVRLNB-CRTABLE        TO CLNK-CRTABLE.
           MOVE WSAA-STORED-AGENT-CLASS TO CLNK-AGENT-CLASS.
           MOVE 0                      TO CLNK-ICOMMTOT,
                                          CLNK-ICOMMPD,
                                          CLNK-ICOMMERND,
                                          CLNK-PAYAMNT,
                                          CLNK-ERNDAMT,
                                          CLNK-INSTPREM,
                                          CLNK-ANNPREM                  <D9604>
                                          CLNK-TARGET-PREM.             <D9604>
                                                                        <CAS1.0>
           MOVE WSAA-COMPAY-KEPT-2     TO CLNK-PAYAMNT.                 <CAS1.0>
           MOVE AGLFLNB-ZRORCODE       TO CLNK-ZORCODE.                 <CAS1.0>
           MOVE AGLFLNB-EFFDATE        TO CLNK-EFDATE.                  <CAS1.0>
                                                                        <CAS1.0>
      *                                                                 <084>
           MOVE 0                      TO WSAA-CLNK-ANNPREM.            <084>
           MOVE 1                      TO CLNK-SEQNO.                   <D9604>
      *                                                                 <084>


           MOVE COVRLNB-PAYRSEQNO      TO WSBB-SUB.
           MOVE WSAA-BILLFREQ(WSBB-SUB) TO WSAA-BILLFREQ-9.
           IF  WSAA-PREMIUM-FLAG       = 'S'
*******        COMPUTE CLNK-ANNPREM ROUNDED                             <084>
             IF T5687-ZRRCOMBAS        NOT = SPACES                     <SPLPRM>
             AND COVRLNB-INSTPREM      = 0                              <SPLPRM>
               COMPUTE WSAA-CLNK-ANNPREM ROUNDED                        <SPLPRM>
                                       = COVRLNB-ZBINSTPREM             <SPLPRM>
                                       * (PCDDLNB-SPLIT-BCOMM / 100)    <SPLPRM>
             ELSE                                                       <SPLPRM>
               COMPUTE WSAA-CLNK-ANNPREM ROUNDED                        <084>
                                = COVRLNB-SINGP
                                  * (PCDDLNB-SPLIT-BCOMM / 100)
             END-IF                                                     <SPLPRM>
*******        MOVE CLNK-ANNPREM TO CLNK-INSTPREM                       <084>
      *                                                   2     <V70L01><084>
      * The following Compute statements are used to round the contents <084>
      * of WSAA-CLNK-ANNPREM and move them into CLNK-ANNPREM and        <084>
      * CLNK-INSTPREM.                                                  <084>
      *                                                                 <084>
           COMPUTE CLNK-ANNPREM ROUNDED                                 <084>
                   = WSAA-CLNK-ANNPREM * 1                              <084>
      *                                                                 <084>
           COMPUTE CLNK-INSTPREM ROUNDED                                <084>
                   = WSAA-CLNK-ANNPREM * 1                              <084>
      *                                                                 <084>
               MOVE '00'         TO CLNK-BILLFREQ
           ELSE
           IF  WSAA-PREMIUM-FLAG       = 'R'
*******        COMPUTE CLNK-ANNPREM ROUNDED                             <084>
             IF T5687-ZRRCOMBAS NOT = SPACES                            <SPLPRM>
               COMPUTE WSAA-CLNK-ANNPREM ROUNDED                        <SPLPRM>
                  = COVRLNB-ZBINSTPREM                                  <SPLPRM>
                  * WSAA-BILLFREQ-9                                     <SPLPRM>
                  * (PCDDLNB-SPLIT-BCOMM / 100)                         <SPLPRM>
             ELSE                                                       <SPLPRM>
               COMPUTE WSAA-CLNK-ANNPREM ROUNDED                        <084>
                  = COVRLNB-INSTPREM
                  * WSAA-BILLFREQ-9
                  * (PCDDLNB-SPLIT-BCOMM / 100)
             END-IF                                                     <SPLPRM>
      *                                                                 <084>
      * The following Compute statement is used to round the contents   <084>
      * of WSAA-CLNK-ANNPREM and move them into CLNK-ANNPREM.           <084>
      *                                                                 <084>
           COMPUTE CLNK-ANNPREM ROUNDED                                 <084>
                   = WSAA-CLNK-ANNPREM * 1.                             <084>
      *                                                                 <084>
           IF T5687-ZRRCOMBAS     NOT = SPACES                          <SPLPRM>
               COMPUTE CLNK-INSTPREM ROUNDED                            <SPLPRM>
                                   = COVRLNB-ZBINSTPREM                 <SPLPRM>
                                   * WSAA-FREQ-FACTOR(WSBB-SUB)         <SPLPRM>
                                   * (PCDDLNB-SPLIT-BCOMM / 100)        <SPLPRM>
           ELSE                                                         <SPLPRM>
               COMPUTE CLNK-INSTPREM ROUNDED
                                   = COVRLNB-INSTPREM *
                                     WSAA-FREQ-FACTOR(WSBB-SUB)
                                  * (PCDDLNB-SPLIT-BCOMM / 100).

      * Set up target and Period for Flexible Premium contracts.<D9604>
           IF WSAA-PREMIUM-FLAG = 'R'                                   <D9604>
              COMPUTE CLNK-TARGET-PREM  ROUNDED                         <D9604>
                   = COVRLNB-INSTPREM *                                 <D9604>
                     WSAA-BILLFREQ-9                                    <D9604>
           END-IF.                                                      <D9604>
           MOVE CHDRLNB-OCCDATE TO CLNK-CURRTO.                         <D9604>
                                                                        <D9604>
           IF WSAA-PREMIUM-FLAG        = 'S'
      ***     IF T5687-BASSCMTH        = SPACES                         <V42004>
              IF WSAA-BASSCMTH         = SPACES                         <V42004>
                 GO TO 233E-AGLF
              END-IF
           ELSE
      ***     IF T5687-BASIC-COMM-METH = SPACES                         <V42004>
      ***        GO TO 233E-AGLF                                        <V42004>
      ***     END-IF                                                    <V42004>

              IF WSAA-BASIC-COMM-METH  = SPACES                         <V42004>
                 GO TO 233E-AGLF                                        <V42004>
              END-IF                                                    <V42004>
           END-IF.


           MOVE SPACES                 TO ITEM-DATA-AREA.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE CHDRLNB-CHDRCOY        TO ITEM-ITEMCOY.
           MOVE T5647                  TO ITEM-ITEMTABL.

           IF WSAA-PREMIUM-FLAG        = 'S'
      ***     MOVE T5687-BASSCMTH      TO ITEM-ITEMITEM,                <V42004>
              MOVE WSAA-BASSCMTH       TO ITEM-ITEMITEM,                <V42004>
                                          AGCM-BASIC-COMM-METH
           ELSE
      ***     MOVE T5687-BASIC-COMM-METH                                <V42004>
      ***                              TO ITEM-ITEMITEM,                <V42004>
      ***                                 AGCM-BASIC-COMM-METH.         <V42004>

              MOVE WSAA-BASIC-COMM-METH                                 <V42004>
                                       TO ITEM-ITEMITEM,                <V42004>
                                          AGCM-BASIC-COMM-METH.         <V42004>

           MOVE READR                  TO ITEM-FUNCTION.
           CALL 'ITEMIO' USING         ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = O-K
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              MOVE ITEM-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
           MOVE ITEM-GENAREA           TO T5647-T5647-REC.

           IF WSAA-PREMIUM-FLAG        = 'S'
      ***     MOVE T5687-BASSCMTH      TO CLNK-METHOD                   <V42004>
              MOVE WSAA-BASSCMTH       TO CLNK-METHOD                   <V42004>
           ELSE
      ***     MOVE T5687-BASIC-COMM-METH                                <V42004>
      ***                              TO CLNK-METHOD.                  <V42004>
                                                                        <V42004>
              MOVE WSAA-BASIC-COMM-METH                                 <V42004>
                                       TO CLNK-METHOD.                  <V42004>

           IF T5647-COMMSUBR           NOT = SPACES
              CALL T5647-COMMSUBR      USING CLNK-CLNKALL-REC
      ****    IF CLNK-STATUZ           NOT = O-K                        <CAS1.0>
              IF CLNK-STATUZ            OF   CLNK-CLNKALL-REC           <CAS1.0>
                                       NOT = O-K                        <CAS1.0>
                 MOVE CLNK-CLNKALL-REC    TO SYSR-PARAMS
      ****       MOVE CLNK-STATUZ         TO SYSR-STATUZ                <CAS1.0>
                 MOVE CLNK-STATUZ         OF CLNK-CLNKALL-REC           <CAS1.0>
                                          TO SYSR-STATUZ                <CAS1.0>
                 PERFORM XXXX-FATAL-ERROR.
      *                                                                 <CAS1.0>
       233E-ZRAP.                                                       <CAS1.0>
      *                                                                 <CAS1.0>
      *    MOVE PCDDLNB-CHDRCOY        TO ZRAP-AGNTCOY.                 <CAS1.0>
      *    MOVE PCDDLNB-AGNTNUM        TO ZRAP-AGNTNUM.                 <CAS1.0>
      *                                                                 <CAS1.0>
      *    CALL 'ZRAPIO'               USING ZRAP-PARAMS.               <CAS1.0>
      *                                                                 <CAS1.0>
      *    IF ZRAP-STATUZ           NOT = O-K                           <CAS1.0>
      *                         AND NOT = ENDP                          <CAS1.0>
      *       MOVE ZRAP-PARAMS         TO SYSR-PARAMS                   <CAS1.0>
      *       MOVE ZRAP-STATUZ         TO SYSR-STATUZ                   <CAS1.0>
      *       PERFORM XXXX-FATAL-ERROR                                  <CAS1.0>
      *    END-IF.                                                      <CAS1.0>
      *                                                                 <CAS1.0>
      *    IF ZRAP-AGNTNUM          NOT = PCDDLNB-AGNTNUM               <CAS1.0>
      *    OR ZRAP-STATUZ               = ENDP                          <CAS1.0>
      *       MOVE SPACES              TO AGLFLNB-REPORTAG              <CAS1.0>
      *       MOVE ENDP                TO ZRAP-STATUZ                   <CAS1.0>
      *       GO 233E-EXIT                                              <CAS1.0>
      *    END-IF.                                                      <CAS1.0>
      *                                                                 <CAS1.0>
      *    MOVE NEXTR                  TO ZRAP-FUNCTION.                <CAS1.0>
      *                                                                 <CAS1.0>
      *    IF ZRAP-EFFDATE              > CHDRLNB-OCCDATE               <CAS1.0>
      *       GO TO 233E-EXIT                                           <CAS1.0>
      *    END-IF.                                                      <CAS1.0>
      *                                                                 <CAS1.0>
      **** MOVE ZRAP-PRCNT             TO WSAA-STORED-OVCPC.            <CAS1.0>
                                                                        <CAS1.0>
      *
       233E-AGLF.
      *****MOVE AGCM-AGNTNUM           TO AGCM-CEDAGENT.                <069>
      **** MOVE AGLFLNB-REPORTAG       TO AGCM-AGNTNUM.                 <CAS1.0>
           MOVE ZRAP-REPORTAG          TO AGCM-AGNTNUM.                 <CAS1.0>
           MOVE PCDDLNB-CHDRCOY        TO AGLFLNB-AGNTCOY.
           MOVE AGLFLNB-REPORTAG       TO AGLFLNB-AGNTNUM.              <CAS1.0>
      **** MOVE ZRAP-REPORTAG          TO AGLFLNB-AGNTNUM.              <CAS1.0>
           MOVE READR                  TO AGLFLNB-FUNCTION.
           CALL 'AGLFLNBIO'            USING AGLFLNB-PARAMS.
           IF AGLFLNB-STATUZ           NOT = O-K
                                   AND NOT = MRNF
              MOVE AGLFLNB-PARAMS      TO SYSR-PARAMS
              MOVE AGLFLNB-STATUZ      TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.

           IF AGLFLNB-STATUZ              = MRNF
              MOVE SPACES              TO AGLFLNB-REPORTAG
              MOVE 0                   TO AGLFLNB-OVCPC
              GO 233E-EXIT.
      *
       233E-5B.

      *                                                         <D9604>
      * Dont use AGLF if Flexible Premium contract *            <D9604>
      *                                                         <D9604>
           IF WSAA-PREMIUM-FLAG = 'R'                                   <D9604>
      ***     AND  T5687-BASCPY         = SPACES                <V42004><CAS1.0>
              AND  WSAA-BASCPY          = SPACES                        <V42004>
      ****    AND  T5687-ZRORPMSP       = SPACES                        <CAS1.0>
              AND AGLFLNB-BCMTAB        = SPACES                        <CAS1.0>
              AND FLEXIBLE-PREMIUM-CONTRACT                             <D9604>
              GO TO 233E-WRITE-AGCM.                                    <D9604>
                                                                        <D9604>
           IF WSAA-PREMIUM-FLAG        = 'S'
      ***     IF T5687-BASSCPY         = SPACES                 <V42004><CAS1.0>
              IF WSAA-BASSCPY          = SPACES                         <V42004>
      ****    AND T5687-ZRORPMSP        = SPACES                        <CAS1.0>
              AND AGLFLNB-BCMTAB        = SPACES                        <CAS1.0>
                   GO TO 233E-WRITE-AGCM.

           IF WSAA-PREMIUM-FLAG        = 'R'
      ***     IF T5687-BASCPY          = SPACES                 <V42004><CAS1.0>
              IF WSAA-BASCPY           = SPACES                         <V42004>
      **** AND T5687-ZRORPMRG           = SPACES                        <CAS1.0>
           AND AGLFLNB-BCMTAB           = SPACES                        <CAS1.0>
                   GO TO 233E-WRITE-AGCM.
                                                                        <D9604>
      **** IF WSAA-PREMIUM-FLAG        = 'R'                    <PHFX39><D9604>
      ****    AND FLEXIBLE-PREMIUM-CONTRACT                     <PHFX39><D9604>
      ****    MOVE 0                   TO CLNK-INSTPREM         <PHFX39><D9604>
      **** END-IF.                                              <PHFX39><D9604>

           MOVE SPACES                 TO ITEM-DATA-AREA.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE CHDRLNB-CHDRCOY        TO ITEM-ITEMCOY.
           MOVE T5644                  TO ITEM-ITEMTABL.

           IF WSAA-PREMIUM-FLAG        = 'S'
      ***     IF T5687-BASSCPY         = SPACES                 <V42004><CAS1.0>
              IF WSAA-BASSCPY          = SPACES                         <V42004>
      ****    IF T5687-ZRORPMSP        = SPACES                         <CAS1.0>
                 MOVE AGLFLNB-BCMTAB   TO ITEM-ITEMITEM
                                          CLNK-METHOD
                                          AGCM-BASCPY
              ELSE
      ***        MOVE T5687-BASSCPY    TO ITEM-ITEMITEM         <V42004><CAS1.0>
      ****       MOVE T5687-ZRORPMSP   TO ITEM-ITEMITEM                 <CAS1.0>
                 MOVE WSAA-BASSCPY     TO ITEM-ITEMITEM                 <V42004>
                                          CLNK-METHOD
                                          AGCM-BASCPY
                                          CLNK-ZCOMCODE                 <CAS1.0>
           ELSE
      ***     IF T5687-BASCPY          = SPACES                 <V42004><CAS1.0>
      ****    IF T5687-ZRORPMRG        = SPACES                         <CAS1.0>
              IF WSAA-BASCPY           = SPACES                         <V42004>
                 MOVE AGLFLNB-BCMTAB   TO ITEM-ITEMITEM
                                          CLNK-METHOD
                                          AGCM-BASCPY
                                          CLNK-ZCOMCODE                 <CAS1.0>
              ELSE
      ***        MOVE T5687-BASCPY     TO ITEM-ITEMITEM         <V42004><CAS1.0>
      ****       MOVE T5687-ZRORPMRG   TO ITEM-ITEMITEM                 <CAS1.0>
                 MOVE WSAA-BASCPY      TO ITEM-ITEMITEM                 <V42004>
                                          AGCM-BASCPY
                                          CLNK-ZCOMCODE                 <CAS1.0>
                                          CLNK-METHOD.


           CALL 'ITEMIO' USING         ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = O-K
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              MOVE ITEM-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
           MOVE ITEM-GENAREA           TO T5644-T5644-REC.

           IF T5644-COMPYSUBR           NOT = SPACES
              CALL T5644-COMPYSUBR      USING CLNK-CLNKALL-REC
      ****    IF CLNK-STATUZ         NOT  = O-K                         <CAS1.0>
              IF CLNK-STATUZ            OF   CLNK-CLNKALL-REC           <CAS1.0>
                                        NOT = O-K                       <CAS1.0>
                 MOVE CLNK-CLNKALL-REC    TO SYSR-PARAMS
      ****       MOVE CLNK-STATUZ         TO SYSR-STATUZ                <CAS1.0>
                 MOVE CLNK-STATUZ       OF   CLNK-CLNKALL-REC           <CAS1.0>
                                          TO SYSR-STATUZ                <CAS1.0>
                 PERFORM XXXX-FATAL-ERROR.

           MOVE CLNK-ERNDAMT           TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO AGCM-COMERN.                  <V76F06>
                                                                        <V76F06>
           MOVE CLNK-PAYAMNT           TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO AGCM-COMPAY.                  <V76F06>
                                                                        <V76F06>
           MOVE CLNK-ICOMMTOT          TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO AGCM-INITCOM.                 <V76F06>
                                                                        <V76F06>
      **** MOVE CLNK-PAYAMNT           TO AGCM-COMPAY.          <V76F06><CAS1.0>
      **** MOVE CLNK-ERNDAMT           TO AGCM-COMERN.          <V76F06><CAS1.0>
      **** MOVE CLNK-ICOMMTOT          TO AGCM-INITCOM.         <V76F06><CAS1.0>
                                                                        <CAS1.0>
       233E-WRITE-AGCM.
      *                                                                 <086>
      * Add 1 to the no. of times this section has been looped.         <086>
      *                                                                 <086>
           ADD 1                          TO WSAA-OVRTIMES              <086>
      *                                                                 <086>
      * Compute the overrider commissions.
      **** COMPUTE                                                      <086>
      ****   AGCM-INITCOM ROUNDED                                       <086>
      ****                = CLNK-ICOMMTOT * (WSAA-STORED-OVCPC / 100).  <086>


      **** COMPUTE                                                      <086>
      ****   AGCM-COMPAY ROUNDED                                        <086>
      ****                = CLNK-PAYAMNT * (WSAA-STORED-OVCPC / 100).   <086>
      **** COMPUTE                                                      <086>
      ****   AGCM-COMERN ROUNDED                                        <086>
      ****                = CLNK-ERNDAMT * (WSAA-STORED-OVCPC / 100).   <086>
      *
      *                                                                 <086>
      * If this is the first time the overrider section is being        <086>
      * performed for a particular policy, compute o/r commission       <086>
      * using the initial commission as a basis for calculations.       <086>
      * Otherwise use the stored values (with suffix 'KEPT') to         <086>
      * allow for the possibility of there being more than one          <086>
      * o/r level.                                                      <086>
      *                                                                 <086>
           IF WSAA-OVRTIMES < 2                                         <086>
                                                                        <086>
             COMPUTE AGCM-INITCOM ROUNDED                               <086>
                          = CLNK-ICOMMTOT * (WSAA-STORED-OVCPC / 100)   <086>
                                                                        <V76F06>
             MOVE AGCM-INITCOM           TO ZRDP-AMOUNT-IN              <V76F06>
             MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY               <V76F06>
             PERFORM 9000-CALL-ROUNDING                                 <V76F06>
             MOVE ZRDP-AMOUNT-OUT        TO AGCM-INITCOM                <V76F06>
                                                                        <V76F06>
             MOVE AGCM-INITCOM TO WSAA-COMTOT-KEPT                      <086>
                                                                        <086>
             COMPUTE AGCM-COMPAY ROUNDED                                <086>
                          = CLNK-PAYAMNT * (WSAA-STORED-OVCPC / 100)    <086>
                                                                        <V76F06>
             MOVE AGCM-COMPAY            TO ZRDP-AMOUNT-IN              <V76F06>
             MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY               <V76F06>
             PERFORM 9000-CALL-ROUNDING                                 <V76F06>
             MOVE ZRDP-AMOUNT-OUT        TO AGCM-COMPAY                 <V76F06>
                                                                        <V76F06>
             MOVE AGCM-COMPAY TO WSAA-COMPAY-KEPT                       <086>
                                                                        <086>
             COMPUTE AGCM-COMERN ROUNDED                                <086>
                          = CLNK-ERNDAMT * (WSAA-STORED-OVCPC / 100)    <086>
                                                                        <V76F06>
             MOVE AGCM-COMERN            TO ZRDP-AMOUNT-IN              <V76F06>
             MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY               <V76F06>
             PERFORM 9000-CALL-ROUNDING                                 <V76F06>
             MOVE ZRDP-AMOUNT-OUT        TO AGCM-COMERN                 <V76F06>
                                                                        <V76F06>
             MOVE AGCM-COMERN TO WSAA-COMERN-KEPT                       <086>
                                                                        <086>
           ELSE                                                         <086>
                                                                        <086>
             COMPUTE AGCM-INITCOM ROUNDED                               <086>
                       = WSAA-COMTOT-KEPT * (WSAA-STORED-OVCPC / 100)   <086>
                                                                        <V76F06>
             MOVE AGCM-INITCOM           TO ZRDP-AMOUNT-IN              <V76F06>
             MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY               <V76F06>
             PERFORM 9000-CALL-ROUNDING                                 <V76F06>
             MOVE ZRDP-AMOUNT-OUT        TO AGCM-INITCOM                <V76F06>
                                                                        <V76F06>
             MOVE AGCM-INITCOM TO WSAA-COMTOT-KEPT                      <086>
                                                                        <086>
             COMPUTE AGCM-COMPAY ROUNDED                                <086>
                       = WSAA-COMPAY-KEPT * (WSAA-STORED-OVCPC / 100)   <086>
                                                                        <V76F06>
             MOVE AGCM-COMPAY            TO ZRDP-AMOUNT-IN              <V76F06>
             MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY               <V76F06>
             PERFORM 9000-CALL-ROUNDING                                 <V76F06>
             MOVE ZRDP-AMOUNT-OUT        TO AGCM-COMPAY                 <V76F06>
                                                                        <V76F06>
             MOVE AGCM-COMPAY TO WSAA-COMPAY-KEPT                       <086>
                                                                        <086>
             COMPUTE AGCM-COMERN ROUNDED                                <086>
                       = WSAA-COMERN-KEPT * (WSAA-STORED-OVCPC / 100)   <086>
                                                                        <V76F06>
             MOVE AGCM-COMERN            TO ZRDP-AMOUNT-IN              <V76F06>
             MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY               <V76F06>
             PERFORM 9000-CALL-ROUNDING                                 <V76F06>
             MOVE ZRDP-AMOUNT-OUT        TO AGCM-COMERN                 <V76F06>
                                                                        <V76F06>
             MOVE AGCM-COMERN TO WSAA-COMERN-KEPT                       <086>
                                                                        <086>
           END-IF.                                                      <086>
                                                                        <086>
      * Store override commission values.
      *
           IF COMP-LEVEL-ACC
               MOVE AGCM-COMERN        TO WSAA-COMP-ERN-OCOMM
               COMPUTE
                   WSAA-COMP-ADV-OCOMM ROUNDED
                               = AGCM-COMPAY - AGCM-COMERN.

      * Write the AGCM record.
           MOVE 0                      TO AGCM-SCMDUE,
                                          AGCM-SCMEARN,
                                          AGCM-RNLCDUE,
                                          AGCM-RNLCEARN.

           MOVE SPACES                 TO AGCM-SRVCPY,
                                          AGCM-RNWCPY.

*******    MOVE CLNK-ANNPREM           TO AGCM-ANNPREM.                 <084>
      *                                                                 <084>
      * The following Compute statements are used to round the contents <084>
      * of WSAA-CLNK-ANNPREM and move them into CLNK-ANNPREM and        <084>
      * AGCM-ANNPREM.                                                   <084>
      *                                                                 <084>
           COMPUTE CLNK-ANNPREM ROUNDED                                 <084>
                   = WSAA-CLNK-ANNPREM * 1                              <084>
      *                                                                 <084>
           COMPUTE AGCM-ANNPREM ROUNDED                                 <084>
                   = WSAA-CLNK-ANNPREM * 1                              <084>
      *                                                                 <084>
      *

      * If all commission values are zero do not write an AGCM.
           IF  AGCM-ANNPREM            = 0
           AND AGCM-INITCOM            = 0
           AND AGCM-COMPAY             = 0
           AND AGCM-COMERN             = 0
           AND AGCM-SCMDUE             = 0
           AND AGCM-SCMEARN            = 0
           AND AGCM-RNLCDUE            = 0
           AND AGCM-RNLCEARN           = 0
              GO 233E-CHECK-NEXT.

           MOVE WSAA-TERMID            TO AGCM-TERMID.
           MOVE WSAA-TRANSACTION-DATE  TO AGCM-TRANSACTION-DATE.
           MOVE WSAA-TRANSACTION-TIME  TO AGCM-TRANSACTION-TIME.
           MOVE WSAA-USER              TO AGCM-USER.
           MOVE COVRLNB-CHDRCOY        TO AGCM-CHDRCOY.
           MOVE COVRLNB-CHDRNUM        TO AGCM-CHDRNUM.
      *****MOVE AGLFLNB-AGNTNUM        TO AGCM-AGNTNUM,                 <077>
           MOVE AGLFLNB-AGNTNUM        TO AGCM-AGNTNUM.                 <077>
      *****                               WSAA-AGCM-AGNTNUM.            <069>
      *****MOVE WSAA-STORED-AGENT-CLASS TO AGCM-AGENT-CLASS.            <077>
           MOVE AGLFLNB-AGENT-CLASS    TO AGCM-AGENT-CLASS.             <077>
           MOVE COVRLNB-LIFE           TO AGCM-LIFE.
           MOVE COVRLNB-COVERAGE       TO AGCM-COVERAGE.
           MOVE COVRLNB-RIDER          TO AGCM-RIDER.
           MOVE COVRLNB-PLAN-SUFFIX    TO AGCM-PLAN-SUFFIX.
           MOVE CHDRLNB-TRANNO         TO AGCM-TRANNO.
           MOVE CHDRLNB-OCCDATE        TO AGCM-EFDATE.
           MOVE COVRLNB-CURRFROM       TO AGCM-CURRFROM.
           MOVE COVRLNB-CURRTO         TO AGCM-CURRTO.
           MOVE 'O'                    TO AGCM-OVRDCAT.
           MOVE '1'                    TO AGCM-VALIDFLAG.
      *****MOVE SPACES                 TO AGCM-CEDAGENT.                <069>
                                                                        <069>
           IF WSAA-AGCM-CEDAGENT        = AGCM-AGNTNUM                  <069>
              MOVE SPACES              TO AGCM-CEDAGENT                 <069>
           ELSE                                                         <069>
              MOVE WSAA-AGCM-CEDAGENT  TO AGCM-CEDAGENT                 <069>
           END-IF.                                                      <069>
                                                                        <069>
      **** MOVE CHDRLNB-PTDATE         TO AGCM-PTDATE.                  <A05940>
                                                                        <A05940>
           IF WSAA-PREMIUM-FLAG         = 'S'                           <A05940>
               MOVE ZEROES             TO AGCM-PTDATE                   <A05940>
           ELSE                                                         <A05940>
               MOVE CHDRLNB-PTDATE     TO AGCM-PTDATE                   <A05940>
           END-IF.                                                      <A05940>
                                                                        <A05940>
           MOVE 1                      TO AGCM-SEQNO.
           MOVE AGCMREC                TO AGCM-FORMAT.
           MOVE WRITR                  TO AGCM-FUNCTION.
           CALL 'AGCMIO'               USING AGCM-PARAMS.
           IF AGCM-STATUZ              NOT = O-K
              MOVE AGCM-PARAMS         TO SYSR-PARAMS
              MOVE AGCM-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
      *
           ADD 1                       TO WSAA-SUB.
           IF WSAA-SUB                 > 10
              MOVE I035                TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
      *
           MOVE AGLFLNB-AGNTNUM       TO WSAA-OVERRIDE-AGNTNUM
                                         (WSAA-AGENT-SUB WSAA-SUB).
           COMPUTE WSAA-OVRD-COMM-PAID(WSAA-AGENT-SUB WSAA-SUB) ROUNDED
                         = WSAA-OVRD-COMM-PAID(WSAA-AGENT-SUB WSAA-SUB)
                           +  (AGCM-COMPAY - AGCM-COMERN).
           ADD  AGCM-COMPAY           TO WSAA-OVERRIDE-COMM
                                         (WSAA-AGENT-SUB WSAA-SUB).
           MOVE AGCM-CEDAGENT         TO WSAA-CEDAGENT
                                         (WSAA-AGENT-SUB WSAA-SUB).

           IF NOT COMP-LEVEL-ACC
               GO TO 233E-CHECK-NEXT.
      *
      * Do override commission ACMVs for this overriding agent for
      * this agent for this component (WSAA-AGENT-SUB and WSAA-SUB
      * should already be correctly set).
      *

           MOVE COVRLNB-CRTABLE         TO LIFA-SUBSTITUTE-CODE(06).

       233E-OVERRIDE-COMM-EARNED.
           IF WSAA-COMP-ERN-OCOMM NOT = ZEROES
              MOVE WSAA-T5645-SACSCODE(10)  TO LIFA-SACSCODE
              MOVE WSAA-T5645-SACSTYPE(10)  TO LIFA-SACSTYP
              MOVE WSAA-T5645-GLMAP(10)     TO LIFA-GLCODE
              MOVE WSAA-T5645-SIGN(10)      TO LIFA-GLSIGN
              MOVE WSAA-T5645-CNTTOT(10)    TO LIFA-CONTOT
              MOVE WSAA-COMP-ERN-OCOMM      TO LIFA-ORIGAMT
              MOVE COVRLNB-CHDRNUM         TO LIFA-TRANREF
              ADD  +1                     TO WSAA-JRNSEQ
              MOVE WSAA-JRNSEQ            TO LIFA-JRNSEQ
              CALL 'LIFACMV'           USING LIFA-LIFACMV-REC
              PERFORM 2950-LIFACMV-CHECK.
      *
       233E-OVERRIDE-COMM-ADVANCED.
           IF WSAA-COMP-ADV-OCOMM NOT = ZEROS
              MOVE WSAA-T5645-SACSCODE(11)  TO LIFA-SACSCODE
              MOVE WSAA-T5645-SACSTYPE(11)  TO LIFA-SACSTYP
              MOVE WSAA-T5645-GLMAP(11)     TO LIFA-GLCODE
              MOVE WSAA-T5645-SIGN(11)      TO LIFA-GLSIGN
              MOVE WSAA-T5645-CNTTOT(11)    TO LIFA-CONTOT
              MOVE WSAA-COMP-ADV-OCOMM      TO LIFA-ORIGAMT
              MOVE COVRLNB-CHDRNUM         TO LIFA-TRANREF
              ADD  +1                       TO WSAA-JRNSEQ
              MOVE WSAA-JRNSEQ              TO LIFA-JRNSEQ
              CALL 'LIFACMV'           USING LIFA-LIFACMV-REC
              PERFORM 2950-LIFACMV-CHECK.
                                                                        <V4L014>
       233E-SUM-OVERRIDE-COMM.                                          <V4L014>
           IF WSAA-COMP-ADV-OCOMM NOT = ZEROS  OR                       <V4L014>
              WSAA-COMP-ERN-OCOMM NOT = ZEROS                           <V4L014>
              MOVE T5645-SACSCODE-15        TO LIFA-SACSCODE            <V4L014>
              MOVE T5645-SACSTYPE-15        TO LIFA-SACSTYP             <V4L014>
              MOVE T5645-GLMAP-15           TO LIFA-GLCODE              <V4L014>
              MOVE T5645-SIGN-15            TO LIFA-GLSIGN              <V4L014>
              MOVE T5645-CNTTOT-15          TO LIFA-CONTOT              <V4L014>
              MOVE AGLFLNB-AGNTNUM          TO LIFA-RLDGACCT            <V4L014>
              MOVE WSAA-COMP-ADV-OCOMM      TO LIFA-ORIGAMT             <V4L014>
              ADD  WSAA-COMP-ERN-OCOMM      TO LIFA-ORIGAMT             <V4L014>
              MOVE WSAA-RLDGACCT            TO LIFA-TRANREF             <V4L014>
              ADD  +1                       TO WSAA-JRNSEQ              <V4L014>
              MOVE WSAA-JRNSEQ              TO LIFA-JRNSEQ              <V4L014>
              CALL 'LIFACMV'          USING LIFA-LIFACMV-REC            <V4L014>
              PERFORM 2950-LIFACMV-CHECK                                <V4L014>
              MOVE WSAA-RLDGACCT            TO LIFA-RLDGACCT            <V4L014>
           END-IF.                                                      <V4L014>
      *
       233E-CHECK-NEXT.
      * Read the next reports to if it exists..
           IF AGLFLNB-REPORTAG         = SPACES
              MOVE 0                   TO AGLFLNB-OVCPC
      *****   GO TO 233E-EXIT.                                          <069>
           END-IF.                                                      <069>
      *
      *  This code is not necessary because the loop condition will     <069>
      *  control exit.                                                  <069>
      *****IF AGLFLNB-OVCPC            = ZEROS                          <069>
      *****   GO 233E-EXIT.                                             <069>

      * Check whether the overiders are looping.
      * Since the online system checks the chain of overriding agents   <069>
      * this is not necessary.                                          <069>
      *****IF AGLFLNB-REPORTAG         = WSAA-AGCM-AGNTNUM              <069>
      *****   MOVE SPACES              TO AGLFLNB-REPORTAG              <069>
      *****   MOVE 0                   TO AGLFLNB-OVCPC                 <069>
      *****   GO TO 233E-EXIT.                                          <069>


      *
       233E-EXIT.
            EXIT.

       233F-COMPONENT-POSTINGS SECTION.
      ********************************
       2331-COMPONENT-POSTINGS.
      *
      * Do the commission postings for this component, 1 per agent.
      *
       2334-POST-INIT-EARNED-COMM.
           IF WSAA-COMP-ERN-ICOMM NOT = 0
              MOVE WSAA-T5645-SACSCODE(06)      TO LIFA-SACSCODE
              MOVE WSAA-T5645-SACSTYPE(06)      TO LIFA-SACSTYP
              MOVE WSAA-T5645-GLMAP(06)         TO LIFA-GLCODE
              MOVE WSAA-T5645-SIGN(06)          TO LIFA-GLSIGN
              MOVE WSAA-T5645-CNTTOT(06)        TO LIFA-CONTOT
              MOVE COVRLNB-CHDRNUM         TO LIFA-TRANREF
              MOVE WSAA-COMP-ERN-ICOMM     TO LIFA-ORIGAMT
              ADD  +1                      TO WSAA-JRNSEQ
              MOVE WSAA-JRNSEQ             TO LIFA-JRNSEQ
              CALL 'LIFACMV'               USING LIFA-LIFACMV-REC
              PERFORM 2950-LIFACMV-CHECK.

       2335-POST-INIT-ADVANCE-COMM.
           IF WSAA-COMP-ADV-ICOMM NOT = 0
              MOVE WSAA-T5645-SACSCODE(07)      TO LIFA-SACSCODE
              MOVE WSAA-T5645-SACSTYPE(07)      TO LIFA-SACSTYP
              MOVE WSAA-T5645-GLMAP(07)         TO LIFA-GLCODE
              MOVE WSAA-T5645-SIGN(07)          TO LIFA-GLSIGN
              MOVE WSAA-T5645-CNTTOT(07)        TO LIFA-CONTOT
              MOVE COVRLNB-CHDRNUM         TO LIFA-TRANREF
              MOVE WSAA-COMP-ADV-ICOMM     TO LIFA-ORIGAMT
              ADD  +1                      TO WSAA-JRNSEQ
              MOVE WSAA-JRNSEQ             TO LIFA-JRNSEQ
              CALL 'LIFACMV'               USING LIFA-LIFACMV-REC
              PERFORM 2950-LIFACMV-CHECK.
                                                                        <V4L014>
           IF WSAA-COMP-ADV-ICOMM NOT = ZEROS  OR                       <V4L014>
              WSAA-COMP-ERN-ICOMM NOT = ZEROS                           <V4L014>
              MOVE T5645-SACSCODE-08        TO LIFA-SACSCODE            <V4L014>
              MOVE T5645-SACSTYPE-08        TO LIFA-SACSTYP             <V4L014>
              MOVE T5645-GLMAP-08           TO LIFA-GLCODE              <V4L014>
              MOVE T5645-SIGN-08            TO LIFA-GLSIGN              <V4L014>
              MOVE T5645-CNTTOT-08          TO LIFA-CONTOT              <V4L014>
   ****                                                                 <V6L000>
   **** IF THIS IS A Single Premium Commission, posting TO 'SG'         <V6L000>
   ****                                                                 <V6L000>
              IF WSAA-SING-PRM-IND = 'Y'                                <V6L000>
                 MOVE WSAA-T5645-SACSCODE(15)  TO LIFA-SACSCODE         <V6L000>
                 MOVE WSAA-T5645-SACSTYPE(15)  TO LIFA-SACSTYP          <V6L000>
                 MOVE WSAA-T5645-GLMAP(15)     TO LIFA-GLCODE           <V6L000>
                 MOVE WSAA-T5645-SIGN(15)      TO LIFA-GLSIGN           <V6L000>
                 MOVE WSAA-T5645-CNTTOT(15)    TO LIFA-CONTOT           <V6L000>
              END-IF                                                    <V6L000>
                                                                        <V6L000>
              MOVE WSAA-SAVE-AGNTNUM        TO LIFA-RLDGACCT            <V4L014>
              MOVE WSAA-COMP-ADV-ICOMM      TO LIFA-ORIGAMT             <V4L014>
              ADD  WSAA-COMP-ERN-ICOMM      TO LIFA-ORIGAMT             <V4L014>
              MOVE COVRLNB-CHDRNUM          TO WSAA-RLDG-CHDRNUM        <V4L014>
              MOVE COVRLNB-LIFE             TO WSAA-RLDG-LIFE           <V4L014>
              MOVE COVRLNB-COVERAGE         TO WSAA-RLDG-COVERAGE       <V4L014>
              MOVE COVRLNB-RIDER            TO WSAA-RLDG-RIDER          <V4L014>
              MOVE COVRLNB-PLAN-SUFFIX      TO WSAA-PLAN                <V4L014>
              MOVE WSAA-PLANSUFF            TO WSAA-RLDG-PLAN-SUFFIX    <V4L014>
              MOVE WSAA-RLDGACCT            TO LIFA-TRANREF             <V4L014>
              ADD  +1                       TO WSAA-JRNSEQ              <V4L014>
              MOVE WSAA-JRNSEQ              TO LIFA-JRNSEQ              <V4L014>
              CALL 'LIFACMV'          USING LIFA-LIFACMV-REC            <V4L014>
              PERFORM 2950-LIFACMV-CHECK                                <V4L014>
      *                                                                 <V73L01>
      * Override Commission                                             <V73L01>
      *                                                                 <V73L01>
              IF TH605-INDIC               = 'Y'                        <V73L01>
                 MOVE AGCM-ANNPREM     TO ZORL-ANNPREM                  <V73L01>
                 PERFORM B100-CALL-ZORCOMPY                             <V73L01>
              END-IF                                                    <V73L01>
              MOVE WSAA-RLDGACCT            TO LIFA-RLDGACCT            <V4L014>
           END-IF.                                                      <V4L014>

       2336-POST-SERV-EARNED-COMM.
           IF WSAA-COMP-ERN-SCOMM NOT = 0
              MOVE WSAA-T5645-SACSCODE(08)      TO LIFA-SACSCODE
              MOVE WSAA-T5645-SACSTYPE(08)      TO LIFA-SACSTYP
              MOVE WSAA-T5645-GLMAP(08)         TO LIFA-GLCODE
              MOVE WSAA-T5645-SIGN(08)          TO LIFA-GLSIGN
              MOVE WSAA-T5645-CNTTOT(08)        TO LIFA-CONTOT
              MOVE COVRLNB-CHDRNUM         TO LIFA-TRANREF
              MOVE WSAA-COMP-ERN-SCOMM     TO LIFA-ORIGAMT
              ADD  +1                      TO WSAA-JRNSEQ
              MOVE WSAA-JRNSEQ             TO LIFA-JRNSEQ
              CALL 'LIFACMV'               USING LIFA-LIFACMV-REC
              PERFORM 2950-LIFACMV-CHECK.

       2337-POST-RENEWAL-EARNED-COMM.
           IF WSAA-COMP-ERN-RCOMM NOT = 0
              MOVE WSAA-T5645-SACSCODE(09)      TO LIFA-SACSCODE
              MOVE WSAA-T5645-SACSTYPE(09)      TO LIFA-SACSTYP
              MOVE WSAA-T5645-GLMAP(09)         TO LIFA-GLCODE
              MOVE WSAA-T5645-SIGN(09)          TO LIFA-GLSIGN
              MOVE WSAA-T5645-CNTTOT(09)        TO LIFA-CONTOT
              MOVE COVRLNB-CHDRNUM         TO LIFA-TRANREF
              MOVE WSAA-COMP-ERN-RCOMM     TO LIFA-ORIGAMT
              ADD  +1                      TO WSAA-JRNSEQ
              MOVE WSAA-JRNSEQ             TO LIFA-JRNSEQ
              CALL 'LIFACMV'               USING LIFA-LIFACMV-REC
              PERFORM 2950-LIFACMV-CHECK.
                                                                        <PHL108>
PHL108 2337A-POST-STAFF-DISCOUNT.                                       <PHL108>
           PERFORM B200-READ-ZDISPF.                                    <PHL108>
                                                                        <PHL108>
           IF ZDISPF-FOUND                                              <PHL108>
           AND WSAA-STORED-STAFF-DIS NOT = ZEROS                        <PHL108>
      * POST 'LP DC'                                                    <PHL108>
              MOVE WSAA-T5645-SACSCODE-02(01)   TO LIFA-SACSCODE        <PHL108>
              MOVE WSAA-T5645-SACSTYPE-02(01)   TO LIFA-SACSTYP         <PHL108>
              MOVE WSAA-T5645-GLMAP-02(01)      TO LIFA-GLCODE          <PHL108>
              MOVE WSAA-T5645-SIGN-02(01)       TO LIFA-GLSIGN          <PHL108>
              MOVE WSAA-T5645-CNTTOT-02(01)     TO LIFA-CONTOT          <PHL108>
              MOVE COVRLNB-CHDRNUM              TO LIFA-TRANREF         <PHL108>
              MOVE WSAA-STORED-STAFF-DIS        TO LIFA-ORIGAMT         <PHL108>
              ADD  +1                           TO WSAA-JRNSEQ          <PHL108>
              MOVE WSAA-JRNSEQ                  TO LIFA-JRNSEQ          <PHL108>
              CALL 'LIFACMV'               USING LIFA-LIFACMV-REC       <PHL108>
              PERFORM 2950-LIFACMV-CHECK                                <PHL108>
                                                                        <PHL108>
      * POST 'LP DF'                                                    <PHL108>
              MOVE WSAA-T5645-SACSCODE-02(02)   TO LIFA-SACSCODE        <PHL108>
              MOVE WSAA-T5645-SACSTYPE-02(02)   TO LIFA-SACSTYP         <PHL108>
              MOVE WSAA-T5645-GLMAP-02(02)      TO LIFA-GLCODE          <PHL108>
              MOVE WSAA-T5645-SIGN-02(02)       TO LIFA-GLSIGN          <PHL108>
              MOVE WSAA-T5645-CNTTOT-02(02)     TO LIFA-CONTOT          <PHL108>
              MOVE COVRLNB-CHDRNUM              TO LIFA-TRANREF         <PHL108>
              MOVE WSAA-STORED-STAFF-DIS        TO LIFA-ORIGAMT         <PHL108>
              ADD  +1                           TO WSAA-JRNSEQ          <PHL108>
              MOVE WSAA-JRNSEQ                  TO LIFA-JRNSEQ          <PHL108>
              CALL 'LIFACMV'               USING LIFA-LIFACMV-REC       <PHL108>
              PERFORM 2950-LIFACMV-CHECK                                <PHL108>
           END-IF.                                                      <PHL108>

       2338-COMPONENT-TAX-RELIEF.
      * Tax Relief must be calculated per component.(It will also
      * be calculated per payer later in the program for posting
      * net figures against suspense).

           MOVE ZERO                       TO WSAA-COMP-TAX-RELIEF.

       2338A-GET-TAX-RELF-SUBROUTINE.
           MOVE SPACES                     TO ITEM-DATA-KEY.
           MOVE 'IT'                       TO ITEM-ITEMPFX.
           MOVE CHDRLNB-CHDRCOY            TO ITEM-ITEMCOY.
           MOVE T6687                      TO ITEM-ITEMTABL.
           MOVE T5688-TAXRELMTH            TO ITEM-ITEMITEM.

           CALL 'ITEMIO' USING ITEM-PARAMS.

           IF ITEM-STATUZ  NOT = O-K  AND
                           NOT = MRNF
               MOVE ITEM-STATUZ            TO SYSR-STATUZ
               MOVE ITEM-PARAMS            TO SYSR-PARAMS
               PERFORM XXXX-FATAL-ERROR.

           IF ITEM-STATUZ = O-K
               MOVE ITEM-GENAREA           TO T6687-T6687-REC
           ELSE
               MOVE SPACES                 TO T6687-T6687-REC.

       2338B-CHECK-RTRN-FILE.
      * Read the RTRN file to see if a cash receipt has been
      * created for this contract.
           MOVE CHDRLNB-CHDRCOY            TO RTRNSAC-RLDGCOY.
           MOVE CHDRLNB-CHDRNUM            TO RTRNSAC-RLDGACCT.
      **** MOVE CHDRLNB-CNTCURR            TO RTRNSAC-ORIGCCY.          <CAS1.0>
           IF WSAA-SUSP-IND                 = 'Y'                       <CAS1.0>
              MOVE ACBLENQ-ORIGCURR        TO RTRNSAC-ORIGCCY           <CAS1.0>
           ELSE                                                         <CAS1.0>
              MOVE SPACE                   TO RTRNSAC-ORIGCCY           <CAS1.0>
           END-IF.                                                      <CAS1.0>
           MOVE T5645-SACSCODE-01          TO RTRNSAC-SACSCODE.
           MOVE T5645-SACSTYPE-01          TO RTRNSAC-SACSTYP.
           MOVE READR                      TO RTRNSAC-FUNCTION.

           CALL 'RTRNSACIO'            USING RTRNSAC-PARAMS.

           IF  RTRNSAC-STATUZ      NOT = O-K
           AND                     NOT = MRNF
              MOVE RTRNSAC-PARAMS          TO SYSR-PARAMS
              PERFORM XXXX-FATAL-ERROR.

           IF RTRNSAC-STATUZ           = MRNF
              MOVE VRCM-MAX-DATE           TO RTRNSAC-EFFDATE.

       2338C-CALC-TAX-RELIEF.
      * If the tax relief method is not spaces calculate the tax
      * relief amount and deduct it from the premium.

           IF T5688-TAXRELMTH NOT = SPACES
               MOVE COVRLNB-PAYRSEQNO      TO WSBB-SUB
               MOVE WSAA-CLNTNUM(WSBB-SUB) TO PRAS-CLNTNUM
               MOVE WSAA-CLNTCOY(WSBB-SUB) TO PRAS-CLNTCOY
               MOVE WSAA-INCOME-SEQ-NO(WSBB-SUB)
                                           TO PRAS-INCOME-SEQ-NO
               MOVE CHDRLNB-CNTTYPE        TO PRAS-CNTTYPE
               MOVE T5688-TAXRELMTH        TO PRAS-TAXRELMTH

      * Use the due date unless a receipt exists with a date later
      * then the due date.

               IF RTRNSAC-EFFDATE = VRCM-MAX-DATE
                   MOVE CHDRLNB-OCCDATE    TO PRAS-EFFDATE
               ELSE
                  IF CHDRLNB-OCCDATE       > RTRNSAC-EFFDATE
                     MOVE CHDRLNB-OCCDATE  TO PRAS-EFFDATE
                  ELSE
                     MOVE RTRNSAC-EFFDATE  TO PRAS-EFFDATE
                  END-IF
               END-IF

               MOVE CHDRLNB-CHDRCOY        TO PRAS-COMPANY
               COMPUTE
                   PRAS-GROSSPREM =
                       WSAA-COMP-SING-PREM + WSAA-COMP-REG-PREM
               MOVE O-K                    TO PRAS-STATUZ

               CALL T6687-TAXRELSUB  USING PRAS-PRASCALC-REC

               IF PRAS-STATUZ  NOT = O-K
                    MOVE PRAS-STATUZ       TO SYSR-STATUZ
                    MOVE T6687-TAXRELSUB   TO SYSR-SUBRNAME
                    PERFORM XXXX-FATAL-ERROR
               END-IF

               MOVE PRAS-TAXRELAMT         TO ZRDP-AMOUNT-IN            <V76F06>
               MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY             <V76F06>
               PERFORM 9000-CALL-ROUNDING                               <V76F06>
               MOVE ZRDP-AMOUNT-OUT        TO WSAA-COMP-TAX-RELIEF      <V76F06>
                                                                        <V76F06>
      ****     MOVE PRAS-TAXRELAMT         TO WSAA-COMP-TAX-RELIEF      <V76F06>
           END-IF.

       233F-EXIT.
            EXIT.

       233G-PREM-TAX SECTION.                                           <V74L01>
      ***********************                                           <V74L01>
       233G-START.                                                      <V74L01>
           MOVE 0                      TO WSAA-PREM-TAX-01              <V74L01>
                                          WSAA-PREM-TAX-02              <V74L01>
                                          WSAA-TAX-BASE-AMT.            <V74L01>
      * Read table TR52E                                                <V74L01>
                                                                        <V74L01>
           MOVE SPACES                 TO WSAA-TR52E-KEY.               <V74L01>
           MOVE TR52D-TXCODE           TO WSAA-TR52E-TXCODE.            <V74L01>
           MOVE CHDRLNB-CNTTYPE        TO WSAA-TR52E-CNTTYPE.           <V74L01>
           MOVE COVRLNB-CRTABLE        TO WSAA-TR52E-CRTABLE.           <V74L01>
           PERFORM 233H-READ-TR52E.                                     <V74L01>
                                                                        <V74L01>
           IF TR52E-TR52E-REC = SPACES                                  <V74L01>
              MOVE SPACES              TO WSAA-TR52E-KEY                <V74L01>
              MOVE TR52D-TXCODE        TO WSAA-TR52E-TXCODE             <V74L01>
              MOVE CHDRLNB-CNTTYPE     TO WSAA-TR52E-CNTTYPE            <V74L01>
              MOVE '****'              TO WSAA-TR52E-CRTABLE            <V74L01>
              PERFORM 233H-READ-TR52E                                   <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF TR52E-TR52E-REC = SPACES                                  <V74L01>
              MOVE SPACES              TO WSAA-TR52E-KEY                <V74L01>
              MOVE TR52D-TXCODE        TO WSAA-TR52E-TXCODE             <V74L01>
              MOVE '***'               TO WSAA-TR52E-CNTTYPE            <V74L01>
              MOVE '****'              TO WSAA-TR52E-CRTABLE            <V74L01>
              PERFORM 233H-READ-TR52E                                   <V74L01>
           END-IF.                                                      <V74L01>
      *                                                                 <V74L01>
      * Call TR52D tax subroutine                                       <V74L01>
           IF TR52E-TAXIND-01          NOT = 'Y'                        <V74L01>
              GO TO 233G-EXIT                                           <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF COVRLNB-SINGP            NOT = ZERO                       <V74L01>
              MOVE 'CPST'              TO TXCL-FUNCTION                 <V74L01>
              MOVE O-K                 TO TXCL-STATUZ                   <V74L01>
              MOVE CHDRLNB-CHDRCOY     TO TXCL-CHDRCOY                  <V74L01>
              MOVE CHDRLNB-CHDRNUM     TO TXCL-CHDRNUM                  <V74L01>
              MOVE COVRLNB-LIFE        TO TXCL-LIFE                     <V74L01>
              MOVE COVRLNB-COVERAGE    TO TXCL-COVERAGE                 <V74L01>
              MOVE COVRLNB-RIDER       TO TXCL-RIDER                    <V74L01>
              MOVE COVRLNB-PLAN-SUFFIX TO TXCL-PLAN-SUFFIX              <V74L01>
              MOVE COVRLNB-CRTABLE     TO TXCL-CRTABLE                  <V74L01>
              MOVE WSAA-TR52E-KEY      TO TXCL-TAXRULE                  <V74L01>
              MOVE CHDRLNB-TRANNO      TO TXCL-TRANNO                   <V74L01>
              MOVE ATMD-LANGUAGE       TO TXCL-LANGUAGE                 <V74L01>
              MOVE SPACES              TO WSAA-RATE-ITEM                <V74L01>
              MOVE CHDRLNB-CNTCURR     TO TXCL-CCY                      <V74L01>
                                          WSAA-CNT-CURR                 <V74L01>
              MOVE TR52E-TXITEM        TO WSAA-TXITEM                   <V74L01>
              MOVE WSAA-RATE-ITEM      TO TXCL-RATE-ITEM                <V74L01>
              MOVE ZERO                TO TXCL-TAX-AMT(1)               <V74L01>
                                          TXCL-TAX-AMT(2)               <V74L01>
              MOVE SPACES              TO TXCL-TAX-TYPE(1)              <V74L01>
                                          TXCL-TAX-TYPE(2)              <V74L01>
                                          TXCL-TAX-ABSORB(1)            <V74L01>
                                          TXCL-TAX-ABSORB(2)            <V74L01>
                                          TXCL-CNT-TAX-IND              <V74L01>
              MOVE 'PREM'              TO TXCL-TRANS-TYPE               <V74L01>
              MOVE CHDRLNB-OCCDATE     TO TXCL-EFFDATE                  <V74L01>
              MOVE WSAA-JRNSEQ         TO TXCL-JRNSEQ                   <V74L01>
              MOVE WSAA-COMP-SING-PREM TO TXCL-AMOUNT-IN                <V74L01>
              MOVE ATMD-BATCH-KEY      TO TXCL-BATCKEY                  <V74L01>
                                                                        <V74L01>
              CALL TR52D-TXSUBR        USING TXCL-LINK-REC              <V74L01>
              IF TXCL-STATUZ           NOT = O-K                        <V74L01>
                 MOVE TXCL-LINK-REC    TO SYSR-STATUZ                   <V74L01>
                 MOVE TXCL-STATUZ      TO SYSR-STATUZ                   <V74L01>
                 PERFORM XXXX-FATAL-ERROR                               <V74L01>
              END-IF                                                    <V74L01>
                                                                        <V74L01>
              IF TXCL-TAX-ABSORB(1)     = 'Y'                           <V74L01>
                 NEXT SENTENCE                                          <V74L01>
              ELSE                                                      <V74L01>
                 ADD TXCL-TAX-AMT(1)   TO WSAA-SP-TAX(WSBB-SUB)         <V74L01>
              END-IF                                                    <V74L01>
              IF TXCL-TAX-ABSORB(2)     = 'Y'                           <V74L01>
                 NEXT SENTENCE                                          <V74L01>
              ELSE                                                      <V74L01>
                 ADD TXCL-TAX-AMT(2)   TO WSAA-SP-TAX(WSBB-SUB)         <V74L01>
              END-IF                                                    <V74L01>
              ADD  TXCL-TAX-AMT(1)     TO WSAA-PREM-TAX-01              <V74L01>
              ADD  TXCL-TAX-AMT(2)     TO WSAA-PREM-TAX-02              <V74L01>
              ADD  TXCL-AMOUNT-IN      TO WSAA-TAX-BASE-AMT             <V74L01>
              MOVE TXCL-JRNSEQ         TO WSAA-JRNSEQ                   <V74L01>
                                                                        <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF COVRLNB-INSTPREM         NOT = ZERO                       <V74L01>
              MOVE 'CPST'              TO TXCL-FUNCTION                 <V74L01>
              MOVE O-K                 TO TXCL-STATUZ                   <V74L01>
              MOVE CHDRLNB-CHDRCOY     TO TXCL-CHDRCOY                  <V74L01>
              MOVE CHDRLNB-CHDRNUM     TO TXCL-CHDRNUM                  <V74L01>
              MOVE COVRLNB-LIFE        TO TXCL-LIFE                     <V74L01>
              MOVE COVRLNB-COVERAGE    TO TXCL-COVERAGE                 <V74L01>
              MOVE COVRLNB-RIDER       TO TXCL-RIDER                    <V74L01>
              MOVE COVRLNB-PLAN-SUFFIX TO TXCL-PLAN-SUFFIX              <V74L01>
              MOVE COVRLNB-CRTABLE     TO TXCL-CRTABLE                  <V74L01>
              MOVE WSAA-TR52E-KEY      TO TXCL-TAXRULE                  <V74L01>
              MOVE SPACES              TO WSAA-RATE-ITEM                <V74L01>
              MOVE CHDRLNB-CNTCURR     TO TXCL-CCY                      <V74L01>
                                          WSAA-CNT-CURR                 <V74L01>
              MOVE CHDRLNB-TRANNO      TO TXCL-TRANNO                   <V74L01>
              MOVE ATMD-LANGUAGE       TO TXCL-LANGUAGE                 <V74L01>
              MOVE TR52E-TXITEM        TO WSAA-TXITEM                   <V74L01>
              MOVE WSAA-RATE-ITEM      TO TXCL-RATE-ITEM                <V74L01>
              MOVE ZERO                TO TXCL-TAX-AMT(1)               <V74L01>
                                          TXCL-TAX-AMT(2)               <V74L01>
              MOVE SPACES              TO TXCL-TAX-TYPE(1)              <V74L01>
                                          TXCL-TAX-TYPE(2)              <V74L01>
                                          TXCL-TAX-ABSORB(1)            <V74L01>
                                          TXCL-TAX-ABSORB(2)            <V74L01>
              MOVE 'PREM'              TO TXCL-TRANS-TYPE               <V74L01>
              MOVE CHDRLNB-OCCDATE     TO TXCL-EFFDATE                  <V74L01>
              MOVE WSAA-JRNSEQ         TO TXCL-JRNSEQ                   <V74L01>
              MOVE ATMD-BATCH-KEY      TO TXCL-BATCKEY                  <V74L01>
              MOVE ZERO                TO WSAA-AMOUNT-IN                <V74L01>
              IF TR52E-ZBASTYP         = 'Y'                            <V74L01>
                 COMPUTE WSAA-AMOUNT-IN = COVRLNB-ZBINSTPREM            <V74L01>
                                     * WSAA-FREQ-FACTOR(WSBB-SUB)       <V74L01>
              ELSE                                                      <V74L01>
                 COMPUTE WSAA-AMOUNT-IN = COVRLNB-INSTPREM              <V74L01>
                                     * WSAA-FREQ-FACTOR(WSBB-SUB)       <V74L01>
              END-IF                                                    <V74L01>
              MOVE WSAA-AMOUNT-IN      TO TXCL-AMOUNT-IN                <V74L01>
                                                                        <V74L01>
              CALL TR52D-TXSUBR        USING TXCL-LINK-REC              <V74L01>
              IF TXCL-STATUZ           NOT = O-K                        <V74L01>
                 MOVE TXCL-LINK-REC    TO SYSR-PARAMS                   <V74L01>
                 MOVE TXCL-STATUZ      TO SYSR-STATUZ                   <V74L01>
                 PERFORM XXXX-FATAL-ERROR                               <V74L01>
              END-IF                                                    <V74L01>
                                                                        <V74L01>
              IF TXCL-TAX-ABSORB(1)     = 'Y'                           <V74L01>
                 NEXT SENTENCE                                          <V74L01>
              ELSE                                                      <V74L01>
                 ADD TXCL-TAX-AMT(1)   TO WSAA-RP-TAX(WSBB-SUB)         <V74L01>
              END-IF                                                    <V74L01>
              IF TXCL-TAX-ABSORB(2)     = 'Y'                           <V74L01>
                 NEXT SENTENCE                                          <V74L01>
              ELSE                                                      <V74L01>
                ADD TXCL-TAX-AMT(2)    TO WSAA-RP-TAX(WSBB-SUB)         <V74L01>
              END-IF                                                    <V74L01>
              ADD  TXCL-TAX-AMT(1)     TO WSAA-PREM-TAX-01              <V74L01>
              ADD  TXCL-TAX-AMT(2)     TO WSAA-PREM-TAX-02              <V74L01>
              ADD  TXCL-AMOUNT-IN      TO WSAA-TAX-BASE-AMT             <V74L01>
                                                                        <V74L01>
              MOVE TXCL-JRNSEQ         TO WSAA-JRNSEQ                   <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
       233G-CREATE-TAXD.                                                <V74L01>
           IF WSAA-PREM-TAX-01          = 0                             <V74L01>
           AND WSAA-PREM-TAX-02         = 0                             <V74L01>
               GO TO 233G-EXIT                                          <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           INITIALIZE                        TAXD-DATA-AREA.            <V74L01>
           MOVE CHDRLNB-CHDRCOY           TO TAXD-CHDRCOY.              <V74L01>
           MOVE CHDRLNB-CHDRNUM           TO TAXD-CHDRNUM.              <V74L01>
           MOVE COVRLNB-LIFE              TO TAXD-LIFE.                 <V74L01>
           MOVE COVRLNB-COVERAGE          TO TAXD-COVERAGE.             <V74L01>
           MOVE COVRLNB-RIDER             TO TAXD-RIDER.                <V74L01>
           MOVE ZERO                      TO TAXD-PLANSFX.              <V74L01>
           MOVE TXCL-EFFDATE              TO TAXD-EFFDATE               <V74L01>
                                             TAXD-INSTFROM              <V74L01>
                                             TAXD-BILLCD.               <V74L01>
                                                                        <V74L01>
           IF WSAA-COMP-SING-PREM      NOT = ZERO                       <V74L01>
              MOVE  VRCM-MAX-DATE      TO TAXD-INSTTO                   <V74L01>
           ELSE                                                         <V74L01>
              MOVE WSAA-BTDATE(WSBB-SUB)                                <V74L01>
                                       TO TAXD-INSTTO                   <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           MOVE SPACES                   TO WSAA-TRANREF.               <V74L01>
           STRING TXCL-TAXRULE, TXCL-RATE-ITEM                          <V74L01>
                  DELIMITED BY SIZE    INTO WSAA-TRANREF.               <V74L01>
           MOVE WSAA-TRANREF           TO TAXD-TRANREF.                 <V74L01>
                                                                        <V74L01>
           MOVE CHDRLNB-TRANNO         TO TAXD-TRANNO.                  <V74L01>
           MOVE 'PREM'                 TO TAXD-TRANTYPE.                <V74L01>
           MOVE WSAA-TAX-BASE-AMT      TO TAXD-BASEAMT                  <V74L01>
           MOVE WSAA-PREM-TAX-01       TO TAXD-TAXAMT01.                <V74L01>
           MOVE WSAA-PREM-TAX-02       TO TAXD-TAXAMT02.                <V74L01>
           MOVE ZERO                   TO TAXD-TAXAMT03.                <V74L01>
           MOVE TXCL-TAX-ABSORB(1)     TO TAXD-TXABSIND01.              <V74L01>
           MOVE TXCL-TAX-ABSORB(2)     TO TAXD-TXABSIND02.              <V74L01>
           MOVE SPACES                 TO TAXD-TXABSIND03.              <V74L01>
           MOVE TXCL-TAX-TYPE(1)       TO TAXD-TXTYPE01.                <V74L01>
           MOVE TXCL-TAX-TYPE(2)       TO TAXD-TXTYPE02.                <V74L01>
           MOVE SPACES                 TO TAXD-TXTYPE03.                <V74L01>
           MOVE 'P'                    TO TAXD-POSTFLG.                 <V74L01>
                                                                        <V74L01>
           MOVE TAXDREC                TO TAXD-FORMAT.                  <V74L01>
           MOVE WRITR                  TO TAXD-FUNCTION.                <V74L01>
           CALL 'TAXDIO'               USING TAXD-PARAMS.               <V74L01>
                                                                        <V74L01>
           IF TAXD-STATUZ              NOT = O-K                        <V74L01>
              MOVE TAXD-PARAMS         TO SYSR-PARAMS                   <V74L01>
              MOVE TAXD-STATUZ         TO SYSR-STATUZ                   <V74L01>
              PERFORM XXXX-FATAL-ERROR                                  <V74L01>
           END-IF.                                                      <V74L01>
      *                                                                 <V74L01>
                                                                        <V74L01>
       233G-EXIT.                                                       <V74L01>
            EXIT.                                                       <V74L01>
                                                                        <V74L01>
       233H-READ-TR52E SECTION.                                         <V74L01>
      *************************                                         <V74L01>
       233H-START.                                                      <V74L01>
           MOVE SPACES                 TO ITDM-DATA-AREA                <V74L01>
                                          TR52E-TR52E-REC.              <V74L01>
           MOVE ATMD-COMPANY           TO ITDM-ITEMCOY.                 <V74L01>
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
               PERFORM XXXX-FATAL-ERROR                                 <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF ((ITDM-ITEMCOY           NOT = ATMD-COMPANY) OR           <V74L01>
               (ITDM-ITEMTABL          NOT = TR52E)          OR         <V74L01>
               (ITDM-ITEMITEM          NOT  = WSAA-TR52E-KEY) OR        <V74L01>
               (ITDM-STATUZ            = ENDP))        AND              <V74L01>
               (WSAA-TR52E-KEY(2:7)    = '*******')                     <V74L01>
               MOVE WSAA-TR52E-KEY     TO SYSR-PARAMS                   <V74L01>
               MOVE ITDM-STATUZ        TO SYSR-STATUZ                   <V74L01>
               PERFORM XXXX-FATAL-ERROR                                 <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF ((ITDM-ITEMCOY           = ATMD-COMPANY) AND              <V74L01>
               (ITDM-ITEMTABL          = TR52E)        AND              <V74L01>
               (ITDM-ITEMITEM          = WSAA-TR52E-KEY) AND            <V74L01>
               (ITDM-STATUZ            NOT = ENDP))                     <V74L01>
               MOVE ITDM-GENAREA       TO TR52E-TR52E-REC               <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
       233H-EXIT.                                                       <V74L01>
           EXIT.                                                        <V74L01>
                                                                        <V74L01>
                                                                        <V74L01>
       2350-UPDATE-PAYER  SECTION.
      ***************************
       2351-UPDATE-PAYER.

           MOVE PAYR-PAYRSEQNO         TO WSBB-SUB.


      * Look up the tax relief subroutine on T6687.

           MOVE SPACES                 TO ITEM-DATA-KEY.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE CHDRLNB-CHDRCOY        TO ITEM-ITEMCOY.
           MOVE T6687                  TO ITEM-ITEMTABL.
           MOVE T5688-TAXRELMTH        TO ITEM-ITEMITEM.

           CALL 'ITEMIO' USING ITEM-PARAMS.

           IF ITEM-STATUZ  NOT = O-K  AND
                           NOT = MRNF
               MOVE ITEM-STATUZ        TO SYSR-STATUZ
               MOVE ITEM-PARAMS        TO SYSR-PARAMS
               PERFORM XXXX-FATAL-ERROR.

           IF ITEM-STATUZ = O-K
               MOVE ITEM-GENAREA       TO T6687-T6687-REC
           ELSE
               MOVE SPACES             TO T6687-T6687-REC.


           IF T5688-FEEMETH            = SPACES
              MOVE 0                   TO MGFL-MGFEE
              GO TO 2352-ADJUST-PREMIUM.

      * Calculate the contract fee

           MOVE SPACES                 TO ITEM-DATA-AREA.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE CHDRLNB-CHDRCOY        TO ITEM-ITEMCOY.
           MOVE T5674                  TO ITEM-ITEMTABL.
           MOVE T5688-FEEMETH          TO ITEM-ITEMITEM.
           MOVE READR                  TO ITEM-FUNCTION.
           CALL 'ITEMIO' USING         ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = O-K
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              MOVE ITEM-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
           MOVE ITEM-GENAREA           TO T5674-T5674-REC.

      *  Set up linkage area and call subroutine.
           MOVE CHDRLNB-CHDRCOY        TO MGFL-COMPANY.
           MOVE CHDRLNB-CNTTYPE        TO MGFL-CNTTYPE.
           MOVE WSAA-BILLFREQ(1)       TO MGFL-BILLFREQ.
           MOVE CHDRLNB-OCCDATE        TO MGFL-EFFDATE.
           MOVE CHDRLNB-CNTCURR        TO MGFL-CNTCURR.
           CALL T5674-COMMSUBR         USING MGFL-MGFEEL-REC.
      *
           IF MGFL-STATUZ         NOT  = O-K
              MOVE MGFL-MGFEEL-REC     TO SYSR-PARAMS
              MOVE MGFL-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.

           MOVE MGFL-MGFEE             TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO WSAA-MGFEE-REG-AMT.           <V76F06>

      **** MOVE MGFL-MGFEE             TO WSAA-MGFEE-REG-AMT.           <V76F06>
           IF CHDRLNB-BILLCD   = CHDRLNB-OCCDATE
              GO TO 2352-SINGP-FEE.

           MOVE MGFL-MGFEE             TO WSAA-MGFEE.
        2352-SINGP-FEE.

           IF WSAA-SING-PRM-IND = 'N'
              GO TO   2352-ADJUST-PREMIUM.

           MOVE '00'                   TO MGFL-BILLFREQ.
           CALL T5674-COMMSUBR         USING MGFL-MGFEEL-REC.
      *
           IF MGFL-STATUZ         NOT  = O-K
              MOVE MGFL-MGFEEL-REC     TO SYSR-PARAMS
              MOVE MGFL-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.

           MOVE MGFL-MGFEE             TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO WSAA-SINGP-FEE.               <V76F06>
                                                                        <V76F06>
      **** MOVE MGFL-MGFEE             TO WSAA-SINGP-FEE.               <V76F06>
        2352-ADJUST-PREMIUM.

      * Adjust the regular premium

              COMPUTE WSAA-REG-PREM-ADJ(WSBB-SUB) =
                                       WSAA-FREQ-FACTOR(WSBB-SUB) *
                                       WSAA-REG-PREM-ACC(WSBB-SUB)

      * Calculate the amount needed to issue this contract

           COMPUTE WSAA-INSTPREM(WSBB-SUB) =
                                   WSAA-REG-PREM-ADJ(WSBB-SUB) +
                                   WSAA-SING-PREM-ACC(WSBB-SUB).

      * Read the RTRN file to see if a cash receipt has been
      * created for this contract.

           MOVE CHDRLNB-CHDRCOY        TO RTRNSAC-RLDGCOY.
           MOVE CHDRLNB-CHDRNUM        TO RTRNSAC-RLDGACCT.
      **** MOVE CHDRLNB-CNTCURR        TO RTRNSAC-ORIGCCY.              <CAS1.0>
           IF WSAA-SUSP-IND             = 'Y'                           <CAS1.0>
              MOVE ACBLENQ-ORIGCURR    TO RTRNSAC-ORIGCCY               <CAS1.0>
           ELSE                                                         <CAS1.0>
              MOVE SPACE               TO RTRNSAC-ORIGCCY               <CAS1.0>
           END-IF.                                                      <CAS1.0>
           MOVE T5645-SACSCODE-01      TO RTRNSAC-SACSCODE.
           MOVE T5645-SACSTYPE-01      TO RTRNSAC-SACSTYP.
           MOVE READR                  TO RTRNSAC-FUNCTION.

           CALL 'RTRNSACIO'            USING RTRNSAC-PARAMS.

           IF  RTRNSAC-STATUZ      NOT = O-K
           AND                     NOT = MRNF
              MOVE RTRNSAC-PARAMS      TO SYSR-PARAMS
              PERFORM XXXX-FATAL-ERROR.

           IF RTRNSAC-STATUZ           = MRNF
              MOVE VRCM-MAX-DATE       TO RTRNSAC-EFFDATE.

      * If the tax relief method is not spaces calculate the tax
      * relief amount and deduct it from the premium.

           IF T5688-TAXRELMTH NOT = SPACES
               MOVE WSAA-CLNTNUM(WSBB-SUB) TO PRAS-CLNTNUM
               MOVE WSAA-CLNTCOY(WSBB-SUB) TO PRAS-CLNTCOY
               MOVE WSAA-INCOME-SEQ-NO(WSBB-SUB) TO PRAS-INCOME-SEQ-NO
               MOVE CHDRLNB-CNTTYPE        TO PRAS-CNTTYPE
               MOVE T5688-TAXRELMTH        TO PRAS-TAXRELMTH

      * Use the due date unless a receipt exists with a date later
      * then the due date.

               IF RTRNSAC-EFFDATE = VRCM-MAX-DATE
                   MOVE CHDRLNB-OCCDATE       TO PRAS-EFFDATE
               ELSE
                  IF CHDRLNB-OCCDATE       > RTRNSAC-EFFDATE
                     MOVE CHDRLNB-OCCDATE       TO PRAS-EFFDATE
                  ELSE
                     MOVE RTRNSAC-EFFDATE       TO PRAS-EFFDATE
                  END-IF
               END-IF

               MOVE CHDRLNB-CHDRCOY        TO PRAS-COMPANY
               MOVE WSAA-INSTPREM(WSBB-SUB) TO PRAS-GROSSPREM
               MOVE O-K                    TO PRAS-STATUZ

               CALL T6687-TAXRELSUB  USING PRAS-PRASCALC-REC

               IF PRAS-STATUZ  NOT = O-K
                    MOVE PRAS-STATUZ       TO SYSR-STATUZ
                    MOVE T6687-TAXRELSUB   TO SYSR-SUBRNAME
                    PERFORM XXXX-FATAL-ERROR
               END-IF

               MOVE PRAS-TAXRELAMT         TO ZRDP-AMOUNT-IN            <V76F06>
               MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY             <V76F06>
               PERFORM 9000-CALL-ROUNDING                               <V76F06>
               MOVE ZRDP-AMOUNT-OUT        TO PRAS-TAXRELAMT            <V76F06>
                                                                        <V76F06>
               MOVE PRAS-INREVNUM       TO WSAA-INREVNUM(WSBB-SUB)
               SUBTRACT PRAS-TAXRELAMT  FROM WSAA-INSTPREM(WSBB-SUB)
               MOVE PRAS-TAXRELAMT      TO WSAA-TAXRELAMT(WSBB-SUB)
           END-IF.


      * Add the contract fee to the instalment premium for
      * payer No. 1.

           IF WSBB-SUB = 1
                IF WSAA-FEE-FREQ > ZERO
                   COMPUTE WSAA-FEE-ADJ = WSAA-MGFEE * WSAA-FEE-FREQ
                ELSE
                   MOVE WSAA-MGFEE         TO WSAA-FEE-ADJ
                END-IF

              IF WSAA-INSTPREM(WSBB-SUB) NOT = ZERO
                 ADD WSAA-FEE-ADJ  WSAA-SINGP-FEE
                                           TO WSAA-INSTPREM(WSBB-SUB)
              END-IF                                                    <V74L01>
                                                                        <V74L01>
              IF TR52D-TXCODE          NOT = SPACE                      <V74L01>
                 PERFORM 235A-CNTFEE-TAX                                <V74L01>
              END-IF                                                    <V74L01>
                                                                        <V74L01>
           END-IF.



      * THE PAYR-SUSPENSE WILL BE CALCULATED IN PHASE B
      * FOR THE MOMENT INITIALISE PAYER-SUSPENSE TO ZERO.
           MOVE 0 TO WSAA-PAYR-SUSPENSE.


      * Calculate the amount of tolerance needed to issue
      * this contract.
      * The tolerance is the difference between the amount due
      * and the amount in suspense.
      * If the Suspense amount is not in the Contract currency,         <CAS1.0>
      * convert the amount due to the Suspense currency before          <CAS1.0>
      * determining if tolerance is needed. Retain amounts due          <CAS1.0>
      * for GL posting purposes.                                        <CAS1.0>

           MOVE 0                      TO WSAA-TOLR-USED(WSBB-SUB).
      **** MOVE WSAA-INSTPREM(WSBB-SUB)   TO WSAA-AMNT-DUE.             <CAS1.0>
                                                                        <CAS1.0>
           IF WSAA-SUSP-IND             = 'N'                           <CAS1.0>
              MOVE ZERO                        TO WSAA-AMNT-DUE         <CAS1.0>
           ELSE                                                         <CAS1.0>
           IF CHDRLNB-CNTCURR           = ACBLENQ-ORIGCURR              <CAS1.0>
              MOVE WSAA-INSTPREM(WSBB-SUB)     TO WSAA-AMNT-DUE         <CAS1.0>
      **** add the taxes to the amount due                              <V74L01>
              COMPUTE WSAA-AMNT-DUE     = WSAA-AMNT-DUE         +       <V74L01>
                                          WSAA-SP-TAX(WSBB-SUB) +       <V74L01>
                                          WSAA-RP-TAX(WSBB-SUB) +       <V74L01>
                                          WSAA-FE-TAX(WSBB-SUB)         <V74L01>
           ELSE                                                         <CAS1.0>
              ADD  WSAA-INSTPREM(WSBB-SUB)     TO WSAA-INSTPREM-TOT     <CAS1.0>
              ADD  WSAA-SP-TAX(WSBB-SUB) ,                              <V74L01>
                   WSAA-RP-TAX(WSBB-SUB) ,                              <V74L01>
                   WSAA-FE-TAX(WSBB-SUB)       TO WSAA-INSTPREM-TOT     <V74L01>
              MOVE SPACES                      TO CLNK-CLNK002-REC      <CAS1.0>
              MOVE ZEROES                      TO CLNK-AMOUNT-OUT       <CAS1.0>
                                                  CLNK-RATE-USED        <CAS1.0>
              MOVE HPAD-HPRRCVDT               TO CLNK-CASHDATE         <CAS1.0>
              MOVE CHDRLNB-CNTCURR             TO CLNK-CURR-IN          <CAS1.0>
              MOVE ACBLENQ-ORIGCURR            TO CLNK-CURR-OUT         <CAS1.0>
              MOVE WSAA-INSTPREM(WSBB-SUB)     TO CLNK-AMOUNT-IN        <CAS1.0>
      **** add the taxes to the amount due                              <V74L01>
              COMPUTE CLNK-AMOUNT-IN    = CLNK-AMOUNT-IN        +       <V74L01>
                                          WSAA-SP-TAX(WSBB-SUB) +       <V74L01>
                                          WSAA-RP-TAX(WSBB-SUB) +       <V74L01>
                                          WSAA-FE-TAX(WSBB-SUB)         <V74L01>
              MOVE CHDRLNB-CHDRCOY             TO CLNK-COMPANY          <CAS1.0>
              MOVE 'SURR'                      TO CLNK-FUNCTION         <CAS1.0>
                                               OF CLNK-CLNK002-REC      <CAS1.0>
              CALL 'XCVRT'                  USING CLNK-CLNK002-REC      <CAS1.0>
                                                                        <CAS1.0>
              IF CLNK-STATUZ                OF    CLNK-CLNK002-REC      <CAS1.0>
                                            NOT = O-K                   <CAS1.0>
                 MOVE CLNK-CLNK002-REC         TO SYSR-PARAMS           <CAS1.0>
                 MOVE CLNK-STATUZ              OF CLNK-CLNK002-REC      <CAS1.0>
                                               TO SYSR-STATUZ           <CAS1.0>
                 PERFORM XXXX-FATAL-ERROR                               <CAS1.0>
              END-IF                                                    <CAS1.0>
                                                                        <V76F06>
              IF CLNK-AMOUNT-OUT       NOT = 0                          <V76F06>
                  MOVE CLNK-CURR-OUT   TO ZRDP-CURRENCY                 <V76F06>
                  MOVE CLNK-AMOUNT-OUT TO ZRDP-AMOUNT-IN                <V76F06>
                  MOVE ACBLENQ-ORIGCURR                                 <V76F06>
                                       TO ZRDP-CURRENCY                 <V76F06>
                  PERFORM 9000-CALL-ROUNDING                            <V76F06>
                  MOVE ZRDP-AMOUNT-OUT TO CLNK-AMOUNT-OUT               <V76F06>
              END-IF                                                    <V76F06>
                                                                        <CAS1.0>
              ADD  CLNK-AMOUNT-OUT             TO WSAA-INSTPREM-TOT-XGE <CAS1.0>
              MOVE CLNK-AMOUNT-OUT             TO WSAA-AMNT-DUE         <CAS1.0>
           END-IF.                                                      <CAS1.0>
                                                                        <CAS1.0>
           IF WSAA-AMNT-DUE > WSAA-PAYR-SUSPENSE
              SUBTRACT WSAA-PAYR-SUSPENSE FROM WSAA-AMNT-DUE
      *       IF WSAA-AMNT-DUE > WSAA-CNT-SUSPENSE                      <V4L001>
      *          COMPUTE WSAA-TOLR-USED(WSBB-SUB) = WSAA-AMNT-DUE -     <V4L001>
      *                                   WSAA-CNT-SUSPENSE             <V4L001>
      *       END-IF                                                    <V4L001>
      *       SUBTRACT WSAA-TOLR-USED(WSBB-SUB) FROM WSAA-AMNT-DUE      <V4L001>
      *       COMPUTE WSAA-CNT-SUSPENSE = WSAA-CNT-SUSPENSE -           <V4L001>
      *                                   WSAA-AMNT-DUE                 <V4L001>
              IF WSAA-AMNT-DUE             > WSAA-CNT-SUSPENSE          <V4L001>
                 IF WSAA-AMNT-DUE          > WSAA-CNT-SUSPENSE    +     <V4L001>
                                             WSAA-PRMDEPST              <V4L001>
                    COMPUTE WSAA-TOLR-USED(WSBB-SUB)                    <V4L001>
                                           = WSAA-AMNT-DUE        -     <V4L001>
                                             WSAA-CNT-SUSPENSE    -     <V4L001>
                                             WSAA-PRMDEPST              <V4L001>
                    SUBTRACT WSAA-TOLR-USED(WSBB-SUB)                   <V4L001>
                                           FROM WSAA-AMNT-DUE           <V4L001>
                    MOVE ZEROES            TO WSAA-CNT-SUSPENSE         <V4L001>
                    MOVE ZEROES            TO WSAA-PRMDEPST             <V4L001>
                 ELSE                                                   <V4L001>
                    COMPUTE WSAA-PRMDEPST  = WSAA-PRMDEPST        -     <V4L001>
                                            (WSAA-AMNT-DUE        -     <V4L001>
                                             WSAA-CNT-SUSPENSE    )     <V4L001>
                    MOVE ZEROES            TO WSAA-CNT-SUSPENSE         <V4L001>
                 END-IF                                                 <V4L001>
              ELSE                                                      <V4L001>
                 COMPUTE WSAA-CNT-SUSPENSE = WSAA-CNT-SUSPENSE  -       <V4L001>
                                             WSAA-AMNT-DUE              <V4L001>
              END-IF                                                    <V4L001>
                                                                        <V4L001>

           END-IF.

           ADD WSAA-TOLR-USED(WSBB-SUB) TO WSAA-TOT-TOLERANCE.


      * Keep a total of single and regular premiums for this
      * contract.

           ADD WSAA-REG-PREM-ADJ(WSBB-SUB) TO WSAA-TOT-REGP-ADJ.
           ADD WSAA-SING-PREM-ACC(WSBB-SUB) TO WSAA-TOT-SINGP.


         2353-REWRITE-PAYER.


           MOVE READH              TO PAYR-FUNCTION.
           CALL 'PAYRIO'           USING PAYR-PARAMS.

           IF PAYR-STATUZ NOT = O-K AND
                          NOT = ENDP
              MOVE PAYR-STATUZ     TO SYSR-STATUZ
              MOVE PAYR-PARAMS     TO SYSR-PARAMS
              PERFORM XXXX-FATAL-ERROR.

      *  Rewrite the PAYR record.

           IF PAYR-BILLFREQ = '00'
               MOVE WSAA-OLD-CESS-DATE TO PAYR-PTDATE,
                                          PAYR-BTDATE.

           IF PAYR-BILLFREQ NOT = '00'
              MOVE WSAA-REG-PREM-ACC(WSBB-SUB) TO PAYR-SINSTAMT01
              IF PAYR-PAYRSEQNO  = 1
                 MOVE WSAA-MGFEE-REG-AMT  TO PAYR-SINSTAMT02
              END-IF
              ADD PAYR-SINSTAMT01,
                  PAYR-SINSTAMT02  GIVING PAYR-SINSTAMT06
              MOVE  PAYR-BTDATE       TO PAYR-PTDATE.

           IF  PAYR-BILLFREQ           = '00'
      **** OR  T5687-SINGLE-PREM-IND   = 'Y'                            <074>
               MOVE T5679-SET-SNGP-CN-STAT TO PAYR-PSTATCODE
           ELSE
               MOVE T5679-SET-CN-PREM-STAT TO PAYR-PSTATCODE.
                                                                        <072>
           MOVE CHDRLNB-TRANNO         TO PAYR-TRANNO.                  <072>

           MOVE REWRT              TO PAYR-FUNCTION.
           CALL 'PAYRIO'           USING PAYR-PARAMS.

           IF PAYR-STATUZ NOT = O-K AND
                          NOT = ENDP
              MOVE PAYR-STATUZ     TO SYSR-STATUZ
              MOVE PAYR-PARAMS     TO SYSR-PARAMS
              PERFORM XXXX-FATAL-ERROR.

      **Get the next payr record.

           MOVE NEXTR              TO PAYR-FUNCTION.
           CALL 'PAYRIO'           USING PAYR-PARAMS.

           IF PAYR-STATUZ NOT = O-K AND
                          NOT = ENDP
              MOVE PAYR-STATUZ     TO SYSR-STATUZ
              MOVE PAYR-PARAMS     TO SYSR-PARAMS
              PERFORM XXXX-FATAL-ERROR.

      *
       2359-EXIT.
            EXIT.
      *
       235A-CNTFEE-TAX SECTION.                                         <V74L01>
      *************************                                         <V74L01>
       235A-START.                                                      <V74L01>
      * Read table TR52E                                                <V74L01>
           MOVE SPACES                 TO WSAA-TR52E-KEY.               <V74L01>
           MOVE TR52D-TXCODE           TO WSAA-TR52E-TXCODE.            <V74L01>
           MOVE CHDRLNB-CNTTYPE        TO WSAA-TR52E-CNTTYPE.           <V74L01>
           MOVE '****'                 TO WSAA-TR52E-CRTABLE.           <V74L01>
           PERFORM 233H-READ-TR52E.                                     <V74L01>
                                                                        <V74L01>
           IF TR52E-TR52E-REC = SPACES                                  <V74L01>
              MOVE SPACES              TO WSAA-TR52E-KEY                <V74L01>
              MOVE TR52D-TXCODE        TO WSAA-TR52E-TXCODE             <V74L01>
              MOVE '***'               TO WSAA-TR52E-CNTTYPE            <V74L01>
              MOVE '****'              TO WSAA-TR52E-CRTABLE            <V74L01>
              PERFORM 233H-READ-TR52E                                   <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
      * Call TR52D tax subroutine                                       <V74L01>
           IF TR52E-TAXIND-02          NOT = 'Y'                        <V74L01>
              GO TO 235A-EXIT                                           <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           MOVE 'CPST'                 TO TXCL-FUNCTION.                <V74L01>
           MOVE O-K                    TO TXCL-STATUZ.                  <V74L01>
           MOVE CHDRLNB-CHDRCOY        TO TXCL-CHDRCOY.                 <V74L01>
           MOVE CHDRLNB-CHDRNUM        TO TXCL-CHDRNUM.                 <V74L01>
           MOVE SPACES                 TO TXCL-LIFE                     <V74L01>
                                          TXCL-COVERAGE                 <V74L01>
                                          TXCL-RIDER                    <V74L01>
                                          TXCL-CRTABLE.                 <V74L01>
           MOVE 'Y'                    TO TXCL-CNT-TAX-IND.             <V74L01>
           MOVE 0                      TO TXCL-PLAN-SUFFIX.             <V74L01>
           MOVE WSAA-TR52E-KEY         TO TXCL-TAXRULE.                 <V74L01>
           MOVE SPACES                 TO WSAA-RATE-ITEM.               <V74L01>
           MOVE CHDRLNB-CNTCURR        TO TXCL-CCY                      <V74L01>
                                          WSAA-CNT-CURR.                <V74L01>
           MOVE CHDRLNB-TRANNO         TO TXCL-TRANNO                   <V74L01>
           MOVE TR52E-TXITEM           TO WSAA-TXITEM.                  <V74L01>
           MOVE WSAA-RATE-ITEM         TO TXCL-RATE-ITEM.               <V74L01>
           MOVE ZERO                   TO TXCL-TAX-AMT(1)               <V74L01>
                                          TXCL-TAX-AMT(2).              <V74L01>
           MOVE SPACES                 TO TXCL-TAX-ABSORB(1)            <V74L01>
                                          TXCL-TAX-ABSORB(2).           <V74L01>
           MOVE CHDRLNB-OCCDATE        TO TXCL-EFFDATE.                 <V74L01>
           MOVE ZERO                   TO WSAA-AMOUNT-IN.               <V74L01>
           COMPUTE WSAA-AMOUNT-IN ROUNDED                               <V74L01>
                                       =  WSAA-SINGP-FEE +              <V74L01>
                                          WSAA-FEE-ADJ.                 <V74L01>
           MOVE WSAA-AMOUNT-IN         TO TXCL-AMOUNT-IN.               <V74L01>
           MOVE 'CNTF'                 TO TXCL-TRANS-TYPE.              <V74L01>
           MOVE ATMD-BATCH-KEY         TO TXCL-BATCKEY.                 <V74L01>
           MOVE WSAA-JRNSEQ            TO TXCL-JRNSEQ                   <V74L01>
                                                                        <V74L01>
           CALL TR52D-TXSUBR           USING TXCL-LINK-REC.             <V74L01>
           IF TXCL-STATUZ              NOT = O-K                        <V74L01>
              MOVE TXCL-STATUZ         TO SYSR-STATUZ                   <V74L01>
              MOVE TXCL-LINK-REC       TO SYSR-PARAMS                   <V74L01>
              PERFORM XXXX-FATAL-ERROR                                  <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           IF TXCL-TAX-ABSORB(1)     = 'Y'                              <V74L01>
              NEXT SENTENCE                                             <V74L01>
           ELSE                                                         <V74L01>
              ADD TXCL-TAX-AMT(1)      TO WSAA-FE-TAX(WSBB-SUB)         <V74L01>
           END-IF.                                                      <V74L01>
           IF TXCL-TAX-ABSORB(2)     = 'Y'                              <V74L01>
              NEXT SENTENCE                                             <V74L01>
           ELSE                                                         <V74L01>
              ADD TXCL-TAX-AMT(2)      TO WSAA-FE-TAX(WSBB-SUB)         <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           MOVE TXCL-JRNSEQ            TO WSAA-JRNSEQ.                  <V74L01>
                                                                        <V74L01>
       235A-CREATE-TAXD.                                                <V74L01>
           IF TXCL-TAX-AMT(1)           = 0                             <V74L01>
           AND TXCL-TAX-AMT(2)          = 0                             <V74L01>
               GO TO 235A-EXIT                                          <V74L01>
           END-IF.                                                      <V74L01>
                                                                        <V74L01>
           INITIALIZE                     TAXD-DATA-AREA.               <V74L01>
           MOVE CHDRLNB-CHDRCOY        TO TAXD-CHDRCOY.                 <V74L01>
           MOVE CHDRLNB-CHDRNUM        TO TAXD-CHDRNUM.                 <V74L01>
           MOVE TXCL-TRANS-TYPE        TO TAXD-TRANTYPE.                <V74L01>
           MOVE SPACES                 TO TAXD-LIFE                     <V74L01>
                                          TAXD-COVERAGE                 <V74L01>
                                          TAXD-RIDER.                   <V74L01>
           MOVE ZERO                   TO TAXD-PLANSFX.                 <V74L01>
           MOVE TXCL-EFFDATE           TO TAXD-EFFDATE                  <V74L01>
                                          TAXD-INSTFROM                 <V74L01>
                                          TAXD-BILLCD.                  <V74L01>
           MOVE PAYR-BTDATE            TO TAXD-INSTTO.                  <V74L01>
           MOVE CHDRLNB-TRANNO         TO TAXD-TRANNO.                  <V74L01>
           MOVE TXCL-AMOUNT-IN         TO TAXD-BASEAMT.                 <V74L01>
                                                                        <V74L01>
           MOVE SPACES                 TO WSAA-TRANREF.                 <V74L01>
           STRING TXCL-TAXRULE, TXCL-RATE-ITEM                          <V74L01>
                  DELIMITED BY SIZE       INTO WSAA-TRANREF.            <V74L01>
           MOVE WSAA-TRANREF              TO TAXD-TRANREF.              <V74L01>
                                                                        <V74L01>
           MOVE TXCL-TAX-AMT(1)        TO TAXD-TAXAMT01.                <V74L01>
           MOVE TXCL-TAX-AMT(2)        TO TAXD-TAXAMT02.                <V74L01>
           MOVE ZERO                   TO TAXD-TAXAMT03.                <V74L01>
           MOVE TXCL-TAX-ABSORB(1)     TO TAXD-TXABSIND01.              <V74L01>
           MOVE TXCL-TAX-ABSORB(2)     TO TAXD-TXABSIND02.              <V74L01>
           MOVE SPACES                 TO TAXD-TXABSIND03.              <V74L01>
           MOVE TXCL-TAX-TYPE(1)       TO TAXD-TXTYPE01.                <V74L01>
           MOVE TXCL-TAX-TYPE(2)       TO TAXD-TXTYPE02.                <V74L01>
           MOVE SPACES                 TO TAXD-TXTYPE03.                <V74L01>
           MOVE 'P'                    TO TAXD-POSTFLG.                 <V74L01>
                                                                        <V74L01>
           MOVE TAXDREC                TO TAXD-FORMAT.                  <V74L01>
           MOVE WRITR                  TO TAXD-FUNCTION.                <V74L01>
           CALL 'TAXDIO'               USING TAXD-PARAMS.               <V74L01>
                                                                        <V74L01>
           IF TAXD-STATUZ              NOT = O-K                        <V74L01>
              MOVE TAXD-PARAMS         TO SYSR-PARAMS                   <V74L01>
              MOVE TAXD-STATUZ         TO SYSR-STATUZ                   <V74L01>
              PERFORM XXXX-FATAL-ERROR                                  <V74L01>
           END-IF.                                                      <V74L01>
      *                                                                 <V74L01>
       235A-EXIT.                                                       <V74L01>
            EXIT.                                                       <V74L01>
                                                                        <V74L01>
       2360-GET-TOLERANCE SECTION.                                      <V42013>
      ***************************                                       <V42013>
       2361-READ-T5667.                                                 <V42013>
                                                                        <V42013>
           MOVE SPACES                 TO ITEM-DATA-KEY.                <V42013>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <V42013>
           MOVE ATMD-COMPANY           TO ITEM-ITEMCOY.                 <V42013>
           MOVE T5667                  TO ITEM-ITEMTABL.                <V42013>
           MOVE WSKY-BATC-BATCTRCDE    TO WSAA-T5667-TRANCD.            <V42013>
           IF WSAA-SUSP-IND             = 'Y'                           <V42013>
              MOVE ACBLENQ-ORIGCURR    TO WSAA-T5667-CURR               <V42013>
           ELSE                                                         <V42013>
              MOVE SPACE               TO WSAA-T5667-CURR               <V42013>
           END-IF.                                                      <V42013>
           MOVE WSAA-T5667-KEY         TO ITEM-ITEMITEM.                <V42013>
           MOVE 'READR'                TO ITEM-FUNCTION.                <V42013>
                                                                        <V42013>
           CALL 'ITEMIO'            USING ITEM-FUNCTION.                <V42013>
           IF ITEM-STATUZ           NOT = O-K AND                       <V42013>
              ITEM-STATUZ           NOT = MRNF                          <V42013>
              MOVE ITEM-PARAMS         TO SYSR-PARAMS                   <V42013>
              MOVE ITEM-STATUZ         TO SYSR-STATUZ                   <V42013>
              PERFORM XXXX-FATAL-ERROR.                                 <V42013>
                                                                        <V42013>
           IF ITEM-STATUZ               = MRNF                          <V42013>
              MOVE ZERO                TO WSAA-TOLERANCE-APP            <V42013>
                                          WSAA-TOLERANCE-APP2           <V42013>
                                          WSAA-AMOUNT-LIMIT             <V42013>
                                          WSAA-AMOUNT-LIMIT2            <V42013>
           ELSE                                                         <V42013>
              MOVE ITEM-GENAREA        TO T5667-T5667-REC               <V42013>
                                                                        <V42013>
              MOVE 1                   TO WSAA-SUB1                     <V42013>
              PERFORM UNTIL WSAA-SUB1   > 11                            <V42013>
                 IF T5667-FREQ(WSAA-SUB1) = PAYR-BILLFREQ               <V42013>
                    MOVE T5667-PRMTOL(WSAA-SUB1)                        <V42013>
                                       TO WSAA-TOLERANCE-APP            <V42013>
                    MOVE T5667-PRMTOLN(WSAA-SUB1)                       <V42013>
                                       TO WSAA-TOLERANCE-APP2           <V42013>
                    MOVE T5667-MAX-AMOUNT(WSAA-SUB1)                    <V42013>
                                       TO WSAA-AMOUNT-LIMIT             <V42013>
                    MOVE T5667-MAXAMT(WSAA-SUB1)                        <V42013>
                                       TO WSAA-AMOUNT-LIMIT2            <V42013>
                    MOVE 11            TO WSAA-SUB1                     <V42013>
                 END-IF                                                 <V42013>
                 ADD 1                 TO WSAA-SUB1                     <V42013>
              END-PERFORM                                               <V42013>
           END-IF.                                                      <V42013>
                                                                        <V42013>
           COMPUTE WSAA-TOLERANCE                                       <V42013>
                 = (WSAA-AMNT-DUE * WSAA-TOLERANCE-APP) / 100.          <V42013>
                                                                        <V42013>
           IF WSAA-TOLERANCE            > WSAA-AMOUNT-LIMIT             <V42013>
              MOVE WSAA-AMOUNT-LIMIT   TO WSAA-TOLERANCE.               <V42013>
                                                                        <V42013>
           COMPUTE WSAA-TOLERANCE2                                      <V42013>
                 = (WSAA-AMNT-DUE * WSAA-TOLERANCE-APP2) / 100.         <V42013>
                                                                        <V42013>
           IF WSAA-TOLERANCE2           > WSAA-AMOUNT-LIMIT2            <V42013>
              MOVE WSAA-AMOUNT-LIMIT2  TO WSAA-TOLERANCE2.              <V42013>
                                                                        <V42013>
       2369-EXIT.                                                       <V42013>
            EXIT.                                                       <V42013>
      /
      *
       2400-CONTRACT-HEADER-REV SECTION.
      **********************************
      *2410-CONTRACT-HEADER-REV.                                        <073>
      *

       2492-UPDATE-HEADER.
           MOVE '1'                    TO CHDRLNB-VALIDFLAG.
           MOVE CHDRLNB-OCCDATE        TO CHDRLNB-CCDATE.
           MOVE T5679-SET-CN-RISK-STAT TO CHDRLNB-STATCODE.
           MOVE DTC1-INT-DATE          TO CHDRLNB-STATDATE.
           MOVE CHDRLNB-TRANNO         TO CHDRLNB-STATTRAN.
           IF  CHDRLNB-BILLFREQ        = '00'
      **** OR  T5687-SINGLE-PREM-IND   = 'Y'                            <074>
               MOVE T5679-SET-SNGP-CN-STAT TO CHDRLNB-PSTATCODE
           ELSE
               MOVE T5679-SET-CN-PREM-STAT TO CHDRLNB-PSTATCODE.
           MOVE DTC1-INT-DATE          TO CHDRLNB-PSTATDATE.
           MOVE CHDRLNB-TRANNO         TO CHDRLNB-PSTATTRAN.
           MOVE CHDRLNB-TRANNO         TO CHDRLNB-TRANLUSED.
      *
      * Should update instalment totals for single premium or regular
      * premium ot both.
      * Total fee = fee * no. of instalment paid.
      * Tolerance = amount paid - premium due - fee due.
      *


           IF  WSAA-TOT-SINGP          NOT = ZERO OR
               WSAA-TOT-REGP-ADJ       NOT = ZERO
               MOVE CHDRLNB-OCCDATE    TO CHDRLNB-INSTFROM
               COMPUTE CHDRLNB-INSTTOT01 = (WSAA-TOT-SINGP +
                                           WSAA-TOT-REGP-ADJ)
                                           - WSAA-TOT-TOLERANCE.

               ADD  WSAA-FEE-ADJ  WSAA-SINGP-FEE GIVING
                                                   CHDRLNB-INSTTOT02

               MOVE WSAA-TOT-TOLERANCE TO CHDRLNB-INSTTOT03
               MOVE 0                  TO CHDRLNB-INSTTOT04
                                          CHDRLNB-INSTTOT05
               ADD CHDRLNB-INSTTOT01,
                   CHDRLNB-INSTTOT02,
                   CHDRLNB-INSTTOT03      GIVING CHDRLNB-INSTTOT06.

           IF CHDRLNB-BILLFREQ            = '00'
              MOVE WSAA-OLD-CESS-DATE     TO CHDRLNB-PTDATE
                                             CHDRLNB-BTDATE
           ELSE
              MOVE CHDRLNB-BILLCD         TO CHDRLNB-SINSTFROM
              MOVE VRCM-MAX-DATE          TO CHDRLNB-SINSTTO
              MOVE WSAA-REG-PREM-ACC(1)   TO CHDRLNB-SINSTAMT01
              MOVE WSAA-MGFEE-REG-AMT     TO CHDRLNB-SINSTAMT02
              MOVE 0                      TO CHDRLNB-SINSTAMT03
                                             CHDRLNB-SINSTAMT04
                                             CHDRLNB-SINSTAMT05
                                             CHDRLNB-SINSTAMT06
              ADD CHDRLNB-SINSTAMT01
                  CHDRLNB-SINSTAMT02      TO CHDRLNB-SINSTAMT06.
      *
      * Tolerance should not be applied to every instalment.
      *
      *

           MOVE KEEPS                  TO CHDRLNB-FUNCTION.
           MOVE CHDRLNBREC             TO CHDRLNB-FORMAT.
           CALL 'CHDRLNBIO'            USING CHDRLNB-PARAMS.
           IF CHDRLNB-STATUZ           NOT = O-K
              MOVE CHDRLNB-PARAMS      TO SYSR-PARAMS
              MOVE CHDRLNB-STATUZ      TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.


           MOVE WRITS                  TO CHDRLNB-FUNCTION.
           MOVE CHDRLNBREC             TO CHDRLNB-FORMAT.
           CALL 'CHDRLNBIO'            USING CHDRLNB-PARAMS.
           IF CHDRLNB-STATUZ           NOT = O-K
              MOVE CHDRLNB-PARAMS      TO SYSR-PARAMS
              MOVE CHDRLNB-STATUZ      TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
      *                                                                 <V74L03>
      * Get the policy dispatch details                                 <V74L03>
      *                                                                 <V74L03>
           PERFORM 2900-READ-TR52Q.                                     <V74L03>
      *                                                                 <CAS1.0>
      * Update the HPAD record to show contract now issued              <CAS1.0>
      *                                                                 <CAS1.0>
           MOVE READH                  TO HPAD-FUNCTION.                <CAS1.0>
      *                                                                 <CAS1.0>
           CALL 'HPADIO'  USING        HPAD-PARAMS.                     <CAS1.0>
                                                                        <CAS1.0>
           IF HPAD-STATUZ              NOT = O-K                        <CAS1.0>
              MOVE HPAD-PARAMS         TO SYSR-PARAMS                   <CAS1.0>
              MOVE HPAD-STATUZ         TO SYSR-STATUZ                   <CAS1.0>
              PERFORM XXXX-FATAL-ERROR                                  <CAS1.0>
           END-IF.                                                      <CAS1.0>
                                                                        <CAS1.0>
           MOVE DTC1-INT-DATE          TO HPAD-HISSDTE.                 <CAS1.0>
           IF HPAD-HOISSDTE            = VRCM-MAX-DATE                  <CAS1.0>
              MOVE DTC1-INT-DATE       TO HPAD-HOISSDTE.                <CAS1.0>
           MOVE WSAA-ZSUFCDTE          TO HPAD-ZSUFCDTE.                <LA4596>
           IF HPAD-ZSUFCDTE            = VRCM-MAX-DATE                  <LA4238>
              MOVE 'Y'                 TO HPAD-PROCFLG.                 <LA4238>
           MOVE '1'                    TO HPAD-VALIDFLAG.               <CAS1.0>
      **** MOVE WSAA-ZSUFCDTE          TO HPAD-ZSUFCDTE.        <LA4596><V71L05>
           PERFORM 2950-UPD-DESPATCH-DETAILS.                           <V74L03>
           MOVE REWRT                  TO HPAD-FUNCTION.                <CAS1.0>
      *                                                                 <CAS1.0>
           CALL 'HPADIO'            USING HPAD-PARAMS.                  <CAS1.0>
                                                                        <CAS1.0>
           IF HPAD-STATUZ           NOT = O-K                           <CAS1.0>
              MOVE HPAD-PARAMS         TO SYSR-PARAMS                   <CAS1.0>
              MOVE HPAD-STATUZ         TO SYSR-STATUZ                   <CAS1.0>
              PERFORM XXXX-FATAL-ERROR                                  <CAS1.0>
           END-IF.                                                      <CAS1.0>
      *
       2490-EXIT.
           EXIT.
      *
      /
      *
       2500-CONTRACT-ACCOUNTING SECTION.
      **********************************
      *2510-CONTRACT-ACCOUNTING.                                        <073>
      *

       2520-NON-CASH-POSTINGS.

           MOVE 'PSTW'                 TO LIFA-FUNCTION.
           MOVE ATMD-BATCH-KEY         TO LIFA-BATCKEY.
           MOVE CHDRLNB-CHDRNUM        TO LIFA-RDOCNUM
                                          LIFA-RLDGACCT.
           MOVE CHDRLNB-TRANNO         TO LIFA-TRANNO.
           MOVE ZERO                   TO LIFA-JRNSEQ.
           MOVE CHDRLNB-CHDRCOY        TO LIFA-RLDGCOY.
           MOVE CHDRLNB-CNTCURR        TO LIFA-ORIGCURR.
           MOVE COVRLNB-CHDRNUM        TO LIFA-TRANREF
           MOVE DESC-LONGDESC          TO LIFA-TRANDESC.
           MOVE 0                      TO LIFA-CRATE,
                                          LIFA-ACCTAMT,
                                          LIFA-RCAMT,
                                          LIFA-CONTOT,
                                          LIFA-RCAMT,
                                          LIFA-FRCDATE.
           MOVE WSAA-TRANSACTION-DATE  TO LIFA-TRANSACTION-DATE.
           MOVE WSAA-TRANSACTION-TIME  TO LIFA-TRANSACTION-TIME.
           MOVE WSAA-USER              TO LIFA-USER.
           MOVE WSAA-TERMID            TO LIFA-TERMID.
           MOVE WSAA-TOTAMNT           TO LIFA-ORIGAMT.
           MOVE SPACES                 TO LIFA-GENLCUR.
           MOVE CHDRLNB-CHDRCOY        TO LIFA-GENLCOY.
           MOVE SPACES                 TO LIFA-POSTYEAR,
                                          LIFA-POSTMONTH.
           MOVE CHDRLNB-OCCDATE        TO LIFA-EFFDATE.
           MOVE VRCM-MAX-DATE          TO LIFA-FRCDATE.
           MOVE CHDRLNB-CNTTYPE        TO LIFA-SUBSTITUTE-CODE(01).



           IF CHDRLNB-INSTTOT02        NOT = 0
              MOVE T5645-SACSCODE-04   TO LIFA-SACSCODE
              MOVE T5645-SACSTYPE-04   TO LIFA-SACSTYP
              MOVE T5645-GLMAP-04      TO LIFA-GLCODE
              MOVE T5645-SIGN-04       TO LIFA-GLSIGN
              MOVE T5645-CNTTOT-04     TO LIFA-CONTOT
      *
      * Can not guarantee the first payment is a full instalment.
      *
              MOVE CHDRLNB-INSTTOT02   to LIFA-ORIGAMT
              CALL 'LIFACMV'           USING LIFA-LIFACMV-REC
              PERFORM 2950-LIFACMV-CHECK.



      * Post the tolerance at contract level
      * Post using the Suspense currency, which may differ from         <CAS1.0>
      * the Contract currency.                                          <CAS1.0>

           MOVE WSAA-TOT-TOLERANCE     TO LIFA-ORIGAMT.

           IF LIFA-ORIGAMT             NOT = 0
              MOVE ACBLENQ-ORIGCURR    TO LIFA-ORIGCURR                 <CAS1.0>

              IF LIFA-ORIGAMT          <= WSAA-TOLERANCE                <V42013>
                 MOVE T5645-SACSCODE-05   TO LIFA-SACSCODE
                 MOVE T5645-SACSTYPE-05   TO LIFA-SACSTYP
                 MOVE T5645-GLMAP-05      TO LIFA-GLCODE
                 MOVE T5645-SIGN-05       TO LIFA-GLSIGN
                 MOVE T5645-CNTTOT-05     TO LIFA-CONTOT
              ELSE                                                      <V42013>
                 MOVE WSAA-T5645-SACSCODE(14)                           <V42013>
                                          TO LIFA-SACSCODE              <V42013>
                 MOVE WSAA-T5645-SACSTYPE(14)                           <V42013>
                                          TO LIFA-SACSTYP               <V42013>
                 MOVE WSAA-T5645-GLMAP(14)                              <V42013>
                                          TO LIFA-GLCODE                <V42013>
                 MOVE WSAA-T5645-SIGN(14) TO LIFA-GLSIGN                <V42013>
                 MOVE WSAA-T5645-CNTTOT(14)                             <V42013>
                                          TO LIFA-CONTOT                <V42013>
                 MOVE CHDRLNB-AGNTNUM     TO LIFA-RLDGACCT              <V42013>
              END-IF                                                    <V42013>

              CALL 'LIFACMV'           USING LIFA-LIFACMV-REC
              MOVE CHDRLNB-CNTCURR     TO LIFA-ORIGCURR                 <CAS1.0>
              MOVE CHDRLNB-CHDRNUM     TO LIFA-RLDGACCT                 <V42013>
      *       PERFORM 2950-LIFACMV-CHECK.                               <V73L01>
              PERFORM 2950-LIFACMV-CHECK                                <V73L01>
      *                                                                 <V73L01>
      * Override Commission                                             <V73L01>
      *                                                                 <V73L01>
              IF TH605-INDIC           = 'Y'                            <V73L01>
                 MOVE LIFA-ORIGAMT     TO ZORL-ANNPREM                  <V73L01>
                 PERFORM B100-CALL-ZORCOMPY                             <V73L01>
              END-IF                                                    <V73L01>
           END-IF.                                                      <V73L01>

           IF WSAA-STAMP-DUTY-ACC      NOT = 0
              MOVE WSAA-STAMP-DUTY-ACC TO LIFA-ORIGAMT
              MOVE T5645-SACSCODE-06   TO LIFA-SACSCODE
              MOVE T5645-SACSTYPE-06   TO LIFA-SACSTYP
              MOVE T5645-GLMAP-06      TO LIFA-GLCODE
              MOVE T5645-SIGN-06       TO LIFA-GLSIGN
              MOVE T5645-CNTTOT-06     TO LIFA-CONTOT
              CALL 'LIFACMV'           USING LIFA-LIFACMV-REC
              PERFORM 2950-LIFACMV-CHECK
              MOVE T5645-SACSCODE-07   TO LIFA-SACSCODE
              MOVE T5645-SACSTYPE-07   TO LIFA-SACSTYP
              MOVE T5645-GLMAP-07      TO LIFA-GLCODE
              MOVE T5645-SIGN-07       TO LIFA-GLSIGN
              MOVE T5645-CNTTOT-07     TO LIFA-CONTOT
              CALL 'LIFACMV'           USING LIFA-LIFACMV-REC
              PERFORM 2950-LIFACMV-CHECK.
      *                                                                 <CAS1.0>
      * If the Contract currency differs from the Suspense currency     <CAS1.0>
      * post the amount due in both the contract & suspense currencies. <CAS1.0>
      * But only if an initial premium is required, ie..                <CAS1.0>
      * BTDATE > CCDATE.                                                <CAS1.0>
      *                                                                 <CAS1.0>
           IF (CHDRLNB-CNTCURR           NOT = ACBLENQ-ORIGCURR)  AND   <CAS1.0>
              (WSAA-INSTPREM-TOT-XGE          > ZERO           )        <CAS1.0>
              MOVE ACBLENQ-ORIGCURR         TO LIFA-ORIGCURR            <CAS1.0>
              MOVE WSAA-INSTPREM-TOT-XGE    TO LIFA-ORIGAMT             <CAS1.0>
              MOVE WSAA-T5645-CNTTOT  (12)  TO LIFA-CONTOT              <CAS1.0>
              MOVE WSAA-T5645-GLMAP   (12)  TO LIFA-GLCODE              <CAS1.0>
              MOVE WSAA-T5645-SACSCODE(12)  TO LIFA-SACSCODE            <CAS1.0>
              MOVE WSAA-T5645-SACSTYPE(12)  TO LIFA-SACSTYP             <CAS1.0>
              MOVE WSAA-T5645-SIGN    (12)  TO LIFA-GLSIGN              <CAS1.0>
                                                                        <CAS1.0>
              CALL 'LIFACMV'             USING LIFA-LIFACMV-REC         <CAS1.0>
              PERFORM 2950-LIFACMV-CHECK                                <CAS1.0>
                                                                        <CAS1.0>
              MOVE CHDRLNB-CNTCURR          TO LIFA-ORIGCURR            <CAS1.0>
              MOVE WSAA-INSTPREM-TOT        TO LIFA-ORIGAMT             <CAS1.0>
              MOVE WSAA-T5645-CNTTOT  (13)  TO LIFA-CONTOT              <CAS1.0>
              MOVE WSAA-T5645-GLMAP   (13)  TO LIFA-GLCODE              <CAS1.0>
              MOVE WSAA-T5645-SACSCODE(13)  TO LIFA-SACSCODE            <CAS1.0>
              MOVE WSAA-T5645-SACSTYPE(13)  TO LIFA-SACSTYP             <CAS1.0>
              MOVE WSAA-T5645-SIGN    (13)  TO LIFA-GLSIGN              <CAS1.0>
                                                                        <CAS1.0>
              CALL 'LIFACMV'             USING LIFA-LIFACMV-REC         <CAS1.0>
              PERFORM 2950-LIFACMV-CHECK                                <CAS1.0>
           END-IF.                                                      <CAS1.0>

      *                                                         <V4L001><V4L001>
      * This paragraph is required before the PAYER-ACCOUTING so that   <V4L001>
      * suspense will have enough money for the amount due. If there    <V4L001>
      * has extract suspense for the amount due then NEXT SENTENCE. If  <V4L001>
      * Adv Prem Deposit amount (APA) is required to pay the amount due <V4L001>
      * then move DELT to withdraw money from APA and put into suspense.<V4L001>
      * Otherwise, take the extra money from suspense and try to post it<V4L001>
      * into APA.                                               <V4L001><V4L001>
      *                                                         <V4L001><V4L001>
           INITIALIZE                       RLPDLON-REC.                <V4L001>
           IF WSAA-TOTAMNT1                 = ZEROES                    <V4L001>
              NEXT SENTENCE                                             <V4L001>
           ELSE                                                         <V4L001>
              IF WSAA-TOTAMNT1              > ZEROES                    <V4L001>
                 MOVE DELT                  TO RLPDLON-FUNCTION         <V4L001>
                 MOVE WSAA-TOTAMNT1         TO RLPDLON-PRMDEPST         <V4L001>
              ELSE                                                      <V4L001>
                 MOVE INSR                  TO RLPDLON-FUNCTION         <V4L001>
                 COMPUTE WSAA-TOTAMNT1      = WSAA-TOTAMNT1 * (-1)      <V4L001>
                 MOVE WSAA-TOTAMNT1         TO RLPDLON-PRMDEPST         <V4L001>
              END-IF                                                    <V4L001>
                                                                        <V4L001>
              PERFORM R200-UPDATE-APA                                   <V4L001>
           END-IF.                                                      <V4L001>
      *                                                                 <V4L001>
      *
       2540-EXIT.                                                       <V4L001>
           EXIT.
      *
       2550-PAYER-ACCOUNTING SECTION.
      **********************************
       2551-PAYER-ACCOUNTING.


      * Deduct the amount needed to issue this contract,
      * less the tolerance applied from the suspense A/C.

           MOVE ZEROS                  TO LIFR-ORIGAMT.                 <D9604>
                                                                        <D9604>
           IF  CHDRLNB-OCCDATE          =  WSAA-BTDATE(WSBB-SUB)
           AND WSAA-BILLFREQ(WSBB-SUB)  NOT =  '00'
           AND WSAA-SING-PREM-ACC(WSBB-SUB) = ZERO
              GO TO 2559-INCREMENT-SUB.

           MOVE 'PSTW'                 TO LIFR-FUNCTION.
           MOVE ATMD-BATCH-KEY         TO LIFR-BATCKEY.
           MOVE CHDRLNB-CHDRNUM        TO LIFR-RDOCNUM,
                                          LIFR-RLDGACCT.

           MOVE CHDRLNB-TRANNO         TO LIFR-TRANNO.
           MOVE ZERO                   TO LIFR-JRNSEQ.
           MOVE CHDRLNB-CHDRCOY        TO LIFR-RLDGCOY.
           MOVE T5645-SACSCODE-01      TO LIFR-SACSCODE.
      **** MOVE CHDRLNB-CNTCURR        TO LIFR-ORIGCURR.                <CAS1.0>
           MOVE ACBLENQ-ORIGCURR       TO LIFR-ORIGCURR.                <CAS1.0>
           MOVE T5645-SACSTYPE-01      TO LIFR-SACSTYP.
           MOVE COVRLNB-CHDRNUM        TO LIFA-TRANREF
           MOVE DESC-LONGDESC          TO LIFR-TRANDESC.
           MOVE 0                      TO LIFR-CRATE,
                                          LIFR-ACCTAMT,
                                          LIFR-RCAMT.
           MOVE WSAA-USER              TO LIFR-USER.
           MOVE WSAA-TERMID            TO LIFR-TERMID.
      **** COMPUTE LIFR-ORIGAMT        =  WSAA-INSTPREM(WSBB-SUB) -     <CAS1.0>
      ****                                WSAA-TOLR-USED(WSBB-SUB).     <CAS1.0>
           IF CHDRLNB-CNTCURR          =  ACBLENQ-ORIGCURR              <CAS1.0>
              COMPUTE LIFR-ORIGAMT     =  WSAA-INSTPREM(WSBB-SUB) -     <CAS1.0>
      ****                                WSAA-TOLR-USED(WSBB-SUB)      <V76F06>
                                          WSAA-TOLR-USED(WSBB-SUB) +    <V74L01>
                                          WSAA-RP-TAX(WSBB-SUB) +       <V74L01>
                                          WSAA-SP-TAX(WSBB-SUB) +       <V74L01>
                                          WSAA-FE-TAX(WSBB-SUB)         <V74L01>
           ELSE                                                         <CAS1.0>
              COMPUTE LIFR-ORIGAMT     =  WSAA-INSTPREM-TOT-XGE   -     <CAS1.0>
                                          WSAA-TOT-TOLERANCE            <CAS1.0>
           END-IF.                                                      <CAS1.0>
           MOVE SPACES                 TO LIFR-GENLCUR.
           MOVE CHDRLNB-CHDRCOY        TO LIFR-GENLCOY.
           MOVE T5645-GLMAP-01         TO LIFR-GLCODE.
           MOVE T5645-SIGN-01          TO LIFR-GLSIGN.
           MOVE T5645-CNTTOT-01        TO LIFR-CONTOT.
           MOVE SPACES                 TO LIFR-POSTYEAR,
                                          LIFR-POSTMONTH.
           MOVE CHDRLNB-OCCDATE        TO LIFR-EFFDATE.
           MOVE VRCM-MAX-DATE          TO LIFR-FRCDATE.
           MOVE CHDRLNB-CNTTYPE        TO LIFR-SUBSTITUTE-CODE(01).

           IF LIFR-ORIGAMT             = 0
              GO TO 2555-NON-CASH-POSTINGS.
           CALL 'LIFRTRN'              USING LIFR-LIFRTRN-REC.

           IF   LIFR-STATUZ         NOT = O-K
                MOVE LIFR-LIFRTRN-REC       TO SYSR-PARAMS
                MOVE LIFR-STATUZ            TO SYSR-STATUZ
                PERFORM XXXX-FATAL-ERROR.

       2555-NON-CASH-POSTINGS.

           IF COMP-LEVEL-ACC
               GO TO 2559-INCREMENT-SUB.

      * Post the single premium.

           MOVE 'PSTW'                 TO LIFA-FUNCTION.
           MOVE ATMD-BATCH-KEY         TO LIFA-BATCKEY.
           MOVE CHDRLNB-CHDRNUM        TO LIFA-RDOCNUM
                                          LIFA-RLDGACCT.
           MOVE CHDRLNB-TRANNO         TO LIFA-TRANNO.
           MOVE ZERO                   TO LIFA-JRNSEQ.
           MOVE CHDRLNB-CHDRCOY        TO LIFA-RLDGCOY.
           MOVE CHDRLNB-CNTCURR        TO LIFA-ORIGCURR.
           MOVE COVRLNB-CHDRNUM        TO LIFA-TRANREF.
           MOVE DESC-LONGDESC          TO LIFA-TRANDESC.
           MOVE 0                      TO LIFA-CRATE,
                                          LIFA-ACCTAMT,
                                          LIFA-RCAMT,
                                          LIFA-CONTOT,
                                          LIFA-RCAMT,
                                          LIFA-FRCDATE.
           MOVE WSAA-TRANSACTION-DATE  TO LIFA-TRANSACTION-DATE.
           MOVE WSAA-TRANSACTION-TIME  TO LIFA-TRANSACTION-TIME.
           MOVE WSAA-USER              TO LIFA-USER.
           MOVE WSAA-TERMID            TO LIFA-TERMID.
           MOVE SPACES                 TO LIFA-GENLCUR.
           MOVE CHDRLNB-CHDRCOY        TO LIFA-GENLCOY.
           MOVE SPACES                 TO LIFA-POSTYEAR,
                                          LIFA-POSTMONTH.
           MOVE CHDRLNB-OCCDATE        TO LIFA-EFFDATE.
           MOVE VRCM-MAX-DATE          TO LIFA-FRCDATE.
           MOVE CHDRLNB-CNTTYPE        TO LIFA-SUBSTITUTE-CODE(01).

           IF WSAA-SING-PREM-ACC(WSBB-SUB)  NOT = 0
              MOVE T5645-SACSCODE-02   TO LIFA-SACSCODE
              MOVE T5645-SACSTYPE-02   TO LIFA-SACSTYP
              MOVE T5645-GLMAP-02      TO LIFA-GLCODE
              MOVE T5645-SIGN-02       TO LIFA-GLSIGN
              MOVE T5645-CNTTOT-02     TO LIFA-CONTOT
              MOVE WSAA-SING-PREM-ACC(WSBB-SUB) TO LIFA-ORIGAMT

              CALL 'LIFACMV'           USING LIFA-LIFACMV-REC
              PERFORM 2950-LIFACMV-CHECK.

      * Post regular premium.

           IF WSAA-REG-PREM-ADJ(WSBB-SUB)  NOT = 0
              MOVE T5645-SACSCODE-03   TO LIFA-SACSCODE
              MOVE T5645-SACSTYPE-03   TO LIFA-SACSTYP
              MOVE T5645-GLMAP-03      TO LIFA-GLCODE
              MOVE T5645-SIGN-03       TO LIFA-GLSIGN
              MOVE T5645-CNTTOT-03     TO LIFA-CONTOT
              MOVE WSAA-REG-PREM-ADJ(WSBB-SUB)  TO LIFA-ORIGAMT
              CALL 'LIFACMV'           USING LIFA-LIFACMV-REC
              PERFORM 2950-LIFACMV-CHECK.

      * Post the tolerance needed to issue this contract.


      * Post the tax relief amount

       2559-INCREMENT-SUB.
      *
      ****    MOVE WSAA-INREVNUM(WSBB-SUB)    TO LIFA-RLDGACCT.         <A06541>
              MOVE CHDRLNB-CHDRNUM            TO LIFA-RLDGACCT.         <A06541>
              MOVE WSAA-TAXRELAMT(WSBB-SUB)   TO LIFA-ORIGAMT.

           IF LIFA-ORIGAMT             NOT = 0
              MOVE WSAA-T5645-SACSCODE (03) TO LIFA-SACSCODE
              MOVE WSAA-T5645-SACSTYPE (03) TO LIFA-SACSTYP
              MOVE WSAA-T5645-GLMAP (03)    TO LIFA-GLCODE
              MOVE WSAA-T5645-SIGN (03)     TO LIFA-GLSIGN
              MOVE WSAA-T5645-CNTTOT (03)   TO LIFA-CONTOT
              CALL 'LIFACMV'           USING LIFA-LIFACMV-REC
              PERFORM 2950-LIFACMV-CHECK.

      * If this is a flexible premium contract then write       <D9604>
      * an FPRM record at payer level                           <D9604>
                                                                        <D9604>
              IF FLEXIBLE-PREMIUM-CONTRACT                              <D9604>
                 PERFORM 6500-WRITE-FPRM-REC                            <D9604>
              END-IF.                                                   <D9604>
                                                                        <D9604>
              ADD 1 TO WSBB-SUB.

        2559-EXIT.
            EXIT.
      /
      *
       2600-COMMISSION-ACCOUNTING SECTION.
      *********************************
       2610-COMMISSION-ACCOUNTING.
      *
           MOVE 'PSTW'                 TO LIFA-FUNCTION.
           MOVE ATMD-BATCH-KEY         TO LIFA-BATCKEY.
           MOVE WSAA-TRANSACTION-TIME  TO LIFA-TRANSACTION-TIME.
           MOVE WSAA-TRANSACTION-DATE  TO LIFA-TRANSACTION-DATE.
           MOVE WSAA-TERMID            TO LIFA-TERMID.
           MOVE WSAA-USER              TO LIFA-USER.
           MOVE CHDRLNB-CHDRNUM        TO LIFA-RDOCNUM,
           MOVE CHDRLNB-CHDRCOY        TO LIFA-RLDGCOY.
           MOVE CHDRLNB-CNTCURR        TO LIFA-ORIGCURR.
           MOVE CHDRLNB-TRANNO         TO LIFA-TRANNO.
           MOVE DESC-LONGDESC          TO LIFA-TRANDESC.
           MOVE 0                      TO LIFA-CRATE,
                                          LIFA-ACCTAMT,
                                          LIFA-RCAMT.
           MOVE WSAA-TOTAMNT           TO LIFA-ORIGAMT.
           MOVE SPACES                 TO LIFA-GENLCUR.
           MOVE CHDRLNB-CHDRCOY        TO LIFA-GENLCOY.
           MOVE SPACES                 TO LIFA-POSTYEAR,
                                          LIFA-POSTMONTH.
           MOVE CHDRLNB-OCCDATE        TO LIFA-EFFDATE.
           MOVE VRCM-MAX-DATE          TO LIFA-FRCDATE.
           MOVE CHDRLNB-CNTTYPE        TO LIFA-SUBSTITUTE-CODE(01).
           MOVE +0                     TO WSAA-JRNSEQ.

       2630-DO-POSTINGS.
      *    IF WSAA-COMM-DUE(WSAA-AGENT-SUB) NOT = 0                     <V4L014>
           IF NOT COMP-LEVEL-ACC          AND                           <V4L014>
              WSAA-COMM-DUE(WSAA-AGENT-SUB) NOT = 0                     <V4L014>
              MOVE T5645-SACSCODE-08   TO LIFA-SACSCODE
              MOVE T5645-SACSTYPE-08   TO LIFA-SACSTYP
              MOVE T5645-GLMAP-08      TO LIFA-GLCODE
              MOVE T5645-SIGN-08       TO LIFA-GLSIGN
              MOVE T5645-CNTTOT-08     TO LIFA-CONTOT
              MOVE WSAA-AGNTNUM(WSAA-AGENT-SUB) TO LIFA-RLDGACCT
              MOVE WSAA-COMM-DUE(WSAA-AGENT-SUB) TO LIFA-ORIGAMT
              MOVE COVRLNB-CHDRNUM        TO LIFA-TRANREF
              ADD  +1                     TO WSAA-JRNSEQ
              MOVE WSAA-JRNSEQ            TO LIFA-JRNSEQ
              CALL 'LIFACMV'           USING LIFA-LIFACMV-REC
      *       PERFORM 2950-LIFACMV-CHECK.                               <V73L01>
              PERFORM 2950-LIFACMV-CHECK                                <V73L01>
      *                                                                 <V73L01>
      * Override Commission                                             <V73L01>
      *                                                                 <V73L01>
              IF TH605-INDIC           = 'Y'                            <V73L01>
                 MOVE WSAA-ANNPREM(WSAA-AGENT-SUB)                      <V73L01>
                                       TO ZORL-ANNPREM                  <V73L01>
                 PERFORM B100-CALL-ZORCOMPY                             <V73L01>
              END-IF                                                    <V73L01>
           END-IF.                                                      <V73L01>

           IF NOT COMP-LEVEL-ACC
               IF WSAA-COMM-EARN(WSAA-AGENT-SUB) NOT = 0
                  MOVE T5645-SACSCODE-09   TO LIFA-SACSCODE
                  MOVE T5645-SACSTYPE-09   TO LIFA-SACSTYP
                  MOVE T5645-GLMAP-09      TO LIFA-GLCODE
                  MOVE T5645-SIGN-09       TO LIFA-GLSIGN
                  MOVE T5645-CNTTOT-09     TO LIFA-CONTOT
                  MOVE CHDRLNB-CHDRNUM              TO LIFA-RLDGACCT
                  MOVE COVRLNB-CHDRNUM        TO LIFA-TRANREF
                  MOVE WSAA-COMM-EARN(WSAA-AGENT-SUB) TO LIFA-ORIGAMT
                  ADD  +1                     TO WSAA-JRNSEQ
                  MOVE WSAA-JRNSEQ            TO LIFA-JRNSEQ
                  CALL 'LIFACMV'           USING LIFA-LIFACMV-REC
                  PERFORM 2950-LIFACMV-CHECK.

           IF NOT COMP-LEVEL-ACC
               IF WSAA-COMM-PAID(WSAA-AGENT-SUB) NOT = 0
                  MOVE T5645-SACSCODE-10   TO LIFA-SACSCODE
                  MOVE T5645-SACSTYPE-10   TO LIFA-SACSTYP
                  MOVE T5645-GLMAP-10      TO LIFA-GLCODE
                  MOVE T5645-SIGN-10       TO LIFA-GLSIGN
                  MOVE T5645-CNTTOT-10     TO LIFA-CONTOT
                  MOVE CHDRLNB-CHDRNUM              TO LIFA-RLDGACCT
                  MOVE COVRLNB-CHDRNUM        TO LIFA-TRANREF
                  MOVE WSAA-COMM-PAID(WSAA-AGENT-SUB) TO LIFA-ORIGAMT
                  ADD  +1                     TO WSAA-JRNSEQ
                  MOVE WSAA-JRNSEQ            TO LIFA-JRNSEQ
                  CALL 'LIFACMV'           USING LIFA-LIFACMV-REC
                  PERFORM 2950-LIFACMV-CHECK.

           IF WSAA-SERV-DUE(WSAA-AGENT-SUB) NOT = 0
              MOVE T5645-SACSCODE-11   TO LIFA-SACSCODE
              MOVE T5645-SACSTYPE-11   TO LIFA-SACSTYP
              MOVE T5645-GLMAP-11      TO LIFA-GLCODE
              MOVE T5645-SIGN-11       TO LIFA-GLSIGN
              MOVE T5645-CNTTOT-11     TO LIFA-CONTOT
              MOVE WSAA-AGNTNUM(WSAA-AGENT-SUB) TO LIFA-RLDGACCT
              MOVE WSAA-SERV-DUE(WSAA-AGENT-SUB) TO LIFA-ORIGAMT
              MOVE COVRLNB-CHDRNUM        TO LIFA-TRANREF
              ADD  +1                     TO WSAA-JRNSEQ
              MOVE WSAA-JRNSEQ            TO LIFA-JRNSEQ
              CALL 'LIFACMV'           USING LIFA-LIFACMV-REC
      *       PERFORM 2950-LIFACMV-CHECK.                               <V73L01>
              PERFORM 2950-LIFACMV-CHECK                                <V73L01>
      *                                                                 <V73L01>
      * Override Commission                                             <V73L01>
      *                                                                 <V73L01>
              IF TH605-INDIC           = 'Y'                            <V73L01>
                 MOVE WSAA-ANNPREM(WSAA-AGENT-SUB)                      <V73L01>
                                       TO ZORL-ANNPREM                  <V73L01>
                 PERFORM B100-CALL-ZORCOMPY                             <V73L01>
              END-IF                                                    <V73L01>
           END-IF.                                                      <V73L01>

           IF NOT COMP-LEVEL-ACC
               IF WSAA-SERV-EARN(WSAA-AGENT-SUB) NOT = 0
                  MOVE T5645-SACSCODE-12   TO LIFA-SACSCODE
                  MOVE T5645-SACSTYPE-12   TO LIFA-SACSTYP
                  MOVE T5645-GLMAP-12      TO LIFA-GLCODE
                  MOVE T5645-SIGN-12       TO LIFA-GLSIGN
                  MOVE T5645-CNTTOT-12     TO LIFA-CONTOT
                  MOVE CHDRLNB-CHDRNUM              TO LIFA-RLDGACCT
                  MOVE COVRLNB-CHDRNUM        TO LIFA-TRANREF
                  MOVE WSAA-SERV-EARN(WSAA-AGENT-SUB) TO LIFA-ORIGAMT
                  ADD  +1                     TO WSAA-JRNSEQ
                  MOVE WSAA-JRNSEQ            TO LIFA-JRNSEQ
                  CALL 'LIFACMV'           USING LIFA-LIFACMV-REC
                  PERFORM 2950-LIFACMV-CHECK.

           IF WSAA-RENL-DUE(WSAA-AGENT-SUB) NOT = 0
              MOVE T5645-SACSCODE-13   TO LIFA-SACSCODE
              MOVE T5645-SACSTYPE-13   TO LIFA-SACSTYP
              MOVE T5645-GLMAP-13      TO LIFA-GLCODE
              MOVE T5645-SIGN-13       TO LIFA-GLSIGN
              MOVE T5645-CNTTOT-13     TO LIFA-CONTOT
              MOVE WSAA-AGNTNUM(WSAA-AGENT-SUB) TO LIFA-RLDGACCT
              MOVE WSAA-RENL-DUE(WSAA-AGENT-SUB) TO LIFA-ORIGAMT
              MOVE COVRLNB-CHDRNUM        TO LIFA-TRANREF
              ADD  +1                     TO WSAA-JRNSEQ
                                                                        <V4L014>
              MOVE COVRLNB-CHDRNUM        TO WSAA-RLDG-CHDRNUM          <V4L014>
              MOVE COVRLNB-LIFE           TO WSAA-RLDG-LIFE             <V4L014>
              MOVE COVRLNB-COVERAGE       TO WSAA-RLDG-COVERAGE         <V4L014>
              MOVE COVRLNB-RIDER          TO WSAA-RLDG-RIDER            <V4L014>
              MOVE COVRLNB-PLAN-SUFFIX    TO WSAA-PLAN                  <V4L014>
              MOVE WSAA-PLANSUFF          TO WSAA-RLDG-PLAN-SUFFIX      <V4L014>
              MOVE WSAA-RLDGACCT          TO LIFA-TRANREF               <V4L014>
                                                                        <V4L014>
              MOVE WSAA-JRNSEQ            TO LIFA-JRNSEQ
              CALL 'LIFACMV'           USING LIFA-LIFACMV-REC
      *       PERFORM 2950-LIFACMV-CHECK.                               <V73L01>
              PERFORM 2950-LIFACMV-CHECK                                <V73L01>
      *                                                                 <V73L01>
      * Override Commission                                             <V73L01>
      *                                                                 <V73L01>
              IF TH605-INDIC           = 'Y'                            <V73L01>
                 MOVE WSAA-ANNPREM(WSAA-AGENT-SUB)                      <V73L01>
                                       TO ZORL-ANNPREM                  <V73L01>
                 PERFORM B100-CALL-ZORCOMPY                             <V73L01>
              END-IF                                                    <V73L01>
           END-IF.                                                      <V73L01>

           IF NOT COMP-LEVEL-ACC
               IF WSAA-RENL-EARN(WSAA-AGENT-SUB) NOT = 0
                  MOVE T5645-SACSCODE-14   TO LIFA-SACSCODE
                  MOVE T5645-SACSTYPE-14   TO LIFA-SACSTYP
                  MOVE T5645-GLMAP-14      TO LIFA-GLCODE
                  MOVE T5645-SIGN-14       TO LIFA-GLSIGN
                  MOVE T5645-CNTTOT-14     TO LIFA-CONTOT
                  MOVE CHDRLNB-CHDRNUM              TO LIFA-RLDGACCT
                  MOVE COVRLNB-CHDRNUM        TO LIFA-TRANREF
                  MOVE WSAA-RENL-EARN(WSAA-AGENT-SUB) TO LIFA-ORIGAMT
                  ADD  +1                     TO WSAA-JRNSEQ
                  MOVE WSAA-JRNSEQ            TO LIFA-JRNSEQ
                  CALL 'LIFACMV'           USING LIFA-LIFACMV-REC
                  PERFORM 2950-LIFACMV-CHECK.
      ****                                                              <V6L000>
      **** Single Premium Commission posting                            <V6L000>
      ****                                                              <V6L000>
           IF NOT COMP-LEVEL-ACC                                        <V6L000>
               IF WSAA-TCOM-DUE(WSAA-AGENT-SUB) NOT = 0                 <V6L000>
                  MOVE WSAA-T5645-SACSCODE(15)  TO LIFA-SACSCODE        <V6L000>
                  MOVE WSAA-T5645-SACSTYPE(15)  TO LIFA-SACSTYP         <V6L000>
                  MOVE WSAA-T5645-GLMAP(15)     TO LIFA-GLCODE          <V6L000>
                  MOVE WSAA-T5645-SIGN(15)      TO LIFA-GLSIGN          <V6L000>
                  MOVE WSAA-T5645-CNTTOT(15)    TO LIFA-CONTOT          <V6L000>
                  MOVE WSAA-AGNTNUM(WSAA-AGENT-SUB) TO LIFA-RLDGACCT    <V6L000>
                  MOVE COVRLNB-CHDRNUM     TO LIFA-TRANREF              <V6L000>
                  MOVE WSAA-TCOM-DUE(WSAA-AGENT-SUB) TO LIFA-ORIGAMT    <V6L000>
                  ADD  +1                     TO WSAA-JRNSEQ            <V6L000>
                  MOVE WSAA-JRNSEQ            TO LIFA-JRNSEQ            <V6L000>
                  CALL 'LIFACMV'           USING LIFA-LIFACMV-REC       <V6L000>
      *           PERFORM 2950-LIFACMV-CHECK.                   <V6L000><V73L01>
                  PERFORM 2950-LIFACMV-CHECK                            <V73L01>
      *                                                                 <V73L01>
      * Override Commission                                             <V73L01>
      *                                                                 <V73L01>
                  IF TH605-INDIC       = 'Y'                            <V73L01>
                     MOVE WSAA-SINGP(WSAA-AGENT-SUB)                    <V73L01>
                                       TO CLNK-ANNPREM                  <V73L01>
                     PERFORM B100-CALL-ZORCOMPY                         <V73L01>
                  END-IF                                                <V73L01>
           END-IF.                                                      <V73L01>

      * OVERIDING COMMISSION TO BE ADDED.
      *
           IF NOT  COMP-LEVEL-ACC                                       <V4L014>
           PERFORM 2691-OVERRIDE-COMMISSION  VARYING WSAA-SUB
                                             FROM 1 BY 1
                                             UNTIL WSAA-SUB > 10.


       2640-POST-EACH-AGENT.
              PERFORM 2630-DO-POSTINGS VARYING WSAA-AGENT-SUB
                                       FROM 2 BY 1
                                       UNTIL WSAA-AGENT-SUB > 10.

       2690-EXIT.
           EXIT.
      *
       2691-OVERRIDE-COMMISSION SECTION.
      **********************************
      *
       2692-START.
      *    Has now been added...... as follows...

           IF WSAA-OVERRIDE-COMM(WSAA-AGENT-SUB WSAA-SUB) NOT = ZEROS
              MOVE T5645-SACSCODE-15   TO LIFA-SACSCODE
              MOVE T5645-SACSTYPE-15   TO LIFA-SACSTYP
              MOVE T5645-GLMAP-15      TO LIFA-GLCODE
              MOVE T5645-SIGN-15       TO LIFA-GLSIGN
              MOVE T5645-CNTTOT-15     TO LIFA-CONTOT
              MOVE WSAA-OVERRIDE-AGNTNUM(WSAA-AGENT-SUB WSAA-SUB)
                                                TO LIFA-RLDGACCT
              MOVE WSAA-OVERRIDE-COMM(WSAA-AGENT-SUB WSAA-SUB)
                                                TO LIFA-ORIGAMT
              MOVE COVRLNB-CHDRNUM        TO LIFA-TRANREF
              ADD  +1                     TO WSAA-JRNSEQ
              MOVE WSAA-JRNSEQ            TO LIFA-JRNSEQ
              CALL 'LIFACMV'           USING LIFA-LIFACMV-REC
              PERFORM 2950-LIFACMV-CHECK

              IF NOT COMP-LEVEL-ACC
                  MOVE WSAA-T5645-SACSCODE(01)  TO LIFA-SACSCODE
                  MOVE WSAA-T5645-SACSTYPE(01)  TO LIFA-SACSTYP
                  MOVE WSAA-T5645-GLMAP(01)     TO LIFA-GLCODE
                  MOVE WSAA-T5645-SIGN(01)      TO LIFA-GLSIGN
                  MOVE WSAA-T5645-CNTTOT(01)    TO LIFA-CONTOT
                  MOVE COVRLNB-CHDRNUM          TO LIFA-RLDGACCT
      ****        MOVE WSAA-OVERRIDE-COMM(WSAA-AGENT-SUB WSAA-SUB)      <075>
      ****                                      TO LIFA-ORIGAMT         <075>
                  COMPUTE LIFA-ORIGAMT =                                <075>
                       WSAA-OVERRIDE-COMM(WSAA-AGENT-SUB WSAA-SUB)      <075>
                       - WSAA-OVRD-COMM-PAID(WSAA-AGENT-SUB WSAA-SUB)   <075>
      ****        MOVE COVRLNB-CHDRNUM        TO LIFA-TRANREF           <A07459>
      ****        ADD  +1                     TO WSAA-JRNSEQ            <A07459>
      ****        MOVE WSAA-JRNSEQ            TO LIFA-JRNSEQ            <A07459>
      ****        CALL 'LIFACMV'           USING LIFA-LIFACMV-REC       <A07459>
      ****        PERFORM 2950-LIFACMV-CHECK.                           <A07459>
                  IF LIFA-ORIGAMT   NOT = ZEROS                         <A07459>
                     MOVE COVRLNB-CHDRNUM TO LIFA-TRANREF               <A07459>
                     ADD +1               TO WSAA-JRNSEQ                <A07459>
                     MOVE WSAA-JRNSEQ     TO LIFA-JRNSEQ                <A07459>
                     CALL 'LIFACMV' USING LIFA-LIFACMV-REC              <A07459>
                     PERFORM 2950-LIFACMV-CHECK                         <A07459>
                  END-IF                                                <A07459>
               END-IF                                                   <A07459>
            END-IF.                                                     <A07459>
      *
           IF NOT COMP-LEVEL-ACC
               IF WSAA-OVRD-COMM-PAID(WSAA-AGENT-SUB WSAA-SUB)
                                              NOT = ZEROS
                  MOVE WSAA-T5645-SACSCODE(02)  TO LIFA-SACSCODE
                  MOVE WSAA-T5645-SACSTYPE(02)  TO LIFA-SACSTYP
                  MOVE WSAA-T5645-GLMAP(02)     TO LIFA-GLCODE
                  MOVE WSAA-T5645-SIGN(02)      TO LIFA-GLSIGN
                  MOVE WSAA-T5645-CNTTOT(02)    TO LIFA-CONTOT
                  MOVE COVRLNB-CHDRNUM          TO LIFA-RLDGACCT
                  MOVE WSAA-OVRD-COMM-PAID(WSAA-AGENT-SUB WSAA-SUB)
                                                TO LIFA-ORIGAMT
                  MOVE COVRLNB-CHDRNUM        TO LIFA-TRANREF
                  ADD  +1                     TO WSAA-JRNSEQ
                  MOVE WSAA-JRNSEQ            TO LIFA-JRNSEQ
                  CALL 'LIFACMV'           USING LIFA-LIFACMV-REC
                  PERFORM 2950-LIFACMV-CHECK.

       2699-EXIT.
            EXIT.
      *
      *2700-REINSURANCE-ACCOUNTING SECTION.                             <070>
      **********************************                                <070>
      *2710-REINSURANCE-ACCOUNTING.                                     <070>
      *                                                                 <070>
      * To be added out when Steve Hathaway has sorted it out..         <070>
      *                                                                 <070>
      *2790-EXIT.                                                       <070>
      *    EXIT.                                                        <070>
      *
      /
      *
       2800-HOUSEKEEPING SECTION.
      ***************************
       2801-HOUSEKEEPING.
      *
           PERFORM 2810-WRITE-PTRN.
           PERFORM 2820-UPDATE-BATCH-HEADER.
      *    PERFORM 2830-POLICY-ROUTINE.                                 <073>
      **** PERFORM 2840-RLSE-SOFTLOCK.                                  <079>
      *
       2809-EXIT.
           EXIT.
      *
       M800-UPDATE-MLIA SECTION.                                        <V4L011>
      **************************                                        <V4L011>
                                                                        <V4L011>
       M800-START.                                                      <V4L011>
                                                                        <V4L011>
           MOVE CHDRLNB-CHDRCOY        TO MLLI-CHDRCOY.                 <V4L011>
           MOVE CHDRLNB-CHDRNUM        TO MLLI-CHDRNUM.                 <V4L011>
           MOVE WSAA-FSU-COY           TO MLLI-FSUCOY.                  <V4L011>
           MOVE ATMD-BATCH-KEY         TO PTRN-DATA-KEY.                <V4L011>
           MOVE PTRN-BATCTRCDE         TO MLLI-BATCTRCDE.               <V4L011>
           MOVE ATMD-LANGUAGE          TO MLLI-LANGUAGE.                <LA3998>
           CALL 'RLLIADB'       USING     MLLI-RLLIA-REC.               <V4L011>
                                                                        <LA3380>
           IF MLLI-STATUZ          NOT = O-K                            <LA3380>
              MOVE MLLI-RLLIA-REC      TO SYSR-PARAMS                   <LA3380>
              MOVE MLLI-STATUZ         TO SYSR-STATUZ                   <LA3380>
              PERFORM XXXX-FATAL-ERROR                                  <LA3380>
           END-IF.                                                      <LA3380>
                                                                        <V4L011>
       M800-EXIT.                                                       <V4L011>
           EXIT.                                                        <V4L011>
      *
       2810-WRITE-PTRN SECTION.
      *************************
       2811-WRITE-PTRN.
      *
           MOVE SPACES                 TO PTRN-DATA-AREA.
           MOVE WSAA-TERMID            TO PTRN-TERMID.
           MOVE WSAA-TRANSACTION-DATE  TO PTRN-TRANSACTION-DATE.
           MOVE WSAA-TRANSACTION-TIME  TO PTRN-TRANSACTION-TIME.
           MOVE WSAA-USER              TO PTRN-USER.
           MOVE ATMD-BATCH-KEY         TO PTRN-DATA-KEY.
           MOVE CHDRLNB-TRANNO         TO PTRN-TRANNO.
      ***  MOVE CHDRLNB-OCCDATE        TO PTRN-PTRNEFF.                 <LA2113>
           MOVE DTC1-INT-DATE          TO PTRN-PTRNEFF.                 <LA2113>
           MOVE DTC1-INT-DATE          TO PTRN-DATESUB.                 <D9703>
           MOVE CHDRLNB-CHDRCOY        TO PTRN-CHDRCOY.
           MOVE CHDRLNB-CHDRNUM        TO PTRN-CHDRNUM.
           MOVE PTRNREC                TO PTRN-FORMAT.
           MOVE WRITR                  TO PTRN-FUNCTION.
           CALL 'PTRNIO'               USING PTRN-PARAMS.
           IF PTRN-STATUZ              NOT = O-K
              MOVE PTRN-PARAMS         TO SYSR-PARAMS
              MOVE PTRN-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
      *
       2819-EXIT.
           EXIT.
      *
      /
      *
       2820-UPDATE-BATCH-HEADER SECTION.
      ***************************
       2821-UPDATE-BATCH-HEADER.
      *
           MOVE SPACES                 TO BCUP-BATCUP-REC.
           MOVE ATMD-BATCH-KEY         TO BCUP-BATCHKEY.
           MOVE 1                      TO BCUP-TRANCNT.
           MOVE 0                      TO BCUP-ETREQCNT,
                                          BCUP-SUB,
                                          BCUP-BCNT,
                                          BCUP-BVAL,
                                          BCUP-ASCNT.
           MOVE WRITS                  TO BCUP-FUNCTION.
           CALL 'BATCUP'               USING BCUP-BATCUP-REC.
           IF BCUP-STATUZ              NOT = O-K
              MOVE BCUP-BATCUP-REC     TO SYSR-PARAMS
              MOVE BCUP-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
      *
       2829-EXIT.
           EXIT.
      *
      /
      *
      *2830-POLICY-ROUTINE SECTION.                                     <073>
      *****************************
      *2831-POLICY-ROUTINE.                                             <073>
      *
      * To be added.
      *
      *2839-EXIT.                                                       <073>
      *    EXIT.                                                        <073>
      *
      /
      *
       2840-RLSE-SOFTLOCK SECTION.
      ****************************
       2841-RLSE-SOFTLOCK.
      * Release the soft lock on the contract.
           MOVE SPACES                 TO SFTL-SFTLOCK-REC.
           MOVE ATMD-COMPANY           TO SFTL-COMPANY.
           MOVE CHDRLNB-CHDRNUM        TO SFTL-ENTITY.
           MOVE 'CH'                   TO SFTL-ENTTYP.
           MOVE ATMD-BATCH-KEY         TO SFTL-TRANSACTION.
           MOVE SPACES                 TO SFTL-STATUZ.
           MOVE 'UNLK'                 TO SFTL-FUNCTION.
           CALL 'SFTLOCK'              USING SFTL-SFTLOCK-REC.
           IF SFTL-STATUZ              NOT = O-K
              MOVE SFTL-SFTLOCK-REC    TO SYSR-PARAMS
              MOVE SFTL-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
       2849-EXIT.
           EXIT.
      /
       2900-READ-TR52Q SECTION.                                         <V74L03>
      *************************                                         <V74L03>
       2900-BEGIN.                                                      <V74L03>
                                                                        <V74L03>
           MOVE SPACES                 TO ITEM-DATA-KEY.                <V74L03>
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.                 <V74L03>
           MOVE CHDRLNB-CHDRCOY        TO ITEM-ITEMCOY.                 <V74L03>
           MOVE TR52Q                  TO ITEM-ITEMTABL.                <V74L03>
           MOVE CHDRLNB-CNTTYPE        TO ITEM-ITEMITEM.                <V74L03>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <V74L03>
           MOVE READR                  TO ITEM-FUNCTION.                <V74L03>
           CALL 'ITEMIO'            USING ITEM-PARAMS.                  <V74L03>
                                                                        <V74L03>
           IF ITEM-STATUZ           NOT = O-K                           <V74L03>
           AND ITEM-STATUZ          NOT = MRNF                          <V74L03>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <V74L03>
               MOVE ITEM-STATUZ        TO SYSR-STATUZ                   <V74L03>
               PERFORM XXXX-FATAL-ERROR                                 <V74L03>
           END-IF.                                                      <V74L03>
                                                                        <V74L03>
           IF ITEM-STATUZ               = MRNF                          <V74L03>
               MOVE SPACES             TO ITEM-DATA-KEY                 <V74L03>
               MOVE SMTP-ITEM          TO ITEM-ITEMPFX                  <V74L03>
               MOVE CHDRLNB-CHDRCOY    TO ITEM-ITEMCOY                  <V74L03>
               MOVE TR52Q              TO ITEM-ITEMTABL                 <V74L03>
               MOVE '***'              TO ITEM-ITEMITEM                 <V74L03>
               MOVE READR              TO ITEM-FUNCTION                 <V74L03>
               CALL 'ITEMIO'        USING ITEM-PARAMS                   <V74L03>
                                                                        <V74L03>
               IF ITEM-STATUZ       NOT = O-K                           <V74L03>
                  MOVE ITEM-PARAMS     TO SYSR-PARAMS                   <V74L03>
                  MOVE ITEM-STATUZ     TO SYSR-STATUZ                   <V74L03>
                  PERFORM XXXX-FATAL-ERROR                              <V74L03>
               ELSE                                                     <V74L03>
                  MOVE ITEM-GENAREA    TO TR52Q-TR52Q-REC               <V74L03>
               END-IF                                                   <V74L03>
           ELSE                                                         <V74L03>
               MOVE ITEM-GENAREA       TO TR52Q-TR52Q-REC               <V74L03>
           END-IF.                                                      <V74L03>
                                                                        <V74L03>
       2900-EXIT.                                                       <V74L03>
           EXIT.                                                        <V74L03>
      /                                                                 <V74L03>
       2950-UPD-DESPATCH-DETAILS SECTION.                               <V74L03>
      ***********************************                               <V74L03>
       2950-BEGIN.                                                      <V74L03>
                                                                        <V74L03>
      *    MOVE HPAD-HISSDTE           TO HPAD-DESPDATE.        <PHE003><V74L03>
           MOVE HPAD-HOISSDTE          TO HPAD-DESPDATE.                <PHE003>
      **** MOVE VRCM-MAX-DATE          TO HPAD-PACKDATE.        <PS043> <V74L03>
           MOVE HPAD-PACKDATE          TO HPAD-PACKDATE.                <PS043>
           MOVE VRCM-MAX-DATE          TO HPAD-REMDTE.                  <V74L03>
           MOVE VRCM-MAX-DATE          TO HPAD-DEEMDATE.                <V74L03>
           MOVE VRCM-MAX-DATE          TO HPAD-NEXT-ACT-DATE.           <V74L03>
                                                                        <V74L03>
           IF HPAD-DLVRMODE             = SPACES                        <V74L03>
              MOVE TR52Q-DLVRMODE-01   TO HPAD-DLVRMODE                 <V74L03>
           END-IF.                                                      <V74L03>
                                                                        <V74L03>
      *                                                                 <V74L03>
      * Check whether the delivery mode is excluded from acknowledgement<V74L03>
      * process                                                         <V74L03>
      *                                                                 <V74L03>
           MOVE 'N'                    TO WSAA-FOUND.                   <V74L03>
           PERFORM VARYING IX FROM 2 BY 1 UNTIL IX > 9 OR FOUND         <V74L03>
              IF TR52Q-DLVRMODE(IX)     = HPAD-DLVRMODE                 <V74L03>
                   MOVE 'Y'            TO WSAA-FOUND                    <V74L03>
             END-IF                                                     <V74L03>
           END-PERFORM.                                                 <V74L03>
                                                                        <V74L03>
           IF FOUND                                                     <V74L03>
              MOVE 'E'                 TO HPAD-INCEXC                   <V74L03>
           ELSE                                                         <V74L03>
              MOVE 'I'                 TO HPAD-INCEXC                   <V74L03>
              MOVE 'DY'                TO DTC2-FREQUENCY                <V74L03>
              MOVE HPAD-DESPDATE       TO DTC2-INT-DATE-1               <V74L03>
              MOVE TR52Q-DAEXPY-01     TO DTC2-FREQ-FACTOR              <V74L03>
              CALL 'DATCON2'           USING DTC2-DATCON2-REC           <V74L03>
              IF DTC2-STATUZ           NOT = O-K                        <V74L03>
                 MOVE DTC2-DATCON2-REC TO SYSR-PARAMS                   <V74L03>
                 MOVE DTC2-STATUZ      TO SYSR-STATUZ                   <V74L03>
                 PERFORM XXXX-FATAL-ERROR                               <V74L03>
              END-IF                                                    <V74L03>
              MOVE DTC2-INT-DATE-2     TO HPAD-NEXT-ACT-DATE            <V74L03>
           END-IF.                                                      <V74L03>
                                                                        <V74L03>
       2950-EXIT.                                                       <V74L03>
           EXIT.                                                        <V74L03>
      /                                                                 <V74L03>
       2950-LIFACMV-CHECK SECTION.
      ****************************
       2950-CHECK.
      *
              IF LIFA-STATUZ  NOT = O-K
                 MOVE LIFA-LIFACMV-REC    TO SYSR-PARAMS
                 MOVE LIFA-STATUZ         TO SYSR-STATUZ
                 PERFORM XXXX-FATAL-ERROR.
      *
       2990-EXIT.
           EXIT.
      *                                                                 <V74L03>
       2A00-COMMISSION-HOLD SECTION.                                    <V74L03>
      ******************************                                    <V74L03>
       2A00-BEGIN.                                                      <V74L03>
                                                                        <V74L03>
           IF  TH605-COMIND             = 'Y'                           <V74L03>
           AND HPAD-INCEXC              = 'I'                           <V74L03>
            PERFORM 2B00-UPDATE-AGPY                                    <V74L03>
           END-IF.                                                      <V74L03>
                                                                        <V74L03>
       2A00-EXIT.                                                       <V74L03>
           EXIT.                                                        <V74L03>
      *                                                                 <V74L03>
       2B00-UPDATE-AGPY SECTION.                                        <V74L03>
      **************************                                        <V74L03>
       2B00-BEGIN.                                                      <V74L03>
                                                                        <V74L03>
           INITIALIZE                     AGPYDOC-PARAMS.               <V74L03>
           MOVE HPAD-CHDRCOY           TO AGPYDOC-BATCCOY.              <V74L03>
           MOVE HPAD-CHDRNUM           TO AGPYDOC-RDOCNUM.              <V74L03>
           MOVE 0                      TO AGPYDOC-TRANNO.               <V74L03>
           MOVE 0                      TO AGPYDOC-JRNSEQ.               <V74L03>
           MOVE AGPYDOCREC             TO AGPYDOC-FORMAT.               <V74L03>
           MOVE BEGN                   TO AGPYDOC-FUNCTION.             <V74L03>
                                                                        <V74L03>
       2B00-CALL.                                                       <V74L03>
           CALL 'AGPYDOCIO'         USING AGPYDOC-PARAMS.               <V74L03>
                                                                        <V74L03>
           IF AGPYDOC-STATUZ        NOT = O-K AND ENDP                  <V74L03>
              MOVE AGPYDOC-STATUZ      TO SYSR-STATUZ                   <V74L03>
              MOVE AGPYDOC-PARAMS      TO SYSR-PARAMS                   <V74L03>
              PERFORM XXXX-FATAL-ERROR                                  <V74L03>
           END-IF.                                                      <V74L03>
                                                                        <V74L03>
           IF AGPYDOC-STATUZ            = ENDP                          <V74L03>
           OR AGPYDOC-BATCCOY       NOT = HPAD-CHDRCOY                  <V74L03>
           OR AGPYDOC-RDOCNUM       NOT = HPAD-CHDRNUM                  <V74L03>
              MOVE ENDP                TO AGPYDOC-STATUZ                <V74L03>
              GO TO 2B00-EXIT                                           <V74L03>
           END-IF.                                                      <V74L03>
                                                                        <V74L03>
           IF AGPYDOC-TRANNO        NOT = CHDRLNB-TRANNO                <V74L03>
              GO TO 2B00-NEXT                                           <V74L03>
           END-IF.                                                      <V74L03>
                                                                        <V74L03>
           IF AGPYDOC-EFFDATE       NOT = VRCM-MAX-DATE                 <V74L03>
              INITIALIZE                  AGPYAGT-PARAMS                <V74L03>
              MOVE AGPYDOC-RRN         TO AGPYAGT-RRN                   <V74L03>
              MOVE AGPYAGTREC          TO AGPYAGT-FORMAT                <V74L03>
              MOVE READD               TO AGPYAGT-FUNCTION              <V74L03>
              CALL 'AGPYAGTIO'         USING AGPYAGT-PARAMS             <V74L03>
              IF AGPYAGT-STATUZ        NOT = O-K                        <V74L03>
                 MOVE AGPYAGT-PARAMS   TO SYSR-PARAMS                   <V74L03>
                 MOVE AGPYAGT-STATUZ   TO SYSR-STATUZ                   <V74L03>
                 PERFORM XXXX-FATAL-ERROR                               <V74L03>
              END-IF                                                    <V74L03>
                                                                        <V74L03>
              MOVE VRCM-MAX-DATE       TO AGPYAGT-EFFDATE               <V74L03>
              MOVE WRITD               TO AGPYAGT-FUNCTION              <V74L03>
              CALL 'AGPYAGTIO'         USING AGPYAGT-PARAMS             <V74L03>
              IF AGPYAGT-STATUZ        NOT = O-K                        <V74L03>
                 MOVE AGPYAGT-PARAMS   TO SYSR-PARAMS                   <V74L03>
                 MOVE AGPYAGT-STATUZ   TO SYSR-STATUZ                   <V74L03>
                 PERFORM XXXX-FATAL-ERROR                               <V74L03>
              END-IF                                                    <V74L03>
           END-IF.                                                      <V74L03>
                                                                        <V74L03>
       2B00-NEXT.                                                       <V74L03>
              MOVE NEXTR               TO AGPYDOC-FUNCTION.             <V74L03>
              GO TO 2B00-CALL.                                          <V74L03>
                                                                        <V74L03>
       2B00-EXIT.                                                       <V74L03>
           EXIT.                                                        <V74L03>
      *                                                                 <V71L12>
       2X00-UNDERWRITING-UNDL SECTION.                                  <V71L12>
      ********************************                                  <V71L12>
       2X00-INIT.                                                       <V71L12>
      *                                                                 <V71L12>
      * Read first Underwriting Life record                             <V71L12>
                                                                        <V71L12>
           MOVE SPACES                 TO UNDL-DATA-AREA.               <V71L12>
           MOVE ATMD-COMPANY           TO UNDL-CHDRCOY.                 <V71L12>
           MOVE CHDRLNB-CHDRNUM        TO UNDL-CHDRNUM.                 <V71L12>
                                                                        <V71L12>
           MOVE BEGN                   TO UNDL-FUNCTION.                <V71L12>
           CALL 'UNDLIO'               USING UNDL-PARAMS.               <V71L12>
                                                                        <V71L12>
           IF UNDL-STATUZ          NOT = O-K AND                        <V71L12>
                                   NOT = ENDP                           <V71L12>
              MOVE UNDL-PARAMS         TO SYSR-PARAMS                   <V71L12>
              MOVE UNDL-STATUZ         TO SYSR-STATUZ                   <V71L12>
              PERFORM XXXX-FATAL-ERROR                                  <V71L12>
           END-IF.                                                      <V71L12>
                                                                        <V71L12>
           PERFORM 2X50-UPDATE-UNDL    UNTIL UNDL-STATUZ = ENDP         <V71L12>
                   OR UNDL-CHDRCOY NOT = ATMD-COMPANY                   <V71L12>
                   OR UNDL-CHDRNUM NOT = CHDRLNB-CHDRNUM.               <V71L12>
      *                                                                 <V71L12>
       2X09-EXIT.                                                       <V71L12>
           EXIT.                                                        <V71L12>
      *                                                                 <V71L12>
       2X50-UPDATE-UNDL SECTION.                                        <V71L12>
      **************************                                        <V71L12>
       2X51-INIT.                                                       <V71L12>
      *                                                                 <V71L12>
           MOVE '1'                    TO UNDL-VALIDFLAG.               <V71L12>
           MOVE CHDRLNB-TRANNO         TO UNDL-TRANNO.                  <V71L12>
                                                                        <V71L12>
           MOVE UPDAT                  TO UNDL-FUNCTION.                <V71L12>
           CALL 'UNDLIO'               USING UNDL-PARAMS.               <V71L12>
                                                                        <V71L12>
           IF UNDL-STATUZ          NOT = O-K                            <V71L12>
              MOVE UNDL-PARAMS         TO SYSR-PARAMS                   <V71L12>
              MOVE UNDL-STATUZ         TO SYSR-STATUZ                   <V71L12>
              PERFORM XXXX-FATAL-ERROR.                                 <V71L12>
                                                                        <V71L12>
           MOVE NEXTR                  TO UNDL-FUNCTION.                <V71L12>
           CALL 'UNDLIO'               USING UNDL-PARAMS.               <V71L12>
                                                                        <V71L12>
           IF UNDL-STATUZ          NOT = O-K                            <V71L12>
                               AND NOT = ENDP                           <V71L12>
              MOVE UNDL-PARAMS         TO SYSR-PARAMS                   <V71L12>
              MOVE UNDL-STATUZ         TO SYSR-STATUZ                   <V71L12>
              PERFORM XXXX-FATAL-ERROR                                  <V71L12>
           END-IF.                                                      <V71L12>
                                                                        <V71L12>
      *                                                                 <V71L12>
       2X59-EXIT.                                                       <V71L12>
           EXIT.                                                        <V71L12>
      *                                                                 <V71L12>
       2Y00-UNDERWRITING-UNDC SECTION.                                  <V71L12>
      ********************************                                  <V71L12>
       2Y00-INIT.                                                       <V71L12>
      *                                                                 <V71L12>
      * Read first Underwriting Component                               <V71L12>
                                                                        <V71L12>
           MOVE SPACES                 TO UNDC-DATA-AREA.               <V71L12>
           MOVE ATMD-COMPANY           TO UNDC-CHDRCOY.                 <V71L12>
           MOVE CHDRLNB-CHDRNUM        TO UNDC-CHDRNUM.                 <V71L12>
           MOVE BEGN                   TO UNDC-FUNCTION.                <V71L12>
           CALL 'UNDCIO'               USING UNDC-PARAMS.               <V71L12>
                                                                        <V71L12>
           IF UNDC-STATUZ          NOT = O-K AND                        <V71L12>
                                   NOT = ENDP                           <V71L12>
              MOVE UNDC-PARAMS         TO SYSR-PARAMS                   <V71L12>
              MOVE UNDC-STATUZ         TO SYSR-STATUZ                   <V71L12>
              PERFORM XXXX-FATAL-ERROR                                  <V71L12>
           END-IF.                                                      <V71L12>
                                                                        <V71L12>
           PERFORM 2Y50-UPDATE-UNDC    UNTIL UNDC-STATUZ = ENDP         <V71L12>
                   OR UNDC-CHDRCOY NOT = ATMD-COMPANY                   <V71L12>
                   OR UNDC-CHDRNUM NOT = CHDRLNB-CHDRNUM.               <V71L12>
      *                                                                 <V71L12>
       2Y09-EXIT.                                                       <V71L12>
           EXIT.                                                        <V71L12>
      *                                                                 <V71L12>
       2Y50-UPDATE-UNDC SECTION.                                        <V71L12>
      **************************                                        <V71L12>
       2Y51-INIT.                                                       <V71L12>
      *                                                                 <V71L12>
           MOVE '1'                    TO UNDC-VALIDFLAG.               <V71L12>
           MOVE CHDRLNB-TRANNO         TO UNDC-TRANNO.                  <V71L12>
                                                                        <V71L12>
           MOVE UPDAT                  TO UNDC-FUNCTION.                <V71L12>
           CALL 'UNDCIO'               USING UNDC-PARAMS.               <V71L12>
                                                                        <V71L12>
           IF UNDC-STATUZ          NOT = O-K                            <V71L12>
              MOVE UNDC-PARAMS         TO SYSR-PARAMS                   <V71L12>
              MOVE UNDC-STATUZ         TO SYSR-STATUZ                   <V71L12>
              PERFORM XXXX-FATAL-ERROR                                  <V71L12>
           END-IF.                                                      <V71L12>
                                                                        <V71L12>
           MOVE NEXTR                  TO UNDC-FUNCTION.                <V71L12>
           CALL 'UNDCIO'               USING UNDC-PARAMS.               <V71L12>
                                                                        <V71L12>
           IF UNDC-STATUZ          NOT = O-K                            <V71L12>
                               AND NOT = ENDP                           <V71L12>
              MOVE UNDC-PARAMS         TO SYSR-PARAMS                   <V71L12>
              MOVE UNDC-STATUZ         TO SYSR-STATUZ                   <V71L12>
              PERFORM XXXX-FATAL-ERROR                                  <V71L12>
           END-IF.                                                      <V71L12>
                                                                        <V71L12>
      *                                                                 <V71L12>
       2Y59-EXIT.                                                       <V71L12>
           EXIT.                                                        <V71L12>
      *                                                                 <V71L12>
       2Z00-UNDERWRITING-UNDQ SECTION.                                  <V71L12>
      ********************************                                  <V71L12>
       2Z00-INIT.                                                       <V71L12>
      *                                                                 <V71L12>
      * Read first Underwriting Question                                <V71L12>
                                                                        <V71L12>
           MOVE SPACES                 TO UNDQ-DATA-AREA.               <V71L12>
           MOVE ATMD-COMPANY           TO UNDQ-CHDRCOY.                 <V71L12>
           MOVE CHDRLNB-CHDRNUM        TO UNDQ-CHDRNUM.                 <V71L12>
           MOVE BEGN                   TO UNDQ-FUNCTION.                <V71L12>
           CALL 'UNDQIO'               USING UNDQ-PARAMS.               <V71L12>
                                                                        <V71L12>
           IF UNDQ-STATUZ          NOT = O-K AND                        <V71L12>
                                   NOT = ENDP                           <V71L12>
              MOVE UNDQ-PARAMS         TO SYSR-PARAMS                   <V71L12>
              MOVE UNDQ-STATUZ         TO SYSR-STATUZ                   <V71L12>
              PERFORM XXXX-FATAL-ERROR                                  <V71L12>
           END-IF.                                                      <V71L12>
                                                                        <V71L12>
           PERFORM 2Z50-UPDATE-UNDQ    UNTIL UNDQ-STATUZ = ENDP         <V71L12>
                   OR UNDQ-CHDRCOY NOT = ATMD-COMPANY                   <V71L12>
                   OR UNDQ-CHDRNUM NOT = CHDRLNB-CHDRNUM.               <V71L12>
      *                                                                 <V71L12>
       2Z09-EXIT.                                                       <V71L12>
           EXIT.                                                        <V71L12>
      *                                                                 <V71L12>
       2Z50-UPDATE-UNDQ SECTION.                                        <V71L12>
      **************************                                        <V71L12>
       2Z51-INIT.                                                       <V71L12>
      *                                                                 <V71L12>
           MOVE '1'                    TO UNDQ-VALIDFLAG.               <V71L12>
           MOVE CHDRLNB-TRANNO         TO UNDQ-TRANNO.                  <V71L12>
                                                                        <V71L12>
           MOVE UPDAT                  TO UNDQ-FUNCTION.                <V71L12>
           CALL 'UNDQIO'               USING UNDQ-PARAMS.               <V71L12>
           IF UNDQ-STATUZ          NOT = O-K                            <V71L12>
              MOVE UNDQ-PARAMS         TO SYSR-PARAMS                   <V71L12>
              MOVE UNDQ-STATUZ         TO SYSR-STATUZ                   <V71L12>
              PERFORM XXXX-FATAL-ERROR                                  <V71L12>
           END-IF.                                                      <V71L12>
                                                                        <V71L12>
           MOVE NEXTR                  TO UNDQ-FUNCTION.                <V71L12>
           CALL 'UNDQIO'               USING UNDQ-PARAMS.               <V71L12>
                                                                        <V71L12>
           IF UNDQ-STATUZ          NOT = O-K                            <V71L12>
                               AND NOT = ENDP                           <V71L12>
              MOVE UNDQ-PARAMS         TO SYSR-PARAMS                   <V71L12>
              MOVE UNDQ-STATUZ         TO SYSR-STATUZ                   <V71L12>
              PERFORM XXXX-FATAL-ERROR                                  <V71L12>
           END-IF.                                                      <V71L12>
                                                                        <V71L12>
      *                                                                 <V71L12>
       2Z59-EXIT.                                                       <V71L12>
           EXIT.                                                        <V71L12>
      /
       3000-INITIALISE-COVR-PREM SECTION.
      ***********************************
      *
       3010-PARA.

           MOVE 0                  TO WSAA-COVT-INSTPREM(WSAA-L WSAA-C)
                                      WSAA-COVT-SINGP(WSAA-L WSAA-C).
           ADD 1                       TO WSAA-C.

           IF WSAA-C > 99
               ADD 1                   TO WSAA-L
               MOVE 1                  TO WSAA-C.

       3090-EXIT.
           EXIT.                                                        <RGS>
*******     EXIT.                                                       <RGS>
      *
      /
       3100-EXCESS-PREMIUM-ALLOCATION SECTION.                          <UL001>
      ****************************************                          <UL001>
      *                                                                 <UL001>
       3110-START.                                                      <UL001>
      *                                                                 <UL001>
      * Fetch coverage/rider generic component processing progs.        <UL001>
           MOVE SPACES                 TO ITEM-DATA-AREA.               <UL001>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <UL001>
           MOVE ATMD-COMPANY           TO ITEM-ITEMCOY.                 <UL001>
           MOVE T5671                  TO ITEM-ITEMTABL.                <UL001>
           MOVE WSAA-EXC-PRM-KEY       TO ITEM-ITEMITEM.                <UL001>
           MOVE READR                  TO ITEM-FUNCTION.                <UL001>
           CALL 'ITEMIO' USING         ITEM-PARAMS.                     <UL001>
           IF ITEM-STATUZ              NOT = O-K AND NOT = MRNF         <UL001>
              MOVE ITEM-PARAMS         TO SYSR-PARAMS                   <UL001>
              MOVE ITEM-STATUZ         TO SYSR-STATUZ                   <UL001>
              PERFORM XXXX-FATAL-ERROR.                                 <UL001>
                                                                        <UL001>
           IF ITEM-STATUZ              = MRNF                           <UL001>
              GO TO 3190-EXIT.                                          <UL001>
                                                                        <UL001>
           MOVE ITEM-GENAREA            TO WSAA-T5671-REC.              <UL001>
                                                                        <UL001>
           MOVE ISUA-COMPANY           TO RNLA-COMPANY.                 <UL001>
           MOVE ISUA-CHDRNUM           TO RNLA-CHDRNUM.                 <UL001>
           MOVE SPACES                 TO RNLA-LIFE                     <UL001>
                                          RNLA-COVERAGE                 <UL001>
                                          RNLA-RIDER.                   <UL001>
           MOVE ZEROES                 TO RNLA-PLAN-SUFFIX.             <UL001>
           MOVE ISUA-BATCCOY           TO RNLA-BATCCOY.                 <UL001>
           MOVE ISUA-BATCBRN           TO RNLA-BATCBRN.                 <UL001>
           MOVE ISUA-BATCACTYR         TO RNLA-BATCACTYR.               <UL001>
           MOVE ISUA-BATCACTMN         TO RNLA-BATCACTMN.               <UL001>
           MOVE ISUA-BATCTRCDE         TO RNLA-BATCTRCDE.               <UL001>
           MOVE ISUA-BATCBATCH         TO RNLA-BATCBATCH.               <UL001>
           MOVE WSAA-EFFDATE           TO RNLA-EFFDATE.                 <UL001>
           MOVE ISUA-USER              TO RNLA-USER.                    <UL001>
           MOVE ISUA-LANGUAGE          TO RNLA-LANGUAGE.                <UL001>
           MOVE ISUA-COVR-INSTPREM     TO RNLA-COVR-INSTPREM.           <UL001>
           MOVE CHDRLNB-CNTCURR        TO RNLA-CNTCURR.                 <UL001>
           MOVE CHDRLNB-CNTTYPE        TO RNLA-CNTTYPE.                 <UL001>
      *                                                                 <UL001>
           PERFORM 3120-SUB-CALL       VARYING WSAA-SUB FROM 1 BY 1     <UL001>
                                       UNTIL WSAA-SUB > 1.              <UL001>
                                                                        <UL001>
           GO TO 3190-EXIT.                                             <UL001>
      *                                                                 <UL001>
      * Do the subroutine calls.                                        <UL001>
      *                                                                 <UL001>
       3120-SUB-CALL.                                                   <UL001>
      *                                                                 <UL001>
           IF WSAA-T5671-SUBPROG(WSAA-SUB)   NOT = SPACES               <UL001>
              CALL WSAA-T5671-SUBPROG(WSAA-SUB)                         <UL001>
                                       USING RNLA-RNLALL-REC.           <UL001>
      *                                                                 <UL001>
       3190-EXIT.                                                       <UL001>
           EXIT.                                                        <UL001>
      /                                                                 <UL001>
                                                                        <UL001>
      **************************************                            <070>
      *4000-CHECK-FOR-REASSURANCE SECTION.                      <R96REA><070>
      **************************************                            <070>
      *                                                                 <070>
      *4000-START.                                              <R96REA><070>
      ****                                                      <R96REA><070>
      **** MOVE SPACES                 TO RACTLNB-PARAMS.       <R96REA><070>
      **** MOVE CHDRLNB-CHDRCOY        TO RACTLNB-CHDRCOY.      <R96REA><070>
      **** MOVE CHDRLNB-CHDRNUM        TO RACTLNB-CHDRNUM.      <R96REA><070>
      **** MOVE COVRLNB-LIFE           TO RACTLNB-LIFE.         <R96REA><070>
      **** MOVE COVRLNB-COVERAGE       TO RACTLNB-COVERAGE.     <R96REA><070>
      **** MOVE COVRLNB-RIDER          TO RACTLNB-RIDER.        <R96REA><070>
      **** MOVE BEGN                   TO RACTLNB-FUNCTION.     <R96REA><070>
      **** MOVE RACTLNBREC             TO RACTLNB-FORMAT.       <R96REA><070>
      ****                                                      <R96REA><070>
      **** CALL 'RACTLNBIO'            USING RACTLNB-PARAMS.    <R96REA><070>
      ****                                                      <R96REA><070>
      **** IF RACTLNB-STATUZ           NOT = O-K                <R96REA><070>
      ****    AND RACTLNB-STATUZ       NOT = ENDP               <R96REA><070>
      ****     MOVE RACTLNB-PARAMS     TO SYSR-PARAMS           <R96REA><070>
      ****     MOVE RACTLNB-STATUZ     TO SYSR-STATUZ           <R96REA><070>
      ****     PERFORM XXXX-FATAL-ERROR                         <R96REA><070>
      **** END-IF.                                              <R96REA><070>
      ****                                                      <R96REA><070>
      **** IF RACTLNB-CHDRCOY          NOT = CHDRLNB-CHDRCOY    <R96REA><070>
      ****    OR RACTLNB-CHDRNUM       NOT = CHDRLNB-CHDRNUM    <R96REA><070>
      ****    OR RACTLNB-LIFE          NOT = COVRLNB-LIFE       <R96REA><070>
      ****    OR RACTLNB-COVERAGE      NOT = COVRLNB-COVERAGE   <R96REA><070>
      ****    OR RACTLNB-RIDER         NOT = COVRLNB-RIDER      <R96REA><070>
      ****    OR RACTLNB-STATUZ        = ENDP                   <R96REA><070>
      ****     MOVE ENDP               TO RACTLNB-STATUZ        <R96REA><070>
      **** END-IF.                                              <R96REA><070>
      ****                                                      <R96REA><070>
      **** PERFORM 4100-PROCESS-RACTS  UNTIL RACTLNB-STATUZ = EN<R96REA><070>
      *                                                                 <070>
      *4090-EXIT.                                               <R96REA><070>
      **** EXIT.                                                <R96REA><070>
      *                                                                 <070>
      /                                                                 <070>
      **************************************                            <070>
      *4100-PROCESS-RACTS SECTION.                              <R96REA><070>
      **************************************                            <070>
      *                                                                 <070>
      *4110-START.                                              <R96REA><070>
      *                                                                 <070>
      * Lock RACT record prior to updating validflag & tranno           <070>
      *                                                                 <070>
      **** MOVE READH                  TO RACTLNB-FUNCTION.     <R96REA><070>
      **** MOVE RACTLNBREC             TO RACTLNB-FORMAT.       <R96REA><070>
      ****                                                      <R96REA><070>
      **** CALL 'RACTLNBIO'            USING RACTLNB-PARAMS.    <R96REA><070>
      ****                                                      <R96REA><070>
      **** IF RACTLNB-STATUZ           NOT = O-K                <R96REA><070>
      ****     MOVE RACTLNB-PARAMS     TO SYSR-PARAMS           <R96REA><070>
      ****     MOVE RACTLNB-STATUZ     TO SYSR-STATUZ           <R96REA><070>
      ****     PERFORM XXXX-FATAL-ERROR                         <R96REA><070>
      **** END-IF.                                              <R96REA><070>
      *                                                                 <070>
      * Update validflag & tranno & REWRT RACT record                   <070>
      *                                                                 <070>
      **** MOVE '1'                    TO RACTLNB-VALIDFLAG.    <R96REA><070>
      **** MOVE CHDRLNB-TRANNO         TO RACTLNB-TRANNO.       <R96REA><070>
      **** MOVE VRCM-DATE              TO RACTLNB-TRANSACTION-DA<R96REA><070>
      **** MOVE VRCM-TIME              TO RACTLNB-TRANSACTION-TI<R96REA><070>
      **** MOVE VRCM-USER              TO RACTLNB-USER.         <R96REA><070>
      **** MOVE VRCM-TERMID            TO RACTLNB-TERMID.       <R96REA><070>
      ****                                                      <R96REA><070>
      **** MOVE REWRT                  TO RACTLNB-FUNCTION.     <R96REA><070>
      **** MOVE RACTLNBREC             TO RACTLNB-FORMAT.       <R96REA><070>
      ****                                                      <R96REA><070>
      **** CALL 'RACTLNBIO'            USING RACTLNB-PARAMS.    <R96REA><070>
      ****                                                      <R96REA><070>
      **** IF RACTLNB-STATUZ           NOT = O-K                <R96REA><070>
      ****     MOVE RACTLNB-PARAMS     TO SYSR-PARAMS           <R96REA><070>
      ****     MOVE RACTLNB-STATUZ     TO SYSR-STATUZ           <R96REA><070>
      ****     PERFORM XXXX-FATAL-ERROR                         <R96REA><070>
      **** END-IF.                                              <R96REA><070>
      *                                                                 <070>
      * Look for next RACT record associated with this component        <070>
      *                                                                 <070>
      **** MOVE NEXTR                  TO RACTLNB-FUNCTION.     <R96REA><070>
      **** MOVE RACTLNBREC             TO RACTLNB-FORMAT.       <R96REA><070>
      ****                                                      <R96REA><070>
      **** CALL 'RACTLNBIO'            USING RACTLNB-PARAMS.    <R96REA><070>
      ****                                                      <R96REA><070>
      **** IF RACTLNB-STATUZ           NOT = O-K                <R96REA><070>
      ****    AND RACTLNB-STATUZ       NOT = ENDP               <R96REA><070>
      ****     MOVE RACTLNB-PARAMS     TO SYSR-PARAMS           <R96REA><070>
      ****     MOVE RACTLNB-STATUZ     TO SYSR-STATUZ           <R96REA><070>
      ****     PERFORM XXXX-FATAL-ERROR                         <R96REA><070>
      **** END-IF.                                              <R96REA><070>
      ****                                                      <R96REA><070>
      **** IF RACTLNB-CHDRCOY          NOT = CHDRLNB-CHDRCOY    <R96REA><070>
      ****    OR RACTLNB-CHDRNUM       NOT = CHDRLNB-CHDRNUM    <R96REA><070>
      ****    OR RACTLNB-LIFE          NOT = COVRLNB-LIFE       <R96REA><070>
      ****    OR RACTLNB-COVERAGE      NOT = COVRLNB-COVERAGE   <R96REA><070>
      ****    OR RACTLNB-RIDER         NOT = COVRLNB-RIDER      <R96REA><070>
      ****    OR RACTLNB-STATUZ        = ENDP                   <R96REA><070>
      ****     MOVE ENDP               TO RACTLNB-STATUZ        <R96REA><070>
      **** END-IF.                                              <R96REA><070>
      *                                                                 <070>
      *4190-EXIT.                                               <R96REA><070>
      **** EXIT.                                                <R96REA><070>
      /                                                                 <070>
       4000-CHECK-FOR-REASSURANCE SECTION.                              <R96REA>
      *************************************                             <R96REA>
       4010-CHECK.                                                      <R96REA>
                                                                        <R96REA>
           IF COVRLNB-LIFE             NOT = WSAA-LIFE                  <R96REA>
               PERFORM 4100-READ-LIFE                                   <R96REA>
               MOVE COVRLNB-LIFE       TO WSAA-LIFE                     <R96REA>
           END-IF.                                                      <R96REA>
                                                                        <R96REA>
           MOVE SPACES                 TO ACVR-ACTVRES-REC.             <R96REA>
           MOVE CHDRLNB-CHDRCOY        TO ACVR-CHDRCOY.                 <R96REA>
           MOVE CHDRLNB-CHDRNUM        TO ACVR-CHDRNUM.                 <R96REA>
           MOVE CHDRLNB-CNTTYPE        TO ACVR-CNTTYPE.                 <R96REA>
           MOVE CHDRLNB-CNTCURR        TO ACVR-CURRENCY.                <R96REA>
           MOVE CHDRLNB-TRANNO         TO ACVR-TRANNO.                  <R96REA>
           MOVE COVTLNB-LIFE           TO ACVR-LIFE.                    <R96REA>
           MOVE COVTLNB-COVERAGE       TO ACVR-COVERAGE.                <R96REA>
           MOVE COVTLNB-RIDER          TO ACVR-RIDER.                   <R96REA>
           MOVE ZEROES                 TO ACVR-PLAN-SUFFIX.             <R96REA>
           MOVE COVTLNB-CRTABLE        TO ACVR-CRTABLE.                 <R96REA>
           MOVE COVTLNB-EFFDATE        TO ACVR-EFFDATE.                 <R96REA>
           MOVE WSAA-FSU-COY           TO ACVR-CLNTCOY.                 <R96REA>
           MOVE WSAA-L1-CLNTNUM        TO ACVR-L1-CLNTNUM.              <R96REA>
           MOVE COVTLNB-JLIFE          TO ACVR-JLIFE.                   <R96REA>
           IF WSAA-L2-CLNTNUM          NOT = SPACES                     <R96REA>
               MOVE WSAA-L2-CLNTNUM    TO ACVR-L2-CLNTNUM               <R96REA>
           END-IF.                                                      <R96REA>
           MOVE ZEROES                 TO ACVR-OLD-SUMINS.              <R96REA>
           MOVE COVTLNB-SUMINS         TO ACVR-NEW-SUMINS.              <R96REA>
           MOVE COVTLNB-EFFDATE        TO ACVR-CRRCD.                   <A06785>
           MOVE ATMD-LANGUAGE          TO ACVR-LANGUAGE.                <V73L05>
           MOVE 'ACT8'                 TO ACVR-FUNCTION.                <R96REA>
           MOVE WSKY-BATC-BATCTRCDE    TO ACVR-BATCTRCDE.               <V76F06>
                                                                        <R96REA>
           CALL 'ACTVRES'              USING ACVR-ACTVRES-REC.          <R96REA>
                                                                        <R96REA>
           IF ACVR-STATUZ              NOT = O-K                        <R96REA>
               MOVE ACVR-STATUZ        TO SYSR-STATUZ                   <R96REA>
               MOVE ACVR-ACTVRES-REC   TO SYSR-PARAMS                   <R96REA>
               PERFORM XXXX-FATAL-ERROR                                 <R96REA>
           END-IF.                                                      <R96REA>
                                                                        <R96REA>
       4090-EXIT.                                                       <R96REA>
           EXIT.                                                        <R96REA>
                                                                        <R96REA>
       4100-READ-LIFE SECTION.                                          <R96REA>
      ************************                                  <R96REA><R96REA>
       4110-LIFE.                                                       <R96REA>
                                                                        <R96REA>
           MOVE SPACES                 TO LIFELNB-PARAMS.               <R96REA>
           MOVE ATMD-COMPANY           TO LIFELNB-CHDRCOY.              <R96REA>
           MOVE CHDRLNB-CHDRNUM        TO LIFELNB-CHDRNUM.              <R96REA>
           MOVE COVRLNB-LIFE           TO LIFELNB-LIFE.                 <R96REA>
           MOVE '00'                   TO LIFELNB-JLIFE.                <R96REA>
           MOVE READR                  TO LIFELNB-FUNCTION.             <R96REA>
                                                                        <R96REA>
           CALL 'LIFELNBIO'            USING LIFELNB-PARAMS.            <R96REA>
                                                                        <R96REA>
           IF LIFELNB-STATUZ           NOT = O-K                        <R96REA>
               MOVE LIFELNB-PARAMS     TO SYSR-PARAMS                   <R96REA>
               MOVE LIFELNB-STATUZ     TO SYSR-STATUZ                   <R96REA>
               PERFORM XXXX-FATAL-ERROR                                 <R96REA>
           END-IF.                                                      <R96REA>
                                                                        <R96REA>
           MOVE LIFELNB-LIFCNUM        TO WSAA-L1-CLNTNUM.              <R96REA>
                                                                        <R96REA>
           MOVE SPACES                 TO LIFELNB-PARAMS.               <R96REA>
           MOVE ATMD-COMPANY           TO LIFELNB-CHDRCOY.              <R96REA>
           MOVE CHDRLNB-CHDRNUM        TO LIFELNB-CHDRNUM.              <R96REA>
           MOVE COVRLNB-LIFE           TO LIFELNB-LIFE.                 <R96REA>
           MOVE '01'                   TO LIFELNB-JLIFE.                <R96REA>
           MOVE READR                  TO LIFELNB-FUNCTION.             <R96REA>
                                                                        <R96REA>
           CALL 'LIFELNBIO'            USING LIFELNB-PARAMS.            <R96REA>
                                                                        <R96REA>
           IF  LIFELNB-STATUZ          NOT = O-K                        <R96REA>
           AND LIFELNB-STATUZ          NOT = MRNF                       <R96REA>
               MOVE LIFELNB-PARAMS     TO SYSR-PARAMS                   <R96REA>
               MOVE LIFELNB-STATUZ     TO SYSR-STATUZ                   <R96REA>
               PERFORM XXXX-FATAL-ERROR                                 <R96REA>
           END-IF.                                                      <R96REA>
                                                                        <R96REA>
           IF LIFELNB-STATUZ           = O-K                            <R96REA>
               MOVE LIFELNB-LIFCNUM    TO WSAA-L2-CLNTNUM               <R96REA>
           ELSE                                                         <R96REA>
               MOVE SPACES             TO WSAA-L2-CLNTNUM               <R96REA>
           END-IF.                                                      <R96REA>
                                                                        <R96REA>
       4190-EXIT.                                                       <R96REA>
           EXIT.                                                        <R96REA>
                                                                        <R96REA>
       5000-WRITE-LETTER SECTION.                                       <081>
      ***************************                                       <081>
      *                                                                 <081>
      *5010-READ-T6634.                                         <PCPPRT><N003>
       5010-READ-TR384.                                                 <PCPPRT>
      *                                                                 <081>
      *  Get the Letter type from T6634.                                <081>
      *                                                                 <081>
           MOVE SPACES                     TO ITEM-DATA-AREA.           <N003>
           MOVE 'IT'                       TO ITEM-ITEMPFX.             <N003>
           MOVE CHDRLNB-CHDRCOY            TO ITEM-ITEMCOY.             <N003>
      **** MOVE T6634                      TO ITEM-ITEMTABL.    <PCPPRT><N003>
           MOVE TR384                      TO ITEM-ITEMTABL.            <PCPPRT>
      *                                                                 <081>
      *  Build key to T6634 from contract type & transaction code.      <081>
      *                                                                 <081>
           STRING                          CHDRLNB-CNTTYPE              <N003>
                                           WSKY-BATC-BATCTRCDE          <N003>
           DELIMITED BY                    SPACE                        <N003>
           INTO                            ITEM-ITEMITEM.               <N003>
                                                                        <N003>
           MOVE READR                      TO ITEM-FUNCTION.            <N003>
                                                                        <N003>
           CALL 'ITEMIO' USING ITEM-PARAMS.                             <N003>
                                                                        <N003>
           IF  ITEM-STATUZ             NOT = O-K                        <N003>
                                       AND   MRNF                       <N003>
               MOVE ITEM-PARAMS            TO SYSR-PARAMS               <N003>
               MOVE G437                   TO SYSR-STATUZ               <N003>
               PERFORM XXXX-FATAL-ERROR.                                <N003>
                                                                        <N003>
      *                                                                 <N003>
      * if not found, read again using generic key.                     <N003>
      *                                                                 <N003>
           IF ITEM-STATUZ              = MRNF                           <N003>
              STRING          '***' WSKY-BATC-BATCTRCDE                 <N003>
              DELIMITED BY    SPACE                                     <N003>
              INTO            ITEM-ITEMITEM                             <N003>
                                                                        <N003>
              MOVE READR               TO ITEM-FUNCTION                 <N003>
                                                                        <N003>
              CALL 'ITEMIO' USING ITEM-PARAMS                           <N003>
              IF ITEM-STATUZ           NOT = O-K                        <N003>
                 MOVE ITEM-PARAMS      TO SYSR-PARAMS                   <N003>
                 MOVE G437             TO SYSR-STATUZ                   <N003>
                 PERFORM XXXX-FATAL-ERROR                               <N003>
              END-IF                                                    <N003>
           END-IF.                                                      <N003>
                                                                        <N003>
      **** MOVE ITEM-GENAREA               TO T6634-T6634-REC.  <PCPPRT><N003>
           MOVE ITEM-GENAREA               TO TR384-TR384-REC.          <PCPPRT>
                                                                        <N003>
      **** IF  T6634-LETTER-TYPE           = SPACES             <PCPPRT><N003>
           IF  TR384-LETTER-TYPE           = SPACES                     <PCPPRT>
               GO TO 5099-EXIT.                                         <N003>
      *                                                                 <081>
      *  Write LETC via LETRQST.                                        <081>
      *                                                                 <081>
           MOVE SPACES                 TO LETRQST-STATUZ.               <N003>
           MOVE CHDRLNB-CHDRCOY        TO LETRQST-REQUEST-COMPANY.      <081>
      **** MOVE T6634-LETTER-TYPE      TO LETRQST-LETTER-TYPE.  <PCPPRT><N003>
           MOVE TR384-LETTER-TYPE      TO LETRQST-LETTER-TYPE.          <PCPPRT>
           MOVE CHDRLNB-OCCDATE        TO LETRQST-LETTER-REQUEST-DATE.  <081>
           MOVE CHDRLNB-COWNCOY        TO LETRQST-CLNTCOY.              <081>
           MOVE CHDRLNB-COWNNUM        TO LETRQST-CLNTNUM.              <081>
           MOVE CHDRLNB-DESPNUM        TO LETRQST-DESPNUM.              <EN001>
           MOVE CHDRLNB-CHDRPFX        TO LETRQST-RDOCPFX.              <PCPRT>
           MOVE CHDRLNB-CHDRCOY        TO LETRQST-RDOCCOY.              <PCPRT>
           MOVE CHDRLNB-CHDRNUM        TO LETRQST-RDOCNUM.              <PCPRT>
           MOVE CHDRLNB-TRANNO         TO LETRQST-TRANNO                <PCPRT>
           MOVE SPACES                 TO LETRQST-OTHER-KEYS.           <081>
      **** MOVE SPACES                 TO WSAA-LETOKEYS.        <PCPRT> <081>
      **** MOVE CHDRLNB-CHDRNUM        TO WSAA-LT-CHDRNUM.      <PCPRT> <081>
      **** MOVE ATMD-LANGUAGE          TO WSAA-LT-LANGUAGE.     <PCPRT> <POLSCD>
      **** MOVE WSAA-LETOKEYS          TO LETRQST-OTHER-KEYS.   <PCPRT> <081>
           MOVE ATMD-LANGUAGE          TO LETRQST-OTHER-KEYS.           <N003>
           MOVE CHDRLNB-CHDRPFX        TO LETRQST-RDOCPFX.              <LIF2.1>
           MOVE CHDRLNB-CHDRCOY        TO LETRQST-RDOCCOY.              <LIF2.1>
           MOVE CHDRLNB-CHDRNUM        TO LETRQST-RDOCNUM.              <LIF2.1>
           MOVE CHDRLNB-CHDRCOY        TO LETRQST-CHDRCOY.              <N003>
           MOVE CHDRLNB-CHDRNUM        TO LETRQST-CHDRNUM.              <N003>
           MOVE CHDRLNB-CNTBRANCH      TO LETRQST-BRANCH.               <PCPRT>
                                                                        <P002>
           MOVE SPACE                  TO WSAA-LETOKEYS.                <P002>
           MOVE ATMD-LANGUAGE          TO WSAA-OKEY-LANGUAGE.           <P002>
           MOVE CHDRLNB-TRANNO         TO WSAA-OKEY-TRANNO.             <P002>
           MOVE WSAA-LETOKEYS          TO LETRQST-OTHER-KEYS.           <P002>
                                                                        <P002>
           MOVE 'ADD'                  TO LETRQST-FUNCTION.             <081>
      *                                                                 <081>
      **** IF  T6634-LETTER-TYPE       NOT = SPACES             <PCPPRT><N003>
           IF  TR384-LETTER-TYPE       NOT = SPACES                     <PCPPRT>
                                                                        <081>
      ****     CALL 'LETRQST' USING LETRQST-PARAMS              <N003>  <081>
      ****     CALL 'HLETRQS' USING LETRQST-PARAMS              <PCPRT  <081>
               CALL 'LETRQST' USING LETRQST-PARAMS                      <PCPPRT>
      *                                                                 <081>
               IF  LETRQST-STATUZ          NOT = O-K                    <081>
                   MOVE LETRQST-PARAMS     TO SYSR-PARAMS               <081>
                   MOVE LETRQST-STATUZ     TO SYSR-STATUZ               <081>
                   PERFORM XXXX-FATAL-ERROR                             <081>
               END-IF                                                   <081>
           END-IF.                                                      <081>
      *                                                                 <081>
      *                                                                 <N003>
       5099-EXIT.                                                       <081>
           EXIT.                                                        <081>
      /                                                                 <081>
       6000-PROC-CHILD SECTION.                                         <V71L05>
      *************************                                         <V71L05>
       6010-PARA.                                                       <V71L05>
                                                                        <V71L05>
           IF WSAA-LIFCNUM              = CHDRLNB-COWNNUM               <V71L05>
               GO TO 6090-EXIT                                          <V71L05>
           END-IF.                                                      <V71L05>
                                                                        <V71L05>
      *                                                                 <V71L05>
      *  Read TR627 to get the attained age                             <V71L05>
      *                                                                 <V71L05>
           INITIALIZE                     ITEM-PARAMS.                  <V71L05>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <V71L05>
           MOVE ATMD-COMPANY           TO ITEM-ITEMCOY.                 <V71L05>
           MOVE TR627                  TO ITEM-ITEMTABL.                <V71L05>
           MOVE CHDRLNB-CNTTYPE        TO ITEM-ITEMITEM.                <V71L05>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <V71L05>
           MOVE READR                  TO ITEM-FUNCTION.                <V71L05>
                                                                        <V71L05>
           CALL 'ITEMIO'            USING ITEM-PARAMS.                  <V71L05>
                                                                        <V71L05>
           IF ITEM-STATUZ           NOT = O-K AND MRNF                  <V71L05>
              MOVE ITEM-PARAMS         TO SYSR-PARAMS                   <V71L05>
              MOVE ITEM-STATUZ         TO SYSR-STATUZ                   <V71L05>
              PERFORM XXXX-FATAL-ERROR                                  <V71L05>
           END-IF.                                                      <V71L05>
                                                                        <V71L05>
           IF ITEM-STATUZ               = MRNF                          <V71L05>
              GO TO 6090-EXIT                                           <V71L05>
           END-IF.                                                      <V71L05>
                                                                        <V71L05>
           MOVE ITEM-GENAREA           TO TR627-TR627-REC.              <V71L05>
                                                                        <V71L05>
      *                                                                 <V71L05>
      *  Read Life record to get life number, use life number to get    <V71L05>
      *  client date of birth                                           <V71L05>
      *                                                                 <V71L05>
           INITIALIZE                     LIFELNBREC-KEY-DATA           <V71L05>
                                          LIFELNBREC-NON-KEY-DATA.      <V71L05>
           MOVE ATMD-COMPANY           TO LIFELNB-CHDRCOY.              <V71L05>
           MOVE CHDRLNB-CHDRNUM        TO LIFELNB-CHDRNUM.              <V71L05>
           MOVE '01'                   TO LIFELNB-LIFE.                 <V71L05>
           MOVE '00'                   TO LIFELNB-JLIFE.                <V71L05>
           MOVE LIFELNBREC             TO LIFELNB-FORMAT.               <V71L05>
           MOVE READR                  TO LIFELNB-FUNCTION.             <V71L05>
                                                                        <V71L05>
           CALL 'LIFELNBIO'         USING LIFELNB-PARAMS.               <V71L05>
                                                                        <V71L05>
           IF LIFELNB-STATUZ        NOT = O-K                           <V71L05>
               MOVE LIFELNB-PARAMS     TO SYSR-PARAMS                   <V71L05>
               MOVE LIFELNB-STATUZ     TO SYSR-STATUZ                   <V71L05>
               PERFORM XXXX-FATAL-ERROR                                 <V71L05>
           END-IF.                                                      <V71L05>
                                                                        <V71L05>
           MOVE SPACES                 TO CLTS-PARAMS.                  <V71L05>
           MOVE 'CN'                   TO CLTS-CLNTPFX.                 <V71L05>
           MOVE WSAA-FSU-COY           TO CLTS-CLNTCOY.                 <V71L05>
           MOVE LIFELNB-LIFCNUM        TO CLTS-CLNTNUM.                 <V71L05>
           MOVE READR                  TO CLTS-FUNCTION.                <V71L05>
                                                                        <V71L05>
           CALL 'CLTSIO'            USING CLTS-PARAMS.                  <V71L05>
                                                                        <V71L05>
           IF CLTS-STATUZ           NOT = O-K                           <V71L05>
              MOVE CLTS-PARAMS         TO SYSR-PARAMS                   <V71L05>
              MOVE CLTS-STATUZ         TO SYSR-STATUZ                   <V71L05>
              PERFORM XXXX-FATAL-ERROR                                  <V71L05>
           END-IF.                                                      <V71L05>
                                                                        <V71L05>
      *                                                                 <V71L05>
      *   Get Sufficient Date by adding the Date of Birth and TR627     <V71L05>
      *   Self-sufficient Age                                           <V71L05>
      *   Use this Sufficient Date if TR627-EAAGE is Exact              <V71L05>
      *                                                                 <V71L05>
           INITIALIZE                     DTC2-DATCON2-REC.             <V71L05>
           MOVE CLTS-CLTDOB            TO DTC2-INT-DATE-1.              <V71L05>
           MOVE '01'                   TO DTC2-FREQUENCY.               <V71L05>
           MOVE TR627-ZSUFCAGE         TO DTC2-FREQ-FACTOR.             <V71L05>
           CALL 'DATCON2'            USING DTC2-DATCON2-REC.            <V71L05>
           IF DTC2-STATUZ            NOT = O-K                          <V71L05>
              MOVE DTC2-DATCON2-REC     TO SYSR-PARAMS                  <V71L05>
              MOVE DTC2-STATUZ          TO SYSR-STATUZ                  <V71L05>
              MOVE 'DATCON2'            TO SYSR-SUBRNAME                <V71L05>
              PERFORM XXXX-FATAL-ERROR                                  <V71L05>
           END-IF.                                                      <V71L05>
                                                                        <V71L05>
           MOVE DTC2-INT-DATE-2        TO WSAA-DOB-PLUS-TR627.          <V71L05>
                                                                        <V71L05>
           IF TR627-EAAGE               = 'E'                           <V71L05>
              MOVE WSAA-DOB-PLUS-TR627 TO WSAA-ZSUFCDTE                 <V71L05>
              GO TO 6090-EXIT                                           <V71L05>
           END-IF.                                                      <V71L05>
                                                                        <V71L05>
      *                                                                 <V71L05>
      *  If TR627-EAAGE is Anniversary, then have to compare the        <V71L05>
      *  above Sufficient Date with policy anniversary date             <V71L05>
      *  Take the Anniversary Date if lesser than the above Sufficient  <V71L05>
      *  Date                                                           <V71L05>
      *                                                                 <V71L05>
           INITIALIZE                     DTC2-DATCON2-REC.             <V71L05>
           MOVE CHDRLNB-OCCDATE        TO DTC2-INT-DATE-1.              <V71L05>
           MOVE '01'                   TO DTC2-FREQUENCY.               <V71L05>
           COMPUTE DTC2-FREQ-FACTOR     =                               <V71L05>
                          TR627-ZSUFCAGE - LIFELNB-ANB-AT-CCD.          <V71L05>
           CALL 'DATCON2'           USING DTC2-DATCON2-REC.             <V71L05>
           IF DTC2-STATUZ           NOT = O-K                           <V71L05>
              MOVE DTC2-DATCON2-REC    TO SYSR-PARAMS                   <V71L05>
              MOVE DTC2-STATUZ         TO SYSR-STATUZ                   <V71L05>
              MOVE 'DATCON2'           TO SYSR-SUBRNAME                 <V71L05>
              PERFORM XXXX-FATAL-ERROR                                  <V71L05>
           END-IF.                                                      <V71L05>
                                                                        <V71L05>
           MOVE DTC2-INT-DATE-2        TO WSAA-ZSUFCDTE.                <V71L05>
                                                                        <V71L05>
           IF WSAA-DOB-PLUS-TR627       > DTC2-INT-DATE-2               <V71L05>
              MOVE DTC2-INT-DATE-2     TO DTC2-INT-DATE-1               <V71L05>
              MOVE '01'                TO DTC2-FREQUENCY                <V71L05>
              MOVE 1                   TO DTC2-FREQ-FACTOR              <V71L05>
              CALL 'DATCON2'        USING DTC2-DATCON2-REC              <V71L05>
              IF DTC2-STATUZ        NOT = O-K                           <V71L05>
                 MOVE DTC2-DATCON2-REC    TO SYSR-PARAMS                <V71L05>
                 MOVE DTC2-STATUZ         TO SYSR-STATUZ                <V71L05>
                 MOVE 'DATCON2'        TO SYSR-SUBRNAME                 <V71L05>
                 PERFORM XXXX-FATAL-ERROR                               <V71L05>
              END-IF                                                    <V71L05>
              MOVE DTC2-INT-DATE-2     TO WSAA-ZSUFCDTE                 <V71L05>
           END-IF.                                                      <V71L05>
      *                                                                 <V71L05>
       6090-EXIT.                                                       <V71L05>
            EXIT.                                                       <V71L05>
      /                                                                 <V71L05>
       6500-WRITE-FPRM-REC SECTION.                                     <D9604>
       6510-START.                                                      <D9604>
                                                                        <D9604>
           INITIALIZE FPRM-DATA-AREA.                                   <D9604>
                                                                        <D9604>
           PERFORM VARYING WSAA-T5729-SUB FROM 1 BY 1                   <D9604>
                   UNTIL WSAA-T5729-SUB > 6                             <D9604>
                   OR T5729-FRQCY(WSAA-T5729-SUB)                       <D9604>
                    = CHDRLNB-BILLFREQ                                  <D9604>
                   CONTINUE                                             <D9604>
           END-PERFORM.                                                 <D9604>
                                                                        <D9604>
           MOVE ZEROS TO WSAA-MIN-OVERDUE.                              <D9604>
                                                                        <D9604>
           EVALUATE WSAA-T5729-SUB                                      <D9604>
              WHEN 1 MOVE      T5729-OVERDUE-MINA-01                    <D9604>
              TO WSAA-MIN-OVERDUE                                       <D9604>
              WHEN 2 MOVE      T5729-OVERDUE-MINB-01                    <D9604>
              TO WSAA-MIN-OVERDUE                                       <D9604>
              WHEN 3 MOVE      T5729-OVERDUE-MINC-01                    <D9604>
              TO WSAA-MIN-OVERDUE                                       <D9604>
              WHEN 4 MOVE      T5729-OVERDUE-MIND-01                    <D9604>
              TO WSAA-MIN-OVERDUE                                       <D9604>
              WHEN 5 MOVE      T5729-OVERDUE-MINE-01                    <D9604>
              TO WSAA-MIN-OVERDUE                                       <D9604>
              WHEN 6 MOVE      T5729-OVERDUE-MINF-01                    <D9604>
              TO WSAA-MIN-OVERDUE                                       <D9604>
              WHEN OTHER MOVE T5729 TO SYSR-PARAMS                      <D9604>
                         PERFORM XXXX-FATAL-ERROR                       <D9604>
           END-EVALUATE.                                                <D9604>
                                                                        <D9604>
           COMPUTE FPRM-MIN-PRM-REQD ROUNDED =                          <D9604>
                   WSAA-MIN-OVERDUE                                     <D9604>
                 / 100                                                  <D9604>
                 * WSAA-REG-PREM-ADJ(WSBB-SUB)                          <D9604>
           END-COMPUTE.                                                 <D9604>
                                                                        <D9604>
           MOVE FPRM-MIN-PRM-REQD      TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO FPRM-MIN-PRM-REQD.            <V76F06>
                                                                        <V76F06>
           MOVE CHDRLNB-CHDRCOY        TO FPRM-CHDRCOY.                 <D9604>
           MOVE CHDRLNB-CHDRNUM        TO FPRM-CHDRNUM.                 <D9604>
           MOVE PAYR-PAYRSEQNO         TO FPRM-PAYRSEQNO.               <D9604>
           MOVE 1                      TO FPRM-VALIDFLAG.               <D9604>
           MOVE CHDRLNB-OCCDATE        TO FPRM-CURRFROM.                <D9604>
           MOVE VRCM-MAX-DATE          TO FPRM-CURRTO.                  <D9604>
           MOVE WSAA-REG-PREM-ADJ(WSBB-SUB)                             <D9604>
                                       TO FPRM-TOTAL-RECD               <D9604>
                                          FPRM-TOTAL-BILLED.            <D9604>
           MOVE CHDRLNB-TRANNO         TO FPRM-TRANNO.                  <D9604>
           MOVE CHDRLNB-USER-PROFILE   TO FPRM-USER-PROFILE.            <D9604>
           MOVE CHDRLNB-JOB-NAME       TO FPRM-JOB-NAME.                <D9604>
           MOVE CHDRLNB-DATIME         TO FPRM-DATIME.                  <D9604>
                                                                        <D9604>
           MOVE FPRMREC                TO FPRM-FORMAT.                  <D9604>
           MOVE WRITR                  TO FPRM-FUNCTION.                <D9604>
           CALL 'FPRMIO'               USING FPRM-PARAMS.               <D9604>
           IF FPRM-STATUZ              NOT = O-K                        <D9604>
              MOVE FPRM-PARAMS         TO SYSR-PARAMS                   <D9604>
              MOVE FPRM-STATUZ         TO SYSR-STATUZ                   <D9604>
              PERFORM XXXX-FATAL-ERROR                                  <D9604>
           END-IF.                                                      <D9604>
                                                                        <D9604>
       6590-EXIT.                                                       <D9604>
           EXIT.                                                        <D9604>
                                                                        <D9604>
       7000-WRITE-FPCO SECTION.                                         <D9604>
       7010-START.                                                      <D9604>
                                                                        <D9604>
           MOVE SPACES                 TO FPCO-CHDRCOY                  <D9604>
                                          FPCO-CHDRNUM                  <D9604>
                                          FPCO-LIFE                     <D9604>
                                          FPCO-COVERAGE                 <D9604>
                                          FPCO-RIDER                    <D9604>
                                          FPCO-ACTIVE-IND.              <D9604>
           MOVE ZEROS                  TO FPCO-PLAN-SUFFIX              <D9604>
                                          FPCO-CURRFROM                 <D9604>
                                          FPCO-TARGFROM.                <D9604>
                                                                        <D9604>
           MOVE CHDRLNB-CHDRCOY        TO FPCO-CHDRCOY.                 <D9604>
           MOVE CHDRLNB-CHDRNUM        TO FPCO-CHDRNUM.                 <D9604>
           MOVE COVRLNB-LIFE           TO FPCO-LIFE.                    <D9604>
           MOVE COVRLNB-JLIFE          TO FPCO-JLIFE.                   <D9604>
           MOVE COVRLNB-COVERAGE       TO FPCO-COVERAGE.                <D9604>
           MOVE COVRLNB-RIDER          TO FPCO-RIDER.                   <D9604>
           MOVE COVRLNB-PLAN-SUFFIX    TO FPCO-PLAN-SUFFIX.             <D9604>
                                                                        <D9604>
           MOVE 1                      TO FPCO-VALIDFLAG.               <D9604>
           MOVE CHDRLNB-OCCDATE        TO FPCO-CURRFROM                 <D9604>
                                          FPCO-TARGFROM                 <D9604>
                                          FPCO-EFFDATE.                 <D9604>
           MOVE VRCM-MAX-DATE          TO FPCO-CURRTO.                  <D9604>
      *                                                         <D9604>
      * Calculate TARGTO as OCCDATE + 1 year                    <D9604> <D9604>
                                                                        <D9604>
           MOVE '01'                    TO DTC2-FREQUENCY.              <D9604>
           MOVE CHDRLNB-OCCDATE         TO DTC2-INT-DATE-1.             <D9604>
           MOVE 1                       TO DTC2-FREQ-FACTOR.            <D9604>
           CALL 'DATCON2'               USING DTC2-DATCON2-REC.         <D9604>
                                                                        <D9604>
           IF DTC2-STATUZ               NOT = O-K                       <D9604>
              MOVE DTC2-DATCON2-REC     TO SYSR-PARAMS                  <D9604>
              MOVE DTC2-STATUZ          TO SYSR-STATUZ                  <D9604>
              PERFORM XXXX-FATAL-ERROR                                  <D9604>
           END-IF.                                                      <D9604>
                                                                        <D9604>
           MOVE DTC2-INT-DATE-2        TO FPCO-TARGTO.                  <D9604>
                                                                        <D9604>
           MOVE FPCO-TARGTO            TO WSAA-ANNIV-EXTRACT-DATE.      <A06557>
                                                                        <A06557>
           MOVE WSAA-BILLCD(WSBB-SUB)  TO WSAA-BILLING-DATE.            <A06557>
                                                                        <A06557>
           IF  (WSAA-BILL-DATE-MNTH    < WSAA-ANNIV-EXTRACT-MNTH)       <A06557>
           OR  (WSAA-BILL-DATE-DAY     < WSAA-ANNIV-EXTRACT-DAY         <A06557>
           AND WSAA-BILL-DATE-MNTH NOT > WSAA-ANNIV-EXTRACT-MNTH)       <A06557>
               MOVE WSAA-BILL-DATE-DAY TO WSAA-ANNIV-EXTRACT-DAY        <A06557>
               MOVE WSAA-BILL-DATE-MNTH TO WSAA-ANNIV-EXTRACT-MNTH      <A06557>
           END-IF.                                                      <A06557>
                                                                        <A06557>
           MOVE WSAA-ANNIV-EXTRACT-DATE TO FPCO-ANNIV-PROC-DATE.        <A06557>
                                                                        <A06557>
           MOVE 'Y'                    TO FPCO-ACTIVE-IND.              <D9604>
                                                                        <D9604>
           MOVE WSAA-BILLFREQ(WSBB-SUB) TO WSAA-FLEX-PREM-FQ.           <D9604>
                                                                        <D9604>
           COMPUTE FPCO-TARGET-PREMIUM =                                <D9604>
                                 COVRLNB-INSTPREM                       <D9604>
                               * WSAA-FLEX-PREM-FQ                      <D9604>
           END-COMPUTE.                                                 <D9604>
                                                                        <D9604>
           IF WSAA-FREQ-FACTOR(WSBB-SUB) = ZEROS                        <D9604>
               MOVE ZEROS              TO FPCO-PREM-REC-PER             <D9604>
               MOVE ZEROS              TO FPCO-BILLED-IN-PERIOD         <D9604>
           ELSE                                                         <D9604>
               COMPUTE FPCO-PREM-REC-PER = COVRLNB-INSTPREM *           <D9604>
                       WSAA-FREQ-FACTOR (WSBB-SUB)                      <D9604>
               COMPUTE FPCO-BILLED-IN-PERIOD  = COVRLNB-INSTPREM *      <D9604>
                       WSAA-FREQ-FACTOR (WSBB-SUB)                      <D9604>
           END-IF.                                                      <D9604>
                                                                        <D9604>
           PERFORM VARYING WSAA-T5729-SUB FROM 1 BY 1                   <D9604>
                   UNTIL WSAA-T5729-SUB > 6                             <D9604>
                   OR T5729-FRQCY(WSAA-T5729-SUB)                       <D9604>
                    = WSAA-BILLFREQ(WSBB-SUB)                           <D9604>
                   CONTINUE                                             <D9604>
           END-PERFORM.                                                 <D9604>
                                                                        <D9604>
           MOVE ZEROS TO WSAA-MIN-OVERDUE.                              <D9604>
                                                                        <D9604>
           EVALUATE WSAA-T5729-SUB                                      <D9604>
              WHEN 1 MOVE      T5729-OVERDUE-MINA-01                    <D9604>
              TO WSAA-MIN-OVERDUE                                       <D9604>
              WHEN 2 MOVE      T5729-OVERDUE-MINB-01                    <D9604>
              TO WSAA-MIN-OVERDUE                                       <D9604>
              WHEN 3 MOVE      T5729-OVERDUE-MINC-01                    <D9604>
              TO WSAA-MIN-OVERDUE                                       <D9604>
              WHEN 4 MOVE      T5729-OVERDUE-MIND-01                    <D9604>
              TO WSAA-MIN-OVERDUE                                       <D9604>
              WHEN 5 MOVE      T5729-OVERDUE-MINE-01                    <D9604>
              TO WSAA-MIN-OVERDUE                                       <D9604>
              WHEN 6 MOVE      T5729-OVERDUE-MINF-01                    <D9604>
              TO WSAA-MIN-OVERDUE                                       <D9604>
              WHEN OTHER MOVE T5729 TO SYSR-PARAMS                      <D9604>
                         PERFORM XXXX-FATAL-ERROR                       <D9604>
           END-EVALUATE.                                                <D9604>
                                                                        <D9604>
           MOVE WSAA-MIN-OVERDUE    TO FPCO-MIN-OVERDUE-PER.            <D9604>
           COMPUTE FPCO-OVERDUE-MIN  ROUNDED =                          <D9604>
                   WSAA-MIN-OVERDUE                                     <D9604>
                 / 100                                                  <D9604>
                 * (COVRLNB-INSTPREM *                                  <D9604>
                       WSAA-FREQ-FACTOR (WSBB-SUB))                     <D9604>
           END-COMPUTE.                                                 <D9604>
                                                                        <D9604>
           MOVE FPCO-OVERDUE-MIN       TO ZRDP-AMOUNT-IN.               <V76F06>
           MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           PERFORM 9000-CALL-ROUNDING.                                  <V76F06>
           MOVE ZRDP-AMOUNT-OUT        TO FPCO-OVERDUE-MIN.             <V76F06>
                                                                        <V76F06>
           MOVE SPACES                 TO FPCO-ANN-PROCESS-IND.         <D9604>
           MOVE CHDRLNB-TRANNO         TO FPCO-TRANNO.                  <D9604>
                                                                        <D9604>
           MOVE FPCOREC                TO FPCO-FORMAT.                  <D9604>
           MOVE WRITR                  TO FPCO-FUNCTION.                <D9604>
           CALL 'FPCOIO'               USING FPCO-PARAMS.               <D9604>
           IF FPCO-STATUZ              NOT = O-K                        <D9604>
              MOVE FPCO-PARAMS         TO SYSR-PARAMS                   <D9604>
              MOVE FPCO-STATUZ         TO SYSR-STATUZ                   <D9604>
              PERFORM XXXX-FATAL-ERROR                                  <D9604>
           END-IF.                                                      <D9604>
                                                                        <D9604>
       7090-EXIT.                                                       <D9604>
           EXIT.                                                        <D9604>
      /                                                                 <DRY001>
       8000-DRY-PROCESSING SECTION.                                     <DRY001>
      *****************************                                     <DRY001>
       8010-START.                                                      <DRY001>
      *                                                                 <DRY001>
      * This section will determine if the DIARY system is present      <DRY001>
      * If so, the appropriate parameters are filled and the            <DRY001>
      * diary processor is called.                                      <DRY001>
      *                                                                 <DRY001>
                                                                        <DRY001>
           MOVE CHDRLNB-CNTTYPE        TO WSAA-T7508-CNTTYPE.           <DRY001>
           MOVE WSKY-BATC-BATCTRCDE    TO WSAA-T7508-BATCTRCDE.         <DRY001>
           PERFORM 8100-READ-T7508.                                     <DRY001>
                                                                        <DRY001>
      *                                                                 <DRY001>
      * If item not found try other types of contract.                  <DRY001>
      *                                                                 <DRY001>
           IF ITEM-STATUZ               = MRNF                          <DRY001>
               MOVE '***'             TO WSAA-T7508-CNTTYPE             <DRY001>
               PERFORM 8100-READ-T7508                                  <DRY001>
           END-IF.                                                      <DRY001>
                                                                        <DRY001>
      *                                                                 <DRY001>
      * If item not found no Diary Update Processing is Required.       <DRY001>
      *                                                                 <DRY001>
           IF  ITEM-STATUZ              = MRNF                          <DRY001>
               GO TO 8090-EXIT                                          <DRY001>
           END-IF.                                                      <DRY001>
                                                                        <DRY001>
           MOVE ITEM-GENAREA           TO T7508-T7508-REC.              <DRY001>
                                                                        <DRY001>
           MOVE O-K                    TO DRYP-STATUZ.                  <DRY001>
           SET  ONLINE-MODE            TO TRUE.                         <DRY001>
           MOVE DTC1-INT-DATE          TO DRYP-RUN-DATE.                <DRY001>
           MOVE WSKY-BATC-BATCCOY      TO DRYP-COMPANY.                 <DRY001>
           MOVE WSKY-BATC-BATCBRN      TO DRYP-BRANCH.                  <DRY001>
           MOVE ATMD-LANGUAGE          TO DRYP-LANGUAGE.                <DRY001>
           MOVE WSKY-BATC-KEY          TO DRYP-BATCH-KEY.               <DRY001>
           MOVE T7508-DRYENTTP-01      TO DRYP-ENTITY-TYPE.             <DRY001>
           MOVE T7508-PROCES-01        TO DRYP-PROC-CODE.               <DRY001>
           MOVE WSAA-PRIMARY-CHDRNUM   TO DRYP-ENTITY.                  <DRY001>
           MOVE DTC1-INT-DATE          TO DRYP-EFFECTIVE-DATE.          <DRY001>
           MOVE ZEROES                 TO DRYP-EFFECTIVE-TIME.          <DRY001>
           MOVE WSAA-FSU-COY           TO DRYP-FSU-COMPANY.             <DRY001>
           MOVE 100                    TO DRYP-PROC-SEQ-NO.             <DRY001>
           MOVE ZERO                   TO DRYP-APLSUPTO.                <DRYAP2>
           MOVE CHDRLNB-STATEMENT-DATE TO DRYP-STMDTE.                  <DRYAP2>
      *                                                                 <DRY001>
      * Fill the parameters.                                            <DRY001>
      *                                                                 <DRY001>
           MOVE CHDRLNB-TRANNO         TO DRYP-TRANNO.                  <DRYAPL>
           MOVE CHDRLNB-BILLCHNL       TO DRYP-BILLCHNL.                <DRYAPL>
           MOVE CHDRLNB-BILLFREQ       TO DRYP-BILLFREQ.                <DRYAPL>
           MOVE CHDRLNB-STATCODE       TO DRYP-STATCODE.                <DRYAPL>
           MOVE CHDRLNB-PSTATCODE      TO DRYP-PSTATCODE.               <DRYAPL>
           MOVE CHDRLNB-BTDATE         TO DRYP-BTDATE.                  <DRYAPL>
           MOVE CHDRLNB-PTDATE         TO DRYP-PTDATE.                  <DRYAPL>
           MOVE CHDRLNB-BILLCD         TO DRYP-BILLCD.                  <DRYAPL>
           MOVE CHDRLNB-CNTTYPE        TO DRYP-CNTTYPE.                 <DRYAPL>
           MOVE COVRLNB-CPI-DATE       TO DRYP-CPI-DATE.                <DRYAPL>
           MOVE COVRLNB-BEN-BILL-DATE  TO DRYP-BBLDATE.                 <DRYAPL>
           MOVE CHDRLNB-OCCDATE        TO DRYP-OCCDATE.                 <DRYAPL>
           MOVE VRCM-MAX-DATE          TO DRYP-CERTDATE.                <DRYAPL>
                                                                        <DRYAPL>
           MOVE CHDRLNB-STATEMENT-DATE TO DRYP-STMDTE.                  <DRYAPL>
           MOVE WSAA-EARLIEST-RCESDTE  TO DRYP-RCESDTE.                 <DRYAPL>
           MOVE WSAA-EARLIEST-CRRCD    TO DRYP-CBUNST.                  <DRYAPL>
      *                                                                 <DRYAPL>
           IF  FLEXIBLE-PREMIUM-CONTRACT                                <DRYAPL>
               MOVE FPCO-TARGTO        TO DRYP-TARGTO                   <DRYAPL>
                                          DRYP-CPI-DATE                 <DRYAPL>
           ELSE                                                         <DRYAPL>
               MOVE VRCM-MAX-DATE      TO DRYP-TARGTO                   <DRYAPL>
           END-IF.                                                      <DRYAPL>
      *                                                                 <DRY001>
           CALL 'DRYPROCES'         USING DRYP-DRYPRC-REC.              <DRY001>
      *                                                                 <DRY001>
           IF DRYP-STATUZ           NOT = O-K                           <DRY001>
               MOVE DRYP-DRYPRC-REC    TO SYSR-PARAMS                   <DRY001>
               MOVE DRYP-STATUZ        TO SYSR-STATUZ                   <DRY001>
               PERFORM XXXX-FATAL-ERROR                                 <DRY001>
           END-IF.                                                      <DRY001>
                                                                        <DRY001>
       8090-EXIT.                                                       <DRY001>
           EXIT.                                                        <DRY001>
                                                                        <DRY001>
       8100-READ-T7508 SECTION.                                         <DRY001>
      *************************                                         <DRY001>
       8110-START.                                                      <DRY001>
                                                                        <DRY001>
           MOVE SPACES                 TO ITEM-DATA-KEY.                <DRY001>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <DRY001>
           MOVE ATMD-COMPANY           TO ITEM-ITEMCOY.                 <DRY001>
           MOVE T7508                  TO ITEM-ITEMTABL.                <DRY001>
           MOVE WSAA-T7508-KEY         TO ITEM-ITEMITEM.                <DRY001>
                                                                        <DRY001>
           MOVE READR                  TO ITEM-FUNCTION.                <DRY001>
                                                                        <DRY001>
           CALL 'ITEMIO'            USING ITEM-PARAMS.                  <DRY001>
                                                                        <DRY001>
           IF  ITEM-STATUZ          NOT = O-K                           <DRY001>
           AND ITEM-STATUZ          NOT = MRNF                          <DRY001>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <DRY001>
               MOVE ITEM-STATUZ        TO SYSR-STATUZ                   <DRY001>
               PERFORM XXXX-FATAL-ERROR                                 <DRY001>
           END-IF.                                                      <DRY001>
                                                                        <DRY001>
       8990-EXIT.                                                       <DRY001>
           EXIT.                                                        <DRY001>
                                                                        <D9604>
      /                                                                 <V76F06>
       9000-CALL-ROUNDING SECTION.                                      <V76F06>
      ****************************                                      <V76F06>
       9100-CALL.                                                       <V76F06>
      *                                                                 <V76F06>
           MOVE SPACES                 TO ZRDP-FUNCTION                 <V76F06>
           MOVE ATMD-COMPANY           TO ZRDP-COMPANY.                 <V76F06>
           MOVE O-K                    TO ZRDP-STATUZ.                  <V76F06>
      *    MOVE CHDRLNB-CNTCURR        TO ZRDP-CURRENCY.                <V76F06>
           MOVE WSKY-BATC-BATCTRCDE    TO ZRDP-BATCTRCDE.               <V76F06>
                                                                        <V76F06>
           CALL 'ZRDECPLC'             USING ZRDP-ZRDECPL-REC.          <V76F06>
                                                                        <V76F06>
           IF  ZRDP-STATUZ             NOT = O-K                        <V76F06>
               MOVE ZRDP-STATUZ        TO SYSR-STATUZ                   <V76F06>
               MOVE ZRDP-ZRDECPL-REC   TO SYSR-PARAMS                   <V76F06>
               PERFORM XXXX-FATAL-ERROR                                 <V76F06>
           END-IF.                                                      <V76F06>
                                                                        <V76F06>
        9900-EXIT.                                                      <V76F06>
            EXIT.                                                       <V76F06>
      /                                                                 <V76F06>
       A000-STATISTICS SECTION.
      ************************
       A010-START.
      *
           MOVE WSKY-BATC-BATCCOY      TO LIFS-BATCCOY.
           MOVE WSKY-BATC-BATCBRN      TO LIFS-BATCBRN.
           MOVE WSKY-BATC-BATCACTYR    TO LIFS-BATCACTYR.
           MOVE WSKY-BATC-BATCACTMN    TO LIFS-BATCACTMN.
           MOVE WSKY-BATC-BATCTRCDE    TO LIFS-BATCTRCDE.
           MOVE WSKY-BATC-BATCBATCH    TO LIFS-BATCBATCH.
      *
           MOVE CHDRLNB-CHDRCOY        TO LIFS-CHDRCOY.
           MOVE CHDRLNB-CHDRNUM        TO LIFS-CHDRNUM.
           MOVE COVRLNB-TRANNO         TO LIFS-TRANNO.
           MOVE 99999                  TO LIFS-TRANNOR.
           MOVE SPACES                 TO LIFS-AGNTNUM.
           MOVE SPACES                 TO LIFS-OLD-AGNTNUM.
      *
           CALL 'LIFSTTR'              USING LIFS-LIFSTTR-REC.
      *
           IF LIFS-STATUZ              NOT = O-K
              MOVE LIFS-LIFSTTR-REC    TO SYSR-PARAMS
              MOVE LIFS-STATUZ         TO SYSR-STATUZ
              PERFORM XXXX-FATAL-ERROR.
      *
       A090-EXIT.
           EXIT.
      *                                                                 <LA2110>
       A100-WOP-DATES SECTION.                                          <LA2110>
      ************************                                          <LA2110>
       A110-WOP.                                                        <LA2110>
      *                                                                 <LA2110>
            MOVE SPACES                TO COVRTRB-PARAMS.               <LA2110>
            MOVE CHDRLNB-CHDRCOY       TO COVRTRB-CHDRCOY.              <LA2110>
            MOVE CHDRLNB-CHDRNUM       TO COVRTRB-CHDRNUM.              <LA2110>
            MOVE '01'                  TO COVRTRB-LIFE.                 <LA2110>
            MOVE '01'                  TO COVRTRB-COVERAGE.             <LA2110>
            MOVE '00'                  TO COVRTRB-RIDER.                <LA2110>
            MOVE ZEROES                TO COVRTRB-PLAN-SUFFIX.          <LA2110>
                                                                        <LA2110>
            MOVE COVRTRBREC            TO COVRTRB-FORMAT.               <LA2110>
            MOVE BEGNH                 TO COVRTRB-FUNCTION.             <LA2110>
                                                                        <LA2110>
            PERFORM A200-READ-COVRTRB  UNTIL COVRTRB-STATUZ = ENDP.     <LA2110>
                                                                        <LA2110>
      *                                                                 <LA2110>
       A190-EXIT.                                                       <LA2110>
           EXIT.                                                        <LA2110>
      *                                                                 <LA2110>
       A200-READ-COVRTRB SECTION.                                       <LA2110>
      ****************************                                      <LA2110>
       A210-COVRTRB.                                                    <LA2110>
           PERFORM A270-COVRTRBIO.                                      <LA2110>
                                                                        <LA2110>
           IF COVRTRB-CHDRCOY       NOT = CHDRLNB-CHDRCOY   OR          <LA2110>
              COVRTRB-CHDRNUM       NOT = CHDRLNB-CHDRNUM   OR          <LA2110>
              COVRTRB-STATUZ            = ENDP                          <LA2110>
              MOVE ENDP                TO COVRTRB-STATUZ                <LA2110>
              GO TO A230-EXIT                                           <LA2110>
           END-IF.                                                      <LA2110>
                                                                        <LA2110>
           PERFORM A300-READ-TR517.                                     <LA2110>
           MOVE 'N'                    TO WSAA-CPI-VALID.               <LA2110>
           IF WOP-MATCH                                                 <LA2110>
              MOVE TR517-TR517-REC     TO WSAA-TR517-REC                <TN01>
              IF  COVRTRB-CPI-DATE      > COVRTRB-RERATE-DATE           <LA2110>
                 MOVE COVRTRB-RERATE-DATE                               <LA2110>
                                       TO WSAA-EARLIEST-RERATE-DATE     <LA2110>
              ELSE                                                      <LA2110>
                 MOVE COVRTRB-CPI-DATE                                  <LA2110>
                                       TO WSAA-EARLIEST-RERATE-DATE     <LA2110>
              END-IF                                                    <LA2110>
                                                                        <LA2110>
      *                                                                 <LA2110>
      * Determine if the WOP is life insured specific.                  <LA2110>
      *                                                                 <LA2110>
              IF TR517-ZRWVFLG-02 NOT  = 'Y'                            <LA2110>
                 MOVE SPACES          TO COVRLNB-PARAMS                 <LA2110>
                 MOVE COVRTRB-CHDRCOY TO COVRLNB-CHDRCOY                <LA2110>
                 MOVE COVRTRB-CHDRNUM TO COVRLNB-CHDRNUM                <LA2110>
                 MOVE COVRTRB-LIFE    TO COVRLNB-LIFE                   <LA2110>
                 MOVE 0               TO COVRLNB-PLAN-SUFFIX            <LA2110>
                                                                        <LA2110>
                 MOVE COVRLNBREC      TO COVRLNB-FORMAT                 <LA2110>
                 MOVE BEGN            TO COVRLNB-FUNCTION               <LA2110>
                                                                        <LA2110>
                 PERFORM A400-SET-WOP-DATE                              <LA2110>
                         UNTIL COVRLNB-CHDRCOY NOT = COVRTRB-CHDRCOY    <LA2110>
                            OR COVRLNB-CHDRNUM NOT = COVRTRB-CHDRNUM    <LA2110>
                            OR COVRLNB-LIFE    NOT = COVRTRB-LIFE       <LA2110>
                            OR COVRLNB-STATUZ      = ENDP               <LA2110>
                 IF WSAA-EARLIEST-RERATE-DATE  NOT = 0                  <LA2110>
                    PERFORM A240-REWRITE-COVRTRB                        <LA2110>
                 END-IF                                                 <LA2110>
              ELSE                                                      <LA2110>
                 MOVE SPACES          TO COVRLNB-PARAMS                 <LA2110>
                 MOVE COVRTRB-CHDRCOY TO COVRLNB-CHDRCOY                <LA2110>
                 MOVE COVRTRB-CHDRNUM TO COVRLNB-CHDRNUM                <LA2110>
                 MOVE 0               TO COVRLNB-PLAN-SUFFIX            <LA2110>
                                                                        <LA2110>
                 MOVE COVRLNBREC      TO COVRLNB-FORMAT                 <LA2110>
                 MOVE BEGN            TO COVRLNB-FUNCTION               <LA2110>
                                                                        <LA2110>
                 PERFORM A400-SET-WOP-DATE                              <LA2110>
                         UNTIL COVRLNB-CHDRCOY NOT = COVRTRB-CHDRCOY    <LA2110>
                            OR COVRLNB-CHDRNUM NOT = COVRTRB-CHDRNUM    <LA2110>
                            OR COVRLNB-STATUZ      = ENDP               <LA2110>
                 IF WSAA-EARLIEST-RERATE-DATE  NOT = 0                  <LA2110>
                    PERFORM A240-REWRITE-COVRTRB                        <LA2110>
                 END-IF                                                 <LA2110>
              END-IF                                                    <LA2110>
           END-IF.                                                      <LA2110>
      *                                                                 <LA2110>
       A220-NEXT.                                                       <LA2110>
           MOVE NEXTR                  TO COVRTRB-FUNCTION.             <LA2110>
      *                                                                 <LA2110>
       A230-EXIT.                                                       <LA2110>
           EXIT.                                                        <LA2110>
      *                                                                 <LA2110>
       A240-REWRITE-COVRTRB SECTION.                                    <LA2110>
      ******************************                                    <LA2110>
       A250-REWRITE.                                                    <LA2110>
      *                                                                 <LA2110>
           MOVE WSAA-EARLIEST-RERATE-DATE                               <LA2110>
                                       TO COVRTRB-RERATE-DATE.          <LA2110>
                                                                        <LA2110>
           IF CPI-VALID                                                 <LA2110>
               MOVE WSAA-EARLIEST-RERATE-DATE                           <LA2110>
                                       TO COVRTRB-CPI-DATE.             <LA2110>
                                                                        <LA2110>
           MOVE UPDAT                  TO COVRTRB-FUNCTION.             <LA2110>
           PERFORM A270-COVRTRBIO.                                      <LA2110>
      *                                                                 <LA2110>
       A260-EXIT.                                                       <LA2110>
           EXIT.                                                        <LA2110>
      *                                                                 <LA2110>
       A270-COVRTRBIO SECTION.                                          <LA2110>
      ************************                                          <LA2110>
       A280-READ-COVRTRB.                                               <LA2110>
      *                                                                 <LA2110>
           CALL 'COVRTRBIO'         USING COVRTRB-PARAMS.               <LA2110>
      *                                                                 <LA2110>
           IF COVRTRB-STATUZ        NOT = O-K        AND                <LA2110>
              COVRTRB-STATUZ        NOT = ENDP                          <LA2110>
              MOVE COVRTRB-STATUZ      TO SYSR-STATUZ                   <LA2110>
              MOVE COVRTRB-PARAMS      TO SYSR-PARAMS                   <LA2110>
              PERFORM XXXX-FATAL-ERROR                                  <LA2110>
           END-IF.                                                      <LA2110>
                                                                        <LA2110>
       A290-EXIT.                                                       <LA2110>
           EXIT.                                                        <LA2110>
                                                                        <LA2110>
       A300-READ-TR517 SECTION.                                         <LA2110>
      *************************                                         <LA2110>
       A310-READ.                                                       <LA2110>
      *                                                                 <LA2110>
      * Read TR517 for Wavier of Premium Component.                     <LA2110>
      *                                                                 <LA2110>
           SET  WOP-NOT-MATCH          TO TRUE.                         <LA2110>
           MOVE SPACES                 TO ITDM-PARAMS.                  <LA2110>
           MOVE 'IT'                   TO ITDM-ITEMPFX.                 <LA2110>
           MOVE COVRTRB-CHDRCOY        TO ITDM-ITEMCOY.                 <LA2110>
           MOVE TR517                  TO ITDM-ITEMTABL.                <LA2110>
           MOVE COVRTRB-CRTABLE        TO ITDM-ITEMITEM.                <LA2110>
           MOVE COVRTRB-CRRCD          TO ITDM-ITMFRM.                  <LA2110>
                                                                        <LA2110>
           MOVE BEGN                   TO ITDM-FUNCTION.                <LA2110>
                                                                        <LA2110>
           CALL 'ITDMIO'           USING  ITDM-PARAMS.                  <LA2110>
                                                                        <LA2110>
           IF  ITDM-STATUZ             NOT = O-K                        <LA2110>
           AND                         NOT = ENDP                       <LA2110>
               MOVE ITDM-STATUZ        TO SYSR-STATUZ                   <LA2110>
               MOVE ITDM-PARAMS        TO SYSR-PARAMS                   <LA2110>
               PERFORM XXXX-FATAL-ERROR                                 <LA2110>
           END-IF.                                                      <LA2110>
                                                                        <LA2110>
           IF  ITDM-STATUZ                 = ENDP                       <LA2110>
           OR  ITDM-ITEMCOY           NOT  = COVRTRB-CHDRCOY            <LA2110>
           OR  ITDM-ITEMTABL          NOT  = TR517                      <LA2110>
           OR  ITDM-ITEMITEM (1:4)    NOT  = COVRTRB-CRTABLE            <LA2110>
               GO TO A390-EXIT                                          <LA2110>
           END-IF.                                                      <LA2110>
                                                                        <LA2110>
           IF  ITDM-STATUZ                 = O-K                        <LA2110>
               SET  WOP-MATCH              TO TRUE                      <LA2110>
               MOVE ITDM-GENAREA           TO TR517-TR517-REC           <LA2110>
           END-IF.                                                      <LA2110>
                                                                        <LA2110>
       A390-EXIT.                                                       <LA2110>
           EXIT.                                                        <LA2110>
                                                                        <LA2110>
       A400-SET-WOP-DATE SECTION.                                       <LA2110>
      ***************************                                       <LA2110>
       A410-SET-WOP.                                                    <LA2110>
      *                                                                 <LA2110>
           CALL 'COVRLNBIO'         USING COVRLNB-PARAMS.               <LA2110>
      *                                                                 <LA2110>
           IF COVRLNB-STATUZ        NOT = O-K        AND                <LA2110>
              COVRLNB-STATUZ        NOT = ENDP                          <LA2110>
              MOVE COVRLNB-STATUZ      TO SYSR-STATUZ                   <LA2110>
              MOVE COVRLNB-PARAMS      TO SYSR-PARAMS                   <LA2110>
              PERFORM XXXX-FATAL-ERROR                                  <LA2110>
           END-IF.                                                      <LA2110>
      *                                                                 <LA3875>
           IF TR517-ZRWVFLG-02 NOT  = 'Y'                               <LA3875>
              IF COVRLNB-CHDRCOY       NOT = COVRTRB-CHDRCOY OR         <LA3875>
                 COVRLNB-CHDRNUM       NOT = COVRTRB-CHDRNUM OR         <LA3875>
                    COVRLNB-LIFE       NOT = COVRTRB-LIFE    OR         <LA3875>
                 COVRLNB-STATUZ            = ENDP                       <LA3875>
                 MOVE ENDP                 TO COVRLNB-STATUZ            <LA3875>
                 GO TO A490-EXIT                                        <LA3875>
              END-IF                                                    <LA3948>
              ELSE                                                      <LA3875>
                 IF COVRLNB-CHDRCOY    NOT = COVRTRB-CHDRCOY OR         <LA3875>
                    COVRLNB-CHDRNUM    NOT = COVRTRB-CHDRNUM OR         <LA3875>
                    COVRLNB-STATUZ         = ENDP                       <LA3875>
                    MOVE ENDP              TO COVRLNB-STATUZ            <LA3875>
                    GO TO A490-EXIT                                     <LA3875>
                 END-IF                                                 <LA3948>
           END-IF.                                                      <LA3875>
                                                                        <LA2110>
           IF COVRLNB-VALIDFLAG     NOT = '1'                           <LA2110>
              GO TO A480-NEXT.                                          <LA2110>
                                                                        <LA2110>
           MOVE 'N'                    TO WSAA-CRTABLE-MATCH.           <LA2110>
           MOVE WSAA-TR517-REC         TO TR517-TR517-REC.              <TN01>
      *                                                                 <TN01>
       A420-CHECK.                                                      <TN01>
           PERFORM VARYING WSAA-CNT FROM 1 BY 1                         <LA2110>
                   UNTIL   WSAA-CNT     > 50                            <LA2110>
                      OR   CRTABLE-MATCH                                <LA2110>
              IF COVRLNB-CRTABLE        = TR517-CTABLE (WSAA-CNT)       <LA2110>
                 SET  CRTABLE-MATCH    TO TRUE                          <LA2110>
              END-IF                                                    <LA2110>
           END-PERFORM.                                                 <LA2110>
           IF  NOT CRTABLE-MATCH                                        <TN01>
           AND TR517-CONTITEM           NOT = SPACES                    <TN01>
               PERFORM A30C-READ-TR517                                  <TN01>
               IF WSAA-WAIVE-CONT       = 'Y'                           <TN01>
                  GO TO A420-CHECK                                      <TN01>
               END-IF                                                   <TN01>
           END-IF.                                                      <TN01>
                                                                        <LA2110>
           IF  CRTABLE-MATCH                                            <LA2110>
           AND (TR517-ZRWVFLG-02        = 'Y'        OR                 <LA2110>
                COVRLNB-LIFE            = COVRTRB-LIFE)                 <LA2110>
                                                                        <PHE003>
               IF TR517-ZRWVFLG-05      = 'Y'                           <PHE003>
               AND COVRLNB-LIFE         = COVRTRB-LIFE                  <PHE003>
                  GO TO A480-NEXT                                       <PHE003>
               END-IF                                                   <PHE003>
                                                                        <PHE003>
               IF COVRLNB-RERATE-DATE   < WSAA-EARLIEST-RERATE-DATE     <LA2110>
               AND COVRLNB-RERATE-DATE  > 0                             <LA2110>
                  MOVE COVRLNB-RERATE-DATE                              <LA2110>
                                       TO WSAA-EARLIEST-RERATE-DATE     <LA2110>
               END-IF                                                   <LA2110>
                                                                        <LA2110>
               IF COVRLNB-CPI-DATE     NOT = VRCM-MAX-DATE              <LA2110>
                  SET CPI-VALID        TO TRUE                          <LA2110>
               END-IF                                                   <LA2110>
                                                                        <LA2110>
               IF COVRLNB-CPI-DATE      < WSAA-EARLIEST-RERATE-DATE     <LA2110>
               AND COVRLNB-CPI-DATE     > 0                             <LA2110>
                  MOVE COVRLNB-CPI-DATE                                 <LA2110>
                                       TO WSAA-EARLIEST-RERATE-DATE     <LA2110>
               END-IF                                                   <LA2110>
           END-IF.                                                      <LA2110>
                                                                        <LA2110>
       A480-NEXT.                                                       <LA2110>
           MOVE NEXTR                  TO COVRLNB-FUNCTION.             <LA2110>
      *                                                                 <LA2110>
       A490-EXIT.                                                       <LA2110>
           EXIT.                                                        <LA2110>
                                                                        <LA2110>
      *                                                         <V4L001><V4L001>
       R200-UPDATE-APA SECTION.                                         <V4L001>
      *************************                                 <V4L001><V4L001>
       R210-LINKUP.                                                     <V4L001>
           MOVE 'PSTW'                 TO RLPDLON-PSTW.                 <V4L001>
           MOVE CHDRLNB-CHDRCOY        TO RLPDLON-CHDRCOY.              <V4L001>
           MOVE CHDRLNB-CHDRNUM        TO RLPDLON-CHDRNUM.              <V4L001>
           MOVE CHDRLNB-OCCDATE        TO RLPDLON-EFFDATE.              <V4L001>
           MOVE ACBLENQ-ORIGCURR       TO RLPDLON-CURRENCY.             <V4L001>
           MOVE CHDRLNB-TRANNO         TO RLPDLON-TRANNO.               <V4L001>
           MOVE WSAA-JRNSEQ            TO RLPDLON-TRANSEQ.              <V4L001>
           MOVE DESC-LONGDESC          TO RLPDLON-LONGDESC.             <V4L001>
           MOVE ATMD-LANGUAGE          TO RLPDLON-LANGUAGE.             <V4L001>
           MOVE ATMD-BATCH-KEY         TO RLPDLON-BATCHKEY.             <V4L001>
           MOVE RLPDLON-TRCDE          TO RLPDLON-AUTH-CODE.            <V4L001>
           MOVE WSAA-TRANSACTION-TIME  TO RLPDLON-TIME.                 <V4L001>
           MOVE WSAA-TRANSACTION-DATE  TO RLPDLON-DATE.                 <V4L001>
           MOVE WSAA-USER              TO RLPDLON-USER.                 <V4L001>
           MOVE WSAA-TERMID            TO RLPDLON-TERMID.               <V4L001>
                                                                        <V4L001>
           CALL 'RLPDLON'              USING RLPDLON-REC.               <V4L001>
                                                                        <V4L001>
           IF RLPDLON-STATUZ           NOT = O-K                        <V4L001>
              MOVE RLPDLON-STATUZ      TO SYSR-STATUZ                   <V4L001>
              MOVE RLPDLON-REC         TO SYSR-PARAMS                   <V4L001>
              PERFORM XXXX-FATAL-ERROR                                  <V4L001>
           END-IF.                                                      <V4L001>
                                                                        <V4L001>
           MOVE RLPDLON-TRANSEQ        TO WSAA-JRNSEQ.                  <V4L001>
                                                                        <V4L001>
       R290-EXIT.                                                       <V4L001>
           EXIT.                                                        <V4L001>
                                                                        <TN01>
       A30C-READ-TR517 SECTION.                                         <TN01>
      *************************                                         <TN01>
       A31C-READ.                                                       <TN01>
      *                                                                 <TN01>
      * Read TR517 for Wavier of Premium Component.                     <TN01>
      *                                                                 <TN01>
           MOVE 'N'                    TO WSAA-WAIVE-CONT.              <TN01>
           MOVE TR517-ZRWVFLGS         TO WSAA-ZRWVFLGS.                <TN01>
           MOVE SPACES                 TO ITDM-PARAMS.                  <TN01>
           MOVE 'IT'                   TO ITDM-ITEMPFX.                 <TN01>
           MOVE COVRTRB-CHDRCOY        TO ITDM-ITEMCOY.                 <TN01>
           MOVE TR517                  TO ITDM-ITEMTABL.                <TN01>
           MOVE TR517-CONTITEM         TO ITDM-ITEMITEM.                <TN01>
           MOVE COVRTRB-CRRCD          TO ITDM-ITMFRM.                  <TN01>
                                                                        <TN01>
           MOVE BEGN                   TO ITDM-FUNCTION.                <TN01>
                                                                        <TN01>
           CALL 'ITDMIO'           USING  ITDM-PARAMS.                  <TN01>
                                                                        <TN01>
           IF  ITDM-STATUZ             NOT = O-K                        <TN01>
           OR  ITDM-ITEMCOY            NOT = COVRTRB-CHDRCOY            <TN01>
           OR  ITDM-ITEMTABL           NOT = TR517                      <TN01>
           OR  ITDM-ITEMITEM           NOT = TR517-CONTITEM             <TN01>
               GO TO A39C-EXIT                                          <TN01>
           END-IF.                                                      <TN01>
                                                                        <TN01>
           MOVE 'Y'                    TO WSAA-WAIVE-CONT.              <TN01>
           MOVE ITDM-GENAREA           TO TR517-TR517-REC.              <TN01>
           MOVE WSAA-ZRWVFLGS          TO TR517-ZRWVFLGS.               <TN01>
                                                                        <TN01>
       A39C-EXIT.                                                       <TN01>
           EXIT.                                                        <TN01>
                                                                        <V42005>
       A500-ADD-UNDERWRITTING SECTION.                                  <V42005>
       A500-CTRL.                                                       <V42005>
                                                                        <V42005>
      **** PERFORM A700-READR-LIFEIO.                                   <V42005>
      **** PERFORM A600-DEL-UNDERWRITTING.                     <LFA1062><V42005>
                                                                        <LFA1062
           INITIALIZE COVRTRBREC-KEY-DATA.                              <LFA1062
           MOVE CHDRLNB-CHDRCOY        TO COVRTRB-CHDRCOY               <LFA1062
           MOVE CHDRLNB-CHDRNUM        TO COVRTRB-CHDRNUM               <LFA1062
           MOVE BEGN                   TO COVRTRB-FUNCTION.             <LFA1062
           MOVE O-K                    TO COVRTRB-STATUZ.               <LFA1062
           PERFORM A600-DEL-UNDERWRITTING                               <LFA1062
             UNTIL COVRTRB-STATUZ NOT = O-K.                            <LFA1062
                                                                        <V42005>
           INITIALIZE COVRTRBREC-KEY-DATA.                              <V42005>
           MOVE CHDRLNB-CHDRCOY        TO COVRTRB-CHDRCOY               <V42005>
           MOVE CHDRLNB-CHDRNUM        TO COVRTRB-CHDRNUM               <V42005>
      **** MOVE LIFE-LIFE              TO COVRTRB-LIFE                  <V42005>
      **** MOVE WSAA-COVERAGE          TO COVRTRB-COVERAGE              <V42005>
      **** MOVE WSAA-RIDER             TO COVRTRB-RIDER                 <V42005>
           MOVE BEGN                   TO COVRTRB-FUNCTION.             <V42005>
           MOVE O-K                    TO COVRTRB-STATUZ.               <V42005>
           PERFORM A800-NEXTR-COVRTRBIO                                 <V42005>
             UNTIL COVRTRB-STATUZ NOT = O-K.                            <V42005>
                                                                        <V42005>
       A500-EXIT.                                                       <V42005>
           EXIT.                                                        <V42005>
                                                                        <V42005>
       A600-DEL-UNDERWRITTING SECTION.                                  <V42005>
       A600-CTRL.                                                       <V42005>
                                                                        <LFA1062
           MOVE COVRTRBREC             TO COVRTRB-FORMAT.               <LFA1062
           CALL 'COVRTRBIO'            USING COVRTRB-PARAMS.            <LFA1062
           IF NOT (COVRTRB-STATUZ      = O-K                            <LFA1062
           AND CHDRLNB-CHDRCOY         = COVRTRB-CHDRCOY                <LFA1062
           AND CHDRLNB-CHDRNUM         = COVRTRB-CHDRNUM)               <LFA1062
               MOVE ENDP               TO COVRTRB-STATUZ                <LFA1062
               GO TO A600-EXIT.                                         <LFA1062
           MOVE NEXTR                  TO COVRTRB-FUNCTION.             <LFA1062
                                                                        <LFA1062
           INITIALIZE UNDW-PARM-REC.                                    <V42005>
           PERFORM A900-READ-LIFE.                                      <LFA1062
    ****** MOVE WSAA-L1-CLNTNUM        TO UNDW-CLNTNUM.        <LFA1062><V42005>
           MOVE LIFELNB-LIFCNUM        TO UNDW-CLNTNUM.                 <LFA1062
           MOVE CHDRLNB-CHDRCOY        TO UNDW-COY.                     <V42005>
           MOVE CHDRLNB-CNTCURR        TO UNDW-CURRCODE.                <V42005>
           MOVE CHDRLNB-CHDRNUM        TO UNDW-CHDRNUM.                 <V42005>
           MOVE ALL '*'                TO UNDW-CRTABLE.                 <V42005>
           MOVE 'DEL'                  TO UNDW-FUNCTION.                <V42005>
           CALL 'CRTUNDWRT'            USING UNDW-PARM-REC.             <V42005>
           IF UNDW-STATUS              NOT = O-K                        <V42005>
              MOVE UNDW-PARM-REC       TO SYSR-PARAMS                   <V42005>
              MOVE UNDW-STATUS         TO SYSR-STATUZ                   <V42005>
              MOVE 'CRTUNDWRT'         TO SYSR-IOMOD                    <V42005>
              PERFORM XXXX-FATAL-ERROR.                                 <V42005>
       A600-EXIT.                                                       <V42005>
           EXIT.                                                        <V42005>
      ****                                                              <V42005>
      *A700-READR-LIFEIO SECTION.                                       <V42005>
      *A700-CTRL.                                                       <V42005>
      **** MOVE CHDRLNB-CHDRCOY        TO LIFE-CHDRCOY                  <V42005>
      **** MOVE CHDRLNB-CHDRNUM        TO LIFE-CHDRNUM                  <V42005>
      **** MOVE WSAA-LIFE              TO LIFE-LIFE.                    <V42005>
      **** MOVE                        TO LIFE-JLIFE                    <V42005>
      **** MOVE                        TO LIFE-CURRFROM                 <V42005>
      **** MOVE READR                  TO LIFE-FUNCTION.                <V42005>
      **** MOVE LIFEREC                TO LIFE-FORMAT.                  <V42005>
      **** CALL 'LIFEIO'               USING LIFE-PARAMS.               <V42005>
      **** IF LIFE-STATUZ              NOT = O-K                        <V42005>
      ****    MOVE LIFE-PARAMS         TO SYSR-PARAMS                   <V42005>
      ****    MOVE LIFE-STATUZ         TO SYSR-STATUZ                   <V42005>
      ****    PERFORM XXXX-FATAL-ERROR.                                 <V42005>
      *A700-EXIT.                                                       <V42005>
      **** EXIT.                                                        <V42005>
                                                                        <V42005>
       A800-NEXTR-COVRTRBIO SECTION.                                    <V42005>
       A800-CTRL.                                                       <V42005>
           MOVE COVRTRBREC             TO COVRTRB-FORMAT.               <V42005>
           CALL 'COVRTRBIO'            USING COVRTRB-PARAMS.            <V42005>
           IF NOT (COVRTRB-STATUZ      = O-K                            <V42005>
           AND CHDRLNB-CHDRCOY         = COVRTRB-CHDRCOY                <V42005>
           AND CHDRLNB-CHDRNUM         = COVRTRB-CHDRNUM)               <V42005>
      **** AND LIFE-LIFE               = COVRTRB-LIFE                   <V42005>
      **** AND WSAA-COVERAGE           = COVRTRB-COVERAGE               <V42005>
      **** AND WSAA-RIDER              = COVRTRB-RIDER)                 <V42005>
               MOVE ENDP               TO COVRTRB-STATUZ                <V42005>
               GO TO A800-EXIT.                                         <V42005>
           MOVE NEXTR                  TO COVRTRB-FUNCTION.             <V42005>
                                                                        <V42005>
           INITIALIZE UNDW-PARM-REC.                                    <V42005>
           PERFORM A900-READ-LIFE.                                      <LFA1062
     ****  MOVE WSAA-L1-CLNTNUM        TO UNDW-CLNTNUM.        <LFA1062><V42005>
           MOVE LIFELNB-LIFCNUM        TO UNDW-CLNTNUM.                 <LFA1062
           MOVE COVRTRB-CHDRCOY        TO UNDW-COY.                     <V42005>
           MOVE COVRTRB-CHDRNUM        TO UNDW-CHDRNUM.                 <V42005>
           MOVE COVRTRB-LIFE           TO UNDW-LIFE.                    <V42005>
           MOVE COVRTRB-CRTABLE        TO UNDW-CRTABLE.                 <V42005>
           MOVE WSKY-BATC-BATCTRCDE    TO UNDW-BATCTRCDE.               <V42005>
           MOVE COVRTRB-SUMINS         TO UNDW-SUMINS.                  <V42005>
           MOVE CHDRLNB-CNTTYPE        TO UNDW-CNTTYP.                  <V42005>
           MOVE CHDRLNB-CNTCURR        TO UNDW-CURRCODE.                <V42005>
           MOVE 'ADD'                  TO UNDW-FUNCTION.                <V42005>
           CALL 'CRTUNDWRT'            USING UNDW-PARM-REC.             <V42005>
           IF UNDW-STATUS              NOT = O-K                        <V42005>
              MOVE UNDW-PARM-REC       TO SYSR-PARAMS                   <V42005>
              MOVE UNDW-STATUS         TO SYSR-STATUZ                   <V42005>
              MOVE 'CRTUNDWRT'         TO SYSR-IOMOD                    <V42005>
              PERFORM XXXX-FATAL-ERROR.                                 <V42005>
       A800-EXIT.                                                       <V42005>
           EXIT.                                                        <V42005>
                                                                        <LFA1062
       A900-READ-LIFE SECTION.                                          <LFA1062
       A900-CTRL.                                                       <LFA1062
                                                                        <LFA1062
           MOVE SPACES                 TO LIFELNB-PARAMS.               <LFA1062
           MOVE ATMD-COMPANY           TO LIFELNB-CHDRCOY.              <LFA1062
           MOVE CHDRLNB-CHDRNUM        TO LIFELNB-CHDRNUM.              <LFA1062
           MOVE COVRLNB-LIFE           TO LIFELNB-LIFE.                 <LFA1062
           MOVE '00'                   TO LIFELNB-JLIFE.                <LFA1062
           MOVE READR                  TO LIFELNB-FUNCTION.             <LFA1062
                                                                        <LFA1062
           CALL 'LIFELNBIO'            USING LIFELNB-PARAMS.            <LFA1062
                                                                        <LFA1062
           IF LIFELNB-STATUZ           NOT = O-K                        <LFA1062
               MOVE LIFELNB-PARAMS     TO SYSR-PARAMS                   <LFA1062
               MOVE LIFELNB-STATUZ     TO SYSR-STATUZ                   <LFA1062
               PERFORM XXXX-FATAL-ERROR                                 <LFA1062
           END-IF.                                                      <LFA1062
       A900-EXIT.                                                       <LFA1062
           EXIT.                                                        <LFA1062
      *                                                                 <V73L01>
       B100-CALL-ZORCOMPY SECTION.                                      <V73L01>
      ***************************                                       <V73L01>
       B110-START.                                                      <V73L01>
      *                                                                 <V73L01>
           MOVE SPACE                  TO ZORL-FUNCTION                 <V73L01>
                                          ZORL-CLAWBACK.                <V73L01>
      *                                                                 <V73L01>
           MOVE LIFA-RLDGACCT          TO ZORL-AGENT.                   <V73L01>
           MOVE LIFA-RLDGCOY           TO ZORL-CHDRCOY.                 <V73L01>
           MOVE LIFA-RDOCNUM           TO ZORL-CHDRNUM.                 <V73L01>
           MOVE LIFA-SUBSTITUTE-CODE(06)                                <V73L01>
                                       TO ZORL-CRTABLE.                 <V73L01>
           MOVE CHDRLNB-OCCDATE        TO ZORL-EFFDATE.                 <V73L01>
      *                                                                 <V73L01>
           IF CHDRLNB-BILLFREQ         = '00'                           <V73L01>
              MOVE WSAA-OLD-CESS-DATE  TO ZORL-PTDATE                   <V73L01>
           ELSE                                                         <V73L01>
              MOVE CHDRLNB-BTDATE      TO ZORL-PTDATE                   <V73L01>
           END-IF.                                                      <V73L01>
      *                                                                 <V73L01>
           MOVE LIFA-ORIGCURR          TO ZORL-ORIGCURR.                <V73L01>
           MOVE LIFA-CRATE             TO ZORL-CRATE.                   <V73L01>
           MOVE LIFA-ORIGAMT           TO ZORL-ORIGAMT.                 <V73L01>
           MOVE CHDRLNB-TRANNO         TO ZORL-TRANNO.                  <V73L01>
           MOVE LIFA-TRANDESC          TO ZORL-TRANDESC.                <V73L01>
           MOVE LIFA-TRANREF           TO ZORL-TRANREF.                 <V73L01>
           MOVE LIFA-GENLCUR           TO ZORL-GENLCUR.                 <V73L01>
           MOVE LIFA-SACSTYP           TO ZORL-SACSTYP.                 <V73L01>
           MOVE LIFA-TERMID            TO ZORL-TERMID.                  <V73L01>
           MOVE LIFA-SUBSTITUTE-CODE(01)                                <V73L01>
                                       TO ZORL-CNTTYPE.                 <V73L01>
           MOVE LIFA-BATCKEY           TO ZORL-BATCH-KEY.               <V73L01>
      *                                                                 <V73L01>
           CALL 'ZORCOMPY'        USING ZORL-ZORLNK-REC.                <V73L01>
      *                                                                 <V73L01>
           IF ZORL-STATUZ          NOT = O-K                            <V73L01>
              MOVE ZORL-STATUZ         TO SYSR-STATUZ                   <V73L01>
              MOVE ZORL-ZORLNK-REC     TO SYSR-PARAMS                   <V73L01>
              PERFORM XXXX-FATAL-ERROR                                  <V73L01>
           END-IF.                                                      <V73L01>
      *                                                                 <V73L01>
       B190-EXIT.                                                       <V73L01>
            EXIT.                                                       <V73L01>
      *                                                                 <PHL108>
PHL108 B200-READ-ZDISPF SECTION.                                        <PHL108>
      *                                                                 <PHL108>
       B210-READ.                                                       <PHL108>
           MOVE 'N'                    TO WSAA-ZDISPF-FOUND.            <PHL108>
           MOVE ZEROS                  TO WSAA-STORED-STAFF-DIS.        <PHL108>
                                                                        <PHL108>
      * Check whether Client is Staff or not:                           <PHE001>
                                                                        <PHE001>
           PERFORM C100-CHECK-STAFF-CLIENT.                             <PHE001>
           IF WSAA-STAFF-FLAG          NOT = 'Y'                        <PHE001>
               GO TO B290-EXIT                                          <PHE001>
           END-IF.                                                      <PHE001>
                                                                        <PHE001>
           MOVE SPACES                 TO ZDIS-DATA-AREA.               <PHL108>
           MOVE CHDRLNB-CHDRCOY        TO ZDIS-CHDRCOY.                 <PHL108>
           MOVE CHDRLNB-CHDRNUM        TO ZDIS-CHDRNUM.                 <PHL108>
           MOVE COVRLNB-LIFE           TO ZDIS-LIFE.                    <PHL108>
           MOVE COVRLNB-COVERAGE       TO ZDIS-COVERAGE.                <PHL108>
           MOVE COVRLNB-RIDER          TO ZDIS-RIDER.                   <PHL108>
           MOVE ZDISREC                TO ZDIS-FORMAT.                  <PHL108>
           MOVE READR                  TO ZDIS-FUNCTION.                <PHL108>
                                                                        <PHL108>
           CALL 'ZDISIO'            USING ZDIS-PARAMS.                  <PHL108>
                                                                        <PHL108>
           IF ZDIS-STATUZ            NOT = O-K                          <PHL108>
           AND ZDIS-STATUZ           NOT = MRNF                         <PHL108>
              MOVE ZDIS-PARAMS         TO SYSR-PARAMS                   <PHL108>
              MOVE ZDIS-STATUZ         TO SYSR-STATUZ                   <PHL108>
              PERFORM XXXX-FATAL-ERROR                                  <PHL108>
           END-IF.                                                      <PHL108>
                                                                        <PHL108>
           IF ZDIS-STATUZ               = O-K                           <PHL108>
              MOVE 'Y'                 TO WSAA-ZDISPF-FOUND             <PHL108>
              MOVE ZDIS-DISCAMT        TO WSAA-STORED-STAFF-DIS         <PHL108>
              GO TO B290-UPDATE-ZDIS                                    <PHL108>
           END-IF.                                                      <PHL108>
                                                                        <PHL108>
           IF ZDIS-STATUZ               = MRNF                          <PHL108>
              GO TO B290-EXIT                                           <PHL108>
           END-IF.                                                      <PHL108>
      *                                                                 <PHL108>
       B290-UPDATE-ZDIS.                                                <PHL108>
                                                                        <PHL108>
      **** MOVE COVRLNB-RERATE-DATE    TO ZDIS-RERATE-DATE.     <CR020B><PHL108>
           MOVE CHDRLNB-TRANNO         TO ZDIS-TRANNO.                  <PHL108>
           MOVE UPDAT                  TO ZDIS-FUNCTION.                <PHL108>
           CALL 'ZDISIO'               USING ZDIS-PARAMS.               <PHL108>
           IF ZDIS-STATUZ          NOT = O-K                            <PHL108>
              MOVE ZDIS-PARAMS         TO SYSR-PARAMS                   <PHL108>
              MOVE ZDIS-STATUZ         TO SYSR-STATUZ                   <PHL108>
              PERFORM XXXX-FATAL-ERROR                                  <PHL108>
           END-IF.                                                      <PHL108>
      *                                                                 <PHL108>
       B290-EXIT.                                                       <PHL108>
           EXIT.                                                        <PHL108>
      *                                                                 <PHL108>
      /                                                                 <PHE001>
       C100-CHECK-STAFF-CLIENT SECTION.                                 <PHE001>
      *********************************                                 <PHE001>
       C101-START.                                                      <PHE001>
      *                                                                 <PHE001>
           MOVE 'N'                    TO WSAA-STAFF-FLAG.              <PHE001>
           MOVE SPACES                 TO WSAA-ITEMCHK.                 <PHE001>
                                                                        <PHE001>
      **** INITIALIZE                  LIFELNB-PARAMS.          <GAPPH2><PHE001>
      **** MOVE CHDRLNB-CHDRCOY        TO LIFELNB-CHDRCOY.      <GAPPH2><PHE001>
      **** MOVE CHDRLNB-CHDRNUM        TO LIFELNB-CHDRNUM.      <GAPPH2><PHE001>
      **** MOVE COVRLNB-LIFE           TO LIFELNB-LIFE.         <GAPPH2><PHE001>
      **** MOVE '00'                   TO LIFELNB-JLIFE.        <GAPPH2><PHE001>
      **** MOVE LIFELNBREC             TO LIFELNB-FORMAT.       <GAPPH2><PHE001>
      **** MOVE READR                  TO LIFELNB-FUNCTION.     <GAPPH2><PHE001>
      ****                                                      <GAPPH2><PHE001>
      **** CALL 'LIFELNBIO'            USING LIFELNB-PARAMS.    <GAPPH2><PHE001>
      ****                                                      <GAPPH2><PHE001>
      **** IF LIFELNB-STATUZ           NOT = O-K                <GAPPH2><PHE001>
      ****     MOVE LIFELNB-PARAMS     TO SYSR-PARAMS           <GAPPH2><PHE001>
      ****     PERFORM XXXX-FATAL-ERROR                         <GAPPH2><PHE001>
      **** END-IF.                                              <GAPPH2><PHE001>
      ****                                                      <GAPPH2><PHE001>
      **** INITIALIZE                  CLEX-PARAMS.             <GAPPH2><PHE001>
      **** MOVE CHDRLNB-COWNPFX        TO CLEX-CLNTPFX.         <GAPPH2><PHE001>
      **** MOVE CHDRLNB-COWNCOY        TO CLEX-CLNTCOY.         <GAPPH2><PHE001>
      **** MOVE LIFELNB-LIFCNUM        TO CLEX-CLNTNUM.         <GAPPH2><PHE001>
      **** MOVE CLEXREC                TO CLEX-FORMAT.          <GAPPH2><PHE001>
      **** MOVE READR                  TO CLEX-FUNCTION.        <GAPPH2><PHE001>
      *                                                                 <GAPPH2>
           MOVE SPACES                 TO CLEX-PARAMS.                  <GAPPH2>
           MOVE CHDRLNB-COWNCOY        TO CLEX-CLNTCOY.                 <GAPPH2>
           MOVE CHDRLNB-COWNNUM        TO CLEX-CLNTNUM.                 <GAPPH2>
           MOVE CHDRLNB-COWNPFX        TO CLEX-CLNTPFX.                 <GAPPH2>
           MOVE CLEXREC                TO CLEX-FORMAT.                  <GAPPH2>
           MOVE READR                  TO CLEX-FUNCTION.                <GAPPH2>
                                                                        <PHE001>
           CALL 'CLEXIO'               USING CLEX-PARAMS.               <PHE001>
                                                                        <PHE001>
           IF CLEX-STATUZ              NOT = O-K                        <PHE001>
           AND                         NOT = MRNF                       <PHE001>
               MOVE CLEX-PARAMS        TO SYSR-PARAMS                   <PHE001>
               PERFORM XXXX-FATAL-ERROR                                 <PHE001>
           END-IF.                                                      <PHE001>
                                                                        <PHE001>
           IF CLEX-STATUZ              NOT = O-K                        <PHE001>
               GO TO C109-EXIT                                          <PHE001>
           END-IF.                                                      <PHE001>
                                                                        <PHE001>
           IF CLEX-RSTAFLAG            = SPACES                         <PHE001>
           OR CLEX-RSTAFLAG            = 'N '                           <PHE001>
               MOVE 'N'                TO WSAA-STAFF-FLAG               <PHE001>
                                                                        <GAPPH2>
               IF   CHDRLNB-JOWNNUM NOT = SPACES                        <GAPPH2>
               MOVE CHDRLNB-COWNCOY    TO CLEX-CLNTCOY                  <GAPPH2>
               MOVE CHDRLNB-JOWNNUM    TO CLEX-CLNTNUM                  <GAPPH2>
               MOVE CHDRLNB-COWNPFX    TO CLEX-CLNTPFX                  <GAPPH2>
               MOVE CLEXREC            TO CLEX-FORMAT                   <GAPPH2>
               MOVE READR              TO CLEX-FUNCTION                 <GAPPH2>
               CALL 'CLEXIO'           USING CLEX-PARAMS                <GAPPH2>
               IF CLEX-STATUZ          NOT = O-K AND MRNF               <GAPPH2>
                  MOVE CLEX-PARAMS     TO SYSR-PARAMS                   <GAPPH2>
                  PERFORM XXXX-FATAL-ERROR                              <GAPPH2>
               END-IF                                                   <GAPPH2>
                                                                        <GAPPH2>
               IF  CLEX-RSTAFLAG       NOT = SPACES                     <GAPPH2>
               AND CLEX-RSTAFLAG       NOT = 'N '                       <GAPPH2>
                   MOVE 'Y'            TO WSAA-STAFF-FLAG               <GAPPH2>
               END-IF                                                   <GAPPH2>
               END-IF                                                   <GAPPH2>
           ELSE                                                         <PHE001>
               MOVE 'Y'                TO WSAA-STAFF-FLAG               <PHE001>
           END-IF.                                                      <PHE001>
                                                                        <PHE001>
           IF WSAA-STAFF-FLAG          = 'Y'                            <PHE001>
               PERFORM C110-CHECK-PROD-DISCOUNT                         <PHE001>
           END-IF.                                                      <PHE001>
      *                                                                 <PHE001>
       C109-EXIT.                                                       <PHE001>
           EXIT.                                                        <PHE001>
      /                                                                 <PHE001>
       C110-CHECK-PROD-DISCOUNT SECTION.                                <PHE001>
      **********************************                                <PHE001>
       C111-START.                                                      <PHE001>
      *                                                                 <PHE001>
           MOVE CHDRLNB-CNTTYPE        TO WSAA-CNTTYPE.                 <PHE001>
           MOVE COVRLNB-CRTABLE        TO WSAA-PRODCODE.                <PHE001>
           MOVE CLEX-RSTAFLAG          TO WSAA-STAFFIND.                <PHE001>
                                                                        <PHE001>
           INITIALIZE                  ITEM-PARAMS.                     <PHE001>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <PHE001>
           MOVE CHDRLNB-CHDRCOY        TO ITEM-ITEMCOY.                 <PHE001>
           MOVE TZ018                  TO ITEM-ITEMTABL.                <PHE001>
           MOVE WSAA-ITEMCHK           TO ITEM-ITEMITEM.                <PHE001>
           MOVE SPACES                 TO ITEM-ITEMSEQ.                 <PHE001>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <PHE001>
           MOVE READR                  TO ITEM-FUNCTION.                <PHE001>
                                                                        <PHE001>
           CALL 'ITEMIO'               USING ITEM-PARAMS.               <PHE001>
                                                                        <PHE001>
           IF ITEM-STATUZ              NOT = O-K                        <PHE001>
           AND                         NOT = MRNF                       <PHE001>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <PHE001>
               PERFORM XXXX-FATAL-ERROR                                 <PHE001>
           END-IF.                                                      <PHE001>
                                                                        <PHE001>
           IF ITEM-STATUZ              NOT = O-K                        <PHE001>
               MOVE 'N'                TO WSAA-STAFF-FLAG               <PHE001>
           END-IF.                                                      <PHE001>
      *                                                                 <PHE001>
       C119-EXIT.                                                       <PHE001>
           EXIT.                                                        <PHE001>
      /                                                                 <PHE001>
       X100-WRITE-ZPFR-FILE SECTION.                                    <DA006>
      ******************************                                    <DA006>
       X101-START.                                                      <DA006>
      *                                                                 <DA006>
           INITIALIZE                  ZPFR-PARAMS.                     <DA006>
           MOVE COVRLNB-CHDRCOY        TO ZPFR-CHDRCOY.                 <DA006>
           MOVE COVRLNB-CHDRNUM        TO ZPFR-CHDRNUM.                 <DA006>
           MOVE COVRLNB-TRANNO         TO ZPFR-TRANNO.                  <DA006>
           MOVE COVRLNB-LIFE           TO ZPFR-LIFE.                    <DA006>
           MOVE COVRLNB-COVERAGE       TO ZPFR-COVERAGE.                <DA006>
           MOVE COVRLNB-RIDER          TO ZPFR-RIDER.                   <DA006>
                                                                        <DA006>
TDO   * Do this to avoid duplicate when restart AT:                     <DA006>
           MOVE ZPFRREC                TO ZPFR-FORMAT.                  <DA006>
           MOVE READR                  TO ZPFR-FUNCTION.                <DA006>
                                                                        <DA006>
           CALL 'ZPFRIO'               USING ZPFR-PARAMS.               <DA006>
                                                                        <DA006>
           IF  ZPFR-STATUZ             NOT = O-K                        <DA006>
           AND                         NOT = MRNF                       <DA006>
               MOVE ZPFR-PARAMS        TO SYSR-PARAMS                   <DA006>
               PERFORM XXXX-FATAL-ERROR                                 <DA006>
           END-IF.                                                      <DA006>
      *                                                                 <DA006>
           IF  ZPFR-STATUZ             = MRNF                           <DA006>
               MOVE WRITR              TO ZPFR-FUNCTION                 <DA006>
           ELSE                                                         <DA006>
               MOVE WRITD              TO ZPFR-FUNCTION                 <DA006>
           END-IF.                                                      <DA006>
                                                                        <DA006>
           MOVE COVRLNB-CHDRCOY        TO ZPFR-CHDRCOY.                 <DA006>
           MOVE COVRLNB-CHDRNUM        TO ZPFR-CHDRNUM.                 <DA006>
           MOVE COVRLNB-TRANNO         TO ZPFR-TRANNO.                  <DA006>
           MOVE COVRLNB-LIFE           TO ZPFR-LIFE.                    <DA006>
           MOVE COVRLNB-COVERAGE       TO ZPFR-COVERAGE.                <DA006>
           MOVE COVRLNB-RIDER          TO ZPFR-RIDER.                   <DA006>
           MOVE COVRLNB-PLAN-SUFFIX    TO ZPFR-PLAN-SUFFIX.             <DA006>
           MOVE COVRLNB-CRTABLE        TO ZPFR-CRTABLE.                 <DA006>
           MOVE CHDRLNB-AGNTNUM        TO ZPFR-AGNTNUM.                 <DA006>
           MOVE CHDRLNB-BILLFREQ       TO ZPFR-BILLFREQ.                <DA006>
           MOVE COVRLNB-CRRCD          TO ZPFR-CRRCD.                   <DA006>
           MOVE COVRLNB-INSTPREM       TO ZPFR-INSTPREM.                <DA006>
           MOVE COVRLNB-STATCODE       TO ZPFR-STATCODE.                <DA006>
           MOVE ZPFRREC                TO ZPFR-FORMAT.                  <DA006>
                                                                        <DA006>
           CALL 'ZPFRIO'               USING ZPFR-PARAMS.               <DA006>
                                                                        <DA006>
           IF  ZPFR-STATUZ             NOT = O-K                        <DA006>
               MOVE ZPFR-PARAMS        TO SYSR-PARAMS                   <DA006>
               PERFORM XXXX-FATAL-ERROR                                 <DA006>
           END-IF.                                                      <DA006>
      *                                                                 <DA006>
       X109-EXIT.                                                       <DA006>
           EXIT.                                                        <DA006>
      /                                                                 <UL006>
       A1000-WRITE-ZPOS SECTION.                                        <UL006>
      **************************                                        <UL006>
       A1010-START.                                                     <UL006>
      *                                                                 <UL006>
           INITIALIZE                     ZPOS-PARAMS.                  <UL006>
           MOVE CHDRLNB-CHDRCOY        TO ZPOS-CHDRCOY.                 <UL006>
           MOVE CHDRLNB-CHDRNUM        TO ZPOS-CHDRNUM.                 <UL006>
           MOVE CHDRLNB-OCCDATE        TO ZPOS-CURRFROM.                <UL006>
           MOVE ZPOSREC                TO ZPOS-FORMAT.                  <UL006>
           MOVE READR                  TO ZPOS-FUNCTION.                <UL006>
      *                                                                 <UL006>
           CALL 'ZPOSIO'            USING ZPOS-PARAMS.                  <UL006>
                                                                        <UL006>
           IF ZPOS-STATUZ           NOT = O-K AND MRNF                  <UL006>
              MOVE ZPOS-STATUZ         TO SYSR-STATUZ                   <UL006>
              MOVE ZPOS-PARAMS         TO SYSR-PARAMS                   <UL006>
              PERFORM XXXX-FATAL-ERROR                                  <UL006>
           END-IF.                                                      <UL006>
                                                                        <UL006>
           IF ZPOS-STATUZ               = O-K                           <UL006>
              MOVE WRITD               TO ZPOS-FUNCTION                 <UL006>
           ELSE                                                         <UL006>
              MOVE WRITR               TO ZPOS-FUNCTION                 <UL006>
           END-IF.                                                      <UL006>
                                                                        <UL006>
           MOVE CHDRLNB-CHDRCOY        TO ZPOS-CHDRCOY.                 <UL006>
           MOVE CHDRLNB-CHDRNUM        TO ZPOS-CHDRNUM.                 <UL006>
           MOVE CHDRLNB-OCCDATE        TO ZPOS-CURRFROM.                <UL006>
           MOVE CHDRLNB-SINSTAMT06     TO ZPOS-INSTPREM.                <UL006>
           MOVE CHDRLNB-STATCODE       TO ZPOS-STATCODE.                <UL006>
           MOVE CHDRLNB-PSTATCODE      TO ZPOS-PSTATCODE.               <UL006>
           MOVE WSAA-BAS-SUMINS        TO ZPOS-ZBASUMIN.                <UL006>
           MOVE '1'                    TO ZPOS-VALIDFLAG.               <UL006>
           MOVE CHDRLNB-OCCDATE        TO ZPOS-OCCDATE                  <UL006>
                                          ZPOS-CURRFROM.                <UL006>
           MOVE 'Y'                    TO ZPOS-ZLFLAG.                  <UL006>
           MOVE CHDRLNB-TRANNO         TO ZPOS-TRANNO.                  <UL006>
           MOVE 1                      TO ZPOS-POLYEAR.                 <UL006>
      *                                                                 <UL006>
      * Calculate Next Annual Date.                                     <UL006>
      *                                                                 <UL006>
           MOVE SPACES                 TO DTC4-DATCON4-REC.             <UL006>
           MOVE '01'                   TO DTC4-FREQUENCY.               <UL006>
           MOVE 1                      TO DTC4-FREQ-FACTOR.             <UL006>
           MOVE CHDRLNB-OCCDATE        TO DTC4-INT-DATE-1.              <UL006>
           MOVE WSAA-OCC-DD            TO DTC4-BILLDAY-NUM.             <UL006>
           MOVE WSAA-OCC-MM            TO DTC4-BILLMONTH-NUM.           <UL006>
      *                                                                 <UL006>
           CALL 'DATCON4'           USING DTC4-DATCON4-REC.             <UL006>
      *                                                                 <UL006>
           IF  DTC4-STATUZ         NOT = O-K                            <UL006>
               MOVE DTC4-STATUZ        TO SYSR-STATUZ                   <UL006>
               MOVE DTC4-DATCON4-REC   TO SYSR-PARAMS                   <UL006>
               PERFORM XXXX-FATAL-ERROR                                 <UL006>
           END-IF.                                                      <UL006>
      *                                                                 <UL006>
           MOVE DTC4-INT-DATE-2        TO ZPOS-CURRTO.                  <UL006>
      *                                                                 <UL006>
           MOVE ZPOSREC                TO ZPOS-FORMAT.                  <UL006>
                                                                        <UL006>
           CALL 'ZPOSIO'            USING ZPOS-PARAMS.                  <UL006>
                                                                        <UL006>
           IF ZPOS-STATUZ           NOT = O-K AND MRNF                  <UL006>
              MOVE ZPOS-STATUZ         TO SYSR-STATUZ                   <UL006>
              MOVE ZPOS-PARAMS         TO SYSR-PARAMS                   <UL006>
              PERFORM XXXX-FATAL-ERROR                                  <UL006>
           END-IF.                                                      <UL006>
      *                                                                 <UL006>
       A1090-EXIT.                                                      <UL006>
           EXIT.                                                        <UL006>
      /                                                                 <UL006>
