      * Generation Parameters SCRVER(02)               Do Not Delete!   <S9503>
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P5035.
      *
      * Copyright 1986-2020, Computer Sciences Corporation.
      *
      *REMARKS.
      *
      * Initialise
      * ----------
      *
      *   Skip  this  section  if  returning from an optional selection
      *   (current stack position action flag = '*').
      *
      *   Read  AGLF  (RETRV)  in   order  to  obtain  the  life  agent
      *   information.
      *
      *   Using  the  information  retrieved  from AGLF, READR AGNT. If
      *   there is no record on this file for the agent number, this is
      *   a new agency agreement to be created. Otherwise, it is an old
      *   one  to  be  modified.  (If in enquiry mode, this record must
      *   exist).
      *
      *   If the agreement does not exist:
      *
      *        - default the agent branch to the sign-on branch look up
      *             its description  from  the  branch  table  (DESC  -
      *             T1692).
      *
      *   If the agreement already exists:
      *
      *        - load  the  screen  from  the  two records read (AGNT &
      *             AGLF) looking up all descriptions,
      *
      *        - if there  are  bank  details on AGLF, put a '+' in the
      *             bank account indicator,
      *
      *        - if there are  and broker contact records (READR AGBN),
      *             put a '+' in the broker contacts indicator,
      *
      *        - If any of the  tied  agency details are not blank (see
      *             S5036 for list of  fields),  put  a '+' in the tied
      *             agent details indicator.
      *
      *   If  in enquiry  mode,  protect  all  fields  except  for  the
      *   indicators at the bottom.
      *
      *  Validation
      *  ----------
      *
      *   Skip this section  if  returning  from  an optional selection
      *   (current stack position action flag = '*').
      *
      *   If in enquiry mode, skip all field validation.  The selection
      *   fields must still be validated though.
      *
      *   Validate the screen  according  to  the  rules defined by the
      *   field help. For all fields which have descriptions, look them
      *   up again if  the  field  has been changed (CHG indicator from
      *   DDS keyword).
      *
      *   If 'CALC' was pressed, re-display the screen.
      *
      *  Updating
      *  --------
      *
      *   Skip this section  if  returning  from  an optional selection
      *   (current stack position action flag = '*').
      *
      *   If any of the  options  have  been  selected, store the agent
      *   details in the AGLF and AGNT I/O modules.
      *
      *   If options have not been selected,
      *
      *        - update the CLRR  (client  roles)  file  for  the Agent
      *             role.   If  the  client  number  has  change  (AGNT
      *             compared to  screen),  delete the "old" client role
      *             and add the new  one.  If there was no agent record
      *             before, just add a client role:
      *             - client prefix to CN
      *             - client company to FSU company
      *             - role to AG
      *             - foreign key prefix to AG
      *             - foreign key company to sign-on company
      *             - foreign key to agent number
      *
      *        - update the CLRR  (client  roles)  file  for  the Payee
      *             role.  If the  payee client number has change (AGNT
      *             compared to  screen),  delete the "old" client role
      *             and add the new  one.  (NB,  if  the  payee is left
      *             blank, the main  client  number  is used.) If there
      *             was no agent record before, just add a client role:
      *             - client prefix to CN
      *             - client company to FSU company
      *             - role to PE
      *             - foreign key prefix to AG
      *             - foreign key company to sign-on company
      *             - foreign key to agent number
      *
      *        - update the AGNT file:
      *             - prefix to AG
      *             - company to sign-on company
      *             - valid flag to 1
      *             - life agent flag to Y
      *             - client prefix to CN
      *             - client company to FSO company
      *             - client number as entered
      *
      *        - update the AGLF file:
      *             - all fields from the screen (or blank/zero)
      *
      *        - write an AGMO record:
      *             - batch key from WSSP
      *             - all fields from the screen (or blank/zero)
      *             - transaction  effective   date   to   TDAY   (from
      *                  DATCON1),
      *
      *        -  update the batch header  by  calling  BATCUP  with  a
      *             function of WRITS and the following parameters:
      *             - transaction count = 1,
      *             - all other amounts zero,
      *             - batch key from WSSP.
      *
      *        - release the soft  lock on the agent (call SFTLOCK with
      *             function UNLK).
      *
      *  Next Program
      *  ------------
      *
      *   Check each 'selection'  indicator  in  turn.  If an option is
      *   selected,  call  GENSSWCH  with  the  appropriate  action  to
      *   retrieve the optional program switching:
      *             A - Tied Agent details
      *             B - Bank Account details
      *             C - Agency/Broker contacts
      *             D - Client Details
      *
      *   For the first one selected, save the next set of programs (8)
      *   from the program  stack  and  flag the current positions with
      *   '*'.
      *
      *   If a selection  had  been  made  previously, its select field
      *   would contain a  '?'.  In  this  case,  retrieve  the  latest
      *   versions  of AGNT  and  AGLF  and  cross  check  the  details
      *   according to the rules defined in the initialisation section.
      *   Set the selection to  blank  if  there are now no details, or
      *   '+' if there are.
      *
      *   If a selection  is made (selection field X), load the program
      *   stack with the programs returned by GENSSWCH, replace the 'X'
      *   in the selection field  with  a '?', add 1 to the pointer and
      *   exit.
      *
      *   Once all the selections have been serviced, re-load the saved
      *   programs onto the stack  and  blank out the '*'. Set the next
      *   program name to  the  current  screen name (to re-display the
      *   screen).
      *
      *   If nothing is  selected,  continue  by just adding one to the
      *   program pointer.
      *
      *****************************************************************
      *              AMENDMENT  HISTORY                               *
      *****************************************************************
      * DATE.....   BY..   AMENDMENT...............  NUMBER
      *
      * DD/MM/YY    X.X.   xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  NNN
      *
      * 13.12.89    F.M.   Change of error message from E305 to F005.    <001>
      * 19.12.89    T.S.   Access the T5644 instead of the T5699    002  <002>
      *                    for the basic commission description.
      * 20.03.90    T.S.   Modified to create the client role 'PE'  003  <002>
      *                    for add function.
      *                    Check if client is dead.
      *                    Validate the client and payee without
      *                    refering (CHG) indicator.
      * 27/03/90    J.F.   Changed the 2000 validation paragraphs        <004>
      *                    to sections. Exit each section as soon        <004>
      *                    as the first error is found.             004  <004>
      *
      * 06.04.90    F.M.   Cater For Client Indicator processing    005
      *                    ensure that updates are not done until   005
      *                    we have returned.
      * 09.04.90    A.L.   We must always look for Client details   006
      *                    in the FSU Company.
      * 09.04.90    J.F.   Don't use the Change Indicators to see        <007>
      *                    if a screen field needs validating -          <007>
      *                    they are unreliable.                     007  <007>
      * 27.04.90    B.C.   In Modify mode, protect Client Number.   008
      *                    (NM FIX).
      * 27.04.90    B.C.   Include DTETRM (Termination Date).       009
      *                    (NM FIX).
      * 08/05/90    B.C  - An agent could be either a corp. or      010
      *                    personal client so when we enquire on a
      *                    client we must have an option E on secondary
      *                    switch in case the client is corporate.
      *                    This windows into a new enquiry prog P5045.
      *                    T1675 items must be updated with this option.
      *                  - Check if client details has been
      *                    selected!
      *                  - Do not READS on AGLF and AGNTLAG if
      *                    not inquiry.
      *                    (IOM FIXES).
      *
      * 21.05.90    F.M.   Always use FSU company when accessing        <011>
      *                    Clients
      *
      * 25.06.90    BPM    Change the GENSWCH Call-check to handle  012
      *                    a MRNF return from T1675.  This must
      *                    cause the program to redisplay with a
      *                    suitable error rather than fall over.  The
      *                    program must allow the initial stack order
      *                    to be resumed.
      *                    Error V045 was added to Error Table.
      *
      * 06.08.90    I.K.   Changed the PAYEESEL & PAYEENME fields       <013>
      *                    to PAYSEL & PAYENME. Because the data
      *                    dictionary fields length for the above
      *                    changed from 47 to 50 characters.
      *
      * 08.05.91    G.D.   Initialize wssp-msgarea and wssp-confirmatio <014>
      *                    key. As random corruption can occur .        <014>
      *
      * 30.05.91    N.F.   SDF 1572.                                    <015>
      *                    Insert validation for CLIENTIND.             <015>
      *                    Change error message E186 to H118.
      *
      * 06/06/91    J.K.   (SDF - 1916)                             016
      *                    - When setting up a new agent, if a
      *                      'reporting to' agent was specified and
      *                      the option to create bank details was
      *                      selected - the bank details for the
      *                      'reporting to' agent were displayed.
      *                      This has been corrected by performing
      *                      a RETRV on the AGLF file after dealing
      *                      with the Overriding Agent.
      *
      * 07/06/91    J.K.   (SDF - 1924)                             017
      *                    - A description of all '?'s was being
      *                      displayed for Bonus Allocation,
      *                      Servicing and Renewal Commission.
      *                    - In the 1000 section, ensure that the
      *                      correct table is read for the Bonus
      *                      Allocation Description.
      *                    - In the 2000 section, ensure that the
      *                      correct table is read for the Renewal
      *                      and Servicing Commision Descriptions.
      *
      * 11/06/91    J.K.   (SDF - 1923)                             018
      *                    If Date Appointed > Date Terminated,
      *                    display an error message.
      *
      * 18/06/91    B.K.   Display correct Confirmation message     019
      *
      * 20/06/91    S.H.   Set up WSSPWINDOW to pass client key
      *                    for client enquiry via FAS               020
      *
      * 01/07/91    J.K.   (SDF - 2110)                             021
      *                    If the client is changed and bank details
      *                    have been set up, make sure the bank
      *                    details screen is revisited for update or
      *                    deletion.
      *
      * 03/07/91    B.C.   If you enter a Reporting Agent number    022
      *                    with > 5 characters then only the first
      *                    five characters are accepted, validation
      *                    has been entered to stop a potential error
      *                    situation.
      *
      * 05/07/91    J.K.   - This fix is related to Amendment 017.  023
      *                    - The problem relates to the way on which
      *                      the fields on the screen were being
      *                      validated.
      *                      e.g. If for the payment method a valid
      *                           code was entered the correct
      *                           description was displayed.
      *                           An invalid code was then entered
      *                           and a description of '?'s was
      *                           displayed.
      *                           However, if you then went and
      *                           entered a valid code - a description
      *                           of '?'s was still displayed.
      *                    - The above applies to most fields on the
      *                      screen.
      *                    - It occurs because if the field was in error
      *                      the value on the screen was not being
      *                      stored.
      *                    - Also, if the value on the screen is
      *                      blank - move spaces to the storage field.
      *
      * 21/11/91    C.C.   Client specific error nos. changed from  024
      *                    V045 to H093.
      *
      * 08/02/93    R.S.   AQR 3207.                                025
      *                    If bank details set up and acc payee
      *                    entered along with new bank details,
      *                    allow program to exit, rather than switching
      *                    alternately between screens S5035 & S5037.
      *
      * 15/03/93    E.H.   AQR 4343.                                026
      *                    The AGMO file, Agent Movements is written
      *                    to in this program. This file is not read
      *                    by any other programs. This file has been
      *                    inherited from LIFE 2.
      *                    It has been decided that this information
      *                    is no longer relevant to the current base.
      *                    The write of this record has been commented
      *                    out. The "archive" of the file AGMOPF and
      *                    the logical will occur for the november 1993
      *                    release.
      *
      * 17/04/93    CJS.   AQR 4357.                                027
      *                    Ensure T3672 compliance when setting up
      *                    a payment method.
      *
      * 30/11/93    COS.   AQR 4884.                                028
      *                    Ensure P5035 was not abending when a
      *                    invalid payment method was entered.
      *
      * 06/01/94    DHP.   AQR 4743.                                029
      *                    Delete the bank details link when changing
      *                    the payment method from Direct Credit
      *                    to any other method and make the Bank
      *                    indicator blank. Perform the same check
      *                    before updating the AGLF file.
      *                    Update Source/Object computer statements.
      *
      * 26/05/94    GD.    AQR 5115/5180                            030
      *                    Change protection for modify action and
      *                    add code error for table T5690.
      *
      * 01/11/94   W.O.    AQR 5517.                                031
      *                    Agent number increased from 5 to 8 chars
      *
      *****************************************************************
      *
      *  ........ New Version of the Amendment History
      *
      *****************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....
      *
      * 23/01/96   01/32  A05743       Andrew Wall
      *
      *            Prevent an agent from being terminated if they have
      *            someone reporting to them.
      *
      * 16/04/96  01/01   D96NUM       Rob Yates                            *
      *                   MINSTA                                            *
      *                   TAXALW                                            *
      *                                                                     *
      *                   The above field(s) have been increased in         *
      *                   size as part of the 1996 Numeric Field            *
      *                   Increase project. This is to enable users         *
      *                   to then convert (if they should wish to do        *
      *                   so) the fields to 'Monetary' and use the          *
      *                   SMART Variable Decimal Places technology          *
      *                   accordingly.                                      *
      *                                                                     *
      * 16/07/96    CAS1.0        Dominic Dunbar
      *                                                                     *
      *                    Additional 'OR Dtails'  option to accept
      *                    OR Code, Effective Date and Reporting To
      *                    and  store  in  AGLF  file  for  further
      *                    computation of personal sales commission.
      *
      * 29/11/97    DUNC  SMART 9503 Conv for Client/Server.        <S9503>
      *
      * 13/10/98  A001,   WOR.    S01
      *           A002    Add the following fields to AGLF :
      *                   -TAGSUSIND "Agent Suspend Indicator"
      *                   -TLAGLICNO "Agent License Number"
      *                   -TLICEXPDT "License Expiry Date"
      *
      * 04/01/99  01/01   V4L014       Balaji . B                           *
      *           Override commission Enhancement                           *
      *                                                                     *
      * 05/01/99  01/01   S02          CSC - Worachart                      *
      *           A007    To maintain Agent Sales Unit in Agent Appointment *
      *                   Details.                                          *
      *                                                                     *
      * 12/01/99  01/01   V4L016       CSC - Daniel Wong                    *
      *           Agent Production Update - Maintain Agency Movements       *
      *                                                                     *
      * 18/01/99  01/01   S03          CSC - Worachart                      *
      *           A004    To keep collateral information for each agent.
      *                                                                     *
      * 27/01/99  01/01   V4L012       CSC - Daniel Wong                    *
      *           Recompile                                                 *
      *                                                                     *
      * 23/03/99  01/01   A001         CSC - Minh Huynh                     *
      *           TO BE MORE FLEXIBLE FOR THE USER TO ENTER THE BLACK  ??   *
      *           LIST INDICATOR, THE NEW TABLE HAS BEEN CREATED WHICH
      *           WILL HOLD ALL OF THE ALLOWABLE VALUE FOR THE BLACK
      *           LIST INDICATORS.
      *                                                                     *
      * 02/04/99  01/01   A002         CSC - Ali Hartono                    *
      *           When Agent's Licence Number is specified, the             *
      *           Expiry Date for Licence Number should be mandatory.       *
      *                                                                     *
      * 19/05/99  01/01   V4L016       Balaji . B                           *
      *           Agent reporting to and override percentage fields    ??   *
      *           should be protected during agent termination.
      *           When the agent is being reinstated the termination
      *           date should be blanked off.
      *
      *                                                                     *
      * 21/05/99  01/01   RECMP        CSC: Araceli                         *
      *           Recompile.                                           ??   *
      *
      *
      * 02/06/99  01/01   V4LAQR       Minh
      * AQR#45    The currency (CURRCODE) should be compulsory since the    *
      *           Agent payment (B5403) change on 960607 by CAS1.0.
      *           if the AGLF-CURRCODE is spaces ,sytems treats it as a     *
      *           multi-currrency contract.                                 *
      *                                                                     *
      *                                                                     *
      * 25/08/00  01/01   LA1151       Davide Maraschi                      *
      *                                                                ??   *
      *           WSAA-REPORTAGS is not initialised, so when
      *           MCAF is written, wrong REPORT TO are updated
      *           on the record.
      *                                                                     *
      *                                                                     *
      * 12/10/00  01/01   LA1174       CSC - Venkatesh Senthamarai          *
      *           AGENCY PRODUCTION FIX                                     *
      *           Agent structure is not captured properly if produc-       *
      *           reporting was done using a bottom to top approach.
      *           Usuage of recursive fetch from higher to lower level
      *           is used to handle the problem.
      *           When Agency Production is reported , accumulation
      *           to group and direct cumm FYP based on TM603 Setup
      *           was not correct prior to this fix.
      *
      * 20/10/00  01/01   V5L001       CSC - Sandeep Gosavi                 *
      *           Life/Asia V5.0 enhancement                                *
      *           Enable 'Reporting to' with 'OR details' and disable
      *           'Override %'.
      *                                                                     *
      * 09/11/00  01/01   V5L003       CSC - Sandeep Gosavi                 *
      *           Introduce consolidted cheque indicator in                 *
      *           tied agent screen S5036.
      *                                                                     *
      * 12/12/00  01/01   LA1233       Ali Hartono                          *
      *           To include Agent Company in MACF file.                    *
      *                                                                     *
      * 20/01/04  01/01   V65F14       Saw Geok Tin
      *           CLBL key structure now includes CLNTNUM to allow
      *           multiple usage of same bank account.
      *
      * 03/12/08  01/01   V73F02       Xu Chen/ASIA/CSC (China)             *
      *           To keep the history of changes, change the logic to       *
      *           retain the existing AGLF,AGNTLAG records with VF = '2',   *
      *           write the new records with VF = '1'.                      *
      *                                                                     *
      * 02/12/09  01/01   V75F01       Saw Hoong Ong/FSG/CSC (Malaysia)     *
      *           Secured Data Access                                       *
      *           For Agent Creation, call BLDENRL in order to create USAE  *
      *           Secured Data Access Entity & ENRL Entity Relationship     *
      *           records.                                                  *
      *                                                                     *
      * 28/09/10  01/01   V76F10       Xu Chen/ASIA/CSC (China)             *
      *           Revert changes made under V73F02. History of records      *
      *           will now be handled using the SMART Audit Trail.          *
      *                                                                     *
      * 24/01/11  01/01   V76F06       Wang Ge/FSG/CSC (Singapore)          *
      *           Conduct currency rounding validation for the input        *
      *           amount to ensure they are tally with the rounding rule:   *
      *           - S5035-MINSTA                                            *
      *                                                                     *
      * 01/09/11  01/01   LA5293       Xu Chen/ASIA/CSC (China)             *
      *           Not pass validflag '1' when creating AGLF record due to   *
      *           V76F10 change. Fix it.                                    *
      *                                                                     *
      * 22/08/13  01/01   AG002        FOR CSC WORKING 1                    *
      *           CHECK TIED AUTOMATIC WHEN DATE TERMINATION IS             *
      *           CHOOSED                                                   *
      *                                                                     *
      * 28/08/13  01/01   AG002        Liem Vo - CSC Developer              *
      *           AGENT REASON TERMINATION                                  *
      *                                                                     *
      * 23/09/13  01/01   AG002        Thoai Anh - CSC Developer            *
      *  MODIF.1: F739: ENTER BOTH FIELDS OR NONE (DATE TERMINATED,         *
      *                 REASON OF TERMINATION) - ACTION 'B'                 *
      *  MODIF.2: ALLOW TERMINATION IF ALL LOW AGENTS ARE TERMINATED
      *                                                                     *
      * 16/10/13  01/01   AG002        Khang Nguyen - CSC Developer         *
      *           Always hide the field overide (OVCPC) on the screen       *
      *                                                                     *
      * 18/12/14  01/01   PHE003       Thanh Do                             *
      *           Update Sales Region and Sales Office of downline          *
      *           agents when modify Sales Region and/or Sales Office
      *           of Leader Agent.
      *                                                                     *
      * 29/12/15  01/01   PHFX01       Thanh Do                             *
      *           Force error if Reporting To Agent not found.              *
      *                                                                     *
      * 21/03/16  01/01   PHE005       Thanh Do                             *
      *           Extend Agent Reporting To up to 20 levels.                *
      *                                                                     *
      * 29/04/16  01/01   PHFX01       Phi Tran - IT DEV                    *
      *           Validate Client Already on Agent File.                    *
      *                                                                     *
      * 11/05/16  01/01   PHE004       Phi Tran - IT DEV                    *
      *           Not report to same level.                                 *
      *                                                                     *
      * 26/05/16  01/01   PHE004       Thanh Do                             *
      *           ONLY Check "Report to same level" when creating or        *
      *           modifying an Agent.
      *                                                                     *
      * 12/10/16  01/01   PHFX08       Phi Tran - IT DEV                    *
      *           Stop Creating Agent from Alternate Address.               *
      *                                                                     *
      * 09/01/18  01/01   DA007        Thanh Do                             *
      *           Add Agent Class for Agent Club.                           *
      *                                                                     *
      * 17/05/18  01/01   DA008        Tuyet Huynh IT - DEV                 *
      *           Add Agent Club class remark.                              *
      *                                                                     *
      * 13/02/19  01/01   DA015        Tuyet Huynh IT - DEV                 *
      *           Add New and Old Leader Grade into structure history       *
      *                                                                     *
      * 30/07/20  01/01   DA023        Phi Tran - IT DEV                    *
      *           Archive History when modify Agent type - Action B.        *
      *           - This rule is setup in TZ606.                            *
      *                                                                     *
      * 08/10/20  01/01   DA025        Van Bao Tuyen - IT                   *
      *           Default date appointment and skip expiry date             *
      *           when creat or modify agent have license no.               *
      *                                                                     *
      * 03/11/20  01/01   DA028        Phi Tran - IT DEV                    *
      *           Keep Agent History :                                      *
      *           - Agent Type                                              *
      *           - Date appointed                                          *
      *           - Area Core.                                              *
      *           - Sale Unit.                                              *
      *                                                                     *
      * 18/11/20  01/01   NB031        Mai Yen Phi - IT                     *
      *           Set MRDT flag for AGENT                                   *
      *                                                                     *
      **DD/MM/YY*************************************************************
      /
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER. IBM-S38.                                        <029>
      *OBJECT-COMPUTER. IBM-S38.                                        <029>
       SOURCE-COMPUTER.                IBM-AS400.                       <029>
       OBJECT-COMPUTER.                IBM-AS400.                       <029>
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       01  WSAA-PROG                   PIC X(05) VALUE 'P5035'.
       01  WSAA-VERSION                PIC X(02) VALUE '02'.
      *
       01  WSAA-OLD-CLNTNUM            PIC X(08).
       01  WSAA-OLD-PAYEE              PIC X(08).
       01  WSAA-AGNUM                  PIC X(08).                       <LA1174>
       01  WSAA-TRANNO                 PIC S9(05) COMP-3.               <LA1174>
       01  WSAA-TRIGGER                PIC X(01) .                      <LA1174>
       01  WSAA-RECORD-FOUND           PIC X(01) .                      <LA1174>
       01  WSAA-INIT-REPSEL            PIC X(10) .                      <LA1174>
       01  WSAA-ZRPTGB                 PIC X(08).                       <LA1174>
       01  WSAA-ZRPTGC                 PIC X(08).                       <LA1174>
       01  WSAA-ZRPTGD                 PIC X(08).                       <LA1174>
       01  WSBB-REPORTAG               PIC X(08).                       <LA1174>
       01  WSAA-STORE1-AGNUM           PIC X(08).                       <LA1174>
       01  WSAA-STORE1-ZRPTGA          PIC X(08).                       <LA1174>
       01  WSAA-STORE1-EFFDATE         PIC 9(08) COMP-3.                <LA1174>
       01  WSAA-STORE1-AGMVTY          PIC X(02).                       <LA1174>
       01  WSAA-STORE1-TRANNO          PIC S9(05) COMP-3.               <LA1174>
       01  WSAA-STOREX-AGNUM           PIC X(08).                       <LA1174>
       01  WSAA-STOREX-ZRPTGA          PIC X(08).                       <LA1174>
       01  WSAA-STOREX-EFFDATE         PIC 9(08) COMP-3.                <LA1174>
       01  WSAA-STOREX-AGMVTY          PIC X(02).                       <LA1174>
       01  WSAA-STOREX-TRANNO          PIC S9(05) COMP-3.               <LA1174>
       01  WSAA-TMP-AGMVTY             PIC X(02).                       <LA1174>
       01  WSAA-X                      PIC S9(03) COMP-3 VALUE 0.
       01  WSAA-Y                      PIC S9(03) COMP-3 VALUE 0.
       01  WSAA-ORINDIC                PIC X(01) VALUE ' ' .            <V4L014>
       01  WSAA-INDEX                  PIC 9(02).                       <V4L016>
       01  WSAA-TODAY                  PIC S9(08).                      <V4L016>
       01  WSSP-REASON-FLG             PIC 9(1) VALUE 0.
       01  WSAA-ORIG-DTETRM            PIC S9(08) VALUE ZEROES.
       01  WSAA-ORIG-BMAFLG            PIC X(01)  VALUE SPACE.
       01  WSAA-TV084-FLAG             PIC X(01).                       <PHE004>
       01  IX                          PIC 9(02).                       <PHE004>
TDO    01  WSAA-T601                   PIC X(04) VALUE 'T601'.          <PHE004>
       01  WSAA-T602                   PIC X(04) VALUE 'T602'.          <PHE004>
      *01  WSAA-AGLVL-OLD              PIC X(02) VALUE SPACES.          <DA007>
      *01  WSAA-AGLVL-STORE            PIC X(02) VALUE SPACES.          <DA007>
       01  WSAA-AGLVL-OLD              PIC X(03) VALUE SPACES.          <DA007>
       01  WSAA-AGLVL-STORE            PIC X(03) VALUE SPACES.          <DA007>
       01  WSAA-FLAG-CLUB              PIC X(01) VALUE SPACE.           <DA008>
       01  WSAA-TYPE-AG.                                                <DA015>
           09 WSAA-AGNT-TYPE           PIC X(02) OCCURS 20 TIMES.       <DA015>

      *                                                                 <V4L016>
       01  WSAA-REPORTAGS.                                              <V4L016>
           09 WSAA-REPORTAG            PIC X(08)                        <V4L016>
      ****                             OCCURS 005 TIMES.        <PHE005><V4L016>
                                       OCCURS 020 TIMES.                <PHE005>
                                                                        <V4L016>
      *01  WSAA-REPORTAG-AREA REDEFINES WSAA-REPORTAGS.         <PHE005><V4L016>
      **** 09 WSAA-REPORTAG01          PIC X(08).               <PHE005><V4L016>
      **** 09 WSAA-REPORTAG02          PIC X(08).               <PHE005><V4L016>
      **** 09 WSAA-REPORTAG03          PIC X(08).               <PHE005><V4L016>
      **** 09 WSAA-REPORTAG04          PIC X(08).               <PHE005><V4L016>
      **** 09 WSAA-REPORTAG05          PIC X(08).               <PHE005><V4L016>
      *
       77  WSAA-SAVE-FLAG              PIC X(01).                       <020>
      *                                                                 <020>
       01  FILLER.
           03  WSAA-SEC-PROG           PIC X(05) OCCURS 8.
      *****
      *    Flags to determine whether new or existing Agent details.
      *****
       01  WSAA-AGENT-EXIST-FLAG       PIC X(01).
           88  WSAA-AGREEMNT-YES       VALUE 'Y'.
           88  WSAA-AGREEMNT-NO        VALUE 'N'.
      *                                                                 <021>
      *****
      *    Flags to determine whether ALL LOW AGENTS ARE TERMINATED.
      *****
       01  WSAA-LOW-AG-TERM-FLAG       PIC X(01).
           88  WSAA-AGTERMNT-YES       VALUE 'Y'.
           88  WSAA-AGTERMNT-NO        VALUE 'N'.
       01  WSAA-INVALID-AGENT          PIC X(01).                       <DA028>
       01  WSAA-FOUND                  PIC X(01).                       <DA028>
      *
       01  WSAA-SAVE-CLNTSEL           PIC X(10).                       <021>
      *                                                                 <021>
       01  WSAA-REPSEL.                                                 <022>
      *    03  WSAA-REPSEL-AGNTNUM     PIC X(05).                  <022><031>
      *    03  WSAA-REPSEL-FILL        PIC X(05).                  <022><031>
           03  WSAA-REPSEL-AGNTNUM     PIC X(08).                       <031>
           03  WSAA-REPSEL-FILL        PIC X(02).                       <031>
                                                                        <A05743>
       01  WSAA-AGLFRPT-KEY.                                            <A05743>
           03  WSAA-AGNTPFX            PIC X(02).                       <A05743>
           03  WSAA-AGNTCOY            PIC X(01).                       <A05743>
           03  WSAA-AGNTNUM            PIC X(08).                       <A05743>
      *                                                                 <AG002>
       01  WSAA-INTRO-EXIST-FLAG       PIC X(01).                       <AG002>
       01  WSAA-REGION-OLD             PIC X(03).                       <PHE003>
       01  WSAA-UNIT-OLD               PIC X(05).                       <PHE003>
       01  WSAA-OLDLDGRD               PIC X(02).                       <DA015>
       01  WSAA-NEWLDGRD               PIC X(02).                       <DA015>
       01  WSAA-TZ606-VALID            PIC X(01).                       <DA023>
       01  WSAA-TZ606-FLAG             PIC X(01).                       <DA023>
       01  WSAA-AGTYPE-SAVE            PIC X(02).                       <DA023>
       01  WSAA-ARACDE-SAVE            PIC X(00003).                    <DA028>
       01  WSAA-TSALESUNT-SAVE         PIC X(00005).                    <DA028>
       01  WSAA-DTEAPP-SAVE            PIC S9(08) COMP-3.               <DA028>
       01  WSAA-L-FLAG                 PIC X(01).                       <DA028>
       01  WSAA-S-FLAG                 PIC X(01).                       <DA028>
       01  WSAA-AGNOTES                PIC X(00050).                    <DA028>
       01  WSAA-IX                     PIC 9(02).                       <DA028>
       01  WSAA-IY                     PIC 9(02).                       <DA028>
       01  WS-CNT                      PIC 9(02).                       <DA028>
      *                                                                 <DA028>
       01  WSAA-AGTYPE-STR             PIC X(02).                       <DA028>
       01  WSAA-ARACDE-STR             PIC X(00003).                    <DA028>
       01  WSAA-TSALESUNT-STR          PIC X(00005).                    <DA028>
       01  WSAA-DTEAPP-STR             PIC X(10).                       <DA028>
      *                                                                 <022>
       01  ERRORS.
           03  TL26                    PIC X(04) VALUE 'TL26'.          <A001>
           03  E058                    PIC X(04) VALUE 'E058'.
           03  E190                    PIC X(04) VALUE 'E190'.          <V5L003>
           03  E186                    PIC X(04) VALUE 'E186'.
           03  E305                    PIC X(04) VALUE 'E305'.
           03  E329                    PIC X(04) VALUE 'E329'.
           03  E374                    PIC X(04) VALUE 'E374'.
           03  E542                    PIC X(04) VALUE 'E542'.
           03  E475                    PIC X(04) VALUE 'E475'.
           03  F176                    PIC X(04) VALUE 'F176'.
           03  F188                    PIC X(04) VALUE 'F188'.
           03  F199                    PIC X(04) VALUE 'F199'.
<003>      03  F782                    PIC X(04) VALUE 'F782'.          <003>
           03  G297                    PIC X(04) VALUE 'G297'.
           03  G447                    PIC X(04) VALUE 'G447'.          027
           03  G747                    PIC X(04) VALUE 'G747'.
           03  H067                    PIC X(04) VALUE 'H067'.          <018>
           03  H118                    PIC X(04) VALUE 'H118'.          <015>
           03  H400                    PIC X(04) VALUE 'H400'.          <A002>
           03  I032                    PIC X(04) VALUE 'I032'.          <A002>
           03  T005                    PIC X(04) VALUE 'T005'.
           03  F005                    PIC X(04) VALUE 'F005'.
           03  H093                    PIC X(04) VALUE 'H093'.          (024)
           03  E492                    PIC X(04) VALUE 'E492'.          <029>
           03  F177                    PIC X(04) VALUE 'F177'.          <030>
           03  E535                    PIC X(04) VALUE 'E535'.          <A05743>
           03  F490                    PIC X(04) VALUE 'F490'.          <S01>
           03  TL13                    PIC X(04) VALUE 'TL13'.          <S02>
           03  EV03                    PIC X(04) VALUE 'EV03'.          <V4L016>
           03  F982                    PIC X(04) VALUE 'F982'.          <V4LAQR>
           03  RFIK                    PIC X(04) VALUE 'RFIK'.          <V76F06>
      *                                                                 <029>
      ***  03  V045                    PIC X(04) VALUE 'V045'.          (024)
           03  E315                    PIC X(04) VALUE 'E315'.          <V5L003>
           03  RLAG                    PIC X(04) VALUE 'RLAG'.
           03  BLAG                    PIC X(04) VALUE 'BLAG'.
           03  E321                    PIC X(04) VALUE 'E321'.          <PHFX01>
           03  EZ84                    PIC X(04) VALUE 'EZ84'.          <PHE004>
           03  F393                    PIC X(04) VALUE 'F393'.          <PHFX08>
           03  A020                    PIC X(04) VALUE 'A020'.          <DA023>
           03  A023                    PIC X(04) VALUE 'A023'.          <DA028>
           03  A024                    PIC X(04) VALUE 'A024'.          <DA028>
      *                                                                 <AG002>
           03  F739                    PIC X(04) VALUE 'F739'.          <AG002>
      *
       01  TABLES.
           03  TT566                   PIC X(05) VALUE 'TT566'.         <A001>
           03  T1692                   PIC X(05) VALUE 'T1692'.
           03  T3672                   PIC X(05) VALUE 'T3672'.         027
           03  T3692                   PIC X(05) VALUE 'T3692'.
           03  T5628                   PIC X(05) VALUE 'T5628'.
           03  T5629                   PIC X(05) VALUE 'T5629'.
           03  T5630                   PIC X(05) VALUE 'T5630'.
           03  T5644                   PIC X(05) VALUE 'T5644'.
           03  T5690                   PIC X(05) VALUE 'T5690'.
           03  T5692                   PIC X(05) VALUE 'T5692'.
           03  T5696                   PIC X(05) VALUE 'T5696'.
           03  T5697                   PIC X(05) VALUE 'T5697'.
           03  T5698                   PIC X(05) VALUE 'T5698'.
           03  T5699                   PIC X(05) VALUE 'T5699'.
           03  TT518                   PIC X(05) VALUE 'TT518'.         <S02>
           03  TH605                   PIC X(05) VALUE 'TH605'.         <V4L014>
           03  TZ001                   PIC X(05) VALUE 'TZ001'.         <V4L014>
           03  TV084                   PIC X(05) VALUE 'TV084'.         <PHE004>
           03  TV102                   PIC X(05) VALUE 'TV102'.         <DA007>
           03  TZ606                   PIC X(05) VALUE 'TZ606'.         <DA023>
      *
       01  FORMATS.
           03  AGLFREC                 PIC X(10) VALUE 'AGLFREC'.
           03  AGNTLAGREC              PIC X(10) VALUE 'AGNTLAGREC'.
           03  AGBNREC                 PIC X(10) VALUE 'AGBNREC'.
      *****03  AGMOREC                 PIC X(10) VALUE 'AGMOREC'.       <026>
           03  CLRRREC                 PIC X(10) VALUE 'CLRRREC'.
           03  CLTSREC                 PIC X(10) VALUE 'CLTSREC'.
           03  CLBLREC                 PIC X(10) VALUE 'CLBLREC'.       <021>
           03  ITEMREC                 PIC X(10) VALUE 'ITEMREC'.
           03  DESCREC                 PIC X(10) VALUE 'DESCREC'.
           03  AGLFRPTREC              PIC X(10) VALUE 'AGLFRPTREC'.    <A05743>
           03  ZRAPREC                 PIC X(10) VALUE 'ZRAPREC'.       <CAS1.0>
           03  AGORREC                 PIC X(10) VALUE 'AGORREC'.       <V4L014>
           03  MACFREC                 PIC X(10) VALUE 'MACFREC'.       <V4L016>
           03  MACFFLVREC              PIC X(10) VALUE 'MACFFLVREC'.    <LA1174>
           03  ZAGIMNTREC              PIC X(10) VALUE 'ZAGIMNTREC'.    <AG002>
           03  AGNTCLNREC              PIC X(10) VALUE 'AGNTCLNREC'.    <PHFX01>
           03  AGNTREC                 PIC X(10) VALUE 'AGNTREC'.       <PHE004>
DA007      03  AGLVREC                 PIC X(10) VALUE 'AGLVREC'.       <DA007>
           03  AGLVTDAREC              PIC X(10) VALUE 'AGLVTDAREC'.    <DA008>
           03  MALFREC                 PIC X(10) VALUE 'MALFREC'.       <DA015>
           03  AGRSREC                 PIC X(10) VALUE 'AGRSREC'.       <DA028>
           03  AGMRREC                 PIC X(10) VALUE 'AGMRREC'.       <NB031>
      *****                                                             <PHFX01>
      *    Stored screen fields for checking changes
      *****
       01  WSAA-STORED-SCREEN-FIELDS.                                   <007>
           05  WSAA-STORE-AGTYPE       PIC X(02).                       <007>
           05  WSAA-STORE-ARACDE       PIC X(03).                       <007>
           05  WSAA-STORE-REPSEL       PIC X(10).                       <007>
           05  WSAA-STORE-PAYMTH       PIC X(02).                       <007>
           05  WSAA-STORE-PAYFRQ       PIC X(02).                       <007>
           05  WSAA-STORE-BCMTAB       PIC X(04).                       <007>
           05  WSAA-STORE-SCMTAB       PIC X(04).                       <007>
           05  WSAA-STORE-RCMTAB       PIC X(04).                       <007>
           05  WSAA-STORE-OCMTAB       PIC X(05).                       <007>
           05  WSAA-STORE-AGENT-CLASS  PIC X(04).                       <007>
           05  WSAA-STORE-TSALESUNT    PIC X(05).                       <S02>
      *
      *****
      *    Batch key to obtain transaction number
      *****
       01  WSAA-BATCKEY.
           COPY BATCKEY.
      *****
      *    Agent key to format 'reporting to' agent for FSU
      *****
       01  WSAA-AGNTKEY.
           COPY AGNTKEY.
      *****                                                             <020>
      *    Client key for FAS client enquiry                            <020>
      *****                                                             <020>
       01  WSAA-CLTSKEY.                                                <020>
           COPY CLTSKEY.                                                <020>
      /
           COPY VARCOM.
      *
           COPY SYSERRREC.
      *
           COPY OPSTATSREC.
      *
      ***  COPY SCRNPARAMS.                                             <S9503>
      /
      ***  COPY S5035SCR.                                               <S9503>
      *
           COPY AGBNSKM.
      *
           COPY AGLFSKM.
      *                                                                 <V4L016>
           COPY AGLFLNBSKM.                                             <V4L016>
      *
      *****COPY AGMOSKM.                                                <026>
      *
           COPY AGNTLAGSKM.
      /
           COPY AGNTCLNSKM.                                             <PHFX01>
      /                                                                 <PHFX01>
           COPY CLTSSKM.
      *
           COPY CLRRSKM.
      *
           COPY CLTRELNREC.
      *
           COPY CLBLSKM.                                                <021>
      *
           COPY DESCSKM.
      *
           COPY ITEMSKM.
      *
           COPY AGLFRPTSKM.                                             <A05743>
      *                                                                 <A05743>
           COPY ZRAPSKM.                                                <CAS1.0>
      *                                                                 <CAS1.0>
           COPY MACFSKM.                                                <V4L016>
           COPY MACFFLVSKM.                                             <LA1174>
           COPY AGORSKM.                                                <V4L014>
           COPY T3672REC.                                               027
           COPY T5690REC.                                               027
           COPY TV084REC.                                               <PHE004>
           COPY TH605REC.                                               <V4L014>
           COPY TZ606REC.                                               <DA023>
           COPY FSUPFXCPY.                                              <V75F01>
      /                                                                 <PHE004>
           COPY AGNTSKM.                                                <PHE004>
      /                                                                 <PHE004>
           COPY BLDENRLREC.                                             <V75F01>
      /                                                                 <V76F06>
           COPY ZRDECPLREC.                                             <V76F06>
      /
           COPY ZAGIMNTSKM.                                             <AG002>
           COPY AGLVSKM.                                                <DA007>
           COPY AGLVTDASKM.                                             <DA008>
           COPY MALFSKM.                                                <DA015>
           COPY AGRSSKM.                                                <DA028>
           COPY AGMRSKM.                                                <NB031>
      /                                                                 <AG002>
      *****************************************************************
      *  Subroutines
      *****************************************************************
           COPY GENSSWREC.
      /
           COPY SFTLOCKREC.
      /
           COPY DATCON1REC.
      /
           COPY BATCUPREC.
      /
           COPY ZAGRGOFREC.                                             <PHE003>
      /                                                                 <PHE003>
      *****************************************************************
       LINKAGE SECTION.
      * Screen copybooks are now part of the linkage.                   <S9503>
      /                                                                 <S9503>
           COPY SCRNPARAMS.                                             <S9503>
      /                                                                 <S9503>
           COPY S5035SCR.                                               <S9503>

           COPY WSSPCOMN.

           COPY WSSPWINDOW.                                             <020>
      /
      * Statement now includes screen copybooks.                        <S9503>
       PROCEDURE DIVISION USING WSSP-COMMON-AREA WSSP-USER-AREA         <S9503>
                                               SCRN-SCREEN-PARAMS       <S9503>
                                               S5035-DATA-AREA      .   <S9503>

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
      *****
      *    Skip  this section if  returning from an optional selection
      *    (current stack position action flag = '*').
      *****
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'
              GO TO 1090-EXIT.

      *****
      *    Initialise working storage and screen fields.
      *****
           MOVE SPACES                 TO AGLF-DATA-AREA.               <014>
           MOVE SPACES                 TO AGNTLAG-DATA-AREA.            <014>
           MOVE SPACES                 TO WSAA-STORE-AGTYPE.            <007>
           MOVE SPACES                 TO WSAA-STORE-ARACDE.            <007>
           MOVE SPACES                 TO WSAA-STORE-REPSEL.            <007>
           MOVE SPACES                 TO WSAA-STORE-PAYMTH.            <007>
           MOVE SPACES                 TO WSAA-STORE-PAYFRQ.            <007>
           MOVE SPACES                 TO WSAA-STORE-BCMTAB.            <007>
           MOVE SPACES                 TO WSAA-STORE-SCMTAB.            <007>
           MOVE SPACES                 TO WSAA-STORE-RCMTAB.            <007>
           MOVE SPACES                 TO WSAA-STORE-OCMTAB.            <007>
           MOVE SPACES                 TO WSAA-STORE-AGENT-CLASS.       <007>
           MOVE SPACES                 TO WSAA-STORE-TSALESUNT.         <S02>
           MOVE SPACES                 TO WSSP-CONFIRMATION-KEY.        <014>
           MOVE SPACES                 TO WSSP-MSGAREA.                 <014>
           MOVE SPACES                 TO WSAA-ORINDIC.                 <V4L014>
           MOVE SPACES                 TO WSAA-AGLVL-OLD                <DA007>
                                          WSAA-AGLVL-STORE.             <DA007>

           MOVE SPACES                 TO WSAA-SAVE-CLNTSEL.            <021>
           MOVE SPACES                 TO S5035-TLAGLICNO.              <S01>
           MOVE SPACES                 TO S5035-TAGSUSIND.              <S01>
           MOVE SPACES                 TO S5035-ADDOPT.                 <DA008>
           MOVE SPACES                 TO WSAA-REPORTAGS.               <LA1151>
           MOVE SPACES                 TO WSAA-REGION-OLD               <PHE003>
                                          WSAA-UNIT-OLD.                <PHE003>
           MOVE VRCM-MAX-DATE          TO WSAA-ORIG-DTETRM.             <PHFX01>

           MOVE WSSP-BATCHKEY          TO WSAA-BATCKEY.
           MOVE SPACES                 TO S5035-DATA-AREA.

           MOVE VRCM-MAX-DATE          TO S5035-DTEAPP
                                          S5035-TLICEXPDT               <S01>
                                          S5035-DTETRM.                 <009>
           MOVE ZERO                   TO S5035-MINSTA
                                          S5035-OVCPC.
           MOVE 'N'                    TO WSAA-TZ606-VALID.             <DA023>
           MOVE 'N'                    TO WSAA-TZ606-FLAG.              <DA023>
           MOVE SPACES                 TO WSAA-AGTYPE-SAVE.             <DA023>
           MOVE SPACES                 TO WSAA-ARACDE-SAVE              <DA028>
                                          WSAA-TSALESUNT-SAVE           <DA028>
                                          WSAA-L-FLAG                   <DA028>
                                          WSAA-S-FLAG.                  <DA028>
           MOVE ZEROES                 TO WSAA-DTEAPP-SAVE.             <DA028>
                                                                        <DA007>
           MOVE 'TDAY'                 TO DTC1-FUNCTION.                <DA007>
           CALL 'DATCON1' USING DTC1-DATCON1-REC.                       <DA007>
           MOVE DTC1-INT-DATE          TO WSAA-TODAY.                   <DA007>
           MOVE WSAA-TODAY             TO S5035-DTEAPP.                 <DA025>

       1010-RETRV-AGLFLNB.
      *****
      *    Retreive the life agent details.
      *****
           MOVE 'RETRV'                TO AGLF-FUNCTION.

           CALL 'AGLFIO' USING AGLF-PARAMS.
           IF AGLF-STATUZ              NOT = O-K
              MOVE AGLF-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.

           MOVE AGLF-AGNTNUM           TO S5035-AGNUM.
           MOVE AGLF-AGNTNUM           TO WSSP-CONFIRMATION-KEY.        <019>
           MOVE AGLF-ARACDE            TO WSAA-ARACDE-SAVE.             <DA028>
           MOVE AGLF-TSALESUNT         TO WSAA-TSALESUNT-SAVE.          <DA028>
           MOVE AGLF-DTEAPP            TO WSAA-DTEAPP-SAVE.             <DA028>
      *
       1020-READR-AGENT-FILE.
      *****
      *    Read the Agent details from the Agent retreived.
      *****
           MOVE SPACES                 TO AGNTLAG-DATA-KEY.
           MOVE AGLF-AGNTNUM           TO AGNTLAG-AGNTNUM.

           PERFORM 1500-AGNTLAGIO-CALL.

      *****
      *    If record not found then set flag to create new details.
      *****
           IF AGNTLAG-STATUZ              NOT = O-K
              IF WSSP-FLAG             = 'I'
                 MOVE E475             TO S5035-REPSEL-ERR
              ELSE
                 MOVE 'N'              TO WSAA-AGENT-EXIST-FLAG.

      *****
      *    If record exists then set flag to list existing details.
      *****
           IF AGNTLAG-STATUZ              = O-K
              MOVE 'Y'                 TO WSAA-AGENT-EXIST-FLAG.
      *
           PERFORM 1800-GET-ORINDIC.                                    <V4L014>
                                                                        <V4L014>
       1030-AGENT-PROCESS.
      *****
      *    Perform sections according to agent details flag.
      *****
           IF WSAA-AGREEMNT-NO
              PERFORM 1100-AGENT-NON-EXIST.

           IF WSAA-AGREEMNT-YES
              PERFORM 1200-AGENT-EXISTS.

           IF TH605-INDIC = 'Y'                                         <V4L014>
      *       MOVE 'Y'                 TO S5035-REPSEL-OUT(PR)  <V5L001><V4L014>
      *       MOVE 'Y'                 TO S5035-REPSEL-OUT(ND)  <V5L001><V4L014>
              MOVE 'Y'                 TO S5035-OVCPC-OUT (PR)          <V5L001>
                                          S5035-OVCPC-OUT (ND)          <V5L001>
           ELSE                                                         <V4L014>
              MOVE 'Y'                 TO S5035-ZRORIND-OUT(PR)         <V4L014>
              MOVE 'Y'                 TO S5035-ZRORIND-OUT(ND)         <V4L014>
           END-IF.                                                      <V4L014>
      * PS000: Always hide the field Override on the screen.    <AG002> <PS000>
           MOVE 'Y'                    TO S5035-OVCPC-OUT (PR)          <AG002>
                                          S5035-OVCPC-OUT (ND).         <AG002>
      *****
      *    Find Agent Branch description.
      *****
           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE T1692                  TO DESC-DESCTABL.
           MOVE S5035-AGNTBR           TO DESC-DESCITEM.
           MOVE READR                  TO DESC-FUNCTION.

           PERFORM 1600-DESCIO-CALL.

           MOVE DESC-LONGDESC          TO S5035-AGBRDESC.

      *****
      *    If entering on enquiry then protect the client number.
      *****
           MOVE 'Y'                    TO S5035-MRDTFLAG-OUT(PR)        <NB031>
           IF WSSP-FLAG                = 'I'
              MOVE 'Y'                 TO S5035-CLNTSEL-OUT(PR)         <008>
                                          S5035-REPSEL-OUT(PR)          <V4L014>
                                          S5035-OVCPC-OUT (PR)          <V5L001>
                                          S5035-DTEAPP-OUT(PR)          <V4L016>
                                          S5035-AGTYPE-OUT(PR)          <V4L016>
                                          S5035-AGNTBR-OUT(PR)          <V4L016>
                                          S5035-DTETRM-OUT(PR)          <V4L016>
                                          S5035-AGLVL-OUT(PR)           <DA007>
      *                                   S5035-BMAFLG-OUT(PR).
                                          S5035-ZBMAFLG-OUT(PR).
      *                                                                 <V4L016>
           IF WSSP-FLAG                = 'T' OR 'R'                     <V4L016>
              MOVE 'Y'                 TO S5035-AGBRDESC-OUT(PR)        <V4L016>
              MOVE 'Y'                 TO S5035-AGCLS-OUT(PR)           <V4L016>
              MOVE 'Y'                 TO S5035-AGCLSD-OUT(PR)          <V4L016>
              MOVE 'Y'                 TO S5035-AGNTBR-OUT(PR)          <V4L016>
              MOVE 'Y'                 TO S5035-AGNUM-OUT(PR)           <V4L016>
              MOVE 'Y'                 TO S5035-AGTYDESC-OUT(PR)        <V4L016>
              MOVE 'Y'                 TO S5035-ARACDE-OUT(PR)          <V4L016>
              MOVE 'Y'                 TO S5035-BCMDESC-OUT(PR)         <V4L016>
              MOVE 'Y'                 TO S5035-BCMTAB-OUT(PR)          <V4L016>
              MOVE 'Y'                 TO S5035-BCTIND-OUT(PR)          <V4L016>
              MOVE 'Y'                 TO S5035-CLIENTIND-OUT(PR)       <V4L016>
              MOVE 'Y'                 TO S5035-CLNTSEL-OUT(PR)         <V4L016>
              MOVE 'Y'                 TO S5035-CLTNAME-OUT(PR)         <V4L016>
              MOVE 'Y'                 TO S5035-CURRCODE-OUT(PR)        <V4L016>
              MOVE 'Y'                 TO S5035-DDIND-OUT(PR)           <V4L016>
              MOVE 'Y'                 TO S5035-DTEAPP-OUT(PR)          <V4L016>
              MOVE 'Y'                 TO S5035-EXCAGR-OUT(PR)          <V4L016>
              MOVE 'Y'                 TO S5035-MINSTA-OUT(PR)          <V4L016>
              MOVE 'Y'                 TO S5035-OCMDESC-OUT(PR)         <V4L016>
              MOVE 'Y'                 TO S5035-OCMTAB-OUT(PR)          <V4L016>
              MOVE 'Y'                 TO S5035-PAYFRQ-OUT(PR)          <V4L016>
              MOVE 'Y'                 TO S5035-PAYMTH-OUT(PR)          <V4L016>
              MOVE 'Y'                 TO S5035-PYFDESC-OUT(PR)         <V4L016>
              MOVE 'Y'                 TO S5035-PYMDESC-OUT(PR)         <V4L016>
              MOVE 'Y'                 TO S5035-RCMDESC-OUT(PR)         <V4L016>
              MOVE 'Y'                 TO S5035-RCMTAB-OUT(PR)          <V4L016>
              MOVE 'Y'                 TO S5035-REPSEL-OUT(PR)          <V4L016>
              MOVE 'Y'                 TO S5035-OVCPC-OUT(PR)           <V4L016>
              MOVE 'Y'                 TO S5035-REPNAME-OUT(PR)         <V4L016>
              MOVE 'Y'                 TO S5035-SCMDSC-OUT(PR)          <V4L016>
              MOVE 'Y'                 TO S5035-SCMTAB-OUT(PR)          <V4L016>
              MOVE 'Y'                 TO S5035-TAGD-OUT(PR)            <V4L016>
              MOVE 'Y'                 TO S5035-AGTYPE-OUT(PR)          <V4L016>
              MOVE 'Y'                 TO S5035-AGLVL-OUT(PR)           <DA007>
              MOVE 'Y'                 TO S5035-AGNTBR-OUT(PR).         <V4L016>
              MOVE 'Y'                 TO S5035-ADDOPT-OUT(PR).         <DA008>
      *                                                                 <V4L016>
      * Reporting To and Date Terminated fields are protected if        <V4L016>
      * agency movement functions are activated.                        <V4L016>
           IF TH605-CRTIND             = 'Y'                            <V4L016>
              IF WSSP-FLAG         NOT = 'A'                            <V4L016>
                 MOVE 'Y'              TO                               <V4L016>
                                          S5035-AGNUM-OUT(PR)           <V4L016>
                                          S5035-OVCPC-OUT(PR)           <V5L001>
                                          S5035-REPSEL-OUT(PR).         <V4L016>
                                                                        <V4L016>
           IF TH605-CRTIND             = 'Y'                            <V4L016>
              IF WSSP-FLAG         NOT = 'T' AND 'R'                    <V4L016>
                 MOVE 'Y'              TO S5035-DTETRM-OUT(PR).         <V4L016>
                                                                        <V4L016>
       1090-EXIT.                                                       <V4L016>
           EXIT.                                                        <V4L016>
      /
      *****************************************************************
       1100-AGENT-NON-EXIST SECTION.
      ******************************
      *****
      *    No details exist for agent. Initialise temporary storage.
      *****
       1110-BRANCH-DETAILS.
           MOVE WSSP-BRANCH            TO S5035-AGNTBR.
           MOVE SPACES                 TO WSAA-OLD-CLNTNUM.
           MOVE SPACES                 TO WSAA-OLD-PAYEE.
      *
       1190-EXIT.
            EXIT.
      /
      *****************************************************************
       1200-AGENT-EXISTS SECTION.
      ******************************
      *
       1210-LOAD-SCREEN.
      *****
      *    Load screen with existing details.
      *****
           MOVE AGNTLAG-AGNTBR         TO S5035-AGNTBR.
           MOVE AGLF-CURRCODE          TO S5035-CURRCODE.
           MOVE AGLF-AGNTNUM           TO S5035-AGNUM.
           MOVE AGLF-DTEAPP            TO S5035-DTEAPP.
           MOVE AGLF-EXCL-AGMT         TO S5035-EXCL-AGMT.
           MOVE AGLF-OVCPC             TO S5035-OVCPC.
           MOVE AGLF-MINSTA            TO S5035-MINSTA.
           MOVE AGLF-TAGSUSIND         TO S5035-TAGSUSIND.              <S01>
           MOVE AGLF-TLAGLICNO         TO S5035-TLAGLICNO.              <S01>
           MOVE AGLF-TLICEXPDT         TO S5035-TLICEXPDT.              <S01>
           IF   AGLF-DTETRM NOT = VRCM-MAX-DATE                         <009>
                MOVE AGLF-DTETRM       TO S5035-DTETRM                  <009>
                MOVE AGLF-DTETRM       TO WSAA-ORIG-DTETRM              <009>
           ELSE                                                         <009>
                MOVE VRCM-MAX-DATE     TO S5035-DTETRM.                 <009>
      *    MOVE AGLF-BMAFLG            TO S5035-BMAFLG.
           MOVE AGLF-BMAFLG            TO S5035-ZBMAFLG.
           MOVE AGLF-BMAFLG            TO WSAA-ORIG-BMAFLG.
           MOVE AGNTLAG-CLNTNUM        TO WSAA-OLD-CLNTNUM.
           IF AGLF-PAYCLT NOT = SPACES
              MOVE AGLF-PAYCLT         TO WSAA-OLD-PAYEE
           ELSE
              MOVE AGNTLAG-CLNTNUM     TO WSAA-OLD-PAYEE.
      * Check Agent MRDT                                                <NB031>
           MOVE SPACE                  TO S5035-MRDTFLAG.               <NB031>
           PERFORM A3000-CHECK-AGENT-MRDT.                              <NB031>
      *
       1220-CLIENT-DETS.
      *****
      *    Read the Clients file for processing the clients name.
      *****
           MOVE AGNTLAG-CLNTNUM        TO S5035-CLNTSEL,
                                          WSAA-SAVE-CLNTSEL.            <021>

           MOVE SPACES                 TO CLTS-DATA-KEY.
           MOVE AGNTLAG-CLNTNUM        TO CLTS-CLNTNUM.

           PERFORM 1700-CLTSIO-CALL.

           MOVE WSSP-LONGCONFNAME      TO S5035-CLTNAME.

           MOVE '+'                    TO S5035-CLIENTIND.
      *
       1230-AGENT-TYPE.
      *****
      *    Read the Agent type and description .
      *****
           MOVE AGNTLAG-AGTYPE         TO S5035-AGTYPE.
           MOVE AGNTLAG-AGTYPE         TO WSAA-AGTYPE-SAVE.             <DA023>

           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE T3692                  TO DESC-DESCTABL.
           MOVE AGNTLAG-AGTYPE         TO DESC-DESCITEM.
           MOVE READR                  TO DESC-FUNCTION.

           PERFORM 1600-DESCIO-CALL.

           MOVE DESC-LONGDESC          TO S5035-AGTYDESC.
      *
       1240-AGENT-AREA.
      *****
      *    Read the Agent Area and description.
      *****
           MOVE AGLF-ARACDE            TO S5035-ARACDE.
           MOVE AGLF-TSALESUNT         TO S5035-TSALESUNT.              <S02>

           MOVE AGLF-ARACDE            TO WSAA-REGION-OLD.              <PHE003>
           MOVE AGLF-TSALESUNT         TO WSAA-UNIT-OLD.                <PHE003>
                                                                        <PHE003>
           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE T5696                  TO DESC-DESCTABL.
           MOVE AGLF-ARACDE            TO DESC-DESCITEM.
           MOVE READR                  TO DESC-FUNCTION.

           PERFORM 1600-DESCIO-CALL.

           MOVE DESC-LONGDESC          TO S5035-ARADESC.
                                                                        <S02>
           MOVE SPACES                 TO DESC-DATA-KEY.                <S02>
           MOVE TT518                  TO DESC-DESCTABL.                <S02>
           MOVE AGLF-TSALESUNT         TO DESC-DESCITEM.                <S02>
                                                                        <S02>
           PERFORM 1600-DESCIO-CALL.                                    <S02>
                                                                        <S02>
           MOVE DESC-LONGDESC          TO S5035-TSALESDSC.              <S02>
           IF S5035-TSALESUNT          = SPACES                         <S02>
              MOVE SPACES              TO S5035-TSALESDSC               <S02>
           END-IF.                                                      <S02>
      *
       1250-REPORTING-TO.
      *****
      *    Read the Agents reporting to name from the clients file.
      *****
           MOVE AGLF-REPORTAG          TO S5035-REPSEL.                 <CAS1.0>
           MOVE AGLF-REPORTAG          TO WSAA-INIT-REPSEL.             <LA1174>
           MOVE SPACES                 TO S5035-REPNAME.

           IF AGLF-REPORTAG = SPACES
              GO TO 1260-PAYEE.

           MOVE SPACES                 TO AGNTLAG-DATA-KEY.
           MOVE AGLF-REPORTAG          TO AGNTLAG-AGNTNUM.

           PERFORM 1500-AGNTLAGIO-CALL.

           IF AGNTLAG-STATUZ           NOT = O-K
              MOVE AGNTLAG-PARAMS      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.

           MOVE SPACES                 TO CLTS-DATA-KEY.
           MOVE AGNTLAG-CLNTNUM        TO CLTS-CLNTNUM.

           PERFORM 1700-CLTSIO-CALL.

           MOVE WSSP-LONGCONFNAME      TO S5035-REPNAME.
      *
       1260-PAYEE.
      *****
      *    Read the Payee name from the clients file.
      *****
           IF AGLF-PAYCLT              = SPACES
              GO TO 1270-PAY-METHOD.

           IF AGLF-PAYCLT              = AGNTLAG-CLNTNUM
           AND AGLF-AGCCQIND           NOT = 'Y'                        <V5L003>
              MOVE SPACES              TO S5035-PAYSEL                  <013>
              GO TO 1270-PAY-METHOD.

           MOVE AGLF-PAYCLT            TO S5035-PAYSEL.                 <013>
           MOVE SPACES                 TO S5035-PAYENME.                <013>

           MOVE SPACES                 TO CLTS-DATA-KEY.
           MOVE AGLF-PAYCLT            TO CLTS-CLNTNUM.

           PERFORM 1700-CLTSIO-CALL.

           MOVE WSSP-LONGCONFNAME      TO S5035-PAYENME.                <013>
      *
       1270-PAY-METHOD.
      *****
      *    Read the Payment method and description.
      *****
           MOVE AGLF-PAYMTH            TO S5035-PAYMTH.

           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE T5690                  TO DESC-DESCTABL.
           MOVE AGLF-PAYMTH            TO DESC-DESCITEM.
           MOVE READR                  TO DESC-FUNCTION.

           PERFORM 1600-DESCIO-CALL.

           MOVE DESC-LONGDESC          TO S5035-PYMDESC.
      *
       1280-PAY-FREQUENCY.
      *****
      *    Read the Payment Frequency and its description.
      *****
           MOVE AGLF-PAYFRQ            TO S5035-PAYFRQ.

           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE T5630                  TO DESC-DESCTABL.
           MOVE AGLF-PAYFRQ            TO DESC-DESCITEM.

           PERFORM 1600-DESCIO-CALL.

           MOVE DESC-LONGDESC          TO S5035-PYFDESC.
      *
       1290-COMMISSION-RATES.

      *****
      *    The four commissions are handled in a distinct section.
      *****
           PERFORM 1400-COMM-TABLE-CALL.
      *
       1300-AGENT-CLASS.
      *****
      *    Read the agent Class description.
      *****
           MOVE AGLF-AGENT-CLASS       TO S5035-AGENT-CLASS.

           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE AGLF-AGENT-CLASS       TO DESC-DESCITEM.
           MOVE T5699                  TO DESC-DESCTABL.

           PERFORM 1600-DESCIO-CALL.

           MOVE DESC-LONGDESC          TO S5035-AGCLSD.
      *
      * Protect the Client Number.                                      <008>
      *                                                                 <008>
      ***  MOVE 'Y'                    TO S5035-CLNTSEL-OUT(PR).        <008>
           MOVE 'Y'                    TO S5035-CLNTSEL-OUT(PR).        <030>
      *
       1310-CHECK-SELECT-INDICATORS.
      *****
      *    The following have been named A,B & C for use within the
      *    1000 section and the 4000 section.
      *
      *    The first will check for changes in the bank details through
      *    the s5037 screen.
      *    The second  will check  for  changes to  the Broker Contacts
      *    details through screen s5038.
      *    The third  will check for  changes to the Tied Agent details
      *    through screen s5036.
      *
      *    Note: 1000 section will only set the flags according to what
      *          it finds in the AGLF and AGNTLAG I/O Modules.
      *****
           PERFORM A100-BANK-DETS-CHECK.
           PERFORM B100-BROKER-CONTACTS.
           PERFORM C100-TIED-AGENCIES.
      **** PERFORM D100-OR-DETAILS.                                     <CAS1.0>
           PERFORM D200-OR-DETAILS.                                     <V4L014>
      *
       1320-REASON-DESC.
           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE TZ001                  TO DESC-DESCTABL.
           MOVE AGLF-BMAFLG            TO DESC-DESCITEM
      *                                   S5035-BMAFLG.
                                          S5035-ZBMAFLG.
           MOVE READR                  TO DESC-FUNCTION.

           MOVE 'IT'                   TO DESC-DESCPFX.
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.
           MOVE SPACES                 TO DESC-LONGDESC.

           CALL 'DESCIO' USING DESC-PARAMS.

   ****    IF DESC-STATUZ              = MRNF AND
   ****       WSSP-SBMACTION           = 'C'

   ****       IF  S5035-BMAFLG         = ' '
   ****         MOVE RLAG              TO S5035-BMAFLG-ERR
   ****       ELSE
   ****         MOVE BLAG              TO S5035-BMAFLG-ERR
   ****       END-IF.

              IF DESC-STATUZ           = MRNF
                  MOVE O-K             TO DESC-STATUZ
                  MOVE SPACES          TO S5035-BMADES
              IF DESC-STATUZ       NOT = O-K
                 MOVE DESC-PARAMS         TO SYSR-PARAMS
                 PERFORM 600-FATAL-ERROR.

           MOVE DESC-LONGDESC          TO S5035-BMADES.

TDO   * Check Agent Club Class if any:                                  <DA007>
      *    PERFORM X100-GET-AGENT-CLUB-INFO.                    <DA008> <DA007>
           PERFORM X400-GET-CLUB-CLASS.                                 <DA008>
           IF AGLVTDA-STATUZ           = O-K                            <DA008>
           AND AGLVTDA-AGNTNUM         = S5035-AGNUM                    <DA008>
           AND AGLVTDA-VALIDFLAG       = '1'                            <DA008>
               MOVE AGLVTDA-AGLVL      TO S5035-AGLVL                   <DA008>
                                          WSAA-AGLVL-STORE              <DA008>
               IF AGLVTDA-EFFDATE      NOT = ZEROES                     <DA008>
               AND AGLVTDA-EFFDATE     NOT = VRCM-MAX-DATE              <DA008>
               AND S5035-AGLVL         NOT = SPACES                     <DA008>
               OR WSSP-FLAG            = 'I'                            <DA008>
                  MOVE '+'             TO S5035-ADDOPT                  <DA008>
               END-IF                                                   <DA008>
           ELSE                                                         <DA008>
               MOVE SPACES             TO S5035-AGLVL                   <DA008>
                                          S5035-AGTYDESC                <DA008>
                                          S5035-ADDOPT                  <DA008>
           END-IF.                                                      <DA008>
                                                                        <DA008>
           IF S5035-AGLVL              NOT = SPACES                     <DA008>
               PERFORM X110-GET-CLUB-DESC                               <DA008>
           END-IF.                                                      <DA008>
       1390-EXIT.
            EXIT.
      /                                                                 <DA007>
       X100-GET-AGENT-CLUB-INFO SECTION.                                <DA007>
      **********************************                                <DA007>
       X101-START.                                                      <DA007>
      *                                                                 <DA007>
           INITIALIZE                  AGLV-PARAMS.                     <DA007>
           MOVE S5035-AGNUM            TO AGLV-AGNTNUM.                 <DA007>
           MOVE 99999999               TO AGLV-TRANDATE.                <DA007>
           MOVE 99999999               TO AGLV-EFFDATE.                 <DA008>
           MOVE AGLVREC                TO AGLV-FORMAT.                  <DA007>
           MOVE BEGN                   TO AGLV-FUNCTION.                <DA007>
                                                                        <DA007>
           CALL 'AGLVIO'               USING AGLV-PARAMS.               <DA007>
                                                                        <DA007>
           IF AGLV-STATUZ              NOT = O-K                        <DA007>
           AND                         NOT = ENDP                       <DA007>
               MOVE AGLV-PARAMS        TO SYSR-PARAMS                   <DA007>
               PERFORM 600-FATAL-ERROR                                  <DA007>
           END-IF.                                                      <DA007>
                                                                        <DA008>
           IF AGLV-STATUZ              = O-K                            <DA007>
           AND AGLV-AGNTNUM            = S5035-AGNUM                    <DA007>
           AND AGLV-VALIDFLAG          = '1'                            <DA007>
               MOVE AGLV-AGLVL         TO S5035-AGLVL                   <DA007>
                                          WSAA-AGLVL-STORE              <DA007>
               IF AGLV-EFFDATE         NOT = ZEROES                     <DA008>
               AND AGLV-EFFDATE        NOT = VRCM-MAX-DATE              <DA008>
               AND S5035-AGLVL         NOT = SPACES                     <DA008>
                  MOVE '+'             TO S5035-ADDOPT                  <DA008>
               END-IF                                                   <DA008>
           ELSE                                                         <DA007>
               MOVE SPACES             TO S5035-AGLVL                   <DA007>
                                          S5035-AGTYDESC                <DA007>
                                          S5035-ADDOPT                  <DA008>
           END-IF.                                                      <DA007>
                                                                        <DA007>
           IF S5035-AGLVL              NOT = SPACES                     <DA007>
               PERFORM X110-GET-CLUB-DESC                               <DA007>
           END-IF.                                                      <DA007>
      *                                                                 <DA007>
       X109-EXIT.                                                       <DA007>
           EXIT.                                                        <DA007>
      /                                                                 <DA007>
       X110-GET-CLUB-DESC SECTION.                                      <DA007>
      ****************************                                      <DA007>
       X111-START.                                                      <DA007>
      *                                                                 <DA007>
           MOVE SPACES                 TO S5035-AGLVLDES.               <DA007>
                                                                        <DA007>
           INITIALIZE                  DESC-PARAMS.                     <DA007>
           MOVE 'IT'                   TO DESC-DESCPFX.                 <DA007>
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.                 <DA007>
           MOVE TV102                  TO DESC-DESCTABL.                <DA007>
           MOVE S5035-AGLVL            TO DESC-DESCITEM.                <DA007>
           MOVE SPACES                 TO DESC-ITEMSEQ.                 <DA007>
           MOVE 'E'                    TO DESC-LANGUAGE.                <DA007>
           MOVE DESCREC                TO DESC-FORMAT.                  <DA007>
           MOVE READR                  TO DESC-FUNCTION.                <DA007>
                                                                        <DA007>
           CALL 'DESCIO'               USING DESC-PARAMS.               <DA007>
                                                                        <DA007>
           IF DESC-STATUZ              NOT = O-K                        <DA007>
           AND                         NOT = MRNF                       <DA007>
               MOVE DESC-PARAMS        TO SYSR-PARAMS                   <DA007>
               PERFORM 600-FATAL-ERROR                                  <DA007>
           END-IF.                                                      <DA007>
                                                                        <DA007>
           IF DESC-STATUZ              = O-K                            <DA007>
               MOVE DESC-SHORTDESC     TO S5035-AGLVLDES                <DA007>
           END-IF.                                                      <DA007>
      *                                                                 <DA007>
       X119-EXIT.                                                       <DA007>
           EXIT.                                                        <DA007>
      /
      *****************************************************************
       1400-COMM-TABLE-CALL SECTION.
      **************************
      *
       1410-BASIC-COMM-DESC.
      *****
      *    Find the basic commission description.
      *****

           MOVE AGLF-BCMTAB            TO S5035-BCMTAB.

           IF S5035-BCMTAB = SPACES
              GO TO 1420-SERVICE-COMM-DESC.

           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE AGLF-BCMTAB            TO DESC-DESCITEM.
           MOVE T5644                  TO DESC-DESCTABL.

           PERFORM 1600-DESCIO-CALL.

           MOVE DESC-LONGDESC          TO S5035-BCMDESC.
      *
       1420-SERVICE-COMM-DESC.
      *****
      *    Find the Service commission description.
      *****

           MOVE AGLF-SCMTAB            TO S5035-SCMTAB.

           IF S5035-SCMTAB = SPACES
              GO TO 1430-RENEWAL-COMM-DESC.

           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE AGLF-SCMTAB            TO DESC-DESCITEM.
           MOVE T5644                  TO DESC-DESCTABL.

           PERFORM 1600-DESCIO-CALL.

           MOVE DESC-LONGDESC          TO S5035-SCMDSC.
      *
       1430-RENEWAL-COMM-DESC.
      *****
      *    Find the Renewal commission description.
      *****

           MOVE AGLF-RCMTAB         TO S5035-RCMTAB.

           IF S5035-RCMTAB = SPACES
              GO TO 1440-BONUS-COMM-DESC.

           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE AGLF-RCMTAB            TO DESC-DESCITEM.
           MOVE T5644                  TO DESC-DESCTABL.

           PERFORM 1600-DESCIO-CALL.

           MOVE DESC-LONGDESC          TO S5035-RCMDESC.
      *
       1440-BONUS-COMM-DESC.
      *****
      *    Find the Bonus commission description.
      *****

           MOVE AGLF-OCMTAB            TO S5035-OCMTAB.

           IF S5035-OCMTAB = SPACES
              GO TO 1490-EXIT.

           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE AGLF-OCMTAB            TO DESC-DESCITEM.
***********MOVE T5644                  TO DESC-DESCTABL.                <017>
           MOVE T5697                  TO DESC-DESCTABL.                <017>

           PERFORM 1600-DESCIO-CALL.

           MOVE DESC-LONGDESC          TO S5035-OCMDESC.
      *
       1490-EXIT.
            EXIT.
      /
      *****************************************************************
       1500-AGNTLAGIO-CALL SECTION.
      **************************
      *
       1510-CALL.
      *****
      *    Read the Agent details.
      *****
           MOVE WSSP-COMPANY           TO AGNTLAG-AGNTCOY.
           MOVE READR                  TO AGNTLAG-FUNCTION.

           CALL 'AGNTLAGIO' USING AGNTLAG-PARAMS.

           IF AGNTLAG-STATUZ           NOT = O-K
                                   AND NOT = MRNF
              MOVE AGNTLAG-PARAMS      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.

       1590-EXIT.
            EXIT.
      /
      *****************************************************************
       1600-DESCIO-CALL SECTION.
      **************************
      *
       1610-DESCRIPTION.
      *****
      *    Call the Description I/O module
      *****

           MOVE 'IT'                   TO DESC-DESCPFX.
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.
           MOVE READR                  TO DESC-FUNCTION.

           CALL 'DESCIO' USING DESC-PARAMS.

           IF DESC-STATUZ              = MRNF
               MOVE O-K                TO DESC-STATUZ
               MOVE ALL '?'            TO DESC-LONGDESC.

           IF DESC-STATUZ              NOT = O-K
                                   AND NOT = MRNF
              MOVE DESC-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.
      *
       1690-EXIT.
            EXIT.
      /
      *****************************************************************
       1700-CLTSIO-CALL SECTION.
      **************************
      *
       1710-DESCRIPTION.
      *****
      *    Call the Clients Description I/O module
      *****
           MOVE 'CN'                   TO CLTS-CLNTPFX.
           MOVE WSSP-FSUCO             TO CLTS-CLNTCOY.
           MOVE READR                  TO CLTS-FUNCTION.

           CALL 'CLTSIO'               USING CLTS-PARAMS.

           IF CLTS-STATUZ              NOT = O-K
              MOVE CLTS-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.

      *****
      *    Call the Subroutine to format the Clients Name.
      *****
           PERFORM PLAINNAME.
      *
       1790-EXIT.
            EXIT.
      /
       1800-GET-ORINDIC SECTION.                                        <V4L014>
      **************************                                        <V4L014>
      *                                                                 <V4L014>
       1810-READ.                                                       <V4L014>
      *                                                         <V4L014><L01>
           MOVE SPACES                 TO ITEM-DATA-KEY.                <V4L014>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <V4L014>
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.                 <V4L014>
           MOVE TH605                  TO ITEM-ITEMTABL.                <V4L014>
           MOVE WSSP-COMPANY           TO ITEM-ITEMITEM.                <V4L014>
           MOVE READR                  TO ITEM-FUNCTION.                <V4L014>
                                                                        <V4L014>
           CALL 'ITEMIO'               USING ITEM-PARAMS.               <V4L014>
                                                                        <V4L014>
           IF ITEM-STATUZ           NOT = O-K AND MRNF                  <V4L014>
              MOVE ITEM-STATUZ         TO SYSR-STATUZ                   <V4L014>
              MOVE ITEM-PARAMS         TO SYSR-PARAMS                   <V4L014>
              PERFORM 600-FATAL-ERROR                                   <V4L014>
           END-IF.                                                      <V4L014>
      *                                                                 <V4L014>
           MOVE ITEM-GENAREA           TO TH605-TH605-REC.              <V4L014>
           MOVE TH605-INDIC            TO WSAA-ORINDIC.                 <V4L014>
      *                                                         <V4L014><S9503>
       1890-EXIT.                                                       <S9503>
           EXIT.                                                        <S9503>
      *****************************************************************
       A100-BANK-DETS-CHECK SECTION.
      ******************************
      *
       A110-BANK-DETS-CHECK.
           IF AGLF-BANKKEY             NOT = SPACES
           OR AGLF-BANKACCKEY          NOT = SPACES
              MOVE '+'                 TO S5035-DDIND
           ELSE
              MOVE ' '                 TO S5035-DDIND.
      *
       A100-EXIT.
            EXIT.
      /
      *****************************************************************
       B100-BROKER-CONTACTS SECTION.
      ******************************
      *
       B110-BROKER-CONTACTS.
           MOVE SPACES                 TO AGBN-DATA-KEY.
           MOVE WSSP-COMPANY           TO AGBN-AGNTCOY.
           MOVE AGLF-AGNTNUM           TO AGBN-AGNTNUM.
           MOVE SPACES                 TO AGBN-CONTACT.
           MOVE BEGN                   TO AGBN-FUNCTION.

           CALL 'AGBNIO' USING AGBN-PARAMS.

           IF AGBN-STATUZ              NOT = O-K
                                   AND NOT = ENDP
              MOVE AGBN-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.

           IF WSSP-COMPANY NOT = AGBN-AGNTCOY
             OR AGLF-AGNTNUM NOT = AGBN-AGNTNUM
              MOVE ENDP                TO AGBN-STATUZ.

           IF AGBN-STATUZ              = O-K
              MOVE '+'                 TO S5035-BCTIND
           ELSE
              MOVE ' '                 TO S5035-BCTIND.
      *
       B100-EXIT.
            EXIT.
      /
      *****************************************************************
       C100-TIED-AGENCIES SECTION.
      ****************************
      *
       C110-TIED-AGENCIES.
      *                                                                 <AG002>
           PERFORM N000-CHECK-INTRO-EXIST.                              <AG002>
                                                                        <AG002>
           IF AGLF-TAXMETH             NOT = SPACES
           OR AGLF-IRDNO               NOT = SPACES
           OR AGLF-TAXCDE              NOT = SPACES
           OR AGLF-TAXALW              NOT = ZEROES
           OR AGLF-SPRSCHM             NOT = SPACES
           OR AGLF-SPRPRC              NOT = ZEROES
           OR AGLF-INTCRD              NOT = ZEROES
           OR AGLF-TCOLPRCT            NOT = ZEROES                     <S03>
           OR AGLF-TCOLMAX             NOT = ZEROES                     <S03>
           OR AGLF-FIXPRC              NOT = ZEROES
           OR AGLF-BMAFLG              NOT = SPACES
           OR AGLF-HOUSE-LOAN          NOT = SPACES
           OR AGLF-COMPUTER-LOAN       NOT = SPACES
           OR AGLF-CAR-LOAN            NOT = SPACES
           OR AGLF-OFFICE-RENT         NOT = SPACES
           OR AGLF-OTHER-LOANS         NOT = SPACES
           OR AGLF-AGCCQIND                = 'Y'                        <V5L003>
           OR WSAA-INTRO-EXIST-FLAG        = 'Y'                        <AG002>
              MOVE '+'                 TO S5035-TAGD
           ELSE
              MOVE ' '                 TO S5035-TAGD.
      *
       C100-EXIT.
            EXIT.
                                                                        <CAS1.0>
      ***************************                                       <CAS1.0>
       D100-OR-DETAILS   SECTION.                                       <CAS1.0>
      ***************************                                       <CAS1.0>
                                                                        <CAS1.0>
           INITIALIZE ZRAP-DATA-AREA.                                   <CAS1.0>
           MOVE AGLF-AGNTNUM           TO ZRAP-AGNTNUM.                 <CAS1.0>
           MOVE AGLF-AGNTCOY           TO ZRAP-AGNTCOY.                 <CAS1.0>
           MOVE BEGN                   TO ZRAP-FUNCTION.                <CAS1.0>
                                                                        <CAS1.0>
           CALL 'ZRAPIO'               USING ZRAP-PARAMS.               <CAS1.0>
                                                                        <CAS1.0>
           IF ZRAP-STATUZ           NOT = O-K AND ENDP                  <CAS1.0>
              MOVE ZRAP-PARAMS         TO SYSR-PARAMS                   <CAS1.0>
              MOVE ZRAP-STATUZ         TO SYSR-STATUZ                   <CAS1.0>
              PERFORM 600-FATAL-ERROR                                   <CAS1.0>
           END-IF.                                                      <CAS1.0>
                                                                        <CAS1.0>
           IF ZRAP-AGNTCOY         NOT = AGLF-AGNTCOY                   <CAS1.0>
           OR ZRAP-AGNTNUM         NOT = AGLF-AGNTNUM                   <CAS1.0>
           OR ZRAP-STATUZ              = ENDP                           <CAS1.0>
              MOVE ' '                 TO S5035-ZRORIND                 <CAS1.0>
           ELSE                                                         <CAS1.0>
              MOVE '+'                 TO S5035-ZRORIND                 <CAS1.0>
           END-IF.                                                      <CAS1.0>
                                                                        <CAS1.0>
      ****                                                              <CAS1.0>
      *D101-OR-DETIALS.                                                 <CAS1.0>
      ****                                                              <CAS1.0>
      **** IF AGLF-ZRORCODE             = SPACES                        <CAS1.0>
      **** OR AGLF-REPORTAG             = SPACES                        <CAS1.0>
      ****    MOVE ' '                 TO S5035-ZRORIND                 <CAS1.0>
      **** ELSE                                                         <CAS1.0>
      ****    MOVE '+'                 TO S5035-ZRORIND                 <CAS1.0>
      **** END-IF.                                                      <CAS1.0>
      ****                                                              <CAS1.0>
       D199-EXIT.                                                       <CAS1.0>
            EXIT.                                                       <CAS1.0>
                                                                        <CAS1.0>
      ***************************                               <V4L014><V4L014>
       D200-OR-DETAILS   SECTION.                                       <V4L014>
      ***************************                               <V4L014><V4L014>
                                                                        <V4L014>
           INITIALIZE AGORREC-NON-KEY-DATA.                             <V4L014>
           MOVE AGLF-AGNTNUM           TO AGOR-AGNTNUM.                 <V4L014>
           MOVE AGLF-AGNTCOY           TO AGOR-AGNTCOY.                 <V4L014>
           MOVE BEGN                   TO AGOR-FUNCTION.                <V4L014>
                                                                        <V4L014>
           CALL 'AGORIO'               USING AGOR-PARAMS.               <V4L014>
                                                                        <V4L014>
           IF AGOR-STATUZ           NOT = O-K AND ENDP                  <V4L014>
              MOVE AGOR-PARAMS         TO SYSR-PARAMS                   <V4L014>
              MOVE AGOR-STATUZ         TO SYSR-STATUZ                   <V4L014>
              PERFORM 600-FATAL-ERROR                                   <V4L014>
           END-IF.                                                      <V4L014>
                                                                        <V4L014>
           IF AGOR-STATUZ               =  ENDP                         <V4L014>
              MOVE ' '                 TO S5035-ZRORIND                 <V4L014>
              GO TO D299-EXIT                                           <V4L014>
           END-IF.                                                      <V4L014>
                                                                        <V4L014>
           IF AGOR-AGNTCOY             = AGLF-AGNTCOY AND               <V4L014>
              AGOR-AGNTNUM             = AGLF-AGNTNUM                   <V4L014>
              MOVE '+'                 TO S5035-ZRORIND                 <V4L014>
           ELSE                                                         <V4L014>
              MOVE ' '                 TO S5035-ZRORIND                 <V4L014>
           END-IF.                                                      <V4L014>
                                                                        <V4L014>
       D299-EXIT.                                                       <V4L014>
            EXIT.                                                       <V4L014>
                                                                        <V4L014>
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
      *****                                                             <S9503>
      *    Skip  this section if  returning from an optional selection  <S9503>
      *    (current stack position action flag = '*').                  <S9503>
      *****                                                             <S9503>
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'                    <S9503>
              MOVE O-K                 TO WSSP-EDTERROR                 <S9503>
              MOVE 3000                TO WSSP-SECTIONNO                <S9503>
           GO TO PRE-EXIT.                                              <S9503>
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
      *    CALL 'S5035IO' USING SCRN-SCREEN-PARAMS                      <S9503>
      *                          S5035-DATA-AREA.                       <S9503>
      * Screen errors are now handled in the calling program.           <S9503>
      *    PERFORM 200-SCREEN-ERRORS.                                   <S9503>
           MOVE O-K                    TO WSSP-EDTERROR.
      *
       2020-VALIDATE-SELECT.
      *****
      *    Error if the Selections do not equal Space, + or X.
      *****
           IF S5035-CLIENTIND          NOT = ' ' AND '+' AND 'X'        <015>
              MOVE  H118               TO S5035-CLIENTIND-ERR.          <015>
                                                                        <015>
           IF S5035-DDIND              NOT = ' ' AND '+' AND 'X'
              MOVE  H118               TO S5035-DDIND-ERR.              <015>
      ********MOVE  E186               TO S5035-DDIND-ERR.              <015>

           IF S5035-BCTIND             NOT = ' ' AND '+' AND 'X'
              MOVE  H118               TO S5035-BCTIND-ERR.             <015>
      ********MOVE  E186               TO S5035-BCTIND-ERR.             <015>

      **** IF S5035-ZRORIND         NOT = ' ' AND '+' AND 'X'           <CAS1.0>
      ****    MOVE  H118               TO S5035-ZRORIND-ERR             <CAS1.0>
      **** END-IF.                                                      <CAS1.0>
           IF S5035-ZRORIND         NOT = ' ' AND '+' AND 'X'           <V4L014>
              MOVE  H118               TO S5035-ZRORIND-ERR             <V4L014>
           END-IF.                                                      <V4L014>
                                                                        <DA008>
           IF S5035-ADDOPT          NOT = ' ' AND '+' AND 'X'           <DA008>
              MOVE  H118               TO S5035-ADDOPT-ERR              <DA008>
           END-IF.                                                      <DA008>
                                                                        <V4L014>
           IF S5035-DTETRM          = VRCM-MAX-DATE AND                 <V4L016>
              WSSP-FLAG             = 'T'                               <V4L016>
              MOVE  E186               TO S5035-DTETRM-ERR              <V4L016>
           END-IF.                                                      <V4L016>
                                                                        <CAS1.0>
           IF S5035-DTETRM          NOT = VRCM-MAX-DATE AND             <V4L016>
              WSSP-FLAG             = 'R'                               <V4L016>
              MOVE  EV03               TO S5035-DTETRM-ERR              <V4L016>
           END-IF.                                                      <V4L016>
                                                                        <V4L016>
           IF S5035-TAGD               NOT = ' ' AND '+' AND 'X'
              MOVE  H118               TO S5035-TAGD-ERR.               <015>
      ********MOVE  E186               TO S5035-TAGD-ERR.               <015>
      *
       2030-ENQUIRY-CHECK.
      *****
      *    If entering on enquiry then skip Validation.
      *****
           IF WSSP-FLAG                = 'I'
              GO TO 2090-EXIT.

           PERFORM 2100-VALIDATE-ALL-FIELDS.
      *
       2040-CHECK-FOR-ERRORS.
      *****
      *    If an error exists on Validation set flag to loop through
      *    2000 section.
      *****
           IF S5035-ERROR-INDICATORS NOT = SPACES
              MOVE 'Y'                 TO WSSP-EDTERROR.
      *
       2050-REDISPLAY-ON-CALC.
      *****
      *    If 'CALC' was entered then re-display the screen.
      *****
           IF SCRN-STATUZ              = 'CALC'
              MOVE 'Y'                 TO WSSP-EDTERROR.
      *
       2090-EXIT.
            EXIT.
      /
      *****************************************************************
      *
       2100-VALIDATE-ALL-FIELDS SECTION.
      **********************************
      *
       2110-VALIDATE-ALL-FIELDS.
      *****
      *    Validate all necessary fields.
      *****
      *****
      *    Validate Client number.
      *****
           IF  S5035-CLNTSEL        NOT = SPACES                        <PHFX01>
           AND WSSP-SBMACTION           =  'A'                          <PHFX01>
               INITIALIZE                 AGNTCLN-PARAMS                <PHFX01>
               MOVE '9'                TO AGNTCLN-CLNTCOY               <PHFX01>
               MOVE S5035-CLNTSEL      TO AGNTCLN-CLNTNUM               <PHFX01>
               MOVE READR              TO AGNTCLN-FUNCTION              <PHFX01>
               MOVE AGNTCLNREC         TO AGNTCLN-FORMAT                <PHFX01>
                                                                        <PHFX01>
               CALL 'AGNTCLNIO'     USING AGNTCLN-PARAMS                <PHFX01>
                                                                        <PHFX01>
               IF  AGNTCLN-STATUZ   NOT = O-K AND MRNF                  <PHFX01>
                   MOVE AGNTCLN-STATUZ     TO SYSR-STATUZ               <PHFX01>
                   MOVE AGNTCLN-STATUZ     TO SYSR-PARAMS               <PHFX01>
               END-IF                                                   <PHFX01>
                                                                        <PHFX01>
               IF  AGNTCLN-STATUZ           = O-K                       <PHFX01>
                   MOVE E321               TO S5035-CLNTSEL-ERR         <PHFX01>
               END-IF                                                   <PHFX01>
           END-IF.                                                      <PHFX01>
                                                                        <PHFX01>
           IF S5035-CLNTSEL            = SPACES
              MOVE E186                TO S5035-CLNTSEL-ERR
           ELSE
<003>**       IF S5035-CLNTSEL-OUT (CHG)    = 'Y'
                 PERFORM 2120-CHG-CLIENT.

           IF (S5035-CLNTSEL       NOT = WSAA-SAVE-CLNTSEL) AND         <021>
              (S5035-DDIND             = '+')                           <021>
              MOVE 'X'                 TO S5035-DDIND                   <021>
              MOVE S5035-CLNTSEL       TO WSAA-SAVE-CLNTSEL.            <021>
                                                                        <021>
           IF (S5035-CLNTSEL           = WSAA-SAVE-CLNTSEL) AND         <021>
              (S5035-DDIND             = '+')                           <021>
                                                                        <021>
              MOVE RETRV               TO AGLF-FUNCTION                 <021>
                                                                        <021>
              CALL 'AGLFIO'            USING AGLF-PARAMS                <021>
                                                                        <021>
              IF AGLF-STATUZ       NOT = O-K                            <021>
                 MOVE AGLF-PARAMS      TO SYSR-PARAMS                   <021>
                 MOVE AGLF-STATUZ      TO SYSR-STATUZ                   <021>
                 PERFORM 600-FATAL-ERROR                                <021>
              END-IF                                                    <021>
                                                                        <021>
              IF (AGLF-PAYCLT      NOT = SPACES) AND                    <021>
                 (S5035-PAYSEL     NOT = SPACES)                        <021>
                 IF (AGLF-PAYCLT   NOT = S5035-PAYSEL)                  <021>
                    MOVE 'X'           TO S5035-DDIND                   <021>
                    GO TO 2115-CONTINUE                                 <021>
                 END-IF                                                 <021>
              END-IF                                                    <021>
                                                                        <021>
              MOVE SPACES              TO CLBL-PARAMS                   <021>
              MOVE CLBLREC             TO CLBL-FORMAT                   <021>
              MOVE AGLF-BANKKEY        TO CLBL-BANKKEY                  <021>
              MOVE AGLF-BANKACCKEY     TO CLBL-BANKACCKEY               <021>
              MOVE WSSP-FSUCO          TO CLBL-CLNTCOY                  <V65F14>
              IF S5035-PAYSEL       NOT = SPACES                        <V65F14>
                 MOVE S5035-PAYSEL     TO CLBL-CLNTNUM                  <V65F14>
              ELSE                                                      <V65F14>
                 MOVE S5035-CLNTSEL    TO CLBL-CLNTNUM                  <V65F14>
              END-IF                                                    <V65F14>
              MOVE READR               TO CLBL-FUNCTION                 <021>
                                                                        <021>
              CALL 'CLBLIO'            USING CLBL-PARAMS                <021>
                                                                        <021>
              IF CLBL-STATUZ       NOT = O-K                            <021>
                 MOVE CLBL-PARAMS      TO SYSR-PARAMS                   <021>
                 MOVE CLBL-STATUZ      TO SYSR-STATUZ                   <021>
                 PERFORM 600-FATAL-ERROR                                <021>
              END-IF                                                    <021>
                                                                        <025>
              IF S5035-PAYSEL             = SPACES                      <025>
                 IF S5035-CLNTSEL        NOT = CLBL-CLNTNUM             <025>
                    MOVE 'X'                TO S5035-DDIND              <025>
                 END-IF                                                 <025>
              ELSE                                                      <025>
                 IF S5035-PAYSEL         NOT = CLBL-CLNTNUM             <025>
                    MOVE 'X'                TO S5035-DDIND              <025>
                 END-IF                                                 <025>
              END-IF                                                    <025>
                                                                        <021>
*******       IF S5035-CLNTSEL     NOT = CLBL-CLNTNUM              <021><025>
*******          MOVE 'X'              TO S5035-DDIND              <021><025>
*******       END-IF                                               <021><025>
                                                                        <021>
           END-IF.                                                      <021>
                                                                        <DA008>
      * Check exist club class details.                                 <DA008>
           IF S5035-AGLVL               = SPACES                        <DA008>
           AND S5035-ADDOPT         NOT = SPACES                        <DA008>
              MOVE 'EZ26'              TO S5035-ADDOPT-ERR              <DA008>
           END-IF.                                                      <DA008>
                                                                        <DA008>
           IF ( WSSP-FLAG               = 'A'                           <DA008>
           OR WSSP-FLAG                 = 'M' )                         <DA008>
           AND S5035-AGLVL          NOT = WSAA-AGLVL-STORE              <DA008>
           AND S5035-AGLVL          NOT = SPACES                        <DA008>
              MOVE 'X'                 TO S5035-ADDOPT                  <DA008>
           END-IF.                                                      <DA008>
                                                                        <DA008>
       2115-CONTINUE.                                                   <DA008>

      *****
      *    Validate Agent type.
      *****
           IF S5035-AGTYPE             = SPACES
              MOVE E186                TO S5035-AGTYPE-ERR
           ELSE
      *       IF S5035-AGTYPE-OUT (CHG)   = 'Y'                         <007>
              IF S5035-AGTYPE NOT = WSAA-STORE-AGTYPE                   <007>
      *          MOVE S5035-AGTYPE     TO WSAA-STORE-AGTYPE             <007>
      *          PERFORM 2160-CHG-AGTYPE.                               <007>
                 PERFORM 2160-CHG-AGTYPE                                <007>
      ***********IF S5035-AGTYPE-ERR = SPACES                      <023><007>
                 MOVE S5035-AGTYPE     TO WSAA-STORE-AGTYPE.            <023>

TDO   * Update Agent Club Desc if changed:                              <DA007>
                                                                        <DA007>
           IF S5035-AGLVL              = SPACES                         <DA007>
               MOVE SPACES             TO S5035-AGLVLDES                <DA007>
               MOVE S5035-AGLVL        TO WSAA-AGLVL-OLD                <DA007>
           ELSE                                                         <DA007>
              IF S5035-AGLVL           NOT = WSAA-AGLVL-OLD             <DA007>
                 MOVE S5035-AGLVL      TO WSAA-AGLVL-OLD                <DA007>
                 PERFORM X110-GET-CLUB-DESC                             <DA007>
              END-IF                                                    <DA007>
           END-IF.                                                      <DA007>
      *****
      *    Validate Area Code.
      *****
           IF S5035-ARACDE             = SPACES
              MOVE F176                TO S5035-ARACDE-ERR
           ELSE
      *       IF S5035-ARACDE-OUT (CHG)     = 'Y'                       <007>
              IF S5035-ARACDE NOT = WSAA-STORE-ARACDE                   <007>
      *          MOVE S5035-ARACDE     TO WSAA-STORE-ARACDE             <007>
      *          PERFORM 2140-CHG-ARACDE.                               <007>
                 PERFORM 2140-CHG-ARACDE                                <007>
      ***********IF S5035-ARACDE-ERR  = SPACES                     <023><007>
                 MOVE S5035-ARACDE     TO WSAA-STORE-ARACDE.            <023>

           IF S5035-ARACDE-ERR = SPACES
              PERFORM 2150-VAL-BRANCH-AREA.

      *****                                                             <S02>
      *    Validate Sales Unit                                          <S02>
      *****                                                             <S02>
           IF  TH605-TSALESIND         = 'Y'                            <S02>
           AND S5035-TSALESUNT         = SPACES                         <S02>
               MOVE TL13               TO S5035-TSALESUNT-ERR           <S02>
           ELSE                                                         <S02>
              IF S5035-TSALESUNT NOT = WSAA-STORE-TSALESUNT             <S02>
                 PERFORM 2900-CHG-TSALESUNT                             <S02>
                 MOVE S5035-TSALESUNT  TO WSAA-STORE-TSALESUNT.         <S02>
                                                                        <S02>
      *****
      *    Validate Reporting to field.
      *****
      *                                                                 <PHE004>
      * Check not report to same level.                                 <PHE004>
      *                                                                 <PHE004>
           IF S5035-REPSEL          NOT = SPACES                        <PHE004>
           AND WSKY-BATC-BATCTRCDE      = WSAA-T601                     <PHFX01>
              MOVE 'N'                 TO WSAA-TV084-FLAG               <PHE004>
              MOVE SPACES              TO ITEM-DATA-KEY                 <PHE004>
              MOVE 'IT'                TO ITEM-ITEMPFX                  <PHE004>
              MOVE WSSP-COMPANY        TO ITEM-ITEMCOY                  <PHE004>
              MOVE TV084               TO ITEM-ITEMTABL                 <PHE004>
              MOVE S5035-AGTYPE        TO ITEM-ITEMITEM                 <PHE004>
              MOVE READR               TO ITEM-FUNCTION                 <PHE004>
                                                                        <PHE004>
              CALL 'ITEMIO'         USING ITEM-PARAMS                   <PHE004>
                                                                        <PHE004>
              IF ITEM-STATUZ        NOT = O-K AND MRNF                  <PHE004>
                  MOVE ITEM-STATUZ     TO SYSR-STATUZ                   <PHE004>
                  MOVE ITEM-PARAMS     TO SYSR-PARAMS                   <PHE004>
                  PERFORM 600-FATAL-ERROR                               <PHE004>
              END-IF                                                    <PHE004>
                                                                        <PHE004>
              IF ITEM-STATUZ            = MRNF                          <PHE004>
                 MOVE 'Y'              TO WSAA-TV084-FLAG               <PHE004>
              END-IF                                                    <PHE004>
                                                                        <PHE004>
              MOVE ITEM-GENAREA        TO TV084-TV084-REC               <PHE004>
      *                                                                 <PHE004>
              MOVE WSSP-COMPANY        TO AGNT-AGNTCOY                  <PHE004>
              MOVE 'AG'                TO AGNT-AGNTPFX                  <PHE004>
              MOVE S5035-REPSEL        TO AGNT-AGNTNUM                  <PHE004>
              MOVE READR               TO AGNT-FUNCTION                 <PHE004>
              CALL 'AGNTIO'         USING AGNT-PARAMS                   <PHE004>
              IF AGNT-STATUZ        NOT = O-K                           <PHE004>
                 MOVE AGNT-STATUZ      TO SYSR-STATUZ                   <PHE004>
                 MOVE AGNT-PARAMS      TO SYSR-PARAMS                   <PHE004>
                 PERFORM 600-FATAL-ERROR                                <PHE004>
              END-IF                                                    <PHE004>
              PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 20              <PHE004>
                                          OR WSAA-TV084-FLAG = 'Y'      <PHE004>
                 IF  AGNT-AGTYPE        = TV084-AGTYPE(IX)              <PHE004>
                     MOVE EZ84         TO S5035-AGTYPE-ERR              <PHE004>
                     MOVE 'Y'          TO WSAA-TV084-FLAG               <PHE004>
                 END-IF                                                 <PHE004>
              END-PERFORM                                               <PHE004>
           END-IF.                                                      <PHE004>
      *                                                                 <PHE004>
           IF S5035-REPSEL             = SPACES
              MOVE SPACES              TO S5035-REPNAME
              MOVE SPACES              TO WSAA-STORE-REPSEL             <023>
           ELSE
      *      IF S5035-REPSEL-OUT (CHG)   = 'Y'                          <007>
             IF S5035-REPSEL NOT = WSAA-STORE-REPSEL                    <007>
      *          MOVE S5035-REPSEL     TO WSAA-STORE-REPSEL             <007>
      *          PERFORM 2170-CHG-REPSEL.                               <007>
                 PERFORM 2170-CHG-REPSEL                                <007>
      ***********IF S5035-REPSEL-ERR = SPACES                      <023><007>
      *<PHFX01>  MOVE S5035-REPSEL     TO WSAA-STORE-REPSEL.    <PHFX01><007>023
                 IF AGNTLAG-STATUZ     = O-K                            <PHFX01>
                    MOVE S5035-REPSEL  TO WSAA-STORE-REPSEL             <PHFX01>
                 END-IF.                                                <PHFX01>

      *                                                                 <016>
      * Do a RETRV on the AGLF file at this point to ensure that        <016>
      * there are no details for the overriding agent left in the       <016>
      * AGLF parameters                                                 <016>
      *                                                                 <016>
           MOVE RETRV                  TO AGLF-FUNCTION.                <016>
                                                                        <016>
           CALL 'AGLFIO'               USING AGLF-PARAMS.               <016>
                                                                        <016>
           IF AGLF-STATUZ              NOT = O-K                        <016>
              MOVE AGLF-PARAMS         TO SYSR-PARAMS                   <016>
              PERFORM 600-FATAL-ERROR.                                  <016>

      *****
      *    Validate Overrider percentage.
      *****
           IF S5035-OVCPC           NOT = 0                             <CAS1.0>
           AND S5035-REPSEL             = SPACES                        <CAS1.0>
                MOVE E374              TO S5035-OVCPC-ERR               <CAS1.0>
           END-IF.                                                      <CAS1.0>
                                                                        <CAS1.0>
      *****
      *    Validate Black List Suspend Indicator.
      *****
      *                                                                 <A001>
      * AS THE NEW TABLE FOR THE BLACK LIST HAS BEEN CREATED THEN THE   <A001>
      * HARD-CODED CAN BE REMOVED.                                      <A001>
      *                                                                 <A001>
      ***  IF S5035-TAGSUSIND       NOT = SPACE AND 'I'           <S01> <A001>
      ***                           AND 'C' AND 'X'               <S01> <A001>
      ***       MOVE F490              TO S5035-TAGSUSIND-ERR     <S01> <A001>
      ***  END-IF.                                                <S01> <A001>
                                                                        <S01>
      *****
      *    Validate Payee number.
      *****
           IF S5035-PAYSEL             = SPACES                         <013>
              MOVE SPACES              TO S5035-PAYENME                 <013>
           ELSE
      *       IF S5035-PAYSEL          = S5035-CLNTSEL          <V5L003><013)
              IF  S5035-PAYSEL         = S5035-CLNTSEL                  <V5L003>
              AND AGLF-AGCCQIND        NOT = 'Y'                        <V5L003>
                 MOVE G297             TO S5035-CLNTSEL-ERR
                 MOVE T005             TO S5035-PAYSEL-ERR              <013>
              ELSE
<003>**           IF S5035-PAYEESEL-OUT (CHG) = 'Y'
                     PERFORM 2190-CHG-PAYSEL.                           <013>

      *****
      *    Validate Payment Method.
      *****
           IF S5035-PAYMTH             = SPACES
              MOVE E186                TO S5035-PAYMTH-ERR
           ELSE
      *       IF S5035-PAYMTH-OUT (CHG) = 'Y'                           <007>
      *027    IF S5035-PAYMTH NOT = WSAA-STORE-PAYMTH                   <007>
      *          MOVE S5035-PAYMTH     TO WSAA-STORE-PAYMTH             <007>
      *          PERFORM 2200-CHG-PAYMTH.                               <007>
                 PERFORM 2200-CHG-PAYMTH                                <007>
      ***********IF S5035-PAYMTH-ERR = SPACES                      <023><007>
                 MOVE S5035-PAYMTH     TO WSAA-STORE-PAYMTH.            <007>023

      *****
      *    Validate Payment Frequency.
      *****
           IF S5035-PAYFRQ             = SPACES
              MOVE E186                TO S5035-PAYFRQ-ERR
           ELSE
      *       IF S5035-PAYFRQ-OUT (CHG) = 'Y'                           <007>
              IF S5035-PAYFRQ NOT = WSAA-STORE-PAYFRQ                   <007>
      *          MOVE S5035-PAYFRQ     TO WSAA-STORE-PAYFRQ             <007>
      *          PERFORM 2210-CHG-PAYFRQ.                               <007>
                 PERFORM 2210-CHG-PAYFRQ                                <007>
      ***********IF S5035-PAYFRQ-ERR = SPACES                      <023><007>
                 MOVE S5035-PAYFRQ     TO WSAA-STORE-PAYFRQ.            <007>023

           IF S5035-PAYMTH-ERR = SPACES AND
              S5035-PAYFRQ-ERR = SPACES
              PERFORM 2220-VAL-METH-FREQ.

      *****                                                             <V4LAQR>
      *    Validate Currency                                            <V4LAQR>
      *****                                                             <V4LAQR>
           IF S5035-CURRCODE           = SPACES                         <V4LAQR>
              MOVE F982                TO S5035-CURRCODE-ERR.           <V4LAQR>
                                                                        <V4LAQR>
      *****
      *    Validate Date appointed.
      *****
           IF S5035-DTEAPP             = VRCM-MAX-DATE
BASE          MOVE E186                TO S5035-DTEAPP-ERR.             <DA028>
      *                                                                 <DA028>
      * Date Appointed not exceed Business Date.                        <DA028>
      *                                                                 <DA028>
           IF S5035-DTEAPP              > WSAA-TODAY                    <DA028>
              MOVE A024                TO S5035-DTEAPP-ERR              <DA028>
           END-IF.                                                      <DA028>

           IF (S5035-DTETRM         NOT = SPACES) AND                   <018>
              (S5035-DTEAPP         NOT = SPACES)                       <018>
              IF S5035-DTEAPP       > S5035-DTETRM                      <018>
                 MOVE H067          TO S5035-DTEAPP-ERR.                <018>

      ****                                                              <A05743>
      **** If terminating an agent, ensure that they have no-one        <A05743>
      **** reporting to them.  If this is not the case, display an      <A05743>
      **** error.                                                       <A05743>
      ****                                                      <AG002> <A05743>
                                                                        <A05743>
           IF S5035-DTETRM          NOT = VRCM-MAX-DATE                 <A05743>
               PERFORM 2280-REPORTING-AGENTS                            <A05743>
      *        IF S5035-BMAFLG          = ' '
               IF S5035-ZBMAFLG          = ' '
      *           MOVE RLAG            TO S5035-BMAFLG-ERR
                  MOVE RLAG            TO S5035-ZBMAFLG-ERR
               END-IF
           END-IF.                                                      <A05743>
                                                                        <A05743>
           IF S5035-DTETRM         NOT = VRCM-MAX-DATE  AND
              WSSP-SBMACTION           = 'B'            AND
      *       S5035-BMAFLG             = ' '
              S5035-ZBMAFLG             = ' '
                 MOVE SPACES           TO S5035-BMADES
      *          MOVE RLAG             TO S5035-BMAFLG-ERR.             <AG002>
                 MOVE F739             TO S5035-DTETRM-ERR              <AG002>
      *                                   S5035-BMAFLG-ERR.             <AG002>
                                          S5035-ZBMAFLG-ERR.

           IF S5035-DTETRM             = VRCM-MAX-DATE  AND
              WSSP-SBMACTION           = 'B'            AND
      *       S5035-BMAFLG         NOT = ' '
              S5035-ZBMAFLG         NOT = ' '
      *          MOVE E186             TO S5035-DTETRM-ERR.             <AG002>
                 MOVE F739             TO S5035-DTETRM-ERR              <AG002>
      *                                   S5035-BMAFLG-ERR.             <AG002>
                                          S5035-ZBMAFLG-ERR.

      *    IF  S5035-BMAFLG             = ' '     AND
           IF  S5035-ZBMAFLG             = ' '     AND
               WSSP-SBMACTION           = 'C'
      *          MOVE RLAG             TO S5035-BMAFLG-ERR.
                 MOVE RLAG             TO S5035-ZBMAFLG-ERR.

           IF WSSP-SBMACTION            = 'H'
      *       IF (S5035-BMAFLG           = ' '     AND
              IF (S5035-ZBMAFLG           = ' '     AND
                 S5035-DTETRM      NOT  = VRCM-MAX-DATE) OR
      *          (S5035-BMAFLG      NOT  = ' '     AND
                 (S5035-ZBMAFLG      NOT  = ' '     AND
                 S5035-DTETRM           = VRCM-MAX-DATE)
      *            MOVE EV03             TO S5035-BMAFLG-ERR.
                   MOVE EV03             TO S5035-ZBMAFLG-ERR.

      ****     MOVE ' '                TO S5035-BMAFLG
      ****     MOVE SPACES             TO S5035-BMADES.

      *    IF  S5035-BMAFLG            NOT = ' '
           IF  S5035-ZBMAFLG            NOT = ' '
                 MOVE SPACES                 TO DESC-DATA-KEY
                 MOVE TZ001                  TO DESC-DESCTABL
      *          MOVE S5035-BMAFLG           TO DESC-DESCITEM
                 MOVE S5035-ZBMAFLG           TO DESC-DESCITEM
                 MOVE READR                  TO DESC-FUNCTION

                 MOVE 'IT'                   TO DESC-DESCPFX
                 MOVE WSSP-COMPANY           TO DESC-DESCCOY
                 MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE
                 MOVE SPACES                 TO DESC-LONGDESC

                 CALL 'DESCIO' USING DESC-PARAMS

                 IF DESC-STATUZ            = MRNF
                   MOVE O-K                TO DESC-STATUZ
                   MOVE SPACES             TO S5035-BMADES
      *            MOVE BLAG               TO S5035-BMAFLG-ERR.
                   MOVE BLAG               TO S5035-ZBMAFLG-ERR.

                 IF DESC-STATUZ        NOT = O-K
                   MOVE DESC-PARAMS        TO SYSR-PARAMS
                   PERFORM 600-FATAL-ERROR.

      *          IF S5035-BMAFLG = ' '
                 IF S5035-ZBMAFLG = ' '
                    MOVE SPACES            TO S5035-BMADES
                 ELSE
                    MOVE DESC-LONGDESC     TO S5035-BMADES
                 END-IF.                                                <A05743>
      *****
      *    Validate Exclusive Agreement.
      *****
           IF S5035-EXCL-AGMT          = SPACES
              MOVE E186                TO S5035-EXCAGR-ERR.

      *****
      *    Validate Basic Commission Payment.
      *****
           IF S5035-BCMTAB             = SPACES
              MOVE SPACES              TO S5035-BCMDESC
              MOVE SPACES              TO WSAA-STORE-BCMTAB             <023>
           ELSE
      *       IF S5035-BCMTAB-OUT (CHG)   = 'Y'                         <007>
              IF S5035-BCMTAB NOT = WSAA-STORE-BCMTAB                   <007>
      *          MOVE S5035-BCMTAB     TO WSAA-STORE-BCMTAB             <007>
      *          PERFORM 2230-CHG-BCMTAB.                               <007>
                 PERFORM 2230-CHG-BCMTAB                                <007>
      ***********IF S5035-BCMTAB-ERR = SPACES                      <023><007>
                 MOVE S5035-BCMTAB     TO WSAA-STORE-BCMTAB.            <007>023
                                                                        <007>
      *****
      *    Validate Servicing Commission Payment.
      *****
           IF S5035-SCMTAB             = SPACES
              MOVE SPACES              TO S5035-SCMDSC
              MOVE SPACES              TO WSAA-STORE-SCMTAB             <023>
           ELSE
      *       IF S5035-SCMTAB-OUT (CHG)   = 'Y'                         <007>
              IF S5035-SCMTAB NOT = WSAA-STORE-SCMTAB                   <007>
      *          MOVE S5035-SCMTAB     TO WSAA-STORE-SCMTAB             <007>
      *          PERFORM 2240-CHG-SCMTAB.                               <007>
                 PERFORM 2240-CHG-SCMTAB                                <007>
      ***********IF S5035-SCMTAB-ERR = SPACES                      <023><007>
                 MOVE S5035-SCMTAB  TO WSAA-STORE-SCMTAB.               <007>023

      *****
      *    Validate Renewal Commission Payment.
      *****
           IF S5035-RCMTAB             = SPACES
              MOVE SPACES              TO S5035-RCMDESC
              MOVE SPACES              TO WSAA-STORE-RCMTAB             <023>
           ELSE
      *       IF S5035-RCMTAB-OUT (CHG)   = 'Y'                         <007>
              IF S5035-RCMTAB NOT = WSAA-STORE-RCMTAB                   <007>
      *          MOVE S5035-RCMTAB     TO WSAA-STORE-RCMTAB             <007>
      *          PERFORM 2250-CHG-RCMTAB.                               <007>
                 PERFORM 2250-CHG-RCMTAB                                <007>
      ***********IF S5035-RCMTAB-ERR = SPACES                      <023><007>
                 MOVE S5035-RCMTAB     TO WSAA-STORE-RCMTAB.            <007>023

      *****
      *    Validate Bonus Commission Payment.
      *****
           IF S5035-OCMTAB             = SPACES
              MOVE SPACES              TO S5035-OCMDESC
              MOVE SPACES              TO WSAA-STORE-OCMTAB             <023>
           ELSE
      *       IF S5035-OCMTAB-OUT (CHG)   = 'Y'                         <007>
              IF S5035-OCMTAB NOT = WSAA-STORE-OCMTAB                   <007>
      *          MOVE S5035-OCMTAB     TO WSAA-STORE-OCMTAB             <007>
      *          PERFORM 2260-CHG-OCMTAB.                               <007>
                 PERFORM 2260-CHG-OCMTAB                                <007>
      ***********IF S5035-OCMTAB-ERR = SPACES                      <023><007>
                 MOVE S5035-OCMTAB     TO WSAA-STORE-OCMTAB.            <007>023

      *****
      *    Validate Agent Class.
      *****
           IF S5035-AGENT-CLASS        = SPACES
              MOVE E186                TO S5035-AGCLS-ERR
           ELSE
      *       IF S5035-AGCLS-OUT (CHG)   = 'Y'                          <007>
              IF S5035-AGENT-CLASS NOT = WSAA-STORE-AGENT-CLASS         <007>
      *          MOVE S5035-AGENT-CLASS  TO WSAA-STORE-AGENT-CLASS      <007>
      *          PERFORM 2270-CHG-AGCLS.                                <007>
                 PERFORM 2270-CHG-AGCLS                                 <007>
      ***********IF S5035-AGCLS-ERR = SPACES                       <023><007>
                 MOVE S5035-AGENT-CLASS  TO WSAA-STORE-AGENT-CLASS.     <007>023
                                                                        <007>
      *****                                                             <A002>
      *    Validate Agent's Licence Number and its Expiry Date.         <A002>
      *****                                                             <A002>
      **** IF S5035-TLAGLICNO          NOT = SPACES             <DA025> <A002>
      **** AND S5035-TLICEXPDT         = VRCM-MAX-DATE          <DA025> <A002>
      ****    MOVE I032                TO S5035-TLICEXPDT-ERR.  <DA025> <A002>
                                                                        <A002>
      *    Validate Agent's Consolidated Cheque indicator               <V5L003>
                                                                        <V5L003>
           IF AGLF-AGCCQIND            = 'Y'   AND                      <V5L003>
              S5035-PAYSEL             = SPACES                         <V5L003>
              MOVE E186                TO S5035-PAYSEL-ERR              <V5L003>
           END-IF.                                                      <V5L003>
                                                                        <V5L003>
           IF S5035-TLAGLICNO          = SPACES                         <A002>
           AND S5035-TLICEXPDT         NOT = VRCM-MAX-DATE              <A002>
              MOVE H400                TO S5035-TLICEXPDT-ERR.          <A002>
                                                                        <A002>
      *    GO TO 2290-EXIT.                                             <004>
      *                                                                 <V76F06>
      * Conduct currency rounding check                                 <V76F06>
      *                                                                 <V76F06>
           IF S5035-MINSTA         NOT = ZEROES AND                     <V76F06>
              S5035-CURRCODE       NOT = SPACE                          <V76F06>
              MOVE S5035-MINSTA        TO ZRDP-AMOUNT-IN                <V76F06>
              MOVE S5035-CURRCODE      TO ZRDP-CURRENCY                 <V76F06>
      *                                                                 <V76F06>
              PERFORM A100-CALL-ROUNDING                                <V76F06>
      *                                                                 <V76F06>
              IF ZRDP-AMOUNT-OUT   NOT = S5035-MINSTA                   <V76F06>
                 MOVE RFIK             TO S5035-MINSTA-ERR              <V76F06>
              END-IF                                                    <V76F06>
           END-IF.                                                      <V76F06>
      *                                                                 <DA028>
           IF  WSSP-FLAG                = 'M'                           <DA028>
           AND ( WSAA-AGTYPE-SAVE   NOT = S5035-AGTYPE                  <DA028>
           OR    WSAA-DTEAPP-SAVE   NOT = S5035-DTEAPP )                <DA028>
               MOVE 'L'                TO WSAA-L-FLAG                   <DA028>
           END-IF.                                                      <DA028>
                                                                        <DA028>
           IF  WSSP-FLAG                 = 'M'                          <DA028>
           AND ( WSAA-ARACDE-SAVE    NOT = S5035-ARACDE                 <DA028>
           OR    WSAA-TSALESUNT-SAVE NOT = S5035-TSALESUNT )            <DA028>
               MOVE 'S'                TO WSAA-S-FLAG                   <DA028>
           END-IF.                                                      <DA028>
                                                                        <DA028>
           IF  WSSP-FLAG                 = 'M'                          <DA028>
           AND WSAA-L-FLAG           NOT = SPACE                        <DA028>
           AND WSAA-S-FLAG           NOT = SPACE                        <DA028>
           AND WSAA-L-FLAG           NOT = WSAA-S-FLAG                  <DA028>
               MOVE A023               TO  S5035-AGNUM-ERR              <DA028>
           END-IF.                                                      <DA028>
      *                                                                 <DA023>
      * Validate Movement Agent Type which is configured in TZ606.      <DA023>
      *                                                                 <DA023>
           IF  WSSP-FLAG                = 'M'                           <DA023>
           AND WSAA-AGTYPE-SAVE   NOT = S5035-AGTYPE                    <DA028>
               PERFORM A1000-READ-TZ606                                 <DA023>
               IF  WSAA-TZ606-FLAG      = 'Y'                           <DA023>
                   IF WSAA-TZ606-VALID NOT = 'Y'                        <DA023>
                      MOVE A020        TO S5035-AGTYPE-ERR              <DA023>
                   ELSE                                                 <DA023>
                      PERFORM A4000-CHECK-AGENT-DOWNLINE                <DA028>
                   END-IF                                               <DA023>
               END-IF                                                   <DA023>
           END-IF.                                                      <DA023>
                                                                        <004>
       2290-EXIT.                                                       <004>
            EXIT.                                                       <004>
      /                                                                 <004>
      *****************************************************************
      *   Performed paragraphs.
      *   Changed them to sections.                                     <004>
      *****************************************************************
      *
      *2120-CHG-CLIENT.                                                 <004>
       2120-CHG-CLIENT SECTION.                                         <004>
      *************************
      *
       2125-GO.

      *****
      *    Validate client number
      *****
           MOVE SPACES                 TO CLTS-DATA-KEY.
           MOVE S5035-CLNTSEL          TO CLTS-CLNTNUM.

           PERFORM 2700-CLTSIO-CALL.

           IF CLTS-STATUZ              = MRNF
      *        MOVE E186               TO S5035-CLNTSEL-ERR             <004>
               MOVE E058               TO S5035-CLNTSEL-ERR             <004>
               GO TO 2129-EXIT.                                         <004>

           IF CLTS-VALIDFLAG           NOT = '1'
              MOVE E329                TO S5035-CLNTSEL-ERR
              GO TO 2129-EXIT.                                          <004>
                                                                        <PHFX08>
           IF  CLTS-CLTIND              = 'D'                           <PHFX08>
               MOVE F393               TO S5035-CLNTSEL-ERR             <PHFX08>
               GO TO 2129-EXIT                                          <PHFX08>
           END-IF.                                                      <PHFX08>

           IF S5035-PAYSEL-ERR = SPACES                                 <003>
<003>         IF CLTS-CLTDOD   NOT = +0                                 <003>
<003>              AND                                                  <003>
<003>         CLTS-CLTDOD   NOT = VRCM-MAX-DATE                         <003>
              MOVE F782                TO S5035-CLNTSEL-ERR             <003>
              GO TO 2129-EXIT.                                          <004>

           IF WSAA-SAVE-CLNTSEL        = SPACES                         <021>
              MOVE S5035-CLNTSEL       TO WSAA-SAVE-CLNTSEL.            <021>

      *    IF S5035-CLNTSEL-ERR = SPACES                                <004>
              PERFORM PLAINNAME.
              MOVE WSSP-LONGCONFNAME   TO S5035-CLTNAME.
      *
       2129-EXIT.                                                       <004>
            EXIT.                                                       <004>
      /                                                                 <004>
      *2140-CHG-ARACDE.                                                 <004>
       2140-CHG-ARACDE SECTION.                                         <004>
      *************************
      *
       2145-GO.

      *****
      *    Look up area code description
      *****
           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE T5696                  TO DESC-DESCTABL.
           MOVE S5035-ARACDE           TO DESC-DESCITEM.
           MOVE READR                  TO DESC-FUNCTION.

           PERFORM 1600-DESCIO-CALL.

           MOVE DESC-LONGDESC          TO S5035-ARADESC.
      *
       2149-EXIT.                                                       <004>
            EXIT.                                                       <004>
      /                                                                 <004>
      *2150-VAL-BRANCH-AREA.                                            <004>
       2150-VAL-BRANCH-AREA SECTION.                                    <004>
      ******************************
      *
       2155-GO.

      *****
      *    Cross validate that the agent area is covered by the branch
      *****
           MOVE SPACES                 TO ITEM-DATA-KEY.
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.
           MOVE T5628                  TO ITEM-ITEMTABL.
           STRING S5035-AGNTBR         DELIMITED BY SIZE
                  S5035-ARACDE         DELIMITED BY SIZE
                                       INTO ITEM-ITEMITEM.

           PERFORM 2800-ITEMIO-CALL.

           IF ITEM-STATUZ              = MRNF
      ****    MOVE E305                TO S5035-ARACDE-ERR.             <001>
              MOVE F005                TO S5035-ARACDE-ERR.             <001>
      *
       2159-EXIT.                                                       <004>
            EXIT.                                                       <004>
      /                                                                 <004>
      *2160-CHG-AGTYPE.                                                 <004>
       2160-CHG-AGTYPE SECTION.                                         <004>
      *************************
      *
       2165-GO.

      *****
      *    Look up agent type description
      *****
           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE T3692                  TO DESC-DESCTABL.
           MOVE S5035-AGTYPE           TO DESC-DESCITEM.
           MOVE READR                  TO DESC-FUNCTION.

           PERFORM 1600-DESCIO-CALL.

           MOVE DESC-LONGDESC          TO S5035-AGTYDESC.
      *
       2169-EXIT.                                                       <004>
            EXIT.                                                       <004>
      /                                                                 <004>
      *2170-CHG-REPSEL.                                                 <004>
       2170-CHG-REPSEL SECTION.                                         <004>
      *************************
      *
       2175-GO.

      *****
      *    Validate agent reported to
      *****
                                                                        <022>
           MOVE S5035-REPSEL           TO WSAA-REPSEL.                  <022>
           IF   WSAA-REPSEL-FILL    NOT = SPACES                        <022>
                MOVE E305               TO S5035-REPSEL-ERR             <022>
                GO TO 2179-EXIT.                                        <022>
                                                                        <022>
           MOVE SPACES                 TO AGNTLAG-DATA-KEY.
           MOVE S5035-REPSEL           TO AGNTLAG-AGNTNUM.

           PERFORM 2500-AGNTLAGIO-CALL.

           IF AGNTLAG-STATUZ           = MRNF
               MOVE E305               TO S5035-REPSEL-ERR
               GO TO 2179-EXIT.                                         <004>

      *****
      *    If OK, look up client name
      *****
           IF S5035-REPSEL-ERR = SPACES
              MOVE SPACES              TO CLTS-DATA-KEY
              MOVE AGNTLAG-CLNTNUM     TO CLTS-CLNTNUM
              PERFORM 1700-CLTSIO-CALL
              MOVE WSSP-LONGCONFNAME   TO S5035-REPNAME.

           IF AGNTLAG-VALIDFLAG        NOT = '1'
               MOVE E475               TO S5035-REPSEL-ERR
               GO TO 2179-EXIT.                                         <004>

           IF AGNTLAG-LIFAGNT          NOT = 'Y'
               MOVE F188               TO S5035-REPSEL-ERR
               GO TO 2179-EXIT.                                         <004>

           IF S5035-REPSEL = S5035-AGNUM
              MOVE G747                TO S5035-REPSEL-ERR
              GO TO 2179-EXIT.                                          <004>

           MOVE S5035-REPSEL           TO AGLF-REPORTAG.

           PERFORM 2180-REPORT-TO-CHECK
              UNTIL AGLF-REPORTAG = SPACES
                 OR S5035-REPSEL-ERR NOT = SPACES.
      *
       2179-EXIT.                                                       <004>
            EXIT.                                                       <004>
      /                                                                 <004>
      *2180-REPORT-TO-CHECK.                                            <004>
       2180-REPORT-TO-CHECK SECTION.                                    <004>
      ******************************
      *
       2185-GO.

      *****
      *    Check Agent Reporting to is not equal to Agent number.
      *****
           MOVE AGLF-REPORTAG          TO AGLF-AGNTNUM.
           MOVE READR                  TO AGLF-FUNCTION.

           CALL 'AGLFIO' USING AGLF-PARAMS.

           IF AGLF-STATUZ              NOT = O-K
              MOVE AGLF-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.

           IF AGLF-REPORTAG = S5035-AGNUM
              MOVE E542                TO S5035-REPSEL-ERR.
      *
       2189-EXIT.                                                       <004>
            EXIT.                                                       <004>
      /                                                                 <004>
      *2190-CHG-PAYEESEL.                                               <004>
       2190-CHG-PAYSEL SECTION.                                         <004>
      *************************
      *
       2195-GO.

      *****
      *    Validate payee client number entered
      *****
           MOVE SPACES                 TO CLTS-DATA-KEY.
           MOVE S5035-PAYSEL           TO CLTS-CLNTNUM.                 <013>

           PERFORM 2700-CLTSIO-CALL.

           IF CLTS-STATUZ              = MRNF
               MOVE E058               TO S5035-PAYSEL-ERR              <013>
               GO TO 2199-EXIT.                                         <004>

           IF CLTS-VALIDFLAG           NOT = '1'
               MOVE E329               TO S5035-PAYSEL-ERR              <013>
               GO TO 2199-EXIT.                                         <004>

           IF S5035-PAYSEL-ERR = SPACES                                 <003>
<003>         IF CLTS-CLTDOD   NOT = +0                                 <003>
<003>              AND                                                  <003>
<003>         CLTS-CLTDOD   NOT = VRCM-MAX-DATE                         <003>
<003>         MOVE F782                TO S5035-PAYSEL-ERR              <003>
              GO TO 2199-EXIT.                                          <004>
           IF S5035-PAYSEL-ERR = SPACES                                 <013>
              PERFORM PLAINNAME
              MOVE WSSP-LONGCONFNAME      TO S5035-PAYENME.             <013>
      *
       2199-EXIT.                                                       <004>
            EXIT.                                                       <004>
      /                                                                 <004>
      *2200-CHG-PAYMTH.                                                 <004>
       2200-CHG-PAYMTH SECTION.                                         <004>
      *************************
      *
       2205-GO.
      *
      *****
      *    Look up payment method description
      *****
           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE T5690                  TO DESC-DESCTABL.
           MOVE S5035-PAYMTH           TO DESC-DESCITEM.
           MOVE READR                  TO DESC-FUNCTION.

           PERFORM 1600-DESCIO-CALL.

           MOVE DESC-LONGDESC          TO S5035-PYMDESC.

           IF DESC-STATUZ NOT = O-K                                     027
               GO TO 2209-EXIT.                                         027
                                                                        027
       2206-CHECK-PAY-DETLS.                                            027
      *****                                                             027
      *    Using the T5690 XDS, look up T3672 to see if a any further   027
      *    details are required for this payment type.                  027
      *****                                                             027
                                                                        027
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 027
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.                 027
           MOVE T5690                  TO ITEM-ITEMTABL.                027
           MOVE S5035-PAYMTH           TO ITEM-ITEMITEM.                027
           MOVE READR                  TO ITEM-FUNCTION.                027
                                                                        027
           CALL 'ITEMIO' USING ITEM-PARAMS.                             027
           IF  ITEM-STATUZ              = MRNF                          <028>
      ****     MOVE G447               TO S5035-PAYMTH-ERR         <028><030>
               MOVE F177               TO S5035-PAYMTH-ERR              <030>
               GO TO 2209-EXIT.                                         <028>

           IF  ITEM-STATUZ          NOT = O-K                           027
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   027
               PERFORM 600-FATAL-ERROR.                                 027
                                                                        027
           MOVE ITEM-GENAREA           TO T5690-T5690-REC.              027
                                                                        027
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.                 027
           MOVE T3672                  TO ITEM-ITEMTABL.                027
           MOVE T5690-PAYMENT-METHOD   TO ITEM-ITEMITEM.                027
           MOVE SPACES                 TO ITEM-ITEMSEQ.                 027
           MOVE READR                  TO ITEM-FUNCTION.                027
                                                                        027
           CALL 'ITEMIO' USING ITEM-PARAMS.                             027
           IF  ITEM-STATUZ              = MRNF                          027
               MOVE G447               TO S5035-PAYMTH-ERR              027
               GO TO 2209-EXIT.                                         027
                                                                        027
           IF  ITEM-STATUZ          NOT = O-K                           027
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   027
               PERFORM 600-FATAL-ERROR.                                 027
                                                                        027
           MOVE ITEM-GENAREA           TO T3672-T3672-REC.              027
                                                                        027
           IF  T3672-BANKACCREQ         = 'Y'                           027
           AND (AGLF-BANKKEY            = SPACES OR                     027
                AGLF-BANKACCKEY         = SPACES)                       027
               MOVE 'X'                 TO S5035-DDIND.                 027
      *
      *****                                                             <029>
      *    Check for the change from direct credit to any other         <029>
      *    payment type.                                                <029>
      *****                                                             <029>
                                                                        <029>
           IF  T3672-BANKACCREQ         = 'N'                           <029>
               IF    S5035-DDIND        = 'X'                           <029>
                     MOVE  E492        TO  S5035-DDIND-ERR              <029>
               ELSE                                                     <029>
                     MOVE  ' '         TO  S5035-DDIND                  <029>
           END-IF.                                                      <029>
                                                                        <029>
           IF  T3672-BANKACCREQ         = 'N'                           <029>
           AND (AGLF-BANKKEY       NOT  = SPACES OR                     <029>
                AGLF-BANKACCKEY    NOT  = SPACES)                       <029>
               MOVE SPACES              TO AGLF-BANKKEY                 <029>
               MOVE SPACES              TO AGLF-BANKACCKEY              <029>
           END-IF.                                                      <029>
      *                                                                 <029>
       2209-EXIT.                                                       <004>
            EXIT.                                                       <004>
      /                                                                 <004>
      *2210-CHG-PAYFRQ.                                                 <004>
       2210-CHG-PAYFRQ SECTION.                                         <004>
      *************************
      *
       2215-GO.

      *****
      *    Look up payment frequency description
      *****
           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE T5630                  TO DESC-DESCTABL.
           MOVE S5035-PAYFRQ           TO DESC-DESCITEM.
           MOVE READR                  TO DESC-FUNCTION.

           PERFORM 1600-DESCIO-CALL.

           MOVE DESC-LONGDESC          TO S5035-PYFDESC.
      *
       2219-EXIT.                                                       <004>
            EXIT.                                                       <004>
      /                                                                 <004>
      *2220-VAL-METH-FREQ.                                              <004>
       2220-VAL-METH-FREQ SECTION.                                      <004>
      ****************************
      *
       2225-GO.

      *****
      *    Cross validate payment method and frequency
      *****
           MOVE SPACES                 TO ITEM-DATA-KEY.
           MOVE T5629                  TO ITEM-ITEMTABL.
           STRING S5035-PAYMTH         DELIMITED BY SIZE
                  S5035-PAYFRQ         DELIMITED BY SIZE
                                       INTO ITEM-ITEMITEM.

           PERFORM 2800-ITEMIO-CALL.

           IF ITEM-STATUZ              = MRNF
               MOVE F199               TO S5035-PAYMTH-ERR
               MOVE F199               TO S5035-PAYFRQ-ERR              <004>
               GO TO 2229-EXIT.                                         <004>

           IF ITEM-VALIDFLAG           NOT = '1'
               MOVE F199               TO S5035-PAYMTH-ERR
               MOVE F199               TO S5035-PAYFRQ-ERR.
      *
       2229-EXIT.                                                       <004>
            EXIT.                                                       <004>
      /                                                                 <004>
      *2230-CHG-BCMTAB.                                                 <004>
       2230-CHG-BCMTAB SECTION.                                         <004>
      *************************
      *
       2235-GO.

      *****
      *    Look up basic commission method description
      *****
***********MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE SPACES                 TO DESC-PARAMS.
           MOVE S5035-BCMTAB           TO DESC-DESCITEM.
<002>**    MOVE T5699                  TO DESC-DESCTABL.
<002>      MOVE T5644                  TO DESC-DESCTABL.

           PERFORM 1600-DESCIO-CALL.

           MOVE DESC-LONGDESC          TO S5035-BCMDESC.
      *
       2239-EXIT.                                                       <004>
            EXIT.                                                       <004>
      /                                                                 <004>
      *2240-CHG-SCMTAB.                                                 <004>
       2240-CHG-SCMTAB SECTION.                                         <004>
      *************************
      *
       2245-GO.

      *****
      *    Look up servicing commission method description
      *****
           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE S5035-SCMTAB           TO DESC-DESCITEM.
***********MOVE T5692                  TO DESC-DESCTABL.                <017>
           MOVE T5644                  TO DESC-DESCTABL.                <017>

           PERFORM 1600-DESCIO-CALL.

           MOVE DESC-LONGDESC          TO S5035-SCMDSC.
      *
       2249-EXIT.                                                       <004>
            EXIT.                                                       <004>
      /                                                                 <004>
      *2250-CHG-RCMTAB.                                                 <004>
       2250-CHG-RCMTAB SECTION.                                         <004>
      *************************
      *
       2250-GO.

      *****
      *    Look up renewal commission method description
      *****
           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE S5035-RCMTAB           TO DESC-DESCITEM.
***********MOVE T5698                  TO DESC-DESCTABL.                <017>
           MOVE T5644                  TO DESC-DESCTABL.                <017>

           PERFORM 1600-DESCIO-CALL.

           MOVE DESC-LONGDESC          TO S5035-RCMDESC.
      *
       2259-EXIT.                                                       <004>
            EXIT.                                                       <004>
      /                                                                 <004>
      *2260-CHG-OCMTAB.                                                 <004>
       2260-CHG-OCMTAB SECTION.                                         <004>
      *************************
      *
       2265-GO.

      *****
      *    Look up bonus commission method description
      *****
           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE S5035-OCMTAB           TO DESC-DESCITEM.
           MOVE T5697                  TO DESC-DESCTABL.

           PERFORM 1600-DESCIO-CALL.

           MOVE DESC-LONGDESC          TO S5035-OCMDESC.
      *
       2269-EXIT.                                                       <004>
            EXIT.                                                       <004>
      /                                                                 <004>
      *2270-CHG-AGCLS.                                                  <004>
       2270-CHG-AGCLS SECTION.                                          <004>
      ************************
      *
       2275-GO.

      *****
      *    Look up agent commission class description
      *****
           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE S5035-AGENT-CLASS      TO DESC-DESCITEM.
           MOVE T5699                  TO DESC-DESCTABL.

           PERFORM 1600-DESCIO-CALL.

           MOVE DESC-LONGDESC          TO S5035-AGCLSD.
      *
       2279-EXIT.                                                       <004>
            EXIT.                                                       <004>
      /                                                                 <004>
       2280-REPORTING-AGENTS SECTION.                                   <A05743>
      *******************************                                   <A05743>
       2290-READR.                                                      <A05743>
                                                                        <A05743>
           INITIALIZE                     AGLFRPT-PARAMS.               <AG002>
           MOVE 'AG'                   TO WSAA-AGNTPFX.                 <A05743>
           MOVE WSSP-COMPANY           TO WSAA-AGNTCOY.                 <A05743>
           MOVE S5035-AGNUM            TO WSAA-AGNTNUM.                 <A05743>
           MOVE WSAA-AGLFRPT-KEY       TO AGLFRPT-REPAGENT01.           <A05743>
                                                                        <A05743>
           MOVE AGLFRPTREC             TO AGLFRPT-FORMAT.               <A05743>
      *    MOVE READR                  TO AGLFRPT-FUNCTION.             <A05743>
                                                                        <A05743>
      *    CALL 'AGLFRPTIO'            USING AGLFRPT-PARAMS.            <A05743>
                                                                        <A05743>
      *    IF AGLFRPT-STATUZ        NOT = O-K                           <A05743>
      *       AND                   NOT = MRNF                          <A05743>
      *        MOVE AGLFRPT-PARAMS     TO SYSR-PARAMS                   <A05743>
      *        MOVE AGLFRPT-STATUZ     TO SYSR-STATUZ                   <A05743>
      *        PERFORM 600-FATAL-ERROR                                  <A05743>
      *    END-IF.                                                      <A05743>
                                                                        <A05743>
      *    IF AGLFRPT-STATUZ            = MRNF                          <A05743>
      *        GO TO 2299-EXIT                                          <A05743>
      *    END-IF.                                                      <A05743>

           MOVE 'N'                    TO WSAA-LOW-AG-TERM-FLAG.
           MOVE BEGN                   TO AGLFRPT-FUNCTION.             <A05743>
                                                                        <A05743>
           PERFORM UNTIL AGLFRPT-STATUZ = ENDP
                      OR WSAA-LOW-AG-TERM-FLAG = 'Y'

               CALL 'AGLFRPTIO'            USING AGLFRPT-PARAMS         <A05743>

               IF AGLFRPT-STATUZ        NOT = O-K                       <A05743>
                  AND                   NOT = ENDP                      <A05743>
                    MOVE AGLFRPT-PARAMS     TO SYSR-PARAMS              <A05743>
                    MOVE AGLFRPT-STATUZ     TO SYSR-STATUZ              <A05743>
                    PERFORM 600-FATAL-ERROR                             <A05743>
               END-IF                                                   <A05743>
                                                                        <A05743>
               IF AGLFRPT-REPAGENT01    NOT = WSAA-AGLFRPT-KEY          <A05743>
                    MOVE ENDP               TO AGLFRPT-STATUZ           <AG002>
               ELSE

                    MOVE AGLFRPT-AGNTNUM    TO AGLFLNB-AGNTNUM          <V4L016>
                    PERFORM 2291-CHECK-LOW-AGENT

                    MOVE NEXTR              TO AGLFRPT-FUNCTION
               END-IF                                                   <A05743>

           END-PERFORM.

      *    IF AGLFRPT-AGNTNUM       NOT = SPACES                        <A05743>
           IF WSAA-LOW-AG-TERM-FLAG     = 'Y'
               MOVE E535               TO S5035-DTETRM-ERR              <A05743>
           END-IF.                                                      <A05743>
                                                                        <A05743>
       2299-EXIT.                                                       <A05743>
            EXIT.                                                       <A05743>

       2291-CHECK-LOW-AGENT SECTION.
       2291-CHECK.
           MOVE WSSP-COMPANY           TO AGLFLNB-AGNTCOY.              <V4L016>
           MOVE READR                  TO AGLFLNB-FUNCTION.             <V4L016>
                                                                        <V4L016>
           CALL 'AGLFLNBIO' USING AGLFLNB-PARAMS.                       <V4L016>
                                                                        <V4L016>
           IF AGLFLNB-STATUZ        NOT = O-K AND MRNF                  <V4L016>
              MOVE AGLFLNB-STATUZ      TO SYSR-STATUZ                   <DA028>
              MOVE AGLFLNB-PARAMS      TO SYSR-PARAMS                   <V4L016>
              PERFORM 600-FATAL-ERROR                                   <V4L016>
           END-IF.                                                      <V4L016>
                                                                        <V4L016>
           IF  AGLFLNB-STATUZ            = O-K                          <V4L016>
           AND AGLFLNB-DTETRM            = VRCM-MAX-DATE
           AND AGLFLNB-REPORTAG          = S5035-AGNUM
               MOVE 'Y'                TO  WSAA-LOW-AG-TERM-FLAG        <V4L016>
           END-IF.                                                      <V4L016>

       2291-EXIT.
           EXIT.

      /                                                                 <A05743>
      *****************************************************************
       2500-AGNTLAGIO-CALL SECTION.
      **************************
      *
       2510-CALL.
      *****
      *    Call the Agent details I/O Module.
      *****
           MOVE WSSP-COMPANY           TO AGNTLAG-AGNTCOY.
           MOVE READR                  TO AGNTLAG-FUNCTION.

           CALL 'AGNTLAGIO' USING AGNTLAG-PARAMS.

           IF AGNTLAG-STATUZ           NOT = O-K
                                   AND NOT = MRNF
              MOVE AGNTLAG-PARAMS      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.
      *
       2590-EXIT.
            EXIT.
      *
      *****************************************************************
       2700-CLTSIO-CALL SECTION.
      **************************
      *
       2710-DESCRIPTION.
      *****
      *    Call the Clients details I/O Module for Client Name.
      *****
           MOVE 'CN'                   TO CLTS-CLNTPFX.
      *****MOVE WSSP-COMPANY           TO CLTS-CLNTCOY.                 <006>
           MOVE WSSP-FSUCO             TO CLTS-CLNTCOY.                 <006>
           MOVE READR                  TO CLTS-FUNCTION.

           CALL 'CLTSIO'               USING CLTS-PARAMS.

           IF CLTS-STATUZ              NOT = O-K
           AND CLTS-STATUZ             NOT = MRNF
              MOVE CLTS-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.
      *
       2790-EXIT.
            EXIT.
      *
      *****************************************************************
       2800-ITEMIO-CALL SECTION.
      **************************
      *
       2810-ITEM-TABLE.
      *****
      *    Call the Item details I/O Module for Table Look Ups.
      *****
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.
           MOVE 'READR'                TO ITEM-FUNCTION.

           CALL 'ITEMIO' USING ITEM-PARAMS.

           IF ITEM-STATUZ              NOT = O-K
           AND ITEM-STATUZ             NOT = MRNF
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.
      *
       2890-EXIT.
            EXIT.
      *                                                                 <S02>
       2900-CHG-TSALESUNT SECTION.                                      <S02>
      ****************************                                      <S02>
      *                                                                 <S02>
       2901-GO.                                                         <S02>
                                                                        <S02>
      *****                                                             <S02>
      *    Look up sales unit description                               <S02>
      *****                                                             <S02>
           MOVE SPACES                 TO DESC-DATA-KEY.                <S02>
           MOVE TT518                  TO DESC-DESCTABL.                <S02>
           MOVE S5035-TSALESUNT        TO DESC-DESCITEM.                <S02>
                                                                        <S02>
           PERFORM 1600-DESCIO-CALL.                                    <S02>
                                                                        <S02>
           MOVE DESC-LONGDESC          TO S5035-TSALESDSC.              <S02>
      *                                                                 <S02>
       2990-EXIT.                                                       <S02>
            EXIT.                                                       <S02>
      /
      *****************************************************************
      *     UPDATE DATABASE IF REQUIRED AND LOG TRANSACTION
      *****************************************************************
      *
       3000-UPDATE SECTION.
      **********************
      *
       3010-UPDATE-DATABASE.
      *****
      *    Skip  this section if  returning from an optional selection
      *    (current stack position action flag = '*').
      *****
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'
              GO TO 3090-EXIT.

      *
       3020-IF-SELECTION.
      *****
      *    Only update the files if no additional screens are selected.
      *****
           IF S5035-DDIND              = 'X'
           OR S5035-BCTIND             = 'X'
      **** OR S5035-ZRORIND            = 'X'                            <CAS1.0>
           OR S5035-ZRORIND            = 'X'                            <V4L014>
           OR S5035-TAGD               = 'X'
           OR S5035-CLIENTIND          = 'X'                            <005>
           OR S5035-ADDOPT             = 'X'                            <DA008>
              PERFORM 3100-SELECT-KEEPS
              GO TO 3090-EXIT
           ELSE
              IF WSSP-FLAG             NOT = 'I'
                 PERFORM 3200-NO-SELECT-UPDTE.

           IF WSSP-FLAG             NOT = 'I'
              PERFORM 3291-UPDATE-AGLF
           END-IF.
                                                                        <PHE003>
      * Update Region/Unit of Downline Agents when changing Region/Unit <PHE003>
      * of Leader Agent:                                                <PHE003>
                                                                        <PHE003>
           IF WSSP-FLAG                = 'M'                            <PHE003>
           AND WSAA-REGION-OLD     NOT = SPACES                         <PHE003>
           AND WSAA-UNIT-OLD       NOT = SPACES                         <PHE003>
           AND (S5035-ARACDE       NOT = WSAA-REGION-OLD                <PHE003>
           OR   S5035-TSALESUNT    NOT = WSAA-UNIT-OLD )                <PHE003>
              MOVE SPACES              TO ZAGRG-FUNCTION                <PHE003>
              MOVE 'AG'                TO ZAGRG-AGPFX                   <PHE003>
              MOVE WSSP-COMPANY        TO ZAGRG-COMPANY                 <PHE003>
              MOVE S5035-AGNUM         TO ZAGRG-LEADER-AG               <PHE003>
              MOVE S5035-ARACDE        TO ZAGRG-REGION                  <PHE003>
              MOVE S5035-TSALESUNT     TO ZAGRG-SALESUNIT               <PHE003>
              MOVE S5035-AGNTBR        TO ZAGRG-BRANCH                  <PHE003>
              MOVE 'B'                 TO ZAGRG-ACTION                  <PHE003>
              MOVE WSKY-BATC-BATCTRCDE TO ZAGRG-TRANCODE                <PHE003>
              MOVE O-K                 TO ZAGRG-STATUZ                  <PHE003>
                                                                        <PHE003>
              CALL 'ZAGMOVUPD'         USING ZAGRG-MOD-REC              <PHE003>
              IF ZAGRG-STATUZ          NOT = O-K                        <PHE003>
                 STRING                                                 <PHE003>
                    ZAGRG-MOD-REC      DELIMITED BY SIZE                <PHE003>
                    WSAA-PROG          DELIMITED BY ' '                 <PHE003>
                                  INTO SYSR-PARAMS                      <PHE003>
                 END-STRING                                             <PHE003>
                 PERFORM 600-FATAL-ERROR                                <PHE003>
           END-IF.                                                      <PHE003>
      *
       3090-EXIT.
            EXIT.
      /
      *****************************************************************
       3100-SELECT-KEEPS SECTION.
      ***************************
      *
       3110-KEEPS-AGLF.

      *****
      *    Move fields from the screen to the life agents file.
      *****

           MOVE WSSP-COMPANY           TO AGLF-AGNTCOY.
           MOVE S5035-AGNUM            TO AGLF-AGNTNUM.
           MOVE S5035-DTEAPP           TO AGLF-DTEAPP.
           MOVE S5035-EXCL-AGMT        TO AGLF-EXCL-AGMT.
           MOVE S5035-ARACDE           TO AGLF-ARACDE.
           MOVE S5035-TSALESUNT        TO AGLF-TSALESUNT.               <S02>
           MOVE S5035-REPSEL           TO AGLF-REPORTAG.                <CAS1.0>
           MOVE S5035-OVCPC            TO AGLF-OVCPC.
           MOVE S5035-PAYSEL           TO AGLF-PAYCLT.                  <013>
           MOVE S5035-PAYMTH           TO AGLF-PAYMTH.
           MOVE S5035-PAYFRQ           TO AGLF-PAYFRQ.
           MOVE S5035-CURRCODE         TO AGLF-CURRCODE.
           MOVE S5035-MINSTA           TO AGLF-MINSTA.
           MOVE S5035-BCMTAB           TO AGLF-BCMTAB.
           MOVE S5035-SCMTAB           TO AGLF-SCMTAB.
           MOVE S5035-RCMTAB           TO AGLF-RCMTAB.
           MOVE S5035-OCMTAB           TO AGLF-OCMTAB.
           MOVE S5035-AGENT-CLASS      TO AGLF-AGENT-CLASS.
           MOVE S5035-DTETRM           TO AGLF-DTETRM.                  <009>
      *    MOVE S5035-BMAFLG           TO AGLF-BMAFLG.                  <009>
           MOVE S5035-ZBMAFLG           TO AGLF-BMAFLG.
           MOVE S5035-TAGSUSIND        TO AGLF-TAGSUSIND.               <S01>
           MOVE S5035-TLAGLICNO        TO AGLF-TLAGLICNO.               <S01>
           MOVE S5035-TLICEXPDT        TO AGLF-TLICEXPDT.               <S01>
      **** MOVE '1'                    TO AGLF-VALIDFLAG.       <V76F10><V73F02>
           MOVE '1'                    TO AGLF-VALIDFLAG.               <LA5293>
                                                                        <V5L003>
           IF WSAA-AGREEMNT-NO AND AGLF-AGCCQIND = SPACES               <V5L003>
              IF  TH605-AGCCQIND       = 'Y'                            <V5L003>
                  MOVE TH605-AGCCQIND  TO AGLF-AGCCQIND                 <V5L003>
              ELSE                                                      <V5L003>
                  MOVE 'N'             TO AGLF-AGCCQIND                 <V5L003>
              END-IF                                                    <V5L003>
           END-IF.                                                      <V5L003>

           MOVE 'KEEPS'                TO AGLF-FUNCTION.
           MOVE AGLFREC                TO AGLF-FORMAT.

           CALL 'AGLFIO' USING AGLF-PARAMS.

           IF AGLF-STATUZ              NOT = O-K
              MOVE AGLF-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.
      *
       3120-KEEPS-AGNT.
      *****
      *    Move fields from the screen to the FSU agent details
      *****
           MOVE WSSP-COMPANY           TO AGNTLAG-AGNTCOY.
           MOVE S5035-AGNUM            TO AGNTLAG-AGNTNUM.
           MOVE 'AG'                   TO AGNTLAG-AGNTPFX.
           MOVE 'CN'                   TO AGNTLAG-CLNTPFX.
           MOVE WSSP-FSUCO             TO AGNTLAG-CLNTCOY.
           MOVE S5035-CLNTSEL          TO AGNTLAG-CLNTNUM.
           MOVE S5035-AGTYPE           TO AGNTLAG-AGTYPE.
           MOVE 'AG'                   TO AGNTLAG-AGNTREL.
           MOVE S5035-AGNTBR           TO AGNTLAG-AGNTBR.
           MOVE '1'                    TO AGNTLAG-VALIDFLAG.
           IF S5035-REPSEL = SPACES
              MOVE SPACES              TO AGNTLAG-REPAGENT01
              MOVE 1                   TO AGNTLAG-REPLVL
           ELSE
              MOVE 'AG'                TO WSKY-AGNT-AGNTPFX
              MOVE WSSP-COMPANY        TO WSKY-AGNT-AGNTCOY
              MOVE S5035-REPSEL        TO WSKY-AGNT-AGNTNUM
              MOVE WSAA-AGNTKEY        TO AGNTLAG-REPAGENT01
              MOVE 2                   TO AGNTLAG-REPLVL.

           MOVE 'Y'                    TO AGNTLAG-LIFAGNT.

           MOVE 'KEEPS'                TO AGNTLAG-FUNCTION.
           MOVE AGNTLAGREC             TO AGNTLAG-FORMAT.

           PERFORM 3800-AGNTLAGIO-CALL.
      *
       3190-EXIT.
            EXIT.
      /
      *****************************************************************
       3200-NO-SELECT-UPDTE SECTION.
      ***************************
      *
       3210-CLRR-AGENT-UPDTE.
      *****
      *    If the client number has not changed, skip this role.
      *****
           IF WSAA-OLD-CLNTNUM = S5035-CLNTSEL
              GO TO 3220-CLRR-PAYEE-UPDTE.

      *    MOVE SPACES                 TO CLRR-DATA-AREA
           MOVE 'AG'                   TO CLRN-CLRRROLE.
           IF WSAA-OLD-CLNTNUM = SPACES
              GO TO 3215-WRITE-AGENT-CLRR.

           MOVE WSAA-OLD-CLNTNUM       TO CLRN-CLNTNUM.
           MOVE 'DEL  '                TO CLRN-FUNCTION.
           PERFORM 3700-CLRRIO-CALL.
      *    MOVE DELET                  TO CLRR-FUNCTION.
      *    PERFORM 3700-CLRRIO-CALL.
      *
       3215-WRITE-AGENT-CLRR.
           MOVE S5035-CLNTSEL          TO CLRN-CLNTNUM.
           MOVE 'ADD  '                TO CLRN-FUNCTION.
           PERFORM 3700-CLRRIO-CALL.
      *
       3220-CLRR-PAYEE-UPDTE.
      *****
      *    If the payee number has not changed, skip this role.
      *****
      *    IF S5035-PAYSEL = SPACES                                     <010>
      *       IF WSAA-OLD-PAYEE        = S5035-CLNTSEL                  <010>
      *          GO TO 3230-AGNT-UPDTE                                  <010>
      *    ELSE                                                         <010>
      *       IF WSAA-OLD-PAYEE        = S5035-PAYSEL                   <010>
      *          GO TO 3230-AGNT-UPDTE.                                 <010>
      *                                                                 <010>
           IF S5035-PAYSEL = SPACES                                     <010>
             AND WSAA-OLD-PAYEE        = S5035-CLNTSEL                  <010>
                 GO TO 3230-AGNT-UPDTE.                                 <010>
           IF WSAA-OLD-PAYEE           = S5035-PAYSEL                   <010>
             AND WSAA-OLD-CLNTNUM      NOT = SPACES                     <010>
                 GO TO 3230-AGNT-UPDTE.                                 <010>

           MOVE 'PE'                   TO CLRN-CLRRROLE.

           IF WSAA-OLD-PAYEE           = SPACES
              GO TO 3225-WRITE-PAYEE-CLRR.

           MOVE WSAA-OLD-PAYEE         TO CLRN-CLNTNUM.
           MOVE 'DEL  '                TO CLRN-FUNCTION.
           PERFORM 3700-CLRRIO-CALL.
      *    MOVE DELET                  TO CLRR-FUNCTION.
      *    PERFORM 3700-CLRRIO-CALL.
      *
       3225-WRITE-PAYEE-CLRR.
           IF S5035-PAYSEL             = SPACES                         <013>
              MOVE S5035-CLNTSEL       TO CLRN-CLNTNUM
           ELSE
              MOVE S5035-PAYSEL        TO CLRN-CLNTNUM.                 <013>
           MOVE 'ADD  '                TO CLRN-FUNCTION.
           PERFORM 3700-CLRRIO-CALL.
      *
       3230-AGNT-UPDTE.
      *****
      *    Release the I/O module for updating.
      *****
           MOVE RLSE                TO AGNTLAG-FUNCTION
           PERFORM 3800-AGNTLAGIO-CALL.
      *****
      *    Update the agent details from the screen.
      *****
      **** MOVE WSSP-COMPANY           TO AGNTLAG-AGNTCOY.      <V76F10><V73F02>
      **** MOVE S5035-AGNUM            TO AGNTLAG-AGNTNUM.      <V76F10><V73F02>
      **** MOVE AGNTLAGREC             TO AGNTLAG-FORMAT.       <V76F10><V73F02>
      **** MOVE READH                  TO AGNTLAG-FUNCTION.     <V76F10><V73F02>
      **** CALL 'AGNTLAGIO'         USING AGNTLAG-PARAMS.       <V76F10><V73F02>
      **** IF AGNTLAG-STATUZ        NOT = O-K AND MRNF          <V76F10><V73F02>
      ****    MOVE AGNTLAG-PARAMS      TO SYSR-PARAMS           <V76F10><V73F02>
      ****    MOVE AGNTLAG-STATUZ      TO SYSR-STATUZ           <V76F10><V73F02>
      ****    PERFORM 600-FATAL-ERROR                           <V76F10><V73F02>
      **** END-IF.                                              <V76F10><V73F02>
      ****                                                      <V76F10><V73F02>
      **** IF AGNTLAG-STATUZ            = O-K                   <V76F10><V73F02>
      ****    MOVE '2'                 TO AGNTLAG-VALIDFLAG     <V76F10><V73F02>
      ****    MOVE AGNTLAGREC          TO AGNTLAG-FORMAT        <V76F10><V73F02>
      ****    MOVE REWRT               TO AGNTLAG-FUNCTION      <V76F10><V73F02>
      ****    CALL 'AGNTLAGIO'         USING AGNTLAG-PARAMS     <V76F10><V73F02>
      ****    IF AGNTLAG-STATUZ        NOT = O-K                <V76F10><V73F02>
      ****       MOVE AGNTLAG-STATUZ      TO SYSR-STATUZ        <V76F10><V73F02>
      ****       MOVE AGNTLAG-PARAMS      TO SYSR-PARAMS        <V76F10><V73F02>
      ****       PERFORM 600-FATAL-ERROR                        <V76F10><V73F02>
      **** END-IF.                                              <V76F10><V73F02>
                                                                        <V73F02>
           MOVE WSSP-COMPANY           TO AGNTLAG-AGNTCOY.
           MOVE S5035-AGNUM            TO AGNTLAG-AGNTNUM.
           MOVE 'AG'                   TO AGNTLAG-AGNTPFX.
           MOVE 'CN'                   TO AGNTLAG-CLNTPFX.
           MOVE WSSP-FSUCO             TO AGNTLAG-CLNTCOY.
           MOVE S5035-CLNTSEL          TO AGNTLAG-CLNTNUM.
           MOVE 'AG'                   TO AGNTLAG-AGNTREL.
           MOVE S5035-AGTYPE           TO AGNTLAG-AGTYPE.
           MOVE S5035-AGNTBR           TO AGNTLAG-AGNTBR.
           MOVE '1'                    TO AGNTLAG-VALIDFLAG.
           IF S5035-REPSEL = SPACES
              MOVE SPACES              TO AGNTLAG-REPAGENT01
              MOVE 1                   TO AGNTLAG-REPLVL
           ELSE
              MOVE 'AG'                TO WSKY-AGNT-AGNTPFX
              MOVE WSSP-COMPANY        TO WSKY-AGNT-AGNTCOY
              MOVE S5035-REPSEL        TO WSKY-AGNT-AGNTNUM
              MOVE WSAA-AGNTKEY        TO AGNTLAG-REPAGENT01
              MOVE 2                   TO AGNTLAG-REPLVL.
           MOVE 'Y'                    TO AGNTLAG-LIFAGNT.
           MOVE VRCM-TERMID            TO VRCM-COMP-TERMID.
           MOVE VRCM-TRANID-N          TO VRCM-COMP-TRANID-N.
           MOVE VRCM-COMP-TRANID       TO AGNTLAG-TRANID.

           MOVE AGNTLAGREC             TO AGNTLAG-FORMAT.
      **** MOVE UPDAT                  TO AGNTLAG-FUNCTION.             <V73F02>
      **** MOVE WRITR                  TO AGNTLAG-FUNCTION.     <V76F10><V73F02>
           MOVE UPDAT                  TO AGNTLAG-FUNCTION.             <V73F02>
      *    IF WSSP-REASON-FLG          = 1
      *       MOVE WRITR               TO AGNTLAG-FUNCTION
      *    ELSE
      *       MOVE UPDAT               TO AGNTLAG-FUNCTION              <V76F10>
      *    END-IF.

           PERFORM 3800-AGNTLAGIO-CALL.

      *
       3240-AGLF-UPDTE.
      *****
      *    Release the I/O module for updating.
      *****
           MOVE RLSE                TO AGLF-FUNCTION
           CALL 'AGLFIO' USING AGLF-PARAMS.
           IF AGLF-STATUZ              NOT = O-K
              MOVE AGLF-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.
      *****
      *    Update the life agent details from the screen.
      *****
      **** MOVE WSSP-COMPANY           TO AGLF-AGNTCOY.         <V76F10><V73F02>
      **** MOVE S5035-AGNUM            TO AGLF-AGNTNUM.         <V76F10><V73F02>
      **** MOVE AGLFREC                TO AGLF-FORMAT.          <V76F10><V73F02>
      **** MOVE READH                  TO AGLF-FUNCTION.        <V76F10><V73F02>
      **** CALL 'AGLFIO'            USING AGLF-PARAMS.          <V76F10><V73F02>
      **** IF AGLF-STATUZ              NOT = O-K AND MRNF       <V76F10><V73F02>
      ****    MOVE AGLF-PARAMS         TO SYSR-PARAMS           <V76F10><V73F02>
      ****    MOVE AGLF-STATUZ         TO SYSR-STATUZ           <V76F10><V73F02>
      ****    PERFORM 600-FATAL-ERROR                           <V76F10><V73F02>
      **** END-IF.                                              <V76F10><V73F02>
      ****                                                      <V76F10><V73F02>
      **** IF AGLF-STATUZ               = O-K                   <V76F10><V73F02>
      ****    MOVE '2'                 TO AGLF-VALIDFLAG        <V76F10><V73F02>
      ****    MOVE AGLFREC             TO AGLF-FORMAT           <V76F10><V73F02>
      ****    MOVE REWRT               TO AGLF-FUNCTION         <V76F10><V73F02>
      ****    CALL 'AGLFIO'         USING AGLF-PARAMS           <V76F10><V73F02>
      ****    IF AGLF-STATUZ        NOT = O-K                   <V76F10><V73F02>
      ****       MOVE AGLF-STATUZ      TO SYSR-STATUZ           <V76F10><V73F02>
      ****       MOVE AGLF-PARAMS      TO SYSR-PARAMS           <V76F10><V73F02>
      ****       PERFORM 600-FATAL-ERROR                        <V76F10><V73F02>
      **** END-IF.                                              <V76F10><V73F02>
                                                                        <V73F02>
           MOVE WSSP-COMPANY           TO AGLF-AGNTCOY.
           MOVE S5035-AGNUM            TO AGLF-AGNTNUM.
           MOVE S5035-DTEAPP           TO AGLF-DTEAPP.
           MOVE S5035-EXCL-AGMT        TO AGLF-EXCL-AGMT.
           MOVE S5035-ARACDE           TO AGLF-ARACDE.
           MOVE S5035-TSALESUNT        TO AGLF-TSALESUNT.               <S02>
           MOVE S5035-REPSEL           TO AGLF-REPORTAG.                <CAS1.0>
           MOVE S5035-OVCPC            TO AGLF-OVCPC.
           MOVE S5035-PAYSEL           TO AGLF-PAYCLT.                  <013)
           MOVE S5035-PAYMTH           TO AGLF-PAYMTH.
                                                                        <029>
           IF  T3672-BANKACCREQ         = 'N'                           <029>
           AND (AGLF-BANKKEY       NOT  = SPACES OR                     <029>
                AGLF-BANKACCKEY    NOT  = SPACES)                       <029>
               MOVE SPACES              TO AGLF-BANKKEY                 <029>
               MOVE SPACES              TO AGLF-BANKACCKEY              <029>
           END-IF.                                                      <029>
                                                                        <V4L016>
      *                                                                 <LA1174>
      *    IF WSSP-FLAG            NOT  = 'M'                   <LA1174><V4L016>
      *       PERFORM M200-UPDATE-MACF                          <LA1174><V4L016>
      *    END-IF.                                              <LA1174><V4L016>
                                                                        <LA1174>
           PERFORM M200-UPDATE-MACF.                                    <LA1174>
                                                                        <029>
           MOVE S5035-PAYFRQ           TO AGLF-PAYFRQ.
           MOVE S5035-CURRCODE         TO AGLF-CURRCODE.
           MOVE S5035-MINSTA           TO AGLF-MINSTA.
           MOVE S5035-BCMTAB           TO AGLF-BCMTAB.
           MOVE S5035-SCMTAB           TO AGLF-SCMTAB.
           MOVE S5035-RCMTAB           TO AGLF-RCMTAB.
           MOVE S5035-OCMTAB           TO AGLF-OCMTAB.
           MOVE S5035-AGENT-CLASS      TO AGLF-AGENT-CLASS.
      *    IF WSSP-FLAG                = 'R'                            <V4L016>
      *       MOVE VRCM-MAX-DATE       TO AGLF-DTETRM                   <V4L016>
      *    ELSE                                                         <V4L016>
           MOVE S5035-DTETRM           TO AGLF-DTETRM.                  <V4L016>
      *    MOVE S5035-BMAFLG           TO AGLF-BMAFLG.                  <009>
           MOVE S5035-ZBMAFLG           TO AGLF-BMAFLG.
      *    END-IF.                                                      <V4L016>
      *    MOVE S5035-DTETRM           TO AGLF-DTETRM.          <V4L016><009>
           MOVE S5035-TAGSUSIND        TO AGLF-TAGSUSIND.               <S01>
           MOVE S5035-TLAGLICNO        TO AGLF-TLAGLICNO.               <S01>
           MOVE S5035-TLICEXPDT        TO AGLF-TLICEXPDT.               <S01>
           MOVE VRCM-TERMID            TO AGLF-TERMID.
           MOVE VRCM-USER              TO AGLF-USER.
           MOVE VRCM-DATE              TO AGLF-TRANSACTION-DATE.
           MOVE VRCM-TIME              TO AGLF-TRANSACTION-TIME.

      **** MOVE UPDAT                  TO AGLF-FUNCTION.                <V73F02>
      **** MOVE '1'                    TO AGLF-VALIDFLAG.       <V76F10><V73F02>
      **** MOVE WRITR                  TO AGLF-FUNCTION.        <V76F10><V73F02>
           MOVE UPDAT                  TO AGLF-FUNCTION.                <V76F10>
           MOVE '1'                    TO AGLF-VALIDFLAG.               <LA5293>
           MOVE AGLFREC                TO AGLF-FORMAT.

           CALL 'AGLFIO' USING AGLF-PARAMS.

           IF AGLF-STATUZ              NOT = O-K
              MOVE AGLF-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.
TDO   *                                                                 <DA007>
      * Update Agent Club Class if any:                                 <DA007>
      *                                                                 <DA007>
           IF S5035-AGLVL              NOT = WSAA-AGLVL-STORE           <DA008>
              PERFORM X300-UPDATE-CLUB-CLASS                            <DA008>
           END-IF.                                                      <DA008>
                                                                        <DA008>
           IF  TH605-CRTIND = 'N' OR  WSAA-TRIGGER = 'Y'                <LA1174>
           PERFORM M001-UPDATE-LEVEL-1.                                 <LA1174>
      *                                                                 <LA1174>
           IF  WSSP-SBMACTION          = 'A'                            <V75F01>
               PERFORM 3300-CALL-BLDENRL                                <V75F01>
           END-IF.                                                      <V75F01>
      *                                                                 <V75F01>
      *3250-AGMO-WRITE.                                                 <026>
      *****
      *    Move fields from the screen to the contract header.
      *****
      *****MOVE TDAY                   TO DTC1-FUNCTION                 <026>
      *****CALL 'DATCON1' USING DTC1-DATCON1-REC                        <026>
      *****
      *    Move fields from the screen to the contract header.
      *****
      *****MOVE SPACES                 TO AGMO-DATA-KEY.                <026>
      *****MOVE WSSP-COMPANY           TO AGMO-AGNTCOY.                 <026>
      *****MOVE AGLF-AGNTNUM           TO AGMO-AGNTNUM.                 <026>
      *****MOVE WSKY-BATC-BATCCOY      TO AGMO-BATCCOY.                 <026>
      *****MOVE WSKY-BATC-BATCBRN      TO AGMO-BATCBRN.                 <026>
      *****MOVE WSKY-BATC-BATCACTYR    TO AGMO-BATCACTYR.               <026>
      *****MOVE WSKY-BATC-BATCACTMN    TO AGMO-BATCACTMN.               <026>
      *****MOVE WSKY-BATC-BATCTRCDE    TO AGMO-BATCTRCDE.               <026>
      *****MOVE WSKY-BATC-BATCBATCH    TO AGMO-BATCBATCH.               <026>
      *****MOVE AGLF-AGNTNUM           TO AGMO-NAGNUM.                  <026>
      *****MOVE DTC1-INT-DATE          TO AGMO-EFFDATE.                 <026>
      *****MOVE VRCM-TERMID            TO AGMO-TERMID.                  <026>
      *****MOVE VRCM-USER              TO AGMO-USER.                    <026>
      *****MOVE VRCM-DATE              TO AGMO-TRANSACTION-DATE.        <026>
      *****MOVE VRCM-TIME              TO AGMO-TRANSACTION-TIME.        <026>
      *****                                                             <026>
      *****MOVE WRITR                  TO AGMO-FUNCTION.                <026>
      *****MOVE AGMOREC                TO AGMO-FORMAT.                  <026>
      *****                                                             <026>
      *****CALL 'AGMOIO' USING AGMO-PARAMS.                             <026>
      *****                                                             <026>
      *****IF AGMO-STATUZ              NOT = O-K                        <026>
      ********MOVE AGMO-PARAMS         TO SYSR-PARAMS                   <026>
      ********PERFORM 600-FATAL-ERROR.                                  <026>
      *
       3260-UPDTE-BATCH-HEADER.
      *****
      *    Update the batch header using BATCUP.
      *****
           MOVE SPACES                 TO BCUP-BATCUP-REC.
           MOVE WSSP-BATCHKEY          TO BCUP-BATCHKEY.
           MOVE 1                      TO BCUP-TRANCNT.
           MOVE ZEROS                  TO BCUP-ETREQCNT
                                          BCUP-SUB
                                          BCUP-BCNT
                                          BCUP-BVAL
                                          BCUP-ASCNT.
           MOVE WRITS                  TO BCUP-FUNCTION.

           CALL 'BATCUP'               USING BCUP-BATCUP-REC.

           IF BCUP-STATUZ              NOT = O-K
              MOVE  BCUP-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.
      *
       3270-UNLK-SOFT-LOCK.
      *****
      *    Release the soft lock on the contract.
      *****
           MOVE SPACES                 TO SFTL-SFTLOCK-REC.
           MOVE WSSP-COMPANY           TO SFTL-COMPANY.
           MOVE AGLF-AGNTNUM           TO SFTL-ENTITY.
           MOVE 'AG'                   TO SFTL-ENTTYP.
           MOVE 'UNLK'                 TO SFTL-FUNCTION.

           CALL 'SFTLOCK'              USING SFTL-SFTLOCK-REC.

           IF SFTL-STATUZ              NOT = O-K
              MOVE SFTL-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.
      *
       3290-EXIT.
            EXIT.

       3291-UPDATE-AGLF SECTION.                                        <V75F01>
      **************************                                        <V75F01>
       3291-UPDATE.                                                     <V75F01>

           IF WSAA-ORIG-DTETRM      NOT = S5035-DTETRM  OR
      *       WSAA-ORIG-BMAFLG      NOT = S5035-BMAFLG
              WSAA-ORIG-BMAFLG      NOT = S5035-ZBMAFLG
                MOVE '2'               TO AGLF-VALIDFLAG
                MOVE WSAA-ORIG-DTETRM  TO AGLF-DTETRM
                MOVE WSAA-ORIG-BMAFLG  TO AGLF-BMAFLG

                MOVE WRITD             TO AGLF-FUNCTION
                MOVE AGLFREC           TO AGLF-FORMAT

                CALL 'AGLFIO'       USING AGLF-PARAMS
                IF AGLF-STATUZ      NOT = O-K
                   MOVE AGLF-PARAMS    TO SYSR-PARAMS
                   MOVE AGLF-STATUZ    TO SYSR-STATUZ
                   PERFORM 600-FATAL-ERROR
                END-IF

                MOVE '1'               TO AGLF-VALIDFLAG
                MOVE S5035-DTETRM      TO AGLF-DTETRM
      *         MOVE S5035-BMAFLG      TO AGLF-BMAFLG
                MOVE S5035-ZBMAFLG      TO AGLF-BMAFLG

                MOVE WRITR             TO AGLF-FUNCTION
                MOVE AGLFREC           TO AGLF-FORMAT

                CALL 'AGLFIO'       USING AGLF-PARAMS
                IF AGLF-STATUZ      NOT = O-K
                   MOVE AGLF-PARAMS    TO SYSR-PARAMS
                   MOVE AGLF-STATUZ    TO SYSR-STATUZ
                   PERFORM 600-FATAL-ERROR
                END-IF
            END-IF.

       3291-EXIT.
            EXIT.

      /                                                                 <V75F01>
      ***************************                                       <V75F01>
       3300-CALL-BLDENRL SECTION.                                       <V75F01>
      ***************************                                       <V75F01>
      *                                                                 <V75F01>
       3310-START.                                                      <V75F01>
      *                                                                 <V75F01>
           INITIALIZE                     BLDENRL-BLDENRLREC.           <V75F01>
           MOVE WSSP-USERID            TO BLDENRL-USERID.               <V75F01>
           MOVE PRFX-AGNT              TO BLDENRL-PREFIX.               <V75F01>
           MOVE WSSP-COMPANY           TO BLDENRL-COMPANY.              <V75F01>
           MOVE S5035-AGNUM            TO BLDENRL-UENTITY.              <V75F01>
      *                                                                 <V75F01>
           CALL 'BLDENRL'              USING BLDENRL-BLDENRLREC.        <V75F01>
      *                                                                 <V75F01>
           IF  BLDENRL-STATUZ          NOT = O-K                        <V75F01>
               MOVE BLDENRL-BLDENRLREC TO SYSR-PARAMS                   <V75F01>
               MOVE BLDENRL-STATUZ     TO SYSR-STATUZ                   <V75F01>
               PERFORM 600-FATAL-ERROR                                  <V75F01>
           END-IF.                                                      <V75F01>
      *                                                                 <V75F01>
       3390-EXIT.                                                       <V75F01>
           EXIT.                                                        <V75F01>
      /                                                                 <V4L016>
       M200-UPDATE-MACF SECTION.                                        <V4L016>
      **************************                                        <V4L016>
                                                                        <V4L016>
       M200-START.                                                      <V4L016>
                                                                        <V4L016>
           INITIALIZE                  MACF-DATA-AREA.                  <V4L016>
           IF WSSP-FLAG                = 'T'                            <V4L016>
              MOVE 'T'                 TO MACF-AGMVTY                   <V4L016>
              MOVE 'T'                 TO WSAA-TMP-AGMVTY               <LA1174>
           ELSE                                                         <V4L016>
              IF WSSP-FLAG             = 'R'                            <V4L016>
                 MOVE 'R'              TO MACF-AGMVTY                   <V4L016>
                 MOVE 'R'              TO WSAA-TMP-AGMVTY               <LA1174>
              ELSE                                                      <V4L016>
                 MOVE 'A'              TO MACF-AGMVTY                   <V4L016>
                 MOVE 'A'              TO WSAA-TMP-AGMVTY               <LA1174>
              END-IF                                                    <V4L016>
           END-IF.                                                      <V4L016>
      *                                                                 <V4L016>
           MOVE 'N'                    TO WSAA-RECORD-FOUND.            <LA1174>
           MOVE 'N'                    TO WSAA-TRIGGER     .            <LA1174>
           MOVE ZEROS                  TO WSAA-TRANNO.                  <LA1174>
           MOVE S5035-AGTYPE           TO MACF-MLAGTTYP.                <V4L016>
           MOVE WSSP-COMPANY           TO MACF-AGNTCOY.                 <LA1233>
           MOVE S5035-AGNUM            TO MACF-AGNTNUM.                 <V4L016>
           MOVE 99999                  TO MACF-TRANNO .                 <LA1174>
           MOVE 'TDAY'                 TO DTC1-FUNCTION.                <V4L016>
           CALL 'DATCON1' USING DTC1-DATCON1-REC.                       <V4L016>
           MOVE DTC1-INT-DATE          TO WSAA-TODAY.                   <V4L016>
           MOVE WSAA-TODAY             TO MACF-EFFDATE.                 <LA1174>
           MOVE SPACES                 TO MACF-AGMVTY.                  <LA1174>
      *                                                                 <LA1174>
           MOVE BEGNH                  TO MACF-FUNCTION.                <LA1174>
           MOVE MACFREC                TO MACF-FORMAT.                  <LA1174>
      *                                                                 <LA1174>
           CALL 'MACFIO' USING MACF-PARAMS.                             <LA1174>
      *                                                                 <LA1174>
           IF MACF-STATUZ        NOT = O-K AND ENDP                     <LA1174>
              MOVE MACF-STATUZ      TO SYSR-STATUZ                      <LA1174>
              MOVE MACF-PARAMS      TO SYSR-PARAMS                      <LA1174>
              PERFORM 600-FATAL-ERROR.                                  <LA1174>
                                                                        <LA1174>
           IF (WSSP-FLAG     = 'T'     OR                               <LA1174>
               WSSP-FLAG     = 'R')    AND                              <LA1174>
               MACF-STATUZ   =  O-K    AND
               MACF-AGNTNUM  = S5035-AGNUM                              <LA1174>
              AND MACF-AGNTCOY = WSSP-COMPANY                           <LA1233>
               MOVE WSAA-TODAY           TO MACF-CURRTO                 <LA1174>
               MOVE MACF-TRANNO          TO WSAA-TRANNO                 <LA1174>
               MOVE REWRT                TO MACF-FUNCTION               <LA1174>
               CALL 'MACFIO' USING MACF-PARAMS                          <LA1174>
      *                                                                 <LA1174>
               IF MACF-STATUZ        NOT = O-K                          <LA1174>
                  MOVE MACF-STATUZ      TO SYSR-STATUZ                  <LA1174>
                  MOVE MACF-PARAMS      TO SYSR-PARAMS                  <LA1174>
                  PERFORM 600-FATAL-ERROR                               <LA1174>
               END-IF                                                   <LA1174>
               GO    TO   M200-UPD-CONT                                 <LA1174>
           END-IF.
      *                                                                 <LA1174>
           IF  WSSP-FLAG     = 'M'     AND                              <DA023>
               MACF-STATUZ   =  O-K    AND                              <DA023>
               MACF-AGNTNUM  = S5035-AGNUM                              <DA023>
              AND MACF-AGNTCOY = WSSP-COMPANY                           <DA023>
              AND WSAA-TZ606-VALID       = 'Y'                          <DA028>
              AND ( WSAA-AGTYPE-SAVE NOT = S5035-AGTYPE                 <DA028>
              OR    WSAA-ARACDE-SAVE     = S5035-ARACDE                 <DA028>
              OR    WSAA-TSALESUNT-SAVE  = S5035-TSALESUNT              <DA028>
              OR    WSAA-DTEAPP-SAVE     = S5035-DTEAPP )               <DA028>
               MOVE WSAA-TODAY           TO MACF-CURRTO                 <DA023>
               MOVE MACF-TRANNO          TO WSAA-TRANNO                 <DA023>
               MOVE REWRT                TO MACF-FUNCTION               <DA023>
               CALL 'MACFIO' USING MACF-PARAMS                          <DA023>
      *                                                                 <DA023>
               IF MACF-STATUZ        NOT = O-K                          <DA023>
                  MOVE MACF-STATUZ      TO SYSR-STATUZ                  <DA023>
                  MOVE MACF-PARAMS      TO SYSR-PARAMS                  <DA023>
                  PERFORM 600-FATAL-ERROR                               <DA023>
               END-IF                                                   <DA023>
               MOVE WSSP-FLAG           TO WSAA-TMP-AGMVTY              <DA028>
               IF S5035-AGTYPE       NOT = 'EA'                         <DA028>
                  IF WSAA-L-FLAG     NOT = SPACE                        <DA028>
                     MOVE 'L'           TO WSAA-TMP-AGMVTY              <DA028>
                  ELSE                                                  <DA028>
                     MOVE 'S'           TO WSAA-TMP-AGMVTY              <DA028>
                  END-IF                                                <DA028>
               END-IF                                                   <DA028>
               GO    TO   M200-UPD-CONT                                 <DA023>
           END-IF.                                                      <DA023>
      *                                                                 <DA023>
           IF MACF-STATUZ               = O-K            AND            <LA1174>
              TH605-CRTIND              = 'Y'                           <LA1174>
              IF MACF-AGNTNUM     NOT   = S5035-AGNUM                   <LA1174>
              OR MACF-AGNTCOY     NOT   = WSSP-COMPANY                  <LA1233>
                 MOVE  'Y'              TO   WSAA-TRIGGER               <LA1174>
                 GO    TO   M200-UPD-CONT                               <LA1174>
              ELSE                                                      <LA1174>
                 GO    TO   M200-EXIT.                                  <LA1174>
      *                                                                 <LA1174>
           IF MACF-STATUZ               = O-K            AND            <LA1174>
              MACF-AGNTCOY              = WSSP-COMPANY   AND            <LA1233>
              MACF-AGNTNUM              = S5035-AGNUM    AND            <LA1174>
              MACF-CURRTO               = VRCM-MAX-DATE  AND            <LA1174>
              TH605-CRTIND              = 'N'                           <LA1174>
              IF MACF-EFFDATE           < WSAA-TODAY                    <LA1174>
              OR MACF-AGMVTY        NOT = 'A'                           <LA1174>
                 MOVE WSAA-TODAY           TO MACF-CURRTO               <LA1174>
                 MOVE MACF-TRANNO          TO WSAA-TRANNO               <LA1174>
                 MOVE REWRT                TO MACF-FUNCTION             <LA1174>
                 CALL 'MACFIO' USING MACF-PARAMS                        <LA1174>
      *                                                                 <LA1174>
                 IF MACF-STATUZ        NOT = O-K                        <LA1174>
                    MOVE MACF-STATUZ      TO SYSR-STATUZ                <LA1174>
                    MOVE MACF-PARAMS      TO SYSR-PARAMS                <LA1174>
                    PERFORM 600-FATAL-ERROR                             <LA1174>
                 END-IF                                                 <LA1174>
              ELSE                                                      <LA1174>
                 IF MACF-EFFDATE              = WSAA-TODAY              <LA1174>
                    MOVE MACF-TRANNO          TO WSAA-TRANNO            <LA1174>
                    MOVE 'Y'                  TO WSAA-RECORD-FOUND      <LA1174>
                 END-IF                                                 <LA1174>
           END-IF.                                                      <LA1174>
      *                                                                 <V4L016>
       M200-UPD-CONT.                                                   <LA1174>
      *                                                                 <LA1174>
           MOVE RLSE                 TO MACF-FUNCTION .                 <LA1174>
           CALL 'MACFIO' USING MACF-PARAMS            .                 <LA1174>
      *                                                                 <LA1174>
           IF MACF-STATUZ        NOT = O-K                              <LA1174>
              MOVE MACF-STATUZ      TO SYSR-STATUZ                      <LA1174>
              MOVE MACF-PARAMS      TO SYSR-PARAMS                      <LA1174>
              PERFORM 600-FATAL-ERROR                                   <LA1174>
           END-IF.                                                      <LA1174>
      *---->                                                            <LA1174>
      * If a record is rewritten at this point then UPDAT will create   <LA1174>
      * a new one with incrementation of Tranno .                       <LA1174>
      *                                                                 <LA1174>
           INITIALIZE                  MACF-DATA-AREA.                  <LA1174>
           MOVE S5035-AGTYPE        TO MACF-MLAGTTYP.                   <LA1174>
           MOVE WSSP-COMPANY        TO MACF-AGNTCOY.                    <LA1233>
           MOVE S5035-AGNUM         TO MACF-AGNTNUM.                    <LA1174>
           MOVE WSAA-TMP-AGMVTY     TO MACF-AGMVTY .                    <LA1174>
           MOVE WSAA-TODAY          TO MACF-EFFDATE.                    <LA1174>
      *                                                                 <LA1174>
           IF WSAA-RECORD-FOUND NOT = 'Y'                               <LA1174>
              ADD   1               TO WSAA-TRANNO .                    <LA1174>
      *                                                                 <LA1174>
           MOVE WSAA-TRANNO         TO MACF-TRANNO .                    <LA1174>
           MOVE 1                   TO WSAA-INDEX.                      <LA1174>
           MOVE SPACES                 TO WSAA-AGNT-TYPE.               <DA015>
                                                                        <V4L016>
           IF S5035-REPSEL              = SPACES                        <V4L016>
              NEXT SENTENCE                                             <V4L016>
           ELSE                                                         <V4L016>
      ****    MOVE S5035-REPSEL        TO AGLFLNB-REPORTAG      <LA1174><V4L016>
              MOVE S5035-REPSEL        TO WSBB-REPORTAG                 <LA1174>
              PERFORM M100-GET-REPORTAG                                 <V4L016>
                     UNTIL WSBB-REPORTAG = SPACES                       <LA1174>
      ****           UNTIL AGLFLNB-REPORTAG = SPACES                    <V4L016>
           END-IF.                                                      <V4L016>
           MOVE 1                      TO WSAA-INDEX.                   <V4L016>
                                                                        <V4L016>
      *    PERFORM VARYING WSAA-INDEX FROM 1 BY 1               <LA1174><V4L016>
      *         UNTIL WSAA-INDEX > 4                            <LA1174><V4L016>
      *       MOVE WSAA-REPORTAG(WSAA-INDEX)                    <LA1174><V4L016>
      *                        TO MACF-REPORTAG(WSAA-INDEX)     <LA1174><V4L016>
      *    END-PERFORM.                                         <LA1174><V4L016>
                                                                        <V4L016>
           MOVE WSAA-REPORTAG(01)  TO MACF-ZRPTGA.                      <LA1174>
           MOVE WSAA-REPORTAG(02)  TO MACF-ZRPTGB.                      <LA1174>
           MOVE WSAA-REPORTAG(03)  TO MACF-ZRPTGC.                      <LA1174>
           MOVE WSAA-REPORTAG(04)  TO MACF-ZRPTGD.                      <LA1174>
           MOVE VRCM-MAX-DATE          TO MACF-CURRFROM                 <LA1174>
                                          MACF-CURRTO.                  <V4L016>
           MOVE WSAA-TODAY             TO MACF-CURRFROM.                <LA1174>
           MOVE ZEROES                 TO MACF-MLPARORC.                <LA1174>
           MOVE UPDAT                  TO MACF-FUNCTION.                <V4L016>
           MOVE MACFREC                TO MACF-FORMAT.                  <V4L016>
                                                                        <V4L016>
           CALL 'MACFIO' USING MACF-PARAMS.                             <V4L016>
                                                                        <V4L016>
           IF MACF-STATUZ              NOT = O-K                        <V4L016>
              MOVE MACF-PARAMS         TO SYSR-PARAMS                   <V4L016>
              PERFORM 600-FATAL-ERROR.                                  <V4L016>
                                                                        <DA015>
           PERFORM 3900-UPDATE-MALF.                                    <DA015>
      *                                                                 <DA028>
      * Update Agent Modify History with reason.                        <DA028>
      *                                                                 <DA028>
           IF  WSSP-FLAG                 = 'M'                          <DA028>
           AND (WSAA-AGTYPE-SAVE    NOT  = S5035-AGTYPE                 <DA028>
           OR   WSAA-ARACDE-SAVE    NOT  = S5035-ARACDE                 <DA028>
           OR   WSAA-TSALESUNT-SAVE NOT  = S5035-TSALESUNT              <DA028>
           OR   WSAA-DTEAPP-SAVE    NOT  = S5035-DTEAPP)                <DA028>
                PERFORM A2000-WRITE-AGRS                                <DA028>
           END-IF.                                                      <DA028>
                                                                        <V4L016>
       M200-EXIT.                                                       <V4L016>
           EXIT.                                                        <V4L016>
      /
       3600-TAKE-GRADE-HEADER SECTION.                                  <DA015>
      ********************************                                  <DA015>
       3601-START.                                                      <DA015>
      *                                                                 <DA015>
           INITIALIZE                  MALF-PARAMS.                     <DA015>
           MOVE WSSP-COMPANY           TO MALF-AGNTCOY.                 <DA015>
           MOVE S5035-AGNUM            TO MALF-AGNTNUM.                 <DA015>
           MOVE 99999                  TO MALF-TRANNO.                  <DA015>
           MOVE SPACES                 TO MALF-AGMVTY.                  <DA015>
           MOVE WSAA-TODAY             TO MALF-EFFDATE.                 <DA015>
           MOVE MALFREC                TO MALF-FORMAT.                  <DA015>
           MOVE BEGNH                  TO MALF-FUNCTION.                <DA015>
                                                                        <DA015>
           CALL 'MALFIO'               USING MALF-PARAMS.               <DA015>
           IF MALF-STATUZ              NOT = O-K                        <DA015>
           AND                         NOT = ENDP                       <DA015>
              MOVE MALF-PARAMS         TO SYSR-PARAMS                   <DA015>
              PERFORM 600-FATAL-ERROR                                   <DA015>
           END-IF.                                                      <DA015>
      *                                                                 <DA015>
       3609-EXIT.                                                       <DA015>
           EXIT.                                                        <DA015>
      /                                                                 <DA015>
      *****************************************************************
       3700-CLRRIO-CALL SECTION.
      **************************
      *
       3710-CALL.
      *****
      *    Call the Role I/O Module.
      *****
           MOVE 'CN'                   TO CLRN-CLNTPFX.
           MOVE WSSP-FSUCO             TO CLRN-CLNTCOY.
           MOVE 'AG'                   TO CLRN-FOREPFX.
           MOVE WSSP-COMPANY           TO CLRN-FORECOY.
           MOVE S5035-AGNUM            TO CLRN-FORENUM.

           CALL 'CLTRELN'              USING CLRN-CLTRELN-REC.

           IF CLRN-STATUZ              NOT = O-K
              MOVE CLRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.
      *
       3790-EXIT.
            EXIT.
      /
      *****************************************************************
       3800-AGNTLAGIO-CALL SECTION.
      **************************
      *
       3810-CALL.
      *****
      *    Call the FSU Agent I/O module.
      *****
           CALL 'AGNTLAGIO' USING AGNTLAG-PARAMS.

           IF AGNTLAG-STATUZ           NOT = O-K
              MOVE AGNTLAG-PARAMS      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.
      *
       3890-EXIT.
            EXIT.
TDO   /                                                                 <DA007>
       3900-UPDATE-MALF SECTION.                                        <DA015>
      *************************                                         <DA015>
       3901-START.                                                      <DA015>
      *                                                                 <DA015>
           MOVE SPACES             TO WSAA-OLDLDGRD                     <DA015>
                                      WSAA-NEWLDGRD.                    <DA015>
           IF WSSP-FLAG                = 'T'                            <DA015>
           OR WSSP-FLAG                = 'R'                            <DA015>
              PERFORM 3600-TAKE-GRADE-HEADER                            <DA015>
              IF MALF-STATUZ           = O-K                            <DA015>
              AND MALF-AGNTNUM         = S5035-AGNUM                    <DA015>
              AND MALF-ZRPTGA          = MACF-ZRPTGA                    <DA015>
              AND MALF-NEWLDGRD        NOT = WSAA-AGNT-TYPE(01)         <DA015>
              AND WSAA-AGNT-TYPE(01)   NOT = SPACES                     <DA015>
                 MOVE MALF-NEWLDGRD    TO WSAA-OLDLDGRD                 <DA015>
                 MOVE WSAA-AGNT-TYPE(01) TO WSAA-NEWLDGRD               <DA015>
              ELSE                                                      <DA015>
                 MOVE WSAA-AGNT-TYPE(01) TO WSAA-NEWLDGRD               <DA015>
                 MOVE SPACES             TO WSAA-OLDLDGRD               <DA015>
              END-IF                                                    <DA015>
           END-IF.                                                      <DA015>
                                                                        <DA015>
           IF WSSP-FLAG                = 'A'                            <DA015>
              MOVE WSAA-AGNT-TYPE(01) TO WSAA-NEWLDGRD                  <DA015>
              MOVE SPACES             TO WSAA-OLDLDGRD                  <DA015>
           END-IF.                                                      <DA015>
                                                                        <DA015>
           INITIALIZE                  MALF-PARAMS.                     <DA015>
           MOVE WSSP-COMPANY           TO MALF-AGNTCOY.                 <DA015>
           MOVE S5035-AGNUM            TO MALF-AGNTNUM.                 <DA015>
           MOVE MACF-TRANNO            TO MALF-TRANNO.                  <DA015>
           MOVE MACF-AGMVTY            TO MALF-AGMVTY.                  <DA015>
           MOVE MACF-EFFDATE           TO MALF-EFFDATE.                 <DA015>
           MOVE MACF-ZRPTGA            TO MALF-ZRPTGA.                  <DA015>
           MOVE WSAA-NEWLDGRD          TO MALF-NEWLDGRD.                <DA015>
           MOVE WSAA-OLDLDGRD          TO MALF-OLDLDGRD.                <DA015>
           MOVE MALFREC                TO MALF-FORMAT.                  <DA015>
           MOVE UPDAT                  TO MALF-FUNCTION.                <DA015>
                                                                        <DA015>
           CALL 'MALFIO'               USING MALF-PARAMS.               <DA015>
           IF MALF-STATUZ              NOT = O-K                        <DA015>
              MOVE MALF-PARAMS         TO SYSR-PARAMS                   <DA015>
              PERFORM 600-FATAL-ERROR                                   <DA015>
           END-IF.                                                      <DA015>
      *                                                                 <DA015>
       3909-EXIT.                                                       <DA015>
           EXIT.                                                        <DA015>
      /                                                                 <DA015>
       X300-UPDATE-CLUB-CLASS SECTION.                                  <DA007>
      ********************************                                  <DA007>
       X301-START.                                                      <DA007>
      *                                                                 <DA007>
           PERFORM X400-GET-CLUB-CLASS.                                 <DA008>
                                                                        <DA008>
           INITIALIZE                  AGLV-PARAMS.                     <DA007>
           MOVE S5035-AGNUM            TO AGLV-AGNTNUM.                 <DA007>
           MOVE WSAA-TODAY             TO AGLV-TRANDATE.                <DA007>
           IF AGLVTDA-STATUZ           = ENDP                           <DA008>
              MOVE WSAA-TODAY          TO AGLV-EFFDATE                  <DA008>
           ELSE                                                         <DA008>
              MOVE AGLVTDA-EFFDATE     TO AGLV-EFFDATE                  <DA008>
           END-IF.                                                      <DA008>
                                                                        <DA008>
           MOVE AGLVREC                TO AGLV-FORMAT.                  <DA007>
           MOVE READR                  TO AGLV-FUNCTION.                <DA007>
                                                                        <DA007>
           CALL 'AGLVIO'               USING AGLV-PARAMS.               <DA007>
                                                                        <DA007>
           IF AGLV-STATUZ              NOT = O-K                        <DA007>
           AND                         NOT = MRNF                       <DA007>
               MOVE AGLV-PARAMS        TO SYSR-PARAMS                   <DA007>
               PERFORM 600-FATAL-ERROR                                  <DA007>
           END-IF.                                                      <DA007>
                                                                        <DA007>
           IF AGLV-STATUZ              NOT = O-K                        <DA007>
           OR S5035-AGNUM              = SPACES                         <DA008>
               MOVE WRITR              TO AGLV-FUNCTION                 <DA007>
               MOVE WSAA-TODAY         TO AGLV-EFFDATE                  <DA008>
           ELSE                                                         <DA007>
               MOVE WRITD              TO AGLV-FUNCTION                 <DA007>
           END-IF.                                                      <DA007>
                                                                        <DA007>
           MOVE S5035-AGNUM            TO AGLV-AGNTNUM.                 <DA007>
           MOVE WSAA-TODAY             TO AGLV-TRANDATE.                <DA007>
           MOVE S5035-AGLVL            TO AGLV-AGLVL.                   <DA007>
           MOVE '1'                    TO AGLV-VALIDFLAG.               <DA007>
           MOVE AGLVREC                TO AGLV-FORMAT.                  <DA007>
                                                                        <DA007>
           CALL 'AGLVIO'               USING AGLV-PARAMS.               <DA007>
                                                                        <DA007>
           IF AGLV-STATUZ              NOT = O-K                        <DA007>
               MOVE AGLV-PARAMS        TO SYSR-PARAMS                   <DA007>
               PERFORM 600-FATAL-ERROR                                  <DA007>
           END-IF.                                                      <DA007>
      *                                                                 <DA007>
       X319-EXIT.                                                       <DA007>
           EXIT.                                                        <DA007>
      /                                                                 <DA008>
       X400-GET-CLUB-CLASS SECTION.                                     <DA008>
      *****************************                                     <DA008>
       X401-START.                                                      <DA008>
      *                                                                 <DA008>
           INITIALIZE                  AGLVTDA-PARAMS.                  <DA008>
           MOVE S5035-AGNUM            TO AGLVTDA-AGNTNUM.              <DA008>
           MOVE 99999999               TO AGLVTDA-TRANDATE.             <DA008>
           MOVE AGLVTDAREC             TO AGLVTDA-FORMAT.               <DA008>
           MOVE BEGN                   TO AGLVTDA-FUNCTION.             <DA008>
                                                                        <DA008>
           CALL 'AGLVTDAIO'            USING AGLVTDA-PARAMS.            <DA008>
                                                                        <DA008>
           IF AGLVTDA-STATUZ           NOT = O-K                        <DA008>
           AND                         NOT = ENDP                       <DA008>
               MOVE AGLVTDA-PARAMS     TO SYSR-PARAMS                   <DA008>
               PERFORM 600-FATAL-ERROR                                  <DA008>
           END-IF.                                                      <DA008>
      *                                                                 <DA008>
       X409-EXIT.                                                       <DA008>
           EXIT.                                                        <DA008>
      /
      *****************************************************************
      *     DECIDE WHICH TRANSACTION PROGRAM IS NEXT
      *****************************************************************
       4000-WHERE-NEXT SECTION.
      *************************
      *
       4010-NEXT-PROGRAM.
           MOVE WSAA-PROG              TO WSSP-NEXTPROG.

      *****
      *    If returning from an optional selection then retreive the
      *    Life Agent details.
      *****
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'
              MOVE RETRV               TO AGLF-FUNCTION
              CALL 'AGLFIO' USING AGLF-PARAMS
              IF AGLF-STATUZ           NOT = O-K
                 MOVE AGLF-PARAMS      TO SYSR-PARAMS
                 PERFORM 600-FATAL-ERROR.

      *****
      *    If returning from an optional selection then retreive the
      *    FSU Agent details.
      *****
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'
              MOVE RETRV               TO AGNTLAG-FUNCTION
              CALL 'AGNTLAGIO' USING AGNTLAG-PARAMS
              IF AGNTLAG-STATUZ        NOT = O-K
                 MOVE AGNTLAG-PARAMS   TO SYSR-PARAMS
                 PERFORM 600-FATAL-ERROR.

      *****
      *    If returning from an optional selection then release any
      *    held Client details.
      *****
           IF   WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'
                MOVE RLSE              TO CLTS-FUNCTION
                CALL 'CLTSIO'       USING CLTS-PARAMS

                IF   CLTS-STATUZ    NOT = O-K
                     MOVE CLTS-PARAMS  TO SYSR-PARAMS
                     PERFORM 600-FATAL-ERROR.
      *
      *****
      *    Initialise Gen Switch.
      *****
           MOVE WSSP-COMPANY           TO GENS-COMPANY.
           MOVE WSAA-PROG              TO GENS-PROG-IN.
           MOVE WSKY-BATC-BATCTRCDE    TO GENS-TRANSACT.

      *****
      *    If first time into this section (stack action blank) save
      *    next eight programs in stack
      *****
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = SPACE
              MOVE WSSP-PROGRAM-PTR    TO WSAA-X
              MOVE 0                   TO WSAA-Y
              PERFORM 4100-SAVE-PROGRAM 8 TIMES.
      *
       4020-VAL-PREV-SELECT.
      *****
      *    Check if bank details selected previously
      *****
           IF S5035-DDIND              = '?'
              PERFORM A100-BANK-DETS-CHECK.
      *****
      *    Check if broker contacts selected previously
      *****
           IF S5035-BCTIND             = '?'
              PERFORM B100-BROKER-CONTACTS.
      *****                                                             <CAS1.0>
      *    Check if OR Detalis selected previously                      <CAS1.0>
      *****                                                             <CAS1.0>
                                                                        <CAS1.0>
      **** IF S5035-ZRORIND             = '?'                           <CAS1.0>
      ****    PERFORM D100-OR-DETAILS                                   <CAS1.0>
      **** END-IF.                                                      <CAS1.0>
                                                                        <CAS1.0>
           IF S5035-ZRORIND             = '?'                           <V4L014>
              PERFORM D200-OR-DETAILS                                   <V4L014>
           END-IF.                                                      <V4L014>
                                                                        <V4L014>
      *****
      *    Check if tied agent details selected previously
      *****
           IF S5035-TAGD               = '?'
              PERFORM C100-TIED-AGENCIES.
      *
      *****
      *    Check if Client details selected previously
      *****
           IF S5035-CLIENTIND          = '?'
              MOVE WSAA-SAVE-FLAG      TO WSSP-FLAG                     <020>
              MOVE '+'                 TO S5035-CLIENTIND.
      *
      *    Check class club details.                                    <DA008>
           IF S5035-ADDOPT             = '?'                            <DA008>
              PERFORM X400-GET-CLUB-CLASS                               <DA008>
              IF AGLVTDA-STATUZ        = O-K                            <DA008>
              AND AGLVTDA-AGNTNUM      = S5035-AGNUM                    <DA008>
              AND AGLVTDA-VALIDFLAG    = '1'                            <DA008>
              AND AGLVTDA-AGLVL        = S5035-AGLVL                    <DA008>
              AND AGLVTDA-EFFDATE      NOT = VRCM-MAX-DATE              <DA008>
              AND AGLVTDA-EFFDATE      NOT = ZEROES                     <DA008>
                  MOVE S5035-AGLVL     TO WSAA-AGLVL-STORE              <DA008>
                  MOVE '+'             TO S5035-ADDOPT                  <DA008>
              ELSE                                                      <DA008>
                  MOVE SPACES          TO S5035-ADDOPT                  <DA008>
              END-IF                                                    <DA008>
           END-IF.                                                      <DA008>
      *                                                                 <DA008>
       4030-CHECK-SELECTIONS.
      *****
      *    Check if bank details is selected
      *****
           IF S5035-DDIND         = 'X'
              MOVE 'B'            TO GENS-FUNCTION
              MOVE '?'            TO S5035-DDIND
              PERFORM 4300-CALL-GENSSW
              GO TO 4090-EXIT.
      *****                                                             <CAS1.0>
      *    Check if OR details is selected                              <CAS1.0>
      *****                                                             <CAS1.0>
                                                                        <CAS1.0>
      **** IF S5035-ZRORIND              = 'X'                          <CAS1.0>
      ****    MOVE 'F'                 TO GENS-FUNCTION                 <CAS1.0>
      ****    MOVE '?'                 TO S5035-ZRORIND                 <CAS1.0>
      ****    PERFORM 4300-CALL-GENSSW                                  <CAS1.0>
      ****    GO TO 4090-EXIT                                           <CAS1.0>
      **** END-IF.                                                      <CAS1.0>
                                                                        <CAS1.0>
           IF S5035-ZRORIND              = 'X'                          <V4L014>
              MOVE 'F'                 TO GENS-FUNCTION                 <V4L014>
              MOVE '?'                 TO S5035-ZRORIND                 <V4L014>
              PERFORM 4300-CALL-GENSSW                                  <V4L014>
              GO TO 4090-EXIT                                           <V4L014>
           END-IF.                                                      <V4L014>
      *****
      *    Check if broker contacts selected
      *****
           IF S5035-BCTIND        = 'X'
              MOVE 'C'            TO GENS-FUNCTION
              MOVE '?'            TO S5035-BCTIND
              PERFORM 4300-CALL-GENSSW
              GO TO 4090-EXIT.
      *****
      *    Check if tied details selected
      *****
           IF S5035-TAGD          = 'X'
              MOVE 'A'            TO GENS-FUNCTION
              MOVE '?'            TO S5035-TAGD
              PERFORM 4300-CALL-GENSSW
              GO TO 4090-EXIT.

      * Check Class Club remark.                                        <DA008>
           IF S5035-ADDOPT             = 'X'                            <DA008>
              MOVE 'G'                 TO GENS-FUNCTION                 <DA008>
              MOVE '?'                 TO S5035-ADDOPT                  <DA008>
              MOVE S5035-AGLVL         TO WSSP-ETNAME                   <DA008>
              MOVE S5035-CLNTSEL       TO WSSP-CLNTKEY                  <DA008>
              PERFORM 4300-CALL-GENSSW                                  <DA008>
              GO TO 4090-EXIT                                           <DA008>
           END-IF.                                                      <DA008>
      *****
      *    Check if Client Details selected
      *****
           IF   S5035-CLIENTIND  NOT = 'X'
                GO TO 4050-END-CHOICES.
      *****MOVE 'D'                    TO GENS-FUNCTION.                <010>
      *****PERFORM 4300-CALL-GENSSW.                                    <010>
           MOVE 'CN'                   TO CLTS-CLNTPFX                  <020>
                                          WSKY-CLTS-CLNTPFX.            <020>
      **** MOVE WSSP-COMPANY           TO CLTS-CLNTCOY.                 <011>
           MOVE WSSP-FSUCO             TO CLTS-CLNTCOY                  <020>
                                          WSKY-CLTS-CLNTCOY.            <020>
           MOVE CLTSREC                TO CLTS-FORMAT.
           MOVE AGNTLAG-CLNTNUM        TO CLTS-CLNTNUM                  <020>
                                          WSKY-CLTS-CLNTNUM.            <020>
           MOVE READS                  TO CLTS-FUNCTION.
           CALL 'CLTSIO'            USING CLTS-PARAMS.

           IF   CLTS-STATUZ         NOT = O-K
                MOVE CLTS-PARAMS       TO SYSR-PARAMS
                PERFORM 600-FATAL-ERROR.
      *                                                                 <020>
           MOVE WSAA-CLTSKEY           TO WSSP-CLNTKEY.                 <020>
      *                                                                 <010>
           IF CLTS-CLTTYPE              = 'P'                           <010>
              MOVE 'D'                    TO GENS-FUNCTION              <010>
           ELSE                                                         <010>
              MOVE 'E'                    TO GENS-FUNCTION.             <010>
           PERFORM 4300-CALL-GENSSW.                                    <010>
      *                                                                 <010>
      * AGLF and AGNTLAG records have already been kept in 3100-section <010>
      * for all actions except for inquiry. The main reason to put this <010>
      * in is because at agent create, those two records would not have <010>
      * been written yet, so the following READS would casue a database <010>
      * error with MRNF ststus!                                         <010>
      *                                                                 <010>
           IF WSSP-FLAG NOT = 'I'                                       <010>
               GO TO 4040-ALREADY-KEPT.                                 <010>

           MOVE AGLF-AGNTNUM           TO AGNTLAG-AGNTNUM.
           MOVE 'READS'                TO AGLF-FUNCTION.
           CALL 'AGLFIO' USING AGLF-PARAMS.
           IF   AGLF-STATUZ         NOT = O-K
                MOVE AGLF-PARAMS       TO SYSR-PARAMS
                PERFORM 600-FATAL-ERROR.

           MOVE WSSP-COMPANY           TO AGNTLAG-AGNTCOY.
           MOVE S5035-AGNUM            TO AGNTLAG-AGNTNUM.
           MOVE READS                  TO AGNTLAG-FUNCTION.
           MOVE AGNTLAGREC             TO AGNTLAG-FORMAT.
           CALL 'AGNTLAGIO'         USING AGNTLAG-PARAMS.
           IF   AGNTLAG-STATUZ      NOT = O-K
                MOVE AGNTLAG-PARAMS    TO SYSR-PARAMS
                PERFORM 600-FATAL-ERROR.

                                                                        <DA008>
       4040-ALREADY-KEPT.                                               <010>

           MOVE WSSP-FLAG              TO WSAA-SAVE-FLAG.               <020>
           MOVE 'I'                    TO WSSP-FLAG.                    <020>
           MOVE '?'                    TO S5035-CLIENTIND.
           GO TO 4090-EXIT.

       4050-END-CHOICES.
      *****
      *    No more selected (or none)
      *       - restore stack form wsaa to wssp
      *****
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'
              MOVE WSSP-PROGRAM-PTR    TO WSAA-X
              MOVE 0                   TO WSAA-Y
              PERFORM 4200-RESTORE-PROGRAM 8 TIMES.

      *****
      *    If current stack action is * then re-display screen
      *       (in this case, some other option(s) were requested)
      *    Otherwise continue as normal.
      *****
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'
               MOVE SPACE            TO WSSP-SEC-ACTN(WSSP-PROGRAM-PTR)
               MOVE SCRN-SCRNAME     TO WSSP-NEXTPROG
           ELSE
               ADD 1                 TO WSSP-PROGRAM-PTR.
      *
       4090-EXIT.
            EXIT.
      /
      *****************************************************************
       4100-SAVE-PROGRAM SECTION.
      ***************************
       4110-SAVE.
           ADD 1                       TO WSAA-X.
           ADD 1                       TO WSAA-Y.
           MOVE WSSP-SEC-PROG (WSAA-X) TO WSAA-SEC-PROG(WSAA-Y).
      *
       4190-EXIT.
            EXIT.
      /
      *****************************************************************
       4200-RESTORE-PROGRAM      SECTION.
      ***********************************
       4210-RESTORE.
           ADD 1                       TO WSAA-X.
           ADD 1                       TO WSAA-Y.
           MOVE WSAA-SEC-PROG (WSAA-Y) TO WSSP-SEC-PROG(WSAA-X).
       4290-EXIT.
            EXIT.
                                                                        <V4L016>
      *****************************************************************
       4300-CALL-GENSSW SECTION.
      **************************
       4310-CALL-SUBROUTINE.
           CALL 'GENSSW' USING GENS-GENSSW-REC.
           IF GENS-STATUZ NOT = O-K
                                AND NOT = MRNF                          (012)
               MOVE GENS-STATUZ        TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR.
      *****
      * If an entry on T1675 was not found by genswch redisplay the screen
      * with an error and the options and extras indicator
      * with its initial load value
      *****
           IF GENS-STATUZ               = MRNF                          (012)
              MOVE ' '                 TO WSSP-SEC-ACTN                 (012)
                                              (WSSP-PROGRAM-PTR)        (012)
      ***     MOVE V045                TO SCRN-ERROR-CODE               (012)
              MOVE H093                TO SCRN-ERROR-CODE               (024)
              MOVE SCRN-SCRNAME        TO WSSP-NEXTPROG                 (012)
                 GO TO 4390-EXIT.                                       (012)
      *****
      *    Load from gensw to WSSP.
      *****
           ADD 1, WSSP-PROGRAM-PTR GIVING WSAA-X
           MOVE 1                      TO WSAA-Y
           PERFORM 4400-LOAD-PROGRAM 8 TIMES.

           MOVE '*'                TO WSSP-SEC-ACTN (WSSP-PROGRAM-PTR).
           ADD 1                       TO WSSP-PROGRAM-PTR.
      *
       4390-EXIT.
            EXIT.
      /
      *****************************************************************
       4400-LOAD-PROGRAM SECTION.
      ***************************
       4210-RESTORE.
           MOVE GENS-PROG-OUT (WSAA-Y) TO WSSP-SEC-PROG(WSAA-X).
           ADD 1                       TO WSAA-X.
           ADD 1                       TO WSAA-Y.
      *
       4290-EXIT.
            EXIT.
      /                                                                 <V76F06>
       A100-CALL-ROUNDING SECTION.                                      <V76F06>
      ****************************                                      <V76F06>
       A100-CALL.                                                       <V76F06>
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
               PERFORM 600-FATAL-ERROR                                  <V76F06>
           END-IF.                                                      <V76F06>
                                                                        <V76F06>
       A190-EXIT.                                                       <V76F06>
           EXIT.                                                        <V76F06>
      /                                                                 <LA1174>
       M001-UPDATE-LEVEL-1 SECTION.                                     <LA1174>
      *                                                                 <LA1174>
       M001-REPORTAG01.                                                 <LA1174>
      *                                                                 <LA1174>
           IF   S5035-REPSEL      =  WSAA-INIT-REPSEL                   <LA1174>
                GO   TO M001-EXIT.                                      <LA1174>
      *                                                                 <LA1174>
           MOVE SPACES                 TO MACFFLV-DATA-KEY.             <LA1174>
           MOVE SPACES                 TO WSAA-AGNUM      .             <LA1174>
           MOVE ZEROS                  TO WSAA-TRANNO     .             <LA1174>
           MOVE WSSP-COMPANY           TO MACFFLV-AGNTCOY.              <LA1233>
           MOVE S5035-AGNUM            TO MACFFLV-ZRPTGA                <LA1174>
                                          WSAA-AGNUM      .             <LA1174>
           MOVE SPACES                 TO MACFFLV-AGNTNUM .             <LA1174>
           MOVE VRCM-MAX-DATE          TO MACFFLV-EFFDATE.              <LA1174>
           MOVE 99999                  TO MACFFLV-TRANNO.               <LA1174>
           MOVE BEGNH                  TO MACFFLV-FUNCTION.             <LA1174>
           MOVE MACFFLVREC             TO MACFFLV-FORMAT.               <LA1174>
      *                                                                 <LA1174>
       M001-LEVEL-CALL.                                                 <LA1174>
      *                                                                 <LA1174>
           CALL 'MACFFLVIO' USING MACFFLV-PARAMS.                       <LA1174>
      *                                                                 <LA1174>
           IF MACFFLV-STATUZ        NOT = O-K AND ENDP                  <LA1174>
              MOVE MACFFLV-STATUZ      TO SYSR-STATUZ                   <LA1174>
              MOVE MACFFLV-PARAMS      TO SYSR-PARAMS                   <LA1174>
              PERFORM 600-FATAL-ERROR.                                  <LA1174>
                                                                        <LA1174>
           IF MACFFLV-STATUZ            = ENDP                          <LA1174>
           OR MACFFLV-AGNTCOY      NOT  = WSSP-COMPANY                  <LA1233>
           OR MACFFLV-ZRPTGA       NOT  = WSAA-AGNUM                    <LA1174>
              GO TO M001-EXIT.                                          <LA1174>
      *                                                                 <LA1174>
           IF MACFFLV-STATUZ            = O-K AND                       <LA1174>
              MACFFLV-CURRTO      NOT   = VRCM-MAX-DATE                 <LA1174>
              MOVE NEXTR          TO    MACFFLV-FUNCTION                <LA1174>
              GO TO  M001-LEVEL-CALL.                                   <LA1174>
      *                                                                 <LA1174>
           IF MACFFLV-STATUZ            = O-K                           <LA1174>
              MOVE SPACES          TO WSAA-STORE1-AGNUM                 <LA1174>
              MOVE SPACES          TO WSAA-STORE1-ZRPTGA                <LA1174>
              MOVE VRCM-MAX-DATE   TO WSAA-STORE1-EFFDATE               <LA1174>
              MOVE MACFFLV-ZRPTGA  TO WSAA-STORE1-ZRPTGA                <LA1174>
              MOVE MACFFLV-AGNTNUM TO WSAA-STORE1-AGNUM                 <LA1174>
              MOVE MACFFLV-EFFDATE TO WSAA-STORE1-EFFDATE               <LA1174>
              MOVE MACFFLV-TRANNO  TO WSAA-STORE1-TRANNO                <LA1174>
              MOVE MACFFLV-AGMVTY  TO WSAA-STORE1-AGMVTY                <LA1174>
           END-IF.                                                      <LA1174>
      *                                                                 <LA1174>
           IF MACFFLV-STATUZ            = O-K           AND             <LA1174>
              MACFFLV-AGNTCOY           = WSSP-COMPANY  AND             <LA1233>
              MACFFLV-ZRPTGA            = WSAA-AGNUM                    <LA1174>
      *       MACFFLV-ZRPTGB        NOT = S5035-PREPSEL                 <LA1174>
              IF MACFFLV-EFFDATE        < WSAA-TODAY    OR              <LA1174>
                 MACFFLV-AGMVTY     NOT = 'A'                           <LA1174>
                 MOVE  WSAA-TODAY         TO MACFFLV-CURRTO             <LA1174>
                 MOVE  MACFFLV-TRANNO     TO WSAA-TRANNO                <LA1174>
                 MOVE  REWRT              TO MACFFLV-FUNCTION           <LA1174>
                 CALL  'MACFFLVIO'  USING    MACFFLV-PARAMS             <LA1174>
                 IF MACFFLV-STATUZ        NOT = O-K                     <LA1174>
                    MOVE MACFFLV-STATUZ      TO SYSR-STATUZ             <LA1174>
                    MOVE MACFFLV-PARAMS      TO SYSR-PARAMS             <LA1174>
                    PERFORM 600-FATAL-ERROR                             <LA1174>
                 END-IF                                                 <LA1174>
                 MOVE  'A'                TO MACFFLV-AGMVTY             <LA1174>
                 ADD    1                 TO WSAA-TRANNO                <LA1174>
                 MOVE  WSAA-TRANNO        TO MACFFLV-TRANNO             <LA1174>
                 MOVE  WRITR              TO MACFFLV-FUNCTION           <LA1174>
                 MOVE  WSAA-TODAY         TO MACFFLV-EFFDATE            <LA1174>
                                             MACFFLV-CURRFROM           <LA1174>
                 MOVE  VRCM-MAX-DATE      TO MACFFLV-CURRTO             <LA1174>
              ELSE                                                      <LA1174>
                 IF MACFFLV-EFFDATE        = WSAA-TODAY                 <LA1174>
                    MOVE  REWRT              TO MACFFLV-FUNCTION        <LA1174>
                 ELSE                                                   <LA1174>
                    GO    TO   M001-NEXT                                <LA1174>
                 END-IF                                                 <LA1174>
              END-IF  .                                                 <LA1174>
                                                                        <LA1174>
              MOVE  S5035-AGNUM       TO WSBB-REPORTAG    .             <LA1174>
              MOVE  SPACES            TO WSAA-REPORTAGS   .             <LA1174>
              MOVE  1                 TO WSAA-INDEX       .             <LA1174>
              PERFORM M100-GET-REPORTAG                                 <LA1174>
                     UNTIL WSBB-REPORTAG = SPACES         .             <LA1174>
              MOVE WSAA-REPORTAG(02)  TO MACFFLV-ZRPTGB   .             <LA1174>
              MOVE WSAA-REPORTAG(03)  TO MACFFLV-ZRPTGC   .             <LA1174>
              MOVE WSAA-REPORTAG(04)  TO MACFFLV-ZRPTGD   .             <LA1174>
              CALL  'MACFFLVIO'  USING   MACFFLV-PARAMS   .             <LA1174>
              IF MACFFLV-STATUZ        NOT = O-K                        <LA1174>
                 MOVE MACFFLV-STATUZ      TO SYSR-STATUZ                <LA1174>
                 MOVE MACFFLV-PARAMS      TO SYSR-PARAMS                <LA1174>
                 PERFORM 600-FATAL-ERROR.                               <LA1174>
                                                                        <LA1174>
      ****    IF MACFFLV-MLAGTTYP  =  'AG'                      <PHE005><LA1174>
              IF MACFFLV-MLAGTTYP  = 'IC' OR 'ID' OR 'IG' OR 'IS'       <PHE005>
                 GO   TO  M001-NEXT                                     <LA1174>
              ELSE                                                      <LA1174>
                 MOVE MACFFLV-AGNTNUM    TO   WSAA-AGNUM                <LA1174>
                 MOVE MACFFLV-ZRPTGB     TO   WSAA-ZRPTGB               <LA1174>
                 MOVE MACFFLV-ZRPTGC     TO   WSAA-ZRPTGC               <LA1174>
                 MOVE MACFFLV-ZRPTGD     TO   WSAA-ZRPTGD               <LA1174>
                 PERFORM M00X-UPDATE-LEVEL-X.                           <LA1174>
                                                                        <LA1174>
       M001-NEXT.                                                       <LA1174>
      *   Reset Initial Key values                                      <LA1174>
           MOVE ZEROS        TO   WSAA-TRANNO     .                     <LA1174>
           MOVE NEXTR        TO   MACFFLV-FUNCTION.                     <LA1174>
           GO TO  M001-LEVEL-CALL.                                      <LA1174>
      *                                                                 <LA1174>
       M001-EXIT.                                                       <LA1174>
           EXIT.                                                        <LA1174>
      /                                                                 <LA1174>
       M00X-UPDATE-LEVEL-X SECTION.                                     <LA1174>
      *                                                                 <LA1174>
       M00X-REPORTAG0X.                                                 <LA1174>
      *                                                                 <LA1174>
           MOVE SPACES                 TO MACFFLV-DATA-KEY.             <LA1174>
           MOVE ZEROS                  TO WSAA-TRANNO     .             <LA1174>
           MOVE WSSP-COMPANY           TO MACFFLV-AGNTCOY .             <LA1233>
           MOVE WSAA-AGNUM             TO MACFFLV-ZRPTGA  .             <LA1174>
           MOVE SPACES                 TO MACFFLV-AGNTNUM .             <LA1174>
           MOVE VRCM-MAX-DATE          TO MACFFLV-EFFDATE .             <LA1174>
           MOVE 99999                  TO MACFFLV-TRANNO  .             <LA1174>
           MOVE BEGNH                  TO MACFFLV-FUNCTION.             <LA1174>
           MOVE MACFFLVREC             TO MACFFLV-FORMAT  .             <LA1174>
      *                                                                 <LA1174>
       M00X-LEVEL-CALL.                                                 <LA1174>
      *                                                                 <LA1174>
           CALL 'MACFFLVIO' USING MACFFLV-PARAMS.                       <LA1174>
      *                                                                 <LA1174>
           IF MACFFLV-STATUZ        NOT = O-K AND ENDP                  <LA1174>
              MOVE MACFFLV-STATUZ      TO SYSR-STATUZ                   <LA1174>
              MOVE MACFFLV-PARAMS      TO SYSR-PARAMS                   <LA1174>
              PERFORM 600-FATAL-ERROR.                                  <LA1174>
                                                                        <LA1174>
           IF MACFFLV-STATUZ            = ENDP                          <LA1174>
           OR MACFFLV-AGNTCOY      NOT  = WSSP-COMPANY                  <LA1233>
           OR MACFFLV-ZRPTGA       NOT  = WSAA-AGNUM                    <LA1174>
              MOVE SPACES              TO MACFFLV-DATA-KEY              <LA1174>
              MOVE SPACES              TO WSAA-AGNUM                    <LA1174>
              MOVE WSSP-COMPANY        TO MACFFLV-AGNTCOY               <LA1233>
              MOVE WSAA-STORE1-ZRPTGA  TO MACFFLV-ZRPTGA                <LA1174>
                                          WSAA-AGNUM                    <LA1174>
              MOVE WSAA-STORE1-AGNUM   TO MACFFLV-AGNTNUM               <LA1174>
              MOVE WSAA-STORE1-EFFDATE TO MACFFLV-EFFDATE               <LA1174>
              MOVE WSAA-STORE1-TRANNO  TO MACFFLV-TRANNO                <LA1174>
              MOVE WSAA-STORE1-AGMVTY  TO MACFFLV-AGMVTY                <LA1174>
              MOVE BEGNH               TO MACFFLV-FUNCTION              <LA1174>
              MOVE MACFFLVREC          TO MACFFLV-FORMAT                <LA1174>
              CALL 'MACFFLVIO' USING MACFFLV-PARAMS                     <LA1174>
              IF MACFFLV-STATUZ        NOT = O-K AND ENDP               <LA1174>
                 MOVE MACFFLV-STATUZ      TO SYSR-STATUZ                <LA1174>
                 MOVE MACFFLV-PARAMS      TO SYSR-PARAMS                <LA1174>
                 PERFORM 600-FATAL-ERROR                                <LA1174>
              END-IF                                                    <LA1174>
              GO TO M00X-EXIT.                                          <LA1174>
                                                                        <LA1174>
           IF MACFFLV-STATUZ            = O-K AND                       <LA1174>
              MACFFLV-CURRTO      NOT   = VRCM-MAX-DATE                 <LA1174>
              MOVE NEXTR          TO    MACFFLV-FUNCTION                <LA1174>
              GO TO  M00X-LEVEL-CALL.                                   <LA1174>
      *                                                                 <LA1174>
           IF MACFFLV-STATUZ            = O-K                           <LA1174>
              MOVE SPACES          TO WSAA-STOREX-AGNUM                 <LA1174>
              MOVE SPACES          TO WSAA-STOREX-ZRPTGA                <LA1174>
              MOVE VRCM-MAX-DATE   TO WSAA-STOREX-EFFDATE               <LA1174>
              MOVE MACFFLV-ZRPTGA  TO WSAA-STOREX-ZRPTGA                <LA1174>
              MOVE MACFFLV-AGNTNUM TO WSAA-STOREX-AGNUM                 <LA1174>
              MOVE MACFFLV-EFFDATE TO WSAA-STOREX-EFFDATE               <LA1174>
              MOVE MACFFLV-TRANNO  TO WSAA-STOREX-TRANNO                <LA1174>
              MOVE MACFFLV-AGMVTY  TO WSAA-STOREX-AGMVTY                <LA1174>
           END-IF.                                                      <LA1174>
      *                                                                 <LA1174>
           IF MACFFLV-STATUZ            = O-K           AND             <LA1174>
              MACFFLV-AGNTCOY           = WSSP-COMPANY  AND             <LA1233>
              MACFFLV-ZRPTGA            = WSAA-AGNUM                    <LA1174>
              IF MACFFLV-EFFDATE        < WSAA-TODAY    OR              <LA1174>
                 MACFFLV-AGMVTY     NOT = 'A'                           <LA1174>
                 MOVE  MACFFLV-TRANNO     TO WSAA-TRANNO                <LA1174>
                 MOVE  WSAA-TODAY         TO MACFFLV-CURRTO             <LA1174>
                 MOVE  REWRT              TO MACFFLV-FUNCTION           <LA1174>
                 CALL  'MACFFLVIO'  USING    MACFFLV-PARAMS             <LA1174>
                 IF MACFFLV-STATUZ        NOT = O-K                     <LA1174>
                    MOVE MACFFLV-STATUZ      TO SYSR-STATUZ             <LA1174>
                    MOVE MACFFLV-PARAMS      TO SYSR-PARAMS             <LA1174>
                    PERFORM 600-FATAL-ERROR                             <LA1174>
                 END-IF                                                 <LA1174>
                 MOVE  'A'                TO MACFFLV-AGMVTY             <LA1174>
                 ADD    1                 TO WSAA-TRANNO                <LA1174>
                 MOVE  WSAA-TRANNO        TO MACFFLV-TRANNO             <LA1174>
                 MOVE  WRITR              TO MACFFLV-FUNCTION           <LA1174>
                 MOVE  WSAA-TODAY         TO MACFFLV-EFFDATE            <LA1174>
                                             MACFFLV-CURRFROM           <LA1174>
                 MOVE  VRCM-MAX-DATE      TO MACFFLV-CURRTO             <LA1174>
              ELSE                                                      <LA1174>
                 IF MACFFLV-EFFDATE        = WSAA-TODAY                 <LA1174>
                    MOVE  REWRT              TO MACFFLV-FUNCTION        <LA1174>
                 ELSE                                                   <LA1174>
                    GO    TO   M00X-NEXT                                <LA1174>
                 END-IF                                                 <LA1174>
              END-IF  .                                                 <LA1174>
                                                                        <LA1174>
              MOVE  WSAA-AGNUM        TO WSBB-REPORTAG    .             <LA1174>
              MOVE  SPACES            TO WSAA-REPORTAGS   .             <LA1174>
              MOVE  1                 TO WSAA-INDEX       .             <LA1174>
              PERFORM M100-GET-REPORTAG                                 <LA1174>
                     UNTIL WSBB-REPORTAG = SPACES         .             <LA1174>
              MOVE WSAA-REPORTAG(02)  TO MACFFLV-ZRPTGB   .             <LA1174>
              MOVE WSAA-REPORTAG(03)  TO MACFFLV-ZRPTGC   .             <LA1174>
              MOVE WSAA-REPORTAG(04)  TO MACFFLV-ZRPTGD   .             <LA1174>
              CALL  'MACFFLVIO'  USING    MACFFLV-PARAMS  .             <LA1174>
              IF MACFFLV-STATUZ        NOT = O-K                        <LA1174>
                 MOVE MACFFLV-STATUZ      TO SYSR-STATUZ                <LA1174>
                 MOVE MACFFLV-PARAMS      TO SYSR-PARAMS                <LA1174>
                 PERFORM 600-FATAL-ERROR.                               <LA1174>
                                                                        <LA1174>
      ****    IF MACFFLV-MLAGTTYP  =  'AG'                      <PHE005><LA1174>
              IF MACFFLV-MLAGTTYP  = 'IC' OR 'ID' OR 'IG' OR 'IS'       <PHE005>
                 GO   TO  M00X-NEXT                                     <LA1174>
              ELSE                                                      <LA1174>
                 MOVE MACFFLV-AGNTNUM    TO   WSAA-AGNUM                <LA1174>
                 MOVE MACFFLV-ZRPTGB     TO   WSAA-ZRPTGB               <LA1174>
                 MOVE MACFFLV-ZRPTGC     TO   WSAA-ZRPTGC               <LA1174>
                 MOVE MACFFLV-ZRPTGD     TO   WSAA-ZRPTGD               <LA1174>
                 PERFORM M00Y-UPDATE-LEVEL-Y .                          <LA1174>
                                                                        <LA1174>
       M00X-NEXT.                                                       <LA1174>
                                                                        <LA1174>
           INITIALIZE                       MACFFLV-DATA-AREA.          <LA1174>
           MOVE ZEROS                    TO WSAA-TRANNO     .           <LA1174>
           MOVE WSSP-COMPANY             TO MACFFLV-AGNTCOY.            <LA1233>
           MOVE WSAA-STOREX-ZRPTGA       TO MACFFLV-ZRPTGA.             <LA1174>
           MOVE WSAA-STOREX-AGNUM        TO MACFFLV-AGNTNUM.            <LA1174>
           MOVE WSAA-STOREX-ZRPTGA       TO WSAA-AGNUM     .            <LA1174>
           MOVE WSAA-STOREX-EFFDATE      TO MACFFLV-EFFDATE.            <LA1174>
           MOVE WSAA-STOREX-AGMVTY       TO MACFFLV-AGMVTY .            <LA1174>
           MOVE WSAA-STOREX-TRANNO       TO MACFFLV-TRANNO .            <LA1174>
           MOVE   NEXTR      TO   MACFFLV-FUNCTION.                     <LA1174>
           GO TO  M00X-LEVEL-CALL.                                      <LA1174>
      *                                                                 <LA1174>
       M00X-EXIT.                                                       <LA1174>
           EXIT.                                                        <LA1174>
      *                                                                 <LA1174>
       M00Y-UPDATE-LEVEL-Y SECTION.                                     <LA1174>
      *                                                                 <LA1174>
       M00Y-REPORTAG0Y.                                                 <LA1174>
      *                                                                 <LA1174>
           MOVE SPACES                 TO MACFFLV-DATA-KEY.             <LA1174>
           MOVE ZEROS                  TO WSAA-TRANNO.                  <LA1174>
           MOVE WSSP-COMPANY           TO MACFFLV-AGNTCOY.              <LA1233>
           MOVE WSAA-AGNUM             TO MACFFLV-ZRPTGA    .           <LA1174>
           MOVE SPACES                 TO MACFFLV-AGNTNUM.              <LA1174>
           MOVE VRCM-MAX-DATE          TO MACFFLV-EFFDATE.              <LA1174>
           MOVE 99999                  TO MACFFLV-TRANNO .              <LA1174>
           MOVE BEGNH                  TO MACFFLV-FUNCTION.             <LA1174>
           MOVE MACFFLVREC             TO MACFFLV-FORMAT.               <LA1174>
      *                                                                 <LA1174>
       M00Y-LEVEL-CALL.                                                 <LA1174>
      *                                                                 <LA1174>
           CALL 'MACFFLVIO' USING MACFFLV-PARAMS.                       <LA1174>
      *                                                                 <LA1174>
           IF MACFFLV-STATUZ        NOT = O-K AND ENDP                  <LA1174>
              MOVE MACFFLV-STATUZ      TO SYSR-STATUZ                   <LA1174>
              MOVE MACFFLV-PARAMS      TO SYSR-PARAMS                   <LA1174>
              PERFORM 600-FATAL-ERROR.                                  <LA1174>
                                                                        <LA1174>
           IF MACFFLV-STATUZ            = ENDP                          <LA1174>
           OR MACFFLV-AGNTCOY      NOT  = WSSP-COMPANY                  <LA1233>
           OR MACFFLV-ZRPTGA       NOT  = WSAA-AGNUM                    <LA1174>
              GO TO M00Y-EXIT.                                          <LA1174>
                                                                        <LA1174>
           IF MACFFLV-STATUZ            = O-K AND                       <LA1174>
              MACFFLV-CURRTO      NOT   = VRCM-MAX-DATE                 <LA1174>
              MOVE NEXTR          TO    MACFFLV-FUNCTION                <LA1174>
              GO TO  M00Y-LEVEL-CALL.                                   <LA1174>
      *                                                                 <LA1174>
           IF MACFFLV-STATUZ            = O-K           AND             <LA1174>
              MACFFLV-AGNTCOY           = WSSP-COMPANY  AND             <LA1233>
              MACFFLV-ZRPTGA            = WSAA-AGNUM                    <LA1174>
      *      (MACFFLV-ZRPTGB        NOT = WSAA-ZRPTGB   OR              <LA1174>
      *       MACFFLV-ZRPTGC        NOT = WSAA-ZRPTGC   OR              <LA1174>
      *       MACFFLV-ZRPTGD        NOT = WSAA-ZRPTGD)                  <LA1174>
              IF MACFFLV-EFFDATE        < WSAA-TODAY    OR              <LA1174>
                 MACFFLV-AGMVTY     NOT = 'A'                           <LA1174>
                 MOVE  MACFFLV-TRANNO     TO WSAA-TRANNO                <LA1174>
                 MOVE  WSAA-TODAY         TO MACFFLV-CURRTO             <LA1174>
                 MOVE  REWRT              TO MACFFLV-FUNCTION           <LA1174>
                 CALL  'MACFFLVIO'  USING    MACFFLV-PARAMS             <LA1174>
                 IF MACFFLV-STATUZ        NOT = O-K                     <LA1174>
                    MOVE MACFFLV-STATUZ      TO SYSR-STATUZ             <LA1174>
                    MOVE MACFFLV-PARAMS      TO SYSR-PARAMS             <LA1174>
                    PERFORM 600-FATAL-ERROR                             <LA1174>
                 END-IF                                                 <LA1174>
                 MOVE  'A'                TO MACFFLV-AGMVTY             <LA1174>
                 ADD    1                 TO WSAA-TRANNO                <LA1174>
                 MOVE  WSAA-TRANNO        TO MACFFLV-TRANNO             <LA1174>
                 MOVE  WRITR              TO MACFFLV-FUNCTION           <LA1174>
                 MOVE  WSAA-TODAY         TO MACFFLV-EFFDATE            <LA1174>
                                             MACFFLV-CURRFROM           <LA1174>
                 MOVE  VRCM-MAX-DATE      TO MACFFLV-CURRTO             <LA1174>
              ELSE                                                      <LA1174>
                 IF MACFFLV-EFFDATE        = WSAA-TODAY                 <LA1174>
                    MOVE  REWRT              TO MACFFLV-FUNCTION        <LA1174>
                 ELSE                                                   <LA1174>
                    GO    TO   M00Y-NEXT                                <LA1174>
                 END-IF                                                 <LA1174>
              END-IF            .                                       <LA1174>
                                                                        <LA1174>
              MOVE  WSAA-AGNUM        TO WSBB-REPORTAG  .               <LA1174>
              MOVE  SPACES            TO WSAA-REPORTAGS .               <LA1174>
              MOVE  1                 TO WSAA-INDEX     .               <LA1174>
              PERFORM M100-GET-REPORTAG                                 <LA1174>
                     UNTIL WSBB-REPORTAG = SPACES       .               <LA1174>
              MOVE WSAA-REPORTAG(02)  TO MACFFLV-ZRPTGB .               <LA1174>
              MOVE WSAA-REPORTAG(03)  TO MACFFLV-ZRPTGC .               <LA1174>
              MOVE WSAA-REPORTAG(04)  TO MACFFLV-ZRPTGD .               <LA1174>
              CALL  'MACFFLVIO'  USING    MACFFLV-PARAMS.               <LA1174>
              IF MACFFLV-STATUZ        NOT = O-K                        <LA1174>
                 MOVE MACFFLV-STATUZ      TO SYSR-STATUZ                <LA1174>
                 MOVE MACFFLV-PARAMS      TO SYSR-PARAMS                <LA1174>
                 PERFORM 600-FATAL-ERROR.                               <LA1174>
                                                                        <LA1174>
       M00Y-NEXT.                                                       <LA1174>
           MOVE   ZEROS      TO   WSAA-TRANNO     .                     <LA1174>
           MOVE   NEXTR      TO   MACFFLV-FUNCTION.                     <LA1174>
           GO TO  M00Y-LEVEL-CALL.                                      <LA1174>
      *                                                                 <LA1174>
       M00Y-EXIT.                                                       <LA1174>
           EXIT.                                                        <LA1174>
      *                                                                 <LA1174>
      /                                                                 <LA1174>
       M100-GET-REPORTAG SECTION.                                       <V4L016>
      ***************************                                       <V4L016>
       M100-REPORTAG.                                                   <V4L016>
      *                                                                 <V4L016>
      **** MOVE AGLFLNB-REPORTAG       TO AGLFLNB-AGNTNUM.      <LA1174><V4L016>
           MOVE WSBB-REPORTAG          TO AGLFLNB-AGNTNUM.              <LA1174>
           MOVE WSSP-COMPANY           TO AGLFLNB-AGNTCOY.              <V4L016>
           MOVE READR                  TO AGLFLNB-FUNCTION.             <V4L016>
                                                                        <V4L016>
           CALL 'AGLFLNBIO' USING AGLFLNB-PARAMS.                       <V4L016>
                                                                        <V4L016>
           IF AGLFLNB-STATUZ        NOT = O-K AND MRNF                  <V4L016>
              MOVE AGLFLNB-STATUZ      TO SYSR-STATUZ                   <V4L016>
              MOVE AGLFLNB-PARAMS      TO SYSR-PARAMS                   <V4L016>
              PERFORM 600-FATAL-ERROR                                   <V4L016>
           END-IF.                                                      <V4L016>
                                                                        <V4L016>
           IF AGLFLNB-STATUZ            = O-K                           <V4L016>
              MOVE AGLFLNB-AGNTNUM     TO WSAA-REPORTAG(WSAA-INDEX)     <V4L016>
              MOVE AGLFLNB-AGTYPE      TO WSAA-AGNT-TYPE(WSAA-INDEX)    <DA015>
              ADD 1                    TO WSAA-INDEX                    <V4L016>
              MOVE AGLFLNB-REPORTAG    TO WSBB-REPORTAG                 <LA1174>
           END-IF.                                                      <V4L016>
                                                                        <V4L016>
       M100-EXIT.                                                       <V4L016>
           EXIT.                                                        <V4L016>
      /                                                                 <V4L016>
       M400-GET-REPORTAG SECTION.                                       <V4L016>
      ***************************                                       <V4L016>
       M400-REPORTAG.                                                   <V4L016>
      *                                                                 <V4L016>
           MOVE AGLFLNB-REPORTAG       TO AGLFLNB-AGNTNUM.              <V4L016>
           MOVE WSSP-COMPANY           TO AGLFLNB-AGNTCOY.              <V4L016>
           MOVE READR                  TO AGLFLNB-FUNCTION.             <V4L016>
                                                                        <V4L016>
           CALL 'AGLFLNBIO' USING AGLFLNB-PARAMS.                       <V4L016>
                                                                        <V4L016>
           IF AGLFLNB-STATUZ        NOT = O-K AND MRNF                  <V4L016>
              MOVE AGLFLNB-STATUZ      TO SYSR-STATUZ                   <V4L016>
              MOVE AGLFLNB-PARAMS      TO SYSR-PARAMS                   <V4L016>
              PERFORM 600-FATAL-ERROR                                   <V4L016>
           END-IF.                                                      <V4L016>
                                                                        <V4L016>
           IF AGLFLNB-STATUZ            = O-K                           <V4L016>
              NEXT SENTENCE                                             <V4L016>
           ELSE                                                         <V4L016>
              MOVE SPACES              TO AGLFLNB-REPORTAG              <V4L016>
           END-IF.                                                      <V4L016>
                                                                        <V4L016>
       M400-EXIT.                                                       <V4L016>
           EXIT.                                                        <V4L016>
      *******************************                                   <AG002>
       N000-CHECK-INTRO-EXIST SECTION.                                  <AG002>
      *******************************                                   <AG002>
       READ-ZAGIMNT.                                                    <AG002>
      *                                                                 <AG002>
           MOVE 'N'                    TO WSAA-INTRO-EXIST-FLAG.        <AG002>
           INITIALIZE                     ZAGIMNT-DATA-KEY.             <AG002>
           MOVE WSSP-COMPANY           TO ZAGIMNT-BATCCOY.              <AG002>
           MOVE '1'                    TO ZAGIMNT-VALIDFLAG.            <AG002>
           MOVE AGLF-AGNTNUM           TO ZAGIMNT-AGNTNUM.              <AG002>
           MOVE 'READR'                TO ZAGIMNT-FUNCTION.             <AG002>
                                                                        <AG002>
           CALL 'ZAGIMNTIO' USING ZAGIMNT-PARAMS.                       <AG002>
                                                                        <AG002>
           IF ZAGIMNT-STATUZ           NOT = O-K                        <AG002>
                                   AND NOT = MRNF                       <AG002>
              MOVE ZAGIMNT-PARAMS      TO SYSR-PARAMS                   <AG002>
              PERFORM 600-FATAL-ERROR.                                  <AG002>
                                                                        <AG002>
           IF ZAGIMNT-STATUZ               = O-K                        <AG002>
              MOVE 'Y'                 TO WSAA-INTRO-EXIST-FLAG         <AG002>
           END-IF.                                                      <AG002>
                                                                        <AG002>
           IF ZAGIMNT-STATUZ               = MRNF                       <AG002>
              MOVE 'N'                 TO WSAA-INTRO-EXIST-FLAG         <AG002>
           END-IF.                                                      <AG002>
      *                                                                 <AG002>
       N000-EXIT.                                                       <AG002>
            EXIT.                                                       <AG002>
      /                                                                 <DA023>
       A1000-READ-TZ606 SECTION.                                        <DA023>
      **************************                                        <DA023>
       A1010-START.                                                     <DA023>
      *                                                                 <DA023>
           MOVE 'N'                    TO WSAA-TZ606-VALID.             <DA023>
           MOVE 'N'                    TO WSAA-TZ606-FLAG.              <DA023>
           INITIALIZE                     ITEM-PARAMS.                  <DA023>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <DA023>
           MOVE TZ606                  TO ITEM-ITEMTABL.                <DA023>
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.                 <DA023>
           MOVE S5035-AGTYPE           TO ITEM-ITEMITEM.                <DA028>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <DA023>
           MOVE READR                  TO ITEM-FUNCTION.                <DA023>
                                                                        <DA023>
           CALL 'ITEMIO' USING            ITEM-PARAMS.                  <DA023>
           IF ITEM-STATUZ       NOT = O-K AND MRNF                      <DA023>
              MOVE ITEM-STATUZ      TO SYSR-STATUZ                      <DA023>
              MOVE ITEM-PARAMS      TO SYSR-PARAMS                      <DA023>
              PERFORM 600-FATAL-ERROR                                   <DA023>
           END-IF.                                                      <DA023>
                                                                        <DA023>
           IF ITEM-STATUZ               = O-K                           <DA028>
              MOVE 'Y'                 TO WSAA-TZ606-FLAG               <DA028>
              MOVE ITEM-GENAREA        TO TZ606-TZ606-REC               <DA028>
              IF WSAA-AGTYPE-SAVE       = TZ606-MLAGTTYP-01 OR          <DA028>
                 WSAA-AGTYPE-SAVE       = TZ606-MLAGTTYP-02 OR          <DA028>
                 WSAA-AGTYPE-SAVE       = TZ606-MLAGTTYP-03 OR          <DA028>
                 WSAA-AGTYPE-SAVE       = TZ606-MLAGTTYP-04 OR          <DA028>
                 WSAA-AGTYPE-SAVE       = TZ606-MLAGTTYP-05 OR          <DA028>
                 WSAA-AGTYPE-SAVE       = TZ606-MLAGTTYP-06 OR          <DA028>
                 WSAA-AGTYPE-SAVE       = TZ606-MLAGTTYP-07 OR          <DA028>
                 WSAA-AGTYPE-SAVE       = TZ606-MLAGTTYP-08 OR          <DA028>
                 WSAA-AGTYPE-SAVE       = TZ606-MLAGTTYP-09 OR          <DA028>
                 WSAA-AGTYPE-SAVE       = TZ606-MLAGTTYP-10 OR          <DA028>
                 WSAA-AGTYPE-SAVE       = TZ606-MLAGTTYP-11 OR          <DA028>
                 WSAA-AGTYPE-SAVE       = TZ606-MLAGTTYP-12 OR          <DA028>
                 WSAA-AGTYPE-SAVE       = TZ606-MLAGTTYP-13 OR          <DA028>
                 WSAA-AGTYPE-SAVE       = TZ606-MLAGTTYP-14 OR          <DA028>
                 WSAA-AGTYPE-SAVE       = TZ606-MLAGTTYP-15 OR          <DA028>
                 WSAA-AGTYPE-SAVE       = TZ606-MLAGTTYP-16 OR          <DA028>
                 WSAA-AGTYPE-SAVE       = TZ606-MLAGTTYP-17 OR          <DA028>
                 WSAA-AGTYPE-SAVE       = TZ606-MLAGTTYP-18 OR          <DA028>
                 WSAA-AGTYPE-SAVE       = TZ606-MLAGTTYP-19             <DA028>
                 MOVE 'Y'              TO WSAA-TZ606-VALID              <DA028>
              END-IF                                                    <DA028>
           END-IF.                                                      <DA028>
      *                                                                 <DA028>
       A1090-EXIT.                                                      <DA028>
            EXIT.                                                       <DA028>
      /                                                                 <DA028>
       A2000-WRITE-AGRS SECTION.                                        <DA028>
      **************************                                        <DA028>
       A2010-START.                                                     <DA028>
      *                                                                 <DA028>
           MOVE SPACES                 TO WSAA-AGTYPE-STR               <DA028>
                                          WSAA-TSALESUNT-STR            <DA028>
                                          WSAA-ARACDE-STR               <DA028>
                                          WSAA-DTEAPP-STR               <DA028>
                                          WSAA-AGNOTES.                 <DA028>
           MOVE ZEROES                 TO WS-CNT                        <DA028>
                                          WSAA-IX                       <DA028>
                                          WSAA-IY.                      <DA028>
                                                                        <DA028>
           INITIALIZE                     AGRS-PARAMS.                  <DA028>
           MOVE WSAA-TMP-AGMVTY        TO AGRS-AGMVTY.                  <DA028>
           MOVE WSAA-TRANNO            TO AGRS-TRANNO.                  <DA028>
           MOVE AGLF-AGNTCOY           TO AGRS-AGNTCOY.                 <DA028>
           MOVE S5035-AGNUM            TO AGRS-AGNTNUM.                 <DA028>
           MOVE WSAA-TODAY             TO AGRS-EFFDATE.                 <DA028>
           MOVE SPACES                 TO AGRS-REACODE.                 <DA028>
                                                                        <DA028>
           IF WSAA-AGTYPE-SAVE      NOT = S5035-AGTYPE                  <DA028>
              MOVE WSAA-AGTYPE-SAVE    TO WSAA-AGTYPE-STR               <DA028>
           END-IF.                                                      <DA028>
                                                                        <DA028>
           IF WSAA-TSALESUNT-SAVE   NOT = S5035-TSALESUNT               <DA028>
              MOVE WSAA-TSALESUNT-SAVE TO WSAA-AGTYPE-STR               <DA028>
           END-IF.                                                      <DA028>
                                                                        <DA028>
           IF WSAA-ARACDE-SAVE      NOT = S5035-ARACDE                  <DA028>
              MOVE WSAA-ARACDE-SAVE    TO WSAA-ARACDE-STR               <DA028>
           END-IF.                                                      <DA028>
                                                                        <DA028>
           IF WSAA-DTEAPP-SAVE      NOT = S5035-DTEAPP                  <DA028>
              MOVE WSAA-DTEAPP-SAVE    TO DTC1-INT-DATE                 <DA028>
              MOVE 'CONV'              TO DTC1-FUNCTION                 <DA028>
              CALL 'DATCON1'        USING DTC1-DATCON1-REC              <DA028>
              MOVE DTC1-EXT-DATE       TO WSAA-DTEAPP-STR               <DA028>
           END-IF.                                                      <DA028>
                                                                        <DA028>
           STRING WSAA-AGTYPE-STR      DELIMITED BY '  '               <DA028>
                  ' '                  DELIMITED BY SIZE                <DA028>
                  WSAA-TSALESUNT-STR   DELIMITED BY SPACE               <DA028>
                  ' '                  DELIMITED BY SIZE                <DA028>
                  WSAA-ARACDE-STR      DELIMITED BY SPACE               <DA028>
                  ' '                  DELIMITED BY SIZE                <DA028>
                  WSAA-DTEAPP-STR      DELIMITED BY SPACE               <DA028>
                                       INTO WSAA-AGNOTES                <DA028>
           END-STRING.                                                  <DA028>
           INSPECT WSAA-AGNOTES TALLYING WS-CNT FOR LEADING ' '.        <DA028>
           COMPUTE WSAA-IX              = WS-CNT + 1.                   <DA028>
           COMPUTE WSAA-IY              = (LENGTH OF WSAA-AGNOTES)      <DA028>
                                        - WS-CNT.                       <DA028>
           MOVE WSAA-AGNOTES(WSAA-IX:WSAA-IY)                           <DA028>
                                       TO AGRS-AGNOTES.                 <DA028>
           MOVE S5035-AGTYPE           TO AGRS-ACCOUNT-TYPE.            <DA028>
           MOVE WRITR                  TO AGRS-FUNCTION.                <DA028>
           MOVE AGRSREC                TO AGRS-FORMAT.                  <DA028>
           CALL 'AGRSIO' USING AGRS-PARAMS.                             <DA028>
           IF AGRS-STATUZ           NOT = O-K AND MRNF                  <DA028>
              MOVE AGRS-STATUZ         TO SYSR-STATUZ                   <DA028>
              MOVE AGRS-PARAMS         TO SYSR-PARAMS                   <DA028>
              PERFORM 600-FATAL-ERROR                                   <DA028>
           END-IF.                                                      <DA028>
      *                                                                 <DA028>
       A2090-EXIT.                                                      <DA028>
            EXIT.                                                       <DA028>
      /                                                                 <DA028>
       A3000-CHECK-AGENT-MRDT SECTION.                                  <NB031>
      **************************                                        <NB031>
       A3010-START.                                                     <NB031>
      *                                                                 <NB031>
           INITIALIZE                  AGMR-PARAMS.                     <NB031>
           MOVE S5035-AGNUM            TO AGMR-AGNTNUM.                 <NB031>
           MOVE AGMRREC                TO AGMR-FORMAT.                  <NB031>
           MOVE READR                  TO AGMR-FUNCTION.                <NB031>
                                                                        <NB031>
           CALL 'AGMRIO'            USING AGMR-PARAMS.                  <NB031>
                                                                        <NB031>
           IF AGMR-STATUZ           NOT = O-K                           <NB031>
           AND                      NOT = MRNF                          <NB031>
              MOVE AGMR-PARAMS         TO SYSR-PARAMS                   <NB031>
              PERFORM 600-FATAL-ERROR                                   <NB031>
           END-IF.                                                      <NB031>
                                                                        <NB031>
           IF AGMR-STATUZ               = O-K                           <NB031>
              MOVE 'Y'                 TO S5035-MRDTFLAG                <NB031>
           END-IF.                                                      <NB031>
      *                                                                 <NB031>
       A3090-EXIT.                                                      <NB031>
           EXIT.                                                        <NB031>
      /                                                                 <NB031>
       A4000-CHECK-AGENT-DOWNLINE SECTION.                              <DA028>
      ************************************                              <DA028>
       A4010-START.                                                     <DA028>
                                                                        <DA028>
           INITIALIZE                     AGLFRPT-PARAMS.               <DA028>
           MOVE 'AG'                   TO WSAA-AGNTPFX.                 <DA028>
           MOVE WSSP-COMPANY           TO WSAA-AGNTCOY.                 <DA028>
           MOVE S5035-AGNUM            TO WSAA-AGNTNUM.                 <DA028>
           MOVE WSAA-AGLFRPT-KEY       TO AGLFRPT-REPAGENT01.           <DA028>
                                                                        <DA028>
           MOVE AGLFRPTREC             TO AGLFRPT-FORMAT.               <DA028>
      *    MOVE READR                  TO AGLFRPT-FUNCTION.             <DA028>
                                                                        <DA028>
      *    CALL 'AGLFRPTIO'            USING AGLFRPT-PARAMS.            <DA028>
                                                                        <DA028>
           MOVE 'N'                    TO WSAA-INVALID-AGENT.           <DA028>
           MOVE BEGN                   TO AGLFRPT-FUNCTION.             <DA028>
                                                                        <DA028>
           PERFORM UNTIL AGLFRPT-STATUZ = ENDP                          <DA028>
                      OR WSAA-INVALID-AGENT    = 'Y'                    <DA028>
                                                                        <DA028>
               CALL 'AGLFRPTIO'            USING AGLFRPT-PARAMS         <DA028>
                                                                        <DA028>
               IF AGLFRPT-STATUZ        NOT = O-K                       <DA028>
                  AND                   NOT = ENDP                      <DA028>
                    MOVE AGLFRPT-PARAMS     TO SYSR-PARAMS              <DA028>
                    MOVE AGLFRPT-STATUZ     TO SYSR-STATUZ              <DA028>
                    PERFORM 600-FATAL-ERROR                             <DA028>
               END-IF                                                   <DA028>
                                                                        <DA028>
               IF AGLFRPT-REPAGENT01    NOT = WSAA-AGLFRPT-KEY          <DA028>
                    MOVE ENDP               TO AGLFRPT-STATUZ           <DA028>
               ELSE                                                     <DA028>
                                                                        <DA028>
                    MOVE AGLFRPT-AGNTNUM    TO AGLFLNB-AGNTNUM          <DA028>
                    PERFORM A5000-CHECK-AGENT-BELOW                     <DA028>
                                                                        <DA028>
                    MOVE NEXTR              TO AGLFRPT-FUNCTION         <DA028>
               END-IF                                                   <DA028>
                                                                        <DA028>
           END-PERFORM.                                                 <DA028>
                                                                        <DA028>
           IF WSAA-INVALID-AGENT        = 'Y'                           <DA028>
               MOVE E535               TO S5035-DTETRM-ERR              <DA028>
           END-IF.                                                      <DA028>
      *                                                                 <DA028>
       A4090-EXIT.                                                      <DA028>
            EXIT.                                                       <DA028>
      /                                                                 <DA028>
       A5000-CHECK-AGENT-BELOW SECTION.                                 <DA028>
      *********************************                                 <DA028>
       A5010-START.                                                     <DA028>
           MOVE WSSP-COMPANY           TO AGLFLNB-AGNTCOY.              <DA028>
           MOVE READR                  TO AGLFLNB-FUNCTION.             <DA028>
                                                                        <DA028>
           CALL 'AGLFLNBIO' USING AGLFLNB-PARAMS.                       <DA028>
                                                                        <DA028>
           IF AGLFLNB-STATUZ        NOT = O-K AND MRNF                  <DA028>
              MOVE AGLFLNB-STATUZ      TO SYSR-STATUZ                   <DA028>
              MOVE AGLFLNB-PARAMS      TO SYSR-PARAMS                   <DA028>
              PERFORM 600-FATAL-ERROR                                   <DA028>
           END-IF.                                                      <DA028>
                                                                        <DA028>
           IF  AGLFLNB-STATUZ           = O-K                           <DA028>
           AND AGLFLNB-DTETRM           = VRCM-MAX-DATE                 <DA028>
           AND AGLFLNB-REPORTAG         = S5035-AGNUM                   <DA028>
               IF TZ606-MLAGTTYP-20     = SPACES                        <DA028>
                  MOVE 'Y'             TO WSAA-INVALID-AGENT            <DA028>
               ELSE                                                     <DA028>
                  PERFORM A6000-CHECK-TYPE-BELOW                        <DA028>
                  IF WSAA-FOUND     NOT = 'Y'                           <DA028>
                     MOVE 'Y'          TO WSAA-INVALID-AGENT            <DA028>
                  END-IF                                                <DA028>
               END-IF                                                   <DA028>
           END-IF.                                                      <DA028>
      *                                                                 <DA028>
       A5090-EXIT.                                                      <DA028>
           EXIT.                                                        <DA028>
      /                                                                 <DA028>
       A6000-CHECK-TYPE-BELOW SECTION.                                  <DA028>
      ********************************                                  <DA028>
       A6010-START.                                                     <DA028>
      *                                                                 <DA028>
      * Get Agent Type of Agent Downlight.                              <DA028>
      *                                                                 <DA028>
           MOVE 'N'                 TO WSAA-FOUND.                      <DA028>
           INITIALIZE                  AGNT-PARAMS.                     <DA028>
           MOVE WSSP-COMPANY        TO AGNT-AGNTCOY.                    <DA028>
           MOVE 'AG'                TO AGNT-AGNTPFX.                    <DA028>
           MOVE AGLFRPT-AGNTNUM     TO AGNT-AGNTNUM.                    <DA028>
           MOVE READR               TO AGNT-FUNCTION.                   <DA028>
           CALL 'AGNTIO'         USING AGNT-PARAMS                      <DA028>
           IF AGNT-STATUZ        NOT = O-K                              <DA028>
              MOVE AGNT-STATUZ      TO SYSR-STATUZ                      <DA028>
              MOVE AGNT-PARAMS      TO SYSR-PARAMS                      <DA028>
              PERFORM 600-FATAL-ERROR                                   <DA028>
           END-IF.                                                      <DA028>
                                                                        <DA028>
           MOVE 20                     TO IX.                           <DA028>
           PERFORM VARYING IX FROM 20 BY 1 UNTIL IX > 38                <DA028>
                                      OR WSAA-FOUND = 'Y'               <DA028>
              IF AGNT-AGTYPE         = TZ606-MLAGTTYP(IX)               <DA028>
                 MOVE 'Y'           TO WSAA-FOUND                       <DA028>
              END-IF                                                    <DA028>
           END-PERFORM.                                                 <DA028>
      *                                                                 <DA028>
       A6090-EXIT.                                                      <DA028>
           EXIT.                                                        <DA028>
      /                                                                 <DA028>
