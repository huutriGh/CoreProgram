      * Generation Parameters SCRVER(02)               Do Not Delete!   <S9503>
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P5006.
      *REMARKS.
      *                       COMPONENT SELECTION
      *                       ===================
      * Initialise
      * ----------
      *
      * Skip  this  section  if  returning from an optional selection
      * (current stack position action flag = '*').
      *
      * Clear the subfile ready for loading.
      *
      * Read  CHDRLNB  (RETRV) in order to obtain the contract header
      * information  and  read  LIFELNB  (RETRV) for the life assured
      * details.
      *
      * Read  the  contract definition description from T5688 for the
      * contract type held on CHDRLNB.

      * LIFE ASSURED AND JOINT LIFE DETAILS
      *
      * To obtain the life assured and joint-life details (if any) do
      * the following;-
      *
      *       - using the life  detaiLs  passed (retrieved above from
      *         LIFELNB, joint life number '00').  Look  up the  name
      *         from  the  client  details  (CLTS)  and  format as  a
      *         "confirmation name".
      *
      *       - read the joint life details using LIFELNB (joint life
      *         number '01').  If  found, look up  the name  from the
      *         client details (CLTS) and  format as  a "confirmation
      *         name".
      *
      * This  program displays component parts for either a life or a
      * coverage.  To determine which version is required, attempt to
      * retrieve the current coverage transaction (from COVTLNB).  If
      * there  is no COVTLNB record to retrieve (MRNF return status),
      * coverages  and  riders  for the entire contract are required.
      * Otherwise, just riders for the current coverage are required.
      *
      * Read  the  contract  structure  table  (T5673),  accessed  by
      * contract  type  (from  CHDRLNB) and effective on the original
      * inception  date  of  the contract. When processing this table
      * item,  it  can  continue  onto  additional  (unlimited) table
      * items.  These  should  be  read in turn during the processing
      * described below.  The  original  inception  date must also be
      * used to provide the correct version of each item read.
      *
      * THE CASE OF ALL COVERAGES AND RIDERS
      *
      * Taking one coverage at  a  time, one record is written to the
      * subfile for the  coverage,  with one for each rider attaching
      * to it. The coverage record is written with a blank rider code
      * and the rider with a blank coverage code. In the table, there
      * is  space for six riders against each coverage. It there is a
      * requirement for more than six riders on a coverage, they will
      * be  continued  on  the next line.  In this case, the coverage
      * will  be  left  blank  to  indicate  a continuation.  NB, the
      * coverage  on the last line of a table item could be continued
      * onto  another  item.  Look  up  the  long  description of the
      * coverage/rider   from  the  general  coverage  details  table
      * (T5687) for each subfile record written.
      *
      * It is possible that the proposal has already been set up when
      * a user  requests  this  screen.  Read  the  current  coverage
      * transaction   records   for   the  coverage  being  processed
      * (COVTCOV,   keyed   company,   contract-no,   coverage  type,
      * coverage-no; select  rider-no  = 0). Accumulate the number of
      * DIFFERENT coverages  of  the same type already stored. (There
      * could be multiple  records  with  the  same  key.  Ignore the
      * duplicates.) Whenever  a  transaction is read for a different
      * coverage, keep  the  number  of the highest transaction read.
      * This is needed  to  calculate  the next coverage number to be
      * allocated. (The  coverage  number  needed  is the highest one
      * used by any coverage  type.) If there is already at least one
      * coverage transaction, put  a  '+'  in  the  select field.  If
      * there is already the maximum allowable (as defined in T5673),
      * protect the select field so that no more can be requested. If
      * the coverage is  mandatory  (this is indicated by an "R", for
      * required, in  the  coverage required indicator) and there are
      * no transactions for  the  coverage on file, "select" the line
      * by moving "X" in to the select field and protect it.
      *
      * When adding  rider details to  subfile, they may be mandatory
      * (indicated in a similar way to mandatory coverages above). In
      * this case, "select" the  line  by moving "X" in to the select
      * field and protect  it.  -  note  that rider selection will be
      * ignored if the coverage is not selected.
      *
      * Details of all  coverages  and  riders  for the contract type
      * should be loaded in this case.
      *
      * THE CASE OF ALL RIDERS FOR ONE COVERAGE ONLY
      *
      * In this case, a COVTLNB record will have been retrieved. This
      * will hold details of  the  coverage to which riders are to be
      * added. Write a coverage  record to the subfile, with a '+' in
      * the selection field,  and with this field protected. (Look up
      * the description as above.)
      *
      * Search through  the  contract structure table entries (T5673)
      * for the applicable  coverage  code. This will give the riders
      * attachable to this  coverage  in this case. Write a record to
      * the subfile for each applicable rider.
      *
      * It is possible that  the riders have already been set up when
      * a  user   requests   this  screen.  Read  the  current  rider
      * transaction record  for  the  rider being processed (COVTRID,
      * keyed  company, contract-no, coverage-no, rider type;  select
      * rider-no *ne 0).  If  there  is already a record, this is the
      * maximum allowable, so  put  a  '+'  in  the  select field and
      * protect it so that no more can be requested.  If the rider is
      * mandatory (this is  indicated by an "R", for required, in the
      * rider required indicator) and there is no transaction for the
      * rider on file, "select"  the  line  by  moving  "X" in to the
      * select field and protect it.
      *
      * Whenever a transaction  is  read  for a different rider, keep
      * the number of  the  highest transaction read.  This is needed
      * to calculate the next rider number to be allocated.
      *
      * In all cases, load  all pages required in the subfile and set
      * the subfile more indicator to no.
      *
      * Validation
      * ----------
      *
      * Skip  this  section  if  returning from an optional selection
      * (current stack position action flag = '*').
      *
      * Read  all   modified  subfile  records.  The  only  allowable
      * selections are  '+',  'X'  and  blank.  Ignore inconsistences
      * between riders being selected for coverages which have not.
      *
      * Updating
      * --------
      *
      * This program performs no updating.
      *
      * Next program
      * ------------
      *
      * If returning from  a  selection (action action is '*'), check
      * that  if  the  current  component  was  mandatory,  that  his
      * actually been entered.  (Use  a  hidden  field in the subfile
      * record when loading in  the 1000 section to indicate if it is
      * a required  section?)  Read the current coverage/ transaction
      * record again (COVTLNB,  the  key  was  set up by this program
      * previously.) If  there  is  no  transaction for the mandatory
      * component, KEEPS the  current  key  again and exit.
      *
      *
      *
      *
      *
      * If not returning  from  a  component (stack action is blank),
      * save the next four  programs currently on the stack. Read the
      * first record from  the  subfile. If this is not selected ('X'
      * in select field), read  the  next  one  and  so  on,  until a
      * selected record is  found,  or  the  end  of  the  subfile is
      * reached.
      *
      * For the case  when  all  coverages  and riders are displayed,
      * keep track of whether  a  coverage  is  selected or not. If a
      * coverage is not  selected,  skip  all its riders even if they
      * are selected.  If  a  coverage  is  selected, re-set the next
      * rider sequence number to zero.
      *
      * For the case  when  only  the  riders  for  one  coverage are
      * displayed, the coverage cannot be selected. Process the rider
      * selected even though the coverage has not.
      *
      * Once the end  of  the  subfile  has been reached, restore the
      * previously saved  four  programs, blank out the stack action,
      * add one to the pointer and exit.
      *
      * If a subfile  record  has been selected, look up the programs
      * required to  be  processed  from  the coverage/rider programs
      * table (T5671  -  accessed  by transaction number concatenated
      * with coverage/rider code from the subfile record). Move these
      * four programs into  the  program  stack  and  set the current
      * stack action to '*'  (so  that the system will return to this
      * program to process the next one).
      *
      * Set up the  key details of the coverage/rider to be processed
      * (in COVTLNB using  the KEEPS function) by the called programs
      * as follows:
      *
      *       Company - WSSP company
      *       Contract no - from CHDRLNB
      *       Life number - from LIFELNB
      *       Coverage number -  from COVTLNB if read in the 1000
      *            section  (i.e.  adding  to  coverage case), or
      *            sequentially from the number calculated in the
      *            1000  section for each coverage selected (i.e.
      *            adding to life case)
      *       Rider number -  '00'  if  this  is  a  coverage, or
      *            sequentially from the number calculated above
      *
      * Add one to  the  program  pointer  and  exit  to  process the
      * required generic component.
      *
      ******************Enhancements for Life Asia 1.0****************
      *
      * In order to allow components to be selected on a specific life,
      * a new indicatore has been introduced to T5673. This indicator
      * specifies whether the component can be selected for just the
      * primary life, or just the subsequent lives or for all lives.
      * This module uses the indicator T5673-ZRLIFIND to determine
      * which component may be selected. The processing is as follows :
      *
      * - For each component found on T5673, check the T5673-ZRLIFIND :
      *
      *   - If we are processing a primary life and the T5673-ZRLIFIND
      *     contains an 'S' for that component, then don't set up the
      *     component details for display.
      *
      *   - If we are processing subsequent lives and T5673-ZRLIFIND
      *     contains an 'P' for that component, then don't set up the
      *     component details for display.
      *****************************************************************
      *              AMENDMENT  HISTORY                               *
      *****************************************************************
      * DATE.....   BY..   AMENDMENT...............  NUMBER
      *
      * DD/MM/YY    X.X.   xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  NNN
      * 08.05.89    A.L.   New field on COVR and COVT files, needs  001
      *                    to be initialised.
      * 25.09.89    A.G.   Intialise COVTCOV-COVERAGE field if on   002
      *                    reading the file with BEGN no coverages
      *                    are found for the contract.
      * 13.02.91    L.T.   Add message to say continuation items    003
      *                    not on T5673. SDF 1292
      *                    Also check to see if an additional
      *                    continuation table is required. SDF 1292
      * 25.06.91    N.F.   Initialise new COVTLNB field (PAYRSEQNO) 004
      *                    to 1.
      *
      *                    AQR 1801/883.
      * 26/06/92    J.L.   Move the chdrlnb-cntcurr to the covtlnb  005
      *                    cntcurr field.
      *
      * 13/10/92   F.O'R.  AQR 74.                                  006
      *                    All selection fields must be checked for
      *                    invalid entries.
      *
      * 16/10/92   F.O'R.  AQR 3309                                 007
      *                    Subscript checking was incorrect while
      *                    looping around the rider array on table
      *                    T5673 and was causing an MCH0603
      *                    (Subscript out of range).
      *                    Also when starting on a new line of riders
      *                    on T5673, no check was being made to ensure
      *                    they were for the same coverage.
      *                    The continuation item was being read in-
      *                    correctly.
      *
      * 23/10/92   F.O'R.  AQR 3250                                 008
      *                    Only one rider of a paticular type may be
      *                    attached to a coverage. When adding a rider
      *                    to a coverage, if a mandatory rider already
      *                    existed it forced it to be reselected.
      *
      * 19/01/94   COS.    AQR 4656                                 009
      *                    Polisy table T3681 is included in a Life
      *                    program.
      *                    Update Source/Object computer to AS400.
      *
      * 17/03/94   D.W.    AQR 5067.                                010
      *                    For the MAY9405 release, a Coverage must
      *                    be defined on a deferred Annuity contract
      *                    in such a way that it cannot be selected
      *                    at Proposal Create/Modify time. It will
      *                    only come into force at time of 'Vesting'.
      *                    Thus, the T5679 entry for this coverage
      *                    must have MAX COVRS set to ZERO. This is
      *                    OK, since this program just thinks that the
      *                    Max number of COVRS has already been
      *                    reached and so protects the selection field
      *                    for said coverage. However, when another
      *                    coverage is selected but not by putting
      *                    an 'X' in the selection field, an error is
      *                    correctly indicated (E005) BUT the
      *                    erroneous field is then protected & the
      *                    user is thus unable to correct the error.
      *                    Some new eror indicators were added to
      *                    S5006 and also this program was amended
      *                    to ensure that if an Invalid action was
      *                    made, the field is left unprotected to
      *                    allow the user to correct it.
      *
      * 11/11/94   F.M.    AQR 5579                                 011
      *                    - When retrieving the RTABLE fields from
      *                    T5673, the last line of 6 riders were
      *                    not being displayed, and if continuation
      *                    items followed, the rest of the records
      *                    on the scroll became out of synch with
      *                    the table entries.
      *
      *                    - The problem was caused by an OR keyword
      *                    when testing if the end of the scroll
      *                    had been reached. Surely we must test
      *                    for both WSAA-SUB1 (controlling the cove-
      *                    rages allowed) AND WSAA-SUB2 (controlling
      *                    the riders) BOTH reaching there maximum.
      *                    To avoid 'subscript out of range'
      *                    errors, WSAA-SUB1 must not be
      *                    incremented once it reaches the max 8.
      *
      *                    - The word 'FULL' was not allowed by the
      *                    editor and so was re-coded.
      *
      *                    - It was also found that adding riders
      *                    to a coverage looped because WSAA-EXIT-
      *                    FLAG was not being reset once the
      *                    coverage had been loaded to the subfile
      *                    preventing the riders from being loaded
      *                    (in 1410-COVERAGE-TRANS).  Once the
      *                    riders were loaded, the SCRN-SUBFILE-MORE
      *                    had to be reset too.
      *
      * 01/03/95    M.A.   AQR 5514.                                012
      *                    Initialize COVT-Benefit flds.
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
      * 26/01/96  01/01   D96NUM       Jon Simpson                          *
      * 06/02/96  01/01   D96NUM       Rob Yates                            *
      * 26/02/96  01/01   D96NUM       Pauline Lund                         *
      *                   SUMINS                                            *
      *                   INSTPREM                                          *
      *                   SINGP                                             *
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
      *                                                                     *
      * 29/11/97    DUNC  SMART 9503 Conv for Client/Server.        <S9503>
      *                                                                     *
      * 08/10/99  01/01   V5L001       Yong Kee Jee                         *
      *           RECOMPILE
      *                                                                     *
      *****************************************************************
      *
      * ......... New Version of the Amendment History (Life Asia 1.0)
      *
      *****************************************************************
      *           AMENDMENT HISTORY
      *****************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....
      *                                                                     *
      * 17/01/07  01/01   V71L12       Wang Ge/FSG/CSC (Singapore)          *
      *           Retrofit UDW001 into LIFE/Asia.                           *
      * 03/03/04  01/01   UDW001       Lynne Dornan                         *
      *           In the 1000 section, after retreiving the Life record     *
      *           previously kept, check if it is the Main Life or the      *
      *           Joint Life. If it is the Joint Life, get the Main         *
      *           Life, which could be Life 01, or Life 02 etc,             *
      *           depending on the number of Lives allowed on the           *
      *           proposal.                                                 *
      *                                                                     *
      * 23/10/14  01/01   PHE003       Phuong Le Dev                        *
      *           Validate Number of RWP in contract                        *
      *                                                                     *
      **DD/MM/YY*************************************************************
      *
      * 17/07/96  01/01   CAS1.0       Sunil Patel
      *                   Amendments for Life Specific Coverage Rider
      *                   Selection. T5673-ZRLIFIND is checked to
      *                   display the appropriate covers/riders for the
      *                   primary and subseuqent lives.
      *
      *                   Bug fixes : Should not reset keys when
      *                               reading joint life. Actually
      *                               WSAA-LIFE-KEY is blank at that
      *                               time.
      *
      * 03/09/96    CAS1.0              Tak Liu.
      *                    Before reading for T5673 continuation
      *                    item, should make sure all the riders
      *                    of the current item have been loaded
      *                    into the subfile if selection is on life.
      *                    Otherwise the last line of riders for
      *                    the current item will always be missed.
      *
      * 25/06/15  01/01   PHE003       Phuong Le Dev                        *
      *           Use SSTRT intead of SRNCH                                 *
      *                                                                     *
      **DD/MM/YY*************************************************************
      /
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER. IBM-S38.                                        <009>
      *OBJECT-COMPUTER. IBM-S38.                                        <009>
       SOURCE-COMPUTER.                 IBM-AS400.                      <009>
       OBJECT-COMPUTER.                 IBM-AS400.                      <009>
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       01  WSAA-PROG                   PIC X(05)  VALUE 'P5006'.
       01  WSAA-VERSION                PIC X(02)  VALUE '01'.
       01  WSAA-SUB1                   PIC S9(02) VALUE 01   COMP-3.
       01  WSAA-SUB2                   PIC S9(02) VALUE 01   COMP-3.
      *****
      *    WSAA-SUB4 and WSAA-SUB5 relate to Program pointers and
      *    counts for the handling of the program stack.
      *****
       01  WSAA-SUB4                   PIC S9(02) VALUE ZERO COMP-3.
       01  WSAA-SUB5                   PIC S9(02) VALUE ZERO COMP-3.
       01  WSAA-CCOUNT                 PIC S9(02) VALUE ZERO COMP-3.
       01  WSAA-NO-SELECT              PIC X(01)  VALUE 'N'.
       01  WSAA-LIFEORCOV              PIC X(01)  VALUE 'N'.
       01  WSAA-SUB-TEST               PIC S9(02) VALUE ZERO COMP-3.    <007>
       01  WSAA-REMAIN                 PIC S9(02) VALUE ZERO COMP-3.    <007>
                                                                        <PHE003>
       01  IDX                         PIC 9(03).                       <PHE003>
       01  IDY                         PIC 9(03).                       <PHE003>
       01  IDZ                         PIC 9(03).                       <PHE003>
       01  WSAA-IS-RWP                 PIC X(01).                       <PHE003>
       01  WSAA-RWP-PROD               PIC X(01).                       <PHE003>
       01  WSAA-CHECK                  PIC X(01).                       <PHE003>
                                                                        <PHE003>
       01  WSAA-TEMP-COMP              PIC X(04).                       <PHE003>
       01  WSAA-ARR-RWPS.                                               <PHE003>
           03  WSAA-ARR-RWP            OCCURS 99 PIC X(04).             <PHE003>
                                                                        <PHE003>
       01  WSAA-COUNT-RWP              PIC 9(03).                       <PHE003>
                                                                        <PHE003>
       01  WSAA-RWPS.                                                   <PHE003>
           03  WSAA-RWPS-POL            OCCURS 99.                      <PHE003>
               05  WSAA-RWP            PIC X(04).                       <PHE003>
               05  WSAA-NO-RWP         PIC 9(03).                       <PHE003>
                                                                        <PHE003>
      *****
      *    The following are used will store the Rider number being
      *    selected.
      *****
       01  WSAA-RIDER-RIDER.
           03  WSAA-RIDER              PIC S9(02).
       01  FILLER REDEFINES WSAA-RIDER-RIDER.
           03  WSAA-RIDER-R            PIC X(02).
      *****
      *    The following are used will store the Coverage number being
      *    selected.
      *****
       01  WSAA-COVER-COVER.
           03  WSAA-COVERAGE           PIC S9(02).
       01  FILLER REDEFINES WSAA-COVER-COVER.
           03  WSAA-COVERAGE-R         PIC X(02).
      *****
      *    The following counts will store the next number for the
      *    creation of a Cover or a Rider.
      *    Numbers are allocated on a next HIGHEST number order.
      *****
       01  WSAA-NEXT-COVRIDNO.
           03  WSAA-NEXT-COVNO         PIC X(02).
           03  WSAA-NEXT-RIDNO         PIC X(02).
       01  FILLER REDEFINES WSAA-NEXT-COVRIDNO.
           03  WSAA-NEXT-COVNO-R       PIC 9(02).
           03  WSAA-NEXT-RIDNO-R       PIC 9(02).
      *****
      *    flag to determine whether a you are processing a  LIFE or a
      *    COVERAGE Cover type.
      *****
       01  WSAA-LINE-TYPE              PIC X(01).
           88  WSAA-LCOVER             VALUE '1'.
           88  WSAA-CCOVER             VALUE '2'.
      *****
      *    flag to determine if the next component is a Cover or Rider
      *    component.
      *****
       01  WSAA-COVER-FLAG             PIC X(01).
           88  WSAA-COVER              VALUE 'Y'.
      *****
      *    flag for exit of perform processing.
      *****
       01  WSAA-EXIT-FLAG              PIC X(01).
           88  WSAA-EXIT               VALUE 'Y'.
      *****
      *    flag for exit of 4000 Next selection check.
      *****
       01  WSAA-SELECT-FLAG            PIC X(01).
           88  WSAA-SELECTION          VALUE 'Y'.
      *****
      *    flag for exit of 4000 processing.
      *****
       01  WSAA-IMMEXIT-FLAG           PIC X(01).
           88  WSAA-IMM-EXIT           VALUE 'Y'.
      *****
      *    Program save area for next four programs.
      *****
       01  WSAA-PROGRAM-SAVE.
           03  WSAA-SEC-PROG           PIC X(05) OCCURS 4 TIMES.
      *****
      *    Concatination of Cover/Rider for table T5671.
      *****
       01  WSAA-CONCAT-NAME.
           03  WSAA-TRANCODE           PIC X(04).
           03  WSAA-CRTABLE            PIC X(04).
      *
      *****
      *    LIFELNB 1st Life key held for access search of Joint life.
      *****
       01  WSAA-LIFE-KEY               PIC X(64).
       01  WSAA-SKIP-COVER             PIC X(01).                       <CAS1.0>
       01  WSAA-KEY.                                                    <V71L12>
           03  WSAA-CHDRCOY            PIC X(1).                        <V71L12>
           03  WSAA-CHDRNUM            PIC X(8).                        <V71L12>
           03  WSAA-LIFE               PIC X(2).                        <V71L12>
           03  WSAA-JLIFE              PIC X(2).                        <V71L12>
      *
       01  ERRORS.
           03  E005                    PIC X(04) VALUE 'E005'.
           03  F290                    PIC X(04) VALUE 'F290'.
           03  H999                    PIC X(04) VALUE 'H999'.          <003>
           03  EV72                    PIC X(04) VALUE 'EV72'.          <PHE003>
           03  EV73                    PIC X(04) VALUE 'EV73'.          <PHE003>
      *
       01  TABLES.
           03  T5671                   PIC X(05) VALUE 'T5671'.
           03  T5673                   PIC X(05) VALUE 'T5673'.
           03  T5687                   PIC X(05) VALUE 'T5687'.
      **** 03  T3681                   PIC X(05) VALUE 'T3681'.            <009>
           03  TV071                   PIC X(05) VALUE 'TV071'.         <PHE003>
      *
       01  FORMATS.
           03  CHDRLNBREC              PIC X(10) VALUE 'CHDRLNBREC'.
           03  CLTSREC                 PIC X(10) VALUE 'CLTSREC'.
           03  COVTLNBREC              PIC X(10) VALUE 'COVTLNBREC'.
           03  COVTCOVREC              PIC X(10) VALUE 'COVTCOVREC'.
           03  COVTRIDREC              PIC X(10) VALUE 'COVTRIDREC'.
           03  DESCREC                 PIC X(10) VALUE 'DESCREC'.
           03  ITDMREC                 PIC X(10) VALUE 'ITEMREC'.       <PHE003>
           03  ITEMREC                 PIC X(10) VALUE 'ITEMREC'.
           03  LIFELNBREC              PIC X(10) VALUE 'LIFELNBREC'.
           03  COVTCOMREC              PIC X(10) VALUE 'COVTCOMREC'.    <PHE003>
      /
       01  WSAA-BATCKEY.
           COPY BATCKEY.
      /
           COPY VARCOM.
      /
           COPY OPSTATSREC.
      /
      ***  COPY SCRNPARAMS.                                             <S9503>
      /
           COPY SYSERRREC.
      /
           COPY T5673REC.
      /
           COPY T5671REC.
           COPY TV071REC.                                               <PHE003>
      /
           COPY CHDRLNBSKM.
      /
           COPY CLTSSKM.
      /
           COPY COVTCOMSKM.                                             <PHE003>
           COPY COVTLNBSKM.
      /
           COPY COVTCOVSKM.
      /
           COPY COVTRIDSKM.
      /
           COPY DESCSKM.
      /
           COPY ITDMSKM.
      *
           COPY ITEMSKM.
      /
           COPY LIFELNBSKM.
      /
      ***  COPY S5006SCR.                                               <S9503>
      /
       LINKAGE SECTION.
      * Screen copybooks are now part of the linkage.                   <S9503>
      /                                                                 <S9503>
           COPY SCRNPARAMS.                                             <S9503>
      /                                                                 <S9503>
           COPY S5006SCR.                                               <S9503>

           COPY WSSPCOMN.

           COPY WSSPLIFE.
      /
      * Statement now includes screen copybooks.                        <S9503>
       PROCEDURE DIVISION USING WSSP-COMMON-AREA WSSP-USER-AREA         <S9503>
                                               SCRN-SCREEN-PARAMS       <S9503>
                                               S5006-DATA-AREA          <S9503>
                                               S5006-SUBFILE-AREA   .   <S9503>

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
       1010-SKIP-ON-FLAG.
      *****
      *    Skip  this section if  returning from an optional selection
      *    (current stack position action flag = '*').
      *****
           MOVE 1                      TO SCRN-SUBFILE-RRN.

           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'
              GO TO 1090-EXIT.

           MOVE ' ' TO WSSP-SEC-ACTN (WSSP-PROGRAM-PTR).
           MOVE WSSP-BATCHKEY          TO WSAA-BATCKEY.
      *
       1020-INITIALISE-SUBFILE.
      *****
      *    Clear the Subfile ready for loading.
      *****
           MOVE 1                      TO WSAA-SUB1
                                          WSAA-SUB2
                                          WSAA-SUB4
                                          WSAA-SUB5.

           MOVE ZERO                   TO WSAA-LINE-TYPE.

           MOVE 'Y'                    TO SCRN-SUBFILE-MORE.

           MOVE 'N'                    TO WSAA-COVER-FLAG
                                          WSAA-EXIT-FLAG
                                          WSAA-SELECT-FLAG
                                          WSAA-NO-SELECT
                                          WSAA-IMMEXIT-FLAG.

           MOVE SPACES                 TO WSAA-PROGRAM-SAVE
                                          WSAA-CONCAT-NAME
                                          WSAA-LIFE-KEY.

           MOVE '00'                   TO WSAA-NEXT-COVNO
                                          WSAA-NEXT-RIDNO.

           MOVE 1                      TO WSAA-SUB1
                                          WSAA-SUB2.

           MOVE ZERO                   TO WSAA-CCOUNT
                                          WSAA-COVERAGE
                                          WSAA-RIDER.

           MOVE SPACES                 TO S5006-DATA-AREA
                                          S5006-SUBFILE-AREA.

           MOVE SCLR                   TO SCRN-FUNCTION.

           CALL 'S5006IO' USING SCRN-SCREEN-PARAMS
                                       S5006-DATA-AREA
                                       S5006-SUBFILE-AREA.

           IF SCRN-STATUZ              NOT = O-K
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.
      *
       1030-RETREIVE-HEADER.
      *****
      *    Read CHDRLNB (retrv) in order to obtain the contract header.
      *****
           MOVE RETRV                  TO CHDRLNB-FUNCTION.

           CALL 'CHDRLNBIO' USING CHDRLNB-PARAMS .

           IF CHDRLNB-STATUZ           NOT = O-K AND
                                       NOT = MRNF
              MOVE CHDRLNB-PARAMS      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.

      *
       1040-READ-CONTRACT-LONGDESC.
      *****
      *    Read Contract header Long description  from table T5688 for
      *    the contract type held on CHDRLND.
      *****
           MOVE SPACES                 TO DESC-DATA-AREA.
           MOVE 'IT'                   TO DESC-DESCPFX.
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.
           MOVE 'T5688'                TO DESC-DESCTABL.
           MOVE CHDRLNB-CNTTYPE        TO DESC-DESCITEM.
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

           MOVE CHDRLNB-CHDRNUM        TO S5006-CHDRNUM.
           MOVE CHDRLNB-CNTTYPE        TO S5006-CNTTYPE.
           MOVE DESC-LONGDESC          TO S5006-CTYPEDES.
      *                                                                 <V71L12>
      * Check if the Life is the Main Life                              <V71L12>
      *                                                                 <V71L12>
           PERFORM 1800-CHECK-LIFE.                                     <V71L12>
      *
       1050-RETREIVE-LIFE.
      *****
      *    Read LIFELNB for the life assured details.
      *****
           MOVE RETRV                  TO LIFELNB-FUNCTION.

           PERFORM 1100-LIFE-DETAILS.
      *
       1060-LIFE-OR-JLIFE.
      *****
      *    LIFELNB must have the key released in order to read the file
      *    again for the Joint Life details.
      *****

           MOVE RLSE                   TO LIFELNB-FUNCTION.

           CALL 'LIFELNBIO' USING LIFELNB-PARAMS.

      *****
      *    Re-Read of the Life and Client details (LIFELNB and CLTS).
      *****
      **** MOVE WSAA-LIFE-KEY          TO LIFELNB-DATA-KEY.             <CAS1.0>
           MOVE '01'                   TO LIFELNB-JLIFE.
           MOVE READR                  TO LIFELNB-FUNCTION.

           PERFORM 1100-LIFE-DETAILS.

      *****
      *    Initial Cover/Rider Table.
      *    n.b. Tables can continue indefinitly.
      *****
           MOVE SPACES                 TO ITDM-DATA-KEY.
           MOVE T5673                  TO ITDM-ITEMTABL.
           MOVE CHDRLNB-CNTTYPE        TO ITDM-ITEMITEM.                <003>
      *
       1070-COVER-RIDER-TABLE.
      *****
      *    Read Table  which is  dated by  effective date of contract.
      *****
           MOVE WSSP-COMPANY           TO ITDM-ITEMCOY.
      **   MOVE CHDRLNB-CNTTYPE        TO ITDM-ITEMITEM.                <003>
           MOVE CHDRLNB-OCCDATE        TO ITDM-ITMFRM.
           MOVE 'BEGN'                 TO ITDM-FUNCTION.

           CALL 'ITDMIO'            USING ITDM-PARAMS.
      *
      *
           IF ITDM-STATUZ              NOT = O-K
                                   AND NOT = ENDP
               MOVE ITDM-STATUZ        TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR.
      *
      *****
      *    Check for any change in key resulting from Table not found
      *****
           IF ITDM-ITEMCOY             NOT = WSSP-COMPANY
                                                                        <003>
      *    OR (ITDM-ITEMTABL           NOT = T5673                      <003>
      *                            AND NOT = T5673-GITEM)               <003>
      *    OR ITDM-ITEMITEM            NOT = CHDRLNB-CNTTYPE            <003>
           OR ITDM-ITEMTABL            NOT = T5673                      <003>
           OR (ITDM-ITEMITEM           NOT = CHDRLNB-CNTTYPE            <003>
                                   AND NOT = T5673-GITEM)               <003>
           OR ITDM-STATUZ              = ENDP
      *       MOVE F290                TO S5006-CHDRNUM-ERR             <003>
              MOVE H999                TO S5006-CHDRNUM-ERR             <003>
              MOVE ENDP                TO ITDM-STATUZ                   <003>
           ELSE
      *****
      *    Move of table to Working Storage.
      *****
              MOVE ITDM-GENAREA TO T5673-T5673-REC.
      *
       1080-RETRIEVE-CURR-COVERAGE.
      *****
      *    It is possible that  the Proposal  has already  been set up
      *    when a user requests this Screen.
      *    Read  the current  coverage transaction  records  for  that
      *    Coverage being processed.
      *****
           MOVE RETRV                  TO COVTLNB-FUNCTION.

           CALL 'COVTLNBIO' USING COVTLNB-PARAMS.

           IF COVTLNB-STATUZ           NOT = O-K
                                   AND MRNF
               MOVE COVTLNB-PARAMS TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.

           IF COVTLNB-STATUZ           = O-K
              MOVE 2                   TO WSAA-LINE-TYPE
           ELSE
              MOVE 1                   TO WSAA-LINE-TYPE.

      *****
      *    LOAD the Subfile Screen until the end of the Subfile has
      *    been reached.
      *****
           PERFORM 1200-LOAD-SUBFILE UNTIL SCRN-SUBFILE-MORE = 'N'.

      *
       1090-EXIT.
            EXIT.
      /
      *****************************************************************
      *
       1100-LIFE-DETAILS SECTION.
      *****
      *    The following will read the life file and format the client
      *    name for processing.
      *****
       1110-READ-LIFE.
      *****
      *    Read the life file for either the Life or Joint Life.
      *****
           CALL 'LIFELNBIO' USING LIFELNB-PARAMS.

           IF LIFELNB-STATUZ           NOT = O-K
                                   AND NOT = MRNF
                                   AND NOT = ENDP
              MOVE LIFELNB-PARAMS      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.

           IF LIFELNB-STATUZ           = MRNF
                                    OR = ENDP
              GO TO 1190-EXIT.

      *
       1120-RETREIVE-CLIENT-DETAILS.
      *****
      *    Read the Client details for the associated Life.
      *****
           MOVE WSSP-FSUCO             TO CLTS-CLNTCOY.
           MOVE LIFELNB-LIFCNUM        TO CLTS-CLNTNUM.
           MOVE 'CN'                   TO CLTS-CLNTPFX.
           MOVE READR                  TO CLTS-FUNCTION.

           CALL 'CLTSIO'               USING CLTS-PARAMS.

           IF CLTS-STATUZ              NOT = O-K
                                   AND NOT = MRNF
              MOVE CLTS-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.
      *
       1130-CLIENT-NAME-FORMAT.
      *****
      *    Format the Client name extracted. Special Format.
      *    SEE Copy Confname in the procedure division.
      *****
           PERFORM PLAINNAME.
      *
       1140-READ-LIFE-JLIFE.
      *****
      *    IF entering on a First Life
      *    THEN extract the Joint Life details if they exist
      *    ELSE If entering on a Joint Life
      *         THEN extract the First Life details.
      *****
           IF LIFELNB-JLIFE            = '00' OR '  '
              PERFORM 1150-LIFE-TO-SCREEN
           ELSE
              PERFORM 1160-JLIFE-TO-SCREEN.

           GO TO 1190-EXIT.
      *
       1150-LIFE-TO-SCREEN.
      *****
      *    Move Life details to screen and set up key for Joint Life
      *    Search.
      *****
           MOVE LIFELNB-LIFE           TO S5006-LIFE.
           MOVE LIFELNB-LIFCNUM        TO S5006-LIFCNUM.
           MOVE WSSP-LONGCONFNAME      TO S5006-LINSNAME.
           MOVE LIFELNB-DATA-KEY       TO WSAA-LIFE-KEY.
           MOVE SPACES                 TO S5006-JLIFE.
      *
       1160-JLIFE-TO-SCREEN.
      *****
      *    Move Joint Life details to screen.
      *****
           MOVE LIFELNB-JLIFE          TO S5006-JLIFE.
           MOVE LIFELNB-LIFCNUM        TO S5006-JLIFCNUM.
           MOVE WSSP-LONGCONFNAME      TO S5006-JLINSNAME.
      *
       1190-EXIT.
            EXIT.
      /
      *****************************************************************
      *
       1200-LOAD-SUBFILE SECTION.
      *****
      *    Creation of Screen Lines for Selection or Viewing.
      *****
       1210-SUBFILE-LOAD.
      *****
      *    Check to determine if  this program  is being entered on a
      *    LIFE.
      *           Display : ALL Coverages and Associated Riders.
      *
      *    Check to determine if  this program  is being  entered on a
      *    COVERAGE.
      *           Display : Coverage and Associated Riders ONLY.
      *****
                                                                        <011>
           MOVE 'N'                  TO WSAA-EXIT-FLAG.                 <011>
                                                                        <011>
           IF COVTLNB-STATUZ           = O-K
              MOVE 'C'                 TO WSAA-LIFEORCOV
              PERFORM 1410-COVERAGE-TRANS
           ELSE
              MOVE 'L'                 TO WSAA-LIFEORCOV
              PERFORM 1310-LIFE-TRANS.

      *****
      *    Check  to  determine  if an  additional  Continuation Table
      *    should be loaded. e.g. T5673-GITEM = 'T5673A'.
      *****
      **** IF WSAA-SUB1                > 8                              <CAS1.0>
      **** AND WSAA-SUB2               > 48                        <011><CAS1.0>
      **** OR WSAA-SUB2                > 48                   <007><011><CAS1.0>
      **** AND WSAA-SUB2               > 48                        <007><CAS1.0>
           IF (WSAA-LIFEORCOV           = 'C' AND                       <CAS1.0>
              (WSAA-SUB1 > 8 OR  WSAA-SUB2 > 48))                       <CAS1.0>
              OR                                                        <CAS1.0>
              (WSAA-LIFEORCOV          = 'L' AND                        <CAS1.0>
              (WSAA-SUB1 > 8 AND WSAA-SUB2 > 48))                       <CAS1.0>
              IF T5673-GITEM           NOT = SPACES
      *          MOVE T5673-GITEM      TO ITDM-ITEMTABL                 <003>
                 MOVE T5673-GITEM      TO ITDM-ITEMITEM                 <003>
                 MOVE 1                TO WSAA-SUB1
                                          WSAA-SUB2
                 MOVE 'Y'              TO SCRN-SUBFILE-MORE
                 PERFORM 1070-COVER-RIDER-TABLE
                 IF ITDM-STATUZ        = O-K                            <003>
                 OR ITDM-STATUZ        = 'FULL'                         <011>
      ****                             OR FULL                     <003><011>
                    GO TO 1210-SUBFILE-LOAD
                 ELSE                                                   <003>
                    MOVE 'N'           TO SCRN-SUBFILE-MORE             <003>
               ELSE
                 MOVE 'N'              TO SCRN-SUBFILE-MORE.
           GO TO 1490-EXIT.
      *
       1310-LIFE-TRANS.
      *****
      *    Entry on LIFE details: additional Continuation Table should
      *    be loaded. i.e. T5673-GITEM = 'T5673A'.
      *
      *    IF processing a Cover
      *    ELSE processing a Rider.
      *
      *    WSAA-LINE-TYPE : = 1 - Life processing a Cover.
      *                     = 2 - Coverage processing that Cover.
      *                     = 3 - Processing the Associate Riders.
      *
      *****
           IF WSAA-LCOVER
              PERFORM 1330-CHECK-COVER
              MOVE 3                   TO WSAA-LINE-TYPE
           ELSE
              PERFORM 1360-RIDER-FILE  6 TIMES
              MOVE 1                   TO WSAA-LINE-TYPE.

      *****
      *    The following check identifies the end of the subfile before
      *    the end of table T5673 (Cover/Rider) is reached.
      *****
           IF WSAA-SUB1                NOT > 8
           AND WSAA-SUB2               NOT > 48
              PERFORM 1320-END-SUBFILE.
      *
       1320-END-SUBFILE.
      *****
      *    If BOTH Cover / Rider Field within table  = spaces then set
      *    end of table flag on.
      *
      *    If it is the end of the Subfile then set the Screen flag to
      *    no more pages.
      *****
           IF T5673-CTABLE (WSAA-SUB1)  = SPACES
           AND T5673-RTABLE (WSAA-SUB2) = SPACES
              MOVE 'N'                 TO SCRN-SUBFILE-MORE
           ELSE
              MOVE 'Y'                 TO SCRN-SUBFILE-MORE.
      *
       1330-CHECK-COVER.
      *****
      *    Read Logical View for a match of Cover types.
      *    Validate Cover type.
      *    Move Cover details to Screen.
      *****
           MOVE 'N'                    TO WSAA-EXIT-FLAG
                                          WSAA-NO-SELECT.
           IF T5673-CTABLE (WSAA-SUB1) NOT = SPACES
              MOVE ZERO                TO WSAA-CCOUNT
              MOVE SPACES              TO COVTCOV-DATA-KEY
              MOVE CHDRLNB-CHDRCOY     TO COVTCOV-CHDRCOY
              MOVE CHDRLNB-CHDRNUM     TO COVTCOV-CHDRNUM
              MOVE LIFELNB-LIFE        TO COVTCOV-LIFE
              MOVE T5673-CTABLE (WSAA-SUB1)
                                       TO COVTCOV-CRTABLE
              MOVE 'BEGN'              TO COVTCOV-FUNCTION

              PERFORM 1340-CHECK-COVTCOV UNTIL WSAA-EXIT
              PERFORM 1350-COVER-VALIDATION
              IF WSAA-SKIP-COVER       = 'N'                            <CAS1.0>
              PERFORM 1600-COVER-TO-SCREEN.

           ADD 1                       TO WSAA-SUB1.
      *
       1340-CHECK-COVTCOV.
      *****
      *    Read Logical View for a match of Cover types.
      *****
           CALL 'COVTCOVIO' USING COVTCOV-PARAMS.

           IF COVTCOV-STATUZ           NOT = O-K
                                   AND NOT = ENDP
              MOVE COVTCOV-PARAMS      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.

      *****
      *    Check for change of key.
      *    If change or end of file then exit.
      *    Else check if Cover Type has reached the Max allowable.
      *****
           IF COVTCOV-CHDRNUM          NOT = CHDRLNB-CHDRNUM
           OR COVTCOV-LIFE             NOT = LIFELNB-LIFE
           OR COVTCOV-STATUZ           = ENDP
              MOVE 'Y'                 TO WSAA-EXIT-FLAG
              MOVE '00'                 TO COVTCOV-COVERAGE             <002>
           ELSE
              IF COVTCOV-CRTABLE       = T5673-CTABLE (WSAA-SUB1)
                 ADD 1                 TO WSAA-CCOUNT.

      *****
      *    Stores the highest Coverage number read.
      *    Used to assign the next Cover number in the 4000 section.
      *****
           IF COVTCOV-COVERAGE         > WSAA-NEXT-COVNO
              MOVE COVTCOV-COVERAGE    TO WSAA-NEXT-COVNO.

           MOVE NEXTR                  TO COVTCOV-FUNCTION.
      *
       1350-COVER-VALIDATION.
      *****
      *    Validate Cover:
      *       IF a match exists and  this can be  added to Place a '+'
      *          in select. (Must contain an 'X' to be processed.
      *       IF a Required Type then Protect with an 'X' as a Protect
      *          Selection.
      *       IF a Type has reached  its Maximum  Copies  then Protect
      *          the Select Space. No Selection allowed.
      *****
           IF WSAA-CCOUNT              > ZERO
              MOVE '+'                 TO S5006-SELECT.
           IF T5673-CTMAXCOV (WSAA-SUB1)
                                       = WSAA-CCOUNT
              MOVE 'Y'                 TO S5006-SELECT-OUT(PR)
                                          WSAA-NO-SELECT.
           IF T5673-CREQ (WSAA-SUB1)   = 'R'
           AND WSAA-CCOUNT             < T5673-CTMAXCOV (WSAA-SUB1)
           AND S5006-SELECT            NOT = '+'
              MOVE 'X'                 TO S5006-SELECT
              MOVE 'Y'                 TO S5006-SELECT-OUT(PR)
                                          S5006-HREQUIRED.
      *
           MOVE 'N'                    TO WSAA-SKIP-COVER.              <CAS1.0>
           IF T5673-ZRLIFIND (WSAA-SUB1) = 'P' AND                      <CAS1.0>
              LIFELNB-LIFE             NOT = '01'                       <CAS1.0>
               MOVE 'Y'                TO WSAA-SKIP-COVER               <CAS1.0>
           END-IF.                                                      <CAS1.0>
           IF T5673-ZRLIFIND (WSAA-SUB1) = 'S' AND                      <CAS1.0>
              LIFELNB-LIFE             = '01'                           <CAS1.0>
               MOVE 'Y'                TO WSAA-SKIP-COVER               <CAS1.0>
           END-IF.                                                      <CAS1.0>
      *
       1360-RIDER-FILE.
      *****
      *    Process the Table 6 times to find a RIDER.
      *    For each Cover  there is a  Possible SIX Riders  within the
      *    Table. However, If the following Cover is Blank, the Riders
      *    are being continued on to the next 6 and so on....
      *
      *    Move Rider Details to screen.
      *****
           MOVE SPACES                 TO S5006-CTABLE
                                          S5006-HREQUIRED
                                          S5006-LONGDESC
                                          S5006-RTABLE
                                          S5006-SELECT
                                          S5006-SELECT-OUT(PR).

           IF T5673-RTABLE (WSAA-SUB2) NOT = SPACES
             IF WSAA-SKIP-COVER        = 'N'                            <CAS1.0>
              PERFORM 1700-RIDER-TO-SCREEN.

           ADD 1                       TO WSAA-SUB2.
      *
       1410-COVERAGE-TRANS.
      *****
      *    IF entering on a Coverage, Find and move to Screen
      *       MOVE 3 to a flag to Process  the Riders  associated with
      *       the previous Cover.
      *****
           IF WSAA-CCOVER
              MOVE  1                  TO WSAA-SUB1                     <007>
              PERFORM 1420-FIND-COVER  UNTIL WSAA-EXIT                  <007>
      ****    PERFORM 1420-FIND-COVER VARYING WSAA-SUB1                 <007>
      ****       FROM 1 BY 1 UNTIL WSAA-EXIT                            <007>
           ELSE
      ****    PERFORM 1430-RIDER-INFO.                                  <011>
      *  Make sure we process ALL the riders and not go beyond 48       <011>
      *  otherwise WSAA-SUB-TEST in 1430-RIDER-INFO section will fail   <011>
      *  with 'subscript out of range' errors.                          <011>
                                                                        <011>
              IF WSAA-SUB2 <= 48                                        <011>
                 PERFORM 1430-RIDER-INFO                                <011>
              END-IF                                                    <011>
           END-IF.                                                      <011>
      *
       1420-FIND-COVER.
      *****
      *    Match the entry Coverage with a Table entry.
      *    Move Cover to Screen.
      *
      *    6 is added to  the Rider  subscript  in  order to  find the
      *    riders associated to the found Cover.
      *****
           MOVE COVTLNB-COVERAGE       TO WSAA-COVERAGE-R.
           MOVE ZERO                   TO WSAA-NEXT-RIDNO.
           IF WSAA-NO-SELECT           = 'Y'
              MOVE 'Y'                 TO S5006-SELECT-OUT(PR).
           IF COVTLNB-CRTABLE          = T5673-CTABLE (WSAA-SUB1)
              MOVE '+'                 TO S5006-SELECT
              MOVE 'Y'                 TO WSAA-EXIT-FLAG
                                          S5006-SELECT-OUT(PR)
              PERFORM 1600-COVER-TO-SCREEN
              MOVE 3                   TO WSAA-LINE-TYPE
           ELSE
      ****    ADD 1                    TO WSAA-SUB1                <007><011>
                                                                        <011>
              IF WSAA-SUB1 <= 8                                         <011>
                 ADD 1                 TO WSAA-SUB1                     <011>
              END-IF                                                    <011>
              ADD 6                    TO WSAA-SUB2.

      *****
      *    The cover may exist on a continuation table. Therefore the
      *    flag is set to exit to - 1210-load-subfile where the table
      *    is loaded.
      *
      *    Note: WSAA-LINE-TYPE is still at 2 for Cover search.
      *****
           IF WSAA-SUB1                > 8
              MOVE 'Y'                 TO WSAA-EXIT-FLAG.
      *
       1430-RIDER-INFO.
      *****
      *    Screen fields are initialised before check for Rider.
      *****
           MOVE SPACES                 TO S5006-CTABLE
                                          S5006-HREQUIRED
                                          S5006-LONGDESC
                                          S5006-RTABLE
                                          S5006-SELECT
                                          S5006-SELECT-OUT(PR).

      *****
      *    Read the table for The associated Rider.
      *
      *    Only 1 rider of any type can be entered.
      *    IF end of riders, set flag to process next cover.
      *
      *    Move Rider Details to screen.
      *****
      *****                                                             <007>
      *    When one line of six riders has been processed, we must      <007>
      *    check that the next line is not for another coverage         <007>
      *    before continuing.                                           <007>
      *                                                                 <007>
      *    When WSAA-SUB2 divided by 6  gives a remainder of 1 we       <007>
      *    know we are on the first rider on a new line.                <007>
      *                                                                 <007>
      *    WSAA-SUB-TEST + 1 will give us the subscript for the         <007>
      *    coverage on that line.                                       <007>
      *                                                                 <007>
      *    If WSAA-SUB-TEST is >  WSAA-SUB1 and the coverage is not     <007>
      *    = spaces then a new coverage has begun.                      <007>
      ****                                                              <007>
                                                                        <007>
           DIVIDE   WSAA-SUB2  BY  6   GIVING     WSAA-SUB-TEST         <007>
                                       REMAINDER  WSAA-REMAIN.          <007>
           ADD  1                      TO WSAA-SUB-TEST.                <007>
                                                                        <007>
           IF  WSAA-REMAIN             =  1                             <007>
           AND WSAA-SUB-TEST           >  WSAA-SUB1                     <007>
               IF  T5673-CTABLE (WSAA-SUB-TEST)  NOT  =  SPACES         <007>
                   MOVE 'N'            TO SCRN-SUBFILE-MORE             <007>
                   MOVE  2             TO WSAA-LINE-TYPE                <007>
                   GO TO  1440-RIDER-CHECK                              <007>
               END-IF                                                   <007>
           END-IF.                                                      <007>
                                                                        <007>
           IF T5673-RTABLE (WSAA-SUB2) = SPACES
              MOVE 'N'                 TO SCRN-SUBFILE-MORE
              MOVE 2                   TO WSAA-LINE-TYPE
           ELSE
              MOVE 'Y'                 TO SCRN-SUBFILE-MORE
              PERFORM 1440-RIDER-CHECK
              PERFORM 1700-RIDER-TO-SCREEN.

      *  Once we have reached the end of the riders, we must set the    <011>
      *  end of the subfile indicator to exit 1200-LOAD-SUBFILE section.<011>
      *  therefore we need not increment WSAA-SUB2.                     <011>
                                                                        <011>
           IF WSAA-SUB2 = 48                                            <011>
              MOVE 'N'          TO SCRN-SUBFILE-MORE                    <011>
           ELSE                                                         <011>
              ADD 1             TO WSAA-SUB2                            <011>
           END-IF.                                                      <011>
                                                                        <011>
      **** ADD 1                       TO WSAA-SUB2.                    <011>
      *
       1440-RIDER-CHECK.
      *****
      *    Check Rider Logical View for LIKE Riders.
      *****
           MOVE SPACES                 TO COVTRID-DATA-KEY.
           MOVE WSSP-COMPANY           TO COVTRID-CHDRCOY.
           MOVE CHDRLNB-CHDRNUM        TO COVTRID-CHDRNUM.
           MOVE LIFELNB-LIFE           TO COVTRID-LIFE.
           MOVE COVTLNB-COVERAGE       TO COVTRID-COVERAGE.
           MOVE T5673-RTABLE (WSAA-SUB2)
                                       TO COVTRID-CRTABLE.
           MOVE 'READR'                TO COVTRID-FUNCTION.

      *****
      *    Check the Rider Logical View File for LIKE Riders.
      *****
           CALL 'COVTRIDIO' USING COVTRID-PARAMS.

           IF COVTRID-STATUZ           NOT = O-K
                                   AND NOT = MRNF
              MOVE COVTRID-PARAMS      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.

      *****
      *    If Rider Exists move a '+'  to the select field and Protect
      *    it. NO Selection can be made.
      *    Only 1 rider of any type can be entered.
      *
      *    Store the highest Rider number read.
      *****
           IF COVTRID-STATUZ           = O-K
              MOVE '+'                 TO S5006-SELECT
              MOVE 'Y'                 TO S5006-SELECT-OUT(PR)
              IF COVTRID-RIDER         > WSAA-NEXT-RIDNO
                 MOVE COVTRID-RIDER    TO WSAA-NEXT-RIDNO.
      *
       1490-EXIT.
            EXIT.
      /
      *****************************************************************
      *
       1600-COVER-TO-SCREEN SECTION.
      *
       1610-COVER-TO-LINE.
      *****
      *    Reference Cover Long description  and output details to the
      *    Screen.
      *****
           MOVE SPACES                 TO DESC-DATA-AREA.
           MOVE 'IT'                   TO DESC-DESCPFX.
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.
           MOVE 'T5687'                TO DESC-DESCTABL.
           MOVE T5673-CTABLE (WSAA-SUB1)
                                       TO DESC-DESCITEM.
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.
           MOVE READR                  TO DESC-FUNCTION.

           CALL 'DESCIO' USING DESC-PARAMS.

           IF DESC-STATUZ              = MRNF
               MOVE O-K                TO DESC-STATUZ
               MOVE ALL '?'            TO DESC-LONGDESC.

           IF DESC-STATUZ              NOT = O-K
                                   AND NOT = MRNF
               MOVE DESC-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.

           MOVE T5673-CTABLE (WSAA-SUB1)
                                       TO S5006-CTABLE.
           MOVE SPACES                 TO S5006-RTABLE.                 <007>
           MOVE DESC-LONGDESC          TO S5006-LONGDESC.
      *****
      *    ADD a line to the Screen and increment Line count.
      *****
           MOVE SADD                   TO SCRN-FUNCTION.

           CALL 'S5006IO' USING SCRN-SCREEN-PARAMS
                                       S5006-DATA-AREA
                                       S5006-SUBFILE-AREA.

           IF SCRN-STATUZ              NOT = O-K
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.

      *
       1690-EXIT.
            EXIT.

      /
      *****************************************************************
      *
       1700-RIDER-TO-SCREEN SECTION.
      *
       1710-RIDER-TO-LINE.
           MOVE SPACES                 TO S5006-RTABLE.

      *****
      *    Validate Rider for being Mandatory.
      *    If  Mandatory,  Protect  and  set  Hidden  field  for  4000
      *       Processing.
      *****
           IF T5673-RREQ (WSAA-SUB2)   = 'R'
            IF S5006-SELECT        NOT = '+'                            <008>
              MOVE 'X'                 TO S5006-SELECT
              MOVE 'Y'                 TO S5006-SELECT-OUT(PR)
                                          S5006-HREQUIRED.
           MOVE SPACES                 TO S5006-CTABLE.

           MOVE T5673-RTABLE (WSAA-SUB2)
                                       TO S5006-RTABLE.


      *****
      *    Reference Rider Long description  and output details to the
      *    Screen.
      *****
           MOVE SPACES                 TO DESC-DATA-AREA.
           MOVE 'IT'                   TO DESC-DESCPFX.
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.
           MOVE 'T5687'                TO DESC-DESCTABL.
           MOVE T5673-RTABLE (WSAA-SUB2)
                                       TO DESC-DESCITEM.
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.
           MOVE READR                  TO DESC-FUNCTION.
      *
           CALL 'DESCIO' USING DESC-PARAMS.

           IF DESC-STATUZ              = MRNF
               MOVE O-K                TO DESC-STATUZ
               MOVE ALL '?'            TO DESC-LONGDESC.

           IF DESC-STATUZ              NOT = O-K
                                   AND NOT = MRNF
              MOVE DESC-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.

           MOVE DESC-LONGDESC          TO S5006-LONGDESC.

      *****
      *    ADD a line to the Screen and increment Line count.
      *****
           MOVE SADD                   TO SCRN-FUNCTION.

           CALL 'S5006IO' USING SCRN-SCREEN-PARAMS
                                       S5006-DATA-AREA
                                       S5006-SUBFILE-AREA.

           IF SCRN-STATUZ              NOT = O-K
               MOVE SCRN-STATUZ        TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR.
      *
       1790-EXIT.
            EXIT.
      ****************************************************************
      *
      /
       1800-CHECK-LIFE SECTION.                                         <V71L12>
      *************************                                         <V71L12>
      *    The following will read the life file and format the client  <V71L12>
      *    name for processing.                                         <V71L12>
      *                                                                 <V71L12>
       1810-READ-LIFE.                                                  <V71L12>
      *                                                                 <V71L12>
      *    Retreive the Life record that was kept earlier.              <V71L12>
      *    If it's the 'joint-life' life, this isn't the record         <V71L12>
      *    that is needed, the main life is needed. So read and keeps   <V71L12>
      *    it.                                                          <V71L12>
      *                                                                 <V71L12>
           MOVE RETRV                  TO LIFELNB-FUNCTION.             <V71L12>
           CALL 'LIFELNBIO' USING LIFELNB-PARAMS                        <V71L12>
                                                                        <V71L12>
           IF LIFELNB-STATUZ           NOT = O-K                        <V71L12>
                                   AND NOT = MRNF                       <V71L12>
                                   AND NOT = ENDP                       <V71L12>
              MOVE LIFELNB-PARAMS      TO SYSR-PARAMS                   <V71L12>
              PERFORM 600-FATAL-ERROR.                                  <V71L12>
                                                                        <V71L12>
           IF LIFELNB-STATUZ           = MRNF                           <V71L12>
                                    OR = ENDP                           <V71L12>
              GO TO 1890-EXIT.                                          <V71L12>
      *                                                                 <V71L12>
      *    Is the Life record the Main Life or the Joint Life?          <V71L12>
                                                                        <V71L12>
           IF LIFELNB-JLIFE            = '00'                           <V71L12>
               GO TO 1890-EXIT                                          <V71L12>
           END-IF.                                                      <V71L12>
                                                                        <V71L12>
      * If the Life record is the Joint Life, KEEPS the Main Life       <V71L12>
                                                                        <V71L12>
           MOVE LIFELNB-CHDRCOY        TO WSAA-CHDRCOY.                 <V71L12>
           MOVE LIFELNB-CHDRNUM        TO WSAA-CHDRNUM.                 <V71L12>
           MOVE LIFELNB-LIFE           TO WSAA-LIFE.                    <V71L12>
           MOVE '00'                   TO WSAA-JLIFE.                   <V71L12>
                                                                        <V71L12>
           MOVE SPACES                 TO LIFELNB-PARAMS.               <V71L12>
           MOVE WSAA-KEY               TO LIFELNB-DATA-KEY.             <V71L12>
                                                                        <V71L12>
           MOVE LIFELNBREC             TO LIFELNB-FORMAT.               <V71L12>
           MOVE READR                  TO LIFELNB-FUNCTION.             <V71L12>
                                                                        <V71L12>
           CALL 'LIFELNBIO'         USING LIFELNB-PARAMS.               <V71L12>
                                                                        <V71L12>
           IF LIFELNB-STATUZ        NOT = O-K                           <V71L12>
               MOVE LIFELNB-PARAMS     TO SYSR-PARAMS                   <V71L12>
               MOVE LIFELNB-STATUZ     TO SYSR-STATUZ                   <V71L12>
               PERFORM 600-FATAL-ERROR                                  <V71L12>
           END-IF.                                                      <V71L12>
                                                                        <V71L12>
      * Now KEEPs the record                                            <V71L12>
                                                                        <V71L12>
           MOVE LIFELNBREC             TO LIFELNB-FORMAT.               <V71L12>
           MOVE KEEPS                  TO LIFELNB-FUNCTION.             <V71L12>
                                                                        <V71L12>
           CALL 'LIFELNBIO'         USING LIFELNB-PARAMS.               <V71L12>
                                                                        <V71L12>
           IF LIFELNB-STATUZ        NOT = O-K                           <V71L12>
               MOVE LIFELNB-PARAMS     TO SYSR-PARAMS                   <V71L12>
               MOVE LIFELNB-STATUZ     TO SYSR-STATUZ                   <V71L12>
               PERFORM 600-FATAL-ERROR                                  <V71L12>
           END-IF.                                                      <V71L12>
                                                                        <V71L12>
       1890-EXIT.                                                       <V71L12>
            EXIT.                                                       <V71L12>
      /                                                                 <PHE003>
       1900-GET-WAIVE-PROD SECTION.                                     <PHE003>
      *****************************                                     <PHE003>
      *                                                                 <PHE003>
       1910-INIT.                                                       <PHE003>
      *                                                                 <PHE003>
           MOVE ZEROES                 TO IDX.                          <PHE003>
           MOVE SPACES                 TO WSAA-ARR-RWPS.                <PHE003>
                                                                        <PHE003>
           MOVE SPACES                 TO DESC-DATA-AREA.               <PHE003>
           MOVE 'IT'                   TO DESC-DESCPFX.                 <PHE003>
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.                 <PHE003>
           MOVE TV071                  TO DESC-DESCTABL.                <PHE003>
           MOVE SPACES                 TO DESC-DESCITEM.                <PHE003>
                                                                        <PHE003>
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.                <PHE003>
           MOVE BEGN                   TO DESC-FUNCTION.                <PHE003>
      *                                                                 <PHE003>
       1920-START.                                                      <PHE003>
      *                                                                 <PHE003>
           CALL 'DESCIO' USING DESC-PARAMS.                             <PHE003>
                                                                        <PHE003>
           IF DESC-STATUZ              NOT = O-K                        <PHE003>
           AND                         NOT = ENDP                       <PHE003>
              MOVE DESC-PARAMS         TO SYSR-PARAMS                   <PHE003>
              PERFORM 600-FATAL-ERROR                                   <PHE003>
           END-IF.                                                      <PHE003>
                                                                        <PHE003>
           IF  DESC-STATUZ             = ENDP                           <PHE003>
           OR  DESC-DESCPFX            NOT = 'IT'                       <PHE003>
           OR  DESC-DESCCOY            NOT = WSSP-COMPANY               <PHE003>
           OR  DESC-DESCTABL           NOT = TV071                      <PHE003>
               GO TO 1990-EXIT                                          <PHE003>
           END-IF.                                                      <PHE003>
                                                                        <PHE003>
           ADD 1                       TO IDX.                          <PHE003>
           MOVE DESC-DESCITEM          TO WSAA-ARR-RWP (IDX).           <PHE003>
      *                                                                 <PHE003>
       1980-NEXT.                                                       <PHE003>
      *                                                                 <PHE003>
           MOVE NEXTR                  TO DESC-FUNCTION.                <PHE003>
           GO TO 1920-START.                                            <PHE003>
      *                                                                 <PHE003>
       1990-EXIT.                                                       <PHE003>
            EXIT.                                                       <PHE003>
      /                                                                 <V71L12>
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
      *    Skip this section  if returning  from an optional selection  <S9503>
      *    (current stack position action flag = '*').                  <S9503>
      *****                                                             <S9503>
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'                    <S9503>
              MOVE O-K                 TO WSSP-EDTERROR                 <S9503>
              MOVE 3000                TO WSSP-SECTIONNO                <S9503>
           GO TO PRE-EXIT.                                              <S9503>
                                                                        <S9503>
           MOVE 1                      TO SCRN-SUBFILE-RRN.             <S9503>
                                                                        <S9503>
           GO TO PRE-EXIT.                                              <S9503>
      *                                                                 <S9503>
       PRE-EXIT.                                                        <S9503>
           EXIT.                                                        <S9503>
      /                                                                 <S9503>
       2000-SCREEN-EDIT SECTION.                                        <PHE003>
      **************************
      *
       2010-SCREEN-IO.
      *    CALL 'S5006IO' USING SCRN-SCREEN-PARAMS                      <S9503>
      *                                S5006-DATA-AREA                  <S9503>
      *                                S5006-SUBFILE-AREA.              <S9503>

      * Screen errors are now handled in the calling program.           <S9503>
      *    PERFORM 200-SCREEN-ERRORS.                                   <S9503>

           MOVE O-K                    TO WSSP-EDTERROR.
           PERFORM 1900-GET-WAIVE-PROD.                                 <PHE003>
      *
       2020-ROLL-UP.
      *****
      *    Check if another page has been requested.
      *****
           IF S5006-ERROR-INDICATORS NOT = SPACES
              MOVE 'Y'                 TO WSSP-EDTERROR
           ELSE
               MOVE O-K                TO WSSP-EDTERROR.

           IF SCRN-STATUZ              = ROLU
                                    OR = ROLD
              GO TO 2090-EXIT.
      *
       2050-VALIDATE-SUBFILE.
      *****
      *    Check for a Change in any of the Screen Fields.
      *****
      *    MOVE SRNCH                  TO SCRN-FUNCTION.                <PHE003>
           MOVE SSTRT                  TO SCRN-FUNCTION.                <PHE003>

           CALL 'S5006IO' USING SCRN-SCREEN-PARAMS
                                       S5006-DATA-AREA
                                       S5006-SUBFILE-AREA.

           IF SCRN-STATUZ              NOT = O-K
                                   AND NOT = ENDP
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.
                                                                        <PHE003>
           IF SCRN-STATUZ              = ENDP                           <PHE003>
              GO TO 2090-EXIT                                           <PHE003>
           END-IF.                                                      <PHE003>

      *
       2060-SELECT-CHECK.
      *****
      *    Validate  Selection being  one  of the following conditions.
      *    IF Valid then go to 4000-next-program.
      *****
           IF S5006-SELECT             = ' ' OR '+' OR 'X'
      ****    GO TO 2080-CONTINUE                               <PHE003><006>
      **** END-IF.                                              <PHE003><006>
      ****    GO TO 2090-EXIT.                                          <006>
              IF  S5006-SELECT         = 'X'                            <PHE003>
                  PERFORM 2100-VALIDATE-RULE                            <PHE003>
              ELSE                                                      <PHE003>
                  GO TO 2080-CONTINUE                                   <PHE003>
              END-IF                                                    <PHE003>
           ELSE                                                         <PHE003>
              MOVE E005                TO S5006-SELECT-ERR              <PHE003>
           END-IF.                                                      <PHE003>
      *****
      *    IF Invalid then create error condition and update Screen.
      *****
       2080-UPDATE-SCREEN.                                              <PHE003>
           IF  S5006-ERROR-SUBFILE     NOT = SPACES                     <PHE003>
               MOVE 'N'                TO S5006-SELECT-OUT(PR)          <PHE003>
               MOVE 'Y'                TO WSSP-EDTERROR                 <PHE003>
           END-IF.                                                      <PHE003>

           MOVE SUPD                   TO SCRN-FUNCTION.

           CALL 'S5006IO' USING SCRN-SCREEN-PARAMS
                                       S5006-DATA-AREA
                                       S5006-SUBFILE-AREA.

           IF SCRN-STATUZ              NOT = O-K
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.

       2080-CONTINUE.                                                   <006>
                                                                        <006>
      *****
      *    Re-check for Change in Screen fields.
      *****
      *    MOVE SRNCH                  TO SCRN-FUNCTION.                <PHE003>
           MOVE SRDN                   TO SCRN-FUNCTION.                <PHE003>

           CALL 'S5006IO' USING SCRN-SCREEN-PARAMS
                                       S5006-DATA-AREA
                                       S5006-SUBFILE-AREA.

           IF SCRN-STATUZ              NOT = O-K
                                   AND NOT = ENDP
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.

      *****
      *    Loop until a Valid change is found.
      *****
           IF SCRN-STATUZ              NOT = ENDP
              GO TO 2060-SELECT-CHECK.
      *
       2090-EXIT.
           EXIT.
      /                                                                 <PHE003>
       2100-VALIDATE-RULE SECTION.                                      <PHE003>
      ****************************                                      <PHE003>
      *                                                                 <PHE003>
       2110-START.                                                      <PHE003>
      *                                                                 <PHE003>
      *--  Check whether selected component is Rider of Waiver premium  <PHE003>
      *--  (RWP)                                                        <PHE003>
      *                                                                 <PHE003>
           PERFORM 2200-READ-TV071.                                     <PHE003>
                                                                        <PHE003>
           IF  WSAA-RWP-PROD           = 'N'                            <PHE003>
               GO TO 2190-EXIT                                          <PHE003>
           END-IF.                                                      <PHE003>
                                                                        <PHE003>
           IF  TV071-COVERC            NOT = ZEROES                     <PHE003>
           OR  TV071-CNTTOT            NOT = ZEROES                     <PHE003>
               PERFORM 2300-VALIDATE-ADD-COMP                           <PHE003>
           END-IF.                                                      <PHE003>
      *                                                                 <PHE003>
       2190-EXIT.                                                       <PHE003>
           EXIT.                                                        <PHE003>
      /                                                                 <PHE003>
       2200-READ-TV071 SECTION.                                         <PHE003>
      *************************                                         <PHE003>
      *                                                                 <PHE003>
       2210-START.                                                      <PHE003>
      *                                                                 <PHE003>
           MOVE 'N'                    TO WSAA-RWP-PROD.                <PHE003>
                                                                        <PHE003>
           MOVE SPACES                 TO ITDM-PARAMS.                  <PHE003>
           MOVE WSSP-COMPANY           TO ITDM-ITEMCOY.                 <PHE003>
           MOVE TV071                  TO ITDM-ITEMTABL.                <PHE003>
           MOVE S5006-CTABLE           TO ITDM-ITEMITEM.                <PHE003>
           MOVE CHDRLNB-OCCDATE        TO ITDM-ITMFRM.                  <PHE003>
           MOVE ITDMREC                TO ITDM-FORMAT.                  <PHE003>
           MOVE BEGN                   TO ITDM-FUNCTION.                <PHE003>
                                                                        <PHE003>
           CALL 'ITDMIO'               USING ITDM-PARAMS.               <PHE003>
                                                                        <PHE003>
           IF  ITDM-STATUZ             NOT = O-K                        <PHE003>
           AND                         NOT = ENDP                       <PHE003>
               MOVE ITDM-PARAMS        TO SYSR-PARAMS                   <PHE003>
               MOVE ITDM-STATUZ        TO SYSR-STATUZ                   <PHE003>
               PERFORM 600-FATAL-ERROR                                  <PHE003>
           END-IF.                                                      <PHE003>
                                                                        <PHE003>
           IF  ITDM-ITEMCOY            NOT = WSSP-COMPANY               <PHE003>
           OR  ITDM-ITEMTABL           NOT = TV071                      <PHE003>
           OR  ITDM-ITEMITEM           NOT = S5006-CTABLE               <PHE003>
           OR  ITDM-STATUZ             = ENDP                           <PHE003>
               GO TO 2290-EXIT                                          <PHE003>
           END-IF.                                                      <PHE003>
                                                                        <PHE003>
           MOVE 'Y'                    TO WSAA-RWP-PROD.                <PHE003>
           MOVE ITDM-GENAREA           TO TV071-TV071-REC.              <PHE003>
      *                                                                 <PHE003>
       2290-EXIT.                                                       <PHE003>
           EXIT.                                                        <PHE003>
      /                                                                 <PHE003>
       2300-VALIDATE-ADD-COMP SECTION.                                  <PHE003>
      ********************************                                  <PHE003>
      *                                                                 <PHE003>
       2310-START.                                                      <PHE003>
      *                                                                 <PHE003>
           PERFORM 2400-LOOP-COVT.                                      <PHE003>
                                                                        <PHE003>
           MOVE 'N'                    TO WSAA-CHECK.                   <PHE003>
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 99               <PHE003>
                                       OR WSAA-RWP (IDX) = SPACES       <PHE003>
               IF  S5006-CTABLE        = WSAA-RWP (IDX)                 <PHE003>
                   MOVE 'Y'            TO WSAA-CHECK                    <PHE003>
               END-IF                                                   <PHE003>
           END-PERFORM.                                                 <PHE003>
                                                                        <PHE003>
           IF  WSAA-CHECK              = 'N'                            <PHE003>
               GO TO 2390-EXIT                                          <PHE003>
           END-IF.                                                      <PHE003>
      *                                                                 <PHE003>
      *--  Validate                                                     <PHE003>
      *                                                                 <PHE003>
           ADD -1                      TO IDX.                          <PHE003>
                                                                        <PHE003>
           IF  TV071-COVERC            > 0                              <PHE003>
               IF  WSAA-NO-RWP (IDX)   >=  TV071-COVERC                 <PHE003>
                   MOVE EV72           TO  S5006-SELECT-ERR             <PHE003>
               END-IF                                                   <PHE003>
           END-IF.                                                      <PHE003>
                                                                        <PHE003>
           IF  TV071-CNTTOT            > 0                              <PHE003>
               IF  WSAA-COUNT-RWP      >=  TV071-CNTTOT                 <PHE003>
                   MOVE EV73           TO  S5006-SELECT-ERR             <PHE003>
               END-IF                                                   <PHE003>
           END-IF.                                                      <PHE003>
      *                                                                 <PHE003>
       2390-EXIT.                                                       <PHE003>
           EXIT.                                                        <PHE003>
      /                                                                 <PHE003>
       2400-LOOP-COVT SECTION.                                          <PHE003>
      ************************                                          <PHE003>
      *                                                                 <PHE003>
       2410-INIT.                                                       <PHE003>
      *                                                                 <PHE003>
           MOVE 1                      TO IDY.                          <PHE003>
           MOVE 0                      TO IDZ.                          <PHE003>
           MOVE ZEROES                 TO WSAA-COUNT-RWP.               <PHE003>
           MOVE SPACES                 TO WSAA-RWPS.                    <PHE003>
           MOVE SPACES                 TO WSAA-TEMP-COMP.               <PHE003>
                                                                        <PHE003>
           INITIALIZE                     COVTCOM-PARAMS.               <PHE003>
           MOVE WSSP-COMPANY           TO COVTCOM-CHDRCOY     .         <PHE003>
           MOVE S5006-CHDRNUM          TO COVTCOM-CHDRNUM     .         <PHE003>
           MOVE SPACES                 TO COVTCOM-CRTABLE     .         <PHE003>
           MOVE COVTCOMREC             TO COVTCOM-FORMAT      .         <PHE003>
           MOVE BEGN                   TO COVTCOM-FUNCTION    .         <PHE003>
      *                                                                 <PHE003>
       2420-START.                                                      <PHE003>
      *                                                                 <PHE003>
           CALL 'COVTCOMIO'            USING COVTCOM-PARAMS.            <PHE003>
                                                                        <PHE003>
           IF  COVTCOM-STATUZ          NOT = O-K                        <PHE003>
           AND                         NOT = ENDP                       <PHE003>
               MOVE COVTCOM-PARAMS     TO SYSR-PARAMS                   <PHE003>
               MOVE COVTCOM-STATUZ     TO SYSR-STATUZ                   <PHE003>
               PERFORM 600-FATAL-ERROR                                  <PHE003>
           END-IF.                                                      <PHE003>
                                                                        <PHE003>
           IF  COVTCOM-STATUZ          = ENDP                           <PHE003>
           OR  COVTCOM-CHDRCOY         NOT = WSSP-COMPANY               <PHE003>
           OR  COVTCOM-CHDRNUM         NOT = S5006-CHDRNUM              <PHE003>
               GO TO 2490-EXIT                                          <PHE003>
           END-IF.                                                      <PHE003>
                                                                        <PHE003>
      *                                                                 <PHE003>
      *--  Check component which is waiver product or not               <PHE003>
      *                                                                 <PHE003>
           MOVE 'N'                    TO WSAA-IS-RWP.                  <PHE003>
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 99               <PHE003>
                                       OR WSAA-ARR-RWP (IDX) = SPACES   <PHE003>
               IF  COVTCOM-CRTABLE     = WSAA-ARR-RWP(IDX)              <PHE003>
                   MOVE 'Y'            TO WSAA-IS-RWP                   <PHE003>
               END-IF                                                   <PHE003>
           END-PERFORM.                                                 <PHE003>
      *                                                                 <PHE003>
      *--  Skip, if it's not waiver product                             <PHE003>
      *                                                                 <PHE003>
           IF  WSAA-IS-RWP             = 'N'                            <PHE003>
               GO TO 2480-NEXT                                          <PHE003>
           END-IF.                                                      <PHE003>
                                                                        <PHE003>
           ADD 1                       TO WSAA-COUNT-RWP.               <PHE003>
                                                                        <PHE003>
           IF  COVTCOM-CRTABLE         NOT = WSAA-TEMP-COMP             <PHE003>
               MOVE COVTCOM-CRTABLE    TO WSAA-TEMP-COMP                <PHE003>
               ADD 1                   TO IDZ                           <PHE003>
      *                                                                 <PHE003>
      *--  Store Component Code & Number of Rider of Waiver Premium     <PHE003>
      *                                                                 <PHE003>
               MOVE COVTCOM-CRTABLE    TO WSAA-RWP    (IDZ)             <PHE003>
               MOVE 1                  TO WSAA-NO-RWP (IDZ)             <PHE003>
               ADD 1                   TO IDY                           <PHE003>
           ELSE                                                         <PHE003>
               ADD 1                   TO WSAA-NO-RWP (IDZ)             <PHE003>
           END-IF.                                                      <PHE003>
      *                                                                 <PHE003>
       2480-NEXT.                                                       <PHE003>
      *                                                                 <PHE003>
           MOVE NEXTR                  TO COVTCOM-FUNCTION.             <PHE003>
           GO TO 2420-START.                                            <PHE003>
      *                                                                 <PHE003>
       2490-EXIT.                                                       <PHE003>
           EXIT.                                                        <PHE003>
      /
      *****************************************************************

      /
      *****************************************************************
      *     UPDATE DATABASE IF REQUIRED AND LOG TRANSACTION
      *****************************************************************
      *
       3000-UPDATE SECTION.
      **********************
      *
       3010-UPDATE-DATABASE.
           GO TO 3090-EXIT.
      *****
      *    NO Update of database files.
      *****

       3090-EXIT.
            EXIT.
      /
      *****************************************************************
      *     DECIDE WHICH TRANSACTION PROGRAM IS NEXT
      *****************************************************************
      *
       4000-WHERE-NEXT SECTION.
      *************************
      *
       4010-NEXT-PROGRAM.
      *****
      *    If  not  returning  from  a  selection ACTION  =  ' '  i.e.
      *       First selection.
      *       Find  first  selection  'X'  and  determine  conditions.
      *
      *    If returning from a selection ACTION = '*' then verify that
      *       if it was mandatory, a transaction has been created?
      *****
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) NOT = '*'
              PERFORM 4100-INIT-SELECTION
           ELSE
              MOVE SPACES TO WSSP-SEC-ACTN (WSSP-PROGRAM-PTR)
              MOVE SRDN                TO SCRN-FUNCTION
              IF S5006-HREQUIRED       = 'Y'
                 PERFORM 4200-VERIFY-COMPONENT.

      *****
      *    If the  end of  the  subfile or a  mandatory type  does not
      *       have  a  transaction  record on  return.  Then exit  the
      *       program  to either  the next  program  or  the  previous
      *       component program respectively.
      *****
           IF WSAA-IMM-EXIT
              GO TO 4090-EXIT.

      *****
      *    Read and Verify the next Subfile record.
      *****
           PERFORM 4300-VERIFY-NEXT-SELECT.

      *****
      *    If the end of the subfile, exit to next initial program.
      *****
           IF WSAA-IMM-EXIT
              GO TO 4090-EXIT.

      *****
      *    Read  Program  table to  find  Cover / Rider programs to be
      *    executed.
      *****
           PERFORM 4500-COVER-RIDER-PROGS.

      *****
      *    Read the  Program  table T5671 for  the components programs
      *    to be executed next.
      *****
           PERFORM 4600-READ-PROGRAM-TABLES.
      *
       4090-EXIT.
            EXIT.
      *
      *****************************************************************
      *
       4100-INIT-SELECTION SECTION.
      *
       4110-SAVE-NEXT-PROGS.
      *****
      *    Save the  next  four  programs  after  P5006  into  Working
      *    Storage for re-instatement at the end of the Subfile.
      *****
           ADD 1, WSSP-PROGRAM-PTR     GIVING WSAA-SUB4.
           MOVE 1                      TO WSAA-SUB5.
           PERFORM 4120-LOOP1          4 TIMES.

      *****
      *    First Read of the Subfile for a Selection.
      *****
           MOVE SSTRT                  TO SCRN-FUNCTION.
      *****
      *    Go to the first  record  in  the Subfile to  find the First
      *    Selection.
      *****
           GO TO 4190-EXIT.
      *
       4120-LOOP1.
      *****
      *    This loop will load the next four  programs from WSSP Stack
      *    into a Working Storage Save area.
      *****
           MOVE WSSP-SEC-PROG (WSAA-SUB4)
                                       TO WSAA-SEC-PROG (WSAA-SUB5).
           ADD 1                       TO WSAA-SUB4
                                          WSAA-SUB5.
      *
       4190-EXIT.
            EXIT.
      /
      *****************************************************************
      *
       4200-VERIFY-COMPONENT SECTION.
      *
       4210-VERIFY-REQ-TRANS.
      *****
      *    IF Returning from an optional Selection then Verify
      *       that any Mandatory Selections were created.
      *****
           MOVE 'N'                    TO WSAA-EXIT-FLAG.
           MOVE 'N'                    TO WSAA-IMMEXIT-FLAG.
           MOVE WSSP-COMPANY           TO COVTLNB-CHDRCOY.
           MOVE CHDRLNB-CHDRNUM        TO COVTLNB-CHDRNUM.
           MOVE LIFELNB-LIFE           TO COVTLNB-LIFE.
           MOVE WSAA-COVERAGE-R        TO COVTLNB-COVERAGE.
           MOVE WSAA-RIDER-R           TO COVTLNB-RIDER.
           MOVE ZEROS                  TO COVTLNB-SEQNBR.
           MOVE BEGN                   TO COVTLNB-FUNCTION.

           PERFORM 4220-VERIFY-TRANS UNTIL WSAA-EXIT.

      *****
      *    IF the transaction has not been created then Keep the
      *       transaction key and pass an error message to the
      *       previous program, then exit.
      *****
           IF COVTLNB-STATUZ           = ENDP
              MOVE ' MANDATORY TRANSACTION - PLEASE COMPLETE.'
                                       TO WSSP-MSGAREA
              MOVE 'Y'                 TO WSAA-IMMEXIT-FLAG
              PERFORM 4600-READ-PROGRAM-TABLES.

           MOVE 'N'                    TO WSAA-EXIT-FLAG.
      *****
      *    IF the mandatory Selection was not created then exit
      *       to the previous program.
      *    ELSE if OK then find next selection from Subfile.
      ****
           GO TO 4290-EXIT.
      *
       4220-VERIFY-TRANS.
      *****
      *    Verify any Mandatory Selections for a transaction
      *    being created.
      *****
           CALL 'COVTLNBIO' USING COVTLNB-PARAMS.

           IF COVTLNB-STATUZ           NOT = O-K
                                   AND NOT = ENDP
              MOVE COVTLNB-PARAMS      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.

      *****
      *    Check for change of key.
      *    If Found or End of file then exit.
      *****
           IF COVTLNB-CHDRCOY          NOT = WSSP-COMPANY
           OR COVTLNB-CHDRNUM          NOT = CHDRLNB-CHDRNUM
           OR COVTLNB-LIFE             NOT = LIFELNB-LIFE
           OR COVTLNB-COVERAGE         NOT = WSAA-COVERAGE-R
           OR COVTLNB-RIDER            NOT = WSAA-RIDER-R
           OR COVTLNB-SEQNBR           NOT = ZERO
           OR COVTLNB-STATUZ           = ENDP
              MOVE 'Y'                 TO WSAA-EXIT-FLAG.

           IF COVTLNB-STATUZ           = O-K
              MOVE 'Y'                 TO WSAA-EXIT-FLAG.

           MOVE NEXTR                  TO COVTLNB-FUNCTION.
      *
       4290-EXIT.
            EXIT.
      /
      *****************************************************************
      *
       4300-VERIFY-NEXT-SELECT SECTION.
      *
       4310-VERIFY.
      *****
      *    Read the Subfile until a Selection has been made.
      *****
           MOVE 'N'                    TO WSAA-SELECT-FLAG
                                          WSAA-IMMEXIT-FLAG.

           PERFORM 4400-NEXT UNTIL WSAA-SELECTION
                                       OR WSAA-IMM-EXIT.
      *
       4390-EXIT.
            EXIT.
      /
      *****************************************************************
      *
       4400-NEXT SECTION.
      *
       4410-NEXT-REC.
      *****
      *    Read next subfile record sequentially.
      *****
           CALL 'S5006IO' USING SCRN-SCREEN-PARAMS
                                       S5006-DATA-AREA
                                       S5006-SUBFILE-AREA.

           IF SCRN-STATUZ              NOT = O-K
                                   AND NOT = ENDP
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.
      *
       4420-IF-SUBFILE-ENDP.
      *****
      *    Check for the end of the Subfile.
      *
      *    If end of Subfile re-load the Saved four programs to
      *       Return to initial stack order.
      *****
           IF SCRN-STATUZ              = ENDP
              PERFORM 4430-RELOAD-PROGS-WSSP
              GO TO 4490-EXIT.

      *****
      *    Flag set off on each sequential read down on a Cover.
      *    Will be set on if selected in the following Condition check.
      *****
           IF S5006-CTABLE             NOT = SPACES
              MOVE 'N'                 TO WSAA-COVER-FLAG.

      *****
      *    IF entering on a LIFE then verify Cover/Rider.
      *       n.b. If Cover not selected then riders can not be
      *       processed.
      *
      *    IF entering on a COVERAGE then the Flag will be set on
      *       as the Cover should not be selected for the Riders
      *       to be processed.
      *
      *    IF Rider = spaces then you are processing a Cover
      *    ELSE you are processing a rider.
      *****
           IF S5006-SELECT             = 'X'
              IF WSAA-LIFEORCOV        = 'L'
                 IF S5006-CTABLE       NOT = SPACES
                    MOVE 'Y'           TO WSAA-COVER-FLAG
                    MOVE 'Y'           TO WSAA-SELECT-FLAG
                 ELSE
                    IF WSAA-COVER
                       MOVE 'Y'        TO WSAA-SELECT-FLAG
                    ELSE
                       MOVE 'N'        TO WSAA-SELECT-FLAG
              ELSE
                 MOVE 'Y'              TO WSAA-COVER-FLAG
                 MOVE 'Y'              TO WSAA-SELECT-FLAG
           ELSE
              MOVE 'N'                 TO WSAA-SELECT-FLAG.

           MOVE SRDN                   TO SCRN-FUNCTION.

           GO TO 4490-EXIT.
      *
       4430-RELOAD-PROGS-WSSP.
      *****
      *    Set Flag for immediate exit as the next action is to go to
      *    the next program in the stack.
      *
      *    Set Subscripts and call Loop.
      *****
           MOVE 'Y'                    TO WSAA-IMMEXIT-FLAG.
           ADD 1, WSSP-PROGRAM-PTR     GIVING WSAA-SUB4.
           MOVE 1                      TO WSAA-SUB5.
           PERFORM 4440-LOOP3          4 TIMES.

      *****
      *    Set action flag to ' ' in order that the program will
      *    resume initial stack order and go to the next program.
      *****
           MOVE ' '              TO WSSP-SEC-ACTN (WSSP-PROGRAM-PTR).
           ADD 1                       TO WSSP-PROGRAM-PTR.
           GO TO 4490-EXIT.
      *
       4440-LOOP3.
      *****
      *    Re-load the Saved four programs from Working Storage to
      *    the WSSP Program Stack.
      *****
           MOVE WSAA-SEC-PROG (WSAA-SUB5)
                                       TO WSSP-SEC-PROG (WSAA-SUB4).
           ADD 1                    TO WSAA-SUB4
                                       WSAA-SUB5.
      *
       4490-EXIT.
            EXIT.
      /
      *****************************************************************
      *
       4500-COVER-RIDER-PROGS SECTION.
      *
       4510-INIT-COVTLNB.
      *****
      *    Initialise COVTLNB Fields for the following KEEPS.
      *****
           MOVE SPACES                 TO COVTLNB-DATA-AREA.
           MOVE 1                      TO COVTLNB-PAYRSEQNO.            <004>
           MOVE ZEROES                 TO COVTLNB-SEQNBR
                                          COVTLNB-ANBCCD (01)
                                          COVTLNB-ANBCCD (02)
                                          COVTLNB-RISK-CESS-AGE
                                          COVTLNB-PREM-CESS-AGE
                                          COVTLNB-BEN-CESS-AGE          <012>
                                          COVTLNB-RISK-CESS-TERM
                                          COVTLNB-PREM-CESS-TERM
                                          COVTLNB-BEN-CESS-TERM         <012>
                                          COVTLNB-SUMINS
                                          COVTLNB-POLINC
                                          COVTLNB-NUMAPP
                                          COVTLNB-INSTPREM              <001>
                                          COVTLNB-SINGP.
           MOVE VRCM-DATE              TO COVTLNB-TRANSACTION-DATE.
           MOVE VRCM-TIME              TO COVTLNB-TRANSACTION-TIME.
           MOVE VRCM-USER              TO COVTLNB-USER.
           MOVE VRCM-MAX-DATE          TO COVTLNB-RISK-CESS-DATE
                                          COVTLNB-RISK-CESS-DATE
                                          COVTLNB-PREM-CESS-DATE
                                          COVTLNB-BEN-CESS-DATE         <012>
                                          COVTLNB-RESERVE-UNITS-DATE
                                          COVTLNB-EFFDATE.
      *
       4520-COVER-OR-RIDER.
      *****
      *    Subfile record a COVER or a RIDER?
      *
      *    n.b. If entering  on a  COVERAGE, the Cover details  should
      *    not get this far.
      *****
           IF S5006-CTABLE             NOT = SPACES
              PERFORM 4530-COVER-NEXT-PROG.

           IF S5006-RTABLE             NOT = SPACES
              PERFORM 4540-RIDER-NEXT-PROG.

      *****
      *    Keep the key of the COVTLNB transaction file for the next
      *       program.
      *****
           PERFORM 4550-KEEPS-COVTLNB.

      *****
      *    Section Skip to next key SECTION.
      *****
           GO TO 4590-EXIT.
      *
       4530-COVER-NEXT-PROG.
      *****
      *    The following flag is set up previously to show that
      *    all conditions have been satisfied.
      *
      *    IF entering on a LIFE and Cover selected 'X' then the
      *       flag is set ON.
      *
      *    IF entering on a COVERAGE then the flag is set OFF.
      *    n.b. The cover can not be selected.
      *
      *    KEY set up to reference program table T5671
      *       n.b. this will say which programs are called for
      *       the required Cover.
      *****
           IF WSAA-COVER
              MOVE S5006-CTABLE        TO WSAA-CRTABLE
              MOVE WSKY-BATC-BATCTRCDE TO WSAA-TRANCODE.

      *****
      *    The next sequential number of the Cover is passed to
      *    the transaction key (only if flag is ON).
      *    Rider key reset = '00'.
      *    STORE the Cover key for Rider processing.
      *****
           ADD 1                       TO WSAA-NEXT-COVNO-R.
           MOVE WSAA-NEXT-COVNO-R      TO WSAA-COVERAGE
                                          COVTLNB-COVERAGE.
           MOVE ZERO                   TO WSAA-RIDER
                                          COVTLNB-RIDER.
      *
       4540-RIDER-NEXT-PROG.
      *****
      *    The following hidden field is set up previously to
      *    provide a flag showing that all conditions have been
      *    satisfied.
      *
      *    IF entering on a LIFE and Cover selected 'X' then the
      *       flag is set ON.
      *    ELSE Riders can not be selected, flag OFF.
      *
      *    IF entering on a COVERAGE and Rider selected 'X' then
      *       flag is set ON.
      *
      *    KEY set up to reference program table T5671
      *       n.b. this will say which programs are called for
      *       the required Rider.
      *****
           IF WSAA-COVER
              MOVE S5006-RTABLE        TO WSAA-CRTABLE
              MOVE WSKY-BATC-BATCTRCDE TO WSAA-TRANCODE.

      *****
      *    The next sequential number of the Rider.
      *    IF a LIFE then rider number  is sequential on the  order of
      *       selection.
      *    ELSE entry is on a COVERAGE where the  rider number assigned
      *       to the number following the highest rider on file.
      *****
           IF WSAA-LIFEORCOV           = 'L'
              MOVE WSAA-COVERAGE       TO COVTLNB-COVERAGE
              ADD 1                    TO WSAA-RIDER
              MOVE WSAA-RIDER          TO COVTLNB-RIDER
           ELSE
              MOVE WSAA-COVERAGE       TO COVTLNB-COVERAGE
              ADD 1                    TO WSAA-NEXT-RIDNO-R
              MOVE WSAA-NEXT-RIDNO     TO COVTLNB-RIDER.
      *
       4550-KEEPS-COVTLNB.
      *****
      *    KEEP the key for the transaction record COVTLNB,
      *    which will be accessed in the next program.
      *****
           MOVE WSAA-CRTABLE           TO COVTLNB-CRTABLE.
           MOVE WSSP-COMPANY           TO COVTLNB-CHDRCOY.
           MOVE CHDRLNB-CHDRNUM        TO COVTLNB-CHDRNUM.
           MOVE CHDRLNB-CNTCURR        TO COVTLNB-CNTCURR.              <005>
           MOVE LIFELNB-LIFE           TO COVTLNB-LIFE.
           MOVE KEEPS                  TO COVTLNB-FUNCTION.
           MOVE COVTLNBREC             TO COVTLNB-FORMAT.

           CALL 'COVTLNBIO' USING COVTLNB-PARAMS.

           IF COVTLNB-STATUZ           NOT = O-K
               MOVE COVTLNB-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
       4590-EXIT.
            EXIT.
      /
      *****************************************************************
      *
       4600-READ-PROGRAM-TABLES SECTION.
      *
       4610-READ-PROGRAM-TABLE.
      *****
      *    Read the  Program  table T5671 for  the components programs
      *    to be executed next.
      *****
           MOVE SPACES                 TO ITEM-DATA-KEY.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.
           MOVE T5671                  TO ITEM-ITEMTABL.
           MOVE WSAA-CONCAT-NAME       TO ITEM-ITEMITEM.
           MOVE 'READR'                TO ITEM-FUNCTION.

           CALL 'ITEMIO' USING ITEM-PARAMS.

           IF ITEM-STATUZ              NOT = O-K
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.

           MOVE ITEM-GENAREA           TO T5671-T5671-REC.
      *
       4620-LOAD-PROGS-TO-WSSP.
      *****
      *    Move the component programs to the WSSP stack.
      *****
           MOVE 1                      TO WSAA-SUB4.
           ADD 1, WSSP-PROGRAM-PTR     GIVING WSAA-SUB5.
           PERFORM 4630-LOOP2          4 TIMES.

      *****
      *    Reset the Action to '*' signifying action desired to
      *    the next program.
      *****
           MOVE '*'              TO WSSP-SEC-ACTN (WSSP-PROGRAM-PTR).
           ADD 1                       TO WSSP-PROGRAM-PTR.
           GO TO 4690-EXIT.
      *
       4630-LOOP2.
      *****
      *    This loop will load four programs from table T5671 to
      *    the WSSP stack.
      *****
           MOVE T5671-PGM (WSAA-SUB4)  TO WSSP-SEC-PROG (WSAA-SUB5).
           ADD 1                       TO WSAA-SUB4
                                          WSAA-SUB5.
      *
       4690-EXIT.
            EXIT.
      /

