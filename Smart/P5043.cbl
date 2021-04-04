      * Generation Parameters SCRVER(02)               Do Not Delete!   <S9503>
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P5043.
      *REMARKS.
      *
      *                   AGENT MAINTENANCE SUBMENU.
      *                   ==========================
      *
      *Validation
      *----------
      *
      * Key 1 - Agent agreement number (AGLF)
      *
      *      Y = mandatory, must exist on file.
      *         - also read AGNTLAG and check that the agent branch is
      *                the same as the sign-on branch
      *
      *      N = optional, if entered must not exist on file, must be
      *           within correct number range (call ALOCNO to check).
      *
      *
      *
      *Updating
      *--------
      *
      * If creating an agent  and  the  number  is  blank,  call  the
      * automatic number allocation routine (ALOCNO) to get a number.
      *
      * During validation, read  and  store the agent header. For new
      * agency agreements, initialise  all the details, and keep them
      * in the I/O module.
      *           Valid flag to 1,
      *           All numeric fields and dates to zero,
      *           Company to sign-on company,
      *           Agent number as returned from ALOCNO,
      *
      * Set up the WSSP-FLAG  ("I"  for actions C, D and E, otherwise
      * "M") in WSSP.
      *
      * Soft lock the agent agreement (call SOFTLCK). If the agent is
      * already locked, display an error message.
      *
      *****************************************************************
      *              AMENDMENT  HISTORY                               *
      *****************************************************************
      * DATE.....   BY..   AMENDMENT...............  NUMBER
      * DD/MM/YY    X.X.   xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  NNN
      *
      * 09.04.90    J.F.   Check the number returned from ALOCNO        <001>
      *                    has not already been used. If so, get        <001>
      *                    the next.                                001 <001>
      * 02.05.90    T.S.   Initialise the WSSP-CLNTKEY to prevent   002 <001>
      *                    showing incorrct client bank detail on
      *                    screen s2571.
      * 19.03.91    S.W    Changed READH on AGNTLAG to READR as     003
      *                    the section is merely checking the
      *                    agent no. exists.
      * 09.05.91    G.D    INITIALZE AGENT RECORDS FIELDS.          004
      *
      * 18.06.91    J.K.   The Agent Number field on the screen     005
      *                    allows for up to 10 characters.
      *                    The Agent Number on the file is only 5
      *                    characters long.
      *                    A problem arose if you entered an Agent
      *                    Number with more than 5 characters......
      *                    e.g. Agent Number = 10003
      *                         If an Agent Number of 10003567 is
      *                         entered, then the details for Agent
      *                         Number 10003 are displayed.
      *                         This is incorrect, an error should
      *                         be displayed.
      *
      * 27/02/92    B.L.   A full stop within an IF condition in    006
      *                    2900-VERIFY-BATCH-CONTROL section, was
      *                    preventing Batch Maintenance programs
      *                    ever being loaded. ie. F8.
      *
      * 04/05/94    TSG.   MAY9405. SANCTN conversion.              SE1
      *                    update source/object computer to AS400.
      *
      * 21/02/95    S.V.   Remarks updated.
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
      * 22/01/96  01/01   D96NUM       Rachel Cartwright                    *
      * 25/01/96  01/01   D96NUM       Rob Yates                            *
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
      * 16/07/96    CAS1.0          Dominic Dunbar.
      *                   The new date field on the AGLF Record is
      *                   initialised.
      * 29/11/97    DUNC  SMART 9503 Conv for Client/Server.        <S9503>
      *                                                                     *
      * 14/01/99  01/01   V4L016       CSC - Daniel Wong                    *
      *           Set WSSP-FLAG to 'A' for agent appointment                *
      *                                                                     *
      * 18/01/99  01/01   S01          CSC - Worachart                      *
      *           A004    To keep collateral information for each agent.
      *                                                                     *
      * 27/01/99  01/01   V4L017       CSC - Daniel Wong                    *
      *           Agent Production Inquiry                                  *
      *                                                                     *
      * 04/02/99  01/01   V4L019       CSC - Daniel Wong                    *
      *           Agent Movements                                           *
      *                                                                     *
      * 20/05/99  01/01   V4L016       Balaji . B                           *
      *           The system should not allow the selection of a
      *           terminated agent for terminate option
      *           The system should not allow the selection of an
      *           active agent for reinstate terminated agent option.
      *
      * 18/05/98  01/01   A06633       Kevin Duffy                          *
      *           It is possible to enter an entity with a spurious char    *
      *           at the end. This is truncated when accessing entity but   *
      *           SFTLOCK has entity moved from screen field. Therefore     *
      *           it locks a 'different' entity and its possible for two    *
      *           people to work on same entity. Set up SFTLOCK entity      *
      *           from field/key used on existence check.                   *
      *                                                                     *
      * 23/08/00  01/01   SDAS         User Id for SMART 0003 upgrade       *
      * 15/12/99  01/01   SDAS         Jacco Landskroon                     *
      *           Additional sanctioning for Secured Data Access.           *
      *           If the sign-on user is a restricted user, only allow      *
      *           access to agent numbers that are sanctioned to the        *
      *           user.                                                     *
      *                                                                     *
      *                                                                     *
      * 22/03/02  01/01   V63F07       Jane Kok Chiew Ping - CSC Mala       *
      *           Apply Agent Numbering at Company level.                   *
      *                                                                     *
      * 16/12/09  01/01   V75F01       Saw Hoong Ong/FSG/CSC (Malaysia)     *
      *           Secured Data Access                                       *
      *           Change to add checking of SDA sanctioning for all         *
      *           actions with agent number required to be entered          *
      *           instead of just action D - Agent Inquiry.                 *
      *                                                                     *
      **DD/MM/YY*************************************************************
      *****************************************************************
      /
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.                                      <SE1>
       OBJECT-COMPUTER. IBM-AS400.                                      <SE1>
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       01  WSAA-PROG                   PIC X(05) VALUE 'P5043'.
       01  WSAA-VERSION                PIC X(02) VALUE '02'.
       01  WSAA-ALLOW-FLAG             PIC X(01).                       <V4L017>
       01  WSAA-AGNTNUM                PIC X(08).                       <V4L017>
      *
       01  ERRORS.
           03  E070                    PIC X(04) VALUE 'E070'.
           03  E073                    PIC X(04) VALUE 'E073'.
           03  E305                    PIC X(04) VALUE 'E305'.
           03  E330                    PIC X(04) VALUE 'E330'.
           03  E455                    PIC X(04) VALUE 'E455'.
           03  G354                    PIC X(04) VALUE 'G354'.
           03  ML01                    PIC X(04) VALUE 'ML01'.          <V4L017>
           03  H912                    PIC X(04) VALUE 'H912'.          <V4L016>
      *
      *01  TABLES.
      *
       01  FORMATS.
           03  AGLFREC                 PIC X(10) VALUE 'AGLFREC'.
           03  MAGUREC                 PIC X(10) VALUE 'MAGUREC'.       <V4L017>
           03  AGLFLNBREC              PIC X(10) VALUE 'AGLFLNBREC'.    <V4L017>
      *
       01  WSAA-ALOCNO-THERE-FLAG      PIC X(01) VALUE 'N'.
           88 ALOCNO-ALREADY-THERE               VALUE 'Y'.
      *
       01  WSAA-BATCHKEY.
           COPY BATCKEY.
      /
           COPY AGLFSKM.
      /                                                                 <V4L017>
           COPY AGLFLNBSKM.                                             <V4L017>
      /                                                                 <V4L017>
           COPY MAGUSKM.                                                <V4L017>
      /
           COPY AGNTLAGSKM.
      /
           COPY VARCOM.
      /
           COPY ALOCNOREC.
      /
           COPY SFTLOCKREC.
      /
           COPY SYSERRREC.
      /
           COPY OPSTATSREC.
      /
           COPY SANCTNREC.
      /
           COPY SUBPROGREC.
      /
           COPY BCBPROGREC.
      /
           COPY BATCDORREC.
      /
      ***  COPY SCRNPARAMS.                                             <S9503>
      /
      ***  COPY S5043SCR.                                               <S9503>
      /                                                                 <SDAS>
           COPY SDASANCREC.                                             <SDAS>
      /                                                                 <SDAS>
           COPY FSUPFXCPY.                                              <SDAS>
      /
       LINKAGE SECTION.
      * Screen copybooks are now part of the linkage.                   <S9503>
      /                                                                 <S9503>
           COPY SCRNPARAMS.                                             <S9503>
      /                                                                 <S9503>
           COPY S5043SCR.                                               <S9503>

           COPY WSSPCOMN.
      /
           COPY WSSPSMART.
      /
      * Statement now includes screen copybooks.                        <S9503>
       PROCEDURE DIVISION USING WSSP-COMMON-AREA WSSP-USER-AREA         <S9503>
                                               SCRN-SCREEN-PARAMS       <S9503>
                                               S5043-DATA-AREA      .   <S9503>

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

<002>      MOVE SPACE TO WSSP-CLNTKEY.                                  <002>

           MOVE SPACES                 TO WSAA-AGNTNUM.                 <V4L017>
           MOVE SPACES                 TO S5043-DATA-AREA.
           MOVE SPACES                 TO AGLF-DATA-AREA.               <004>
           MOVE SPACES                 TO AGNTLAG-DATA-AREA.            <004>
           MOVE WSSP-SBMACTION         TO S5043-ACTION.
           MOVE WSSP-BATCHKEY          TO WSKY-BATC-KEY.
           IF WSKY-BATC-BATCACTMN      NOT = WSSP-ACCTMONTH
           OR WSKY-BATC-BATCACTYR      NOT = WSSP-ACCTYEAR
               MOVE E070               TO SCRN-ERROR-CODE.

           MOVE RLSE                   TO AGLF-FUNCTION.
           CALL 'AGLFIO' USING AGLF-PARAMS
           IF AGLF-STATUZ              NOT = O-K
              MOVE AGLF-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.                                  <V4L017>
      *
       1090-EXIT.
            EXIT.
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
      *    CALL 'S5043IO' USING SCRN-SCREEN-PARAMS                      <S9503>
      *                          S5043-DATA-AREA.                       <S9503>
      * Screen errors are now handled in the calling program.           <S9503>
      *    PERFORM 200-SCREEN-ERRORS.                                   <S9503>
           MOVE O-K                    TO WSSP-EDTERROR.

       2020-VALIDATE.
           PERFORM 2100-VALIDATE-ACTION.

           IF S5043-ACTION-ERR         = SPACES
              IF SCRN-STATUZ           NOT = 'BACH'
                  PERFORM 2200-VALIDATE-KEYS
              ELSE
                  PERFORM 2400-VERIFY-BATCH-CONTROL.

      ****                                                              <SDAS>
      **** If enquiring on an agent, ensure that a user with restricted <SDAS>
      **** data access is authorised to view this agent.                <SDAS>
      ****                                                              <SDAS>
                                                                        <SDAS>
      **** IF SCRN-ACTION               = 'D'                   <V75F01><SDAS>
      ****     PERFORM 2500-CHECK-RUSER                         <V75F01><SDAS>
      ****     IF SDAS-STATUZ       NOT = O-K                   <V75F01><SDAS>
      ****         MOVE SDAS-STATUZ    TO S5043-AGNTSEL-ERR     <V75F01><SDAS>
      ****     END-IF                                           <V75F01><SDAS>
      **** END-IF.                                              <V75F01><SDAS>
       2080-CHECK-FOR-ERRORS.
           IF S5043-ERROR-INDICATORS NOT = SPACES
              MOVE 'Y'                 TO WSSP-EDTERROR.
      *
       2090-EXIT.
            EXIT.
      /
       2100-VALIDATE-ACTION SECTION.
      ******************************
      *
       2110-CHECK-AGAINST-TABLE.
           MOVE SCRN-ACTION            TO SUBP-ACTION.
           MOVE WSSP-COMPANY           TO SUBP-COMPANY.
           MOVE WSAA-PROG              TO SUBP-PROG-CODE.
           CALL 'SUBPROG' USING SUBP-SUBPROG-REC.
           IF SUBP-STATUZ NOT = O-K
              MOVE SUBP-STATUZ         TO S5043-ACTION-ERR
              GO TO 2190-EXIT.
      *
       2120-CHECK-SANCTIONS.
           MOVE 'SUBM'                 TO SNCT-FUNCTION.
      **** MOVE WSSP-PASSWORD          TO SNCT-PASSWORD.                <SE1>
      **** MOVE WSSP-USERID            TO SNCT-PASSWORD.                <SE1>
           MOVE WSSP-USERID            TO SNCT-USERID  .                <SE1>
           MOVE WSSP-COMPANY           TO SNCT-COMPANY.
           MOVE WSSP-BRANCH            TO SNCT-BRANCH.
           MOVE SUBP-TRANSCD           TO SNCT-TRANSCD.
           CALL 'SANCTN' USING WSSP-COMMON-AREA SNCT-SANCTN-REC.
           IF SNCT-STATUZ              = BOMB
               MOVE SNCT-STATUZ        TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR.
           IF SNCT-STATUZ              NOT = O-K
               MOVE SNCT-STATUZ        TO S5043-ACTION-ERR.
      *
       2190-EXIT.
            EXIT.
      /
       2200-VALIDATE-KEYS SECTION.
      ****************************
      *
       2210-VALIDATE-KEY1.
           IF SUBP-KEY1                  = SPACES
              GO TO 2290-EXIT.

           MOVE WSSP-COMPANY           TO AGLF-AGNTCOY.
           MOVE S5043-AGNTSEL          TO AGLF-AGNTNUM.
           MOVE READR                  TO AGLF-FUNCTION.

           IF S5043-AGNTSEL            NOT = SPACES
              CALL 'AGLFIO' USING AGLF-PARAMS
              IF AGLF-STATUZ           NOT = O-K
                                   AND NOT = MRNF
                 MOVE AGLF-PARAMS      TO SYSR-PARAMS
                 PERFORM 600-FATAL-ERROR
              ELSE
                 NEXT SENTENCE
           ELSE
              MOVE MRNF                TO AGLF-STATUZ.

           IF AGLF-STATUZ              = MRNF
            AND SUBP-KEY1 = 'Y'
              MOVE E305                TO S5043-AGNTSEL-ERR
              GO TO 2290-EXIT.

           IF  SUBP-KEY1               = 'Y'                            <V75F01>
               PERFORM 2500-CHECK-RUSER                                 <V75F01>
               IF  SDAS-STATUZ         NOT = O-K                        <V75F01>
                   MOVE SDAS-STATUZ    TO S5043-AGNTSEL-ERR             <V75F01>
                   GO TO 2290-EXIT                                      <V75F01>
               END-IF                                                   <V75F01>
           END-IF.                                                      <V75F01>
      *                                                                 <V75F01>
           IF SUBP-KEY1                = 'Y'                            <V4L016>
           AND S5043-ACTION            = 'C'                            <V4L016>
           AND AGLF-DTETRM             NOT = VRCM-MAX-DATE              <V4L016>
               MOVE H912               TO S5043-AGNTSEL-ERR             <V4L016>
               GO TO 2290-EXIT                                          <V4L016>
           END-IF.                                                      <V4L016>
           IF SUBP-KEY1                = 'Y'                            <V4L016>
           AND S5043-ACTION            = 'H'                            <V4L016>
           AND AGLF-DTETRM             = VRCM-MAX-DATE                  <V4L016>
               MOVE H912               TO S5043-AGNTSEL-ERR             <V4L016>
               GO TO 2290-EXIT                                          <V4L016>
           END-IF.                                                      <V4L016>
           IF AGLF-AGNTNUM             NOT = S5043-AGNTSEL              <005>
              MOVE E305                TO S5043-AGNTSEL-ERR             <005>
              GO TO 2290-EXIT.                                          <005>

           IF AGLF-STATUZ              = O-K
            AND SUBP-KEY1 = 'N'
              MOVE E330                TO S5043-AGNTSEL-ERR.
      *****
      *    For new agents, if a number is entered,
      *       check it against the automatic number allocation range.
      *****
           IF SUBP-KEY1                = 'N'
            AND S5043-AGNTSEL          NOT = SPACES
            AND S5043-AGNTSEL-ERR      = SPACES
              PERFORM 2300-CHECK-ALOCNO.
      *****
      *    For transactions on existing agents,
      *       check the agent selected is form correct branch
      *****
           IF SUBP-KEY1                = 'Y'
            AND S5043-AGNTSEL-ERR      = SPACES
              MOVE WSSP-COMPANY        TO AGNTLAG-AGNTCOY
              MOVE S5043-AGNTSEL       TO AGNTLAG-AGNTNUM
              MOVE READR               TO AGNTLAG-FUNCTION
              CALL 'AGNTLAGIO' USING AGNTLAG-PARAMS
              IF AGNTLAG-STATUZ NOT = O-K
                 MOVE AGNTLAG-PARAMS   TO SYSR-PARAMS
                 PERFORM 600-FATAL-ERROR
              ELSE
                 IF AGNTLAG-AGNTBR NOT = WSSP-BRANCH
                    MOVE E455          TO S5043-AGNTSEL-ERR.
                                                                        <V4L017>
           MOVE 'N'                TO WSAA-ALLOW-FLAG.                  <V4L017>
           IF S5043-ACTION          = 'G'                               <V4L017>
              PERFORM M500-CHECK-ALLOW-FLAG                             <V4L017>
              IF WSAA-ALLOW-FLAG  NOT = 'Y'                             <V4L017>
                 MOVE ML01         TO S5043-AGNTSEL-ERR                 <V4L017>
              END-IF                                                    <V4L017>
           END-IF.                                                      <V4L017>
      *
       2290-EXIT.                                                       <V4L017>
            EXIT.
      *
       2300-CHECK-ALOCNO SECTION.
      ***************************
      *
       2310-CHECK.
           MOVE 'CHECK'                TO ALNO-FUNCTION.
           MOVE 'AG'                   TO ALNO-PREFIX.
           MOVE WSSP-COMPANY           TO ALNO-COMPANY.
           MOVE WSSP-BRANCH            TO ALNO-GENKEY.
           MOVE S5043-AGNTSEL          TO ALNO-ALOC-NO.

           CALL 'ALOCNO' USING ALNO-ALOCNO-REC.

      ***  IF ALNO-STATUZ              = BOMB                           <V63F07>
      ***      MOVE ALNO-STATUZ        TO SYSR-STATUZ                   <V63F07>
      ***      PERFORM 600-FATAL-ERROR.                                 <V63F07>

      ***  IF ALNO-STATUZ              NOT = O-K                        <V63F07>
      ***      MOVE ALNO-STATUZ        TO S5043-AGNTSEL-ERR.            <V63F07>
      *
           IF ALNO-STATUZ           NOT = O-K                           <V63F07>
              MOVE SPACES              TO ALNO-GENKEY                   <V63F07>
              CALL 'ALOCNO'         USING ALNO-ALOCNO-REC               <V63F07>
              IF ALNO-STATUZ            = BOMB                          <V63F07>
                 MOVE ALNO-STATUZ      TO SYSR-STATUZ                   <V63F07>
                 PERFORM 600-FATAL-ERROR                                <V63F07>
              ELSE                                                      <V63F07>
                 IF ALNO-STATUZ     NOT = O-K                           <V63F07>
                    MOVE ALNO-STATUZ   TO S5043-AGNTSEL-ERR             <V63F07>
                 END-IF                                                 <V63F07>
              END-IF                                                    <V63F07>
           END-IF.                                                      <V63F07>
                                                                        <V63F07>
       2390-EXIT.
            EXIT.
      *
      /
       2400-VERIFY-BATCH-CONTROL SECTION.
      ***********************************
      *
       2410-VALIDATE-REQUEST.
           IF SUBP-BCHRQD              NOT = 'Y'
      ***      MOVE E073               TO S5043-ACTION-ERR.             <006>
               MOVE E073               TO S5043-ACTION-ERR              <006>
               GO TO 2490-EXIT.
      *
       2420-RETRIEVE-BATCH-PROGS.
           MOVE SUBP-TRANSCD           TO BCBP-TRANSCD.
           MOVE WSSP-COMPANY           TO BCBP-COMPANY.
           CALL 'BCBPROG' USING BCBP-BCBPROG-REC.
           IF BCBP-STATUZ              NOT = O-K
               MOVE BCBP-STATUZ        TO S5043-ACTION-ERR
               GO TO 2490-EXIT.

           MOVE BCBP-NXTPROG1          TO WSSP-NEXT1PROG.
           MOVE BCBP-NXTPROG2          TO WSSP-NEXT2PROG.
           MOVE BCBP-NXTPROG3          TO WSSP-NEXT3PROG.
           MOVE BCBP-NXTPROG4          TO WSSP-NEXT4PROG.

       2490-EXIT.
            EXIT.
      /                                                                 <SDAS>
       2500-CHECK-RUSER SECTION.                                        <SDAS>
      **************************                                        <SDAS>
       2510-BEGIN.                                                      <SDAS>
                                                                        <SDAS>
           INITIALIZE SDAS-SANC-REC.                                    <SDAS>
                                                                        <SDAS>
           MOVE 'VENTY'                TO SDAS-FUNCTION.                <SDAS>
           MOVE O-K                    TO SDAS-STATUZ.                  <SDAS>
                                                                        <SDAS>
           MOVE WSSP-USERID            TO SDAS-USERID.                  <SDAS>
           MOVE PRFX-AGNT              TO SDAS-ENTYPFX.                 <SDAS>
           MOVE WSSP-COMPANY           TO SDAS-ENTYCOY.                 <SDAS>
           MOVE S5043-AGNTSEL          TO SDAS-ENTYNUM.                 <SDAS>
                                                                        <SDAS>
           CALL 'SDASANC'           USING SDAS-SANC-REC.                <SDAS>
                                                                        <SDAS>
           IF SDAS-STATUZ               = BOMB                          <SDAS>
               MOVE SDAS-STATUZ        TO SYSR-STATUZ                   <SDAS>
               MOVE SDAS-SANC-REC      TO SYSR-PARAMS                   <SDAS>
               PERFORM 600-FATAL-ERROR                                  <SDAS>
           END-IF.                                                      <SDAS>
                                                                        <SDAS>
       2590-EXIT.                                                       <SDAS>
            EXIT.                                                       <SDAS>
      /                                                                 <V4L017>
       M500-CHECK-ALLOW-FLAG SECTION.                                   <V4L017>
      *******************************                                   <V4L017>
                                                                        <V4L017>
       M500-START.                                                      <V4L017>
                                                                        <V4L017>
            MOVE WSSP-COMPANY          TO MAGU-COMPANY.                 <V4L017>
            MOVE WSSP-USERID           TO MAGU-USERID.                  <V4L017>
            MOVE MAGUREC               TO MAGU-FORMAT.                  <V4L017>
            MOVE READR                 TO MAGU-FUNCTION.                <V4L017>
            CALL 'MAGUIO'       USING     MAGU-PARAMS.                  <V4L017>
            IF MAGU-STATUZ          NOT = O-K AND MRNF                  <V4L017>
               MOVE MAGU-STATUZ        TO SYSR-STATUZ                   <V4L017>
               MOVE MAGU-PARAMS        TO SYSR-PARAMS                   <V4L017>
               PERFORM 600-FATAL-ERROR                                  <V4L017>
            END-IF.                                                     <V4L017>
                                                                        <V4L017>
            IF MAGU-STATUZ          NOT = O-K                           <V4L017>
               MOVE 'Y'                TO WSAA-ALLOW-FLAG               <V4L017>
               GO TO M500-EXIT                                          <V4L017>
            END-IF.                                                     <V4L017>
                                                                        <V4L017>
            IF MAGU-AGNTNUM01       NOT = SPACES                        <V4L017>
               MOVE MAGU-AGNTNUM01     TO WSAA-AGNTNUM                  <V4L017>
               IF MAGU-AGNTNUM01        = S5043-AGNTSEL                 <V4L017>
                  MOVE 'Y'             TO WSAA-ALLOW-FLAG               <V4L017>
               ELSE                                                     <V4L017>
                  PERFORM M600-CHECK-AGENT                              <V4L017>
               END-IF                                                   <V4L017>
            END-IF.                                                     <V4L017>
                                                                        <V4L017>
            IF MAGU-AGNTNUM02       NOT = SPACES                        <V4L017>
             AND WSAA-ALLOW-FLAG    NOT = 'Y'                           <V4L017>
               MOVE MAGU-AGNTNUM02     TO WSAA-AGNTNUM                  <V4L017>
               IF MAGU-AGNTNUM02        = S5043-AGNTSEL                 <V4L017>
                  MOVE 'Y'             TO WSAA-ALLOW-FLAG               <V4L017>
               ELSE                                                     <V4L017>
                  PERFORM M600-CHECK-AGENT                              <V4L017>
               END-IF                                                   <V4L017>
            END-IF.                                                     <V4L017>
                                                                        <V4L017>
            IF MAGU-AGNTNUM03       NOT = SPACES                        <V4L017>
             AND WSAA-ALLOW-FLAG    NOT = 'Y'                           <V4L017>
               MOVE MAGU-AGNTNUM03     TO WSAA-AGNTNUM                  <V4L017>
               IF MAGU-AGNTNUM03        = S5043-AGNTSEL                 <V4L017>
                  MOVE 'Y'             TO WSAA-ALLOW-FLAG               <V4L017>
               ELSE                                                     <V4L017>
                  PERFORM M600-CHECK-AGENT                              <V4L017>
               END-IF                                                   <V4L017>
             END-IF.                                                    <V4L017>
                                                                        <V4L017>
       M500-EXIT.                                                       <V4L017>
           EXIT.                                                        <V4L017>
                                                                        <V4L017>
       M600-CHECK-AGENT SECTION.                                        <V4L017>
      **************************                                        <V4L017>
                                                                        <V4L017>
       M600-START.                                                      <V4L017>
                                                                        <V4L017>
           MOVE WSSP-COMPANY              TO AGLFLNB-AGNTCOY.           <V4L017>
           MOVE S5043-AGNTSEL             TO AGLFLNB-AGNTNUM.           <V4L017>
           MOVE READR                     TO AGLFLNB-FUNCTION.          <V4L017>
           CALL 'AGLFLNBIO'        USING AGLFLNB-PARAMS.                <V4L017>
           IF AGLFLNB-STATUZ           NOT = O-K AND MRNF               <V4L017>
              MOVE AGLFLNB-STATUZ         TO SYSR-STATUZ                <V4L017>
              MOVE AGLFLNB-PARAMS         TO SYSR-PARAMS                <V4L017>
              PERFORM 600-FATAL-ERROR                                   <V4L017>
           END-IF.                                                      <V4L017>
                                                                        <V4L017>
           PERFORM M700-CHECK-REPORTAG                                  <V4L017>
                 UNTIL AGLFLNB-REPORTAG = SPACES
                    OR WSAA-ALLOW-FLAG = 'Y'.                           <V4L017>
                                                                        <V4L017>
       M600-EXIT.                                                       <V4L017>
           EXIT.                                                        <V4L017>
                                                                        <V4L017>
       M700-CHECK-REPORTAG SECTION.                                     <V4L017>
      *****************************                                     <V4L017>
                                                                        <V4L017>
       M700-START.                                                      <V4L017>
                                                                        <V4L017>
           MOVE WSSP-COMPANY              TO AGLFLNB-AGNTCOY.           <V4L017>
           MOVE AGLFLNB-REPORTAG          TO AGLFLNB-AGNTNUM.
           MOVE READR                     TO AGLFLNB-FUNCTION.          <V4L017>
           CALL 'AGLFLNBIO'        USING     AGLFLNB-PARAMS.            <V4L017>
           IF AGLFLNB-STATUZ           NOT = O-K AND MRNF               <V4L017>
              MOVE AGLFLNB-STATUZ         TO SYSR-STATUZ                <V4L017>
              MOVE AGLFLNB-PARAMS         TO SYSR-PARAMS                <V4L017>
              PERFORM 600-FATAL-ERROR                                   <V4L017>
           END-IF.                                                      <V4L017>
                                                                        <V4L017>
           IF AGLFLNB-AGNTNUM              = WSAA-AGNTNUM               <V4L017>
              MOVE 'Y'                    TO WSAA-ALLOW-FLAG            <V4L017>
           END-IF.                                                      <V4L017>
                                                                        <V4L017>
       M700-EXIT.                                                       <V4L017>
           EXIT.                                                        <V4L017>
      /
      *****************************************************************
      *     UPDATE DATABASE IF REQUIRED AND LOG TRANSACTION
      *****************************************************************
      *
       3000-UPDATE SECTION.
      **********************
      *
       3010-UPDATE-WSSP.
           MOVE SCRN-ACTION            TO WSSP-SBMACTION.
           MOVE WSSP-BATCHKEY          TO WSAA-BATCHKEY.
           MOVE SUBP-TRANSCD           TO WSKY-BATC-BATCTRCDE.
           MOVE WSAA-BATCHKEY          TO WSSP-BATCHKEY.
           MOVE WSAA-PROG              TO WSSP-SUBMENU.
           IF SCRN-STATUZ               = 'BACH'
              GO TO 3090-EXIT.

           MOVE SUBP-NXT1PROG          TO WSSP-SEC-PROG (1).
           MOVE SUBP-NXT2PROG          TO WSSP-SEC-PROG (2).
           MOVE SUBP-NXT3PROG          TO WSSP-SEC-PROG (3).
           MOVE SUBP-NXT4PROG          TO WSSP-SEC-PROG (4).
      *
      *  Update WSSP Key details
      *
      *    IF SCRN-ACTION = 'C' OR 'D' OR 'E'                           <V4L016>
      *       MOVE 'I'                 TO WSSP-FLAG                     <V4L016>
      *    ELSE                                                         <V4L016>
      *       MOVE 'M'                 TO WSSP-FLAG.                    <V4L016>
      *                                                                 <V4L016>
      *  Set the WSSP-FLAG to 'A' if the SCRN-ACTION is A               <V4L016>
      *                                                                 <V4L016>
      ***  IF SCRN-ACTION = 'C' OR 'D' OR 'E'                   <V4L017><V4L016>
!!!!!!     IF SCRN-ACTION = 'D' OR 'E' OR 'F' OR 'G'                    <V4L019>
              MOVE 'I'                 TO WSSP-FLAG                     <V4L016>
           ELSE                                                         <V4L016>
              IF SCRN-ACTION = 'A'                                      <V4L016>
                 MOVE 'A'              TO WSSP-FLAG                     <V4L016>
              ELSE                                                      <V4L016>
                 IF SCRN-ACTION        = 'C'                            <V4L016>
                    MOVE 'T'           TO WSSP-FLAG                     <V4L016>
                 ELSE                                                   <V4L016>
                   IF SCRN-ACTION      = 'H'                            <V4L016>
                      MOVE 'R'         TO WSSP-FLAG                     <V4L016>
                   ELSE                                                 <V4L016>
                      MOVE 'M'              TO WSSP-FLAG                <V4L016>
                   END-IF                                               <V4L016>
                 END-IF                                                 <V4L016>
              END-IF                                                    <V4L016>
           END-IF.                                                      <V4L016>
      *
      *    Allocate new contract number if not entered.
      *
           IF S5043-AGNTSEL            = SPACES
            AND SUBP-KEY1              = 'N'
              PERFORM 3200-ALLOCATE-NUMBER.

           IF S5043-ERROR-INDICATORS   NOT = SPACES
              GO TO 3080-BATCHING.
      *
      *    Soft lock contract.
      *
           IF WSSP-FLAG = 'I'
              GO TO 3020-PASS-AGENT-DETAILS.

           MOVE 'LOCK'                 TO SFTL-FUNCTION.
           MOVE WSSP-COMPANY           TO SFTL-COMPANY.
           MOVE 'AG'                   TO SFTL-ENTTYP.
           IF S5043-AGNTSEL            NOT = SPACES
      ****    MOVE S5043-AGNTSEL       TO SFTL-ENTITY                   <A06633>
              MOVE AGLF-AGNTNUM        TO SFTL-ENTITY                   <A06633>
           ELSE
              MOVE ALNO-ALOC-NO        TO SFTL-ENTITY.
           MOVE SUBP-TRANSCD           TO SFTL-TRANSACTION.
           MOVE VRCM-USER              TO SFTL-USER.

           CALL 'SFTLOCK' USING SFTL-SFTLOCK-REC.
           IF SFTL-STATUZ              NOT = O-K
                                   AND NOT = 'LOCK'
              MOVE SFTL-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.

           IF SFTL-STATUZ              = 'LOCK'
              MOVE G354                TO S5043-AGNTSEL-ERR.

       3020-PASS-AGENT-DETAILS.
      *****
      *    For new Agents, initialise the header
      *****
           IF SUBP-KEY1                = 'N'
              PERFORM 3300-INITIALISE-HEADER.
      *****
      *    Store the Agent header for use by the transaction programs
      *****
           MOVE 'KEEPS'                TO AGLF-FUNCTION.
           MOVE AGLFREC                TO AGLF-FORMAT.

           CALL 'AGLFIO' USING AGLF-PARAMS.

           IF AGLF-STATUZ              NOT = O-K
              MOVE AGLF-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.
      *
       3080-BATCHING.
           IF SUBP-BCHRQD              = 'Y'
           AND S5043-ERROR-INDICATORS  = SPACES
              PERFORM 3100-UPDATE-BATCH-CONTROL.

           IF S5043-ERROR-INDICATORS NOT = SPACES
              MOVE 'Y'                 TO WSSP-EDTERROR.
      *
       3090-EXIT.
            EXIT.

      /
       3100-UPDATE-BATCH-CONTROL SECTION.
      ***********************************
      *
       3110-AUTOMATIC-BATCHING.
           MOVE 'AUTO'                 TO BATD-FUNCTION.
           MOVE WSSP-TRANID            TO BATD-TRANID.
           MOVE WSSP-BATCHKEY          TO BATD-BATCHKEY.
           CALL 'BATCDOR' USING BATD-BATCDOR-REC.
           IF BATD-STATUZ              NOT = O-K
               MOVE BATD-STATUZ        TO S5043-ACTION-ERR.

           MOVE BATD-BATCHKEY          TO WSSP-BATCHKEY.
      *
       3190-EXIT.
            EXIT.
      /
       3200-ALLOCATE-NUMBER SECTION.
      ******************************
       3210-CALL-ALOCNO.
      *                                                                 <001>
           MOVE 'N'                   TO WSAA-ALOCNO-THERE-FLAG.        <001>
      *
           MOVE 'NEXT'                TO ALNO-FUNCTION.
           MOVE 'AG'                  TO ALNO-PREFIX.
           MOVE WSSP-COMPANY          TO ALNO-COMPANY.
           MOVE WSSP-BRANCH           TO ALNO-GENKEY.
      *
           CALL 'ALOCNO' USING ALNO-ALOCNO-REC.
      *
           IF ALNO-STATUZ         NOT = O-K                             <V63F07>
              MOVE SPACES            TO ALNO-GENKEY                     <V63F07>
              CALL 'ALOCNO'       USING ALNO-ALOCNO-REC                 <V63F07>
           END-IF.                                                      <V63F07>
                                                                        <V63F07>
           IF ALNO-STATUZ             = BOMB
               MOVE ALNO-STATUZ       TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR.
      *
           IF ALNO-STATUZ             NOT = O-K
               MOVE ALNO-STATUZ       TO S5043-AGNTSEL-ERR
           ELSE
               PERFORM 3230-CHECK-ALOCNO                                <001>
               IF ALOCNO-ALREADY-THERE                                  <001>
                  GO TO 3210-CALL-ALOCNO                                <001>
               ELSE                                                     <001>
                  MOVE ALNO-ALOC-NO   TO AGLF-AGNTNUM                   <A06633>
                  MOVE ALNO-ALOC-NO   TO S5043-AGNTSEL.                 <001>
      *
       3290-EXIT.
           EXIT.
      *
       3230-CHECK-ALOCNO SECTION.                                       <001>
      ******************************                                    <001>
       3231-CHECK-ALOCNO.                                               <001>
      *                                                                 <001>
           MOVE SPACES                 TO AGNTLAG-DATA-KEY.             <001>
           MOVE WSSP-COMPANY           TO AGNTLAG-AGNTCOY.              <001>
           MOVE ALNO-ALOC-NO           TO AGNTLAG-AGNTNUM.              <001>
   ******* MOVE READH                  TO AGNTLAG-FUNCTION.        <001><003>
           MOVE READR                  TO AGNTLAG-FUNCTION.             <003>
      *                                                                 <001>
           CALL 'AGNTLAGIO'        USING AGNTLAG-PARAMS.                <001>
           IF AGNTLAG-STATUZ NOT = O-K AND MRNF                         <001>
              MOVE AGNTLAG-PARAMS      TO SYSR-PARAMS                   <001>
              PERFORM 600-FATAL-ERROR.                                  <001>
                                                                        <001>
           IF AGNTLAG-STATUZ = O-K                                      <001>
              MOVE 'Y'                 TO WSAA-ALOCNO-THERE-FLAG.       <001>
      *                                                                 <001>
       3239-EXIT.                                                       <001>
           EXIT.                                                        <001>
      *                                                                 <001>
       3300-INITIALISE-HEADER SECTION.
      ********************************
       3310-SETUP.
           MOVE SPACES                 TO AGLF-DATA-KEY.
           MOVE WSSP-COMPANY           TO AGLF-AGNTCOY.
           IF S5043-AGNTSEL            NOT = SPACES
              MOVE S5043-AGNTSEL       TO AGLF-AGNTNUM
           ELSE
              MOVE ALNO-ALOC-NO        TO AGLF-AGNTNUM.
           MOVE ZERO                   TO AGLF-DTEPAY,
                                          AGLF-FIXPRC,
                                          AGLF-SPRPRC,
                                          AGLF-INTCRD,
                                          AGLF-MINSTA,
                                          AGLF-OVCPC,
                                          AGLF-TCOLPRCT                 <S01>
                                          AGLF-TCOLMAX                  <S01>
                                          AGLF-TAXALW.
           MOVE VRCM-MAX-DATE          TO AGLF-DTEAPP,
                                          AGLF-DTETRM,
                                          AGLF-EFFDATE,                 <CAS1.0>
                                          AGLF-DTEEXP.
           MOVE VRCM-DATE              TO AGLF-TRANSACTION-DATE.
           MOVE VRCM-TIME              TO AGLF-TRANSACTION-TIME.
           MOVE VRCM-USER              TO AGLF-USER.
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
           IF SCRN-STATUZ              NOT = 'BACH'
              MOVE 1                   TO WSSP-PROGRAM-PTR
           ELSE
              MOVE 0                   TO WSSP-PROGRAM-PTR
              MOVE WSSP-NEXT1PROG      TO WSSP-NEXTPROG.
      *
       4090-EXIT.
            EXIT.
