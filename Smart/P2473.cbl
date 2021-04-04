      * Generation Parameters SCRVER(02)               Do Not Delete!   <S9503>
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                                         P2473.
      *REMARKS.
      *
      * This is  the  window  scroll  inquiry  program  for both the
      * personal and corporate client.
      *
      * This is a copy of P2464, with modifications to allow only
      * select and filter options.
      *
      * A client on the screen may be processed by entering the
      * following selection codes against the required client:
      *                   1 - Select
      *
      * The following function keys are also available:
      *        CF10 (SWCH) - Display screen filter
      *
      *
      * The window sub types to be used when accessing this window are:
      * M - client update routines are not available
      *     defaults to N name type
      * N - client large name returned
      * C - client plain name returned
      * P - client payee name returned
      * A - client addressee name returned
      * ' ' - no name returned
      *
      ******************Enhancements for Life Asia 1.0****************
      *
      * Two new enhancements have been introduced in this module.
      * These are :
      *
      *        (i) Searching on ID
      *       (ii) Display Alias Master Client
      *
      * (i) Searching on ID
      * -------------------
      *
      *  In Asia, an ID number is frequently used and in most
      *  cases is compulsory when applying for a life insurance
      *  policy. This module now allows the user to search on an ID
      *  beginning with the ID entered
      *
      *  The following new fields have been introduced
      *  to the sreen. ZRSECNO has been introduced to replace
      *  the address field CLTADDR01. This field will be used to
      *  display the client's ID number. SECUITYNO is used
      *  as an additional search key to SURNAME. To enable the client
      *  details to fit onto the screen ZRCLTD repaces CLNTNUM.
      *  ZRCLTD displays a number of client details. Some of the
      *  working storage fields which contain the client details
      *  have been truncated so that when they are all combined
      *  together they are able to fit into ZRCLTD.
      *
      *  The logical CLNTSSN is now used here to search on the
      *  security number which is to be used to hold the ID number.
      *
      *  The following changes have been made :
      *
      *  (a) In the 2000 section if S2473-SECUITYNO is not spaces
      *      (signifying that an ID has been entered), then search
      *      on ID using the logical CLNTSSN. If no ID has been
      *      entered then search on the SURNAME as per the original
      *      processing.
      *
      *  (b) If the ID Number is entered, then set up the logical
      *      CLNTSSN and do a BEGN read. Read sequentially on this
      *      logical and setting up the subfile lines until the
      *      screen has been filled (SCRN-STATUZ > 12) or ENDP
      *      has been reached.
      *
      *  (b) If the ROLU key has been pressed, then continue
      *      reading sequentially on the CLNTSSN and setting
      *      up the subfile lines until the screen has been filled
      *      (SCRN-STATUZ > 12) or ENDP has been reached.(This is
      *      assuming that a value is still present in
      *      S2473-SECUITYNO).
      *
      *
      *  This processing is similar to the search processing on the
      *  Surname field. To ensure that the ID number does not get
      *  lost it is saved in the linkage field WSSP-CHDR-TYPEDESC
      *  (PIC x(30)) into WSAA-ID when necessary (PIC X(24)).
      *
      *
      * (ii) Display Alias Master Client
      * --------------------------------
      *
      * This facility gives the option to enter a '7' in the select
      * field to display a 'pop up' window showing the master client
      * of the alias. The other change is to be able to identify an
      * alias by displaying a '*' in the field S2473-ALFLAG.The
      * new logical has been introduced to access the client who is
      * the master of the alias selected.
      *
      * The changes are as follows :
      *
      *
      *  (a) In the 8000 section set up CLRRFOR and BEGN on this
      *      logical. What we are looking for is a client role
      *      of 'AL' for the client that has been selected. If a
      *      valid one is found i.e. CLRRFOR-USED-TO-BE is spaces
      *      then a '*' is displayed in the S2473-ALFLAG.
      *
      *  (b) In the 2000 section allow for the user to select '7'.
      *
      *  (c) In the 4000 section allow for a check on '7' so that
      *      the appropriate code can be sent to T1690 to call the
      *      master client display module.
      *
      *
      *****************************************************************
      *              AMENDMENT  HISTORY                               *
      *****************************************************************
      * DATE.....   BY..   AMENDMENT...............  NUMBER
      *
      * DD/MM/YY    X.X.   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  NNN
      * 11/04/89    A.C.   VERSION 3 - Including European requirements
      *
      * 22/03/95    M.A.   VERSION 4 - Release 9503.
      *                    Comments 001 to 020 & SE1 removed from
      *                    amendment history.
      *
      * 03/03/94    GPP    AQR 4972.                                021
      *                    Replace LARGENMCPY,PAYENMCPY,PLAINMCPY
      *                    SPCOUTREC with new subroutine NAMADRS.
      *                    When loading client names on subfile,
      *                    use CLTS dataset because CLTA, CLTB, CLTN
      *                    CLTN returns names in UPPER CASE.
      *                    Use CLRR-AGNT for Fac RI. RACC no
      *                    longer used.
      *                    Incorrect GO TO & --> CLRR scan was
      *                    skipped.
      *                  - If linked account:-
      *                    a) make it appear with indicator D/L &
      *                    b) select=1 not allowed.
      *
      * 22/09/94    M.A    AQR 4972.                                022
      *                    Use CLRR-AGNT for FAC RI.
      *                    RACC No. no longer used.
      *
      * 10/01/95    �.E.   AQR 5626.                                023
      *                    When using the Locate-function, the role
      *                    disappeared on the screen. So, in 2150-
      *                    move the role to the screen field.
      *
      * 10/11/94    M.A.   AQR 5514.                                024
      *                    Address display format update.
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
      * 05/10/95  01/01   D509CS       Sarah Davies                         *
      *           Code standardisation for Client/Server conversion.        *
      *                                                                     *
      * 30/10/95  01/01   A06277       Rob Yates
      *           Previously, the T2241 table item to use in address
      *           formatting was selected based on Sign on Language
      *           plus Client Country code, with the latter taken from
      *           the first Client read.
      *           Now, the address of each Client selected for the
      *           search will be validated to display the address line
      *           specified on T2241 for the combination of country code
      *           and language of that client. To avoid unnecessary
      *           table reads, T2241 is only read on change of Client
      *           Country Code.  There is no need to read T2241
      *           immediately on entering the program.
      *
      * 28/11/95  01/01   A05833       Andrew Wall
      *
      *           If no clients are found on the first read of the
      *           file, do not display the subfile.  This usually
      *           happens because the starting position specifed in
      *           the search criteria is after the last record on the
      *           file.
      *****************************************************************
      *
      * ......... New Version of the Amendment History (Life Asia 1.0)
      *
      *****************************************************************
      *           AMENDMENT HISTORY
      *****************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....
      *                                                                     *
      *
      * 05/07/96  01/01   CAS1.0       Sunil Patel
      *           Enhancement to be able to search on an ID number.
      *           The field SECUITYNO has now been introduced on S2473.
      *           ID numbers replace the field that used to show the
      *           first line of the address.
      *
      *           Previously it was not possible to identify the
      *           master client of an alias. This enhancement
      *           identifies an alias by displaying a '*' in the
      *           ALFLAG. If a '7' is entered in the select field
      *           a window is displayed showing details of the
      *           master client.
      *
      *           If the exact match is not selected, no
      *           need to check exact match of names.
      *           Prior to this amendment, it takes ages to
      *           loop through the whole client file to
      *           look for matched names.
      *
      * 30/08/96  01/01   CAS1.0       Cat Chiu
      *           Alias client are notated with an 'A' rather than a '*'.
      *
      *  2/09/96  01/01   CAS1.0       Cat Chiu
      *           Option '7' for alias master pop-up is removed, since
      *           the purpose can be served by option '5' on the alias
      *           client.
      *
      * 12/12/96  01/01   D9607        Release User July, 96                *
      *           Expand WSAA-SUBFILE-RRN to 9(5)   - SAME AS D9607C        *
      * 15/05/96  01/01   A06461       Steve Thomas                         *
      *           Changes required for inadequacies of client scroll        *
      *           and several aqrs under this tag.                          *
      *           Initialise WSSP-DOB01-F with VRCM-MAX-DATE.               *
      *                                                                     *
      * 20/05/96  01/01   A05094       Steve Thomas                         *
      *           Change relating to Client Roles and invalid data in       *
      *           the foreign company.                                      *
      *                                                                     *
      * 07/10/96  01/01   D9607C       Andrew Wall                          *
      *           Application changes due to SCRNPARAMS change.             *
      *
      * 16/01/97  01/01   GRP2.0       Ariane Durr                          *
      *           When WSSP-WINDOW-TYPE = 'A' and WSSP-WINDOW-SUB-TYPE      *
      *           = 'X', role is "groupe agent": 'GA'                       *
      *                                                                     *
      * 28/01/97  01/01   GRP2.0       Ronald Macarulay                     *
      *           RECOMPILE ONLY                                       ??   *
      *                                                                     *
      * 12/02/97  01/01   FUPLET       Dominic Dunbar                       *
      *           Client Role of "Doctor" Added.                       ??   *
      *                                                                     *
      * 15/10/97  01/01   AY2K         Chuan Heok of Continuum Singap       *
      *           Code standardisation for client/server conversion.        *
      *                                                                     *
      * 28/11/97    DUNC  SMART 9503 Conv for Client/Server.        <S9503>
      *                                                                     *
      * 08/01/98  01/01   PSE30        Lew Kheep Seng                       *
      *           RECOMPILE.                                                *
      *                                                                     *
      * 12/01/98  01/01   FPS30        Tan Yoke Wah                         *
      *           Due to client server enablement, SCRNPARAMS is       ??   *
      *           in linkage section. Checking of SCRN-STATUZ '????'   ??   *
      *           is to be changed to another variable WSAA-IND        ??   *
      *                                                                     *
      * 14/01/98  01/01   GP3IND       Teddy Halim                          *
      *     BASE  Change WSSP-SURNAME-F to capital if S2473-EXACT not       *
      *           equal to 'Y' so that the search can be used for both      *
      *           capital and small letters.                                *
      *   IWPERF  Incorporate the following new logical files: CLOW,        *
      *           CLPE and CLPY into this program so that the scroll        *
      *           will be faster for the Owner, Payee and Payor roles       *
      *           respectively.                                             *
      *                                                                     *
      * 16/01/98  01/01   FPS30        Lew Kheep Seng                       *
      *           RECOMPILE.                                                *
      *                                                                     *
      * 30/04/98  01/01   FSU307       TYW                                  *
      *           Since Surname and Given Name have been expanded,          *
      *           display Corporate Client Name based on expanded           *
      *           surname                                                   *
      *                                                                     *
      * 14/05/98  01/01   FSU320       Ariane Durr                          *
      *           When search on ID number, the screen show the last        *
      *           page of the subfile.                                      *
      *                                                                     *
      * 05/06/98  01/01   V4F002       Dan Ridon - CSC                      *
      *           OAC Customisation on CLIENT WINDOWING                     *
      *           o S2473 must show the Address of the Client.              *
      *            - Remove the Sex and Date of Birth.                      *
      *            - Reduce the field size of the Client Name from 38 to 20.*
      *            - Reduce the field size of ID Number from 24 to 20. The  *
      *              impact of this change is questionable since this field *
      *              is used as a search key in the screen , but OAC        *
      *              management is aware and they have confirm their        *
      *              requirements is less than 20 characters only.          *
      *            - Due to the limited space available, a truncated version*
      *              (18 characters only) of the 1st address line will be   *
      *              displayed.                                             *
      *                                                                     *
      * 01/03/99  01/01   V4G002       SIEW FOONG                           *
      *           When WSAA-ACTION = 'B' and WSAA-PGMNAM = 'PR937'          *
      *           , role is "groupe member: 'LF'                            *
      *                                                                     *
      * 02/03/99  01/01   FSA552       Lew                                  *
      *           Include the following logical files (CLGA & GLGC)         *
      *           into this program to speed up the scroll process on       *
      *           Group Agent and Group Claims respectively.                *
      *                                                                     *
      * 21/04/99  01/01   V4G4.1       SIEW FOONG                           *
      *           Include Group Claim client role.                          *
      *                                                                     *
      * 08/10/99  01/01   V5L001       Josephine Chiam                      *
      *           Recompile                                            ??   *
      *                                                                     *
      * 21/10/99  01/01   V5L001       Josephine Chiam                      *
      *           Chinese character is lower value than spaces, thus   ??   *
      *           when has to move low-values instead of spaces.       ??   *
      *                                                                     *
      * 02/12/99  01/01   FSA899       James Low                            *
      *           Fixes:
      *         a)If given name of client is blank (PERSONAL CLIENT),  ??   *
      *           the given name from previous client is displayed.    ??   *
      *                                                                     *
      *         b)If 'Locate' is entered for a client whose given name ??   *
      *           is blank, upon pressing ENTER key a second time,     ??   *
      *           the given name will become non-blank.                ??   *
      *
      * 14/12/99  01/01                Ch'ng Chin Tat (Retrofit A06048)
      * 12/11/98  01/01   A06048       Dylan Jenkins                        *
      *           When called from Client Bank Account Maintenance, in      *
      *           Modify or Inquiry, windowing on client number currently   *
      *           shows all clients. This is wrong, the window should       *
      *           show only the clients with bank details.                  *
      *                                                                     *
      * 27/12/99  01/01   V43L007      Josephine Chiam                      *
      *           Remove the comma if Given Name is blank.             ??   *
      *                                                                     *
      * 09/02/00  01/01   FSA1003      Tan Yoke Wah                         *
      *           Remove the check of DBCS when calling UPPER routine -??   *
      *           UPPER routine has been changed to convert lower case ??   *
      *           characters (a to z) to upper case characters (A to Z)??   *
      *
      * 19/06/00  01/01   MLS001       Boboy                                *
      *           Get the ID text from TR386
      * 23/08/00  01/01   SDAS         User Id for SMART 0003 upgrade       *
      * 03/02/00  01/01   SDAS         Graham Judd                          *
      *                                                                     *
      *           S2473 is now a WINDOW screen to resolve a number of       *
      *           overlay problems when used within Secured Data Access     *
      *           Entity maintenance.  A Hide Window call has been          *
      *           added to run before exiting the screen.                   *
      *           To ensure that the program uses the company passed        *
      *           to it as an additional field within Secured Data          *
      *           Access when reading client and client role records,       *
      *           read T3568 in the sign-on company using the program       *
      *           number in the key.  If the item exists, obtain the        *
      *           company from the additional fields and use this in        *
      *           place of the sign-on company in linkage, which, in        *
      *           the case of SDA will be 0, the SMART company.             *
      *           Hard-coding of subfile size in subfile load section       *
      *           has been replaced by screen copybook value.               *
      *                                                                     *
      * 21/09/00  01/01   FA1158       Yoke Wah                             *
      *           If TR386 for item xP2473 not setup, system should not??   *
      *           abend. Instead, move spaces to ID text field         ??   *
      *
      * 15/11/00  01/01   MLS002       Chu Kok Kuan
      *           To read table TR386 using generic item key instead
      *           of using combination of language and program ID.
      *
      * 13/12/2000 01/01  FA1226       Cynthia Lim
      *           Modify to the TR386REC changes.
      *                                                                     *
      * 24/06/01  01/01   V6F107       Ana Edy Poerwati                     *
      *           Enable the corporate client to be searched by Licence     *
      *           Number.                                                   *
      *                                                                     *
      * 07/12/01  01/01   V62F05       Kok Chiew Ping - CSC Malaysia        *
      *           Take care of agent role 'AG' instead of 'GA'.             *
      *                                                                     *
      * 19/09/02  01/01   V64F04       Lim Ming Jern                        *
      *           Add a new field CLTSTAT (Client Status) and change the    *
      *           field ZNAMEADR to NAMEADR.
      *                                                                     *
      * 19/07/04  01/01   FA3396       Nancie lin - FSG Taiwan              *
      *           reactivate T2241-IND for address line display        ??   *
      *                                                                     *
      * 02/11/04  01/01   MLS006       Lian Chui Tin                        *
      *           Recompile due to screen change.                           *
      *                                                                     *
      * 22/08/06  01/01   FA3967       Chu Kok Kuan - CSC Malaysia          *
      * R9-056M   To show client indicator when subfile is                  *
      *           filtered by ID No.                                        *
      *                                                                     *
      * 14/03/11  01/01   V76F13       Nath Mathmaluwe (FSG Singapore       *
      *           Add process for new Credit Card role.                     *
      *                                                                     *
      **DD/MM/YY*************************************************************
      *
      *****************************************************************
      /
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.                               IBM-AS400.
       OBJECT-COMPUTER.                               IBM-AS400.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WSAA-PROG                   PIC X(05) VALUE 'P2473'.
       01  WSAA-VERSION                PIC X(02) VALUE '04'.
       01  WSAA-STORE-CLNTNUM          PIC X(08).
       01  WSAA-SRHLIMIT               PIC S9(07) VALUE 0.
       01  WSAA-PROGRAM-PTR            PIC S9(3) COMP-3.
       01  WSAA-MATCH                  PIC X(01) VALUE 'Y'.
       01  WSAA-FILE-STATUZ            PIC X(04).
       01  WSAA-SELECTION-MADE         PIC X(01) VALUE 'N'.
      *01  WSAA-SUBFILE-RRN            PIC S9(3) COMP-3.                <D9607>
       01  WSAA-SUBFILE-RRN            PIC S9(5) COMP-3.                <D9607>
      *
       01  WSAA-SEARCH                 PIC X(04).
      *01  WSAA-NUMBER                 PIC S9(03).                      <D9607>
       01  WSAA-NUMBER                 PIC S9(05).                      <D9607>
       01  WSAA-REM                    PIC S9(03).
       01  WSAA-ROLE                   PIC X(02).
       01  WSAA-ROLE-DESC              PIC X(10).                       <CAS1.0>
       01  WSAA-CLIENT-NUM             PIC X(8).                        <CAS1.0>
      *
       01  WSAA-LARGE-NAME             PIC X(05)  VALUE 'LGNMS'.        <021>
       01  WSAA-PAYEE-GIVN             PIC X(05)  VALUE 'PYNMN'.        <021>
       01  WSAA-PLAIN-NAME             PIC X(05)  VALUE 'PLNAM'.        <021>
      *
       01  WSAA-LENGTH                 PIC 9(02).
       01  WSAA-COMPANY                PIC X(01).                       <D9607>
       01  WSAA-ALT-DBCS               PIC X(01).                       <V5L001>
      *
       01  WSAA-CONVERT                PIC X(99) VALUE SPACES .         <D9607>
       01  REC-FOUND                   PIC X(01).
       01  WSAA-CONSUR01               PIC X(30) VALUE SPACES .         <D9607>
       01  WSAA-CONGIV01               PIC X(20) VALUE SPACES .         <D9607>
       01  WSAA-CONMID01               PIC X(20) VALUE SPACES .         <D9607>
       01  WSAA-CONMID02               PIC X(20) VALUE SPACES .         <D9607>
      *
       77  WSAA-X                      PIC S9(01) COMP-3.
       77  WSAA-Y                      PIC S9(01) COMP-3.
       77  WSAA-SEARCH-NAME            PIC X(30).
      *
      *    Storage for next programs to be called.
      *
       01  WSAA-SAVE-NEXTPROGS.
           03  WSAA-SAVE-PROG     PIC X(05) OCCURS 8 TIMES.
      *
      *    area for unstringing entered name
      *
       01  WSAA-CHARACTERS.
           03 WSAA-STRING.
              05  WSAA-SURNAME         PIC X(30).
              05  WSAA-GIVNAME         PIC X(20).
           03 WSAA-CHARS               REDEFINES WSAA-STRING.
              05  WSAA-CHAR            PIC X
                                       OCCURS 50
                                       INDEXED BY WA.
      *
      *    area for unstringing name from client file
      *
       01  WSBB-CHARACTERS.
           03 WSBB-STRING.
              05  WSBB-SURNAME         PIC X(30).
              05  WSBB-GIVNAME         PIC X(20).
           03 WSBB-CHARS               REDEFINES WSBB-STRING.
              05  WSBB-CHAR            PIC X
                                       OCCURS 50
                                       INDEXED BY WB.
      *
      *    area for formatting client name for screen
      *
       01  WSAA-PERS-DTL.
   ****    03 WSAA-PERS-NAME           PIC X(29).                       <CAS1.0>
           03 WSAA-PERS-NAME           PIC X(25).
      **** 03 FILLER                   PIC X  VALUE SPACE.              <V4F002>
      **** 03 WSAA-PERS-CLTSEX         PIC X(2).                        <V4F002>
      **** 03 WSAA-PERS-CLTDOBX        PIC X(10).                       <V4F002>
      *
       01  WSAA-CORP-DTL.
   ****    03 WSAA-CORP-NAMEA          PIC X(30).                       <CAS1.0>
     **    03 WSAA-CORP-NAMEA          PIC X(26).                       <FSU307>
     **    03 WSAA-CORP-NAMEB          PIC X(12).                       <FSU307>
           03 WSAA-CORP-NAMEA          PIC X(38).                       <FSU307>
      *
       01  WSAA-RECORD-ADDED           PIC X.
       01  WSAA-ID                     PIC X(24).                       <CAS1.0>
       01  WSAA-IND                    PIC X(01).                       <FPS30>
      *
       01  WSAA-CNT-LNG.                                                <024>
           03  WSAA-LNG                PIC X(01).                       <024>
           03  WSAA-CNT                PIC X(03).                       <024>

      *
      *    store data from the different client schemas
      *
       01  WSCC-CLTN-FIELDS.
           03  WSCC-SURNAME                    PIC X(0030).
           03  WSCC-GIVNAME                    PIC X(0020).
           03  WSCC-CLNTNUM                    PIC X(0008).
           03  WSCC-MIDDL01                    PIC X(0020).
           03  WSCC-MIDDL02                    PIC X(0020).
           03  WSCC-CLTTYPE                    PIC X(0001).
           03  WSCC-CLTSEX                     PIC X(0001).
           03  WSCC-SERVBRH                    PIC X(0002).
           03  WSCC-CLTDOB                     PIC S9(08) COMP-3.
           03  WSCC-CLTSTAT                    PIC X(0002).
           03  WSCC-CLTADDR01                  PIC X(0030).
           03  WSCC-CLTADDR02                  PIC X(0030).             <024>
           03  WSCC-CLTADDR03                  PIC X(0030).             <024>
           03  WSCC-CLTADDR04                  PIC X(0030).             <024>
           03  WSCC-CLTADDR05                  PIC X(0030).             <024>
           03  WSCC-CLTIND                     PIC X(0001).
           03  WSCC-SECUITYNO                  PIC X(0024).             <CAS1.0>
           03  WSCC-ROLEFLAGS.
               05  WSCC-ROLEFLAG               PIC X(01) OCCURS 35.
           03  WSCC-LSURNAME                   PIC X(0060).             <FSU307>

       01  WSCC-SUB                            PIC 9(02) COMP-3.
       01  WSCC-SUB1                           PIC 9(02) COMP-3.
       01  WSCC-SUB2                           PIC 9(02) COMP-3.
       01  WSAA-REDEFINES.                                              <V4G002>
           03  WSAA-ACTION             PIC X(1).                        <V4G002>
           03  WSAA-PGMNAM             PIC X(5).                        <V4G002>
           03  FILLER                  PIC X(194).                      <V4G002>
      *
       01  WSAA-BATCKEY.
           COPY BATCKEY.
      *
       01  WSAA-CTRYCODE                       PIC X(03).               <A06277>
      *                                                                 <A06277>
       01  WSAA-CLTNKEY.
           COPY CLTNKEY.
      *
       01  WSAA-CLTSKEY.
           COPY CLTSKEY.
      *
       01  WSSP-REDEFINES.                                              <A06048>
           03  WSAA-WIN-ACTN           PIC X.                           <A06048>
               88  ACTION-IS-CREATE             VALUE 'A'.              <A06048>
               88  ACTION-IS-MODIFY             VALUE 'B'.              <A06048>
               88  ACTION-IS-INQUIRE            VALUE 'C'.              <A06048>
           03  FILLER                  PIC X(199).                      <A06048>
      *                                                                 <A06048>
       01  WSAA-RUN-FLAG               PIC X.                           <A06048>
           88  FIRST-TIME-THRU                  VALUE 'Y'.              <A06048>
      *
       01  WSAA-SDA                    PIC X(01).                       <SDAS>
                                                                        <SDAS>
       01  WSAA-SDA-ADDITIONAL-FIELDS.                                  <SDAS>
           03 WSAA-SDA-PGMNAM          PIC X(05).                       <SDAS>
           03 WSAA-SDA-ENTPFX          PIC X(02).                       <SDAS>
           03 WSAA-SDA-COMPANY         PIC X(01).                       <SDAS>
                                                                        <SDAS>
      *01  WSAA-TR386-KEY.                                      <MLS002><MLS001>
      **** 03  WSAA-TR386-LANG         PIC X(01).               <MLS002><MLS001>
      **** 03  WSAA-TR386-PGM          PIC X(05).               <MLS002><MLS001>
      **** 03  WSAA-TR386-ID           PIC X(03).               <MLS002><MLS001>
       01  WSAA-TR386-FSUCO-KEY.                                        <MLS002>
           03  WSAA-TR386-FSUCO-LANG   PIC X(01).                       <MLS002>
           03  WSAA-TR386-FSUCO-ID     PIC X(06) VALUE '***NID'.        <MLS002>
      *
       01  ERRORS.
           03  E005                    PIC X(04) VALUE 'E005'.
           03  E040                    PIC X(04) VALUE 'E040'.
           03  F577                    PIC X(04) VALUE 'F577'.
           03  F612                    PIC X(04) VALUE 'F612'.
           03  G844                    PIC X(04) VALUE 'G844'.
           03  G601                    PIC X(04) VALUE 'G601'.
      *
       01  FORMATS.
           03  CLTSREC                 PIC X(10) VALUE 'CLTSREC'.
           03  CLTNREC                 PIC X(10) VALUE 'CLTNREC'.
           03  CLTAREC                 PIC X(10) VALUE 'CLTAREC'.
           03  CLTBREC                 PIC X(10) VALUE 'CLTBREC'.
           03  CLOWREC                 PIC X(10) VALUE 'CLOWREC'.       <GP3IND>
           03  CLPEREC                 PIC X(10) VALUE 'CLPEREC'.       <GP3IND>
           03  CLPYREC                 PIC X(10) VALUE 'CLPYREC'.       <GP3IND>
!!!!!!**** 03  CLGAREC                 PIC X(10) VALUE 'CLGAREC'.<FSA552<V62F05>
           03  CLGCREC                 PIC X(10) VALUE 'CLGCREC'.       <FSA552>
           03  CLRRWINREC              PIC X(10) VALUE 'CLRRWINREC'.
           03  ITEMREC                 PIC X(10) VALUE 'ITEMREC'.       <024>
           03  CLNTSSNREC              PIC X(10) VALUE 'CLNTSSNREC'.    <CAS1.0>
           03  CLRRFORREC              PIC X(10) VALUE 'CLRRFORREC'.    <CAS1.0>
           03  CLCCREC                 PIC X(10) VALUE 'CLCCREC'.       <V76F13>

       01  TABLES.
           03  T3639                   PIC X(05) VALUE 'T3639'.
           03  T2241                   PIC X(05) VALUE 'T2241'.         <024>
           03  T1680                   PIC X(05) VALUE 'T1680'.         <V5L001>
           03  TR386                   PIC X(05) VALUE 'TR386'.         <MLS001>
           03  T3568                   PIC X(05) VALUE 'T3568'.         <SDAS>
      /
           COPY CLRRWINSKM.
      /
           COPY CLTRELNREC.
      /
           COPY UPPERREC.                                               <GP3IND>
      /                                                                 <GP3IND>
           COPY GENSSWREC.
      /
           COPY VARCOM.
      /
           COPY FSUPFXCPY.
      *
********** COPY SPCOUTREC.                                              <021>
      *
********** COPY CLTSNAMCPY.                                             <021>
      *
           COPY CLNTRLSREC.
      *
********** COPY LARGENMREC.                                             <021>
      *                                                                 <021>
           COPY NAMADRSREC.                                             <021>
      *                                                                 <021>
           COPY NAMADRSCPY.                                             <021>
      *                                                                 <021>
           COPY SYSERRREC.
      *
      ***  COPY SCRNPARAMS.                                             <S9503>
      /
      ***  COPY S2473SCR.                                               <S9503>
      /
           COPY CLTSSKM.
      /
           COPY CLTNSKM.
      /
           COPY CLTASKM.
      /
           COPY CLTBSKM.
      /                                                                 <GP3IND>
           COPY CLOWSKM.                                                <GP3IND>
      /                                                                 <GP3IND>
           COPY CLPESKM.                                                <GP3IND>
      /                                                                 <GP3IND>
           COPY CLPYSKM.                                                <GP3IND>
      /
      **** COPY CLGASKM.                                        <V62F05><FSA552>
      /                                                                 <FSA552>
           COPY CLGCSKM.                                                <FSA552>
      /                                                                 <A1R552>
           COPY DESCSKM.
      /
           COPY ITEMSKM.                                                <024>
      /                                                                 <024>
           COPY OPSTATSREC.
      /
           COPY DATCON1REC.
      /                                                                 <024>
           COPY T2241REC.                                               <024>
      /
           COPY T1680REC.                                               <V5L001>
      /
           COPY TR386REC.                                               <MLS001>
      /
           COPY CLTSRLSCPY.
      /                                                                 <CAS1.0>
           COPY CLNTSSNSKM.                                             <CAS1.0>
      /                                                                 <CAS1.0>
           COPY CLRRFORSKM.                                             <CAS1.0>
      /                                                                 <CAS1.0>
           COPY WINDTYP.
      /                                                                 <V76F13>
           COPY CLCCSKM.                                                <V76F13>
      *
      /
       LINKAGE SECTION.
      * Screen copybooks are now part of the linkage.                   <S9503>
      /                                                                 <S9503>
           COPY SCRNPARAMS.                                             <S9503>
      /                                                                 <S9503>
           COPY S2473SCR.                                               <S9503>
      *
           COPY WSSPCOMN.
      *
           COPY WSSPWINDOW.
      /
      * Statement now includes screen copybooks.                        <S9503>
       PROCEDURE DIVISION USING WSSP-COMMON-AREA WSSP-USER-AREA         <S9503>
                                               SCRN-SCREEN-PARAMS       <S9503>
                                               S2473-DATA-AREA          <S9503>
                                               S2473-SUBFILE-AREA   .   <S9503>
      *
      *                                                                 <S9503>
      * MAINF has been replaced by MAING as the screen                  <S9503>
      * or driver now calls the program.                                <S9503>
      *                                                                 <S9503>
           COPY MAING.                                                  <S9503>
      /
*********  COPY PAYENMCPY.                                              <021>
      /
*********  COPY PLAINMCPY.                                              <021>
      /
*********  COPY LARGENMCPY.                                             <021>
      /
      *****************************************************************
      *      INITIALISE FIELDS FOR SHOWING ON SCREEN
      *****************************************************************
      *
       1000-INITIALISE SECTION.
      *************************
      *
       1001-INITIALISE.
      *
      *    If servicing selected items,
      *       skip this section.
      *     (No screen load required during iterative processing)
      *
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'
              GO TO 1900-EXIT.

           PERFORM 1200-READ-T1680.                                     <V5L001>
      *
      * INTIALISE WORKING STORAGE FIELDS.
      *
           IF WSSP-SRHLIMIT NOT NUMERIC                                 <D9607>
              MOVE 9999999             TO WSSP-SRHLIMIT                 <D9607>
           END-IF.                                                      <D9607>
                                                                        <D9607>
           MOVE SPACES                 TO WSCC-CLTN-FIELDS.
           MOVE ZERO                   TO WSCC-CLTDOB.
           MOVE SPACES                 TO WSAA-STORE-CLNTNUM.
           MOVE SPACES                 TO WSAA-CLIENT-NUM               <CAS1.0>
           MOVE SPACES                 TO WSSP-CHDR-TYPEDESC.           <CAS1.0>
           MOVE SPACES                 TO WSAA-ID.                      <CAS1.0>
           MOVE ADDITIONAL-FIELDS      TO WSAA-REDEFINES.               <V4G002>
      *
           MOVE SPACES                 TO NMAD-NAMADRS-REC.             <021>
      *                                                                 <021>
           MOVE SPACES                 TO WSAA-CTRYCODE.                <A06277>
                                                                        <SDAS>
           MOVE 'N'                    TO WSAA-SDA.                     <SDAS>
      *                                                                 <024>
      * Read Address validation table for Enquiry Address line.         <024>
      *                                                                 <024>
      ****                                                              <A06277>
      **** There is no point in reading the address validation table    <A06277>
      **** before any clients have been loaded since the table must     <A06277>
      **** be read each time the client country changes.                <A06277>
      ****                                                              <A06277>
      **** PERFORM 1300-READ-ADDR-VAL-TABLE.                    <A06277><024>
                                                                        <A06048>
           MOVE ADDITIONAL-FIELDS      OF WSSP-USER-AREA                <A06048>
                                       TO WSSP-REDEFINES.               <A06048>
                                                                        <A06048>
      * Find role flag number.
      *
      *    Set up role in WSAA-ROLE depending on window type
      *
           MOVE SPACES                 TO WSAA-ROLE.
      **** MOVE WSSP-WINDOW-TYPE       TO WIND-WINDTYPE.                <A06048>
                                                                        <A06048>
           IF  WSSP-LASTPROG           =  WSAA-PROG                     <A06048>
               MOVE 'N'                TO WSAA-RUN-FLAG                 <A06048>
           ELSE                                                         <A06048>
               MOVE 'Y'                TO WSAA-RUN-FLAG                 <A06048>
           END-IF.                                                      <A06048>
                                                                        <A06048>
           IF  FIRST-TIME-THRU                                          <A06048>
             AND ACTION-IS-CREATE                                       <A06048>
               MOVE WIND-TYPE-CLIENT   TO WIND-WINDTYPE                 <A06048>
           ELSE                                                         <A06048>
               MOVE WSSP-WINDOW-TYPE   TO WIND-WINDTYPE                 <A06048>
           END-IF.                                                      <A06048>
                                                                        <A06048>
           IF  WIND-ALIAS
               MOVE CLRF-ALIAS         TO WSAA-ROLE
           ELSE
           IF  WIND-ADDRESS
               MOVE CLRF-ADDRESS       TO WSAA-ROLE
           ELSE
           IF  WIND-AGENT
      ****     IF WSSP-WINDOW-SUB-TYPE = 'X'                            <V62F05>
      ****        MOVE CLRF-GROUP-AGENT  TO WSAA-ROLE                   <V62F05>
      ****     ELSE                                                     <V62F05>
               MOVE CLRF-AGNT          TO WSAA-ROLE
      ****     END-IF                                                   <V62F05>
           ELSE
           IF  WIND-CONTRACT
               MOVE CLRF-OWNR          TO WSAA-ROLE
                                                                        <V4G002>
               IF WSAA-ACTION          = 'A'  AND                       <V4G002>
                  WSAA-PGMNAM          = 'PR937'                        <V4G002>
                  MOVE CLRF-OWNR           TO WSAA-ROLE                 <V4G002>
               ELSE                                                     <V4G002>
               IF WSAA-ACTION          = 'B' AND                        <V4G002>
                  WSAA-PGMNAM          = 'PR937'                        <V4G002>
                  MOVE CLRF-LIFE           TO WSAA-ROLE                 <V4G002>
               END-IF                                                   <V4G002>

           ELSE
           IF  WIND-REINSURANCE
      *        MOVE CLRF-RACC          TO WSAA-ROLE                     <022>
               MOVE CLRF-AGNT          TO WSAA-ROLE                     <022>
           ELSE
           IF  WIND-TREATY
               MOVE CLRF-TRTY          TO WSAA-ROLE
           ELSE
           IF  WIND-CLBA
               MOVE CLRF-CLBA          TO WSAA-ROLE
           ELSE
           IF  WIND-CCARD                                               <V76F13>
               MOVE CLRF-CCARD         TO WSAA-ROLE                     <V76F13>
           ELSE                                                         <V76F13>
           IF WIND-LIFE-RI
               MOVE CLRF-RILF          TO WSAA-ROLE
           ELSE
           IF WIND-CLMPAYEE
               MOVE CLRF-CLMPAYEE      TO WSAA-ROLE
           ELSE
           IF WIND-GROUP
               MOVE CLRF-GRUP          TO WSAA-ROLE
           ELSE
           IF WIND-DOCTOR                                               <FUPLET>
               MOVE CLRF-DOCTOR        TO WSAA-ROLE                     <FUPLET>
           ELSE                                                         <FUPLET>
           IF  WIND-CLAIM
               IF WSSP-WINDOW-SUB-TYPE = 'N'
                  MOVE CLRF-CLAM       TO WSAA-ROLE
               ELSE
               IF WSSP-WINDOW-SUB-TYPE = 'E'
                  MOVE CLRF-CLMCASH    TO WSAA-ROLE
               ELSE
               IF WSSP-WINDOW-SUB-TYPE = 'X'                            <V4G4.1>
                  MOVE CLRF-GROUP-CLAIMANT TO WSAA-ROLE                 <V4G4.1>
               ELSE                                                     <V4G4.1>
               IF WSSP-WINDOW-SUB-TYPE = 'R'
                  MOVE CLRF-OWNR       TO WSAA-ROLE.
      *

           IF WSAA-ROLE                = SPACES

               IF WSSP-ROLE-F(1)       NOT = SPACES
                   MOVE WSSP-ROLE-F(1) TO WSAA-ROLE
               ELSE
                   MOVE SPACES         TO DESC-SHORTDESC
                   GO TO 1050-CONTINUE
               END-IF
           END-IF.

           MOVE SPACES                 TO CLRN-CLTRELN-REC.
           MOVE 'FLG  '                TO CLRN-FUNCTION.
           MOVE WSAA-ROLE              TO CLRN-CLRRROLE.

           CALL 'CLTRELN'              USING CLRN-CLTRELN-REC.
           IF CLRN-STATUZ              NOT = O-K
              MOVE CLRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.

           MOVE CLRN-FLAG-NO           TO WSCC-SUB.
      *
      *    Find role description to display at top of screen
      *
           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE 'IT'                   TO DESC-DESCPFX.
           MOVE WSSP-FSUCO             TO DESC-DESCCOY.
           MOVE T3639                  TO DESC-DESCTABL.
           MOVE WSAA-ROLE              TO DESC-DESCITEM.
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.
           MOVE READR                  TO DESC-FUNCTION.
           CALL 'DESCIO' USING DESC-PARAMS.
           IF DESC-STATUZ              NOT = O-K
                                         AND MRNF
               MOVE DESC-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
       1050-CONTINUE.
      *
           MOVE SPACES                 TO WSAA-CLTNKEY.
           MOVE PRFX-CLNT              TO WSKY-CLTN-CLNTPFX.
           MOVE WSSP-FSUCO             TO WSKY-CLTN-CLNTCOY.
           MOVE SPACES                 TO S2473-DATA-AREA.
           MOVE SPACES                 TO S2473-SUBFILE-AREA.
           IF WSSP-DOB01-F NOT NUMERIC
      ****     MOVE ZEROS              TO WSSP-DOB01-F.                 <D9607>
               MOVE VRCM-MAX-DATE      TO WSSP-DOB01-F.                 <D9607>
           IF WSSP-DOB02-F NOT NUMERIC
               MOVE 99999999           TO WSSP-DOB02-F.
           MOVE SPACES                 TO WSSP-CONFIRMATION.
           MOVE WSSP-SURNAME-F         TO S2473-SURNAME.
           MOVE WSSP-EXACT             TO S2473-EXACT.
      *
      * Depending on which field is passed set up first read.
      *
      *
      *                                                                 <CAS1.0>
      * If an ID has been entered then ignore the searches on anything  <CAS1.0>
      * else.                                                           <CAS1.0>
      *                                                                 <CAS1.0>
            IF S2473-SECUITYNO       NOT = SPACES                       <CAS1.0>
               GO TO 1150-CONT.                                         <CAS1.0>
      *                                                                 <CAS1.0>
           IF WSSP-SURNAME-F           = SPACES
              MOVE WSSP-VALUE          TO WSSP-SURNAME-F,
                                          S2473-SURNAME.
           IF WSSP-VALUE               = SPACES
              MOVE WSSP-SURNAME-F      TO WSSP-VALUE.
      *
           MOVE DESC-SHORTDESC         TO S2473-ROLEDC.
           MOVE DESC-SHORTDESC         TO WSAA-ROLE-DESC.               <CAS1.0>
      *
      *    Determine length of name string entered
      *
           MOVE WSSP-SURNAME-F         TO WSAA-STRING.
           IF WSSP-SURNAME-F           = SPACE
              MOVE 0                   TO WSAA-LENGTH
              GO TO 1150-CONT.
           IF WSSP-CLTTYPE-F           NOT = 'P'
              MOVE WSSP-GIVNAME-F      TO WSAA-GIVNAME
              SET WA                   TO 50
           ELSE
              SET WA                   TO 30.
      *
       1100-LOOP.
           IF WSAA-CHAR(WA)            NOT = SPACE
              SET WSAA-LENGTH          TO WA
           ELSE
           IF WA                       = 1
              MOVE 0                   TO WSAA-LENGTH
           ELSE
              SET WA                   DOWN BY 1
              GO TO 1100-LOOP.
      *
       1150-CONT.
           MOVE 0                      TO WSAA-SRHLIMIT.                <D9607>
           MOVE SCLR                   TO SCRN-FUNCTION.
           CALL 'S2473IO' USING SCRN-SCREEN-PARAMS
                                 S2473-DATA-AREA
                                 S2473-SUBFILE-AREA.
           IF SCRN-STATUZ              NOT = O-K
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.

           PERFORM 1400-GET-ID-TEXT.                                    <MLS001>

           PERFORM A100-FIND-FIRST.
           MOVE SPACES                 TO WSSP-VALUE.
                                                                        <A05833>
      ****                                                              <A05833>
      **** If the search does not return a record, do not attempt to    <A05833>
      **** load the subfile.                                            <A05833>
      ****                                                              <A05833>
                                                                        <A05833>
           IF SCRN-ERROR-CODE           = E040                          <A05833>
               MOVE 'Y'                TO WSSP-EDTERROR                 <A05833>
               GO TO 1900-EXIT                                          <A05833>
           END-IF.                                                      <A05833>
                                                                        <A05833>
      ****                                                      <>      <SDAS>
      **** Read T3568 to determine if this program has been accessed    <SDAS>
      **** from Secured Data Access.                            <>      <SDAS>
      ****                                                      <>      <SDAS>
                                                                        <SDAS>
           INITIALIZE ITEMREC-KEY-DATA                                  <SDAS>
                      ITEMREC-NON-KEY-DATA.                             <SDAS>
                                                                        <SDAS>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <SDAS>
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.                 <SDAS>
           MOVE T3568                  TO ITEM-ITEMTABL.                <SDAS>
           MOVE WSAA-PROG              TO ITEM-ITEMITEM.                <SDAS>
                                                                        <SDAS>
           MOVE READR                  TO ITEM-FUNCTION.                <SDAS>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <SDAS>
                                                                        <SDAS>
           CALL 'ITEMIO'            USING ITEM-PARAMS.                  <SDAS>
                                                                        <SDAS>
           IF ITEM-STATUZ           NOT = O-K                           <SDAS>
              AND                   NOT = MRNF                          <SDAS>
               MOVE ITEM-STATUZ        TO SYSR-STATUZ                   <SDAS>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <SDAS>
               PERFORM 600-FATAL-ERROR                                  <SDAS>
           END-IF.                                                      <SDAS>
                                                                        <SDAS>
           IF ITEM-STATUZ               = O-K                           <SDAS>
               MOVE 'Y'                TO WSAA-SDA                      <SDAS>
               MOVE ADDITIONAL-FIELDS  OF WSSP-USER-AREA                <SDAS>
                                       TO WSAA-SDA-ADDITIONAL-FIELDS    <SDAS>
           END-IF.                                                      <SDAS>
                                                                        <SDAS>
           PERFORM 5000-LOAD-SUBFILE-PAGE.
      *
      *  Remember where name search starts (to see if it is overkeyed
      *                                         on the screen later)
           MOVE S2473-SURNAME          TO WSAA-SEARCH-NAME.
      *
       1900-EXIT.
            EXIT.
      /
       1200-READ-T1680 SECTION.                                         <V5L001>
      *************************                                         <V5L001>
       1210-T1680.                                                      <V5L001>
            MOVE 'IT'                  TO ITEM-ITEMPFX.                 <V5L001>
            MOVE ZERO                  TO ITEM-ITEMCOY.                 <V5L001>
            MOVE T1680                 TO ITEM-ITEMTABL.                <V5L001>
            MOVE WSSP-LANGUAGE         TO ITEM-ITEMITEM.                <V5L001>
            MOVE ITEMREC               TO ITEM-FORMAT.                  <V5L001>
            MOVE READR                 TO ITEM-FUNCTION.                <V5L001>
                                                                        <V5L001>
            CALL 'ITEMIO'           USING ITEM-PARAMS.                  <V5L001>
            IF ITEM-STATUZ          NOT = O-K                           <V5L001>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <V5L001>
               PERFORM 600-FATAL-ERROR.                                 <V5L001>
                                                                        <V5L001>
            MOVE ITEM-GENAREA          TO T1680-T1680-REC.              <V5L001>
                                                                        <V5L001>
            MOVE SPACES                TO WSAA-ALT-DBCS.                <V5L001>
            IF T1680-FUNCKEYPR      NOT = '1'                           <V5L001>
             AND T1680-LANGUAGE-DBCS    = 'Y'                           <V5L001>
               MOVE 'Y'                TO WSAA-ALT-DBCS.                <V5L001>
                                                                        <V5L001>
       1290-EXIT.                                                       <V5L001>
            EXIT.                                                       <V5L001>
                                                                        <V5L001>
      *********************************                                 <024>
       1300-READ-ADDR-VAL-TABLE SECTION.                                <024>
      *********************************                                 <024>
       1310-INIT.                                                       <024>
      *                                                                 <024>
      *  Read Address validation rule Table                             <024>
      *                                                                 <024>
           MOVE SPACES                 TO ITEM-DATA-KEY.                <024>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <024>
           MOVE WSSP-FSUCO             TO ITEM-ITEMCOY.                 <024>
           MOVE T2241                  TO ITEM-ITEMTABL.                <024>
           MOVE CLTS-CTRYCODE          TO WSAA-CNT.                     <024>
           MOVE WSSP-LANGUAGE          TO WSAA-LNG.                     <024>
           MOVE WSAA-CNT-LNG           TO ITEM-ITEMITEM.                <024>
           MOVE READR                  TO ITEM-FUNCTION.                <024>
      *                                                                 <024>
           CALL 'ITEMIO' USING ITEM-PARAMS.                             <024>
           IF ITEM-STATUZ          NOT = O-K                            <024>
                               AND NOT = MRNF                           <024>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <024>
               MOVE ITEM-STATUZ        TO SYSR-STATUZ                   <024>
               PERFORM 600-FATAL-ERROR                                  <024>
           END-IF.                                                      <024>
      *                                                                 <024>
           IF ITEM-STATUZ              = MRNF                           <024>
      *                                                                 <024>
      *  Read Default validation if specific not found                  <024>
      *                                                                 <024>
               MOVE SPACES             TO ITEM-DATA-KEY                 <024>
               MOVE 'IT'               TO ITEM-ITEMPFX                  <024>
               MOVE WSSP-FSUCO         TO ITEM-ITEMCOY                  <024>
               MOVE T2241              TO ITEM-ITEMTABL                 <024>
               MOVE '***'              TO WSAA-CNT                      <024>
               MOVE WSSP-LANGUAGE      TO WSAA-LNG                      <024>
               MOVE WSAA-CNT-LNG       TO ITEM-ITEMITEM                 <024>
               MOVE READR              TO ITEM-FUNCTION                 <024>
      *                                                                 <024>
               CALL 'ITEMIO' USING ITEM-PARAMS                          <024>
               IF ITEM-STATUZ      NOT = O-K                            <024>
                               AND NOT = MRNF                           <024>
                   MOVE ITEM-PARAMS    TO SYSR-PARAMS                   <024>
                   MOVE ITEM-STATUZ    TO SYSR-STATUZ                   <024>
                   PERFORM 600-FATAL-ERROR                              <024>
               END-IF                                                   <024>
      *                                                                 <024>
           END-IF.                                                      <024>
      *                                                                 <024>
           IF ITEM-STATUZ              = O-K                            <024>
               MOVE ITEM-GENAREA       TO T2241-T2241-REC.              <024>
      *                                                                 <024>
       1390-EXIT.                                                       <024>
           EXIT.                                                        <024>
      /
      *********************************                                 <MLS001>
       1400-GET-ID-TEXT SECTION.                                        <MLS001>
      *********************************                                 <MLS001>
       1410-INIT.                                                       <MLS001>
      *                                                                 <MLS001>
      *--- Read TR386 table to get ID literals                          <MLS001>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <MLS001>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <MLS001>
           MOVE WSSP-FSUCO             TO ITEM-ITEMCOY.                 <MLS001>
           MOVE TR386                  TO ITEM-ITEMTABL.                <MLS001>
      **** MOVE WSSP-LANGUAGE          TO WSAA-TR386-LANG.      <MLS002><MLS001>
      **** MOVE WSAA-PROG              TO WSAA-TR386-PGM.       <MLS002><MLS001>
      **** MOVE SPACES                 TO WSAA-TR386-ID.        <MLS002><MLS001>
      **** MOVE WSAA-TR386-KEY         TO ITEM-ITEMITEM.        <MLS002><MLS001>
           MOVE WSSP-LANGUAGE          TO WSAA-TR386-FSUCO-LANG.        <MLS002>
           MOVE WSAA-TR386-FSUCO-KEY   TO ITEM-ITEMITEM.                <MLS002>
           MOVE READR                  TO ITEM-FUNCTION.                <MLS001>
                                                                        <MLS001>
           CALL  'ITEMIO'           USING ITEM-PARAMS.                  <MLS001>
                                                                        <MLS001>
           IF  ITEM-STATUZ              = MRNF                          <FA1158>
               MOVE SPACES             TO ITEM-GENAREA                  <FA1158>
           ELSE                                                         <FA1158>
             IF  ITEM-STATUZ          NOT = O-K                         <FA1158>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <MLS001>
               PERFORM 600-FATAL-ERROR.                                 <MLS001>
                                                                        <MLS001>
           MOVE ITEM-GENAREA           TO TR386-TR386-REC.              <MLS001>
      ***  MOVE TR386-PROGDESC(1)      TO S2473-IDNUMTXT-01.            <MLS001>
      ***  MOVE TR386-PROGDESC(2)      TO S2473-IDNUMTXT-02.            <MLS001>
           MOVE TR386-PROGDESC-01      TO S2473-IDNUMTXT-01.            <FA1226>
           MOVE TR386-PROGDESC-02      TO S2473-IDNUMTXT-02.            <FA1226>
      *                                                                 <MLS001>
      *                                                                 <MLS001>
       1490-EXIT.                                                       <MLS001>
           EXIT.                                                        <MLS001>
      /                                                                 <024>
      *****************************************************************
      *     RETRIEVE SCREEN FIELDS AND EDIT
      *****************************************************************
      *
       PRE-SCREEN-EDIT SECTION.                                         <S9503>
      ************************                                          <S9503>
      *                                                                 <S9503>
       PRE-START.                                                       <S9503>
      *                                                                 <S9503>
           MOVE O-K                    TO WSSP-EDTERROR.                <S9503>
      *                                                                 <S9503>
      *                                                                 <S9503>
      *    If servicing selected items,                                 <S9503>
      *       skip this section.                                        <S9503>
      *     (No screen validation during iterative processing)          <S9503>
      *                                                                 <S9503>
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'                    <S9503>
              MOVE 3000                TO WSSP-SECTIONNO                <S9503>
           GO TO PRE-EXIT.                                              <S9503>
      *                                                                 <S9503>
      *    Output the screen.                                           <S9503>
      *                                                                 <S9503>
           GO TO PRE-EXIT.                                              <S9503>
      *                                                                 <S9503>
       PRE-EXIT.                                                        <S9503>
           EXIT.                                                        <S9503>
      /                                                                 <S9503>
       2000-SCREEN-EDIT SECTION.
      **************************
      *
       2001-SCREEN-IO.
      *    CALL 'S2473IO' USING SCRN-SCREEN-PARAMS                      <S9503>
      *                          S2473-DATA-AREA                        <S9503>
      *                          S2473-SUBFILE-AREA.                    <S9503>
      * Screen errors are now handled in the calling program.           <S9503>
      *    PERFORM 200-SCREEN-ERRORS.                                   <S9503>
      *
       2100-CONT.
      *
      *    CF10 - Filter screen requested
      *
           IF SCRN-STATUZ              = 'SWCH'
      *  The exit paragraph name will be standardised to 2090 -EXIT.    <S9503>
      *       GO TO 2900-EXIT.                                          <S9503>
              GO TO 2090-EXIT.                                          <S9503>
      *
       2110-NEXT.
      *
           IF  S2473-ERROR-INDICATORS NOT = SPACES
               MOVE 'Y'          TO WSSP-EDTERROR
           ELSE
               MOVE O-K          TO WSSP-EDTERROR.
      *
      *    Scroll - But depends on whether an ID is present or not
      *
           IF SCRN-STATUZ              = ROLU
              IF S2473-SECUITYNO       = SPACES                         <CAS1.0>
   ****       PERFORM 5000-LOAD-SUBFILE-PAGE                            <CAS1.0>
                  PERFORM 5000-LOAD-SUBFILE-PAGE                        <CAS1.0>
              ELSE                                                      <CAS1.0>
                 PERFORM A1000-LOAD-FOR-IDNO                            <CAS1.0>
              END-IF                                                    <CAS1.0>
              MOVE SPACES                 TO WSSP-EDTERROR
      *  The exit paragraph name will be standardised to 2090 -EXIT.    <S9503>
   ****       GO TO 2900-EXIT.                                          <S9503>
   ****       GO TO 2090-EXIT.                                          <S9503>
      *  The exit paragraph name will be standardised to 2090 -EXIT.    <S9503>
      *       GO TO 2900-EXIT                                           <S9503>>
              GO TO 2090-EXIT                                           <S9503>>
           END-IF.                                                      <CAS1.0>
      *
      *    If the name or exact match fields have been changed start
      *    scrolling from new name
      *
           MOVE WSSP-CHDR-TYPEDESC TO WSAA-ID.                          <CAS1.0>
           IF S2473-SECUITYNO      NOT = SPACES                         <CAS1.0>
              IF S2473-SECUITYNO       = WSAA-ID                        <CAS1.0>
                 GO TO 2200-VALIDATE.                                   <CAS1.0>
      *                                                                 <CAS1.0>
           IF S2473-SECUITYNO      NOT = SPACES                         <CAS1.0>
              MOVE S2473-SECUITYNO TO WSSP-CHDR-TYPEDESC                <CAS1.0>
              GO TO 2150-CONT                                           <CAS1.0>
           ELSE                                                         <CAS1.0>
              MOVE SPACES          TO WSSP-CHDR-TYPEDESC.               <CAS1.0>
      *                                                                 <CAS1.0>
           IF S2473-SURNAME            = WSSP-SURNAME-F AND
              S2473-EXACT              = WSSP-EXACT
              GO TO 2200-VALIDATE.
      *
           MOVE S2473-EXACT            TO WSSP-EXACT.
           IF WSSP-SURNAME-F           = S2473-SURNAME
              GO TO 2150-CONT
           ELSE
              MOVE S2473-SURNAME       TO WSSP-SURNAME-F
              MOVE SPACES              TO CLTN-GIVNAME
              MOVE SPACES              TO CLTS-GIVNAME
              MOVE SPACES              TO CLTA-GIVNAME
              MOVE SPACES              TO CLOW-GIVNAME                  <GP3IND>
              MOVE SPACES              TO CLPE-GIVNAME                  <GP3IND>
              MOVE SPACES              TO CLPY-GIVNAME                  <GP3IND>
              MOVE SPACES              TO CLTB-GIVNAME.
      *
      *    Determine length of name string entered
      *
           MOVE WSSP-SURNAME-F         TO WSAA-SURNAME.
           IF WSSP-SURNAME-F           = SPACE
              MOVE 0                   TO WSAA-LENGTH
              GO TO 2150-CONT.
           SET WA                      TO 30.
      *
       2120-LOOP.
           IF WSAA-CHAR(WA)            NOT = SPACE
              SET WSAA-LENGTH          TO WA
           ELSE
           IF WA                       = 1
              MOVE 0                   TO WSAA-LENGTH
           ELSE
              SET WA                   DOWN BY 1
              GO TO 2120-LOOP.
      *
       2150-CONT.
           MOVE SPACES                 TO S2473-DATA-AREA.
           MOVE SPACES                 TO S2473-SUBFILE-AREA.
           IF S2473-SECUITYNO        = SPACES                           <CAS1.0>
              MOVE WSSP-SURNAME-F      TO S2473-SURNAME                 <CAS1.0>
              MOVE WSSP-EXACT          TO S2473-EXACT.                  <CAS1.0>
      **** MOVE WSSP-SURNAME-F         TO S2473-SURNAME.                <CAS1.0>
      **** MOVE WSSP-EXACT             TO S2473-EXACT.                  <CAS1.0>
      *                                                                 <023>
      * Move the client role to the screen field.                       <023>
      *                                                                 <023>
           MOVE DESC-SHORTDESC         TO S2473-ROLEDC.                 <023>
      *                                                                 <023>
           MOVE SCLR                   TO SCRN-FUNCTION.
           CALL 'S2473IO' USING SCRN-SCREEN-PARAMS
                                 S2473-DATA-AREA
                                 S2473-SUBFILE-AREA.
           IF SCRN-STATUZ              NOT = O-K
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.

           PERFORM 1400-GET-ID-TEXT.                                    <MLS001>
      *
      **** PERFORM A100-FIND-FIRST.                                     <CAS1.0>
           MOVE WSAA-ROLE-DESC       TO S2473-ROLEDC.                   <CAS1.0>
           IF WSSP-CHDR-TYPEDESC      = SPACES                          <CAS1.0>
              PERFORM A100-FIND-FIRST                                   <CAS1.0>
              GO TO 2200-LOAD.                                          <CAS1.0>
      *                                                                 <CAS1.0>
           MOVE WSSP-CHDR-TYPEDESC     TO S2473-SECUITYNO.              <CAS1.0>
           MOVE BEGN                   TO CLNTSSN-FUNCTION.             <CAS1.0>
           MOVE S2473-SECUITYNO        TO CLNTSSN-SECUITYNO.            <CAS1.0>
           MOVE SPACES                 TO S2473-SURNAME                 <CAS1.0>
                                          S2473-EXACT                   <CAS1.0>
                                          S2473-ROLEDC.                 <CAS1.0>
           MOVE CLNTSSNREC             TO CLNTSSN-FORMAT.               <CAS1.0>
           CALL 'CLNTSSNIO'            USING CLNTSSN-PARAMS.            <CAS1.0>
           IF  CLNTSSN-STATUZ          NOT = O-K                        <CAS1.0>
           AND CLNTSSN-STATUZ          NOT = ENDP                       <CAS1.0>
               MOVE CLNTSSN-PARAMS      TO SYSR-PARAMS                  <CAS1.0>
               PERFORM 600-FATAL-ERROR.                                 <CAS1.0>
      *                                                                 <CAS1.0>
           IF CLNTSSN-STATUZ              = ENDP                        <CAS1.0>
              MOVE E040                TO SCRN-ERROR-CODE               <CAS1.0>
              MOVE 'Y'                 TO WSSP-EDTERROR                 <CAS1.0>
      *  The exit paragraph name will be standardised to 2090 -EXIT.    <S9503>
      *       GO TO 2900-EXIT.                                          <S9503>>
              GO TO 2090-EXIT.                                          <S9503>>
      *                                                                 <CAS1.0>
           IF CLNTSSN-CLNTCOY NOT = WSSP-FSUCO                          <CAS1.0>
              MOVE ENDP               TO CLNTSSN-STATUZ.                <CAS1.0>
      *                                                                 <CAS1.0>
           IF CLNTSSN-STATUZ              = ENDP                        <CAS1.0>
              MOVE CLNTSSN-STATUZ         TO WSAA-FILE-STATUZ           <CAS1.0>
              MOVE E040                TO SCRN-ERROR-CODE               <AY2K>
              MOVE 'Y'                 TO WSSP-EDTERROR                 <AY2K>
      *  The exit paragraph name will be standardised to 2090 -EXIT.    <S9503>
      *       GO TO 2900-EXIT.                                          <S9503>
              GO TO 2090-EXIT.                                          <S9503>
      ***     GO TO 1900-EXIT.                                  <AY2K>  <CAS1.0>
      *                                                                 <CAS1.0>
           MOVE CLNTSSN-CLNTPFX        TO CLTS-CLNTPFX.                 <CAS1.0>
           MOVE CLNTSSN-CLNTCOY        TO CLTS-CLNTCOY.                 <CAS1.0>
           MOVE CLNTSSN-CLNTNUM        TO CLTS-CLNTNUM.                 <CAS1.0>
           MOVE READR                  TO CLTS-FUNCTION.                <CAS1.0>
           MOVE CLTSREC                TO CLTS-FORMAT.                  <CAS1.0>
           CALL 'CLTSIO'               USING CLTS-PARAMS.               <CAS1.0>
           IF CLTS-STATUZ              NOT = O-K                        <CAS1.0>
               MOVE CLTS-PARAMS        TO SYSR-PARAMS                   <CAS1.0>
               PERFORM 600-FATAL-ERROR.                                 <CAS1.0>
      *                                                                 <CAS1.0>
           MOVE CLTS-SURNAME           TO WSCC-SURNAME.                 <CAS1.0>
           MOVE CLTS-GIVNAME           TO WSCC-GIVNAME.                 <CAS1.0>
           MOVE CLTS-MIDDL01           TO WSCC-MIDDL01.                 <CAS1.0>
           MOVE CLTS-MIDDL02           TO WSCC-MIDDL02.                 <CAS1.0>
           MOVE CLTS-CLTTYPE           TO WSCC-CLTTYPE.                 <CAS1.0>
           MOVE CLTS-CLTSTAT           TO WSCC-CLTSTAT.                 <CAS1.0>
           MOVE CLTS-SERVBRH           TO WSCC-SERVBRH.                 <CAS1.0>
           MOVE CLTS-CLTSEX            TO WSCC-CLTSEX.                  <CAS1.0>
           MOVE CLTS-CLTDOB            TO WSCC-CLTDOB.                  <CAS1.0>
           MOVE CLTS-SECUITYNO         TO WSCC-SECUITYNO.               <CAS1.0>
           MOVE CLTS-CLTIND            TO WSCC-CLTIND.                  <CAS1.0>
           MOVE CLTS-ROLEFLAGS         TO WSCC-ROLEFLAGS.               <CAS1.0>
           MOVE CLTS-LSURNAME          TO WSCC-LSURNAME.                <FSU307>
           MOVE CLTS-CLTADDR01         TO WSCC-CLTADDR01.               <V4F002>
      *                                                                 <CAS1.0>
       2200-LOAD.                                                       <CAS1.0>
      *                                                                 <CAS1.0>
           IF SCRN-ERROR-CODE          = E040
              MOVE 'Y'                 TO WSSP-EDTERROR
      *  The exit paragraph name will be standardised to 2090 -EXIT.    <S9503>
      *       GO TO 2900-EXIT.                                          <S9503>
              GO TO 2090-EXIT.                                          <S9503>
      *
      **** PERFORM 5000-LOAD-SUBFILE-PAGE.                              <CAS1.0>
           IF S2473-SECUITYNO       = SPACES                            <CAS1.0>
              PERFORM 5000-LOAD-SUBFILE-PAGE                            <CAS1.0>
           ELSE                                                         <CAS1.0>
              PERFORM A1000-LOAD-FOR-IDNO.                              <CAS1.0>
      *                                                                 <CAS1.0>
           MOVE 'Y'                    TO WSSP-EDTERROR.
      *  The exit paragraph name will be standardised to 2090 -EXIT.    <S9503>
      *    GO TO 2900-EXIT.                                             <S9503>
           GO TO 2090-EXIT.                                             <S9503>
      *
      *    Validate the select field for each record
      *
       2200-VALIDATE.
      *
      *    Find the first changed record
      *
           MOVE SRNCH                  TO SCRN-FUNCTION.
           CALL 'S2473IO' USING SCRN-SCREEN-PARAMS
                                 S2473-DATA-AREA
                                 S2473-SUBFILE-AREA.
           IF SCRN-STATUZ NOT = O-K
                            AND ENDP
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.
      *
           IF SCRN-STATUZ              = ENDP
      *  The exit paragraph name will be standardised to 2090 -EXIT.    <S9503>
      *          GO TO 2900-EXIT.                                       <S9503>
                 GO TO 2090-EXIT.                                       <S9503>
      *
       2250-CHECK.
      *
           IF (S2473-SLT            NOT = SPACE) AND
              (S2473-SLT            NOT = 1)     AND
              (S2473-SLT            NOT = 5)     AND
   ***        (S2473-SLT            NOT = 6)                            <CAS1.0>
   ****       (S2473-SLT            NOT = 6)                            <CAS1.0>
      ****    (S2473-SLT            NOT = 6)     AND                    <CAS1.0>
      ****    (S2473-SLT            NOT = 7)                            <CAS1.0>
              MOVE E005                TO S2473-SLT-ERR
              MOVE 'Y'                 TO WSSP-EDTERROR
           END-IF.
      **** IF S2473-SLT                = 1     AND                      <021>
      ****   (S2473-AAFLAG             = 'D'   OR                       <021>
      ****    S2473-ALFLAG             = 'L')                           <021>
      ****    MOVE E005                TO S2473-SLT-ERR                 <021>
      ****    MOVE 'Y'                 TO WSSP-EDTERROR                 <021>
      **** END-IF.                                                      <021>
      *
           MOVE SUPD                   TO SCRN-FUNCTION.
           CALL 'S2473IO' USING SCRN-SCREEN-PARAMS
                             S2473-DATA-AREA
                             S2473-SUBFILE-AREA.
           IF SCRN-STATUZ NOT = O-K
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.
      *
           MOVE SRNCH                  TO SCRN-FUNCTION.
           CALL 'S2473IO' USING SCRN-SCREEN-PARAMS
                                 S2473-DATA-AREA
                                 S2473-SUBFILE-AREA.
           IF SCRN-STATUZ NOT = O-K
                            AND ENDP
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.
      *
           IF SCRN-STATUZ              NOT = ENDP
              GO TO 2250-CHECK.
      *
      *  The exit paragraph name will be standardised to 2090 -EXIT.    <S9503>
      *2900-EXIT.                                                       <S9503>
       2090-EXIT.                                                       <S9503>
            EXIT.
      /
      *****************************************************************
      *     UPDATE DATABASE IF REQUIRED AND LOG TRANSACTION
      *****************************************************************
      *
       3000-UPDATE SECTION.
      **********************
      *
       3100-LOAD-WSSP-FIELDS.
      *
       3900-EXIT.
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
           MOVE WSAA-PROG              TO WSSP-NEXTPROG.
      *
      *    Handle function key pressed
      *       - filter requested
      *
           IF SCRN-STATUZ              = 'SWCH'
              PERFORM 4100-SAVE-STACK
              PERFORM 4200-GEN-SWITCH
      *       MOVE '????'              TO SCRN-STATUZ                   <FPS30>
              MOVE '?'                 TO WSAA-IND                      <FPS30>
      ****    GO TO 4090-EXIT                                   <>      <SDAS>
              GO TO 4080-HIDE                                           <SDAS>
           END-IF.
      *
      *       - return from filter request (re-load subfile)
      *
      ***  IF SCRN-STATUZ              = '????'                         <FPS30>
           IF WSAA-IND                 = '?'                            <FPS30>
              PERFORM 4300-RESTORE-STACK
              MOVE ' '                 TO WSSP-SEC-ACTN
                                                  (WSSP-PROGRAM-PTR)
              MOVE SPACES              TO WSAA-IND                      <FPS30>
      ****    GO TO 4090-EXIT                                   <>      <SDAS>
              GO TO 4080-HIDE                                           <SDAS>
           END-IF.
      *
      *    Read first/next subfile record (if required)
      *
      *       - first time in
      *
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = ' '
              PERFORM 4100-SAVE-STACK
              MOVE SSTRT               TO SCRN-FUNCTION
              PERFORM 4400-SUBFILE-IO
           ELSE
              IF S2473-SLT = '?'
      *
      *       - return after alias/address select
      *
                 IF WSSP-VALUE NOT = SPACES
                    MOVE '+'           TO S2473-SLT
                    MOVE WSSP-VALUE    TO S2473-CLNTNUM
                 ELSE
      *               item not selected
      *
                    MOVE SRDN          TO SCRN-FUNCTION
                    PERFORM 4400-SUBFILE-IO
                 END-IF
              ELSE
      *
      *       - return after application subject selected
      *
                 IF WSSP-VALUE NOT = SPACES
                    MOVE SPACE         TO WSSP-SEC-ACTN
                                                  (WSSP-PROGRAM-PTR)
                    MOVE SPACE         TO WSSP-SEC-PROG
                                                  (WSSP-PROGRAM-PTR)
      ****          GO TO 4090-EXIT                             <>      <SDAS>
                    GO TO 4080-HIDE                                     <SDAS>
                 ELSE
      *               item not selected
      *
                    MOVE SRDN          TO SCRN-FUNCTION
                    PERFORM 4400-SUBFILE-IO
                 END-IF
              END-IF
           END-IF.
      *
      *  Process each line in the subfile
      *
           PERFORM UNTIL SCRN-STATUZ = ENDP
      *
      *  Alias/alternate name scroll requested
      *
              IF S2473-SLT = '5' OR '6'                                 <CAS1.0>
      ****    IF S2473-SLT = '5' OR '6'                            <CAS1.0><015>
      ****    IF S2473-SLT = '5' OR '6' OR '7'                          <CAS1.0>
                 PERFORM 4200-GEN-SWITCH
                 PERFORM 4500-RESET-SUBFILE-LINE
                 MOVE SCRN-SUBFILE-RRN TO WSAA-SUBFILE-RRN
                 MOVE SPACES           TO WSAA-CLTSKEY
                 MOVE PRFX-CLNT        TO WSKY-CLTS-CLNTPFX
                 MOVE WSSP-FSUCO       TO WSKY-CLTS-CLNTCOY
                 MOVE S2473-CLNTNUM    TO WSKY-CLTS-CLNTNUM
                 MOVE WSAA-CLTSKEY     TO WSSP-CLNTKEY
                 MOVE '?'              TO S2473-SLT
      ****       GO TO 4090-EXIT                                <>      <SDAS>
                 GO TO 4080-HIDE                                        <SDAS>
              END-IF
      *
      *  Client selected
      *
              IF S2473-SLT = '1' OR '+'
                 IF S2473-SLT = '1'
                    PERFORM 4500-RESET-SUBFILE-LINE
                    MOVE SCRN-SUBFILE-RRN TO WSAA-SUBFILE-RRN
                 END-IF
                 PERFORM 4600-CONFIRMATION-FIELDS
                 IF WSSP-SEC-ACTN(WSSP-PROGRAM-PTR) = '*'
                    PERFORM 4300-RESTORE-STACK
                 END-IF
                 MOVE '*'              TO WSSP-SEC-ACTN
                                                  (WSSP-PROGRAM-PTR)
                 ADD 1                 TO WSSP-PROGRAM-PTR
      ****       GO TO 4090-EXIT                                <>      <SDAS>
                 GO TO 4080-HIDE                                        <SDAS>
              END-IF
      *
      *  Next line
      *
              MOVE SRDN                TO SCRN-FUNCTION
              PERFORM 4400-SUBFILE-IO
           END-PERFORM.
      *
      *  No items selected
      *
           IF WSSP-SEC-ACTN(WSSP-PROGRAM-PTR) = '*'
              PERFORM 4300-RESTORE-STACK
              MOVE SPACES              TO WSSP-SEC-ACTN
                                                  (WSSP-PROGRAM-PTR)
              MOVE WSAA-SUBFILE-RRN    TO SCRN-SUBFILE-RRN
              MOVE SCRN-SCRNAME        TO WSSP-NEXTPROG
           ELSE
              IF WSAA-SEARCH-NAME = S2473-SURNAME
                 MOVE SPACES           TO WSSP-SEC-PROG
                                                  (WSSP-PROGRAM-PTR)
              END-IF
           END-IF.
                                                                        <SDAS>
       4080-HIDE.                                                       <SDAS>
                                                                        <SDAS>
            MOVE 'HIDEW'               TO SCRN-FUNCTION.                <SDAS>
                                                                        <SDAS>
            CALL 'S2473IO'          USING SCRN-SCREEN-PARAMS            <SDAS>
                                          S2473-DATA-AREA               <SDAS>
                                          S2473-SUBFILE-AREA.           <SDAS>
      *
       4090-EXIT.
            EXIT.
      *************************
       4100-SAVE-STACK SECTION.
      *************************
       4110-SET-POINTERS.
           MOVE 1                      TO WSAA-Y.
           ADD 1, WSSP-PROGRAM-PTR     GIVING WSAA-X.
           PERFORM 8 TIMES
              MOVE WSSP-SEC-PROG(WSAA-X) TO WSAA-SAVE-PROG(WSAA-Y)
              ADD 1                    TO WSAA-X
              ADD 1                    TO WSAA-Y
           END-PERFORM.
      *
       4190-EXIT.
            EXIT.
      *************************
       4200-GEN-SWITCH SECTION.
      *************************
       4210-SET-PARAMETERS.
           MOVE WSSP-COMPANY           TO GENS-COMPANY.
           MOVE WSAA-PROG              TO GENS-PROG-IN.
           MOVE '****'                 TO GENS-TRANSACT.
      *
           IF  SCRN-STATUZ              = 'SWCH'
               MOVE 'A'                    TO GENS-FUNCTION
           ELSE
               IF  S2473-SLT                = '5'
                   MOVE 'B'                TO GENS-FUNCTION
               ELSE
                   IF S2473-SLT             = '6'
                      MOVE 'C'             TO GENS-FUNCTION
      ****         END-IF                                          <CAS1.0><015>
      ****     ELSE                                                     <CAS1.0>
      ****         IF S2473-SLT             = '7'                       <CAS1.0>
      ****            MOVE 'D'             TO GENS-FUNCTION             <CAS1.0>
      ****         END-IF                                               <CAS1.0>
               END-IF
           END-IF.
      *
           CALL 'GENSSW' USING GENS-GENSSW-REC.
           IF GENS-STATUZ              NOT = O-K
               MOVE GENS-STATUZ        TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR.
      *
           MOVE 1                      TO WSAA-X.
           ADD 1, WSSP-PROGRAM-PTR     GIVING WSAA-Y.
           PERFORM 8 TIMES
              MOVE GENS-PROG-OUT(WSAA-X) TO WSSP-SEC-PROG(WSAA-Y)
              ADD 1                    TO WSAA-X
              ADD 1                    TO WSAA-Y
           END-PERFORM.
      *
           MOVE '*'                    TO WSSP-SEC-ACTN
                                                  (WSSP-PROGRAM-PTR).
           ADD 1                       TO WSSP-PROGRAM-PTR.
      *
       4290-EXIT.
            EXIT.
      ****************************
       4300-RESTORE-STACK SECTION.
      ****************************
       4310-SET-POINTERS.
           MOVE 1                      TO WSAA-X.
           ADD 1, WSSP-PROGRAM-PTR     GIVING WSAA-Y.
           PERFORM 8 TIMES
              MOVE WSAA-SAVE-PROG(WSAA-X) TO WSSP-SEC-PROG(WSAA-Y)
              ADD 1                    TO WSAA-X
              ADD 1                    TO WSAA-Y
           END-PERFORM.
      *
       4390-EXIT.
            EXIT.
      *************************
       4400-SUBFILE-IO SECTION.
      *************************
       4410-CALL-S2473IO.
           CALL 'S2473IO'              USING SCRN-SCREEN-PARAMS
                                             S2473-DATA-AREA
                                             S2473-SUBFILE-AREA.
           IF SCRN-STATUZ NOT = O-K
                      AND NOT = ENDP
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.
      *
       4490-EXIT.
            EXIT.
      *********************************
       4500-RESET-SUBFILE-LINE SECTION.
      *********************************
       4510-BLANK-OUT-SELECT.
           MOVE SPACE                  TO S2473-SLT.
           MOVE SUPD                   TO SCRN-FUNCTION.
           PERFORM 4400-SUBFILE-IO.
      *
       4590-EXIT.
            EXIT.
      **********************************
       4600-CONFIRMATION-FIELDS SECTION.
      **********************************
       4610-CLIENT-SELECTED.
           MOVE SPACES                 TO WSAA-CLTSKEY.
           MOVE PRFX-CLNT              TO WSKY-CLTS-CLNTPFX.
           MOVE WSSP-FSUCO             TO WSKY-CLTS-CLNTCOY.
           MOVE S2473-CLNTNUM          TO WSKY-CLTS-CLNTNUM
                                          WSSP-VALUE.

           MOVE WSAA-CLTSKEY           TO CLTS-DATA-KEY.
           MOVE WSAA-CLTSKEY           TO WSSP-CLNTKEY.
           MOVE READR                  TO CLTS-FUNCTION.
           MOVE CLTSREC                TO CLTS-FORMAT.

           CALL 'CLTSIO'                 USING CLTS-PARAMS.
           IF CLTS-STATUZ              NOT = O-K
               MOVE CLTS-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
      *    Set up required confirmation field.
      *
           MOVE CLTS-SURNAME           TO WSSP-CONFIRMATION.
********   MOVE WSSP-LANGUAGE          TO WSNM-LANGUAGE.                <021>
           MOVE WSSP-LANGUAGE          TO NMAD-LANGUAGE.                <021>
           IF  WSSP-WINDOW-SUB-TYPE    = 'M'
               MOVE 'N'                TO NMAD-IN-NAME-TYPE             <021>
           ELSE
*********      MOVE WSSP-WINDOW-SUB-TYPE   TO CLNM-IN-NAME-TYPE.        <021>
               MOVE WSSP-WINDOW-SUB-TYPE   TO NMAD-IN-NAME-TYPE.        <021>

*********  MOVE CLTS-SURNAME           TO CLNM-IN-SURNAME               <021>
*********  MOVE CLTS-GIVNAME           TO CLNM-IN-GIVNAME               <021>
           MOVE CLTS-CLNTNUM           TO NMAD-CLNT-NUMBER.             <021>
           MOVE PRFX-CLNT              TO NMAD-CLNT-PREFIX.             <021>
           MOVE CLTS-CLNTCOY           TO NMAD-CLNT-COMPANY.            <021>
*********  MOVE CLTS-SALUTL            TO CLNM-IN-SALUT.                <021>
*********  MOVE CLTS-INITIALS          TO CLNM-IN-INITIALS.             <021>

           IF CLTS-CLTTYPE          = 'C'
*********      MOVE CLTS-SURNAME    TO WSAA-CORPNAME1                   <021>
*********      MOVE CLTS-GIVNAME    TO WSAA-CORPNAME2                   <021>
*********      MOVE WSAA-CORPNAME   TO WSNM-LARGE-NAME                  <021>
               MOVE WSAA-LARGE-NAME TO NMAD-FUNCTION                    <021>
               CALL 'NAMADRS'  USING  NMAD-NAMADRS-REC                  <021>
               IF NMAD-STATUZ       NOT = O-K                           <021>
                  MOVE NMAD-STATUZ  TO SYSR-STATUZ                      <021>
                  PERFORM 600-FATAL-ERROR                               <021>
               ELSE                                                     <021>
                  MOVE NMAD-NAME       TO WSSP-CONFIRMATION             <021>
*********         MOVE WSNM-LARGE-NAME TO WSSP-CONFIRMATION             <021>
           ELSE

           IF NMAD-LARGE-NAME                                           <021>
               MOVE WSAA-LARGE-NAME TO NMAD-FUNCTION                    <021>
               CALL 'NAMADRS'  USING  NMAD-NAMADRS-REC                  <021>
               IF NMAD-STATUZ       NOT = O-K                           <021>
                  MOVE NMAD-STATUZ  TO SYSR-STATUZ                      <021>
                  PERFORM 600-FATAL-ERROR                               <021>
               ELSE                                                     <021>
                  MOVE NMAD-NAME       TO WSSP-CONFIRMATION             <021>
*********      MOVE WSNM-LARGE-NAME TO WSSP-CONFIRMATION                <021>
           ELSE

           IF NMAD-PLAIN-NAME                                           <021>
**********     PERFORM PLNM-PLAIN-NAME                                  <021>
               MOVE WSAA-PLAIN-NAME TO NMAD-FUNCTION                    <021>
               CALL 'NAMADRS'  USING  NMAD-NAMADRS-REC                  <021>
               IF NMAD-STATUZ       NOT = O-K                           <021>
                  MOVE NMAD-STATUZ  TO SYSR-STATUZ                      <021>
                  PERFORM 600-FATAL-ERROR                               <021>
               ELSE                                                     <021>
               MOVE NMAD-NAME       TO WSSP-CONFIRMATION                <021>
**********     MOVE WSNM-SURNAME-OUT   TO WSSP-CONFIRMATION             <021>
           ELSE

********** IF CLNM-PAYEE-NAME                                           <021>
**********     PERFORM PYNM-PAYEE-NAME                                  <021>
**********     MOVE WSNM-SURNAME-OUT   TO WSSP-CONFIRMATION.            <021>
           IF NMAD-PAYEE-GIVN                                           <021>
               MOVE WSAA-PAYEE-GIVN TO NMAD-FUNCTION                    <021>
               CALL 'NAMADRS'  USING  NMAD-NAMADRS-REC                  <021>
               IF NMAD-STATUZ       NOT = O-K                           <021>
                  MOVE NMAD-STATUZ  TO SYSR-STATUZ                      <021>
                  PERFORM 600-FATAL-ERROR                               <021>
               ELSE                                                     <021>
               MOVE NMAD-NAME       TO WSSP-CONFIRMATION.               <021>
      *
       4690-EXIT.
           EXIT.
      /
       5000-LOAD-SUBFILE-PAGE SECTION.
      ********************************
       5100-WRITE-TO-SUBFILE.
           MOVE SPACE                  TO S2473-SLT.
           MOVE SPACE                  TO WSAA-FILE-STATUZ.
           MOVE WSAA-STORE-CLNTNUM     TO S2473-CLNTNUM.
   ****                                                         <CAS1.0>
           IF T2241-IND = '5'                                           <FA3396>
              MOVE WSCC-CLTADDR05      TO S2473-NAMEADR                 <FA3396>
   ****       MOVE WSCC-CLTADDR05      TO S2473-CLTADD                  <FA3396>
           ELSE                                                         <FA3396>
           IF T2241-IND = '4'                                           <FA3396>
              MOVE WSCC-CLTADDR04      TO S2473-NAMEADR                 <FA3396>
   ****       MOVE WSCC-CLTADDR04      TO S2473-CLTADD                  <FA3396>
           ELSE                                                         <FA3396>
           IF T2241-IND = '3'                                           <FA3396>
              MOVE WSCC-CLTADDR03      TO S2473-NAMEADR                 <FA3396>
   ****       MOVE WSCC-CLTADDR03      TO S2473-CLTADD                  <FA3396>
           ELSE                                                         <FA3396>
           IF T2241-IND = '2'                                           <FA3396>
              MOVE WSCC-CLTADDR02      TO S2473-NAMEADR                 <FA3396>
   ****       MOVE WSCC-CLTADDR02      TO S2473-CLTADD                  <FA3396>
           ELSE                                                         <FA3396>
              MOVE WSCC-CLTADDR01      TO S2473-NAMEADR.                <FA3396>
   ****       MOVE WSCC-CLTADDR01      TO S2473-CLTADD.         <CAS1.0><024>
      **** MOVE WSCC-SECUITYNO         TO S2473-ZRSECNO.        <V4F002><CAS1.0>
           MOVE WSCC-SECUITYNO         TO S2473-ZDESC.                  <V4F002>
      **** MOVE WSCC-CLTADDR01         TO S2473-CLTADD.                 <024>
           MOVE WSCC-CLTTYPE           TO S2473-CLTTYPE.
      **** MOVE WSCC-CLTADDR01         TO S2473-ZNAMEADR.       <V64F04><V4F002>
      **** MOVE WSCC-CLTADDR01         TO S2473-NAMEADR.        <FA3396><V64F04>
           MOVE WSCC-CLTSTAT           TO S2473-CLTSTAT.                <V64F04>
                                                                        <V4F002>
      *
      **** IF WSCC-CLTTYPE             = 'C' OR
      ****    WSCC-CLTDOB              = 0   OR
      ****    WSCC-CLTDOB              = VRCM-MAX-DATE
      ****    MOVE SPACES              TO WSAA-PERS-CLTDOBX
              GO TO 5150-CONT.
      *
      **** MOVE WSCC-CLTDOB            TO DTC1-INT-DATE.
      **** IF DTC1-INT-DATE           NOT NUMERIC
      ****    GO TO 5150-CONT.
      **** MOVE 'CONV'                 TO DTC1-FUNCTION.
      **** CALL 'DATCON1'  USING DTC1-DATCON1-REC.
      **** IF DTC1-STATUZ              NOT = O-K
      ****    MOVE DTC1-STATUZ         TO SYSR-STATUZ
      ****    MOVE DTC1-INT-DATE      TO SYSR-PARAMS
      ****    PERFORM 600-FATAL-ERROR.
      **** MOVE DTC1-EXT-DATE          TO WSAA-PERS-CLTDOBX.
      *
       5150-CONT.
           IF WSCC-CLTTYPE             = 'C'
              MOVE WSCC-LSURNAME       TO WSAA-CORP-NAMEA               <FSU307>
      **      MOVE WSCC-GIVNAME        TO WSAA-CORP-NAMEB               <FSU307>
      ****    MOVE WSAA-CORP-DTL       TO S2473-CLTDTL
              MOVE WSAA-CORP-DTL       TO S2473-OWNNAM                  <V4F002>
              GO TO 5160-CONT.                                          <021>
      *
      **** MOVE WSCC-CLTSEX            TO WSAA-PERS-CLTSEX.
      *
      *    Format personal client name
      *
      *    IF THEIR IS NO CLNTNUM THERE IS A BLANK SUBFILE
      *
           IF S2473-CLNTNUM             = SPACES
              GO TO 5160-CONT.

           MOVE SPACES                 TO WSAA-PERS-NAME.
           IF     WSCC-GIVNAME        NOT = SPACES                      <V43L007
           STRING WSCC-SURNAME DELIMITED '  '
                  ', '         DELIMITED SIZE
                  WSCC-GIVNAME DELIMITED '  '
                                       INTO WSAA-PERS-NAME              <V43L007
           ELSE                                                         <V43L007
           MOVE   WSCC-SURNAME         TO WSAA-PERS-NAME                <V43L007
           END-IF.                                                      <V43L007
                                                                        <V43L007
      **** MOVE WSAA-PERS-DTL          TO S2473-CLTDTL.                 <V4F002>
           MOVE WSAA-PERS-DTL          TO S2473-OWNNAM.                 <V4F002>
      *
      *
      *
       5160-CONT.
      *
      *  If windowing with a specific role,  check if client
      *  has this type of role, if not, display the client
      *  but protect the select field.
      *
           PERFORM 8000-FLAGS.
      *                                                                 <021>
           IF WSCC-CLTIND              = 'L' AND                        <021>
              S2473-ALFLAG             = SPACE                          <021>
              MOVE WSCC-CLTIND         TO S2473-ALFLAG                  <021>
           END-IF.                                                      <021>
           IF WSCC-CLTIND              = 'D' AND                        <021>
              S2473-AAFLAG             = SPACE                          <021>
              MOVE WSCC-CLTIND         TO S2473-AAFLAG                  <021>
           END-IF.                                                      <021>
      *
              IF CLRN-COY-IND = 'F'                                     <D9607>
                  MOVE WSSP-FSUCO      TO WSAA-COMPANY                  <D9607>
              END-IF .                                                  <D9607>
              IF CLRN-COY-IND = 'S'                                     <D9607>
                  MOVE SPACES          TO WSAA-COMPANY                  <D9607>
              END-IF .                                                  <D9607>
                                                                        <SDAS>
      ****                                                      <>      <SDAS>
      **** If accessing this screen within SDA, use the company passed  <SDAS>
      **** through in additional fields rather than the sign-on company <SDAS>
      **** which will be 0.                                     <>      <SDAS>
      ****                                                      <>      <SDAS>
                                                                        <SDAS>
           IF CLRN-COY-IND              = 'C'                           <SDAS>
               IF WSAA-SDA              = 'Y'                           <SDAS>
                   MOVE WSAA-SDA-COMPANY TO WSAA-COMPANY                <SDAS>
               ELSE                                                     <SDAS>
                   MOVE WSSP-COMPANY   TO WSAA-COMPANY                  <SDAS>
               END-IF                                                   <SDAS>
           END-IF.                                                      <SDAS>
                                                                        <SDAS>
      ****    IF CLRN-COY-IND = 'C'                             <SDAS>  <D9607>>
      ****        MOVE WSSP-COMPANY    TO WSAA-COMPANY          <SDAS>  <D9607>>
      ****    END-IF.                                           <SDAS>  <D9607>>
      *                                                                 <D9607>
           IF  WSAA-ROLE          NOT  =  SPACES
               MOVE SPACES             TO CLRRWIN-PARAMS
                                          S2473-SLT-OUT (PR)
      ****     MOVE WSSP-COMPANY       TO CLRRWIN-FORECOY               <D9607>
               MOVE WSAA-COMPANY       TO CLRRWIN-FORECOY               <D9607>
               MOVE WSAA-STORE-CLNTNUM TO CLRRWIN-CLNTNUM
               MOVE WSAA-ROLE          TO CLRRWIN-CLRRROLE
               MOVE CLRRWINREC         TO CLRRWIN-FORMAT
               MOVE BEGN               TO CLRRWIN-FUNCTION
      *
               CALL 'CLRRWINIO'     USING CLRRWIN-PARAMS
      *
               IF  (CLRRWIN-STATUZ NOT  =  O-K ) AND
                   (CLRRWIN-STATUZ NOT  =  ENDP)
                   MOVE CLRRWIN-PARAMS  TO SYSR-PARAMS
      *****        GO TO  600-FATAL-ERROR                       <D509CS>
                   PERFORM 600-FATAL-ERROR                              <D509CS>
               END-IF
      *
              MOVE 'N'                 TO REC-FOUND
      *
      *     . Check that a match for client, role and sign on company
      *       exists.
      *
              IF  (CLRRWIN-CLNTNUM    = WSAA-STORE-CLNTNUM) AND
                  (CLRRWIN-STATUZ NOT = ENDP)               AND
                  (CLRRWIN-CLRRROLE   = WSAA-ROLE)          AND
      *****       (CLRRWIN-FORECOY    = WSSP-COMPANY)                   <D9607>
                  (CLRRWIN-FORECOY    = WSAA-COMPANY)                   <D9607>
      *
                  MOVE 'Y'          TO REC-FOUND
              ELSE
                  MOVE 'N' TO WSAA-RECORD-ADDED
                  GO TO 5170-NEXT-REC
              END-IF
      *
           END-IF.
      *
      *
           MOVE SADD                   TO SCRN-FUNCTION.
           MOVE 'Y'                    TO WSAA-RECORD-ADDED.
      *
           CALL 'S2473IO' USING SCRN-SCREEN-PARAMS
                                S2473-DATA-AREA
                                S2473-SUBFILE-AREA.
           IF SCRN-STATUZ              NOT = O-K
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR.

      *
       5170-NEXT-REC.
      *

      *
      *    Find next record - schema used depends on role
      *
           MOVE WSAA-ROLE              TO CLRL-VALUE.
           IF WSAA-ROLE                = SPACES
              GO TO 5750-NEXT-CLTN
           ELSE
           IF CLRL-FLAG02
              GO TO 5200-NEXT-CLTA
           ELSE
           IF CLRL-FLAG05
              GO TO 5250-NEXT-CLTB
           ELSE
           IF CLRL-FLAG18                                               <GP3IND>
              GO TO 5300-NEXT-CLOW                                      <GP3IND>
           ELSE                                                         <GP3IND>
           IF CLRL-FLAG19                                               <GP3IND>
              GO TO 5350-NEXT-CLPE                                      <GP3IND>
           ELSE                                                         <GP3IND>
           IF CLRL-FLAG20                                               <GP3IND>
              GO TO 5400-NEXT-CLPY                                      <GP3IND>
           ELSE                                                         <GP3IND>
      **** IF CLRL-FLAG28                                       <V62F05><FSA552>
      ****    GO TO 5450-NEXT-CLGA                              <V62F05><FSA552>
      **** ELSE                                                 <V62F05><FSA552>
           IF CLRL-FLAG29                                               <FSA552>
              GO TO 5500-NEXT-CLGC                                      <FSA552>
           ELSE                                                         <FSA552>
           IF CLRL-FLAG32                                               <V76F13>
              GO TO 5600-NEXT-CLCC                                      <V76F13>
           ELSE                                                         <V76F13>
              GO TO 5750-NEXT-CLTN.
      ******
       5200-NEXT-CLTA.
                                                                        <D9607>
           IF WSAA-SRHLIMIT           >=  WSSP-SRHLIMIT                 <D9607>
              MOVE ENDP                TO WSAA-FILE-STATUZ              <D9607>
              GO TO 5800-CONT                                           <D9607>
           END-IF.                                                      <D9607>
                                                                        <D9607>
           MOVE NEXTR                  TO CLTA-FUNCTION.
           CALL 'CLTAIO' USING CLTA-PARAMS.
           IF CLTA-STATUZ              NOT = O-K
                                       AND ENDP
              MOVE CLTA-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.
      *
           IF CLTA-CLNTCOY NOT = WSSP-FSUCO
              MOVE ENDP                TO CLTA-STATUZ.
      *
           IF CLTA-STATUZ              = ENDP
              MOVE CLTA-STATUZ         TO WSAA-FILE-STATUZ
              GO TO 5800-CONT.
      *
      **** MOVE CLTA-SURNAME           TO WSCC-SURNAME.                 <021>
      **** MOVE CLTA-GIVNAME           TO WSCC-GIVNAME.                 <021>
      ****                                                              <021>
           MOVE CLTA-CLNTPFX           TO CLTS-CLNTPFX.                 <021>
           MOVE CLTA-CLNTCOY           TO CLTS-CLNTCOY.                 <021>
           MOVE CLTA-CLNTNUM           TO CLTS-CLNTNUM.                 <021>
                                                                        <021>
           PERFORM  B100-CLIENT-NAME.                                   <021>
                                                                        <021>
           MOVE CLTA-MIDDL01           TO WSCC-MIDDL01.
           MOVE CLTA-MIDDL02           TO WSCC-MIDDL02.
           MOVE CLTA-CLTTYPE           TO WSCC-CLTTYPE.
           MOVE CLTA-CLTSTAT           TO WSCC-CLTSTAT.
           MOVE CLTA-SERVBRH           TO WSCC-SERVBRH.
           MOVE CLTA-CLTSEX            TO WSCC-CLTSEX.
           MOVE CLTA-CLTDOB            TO WSCC-CLTDOB.
           MOVE CLTA-CLTADDR01         TO WSCC-CLTADDR01.
           MOVE CLTA-CLTADDR02         TO WSCC-CLTADDR02.               <024>
           MOVE CLTA-CLTADDR03         TO WSCC-CLTADDR03.               <024>
           MOVE CLTA-CLTADDR04         TO WSCC-CLTADDR04.               <024>
           MOVE CLTA-CLTADDR05         TO WSCC-CLTADDR05.               <024>
           MOVE CLTA-CLTIND            TO WSCC-CLTIND.
           MOVE CLTA-SECUITYNO         TO WSCC-SECUITYNO.               <CAS1.0>
           MOVE CLTA-ROLEFLAGS         TO WSCC-ROLEFLAGS.
           MOVE SPACES                 TO WSAA-CONGIV01.                <FSA899>
                                                                        <D9607>
           IF WSCC-CLTTYPE = WSSP-CLTTYPE-F                             <D9607>
           OR WSSP-CLTTYPE-F = SPACES                                   <D9607>
              ADD 1                    TO WSAA-SRHLIMIT                 <D9607>
           END-IF.                                                      <D9607>
      *
           PERFORM 7000-CHECK-NAME.
           IF WSAA-MATCH               = 'N'
              GO TO 5200-NEXT-CLTA.
           PERFORM 6000-CHECK-MATCH.
           IF WSAA-MATCH               = 'N'
              GO TO 5200-NEXT-CLTA.
           MOVE CLTA-CLNTNUM           TO WSAA-STORE-CLNTNUM.
           PERFORM 9000-CHECK-ROLE.
           IF WSAA-MATCH               = 'N'
              GO TO 5200-NEXT-CLTA
           ELSE
              MOVE CLTA-STATUZ         TO WSAA-FILE-STATUZ
              GO TO 5800-CONT.
      ******
       5250-NEXT-CLTB.
                                                                        <D9607>
           IF WSAA-SRHLIMIT           >=  WSSP-SRHLIMIT                 <D9607>
              MOVE ENDP                TO WSAA-FILE-STATUZ              <D9607>
              GO TO 5800-CONT                                           <D9607>
           END-IF.                                                      <D9607>
                                                                        <D9607>
           MOVE NEXTR                  TO CLTB-FUNCTION.
           CALL 'CLTBIO' USING CLTB-PARAMS.
           IF CLTB-STATUZ              NOT = O-K
                                       AND ENDP
              MOVE CLTB-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.
      *
           IF CLTB-CLNTCOY NOT = WSSP-FSUCO
              MOVE ENDP                TO CLTB-STATUZ.
      *
           IF CLTB-STATUZ              = ENDP
              MOVE CLTB-STATUZ         TO WSAA-FILE-STATUZ
              GO TO 5800-CONT.
      *
           IF CLTB-CLTIND = 'D'
              GO TO 5250-NEXT-CLTB.
      *
      **** MOVE CLTB-SURNAME           TO WSCC-SURNAME.                 <021>
      **** MOVE CLTB-GIVNAME           TO WSCC-GIVNAME.                 <021>
      ****                                                              <021>
           MOVE CLTB-CLNTPFX           TO CLTS-CLNTPFX.                 <021>
           MOVE CLTB-CLNTCOY           TO CLTS-CLNTCOY.                 <021>
           MOVE CLTB-CLNTNUM           TO CLTS-CLNTNUM.                 <021>
                                                                        <021>
           PERFORM  B100-CLIENT-NAME.                                   <021>
                                                                        <021>
           MOVE CLTB-MIDDL01           TO WSCC-MIDDL01.
           MOVE CLTB-MIDDL02           TO WSCC-MIDDL02.
           MOVE CLTB-CLTTYPE           TO WSCC-CLTTYPE.
           MOVE CLTB-CLTSTAT           TO WSCC-CLTSTAT.
           MOVE CLTB-SERVBRH           TO WSCC-SERVBRH.
           MOVE CLTB-CLTSEX            TO WSCC-CLTSEX.
           MOVE CLTB-CLTDOB            TO WSCC-CLTDOB.
           MOVE CLTB-CLTADDR01         TO WSCC-CLTADDR01.
           MOVE CLTB-CLTADDR02         TO WSCC-CLTADDR02.               <024>
           MOVE CLTB-CLTADDR03         TO WSCC-CLTADDR03.               <024>
           MOVE CLTB-CLTADDR04         TO WSCC-CLTADDR04.               <024>
           MOVE CLTB-CLTADDR05         TO WSCC-CLTADDR05.               <024>
           MOVE CLTB-CLTIND            TO WSCC-CLTIND.
           MOVE CLTB-SECUITYNO         TO WSCC-SECUITYNO.               <CAS1.0>
           MOVE CLTB-ROLEFLAGS         TO WSCC-ROLEFLAGS.
           MOVE SPACES                 TO WSAA-CONGIV01.                <FSA899>

           IF WSCC-CLTTYPE = WSSP-CLTTYPE-F                             <D9607>
           OR WSSP-CLTTYPE-F = SPACES                                   <D9607>
              ADD 1                    TO WSAA-SRHLIMIT                 <D9607>
           END-IF.                                                      <D9607>
      *
           PERFORM 7000-CHECK-NAME.
           IF WSAA-MATCH               = 'N'
              GO TO 5250-NEXT-CLTB.
           PERFORM 6000-CHECK-MATCH.
           IF WSAA-MATCH               = 'N'
              GO TO 5250-NEXT-CLTB.
           MOVE CLTB-CLNTNUM           TO WSAA-STORE-CLNTNUM.
           PERFORM 9000-CHECK-ROLE.
           IF WSAA-MATCH               = 'N'
              GO TO 5250-NEXT-CLTB
           ELSE
              MOVE CLTB-STATUZ         TO WSAA-FILE-STATUZ
              GO TO 5800-CONT.
      ******
       5300-NEXT-CLOW.                                                  <GP3IND>
                                                                        <GP3IND>
           IF WSAA-SRHLIMIT           >=  WSSP-SRHLIMIT                 <GP3IND>
              MOVE ENDP                TO WSAA-FILE-STATUZ              <GP3IND>
              GO TO 5800-CONT                                           <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
           MOVE NEXTR                  TO CLOW-FUNCTION.                <GP3IND>
           CALL 'CLOWIO'            USING CLOW-PARAMS.                  <GP3IND>
           IF CLOW-STATUZ          NOT =  O-K AND ENDP                  <GP3IND>
              MOVE CLOW-STATUZ         TO SYSR-STATUZ                   <GP3IND>
              MOVE CLOW-PARAMS         TO SYSR-PARAMS                   <GP3IND>
              PERFORM 600-FATAL-ERROR                                   <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
           IF CLOW-CLNTCOY         NOT =  WSSP-FSUCO                    <GP3IND>
              MOVE ENDP                TO CLOW-STATUZ                   <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
           IF CLOW-STATUZ              =  ENDP                          <GP3IND>
              MOVE CLOW-STATUZ         TO WSAA-FILE-STATUZ              <GP3IND>
              GO TO 5800-CONT                                           <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
           MOVE CLOW-CLNTPFX           TO CLTS-CLNTPFX.                 <GP3IND>
           MOVE CLOW-CLNTCOY           TO CLTS-CLNTCOY.                 <GP3IND>
           MOVE CLOW-CLNTNUM           TO CLTS-CLNTNUM.                 <GP3IND>
                                                                        <GP3IND>
           PERFORM B100-CLIENT-NAME.                                    <GP3IND>
                                                                        <GP3IND>
           MOVE CLOW-MIDDL01           TO WSCC-MIDDL01.                 <GP3IND>
           MOVE CLOW-MIDDL02           TO WSCC-MIDDL02.                 <GP3IND>
           MOVE CLOW-CLTTYPE           TO WSCC-CLTTYPE.                 <GP3IND>
           MOVE CLOW-CLTSTAT           TO WSCC-CLTSTAT.                 <GP3IND>
           MOVE CLOW-SERVBRH           TO WSCC-SERVBRH.                 <GP3IND>
           MOVE CLOW-CLTSEX            TO WSCC-CLTSEX.                  <GP3IND>
           MOVE CLOW-CLTDOB            TO WSCC-CLTDOB.                  <GP3IND>
           MOVE CLOW-CLTADDR01         TO WSCC-CLTADDR01.               <GP3IND>
           MOVE CLOW-CLTADDR02         TO WSCC-CLTADDR02.               <GP3IND>
           MOVE CLOW-CLTADDR03         TO WSCC-CLTADDR03.               <GP3IND>
           MOVE CLOW-CLTADDR04         TO WSCC-CLTADDR04.               <GP3IND>
           MOVE CLOW-CLTADDR05         TO WSCC-CLTADDR05.               <GP3IND>
           MOVE CLOW-CLTIND            TO WSCC-CLTIND.                  <GP3IND>
           MOVE CLOW-SECUITYNO         TO WSCC-SECUITYNO.               <GP3IND>
           MOVE CLOW-ROLEFLAGS         TO WSCC-ROLEFLAGS.               <GP3IND>
           MOVE SPACES                 TO WSAA-CONGIV01.                <FSA899>
                                                                        <GP3IND>
           IF WSCC-CLTTYPE             =  WSSP-CLTTYPE-F                <GP3IND>
           OR WSSP-CLTTYPE-F           =  SPACES                        <GP3IND>
              ADD 1                    TO WSAA-SRHLIMIT                 <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
           PERFORM 7000-CHECK-NAME.                                     <GP3IND>
           IF WSAA-MATCH               =  'N'                           <GP3IND>
              GO TO 5300-NEXT-CLOW                                      <GP3IND>
           END-IF.                                                      <GP3IND>
           PERFORM 6000-CHECK-MATCH.                                    <GP3IND>
           IF WSAA-MATCH               =  'N'                           <GP3IND>
              GO TO 5300-NEXT-CLOW                                      <GP3IND>
           END-IF.                                                      <GP3IND>
           MOVE CLOW-CLNTNUM           TO WSAA-STORE-CLNTNUM.           <GP3IND>
           PERFORM 9000-CHECK-ROLE.                                     <GP3IND>
           IF WSAA-MATCH               =  'N'                           <GP3IND>
              GO TO 5300-NEXT-CLOW                                      <GP3IND>
           ELSE                                                         <GP3IND>
              MOVE CLOW-STATUZ         TO WSAA-FILE-STATUZ              <GP3IND>
              GO TO 5800-CONT                                           <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
      ******                                                            <GP3IND>
       5350-NEXT-CLPE.                                                  <GP3IND>
                                                                        <GP3IND>
           IF WSAA-SRHLIMIT           >=  WSSP-SRHLIMIT                 <GP3IND>
              MOVE ENDP                TO WSAA-FILE-STATUZ              <GP3IND>
              GO TO 5800-CONT                                           <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
           MOVE NEXTR                  TO CLPE-FUNCTION.                <GP3IND>
           CALL 'CLPEIO'            USING CLPE-PARAMS.                  <GP3IND>
           IF CLPE-STATUZ          NOT =  O-K AND ENDP                  <GP3IND>
              MOVE CLPE-STATUZ         TO SYSR-STATUZ                   <GP3IND>
              MOVE CLPE-PARAMS         TO SYSR-PARAMS                   <GP3IND>
              PERFORM 600-FATAL-ERROR                                   <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
           IF CLPE-CLNTCOY         NOT =  WSSP-FSUCO                    <GP3IND>
              MOVE ENDP                TO CLPE-STATUZ                   <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
           IF CLPE-STATUZ              =  ENDP                          <GP3IND>
              MOVE CLPE-STATUZ         TO WSAA-FILE-STATUZ              <GP3IND>
              GO TO 5800-CONT                                           <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
           MOVE CLPE-CLNTPFX           TO CLTS-CLNTPFX.                 <GP3IND>
           MOVE CLPE-CLNTCOY           TO CLTS-CLNTCOY.                 <GP3IND>
           MOVE CLPE-CLNTNUM           TO CLTS-CLNTNUM.                 <GP3IND>
                                                                        <GP3IND>
           PERFORM B100-CLIENT-NAME.                                    <GP3IND>
                                                                        <GP3IND>
           MOVE CLPE-MIDDL01           TO WSCC-MIDDL01.                 <GP3IND>
           MOVE CLPE-MIDDL02           TO WSCC-MIDDL02.                 <GP3IND>
           MOVE CLPE-CLTTYPE           TO WSCC-CLTTYPE.                 <GP3IND>
           MOVE CLPE-CLTSTAT           TO WSCC-CLTSTAT.                 <GP3IND>
           MOVE CLPE-SERVBRH           TO WSCC-SERVBRH.                 <GP3IND>
           MOVE CLPE-CLTSEX            TO WSCC-CLTSEX.                  <GP3IND>
           MOVE CLPE-CLTDOB            TO WSCC-CLTDOB.                  <GP3IND>
           MOVE CLPE-CLTADDR01         TO WSCC-CLTADDR01.               <GP3IND>
           MOVE CLPE-CLTADDR02         TO WSCC-CLTADDR02.               <GP3IND>
           MOVE CLPE-CLTADDR03         TO WSCC-CLTADDR03.               <GP3IND>
           MOVE CLPE-CLTADDR04         TO WSCC-CLTADDR04.               <GP3IND>
           MOVE CLPE-CLTADDR05         TO WSCC-CLTADDR05.               <GP3IND>
           MOVE CLPE-CLTIND            TO WSCC-CLTIND.                  <GP3IND>
           MOVE CLPE-SECUITYNO         TO WSCC-SECUITYNO.               <GP3IND>
           MOVE CLPE-ROLEFLAGS         TO WSCC-ROLEFLAGS.               <GP3IND>
           MOVE SPACES                 TO WSAA-CONGIV01.                <FSA899>
                                                                        <GP3IND>
           IF WSCC-CLTTYPE             =  WSSP-CLTTYPE-F                <GP3IND>
           OR WSSP-CLTTYPE-F           =  SPACES                        <GP3IND>
              ADD 1                    TO WSAA-SRHLIMIT                 <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
           PERFORM 7000-CHECK-NAME.                                     <GP3IND>
           IF WSAA-MATCH               =  'N'                           <GP3IND>
              GO TO 5350-NEXT-CLPE                                      <GP3IND>
           END-IF.                                                      <GP3IND>
           PERFORM 6000-CHECK-MATCH.                                    <GP3IND>
           IF WSAA-MATCH               =  'N'                           <GP3IND>
              GO TO 5350-NEXT-CLPE                                      <GP3IND>
           END-IF.                                                      <GP3IND>
           MOVE CLPE-CLNTNUM           TO WSAA-STORE-CLNTNUM.           <GP3IND>
           PERFORM 9000-CHECK-ROLE.                                     <GP3IND>
           IF WSAA-MATCH               =  'N'                           <GP3IND>
              GO TO 5350-NEXT-CLPE                                      <GP3IND>
           ELSE                                                         <GP3IND>
              MOVE CLPE-STATUZ         TO WSAA-FILE-STATUZ              <GP3IND>
              GO TO 5800-CONT                                           <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
      ******                                                            <GP3IND>
       5400-NEXT-CLPY.                                                  <GP3IND>
                                                                        <GP3IND>
           IF WSAA-SRHLIMIT           >=  WSSP-SRHLIMIT                 <GP3IND>
              MOVE ENDP                TO WSAA-FILE-STATUZ              <GP3IND>
              GO TO 5800-CONT                                           <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
           MOVE NEXTR                  TO CLPY-FUNCTION.                <GP3IND>
           CALL 'CLPYIO'            USING CLPY-PARAMS.                  <GP3IND>
           IF CLPY-STATUZ          NOT =  O-K AND ENDP                  <GP3IND>
              MOVE CLPY-STATUZ         TO SYSR-STATUZ                   <GP3IND>
              MOVE CLPY-PARAMS         TO SYSR-PARAMS                   <GP3IND>
              PERFORM 600-FATAL-ERROR                                   <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
           IF CLPY-CLNTCOY         NOT =  WSSP-FSUCO                    <GP3IND>
              MOVE ENDP                TO CLPY-STATUZ                   <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
           IF CLPY-STATUZ              =  ENDP                          <GP3IND>
              MOVE CLPY-STATUZ         TO WSAA-FILE-STATUZ              <GP3IND>
              GO TO 5800-CONT                                           <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
           MOVE CLPY-CLNTPFX           TO CLTS-CLNTPFX.                 <GP3IND>
           MOVE CLPY-CLNTCOY           TO CLTS-CLNTCOY.                 <GP3IND>
           MOVE CLPY-CLNTNUM           TO CLTS-CLNTNUM.                 <GP3IND>
                                                                        <GP3IND>
           PERFORM B100-CLIENT-NAME.                                    <GP3IND>
                                                                        <GP3IND>
           MOVE CLPY-MIDDL01           TO WSCC-MIDDL01.                 <GP3IND>
           MOVE CLPY-MIDDL02           TO WSCC-MIDDL02.                 <GP3IND>
           MOVE CLPY-CLTTYPE           TO WSCC-CLTTYPE.                 <GP3IND>
           MOVE CLPY-CLTSTAT           TO WSCC-CLTSTAT.                 <GP3IND>
           MOVE CLPY-SERVBRH           TO WSCC-SERVBRH.                 <GP3IND>
           MOVE CLPY-CLTSEX            TO WSCC-CLTSEX.                  <GP3IND>
           MOVE CLPY-CLTDOB            TO WSCC-CLTDOB.                  <GP3IND>
           MOVE CLPY-CLTADDR01         TO WSCC-CLTADDR01.               <GP3IND>
           MOVE CLPY-CLTADDR02         TO WSCC-CLTADDR02.               <GP3IND>
           MOVE CLPY-CLTADDR03         TO WSCC-CLTADDR03.               <GP3IND>
           MOVE CLPY-CLTADDR04         TO WSCC-CLTADDR04.               <GP3IND>
           MOVE CLPY-CLTADDR05         TO WSCC-CLTADDR05.               <GP3IND>
           MOVE CLPY-CLTIND            TO WSCC-CLTIND.                  <GP3IND>
           MOVE CLPY-SECUITYNO         TO WSCC-SECUITYNO.               <GP3IND>
           MOVE CLPY-ROLEFLAGS         TO WSCC-ROLEFLAGS.               <GP3IND>
           MOVE SPACES                 TO WSAA-CONGIV01.                <FSA899>
                                                                        <GP3IND>
           IF WSCC-CLTTYPE             =  WSSP-CLTTYPE-F                <GP3IND>
           OR WSSP-CLTTYPE-F           =  SPACES                        <GP3IND>
              ADD 1                    TO WSAA-SRHLIMIT                 <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
           PERFORM 7000-CHECK-NAME.                                     <GP3IND>
           IF WSAA-MATCH               =  'N'                           <GP3IND>
              GO TO 5400-NEXT-CLPY                                      <GP3IND>
           END-IF.                                                      <GP3IND>
           PERFORM 6000-CHECK-MATCH.                                    <GP3IND>
           IF WSAA-MATCH               =  'N'                           <GP3IND>
              GO TO 5400-NEXT-CLPY                                      <GP3IND>
           END-IF.                                                      <GP3IND>
           MOVE CLPY-CLNTNUM           TO WSAA-STORE-CLNTNUM.           <GP3IND>
           PERFORM 9000-CHECK-ROLE.                                     <GP3IND>
           IF WSAA-MATCH               =  'N'                           <GP3IND>
              GO TO 5400-NEXT-CLPY                                      <GP3IND>
           ELSE                                                         <GP3IND>
              MOVE CLPY-STATUZ         TO WSAA-FILE-STATUZ              <GP3IND>
              GO TO 5800-CONT                                           <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
                                                                        <FSA552>
      *5450-NEXT-CLGA.                                          <V62F05><FSA552>
      ****                                                      <V62F05><FSA552>
      **** IF WSAA-SRHLIMIT           >=  WSSP-SRHLIMIT         <V62F05><FSA552>
      ****    MOVE ENDP                TO WSAA-FILE-STATUZ      <V62F05><FSA552>
      ****    GO TO 5800-CONT                                   <V62F05><FSA552>
      **** END-IF.                                              <V62F05><FSA552>
      ****                                                      <V62F05><FSA552>
      **** MOVE NEXTR                  TO CLGA-FUNCTION.        <V62F05><FSA552>
      **** CALL 'CLGAIO'            USING CLGA-PARAMS.          <V62F05><FSA552>
      **** IF CLGA-STATUZ          NOT =  O-K AND ENDP          <V62F05><FSA552>
      ****    MOVE CLGA-STATUZ         TO SYSR-STATUZ           <V62F05><FSA552>
      ****    MOVE CLGA-PARAMS         TO SYSR-PARAMS           <V62F05><FSA552>
      ****    PERFORM 600-FATAL-ERROR.                          <V62F05><FSA552>
      ****                                                      <V62F05><FSA552>
      **** IF CLGA-CLNTCOY         NOT =  WSSP-FSUCO            <V62F05><FSA552>
      ****    MOVE ENDP                TO CLGA-STATUZ.          <V62F05><FSA552>
      ****                                                      <V62F05><FSA552>
      **** IF CLGA-STATUZ              =  ENDP                  <V62F05><FSA552>
      ****    MOVE CLGA-STATUZ         TO WSAA-FILE-STATUZ      <V62F05><FSA552>
      ****    GO TO 5800-CONT.                                  <V62F05><FSA552>
      ****                                                      <V62F05><FSA552>
      **** MOVE CLGA-CLNTPFX           TO CLTS-CLNTPFX.         <V62F05><FSA552>
      **** MOVE CLGA-CLNTCOY           TO CLTS-CLNTCOY.         <V62F05><FSA552>
      **** MOVE CLGA-CLNTNUM           TO CLTS-CLNTNUM.         <V62F05><FSA552>
      ****                                                      <V62F05><FSA552>
      **** PERFORM B100-CLIENT-NAME.                            <V62F05><FSA552>
      ****                                                      <V62F05><FSA552>
      **** MOVE CLGA-MIDDL01           TO WSCC-MIDDL01.         <V62F05><FSA552>
      **** MOVE CLGA-MIDDL02           TO WSCC-MIDDL02.         <V62F05><FSA552>
      **** MOVE CLGA-CLTTYPE           TO WSCC-CLTTYPE.         <V62F05><FSA552>
      **** MOVE CLGA-CLTSTAT           TO WSCC-CLTSTAT.         <V62F05><FSA552>
      **** MOVE CLGA-SERVBRH           TO WSCC-SERVBRH.         <V62F05><FSA552>
      **** MOVE CLGA-CLTSEX            TO WSCC-CLTSEX.          <V62F05><FSA552>
      **** MOVE CLGA-CLTDOB            TO WSCC-CLTDOB.          <V62F05><FSA552>
      **** MOVE CLGA-CLTADDR01         TO WSCC-CLTADDR01.       <V62F05><FSA552>
      **** MOVE CLGA-CLTADDR02         TO WSCC-CLTADDR02.       <V62F05><FSA552>
      **** MOVE CLGA-CLTADDR03         TO WSCC-CLTADDR03.       <V62F05><FSA552>
      **** MOVE CLGA-CLTADDR04         TO WSCC-CLTADDR04.       <V62F05><FSA552>
      **** MOVE CLGA-CLTADDR05         TO WSCC-CLTADDR05.       <V62F05><FSA552>
      **** MOVE CLGA-CLTIND            TO WSCC-CLTIND.          <V62F05><FSA552>
      **** MOVE CLGA-SECUITYNO         TO WSCC-SECUITYNO.       <V62F05><FSA552>
      **** MOVE CLGA-ROLEFLAGS         TO WSCC-ROLEFLAGS.       <V62F05><FSA552>
      **** MOVE SPACES                 TO WSAA-CONGIV01.        <V62F05><FSA899>
      ****                                                      <V62F05><FSA552>
      **** IF WSCC-CLTTYPE             =  WSSP-CLTTYPE-F        <V62F05><FSA552>
      **** OR WSSP-CLTTYPE-F           =  SPACES                <V62F05><FSA552>
      ****    ADD 1                    TO WSAA-SRHLIMIT         <V62F05><FSA552>
      **** END-IF.                                              <V62F05><FSA552>
      ****                                                      <V62F05><FSA552>
      **** PERFORM 7000-CHECK-NAME.                             <V62F05><FSA552>
      **** IF WSAA-MATCH               =  'N'                   <V62F05><FSA552>
      ****    GO TO 5450-NEXT-CLGA.                             <V62F05><FSA552>
      **** PERFORM 6000-CHECK-MATCH.                            <V62F05><FSA552>
      **** IF WSAA-MATCH               =  'N'                   <V62F05><FSA552>
      ****    GO TO 5450-NEXT-CLGA.                             <V62F05><FSA552>
      **** MOVE CLGA-CLNTNUM           TO WSAA-STORE-CLNTNUM.   <V62F05><FSA552>
      **** PERFORM 9000-CHECK-ROLE.                             <V62F05><FSA552>
      **** IF WSAA-MATCH               =  'N'                   <V62F05><FSA552>
      ****    GO TO 5450-NEXT-CLGA                              <V62F05><FSA552>
      **** ELSE                                                 <V62F05><FSA552>
      ****    MOVE CLGA-STATUZ         TO WSAA-FILE-STATUZ      <V62F05><FSA552>
      ****    GO TO 5800-CONT.                                  <V62F05><FSA552>
      ******                                                            <FSA552>
       5500-NEXT-CLGC.                                                  <FSA552>
                                                                        <FSA552>
           IF WSAA-SRHLIMIT           >=  WSSP-SRHLIMIT                 <FSA552>
              MOVE ENDP                TO WSAA-FILE-STATUZ              <FSA552>
              GO TO 5800-CONT                                           <FSA552>
           END-IF.                                                      <FSA552>
                                                                        <FSA552>
           MOVE NEXTR                  TO CLGC-FUNCTION.                <FSA552>
           CALL 'CLGCIO'            USING CLGC-PARAMS.                  <FSA552>
           IF CLGC-STATUZ          NOT =  O-K AND ENDP                  <FSA552>
              MOVE CLGC-STATUZ         TO SYSR-STATUZ                   <FSA552>
              MOVE CLGC-PARAMS         TO SYSR-PARAMS                   <FSA552>
              PERFORM 600-FATAL-ERROR.                                  <FSA552>
                                                                        <FSA552>
           IF CLGC-CLNTCOY         NOT =  WSSP-FSUCO                    <FSA552>
              MOVE ENDP                TO CLGC-STATUZ.                  <FSA552>
                                                                        <FSA552>
           IF CLGC-STATUZ              =  ENDP                          <FSA552>
              MOVE CLGC-STATUZ         TO WSAA-FILE-STATUZ              <FSA552>
              GO TO 5800-CONT.                                          <FSA552>
                                                                        <FSA552>
           MOVE CLGC-CLNTPFX           TO CLTS-CLNTPFX.                 <FSA552>
           MOVE CLGC-CLNTCOY           TO CLTS-CLNTCOY.                 <FSA552>
           MOVE CLGC-CLNTNUM           TO CLTS-CLNTNUM.                 <FSA552>
                                                                        <FSA552>
           PERFORM B100-CLIENT-NAME.                                    <FSA552>
                                                                        <FSA552>
           MOVE CLGC-MIDDL01           TO WSCC-MIDDL01.                 <FSA552>
           MOVE CLGC-MIDDL02           TO WSCC-MIDDL02.                 <FSA552>
           MOVE CLGC-CLTTYPE           TO WSCC-CLTTYPE.                 <FSA552>
           MOVE CLGC-CLTSTAT           TO WSCC-CLTSTAT.                 <FSA552>
           MOVE CLGC-SERVBRH           TO WSCC-SERVBRH.                 <FSA552>
           MOVE CLGC-CLTSEX            TO WSCC-CLTSEX.                  <FSA552>
           MOVE CLGC-CLTDOB            TO WSCC-CLTDOB.                  <FSA552>
           MOVE CLGC-CLTADDR01         TO WSCC-CLTADDR01.               <FSA552>
           MOVE CLGC-CLTADDR02         TO WSCC-CLTADDR02.               <FSA552>
           MOVE CLGC-CLTADDR03         TO WSCC-CLTADDR03.               <FSA552>
           MOVE CLGC-CLTADDR04         TO WSCC-CLTADDR04.               <FSA552>
           MOVE CLGC-CLTADDR05         TO WSCC-CLTADDR05.               <FSA552>
           MOVE CLGC-CLTIND            TO WSCC-CLTIND.                  <FSA552>
           MOVE CLGC-SECUITYNO         TO WSCC-SECUITYNO.               <FSA552>
           MOVE CLGC-ROLEFLAGS         TO WSCC-ROLEFLAGS.               <FSA552>
           MOVE SPACES                 TO WSAA-CONGIV01.                <FSA899>
                                                                        <FSA552>
           IF WSCC-CLTTYPE             =  WSSP-CLTTYPE-F                <FSA552>
           OR WSSP-CLTTYPE-F           =  SPACES                        <FSA552>
              ADD 1                    TO WSAA-SRHLIMIT                 <FSA552>
           END-IF.                                                      <FSA552>
                                                                        <FSA552>
           PERFORM 7000-CHECK-NAME.                                     <FSA552>
           IF WSAA-MATCH               =  'N'                           <FSA552>
              GO TO 5500-NEXT-CLGC.                                     <FSA552>
           PERFORM 6000-CHECK-MATCH.                                    <FSA552>
           IF WSAA-MATCH               =  'N'                           <FSA552>
              GO TO 5500-NEXT-CLGC.                                     <FSA552>
           MOVE CLGC-CLNTNUM           TO WSAA-STORE-CLNTNUM.           <FSA552>
           PERFORM 9000-CHECK-ROLE.                                     <FSA552>
           IF WSAA-MATCH               =  'N'                           <FSA552>
              GO TO 5500-NEXT-CLGC                                      <FSA552>
           ELSE                                                         <FSA552>
              MOVE CLGC-STATUZ         TO WSAA-FILE-STATUZ              <FSA552>
              GO TO 5800-CONT.                                          <FSA552>
                                                                        <FSA552>
      ******                                                            <GP3IND>
       5600-NEXT-CLCC.                                                  <V76F13>
                                                                        <V76F13>
           IF WSAA-SRHLIMIT           >=  WSSP-SRHLIMIT                 <V76F13>
              MOVE ENDP                TO WSAA-FILE-STATUZ              <V76F13>
              GO TO 5800-CONT                                           <V76F13>
           END-IF.                                                      <V76F13>
                                                                        <V76F13>
           MOVE NEXTR                  TO CLCC-FUNCTION.                <V76F13>
           CALL 'CLCCIO'            USING CLCC-PARAMS.                  <V76F13>
           IF CLCC-STATUZ           NOT = O-K                           <V76F13>
                                      AND ENDP                          <V76F13>
              MOVE CLCC-STATUZ         TO SYSR-STATUZ                   <V76F13>
              MOVE CLCC-PARAMS         TO SYSR-PARAMS                   <V76F13>
              PERFORM 600-FATAL-ERROR                                   <V76F13>
           END-IF.                                                      <V76F13>
                                                                        <V76F13>
           IF CLCC-CLNTCOY          NOT = WSSP-FSUCO                    <V76F13>
              MOVE ENDP                TO CLCC-STATUZ                   <V76F13>
           END-IF.                                                      <V76F13>
                                                                        <V76F13>
           IF CLCC-STATUZ               = ENDP                          <V76F13>
              MOVE CLCC-STATUZ         TO WSAA-FILE-STATUZ              <V76F13>
              GO TO 5800-CONT                                           <V76F13>
           END-IF.                                                      <V76F13>
                                                                        <V76F13>
           IF CLCC-CLTIND               = 'D'                           <V76F13>
              GO TO 5600-NEXT-CLCC                                      <V76F13>
           END-IF.                                                      <V76F13>
                                                                        <V76F13>
           MOVE CLCC-CLNTPFX           TO CLTS-CLNTPFX.                 <V76F13>
           MOVE CLCC-CLNTCOY           TO CLTS-CLNTCOY.                 <V76F13>
           MOVE CLCC-CLNTNUM           TO CLTS-CLNTNUM.                 <V76F13>
                                                                        <V76F13>
           PERFORM  B100-CLIENT-NAME.                                   <V76F13>
                                                                        <V76F13>
           MOVE CLCC-MIDDL01           TO WSCC-MIDDL01.                 <V76F13>
           MOVE CLCC-MIDDL02           TO WSCC-MIDDL02.                 <V76F13>
           MOVE CLCC-CLTTYPE           TO WSCC-CLTTYPE.                 <V76F13>
           MOVE CLCC-CLTSTAT           TO WSCC-CLTSTAT.                 <V76F13>
           MOVE CLCC-SERVBRH           TO WSCC-SERVBRH.                 <V76F13>
           MOVE CLCC-CLTSEX            TO WSCC-CLTSEX.                  <V76F13>
           MOVE CLCC-CLTDOB            TO WSCC-CLTDOB.                  <V76F13>
           MOVE CLCC-CLTADDR01         TO WSCC-CLTADDR01.               <V76F13>
           MOVE CLCC-CLTADDR02         TO WSCC-CLTADDR02.               <V76F13>
           MOVE CLCC-CLTADDR03         TO WSCC-CLTADDR03.               <V76F13>
           MOVE CLCC-CLTADDR04         TO WSCC-CLTADDR04.               <V76F13>
           MOVE CLCC-CLTADDR05         TO WSCC-CLTADDR05.               <V76F13>
           MOVE CLCC-CLTIND            TO WSCC-CLTIND.                  <V76F13>
           MOVE CLCC-SECUITYNO         TO WSCC-SECUITYNO.               <V76F13>
           MOVE CLCC-ROLEFLAGS         TO WSCC-ROLEFLAGS.               <V76F13>
           MOVE SPACES                 TO WSAA-CONGIV01.                <V76F13>
                                                                        <V76F13>
           IF WSCC-CLTTYPE              = WSSP-CLTTYPE-F                <V76F13>
           OR WSSP-CLTTYPE-F            = SPACES                        <V76F13>
              ADD 1                    TO WSAA-SRHLIMIT                 <V76F13>
           END-IF.                                                      <V76F13>
                                                                        <V76F13>
           PERFORM 7000-CHECK-NAME.                                     <V76F13>
           IF WSAA-MATCH                = 'N'                           <V76F13>
              GO TO 5600-NEXT-CLCC                                      <V76F13>
           END-IF.                                                      <V76F13>
                                                                        <V76F13>
           PERFORM 6000-CHECK-MATCH.                                    <V76F13>
                                                                        <V76F13>
           IF WSAA-MATCH                = 'N'                           <V76F13>
              GO TO 5600-NEXT-CLCC                                      <V76F13>
           END-IF.                                                      <V76F13>
                                                                        <V76F13>
           MOVE CLCC-CLNTNUM           TO WSAA-STORE-CLNTNUM.           <V76F13>
           PERFORM 9000-CHECK-ROLE.                                     <V76F13>
                                                                        <V76F13>
           IF WSAA-MATCH                = 'N'                           <V76F13>
              GO TO 5600-NEXT-CLCC                                      <V76F13>
           ELSE                                                         <V76F13>
              MOVE CLCC-STATUZ         TO WSAA-FILE-STATUZ              <V76F13>
              GO TO 5800-CONT                                           <V76F13>
           END-IF.                                                      <V76F13>
      ********                                                  <V76F13><D9607>
       5750-NEXT-CLTN.                                                  <V76F13>
                                                                        <V76F13>
           IF WSAA-SRHLIMIT           >=  WSSP-SRHLIMIT                 <D9607>
              MOVE ENDP                TO WSAA-FILE-STATUZ              <D9607>
              GO TO 5800-CONT                                           <D9607>
           END-IF.                                                      <D9607>
                                                                        <D9607>
           MOVE NEXTR                  TO CLTN-FUNCTION.
           CALL 'CLTNIO' USING CLTN-PARAMS.
           IF CLTN-STATUZ              NOT = O-K
                                       AND ENDP
              MOVE CLTN-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.
      *
           IF CLTN-CLNTCOY NOT = WSSP-FSUCO
              MOVE ENDP                TO CLTN-STATUZ.
      *
           IF CLTN-STATUZ              = ENDP
              MOVE CLTN-STATUZ         TO WSAA-FILE-STATUZ
              GO TO 5800-CONT.
      *
      **** MOVE CLTN-SURNAME           TO WSCC-SURNAME.                 <021>
      **** MOVE CLTN-GIVNAME           TO WSCC-GIVNAME.                 <021>
      ****                                                              <021>
           MOVE CLTN-CLNTPFX           TO CLTS-CLNTPFX.                 <021>
           MOVE CLTN-CLNTCOY           TO CLTS-CLNTCOY.                 <021>
           MOVE CLTN-CLNTNUM           TO CLTS-CLNTNUM.                 <021>
                                                                        <021>
           PERFORM  B100-CLIENT-NAME.                                   <021>
                                                                        <021>
           MOVE CLTN-MIDDL01           TO WSCC-MIDDL01.
           MOVE CLTN-MIDDL02           TO WSCC-MIDDL02.
           MOVE CLTN-CLTTYPE           TO WSCC-CLTTYPE.
           MOVE CLTN-CLTSTAT           TO WSCC-CLTSTAT.
           MOVE CLTN-SERVBRH           TO WSCC-SERVBRH.
           MOVE CLTN-CLTSEX            TO WSCC-CLTSEX.
           MOVE CLTN-CLTDOB            TO WSCC-CLTDOB.
           MOVE CLTN-CLTADDR01         TO WSCC-CLTADDR01.
           MOVE CLTN-CLTADDR02         TO WSCC-CLTADDR02.               <023>
           MOVE CLTN-CLTADDR03         TO WSCC-CLTADDR03.               <023>
           MOVE CLTN-CLTADDR04         TO WSCC-CLTADDR04.               <023>
           MOVE CLTN-CLTADDR05         TO WSCC-CLTADDR05.               <023>
           MOVE CLTN-CLTIND            TO WSCC-CLTIND.
           MOVE CLTN-SECUITYNO         TO WSCC-SECUITYNO.               <CAS1.0>
           MOVE CLTN-ROLEFLAGS         TO WSCC-ROLEFLAGS.
           MOVE SPACES                 TO WSAA-CONGIV01.                <FSA899>

      *
           IF WSCC-CLTTYPE = WSSP-CLTTYPE-F                             <D9607>
           OR WSSP-CLTTYPE-F = SPACES                                   <D9607>
              ADD 1                    TO WSAA-SRHLIMIT                 <D9607>
           END-IF.                                                      <D9607>
                                                                        <D9607>
           PERFORM 7000-CHECK-NAME.
           IF WSAA-MATCH               = 'N'
              GO TO 5750-NEXT-CLTN.
           PERFORM 6000-CHECK-MATCH.
           IF WSAA-MATCH               = 'N'
              GO TO 5750-NEXT-CLTN.
           MOVE CLTN-CLNTNUM           TO WSAA-STORE-CLNTNUM.
           PERFORM 9000-CHECK-ROLE.
           IF WSAA-MATCH               = 'N'
              GO TO 5750-NEXT-CLTN
           ELSE
              MOVE CLTN-STATUZ         TO WSAA-FILE-STATUZ
              GO TO 5800-CONT.
      *******
       5800-CONT.

           IF (WSAA-RECORD-ADDED       = 'N')
              GO TO 5850-CHECK-STATUZ.

           MOVE 'N'                 TO WSAA-RECORD-ADDED.

      **** DIVIDE SCRN-SUBFILE-RRN BY 14 GIVING WSAA-NUMBER     <>      <SDAS>
      ****                               REMAINDER WSAA-REM.    <>      <SDAS>
                                                                        <SDAS>
           DIVIDE SCRN-SUBFILE-RRN BY S2473-SUBFILE-PAGE                <SDAS>
                  GIVING WSAA-NUMBER     REMAINDER WSAA-REM.            <SDAS>
                                                                        <SDAS>
           IF WSAA-REM = 0
              GO TO 5890-SET-MORE-SIGN.

      *
       5850-CHECK-STATUZ.
      *

           IF WSAA-FILE-STATUZ         NOT = ENDP
              GO TO 5100-WRITE-TO-SUBFILE.
      *
       5890-SET-MORE-SIGN.
           IF WSAA-FILE-STATUZ         NOT = ENDP
              MOVE 'Y'                 TO SCRN-SUBFILE-MORE
           ELSE
              MOVE SPACE               TO SCRN-SUBFILE-MORE.
      *
       5900-EXIT.
           EXIT.
      /
      *****************************************************************
      *     CHECK IF RECORD IS IN SPECIFIED RANGE
      *****************************************************************
       6000-CHECK-MATCH SECTION.
      *
       6010-CHECK-MATCH.
      *
           MOVE 'Y'                   TO WSAA-MATCH.
      *
           IF WSSP-CLTTYPE-F          NOT = SPACES AND
              WSSP-CLTTYPE-F          NOT = WSCC-CLTTYPE
               MOVE 'N'                TO WSAA-MATCH
               GO TO 6900-EXIT.
      *
           IF WSSP-CLTSTAT-F           NOT = SPACES AND
              WSSP-CLTSTAT-F           NOT = WSCC-CLTSTAT
               MOVE 'N'                TO WSAA-MATCH
               GO TO 6900-EXIT.
      *
           IF WSSP-SERVBRH-F           NOT = SPACES AND
              WSSP-SERVBRH-F           NOT = WSCC-SERVBRH
               MOVE 'N'                TO WSAA-MATCH
               GO TO 6900-EXIT.
      *
      **** IF WSCC-CLTIND              = 'D'                            <021>
      ****     MOVE 'N'                TO WSAA-MATCH                    <021>
      ****     GO TO 6900-EXIT.                                         <021>
      *
           IF WSCC-CLTTYPE            = 'C'
               GO TO 6015-CHECK-NAME.
                                                                        <A06461>
      ******************************************************            <A06461>
      * Convert WSCC fields (Givenname and 2 Middle Names) *            <A06461>
      * to upper case.                                     *            <A06461>
      ******************************************************            <A06461>
                                                                        <A06461>
           IF WSCC-GIVNAME = SPACES                                     <FSA899>
              MOVE SPACES              TO WSAA-CONGIV01                 <FSA899>
           END-IF.                                                      <FSA899>

           IF WSCC-GIVNAME NOT = SPACES                                 <A06461>
              MOVE WSCC-GIVNAME        TO WSAA-CONGIV01                 <A06461>
              MOVE WSCC-GIVNAME        TO WSAA-CONVERT                  <A06461>
              PERFORM B200-CONVERT                                      <A06461>
              MOVE WSAA-CONVERT        TO WSCC-GIVNAME                  <A06461>
           END-IF.                                                      <A06461>
      *                                                                 <A06461>
           IF WSCC-MIDDL01 NOT = SPACES                                 <A06461>
              MOVE WSCC-MIDDL01        TO WSAA-CONMID01                 <A06461>
              MOVE WSCC-MIDDL01        TO WSAA-CONVERT                  <A06461>
              PERFORM B200-CONVERT                                      <A06461>
              MOVE WSAA-CONVERT        TO WSCC-MIDDL01                  <A06461>
           END-IF.                                                      <A06461>
      *                                                                 <A06461>
           IF WSCC-MIDDL02 NOT = SPACES                                 <A06461>
              MOVE WSCC-MIDDL02        TO WSAA-CONMID02                 <A06461>
              MOVE WSCC-MIDDL02        TO WSAA-CONVERT                  <A06461>
              PERFORM B200-CONVERT                                      <A06461>
              MOVE WSAA-CONVERT        TO WSCC-MIDDL02                  <A06461>
           END-IF.                                                      <A06461>
                                                                        <A06461>
      *
      *    Given name
      *
           IF WSSP-EXACT               = 'Y'                            <CAS1.0>
           IF WSSP-GIVNAME-F           NOT = SPACES AND
              WSSP-GIVNAME-F           NOT = WSCC-GIVNAME
               MOVE 'N'               TO WSAA-MATCH
               MOVE WSAA-CONGIV01     TO WSCC-GIVNAME
               MOVE WSAA-CONMID01     TO WSCC-MIDDL01
               MOVE WSAA-CONMID02     TO WSCC-MIDDL02
               GO TO 6900-EXIT.
      *
      *    First middle name
      *
           IF WSSP-EXACT               = 'Y'                            <CAS1.0>
           IF WSSP-MIDDL01-F          NOT = SPACES AND
              WSSP-MIDDL01-F          NOT = WSCC-MIDDL01
               MOVE 'N'                TO WSAA-MATCH
               MOVE WSAA-CONGIV01     TO WSCC-GIVNAME
               MOVE WSAA-CONMID01     TO WSCC-MIDDL01
               MOVE WSAA-CONMID02     TO WSCC-MIDDL02
               GO TO 6900-EXIT.
      *
      *    Second middle name
      *
           IF WSSP-EXACT               = 'Y'                            <CAS1.0>
           IF WSSP-MIDDL02-F          NOT = SPACES AND
              WSSP-MIDDL02-F          NOT = WSCC-MIDDL02
               MOVE 'N'                TO WSAA-MATCH
               GO TO 6900-EXIT.

           MOVE WSAA-CONGIV01         TO WSCC-GIVNAME.
           MOVE WSAA-CONMID01         TO WSCC-MIDDL01.
           MOVE WSAA-CONMID02         TO WSCC-MIDDL02.
      *
      *    Check client sex
      *
           IF WSSP-CLTSEX-F            NOT = SPACES AND
              WSSP-CLTSEX-F            NOT = WSCC-CLTSEX
               MOVE 'N'                TO WSAA-MATCH
               GO TO 6900-EXIT.
      *
      *    Date of birth
      *
           IF WSSP-DOB01-F NOT = VRCM-MAX-DATE
           IF WSSP-DOB01-F        > WSCC-CLTDOB OR
              WSSP-DOB02-F        < WSCC-CLTDOB
               MOVE 'N'                TO WSAA-MATCH
               GO TO 6900-EXIT.
      *
       6015-CHECK-NAME.
      *
      *    Check for an exact match of name
      *
           IF WSSP-EXACT              = 'Y'
               MOVE WSCC-SURNAME      TO WSAA-CONSUR01
               MOVE WSCC-SURNAME      TO WSAA-CONVERT
               PERFORM B200-CONVERT
               MOVE WSAA-CONVERT      TO WSCC-SURNAME
               IF WSSP-SURNAME-F       NOT = WSCC-SURNAME
                   MOVE 'N'           TO WSAA-MATCH
                   MOVE WSAA-CONSUR01 TO WSCC-SURNAME
                   GO TO 6900-EXIT
               ELSE
               MOVE WSAA-CONSUR01     TO WSCC-SURNAME
               IF WSCC-CLTTYPE        = 'C' AND
                   WSSP-GIVNAME-F      NOT = WSCC-GIVNAME
                   MOVE 'N'           TO WSAA-MATCH
                   GO TO 6900-EXIT
               ELSE
                   GO TO 6900-EXIT.
      *
      *    Check for a partial match
      *
           IF WSSP-SURNAME-F           = SPACE
              GO TO 6900-EXIT.
      *
       6900-EXIT.
           EXIT.
      /
      *****************************************************************
      *     CHECK IF SURNAME/CORPORATE NAME IS IN SPECIFIED RANGE
      *****************************************************************
       7000-CHECK-NAME SECTION.
      *     This subroutine checks if key is out of range i.e. does
      *     not check if name is a match
      *
       7100-CHECK-NAME.
      *
           MOVE 'Y'                   TO WSAA-MATCH.
      *
      *    Check for an exact match
      *
           IF WSSP-EXACT              = 'Y'
               MOVE WSCC-SURNAME      TO WSAA-CONSUR01                  <A06461>
               MOVE WSCC-SURNAME      TO WSAA-CONVERT                   <A06461>
               PERFORM B200-CONVERT                                     <A06461>
               MOVE WSAA-CONVERT      TO WSCC-SURNAME                   <A06461>
               IF WSSP-SURNAME-F       NOT = WSCC-SURNAME
                   MOVE 'N'           TO WSAA-MATCH
                   MOVE WSAA-CONSUR01 TO WSCC-SURNAME
      *****        GO TO 7900-EXIT.
                   GO TO 7900-EXIT
               ELSE
                   MOVE WSAA-CONSUR01 TO WSCC-SURNAME
               END-IF
           END-IF.
      *
      *    Check for a partial match
      *
           IF WSSP-SURNAME-F           = SPACE
              GO TO 7900-EXIT.
      *
      *
       7900-EXIT.
           EXIT.
      /
      *****************************************************************
      *     CHECK IF ALIASES OR EXTRA ADDRESSES EXIST FOR CLIENT
      *****************************************************************
       8000-FLAGS SECTION.
      *
       8010-FLAGS.
      *
           MOVE SPACES                TO S2473-ALFLAG
                                         S2473-AAFLAG.
      *
      *    Check client alias
      *
      *   .. by getting the flag number and checking the role.
      *
           MOVE BEGN                   TO CLRRWIN-FUNCTION.
           MOVE CLRRWINREC             TO CLRRWIN-FORMAT.
      *
           MOVE SPACES                 TO CLRRWIN-DATA-KEY.
           MOVE WSSP-FSUCO             TO CLRRWIN-FORECOY.
           MOVE S2473-CLNTNUM          TO CLRRWIN-CLNTNUM.
           MOVE 'AL'                   TO CLRRWIN-CLRRROLE.
      *
       8050-NEXT-CLRRWIN.
      *
           CALL 'CLRRWINIO'              USING CLRRWIN-PARAMS.
      *
           IF CLRRWIN-STATUZ           NOT = O-K AND
              CLRRWIN-STATUZ           NOT = ENDP
               MOVE CLRRWIN-PARAMS     TO SYSR-PARAMS
               MOVE CLRRWIN-STATUZ     TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR.
      *
           IF CLRRWIN-STATUZ           = ENDP OR
              CLRRWIN-CLNTNUM          NOT = S2473-CLNTNUM OR
              CLRRWIN-CLRRROLE         NOT = 'AL'          OR
              CLRRWIN-FORECOY          NOT = WSSP-FSUCO
              PERFORM 8950-CHECK-FOREKEY                                <CAS1.0>
              GO TO 8500-ADDRESS.
      *
           IF CLRRWIN-USED-TO-BE       NOT = SPACE
              MOVE NEXTR               TO CLRRWIN-FUNCTION
              GO TO 8050-NEXT-CLRRWIN.
      *
            MOVE '+'                   TO S2473-ALFLAG.
      *
      *    Check client address
      *

       8500-ADDRESS.
           MOVE BEGN                   TO CLRRWIN-FUNCTION.
           MOVE CLRRWINREC             TO CLRRWIN-FORMAT.
      *
           MOVE SPACES                 TO CLRRWIN-DATA-KEY.
           MOVE WSSP-FSUCO             TO CLRRWIN-FORECOY.
           MOVE S2473-CLNTNUM          TO CLRRWIN-CLNTNUM.
           MOVE 'AA'                   TO CLRRWIN-CLRRROLE.
      *
       8550-CLRRWIN.
      *
           CALL 'CLRRWINIO'              USING CLRRWIN-PARAMS.
      *
           IF CLRRWIN-STATUZ           NOT = O-K AND
              CLRRWIN-STATUZ           NOT = ENDP
               MOVE CLRRWIN-PARAMS     TO SYSR-PARAMS
               MOVE CLRRWIN-STATUZ     TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR.
      *
           IF CLRRWIN-STATUZ           = ENDP OR
              CLRRWIN-CLNTNUM          NOT = S2473-CLNTNUM OR
              CLRRWIN-CLRRROLE         NOT = 'AA'          OR
              CLRRWIN-FORECOY          NOT = WSSP-FSUCO
              GO TO 8900-EXIT.
      *
           IF CLRRWIN-USED-TO-BE       NOT = SPACE
              MOVE NEXTR               TO CLRRWIN-FUNCTION
              GO TO 8550-CLRRWIN.
      *
            MOVE '+'                   TO S2473-AAFLAG.
      *
       8900-EXIT.
           EXIT.
      /
      *  Check the foreign client. If it matches with the client        <CAS1.0>
      *  then set the alias indicator to '*'.                           <CAS1.0>
      *                                                                 <CAS1.0>
       8950-CHECK-FOREKEY SECTION.                                      <CAS1.0>
       8950-CHK-FORE.                                                   <CAS1.0>
      *                                                                 <CAS1.0>
           MOVE BEGN                   TO CLRRFOR-FUNCTION.             <CAS1.0>
           MOVE CLRRFORREC             TO CLRRFOR-FORMAT.               <CAS1.0>
      *                                                                 <CAS1.0>
           MOVE SPACES                 TO CLRRFOR-DATA-KEY.             <CAS1.0>
           MOVE WSSP-FSUCO             TO CLRRFOR-FORECOY.              <CAS1.0>
           MOVE S2473-CLNTNUM          TO CLRRFOR-FORENUM.              <CAS1.0>
           MOVE 'AL'                   TO CLRRFOR-CLRRROLE.             <CAS1.0>
      *                                                                 <CAS1.0>
       8950-NEXT-CLRRFOR.                                               <CAS1.0>
      *                                                                 <CAS1.0>
           CALL 'CLRRFORIO'              USING CLRRFOR-PARAMS.          <CAS1.0>
      *                                                                 <CAS1.0>
           IF CLRRFOR-STATUZ           NOT = O-K AND                    <CAS1.0>
              CLRRFOR-STATUZ           NOT = ENDP                       <CAS1.0>
               MOVE CLRRFOR-PARAMS     TO SYSR-PARAMS                   <CAS1.0>
               MOVE CLRRFOR-STATUZ     TO SYSR-STATUZ                   <CAS1.0>
               PERFORM 600-FATAL-ERROR.                                 <CAS1.0>
      *                                                                 <CAS1.0>
           MOVE CLRRFOR-FORENUM        TO WSAA-CLIENT-NUM.              <CAS1.0>
           IF CLRRFOR-STATUZ           = ENDP OR                        <CAS1.0>
              WSAA-CLIENT-NUM          NOT = S2473-CLNTNUM OR           <CAS1.0>
              CLRRFOR-CLRRROLE         NOT = 'AL'          OR           <CAS1.0>
              CLRRFOR-FORECOY          NOT = WSSP-FSUCO                 <CAS1.0>
              GO TO 8999-EXIT                                           <CAS1.0>
           END-IF.                                                      <CAS1.0>
      *                                                                 <CAS1.0>
           IF CLRRFOR-USED-TO-BE       NOT = SPACE                      <CAS1.0>
              MOVE NEXTR               TO CLRRFOR-FUNCTION              <CAS1.0>
              GO TO 8950-NEXT-CLRRFOR.                                  <CAS1.0>
      *                                                                 <CAS1.0>
      **** MOVE '*'                   TO S2473-ALFLAG.                  <CAS1.0>
           MOVE 'A'                   TO S2473-ALFLAG.                  <CAS1.0>
      *                                                                 <CAS1.0>
       8999-EXIT.                                                       <CAS1.0>
            EXIT.                                                       <CAS1.0>
      *                                                                 <CAS1.0>
      *****************************************************************
      *     CHECK IF CLIENT HAS ENTERED ROLE
      *****************************************************************
       9000-CHECK-ROLE SECTION.
       9500-CHK-ROLE.
      *
           IF WSAA-ROLE                   = SPACES
               GO TO 9900-EXIT.

           IF WSCC-ROLEFLAG(WSCC-SUB)     NOT = 'Y'
              MOVE 'N'                    TO WSAA-MATCH.

      *
       9900-EXIT.
           EXIT.
      /
      *****************************************************************
      *     FIND FIRST RECORD IN RANGE
      *****************************************************************
       A100-FIND-FIRST SECTION.
       A100-FIRST.
      *
      *    If the entered role corresponds to one of the ROLEFLAG
      *    fields on the client file.
      *
           IF  S2473-EXACT         NOT =  'Y'                           <GP3IND>
           AND WSSP-SURNAME-F      NOT =  SPACES                        <GP3IND>
      *** skip calling UPPER if alternate DBCS language is used.        <V5L001>
     ***   AND WSAA-ALT-DBCS       NOT =  'Y'                  <V5L001> <FSA1003
              MOVE O-K                 TO UPPER-STATUZ                  <GP3IND>
              MOVE WSSP-SURNAME-F      TO UPPER-NAME                    <GP3IND>
              CALL 'UPPER'          USING UPPER-UPPER-REC               <GP3IND>
              IF UPPER-STATUZ          =  O-K                           <GP3IND>
                 MOVE UPPER-NAME       TO WSSP-SURNAME-F                <GP3IND>
                                          S2473-SURNAME                 <GP3IND>
              END-IF                                                    <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
           MOVE WSAA-ROLE              TO CLRL-VALUE.
           IF WSAA-ROLE                = SPACES
              GO TO A200-FIRST-CLTN
           ELSE
           IF CLRL-FLAG02
              GO TO A300-FIRST-CLTA
           ELSE
           IF CLRL-FLAG05
              GO TO A400-FIRST-CLTB
           ELSE                                                         <GP3IND>
           IF CLRL-FLAG18                                               <GP3IND>
              GO TO A500-FIRST-CLOW                                     <GP3IND>
           ELSE                                                         <GP3IND>
           IF CLRL-FLAG19                                               <GP3IND>
              GO TO A600-FIRST-CLPE                                     <GP3IND>
           ELSE                                                         <GP3IND>
           IF CLRL-FLAG20                                               <GP3IND>
              GO TO A700-FIRST-CLPY                                     <GP3IND>
           ELSE
      **** IF CLRL-FLAG28                                       <V62F05><FSA552>
      ****    GO TO A800-FIRST-CLGA                             <V62F05><FSA552>
      **** ELSE                                                 <V62F05><FSA552>
           IF CLRL-FLAG29                                               <FSA552>
              GO TO A900-FIRST-CLGC                                     <FSA552>
           ELSE                                                         <FSA552>
           IF CLRL-FLAG32                                               <V76F13>
              GO TO A800-FIRST-CLCC                                     <V76F13>
           ELSE                                                         <V76F13>
              GO TO A200-FIRST-CLTN.
      *******
      *    Find the first record in range - using CLTA
      *
       A200-FIRST-CLTN.
      *
           MOVE LOW-VALUES             TO CLTNREC-KEY-DATA.             <V5L001>
           MOVE CLTNREC                TO CLTN-FORMAT.
           MOVE WSSP-FSUCO             TO CLTN-CLNTCOY.
           MOVE PRFX-CLNT              TO CLTN-CLNTPFX.
           MOVE WSSP-SURNAME-F         TO CLTN-SURNAME.
           IF   WSSP-SURNAME-F = SPACES                                 <V5L001>
                MOVE LOW-VALUES        TO CLTN-SURNAME.                 <V5L001>
                                                                        <V5L001>
      ***  MOVE SPACES                 TO CLTN-GIVNAME.                 <V5L001>
           MOVE LOW-VALUES             TO CLTN-GIVNAME.                 <V5L001>
           MOVE SPACES                 TO CLTN-CLNTNUM.
           IF WSSP-CLTTYPE-F           NOT = 'C'
              MOVE WSSP-GIVNAME-F      TO CLTN-GIVNAME.
           IF   WSSP-GIVNAME-F = SPACES                                 <V5L001>
                MOVE LOW-VALUES        TO CLTN-GIVNAME.                 <V5L001>
           MOVE BEGN                   TO CLTN-FUNCTION.
      *
       A250-NEXT.
      *
           CALL 'CLTNIO'                 USING CLTN-PARAMS.
           IF CLTN-STATUZ              NOT = O-K
                                   AND NOT = ENDP
               MOVE CLTN-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
           IF CLTN-CLNTCOY NOT = WSSP-FSUCO
               MOVE ENDP               TO CLTN-STATUZ.
      *
           IF CLTN-STATUZ = ENDP
              MOVE E040                TO SCRN-ERROR-CODE
              MOVE SPACES              TO WSCC-CLTADDR01
              MOVE SPACES              TO WSCC-CLTADDR02                <024>
              MOVE SPACES              TO WSCC-CLTADDR03                <024>
              MOVE SPACES              TO WSCC-CLTADDR04                <024>
              MOVE SPACES              TO WSCC-CLTADDR05                <024>
              MOVE SPACES              TO WSCC-SECUITYNO                <CAS1.0>
              MOVE SPACES              TO WSCC-SURNAME
              MOVE SPACES              TO WSCC-GIVNAME
              GO TO A900-EXIT.

           IF WSAA-SRHLIMIT           >=  WSSP-SRHLIMIT
              MOVE E040                TO SCRN-ERROR-CODE
              GO TO A900-EXIT
           END-IF.
      *
           MOVE NEXTR                  TO CLTN-FUNCTION.
           IF CLTN-STATUZ = O-K
      ****     MOVE CLTN-SURNAME           TO WSCC-SURNAME              <021>
      ****     MOVE CLTN-GIVNAME           TO WSCC-GIVNAME              <021>
               MOVE CLTN-CLNTPFX           TO CLTS-CLNTPFX              <021>
               MOVE CLTN-CLNTCOY           TO CLTS-CLNTCOY              <021>
               MOVE CLTN-CLNTNUM           TO CLTS-CLNTNUM              <021>
               PERFORM  B100-CLIENT-NAME                                <021>
               MOVE CLTN-MIDDL01           TO WSCC-MIDDL01
               MOVE CLTN-MIDDL02           TO WSCC-MIDDL02
               MOVE CLTN-CLTTYPE           TO WSCC-CLTTYPE
               MOVE CLTN-CLTSTAT           TO WSCC-CLTSTAT
               MOVE CLTN-SERVBRH           TO WSCC-SERVBRH
               MOVE CLTN-CLTSEX            TO WSCC-CLTSEX
               MOVE CLTN-CLTDOB            TO WSCC-CLTDOB
               MOVE CLTN-CLTADDR01         TO WSCC-CLTADDR01
               MOVE CLTN-CLTADDR02         TO WSCC-CLTADDR02            <024>
               MOVE CLTN-CLTADDR03         TO WSCC-CLTADDR03            <024>
               MOVE CLTN-CLTADDR04         TO WSCC-CLTADDR04            <024>
               MOVE CLTN-CLTADDR05         TO WSCC-CLTADDR05            <024>
               MOVE CLTN-CLTIND            TO WSCC-CLTIND
               MOVE CLTN-SECUITYNO         TO WSCC-SECUITYNO            <CAS1.0>
               MOVE CLTN-ROLEFLAGS         TO WSCC-ROLEFLAGS

               IF WSCC-CLTTYPE = WSSP-CLTTYPE-F
               OR WSSP-CLTTYPE-F = SPACES
                  ADD 1                    TO WSAA-SRHLIMIT
               END-IF
                                                                        <A06461>
               PERFORM 7000-CHECK-NAME
               IF WSAA-MATCH               = 'N'
                  GO TO A250-NEXT
               ELSE
                   PERFORM 6000-CHECK-MATCH
                   IF WSAA-MATCH               = 'N'
                      GO TO A250-NEXT
                   ELSE
                      MOVE CLTN-CLNTNUM   TO WSAA-STORE-CLNTNUM
                      PERFORM 9000-CHECK-ROLE
                      IF WSAA-MATCH               = 'N'
                         GO TO A250-NEXT.
      *
              GO TO A900-EXIT.
      *
       A300-FIRST-CLTA.
      *
           MOVE LOW-VALUES             TO CLTAREC-KEY-DATA.             <V5L001>
           MOVE CLTAREC                TO CLTA-FORMAT.
           MOVE WSSP-FSUCO             TO CLTA-CLNTCOY.
           MOVE PRFX-CLNT              TO CLTA-CLNTPFX.
           MOVE WSSP-SURNAME-F         TO CLTA-SURNAME.
           IF   WSSP-SURNAME-F = SPACES                                 <V5L001>
                MOVE LOW-VALUES        TO CLTA-SURNAME                  <V5L001>
           END-IF.                                                      <V5L001>
      **** MOVE SPACES                 TO CLTA-GIVNAME.                 <V5L001>
           MOVE LOW-VALUES             TO CLTA-GIVNAME.                 <V5L001>
           MOVE SPACES                 TO CLTA-CLNTNUM.
           IF WSSP-CLTTYPE-F           NOT = 'C'
              MOVE WSSP-GIVNAME-F      TO CLTA-GIVNAME.
           IF   WSSP-GIVNAME-F = SPACES                                 <V5L001>
                MOVE LOW-VALUES        TO CLTA-GIVNAME                  <V5L001>
           END-IF.                                                      <V5L001>
           MOVE SPACES                 TO WSSP-VALUE.
           MOVE BEGN                   TO CLTA-FUNCTION.
      *
       A350-NEXT.
      *
           CALL 'CLTAIO'                 USING CLTA-PARAMS.
           IF CLTA-STATUZ              NOT = O-K
                                   AND NOT = ENDP
               MOVE CLTA-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
           IF CLTA-CLNTCOY NOT = WSSP-FSUCO
               MOVE ENDP               TO CLTA-STATUZ.
      *
           IF CLTA-STATUZ = ENDP
              MOVE E040                TO SCRN-ERROR-CODE
              GO TO A900-EXIT.
      *
           IF WSAA-SRHLIMIT           >=  WSSP-SRHLIMIT
              MOVE E040                TO SCRN-ERROR-CODE
              GO TO A900-EXIT
           END-IF.

           MOVE NEXTR                  TO CLTA-FUNCTION.
           IF CLTA-STATUZ = O-K
      ****     MOVE CLTA-SURNAME           TO WSCC-SURNAME              <021>
      ****     MOVE CLTA-GIVNAME           TO WSCC-GIVNAME              <021>
               MOVE CLTA-CLNTPFX           TO CLTS-CLNTPFX              <021>
               MOVE CLTA-CLNTCOY           TO CLTS-CLNTCOY              <021>
               MOVE CLTA-CLNTNUM           TO CLTS-CLNTNUM              <021>
               PERFORM  B100-CLIENT-NAME                                <021>
               MOVE CLTA-MIDDL01           TO WSCC-MIDDL01
               MOVE CLTA-MIDDL02           TO WSCC-MIDDL02
               MOVE CLTA-CLTTYPE           TO WSCC-CLTTYPE
               MOVE CLTA-CLTSTAT           TO WSCC-CLTSTAT
               MOVE CLTA-SERVBRH           TO WSCC-SERVBRH
               MOVE CLTA-CLTSEX            TO WSCC-CLTSEX
               MOVE CLTA-CLTDOB            TO WSCC-CLTDOB
               MOVE CLTA-CLTADDR01         TO WSCC-CLTADDR01
               MOVE CLTA-CLTADDR02         TO WSCC-CLTADDR02            <024>
               MOVE CLTA-CLTADDR03         TO WSCC-CLTADDR03            <024>
               MOVE CLTA-CLTADDR04         TO WSCC-CLTADDR04            <024>
               MOVE CLTA-CLTADDR05         TO WSCC-CLTADDR05            <024>
               MOVE CLTA-CLTIND            TO WSCC-CLTIND
               MOVE CLTA-SECUITYNO         TO WSCC-SECUITYNO            <CAS1.0>
               MOVE CLTA-ROLEFLAGS         TO WSCC-ROLEFLAGS

               IF WSCC-CLTTYPE = WSSP-CLTTYPE-F
               OR WSSP-CLTTYPE-F = SPACES
                  ADD 1                    TO WSAA-SRHLIMIT
               END-IF

               PERFORM 7000-CHECK-NAME
               IF WSAA-MATCH               = 'N'
                  GO TO A350-NEXT
               ELSE
                   PERFORM 6000-CHECK-MATCH
                   IF WSAA-MATCH               = 'N'
                      GO TO A350-NEXT
                   ELSE
                      MOVE CLTA-CLNTNUM   TO WSAA-STORE-CLNTNUM
                      PERFORM 9000-CHECK-ROLE
                      IF WSAA-MATCH               = 'N'
                         GO TO A350-NEXT.
      *
              GO TO A900-EXIT.
      *
       A400-FIRST-CLTB.
      *
           MOVE LOW-VALUES             TO CLTBREC-KEY-DATA.             <V5L001>
           MOVE CLTBREC                TO CLTB-FORMAT.
           MOVE WSSP-FSUCO             TO CLTB-CLNTCOY.
           MOVE PRFX-CLNT              TO CLTB-CLNTPFX.
           MOVE WSSP-SURNAME-F         TO CLTB-SURNAME.
           IF   WSSP-SURNAME-F = SPACES                                 <V5L001>
                MOVE LOW-VALUES        TO CLTB-SURNAME                  <V5L001>
           END-IF.                                                      <V5L001>
      ***  MOVE SPACES                 TO CLTB-GIVNAME.                 <V5L001>
           MOVE LOW-VALUES             TO CLTB-GIVNAME.                 <V5L001>
           MOVE SPACES                 TO CLTB-CLNTNUM.
           IF WSSP-CLTTYPE-F           NOT = 'C'
              MOVE WSSP-GIVNAME-F      TO CLTB-GIVNAME.
           IF   WSSP-GIVNAME-F = SPACES                                 <V5L001>
                MOVE LOW-VALUES        TO CLTB-GIVNAME                  <V5L001>
           END-IF.                                                      <V5L001>
           MOVE SPACES                 TO WSSP-VALUE.
           MOVE BEGN                   TO CLTB-FUNCTION.
      *
       A450-NEXT.
      *
           CALL 'CLTBIO'                 USING CLTB-PARAMS.
           IF CLTB-STATUZ              NOT = O-K
                                   AND NOT = ENDP
               MOVE CLTB-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
           IF CLTB-CLNTCOY NOT = WSSP-FSUCO
               MOVE ENDP               TO CLTB-STATUZ.
      *
           IF CLTB-STATUZ = ENDP
              MOVE E040                TO SCRN-ERROR-CODE
              GO TO A900-EXIT.
      *
           IF WSAA-SRHLIMIT           >=  WSSP-SRHLIMIT
              MOVE E040                TO SCRN-ERROR-CODE
              GO TO A900-EXIT
           END-IF.

           MOVE NEXTR                  TO CLTB-FUNCTION.
           IF CLTB-STATUZ = O-K
      ****     MOVE CLTB-SURNAME           TO WSCC-SURNAME              <021>
      ****     MOVE CLTB-GIVNAME           TO WSCC-GIVNAME              <021>
               MOVE CLTB-CLNTPFX           TO CLTS-CLNTPFX              <021>
               MOVE CLTB-CLNTCOY           TO CLTS-CLNTCOY              <021>
               MOVE CLTB-CLNTNUM           TO CLTS-CLNTNUM              <021>
               PERFORM  B100-CLIENT-NAME                                <021>
               MOVE CLTB-MIDDL01           TO WSCC-MIDDL01
               MOVE CLTB-MIDDL02           TO WSCC-MIDDL02
               MOVE CLTB-CLTTYPE           TO WSCC-CLTTYPE
               MOVE CLTB-CLTSTAT           TO WSCC-CLTSTAT
               MOVE CLTB-SERVBRH           TO WSCC-SERVBRH
               MOVE CLTB-CLTSEX            TO WSCC-CLTSEX
               MOVE CLTB-CLTDOB            TO WSCC-CLTDOB
               MOVE CLTB-CLTADDR01         TO WSCC-CLTADDR01
               MOVE CLTB-CLTADDR02         TO WSCC-CLTADDR02            <024>
               MOVE CLTB-CLTADDR03         TO WSCC-CLTADDR03            <024>
               MOVE CLTB-CLTADDR04         TO WSCC-CLTADDR04            <024>
               MOVE CLTB-CLTADDR05         TO WSCC-CLTADDR05            <024>
               MOVE CLTB-CLTIND            TO WSCC-CLTIND
               MOVE CLTB-SECUITYNO         TO WSCC-SECUITYNO            <CAS1.0>
               MOVE CLTB-ROLEFLAGS         TO WSCC-ROLEFLAGS

               IF WSCC-CLTTYPE = WSSP-CLTTYPE-F
               OR WSSP-CLTTYPE-F = SPACES
                  ADD 1                    TO WSAA-SRHLIMIT
               END-IF

               PERFORM 7000-CHECK-NAME
               IF WSAA-MATCH               = 'N'
                  GO TO A450-NEXT
               ELSE
                   PERFORM 6000-CHECK-MATCH
                   IF WSAA-MATCH               = 'N'
                      GO TO A450-NEXT
                   ELSE
                      MOVE CLTB-CLNTNUM   TO WSAA-STORE-CLNTNUM
                      PERFORM 9000-CHECK-ROLE
                      IF WSAA-MATCH               = 'N'
                         GO TO A450-NEXT.
      *
              GO TO A900-EXIT.                                          <GP3IND>
      *                                                                 <GP3IND>
       A500-FIRST-CLOW.                                                 <GP3IND>
      *                                                                 <GP3IND>
           MOVE LOW-VALUES             TO CLOWREC-KEY-DATA.             <V5L001>
           MOVE CLOWREC                TO CLOW-FORMAT.                  <GP3IND>
           MOVE WSSP-FSUCO             TO CLOW-CLNTCOY.                 <GP3IND>
           MOVE PRFX-CLNT              TO CLOW-CLNTPFX.                 <GP3IND>
           MOVE WSSP-SURNAME-F         TO CLOW-SURNAME.                 <GP3IND>
           IF   WSSP-SURNAME-F = SPACES                                 <V5L001>
                MOVE LOW-VALUES        TO CLOW-SURNAME                  <V5L001>
           END-IF.                                                      <V5L001>
      **** MOVE SPACES                 TO CLOW-GIVNAME.         <V5L001><GP3IND>
           MOVE LOW-VALUES             TO CLOW-GIVNAME.                 <V5L001>
           MOVE SPACES                 TO CLOW-CLNTNUM.                 <GP3IND>
           IF WSSP-CLTTYPE-F       NOT =  'C'                           <GP3IND>
              MOVE WSSP-GIVNAME-F      TO CLOW-GIVNAME                  <GP3IND>
           END-IF.                                                      <GP3IND>
           IF   WSSP-GIVNAME-F = SPACES                                 <V5L001>
                MOVE LOW-VALUES        TO CLOW-GIVNAME                  <V5L001>
           END-IF.                                                      <V5L001>
           MOVE SPACES                 TO WSSP-VALUE.                   <GP3IND>
           MOVE BEGN                   TO CLOW-FUNCTION.                <GP3IND>
      *                                                                 <GP3IND>
       A550-NEXT.                                                       <GP3IND>
      *                                                                 <GP3IND>
           CALL 'CLOWIO'            USING CLOW-PARAMS.                  <GP3IND>
           IF CLOW-STATUZ          NOT =  O-K AND ENDP                  <GP3IND>
              MOVE CLOW-STATUZ         TO SYSR-STATUZ                   <GP3IND>
              MOVE CLOW-PARAMS         TO SYSR-PARAMS                   <GP3IND>
              PERFORM 600-FATAL-ERROR                                   <GP3IND>
           END-IF.                                                      <GP3IND>
           IF CLOW-CLNTCOY         NOT =  WSSP-FSUCO                    <GP3IND>
              MOVE ENDP                TO CLOW-STATUZ                   <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
           IF CLOW-STATUZ              =  ENDP                          <GP3IND>
              MOVE E040                TO SCRN-ERROR-CODE               <GP3IND>
              GO TO A900-EXIT                                           <GP3IND>
           END-IF.                                                      <GP3IND>
      *                                                                 <GP3IND>
           IF WSAA-SRHLIMIT           >=  WSSP-SRHLIMIT                 <GP3IND>
              MOVE E040                TO SCRN-ERROR-CODE               <GP3IND>
              GO TO A900-EXIT                                           <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
           MOVE NEXTR                  TO CLOW-FUNCTION.                <GP3IND>
           IF CLOW-STATUZ              =  O-K                           <GP3IND>
              MOVE CLOW-CLNTPFX        TO CLTS-CLNTPFX                  <GP3IND>
              MOVE CLOW-CLNTCOY        TO CLTS-CLNTCOY                  <GP3IND>
              MOVE CLOW-CLNTNUM        TO CLTS-CLNTNUM                  <GP3IND>
              PERFORM B100-CLIENT-NAME                                  <GP3IND>
              MOVE CLOW-MIDDL01        TO WSCC-MIDDL01                  <GP3IND>
              MOVE CLOW-MIDDL02        TO WSCC-MIDDL02                  <GP3IND>
              MOVE CLOW-CLTTYPE        TO WSCC-CLTTYPE                  <GP3IND>
              MOVE CLOW-CLTSTAT        TO WSCC-CLTSTAT                  <GP3IND>
              MOVE CLOW-SERVBRH        TO WSCC-SERVBRH                  <GP3IND>
              MOVE CLOW-CLTSEX         TO WSCC-CLTSEX                   <GP3IND>
              MOVE CLOW-CLTDOB         TO WSCC-CLTDOB                   <GP3IND>
              MOVE CLOW-CLTADDR01      TO WSCC-CLTADDR01                <GP3IND>
              MOVE CLOW-CLTADDR02      TO WSCC-CLTADDR02                <GP3IND>
              MOVE CLOW-CLTADDR03      TO WSCC-CLTADDR03                <GP3IND>
              MOVE CLOW-CLTADDR04      TO WSCC-CLTADDR04                <GP3IND>
              MOVE CLOW-CLTADDR05      TO WSCC-CLTADDR05                <GP3IND>
              MOVE CLOW-CLTIND         TO WSCC-CLTIND                   <GP3IND>
              MOVE CLOW-SECUITYNO      TO WSCC-SECUITYNO                <GP3IND>
              MOVE CLOW-ROLEFLAGS      TO WSCC-ROLEFLAGS                <GP3IND>
                                                                        <GP3IND>
              IF WSCC-CLTTYPE          =  WSSP-CLTTYPE-F                <GP3IND>
              OR WSSP-CLTTYPE-F        =  SPACES                        <GP3IND>
                 ADD 1                 TO WSAA-SRHLIMIT                 <GP3IND>
              END-IF                                                    <GP3IND>
                                                                        <GP3IND>
              PERFORM 7000-CHECK-NAME                                   <GP3IND>
              IF WSAA-MATCH            =  'N'                           <GP3IND>
                 GO TO A550-NEXT                                        <GP3IND>
              ELSE                                                      <GP3IND>
                 PERFORM 6000-CHECK-MATCH                               <GP3IND>
                 IF WSAA-MATCH         =  'N'                           <GP3IND>
                    GO TO A550-NEXT                                     <GP3IND>
                 ELSE                                                   <GP3IND>
                    MOVE CLOW-CLNTNUM  TO WSAA-STORE-CLNTNUM            <GP3IND>
                    PERFORM 9000-CHECK-ROLE                             <GP3IND>
                    IF WSAA-MATCH      =  'N'                           <GP3IND>
                       GO TO A550-NEXT.                                 <GP3IND>
      *                                                                 <GP3IND>
              GO TO A900-EXIT.                                          <GP3IND>
      *                                                                 <GP3IND>
       A600-FIRST-CLPE.                                                 <GP3IND>
      *                                                                 <GP3IND>
           MOVE LOW-VALUES             TO CLPEREC-KEY-DATA.             <V5L001>
           MOVE CLPEREC                TO CLPE-FORMAT.                  <GP3IND>
           MOVE WSSP-FSUCO             TO CLPE-CLNTCOY.                 <GP3IND>
           MOVE PRFX-CLNT              TO CLPE-CLNTPFX.                 <GP3IND>
           MOVE WSSP-SURNAME-F         TO CLPE-SURNAME.                 <GP3IND>
           IF   WSSP-SURNAME-F = SPACES                                 <V5L001>
                MOVE LOW-VALUES        TO CLPE-SURNAME                  <V5L001>
           END-IF.                                                      <V5L001>
      **** MOVE SPACES                 TO CLPE-GIVNAME.         <V5L001><GP3IND>
           MOVE LOW-VALUES             TO CLPE-GIVNAME.                 <V5L001>
           MOVE SPACES                 TO CLPE-CLNTNUM.                 <GP3IND>
           IF WSSP-CLTTYPE-F       NOT =  'C'                           <GP3IND>
              MOVE WSSP-GIVNAME-F      TO CLPE-GIVNAME                  <GP3IND>
           END-IF.                                                      <GP3IND>
           IF   WSSP-GIVNAME-F = SPACES                                 <V5L001>
                MOVE LOW-VALUES        TO CLPE-GIVNAME                  <V5L001>
           END-IF.                                                      <V5L001>
           MOVE SPACES                 TO WSSP-VALUE.                   <GP3IND>
           MOVE BEGN                   TO CLPE-FUNCTION.                <GP3IND>
      *                                                                 <GP3IND>
       A650-NEXT.                                                       <GP3IND>
      *                                                                 <GP3IND>
           CALL 'CLPEIO'            USING CLPE-PARAMS.                  <GP3IND>
           IF CLPE-STATUZ          NOT =  O-K AND ENDP                  <GP3IND>
              MOVE CLPE-STATUZ         TO SYSR-STATUZ                   <GP3IND>
              MOVE CLPE-PARAMS         TO SYSR-PARAMS                   <GP3IND>
              PERFORM 600-FATAL-ERROR                                   <GP3IND>
           END-IF.                                                      <GP3IND>
           IF CLPE-CLNTCOY         NOT =  WSSP-FSUCO                    <GP3IND>
              MOVE ENDP                TO CLPE-STATUZ                   <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
           IF CLPE-STATUZ              =  ENDP                          <GP3IND>
              MOVE E040                TO SCRN-ERROR-CODE               <GP3IND>
              GO TO A900-EXIT                                           <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
           IF WSAA-SRHLIMIT           >=  WSSP-SRHLIMIT                 <GP3IND>
              MOVE E040                TO SCRN-ERROR-CODE               <GP3IND>
              GO TO A900-EXIT                                           <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
           MOVE NEXTR                  TO CLPE-FUNCTION.                <GP3IND>
           IF CLPE-STATUZ              =  O-K                           <GP3IND>
              MOVE CLPE-CLNTPFX        TO CLTS-CLNTPFX                  <GP3IND>
              MOVE CLPE-CLNTCOY        TO CLTS-CLNTCOY                  <GP3IND>
              MOVE CLPE-CLNTNUM        TO CLTS-CLNTNUM                  <GP3IND>
              PERFORM B100-CLIENT-NAME                                  <GP3IND>
              MOVE CLPE-MIDDL01        TO WSCC-MIDDL01                  <GP3IND>
              MOVE CLPE-MIDDL02        TO WSCC-MIDDL02                  <GP3IND>
              MOVE CLPE-CLTTYPE        TO WSCC-CLTTYPE                  <GP3IND>
              MOVE CLPE-CLTSTAT        TO WSCC-CLTSTAT                  <GP3IND>
              MOVE CLPE-SERVBRH        TO WSCC-SERVBRH                  <GP3IND>
              MOVE CLPE-CLTSEX         TO WSCC-CLTSEX                   <GP3IND>
              MOVE CLPE-CLTDOB         TO WSCC-CLTDOB                   <GP3IND>
              MOVE CLPE-CLTADDR01      TO WSCC-CLTADDR01                <GP3IND>
              MOVE CLPE-CLTADDR02      TO WSCC-CLTADDR02                <GP3IND>
              MOVE CLPE-CLTADDR03      TO WSCC-CLTADDR03                <GP3IND>
              MOVE CLPE-CLTADDR04      TO WSCC-CLTADDR04                <GP3IND>
              MOVE CLPE-CLTADDR05      TO WSCC-CLTADDR05                <GP3IND>
              MOVE CLPE-CLTIND         TO WSCC-CLTIND                   <GP3IND>
              MOVE CLPE-SECUITYNO      TO WSCC-SECUITYNO                <GP3IND>
              MOVE CLPE-ROLEFLAGS     TO WSCC-ROLEFLAGS                 <GP3IND>
                                                                        <GP3IND>
              IF WSCC-CLTTYPE          =  WSSP-CLTTYPE-F                <GP3IND>
              OR WSSP-CLTTYPE-F        =  SPACES                        <GP3IND>
                 ADD 1                 TO WSAA-SRHLIMIT                 <GP3IND>
              END-IF                                                    <GP3IND>
                                                                        <GP3IND>
              PERFORM 7000-CHECK-NAME                                   <GP3IND>
              IF WSAA-MATCH            =  'N'                           <GP3IND>
                 GO TO A650-NEXT                                        <GP3IND>
              ELSE                                                      <GP3IND>
                 PERFORM 6000-CHECK-MATCH                               <GP3IND>
                 IF WSAA-MATCH         =  'N'                           <GP3IND>
                    GO TO A650-NEXT                                     <GP3IND>
                 ELSE                                                   <GP3IND>
                    MOVE CLPE-CLNTNUM  TO WSAA-STORE-CLNTNUM            <GP3IND>
                    PERFORM 9000-CHECK-ROLE                             <GP3IND>
                    IF WSAA-MATCH      =  'N'                           <GP3IND>
                       GO TO A650-NEXT.                                 <GP3IND>
      *                                                                 <GP3IND>
              GO TO A900-EXIT.                                          <GP3IND>
      *                                                                 <GP3IND>
       A700-FIRST-CLPY.                                                 <GP3IND>
      *                                                                 <GP3IND>
           MOVE LOW-VALUES             TO CLPYREC-KEY-DATA.             <V5L001>
           MOVE CLPYREC                TO CLPY-FORMAT.                  <GP3IND>
           MOVE WSSP-FSUCO             TO CLPY-CLNTCOY.                 <GP3IND>
           MOVE PRFX-CLNT              TO CLPY-CLNTPFX.                 <GP3IND>
           MOVE WSSP-SURNAME-F         TO CLPY-SURNAME.                 <GP3IND>
           IF   WSSP-SURNAME-F = SPACES                                 <V5L001>
                MOVE LOW-VALUES        TO CLPY-SURNAME                  <V5L001>
           END-IF.                                                      <V5L001>
      ***  MOVE SPACES                 TO CLPY-GIVNAME.         <V5L001><GP3IND>
           MOVE LOW-VALUES             TO CLPY-GIVNAME.                 <V5L001>
           MOVE SPACES                 TO CLPY-CLNTNUM.                 <GP3IND>
           IF WSSP-CLTTYPE-F       NOT =  'C'                           <GP3IND>
              MOVE WSSP-GIVNAME-F      TO CLPY-GIVNAME                  <GP3IND>
           END-IF.                                                      <GP3IND>
           IF   WSSP-GIVNAME-F = SPACES                                 <V5L001>
                MOVE LOW-VALUES        TO CLPY-GIVNAME                  <V5L001>
           END-IF.                                                      <V5L001>
           MOVE SPACES                 TO WSSP-VALUE.                   <GP3IND>
           MOVE BEGN                   TO CLPY-FUNCTION.                <GP3IND>
      *                                                                 <GP3IND>
       A750-NEXT.                                                       <GP3IND>
      *                                                                 <GP3IND>
           CALL 'CLPYIO'            USING CLPY-PARAMS.                  <GP3IND>
           IF CLPY-STATUZ          NOT =  O-K AND ENDP                  <GP3IND>
              MOVE CLPY-STATUZ         TO SYSR-STATUZ                   <GP3IND>
              MOVE CLPY-PARAMS         TO SYSR-PARAMS                   <GP3IND>
              PERFORM 600-FATAL-ERROR                                   <GP3IND>
           END-IF.                                                      <GP3IND>
           IF CLPY-CLNTCOY         NOT =  WSSP-FSUCO                    <GP3IND>
              MOVE ENDP                TO CLPY-STATUZ                   <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
           IF CLPY-STATUZ              =  ENDP                          <GP3IND>
              MOVE E040                TO SCRN-ERROR-CODE               <GP3IND>
              GO TO A900-EXIT                                           <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
           IF WSAA-SRHLIMIT           >=  WSSP-SRHLIMIT                 <GP3IND>
              MOVE E040                TO SCRN-ERROR-CODE               <GP3IND>
              GO TO A900-EXIT                                           <GP3IND>
           END-IF.                                                      <GP3IND>
                                                                        <GP3IND>
           MOVE NEXTR                  TO CLPY-FUNCTION.                <GP3IND>
           IF CLPY-STATUZ              =  O-K                           <GP3IND>
              MOVE CLPY-CLNTPFX        TO CLTS-CLNTPFX                  <GP3IND>
              MOVE CLPY-CLNTCOY        TO CLTS-CLNTCOY                  <GP3IND>
              MOVE CLPY-CLNTNUM        TO CLTS-CLNTNUM                  <GP3IND>
              PERFORM B100-CLIENT-NAME                                  <GP3IND>
              MOVE CLPY-MIDDL01        TO WSCC-MIDDL01                  <GP3IND>
              MOVE CLPY-MIDDL02        TO WSCC-MIDDL02                  <GP3IND>
              MOVE CLPY-CLTTYPE        TO WSCC-CLTTYPE                  <GP3IND>
              MOVE CLPY-CLTSTAT        TO WSCC-CLTSTAT                  <GP3IND>
              MOVE CLPY-SERVBRH        TO WSCC-SERVBRH                  <GP3IND>
              MOVE CLPY-CLTSEX         TO WSCC-CLTSEX                   <GP3IND>
              MOVE CLPY-CLTDOB         TO WSCC-CLTDOB                   <GP3IND>
              MOVE CLPY-CLTADDR01      TO WSCC-CLTADDR01                <GP3IND>
              MOVE CLPY-CLTADDR02      TO WSCC-CLTADDR02                <GP3IND>
              MOVE CLPY-CLTADDR03      TO WSCC-CLTADDR03                <GP3IND>
              MOVE CLPY-CLTADDR04      TO WSCC-CLTADDR04                <GP3IND>
              MOVE CLPY-CLTADDR05      TO WSCC-CLTADDR05                <GP3IND>
              MOVE CLPY-CLTIND         TO WSCC-CLTIND                   <GP3IND>
              MOVE CLPY-SECUITYNO      TO WSCC-SECUITYNO                <GP3IND>
              MOVE CLPY-ROLEFLAGS      TO WSCC-ROLEFLAGS                <GP3IND>
                                                                        <GP3IND>
              IF WSCC-CLTTYPE          =  WSSP-CLTTYPE-F                <GP3IND>
              OR WSSP-CLTTYPE-F        =  SPACES                        <GP3IND>
                 ADD 1                 TO WSAA-SRHLIMIT                 <GP3IND>
              END-IF                                                    <GP3IND>
                                                                        <GP3IND>
              PERFORM 7000-CHECK-NAME                                   <GP3IND>
              IF WSAA-MATCH            =  'N'                           <GP3IND>
                 GO TO A750-NEXT                                        <GP3IND>
              ELSE                                                      <GP3IND>
                 PERFORM 6000-CHECK-MATCH                               <GP3IND>
                 IF WSAA-MATCH         =  'N'                           <GP3IND>
                    GO TO A750-NEXT                                     <GP3IND>
                 ELSE                                                   <GP3IND>
                    MOVE CLPY-CLNTNUM  TO WSAA-STORE-CLNTNUM            <GP3IND>
                    PERFORM 9000-CHECK-ROLE                             <GP3IND>
                    IF WSAA-MATCH      =  'N'                           <GP3IND>
                       GO TO A750-NEXT.                                 <GP3IND>
                                                                        <FSA552>
              GO TO A900-EXIT.                                          <FSA552>
      *                                                                 <FSA552>
      *                                                                 <FSA552>
      *A800-FIRST-CLGA.                                         <V62F05><FSA552>
      *                                                                 <FSA552>
      **** MOVE LOW-VALUES             TO CLGAREC-KEY-DATA.     <V62F05><V5L001>
      **** MOVE CLGAREC                TO CLGA-FORMAT.          <V62F05><FSA552>
      **** MOVE WSSP-FSUCO             TO CLGA-CLNTCOY.         <V62F05><FSA552>
      **** MOVE PRFX-CLNT              TO CLGA-CLNTPFX.         <V62F05><FSA552>
      **** MOVE WSSP-SURNAME-F         TO CLGA-SURNAME.         <V62F05><FSA552>
      **** IF   WSSP-SURNAME-F = SPACES                         <V62F05><V5L001>
      ****      MOVE LOW-VALUES        TO CLGA-SURNAME          <V62F05><V5L001>
      **** END-IF.                                              <V62F05><V5L001>
      **** MOVE SPACES                 TO CLGA-GIVNAME.         <V5L001><FSA552>
      **** MOVE LOW-VALUES             TO CLGA-GIVNAME.         <V62F05><V5L001>
      **** MOVE SPACES                 TO CLGA-CLNTNUM.         <V62F05><FSA552>
      **** IF WSSP-CLTTYPE-F       NOT =  'C'                   <V62F05><FSA552>
      ****    MOVE WSSP-GIVNAME-F      TO CLGA-GIVNAME.         <V62F05><FSA552>
      **** IF   WSSP-GIVNAME-F = SPACES                         <V62F05><V5L001>
      ****      MOVE LOW-VALUES        TO CLGA-GIVNAME          <V62F05><V5L001>
      **** END-IF.                                              <V62F05><V5L001>
      **** MOVE SPACES                 TO WSSP-VALUE            <V62F05><FSA552>
      **** MOVE BEGN                   TO CLGA-FUNCTION.        <V62F05><FSA552>
      *                                                                 <FSA552>
      *A850-NEXT.                                               <V62F05><FSA552>
      *                                                                 <FSA552>
      **** CALL 'CLGAIO'            USING CLGA-PARAMS.          <V62F05><FSA552>
      **** IF CLGA-STATUZ          NOT =  O-K AND ENDP          <V62F05><FSA552>
      ****    MOVE CLGA-STATUZ         TO SYSR-STATUZ           <V62F05><FSA552>
      ****    MOVE CLGA-PARAMS         TO SYSR-PARAMS           <V62F05><FSA552>
      ****    PERFORM 600-FATAL-ERROR.                          <V62F05><FSA552>
      ****                                                      <V62F05><FSA552>
      **** IF CLGA-CLNTCOY         NOT =  WSSP-FSUCO            <V62F05><FSA552>
      ****     MOVE ENDP               TO CLGA-STATUZ.          <V62F05><FSA552>
      ****                                                      <V62F05><FSA552>
      **** IF CLGA-STATUZ              =  ENDP                  <V62F05><FSA552>
      ****    MOVE E040                TO SCRN-ERROR-CODE       <V62F05><FSA552>
      ****    GO TO A900-EXIT.                                  <V62F05><FSA552>
      ****                                                      <V62F05><FSA552>
      **** IF WSAA-SRHLIMIT           >=  WSSP-SRHLIMIT         <V62F05><FSA552>
      ****    MOVE E040                TO SCRN-ERROR-CODE       <V62F05><FSA552>
      ****    GO TO A900-EXIT                                   <V62F05><FSA552>
      **** END-IF.                                              <V62F05><FSA552>
      ****                                                      <V62F05><FSA552>
      **** MOVE NEXTR                  TO CLGA-FUNCTION.        <V62F05><FSA552>
      **** IF CLGA-STATUZ = O-K                                 <V62F05><FSA552>
      ****    MOVE CLGA-CLNTPFX        TO CLTS-CLNTPFX          <V62F05><FSA552>
      ****    MOVE CLGA-CLNTCOY        TO CLTS-CLNTCOY          <V62F05><FSA552>
      ****    MOVE CLGA-CLNTNUM        TO CLTS-CLNTNUM          <V62F05><FSA552>
      ****    PERFORM B100-CLIENT-NAME                          <V62F05><FSA552>
      ****    MOVE CLGA-MIDDL01        TO WSCC-MIDDL01          <V62F05><FSA552>
      ****    MOVE CLGA-MIDDL02        TO WSCC-MIDDL02          <V62F05><FSA552>
      ****    MOVE CLGA-CLTTYPE        TO WSCC-CLTTYPE          <V62F05><FSA552>
      ****    MOVE CLGA-CLTSTAT        TO WSCC-CLTSTAT          <V62F05><FSA552>
      ****    MOVE CLGA-SERVBRH        TO WSCC-SERVBRH          <V62F05><FSA552>
      ****    MOVE CLGA-CLTSEX         TO WSCC-CLTSEX           <V62F05><FSA552>
      ****    MOVE CLGA-CLTDOB         TO WSCC-CLTDOB           <V62F05><FSA552>
      ****    MOVE CLGA-CLTADDR01      TO WSCC-CLTADDR01        <V62F05><FSA552>
      ****    MOVE CLGA-CLTADDR02      TO WSCC-CLTADDR02        <V62F05><FSA552>
      ****    MOVE CLGA-CLTADDR03      TO WSCC-CLTADDR03        <V62F05><FSA552>
      ****    MOVE CLGA-CLTADDR04      TO WSCC-CLTADDR04        <V62F05><FSA552>
      ****    MOVE CLGA-CLTADDR05      TO WSCC-CLTADDR05        <V62F05><FSA552>
      ****    MOVE CLGA-CLTIND         TO WSCC-CLTIND           <V62F05><FSA552>
      ****    MOVE CLGA-SECUITYNO      TO WSCC-SECUITYNO        <V62F05><FSA552>
      ****    MOVE CLGA-ROLEFLAGS      TO WSCC-ROLEFLAGS        <V62F05><FSA552>
      ****                                                      <V62F05><FSA552>
      ****    IF WSCC-CLTTYPE          =  WSSP-CLTTYPE-F        <V62F05><FSA552>
      ****    OR WSSP-CLTTYPE-F        =  SPACES                <V62F05><FSA552>
      ****       ADD 1                 TO WSAA-SRHLIMIT         <V62F05><FSA552>
      ****    END-IF                                            <V62F05><FSA552>
      ****                                                      <V62F05><FSA552>
      ****    PERFORM 7000-CHECK-NAME                           <V62F05><FSA552>
      ****    IF WSAA-MATCH            =  'N'                   <V62F05><FSA552>
      ****       GO TO A850-NEXT                                <V62F05><FSA552>
      ****    ELSE                                              <V62F05><FSA552>
      ****       PERFORM 6000-CHECK-MATCH                       <V62F05><FSA552>
      ****       IF WSAA-MATCH         =  'N'                   <V62F05><FSA552>
      ****          GO TO A850-NEXT                             <V62F05><FSA552>
      ****       ELSE                                           <V62F05><FSA552>
      ****          MOVE CLGA-CLNTNUM  TO WSAA-STORE-CLNTNUM    <V62F05><FSA552>
      ****          PERFORM 9000-CHECK-ROLE                     <V62F05><FSA552>
      ****          IF WSAA-MATCH      =  'N'                   <V62F05><FSA552>
      ****             GO TO A850-NEXT.                         <V62F05><FSA552>
      *                                                                 <FSA552>
      ****    GO TO A900-EXIT.                                  <V62F05><FSA552>
      *                                                                 <FSA552>
       A800-FIRST-CLCC.                                                 <V76F13>
      *                                                                 <V76F13>
           MOVE LOW-VALUES             TO CLCCREC-KEY-DATA.             <V76F13>
           MOVE CLCCREC                TO CLCC-FORMAT.                  <V76F13>
           MOVE WSSP-FSUCO             TO CLCC-CLNTCOY.                 <V76F13>
           MOVE PRFX-CLNT              TO CLCC-CLNTPFX.                 <V76F13>
           MOVE WSSP-SURNAME-F         TO CLCC-SURNAME.                 <V76F13>
           IF   WSSP-SURNAME-F = SPACES                                 <V76F13>
                MOVE LOW-VALUES        TO CLCC-SURNAME                  <V76F13>
           END-IF.                                                      <V76F13>
      *                                                                 <V76F13>
           MOVE LOW-VALUES             TO CLCC-GIVNAME.                 <V76F13>
           MOVE SPACES                 TO CLCC-CLNTNUM.                 <V76F13>
      *                                                                 <V76F13>
           IF WSSP-CLTTYPE-F        NOT = 'C'                           <V76F13>
              MOVE WSSP-GIVNAME-F      TO CLCC-GIVNAME                  <V76F13>
           END-IF.                                                      <V76F13>
           IF   WSSP-GIVNAME-F          = SPACES                        <V76F13>
                MOVE LOW-VALUES        TO CLCC-GIVNAME                  <V76F13>
           END-IF.                                                      <V76F13>
           MOVE SPACES                 TO WSSP-VALUE.                   <V76F13>
           MOVE BEGN                   TO CLCC-FUNCTION.                <V76F13>
      *                                                                 <V76F13>
       A850-NEXT.                                                       <V76F13>
      *                                                                 <V76F13>
           CALL 'CLCCIO'            USING CLCC-PARAMS.                  <V76F13>
           IF CLCC-STATUZ           NOT = O-K                           <V76F13>
                                AND NOT = ENDP                          <V76F13>
               MOVE CLCC-STATUZ        TO SYSR-STATUZ                   <V76F13>
               MOVE CLCC-PARAMS        TO SYSR-PARAMS                   <V76F13>
               PERFORM 600-FATAL-ERROR                                  <V76F13>
           END-IF.                                                      <V76F13>
      **   IF (CLCC-CLNTCOY         NOT = WSSP-FSUCO)   OR              <V76F13>
      **      (CLCC-CLNTPFX         NOT = PRFX-CLNT )   OR              <V76F13>
      **      (CLCC-STATUZ              = ENDP)                         <V76F13>
      **      MOVE NEXTP               TO CLCC-FUNCTION                 <V76F13>
      **      CALL 'CLCCIO'         USING CLCC-PARAMS                   <V76F13>
      **        IF CLCC-STATUZ      NOT = O-K                           <V76F13>
      **                        AND NOT = ENDP                          <V76F13>
      **           MOVE CLCC-STATUZ    TO SYSR-STATUZ                   <V76F13>
      **           MOVE CLCC-PARAMS    TO SYSR-PARAMS                   <V76F13>
      **           PERFORM 600-FATAL-ERROR                              <V76F13>
      **        END-IF                                                  <V76F13>
      **   END-IF.                                                      <V76F13>
           IF CLCC-CLNTCOY          NOT = WSSP-FSUCO                    <V76F13>
               MOVE ENDP               TO CLCC-STATUZ                   <V76F13>
           END-IF.                                                      <V76F13>
      *                                                                 <V76F13>
           IF CLCC-STATUZ = ENDP                                        <V76F13>
              MOVE E040                TO SCRN-ERROR-CODE               <V76F13>
              GO TO A900-EXIT                                           <V76F13>
           END-IF.                                                      <V76F13>
      *                                                                 <V76F13>
           IF WSAA-SRHLIMIT           >=  WSSP-SRHLIMIT                 <V76F13>
              MOVE E040                TO SCRN-ERROR-CODE               <V76F13>
              GO TO A900-EXIT                                           <V76F13>
           END-IF.                                                      <V76F13>
                                                                        <V76F13>
           MOVE NEXTR                  TO CLCC-FUNCTION.                <V76F13>
           IF CLCC-STATUZ               = O-K                           <V76F13>
              MOVE CLCC-CLNTPFX        TO CLTS-CLNTPFX                  <V76F13>
              MOVE CLCC-CLNTCOY        TO CLTS-CLNTCOY                  <V76F13>
              MOVE CLCC-CLNTNUM        TO CLTS-CLNTNUM                  <V76F13>
              PERFORM  B100-CLIENT-NAME                                 <V76F13>
              MOVE CLCC-MIDDL01        TO WSCC-MIDDL01                  <V76F13>
              MOVE CLCC-MIDDL02        TO WSCC-MIDDL02                  <V76F13>
              MOVE CLCC-CLTTYPE        TO WSCC-CLTTYPE                  <V76F13>
              MOVE CLCC-CLTSTAT        TO WSCC-CLTSTAT                  <V76F13>
              MOVE CLCC-SERVBRH        TO WSCC-SERVBRH                  <V76F13>
              MOVE CLCC-CLTSEX         TO WSCC-CLTSEX                   <V76F13>
              MOVE CLCC-CLTDOB         TO WSCC-CLTDOB                   <V76F13>
              MOVE CLCC-CLTADDR01      TO WSCC-CLTADDR01                <V76F13>
              MOVE CLCC-CLTADDR02      TO WSCC-CLTADDR02                <V76F13>
              MOVE CLCC-CLTADDR03      TO WSCC-CLTADDR03                <V76F13>
              MOVE CLCC-CLTADDR04      TO WSCC-CLTADDR04                <V76F13>
              MOVE CLCC-CLTADDR04      TO WSCC-CLTADDR04                <V76F13>
              MOVE CLCC-CLTADDR05      TO WSCC-CLTADDR05                <V76F13>
              MOVE CLCC-CLTIND         TO WSCC-CLTIND                   <V76F13>
              MOVE CLCC-SECUITYNO      TO WSCC-SECUITYNO                <V76F13>
              MOVE CLCC-ROLEFLAGS      TO WSCC-ROLEFLAGS                <V76F13>
                                                                        <V76F13>
              IF WSCC-CLTTYPE          = WSSP-CLTTYPE-F                 <V76F13>
              OR WSSP-CLTTYPE-F        = SPACES                         <V76F13>
                 ADD 1                TO WSAA-SRHLIMIT                  <V76F13>
              END-IF                                                    <V76F13>
                                                                        <V76F13>
               PERFORM 7000-CHECK-NAME                                  <V76F13>
               IF WSAA-MATCH            = 'N'                           <V76F13>
                  GO TO A850-NEXT                                       <V76F13>
               ELSE                                                     <V76F13>
                   PERFORM 6000-CHECK-MATCH                             <V76F13>
                   IF WSAA-MATCH        = 'N'                           <V76F13>
                      GO TO A850-NEXT                                   <V76F13>
                   ELSE                                                 <V76F13>
                      MOVE CLCC-CLNTNUM TO WSAA-STORE-CLNTNUM           <V76F13>
                      PERFORM 9000-CHECK-ROLE                           <V76F13>
                      IF WSAA-MATCH     = 'N'                           <V76F13>
                         GO TO A850-NEXT                                <V76F13>
                      END-IF                                            <V76F13>
                   END-IF                                               <V76F13>
               END-IF                                                   <V76F13>
           END-IF.                                                      <V76F13>
      *                                                                 <V76F13>
              GO TO A900-EXIT.                                          <V76F13>
                                                                        <V76F13>
       A900-FIRST-CLGC.                                                 <FSA552>
      *                                                                 <FSA552>
           MOVE LOW-VALUES             TO CLGCREC-KEY-DATA.             <V5L001>
           MOVE CLGCREC                TO CLGC-FORMAT.                  <FSA552>
           MOVE WSSP-FSUCO             TO CLGC-CLNTCOY.                 <FSA552>
           MOVE PRFX-CLNT              TO CLGC-CLNTPFX.                 <FSA552>
           MOVE WSSP-SURNAME-F         TO CLGC-SURNAME.                 <FSA552>
           IF   WSSP-SURNAME-F = SPACES                                 <V5L001>
                MOVE LOW-VALUES        TO CLGC-SURNAME                  <V5L001>
           END-IF.                                                      <V5L001>
      **** MOVE SPACES                 TO CLGC-GIVNAME.         <V5L001><FSA552>
           MOVE LOW-VALUES             TO CLGC-GIVNAME.                 <V5L001>
           MOVE SPACES                 TO CLGC-CLNTNUM.                 <FSA552>
           IF WSSP-CLTTYPE-F       NOT =  'C'                           <FSA552>
              MOVE WSSP-GIVNAME-F      TO CLGC-GIVNAME.                 <FSA552>
           IF   WSSP-GIVNAME-F = SPACES                                 <V5L001>
                MOVE LOW-VALUES        TO CLGC-GIVNAME                  <V5L001>
           END-IF.                                                      <V5L001>
           MOVE SPACES                 TO WSSP-VALUE.                   <FSA552>
           MOVE BEGN                   TO CLGC-FUNCTION.                <FSA552>
      *                                                                 <FSA552>
       A950-NEXT.                                                       <FSA552>
      *                                                                 <FSA552>
           CALL 'CLGCIO'            USING CLGC-PARAMS.                  <FSA552>
           IF CLGC-STATUZ          NOT =  O-K AND ENDP                  <FSA552>
              MOVE CLGC-STATUZ         TO SYSR-STATUZ                   <FSA552>
              MOVE CLGC-PARAMS         TO SYSR-PARAMS                   <FSA552>
              PERFORM 600-FATAL-ERROR.                                  <FSA552>
                                                                        <FSA552>
           IF CLGC-CLNTCOY         NOT =  WSSP-FSUCO                    <FSA552>
              MOVE ENDP                TO CLGC-STATUZ.                  <FSA552>
                                                                        <FSA552>
           IF CLGC-STATUZ              =  ENDP                          <FSA552>
              MOVE E040                TO SCRN-ERROR-CODE               <FSA552>
              GO TO A900-EXIT.                                          <FSA552>
                                                                        <FSA552>
           IF WSAA-SRHLIMIT           >=  WSSP-SRHLIMIT                 <FSA552>
              MOVE E040                TO SCRN-ERROR-CODE               <FSA552>
              GO TO A900-EXIT                                           <FSA552>
           END-IF.                                                      <FSA552>
                                                                        <FSA552>
           MOVE NEXTR                  TO CLGC-FUNCTION.                <FSA552>
           IF CLGC-STATUZ              =  O-K                           <FSA552>
              MOVE CLGC-CLNTPFX        TO CLTS-CLNTPFX                  <FSA552>
              MOVE CLGC-CLNTCOY        TO CLTS-CLNTCOY                  <FSA552>
              MOVE CLGC-CLNTNUM        TO CLTS-CLNTNUM                  <FSA552>
              PERFORM B100-CLIENT-NAME                                  <FSA552>
              MOVE CLGC-MIDDL01        TO WSCC-MIDDL01                  <FSA552>
              MOVE CLGC-MIDDL02        TO WSCC-MIDDL02                  <FSA552>
              MOVE CLGC-CLTTYPE        TO WSCC-CLTTYPE                  <FSA552>
              MOVE CLGC-CLTSTAT        TO WSCC-CLTSTAT                  <FSA552>
              MOVE CLGC-SERVBRH        TO WSCC-SERVBRH                  <FSA552>
              MOVE CLGC-CLTSEX         TO WSCC-CLTSEX                   <FSA552>
              MOVE CLGC-CLTDOB         TO WSCC-CLTDOB                   <FSA552>
              MOVE CLGC-CLTADDR01      TO WSCC-CLTADDR01                <FSA552>
              MOVE CLGC-CLTADDR02      TO WSCC-CLTADDR02                <FSA552>
              MOVE CLGC-CLTADDR03      TO WSCC-CLTADDR03                <FSA552>
              MOVE CLGC-CLTADDR04      TO WSCC-CLTADDR04                <FSA552>
              MOVE CLGC-CLTADDR05      TO WSCC-CLTADDR05                <FSA552>
              MOVE CLGC-CLTIND         TO WSCC-CLTIND                   <FSA552>
              MOVE CLGC-SECUITYNO      TO WSCC-SECUITYNO                <FSA552>
              MOVE CLGC-ROLEFLAGS      TO WSCC-ROLEFLAGS                <FSA552>
                                                                        <FSA552>
              IF WSCC-CLTTYPE          =  WSSP-CLTTYPE-F                <FSA552>
              OR WSSP-CLTTYPE-F        =  SPACES                        <FSA552>
                 ADD  1                TO WSAA-SRHLIMIT                 <FSA552>
              END-IF                                                    <FSA552>
                                                                        <FSA552>
              PERFORM 7000-CHECK-NAME                                   <FSA552>
              IF WSAA-MATCH            =  'N'                           <FSA552>
                 GO TO A950-NEXT                                        <FSA552>
              ELSE                                                      <FSA552>
                 PERFORM 6000-CHECK-MATCH                               <FSA552>
                 IF WSAA-MATCH         =  'N'                           <FSA552>
                    GO TO A950-NEXT                                     <FSA552>
                 ELSE                                                   <FSA552>
                    MOVE CLGC-CLNTNUM  TO WSAA-STORE-CLNTNUM            <FSA552>
                    PERFORM 9000-CHECK-ROLE                             <FSA552>
                    IF WSAA-MATCH      =  'N'                           <FSA552>
                       GO TO A950-NEXT.                                 <FSA552>
      *                                                                 <GP3IND>
       A900-EXIT.
           EXIT.
      *                                                                 <021>
       B100-CLIENT-NAME SECTION.                                        <021>
      *************************                                         <021>
                                                                        <021>
           MOVE READR                  TO CLTS-FUNCTION.                <021>
           MOVE CLTSREC                TO CLTS-FORMAT.                  <021>
                                                                        <021>
           CALL 'CLTSIO' USING CLTS-PARAMS.                             <021>
           IF CLTS-STATUZ              NOT = O-K                        <021>
              MOVE CLTS-PARAMS         TO SYSR-PARAMS                   <021>
              PERFORM 600-FATAL-ERROR.                                  <021>
      *                                                                 <021>
           MOVE CLTS-SURNAME           TO WSCC-SURNAME.                 <021>
           MOVE CLTS-GIVNAME           TO WSCC-GIVNAME.                 <021>
           MOVE CLTS-LSURNAME          TO WSCC-LSURNAME.                <FSU307>
                                                                        <021>
      ****                                                              <A06277>
      **** Check whether the client country has changed.  If so, read   <A06277>
      **** the address validation table to obtain the address line      <A06277>
      **** number to be displayed.                                      <A06277>
      ****                                                              <A06277>
                                                                        <A06277>
           IF CLTS-CTRYCODE         NOT = WSAA-CTRYCODE                 <A06277>
               PERFORM 1300-READ-ADDR-VAL-TABLE                         <A06277>
               MOVE CLTS-CTRYCODE      TO WSAA-CTRYCODE                 <A06277>
           END-IF.                                                      <A06277>
      *                                                                 <A06277>
       B100-EXIT.                                                       <021>
           EXIT.                                                        <021>
      *
       B200-CONVERT SECTION.
      **********************
      *
           INSPECT WSAA-CONVERT REPLACING ALL 'a' BY 'A'.
           INSPECT WSAA-CONVERT REPLACING ALL 'b' BY 'B'.
           INSPECT WSAA-CONVERT REPLACING ALL 'c' BY 'C'.
           INSPECT WSAA-CONVERT REPLACING ALL 'd' BY 'D'.
           INSPECT WSAA-CONVERT REPLACING ALL 'e' BY 'E'.
           INSPECT WSAA-CONVERT REPLACING ALL 'f' BY 'F'.
           INSPECT WSAA-CONVERT REPLACING ALL 'g' BY 'G'.
           INSPECT WSAA-CONVERT REPLACING ALL 'h' BY 'H'.
           INSPECT WSAA-CONVERT REPLACING ALL 'i' BY 'I'.
           INSPECT WSAA-CONVERT REPLACING ALL 'j' BY 'J'.
           INSPECT WSAA-CONVERT REPLACING ALL 'k' BY 'K'.
           INSPECT WSAA-CONVERT REPLACING ALL 'l' BY 'L'.
           INSPECT WSAA-CONVERT REPLACING ALL 'm' BY 'M'.
           INSPECT WSAA-CONVERT REPLACING ALL 'n' BY 'N'.
           INSPECT WSAA-CONVERT REPLACING ALL 'o' BY 'O'.
           INSPECT WSAA-CONVERT REPLACING ALL 'p' BY 'P'.
           INSPECT WSAA-CONVERT REPLACING ALL 'q' BY 'Q'.
           INSPECT WSAA-CONVERT REPLACING ALL 'r' BY 'R'.
           INSPECT WSAA-CONVERT REPLACING ALL 's' BY 'S'.
           INSPECT WSAA-CONVERT REPLACING ALL 't' BY 'T'.
           INSPECT WSAA-CONVERT REPLACING ALL 'u' BY 'U'.
           INSPECT WSAA-CONVERT REPLACING ALL 'v' BY 'V'.
           INSPECT WSAA-CONVERT REPLACING ALL 'w' BY 'W'.
           INSPECT WSAA-CONVERT REPLACING ALL 'x' BY 'X'.
           INSPECT WSAA-CONVERT REPLACING ALL 'y' BY 'Y'.
           INSPECT WSAA-CONVERT REPLACING ALL 'z' BY 'Z'.
      *
       B200-EXIT.
           EXIT.
      *
       A1000-LOAD-FOR-IDNO SECTION.                                     <CAS1.0>
      ********************************                                  <CAS1.0>
      *                                                                 <CAS1.0>
       A1000-WRITE-TO-SUBFILE.                                          <CAS1.0>
      *                                                                 <CAS1.0>
      * Because SECUITYNO is also used for Corporate Clients to         <CAS1.0>
      * store the licence number, a check is required so that we        <CAS1.0>
      * only pick up personal clients.                                  <CAS1.0>
      *                                                                 <CAS1.0>
      *--- Allow to search Corporate Client by ID Number
      **** IF CLNTSSN-CLTTYPE = 'C'                             <V6F107><CAS1.0>
      ****    GO TO A1300-NEXT-REC.                             <V6F107><CAS1.0>
      *                                                                 <CAS1.0>
           MOVE SPACE                  TO S2473-SLT.                    <CAS1.0>
           MOVE SPACE                  TO WSAA-FILE-STATUZ.             <CAS1.0>
           MOVE CLTS-CLNTNUM           TO S2473-CLNTNUM.                <CAS1.0>
      **** MOVE WSCC-SECUITYNO         TO S2473-ZRSECNO.        <V4F002><CAS1.0>
           MOVE WSCC-SECUITYNO         TO S2473-ZDESC.                  <V4F002>
           MOVE WSCC-CLTTYPE           TO S2473-CLTTYPE.                <CAS1.0>
      **** MOVE WSCC-CLTADDR01         TO S2473-ZNAMEADR.       <V64F04><V4F002>
           MOVE WSCC-CLTADDR01         TO S2473-NAMEADR.                <V64F04>
           MOVE WSCC-CLTSTAT           TO S2473-CLTSTAT.                <V64F04>
      *                                                                 <CAS1.0>
      **** MOVE WSCC-CLTDOB            TO DTC1-INT-DATE.        <V4F002><CAS1.0>
      **** IF DTC1-INT-DATE           NOT NUMERIC               <V4F002><CAS1.0>
      ****    GO TO A1100-CONT.                                 <V4F002><CAS1.0>
      *                                                                 <CAS1.0>
      **** MOVE 'CONV'                 TO DTC1-FUNCTION.        <V4F002><CAS1.0>
      **** CALL 'DATCON1'  USING DTC1-DATCON1-REC.              <V4F002><CAS1.0>
      **** IF DTC1-STATUZ              NOT = O-K                <V4F002><CAS1.0>
      ****    MOVE DTC1-STATUZ         TO SYSR-STATUZ           <V4F002><CAS1.0>
      ****    MOVE DTC1-INT-DATE      TO SYSR-PARAMS            <V4F002><CAS1.0>
      ****    PERFORM 600-FATAL-ERROR.                          <V4F002><CAS1.0>
      **** MOVE DTC1-EXT-DATE          TO WSAA-PERS-CLTDOBX.    <V4F002><CAS1.0>
      *                                                                 <CAS1.0>
       A1100-CONT.                                                      <CAS1.0>
      *                                                                 <CAS1.0>
      **** MOVE WSCC-CLTSEX            TO WSAA-PERS-CLTSEX.     <V4F002><CAS1.0>
      *                                                                 <CAS1.0>
      *    Format personal client name                                  <CAS1.0>
      *                                                                 <CAS1.0>
      *    IF THEIR IS NO CLNTNUM THERE IS A BLANK SUBFILE              <CAS1.0>
      *                                                                 <CAS1.0>
           IF S2473-CLNTNUM             = SPACES                        <CAS1.0>
              GO TO A1200-CONT.                                         <CAS1.0>
                                                                        <CAS1.0>
           MOVE SPACES                 TO WSAA-PERS-NAME.               <CAS1.0>
           IF   WSCC-GIVNAME NOT = SPACES                               <V43L007
           STRING WSCC-SURNAME DELIMITED '  '                           <CAS1.0>
                  ', '         DELIMITED SIZE                           <CAS1.0>
                  WSCC-GIVNAME DELIMITED '  '                           <CAS1.0>
                                       INTO WSAA-PERS-NAME              <CAS1.0>
           ELSE                                                         <V43L007
           MOVE WSCC-SURNAME           TO WSAA-PERS-NAME                <V43L007
           END-IF.                                                      <V43L007
                                                                        <V43L007
      **** MOVE WSAA-PERS-DTL          TO S2473-CLTDTL.         <V4F002><CAS1.0>
           MOVE WSAA-PERS-DTL          TO S2473-OWNNAM.                 <V4F002>
      *                                                                 <CAS1.0>
       A1200-CONT.                                                      <CAS1.0>
      *                                                                 <CAS1.0>
      *  If windowing with a specific role,  check if client            <CAS1.0>
      *  has this type of role, if not, display the client              <CAS1.0>
      *  but protect the select field.                                  <CAS1.0>
      *                                                                 <CAS1.0>
           PERFORM 8000-FLAGS.                                          <CAS1.0>
           IF WSCC-CLTIND              = 'L' AND                        <FA3967>
              S2473-ALFLAG             = SPACE                          <FA3967>
              MOVE WSCC-CLTIND         TO S2473-ALFLAG                  <FA3967>
           END-IF.                                                      <FA3967>
           IF WSCC-CLTIND              = 'D' AND                        <FA3967>
              S2473-AAFLAG             = SPACE                          <FA3967>
              MOVE WSCC-CLTIND         TO S2473-AAFLAG                  <FA3967>
           END-IF.                                                      <FA3967>
      *                                                                 <CAS1.0>
           MOVE SADD                   TO SCRN-FUNCTION.                <CAS1.0>
           MOVE 'Y'                    TO WSAA-RECORD-ADDED.            <FSU320>
      *                                                                 <CAS1.0>
           CALL 'S2473IO' USING SCRN-SCREEN-PARAMS                      <CAS1.0>
                                S2473-DATA-AREA                         <CAS1.0>
                                S2473-SUBFILE-AREA.                     <CAS1.0>
           IF SCRN-STATUZ              NOT = O-K                        <CAS1.0>
              MOVE SCRN-STATUZ         TO SYSR-STATUZ                   <CAS1.0>
              PERFORM 600-FATAL-ERROR.                                  <CAS1.0>
      *                                                                 <CAS1.0>
       A1300-NEXT-REC.                                                  <CAS1.0>
      *                                                                 <CAS1.0>
      *    Find next CLNTSSNSKM record.                                 <CAS1.0>
      *                                                                 <CAS1.0>
           MOVE NEXTR                  TO CLNTSSN-FUNCTION.             <CAS1.0>
           MOVE CLNTSSNREC                TO CLNTSSN-FORMAT.            <CAS1.0>
           CALL 'CLNTSSNIO'            USING CLNTSSN-PARAMS.            <CAS1.0>
           IF  CLNTSSN-STATUZ          NOT = O-K                        <CAS1.0>
           AND CLNTSSN-STATUZ          NOT = ENDP                       <CAS1.0>
               MOVE CLNTSSN-PARAMS      TO SYSR-PARAMS                  <CAS1.0>
               PERFORM 600-FATAL-ERROR.                                 <CAS1.0>
      *                                                                 <CAS1.0>
           IF CLNTSSN-CLNTCOY NOT = WSSP-FSUCO                          <CAS1.0>
              MOVE ENDP               TO CLNTSSN-STATUZ.                <CAS1.0>
      *                                                                 <CAS1.0>
           IF CLNTSSN-STATUZ              = ENDP                        <CAS1.0>
              MOVE CLNTSSN-STATUZ         TO WSAA-FILE-STATUZ           <CAS1.0>
              GO TO A1500-CONT.                                         <CAS1.0>
      *                                                                 <CAS1.0>
           MOVE CLNTSSN-CLNTPFX        TO CLTS-CLNTPFX.                 <CAS1.0>
           MOVE CLNTSSN-CLNTCOY        TO CLTS-CLNTCOY.                 <CAS1.0>
           MOVE CLNTSSN-CLNTNUM        TO CLTS-CLNTNUM.                 <CAS1.0>
           MOVE READR                  TO CLTS-FUNCTION.                <CAS1.0>
           MOVE CLTSREC                TO CLTS-FORMAT.                  <CAS1.0>
           CALL 'CLTSIO'               USING CLTS-PARAMS.               <CAS1.0>
           IF CLTS-STATUZ              NOT = O-K AND MRNF               <CAS1.0>
               MOVE CLTS-PARAMS        TO SYSR-PARAMS                   <CAS1.0>
               PERFORM 600-FATAL-ERROR.                                 <CAS1.0>
      *                                                                 <CAS1.0>
           MOVE CLTS-SURNAME           TO WSCC-SURNAME.                 <CAS1.0>
           MOVE CLTS-GIVNAME           TO WSCC-GIVNAME.                 <CAS1.0>
           MOVE CLTS-MIDDL01           TO WSCC-MIDDL01.                 <CAS1.0>
           MOVE CLTS-MIDDL02           TO WSCC-MIDDL02.                 <CAS1.0>
           MOVE CLTS-CLTTYPE           TO WSCC-CLTTYPE.                 <CAS1.0>
           MOVE CLTS-CLTSTAT           TO WSCC-CLTSTAT.                 <CAS1.0>
           MOVE CLTS-SERVBRH           TO WSCC-SERVBRH.                 <CAS1.0>
           MOVE CLTS-CLTSEX            TO WSCC-CLTSEX.                  <CAS1.0>
           MOVE CLTS-CLTDOB            TO WSCC-CLTDOB.                  <CAS1.0>
           MOVE CLTS-SECUITYNO         TO WSCC-SECUITYNO.               <CAS1.0>
           MOVE CLTS-CLTIND            TO WSCC-CLTIND.                  <CAS1.0>
           MOVE CLTS-ROLEFLAGS         TO WSCC-ROLEFLAGS.               <CAS1.0>
           MOVE CLTS-LSURNAME          TO WSCC-LSURNAME.                <FSU307>
           MOVE CLTS-CLTADDR01         TO WSCC-CLTADDR01.               <V4F002>
      *                                                                 <CAS1.0>
       A1500-CONT.                                                      <CAS1.0>
      *                                                                 <CAS1.0>
           IF (WSAA-RECORD-ADDED       = 'N')                           <CAS1.0>
              GO TO A1500-CHECK-STATUZ.                                 <CAS1.0>
                                                                        <CAS1.0>
           MOVE 'N'                 TO WSAA-RECORD-ADDED.               <CAS1.0>
                                                                        <CAS1.0>
           DIVIDE SCRN-SUBFILE-RRN BY 14 GIVING WSAA-NUMBER             <CAS1.0>
                                         REMAINDER WSAA-REM.            <CAS1.0>
           IF WSAA-REM = 0                                              <CAS1.0>
              GO TO A1600-SET-MORE-SIGN.                                <CAS1.0>
      *                                                                 <CAS1.0>
       A1500-CHECK-STATUZ.                                              <CAS1.0>
      *                                                                 <CAS1.0>
           IF WSAA-FILE-STATUZ         NOT = ENDP                       <CAS1.0>
              GO TO A1000-WRITE-TO-SUBFILE.                             <CAS1.0>
      *                                                                 <CAS1.0>
       A1600-SET-MORE-SIGN.                                             <CAS1.0>
           IF WSAA-FILE-STATUZ         NOT = ENDP                       <CAS1.0>
              MOVE 'Y'                 TO SCRN-SUBFILE-MORE             <CAS1.0>
           ELSE                                                         <CAS1.0>
              MOVE SPACE               TO SCRN-SUBFILE-MORE.            <CAS1.0>
                                                                        <V6F107>
           MOVE 1                      TO SCRN-SUBFILE-RRN.             <V6F107>
      *                                                                 <CAS1.0>
       A1999-EXIT.                                                      <CAS1.0>
           EXIT.                                                        <CAS1.0>
      *                                                                 <021>
